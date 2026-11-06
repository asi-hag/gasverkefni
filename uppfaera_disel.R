# Keyra úr rót verkefnisins: source("uppfaera_disel.R")
# Sama aðferð og í uppfaera_dagleg.R: lægsta verð hvers fyrirtækis á dag,
# síðan óvegið meðaltal fyrirtækjanna. Notar diesel, ekki diesel_discount.
# Markaðsviðmið: EIA/FRED DDFUELNYH, staðverð díselolíu í New York Harbor.

suppressPackageStartupMessages({
  library(jsonlite)
  library(tidyverse)
  library(lubridate)
  library(writexl)
})

SKRA_MAPPA <- getwd()
CACHE_DISEL <- file.path(SKRA_MAPPA, "cache_disel.rds")
EXCEL_DAGAR <- file.path(SKRA_MAPPA, "disel_dagverd.xlsx")
EXCEL_VIKUR <- file.path(SKRA_MAPPA, "disel_vikur_fra_2017.xlsx")
FYRIRTAEKI <- c("Atlantsolía", "Costco Iceland", "N1", "ÓB", "Olís", "Orkan")
UPPHAF <- as_date("2016-12-01")

cat("=== Uppfærsla díselverðs ===\n")
# Setja options(disel.uppfaera_json = FALSE) til að nota fyrirliggjandi JSON.
if (getOption("disel.uppfaera_json", TRUE)) {
  uppfaera <- system2("python3", shQuote(file.path(SKRA_MAPPA, "extract_history.py")))
  if (uppfaera != 0) stop("Python-skriftan mistókst. Athugaðu extract_history.py.")
}

df_disel <- fromJSON(file.path(SKRA_MAPPA, "gas_prices_history.json")) |>
  mutate(timestamp = ymd_hms(timestamp)) |>
  unnest(prices) |>
  unnest(stations) |>
  transmute(date = as_date(timestamp), company, diesel)

reikna_disel_dagleg <- function(df, fra_date, til_date = today()) {
  # Halda síðasta þekkta verði fyrir upphaf endurreiknings svo fyrirtæki
  # falli ekki úr meðaltalinu þegar engin ný færsla er á fyrstu dögunum.
  verd <- df |>
    filter(is.finite(diesel), diesel > 150, date <= til_date,
           company %in% FYRIRTAEKI) |>
    group_by(date, company) |>
    summarise(min_diesel = min(diesel), .groups = "drop")

  fyrri_verd <- verd |>
    filter(date < fra_date) |>
    group_by(company) |>
    slice_max(date, n = 1, with_ties = FALSE) |>
    ungroup()

  bind_rows(fyrri_verd, filter(verd, date >= fra_date)) |>
    complete(date = seq.Date(fra_date, til_date, by = "day"),
             company = FYRIRTAEKI) |>
    arrange(company, date) |>
    group_by(company) |>
    fill(min_diesel, .direction = "down") |>
    ungroup() |>
    filter(date >= fra_date, !is.na(min_diesel)) |>
    group_by(date) |>
    summarise(avg_min_diesel = mean(min_diesel),
              fjoldi_fyrirtaekja = n(), .groups = "drop")
}

if (file.exists(CACHE_DISEL)) {
  disel_cache <- readRDS(CACHE_DISEL) |>
    select(date, avg_min_diesel, fjoldi_fyrirtaekja)
  fra_date <- if (nrow(disel_cache) > 0) {
    max(UPPHAF, min(max(disel_cache$date), today()) - days(3))
  } else {
    UPPHAF
  }
  disel <- bind_rows(
    filter(disel_cache, date < fra_date),
    reikna_disel_dagleg(df_disel, fra_date)
  ) |> arrange(date)
} else {
  disel <- reikna_disel_dagleg(df_disel, UPPHAF)
}
source(file.path(SKRA_MAPPA, "markadsverd.R"))
# Gengisgögn eru uppfærð af uppfaera_dagleg.R; sækja hér fyrir sjálfstæða keyrslu.
usd_skra <- file.path(SKRA_MAPPA, "cache_usd.rds")
if (!file.exists(usd_skra) || difftime(Sys.time(), file.mtime(usd_skra), units = "hours") >= 20) {
  usd_gengi <- xml2::read_xml("https://sedlabanki.is/xmltimeseries/Default.aspx?DagsFra=2016-01-01&TimeSeriesID=4055&Type=xml") |>
    xml2::xml_find_all(".//TimeSeriesData/Entry") |>
    map_dfr(function(x) tibble(
      date = as_date(mdy_hms(xml2::xml_text(xml2::xml_find_first(x, ".//Date")))),
      usd_exchange_rate = as.numeric(xml2::xml_text(xml2::xml_find_first(x, ".//Value")))
    )) |> arrange(date) |> filter(!is.na(usd_exchange_rate))
  stopifnot(nrow(usd_gengi) > 0)
  saveRDS(usd_gengi, usd_skra)
} else {
  usd_gengi <- readRDS(usd_skra)
}
disel <- sameina_markadsverd(disel, saekja_stadverd("DDFUELNYH", SKRA_MAPPA), usd_gengi)
saveRDS(disel, CACHE_DISEL)
weekly_comparison_disel <- vista_vikur_2025(disel, "avg_min_diesel", "DDFUELNYH",
                 file.path(SKRA_MAPPA, "disel_vikur_fra_2025.xlsx"))

# Eldri samantekt varðveitt fyrir disel_vikur_fra_2017.xlsx.
weekly_comparison_disel_eldri <- disel |>
  mutate(week = week(date), year = year(date)) |>
  group_by(year, week) |>
  summarise(week_start = min(date),
            avg_diesel = mean(avg_min_diesel), .groups = "drop") |>
  arrange(week_start)

write_xlsx(disel, EXCEL_DAGAR)
write_xlsx(weekly_comparison_disel_eldri, EXCEL_VIKUR)
cat("Vistað:", EXCEL_DAGAR, "og", EXCEL_VIKUR, "\n")
print(tail(disel))
