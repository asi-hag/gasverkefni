# uppfaera_dagleg.R
# Keyrir sjálfkrafa á hverjum degi kl. 8:30
# - Uppfærir JSON gögn með Python
# - Reiknar aðeins nýjar dagsetningar (incremental)
# - Vistar Excel skjöl og afritar í SharePoint

cat("=== Dagleg uppfærsla bensínverðs ===\n")
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# --- Slóðir ---
SKRA_MAPPA  <- getwd()
CACHE_GAS   <- file.path(SKRA_MAPPA, "cache_gas.rds")
CACHE_MM    <- file.path(SKRA_MAPPA, "cache_min_max.rds")
CACHE_WTI   <- file.path(SKRA_MAPPA, "cache_wti.rds")
CACHE_USD   <- file.path(SKRA_MAPPA, "cache_usd.rds")
EXCEL_VIKUR <- file.path(SKRA_MAPPA, "vikur_fra_2017.xlsx")
EXCEL_MM    <- file.path(SKRA_MAPPA, "min_max_dagverd.xlsx")

FYRIRTAEKI <- c("Atlantsolía", "Costco Iceland", "N1", "ÓB", "Olís", "Orkan")

# --- 1. Uppfæra JSON ---
cat("1. Uppfæri gas_prices_history.json...\n")
uppfaera <- system(
  paste("python3", file.path(SKRA_MAPPA, "extract_history.py")),
  wait = TRUE
)
if (uppfaera != 0) stop("Python scriptan mistókst. Athugaðu extract_history.py.")
cat("   JSON uppfært.\n\n")

# --- 2. Pakkar ---
suppressPackageStartupMessages({
  library(jsonlite)
  library(tidyverse)
  library(lubridate)
  library(rio)
  library(xml2)
  library(writexl)
})

# --- 3. Lesa og hreinsa JSON ---
cat("2. Les JSON gögn...\n")
gas_data_raw <- fromJSON(file.path(SKRA_MAPPA, "gas_prices_history.json"))

df <- gas_data_raw %>%
  mutate(timestamp = ymd_hms(timestamp)) %>%
  unnest(prices) %>%
  unnest(stations) %>%
  unnest_wider(geo) %>%
  select(timestamp, station_name = name, company, key,
         bensin95, bensin95_discount, diesel, diesel_discount, lat, lon) %>%
  mutate(
    date  = as_date(timestamp),
    year  = year(date),
    month = month(date)
  )

cat("   ", nrow(df), "línur,", n_distinct(df$date), "einstaka dagar.\n\n")

# --- 4. WTI og USD (cache í 20 klukkustundir) ---
cache_gildur <- function(skra) {
  file.exists(skra) && difftime(Sys.time(), file.mtime(skra), units = "hours") < 20
}

cat("3. WTI og USD gögn...\n")

if (cache_gildur(CACHE_WTI)) {
  cat("   WTI: nota cache.\n")
  wti_data <- readRDS(CACHE_WTI)
} else {
  cat("   WTI: sæki nýtt...\n")
  temp_file <- tempfile(fileext = ".xls")
  download.file("https://ir.eia.gov/wpsr/psw11.xls", temp_file, mode = "wb", quiet = TRUE)
  wti_data <- import(temp_file, which = "Data 2", skip = 2) %>%
    select(1:2) %>%
    setNames(c("date", "price")) %>%
    mutate(date = as_date(date)) %>%
    filter(!is.na(price))
  saveRDS(wti_data, CACHE_WTI)
}

if (cache_gildur(CACHE_USD)) {
  cat("   USD: nota cache.\n")
  usd_gengi <- readRDS(CACHE_USD)
} else {
  cat("   USD: sæki nýtt...\n")
  usd_url <- "https://sedlabanki.is/xmltimeseries/Default.aspx?DagsFra=2016-01-01&TimeSeriesID=4055&Type=xml"
  usd_gengi <- read_xml(usd_url) %>%
    xml_find_all(".//TimeSeriesData/Entry") %>%
    map_df(~ data.frame(
      date = mdy_hms(xml_text(xml_find_first(.x, ".//Date"))),
      usd_exchange_rate = as.numeric(xml_text(xml_find_first(.x, ".//Value")))
    )) %>%
    mutate(date = as_date(date)) %>%
    arrange(date) %>%
    fill(usd_exchange_rate, .direction = "up") %>%
    na.omit()
  saveRDS(usd_gengi, CACHE_USD)
}

cat("\n")

# --- 5. Incremental: dagleg samantekt (gas) ---
reikna_gas_dagleg <- function(df, wti, usd, fra_date) {
  df %>%
    filter(bensin95 > 150, date >= fra_date, date <= today(),
           company %in% FYRIRTAEKI) %>%
    group_by(date, company) %>%
    summarise(min_bensin95 = min(bensin95, na.rm = TRUE), .groups = "drop") %>%
    complete(date = seq.Date(fra_date, today(), by = "day"), company) %>%
    group_by(company) %>% arrange(date) %>%
    fill(min_bensin95, .direction = "down") %>%
    ungroup() %>%
    filter(!is.na(min_bensin95)) %>%
    group_by(date) %>%
    summarise(avg_min_bensin95 = mean(min_bensin95, na.rm = TRUE), .groups = "drop") %>%
    left_join(wti, by = "date") %>%
    left_join(usd, by = "date") %>%
    mutate(
      wti_usd_per_liter = price / 3.785,
      wti_isk_per_liter = (price * usd_exchange_rate) / 3.785
    ) %>%
    select(date, avg_min_bensin95,
           wti_price_usd = price, usd_exchange_rate, wti_usd_per_liter, wti_isk_per_liter) %>%
    fill(wti_price_usd, usd_exchange_rate, wti_usd_per_liter, wti_isk_per_liter,
         .direction = "down")
}

cat("4. Reikna vikusamantekt (gas)...\n")
if (file.exists(CACHE_GAS)) {
  gas_cache <- readRDS(CACHE_GAS)
  # Overlap 3 dagar til að grípa seint komnar færslur
  fra_date <- max(gas_cache$date) - days(3)
  cat("   Cache til:", format(max(gas_cache$date)), "- reikna frá:", format(fra_date), "\n")
  nytt <- reikna_gas_dagleg(df, wti_data, usd_gengi, fra_date)
  gas <- bind_rows(filter(gas_cache, date < fra_date), nytt) %>%
    arrange(date) %>%
    fill(wti_price_usd, usd_exchange_rate, wti_usd_per_liter, wti_isk_per_liter, .direction = "down")
} else {
  cat("   Engin cache - reikna allt frá 2016.\n")
  gas <- reikna_gas_dagleg(df, wti_data, usd_gengi, ymd("2016-12-01"))
}
saveRDS(gas, CACHE_GAS)
cat("   gas:", nrow(gas), "dagar.\n\n")

weekly_comparison <- gas %>%
  mutate(day = wday(date), week = week(date), year = year(date)) %>%
  group_by(year, week) %>%
  summarise(
    week_start  = min(date),
    avg_bensin95 = mean(avg_min_bensin95, na.rm = TRUE),
    avg_wti_usd  = mean(wti_usd_per_liter, na.rm = TRUE),
    avg_wti_isk  = mean(wti_isk_per_liter, na.rm = TRUE),
    avg_usd_rate = mean(usd_exchange_rate, na.rm = TRUE),
    .groups = "drop"
  )

# --- 6. Incremental: min/max verð ---
reikna_min_max <- function(df, fra_date) {
  df %>%
    filter(bensin95 > 150, date >= fra_date, date <= today(),
           company %in% FYRIRTAEKI) %>%
    group_by(date, company) %>%
    summarise(
      min = min(bensin95, na.rm = TRUE),
      max = max(bensin95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(max = ifelse(company == "Costco Iceland", min, max)) %>%
    complete(date = seq.Date(fra_date, today(), by = "day"), company) %>%
    group_by(company) %>% arrange(date) %>%
    fill(min, max, .direction = "down") %>%
    ungroup() %>%
    na.omit()
}

cat("5. Reikna min/max verð...\n")
if (file.exists(CACHE_MM)) {
  mm_cache <- readRDS(CACHE_MM)
  fra_date_mm <- max(mm_cache$date) - days(3)
  cat("   Cache til:", format(max(mm_cache$date)), "- reikna frá:", format(fra_date_mm), "\n")
  nytt_mm <- reikna_min_max(df, fra_date_mm)
  min_max <- bind_rows(filter(mm_cache, date < fra_date_mm), nytt_mm) %>% arrange(date)
} else {
  cat("   Engin cache - reikna frá 2025.\n")
  min_max <- reikna_min_max(df, ymd("2025-01-01"))
}
saveRDS(min_max, CACHE_MM)
cat("   min_max:", nrow(min_max), "línur.\n\n")

# --- 7. Vista Excel skjöl ---
cat("6. Vista Excel skjöl...\n")
weekly_comparison |> write_xlsx(EXCEL_VIKUR)
min_max |>
  pivot_wider(
    names_from = company,
    values_from = c(min, max),
    names_vary = "slowest"
  ) |>
  write_xlsx(EXCEL_MM)
cat("   vikur_fra_2017.xlsx og min_max_dagverd.xlsx vistuð.\n\n")

cat("\n=== Lokið:", format(Sys.time(), "%H:%M:%S"), "===\n")
