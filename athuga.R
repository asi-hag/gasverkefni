# Gasvaktin

# Uppfæra eldsneytisgögn
cat("Uppfæri gas_prices_history.json...\n")
uppfaera <- system("python3 extract_history.py", wait = TRUE)
if (uppfaera != 0) stop("Python scriptan mistókst. Athugaðu hvort extract_history.py virki rétt.")
cat("Gögn uppfærð.\n\n")

# Pakkar
library(jsonlite)
library(tidyverse)
library(lubridate)
library(janitor)
library(rio)
library(xml2)

# Lesa inn gögnin
gas_data_raw <- fromJSON("gas_prices_history.json")

# Umbreyta í tidy format
gas_data <- gas_data_raw %>%
  # Umbreyta timestamp í dagsetningu
  mutate(timestamp = ymd_hms(timestamp)) %>%
  # Taka út stöðvar úr nested structure
  unnest(prices) %>%
  unnest(stations) %>%
  # Taka út geo coordinates (geo er nested list)
  unnest_wider(geo) %>%
  # Velja mikilvægustu breyturnar
  select(
    timestamp,
    station_name = name,
    company,
    key,
    bensin95,
    bensin95_discount,
    diesel,
    diesel_discount,
    lat,
    lon
  )

library(skimr)
library(stringdist)

#

df<- gas_data |> mutate(
  date=as_date(timestamp),
  year = year(date),
  month = month(date)
)

# Lesa opinber gjöld
opinber_gjold <- import("opinber_gjold.xlsx") |> select(date, gjold)


# Bensínverð

ny_url<- "https://ir.eia.gov/wpsr/psw11.xls"
temp_file <- tempfile(fileext = ".xls")
download.file(ny_url, temp_file, mode = "wb")

wti_data_raw <- import(temp_file, which = "Data 2", skip = 2) %>%
  select(1:2) %>%
  setNames(c("date", "price")) %>%
  mutate(date = as_date(date)) %>%
  filter(!is.na(price))


# Gengi dollar

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

# Sameinaður data frame
combined_df <- df %>%
  filter(bensin95>150) |>
  # Finna lægsta verð hvers fyrirtækis á dag
  group_by(date, company) %>%
  summarise(
    min_bensin95 = min(bensin95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Sía fyrir des 2024 til dagsins í dag
  filter(date >= ymd("2016-12-01") & date <= today()) %>%
  # Finna fyrirtæki sem eru virk í 2025 (hafa gögn eftir 1. janúar 2025)
  group_by(company) %>%
  filter(
    company %in% c("Atlantsolía", "Costco Iceland", "N1", "ÓB", "Olís", "Orkan" )
  ) %>%
  ungroup() %>%
  # Complete til að fá allar dagsetningar fyrir hvert fyrirtæki
  complete(
    date = seq.Date(ymd("2016-12-01"), today(), by = "day"),
    company
  ) %>%
  # Filla í NA gildi með síðasta þekkta gildi fyrir hvert fyrirtæki
  group_by(company) %>%
  arrange(date) |>
  fill(min_bensin95, .direction = "down") %>%
  ungroup() %>%
  # Taka meðaltal af lægstu verðum allra fyrirtækja á hverjum degi
  group_by(date) %>%
  summarise(
    avg_min_bensin95 = mean(min_bensin95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Sameina við WTI olíuverð
  left_join(wti_data_raw, by = "date") %>%
  # Sameina við gengi
  left_join(usd_gengi, by = "date") %>%
  # Reikna WTI verð bæði í dollurum og krónum á lítra
  # WTI er í dollurum á gallon (1 gallon = 3.785 lítrar)
  mutate(
    wti_usd_per_liter = price / 3.785,
    wti_isk_per_liter = (price * usd_exchange_rate) / 3.785
  ) %>%
  # Endurraða dálkum
  select(date, avg_min_bensin95,
         wti_price_usd = price, usd_exchange_rate, wti_usd_per_liter, wti_isk_per_liter)

# Skoða niðurstöðuna
combined_df %>% glimpse()
combined_df %>% head(20)

combined_df |> tail()

gas<- combined_df |> fill(wti_price_usd, usd_exchange_rate, wti_usd_per_liter, wti_isk_per_liter, .direction = "down") 

# Vikusamanburður
weekly_comparison <- gas %>%
  # Bæta við vikunúmeri
  mutate(
    day = wday(date),
    week = week(date),
    year = year(date)
  ) %>%
  # Reikna meðaltal fyrir hverja viku
  group_by(year, week) %>%
  summarise(
    week_start = min(date),
    avg_bensin95 = mean(avg_min_bensin95, na.rm = TRUE),
    avg_wti_usd = mean(wti_usd_per_liter, na.rm = TRUE),
    avg_wti_isk = mean(wti_isk_per_liter, na.rm = TRUE),
    avg_usd_rate = mean(usd_exchange_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Raða eftir dagsetningu
  arrange(week_start) 

# Skoða niðurstöðuna
weekly_comparison %>% glimpse()
weekly_comparison %>% head(10)


weekly_comparison |> filter(year==2026) |> clipr::write_clip()

# Umbreyta í long format fyrir ggplot
weekly_long <- weekly_comparison %>%
  filter(year(week_start) == 2025) %>%
  select(week_start, week, bensin95_index, bensin_index, wti_usd_index, wti_isk_index, usd_rate_index) %>%
  pivot_longer(
    cols = c(bensin95_index, bensin_index, wti_usd_index, wti_isk_index, usd_rate_index),
    names_to = "variable",
    values_to = "index"
  ) %>%
  mutate(
    variable = case_when(
      variable == "bensin95_index" ~ "Bensínverð",
      variable == "bensin_index" ~ "Bensínverð (strípað)",
      variable == "wti_usd_index" ~ "WTI olíuverð (USD/L)",
      variable == "wti_isk_index" ~ "WTI olíuverð (ISK/L)",
      variable == "usd_rate_index" ~ "USD gengi"
    )
  )

# Mynd með vísitölum
# Umbreyta week_start í Date fyrst
weekly_long <- weekly_long |>
  mutate(week_start = as_date(week_start))



# Mynd með nafnstærðum
weekly_levels <- weekly_comparison %>%
  filter(year(week_start) > 2024) %>%
  mutate(week_start = as_date(week_start)) %>%
  select(week_start, week, avg_bensin95, avg_bensin, avg_wti_isk, avg_wti_usd) %>%
  pivot_longer(
    cols = c(avg_bensin95, avg_bensin, avg_wti_isk, avg_wti_usd),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = case_when(
      variable == "avg_bensin95" ~ "Bensínverð (kr/L)",
      variable == "avg_bensin" ~ "Bensínverð strípað (kr/L)",
      variable == "avg_wti_isk" ~ "WTI olíuverð (kr/L)", 
      variable == "avg_wti_usd" ~ "WTI olíuverð (USD/L)"
    )
  )




gas_week<- weekly_wide <- weekly_levels |>
  select(-week) |>
  pivot_wider(names_from=variable, values_from = value)

# Vista í Excel skjal
library(writexl)
write_xlsx(weekly_comparison, "vikusamanburður_2017.xlsx")

gas_week |> tail()


# Athugum lækkun um áramót
iblondun <- (1-7.86135327445104/100)


df1 |> 
  filter(
    date == as_date("2025-12-30")| date == as_date("2026-01-03")
  ) |> arrange(timestamp) |> 
  select(date, company, key, bensin95) |> 
  pivot_wider(names_from = "date", values_from = bensin95) |> 
  mutate(fyrir = (`2025-12-30`/1.24) - 108.80*iblondun, 
          eftir = (`2026-01-03`/1.24) - 24.25*iblondun) |> 
  mutate(munur=round(fyrir-eftir, 2)) |> 
  ggplot(aes(x=munur))+geom_bar(stat="count") +
  facet_wrap(~company, scales = "free_y")

  

df1 |> 
  filter(
    date == as_date("2025-12-30")| date == as_date("2026-01-03")
  ) |> arrange(timestamp) |> 
  select(date, company, key, bensin95) |> 
  pivot_wider(names_from = "date", values_from = bensin95) |> 
  mutate(fyrir = (`2025-12-30`/1.24) - 108.80*iblondun, 
          eftir = (`2026-01-03`/1.24) - 24.25*iblondun) |> 
  mutate(munur=round(fyrir-eftir, 2)) |> group_by(company) |> 
  count(munur) |> clipr::write_clip()

df1 |> 
  filter(
    date >= as_date("2025-12-30")) |> 
  arrange(timestamp) |> 
  filter(key %in% c("n1_001", "n1_023")) |> 
  ggplot(aes(x=timestamp, y=bensin95, color=key))+geom_line()+
  scale_x_datetime(date_labels = "%d/%m %H:%M:%S", date_breaks = "12 hours")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


  
gas |> arrange(date) |> tail() |> glimpse()


df1 |> 
  filter(
    date == as_date("2025-12-30")| date == as_date("2026-01-03")
  ) |> arrange(timestamp) |> 
  select(date, company, key, bensin95) |> 
  pivot_wider(names_from = "date", values_from = bensin95) |> 
  mutate(fyrir = (`2025-12-30`), 
          eftir = (`2026-01-03`)) |> 
  mutate(munur=round(fyrir-eftir, 2),
          pr =(fyrir/eftir -1)*100
        ) |> 
  group_by(company) |> skimr::skim(munur, pr)


stodvar<- df1 |> 
  filter(
    date == as_date("2025-12-30")| date == as_date("2026-01-03")
  ) |> arrange(timestamp) |> 
  filter(is.na(bensin95_discount)) |> distinct(company, .keep_all = TRUE) |> pull(key)

df1 |> 
  filter(
    date == as_date("2025-12-30")| date == as_date("2026-01-03")
  ) |> 
  arrange(timestamp) |> 
  filter(key %in% stodvar) |> 
  select(timestamp, key, diesel) |>
  print(digits = 10)


df1 |> filter(company=="Olís", date == as_date("2026-01-03")) |> 
  arrange(diesel)
