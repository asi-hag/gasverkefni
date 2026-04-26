# skoða gögn frá upphafi bensínvaktarinnar


# Pakkar
library(jsonlite)
library(tidyverse)
library(lubridate)
library(janitor)
library(rio)
library(xml2)
library(writexl)

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
# opinber_gjold <- import("opinber_gjold.xlsx") |> select(-gjold, -vsk)

# opinber_gjold<- opinber_gjold |> arrange(date) |> 
 # complete(
 #     date = seq.Date(ymd("2007-08-01"), today(), by = "day"),
 # ) %>%
  # færa frá mánuðum yfir í daga
 # fill(gjald_a, gjald_s, gjald_k, .direction = "down")


# Sameina við df
# df <- df |> left_join(opinber_gjold, by = "date")


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
  # Sía fyrir des 2016 til dagsins í dag
  filter(date >= ymd("2016-12-01") & date <= today(),
)  %>%
  filter(
    company %in% c("Atlantsolía", "Costco Iceland", "N1", "ÓB", "Olís", "Orkan" )
  ) |> 
  # Complete til að fá allar dagsetningar fyrir hvert fyrirtæki
  complete(
    date = seq.Date(ymd("2016-12-01"), today(), by = "day"),
    company
  ) %>%
  # Filla í NA gildi með síðasta þekkta gildi fyrir hvert fyrirtæki
  group_by(company) %>% arrange(date) |> 
  fill(min_bensin95, .direction = "down") %>%
  ungroup() %>%
  # Sía út raðir þar sem ennþá er NA (fyrirtæki sem hafa engin gögn í byrjun)
  filter(!is.na(min_bensin95)) %>%
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
  )

weekly_comparison |> filter(year==2026)


weekly_comparison |> write_xlsx("vikur_fra_2017.xlsx")


## min max greining

min_max <- df %>%
  filter(bensin95>150) |>
  # Finna lægsta og hæsta verð hvers fyrirtækis á dag
  group_by(date, company) %>%
  summarise(
    min = min(bensin95, na.rm = TRUE),
    max = max(bensin95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(max = ifelse(company == "Costco Iceland", min, max)) %>%
  # Sía fyrir jan 2025 til dagsins í dag
  filter(date > ymd("2024-12-31") & date <= today(),
company %in% c("Atlantsolía", "Costco Iceland", "N1", "ÓB", "Olís", "Orkan" )) %>%
  # Complete til að fá allar dagsetningar fyrir hvert fyrirtæki
  ## Byrja á að skoða bara frá 2025
  complete(
    date = seq.Date(ymd("2025-01-01"), today(), by = "day"),
    company
  ) %>%
  # Filla í NA gildi með síðasta þekkta gildi fyrir hvert fyrirtæki
  group_by(company) %>%
  arrange(date) |>
  fill(min, max, .direction = "down") |>
  ungroup() |>
  na.omit() 


# ef excel
min_max  |> 
  pivot_wider(
    names_from = company, values_from = c(min, max),
    names_vary = "slowest"
  ) |> write_xlsx("min_max_dagverd.xlsx")


min_max |> 
  filter(date>"2026-02-01") |> 
  ggplot(
    aes(x=date, y=min, colour = company)
  ) + geom_step()+ 
  facet_wrap(~company)+
  theme_minimal()+
 scale_x_date(date_labels = "%d/%m", date_breaks = "2 week")+ 
  theme(legend.position = "")+
  labs(x="", y="", colour ="",
title = "Lægstu verð", subtitle = "Frá febrúar 2026")



min_max |> 
  filter(date>"2026-02-01") |> 
  ggplot(
    aes(x=date, y=max, colour = company)
  ) + geom_step()+ 
  facet_wrap(~company)+
  theme_minimal()+
 scale_x_date(date_labels = "%d/%m", date_breaks = "2 week")+ 
  theme(legend.position = "")+
  labs(x="", y="", colour ="",
title = "Hæstu verð", subtitle = "Frá febrúar 2026")


min_max |> pivot_longer(c(min, max)) |> 
  filter(date>"2026-03-01") |> 
  ggplot(
    aes(x=date, y=value, colour = name)
  ) + geom_step()+ 
  facet_wrap(~company)+
  theme_minimal()+
 scale_x_date(date_labels = "%d/%m", date_breaks = "1 week")+ 
  theme(legend.position = "bottom")+
  labs(x="", y="", colour ="",
title = "Verð - min/max", subtitle = "Frá mars 2026")
