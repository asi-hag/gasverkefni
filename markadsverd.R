# Sameiginleg staðverðsviðmið frá EIA/FRED, New York Harbor.
saekja_stadverd <- function(series, mappa = getwd()) {
  skra <- file.path(mappa, paste0("cache_", series, ".rds"))
  if (file.exists(skra) && difftime(Sys.time(), file.mtime(skra), units = "hours") < 20) {
    return(readRDS(skra))
  }
  x <- read.csv(paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series),
                na.strings = ".") |>
    setNames(c("date", "world_usd_gallon")) |>
    mutate(date = as_date(date)) |>
    filter(!is.na(world_usd_gallon)) |>
    arrange(date)
  stopifnot(nrow(x) > 0, !anyDuplicated(x$date))
  saveRDS(x, skra)
  x
}

sameina_markadsverd <- function(daglegt, markadur, usd) {
  # Nota líka síðustu þekktu athugun fyrir upphaf innlendu gagnanna.
  dagar <- tibble(date = seq(min(c(daglegt$date, markadur$date, usd$date)),
                            max(daglegt$date), by = "day")) |>
    left_join(mutate(markadur, markadsverd_dags = date), by = "date") |>
    left_join(transmute(usd, date, usd_exchange_rate, gengi_dags = date), by = "date") |>
    arrange(date) |>
    fill(world_usd_gallon, usd_exchange_rate, markadsverd_dags, gengi_dags, .direction = "down") |>
    mutate(world_usd_per_liter = world_usd_gallon / 3.785411784,
           world_isk_per_liter = world_usd_per_liter * usd_exchange_rate)
  left_join(daglegt, dagar, by = "date")
}

vista_vikur_2025 <- function(daglegt, verd_dalkur, series, skra) {
  vikur <- daglegt |>
    filter(date >= as_date("2025-01-01"), date <= today()) |>
    mutate(vika = floor_date(date, "week", week_start = 1)) |>
    group_by(vika) |>
    summarise(fra = min(date), til = max(date), dagar = n(),
              innlent_isk_l = mean(.data[[verd_dalkur]]),
              markadsverd_usd_l = mean(world_usd_per_liter),
              markadsverd_isk_l = mean(world_isk_per_liter),
              usd_isk = mean(usd_exchange_rate),
              nyjasta_markadsverd = max(markadsverd_dags),
              nyjasta_gengi = max(gengi_dags), .groups = "drop")
  skyringar <- tibble(
    atridi = c("Heimild", "Innlent verð", "Markaðsverð", "Vikur", "Eyður", "Umreikningur"),
    lysing = c(paste0("https://fred.stlouisfed.org/series/", series),
      "Óvegið meðaltal lægsta verðs hvers fyrirtækis, án afsláttar; verð >150 kr./l.",
      "Staðverð í New York Harbor frá EIA/FRED; ekki innkaupsverð til Íslands.",
      "Mánudagur–sunnudagur. Fyrsta og síðasta vika geta verið óheilar; fra/til sýna dagana sem eru teknir með.",
      "Síðasta þekkta verð og gengi framfyllt á almanaksdaga, líka helgar og nýjustu daga.",
      "USD/gallon deilt með 3.785411784; margfaldað með USD-gengi hvers dags fyrir vikumeðaltal."))
  writexl::write_xlsx(list(Vikumedaltol = vikur, Skyringar = skyringar), skra)
  invisible(vikur)
}
