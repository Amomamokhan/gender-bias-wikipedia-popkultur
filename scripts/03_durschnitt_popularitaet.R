# ==============================================================================
# PROJEKT: Gender Bias - Popularitäts-Check (Skript 03 - NO-PACKAGE VERSION)
# Ziel: Durchschnittliche tägliche Aufrufe via Wikimedia REST API
# Stand: 12.05.2026
# ==============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)

# 1. Daten aus Skript 01 laden
if (file.exists("data/raw_gender_bias_data.csv")) {
  raw_data <- read.csv("data/raw_gender_bias_data.csv")
  cat("Daten geladen. Starte Popularitäts-Check für", nrow(raw_data), "Artikel...\n")
} else {
  stop("FEHLER: 'data/raw_gender_bias_data.csv' nicht gefunden!")
}

# 2. Funktion: Pageviews direkt von der Wikimedia API abrufen
get_views_manual <- function(article_title) {
  # Zeitraum: Letzte 30 Tage (Format: YYYYMMDD)
  end_date <- format(Sys.Date() - 1, "%Y%m%d")
  start_date <- format(Sys.Date() - 30, "%Y%m%d")
  
  # URL für die Wikimedia REST API (de.wikipedia)
  # Wichtig: 'user' filtert Bots aus!
  encoded_title <- URLencode(article_title, reserved = TRUE)
  url <- paste0("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
                "de.wikipedia/all-access/user/", encoded_title, "/daily/", 
                start_date, "/", end_date)
  
  res <- tryCatch(GET(url, add_headers(`User-Agent` = "GenderBiasStudy/1.0 (contact: your-email@example.com)")), 
                  error = function(e) return(NULL))
  
  if (!is.null(res) && status_code(res) == 200) {
    data <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
    return(mean(data$items$views, na.rm = TRUE))
  } else {
    return(NA) # Falls Artikel nicht gefunden oder API-Fehler
  }
}

# 3. Abruf-Loop (mit Fortschrittsanzeige)
cat("Hole Daten (dies kann einige Minuten dauern) ")
popularity_list <- list()

# Wir gehen die Zeilen einzeln durch
for(i in 1:nrow(raw_data)) {
  views <- get_views_manual(raw_data$title[i])
  popularity_list[[i]] <- data.frame(
    role = raw_data$role[i],
    geschlecht = raw_data$geschlecht[i],
    title = raw_data$title[i],
    avg_daily_views = views
  )
  if(i %% 50 == 0) cat("|") # Alle 50 Artikel ein Fortschrittsbalken
  Sys.sleep(0.05) # Kurze Pause, um die API nicht zu stressen
}

# 4. Zusammenführen und Statistiken berechnen
popularity_df <- bind_rows(popularity_list)

popularity_summary <- popularity_df %>%
  group_by(role, geschlecht) %>%
  summarise(
    n = n(),
    views_mean = round(mean(avg_daily_views, na.rm = TRUE), 2),
    .groups = "drop"
  )

# 5. Speichern
write.csv(popularity_summary, "data/popularity_stats_manual.csv", row.names = FALSE)

# 6. Visualisierung
plot_pop <- ggplot(popularity_summary, aes(x = role, y = views_mean, fill = geschlecht)) +
  geom_col(position = "dodge") +
  # Labels mit den exakten Werten hinzufügen
  geom_text(aes(label = round(views_mean)), position = position_dodge(0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Weiblich" = "#E05A5A", "Männlich" = "#4A90D9")) +
  labs(title = "Ø Tägliche Aufrufe (letzte 30 Tage)",
       subtitle = "Quelle: Wikimedia REST API (User-only views)",
       y = "Durchschnittliche Aufrufe pro Tag", x = "Berufsrolle") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

ggsave("visualisierung_popularitaet_manual.png", plot_pop, width = 12, height = 7)

cat("\nFERTIG! Ergebnisse in 'visualisierung_popularitaet_manual.png' gespeichert.\n")