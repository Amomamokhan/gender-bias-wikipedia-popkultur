# ==============================================================================
# PROJEKT: Gender Bias - Popularitäts-Check (Skript 03)
# Ziel: Messen, wie oft die Artikel tatsächlich angeklickt werden.
# ==============================================================================

# BLOCK 0: Die Werkzeuge
# Wir brauchen httr und jsonlite, um die "Sprache des Internets" zu verstehen,
# und die üblichen Verdächtigen (dplyr, ggplot2) für Tabellen und Grafik.
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)

# 1. Daten-Import: Das Gedächtnis des Projekts
# Wir laden die Basistabelle aus Skript 1. Ohne die Namen der Personen 
# wüssten wir nicht, für wen wir die Klickzahlen abfragen sollen.
if (file.exists("data/raw_gender_bias_data.csv")) {
  raw_data <- read.csv("data/raw_gender_bias_data.csv")
  cat("Daten geladen. Starte Popularitäts-Check...\n")
} else {
  stop("FEHLER: Basisdaten nicht gefunden!")
}

# 2. Die "Abfrage-Maschine": Kontakt zur Wikipedia aufnehmen
# Das ist eine Funktion – ein kleiner Roboter, dem wir einen Namen geben 
# und der uns die durchschnittlichen Klicks der letzten 30 Tage zurückgibt.
get_views_manual <- function(article_title) {
  
  # Wir definieren den Zeitraum: Gestern minus 30 Tage.
  end_date <- format(Sys.Date() - 1, "%Y%m%d")
  start_date <- format(Sys.Date() - 30, "%Y%m%d")
  
  # URL-Vorbereitung: Wir bauen den Internet-Link zusammen.
  # 'user' im Link sorgt dafür, dass nur echte Menschen gezählt werden, keine Bots.
  encoded_title <- URLencode(article_title, reserved = TRUE)
  url <- paste0("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
                "de.wikipedia/all-access/user/", encoded_title, "/daily/", 
                start_date, "/", end_date)
  
  # Die eigentliche Anfrage: Wir klopfen bei der Wikipedia-API an.
  res <- tryCatch(GET(url, add_headers(`User-Agent` = "GenderBiasStudy/1.0")), 
                  error = function(e) return(NULL))
  
  # Wenn die Wikipedia antwortet (Code 200), rechnen wir den Durchschnitt aus.
  if (!is.null(res) && status_code(res) == 200) {
    data <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
    return(mean(data$items$views, na.rm = TRUE))
  } else {
    return(NA) # Falls der Artikel gelöscht wurde oder die API zickt.
  }
}

# 3. Der Loop: Die Fleißarbeit
# Das Skript geht nun jeden deiner ~1.800 Artikel nacheinander durch.
# Damit das Programm nicht abstürzt, macht es nach jedem Artikel eine winzige Pause.
cat("Hole Daten (dies kann einige Minuten dauern) ")
popularity_list <- list()

for(i in 1:nrow(raw_data)) {
  views <- get_views_manual(raw_data$title[i])
  
  # Wir speichern das Ergebnis (Rolle, Geschlecht, Titel und Klicks) in einer neuen Liste.
  popularity_list[[i]] <- data.frame(
    role = raw_data$role[i],
    geschlecht = raw_data$geschlecht[i],
    title = raw_data$title[i],
    avg_daily_views = views
  )
  
  # Alle 50 Artikel schreiben wir einen Strich "|" in die Konsole.
  # So weißt du, dass das Programm noch arbeitet und nicht "eingefroren" ist.
  if(i %% 50 == 0) cat("|") 
  Sys.sleep(0.05) # Kurze Verschnaufpause für die Internetleitung
}

# 4. Statistik: Den Berg an Daten zusammenfassen
# Aus der riesigen Liste machen wir eine handliche Tabelle mit Durchschnittswerten.
popularity_df <- bind_rows(popularity_list)

popularity_summary <- popularity_df %>%
  group_by(role, geschlecht) %>%
  summarise(
    n = n(),
    views_mean = round(mean(avg_daily_views, na.rm = TRUE), 2),
    .groups = "drop"
  )

# 5. Export: Die Ergebnisse sichern
# Wir speichern die nackten Zahlen als CSV-Datei, falls du sie später in Excel brauchst.
write.csv(popularity_summary, "data/popularity_stats_manual.csv", row.names = FALSE)

# 6. Grafik: Das Ergebnis sichtbar machen
# Wir zeichnen ein Balkendiagramm, das die Klicks pro Tag vergleicht.
plot_pop <- ggplot(popularity_summary, aes(x = role, y = views_mean, fill = geschlecht)) +
  geom_col(position = "dodge") + # Balken nebeneinander statt übereinander
  
  # Wir schreiben die exakten Zahlen oben auf die Balken drauf.
  geom_text(aes(label = round(views_mean)), 
            position = position_dodge(0.9), 
            vjust = -0.5, 
            size = 3) +
  
  # Unsere Standardfarben: Rot für Frauen, Blau für Männer.
  scale_fill_manual(values = c("Weiblich" = "#E05A5A", "Männlich" = "#4A90D9")) +
  
  labs(title = "Ø Tägliche Aufrufe (letzte 30 Tage)",
       subtitle = "Quelle: Wikimedia REST API (Nur echte Seitenaufrufe durch Nutzer)",
       y = "Durchschnittliche Aufrufe pro Tag", x = "Berufsrolle") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# Das fertige Bild speichern
ggsave("visualisierung_popularitaet_manual.png", plot_pop, width = 12, height = 7)

cat("\nFERTIG! Das Diagramm liegt jetzt in deinem Ordner.")