# ==============================================================================
# PROJEKT: Gender Bias - Top 100 Repräsentations-Analyse (LOKALE VERSION)
# Methode: Nutzt vorhandene CSV-Daten statt neuer Wikidata-Abfragen
# Stand: 05.05.2026
# ==============================================================================

library(dplyr)
library(ggplot2)
library(scales)

# 1. Daten aus Skript 1 laden
if (file.exists("data/raw_gender_bias_data.csv")) {
  raw_data <- read.csv("data/raw_gender_bias_data.csv")
  cat("Daten erfolgreich geladen. Verarbeite", nrow(raw_data), "Einträge...\n")
} else {
  stop("FEHLER: Die Datei 'data/raw_gender_bias_data.csv' wurde nicht gefunden!")
}

# 2. Top 100 pro Kategorie aus dem lokalen Pool ermitteln
top100_lokal <- raw_data %>%
  group_by(role) %>%
  # Sortiere alle (Männer & Frauen gemischt) nach globaler Relevanz (Sitelinks)
  arrange(desc(sitelinks)) %>%
  # Nimm die obersten 100 Plätze
  slice_head(n = 100) %>%
  # Zähle, wie viele Männer und Frauen in diesen Top 100 gelandet sind
  count(role, geschlecht) %>%
  # Prozente berechnen
  mutate(prozent = n / sum(n)) %>%
  ungroup()

# 3. Speichern der Ergebnisse
write.csv(top100_lokal, "data/top100_gender_distribution_lokal.csv", row.names = FALSE)

# 4. Visualisierung (Die 100% Stacked Bar Chart)
plot_top100 <- ggplot(top100_lokal, aes(x = role, y = n, fill = geschlecht)) +
  geom_col(position = "fill", width = 0.7) +
  # Exakte Prozent-Labels einfügen
  geom_text(aes(label = paste0(round(prozent * 100), "%")), 
            position = position_fill(vjust = 0.5), 
            color = "white", 
            fontface = "bold", 
            size = 4.5) +
  # Die 50% Paritäts-Linie
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "white", linewidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Weiblich" = "#E05A5A", "Männlich" = "#4A90D9")) +
  labs(title = "Gender Distribution in den Top 100 (Lokale Analyse)",
       subtitle = "Basierend auf den 100 relevantesten Personen pro Kategorie aus dem Gesamtdatensatz",
       y = "Anteil in Prozent", x = "Berufsrolle",
       fill = "Geschlecht") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 11),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"))

# Grafik speichern
ggsave("visualisierung_top100_LOKAL_FINAL.png", plot_top100, width = 12, height = 7)

cat("\nERFOLG! Alle 6 Rollen wurden verarbeitet.\n")
cat("Die Datei 'visualisierung_top100_LOKAL_FINAL.png' wurde erstellt.")