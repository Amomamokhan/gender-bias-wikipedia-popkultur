# ==============================================================================
# PROJEKT: Gender Bias - Top 100 Repräsentations-Analyse (LOKALE VERSION)
# Ziel: Wer ist am sichtbarsten? (Die 100 wichtigsten Personen pro Branche)
# ==============================================================================

# BLOCK 0: Die Werkzeuge
# Wir nutzen dplyr für die Tabellen-Logik und ggplot2 für das Design.
library(dplyr)
library(ggplot2)
library(scales) # Hilft uns, Zahlen in schöne Prozente (z.B. 50%) umzuwandeln

# 1. Daten-Import: Die Basis laden
# Wir schauen nach, ob die Datei aus Skript 1 da ist. Falls nicht, bricht das 
# Programm mit einer Fehlermeldung ab, statt "Quatsch" zu berechnen.
if (file.exists("data/raw_gender_bias_data.csv")) {
  raw_data <- read.csv("data/raw_gender_bias_data.csv")
  cat("Daten erfolgreich geladen. Verarbeite", nrow(raw_data), "Einträge...\n")
} else {
  stop("FEHLER: Die Datei 'data/raw_gender_bias_data.csv' wurde nicht gefunden!")
}

# 2. Die "Top 100" Ermittlung: Den Filter anwenden
# Das ist das Herzstück der Analyse. Wir mischen alle Männer und Frauen einer
# Branche zusammen und schauen, wer die meisten Sitelinks (globale Bekanntheit) hat.
top100_lokal <- raw_data %>%
  # Schritt A: Wir gruppieren nach Berufsrolle (z.B. Autor_in)
  group_by(role) %>%
  
  # Schritt B: Wir sortieren JEDE Gruppe nach Sitelinks (absteigend)
  # Die "Wichtigsten" stehen jetzt ganz oben.
  arrange(desc(sitelinks)) %>%
  
  # Schritt C: Wir schneiden die Liste nach den ersten 100 Personen ab
  slice_head(n = 100) %>%
  
  # Schritt D: Wir zählen, wie viele Männer und Frauen jetzt noch übrig sind
  count(role, geschlecht) %>%
  
  # Schritt E: Wir berechnen den Anteil (z.B. 0.04 für 4%)
  mutate(prozent = n / sum(n)) %>%
  
  # Gruppierung aufheben, um wieder eine normale Tabelle zu haben
  ungroup()

# 3. Ergebnisse sichern
# Wir speichern diese neue Statistik-Tabelle als CSV-Datei ab.
write.csv(top100_lokal, "data/top100_gender_distribution_lokal.csv", row.names = FALSE)

# 4. Visualisierung: Das "Stapel-Diagramm" erstellen
# Wir bauen ein Diagramm, bei dem die Balken immer genau 100% hoch sind (position = "fill").
plot_top100 <- ggplot(top100_lokal, aes(x = role, y = n, fill = geschlecht)) +
  
  # Zeichnet die farbigen Balken (Rot für Frauen, Blau für Männer)
  geom_col(position = "fill", width = 0.7) +
  
  # Beschriftung: Schreibt die Prozentzahlen direkt IN die Balken
  # "vjust = 0.5" sorgt dafür, dass die Zahl genau mittig im Farbblock steht.
  geom_text(aes(label = paste0(round(prozent * 100), "%")), 
            position = position_fill(vjust = 0.5), 
            color = "white", 
            fontface = "bold", 
            size = 4.5) +
  
  # Die Paritäts-Linie: Eine gestrichelte weiße Linie bei 50%
  # Alles was unter oder über dieser Linie liegt, zeigt sofort den Bias.
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "white", linewidth = 1) +
  
  # Achsen-Design: Umwandlung der Skala in Prozente (0.25 -> 25%)
  scale_y_continuous(labels = scales::percent) +
  
  # Unsere Hausfarben (Rot und Blau)
  scale_fill_manual(values = c("Weiblich" = "#E05A5A", "Männlich" = "#4A90D9")) +
  
  # Beschriftungen (Titel, Untertitel und Achsen)
  labs(title = "Gender Distribution in den Top 100 (Lokale Analyse)",
       subtitle = "Basierend auf den 100 relevantesten Personen pro Kategorie",
       y = "Anteil in Prozent", x = "Berufsrolle",
       fill = "Geschlecht") +
  
  # Modernes, sauberes Design (kein grauer Hintergrund)
  theme_minimal() +
  
  # Feinschliff: Text auf der X-Achse schräg stellen, damit er nicht überlappt
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 11),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"))

# 5. Finale: Das Bild exportieren
# Wir speichern das Diagramm als große, scharfe PNG-Datei für dein Manuskript.
ggsave("visualisierung_top100_LOKAL_FINAL.png", plot_top100, width = 12, height = 7)

# Erfolgsmeldungen in der Konsole ausgeben
cat("\nERFOLG! Alle 6 Rollen wurden verarbeitet.\n")
cat("Die Datei 'visualisierung_top100_LOKAL_FINAL.png' wurde erstellt.")