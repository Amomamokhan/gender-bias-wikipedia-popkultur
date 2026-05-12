# ==============================================================================
# PROJEKT: Gender Bias in der Wikipedia (V12 - Finale Version)
# ==============================================================================

# BLOCK 0: Die Werkzeugkiste
# Hier laden wir externe Pakete (Zusatzfunktionen). Ohne diese könnte R 
# weder mit dem Internet kommunizieren noch schöne Grafiken zeichnen.
library(WikidataQueryServiceR) # Für die Verbindung zur Wikidata-Datenbank
library(httr)                  # Für Web-Abfragen (wie ein Browser)
library(jsonlite)              # Um Datenformate (JSON) für R lesbar zu machen
library(dplyr)                 # Zum Sortieren und Filtern von Tabellen
library(ggplot2)               # Das Profi-Werkzeug für die Grafiken
library(purrr)                 # Für fortgeschrittene Listen-Operationen

# 1. Rollendefinition: Wer wird untersucht?
# Wir legen hier fest, welche Berufe wir vergleichen wollen. Jede Rolle hat 
# eine eindeutige "Q-Nummer" (die ID bei Wikidata), damit es keine Verwechslungen gibt.
roles <- list(
  list(id = "Q33999",   label = "Schauspieler_in"),
  list(id = "Q177220",  label = "Saenger_in"),
  list(id = "Q36180",   label = "Autor_in"),
  list(id = "Q2526255", label = "Regisseur_in"),
  list(id = "Q3455803", label = "Kameramann_frau"),
  list(id = "Q183945",  label = "Musikproduzent_in")
)

# 2. Wikidata Funktion: Die Suchmaschine
# Diese Funktion geht zu Wikidata und sagt: "Gib mir eine Liste von bis zu 500 Personen,
# die Beruf X haben, Geschlecht Y sind und sortiere sie nach ihrer Bekanntheit (Sitelinks)."
get_top_qids <- function(role_id, gender_id) {
  query <- paste0('
    SELECT ?person ?sitelinks WHERE {
      ?person wdt:P106 wd:', role_id, ' .
      ?person wdt:P21 wd:', gender_id, ' .
      ?person wikibase:sitelinks ?sitelinks .
    } ORDER BY DESC(?sitelinks) LIMIT 500')
  
  # tryCatch fängt Fehler ab, falls das Internet mal kurz weg ist
  res <- tryCatch(query_wikidata(query), error = function(e) return(NULL))
  if(!is.null(res) && is.data.frame(res)) {
    # Wir säubern die IDs, damit nur noch der Code (z.B. Q123) übrig bleibt
    res$qid <- gsub("http://www.wikidata.org/entity/", "", res$person)
    return(res)
  }
  return(NULL)
}

# 3. Wikipedia Funktion: Die Messstation
# Sobald wir eine Person gefunden haben, müssen wir wissen: Wie lang ist ihr Artikel?
# Diese Funktion holt erst den Namen und checkt dann in der Wikipedia die "Größe" in Bytes.
get_de_info_stable <- function(qid) {
  # Schritt A: Den exakten Namen des deutschen Wikipedia-Artikels finden
  wd_url <- paste0("https://www.wikidata.org/w/api.php?action=wbgetentities&ids=", 
                   qid, "&props=sitelinks/urls&sitefilter=dewiki&format=json")
  
  wd_res <- tryCatch(GET(wd_url, timeout(8)), error = function(e) return(NULL))
  if (is.null(wd_res) || status_code(wd_res) != 200) return(NULL)
  
  # UTF-8 sorgt dafür, dass Umlaute (ä, ö, ü) nicht kaputt gehen
  wd_text <- content(wd_res, as = "text", encoding = "UTF-8")
  wd_data <- fromJSON(wd_text, simplifyVector = FALSE)
  
  if (is.null(wd_data$entities[[qid]]$sitelinks$dewiki$title)) return(NULL)
  title <- wd_data$entities[[qid]]$sitelinks$dewiki$title
  
  # Schritt B: Die tatsächliche Artikelgröße in der Wikipedia abfragen
  wp_url <- paste0("https://de.wikipedia.org/w/api.php?action=query&prop=info&inprop=size&titles=", 
                   URLencode(title), "&format=json")
  wp_res <- tryCatch(GET(wp_url, timeout(8)), error = function(e) return(NULL))
  if (is.null(wp_res) || status_code(wp_res) != 200) return(NULL)
  
  wp_text <- content(wp_res, as = "text", encoding = "UTF-8")
  wp_data <- fromJSON(wp_text, simplifyVector = FALSE)
  
  # Wir nehmen die Informationen der ersten gefundenen Seite (pages[[1]])
  pages <- wp_data$query$pages
  if (is.null(pages) || length(pages) == 0) return(NULL)
  
  page <- pages[[1]]
  # Wir geben den Titel und die Länge (in Bytes) zurück
  return(list(title = title, size = page$length))
}

# 4. Haupt-Loop: Die Fließbandarbeit
# Hier passiert die eigentliche Arbeit. Das Skript geht jede Berufsrolle durch,
# holt die Männer und Frauen, misst die Artikel und speichert alles in einer Liste.
all_results <- list()

for(r in roles) {
  cat("\n--- Berufsrolle:", r$label, "---\n")
  
  for(g in list(list(id="Q6581072", n="Weiblich"), list(id="Q6581097", n="Männlich"))) {
    cat("  Hole IDs für", g$n, "...")
    qids_df <- get_top_qids(r$id, g$id)
    
    if(is.null(qids_df)) {
      cat(" Fehler bei Wikidata.\n")
      next # Falls ein Fehler passiert, springe zur nächsten Gruppe
    }
    
    cat(" OK. Suche DE-Artikel (max. 150 Treffer)...\n  Progress: ")
    
    found_count <- 0
    temp_list <- list()
    
    # Wir klappern die Liste ab, bis wir 150 Artikel für diese Kategorie gefunden haben
    for(i in 1:nrow(qids_df)) {
      info <- get_de_info_stable(qids_df$qid[i])
      
      if(!is.null(info) && !is.na(info$size)) {
        # Wenn ein Artikel existiert, speichern wir Name, Größe und Sitelinks
        temp_list[[length(temp_list)+1]] <- data.frame(
          role = r$label,
          geschlecht = g$n,
          title = info$title,
          size_bytes = info$size,
          sitelinks = qids_df$sitelinks[i]
        )
        found_count <- found_count + 1
        cat("+") # Signal für Erfolg
      } else {
        cat("-") # Signal: Kein deutscher Artikel vorhanden
      }
      
      if(found_count >= 150) break # Stop bei 150 Treffern
      Sys.sleep(0.1) # Kurze Pause, um den Server nicht zu überlasten
    }
    
    # Zusammenfügen der Ergebnisse dieser Gruppe
    all_results[[paste0(r$label, g$n)]] <- bind_rows(temp_list)
    cat("\n  Fertig:", found_count, "Artikel gefunden.\n")
    # Zwischen-Backup speichern, falls der Strom ausfällt
    saveRDS(all_results, "data/temp_backup.rds")
  }
}

# 5. Finale Auswertung & Plot: Die Goldgrube
# Hier werden aus den Rohdaten (Bytes) lesbare Werte (Kilobytes) gemacht
# und der Durchschnitt pro Gruppe berechnet.
if(length(all_results) > 0) {
  # Alle Einzeltabellen zu einer großen Gesamttabelle zusammenkleben
  final_data <- bind_rows(all_results) %>% mutate(size_kb = size_bytes / 1024)
  # Die Rohdaten als CSV speichern
  write.csv(final_data, "data/raw_gender_bias_data.csv", row.names = FALSE)
  
  # Statistik-Tabelle erstellen: Durchschnittliche Größe berechnen
  statistik <- final_data %>%
    group_by(role, geschlecht) %>%
    summarise(n = n(), Durchschnitt_KB = round(mean(size_kb, na.rm = TRUE), 2), .groups = "drop")
  
  # Die fertige Statistik-Tabelle speichern
  write.csv(statistik, "data/gender_bias_statistik.csv", row.names = FALSE)
  
  # Die Grafik zeichnen (Balkendiagramm)
  ggplot(statistik, aes(x = role, y = Durchschnitt_KB, fill = geschlecht)) +
    geom_col(position = "dodge") + # Nebeneinander stehende Balken
    geom_text(aes(label = paste0("n=", n)), position = position_dodge(0.9), vjust = -0.5, size = 3) +
    scale_fill_manual(values = c("Weiblich" = "#E05A5A", "Männlich" = "#4A90D9")) +
    labs(title = "Gender Bias: Artikelgröße in der DE-Wikipedia",
         subtitle = paste("Stand:", Sys.Date()),
         y = "Durchschnittliche Größe (KB)", x = "Rolle") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1)) # Text schräg stellen für bessere Lesbarkeit
  
  # Das Bild als PNG-Datei sichern
  ggsave("visualisierung_bias_final.png", width = 12, height = 7)
  cat("\nANALYSE ERFOLGREICH! Datei gespeichert.\n")
}