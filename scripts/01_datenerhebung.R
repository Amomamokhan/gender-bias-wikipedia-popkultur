# ==============================================================================
# PROJEKT: Gender Bias in der Wikipedia (V12 - Encoding & List Fix)
# Stand: 02.05.2026 | Methode: Stabiles JSON-Parsing & UTF-8
# ==============================================================================

library(WikidataQueryServiceR)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(purrr)

# 1. Rollendefinition
roles <- list(
  list(id = "Q33999",   label = "Schauspieler_in"),
  list(id = "Q177220",  label = "Saenger_in"),
  list(id = "Q36180",   label = "Autor_in"),
  list(id = "Q2526255", label = "Regisseur_in"),
  list(id = "Q3455803", label = "Kameramann_frau"),
  list(id = "Q183945",  label = "Musikproduzent_in")
)

# 2. Wikidata Funktion: IDs holen
get_top_qids <- function(role_id, gender_id) {
  query <- paste0('
    SELECT ?person ?sitelinks WHERE {
      ?person wdt:P106 wd:', role_id, ' .
      ?person wdt:P21 wd:', gender_id, ' .
      ?person wikibase:sitelinks ?sitelinks .
    } ORDER BY DESC(?sitelinks) LIMIT 500')
  
  res <- tryCatch(query_wikidata(query), error = function(e) return(NULL))
  if(!is.null(res) && is.data.frame(res)) {
    res$qid <- gsub("http://www.wikidata.org/entity/", "", res$person)
    return(res)
  }
  return(NULL)
}

# 3. Wikipedia Funktion: Jetzt mit sicherem JSON-Check & Encoding
get_de_info_stable <- function(qid) {
  # Wikidata-Titel holen
  wd_url <- paste0("https://www.wikidata.org/w/api.php?action=wbgetentities&ids=", 
                   qid, "&props=sitelinks/urls&sitefilter=dewiki&format=json")
  
  wd_res <- tryCatch(GET(wd_url, timeout(8)), error = function(e) return(NULL))
  if (is.null(wd_res) || status_code(wd_res) != 200) return(NULL)
  
  # Kodierung erzwingen und als Text parsen
  wd_text <- content(wd_res, as = "text", encoding = "UTF-8")
  wd_data <- fromJSON(wd_text, simplifyVector = FALSE)
  
  # Prüfen, ob wir eine gültige Entität bekommen haben
  if (is.null(wd_data$entities[[qid]]$sitelinks$dewiki$title)) return(NULL)
  title <- wd_data$entities[[qid]]$sitelinks$dewiki$title
  
  # Wikipedia-Größe holen
  wp_url <- paste0("https://de.wikipedia.org/w/api.php?action=query&prop=info&inprop=size&titles=", 
                   URLencode(title), "&format=json")
  wp_res <- tryCatch(GET(wp_url, timeout(8)), error = function(e) return(NULL))
  if (is.null(wp_res) || status_code(wp_res) != 200) return(NULL)
  
  wp_text <- content(wp_res, as = "text", encoding = "UTF-8")
  wp_data <- fromJSON(wp_text, simplifyVector = FALSE)
  
  # Sicherer Zugriff auf die "pages"
  pages <- wp_data$query$pages
  if (is.null(pages) || length(pages) == 0) return(NULL)
  
  page <- pages[[1]]
  return(list(title = title, size = page$length))
}

# 4. Haupt-Loop (Unverändert stabil)
all_results <- list()

for(r in roles) {
  cat("\n--- Berufsrolle:", r$label, "---\n")
  
  for(g in list(list(id="Q6581072", n="Weiblich"), list(id="Q6581097", n="Männlich"))) {
    cat("  Hole IDs für", g$n, "...")
    qids_df <- get_top_qids(r$id, g$id)
    
    if(is.null(qids_df)) {
      cat(" Fehler bei Wikidata.\n")
      next
    }
    
    cat(" OK. Suche DE-Artikel (max. 150 Treffer)...\n  Progress: ")
    
    found_count <- 0
    temp_list <- list()
    
    for(i in 1:nrow(qids_df)) {
      info <- get_de_info_stable(qids_df$qid[i])
      
      if(!is.null(info) && !is.na(info$size)) {
        temp_list[[length(temp_list)+1]] <- data.frame(
          role = r$label,
          geschlecht = g$n,
          title = info$title,
          size_bytes = info$size,
          sitelinks = qids_df$sitelinks[i]
        )
        found_count <- found_count + 1
        cat("+") 
      } else {
        cat("-") 
      }
      
      if(found_count >= 150) break
      Sys.sleep(0.1) 
    }
    
    all_results[[paste0(r$label, g$n)]] <- bind_rows(temp_list)
    cat("\n  Fertig:", found_count, "Artikel gefunden.\n")
    saveRDS(all_results, "data/temp_backup.rds")
  }
}

# 5. Finale Auswertung & Plot
if(length(all_results) > 0) {
  final_data <- bind_rows(all_results) %>% mutate(size_kb = size_bytes / 1024)
  write.csv(final_data, "data/raw_gender_bias_data.csv", row.names = FALSE)
  
  statistik <- final_data %>%
    group_by(role, geschlecht) %>%
    summarise(n = n(), Durchschnitt_KB = round(mean(size_kb, na.rm = TRUE), 2), .groups = "drop")
  
  write.csv(statistik, "data/gender_bias_statistik.csv", row.names = FALSE)
  
  ggplot(statistik, aes(x = role, y = Durchschnitt_KB, fill = geschlecht)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = paste0("n=", n)), position = position_dodge(0.9), vjust = -0.5, size = 3) +
    scale_fill_manual(values = c("Weiblich" = "#E05A5A", "Männlich" = "#4A90D9")) +
    labs(title = "Gender Bias: Artikelgröße in der DE-Wikipedia",
         subtitle = paste("Stand:", Sys.Date()),
         y = "Durchschnittliche Größe (KB)", x = "Rolle") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
  
  ggsave("visualisierung_bias_final.png", width = 12, height = 7)
  cat("\nANALYSE ERFOLGREICH! Datei gespeichert.\n")
}