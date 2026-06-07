# SCRAPER POSTMORTEMÓW - danluu/post-mortems
# Wymagane pakiety:
install.packages(c("httr", "rvest", "stringr", "dplyr", "xml2"))

library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(xml2)

# KONFIGURACJA ----
OUTPUT_DIR     <- "dataset"          # folder docelowy
MIN_WORDS      <- 100                # minimalna liczba słów żeby zapisać plik
MAX_ARTICLES   <- 80                 # ile artykułów próbować pobrać
SLEEP_BETWEEN  <- 1.5                # przerwa między requestami (sekundy)

dir.create(OUTPUT_DIR, showWarnings = FALSE)

# PobierANIE README z listą linków ----
cat("Pobieram listę linków z danluu/post-mortems...\n")

readme_url <- "https://raw.githubusercontent.com/danluu/post-mortems/master/README.md"

response <- tryCatch(
  GET(readme_url, timeout(30)),
  error = function(e) { cat("BŁĄD: Nie można pobrać README:", e$message, "\n"); NULL }
)

if (is.null(response) || status_code(response) != 200) {
  stop("Nie udało się pobrać README. Sprawdź połączenie.")
}

readme_text <- content(response, as = "text", encoding = "UTF-8")

# Wyciąganie linków z markdown ----
links_raw <- str_extract_all(readme_text, "\\[([^\\]]+)\\]\\((https?://[^)]+)\\)")[[1]]

companies <- str_match(links_raw, "\\[([^\\]]+)\\]\\((https?://[^)]+)\\)")
companies_df <- data.frame(
  name = companies[, 2],
  url  = companies[, 3],
  stringsAsFactors = FALSE
)

# Usuwanie linków do do githuba, wikipedii itp.
exclude_patterns <- c("github\\.com/danluu", "wikipedia\\.org", "twitter\\.com",
                      "youtube\\.com", "amazon\\.com/dp", "\\.pdf$")
exclude_regex <- paste(exclude_patterns, collapse = "|")

companies_df <- companies_df %>%
  filter(!str_detect(url, exclude_regex)) %>%
  filter(str_length(name) > 2) %>%
  distinct(url, .keep_all = TRUE)

cat(sprintf("Znaleziono %d unikalnych linków.\n", nrow(companies_df)))

# Ogranicza liczbę artykułów jeśli ustawiono
if (!is.null(MAX_ARTICLES)) {
  companies_df <- head(companies_df, MAX_ARTICLES)
  cat(sprintf("Ograniczono do %d artykułów (MAX_ARTICLES).\n", MAX_ARTICLES))
}

# Funkcja scrapująca tekst ze strony ----
scrape_article <- function(url) {
  response <- tryCatch(
    GET(url, timeout(15), add_headers(`User-Agent` = "Mozilla/5.0 (research bot)")),
    error = function(e) NULL
  )
  
  if (is.null(response)) return(NULL)
  if (status_code(response) != 200) return(NULL)
  
  # Sprawdzanie czy to HTML
  content_type <- headers(response)$`content-type`
  if (!is.null(content_type) && str_detect(content_type, "pdf|image|video")) return(NULL)
  
  page <- tryCatch(
    read_html(content(response, as = "text", encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(page)) return(NULL)
  
  # Usuwanie nawigacji, stopki, skryptów
  page %>%
    html_nodes("script, style, nav, footer, header, .sidebar, .menu") %>%
    xml2::xml_remove()
  
  # Wyciągnij tekst z głównej treści
  text <- page %>% html_nodes("article") %>% html_text(trim = TRUE) %>% paste(collapse = " ")
  
  if (str_length(text) < 200) {
    text <- page %>% html_nodes("main") %>% html_text(trim = TRUE) %>% paste(collapse = " ")
  }
  
  if (str_length(text) < 200) {
    text <- page %>% html_nodes("body") %>% html_text(trim = TRUE) %>% paste(collapse = " ")
  }
  
  # Wyczyść whitespace
  text <- str_replace_all(text, "\\s+", " ") %>% str_trim()
  
  return(text)
}

# Generuj bezpieczną nazwę pliku ----
safe_filename <- function(name, url, index) {
  # Próbuj użyć nazwy firmy
  clean_name <- name %>%
    str_replace_all("[^a-zA-Z0-9 ]", "") %>%
    str_trim() %>%
    str_replace_all(" +", "_") %>%
    str_to_lower()
  
  # Jeśli nazwa za krótka - użyj domeny
  if (str_length(clean_name) < 3) {
    clean_name <- str_extract(url, "(?<=://)([^/]+)") %>%
      str_replace_all("\\.", "_")
  }
  
  sprintf("%03d_%s.txt", index, clean_name)
}

# Główna pętla scrapowania ----
cat("\nRozpoczynamy scraping...\n")
cat(rep("-", 50), "\n", sep = "")

success_count <- 0
fail_count    <- 0
log_entries   <- list()

for (i in seq_len(nrow(companies_df))) {
  name <- companies_df$name[i]
  url  <- companies_df$url[i]
  
  cat(sprintf("[%d/%d] %s\n", i, nrow(companies_df), name))
  
  text <- scrape_article(url)
  
  if (is.null(text) || str_length(text) < 10) {
    cat("  -> POMINIĘTO (brak treści lub błąd)\n")
    fail_count <- fail_count + 1
    log_entries[[i]] <- data.frame(name = name, url = url, status = "failed", words = 0)
    Sys.sleep(SLEEP_BETWEEN)
    next
  }
  
  word_count <- str_count(text, "\\S+")
  
  if (word_count < MIN_WORDS) {
    cat(sprintf("  -> POMINIĘTO (za mało słów: %d < %d)\n", word_count, MIN_WORDS))
    fail_count <- fail_count + 1
    log_entries[[i]] <- data.frame(name = name, url = url, status = "too_short", words = word_count)
    Sys.sleep(SLEEP_BETWEEN)
    next
  }
  
  filename <- safe_filename(name, url, i)
  filepath <- file.path(OUTPUT_DIR, filename)
  
  writeLines(text, filepath, useBytes = FALSE)
  cat(sprintf("  -> OK  (%d słów) -> %s\n", word_count, filename))
  
  success_count <- success_count + 1
  log_entries[[i]] <- data.frame(name = name, url = url, status = "ok", words = word_count)
  
  Sys.sleep(SLEEP_BETWEEN)
}

# Podsumowanie ----
cat("\n", rep("=", 50), "\n", sep = "")
cat(sprintf("GOTOWE!\n"))
cat(sprintf("  Zapisano:  %d plików w '%s/'\n", success_count, OUTPUT_DIR))
cat(sprintf("  Pominięto: %d (błędy / za krótkie)\n", fail_count))

# Zapisz log do CSV
log_df <- bind_rows(log_entries)
write.csv(log_df, "scraping_log.csv", row.names = FALSE)
cat("  Log zapisany do: scraping_log.csv\n")

if (success_count < 10) {
  cat("\nUWAGA: Mało plików — część stron mogła zablokować scraping.\n")
  cat("scraping_log.csv pokazuje, które URLe zawiodły.\n")
}