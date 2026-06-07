#' ---
#' title: "Analiza tekstu – Raporty z awarii IT (Post-mortems)"
#' author: "Bartosz Kalinowski, Karol Jahn, Michał Płaza"
#' date:   " "
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable
#'     highlight: kate
#'     toc: true
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show
#'     number_sections: false
#' ---


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)

#' # Wymagane pakiety
# Wymagane pakiety ----
library(tm)
library(SnowballC)
library(cluster)
library(wordcloud)
library(factoextra)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(DT)
library(syuzhet)
library(tidyr)
library(stringr)
library(tidytext)  
library(plotly)  

#' # Dane tekstowe
# Dane tekstowe ----


docs   <- DirSource("dataset")
corpus <- VCorpus(docs)

#' # 1. Przetwarzanie i oczyszczanie tekstu
# 1. Przetwarzanie i oczyszczanie tekstu ----

# Zapewnienie kodowania UTF-8 w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "")))

# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

# Usuń zbędne znaki, pozostałości URL, HTML itp.
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "@\\w+")
corpus <- tm_map(corpus, toSpace, "\\|")
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")
corpus <- tm_map(corpus, toSpace, "http\\w*")
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€")

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

# Usunięcie słów mało wnoszących do analizy technicznej awarii
corpus <- tm_map(corpus, removeWords, c("can", "will", "just", "dont", "get", "like", 
                                        "one", "however", "also", "using", "can", "new"))
corpus <- tm_map(corpus, stripWhitespace)

#' # Stemming
# Stemming ----

# Zachowaj kopię korpusu do użycia jako słownik przy uzupełnianiu rdzeni
corpus_copy <- corpus

# Wykonaj stemming w korpusie
corpus_stemmed <- tm_map(corpus, stemDocument)

# Ręczne przywrócenie najważniejszych terminów technicznych, bo stem_complete zajmuje zbyt dużo czasu 
fixStems <- content_transformer(function(x) {
  x <- gsub("\\bdatabas\\b", "database", x)
  x <- gsub("\\bfailur\\b", "failure", x)
  x <- gsub("\\bnetwork\\b", "network", x)
  x <- gsub("\\berror\\b", "error", x)
  x <- gsub("\\brespons\\b", "response", x)
  x <- gsub("\\bconfigur\\b", "configuration", x)
  return(x)
})
corpus_stemmed <- tm_map(corpus_stemmed, fixStems)
corpus_stemmed <- tm_map(corpus_stemmed, stripWhitespace)

# Użyj stemmed corpus do dalszej analizy
corpus_processed <- corpus_stemmed

#' # Tokenizacja
# Tokenizacja ----

# Macierz TDM i DTM
tdm   <- TermDocumentMatrix(corpus_processed)
tdm_m <- as.matrix(tdm)
dtm   <- DocumentTermMatrix(corpus_processed)
dtm_m <- as.matrix(dtm)

#' # 2. Analiza częstości słów
# 2. Analiza częstości słów ----

# Zlicz częstości słów w całym korpusie
v      <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)

# Wyświetl top 10
print(head(tdm_df, 10))

#' ## Wykres: Top 20 najczęstszych słów
# Wykres: Top 20 najczęstszych słów ----
ggplot(head(tdm_df, 20), aes(x = reorder(word, freq), y = freq, fill = freq)) +
  geom_col(show.legend = FALSE, color = "white") +
  scale_fill_gradient(low = "#81C9E3", high = "#1A5F7A") +
  coord_flip() +
  labs(
    title    = "Top 20 najczęściej występujących słów",
    subtitle = "Raporty post-mortem po oczyszczeniu i stemmingu",
    x        = NULL,
    y        = "Liczba wystąpień"
  ) +
  theme_minimal(base_size = 14)

#' ## Chmura słów (globalny korpus)
# Chmura słów (globalny korpus) ----
set.seed(42)
wordcloud(words = tdm_df$word, freq = tdm_df$freq,
          min.freq = 2, max.words = 80,
          random.order = FALSE, rot.per = 0.2,
          colors = brewer.pal(9, "YlOrRd"))

#' ## Interaktywna tabela częstości
# Interaktywna tabela częstości ----
datatable(
  head(tdm_df, 50),
  caption  = "Top 50 słów w korpusie raportów awarii",
  rownames = FALSE,
  options  = list(pageLength = 10)
)

#' # 3. Klastrowanie k-średnich
# 3. Klastrowanie k-średnich ----

#' ## Przygotowanie macierzy TF-IDF z usunięciem rzadkich terminów
# Tworzymy macierz TF-IDF
dtm_tfidf <- DocumentTermMatrix(
  corpus_processed,
  control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE))
)

# Usuń rzadkie terminy (pojawiające się w mniej niż 5% dokumentów)
dtm_tfidf <- removeSparseTerms(dtm_tfidf, sparse = 0.95)
dtm_tfidf_m <- as.matrix(dtm_tfidf)
rownames(dtm_tfidf_m) <- names(corpus_processed)

# Sprawdź czy macierz nie jest pusta
if (ncol(dtm_tfidf_m) < 3) {
  stop("Zbyt mało terminów po usunięciu rzadkich. Zmniejsz parametr sparse.")
}

#' ## Dobór liczby klastrów
# Dobór optymalnej liczby klastrów
fviz_nbclust(dtm_tfidf_m, kmeans, method = "silhouette") +
  labs(
    title = "Dobór liczby klastrów (Przyczyny awarii)", 
    subtitle = "Metoda sylwetki na bazie macierzy TF-IDF"
  )

# Automatyczny dobór k na podstawie sylwetki
# Uruchomienie dla k od 2 do 8 i wybór najlepszego
sil_width <- sapply(2:8, function(k) {
  km <- kmeans(dtm_tfidf_m, centers = k, nstart = 10)
  ss <- silhouette(km$cluster, dist(dtm_tfidf_m))
  mean(ss[, 3])
})
optimal_k <- which.max(sil_width) + 1
cat("Optymalna liczba klastrów (wg sylwetki):", optimal_k, "\n")

# Uruchomienie K-means z optymalną liczbą klastrów
set.seed(123)
k_awarie <- min(optimal_k, 5)  # Maksymalnie 5 klastrów dla czytelności
klastrowanie <- kmeans(dtm_tfidf_m, centers = k_awarie, nstart = 25)

#' ## Wizualizacja klastrów
# Wizualizacja klastrów ----
fviz_cluster(
  list(data = dtm_tfidf_m, cluster = klastrowanie$cluster),
  geom = "point",
  main = paste("Podział raportów na", k_awarie, "główne przyczyny awarii (TF-IDF)")
)

#' ## Tabela przypisania dokumentów do klastrów
# Tabela przypisania dokumentów do klastrów ----
cluster_info <- lapply(1:k_awarie, function(i) {
  idx       <- which(klastrowanie$cluster == i)
  if (length(idx) > 0) {
    docs_cl   <- dtm_tfidf_m[idx, , drop = FALSE]
    # Sortujemy po najwyższej wadze TF-IDF w danym klastrze
    wf        <- sort(colSums(docs_cl), decreasing = TRUE)
    data.frame(
      Klaster           = i,
      Liczba_dokumentow = length(idx),
      Top_5_slow_przyczyn = paste(names(wf)[1:min(5, length(wf))], collapse = ", "),
      stringsAsFactors  = FALSE
    )
  } else {
    data.frame(
      Klaster = i, Liczba_dokumentow = 0, 
      Top_5_slow_przyczyn = "brak dokumentów", stringsAsFactors = FALSE
    )
  }
})
cluster_info_df <- do.call(rbind, cluster_info)

document_names            <- names(corpus_processed)
documents_clusters        <- data.frame(Dokument = document_names, Klaster = klastrowanie$cluster, stringsAsFactors = FALSE)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Czyszczenie nazwy dokumentu z ID
clean_doc_names <- stringr::str_replace(documents_clusters_z_info$Dokument, "^[0-9]+_", "")
documents_clusters_z_info$Firma <- stringr::str_replace(clean_doc_names, "\\.txt$", "")

datatable(
  documents_clusters_z_info %>% select(Dokument, Firma, Klaster, Liczba_dokumentow, Top_5_slow_przyczyn),
  caption  = "Raporty pogrupowane pod kątem przyczyny awarii (Słowa kluczowe TF-IDF)",
  rownames = FALSE,
  options  = list(pageLength = 10)
)

#' ## Chmury słów per klaster tematyczny
# Chmury słów per klaster tematyczny ----
par(mfrow = c(ceiling(k_awarie/2), 2), mar = c(2, 2, 3, 2))

for (i in 1:k_awarie) {
  idx    <- which(klastrowanie$cluster == i)
  if (length(idx) > 0) {
    cl_mat <- dtm_tfidf_m[idx, , drop = FALSE]
    wf     <- colSums(cl_mat)
    wf_sorted <- sort(wf, decreasing = TRUE)
    
    if (length(wf_sorted) > 0) {
      # Bez skalowania - wordcloud akceptuje wartości ułamkowe
      wordcloud(names(wf_sorted), freq = wf_sorted, max.words = 15, 
                random.order = FALSE, colors = brewer.pal(8, "Dark2"))
      title(paste("Charakterystyka awarii – Klaster", i))
    }
  }
}

# Resetuj parametry wykresu
par(mfrow = c(1, 1))

#' ## Wykres liczności klastrów awarii
# Wykres liczności klastrów awarii ----
ggplot(documents_clusters, aes(x = as.factor(Klaster), fill = as.factor(Klaster))) +
  geom_bar(width = 0.5, color = "white") +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Liczba awarii w podziale na typy przyczyny",
    subtitle = paste("Podział dokonany algorytmem K-means + TF-IDF (k =", k_awarie, ")"),
    x     = "Grupa (Klaster) awarii",
    y     = "Liczba raportów",
    fill  = "Klaster"
  ) +
  theme_minimal(base_size = 14)

#' # 4. Analiza sentymentu
# 4. Analiza sentymentu ----

# Surowy tekst do analizy emocjonalnej
raw_texts       <- sapply(corpus_copy, function(doc) as.character(doc$content))
doc_names_short <- gsub("\\.txt$", "", names(corpus_copy))

# Obliczamy macierz sentymentów NRC
sentiment_list <- lapply(raw_texts, function(text) colSums(get_nrc_sentiment(text)))
sentiment_df   <- as.data.frame(do.call(rbind, sentiment_list))
sentiment_df$dokument <- doc_names_short

# Wyciąganie czystej firmy bez numerów ID z przodu
sentiment_df$firma <- stringr::str_replace(doc_names_short, "^[0-9]+_", "")

emotions_cols <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")

#' ## Heatmapa emocji NRC (tylko top 30 dokumentów)
# Heatmapa emocji NRC ----

# Wybierz top 30 dokumentów z największą liczbą emocji
top_docs_emotion <- sentiment_df %>%
  mutate(total_emotion = rowSums(select(., all_of(emotions_cols)))) %>%
  arrange(desc(total_emotion)) %>%
  head(30) %>%
  pull(dokument)

sentiment_df %>%
  filter(dokument %in% top_docs_emotion) %>%
  select(dokument, all_of(emotions_cols)) %>%
  pivot_longer(cols = all_of(emotions_cols), names_to = "emocja", values_to = "wynik") %>%
  ggplot(aes(x = emocja, y = reorder(dokument, wynik), fill = wynik)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#B22222", name = "Wynik NRC") +
  labs(
    title    = "Heatmapa emocji w tekstach post-mortem",
    subtitle = "Top 30 raportów z największą liczbą słów nacechowanych emocjonalnie",
    x        = "Emocja",
    y        = "Dokument"
  ) +
  theme_minimal(base_size = 10) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 20)
  )

#' ## Sentyment pozytywny vs. negatywny per raport (top 30)
# Sentyment pozytywny vs. negatywny per raport ----

# Wybierz top 30 dokumentów z największą liczbą słów sentymentowych
top_docs_sentiment <- sentiment_df %>%
  mutate(total = positive + negative) %>%
  arrange(desc(total)) %>%
  head(30) %>%
  pull(dokument)

sentiment_df %>%
  filter(dokument %in% top_docs_sentiment) %>%
  select(dokument, positive, negative, firma) %>%
  pivot_longer(cols = c("positive", "negative"), names_to = "typ", values_to = "wynik") %>%
  ggplot(aes(x = reorder(dokument, wynik), y = wynik, fill = typ)) +
  geom_col(position = "dodge", color = "white") +
  scale_fill_manual(values = c("positive" = "#2E8B57", "negative" = "#B22222")) +
  coord_flip() +
  labs(
    title = "Sentyment pozytywny vs. negatywny w raportach awarii",
    subtitle = "Top 30 raportów z największą liczbą słów nacechowanych",
    x     = "Dokument",
    y     = "Liczba słów nacechowanych",
    fill  = "Zabarwienie"
  ) +
  theme_minimal(base_size = 11)

#' ## Profil emocjonalny per firma (top 10 firm)
# Profil emocjonalny per firma ----

# Wybierz top 10 firm z największą liczbą emocji
top_firms <- sentiment_df %>%
  group_by(firma) %>%
  summarise(total_emotion = sum(across(all_of(emotions_cols))), .groups = "drop") %>%
  arrange(desc(total_emotion)) %>%
  head(10) %>%
  pull(firma)

sentiment_df %>%
  filter(firma %in% top_firms) %>%
  select(firma, all_of(emotions_cols)) %>%
  group_by(firma) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  pivot_longer(cols = all_of(emotions_cols), names_to = "emocja", values_to = "wynik") %>%
  ggplot(aes(x = emocja, y = wynik, fill = firma)) +
  geom_col(position = "dodge", color = "white") +
  labs(
    title    = "Profil emocjonalny wg firm / infrastruktur",
    subtitle = "Top 10 organizacji – suma słów nacechowanych emocjonalnie (NRC)",
    x        = "Emocja",
    y        = "Łączny wynik NRC",
    fill     = "Firma"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  )

#' ## Profil emocjonalny per firma
# Profil emocjonalny per firma ----
sentiment_df %>%
  filter(firma %in% top_firms) %>%
  select(firma, all_of(emotions_cols)) %>%
  group_by(firma) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  pivot_longer(cols = all_of(emotions_cols), names_to = "emocja", values_to = "wynik") %>%
  ggplot(aes(x = emocja, y = wynik, fill = emocja)) +
  geom_col(show.legend = FALSE, color = "white") +
  facet_wrap(~firma, scales = "free_y", ncol = 2) +
  labs(
    title    = "Profil emocjonalny per firma (widok szczegółowy)",
    subtitle = "Top 10 organizacji",
    x        = "Emocja",
    y        = "Łączny wynik NRC"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text = element_text(face = "bold", size = 9)
  )

#' ## Interaktywna tabela wyników sentymentu
# Interaktywna tabela wyników sentymentu ----
datatable(
  sentiment_df %>% select(dokument, firma, positive, negative, all_of(emotions_cols)),
  caption  = "Wyniki analizy sentymentu NRC dla każdego dokumentu post-mortem",
  rownames = FALSE,
  options  = list(pageLength = 10, scrollX = TRUE)
)

#' # 5. TF-IDF per firma
# 5. TF-IDF per firma ----

# Macierz DTM z wagami TF-IDF (bez rzadkich terminów dla czytelności)
dtm_tfidf_company <- DocumentTermMatrix(
  corpus_processed,
  control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE))
)

# Usuń bardzo rzadkie terminy dla lepszej czytelności
dtm_tfidf_company <- removeSparseTerms(dtm_tfidf_company, sparse = 0.97)
dtm_tfidf_company_m <- as.matrix(dtm_tfidf_company)
rownames(dtm_tfidf_company_m) <- gsub("\\.txt$", "", rownames(dtm_tfidf_company_m))

# Bezpieczne odcięcie ID z nazw wierszy macierzy TF-IDF
clean_rows <- stringr::str_replace(rownames(dtm_tfidf_company_m), "^[0-9]+_", "")
companies  <- clean_rows

# Agregacja TF-IDF per firma (średnia) i top słów
tfidf_company_df <- as.data.frame(dtm_tfidf_company_m)
tfidf_company_df$firma <- companies

company_top_df <- tfidf_company_df %>%
  group_by(firma) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop") %>%
  pivot_longer(cols = -firma, names_to = "slowo", values_to = "tfidf") %>%
  group_by(firma) %>%
  slice_max(order_by = tfidf, n = 8, with_ties = FALSE) %>%  # Top 8 słów per firma
  ungroup() %>%
  filter(tfidf > 0)  # Tylko słowa, które faktycznie występują



#' ## Interaktywna tabela TF-IDF per firma
# Interaktywna tabela TF-IDF per firma ----
datatable(
  company_top_df,
  caption  = "Top 8 unikalnych / charakterystycznych słów (TF-IDF) dla każdej firmy",
  rownames = FALSE,
  options  = list(pageLength = 15, scrollX = TRUE)
)

#' # 6. Asocjacje słów
# 6. Asocjacje słów ----

# Dobór słów kluczowych pod kątem inżynierii i incydentów sieciowych
key_terms       <- c("error", "database", "network", "failure", "traffic", "server", "latency")
available_terms <- rownames(tdm_m)
key_terms       <- key_terms[key_terms %in% available_terms]

if (length(key_terms) > 0) {
  # Obliczenie asocjacji (top 10 per termin, próg korelacji 0.15 - podniesiony dla lepszej jakości)
  assoc_list <- lapply(key_terms, function(term) {
    assocs <- findAssocs(tdm, term, corlimit = 0.15)[[1]]
    if (length(assocs) == 0) return(NULL)
    top_assocs <- head(sort(assocs, decreasing = TRUE), 10)
    data.frame(
      termin_glowny = term,
      asocjacja     = names(top_assocs),
      korelacja     = as.numeric(top_assocs),
      stringsAsFactors = FALSE
    )
  })
  assoc_df <- do.call(rbind, Filter(Negate(is.null), assoc_list))
} else {
  assoc_df <- NULL
  cat("Brak dostępnych słów kluczowych w macierzy terminów.\n")
}

#' ## Wykres asocjacji słów kluczowych
# Wykres asocjacji słów kluczowych ----
if (!is.null(assoc_df) && nrow(assoc_df) > 0) {
  ggplot(assoc_df, aes(x = reorder(asocjacja, korelacja), y = korelacja, fill = termin_glowny)) +
    geom_col(show.legend = TRUE, color = "white") +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap(~termin_glowny, scales = "free_y", ncol = 2) +
    coord_flip() +
    labs(
      title    = "Asocjacje słów kluczowych w post-mortemach",
      subtitle = "Słowa najsilniej skorelowane z technicznymi terminami kluczowymi (korelacja Pearsona, próg ≥ 0.15)",
      x        = NULL,
      y        = "Współczynnik korelacji",
      fill     = "Termin główny"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 10),
      axis.text.y = element_text(size = 8)
    )
} else {
  message("Brak wystarczających asocjacji przy zadanym progu korelacji dla wybranych słów technicznych.")
  message("Spróbuj obniżyć corlimit lub sprawdź, czy słowa kluczowe istnieją w macierzy.")
}

#' ## Interaktywna tabela asocjacji
# Interaktywna tabela asocjacji ----
if (!is.null(assoc_df) && nrow(assoc_df) > 0) {
  datatable(
    assoc_df,
    caption  = "Asocjacje inżynieryjnych słów kluczowych (korelacja Pearsona)",
    rownames = FALSE,
    options  = list(pageLength = 15, scrollX = TRUE)
  )
}

#' # Podsumowanie i wnioski
# Podsumowanie ----
cat("\n=== PODSUMOWANIE ANALIZY ===\n")
cat("Liczba dokumentów w korpusie:", length(corpus_processed), "\n")
cat("Liczba unikalnych słów po oczyszczeniu:", nrow(tdm_m), "\n")
cat("Liczba słów po usunięciu rzadkich terminów (TF-IDF):", ncol(dtm_tfidf_m), "\n")
cat("Liczba klastrów wybrana automatycznie:", k_awarie, "\n")
cat("Liczba firm w analizie sentymentu:", length(unique(sentiment_df$firma)), "\n")
cat("Liczba słów kluczowych z asocjacjami:", length(unique(assoc_df$termin_glowny)), "\n")