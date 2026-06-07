# it-postmortem-text-mining

Text mining and NLP analysis of real-world IT incident postmortems using R.  
The project scrapes public postmortem reports (Amazon, Google, Cloudflare, GitHub and others) and applies clustering, sentiment analysis, and keyword association to identify patterns in how major outages happen and are described.

> **Course project** · Projektowanie systemów informatycznych (PSI)  
> Bartosz Kalinowski · Karol Jahn · Michał Płaza

## Team responsibilities
 
| Author | Area | Sections |
|---|---|---|
| **Karol Jahn** | Data collection & preprocessing | Web scraping (`scrape_postmortem.R`), corpus cleaning, stopword removal, stemming, TDM/DTM construction, word frequency analysis |
| **Bartosz Kalinowski** | Clustering & TF-IDF | K-means clustering, silhouette-based `k` selection, cluster visualisations, word clouds per cluster, TF-IDF per company |
| **Michał Płaza** | Sentiment & associations | NRC sentiment analysis, emotion heatmaps, per-company emotion profiles, keyword association analysis, conclusions, SRS documentation|
 
---

## What this project does

1. **Scrapes** postmortem reports from the [danluu/post-mortems](https://github.com/danluu/post-mortems) collection using `rvest`.
2. **Cleans and preprocesses** the corpus — lowercasing, stopword removal, stemming, TF-IDF weighting.
3. **Clusters** documents with K-means on a TF-IDF matrix; optimal `k` is selected automatically via the silhouette method.
4. **Analyses sentiment** using the NRC lexicon (`syuzhet`) — per document and per company.
5. **Extracts keyword associations** (Pearson correlation) for terms like `error`, `database`, `network`, `failure`.
6. Renders everything into a self-contained **HTML report** via R Markdown / `knitr`.

---

## Repository structure

```
it-postmortem-text-mining/
├── dataset/                  # 65+ scraped postmortem .txt files
│   ├── 002_amazon.txt
│   ├── 005_cloudflare.txt
│   └── ...
├── main.R             # Main analysis script (also knitted as R Markdown)
├── main.html          # Rendered HTML report
├── scrape_postmortem.R       # Web scraper — builds the dataset/
├── scraping_log.csv          # Log of scraping runs (status, word count, URL)
├── SRS.docx
└── README.md
```

---

## Key R packages

| Package | Purpose |
|---|---|
| `tm`, `SnowballC` | Text corpus management, stemming |
| `cluster`, `factoextra` | K-means clustering, silhouette analysis |
| `syuzhet` | NRC sentiment / emotion scoring |
| `tidytext`, `tidyr`, `dplyr` | Tidy text transformations |
| `ggplot2`, `plotly` | Static and interactive visualisations |
| `wordcloud` | Word clouds per cluster |
| `DT` | Interactive data tables in HTML output |
| `rvest`, `httr` | Web scraping |

---

## How to run

**1. Scrape the dataset** (optional — dataset already included):
```r
source("scrape_postmortem.R")
```

**2. Run the full analysis and render the report:**
```r
# Option A — render HTML report
rmarkdown::render("psi_projekt.R", output_format = "html_document")

# Option B — run interactively, section by section
source("psi_projekt.R")
```

> Tested on R 4.3+. Install missing packages with `install.packages(c("tm", "SnowballC", "cluster", "wordcloud", "factoextra", "syuzhet", "tidytext", "ggrepel", "DT", "plotly"))`.

---

## Selected results

- **65 postmortem reports** from 15+ companies covering outages from the 1970s (ARPANET) to the present.
- The silhouette method identifies **3–5 distinct failure clusters** — typically grouping reports around themes such as network/routing issues, database failures, and deployment/configuration errors.
- Sentiment analysis shows that `fear` and `anticipation` dominate across companies, while `anger` and `disgust` are comparatively rare — consistent with the clinical, technical tone of engineering postmortems.
- TF-IDF association analysis finds strong co-occurrence of `latency` ↔ `traffic`, `database` ↔ `replication`, and `failure` ↔ `deploy`.

---

## Dataset source

Postmortem URLs sourced from:  
**[https://github.com/danluu/post-mortems](https://github.com/danluu/post-mortems)** — a community-maintained list of public IT incident reports.

Companies represented include: Amazon, Google, Cloudflare, GitHub, Microsoft, Facebook, TravisCI, Etsy, GoCardless, Datadog, Stack Overflow, and others.
