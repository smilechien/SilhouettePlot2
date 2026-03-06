# SilhouettePlot Shiny App

An R Shiny application for generating **FLCA Top20 reports** from bibliometric and co-word data. The app supports uploaded files, PubMed search hyperlinks, demo data, and remote CSV links, then produces an HTML report plus downloadable figures and tables.

## Main features

- Builds a **Top-N network view** after FLCA-based sampling
- Produces the following outputs:
  - Network (Top20)
  - SS plot
  - Kano1
  - Kano2
  - PCA
  - Sankey
- Creates a **self-contained HTML report**
- Supports figure/table downloads and a ZIP file containing all figures
- Accepts both local upload and URL-based input

## Supported input types

The app accepts:

1. **2-column files**: `Leader, Follower`
2. **3-column files**: `Leader, Follower, WCD`
3. **Multi-column records**
4. **PubMed summary text**
5. **AMA references**
6. **WoS one-column `;`-separated data**
7. **PubMed search hyperlinks**
8. **Remote CSV files via URL query parameter**

## Important author rule

- **AMA-reference inputs** keep **all authors** and the **journal**.
- **PubMed URL** and **WoS one-column author inputs** keep only the **first and last authors** plus the **journal** for visual analysis.

## Repository structure

Place these files in the same app folder:

```text
app.R
utils.R
renderSSplot.R
sankey.R
appstable.R           # optional
report_template.Rmd
```

The app sources these files at startup. If `utils.R` or `report_template.Rmd` is missing, the app will not run correctly.

## Required R packages

Install the required packages before launching the app:

```r
install.packages(c(
  "shiny", "dplyr", "rmarkdown", "igraph",
  "ggplot2", "ggrepel", "grid", "readr"
))
```

## Run locally

In R or RStudio:

```r
setwd("path/to/your/app/folder")
shiny::runApp()
```

## How to use

1. Upload a file, paste a **PubMed search hyperlink**, or enable **Use demo**.
2. Set:
   - **Top N**: number of nodes kept after FLCA (default `20`)
   - **Major sampling: per cluster**: balance of selected nodes across clusters
3. Click **Generate HTML report**.
4. Open the results in:
   - **Figures** tab
   - **Downloads** tab
   - **Report** tab

## URL parameters

The app supports URL-driven input.

### Remote CSV

```text
?autorun=1&csv_url=<remote_csv_link>
```

Example:

```text
https://smilechien.shinyapps.io/zssplotauthor3/?autorun=1&csv_url=https://raw.githubusercontent.com/smilechien/raschonline/main/DrKan.csv
```

### PubMed hyperlink

```text
?autorun=1&pubmed_url=<pubmed_search_url>
```

Example:

```text
https://smilechien.shinyapps.io/zssplotauthor3/?autorun=1&pubmed_url=https://pubmed.ncbi.nlm.nih.gov/?term=Tsair-Wei+Chien%5BAuthor%5D&sort=date
```

## Outputs

### Figures

- Network (Top20)
- SS plot
- Kano1
- Kano2
- PCA
- Sankey

### Downloadable files

- `report.html`
- Top20 nodes CSV
- Top20 relations CSV
- PNG figures
- ZIP file with all figures

## Notes

- The app title is **FLCA Top20 Report (PubMed, WoS, and coword data)**.
- The app includes fallback reading for `csv/txt/tsv` files when needed.
- `appstable.R` is optional, but the other source files are expected by `app.R`.
- `report_template.Rmd` must exist in the app folder because the report is created from that template.

## Suggested GitHub release checklist

Before submitting the repository, make sure it includes:

- `app.R`
- all sourced helper files
- `report_template.Rmd`
- at least one demo dataset
- a license file
- version information in the README or release notes
- a screenshot of the app home page and one example report figure

## Citation / journal context

This app is intended for bibliometric network analysis with silhouette-enhanced visualization, especially for SoftwareX-style reproducible software dissemination.
