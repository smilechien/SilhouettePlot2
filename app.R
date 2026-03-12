# ---- Always-available helper: cannot be missing ----
ensure_dir <- function(path) {
  if (is.null(path) || !nzchar(path)) stop("out_dir is empty.")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) stop("Failed to create out_dir: ", path)
  invisible(path)
}

# ---- Simple sourcing (assuming working directory is app directory) ----
app_dir <- getwd()

source(file.path(app_dir, "utils.R"), local = TRUE)
try(source(file.path(app_dir, "renderSSplot.R"), local = TRUE), silent = TRUE)
try(source(file.path(app_dir, "sankey.R"),       local = TRUE), silent = TRUE)
if (file.exists(file.path(app_dir, "appstable.R"))) {
  try(source(file.path(app_dir, "appstable.R"), local = TRUE), silent = TRUE)
}

if (file.exists(file.path(app_dir, "appAAC.R"))) {
  try(source(file.path(app_dir, "appAAC.R"), local = FALSE), silent = TRUE)
}
if (file.exists(file.path(app_dir, "kano.R"))) {
  try(source(file.path(app_dir, "kano.R"), local = FALSE), silent = TRUE)
}



# ---- Encoding + symbol normalization helpers ----
.read_lines_auto <- function(path) {
  encs <- c("UTF-8", "Windows-1252", "CP950", "Big5", "latin1")
  for (enc in encs) {
    z <- tryCatch(readLines(path, warn = FALSE, encoding = enc), error = function(e) NULL)
    if (!is.null(z)) {
      z <- iconv(z, from = enc, to = "UTF-8", sub = "")
      if (!is.null(z)) return(z)
    }
  }
  stop("Cannot read text file with supported encodings: ", basename(path))
}

.normalize_text_symbols <- function(x) {
  x <- as.character(x %||% "")
  x <- suppressWarnings(iconv(x, from = "", to = "UTF-8", sub = ""))
  x[is.na(x)] <- ""
  x <- gsub("[‘’ʼ]", "'", x, perl = TRUE)
  x <- gsub("[“”]", '"', x, perl = TRUE)
  x <- gsub("[–—−]", "-", x, perl = TRUE)
  x <- gsub("O[[:space:]]*[’'`]?Neill", "O'Neill", x, perl = TRUE)
  x <- gsub("O[[:space:]]+Neil\b", "O'Neil", x, perl = TRUE)
  x <- gsub(" ", " ", x, perl = TRUE)
  x <- gsub("[[:cntrl:]]", "", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  Encoding(x) <- "UTF-8"
  trimws(x)
}


.normalize_df_symbols <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  for (j in seq_along(df)) {
    if (is.character(df[[j]])) df[[j]] <- .normalize_text_symbols(df[[j]])
  }
  df
}

.read_any_table_auto <- function(path, ...) {
  ext <- tolower(tools::file_ext(path))
  encs <- c("UTF-8", "Windows-1252", "CP950", "Big5", "latin1")
  dots <- list(...)
  dots$check.names <- NULL
  dots$stringsAsFactors <- NULL
  dots$fileEncoding <- NULL
  if (ext == "txt") {
    lines <- .read_lines_auto(path)
    lines <- .normalize_text_symbols(lines)
    return(data.frame(V1 = lines, stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (ext %in% c("csv", "tsv")) {
    for (enc in encs) {
      if (ext == "csv") {
        args <- c(list(file = path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = enc), dots)
        out <- tryCatch(do.call(utils::read.csv, args), error = function(e) NULL)
        if (!is.null(out)) return(.normalize_df_symbols(out))
      }
      args <- c(list(file = path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = enc), dots)
      out <- tryCatch(do.call(utils::read.delim, args), error = function(e) NULL)
      if (!is.null(out)) return(.normalize_df_symbols(out))
    }
  }
  lines <- tryCatch(.read_lines_auto(path), error = function(e) character(0))
  if (length(lines)) return(data.frame(V1 = .normalize_text_symbols(lines), stringsAsFactors = FALSE, check.names = FALSE))
  stop("No available reader for file: ", basename(path))
}

# Force a single safe reader wrapper so duplicate check.names / fileEncoding
# arguments from older helpers do not leak into utils::read.csv/read.delim.
read_any_table <- function(path, ...) {
  return(.read_any_table_auto(path, ...))
}

options(stringsAsFactors = FALSE)
# ---- Guard: avoid 'cannot change locked binding for data' ----
try({
  if (exists("data", envir = .GlobalEnv, inherits = FALSE) && bindingIsLocked("data", .GlobalEnv)) {
    unlockBinding("data", .GlobalEnv)
  }
}, silent = TRUE)
options(repos = c(CRAN="https://cloud.r-project.org"))
pkgs <- c("shiny","dplyr","tidyr","rmarkdown","igraph","ggplot2","ggrepel","grid","readr","scales")
miss <- pkgs[!vapply(pkgs, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))]
if (length(miss)) {
  stop(
    "Missing required packages: ", paste(miss, collapse = ", "),
    "
Please install them first, e.g.: install.packages(c(",
    paste(sprintf('\"%s\"', miss), collapse = ", "),
    "))"
  )
}
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(rmarkdown)
  library(igraph)
})


# ---- PubMed YearTermTrend / Slope helpers ----
.extract_first_year <- function(x) {
  x <- paste(x %||% "", collapse = " ")
  m <- regexpr("\\b(?:19|20)\\d{2}\\b", x, perl = TRUE)
  if (m[1] < 1) return(NA_integer_)
  suppressWarnings(as.integer(substr(x, m[1], m[1] + attr(m, "match.length") - 1L)))
}



if (!exists(".strip_profile_symbols", mode = "function")) {
  .strip_profile_symbols <- function(x) {
    x <- as.character(x %||% "")
    x <- gsub("^[[:space:][:punct:]]+|[[:space:][:punct:]]+$", "", x, perl = TRUE)
    trimws(x)
  }
}

if (!exists(".clean_journal_label", mode = "function")) {
  .clean_journal_label <- function(x) {
    x <- as.character(x %||% "")
    x <- .normalize_text_symbols(x)
    x <- gsub("\\s+", " ", x, perl = TRUE)
    x <- gsub("\\s*doi:.*$", "", x, ignore.case = TRUE, perl = TRUE)
    x <- gsub("\\s*Epub.*$", "", x, ignore.case = TRUE, perl = TRUE)
    x <- gsub("\\s*\\[.*$", "", x, perl = TRUE)
    x <- trimws(x)
    x[nzchar(x) == FALSE] <- NA_character_
    x
  }
}


.extract_year_from_text <- function(x) {
  x <- .normalize_text_symbols(as.character(x %||% ""))
  m <- regexpr("\\b(?:19|20)\\d{2}\\b", x, perl = TRUE)
  if (m[1] < 1) return(NA_integer_)
  suppressWarnings(as.integer(substr(x, m[1], m[1] + attr(m, "match.length") - 1L)))
}

.extract_journal_from_reference_line <- function(x) {
  x <- .normalize_text_symbols(as.character(x %||% ""))
  x <- gsub("[\r\n]+", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x <- trimws(x)
  if (!nzchar(x)) return(NA_character_)

  cand <- character(0)
  # APA-like: Authors (2023). Title. Journal, ...
  if (grepl("\\(\\d{4}\\)", x, perl = TRUE)) {
    rest <- sub("^.*?\\(\\d{4}\\)\\.\\s*", "", x, perl = TRUE)
    segs <- trimws(unlist(strsplit(rest, "\\.\\s+", perl = TRUE), use.names = FALSE))
    segs <- segs[nzchar(segs)]
    if (length(segs) >= 2L) cand <- c(cand, segs[2L])
  }
  # AMA-like: Authors. Title. Journal. 2023;...
  pre_year <- sub("\\b(?:19|20)\\d{2}\\b.*$", "", x, perl = TRUE)
  segs2 <- trimws(unlist(strsplit(pre_year, "\\.\\s+", perl = TRUE), use.names = FALSE))
  segs2 <- segs2[nzchar(segs2)]
  if (length(segs2) >= 1L) cand <- c(cand, tail(segs2, 1L))

  cand <- .clean_journal_label(cand)
  cand <- cand[!is.na(cand) & nzchar(cand)]
  cand <- cand[!grepl("^(doi|pmid|epub|available from)", cand, ignore.case = TRUE)]
  if (!length(cand)) return(NA_character_)
  cand[[which.max(nchar(cand))]]
}

if (!exists(".extract_reference_journal_year_df", mode = "function")) {
  .extract_reference_journal_year_df <- function(df) {
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)
    nms <- names(df)
    journal_col <- intersect(nms, c("Journal","journal","SO","JT","TA","Source title","Source","SRCTITLE"))[1]
    year_col <- intersect(nms, c("Year","year","PY","Publication Year","PubYear"))[1]
    if (!is.na(journal_col) && !is.na(year_col)) {
      out <- data.frame(
        Journal = .clean_journal_label(df[[journal_col]]),
        Year = suppressWarnings(as.integer(df[[year_col]])),
        stringsAsFactors = FALSE
      )
      out <- out[is.finite(out$Year) & !is.na(out$Journal) & nzchar(out$Journal), , drop = FALSE]
      rownames(out) <- NULL
      return(out)
    }
    # fallback: one-column or free-text reference rows (AMA/APA-like)
    if (ncol(df) >= 1L) {
      lines <- apply(df, 1, function(z) paste(z[!is.na(z) & nzchar(trimws(as.character(z)))], collapse = " "))
      lines <- .normalize_text_symbols(trimws(as.character(lines)))
      lines <- lines[nzchar(lines)]
      if (length(lines)) {
        out <- data.frame(
          Journal = vapply(lines, .extract_journal_from_reference_line, character(1)),
          Year = vapply(lines, .extract_year_from_text, integer(1)),
          stringsAsFactors = FALSE
        )
        out$Journal <- .clean_journal_label(out$Journal)
        out <- out[is.finite(out$Year) & !is.na(out$Journal) & nzchar(out$Journal), , drop = FALSE]
        rownames(out) <- NULL
        if (nrow(out)) return(out)
      }
    }
    NULL
  }
}

.parse_pubmed_summary_records <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  lines <- tryCatch(.read_lines_auto(path), error = function(e) NULL)
  if (is.null(lines) || !length(lines)) return(NULL)
  lines <- .normalize_text_symbols(lines)
  idx <- grep("^PMID-\\s*\\d+", lines, perl = TRUE)
  if (!length(idx)) return(NULL)
  ends <- c(idx[-1] - 1L, length(lines))
  out <- vector("list", length(idx))
  for (k in seq_along(idx)) {
    rec <- lines[idx[k]:ends[k]]
    au <- trimws(sub("^AU\\s*-\\s*", "", rec[grepl("^AU\\s*-", rec)]))
    if (!length(au)) au <- trimws(sub("^FAU\\s*-\\s*", "", rec[grepl("^FAU\\s*-", rec)]))
    au <- au[nzchar(au)]
    au <- unique(.normalize_author_token(au))
    au <- .keep_first_last(au)
    dp_lines <- rec[grepl("^DP\\s*-", rec)]
    y <- .extract_first_year(dp_lines)
    if (!is.finite(y)) {
      ad_lines <- rec[grepl("^PHST\\s*-", rec) | grepl("^DEP\\s*-", rec) | grepl("^EDAT\\s*-", rec)]
      y <- .extract_first_year(ad_lines)
    }
    terms <- unique(au)
    terms <- .strip_profile_symbols(terms)
    terms <- trimws(as.character(terms))
    terms <- terms[!is.na(terms) & nzchar(terms)]
    out[[k]] <- data.frame(article_id = k, Year = y, term = terms, stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}


.parse_pubmed_summary_journal_year <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  lines <- tryCatch(.read_lines_auto(path), error = function(e) NULL)
  if (is.null(lines) || !length(lines)) return(NULL)
  lines <- .normalize_text_symbols(lines)
  idx <- grep("^PMID-\\s*\\d+", lines, perl = TRUE)
  if (!length(idx)) return(NULL)
  ends <- c(idx[-1] - 1L, length(lines))
  out <- vector("list", length(idx))
  for (k in seq_along(idx)) {
    rec <- lines[idx[k]:ends[k]]
    jt_lines <- rec[grepl("^JT\\s*-", rec)]
    if (!length(jt_lines)) jt_lines <- rec[grepl("^TA\\s*-", rec)]
    if (!length(jt_lines)) jt_lines <- rec[grepl("^SO\\s*-", rec)]
    jt <- if (length(jt_lines)) trimws(sub("^[A-Z]{2}\\s*-\\s*", "", jt_lines[1])) else NA_character_
    jt <- .clean_journal_label(jt)
    dp_lines <- rec[grepl("^DP\\s*-", rec)]
    y <- .extract_first_year(dp_lines)
    if (!is.finite(y)) {
      ad_lines <- rec[grepl("^PHST\\s*-", rec) | grepl("^DEP\\s*-", rec) | grepl("^EDAT\\s*-", rec)]
      y <- .extract_first_year(ad_lines)
    }
    out[[k]] <- data.frame(Journal = jt, Year = y, stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, out)
  out$Journal <- trimws(as.character(out$Journal))
  out <- out[!is.na(out$Year) & is.finite(out$Year) & nzchar(out$Journal), , drop = FALSE]
  rownames(out) <- NULL
  out
}



.parse_pubmed_summary_keyword_year <- function(path, field_tags = c("AU","FAU")) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  lines <- tryCatch(.read_lines_auto(path), error = function(e) NULL)
  if (is.null(lines) || !length(lines)) return(NULL)
  lines <- .normalize_text_symbols(lines)
  idx <- grep("^PMID-\\s*\\d+", lines, perl = TRUE)
  if (!length(idx)) return(NULL)
  ends <- c(idx[-1] - 1L, length(lines))
  out <- vector("list", length(idx))
  for (k in seq_along(idx)) {
    rec <- lines[idx[k]:ends[k]]
    tags <- unlist(lapply(field_tags, function(tag) rec[grepl(paste0("^", tag, "\\s*-"), rec)]), use.names = FALSE)
    vals <- trimws(sub("^[A-Z]{2,4}\\s*-\\s*", "", tags))
    vals <- .normalize_author_token(vals)
    vals <- .keep_first_last(unique(vals[nzchar(vals)]))
    dp_lines <- rec[grepl("^DP\\s*-", rec)]
    y <- .extract_first_year(dp_lines)
    if (!is.finite(y)) {
      so_lines <- rec[grepl("^SO\\s*-", rec)]
      y <- .extract_first_year(so_lines)
    }
    out[[k]] <- data.frame(article_id = k, Year = y, term = vals, stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, out)
  out <- out[is.finite(out$Year) & nzchar(out$term), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.summarize_journal_recent10y <- function(journal_year_df, recent_n = 10L, top_n = 10L) {
  if (is.null(journal_year_df) || !nrow(journal_year_df)) return(NULL)
  d <- as.data.frame(journal_year_df, stringsAsFactors = FALSE)
  d$Year <- suppressWarnings(as.integer(d$Year))
  d$Journal <- .strip_profile_symbols(trimws(as.character(d$Journal)))
  d <- d[is.finite(d$Year) & nzchar(d$Journal), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  ymax <- max(d$Year, na.rm = TRUE)
  recent_cutoff <- ymax - recent_n + 1L
  d <- d[d$Year >= recent_cutoff & d$Year <= ymax, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  counts <- d %>% dplyr::count(Journal, Year, name = "Count")
  top_tbl <- counts %>%
    dplyr::group_by(.data$Journal) %>%
    dplyr::summarise(Total = sum(.data$Count), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$Total), .data$Journal) %>%
    dplyr::slice_head(n = top_n)
  years <- seq.int(recent_cutoff, ymax, by = 1L)
  counts_top <- counts %>%
    dplyr::filter(.data$Journal %in% top_tbl$Journal) %>%
    tidyr::complete(Journal = top_tbl$Journal, Year = years, fill = list(Count = 0L)) %>%
    dplyr::left_join(top_tbl, by = "Journal")
  summary_table <- counts_top %>%
    dplyr::select(.data$Journal, .data$Year, .data$Count) %>%
    tidyr::pivot_wider(names_from = .data$Year, values_from = .data$Count, values_fill = 0L) %>%
    dplyr::left_join(top_tbl, by = "Journal") %>%
    dplyr::relocate(.data$Total, .after = .data$Journal) %>%
    dplyr::arrange(dplyr::desc(.data$Total), .data$Journal)
  list(counts = counts_top, top20 = top_tbl, summary_table = summary_table, years = years)
}

.plot_journal_slopegraph <- function(jobj, title = "Top 10 journals over recent 10 years") {
  if (is.null(jobj) || is.null(jobj$counts) || !nrow(jobj$counts)) return(NULL)
  d <- as.data.frame(jobj$counts, stringsAsFactors = FALSE)
  d$Year <- suppressWarnings(as.integer(d$Year))
  d$Count <- suppressWarnings(as.numeric(d$Count))
  d$Journal <- trimws(as.character(d$Journal))
  d <- d[is.finite(d$Year) & is.finite(d$Count) & nzchar(d$Journal), , drop = FALSE]
  if (!nrow(d)) return(NULL)

  yrs <- sort(unique(d$Year))
  if (length(yrs) < 2) return(NULL)
  y0 <- min(yrs, na.rm = TRUE)
  y1 <- max(yrs, na.rm = TRUE)

  top_tbl <- jobj$top20
  if (is.null(top_tbl) || !nrow(top_tbl)) return(NULL)
  top_tbl <- as.data.frame(top_tbl, stringsAsFactors = FALSE)
  top_tbl$Journal <- trimws(as.character(top_tbl$Journal))
  top_tbl$Total <- suppressWarnings(as.numeric(top_tbl$Total))
  top_tbl <- top_tbl[is.finite(top_tbl$Total) & nzchar(top_tbl$Journal), , drop = FALSE]
  top_tbl <- top_tbl[order(-top_tbl$Total, top_tbl$Journal), , drop = FALSE]
  if (!nrow(top_tbl)) return(NULL)
  ord <- unique(top_tbl$Journal)

  ends <- dplyr::bind_rows(
    d[d$Year == y0, c("Journal", "Count"), drop = FALSE] |> dplyr::mutate(Endpoint = as.character(y0)),
    d[d$Year == y1, c("Journal", "Count"), drop = FALSE] |> dplyr::mutate(Endpoint = as.character(y1))
  )
  ends <- ends |>
    dplyr::filter(.data$Journal %in% ord) |>
    dplyr::group_by(.data$Journal, .data$Endpoint) |>
    dplyr::summarise(Count = sum(.data$Count), .groups = "drop") |>
    tidyr::complete(Journal = ord, Endpoint = c(as.character(y0), as.character(y1)), fill = list(Count = 0))
  ends$Journal <- factor(ends$Journal, levels = rev(ord))
  left_df <- ends[ends$Endpoint == as.character(y0), , drop = FALSE]
  right_df <- ends[ends$Endpoint == as.character(y1), , drop = FALSE]
  ymax <- max(ends$Count, na.rm = TRUE)

  ggplot2::ggplot(ends, ggplot2::aes(x = .data$Endpoint, y = .data$Count, group = .data$Journal)) +
    ggplot2::geom_line(linewidth = 0.9, color = "black", na.rm = TRUE) +
    ggplot2::geom_point(size = 2.6, color = "black", na.rm = TRUE) +
    ggplot2::geom_text(data = left_df, ggplot2::aes(x = .data$Endpoint, y = .data$Count, label = .data$Journal),
                       hjust = 1, nudge_x = -0.06, size = 3.1, inherit.aes = FALSE, na.rm = TRUE) +
    ggplot2::geom_text(data = right_df, ggplot2::aes(x = .data$Endpoint, y = .data$Count, label = .data$Journal),
                       hjust = 0, nudge_x = 0.06, size = 3.1, inherit.aes = FALSE, na.rm = TRUE) +
    ggplot2::labs(title = sprintf("%s (%s vs %s)", title, y0, y1), x = NULL, y = "Count") +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0.25, 0.25))) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(0, max(1, ymax * 1.12))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.text.x = ggplot2::element_text(face = "bold", size = 11),
      plot.margin = ggplot2::margin(5.5, 160, 5.5, 160)
    )
}


.document_frequency_in_data <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)
  keys_per_row <- apply(df, 1, function(z) {
    z <- as.character(z)
    z <- z[!is.na(z) & nzchar(trimws(z))]
    if (!length(z)) return(character(0))
    unique(.kk_name_key(z))
  })
  all_keys <- unlist(keys_per_row, use.names = FALSE)
  if (!length(all_keys)) return(NULL)
  stats::setNames(as.numeric(table(all_keys)), names(table(all_keys)))
}

.kk_name_key <- function(x) {
  x <- .normalize_text_symbols(x)
  x <- tolower(x)
  x <- gsub("[.]", " ", x, perl = TRUE)
  x <- gsub("[-_]", " ", x, perl = TRUE)
  x <- gsub("[^[:alnum:]' ]", " ", x, perl = TRUE)
  x <- gsub("\b(jr|sr|iii|ii|iv)\b", " ", x, perl = TRUE)
  x <- gsub("o[[:space:]]*neill", "oneill", x, perl = TRUE)
  x <- gsub("[']", "", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}

.summarize_article_trend_recent10y <- function(journal_year_df, recent_n = 10L) {
  if (is.null(journal_year_df) || !nrow(journal_year_df)) return(NULL)
  d <- as.data.frame(journal_year_df, stringsAsFactors = FALSE)
  d$Year <- suppressWarnings(as.integer(d$Year))
  d <- d[is.finite(d$Year), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  recent_cutoff <- as.integer(format(Sys.Date(), "%Y")) - recent_n + 1L
  d <- d[d$Year >= recent_cutoff, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  yy <- sort(unique(d$Year))
  out <- d %>% dplyr::count(Year, name = "Articles")
  out <- tidyr::complete(out, Year = yy, fill = list(Articles = 0L))
  out
}

.plot_article_bar_recent10y <- function(df, title = "Article trend over recent 10 years") {
  if (is.null(df) || !nrow(df)) return(NULL)
  d <- as.data.frame(df, stringsAsFactors = FALSE)
  d$Year <- factor(d$Year, levels = d$Year)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$Year, y = .data$Articles)) +
    ggplot2::geom_col(width = 0.72, fill = "grey35") +
    ggplot2::geom_text(ggplot2::aes(label = .data$Articles), vjust = -0.35, size = 3.6) +
    ggplot2::labs(title = title, x = NULL, y = "Articles") +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.text.x = ggplot2::element_text(face = "bold")
    )
}

.compute_slope_trend_top20 <- function(term_year_long, top_terms, recent_n = 10L, p_cut = 0.05) {
  if (is.null(term_year_long) || !nrow(term_year_long) || !length(top_terms)) return(NULL)
  d <- as.data.frame(term_year_long, stringsAsFactors = FALSE)
  d$Year <- suppressWarnings(as.integer(d$Year))
  d$term <- trimws(as.character(d$term))
  d <- d[is.finite(d$Year) & !is.na(d$term) & nzchar(d$term), , drop = FALSE]
  d <- d[d$term %in% top_terms, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  now_y <- as.integer(format(Sys.Date(), "%Y"))
  d <- d[d$Year >= (now_y - recent_n + 1L) & d$Year <= now_y, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  years <- sort(unique(d$Year))
  if (length(years) < 2L) return(NULL)
  years <- tail(years, min(recent_n, length(years)))
  counts <- d %>%
    dplyr::count(term, Year, name = "Count") %>%
    tidyr::complete(term = top_terms, Year = years, fill = list(Count = 0L)) %>%
    dplyr::filter(.data$term %in% top_terms)
  half <- floor(length(years) / 2L)
  prior_years <- years[seq_len(max(1L, half))]
  post_years  <- years[setdiff(seq_along(years), seq_len(max(1L, half)))]
  if (!length(post_years)) post_years <- tail(years, max(1L, half))
  trend_tbl <- do.call(rbind, lapply(top_terms, function(tt) {
    z <- counts[counts$term == tt, , drop = FALSE]
    z <- z[match(years, z$Year), , drop = FALSE]
    prior <- z$Count[z$Year %in% prior_years]
    post  <- z$Count[z$Year %in% post_years]
    pval <- tryCatch({
      if (length(prior) >= 2L && length(post) >= 2L) stats::t.test(post, prior)$p.value else NA_real_
    }, error = function(e) NA_real_)
    m1 <- mean(prior, na.rm = TRUE)
    m2 <- mean(post,  na.rm = TRUE)
    tot <- sum(z$Count, na.rm = TRUE)
    trend <- if (!is.na(pval) && pval < p_cut && m2 > m1) "Increasing" else if (!is.na(pval) && pval < p_cut && m2 < m1) "Decreasing" else "Stationary"
    data.frame(term = tt, prior_mean = m1, post_mean = m2, total_count = tot, p_value = pval, trend = trend, stringsAsFactors = FALSE)
  }))
  trend_tbl$trend <- factor(trend_tbl$trend, levels = c("Decreasing", "Stationary", "Increasing"))
  counts <- dplyr::left_join(counts, trend_tbl, by = "term")
  list(counts = counts, trend = trend_tbl, years = years, prior_years = prior_years, post_years = post_years)
}

.plot_slope_trend <- function(sobj, title = NULL, min.space = 0.05) {
  if (is.null(sobj) || is.null(sobj$counts) || !nrow(sobj$counts)) return(NULL)
  counts <- as.data.frame(sobj$counts, stringsAsFactors = FALSE)
  years <- sort(unique(sobj$years))
  if (length(years) < 2L) return(NULL)

  ord <- sobj$trend %>%
    dplyr::arrange(.data$total_count, .data$term) %>%
    dplyr::pull(.data$term)

  wide <- counts %>%
    dplyr::filter(.data$Year %in% years, .data$term %in% ord) %>%
    dplyr::select(.data$term, .data$Year, .data$Count, .data$trend) %>%
    tidyr::pivot_wider(names_from = .data$Year, values_from = .data$Count, values_fill = 0L)

  wide$term <- factor(wide$term, levels = ord)
  wide <- wide[order(wide$term), , drop = FALSE]

  year_cols <- as.character(years)
  mat <- as.matrix(wide[, year_cols, drop = FALSE])
  storage.mode(mat) <- "numeric"
  rng <- range(mat, na.rm = TRUE)
  span <- diff(rng)
  if (!is.finite(span) || span <= 0) span <- 1
  min_gap <- min.space * span

  shifts <- rep(0, nrow(mat))
  if (nrow(mat) >= 2L) {
    for (i in 2:nrow(mat)) {
      prev <- mat[i - 1L, ] + shifts[i - 1L]
      cur  <- mat[i, ]
      dmin <- min(cur - prev, na.rm = TRUE)
      shifts[i] <- if (is.finite(dmin) && dmin < min_gap) (min_gap - dmin) else 0
    }
  }
  cum_shift <- cumsum(shifts)
  ypos_mat <- sweep(mat, 1L, cum_shift, `+`)

  term_trend <- counts[, c("term", "trend")]
  term_trend <- term_trend[!duplicated(term_trend$term), , drop = FALSE]
  ypos_df <- data.frame(term = as.character(wide$term), ypos_mat, check.names = FALSE)
  ypos_df <- dplyr::left_join(ypos_df, term_trend, by = "term")
  long <- reshape2::melt(ypos_df, id.vars = c("term", "trend"), variable.name = "Year", value.name = "ypos")
  long$Year <- suppressWarnings(as.integer(as.character(long$Year)))
  long <- dplyr::left_join(long, counts[, c("term", "Year", "Count")], by = c("term", "Year"))
  long$trend <- factor(long$trend, levels = c("Decreasing", "Stationary", "Increasing"))
  long$x <- factor(long$Year, levels = years, labels = as.character(years))
  long$CountLabel <- ifelse(is.na(long$Count) | long$Count <= 0, "", as.character(long$Count))

  left_df <- long[long$Year == min(years), , drop = FALSE]
  left_df <- left_df[match(ord, left_df$term), , drop = FALSE]
  draw_df <- long[is.finite(long$Count) & long$Count > 0, , drop = FALSE]

  title <- title %||% sprintf("Slope graph of Top20 terms (%s vs %s)", paste(range(sobj$prior_years), collapse = "-"), paste(range(sobj$post_years), collapse = "-"))

  ggplot2::ggplot(draw_df, ggplot2::aes(x = .data$x, y = .data$ypos, group = .data$term, color = .data$trend)) +
    ggplot2::geom_line(linewidth = 0.8, alpha = 0.95, na.rm = TRUE) +
    ggplot2::geom_point(size = 2.0, stroke = 0.2, na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = .data$CountLabel), size = 3.0, vjust = -0.55, show.legend = FALSE, na.rm = TRUE) +
    ggplot2::scale_y_continuous(name = NULL, breaks = left_df$ypos, labels = left_df$term) +
    ggplot2::scale_color_manual(values = c(Decreasing = "blue", Stationary = "black", Increasing = "red")) +
    ggplot2::labs(title = title, x = NULL, y = NULL, color = "Trend (post vs prior half)") +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "top",
      axis.text.y = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(face = "bold"),
      axis.ticks.y = ggplot2::element_blank()
    )
}


# ============================================================
# Kano plot renderer (beautiful + safe) - overrides utils.R
# Produces the style similar to the reference Kano plot.
# ============================================================
render_kano_png <- function(out_png, nodes, data, xcol = "value2", ycol = "value",
                            title = "Kano plot", xlab = "Edge(Influence)", ylab = "Density(Dominance)",
                            add_circle = TRUE) {
  if (exists("plot_kano_real_xy", mode = "function") || exists("kano_plot", mode = "function") || exists("kano_plot_ss_astar", mode = "function")) {
    try({
      suppressPackageStartupMessages({
        require(ggplot2)
      })
      p <- NULL
      # Match apppubmed.R behavior first so kano.R's 3-circle functions are actually used
      if (identical(xcol, "value2") && identical(ycol, "value") && exists("kano_plot", mode = "function")) {
        p <- kano_plot(nodes, edges = data, xlab = xlab, ylab = ylab, title_txt = title)
      } else if (((identical(xcol, "ssi") || identical(xcol, "ss")) && (identical(ycol, "a_star1") || identical(ycol, "a_star"))) ||
                 ((identical(xcol, "a_star1") || identical(xcol, "a_star")) && (identical(ycol, "ssi") || identical(ycol, "ss")))) {
        nd <- as.data.frame(nodes, stringsAsFactors = FALSE)
        if (!("ss" %in% names(nd))) {
          if ("ssi" %in% names(nd)) nd$ss <- suppressWarnings(as.numeric(nd$ssi))
        }
        if (!("a_star" %in% names(nd))) {
          if ("a_star1" %in% names(nd)) nd$a_star <- suppressWarnings(as.numeric(nd$a_star1))
        }
        if (exists("kano_plot_ss_astar", mode = "function")) {
          p <- kano_plot_ss_astar(nd, edges = data, xlab = "SS(i)", ylab = "a*(i)", title_txt = title)
        } else if (exists("plot_kano_real_xy", mode = "function")) {
          p <- plot_kano_real_xy(nodes = nd, edges = data, xcol = "ss", ycol = "a_star", sizecol = "value", title_txt = title, xlab = "SS(i)", ylab = "a*(i)")
        }
      } else if (exists("plot_kano_real_xy", mode = "function")) {
        p <- plot_kano_real_xy(nodes = nodes, edges = data, xcol = xcol, ycol = ycol, sizecol = ycol, title_txt = title, xlab = xlab, ylab = ylab)
      } else if (exists("kano_plot", mode = "function")) {
        p <- kano_plot(nodes, edges = data, xlab = xlab, ylab = ylab, title_txt = title)
      }
      if (!is.null(p)) {
        ggplot2::ggsave(filename = out_png, plot = p, width = 10, height = 7, dpi = 200, units = "in", bg = "white")
        return(invisible(out_png))
      }
    }, silent = TRUE)
  }
  suppressPackageStartupMessages({
    require(ggplot2)
    require(ggrepel)
    require(dplyr)
    require(grid)
  })

  nodes <- as.data.frame(nodes, stringsAsFactors = FALSE)
  data  <- as.data.frame(data,  stringsAsFactors = FALSE)

  need_cols <- c("name", "carac", xcol, ycol)
  miss <- setdiff(need_cols, names(nodes))
  if (length(miss) > 0) stop("`nodes` missing: ", paste(miss, collapse = ", ") )

  # Coerce numeric
  nodes[[xcol]] <- suppressWarnings(as.numeric(nodes[[xcol]]))
  nodes[[ycol]] <- suppressWarnings(as.numeric(nodes[[ycol]]))
  nodes <- nodes[is.finite(nodes[[xcol]]) & is.finite(nodes[[ycol]]), , drop = FALSE]
  if (nrow(nodes) < 2) stop("Not enough valid nodes to draw Kano plot.")

  nodes$carac <- as.factor(nodes$carac)

  # Default color set (cluster colors)
  specified_colors <- c(
    "#FF0000", "#0000FF", "#998000", "#008000", "#800080",
    "#FFC0CB", "#000000", "#ADD8E6", "#FF4500", "#A52A2A",
    "#8B4513", "#FF8C00", "#32CD32", "#4682B4", "#9400D3",
    "#FFD700", "#C0C0C0", "#DC143C", "#1E90FF"
  )
  levels_carac <- levels(nodes$carac)
  num_clusters <- length(levels_carac)
  full_color_set <- if (num_clusters > length(specified_colors)) {
    c(specified_colors, grDevices::hcl.colors(num_clusters - length(specified_colors), "Dark 3", rev = TRUE))
  } else specified_colors
  color_mapping <- setNames(full_color_set[seq_len(num_clusters)], levels_carac)
  nodes$color <- unname(color_mapping[as.character(nodes$carac)])

  # Build edges with coordinates (robust to colnames)
  if (ncol(data) >= 2) {
    colnames(data)[1:2] <- c("Source", "Target")
  }
  if (ncol(data) >= 3) {
    colnames(data)[3] <- "WCD"
  } else {
    data$WCD <- 1
  }

  edges <- data %>%
    dplyr::left_join(nodes %>% dplyr::select(name, dplyr::all_of(c(xcol, ycol)), color),
                    by = c("Source" = "name")) %>%
    dplyr::rename(x = .data[[xcol]], y = .data[[ycol]], color_source = color) %>%
    dplyr::left_join(nodes %>% dplyr::select(name, dplyr::all_of(c(xcol, ycol)), color),
                    by = c("Target" = "name")) %>%
    dplyr::rename(xend = .data[[xcol]], yend = .data[[ycol]], color_target = color)

  edges$edge_color <- edges$color_target
  edges <- edges[is.finite(edges$x) & is.finite(edges$y) & is.finite(edges$xend) & is.finite(edges$yend), , drop = FALSE]

  mean_x <- mean(nodes[[xcol]], na.rm = TRUE)
  mean_y <- mean(nodes[[ycol]], na.rm = TRUE)

  max_x <- max(c(nodes[[xcol]], edges$x, edges$xend), na.rm = TRUE)
  min_x <- min(c(nodes[[xcol]], edges$x, edges$xend), na.rm = TRUE)
  max_y <- max(c(nodes[[ycol]], edges$y, edges$yend), na.rm = TRUE)
  min_y <- min(c(nodes[[ycol]], edges$y, edges$yend), na.rm = TRUE)

  dx <- max_x - min_x;  if (!is.finite(dx) || dx == 0) dx <- 1
  dy <- max_y - min_y;  if (!is.finite(dy) || dy == 0) dy <- 1
  expand_x <- dx * 0.1
  expand_y <- dy * 0.1

  # Kano wings
  t <- seq(0, 1, length.out = 300)
  spread_x <- expand_x * 8
  spread_y <- expand_y * 10
  lower_curve <- data.frame(
    x = t * spread_x - spread_x / 2 + mean_x,
    y = mean_y - spread_y * (1 - t)^2
  )
  upper_curve <- data.frame(
    x = -t * spread_x + spread_x / 2 + mean_x,
    y = mean_y + spread_y * (1 - t)^2
  )

  diag_line <- data.frame(
    x = seq(mean_x - 3 * expand_x, mean_x + 3 * expand_x, length.out = 300),
    y = seq(mean_y - 3 * expand_y, mean_y + 3 * expand_y, length.out = 300)
  )
  slope_63_5 <- tan(43.5 * pi / 180)
  diag_line_63_5 <- data.frame(
    x = seq(mean_x - 5 * expand_x, mean_x + 5 * expand_x, length.out = 300)
  )
  diag_line_63_5$y <- slope_63_5 * (diag_line_63_5$x - mean_x) + mean_y

  visual_ratio <- if (identical(xcol, "value2") && identical(ycol, "value")) 0.32 else (1 / 1.5)
  circle_data <- NULL
  if (isTRUE(add_circle)) {
    x_lower <- function(tt) ( tt * spread_x - spread_x/2 + mean_x )
    y_lower <- function(tt) ( mean_y - spread_y * (1 - tt)^2 )
    x_upper <- function(tt) ( -tt * spread_x + spread_x/2 + mean_x )
    y_upper <- function(tt) ( mean_y + spread_y * (1 - tt)^2 )

    dist2_lower <- function(tt){
      dx2 <- x_lower(tt) - mean_x
      dy2 <- (y_lower(tt) - mean_y) * visual_ratio
      dx2*dx2 + dy2*dy2
    }
    dist2_upper <- function(tt){
      dx2 <- x_upper(tt) - mean_x
      dy2 <- (y_upper(tt) - mean_y) * visual_ratio
      dx2*dx2 + dy2*dy2
    }
    min_lower <- optimize(dist2_lower, interval = c(0, 1))$objective
    min_upper <- optimize(dist2_upper, interval = c(0, 1))$objective
    circle_radius <- (min(min_lower, min_upper) ** 0.5) * 0.999
    theta <- seq(0, 2*3.141592653589793, length.out = 800)
    circle_data <- data.frame(
      x = mean_x + circle_radius * cos(theta),
      y = mean_y + (circle_radius * sin(theta)) / visual_ratio
    )
  }

  size_plot <- suppressWarnings(as.numeric(nodes[[ycol]]))
  size_plot[!is.finite(size_plot)] <- NA
  min_pos <- suppressWarnings(min(size_plot[size_plot > 0], na.rm = TRUE))
  if (!is.finite(min_pos)) min_pos <- 1e-3
  size_plot[size_plot <= 0] <- min_pos
  nodes$size_plot <- size_plot

  p_kano <- ggplot(nodes, aes(x = .data[[xcol]], y = .data[[ycol]])) +
    geom_segment(
      data = edges,
      aes(x = x, y = y, xend = xend, yend = yend),
      color = "gray60", linewidth = 0.8, alpha = 0.7
    ) +
    geom_point(aes(size = size_plot, fill = color), color = "black", shape = 21, alpha = 0.9) +
    geom_text_repel(
      aes(label = name),
      size = 3.2,
      max.overlaps = Inf,
      box.padding = 0.45,
      point.padding = 0.25,
      force = 1.2,
      force_pull = 0.2,
      min.segment.length = 0,
      segment.alpha = 0.65,
      seed = 123
    ) +
    scale_fill_identity() +
    scale_size(range = c(3, 12)) +
    geom_vline(xintercept = mean_x, linetype = "dashed", color = "red") +
    geom_hline(yintercept = mean_y, linetype = "dashed", color = "red") +
    geom_line(data = lower_curve, aes(x = x, y = y), color = "blue", linewidth = 2) +
    geom_line(data = upper_curve, aes(x = x, y = y), color = "blue", linewidth = 2) +
    geom_line(data = diag_line,  aes(x = x, y = y), color = "gray70", linetype = "dotted") +
    geom_line(data = diag_line_63_5, aes(x = x, y = y), color = "gray70", linetype = "dashed")

  if (!is.null(circle_data)) {
    p_kano <- p_kano + geom_path(data = circle_data, aes(x = x, y = y), color = "purple", linewidth = 1.1)
  }

  p_kano <- p_kano +
    coord_fixed(ratio = visual_ratio, clip = "off") +
    scale_x_continuous(limits = c(min_x - 3 * expand_x, max_x + 3 * expand_x)) +
    scale_y_continuous(limits = c(min_y - 8 * expand_y, max_y + 22 * expand_y),
                     expand = ggplot2::expansion(mult = c(0.02, 0.06))) +
    labs(title = title, x = xlab, y = ylab, size = "Dominance") +
    theme_minimal(base_family = "Microsoft JhengHei") +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), legend.position = "none")

  grDevices::png(out_png, width = 1200, height = 1200, res = 130)
  on.exit({ grDevices::dev.off() }, add = TRUE)
  print(p_kano)
  invisible(out_png)
}
# ---- Report Rmd template helper (created on-demand) ----
ensure_report_rmd_template <- function(report_rmd_path) {
  if (is.null(report_rmd_path) || !nzchar(report_rmd_path)) return(invisible(FALSE))
  dir.create(dirname(report_rmd_path), recursive = TRUE, showWarnings = FALSE)
  src <- file.path(app_dir, "report_template.Rmd")
  if (!file.exists(src)) stop("report_template.Rmd not found in app folder: ", src)
  file.copy(src, report_rmd_path, overwrite = TRUE)
  invisible(TRUE)
}
# ------------------------------------------------------------
# mtext safety override (prevents "'text' length cannot be zero")
# Some plotting code calls mtext(sprintf(...)). If sprintf returns character(0),
# graphics::mtext errors. This wrapper safely no-ops / coerces to "".
# ------------------------------------------------------------
mtext <- function(text, ...) {
  if (is.null(text) || length(text) == 0) return(invisible(NULL))
  text <- as.character(text[1])
  if (is.na(text)) text <- ""
  graphics::mtext(text, ...)
}
# ---- Load FLCA module ----
flca_loaded   <- FALSE
flca_load_err <- NULL
tryCatch({
  source(file.path(app_dir, "flca_ms_sil_module.R"), local = FALSE)

  if (exists("run_flca_ms_sil_runner", mode = "function")) {
    flca_ma_sil_runner <- function(nodes, edges0, cfg = list(), verbose = FALSE, ...) {
      run_flca_ms_sil_runner(nodes = nodes, edges0 = edges0, cfg = cfg, verbose = isTRUE(verbose))
    }
    flca_loaded <- TRUE
  } else if (exists("flca_ma_sil_runner", mode = "function")) {
    flca_loaded <- TRUE
  } else {
    flca_load_err <- "run_flca_ms_sil_runner / flca_ma_sil_runner not found in flca_ms_sil_module.R"
  }
}, error = function(e) {
  flca_load_err <- paste0("Failed to source flca_ms_sil_module.R: ", conditionMessage(e))
})
if (!isTRUE(flca_loaded)) {
  message("[WARN] FLCA module not loaded: ", flca_load_err)
  flca_ma_sil_runner <- function(...) {
    msg <- if (!is.null(flca_load_err) && nzchar(flca_load_err)) flca_load_err else "FLCA module not loaded"
    stop(msg)
  }
}

options(FLCA_SHINY_NO_SIDE_EFFECTS = TRUE)
source("renderSSplot.R", local = TRUE)  # provides render_panel()
.extract_raw_query_param <- function(search, key) {
  search <- as.character(search %||% "")
  if (!nzchar(search)) return(NA_character_)
  pat <- paste0("(?:^|[?&])", key, "=(.*)$")
  m <- regexec(pat, search, perl = TRUE)
  mm <- regmatches(search, m)[[1]]
  if (length(mm) < 2) return(NA_character_)
  val <- mm[2]
  val <- sub("^\\?", "", val)
  if (key == "pubmed_url") {
    val <- sub("&autorun=.*$", "", val, perl = TRUE)
  } else if (key == "csv_url") {
    val <- sub("&autorun=.*$", "", val, perl = TRUE)
    val <- sub("&pubmed_url=.*$", "", val, perl = TRUE)
  }
  utils::URLdecode(val)
}

.download_remote_to_file <- function(remote_url, destfile) {
  remote_url <- as.character(remote_url %||% "")
  if (!nzchar(remote_url)) return(FALSE)
  ok <- tryCatch({
    utils::download.file(remote_url, destfile = destfile, mode = "wb", quiet = TRUE, method = "libcurl")
    file.exists(destfile) && isTRUE(file.info(destfile)$size > 0)
  }, error = function(e) FALSE)
  if (ok) return(TRUE)
  ok <- tryCatch({
    con <- url(remote_url, open = "rb")
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    buf <- readBin(con, what = "raw", n = 5e7)
    writeBin(buf, destfile)
    file.exists(destfile) && isTRUE(file.info(destfile)$size > 0)
  }, error = function(e) FALSE)
  ok
}

ui <- fluidPage(
  titlePanel("FLCA Top20 Report (PubMed, WoS, and coword data)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload data (2 columns: Leader,Follower OR 3 columns: Leader,Follower,WCD)",
                accept = c(".csv", ".txt", ".tsv")),
      textInput("pubmed_url", "PubMed search hyperlink", value = "",
                placeholder = "https://pubmed.ncbi.nlm.nih.gov/?term=Tsair-Wei+Chien%5BAuthor%5D&sort=date"),
      checkboxInput("use_demo", "Use demo (country.csv) if no upload", value = FALSE),
      numericInput("topn", "Top N (sampling after FLCA)", value = 20, min = 10, max = 50),
      numericInput("per_cluster", "Major sampling: per cluster", value = 4, min = 1, max = 10),
      actionButton("run", "Generate HTML report", class = "btn-primary"),
      br(), br(),
      uiOutput("report_link"),
      downloadButton("dl_report", "Download report.html"),
      br(), br(),
      tags$hr(),
      h5("Example data"),
      downloadButton("dl_demo_example", "1. demo_edges.csv"),
      br(),
      downloadButton("dl_pubmedsummary_example", "2. pubmedsummary.txt"),
      br(),
      downloadButton("dl_reference_example", "3. referenceAMA.csv"),
      br(),
      downloadButton("dl_reference_ch_example", "4. referenceAMAChinese.csv"),
      br(),
      downloadButton("dl_reference_apa_example", "5. referenceAPA.csv"),
      br(),
      downloadButton("dl_mrt_csv_example", "6. MRT.csv"),
      br(),
      downloadButton("dl_mrt3_example", "8. MRT3column.csv"),
      br(),
      downloadButton("dl_summary_example", "9. summary-Tsair-WeiC-set.txt"),
      br(),
      downloadButton("dl_wos_example", "10. wosauthor.csv")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",

        tabPanel("Home",
          h4("Welcome"),
          p("Upload data (or choose demo), then click 'Generate HTML report'. Your report will appear in the Report tab."),
          tags$details(open = TRUE, style="border:1px solid #ddd;border-radius:12px;margin:12px 0;overflow:hidden;",
            tags$summary(style="padding:10px 14px;cursor:pointer;background:#f6f6f6;font-weight:700;",
                         "ReadMe (How to use this App)"),
            tags$div(style="padding:8px 14px 0 14px;color:#8B0000;font-weight:700;", "Note: AMA-reference inputs keep all authors plus journal; PubMed URL and WoS one-column author inputs keep only the first and last authors plus journal."),
            tags$div(style="padding:12px 14px;line-height:1.6;",
              tags$ol(
                tags$li(tags$b("Upload"), " a CSV/TXT/TSV with 2 columns (Leader, Follower), 3 columns (Leader, Follower, WCD), multi-column records, PubMed summary text, AMA references, or WoS one-column ';' data."),
                tags$li(tags$b("PubMed URL"), " paste a PubMed search hyperlink to fetch all MEDLINE summary records automatically."),
                tags$li(tags$b("Example link: CSV"), tags$a(" https://smilechien.shinyapps.io/zssplotauthor3/?autorun=1&csv_url=https://raw.githubusercontent.com/smilechien/raschonline/main/DrKan.csv", href = "https://smilechien.shinyapps.io/zssplotauthor3/?autorun=1&csv_url=https://raw.githubusercontent.com/smilechien/raschonline/main/DrKan.csv", target = "_blank")),
                tags$li(tags$b("Example link: PubMed"), tags$a(" https://smilechien.shinyapps.io/zssplotauthor3/?autorun=1&pubmed_url=https://pubmed.ncbi.nlm.nih.gov/?term=Tsair-Wei+Chien%5BAuthor%5D&sort=date", href = "https://smilechien.shinyapps.io/zssplotauthor3/?autorun=1&pubmed_url=https://pubmed.ncbi.nlm.nih.gov/?term=Tsair-Wei+Chien%5BAuthor%5D&sort=date", target = "_blank")),
                tags$li(tags$b("Run"), " click 'Generate HTML report' to compute FLCA, major sampling, and figures."),
                tags$li(tags$b("View"), " figures in the Figures tab (Network, SSplot, Kano1, Kano2, PCA, Sankey)."),
                tags$li(tags$b("Download"), " tables/figures from the Downloads tab (includes demo data for learning)."),
                tags$li(tags$b("Report"), " open the self-contained HTML report, or download report.html.")
              ),
              tags$ul(
                tags$li(tags$strong("Author rule for visual analysis:"), " AMA-reference inputs keep all authors and the journal. PubMed URL and WoS author inputs keep only the first and last authors and the journal."),
                tags$li("Top N controls how many nodes are kept after FLCA (default 20)."),
                tags$li("Major sampling per cluster controls balance across clusters."),
                tags$li("If you only want to learn the format, download the demo data first.")
              )
            )
          ),
          tags$ul(
            tags$li("Accepted formats: CSV / TXT / TSV or a PubMed search hyperlink"),
            tags$li("Input types: 2/3-column coword, multi-column records, PubMed summary, AMA references, WoS one-column ';' data"),
            tags$li("Output: self-contained HTML report + PNG figures")
          )
        ),

        tabPanel("Figures",
          tabsetPanel(
            id = "fig_tabs",
            tabPanel("Network (Top20)",
              h4("Network (Top20)"),
              imageOutput("fig_network")
            ),
            tabPanel("SS plot",
              h4("SS plot"),
              imageOutput("fig_ssplot")
            ),
            tabPanel("Kano1",
              h4("Kano1"),
              imageOutput("fig_kano1")
            ),
            tabPanel("Kano2",
              h4("Kano2"),
              imageOutput("fig_kano2")
            ),
            tabPanel("PCA",
              h4("PCA"),
              imageOutput("fig_pca")
            ),
            tabPanel("Sankey",
              h4("Sankey (Top20 nodes & relations)"),
              plotOutput("sankey_plot", height = "600px"),
              uiOutput("sankey_code_block")
            ),
            tabPanel("Chord",
              h4("Chord diagram (Top20 nodes & relations)"),
              tags$p(class = "small-note",
                     "If 'chorddiag' is installed, an interactive chord is shown; otherwise a static 'circlize' chord is used. Colors follow node clusters."),
              uiOutput("chord_ui"),
              tags$hr(),
              verbatimTextOutput("chord_debug")
            ),
            tabPanel("Slope",
              h4("Slope graph: prior vs post half-years of recent 10 years"),
              tags$p(class = "small-note",
                     "Available for PubMed summary text or PubMed URL runs when publication years are available. Red = increasing, blue = decreasing, black = stationary based on t test between prior and post half-years."),
              plotOutput("slope_plot", height = "760px"),
              tags$hr(),
              tableOutput("slope_table")
            ),
            tabPanel("Journal",
              h4("Top 10 journals over recent 10 years"),
              tags$p(class = "small-note",
                     "Shows a slopegraph and a journal × recent-10-years count table when journal and year metadata can be extracted from PubMed summary, PubMed reference, or other table-like inputs containing journal/year columns."),
              plotOutput("journal_plot", height = "760px"),
              tags$hr(),
              tableOutput("journal_table")
            ),
            tabPanel("K*K count",
              h4("Top 20 nodes: 20×20 matrix"),
              tags$p(class = "small-note",
                     "Diagonal = publication count (value). Off-diagonal = single-link WCD. Displayed values use ln(x+1), are rescaled to 0–5, rounded to integers, and any off-diagonal raw WCD > 0 is shown at least as 1."),
              tags$p(class = "small-note",
                     HTML("The first column is labeled ID and contains the Top20 node names. Profile is coded 0 for the top 10 rows and 1 for the bottom 10 rows, which can be exported for downstream Rasch analysis. Suggested follow-up: <a href='https://raschonlinez-dot-taaacoword2512.df.r.appspot.com/' target='_blank'>RaschOnline</a>.")),
              tableOutput("kk_matrix_table"),
              tags$hr(),
              h4("K×K full-edge matrix among Top 20 nodes"),
              tableOutput("kk_full_links_table"),
              tags$hr(),
              h4("K-means network (k = 3) from K×K distance"),
              tags$p(class = "small-note",
                     "Clustering uses the continuous full-edge K×K importance matrix before rounding. Importance is normalized by x/max(x), then converted to distance = 1 - normalized importance. Off-diagonal edges with positive WCD are shown in the network."),
              uiOutput("kk_network_ui")
            ),
            tabPanel("Trend/Article",
              h4("Article trend over recent 10 years"),
              tags$p(class = "small-note",
                     "Bar chart of article counts by year for the recent 10 years."),
              plotOutput("article_trend_plot", height = "480px"),
              tags$hr(),
              tableOutput("article_trend_table")
            )
          )
        ),

        tabPanel("AAC",
          if (exists("aac_ui", mode = "function")) aac_ui("aac") else tags$div("appAAC.R not loaded")
        ),

        tabPanel("Downloads",
          h4("Download demo data (for learning)"),
          downloadButton("dl_demo_data", "Download demo dataset (CSV)"),
          br(), br(),
          h4("Download tables"),
          downloadButton("dl_top20_nodes", "Download Top20 nodes (CSV)"),
          downloadButton("dl_top20_edges", "Download Top20 relations (CSV)"),
          downloadButton("dl_top20_edges_full", "Download Top20 full links (CSV)"),
          br(), br(),
          h4("Download figures"),
          downloadButton("dl_fig_network", "Download Network PNG"),
          downloadButton("dl_fig_ssplot",  "Download SSplot PNG"),
          downloadButton("dl_fig_kano1",   "Download Kano1 PNG"),
          downloadButton("dl_fig_kano2",   "Download Kano2 PNG"),
          downloadButton("dl_fig_pca",     "Download PCA PNG"),
          downloadButton("dl_fig_slope",   "Download Slope PNG"),
          downloadButton("dl_all_figs",    "Download ALL figures (ZIP)"),
          br(), br(),
          h5("Top20 nodes (preview)"),
          tableOutput("tbl_top20_nodes"),
          h5("Top20 relations (preview)"),
          tableOutput("tbl_top20_edges"),
          h5("Top20 full links (preview)"),
          tableOutput("tbl_top20_edges_full")
        ),

        tabPanel("Report",
          uiOutput("report_iframe")
        )
      )
    )
  )
)
server <- function(input, output, session) {
  carac_frozen_map <- NULL
  rv <- reactiveValues(
    doc_freq_vec = NULL,
    report_path   = NULL,
    report_prefix = NULL,
    fig_paths     = NULL,
    top20_nodes   = NULL,
    top20_edges   = NULL,
    top20_edges_full = NULL,
    sankey_url    = NULL,
    sankey_code   = NULL,
    remote_csv_path = NULL,
    pubmed_summary_path = NULL,
    raw_input_df = NULL,
    raw_input_path = NULL,
    journal_obj = NULL,
    article_trend_df = NULL,
    raw_input_name = NULL,
    autorun_fired = FALSE
  )
  if (exists("aac_server", mode = "function")) aac_server("aac", rv)
  .autorun_query_values <- reactive({
    search <- session$clientData$url_search %||% ""
    qs <- tryCatch(shiny::parseQueryString(search), error = function(e) list())
    raw_csv <- .extract_raw_query_param(search, "csv_url")
    if (is.na(raw_csv) || !nzchar(raw_csv)) raw_csv <- as.character(qs$csv_url %||% "")
    raw_pubmed <- .extract_raw_query_param(search, "pubmed_url")
    if (is.na(raw_pubmed) || !nzchar(raw_pubmed)) raw_pubmed <- as.character(qs$pubmed_url %||% "")
    list(
      autorun = !is.null(qs$autorun) && as.character(qs$autorun)[1] %in% c("1", "true", "TRUE", "yes"),
      csv_url = trimws(as.character(raw_csv %||% "")),
      pubmed_url = trimws(as.character(raw_pubmed %||% ""))
    )
  })
  
  observe({
    qs <- tryCatch(shiny::parseQueryString(session$clientData$url_search %||% ""), error = function(e) list())
    raw_csv <- .extract_raw_query_param(session$clientData$url_search %||% "", "csv_url")
    if (is.na(raw_csv) || !nzchar(raw_csv)) raw_csv <- as.character(qs$csv_url %||% "")
    if (is.na(raw_csv) || !nzchar(raw_csv)) return()
    if (!is.null(rv$remote_csv_path) && nzchar(rv$remote_csv_path)) return()
    tmp_ext <- tolower(tools::file_ext(raw_csv))
    if (!nzchar(tmp_ext)) tmp_ext <- "csv"
    tmp <- tempfile(fileext = paste0(".", tmp_ext))
    ok <- .download_remote_to_file(raw_csv, tmp)
    if (!ok) {
      showNotification("Failed to download csv_url data.", type = "error", duration = NULL)
      return()
    }
    rv$remote_csv_path <- tmp
    showNotification(paste0("Loaded remote data from csv_url: ", basename(tmp)), type = "message")
    if (!is.null(qs$autorun) && as.character(qs$autorun)[1] %in% c("1", "true", "TRUE", "yes")) {
      isolate({
        shiny::updateCheckboxInput(session, "use_demo", value = FALSE)
      })
      shiny::updateActionButton(session, "run", label = "Generate HTML report")
      later::later(function() {
        try(session$sendInputMessage("run", list(value = as.numeric(Sys.time()))), silent = TRUE)
      }, delay = 0.6)
    }
  })



  observe({
    req(session$clientData$url_search)
    qs <- tryCatch(shiny::parseQueryString(session$clientData$url_search %||% ""), error = function(e) list())
    raw_pubmed <- .extract_raw_query_param(session$clientData$url_search %||% "", "pubmed_url")
    if (is.na(raw_pubmed) || !nzchar(raw_pubmed)) raw_pubmed <- as.character(qs$pubmed_url %||% "")
    if (!is.na(raw_pubmed) && nzchar(raw_pubmed) && !identical(input$pubmed_url, raw_pubmed)) {
      shiny::updateTextInput(session, "pubmed_url", value = raw_pubmed)
    }
  })


  observe({
    qv <- .autorun_query_values()
    if (!isTRUE(qv$autorun) || isTRUE(rv$autorun_fired)) return()
    if (!nzchar(qv$pubmed_url) && !nzchar(qv$csv_url)) return()
    rv$autorun_fired <- TRUE
    isolate({ shiny::updateCheckboxInput(session, "use_demo", value = FALSE) })
    shiny::updateActionButton(session, "run", label = "Generate HTML report")
    later::later(function() {
      try(session$sendInputMessage("run", list(value = as.numeric(Sys.time()))), silent = TRUE)
    }, delay = 1.0)
  })

  observeEvent(input$run, {
    
            # ---- Scalar-safe helper (prevents knitr 'text length zero') ----
            safe1 <- function(x) {
              if (is.null(x) || length(x) == 0) return("")
              x1 <- x[[1]]
              if (is.null(x1) || length(x1) == 0) return("")
              x1 <- as.character(x1)
              if (is.na(x1) || !nzchar(x1)) return("")
              x1
            }
    withProgress(message = "Generating report...", value = 0, {
      tryCatch({
        incProgress(0.1, detail = "Loading data")
        # Prefer uploaded file if provided; otherwise fall back to URL params from the current page.
        qv <- .autorun_query_values()
        pubmed_url_now <- trimws(as.character(input$pubmed_url %||% ""))
        if (!nzchar(pubmed_url_now) && nzchar(qv$pubmed_url)) pubmed_url_now <- qv$pubmed_url
        remote_csv_now <- rv$remote_csv_path
        if ((is.null(remote_csv_now) || !nzchar(remote_csv_now) || !file.exists(remote_csv_now)) && nzchar(qv$csv_url)) {
          tmp_ext <- tolower(tools::file_ext(qv$csv_url)); if (!nzchar(tmp_ext)) tmp_ext <- "csv"
          tmp <- tempfile(fileext = paste0(".", tmp_ext))
          ok <- .download_remote_to_file(qv$csv_url, tmp)
          if (ok && file.exists(tmp)) {
            rv$remote_csv_path <- tmp
            remote_csv_now <- tmp
          }
        }
        rv$pubmed_summary_path <- NULL
        rv$raw_input_df <- NULL
        rv$raw_input_path <- NULL
        rv$journal_obj <- NULL
        rv$article_trend_df <- NULL
        rv$raw_input_name <- NULL
        rv$doc_freq_vec <- NULL
        if (!is.null(input$file) && !is.na(input$file$datapath) && nzchar(input$file$datapath)) {
          up_raw <- read_any_table(input$file$datapath)
          rv$raw_input_df <- up_raw
          rv$raw_input_path <- input$file$datapath
          rv$raw_input_name <- input$file$name %||% basename(input$file$datapath)
          if (isTRUE(.is_pubmed_summary_df(up_raw, rv$raw_input_name))) rv$pubmed_summary_path <- input$file$datapath
          dat <- smart_prepare_uploaded_data(up_raw, rv$raw_input_name)
          rv$doc_freq_vec <- .document_frequency_in_data(up_raw)
        } else if (nzchar(pubmed_url_now)) {
          pub_df <- fetch_pubmed_summary_df(pubmed_url_now)
          rv$raw_input_df <- pub_df
          rv$raw_input_path <- "pubmedsummary_from_url.txt"
          rv$raw_input_name <- "pubmedsummary_from_url.txt"
          dat <- smart_prepare_uploaded_data(pub_df, "pubmedsummary_from_url.txt")
          rv$doc_freq_vec <- .document_frequency_in_data(pub_df)
          rv$pubmed_summary_path <- save_pubmed_summary_text(pub_df)
        } else if (!is.null(remote_csv_now) && nzchar(remote_csv_now) && file.exists(remote_csv_now)) {
          remote_raw <- read_any_table(remote_csv_now)
          rv$raw_input_df <- remote_raw
          rv$raw_input_path <- remote_csv_now
          rv$raw_input_name <- basename(remote_csv_now)
          dat <- smart_prepare_uploaded_data(remote_raw, rv$raw_input_name)
          rv$doc_freq_vec <- .document_frequency_in_data(remote_raw)
        } else if (isTRUE(input$use_demo)) {
          demo_path <- file.path(app_dir, "demo", "demo_edges.csv")
          if (!file.exists(demo_path)) stop("Demo data not found: ", demo_path)
          demo_raw <- read_any_table(demo_path)
          rv$raw_input_df <- demo_raw
          rv$raw_input_path <- demo_path
          rv$raw_input_name <- basename(demo_path)
          dat <- smart_prepare_uploaded_data(demo_raw, rv$raw_input_name)
          rv$doc_freq_vec <- .document_frequency_in_data(demo_raw)
        } else {
          stop("No data uploaded. Please upload a file, paste a PubMed hyperlink, provide ?csv_url=..., or select 'Use demo'.")
        }
        # ---- Minimal numeric coercion (ensure third column is numeric or default to 1) ----
        if (ncol(dat) >= 3) {
          dat[[3]] <- suppressWarnings(as.numeric(dat[[3]]))
          dat[[3]][!is.finite(dat[[3]])] <- 1
        } else {
          dat[[3]] <- 1
        }
        incProgress(0.2, detail = "Normalizing network")
        net <- normalize_network(dat)
        # ---- Safety: force net$edges_full$WCD numeric ----
        if (!is.null(net$edges_full) && ("WCD" %in% names(net$edges_full))) {
          net$edges_full$WCD <- suppressWarnings(as.numeric(net$edges_full$WCD))
          net$edges_full$WCD[!is.finite(net$edges_full$WCD)] <- 1
        }
        incProgress(0.35, detail = "Running FLCA on full data")
        flca_cfg <- list(
          top_clusters = 5,
          base_per_cluster = input$per_cluster,
          target_n = input$topn,
          intra_delta = 2,
          inter_delta = 5,
          eps = 1e-9
        )
        flca_out <- tryCatch(
          flca_ma_sil_runner(net$nodes_base, net$edges_full, cfg = flca_cfg, verbose = FALSE),
          error = function(e) {
            stop("[FLCA] flca_ma_sil_runner failed: ", conditionMessage(e), call. = FALSE)
          }
        )
        nodes_full <- if (!is.null(flca_out$nodes_full)) flca_out$nodes_full else flca_out$nodes
        n_flca_out <- if (!is.null(nodes_full)) nrow(nodes_full) else 0
        n_pre_flca <- if (!is.null(net$nodes_base)) nrow(net$nodes_base) else 0
        if (exists('n_pre_flca') && is.finite(n_pre_flca) && n_pre_flca > 0 && n_flca_out > 0 && n_flca_out != n_pre_flca) {
          showNotification(sprintf('Warning: FLCA returned %d nodes but input had %d nodes. Check flca_ma_sil_runner for unintended truncation.', n_flca_out, n_pre_flca), type='warning', duration = NULL)
        }
        # link metrics from pre-FLCA full links
        incProgress(0.45, detail = "Computing link metrics")
        nodes_full2 <- add_link_metrics(nodes_full, net$edges_full, net$two_col_input)
        if (!is.null(nodes_full2) && !is.null(rv$doc_freq_vec) && length(rv$doc_freq_vec)) {
          kmap <- .kk_name_key(as.character(nodes_full2$name))
          dfv <- unname(rv$doc_freq_vec[kmap])
          keep_old <- is.na(dfv) | !is.finite(dfv)
          if (!("value_raw_old" %in% names(nodes_full2))) nodes_full2$value_raw_old <- nodes_full2$value
          dfv[keep_old] <- suppressWarnings(as.numeric(nodes_full2$value[keep_old]))
          nodes_full2$value <- as.numeric(dfv)
        }
        if (!is.null(nodes_full2) && !is.null(carac_frozen_map)) {
          nodes_full2$carac <- unname(carac_frozen_map[trimws(as.character(nodes_full2$name))])
          nodes_full2$carac_frozen <- nodes_full2$carac
        }
        # major sampling topN
        incProgress(0.55, detail = "Major sampling TopN")
        nodes20 <- major_sample_topN(nodes_full2, cap_limit = input$topn, per_cluster = input$per_cluster)
        if (!is.null(nodes20) && !is.null(rv$doc_freq_vec) && length(rv$doc_freq_vec)) {
          kmap20 <- .kk_name_key(as.character(nodes20$name))
          dfv20 <- unname(rv$doc_freq_vec[kmap20])
          keep_old20 <- is.na(dfv20) | !is.finite(dfv20)
          if (!("value_raw_old" %in% names(nodes20))) nodes20$value_raw_old <- nodes20$value
          dfv20[keep_old20] <- suppressWarnings(as.numeric(nodes20$value[keep_old20]))
          nodes20$value <- as.numeric(dfv20)
        }
        if (!is.null(nodes20) && !is.null(carac_frozen_map)) {
          nodes20$carac <- unname(carac_frozen_map[trimws(as.character(nodes20$name))])
          nodes20$carac_frozen <- nodes20$carac
        }
        n_pre_flca <- if (!is.null(net$nodes_base)) nrow(net$nodes_base) else 0
        n_post_flca <- if (!is.null(nodes_full2)) nrow(nodes_full2) else 0
        n_topN <- if (!is.null(nodes20)) nrow(nodes20) else 0
        tab_pre_flca  <- if (!is.null(net$nodes_base) && ('carac' %in% names(net$nodes_base))) as.data.frame(table(net$nodes_base$carac, useNA='ifany')) else data.frame(carac=NA, n=n_pre_flca)
        tab_post_flca <- if (!is.null(nodes_full2) && ('carac' %in% names(nodes_full2))) as.data.frame(table(nodes_full2$carac, useNA='ifany')) else data.frame(carac=NA, n=n_post_flca)
        tab_top20     <- if (!is.null(nodes20) && ('carac' %in% names(nodes20))) as.data.frame(table(nodes20$carac, useNA='ifany')) else data.frame(carac=NA, n=n_topN)
        # edges among top20
        edges_full20 <- net$edges_full %>%
          filter(Leader %in% nodes20$name, follower %in% nodes20$name)
        if (!is.null(carac_frozen_map) && nrow(edges_full20) > 0) {
          edges_full20$Leader_carac_frozen   <- unname(carac_frozen_map[trimws(as.character(edges_full20$Leader))])
          edges_full20$follower_carac_frozen <- unname(carac_frozen_map[trimws(as.character(edges_full20$follower))])
        }
        edges_one20 <- build_one_link_edges(edges_full20)
        # compute SS(i) and round
        incProgress(0.65, detail = "Computing SS(i) and AAC")
        # use full edge list for SS calculation to ensure proper penalties
        ss <- compute_ssi_top20(nodes20, net$edges_full)
        nodes20 <- ss$nodes20
        if (!is.null(nodes20) && !is.null(carac_frozen_map)) {
          nodes20$carac <- unname(carac_frozen_map[trimws(as.character(nodes20$name))])
          nodes20$carac_frozen <- nodes20$carac
        }
        nodes20 <- round_numeric_df(nodes20, digits = 2)
        rv$top20_nodes <- nodes20
        rv$top20_edges_full <- edges_full20
        rv$top20_edges <- edges_one20
        aac_tbl <- aac_summary_top3(nodes20, metrics = c("value", "value2", "ssi", "a_star1")) %>%
          dplyr::select(metric, AAC)
        # PCA
        incProgress(0.75, detail = "Computing PCA")
        # PCA uses full link matrix built from the same edge set as SS
        pca_obj <- pca_from_link_matrix(nodes20, net$edges_full)
        # output dir
        ts <- gsub("[: ]", "", format(Sys.time(), "%Y%m%d_%H%M%OS3"))
        out_dir <- file.path(tempdir(), paste0("flca_report_", ts))
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        fig_paths <- list(
          network = file.path(out_dir, "network_top20.png"),
          ssplot  = file.path(out_dir, "ssplot_top20.png"),
          kano1   = file.path(out_dir, "kano1_value2_vs_value.png"),
          kano2   = file.path(out_dir, "kano2_astar_vs_ssi.png"),
          pca     = file.path(out_dir, "pca_pc1_pc2.png"),
          slope   = file.path(out_dir, "slope_top20.png"),
          journal = file.path(out_dir, "journal_slope_recent10y.png"),
          article = file.path(out_dir, "article_trend_recent10y.png")
        )
        rv$fig_paths <- fig_paths
        # render PNGs
        incProgress(0.9, detail = "Rendering PNG plots")
        render_network_png(fig_paths$network, nodes20, edges_one20)
     
        render_ssplot_png(fig_paths$ssplot, nodes20, net$edges_full)  # uses render_panel with sil_df adapter
        render_kano_png(fig_paths$kano1, nodes20, edges_one20,
                        xcol = "value2", ycol = "value",
                        title = "Kano1: value2 (x) vs value (y)",
                        xlab = "Influence (value2)", ylab = "Density (value)")
        render_kano_png(fig_paths$kano2, nodes20, edges_one20,
                        xcol = "ssi", ycol = "a_star1",
                        title = "Kano2: SSi (x) vs a* (y)",
                        xlab = "SSi", ylab = "a*")
        render_pca_png(fig_paths$pca, nodes20, edges_one20, pca_obj)
        # slope object from PubMed summary / URL only
        slope_obj <- NULL
        jobj <- NULL
        article_trend_df <- NULL
        if (!is.null(rv$pubmed_summary_path) && file.exists(rv$pubmed_summary_path)) {
          term_year_long <- .parse_pubmed_summary_records(rv$pubmed_summary_path)
          if (!is.null(term_year_long) && nrow(term_year_long)) {
            slope_obj <- .compute_slope_trend_top20(term_year_long, top_terms = as.character(nodes20$name), recent_n = 10L, p_cut = 0.05)
          }
          jy <- tryCatch(.parse_pubmed_summary_journal_year(rv$pubmed_summary_path), error = function(e) NULL)
          if (!is.null(jy) && nrow(jy)) {
            jobj <- .summarize_journal_recent10y(jy, recent_n = 10L, top_n = 10L)
            article_trend_df <- .summarize_article_trend_recent10y(jy, recent_n = 10L)
          }
        }
        if ((is.null(jobj) || is.null(article_trend_df)) && isFALSE(isTRUE(.is_pubmed_summary_df(rv$raw_input_df, rv$raw_input_name))) && !is.null(rv$raw_input_df)) {
          jy2 <- tryCatch(.extract_reference_journal_year_df(rv$raw_input_df), error = function(e) NULL)
          if (!is.null(jy2) && nrow(jy2)) {
            if (is.null(jobj)) jobj <- .summarize_journal_recent10y(jy2, recent_n = 10L, top_n = 10L)
            if (is.null(article_trend_df)) article_trend_df <- .summarize_article_trend_recent10y(jy2, recent_n = 10L)
          }
        }
        rv$journal_obj <- jobj
        rv$article_trend_df <- article_trend_df
        if (!is.null(slope_obj) && !is.null(slope_obj$counts) && nrow(slope_obj$counts)) {
          p_slope <- .plot_slope_trend(slope_obj)
          if (!is.null(p_slope)) ggplot2::ggsave(fig_paths$slope, p_slope, width = 12.5, height = 8.5, dpi = 150)
        }
        if (!is.null(jobj) && !is.null(jobj$counts) && nrow(jobj$counts)) {
          p_j <- .plot_journal_slopegraph(jobj, title = "Top 10 journals over recent 10 years")
          if (!is.null(p_j)) ggplot2::ggsave(fig_paths$journal, p_j, width = 12.5, height = 8.5, dpi = 150)
        }
        if (!is.null(article_trend_df) && nrow(article_trend_df)) {
          p_a <- .plot_article_bar_recent10y(article_trend_df, title = "Article trend over recent 10 years")
          if (!is.null(p_a)) ggplot2::ggsave(fig_paths$article, p_a, width = 10, height = 4.8, dpi = 150)
        }
        # Sankey code and shareable URL from Top20 one-link edges
        sankey_payload <- NULL
        if (exists("build_sankey_for_top20", mode = "function")) {
          sankey_payload <- tryCatch(build_sankey_for_top20(nodes20, edges_one20), error = function(e) NULL)
        }
        if (is.null(sankey_payload)) {
          sankey_code <- paste(
            paste0(nodes20$name, " [", nodes20$value, "]"),
            apply(edges_one20, 1, function(r) paste0(r[[1]], " [", r[[3]], "] ", r[[2]])),
            sep = "\n"
          )
          rv$sankey_code <- sankey_code
          rv$sankey_url  <- NA_character_
        } else {
          rv$sankey_code <- safe1(sankey_payload$code)
          rv$sankey_url  <- safe1(sankey_payload$url)
        }
        # ---- Build report bundle ----
        ensure_report_rmd_template(file.path(out_dir, "report_template.Rmd"))
        report_env <- new.env(parent = globalenv())
        report_env$nodes20 <- nodes20
        report_env$edges_one20 <- edges_one20
        report_env$aac_tbl <- aac_tbl
        report_env$tab_pre_flca <- tab_pre_flca
        report_env$tab_post_flca <- tab_post_flca
        report_env$tab_top20 <- tab_top20
        report_env$n_pre_flca <- n_pre_flca
        report_env$n_post_flca <- n_post_flca
        report_env$n_topN <- n_topN
        report_env$sankey_url <- rv$sankey_url
        report_env$sankey_code <- rv$sankey_code
        report_env$fig_network <- normalizePath(fig_paths$network, winslash = "/", mustWork = FALSE)
        report_env$fig_ssplot  <- normalizePath(fig_paths$ssplot,  winslash = "/", mustWork = FALSE)
        report_env$fig_kano1   <- normalizePath(fig_paths$kano1,   winslash = "/", mustWork = FALSE)
        report_env$fig_kano2   <- normalizePath(fig_paths$kano2,   winslash = "/", mustWork = FALSE)
        report_env$fig_pca     <- normalizePath(fig_paths$pca,     winslash = "/", mustWork = FALSE)
        report_env$fig_slope   <- normalizePath(fig_paths$slope,   winslash = "/", mustWork = FALSE)
        report_env$fig_journal <- normalizePath(fig_paths$journal, winslash = "/", mustWork = FALSE)
        report_env$fig_article <- normalizePath(fig_paths$article, winslash = "/", mustWork = FALSE)
        rpt <- rmarkdown::render(
          input = file.path(out_dir, "report_template.Rmd"),
          output_file = file.path(out_dir, "report.html"),
          envir = report_env,
          quiet = TRUE
        )
        rv$report_path <- rpt
        rv$report_prefix <- out_dir
        updateTabsetPanel(session, "tabs", selected = "Report")
        incProgress(1, detail = "Done")
        showNotification("Report generated successfully.", type = "message")
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = NULL)
      })
    })
  })

  output$fig_network <- renderImage({
    req(rv$fig_paths$network)
    list(src = rv$fig_paths$network, contentType = "image/png", alt = "network")
  }, deleteFile = FALSE)
  output$fig_ssplot <- renderImage({
    req(rv$fig_paths$ssplot)
    list(src = rv$fig_paths$ssplot, contentType = "image/png", alt = "ssplot")
  }, deleteFile = FALSE)
  output$fig_kano1 <- renderImage({
    req(rv$fig_paths$kano1)
    list(src = rv$fig_paths$kano1, contentType = "image/png", alt = "kano1")
  }, deleteFile = FALSE)
  output$fig_kano2 <- renderImage({
    req(rv$fig_paths$kano2)
    list(src = rv$fig_paths$kano2, contentType = "image/png", alt = "kano2")
  }, deleteFile = FALSE)
  output$fig_pca <- renderImage({
    req(rv$fig_paths$pca)
    list(src = rv$fig_paths$pca, contentType = "image/png", alt = "pca")
  }, deleteFile = FALSE)

  output$report_link <- renderUI({
    req(rv$report_path)
    tags$a("Open generated report", href = rv$report_path, target = "_blank")
  })

  output$report_iframe <- renderUI({
    req(rv$report_path)
    tags$iframe(src = rv$report_path, style = "width:100%;height:900px;border:none;")
  })

  output$dl_report <- downloadHandler(
    filename = function() paste0("flca_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
    content = function(file) file.copy(rv$report_path, file, overwrite = TRUE)
  )

  output$slope_plot <- renderPlot({
  fp <- rv$fig_paths$slope
  validate(need(!is.null(fp) && file.exists(fp),
                "Slope graph is available only for PubMed summary text or PubMed URL runs with year metadata."))
  img <- tryCatch(png::readPNG(fp), error = function(e) NULL)
  validate(need(!is.null(img), "Failed to read slope PNG."))
  grid::grid.raster(img)
})

output$slope_table <- renderTable({
  fp <- rv$pubmed_summary_path
  validate(need(!is.null(fp) && file.exists(fp), "Slope table is available only for PubMed summary text or PubMed URL runs."))
  term_year_long <- .parse_pubmed_summary_records(fp)
  validate(need(!is.null(term_year_long) && nrow(term_year_long) > 0, "No PubMed year-term records available."))
  req(rv$top20_nodes)
  sobj <- .compute_slope_trend_top20(term_year_long, top_terms = as.character(rv$top20_nodes$name), recent_n = 10L, p_cut = 0.05)
  validate(need(!is.null(sobj) && !is.null(sobj$trend) && nrow(sobj$trend) > 0, "No slope trend table available."))
  out <- sobj$trend
  out$p_value <- signif(out$p_value, 3)
  out
}, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "xs")

output$journal_plot <- renderPlot({
  jobj <- rv$journal_obj
  if (is.null(jobj) || is.null(jobj$counts) || !nrow(jobj$counts)) {
    jy <- NULL
    if (!is.null(rv$pubmed_summary_path) && file.exists(rv$pubmed_summary_path)) {
      jy <- tryCatch(.parse_pubmed_summary_journal_year(rv$pubmed_summary_path), error = function(e) NULL)
    }
    if ((is.null(jy) || !nrow(jy)) && isFALSE(isTRUE(.is_pubmed_summary_df(rv$raw_input_df, rv$raw_input_name))) && !is.null(rv$raw_input_df)) {
      jy <- tryCatch(.extract_reference_journal_year_df(rv$raw_input_df), error = function(e) NULL)
    }
    if ((is.null(jy) || !nrow(jy)) && !is.null(rv$raw_input_path) && file.exists(rv$raw_input_path)) {
      tmp_df <- tryCatch(read_any_table(rv$raw_input_path), error = function(e) NULL)
      jy <- tryCatch(.extract_reference_journal_year_df(tmp_df), error = function(e) jy)
    }
    if (!is.null(jy) && nrow(jy)) jobj <- .summarize_journal_recent10y(jy, recent_n = 10L, top_n = 10L)
  }
  validate(need(!is.null(jobj) && !is.null(jobj$counts) && nrow(jobj$counts) > 0,
                "Available only when journal and year metadata can be extracted, e.g. PubMed summary text or reference files with year metadata."))
  p <- .plot_journal_slopegraph(jobj, title = "Top 10 journals over recent 10 years")
  validate(need(!is.null(p), "No journal-year data available."))
  print(p)
})

output$journal_table <- renderTable({
  jobj <- rv$journal_obj
  if (is.null(jobj) || is.null(jobj$summary_table) || !nrow(jobj$summary_table)) {
    jy <- NULL
    if (!is.null(rv$pubmed_summary_path) && file.exists(rv$pubmed_summary_path)) {
      jy <- tryCatch(.parse_pubmed_summary_journal_year(rv$pubmed_summary_path), error = function(e) NULL)
    }
    if ((is.null(jy) || !nrow(jy)) && isFALSE(isTRUE(.is_pubmed_summary_df(rv$raw_input_df, rv$raw_input_name))) && !is.null(rv$raw_input_df)) {
      jy <- tryCatch(.extract_reference_journal_year_df(rv$raw_input_df), error = function(e) NULL)
    }
    if ((is.null(jy) || !nrow(jy)) && !is.null(rv$raw_input_path) && file.exists(rv$raw_input_path)) {
      tmp_df <- tryCatch(read_any_table(rv$raw_input_path), error = function(e) NULL)
      jy <- tryCatch(.extract_reference_journal_year_df(tmp_df), error = function(e) jy)
    }
    if (!is.null(jy) && nrow(jy)) jobj <- .summarize_journal_recent10y(jy, recent_n = 10L, top_n = 10L)
  }
  validate(need(!is.null(jobj) && !is.null(jobj$summary_table) && nrow(jobj$summary_table) > 0,
                "Journal summary is available only for inputs with journal/year metadata."))
  out <- jobj$summary_table
  yr_cols <- setdiff(names(out), c("Journal", "Total"))
  for (nm in yr_cols) out[[nm]] <- as.integer(round(suppressWarnings(as.numeric(out[[nm]]))))
  out$Total <- as.integer(round(suppressWarnings(as.numeric(out$Total))))
  out
}, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "xs")

output$kk_matrix_table <- renderTable({
  req(rv$top20_nodes)
  raw <- .build_kk_raw_matrix(rv$top20_nodes, rv$top20_edges)
  validate(need(!is.null(raw) && nrow(raw) > 0, "No Top20 matrix available."))
  kk <- .scale_matrix_0_5(raw)
  validate(need(!is.null(kk) && nrow(kk) > 0, "No Top20 matrix available."))
  out <- as.data.frame(kk, stringsAsFactors = FALSE)
  out[] <- lapply(out, function(z) as.integer(round(suppressWarnings(as.numeric(z)))))
  prof <- c(rep(0L, min(10L, nrow(out))), rep(1L, max(0L, nrow(out) - min(10L, nrow(out)))))
  out_df <- data.frame(ID = rownames(kk), out, Profile = prof, stringsAsFactors = FALSE)
  names(out_df) <- c("ID", colnames(kk), "Profile")
  rownames(out_df) <- NULL
  out_df
}, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "xs", rownames = FALSE)

output$kk_full_links_table <- renderTable({
  req(rv$top20_nodes)
  raw <- .build_kk_raw_matrix(rv$top20_nodes, rv$top20_edges_full)
  validate(need(!is.null(raw) && nrow(raw) > 0, "No Top20 full-edge matrix available."))
  kk <- .scale_matrix_0_5(raw)
  validate(need(!is.null(kk) && nrow(kk) > 0, "No Top20 full-edge matrix available."))
  out <- as.data.frame(kk, stringsAsFactors = FALSE)
  out[] <- lapply(out, function(z) as.integer(round(suppressWarnings(as.numeric(z)))))
  prof <- c(rep(0L, min(10L, nrow(out))), rep(1L, max(0L, nrow(out) - min(10L, nrow(out)))))
  out_df <- data.frame(ID = rownames(kk), out, Profile = prof, stringsAsFactors = FALSE)
  names(out_df) <- c("ID", colnames(kk), "Profile")
  rownames(out_df) <- NULL
  out_df
}, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "xs", rownames = FALSE)

output$kk_network_ui <- renderUI({
  obj <- .build_kk_network_obj(rv$top20_nodes, rv$top20_edges_full, k = 3L)
  .render_kk_network(obj)
})

output$kk_network_plot <- renderPlot({
  obj <- .build_kk_network_obj(rv$top20_nodes, rv$top20_edges_full, k = 3L)
  validate(need(!is.null(obj) && !is.null(obj$edges) && nrow(obj$edges) > 0,
                "No off-diagonal positive WCD edges are available for network construction."))
  if (!requireNamespace("igraph", quietly = TRUE)) {
    plot.new(); text(0.5, 0.5, "igraph package is required for static K×K network plot", cex = 1.1)
    return(invisible(NULL))
  }
  nd <- obj$nodes; ed <- obj$edges
  g <- igraph::graph_from_data_frame(data.frame(from = nd$label[ed$from], to = nd$label[ed$to], weight = ed$weight), directed = FALSE,
                                     vertices = data.frame(name = nd$label, cluster = nd$cluster, value = nd$value))
  pal <- .chord_cluster_palette_map(sort(unique(nd$cluster)))
  vcols <- pal[as.character(igraph::V(g)$cluster)]
  vcols[is.na(vcols)] <- "#6495ED"
  set.seed(123)
  lay <- igraph::layout_with_fr(g, weights = igraph::E(g)$weight)
  vsize <- scales::rescale(pmax(1, igraph::V(g)$value), to = c(10, 28))
  ewidth <- scales::rescale(pmax(1, igraph::E(g)$weight), to = c(1, 8))
  plot(g, layout = lay, vertex.color = vcols, vertex.size = vsize, vertex.label = igraph::V(g)$name,
       vertex.label.cex = 0.8, edge.width = ewidth, main = "K-means network (k = 3) from K×K distance")
})

output$article_trend_plot <- renderPlot({
  validate(need(!is.null(rv$article_trend_df) && nrow(rv$article_trend_df) > 0,
                "Available only when year metadata can be extracted, e.g. PubMed summary text or reference files with years."))
  p <- .plot_article_bar_recent10y(rv$article_trend_df, title = "Article trend over recent 10 years")
  validate(need(!is.null(p), "No year data available."))
  print(p)
})

output$article_trend_table <- renderTable({
  req(rv$article_trend_df)
  rv$article_trend_df
}, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "xs")

  output$sankey_code_block <- renderUI({
    if (is.null(rv$top20_nodes) || is.null(rv$top20_edges)) {
      return(tags$p("Run analysis to generate Sankey diagram (Top20 nodes & relations)."))
    }
    tagList(
      tags$h4("SankeyMATIC code (nodes + relations)"),
      tags$details(
        tags$summary("Click to expand"),
        tags$pre(style = "max-height:300px; overflow:auto; white-space:pre-wrap;", rv$sankey_code)
      )
    )
  })

# ---- Top20 nodes / relations ----
# ---- Downloads & previews for # ---- Top20 nodes / relations ----
output$tbl_top20_nodes <- renderTable({
  req(rv$top20_nodes)
  head(rv$top20_nodes, 20)
})

output$tbl_top20_edges <- renderTable({
  req(rv$top20_edges)
  head(rv$top20_edges, 20)
})


output$tbl_top20_edges_full <- renderTable({
  req(rv$top20_edges_full)
  head(rv$top20_edges_full, 20)
})

output$sankey_plot <- renderPlot({
  # 使用 igraph 畫出 Top20 節點與關係的「Sankey 風格」網路圖：
  # - 氣泡大小：依節點 value
  # - 顏色：依 cluster/carac
  # - 邊寬：依 WCD（邊權重）
  req(rv$top20_nodes, rv$top20_edges)
  nodes_df <- rv$top20_nodes
  edges_df <- rv$top20_edges

  # 丟棄 self-loop 關係，避免看到自己指向自己的大圈圈
  follower_col <- if ("Follower" %in% names(edges_df)) "Follower" else "follower"
  edges_df$Leader <- as.character(edges_df$Leader)
  edges_df[[follower_col]] <- as.character(edges_df[[follower_col]])
  edges_no_self <- edges_df[edges_df$Leader != edges_df[[follower_col]], , drop = FALSE]

  if (nrow(edges_no_self) == 0) {
    plot.new()
    text(0.5, 0.5, "No non-self edges for Sankey plot", cex = 1.2)
    return(invisible(NULL))
  }

  # 確保節點表有 name / carac / value 欄位
  if (!("name" %in% names(nodes_df))) nodes_df$name <- as.character(nodes_df[[1]])
  nodes_df$name <- as.character(nodes_df$name)
  if (!("carac" %in% names(nodes_df))) nodes_df$carac <- NA_integer_
  if (!("value" %in% names(nodes_df))) nodes_df$value <- 1

  # 節點全集：所有出現在節點表或邊上的名字
  node_names <- unique(c(
    as.character(nodes_df$name),
    as.character(edges_no_self$Leader),
    as.character(edges_no_self[[follower_col]])
  ))
  node_df <- data.frame(name = node_names, stringsAsFactors = FALSE)

  # 把 carac / value 合併進來
  node_df <- merge(
    node_df,
    nodes_df[, c("name", "carac", "value")],
    by = "name",
    all.x = TRUE,
    sort = FALSE
  )

  # 建立有向圖，邊權重是 WCD
  edge_df2 <- data.frame(
    from = as.character(edges_no_self$Leader),
    to   = as.character(edges_no_self[[follower_col]]),
    WCD  = suppressWarnings(as.numeric(edges_no_self$WCD)),
    stringsAsFactors = FALSE
  )

  g <- igraph::graph_from_data_frame(
    d = edge_df2,
    directed = TRUE,
    vertices = node_df
  )

  # 顏色：依 carac（cluster）
  cols <- cluster_color_vec(node_df)
  igraph::V(g)$color <- cols[igraph::V(g)$name]

  # 氣泡大小：依 value
  vval <- node_df$value[match(igraph::V(g)$name, node_df$name)]
  vval[!is.finite(vval)] <- 0
  vsize <- tryCatch(
    scales::rescale(vval, to = c(12, 50)),
    error = function(e) 8
  )

  # 邊寬：依 WCD
  ewd <- edge_df2$WCD
  ewd[!is.finite(ewd)] <- 1
  ewidth <- tryCatch(
    scales::rescale(ewd, to = c(1, 8)),
    error = function(e) rep(1, length(ewd))
  )

  set.seed(123)
  lay <- igraph::layout_with_fr(g, weights = igraph::E(g)$WCD)

  plot(
    g,
    layout = lay,
    vertex.label = igraph::V(g)$name,
    vertex.label.cex = 0.8,
    vertex.size = vsize,
    edge.width = ewidth,
    edge.arrow.size = 0.35,
    main = "Sankey-style network of Top20 nodes & relations"
  )
})

output$chord_ui <- renderUI({
  req(rv$top20_nodes, rv$top20_edges)
  if (requireNamespace("chorddiag", quietly = TRUE)) {
    chorddiag::chorddiagOutput("chord_plot", width = "100%", height = "760px")
  } else {
    plotOutput("chord_plot_static", height = "760px")
  }
})

output$chord_debug <- renderText({
  req(rv$top20_nodes, rv$top20_edges)
  paste0(
    "nodes=", nrow(rv$top20_nodes),
    "; edges=", nrow(rv$top20_edges),
    "; using ", if (requireNamespace("chorddiag", quietly = TRUE)) "interactive chorddiag" else "static circlize/igraph fallback"
  )
})

output$chord_plot <- chorddiag::renderChorddiag({
  req(rv$top20_nodes, rv$top20_edges)
  ed <- rv$top20_edges
  nd <- rv$top20_nodes
  follower_col <- if ("Follower" %in% names(ed)) "Follower" else "follower"
  node_names <- as.character(nd$name)
  mat <- matrix(0, nrow = length(node_names), ncol = length(node_names), dimnames = list(node_names, node_names))
  for (i in seq_len(nrow(ed))) {
    a <- as.character(ed$Leader[i]); b <- as.character(ed[[follower_col]][i])
    w <- suppressWarnings(as.numeric(ed$WCD[i])); if (!is.finite(w)) w <- 0
    if (a %in% node_names && b %in% node_names && a != b) {
      mat[a, b] <- mat[a, b] + w
    }
  }
  pal <- cluster_color_vec(nd)
  pal <- pal[node_names]
  pal[is.na(pal)] <- "#6495ED"
  chorddiag::chorddiag(
    x = mat,
    groupColors = pal,
    type = "directional",
    showTicks = FALSE,
    showGroupnames = TRUE,
    margin = 90
  )
})

output$chord_plot_static <- renderPlot({
  req(rv$top20_nodes, rv$top20_edges)
  if (!requireNamespace("circlize", quietly = TRUE)) {
    plot.new(); text(0.5, 0.5, "Install 'circlize' or 'chorddiag' to show chord diagram", cex = 1.1)
    return(invisible(NULL))
  }
  ed <- rv$top20_edges
  nd <- rv$top20_nodes
  follower_col <- if ("Follower" %in% names(ed)) "Follower" else "follower"
  node_names <- as.character(nd$name)
  mat <- matrix(0, nrow = length(node_names), ncol = length(node_names), dimnames = list(node_names, node_names))
  for (i in seq_len(nrow(ed))) {
    a <- as.character(ed$Leader[i]); b <- as.character(ed[[follower_col]][i])
    w <- suppressWarnings(as.numeric(ed$WCD[i])); if (!is.finite(w)) w <- 0
    if (a %in% node_names && b %in% node_names && a != b) {
      mat[a, b] <- mat[a, b] + w
    }
  }
  pal <- cluster_color_vec(nd)
  grid.col <- pal[node_names]
  grid.col[is.na(grid.col)] <- "#6495ED"
  circlize::circos.clear()
  circlize::chordDiagram(mat, grid.col = grid.col, transparency = 0.2, directional = 1, direction.type = c("arrows", "diffHeight"))
  title("Chord diagram (Top20 nodes & relations)")
})

.build_kk_raw_matrix <- function(nodes, edges20) {
  if (is.null(nodes) || !is.data.frame(nodes) || !nrow(nodes)) return(NULL)
  node_names <- as.character(nodes$name)
  node_names <- node_names[!is.na(node_names) & nzchar(node_names)]
  if (!length(node_names)) return(NULL)
  mat <- matrix(0, nrow = length(node_names), ncol = length(node_names), dimnames = list(node_names, node_names))
  vv <- suppressWarnings(as.numeric(nodes$value))
  vv[!is.finite(vv)] <- 0
  diag(mat) <- vv[seq_len(min(length(vv), nrow(mat)))]
  if (is.null(edges20) || !is.data.frame(edges20) || !nrow(edges20)) {
    df0 <- as.data.frame(mat)
    names(df0) <- colnames(mat)
    rownames(df0) <- rownames(mat)
    return(df0)
  }
  nms <- names(edges20)
  from_col <- intersect(nms, c("from","From","node1","Node1","source","Source","Leader"))[1]
  to_col   <- intersect(nms, c("to","To","node2","Node2","target","Target","Follower","follower"))[1]
  w_col    <- intersect(nms, c("WCD","weight","Weight","value2","value","Value","n_pubmed","n","freq"))[1]
  if (is.na(from_col) || is.na(to_col)) {
    df0 <- as.data.frame(mat)
    names(df0) <- colnames(mat)
    rownames(df0) <- rownames(mat)
    return(df0)
  }
  if (is.na(w_col)) { edges20$..w <- 0; w_col <- "..w" }

  node_keys <- .kk_name_key(node_names)
  key_to_idx <- split(seq_along(node_keys), node_keys)
  fr <- .kk_name_key(as.character(edges20[[from_col]]))
  to <- .kk_name_key(as.character(edges20[[to_col]]))
  ww <- suppressWarnings(as.numeric(edges20[[w_col]]))
  ww[!is.finite(ww)] <- 0

  for (i in seq_along(fr)) {
    akey <- fr[i]; bkey <- to[i]
    if (is.na(akey) || is.na(bkey) || !nzchar(akey) || !nzchar(bkey)) next
    ia <- key_to_idx[[akey]]
    ib <- key_to_idx[[bkey]]
    if (length(ia) < 1 || length(ib) < 1) next
    wa <- ww[i]
    if (!is.finite(wa) || wa <= 0) next
    for (a in ia) for (b in ib) {
      if (a == b) next
      mat[a, b] <- mat[a, b] + wa
      mat[b, a] <- mat[b, a] + wa
    }
  }
  { df0 <- as.data.frame(mat); names(df0) <- colnames(mat); rownames(df0) <- rownames(mat); df0 }
}

.scale_matrix_0_5 <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)
  rn <- rownames(df)
  m <- as.matrix(df)
  storage.mode(m) <- "numeric"
  m[!is.finite(m)] <- 0
  raw_m <- m
  ln_m <- log(m + 1)
  vals <- as.numeric(ln_m)
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(df)
  vmin <- min(vals, na.rm = TRUE)
  vmax <- max(vals, na.rm = TRUE)
  if (!is.finite(vmin) || !is.finite(vmax) || identical(vmax, vmin)) {
    sm <- matrix(0L, nrow = nrow(m), ncol = ncol(m), dimnames = dimnames(m))
  } else {
    sm <- round(5 * (ln_m - vmin) / (vmax - vmin))
    sm[sm < 0] <- 0
    sm[sm > 5] <- 5
    storage.mode(sm) <- "integer"
  }
  diag(sm) <- pmax(diag(sm), as.integer(round(diag(sm))))
  off_idx <- row(raw_m) != col(raw_m)
  sm[off_idx & raw_m > 0 & sm < 1] <- 1L
  out <- as.data.frame(sm); names(out) <- colnames(sm)
  out[] <- lapply(out, function(x) as.integer(round(x)))
  rownames(out) <- rn
  out
}

.build_kk_continuous_similarity <- function(nodes, edges20) {
  raw <- .build_kk_raw_matrix(nodes, edges20)
  if (is.null(raw) || !nrow(raw)) return(NULL)
  m <- as.matrix(raw)
  storage.mode(m) <- "numeric"
  m[!is.finite(m)] <- 0
  mx <- max(m, na.rm = TRUE)
  if (!is.finite(mx) || mx <= 0) return(m)
  m / mx
}

.build_kk_network_obj <- function(nodes, edges20, k = 3L) {
  sim <- .build_kk_continuous_similarity(nodes, edges20)
  if (is.null(sim) || !nrow(sim)) return(NULL)
  node_names <- rownames(sim)
  if (is.null(node_names)) node_names <- as.character(nodes$name)
  dist_mat <- 1 - sim
  diag(dist_mat) <- 0
  km <- tryCatch(stats::kmeans(dist_mat, centers = min(k, nrow(dist_mat)), nstart = 20), error = function(e) NULL)
  cluster <- if (is.null(km)) rep(1L, nrow(dist_mat)) else km$cluster
  diag_vals <- diag(as.matrix(.build_kk_raw_matrix(nodes, edges20)))
  diag_vals[!is.finite(diag_vals)] <- 0
  nodes_df <- data.frame(
    id = seq_along(node_names),
    label = node_names,
    value = diag_vals,
    cluster = as.integer(cluster),
    stringsAsFactors = FALSE
  )
  edges_raw <- .build_kk_raw_matrix(nodes, edges20)
  mraw <- as.matrix(edges_raw)
  storage.mode(mraw) <- "numeric"
  mraw[!is.finite(mraw)] <- 0
  el <- which(row(mraw) < col(mraw) & mraw > 0, arr.ind = TRUE)
  if (!nrow(el)) return(list(nodes = nodes_df, edges = NULL, sim = sim, dist = dist_mat))
  edges_df <- data.frame(
    from = el[,1],
    to = el[,2],
    weight = mraw[el],
    stringsAsFactors = FALSE
  )
  list(nodes = nodes_df, edges = edges_df, sim = sim, dist = dist_mat)
}

.render_kk_network <- function(obj) {
  if (is.null(obj) || is.null(obj$nodes) || !nrow(obj$nodes)) return(tags$p("No K×K matrix available."))
  if (is.null(obj$edges) || !nrow(obj$edges)) return(tags$p("No off-diagonal positive WCD edges are available for network construction."))
  plotOutput("kk_network_plot", height = "760px")
}

.build_chord_matrix <- function(nodes, edges20) {
  if (is.null(nodes) || !is.data.frame(nodes) || nrow(nodes) < 2) return(NULL)
  if (is.null(edges20) || !is.data.frame(edges20) || nrow(edges20) < 1) return(NULL)
  if ("name" %in% names(nodes)) nodes$name <- gsub("<br>.*$", "", as.character(nodes$name))
  else if ("label" %in% names(nodes)) nodes$name <- gsub("<br>.*$", "", as.character(nodes$label))
  else if ("id" %in% names(nodes)) nodes$name <- as.character(nodes$id)
  else nodes$name <- as.character(nodes[[1]])
  nodes <- nodes[!is.na(nodes$name) & nzchar(nodes$name), , drop = FALSE]
  nodes <- nodes[!duplicated(nodes$name), , drop = FALSE]
  if (nrow(nodes) < 2) return(NULL)
  nms <- names(edges20)
  from_col <- intersect(nms, c("Leader","from","From","Source","source","node1","Node1"))[1]
  to_col   <- intersect(nms, c("follower","Follower","to","To","Target","target","node2","Node2"))[1]
  w_col    <- intersect(nms, c("WCD","weight","Weight","value","Value","n_pubmed","n","freq"))[1]
  if (is.na(from_col) || is.na(to_col)) return(NULL)
  if (is.na(w_col)) { edges20$.w <- 1; w_col <- ".w" }
  from <- as.character(edges20[[from_col]])
  to   <- as.character(edges20[[to_col]])
  w    <- suppressWarnings(as.numeric(edges20[[w_col]]))
  w[!is.finite(w)] <- 0
  keep <- from %in% nodes$name & to %in% nodes$name & w > 0
  from <- from[keep]; to <- to[keep]; w <- w[keep]
  if (!length(w)) return(NULL)
  mat <- matrix(0, nrow = nrow(nodes), ncol = nrow(nodes), dimnames = list(nodes$name, nodes$name))
  for (i in seq_along(w)) {
    a <- match(from[i], nodes$name)
    b <- match(to[i], nodes$name)
    if (!is.na(a) && !is.na(b) && a != b) {
      mat[a, b] <- mat[a, b] + w[i]
      mat[b, a] <- mat[b, a] + w[i]
    }
  }
  mat
}

.safe_chorddiag_widget <- function(mat, group, groupColors, groupnamePadding = 20) {
  if (!requireNamespace("chorddiag", quietly = TRUE)) return(NULL)
  tryCatch(
    chorddiag::chorddiag(
      mat,
      groupnamePadding = groupnamePadding,
      margin = 90,
      showTicks = FALSE,
      groupColors = groupColors,
      group = group,
      type = "bipartite"
    ),
    error = function(e) NULL
  )
}

.chord_cluster_base_palette <- c(
  "#FF0000", "#0000FF", "#998000", "#008000", "#800080",
  "#FFC0CB", "#000000", "#ADD8E6", "#FF4500", "#A52A2A",
  "#8B4513", "#FF8C00", "#32CD32", "#4682B4", "#9400D3",
  "#FFD700", "#C0C0C0", "#DC143C", "#1E90FF"
)

.chord_cluster_palette_map <- function(cluster_levels) {
  clv <- as.character(unique(cluster_levels))
  clv <- clv[!is.na(clv) & nzchar(clv)]
  if (!length(clv)) clv <- "1"
  cols <- .chord_cluster_base_palette[((seq_along(clv)-1) %% length(.chord_cluster_base_palette)) + 1]
  stats::setNames(cols, clv)
}

# ---- Demo/example downloads ----
output$dl_demo_data <- downloadHandler(
  filename = function() "demo_edges.csv",
  content = function(file) file.copy(file.path(app_dir, "demo", "demo_edges.csv"), file, overwrite = TRUE)
)
output$dl_demo_example <- downloadHandler(
  filename = function() "demo_edges.csv",
  content = function(file) file.copy(file.path(app_dir, "demo", "demo_edges.csv"), file, overwrite = TRUE)
)
output$dl_pubmedsummary_example <- downloadHandler(
  filename = function() "pubmedsummary.txt",
  content = function(file) file.copy(file.path(app_dir, "demo", "pubmedsummary.txt"), file, overwrite = TRUE)
)
output$dl_reference_example <- downloadHandler(
  filename = function() "referenceAMA.csv",
  content = function(file) file.copy(file.path(app_dir, "demo", "referenceAMA.csv"), file, overwrite = TRUE)
)
output$dl_reference_ch_example <- downloadHandler(
  filename = function() "referenceAMAChinese.csv",
  content = function(file) file.copy(file.path(app_dir, "demo", "referenceAMAChinese.csv"), file, overwrite = TRUE)
)
output$dl_reference_apa_example <- downloadHandler(
  filename = function() "referenceAPA.csv",
  content = function(file) file.copy(file.path(app_dir, "demo", "referenceAPA.csv"), file, overwrite = TRUE)
)
output$dl_mrt_csv_example <- downloadHandler(
  filename = function() "MRT.csv",
  content = function(file) file.copy(file.path(app_dir, "demo", "MRT.csv"), file, overwrite = TRUE)
)
output$dl_mrt3_example <- downloadHandler(
  filename = function() "MRT3column.csv",
  content = function(file) file.copy(file.path(app_dir, "demo", "MRT3column.csv"), file, overwrite = TRUE)
)
output$dl_summary_example <- downloadHandler(
  filename = function() "summary-Tsair-WeiC-set.txt",
  content = function(file) file.copy(file.path(app_dir, "demo", "summary-Tsair-WeiC-set.txt"), file, overwrite = TRUE)
)
output$dl_wos_example <- downloadHandler(
  filename = function() "wosauthor.csv",
  content = function(file) file.copy(file.path(app_dir, "demo", "wosauthor.csv"), file, overwrite = TRUE)
)

# ---- Downloads for generated outputs ----
output$dl_top20_nodes <- downloadHandler(
  filename = function() "top20_nodes.csv",
  content = function(file) utils::write.csv(rv$top20_nodes, file, row.names = FALSE)
)
output$dl_top20_edges <- downloadHandler(
  filename = function() "top20_edges.csv",
  content = function(file) utils::write.csv(rv$top20_edges, file, row.names = FALSE)
)
output$dl_top20_edges_full <- downloadHandler(
  filename = function() "top20_edges_full.csv",
  content = function(file) utils::write.csv(rv$top20_edges_full, file, row.names = FALSE)
)
output$dl_fig_network <- downloadHandler(
  filename = function() basename(rv$fig_paths$network),
  content = function(file) file.copy(rv$fig_paths$network, file, overwrite = TRUE)
)
output$dl_fig_ssplot <- downloadHandler(
  filename = function() basename(rv$fig_paths$ssplot),
  content = function(file) file.copy(rv$fig_paths$ssplot, file, overwrite = TRUE)
)
output$dl_fig_kano1 <- downloadHandler(
  filename = function() basename(rv$fig_paths$kano1),
  content = function(file) file.copy(rv$fig_paths$kano1, file, overwrite = TRUE)
)
output$dl_fig_kano2 <- downloadHandler(
  filename = function() basename(rv$fig_paths$kano2),
  content = function(file) file.copy(rv$fig_paths$kano2, file, overwrite = TRUE)
)
output$dl_fig_pca <- downloadHandler(
  filename = function() basename(rv$fig_paths$pca),
  content = function(file) file.copy(rv$fig_paths$pca, file, overwrite = TRUE)
)
output$dl_fig_slope <- downloadHandler(
  filename = function() basename(rv$fig_paths$slope),
  content = function(file) file.copy(rv$fig_paths$slope, file, overwrite = TRUE)
)
output$dl_all_figs <- downloadHandler(
  filename = function() paste0("all_figures_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
  content = function(file) {
    fps <- unlist(rv$fig_paths, use.names = FALSE)
    fps <- fps[file.exists(fps)]
    old <- setwd(dirname(fps[1]))
    on.exit(setwd(old), add = TRUE)
    utils::zip(zipfile = file, files = basename(fps))
  }
)
}

shinyApp(ui, server)
