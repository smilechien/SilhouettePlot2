# ---- Always-available helper: cannot be missing ----
ensure_dir <- function(path) {
  if (is.null(path) || !nzchar(path)) stop("out_dir is empty.")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) stop("Failed to create out_dir: ", path)
  invisible(path)
}

safe_join_name <- function(nodes, map_nm) {
  # Robust join that never relies on .$ or .data pronouns.
  if (!is.data.frame(nodes)) nodes <- tryCatch(as.data.frame(nodes), error = function(e) data.frame())
  if (!is.data.frame(map_nm)) map_nm <- tryCatch(as.data.frame(map_nm), error = function(e) data.frame())

  if (!"name" %in% names(nodes)) nodes$name <- NA_character_
  nodes$name  <- as.character(nodes$name)
  nodes$name2 <- as.character(nodes$name)
  nodes$name  <- NULL

  if (nrow(map_nm) == 0 || !all(c("name2","name") %in% names(map_nm))) {
    nodes$name <- nodes$name2
    nodes$name2 <- NULL
    return(nodes)
  }

  map_nm$name2 <- as.character(map_nm$name2)
  map_nm$name  <- as.character(map_nm$name)

  df <- dplyr::left_join(nodes, map_nm, by = "name2")
  if (!"name" %in% names(df)) df$name <- NA_character_
  df$name <- dplyr::coalesce(df$name, df$name2)
  df$name2 <- NULL
  df
}



# utils.R (PNG-first, shinyapps.io friendly)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(igraph)
  library(ggplot2)
  library(ggrepel)
})





message("[UTILS] loaded utils.R from: ", normalizePath("utils.R"))

# ------------------------------------------------------------
# safe_coalesce_name: ensure 'name' exists then coalesce with name2
# ------------------------------------------------------------
safe_coalesce_name <- function(df) {
  if (!("name2" %in% names(df))) return(df)
  if (!("name" %in% names(df))) df[["name"]] <- NA_character_
  if (!("name" %in% names(df))) df[["name"]] <- NA_character_
  if (!("name2" %in% names(df))) df[["name2"]] <- NA_character_
  df[["name"]] <- dplyr::coalesce(df[["name"]], df[["name2"]])
df
}

specified_colors <- c(
  "#FF0000", "#0000FF", "#998000", "#008000", "#800080",
  "#FFC0CB", "#000000", "#ADD8E6", "#FF4500", "#A52A2A",
  "#8B4513", "#FF8C00", "#32CD32", "#4682B4", "#9400D3",
  "#FFD700", "#C0C0C0", "#DC143C", "#1E90FF"
)

full_color_set <- function(groups) {
  # groups: vector of group labels; returns a named color vector for each unique group.
  groups <- as.character(groups)
  ug <- unique(groups)
  ug <- ug[!is.na(ug) & nzchar(ug)]
  if (!length(ug)) return(setNames(specified_colors[seq_len(1)], ""))
  base_cols <- specified_colors
  cols <- base_cols[(seq_along(ug) - 1L) %% length(base_cols) + 1L]
  stats::setNames(cols, ug)
}


read_any_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  read_lines_robust <- function(path) {
    encs <- c("UTF-8", "windows-1252", "latin1", "cp950")
    for (enc in encs) {
      out <- try(readLines(path, warn = FALSE, encoding = enc), silent = TRUE)
      if (!inherits(out, "try-error") && length(out)) {
        out2 <- suppressWarnings(iconv(out, from = enc, to = "UTF-8", sub = ""))
        out2[is.na(out2)] <- ""
        return(out2)
      }
    }
    character(0)
  }
  if (ext == "txt") return(data.frame(V1 = read_lines_robust(path), stringsAsFactors = FALSE))
  if (ext == "csv") {
    out <- try(readr::read_csv(path, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8")), silent = TRUE)
    if (!inherits(out, "try-error")) return(as.data.frame(out, stringsAsFactors = FALSE))
    out <- try(utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE), silent = TRUE)
    if (!inherits(out, "try-error")) return(as.data.frame(lapply(out, .safe_utf8), stringsAsFactors = FALSE))
    out <- try(utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "windows-1252", check.names = FALSE), silent = TRUE)
    if (!inherits(out, "try-error")) return(as.data.frame(lapply(out, .safe_utf8), stringsAsFactors = FALSE))
    out <- try(utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "CP950", check.names = FALSE), silent = TRUE)
    if (!inherits(out, "try-error")) return(as.data.frame(lapply(out, .safe_utf8), stringsAsFactors = FALSE))
    out <- try(utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "latin1", check.names = FALSE), silent = TRUE)
    if (!inherits(out, "try-error")) return(as.data.frame(lapply(out, .safe_utf8), stringsAsFactors = FALSE))
    return(data.frame(V1 = read_lines_robust(path), stringsAsFactors = FALSE))
  }
  if (ext == "tsv") {
    out <- try(readr::read_delim(path, delim = "	", show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8")), silent = TRUE)
    if (!inherits(out, "try-error")) return(as.data.frame(out, stringsAsFactors = FALSE))
    return(data.frame(V1 = read_lines_robust(path), stringsAsFactors = FALSE))
  }
  out <- try(readr::read_delim(path, delim = ",", show_col_types = FALSE), silent = TRUE)
  if (inherits(out, "try-error")) out <- try(readr::read_table(path, col_names = FALSE, show_col_types = FALSE), silent = TRUE)
  if (inherits(out, "try-error")) return(data.frame(V1 = read_lines_robust(path), stringsAsFactors = FALSE))
  as.data.frame(out, stringsAsFactors = FALSE)
}
normalize_network <- function(dat) {
  if (ncol(dat) < 2) stop("Need at least 2 columns.")

  Leader <- dat[[1]]
  follower <- dat[[2]]
  if (ncol(dat) >= 3) {
    WCD <- suppressWarnings(as.numeric(dat[[3]]))
    WCD[!is.finite(WCD)] <- 1
    two_col <- FALSE
  } else {
    WCD <- rep(1, nrow(dat))
    two_col <- TRUE
  }

  edges_full <- data.frame(
    Leader = trimws(as.character(Leader)),
    follower = trimws(as.character(follower)),
    WCD = as.numeric(WCD),
    stringsAsFactors = FALSE
  ) %>%
    filter(Leader != "", follower != "") %>%
    mutate(WCD = ifelse(is.na(WCD) | !is.finite(WCD), 1, WCD))

  attr(edges_full, "input_kind") <- attr(dat, "input_kind", exact = TRUE)
  attr(edges_full, "node_pub_count") <- attr(dat, "node_pub_count", exact = TRUE)

  nodes_base <- bind_rows(
    transmute(edges_full, name = Leader, w = WCD),
    transmute(edges_full, name = follower, w = WCD)
  ) %>%
    group_by(name) %>%
    summarise(value = sum(w, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(value), name)

  list(edges_full = edges_full, two_col_input = two_col, nodes_base = nodes_base)
}
# ---- Major sampling TopN AFTER FLCA ----
major_sample_topN <- function(nodes, cap_limit = 20L, per_cluster = 4L) {
  stopifnot(all(c("name", "value", "carac") %in% names(nodes)))

  nodes <- nodes %>% arrange(desc(value), name)

  pick1 <- nodes %>%
    group_by(carac) %>%
    arrange(desc(value), name) %>%
    slice_head(n = per_cluster) %>%
    ungroup()

  if (nrow(pick1) < cap_limit) {
    pick2 <- nodes %>%
      filter(!name %in% pick1$name) %>%
      arrange(desc(value), name) %>%
      slice_head(n = cap_limit - nrow(pick1))
    pick <- bind_rows(pick1, pick2)
  } else {
    pick <- pick1 %>% slice_head(n = cap_limit)
  }

  pick %>% arrange(desc(value), name)
}

round_numeric_df <- function(df, digits = 2) {
  for (nm in names(df)) if (is.numeric(df[[nm]])) df[[nm]] <- round(df[[nm]], digits)
  df
}

cluster_color_vec <- function(nodes20) {
  cols <- specified_colors[(as.integer(nodes20$carac) - 1) %% length(specified_colors) + 1]
  setNames(cols, nodes20$name)
}

top3_names <- function(nodes20) {
  nodes20 %>% arrange(desc(value), name) %>% slice_head(n = 3) %>% pull(name)
}

# ---- Compute node occurrence metrics from full dataset ----
# Rule used in the app:
#   value2 = sum of off-diagonal edge weights touching the node
#   self_loop = sum of diagonal/self-loop weights for the node
#   value = value2 + self_loop = total occurrence count in dataset
compute_node_occurrence_metrics <- function(edges_full, node_names = NULL) {
  if (!is.data.frame(edges_full)) edges_full <- as.data.frame(edges_full)
  if ("from" %in% names(edges_full) && !"Leader" %in% names(edges_full)) edges_full$Leader <- edges_full$from
  if ("to" %in% names(edges_full) && !"follower" %in% names(edges_full)) edges_full$follower <- edges_full$to
  if ("Follower" %in% names(edges_full) && !"follower" %in% names(edges_full)) edges_full$follower <- edges_full$Follower
  if (!"WCD" %in% names(edges_full)) edges_full$WCD <- 1

  edges_full$Leader   <- trimws(as.character(edges_full$Leader))
  edges_full$follower <- trimws(as.character(edges_full$follower))
  edges_full$WCD      <- suppressWarnings(as.numeric(edges_full$WCD))
  edges_full$WCD[!is.finite(edges_full$WCD)] <- 0

  e0 <- edges_full[!is.na(edges_full$Leader) & !is.na(edges_full$follower), , drop = FALSE]
  e0 <- e0[e0$Leader != "" & e0$follower != "", c("Leader", "follower", "WCD"), drop = FALSE]
  nm <- unique(c(node_names, e0$Leader, e0$follower))
  nm <- trimws(as.character(nm)); nm <- nm[!is.na(nm) & nzchar(nm)]; nm <- unique(nm)
  out <- data.frame(name = nm, value2 = 0, self_loop = 0, value = 0, n_dataset = 0, stringsAsFactors = FALSE)
  if (!length(nm)) return(out)

  e_off <- e0[e0$Leader != e0$follower, , drop = FALSE]
  if (nrow(e_off) > 0) {
    deg <- rbind(data.frame(name = e_off$Leader, w = e_off$WCD, stringsAsFactors = FALSE),
                 data.frame(name = e_off$follower, w = e_off$WCD, stringsAsFactors = FALSE))
    deg_sum <- aggregate(w ~ name, data = deg, FUN = function(x) sum(x, na.rm = TRUE))
    out$value2[match(deg_sum$name, out$name)] <- as.numeric(deg_sum$w)
  }
  e_self <- e0[e0$Leader == e0$follower, , drop = FALSE]
  if (nrow(e_self) > 0) {
    self_sum <- aggregate(WCD ~ Leader, data = e_self, FUN = function(x) sum(x, na.rm = TRUE))
    out$self_loop[match(self_sum$Leader, out$name)] <- as.numeric(self_sum$WCD)
  }

  kind <- attr(edges_full, "input_kind", exact = TRUE)
  pub_df <- attr(edges_full, "node_pub_count", exact = TRUE)
  if (identical(kind, "record_based") && is.data.frame(pub_df) && all(c("name","value") %in% names(pub_df))) {
    pub_df$name <- trimws(as.character(pub_df$name))
    pub_df$value <- suppressWarnings(as.numeric(pub_df$value))
    pub_df$value[!is.finite(pub_df$value)] <- 0
    out$value <- pub_df$value[match(out$name, pub_df$name)]
    out$value[is.na(out$value)] <- 0
  } else {
    out$value <- out$value2 + out$self_loop
  }
  out$n_dataset <- out$value
  out
}

add_link_metrics <- function(nodes, edges_full, two_col_input = FALSE) {
  if (!is.data.frame(nodes)) nodes <- as.data.frame(nodes)
  if (!is.data.frame(edges_full)) edges_full <- as.data.frame(edges_full)
  if (!"name" %in% names(nodes)) stop("nodes must have column: name")
  if (!"carac" %in% names(nodes)) nodes$carac <- NA_integer_
  nodes$name <- trimws(as.character(nodes$name))
  drop_cols <- intersect(c("value", "value2", "self_loop", "n_dataset"), names(nodes))
  if (length(drop_cols) > 0) nodes <- nodes[, setdiff(names(nodes), drop_cols), drop = FALSE]
  metric_df <- compute_node_occurrence_metrics(edges_full, node_names = nodes$name)
  nodes <- dplyr::left_join(nodes, metric_df, by = "name")
  nodes$value2[is.na(nodes$value2)] <- 0
  nodes$self_loop[is.na(nodes$self_loop)] <- 0
  nodes$value[is.na(nodes$value)] <- 0
  nodes$n_dataset[is.na(nodes$n_dataset)] <- nodes$value[is.na(nodes$n_dataset)]
  nodes$a_i <- NA_real_
  nodes$b_i <- NA_real_
  nodes$a_star1 <- NA_real_
  nodes
}

`%||%` <- function(a,b) if (!is.null(a) && length(a) && !is.na(a[1]) && nzchar(a[1])) a else b
.safe_utf8 <- function(x) {
  x <- as.character(x)
  conv_one <- function(s) {
    if (is.null(s) || length(s) == 0 || is.na(s) || !nzchar(s)) return("")
    for (enc in c("", "UTF-8", "windows-1252", "latin1", "CP950")) {
      y <- suppressWarnings(iconv(s, from = enc, to = "UTF-8", sub = ""))
      if (!is.na(y) && nzchar(y)) return(y)
    }
    y <- suppressWarnings(enc2utf8(s))
    if (is.na(y)) "" else y
  }
  vapply(x, conv_one, character(1), USE.NAMES = FALSE)
}
.count_unique_terms_by_row <- function(row_terms_list) {
  all_terms <- unique(unlist(row_terms_list, use.names = FALSE))
  all_terms <- trimws(as.character(all_terms)); all_terms <- all_terms[nzchar(all_terms)]
  if (!length(all_terms)) return(data.frame(name=character(0), value=numeric(0), stringsAsFactors = FALSE))
  vals <- sapply(all_terms, function(tt) sum(vapply(row_terms_list, function(v) tt %in% v, logical(1))), USE.NAMES = FALSE)
  data.frame(name = all_terms, value = as.numeric(vals), stringsAsFactors = FALSE)
}
.keep_first_last <- function(x) {
  x <- .safe_utf8(x)
  x <- trimws(x)
  x <- x[nzchar(x)]
  x <- unique(x)
  if (!length(x)) return(character(0))
  if (length(x) == 1L) return(x)
  unique(c(x[1], x[length(x)]))
}
.normalize_author_token <- function(x) {
  x <- .safe_utf8(x)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}
.split_wos_authors <- function(x, sep_pattern = "\\s*[;；]\\s*") {
  x <- .safe_utf8(x)
  out <- trimws(unlist(strsplit(x, sep_pattern, perl = TRUE), use.names = FALSE))
  out <- out[nzchar(out)]
  .normalize_author_token(out)
}
.rows_terms_to_edges <- function(row_terms_list) {
  row_terms_list <- lapply(row_terms_list, function(v) unique(trimws(as.character(v))[nzchar(trimws(as.character(v)))]))
  row_terms_list <- row_terms_list[lengths(row_terms_list) > 0]
  if (!length(row_terms_list)) stop("No valid terms found in uploaded data.")
  pub_df <- .count_unique_terms_by_row(row_terms_list)
  edges_list <- vector("list", length(row_terms_list))
  for (i in seq_along(row_terms_list)) {
    terms <- sort(row_terms_list[[i]])
    self_df <- data.frame(Leader = terms, follower = terms, WCD = 1, stringsAsFactors = FALSE)
    pair_df <- NULL
    if (length(terms) >= 2) {
      cmb <- utils::combn(terms, 2)
      pair_df <- data.frame(Leader = cmb[1,], follower = cmb[2,], WCD = 1, stringsAsFactors = FALSE)
    }
    edges_list[[i]] <- if (is.null(pair_df)) self_df else rbind(self_df, pair_df)
  }
  edges <- do.call(rbind, edges_list)
  edges <- stats::aggregate(WCD ~ Leader + follower, data = edges, FUN = sum)
  attr(edges, "input_kind") <- "record_based"
  attr(edges, "node_pub_count") <- pub_df
  edges
}
.one_col_semicolon_to_edges <- function(dat, sep_pattern = "\\s*[;；]\\s*") {
  vals <- trimws(as.character(dat[[1]])); vals <- vals[!is.na(vals) & nzchar(vals)]
  terms_list <- lapply(vals, function(x) trimws(unlist(strsplit(x, sep_pattern, perl = TRUE), use.names = FALSE)))
  .rows_terms_to_edges(terms_list)
}
.one_col_first_last_semicolon_to_edges <- function(dat, sep_pattern = "\\s*[;；]\\s*") {
  vals <- trimws(as.character(dat[[1]])); vals <- vals[!is.na(vals) & nzchar(vals)]
  terms_list <- lapply(vals, function(x) .keep_first_last(.split_wos_authors(x, sep_pattern = sep_pattern)))
  .rows_terms_to_edges(terms_list)
}
.multi_col_to_edges <- function(dat) {
  terms_list <- lapply(seq_len(nrow(dat)), function(i) {
    v <- unlist(dat[i,], use.names = FALSE); v <- trimws(as.character(v)); v[!is.na(v) & nzchar(v)]
  })
  .rows_terms_to_edges(terms_list)
}
.is_pubmed_summary_df <- function(dat, file_path = NULL) {
  nm <- tolower(basename(file_path %||% ""))
  if (grepl("pubmedsummary", nm, fixed = TRUE)) return(TRUE)
  if (!is.data.frame(dat) || ncol(dat) != 1) return(FALSE)
  x <- paste(head(as.character(dat[[1]]), 200), collapse = "\n")
  grepl("(?m)^PMID-\\s*\\d+", x, perl = TRUE)
}
.is_ama_reference_df <- function(dat, file_path = NULL) {
  nm <- tolower(basename(file_path %||% ""))
  if (grepl("reference", nm, fixed = TRUE)) return(TRUE)
  if (!is.data.frame(dat) || ncol(dat) < 1) return(FALSE)
  x <- paste(head(as.character(dat[[ncol(dat)]]), 50), collapse = "\n")
  has_year <- grepl("\\b(?:19|20)\\d{2}(?:[;/]|\\b)|\\b(?:19|20)\\d{2}/\\d{1,2}/\\d{1,2}\\b", x, perl = TRUE)
  has_doi  <- grepl("\\bdoi[: ]", x, perl = TRUE)
  has_pmid <- grepl("\\bPMID[: ]", x, perl = TRUE)
  has_journalish <- grepl("\\.[[:space:]]*[A-Z][^.]+\\.[[:space:]]*(?:19|20)\\d{2}|(?:19|20)\\d{2}/\\d{1,2}/\\d{1,2}", x, perl = TRUE)
  isTRUE((has_year && has_journalish) || has_doi || has_pmid)
}
.pubmedsummary_to_edges <- function(dat) {
  lines <- .safe_utf8(dat[[1]])
  lines <- lines[nzchar(lines)]
  if (!length(lines)) stop("Empty PubMed summary text.")
  idx <- grep("^PMID-\\s*\\d+", lines, perl = TRUE)
  if (!length(idx)) stop("No PMID records found in PubMed summary text.")
  ends <- c(idx[-1] - 1L, length(lines))
  terms_list <- vector("list", length(idx))
  for (k in seq_along(idx)) {
    rec <- lines[idx[k]:ends[k]]
    au <- trimws(sub("^AU\\s*-\\s*", "", rec[grepl("^AU\\s*-", rec)]))
    au <- .keep_first_last(.normalize_author_token(au))
    jt_lines <- rec[grepl("^JT\\s*-", rec)]
    if (!length(jt_lines)) jt_lines <- rec[grepl("^TA\\s*-", rec)]
    jt <- if (length(jt_lines)) trimws(sub("^[A-Z]{2}\\s*-\\s*", "", jt_lines[1])) else character(0)
    terms <- unique(c(au, jt))
    terms <- terms[nzchar(terms)]
    terms_list[[k]] <- terms
  }
  .rows_terms_to_edges(terms_list)
}
.extract_ama_authors <- function(ref) {
  ref <- .safe_utf8(ref)
  ref <- trimws(sub('^\\s*"|"\\s*$', '', ref, perl = TRUE))
  ref <- trimws(sub("^\\s*\\[?\\d+\\]?\\s*[.,]?\\s*", "", ref, perl = TRUE))
  segs <- trimws(unlist(strsplit(ref, "\\.", perl = TRUE), use.names = FALSE))
  segs <- segs[nzchar(segs)]
  if (!length(segs)) return(character(0))
  author_part <- segs[1]
  author_part <- sub("\\set al\\.?$", "", author_part, perl = TRUE)
  toks <- trimws(unlist(strsplit(author_part, ",", fixed = TRUE), use.names = FALSE))
  toks <- .normalize_author_token(toks)
  toks <- toks[nzchar(toks)]
  unique(toks)
}
.extract_ama_journal <- function(ref) {
  clean_journal <- function(x) {
    x <- trimws(as.character(x %||% ""))
    x <- trimws(sub("(?:available at|published|epub).*$", "", x, ignore.case = TRUE, perl = TRUE))
    x <- trimws(sub("\\b(?:19|20)\\d{2}(?:/\\d{1,2}/\\d{1,2})?.*$", "", x, perl = TRUE))
    x <- trimws(sub("[;,:].*$", "", x, perl = TRUE))
    x <- trimws(gsub("[^[:alnum:] ]+", " ", x, perl = TRUE))
    x <- trimws(gsub("\\s+", " ", x, perl = TRUE))
    x
  }

  ref2 <- .safe_utf8(ref)
  ref2 <- gsub("\\s+", " ", ref2, perl = TRUE)
  ref2 <- trimws(sub("^\\s*\"|\"\\s*$", "", ref2, perl = TRUE))
  ref2 <- trimws(sub("^\\s*\\[?\\d+\\]?\\s*[.,]?\\s*", "", ref2, perl = TRUE))
  segs <- trimws(unlist(strsplit(ref2, "\\.", perl = TRUE), use.names = FALSE))
  segs <- segs[nzchar(segs)]
  if (!length(segs)) return(NA_character_)

  year_pat <- "\\b(?:19|20)\\d{2}(?:/\\d{1,2}/\\d{1,2})?\\b"
  start_idx <- if (length(segs) >= 3) 3L else 1L
  segs2 <- segs[start_idx:length(segs)]
  year_rel <- which(grepl(year_pat, segs2, perl = TRUE))[1]

  cand <- NA_character_
  prev_cand <- NA_character_
  if (!is.na(year_rel)) {
    year_idx <- start_idx + year_rel - 1L
    seg_y <- segs[year_idx]
    pre_y <- trimws(sub(paste0("^(.*?)", year_pat, ".*$"), "\\1", seg_y, perl = TRUE))
    if (grepl("[[:alpha:]]", pre_y)) cand <- pre_y
    if (year_idx > 1) prev_cand <- segs[year_idx - 1L]
  }
  if (is.na(cand) || !nzchar(cand)) {
    cand <- prev_cand
  }
  if (is.na(cand) || !nzchar(cand)) {
    cand_set <- segs2[!grepl("^(available at|url|doi|pmid|published|epub)\\b", segs2, ignore.case = TRUE, perl = TRUE)]
    cand <- if (length(cand_set)) cand_set[1] else NA_character_
  }

  j <- clean_journal(cand)
  if ((!nzchar(j) || !grepl("[[:alpha:]]", j)) && nzchar(prev_cand)) {
    j2 <- clean_journal(prev_cand)
    if (nzchar(j2) && grepl("[[:alpha:]]", j2)) j <- j2
  }
  if (!nzchar(j)) return(NA_character_)
  j
}
.ama_reference_to_edges <- function(dat) {
  ref_col <- dat[[ncol(dat)]]
  refs <- .safe_utf8(ref_col)
  refs <- refs[nzchar(trimws(refs))]
  refs <- sub("^\\s*\\[?\\d+\\]?\\s*,?", "", refs, perl = TRUE)
  terms_list <- lapply(refs, function(ref) {
    au <- .extract_ama_authors(ref)
    jr <- .extract_ama_journal(ref)
    unique(c(au, jr)[nzchar(c(au, jr))])
  })
  .rows_terms_to_edges(terms_list)
}

.extract_query_param <- function(url, key) {
  if (is.null(url) || !nzchar(url)) return(NA_character_)
  url <- .safe_utf8(url)
  m <- regexec(paste0("(?:[?&])", key, "=([^&#]+)"), url, perl = TRUE)
  mm <- regmatches(url, m)[[1]]
  if (length(mm) < 2) return(NA_character_)
  val <- mm[2]
  val <- gsub("\\+", " ", val, perl = TRUE)
  utils::URLdecode(val)
}
.pubmed_search_url_to_term <- function(url) {
  if (is.null(url) || !nzchar(trimws(url))) stop("PubMed hyperlink is empty.")
  url <- trimws(.safe_utf8(url))
  term <- .extract_query_param(url, "term")
  if (is.na(term) || !nzchar(term)) {
    if (!grepl("^https?://", url, perl = TRUE)) term <- url
  }
  if (is.na(term) || !nzchar(term)) stop("PubMed hyperlink does not contain a term= parameter.")
  term
}
.fetch_url_text <- function(url) {
  txt <- tryCatch({
    tf <- tempfile(fileext = ".txt")
    on.exit(unlink(tf), add = TRUE)
    utils::download.file(url, tf, mode = "wb", quiet = TRUE, method = "libcurl")
    paste(readLines(tf, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }, error = function(e) "")
  if (!nzchar(txt)) {
    con <- try(url(url, open = "rt", encoding = "UTF-8"), silent = TRUE)
    if (!inherits(con, "try-error")) {
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      txt <- tryCatch(paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                      error = function(e) "")
    }
  }
  if (!nzchar(txt)) stop(paste("Failed to download URL:", url))
  .safe_utf8(txt)
}
.fetch_pubmed_esearch_ids <- function(term, max_records = 5000L) {
  esearch_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmode=json&retmax=",
    max_records,
    "&term=",
    utils::URLencode(term, reserved = TRUE)
  )
  txt <- .fetch_url_text(esearch_url)
  count <- suppressWarnings(as.integer(sub('.*"count"\\s*:\\s*"?(\\d+)"?.*', '\\1', txt, perl = TRUE)))
  id_block <- sub('.*"idlist"\\s*:\\s*\\[([^\\]]*)\\].*', '\\1', txt, perl = TRUE)
  ids <- regmatches(id_block, gregexpr('"[0-9]+"', id_block, perl = TRUE))[[1]]
  ids <- gsub('"', '', ids, fixed = TRUE)
  ids <- ids[nzchar(ids)]
  if (!is.finite(count) || is.na(count)) count <- length(ids)
  list(count = count, ids = head(ids, max_records))
}
fetch_pubmed_summary_df <- function(pubmed_url, batch_size = 200L, max_records = 5000L) {
  term <- .pubmed_search_url_to_term(pubmed_url)
  info <- .fetch_pubmed_esearch_ids(term, max_records = max_records)
  n <- min(info$count, length(info$ids), max_records)
  if (n < 1) stop("No PubMed results found for the provided hyperlink.")
  ids <- info$ids[seq_len(n)]
  lines_all <- character(0)
  starts <- seq.int(1L, n, by = batch_size)
  for (st in starts) {
    id_chunk <- ids[st:min(st + batch_size - 1L, n)]
    efetch_url <- paste0(
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&rettype=medline&retmode=text&id=",
      paste(id_chunk, collapse = ",")
    )
    txt <- .fetch_url_text(efetch_url)
    lines <- unlist(strsplit(txt, "\\r?\\n", perl = TRUE))
    lines_all <- c(lines_all, lines, "")
  }
  lines_all <- .safe_utf8(lines_all)
  data.frame(V1 = lines_all, stringsAsFactors = FALSE)
}
save_pubmed_summary_text <- function(dat, file = tempfile(fileext = ".txt")) {
  lines <- .safe_utf8(dat[[1]])
  writeLines(lines, con = file, useBytes = TRUE)
  file
}

smart_prepare_uploaded_data <- function(dat, file_path = NULL) {
  dat <- as.data.frame(lapply(as.data.frame(dat, stringsAsFactors = FALSE), .safe_utf8), stringsAsFactors = FALSE)
  if (nrow(dat) == 0) stop("Uploaded data has no rows.")
  if (.is_pubmed_summary_df(dat, file_path)) return(.pubmedsummary_to_edges(dat))
  if (.is_ama_reference_df(dat, file_path)) return(.ama_reference_to_edges(dat))
  if (ncol(dat) == 1) {
    col1 <- trimws(as.character(dat[[1]]))
    if (any(grepl("[;；]", col1, perl = TRUE), na.rm = TRUE)) {
      nm <- tolower(basename(file_path %||% ""))
      looks_like_wos_authors <- mean(grepl(",", col1) & grepl("[;；]", col1), na.rm = TRUE) > 0.5
      if (grepl("wos|author", nm) || isTRUE(looks_like_wos_authors)) return(.one_col_first_last_semicolon_to_edges(dat))
      return(.one_col_semicolon_to_edges(dat))
    }
    out <- data.frame(Leader = col1[nzchar(col1)], follower = col1[nzchar(col1)], WCD = 1, stringsAsFactors = FALSE)
    pub <- as.data.frame(table(out$Leader), stringsAsFactors = FALSE); names(pub) <- c("name","value"); pub$value <- as.numeric(pub$value)
    attr(out, "input_kind") <- "record_based"
    attr(out, "node_pub_count") <- pub
    return(out)
  }
  if (ncol(dat) > 3) return(.multi_col_to_edges(dat))
  out <- dat
  names(out)[1:2] <- c("Leader", "follower")
  if (ncol(out) >= 3) { out[[3]] <- suppressWarnings(as.numeric(out[[3]])); out[[3]][!is.finite(out[[3]])] <- 1; names(out)[3] <- "WCD" } else { out$WCD <- 1 }
  out <- out[, c("Leader","follower","WCD"), drop = FALSE]
  out$Leader <- trimws(as.character(out$Leader)); out$follower <- trimws(as.character(out$follower))
  out <- out[nzchar(out$Leader) & nzchar(out$follower), , drop = FALSE]
  attr(out, "input_kind") <- "edge_based"
  out
}
# ---- One-link edges among Top20 (sum weights by unordered pair) ----
build_one_link_edges <- function(edges_full20) {
  if (nrow(edges_full20) == 0) return(edges_full20[0, c("Leader", "follower", "WCD")])

  edges_full20 %>%
    mutate(pair = ifelse(Leader < follower,
                         paste(Leader, follower, sep = "||"),
                         paste(follower, Leader, sep = "||"))) %>%
    group_by(pair) %>%
    summarise(
      Leader = first(Leader),
      follower = first(follower),
      WCD = sum(WCD, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(Leader, follower, WCD)
}

# ---- 20x20 link matrix (Top20) ----
make_link_matrix <- function(nodes20, edges_full20) {
  nm <- nodes20$name
  idx <- setNames(seq_along(nm), nm)
  M <- matrix(0, nrow = length(nm), ncol = length(nm), dimnames = list(nm, nm))

  if (nrow(edges_full20) == 0) return(M)

  for (k in seq_len(nrow(edges_full20))) {
    a <- edges_full20$Leader[k]
    b <- edges_full20$follower[k]
    w <- edges_full20$WCD[k]
    if (!a %in% nm || !b %in% nm) next
    i <- idx[[a]]
    j <- idx[[b]]
    M[i, j] <- M[i, j] + w
    M[j, i] <- M[j, i] + w
  }
  M
}

# ---- SS(i): simple silhouette-like score from link-matrix distance ----
# ---- SS(i): use silhouetteScore3 (raschonline) ----
compute_ssi_top20 <- function(nodes20, edges_full, intra_delta = 2, inter_delta = 5, eps = 1e-9) {
  # Compute silhouette-like metrics for the Top20 nodes using the OneLink
  # SS method with penalties. The adjacency weights (WCD) are
  # converted to distances by taking their reciprocal; missing edges
  # within a cluster incur a penalty of (1 + intra_delta) times the
  # maximum finite cost, while missing edges across clusters incur a
  # penalty of (1 + inter_delta) times the maximum cost. This
  # implementation follows the method used in silhouetteScore3.R for
  # OneLink networks.

  stopifnot(is.data.frame(nodes20), is.data.frame(edges_full))
  if (!all(c("name", "carac") %in% names(nodes20))) {
    stop("nodes20 must contain columns: name, carac")
  }
  if (!"value" %in% names(nodes20)) nodes20$value <- 1
  if (!"value2" %in% names(nodes20)) nodes20$value2 <- nodes20$value

  # Clean node attributes
  nodes20$name  <- trimws(as.character(nodes20$name))
  nodes20$carac <- suppressWarnings(as.integer(nodes20$carac))
  nodes20$value <- suppressWarnings(as.numeric(nodes20$value))
  nodes20$value2 <- suppressWarnings(as.numeric(nodes20$value2))

  # Remove duplicate names
  nodes20 <- nodes20[!duplicated(nodes20$name), , drop = FALSE]
  nms <- nodes20$name
  n   <- length(nms)

  # Return default values if fewer than 2 nodes
  if (n < 2) {
    nodes20$ssi <- 0
    nodes20$a_i <- 0
    nodes20$b_i <- 0
    nodes20$a_star1 <- 1
    sil_df <- data.frame(
      name = nodes20$name,
      name2 = nodes20$name,
      sil_width = 0,
      carac = nodes20$carac,
      value = nodes20$value,
      value2 = nodes20$value2,
      a_i = 0,
      b_i = 0,
      neighbor_name = NA_character_,
      neighborC = NA_integer_,
      role = "none",
      wsel = NA_real_,
      stringsAsFactors = FALSE
    )
    return(list(nodes20 = nodes20, sil_df = sil_df, res = list(W_km = NULL, D_sym = NULL)))
  }

  # Normalize edges: unify Leader/follower names and WCD weights
  ef <- edges_full
  if ("from" %in% names(ef) && !"Leader" %in% names(ef)) ef$Leader <- ef$from
  if ("to" %in% names(ef) && !"follower" %in% names(ef)) ef$follower <- ef$to
  if ("Follower" %in% names(ef) && !"follower" %in% names(ef)) ef$follower <- ef$Follower
  if (!"WCD" %in% names(ef)) ef$WCD <- 1

  ef$Leader   <- trimws(as.character(ef$Leader))
  ef$follower <- trimws(as.character(ef$follower))
  ef$WCD      <- suppressWarnings(as.numeric(ef$WCD))
  ef$WCD[!is.finite(ef$WCD)] <- 0

  # Remove self-loops and empty names
  ef <- ef[ef$Leader != "" & ef$follower != "", , drop = FALSE]
  ef <- ef[ef$Leader != ef$follower, , drop = FALSE]

  # Build an adjacency matrix A (weights) for the Top20 nodes
  # Initialize with NA (missing) to differentiate absent edges from zero weights
  A <- matrix(NA_real_, nrow = n, ncol = n, dimnames = list(nms, nms))
  diag(A) <- 0
  if (nrow(ef) > 0) {
    # Consider only edges where both endpoints are in Top20
    ef_sub <- ef[ef$Leader %in% nms & ef$follower %in% nms, , drop = FALSE]
    if (nrow(ef_sub) > 0) {
      # Aggregate multiple edges between the same pair (sum WCD)
      key <- paste(ef_sub$Leader, ef_sub$follower, sep = "\r")
      wsum <- tapply(ef_sub$WCD, key, function(x) sum(x, na.rm = TRUE))
      parts <- strsplit(names(wsum), "\r", fixed = TRUE)
      for (i in seq_along(parts)) {
        L <- parts[[i]][1]
        F <- parts[[i]][2]
        w <- as.numeric(wsum[[i]])
        if (!is.finite(w) || w <= 0) next
        # fill both directions
        A[L, F] <- w
        A[F, L] <- w
      }
    }
  }

  # Compute cost matrix: 1/(A + eps) for existing edges; NA otherwise
  cost <- matrix(NA_real_, nrow = n, ncol = n, dimnames = list(nms, nms))
  diag(cost) <- 0
  # Finite edges to cost
  idx_finite <- which(is.finite(A) & A > 0, arr.ind = TRUE)
  if (nrow(idx_finite) > 0) {
    for (k in seq_len(nrow(idx_finite))) {
      i <- idx_finite[k, 1]
      j <- idx_finite[k, 2]
      w <- A[i, j]
      cost[i, j] <- 1 / (w + eps)
    }
  }
  # Determine maximum finite cost
  finite_costs <- cost[is.finite(cost) & cost > 0]
  maxc <- if (length(finite_costs) > 0) max(finite_costs, na.rm = TRUE) else 1

  # membership vector
  memb <- nodes20$carac
  # Create a matrix of same-cluster indicators
  same_cluster <- outer(memb, memb, `==`)
  # Fill missing entries with penalty
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      if (!is.finite(cost[i, j])) {
        if (same_cluster[i, j]) {
          cost[i, j] <- maxc * (1 + intra_delta)
        } else {
          cost[i, j] <- maxc * (1 + inter_delta)
        }
      }
    }
  }

  # Symmetrize cost matrix using minimum of both directions
  cost_sym <- pmin(cost, t(cost))
  diag(cost_sym) <- 0

  # Compute a_i and b_i from cost_sym
  a_i <- numeric(n)
  b_i <- numeric(n)
  unique_clusters <- unique(memb)
  for (i in seq_len(n)) {
    # same cluster (excluding self)
    same_idx <- which(memb == memb[i] & seq_len(n) != i)
    if (length(same_idx) > 0) {
      a_i[i] <- mean(cost_sym[i, same_idx], na.rm = TRUE)
    } else {
      a_i[i] <- 0
    }
    # other clusters
    other_clusters <- setdiff(unique_clusters, memb[i])
    if (length(other_clusters) > 0) {
      b_candidates <- sapply(other_clusters, function(cc) {
        idx <- which(memb == cc)
        mean(cost_sym[i, idx], na.rm = TRUE)
      })
      b_i[i] <- suppressWarnings(min(b_candidates, na.rm = TRUE))
      if (!is.finite(b_i[i])) b_i[i] <- 0
    } else {
      b_i[i] <- 0
    }
  }
  names(a_i) <- nms
  names(b_i) <- nms

  # Compute silhouette scores
  ssi <- numeric(n)
  for (i in seq_len(n)) {
    denom <- max(a_i[i], b_i[i])
    if (is.finite(denom) && denom > 0) {
      ssi[i] <- (b_i[i] - a_i[i]) / denom
    } else {
      ssi[i] <- 0
    }
  }
  names(ssi) <- nms

  # Populate nodes20
  nodes20$a_i <- as.numeric(a_i[nodes20$name])
  nodes20$b_i <- as.numeric(b_i[nodes20$name])
  nodes20$a_star1 <- ifelse(is.finite(nodes20$a_i), 1 / (1 + nodes20$a_i), NA_real_)
  nodes20$ssi <- as.numeric(ssi[nodes20$name])

  # Derive neighbor information based on observed weights in A
  # For each node i, find the neighbor j (within Top20) with the largest
  # aggregated WCD weight. Use this to populate neighbor_name, neighborC,
  # wsel (weight of selected edge) and a simple role indicator. If no
  # neighbors exist (i.e. all entries are NA), leave fields as NA.
  neighbor_name <- rep(NA_character_, n)
  neighborC    <- rep(NA_integer_,   n)
  wsel         <- rep(NA_real_,       n)
  role         <- rep("none",        n)
  # Precompute row sums of A for role heuristic
  row_sum <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    # exclude self
    wrow <- as.numeric(A[i, ])
    wrow[i] <- NA
    # row sum for role heuristic
    row_sum[i] <- sum(wrow[is.finite(wrow)], na.rm = TRUE)
    # find neighbor with maximum WCD weight
    if (all(is.na(wrow))) next
    j <- which.max(wrow)
    if (length(j) == 0 || !is.finite(wrow[j])) next
    neighbor_name[i] <- nms[j]
    neighborC[i]    <- memb[j]
    wsel[i]         <- wrow[j]
  }
  # Determine simple role: if both nodes have finite row_sum, the one with
  # larger row_sum is considered the leader. Otherwise leave as "none".
  for (i in seq_len(n)) {
    nb_name <- neighbor_name[i]
    if (is.na(nb_name) || !(nb_name %in% nms)) next
    j <- match(nb_name, nms)
    if (is.finite(row_sum[i]) && is.finite(row_sum[j])) {
      if (row_sum[i] >= row_sum[j]) {
        role[i] <- "leader"
      } else {
        role[i] <- "follower"
      }
    }
  }

  sil_df <- data.frame(
    name = nodes20$name,
    name2 = nodes20$name,
    sil_width = as.numeric(ssi[nodes20$name]),
    carac = nodes20$carac,
    value = nodes20$value,
    value2 = nodes20$value2,
    a_i = nodes20$a_i,
    b_i = nodes20$b_i,
    neighbor_name = neighbor_name[match(nodes20$name, nms)],
    neighborC = neighborC[match(nodes20$name, nms)],
    role = role[match(nodes20$name, nms)],
    wsel = wsel[match(nodes20$name, nms)],
    stringsAsFactors = FALSE
  )

  list(nodes20 = nodes20, sil_df = sil_df, res = list(W_km = A, D_sym = cost_sym))
}

# ---- PCA from Top20 link matrix ----
pca_from_link_matrix <- function(nodes20, edges_full20) {
  M <- make_link_matrix(nodes20, edges_full20)
  prcomp(M, center = TRUE, scale. = FALSE)
}

# ---- AAC from top3 values (NOT eigenvalues) ----
aac_from_top3 <- function(x) {
  x <- sort(as.numeric(x), decreasing = TRUE)
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NA_real_)
  v1 <- x[1]; v2 <- x[2]; v3 <- x[3]
  r <- (v1/v2) / (v2/v3)
  r / (1 + r)
}

aac_summary_top3 <- function(nodes20, metrics = c("value", "value2", "ssi", "a_star1")) {
  tibble::tibble(
    metric = metrics,
    AAC = vapply(metrics, function(m) aac_from_top3(nodes20[[m]]), numeric(1))
  ) %>%
    mutate(AAC = round(AAC, 2))
}

# ---- SankeyMATIC code including nodes (self-loops) + relations ----

sankey_build <- function(nodes20, edges_one20) {
  # nodes20: data.frame with at least $name, and ideally $carac
  # edges_one20: data.frame with Leader, follower, WCD (Top20 relations)
  # SankeyMATIC code is generated via make_sankeymatic_code() from sankey.R when available,
  # and falls back to a simple flow-only code otherwise.

  # Defensive: ensure data.frames
  if (is.null(nodes20) || !is.data.frame(nodes20)) {
    nodes20 <- data.frame(name = character(0), carac = character(0), value = numeric(0))
  }
  if (is.null(edges_one20) || !is.data.frame(edges_one20)) {
    edges_one20 <- data.frame(Leader = character(0), follower = character(0), WCD = numeric(0))
  }

  # Standardize node columns
  if (!"name" %in% names(nodes20)) nodes20$name <- NA_character_
  if (!"carac" %in% names(nodes20)) nodes20$carac <- ""
  nd <- data.frame(
    name  = as.character(nodes20$name),
    carac = as.character(nodes20$carac),
    stringsAsFactors = FALSE
  )

  # Standardize edge columns
  if (!"Leader"   %in% names(edges_one20)) edges_one20$Leader   <- NA_character_
  if (!"follower" %in% names(edges_one20)) edges_one20$follower <- NA_character_
  if (!"WCD"      %in% names(edges_one20)) edges_one20$WCD      <- NA_real_

  lf_edges <- data.frame(
    Leader   = as.character(edges_one20$Leader),
    follower = as.character(edges_one20$follower),
    WCD      = suppressWarnings(as.numeric(edges_one20$WCD)),
    stringsAsFactors = FALSE
  )

  # Drop self-loops and empty endpoints
  lf_edges <- lf_edges[
    lf_edges$Leader != lf_edges$follower &
      nzchar(lf_edges$Leader) &
      nzchar(lf_edges$follower),
    ,
    drop = FALSE
  ]

  # Nothing to encode
  if (!nrow(lf_edges)) {
    return(list(url = "", url_i = "", code = ""))
  }

  # Prefer make_sankeymatic_code from sankey.R (includes node colors)
  code <- ""
  if (exists("make_sankeymatic_code")) {
    code <- tryCatch(
      make_sankeymatic_code(nodes = nd, lf_edges = lf_edges),
      error = function(e) ""
    )
  }
  # Fallback: simple Leader [WCD] follower lines
  if (!nzchar(code)) {
    flow_lines <- sprintf(
      "%s [%s] %s",
      lf_edges$Leader,
      signif(ifelse(is.finite(lf_edges$WCD), lf_edges$WCD, 1), 3),
      lf_edges$follower
    )
    code <- paste(flow_lines, collapse = "\n")
  }

  if (!nzchar(code)) {
    return(list(url = "", url_i = "", code = ""))
  }

  # Simple ?s= variant (raw text)
  url_s <- paste0(
    "https://sankeymatic.com/build/?s=",
    utils::URLencode(code, reserved = TRUE)
  )

  # Compressed ?i= variant (gzip + base64url) when base64enc is available
  url_i <- ""
  if (requireNamespace("base64enc", quietly = TRUE)) {
    raw <- charToRaw(code)
    gz  <- memCompress(raw, type = "gzip")
    b64 <- base64enc::base64encode(gz)
    # base64url: + -> -, / -> _, strip =
    b64u <- gsub("\\+", "-", b64)
    b64u <- gsub("/", "_", b64u)
    b64u <- gsub("=+$", "", b64u)
    payload <- paste0("g-", b64u)
    url_i <- paste0(
      "https://sankeymatic.com/build/?i=",
      utils::URLencode(payload, reserved = TRUE)
    )
  }

  list(
    url   = url_s,
    url_i = url_i,
    code  = code
  )
}

# ---- Plot helpers (PNG-first) ----
render_network_png <- function(path, nodes20, edges_one20) {
  g <- igraph::graph_from_data_frame(edges_one20, directed = FALSE, vertices = nodes20)
  set.seed(1)
  lay <- igraph::layout_with_fr(g)
  cols <- cluster_color_vec(nodes20)
  top3 <- top3_names(nodes20)

  V(g)$label <- ifelse(V(g)$name %in% top3, V(g)$name, NA)

  # match sizes to vertex order (igraph vertex order != nodes20 row order)
  vval <- nodes20$value[match(V(g)$name, nodes20$name)]
  vval[!is.finite(vval)] <- 0

  png(path, width = 1800, height = 1200, res = 160)
  plot(
    g,
    layout = lay,
    vertex.color = cols[V(g)$name],
    vertex.size = scales::rescale(vval, to = c(12, 56)),
    vertex.label.cex = 1.7,
    vertex.label.font = 2,
    edge.width = if (ecount(g) > 0) scales::rescale(E(g)$WCD, to = c(1, 10)) else 1,
    main = "Network (Top20)"
  )
  dev.off()
}

kano_wings_data <- function(mean_x, mean_y, radius, spread_x_mult = 5, spread_y_mult = 5.7, n = 300) {
  t <- seq(0, 1, length.out = n)
  spread_x <- spread_x_mult * radius
  spread_y <- spread_y_mult * radius

  lower_curve <- data.frame(
    x = t * spread_x - spread_x / 2 + mean_x,
    y = mean_y - spread_y * (1 - t)^2
  )
  upper_curve <- data.frame(
    x = -t * spread_x + spread_x / 2 + mean_x,
    y = mean_y + spread_y * (1 - t)^2
  )
  list(lower = lower_curve, upper = upper_curve)
}

render_kano_png <- function(path, nodes20, edges_one20, xcol, ycol, title, xlab, ylab, add_circle = TRUE) {

  df <- nodes20
  if (is.null(df)) df <- data.frame()
  if (!is.data.frame(df)) df <- tryCatch(as.data.frame(df), error = function(e) data.frame())

  # Ensure required columns exist
  if (!("name" %in% names(df))) df[["name"]] <- NA_character_
  if (!("carac" %in% names(df))) df[["carac"]] <- NA_integer_
  if (!("value" %in% names(df))) df[["value"]] <- 1

  df[["x"]] <- suppressWarnings(as.numeric(df[[xcol]]))
  df[["y"]] <- suppressWarnings(as.numeric(df[[ycol]]))
  df[["color"]] <- specified_colors[(as.integer(df[["carac"]]) - 1) %% length(specified_colors) + 1]

  mean_x <- mean(df[["x"]], na.rm = TRUE)
  mean_y <- mean(df[["y"]], na.rm = TRUE)

  radius <- 0.18 * max(stats::sd(df[["x"]], na.rm = TRUE), stats::sd(df[["y"]], na.rm = TRUE), 1)
  wings <- kano_wings_data(mean_x, mean_y, radius)

  circle <- NULL
  if (isTRUE(add_circle)) {
    t <- seq(0, 2*pi, length.out = 200)
    circle <- data.frame(x = mean_x + radius*cos(t), y = mean_y + radius*sin(t))
  }

  top3 <- top3_names(df)

  # edges in Kano space (base lookup; no .data)
  if (!is.null(edges_one20) && is.data.frame(edges_one20) && nrow(edges_one20) > 0 &&
      all(c("Leader","follower") %in% names(edges_one20))) {

    idxL <- match(as.character(edges_one20[["Leader"]]), as.character(df[["name"]]))
    idxF <- match(as.character(edges_one20[["follower"]]), as.character(df[["name"]]))

    e <- data.frame(
      x    = df[["x"]][idxL],
      y    = df[["y"]][idxL],
      xend = df[["x"]][idxF],
      yend = df[["y"]][idxF],
      w    = if ("WCD" %in% names(edges_one20)) suppressWarnings(as.numeric(edges_one20[["WCD"]])) else 1
    )

    ok <- is.finite(e$x) & is.finite(e$y) & is.finite(e$xend) & is.finite(e$yend)
    e <- e[ok, , drop = FALSE]
  } else {
    e <- data.frame(x = numeric(), y = numeric(), xend = numeric(), yend = numeric(), w = numeric())
  }

  grDevices::png(path, width = 1800, height = 1200, res = 160)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_segment(
      data = e,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend, linewidth = w),
      inherit.aes = FALSE, color = "gray60", alpha = 0.7
    ) +
    ggplot2::geom_point(ggplot2::aes(size = value, fill = color), shape = 21, color = "black", alpha = 0.9) +
    ggrepel::geom_text_repel(
      data = df[df[["name"]] %in% top3, , drop = FALSE],
      ggplot2::aes(label = name), size = 4, max.overlaps = 50
    ) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme_minimal(base_size = 14)

  if (!is.null(wings)) {
    p <- p +
      ggplot2::geom_path(data = wings$lower, ggplot2::aes(x = x, y = y),
                         inherit.aes = FALSE, linetype = "dashed", linewidth = 0.8) +
      ggplot2::geom_path(data = wings$upper, ggplot2::aes(x = x, y = y),
                         inherit.aes = FALSE, linetype = "dashed", linewidth = 0.8)
  }
  if (!is.null(circle)) {
    p <- p + ggplot2::geom_path(data = circle, ggplot2::aes(x = x, y = y),
                                inherit.aes = FALSE, linetype = "dotted", linewidth = 0.8)
  }

  print(p)
  grDevices::dev.off()
  invisible(path)
}

render_pca_png <- function(path, nodes20, edges_one20, pca_obj) {
  scores <- as.data.frame(pca_obj$x[, 1:2, drop = FALSE])
  scores$name <- rownames(pca_obj$x)
  scores <- scores %>% left_join(nodes20, by = "name")
  scores$color <- specified_colors[(as.integer(scores$carac) - 1) %% length(specified_colors) + 1]

  top3 <- top3_names(nodes20)

  if (nrow(edges_one20) > 0) {
    e <- edges_one20 %>%
      transmute(
        x = scores$PC1[match(Leader, scores$name)],
        y = scores$PC2[match(Leader, scores$name)],
        xend = scores$PC1[match(follower, scores$name)],
        yend = scores$PC2[match(follower, scores$name)],
        w = WCD
      ) %>%
      filter(is.finite(x) & is.finite(y) & is.finite(xend) & is.finite(yend))
  } else {
    e <- data.frame(x = numeric(), y = numeric(), xend = numeric(), yend = numeric(), w = numeric())
  }

  png(path, width = 1800, height = 1200, res = 160)
  p <- ggplot(scores, aes(x = PC1, y = PC2)) +
    geom_segment(data = e, aes(x = x, y = y, xend = xend, yend = yend, size = w),
                 inherit.aes = FALSE, color = "gray50", alpha = 0.6) +
    geom_point(aes(size = value, fill = color), shape = 21, color = "black", alpha = 0.9) +
    geom_text_repel(data = scores %>% filter(name %in% top3),
                    aes(label = name), size = 5, fontface = "bold", max.overlaps = 50) +
    scale_fill_identity() +
    scale_size(range = c(2, 16), guide = "none") +
    labs(
      title = "PCA on Top20 link-matrix (prior FLCA)",
      x = "PC1",
      y = "PC2"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", size = 20),
      axis.title = element_text(face = "bold", size = 18)
    )
  print(p)
  dev.off()
}

# ---- SS plot PNG using renderSSplot.R::render_panel (adapter: ssi -> sil_width) ----
render_ssplot_png <- function(path, nodes20, edges_full20) {
  # Use silhouette computation + original panel renderer.
  if (!exists("render_panel")) {
    stop("render_panel() not found. Please ensure renderSSplot.R is sourced.")
  }

  ss_pack <- compute_ssi_top20(nodes20, edges_full20)
  sil_df <- ss_pack$sil_df
  if (!is.data.frame(sil_df)) sil_df <- as.data.frame(sil_df)

  # align labels: sil_df has name/name2, so map nodes20 to display-name used in sil_df$name
  map_nm <- unique(sil_df[, intersect(c("name2","name"), names(sil_df)), drop = FALSE])
  if (!all(c("name2","name") %in% names(map_nm))) {
    nodes0 <- nodes20
  } else {
    nodes0 <- nodes20
    nodes0$name2 <- as.character(nodes0$name)
    nodes0$name <- NULL
    nodes0 <- dplyr::left_join(nodes0, map_nm, by = "name2")
    if (!"name" %in% names(nodes0)) nodes0$name <- nodes0$name2
    nodes0$name <- dplyr::coalesce(as.character(nodes0$name), as.character(nodes0$name2))
    nodes0$name2 <- NULL
  }

  png(path, width = 1800, height = 1200, res = 160)
  on.exit(dev.off(), add = TRUE)
  render_panel(sil_df = sil_df, nodes0 = nodes0)
}


# ---- SankeyMATIC code including nodes (self-loops) + relations ----
sankey_build <- function(nodes20, edges_one20) {
  # nodes20: data.frame with at least $name, $value
  # edges_one20: data.frame with Leader, follower, WCD (Top20 relations)
  # 新版：不再自動產生 self-loop，並且丟棄 Leader == follower 的關係，
  # 以避免在 Sankey 圖 / SankeyMATIC 中出現自我循環的大圈。

  if (is.null(nodes20) || !is.data.frame(nodes20)) {
    nodes20 <- data.frame(name = character(0), value = numeric(0))
  }
  if (is.null(edges_one20) || !is.data.frame(edges_one20)) {
    edges_one20 <- data.frame(Leader = character(0), follower = character(0), WCD = numeric(0))
  }

  # 標準化欄位
  if (!"name" %in% names(nodes20)) {
    nodes20$name <- NA_character_
  }
  nm  <- as.character(nodes20$name)
  val <- suppressWarnings(as.numeric(nodes20$value))
  if (length(val) != length(nm)) val <- rep(NA_real_, length(nm))
  val[!is.finite(val)] <- NA_real_

  if (!"Leader"   %in% names(edges_one20)) edges_one20$Leader   <- NA_character_
  if (!"follower" %in% names(edges_one20)) edges_one20$follower <- NA_character_
  if (!"WCD"      %in% names(edges_one20)) edges_one20$WCD      <- NA_real_

  edges_one20$Leader   <- as.character(edges_one20$Leader)
  edges_one20$follower <- as.character(edges_one20$follower)

  # 丟棄 self-loop 關係（Leader == follower）
  edges_clean <- edges_one20[edges_one20$Leader != edges_one20$follower, , drop = FALSE]

  lines <- character(0)

  # 只保留非 self-loop 的關係邊
  if (nrow(edges_clean) > 0) {
    for (k in seq_len(nrow(edges_clean))) {
      a <- as.character(edges_clean$Leader[k])
      b <- as.character(edges_clean$follower[k])
      w <- suppressWarnings(as.numeric(edges_clean$WCD[k]))
      if (!is.finite(w)) w <- 1
      if (!nzchar(a) || !nzchar(b)) next
      lines <- c(
        lines,
        sprintf("%s [%s] %s", a, signif(w, 3), b)
      )
    }
  }

  # 沒有任何有效邊：回傳空字串
  if (!length(lines)) {
    return(list(
      url   = "",
      url_i = "",
      code  = ""
    ))
  }

  code <- paste(lines, collapse = "\n")

  # Simple ?s= variant
  url_s <- paste0(
    "https://sankeymatic.com/build/?s=",
    utils::URLencode(code, reserved = TRUE)
  )

  # Compressed ?i= variant (gzip + base64url)
  url_i <- ""
  if (requireNamespace("base64enc", quietly = TRUE)) {
    raw <- charToRaw(code)
    gz  <- memCompress(raw, type = "gzip")
    b64 <- base64enc::base64encode(gz)
    # base64url: + -> -, / -> _, strip =
    b64u <- gsub("\\+", "-", b64)
    b64u <- gsub("/", "_", b64u)
    b64u <- gsub("=+$", "", b64u)
    payload <- paste0("g-", b64u)
    url_i <- paste0(
      "https://sankeymatic.com/build/?i=",
      utils::URLencode(payload, reserved = TRUE)
    )
  }

  list(url = url_s, url_i = url_i, code = code)
}



# ---- Sankey builder (override) ----
# This version ALWAYS outputs:
# - flow lines:  Leader [WCD] follower
# - node color lines: "# Node colors (edit freely):" + ": name color"
# and prepends a SankeyMATIC link line at the top of the code.
sankey_build <- function(nodes20, edges_one20) {
  # Defensive: ensure data.frames
  if (is.null(nodes20) || !is.data.frame(nodes20)) {
    nodes20 <- data.frame(name = character(0), carac = character(0), value = numeric(0))
  }
  if (is.null(edges_one20) || !is.data.frame(edges_one20)) {
    edges_one20 <- data.frame(Leader = character(0), follower = character(0), WCD = numeric(0))
  }

  # Standardize node columns
  if (!"name" %in% names(nodes20))  nodes20$name  <- NA_character_
  if (!"carac" %in% names(nodes20)) nodes20$carac <- ""
  nd <- data.frame(
    name  = as.character(nodes20$name),
    carac = as.character(nodes20$carac),
    stringsAsFactors = FALSE
  )
  nd <- nd[!is.na(nd$name) & nzchar(nd$name), , drop = FALSE]

  # Standardize edge columns
  if (!"Leader"   %in% names(edges_one20)) edges_one20$Leader   <- NA_character_
  if (!"follower" %in% names(edges_one20)) edges_one20$follower <- NA_character_
  if (!"WCD"      %in% names(edges_one20)) edges_one20$WCD      <- NA_real_

  lf_edges <- data.frame(
    Leader   = as.character(edges_one20$Leader),
    follower = as.character(edges_one20$follower),
    WCD      = suppressWarnings(as.numeric(edges_one20$WCD)),
    stringsAsFactors = FALSE
  )

  # Drop self-loops and empty endpoints
  lf_edges <- lf_edges[
    lf_edges$Leader != lf_edges$follower &
      nzchar(lf_edges$Leader) &
      nzchar(lf_edges$follower),
    ,
    drop = FALSE
  ]

  # Nothing to encode
  if (!nrow(lf_edges)) {
    return(list(url = "", url_i = "", code = ""))
  }

  # Flow lines: Leader [WCD] follower
  w <- lf_edges$WCD
  w[!is.finite(w)] <- 1
  flow_lines <- sprintf(
    "%s [%s] %s #000000",
    lf_edges$Leader,
    signif(w, 3),
    lf_edges$follower
  )

  # Node color lines from nodes + cluster groups
  groups <- nd$carac
  # Prefer full_color_set() when available
  if (exists("full_color_set")) {
    palette <- full_color_set(groups)
  } else {
    base_cols <- c("#FF0000", "#0000FF", "#008000", "#800080", "#FFA500", "#000000")
    groups_chr <- as.character(groups)
    ug <- unique(groups_chr)
    ug <- ug[!is.na(ug) & nzchar(ug)]
    if (!length(ug)) {
      palette <- stats::setNames(base_cols[1], "")
    } else {
      cols <- base_cols[(seq_along(ug) - 1L) %% length(base_cols) + 1L]
      palette <- stats::setNames(cols, ug)
    }
  }
  node_cols <- palette[nd$carac]
  node_cols[is.na(node_cols)] <- "#999999"
  node_color_lines <- sprintf(": %s %s", nd$name, node_cols)

  # Assemble code body: flows + node colors
  code_body <- paste(
    c(flow_lines, "", "# Node colors (edit freely):", node_color_lines),
    collapse = "\n"
  )

  if (!nzchar(code_body)) {
    return(list(url = "", url_i = "", code = ""))
  }

  # Basic ?s= URL
  url_s <- paste0(
    "https://sankeymatic.com/build/?s=",
    utils::URLencode(code_body, reserved = TRUE)
  )

  # Compressed ?i= URL (gzip + base64url) when base64enc is available
  url_i <- ""
  if (requireNamespace("base64enc", quietly = TRUE)) {
    raw <- charToRaw(code_body)
    gz  <- memCompress(raw, type = "gzip")
    b64 <- base64enc::base64encode(gz)
    # base64url: + -> -, / -> _, strip =
    b64u <- gsub("\\+", "-", b64)
    b64u <- gsub("/", "_", b64u)
    b64u <- gsub("=+$", "", b64u)
    payload <- paste0("g-", b64u)
    url_i <- paste0(
      "https://sankeymatic.com/build/?i=",
      utils::URLencode(payload, reserved = TRUE)
    )
  }

  # Prepend a comment with the SankeyMATIC link to the displayed code
  best_link <- if (nzchar(url_i)) url_i else url_s
  if (nzchar(best_link)) {
    code <- paste0(
      "# SankeyMATIC link: ", best_link, "\n\n",
      code_body
    )
  } else {
    code <- code_body
  }

  list(
    url   = url_s,
    url_i = url_i,
    code  = code
  )
}
