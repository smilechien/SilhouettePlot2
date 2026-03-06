make_sankeymatic_code <- function(nodes, lf_edges, top_n_links = 200, palette = NULL) {
  # Ensure data.frames with required columns
  if (is.null(nodes) || !is.data.frame(nodes)) {
    nodes <- data.frame(name = character(0), carac = character(0), stringsAsFactors = FALSE)
  }
  if (is.null(lf_edges) || !is.data.frame(lf_edges)) {
    lf_edges <- data.frame(Leader = character(0), follower = character(0), WCD = numeric(0), stringsAsFactors = FALSE)
  }

  nd <- data.frame(
    name  = as.character(nodes$name),
    carac = as.character(nodes$carac),
    stringsAsFactors = FALSE
  )
  nd <- nd[!is.na(nd$name) & nzchar(nd$name), , drop = FALSE]

  e <- data.frame(
    Leader   = as.character(lf_edges$Leader),
    follower = as.character(lf_edges$follower),
    WCD      = suppressWarnings(as.numeric(lf_edges$WCD)),
    stringsAsFactors = FALSE
  )
  e <- e[!is.na(e$Leader) & nzchar(e$Leader) &
           !is.na(e$follower) & nzchar(e$follower), , drop = FALSE]

  if (nrow(e) > 0) {
    e <- e[order(-e$WCD), , drop = FALSE]
    top_n <- min(top_n_links, nrow(e))
    e <- e[seq_len(top_n), , drop = FALSE]
  }

  # Colors per cluster (carac)
  cl <- unique(nd$carac)
  if (is.null(palette)) palette <- full_color_set(cl)
  color_vec <- palette[nd$carac]
  color_vec[is.na(color_vec)] <- "#808080"

  # Node color lines (deduplicate by name)
  node_color_lines <- sprintf(": %s %s", nd$name, color_vec)
  node_color_lines <- node_color_lines[!duplicated(nd$name)]

  # Flow lines (Leader [WCD] follower)
  if (nrow(e) > 0) {
    w <- e$WCD
    w[!is.finite(w)] <- 1
    flow_lines <- sprintf("%s [%s] %s",
                          e$Leader,
                          signif(w, 3),
                          e$follower)
  } else {
    flow_lines <- character(0)
  }

  paste(c(flow_lines, "", "# Node colors (edit freely):", node_color_lines),
        collapse = "\n")
}
