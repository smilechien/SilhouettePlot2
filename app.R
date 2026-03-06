# ---- Always-available helper: cannot be missing ----
ensure_dir <- function(path) {
  if (is.null(path) || !nzchar(path)) stop("out_dir is empty.")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) stop("Failed to create out_dir: ", path)
  invisible(path)
}

# ---- Simple sourcing (assuming working directory is app directory) ----
app_dir <- getwd()

try(source(file.path(app_dir, "utils.R"),  local = TRUE), silent = TRUE)
try(source(file.path(app_dir, "renderSSplot.R"), local = TRUE), silent = TRUE)
try(source(file.path(app_dir, "sankey.R"),       local = TRUE), silent = TRUE)
source("appstable.R", local = TRUE)     # <-- 加這行：恢復正確 render_ssplot_png

options(stringsAsFactors = FALSE)
# ---- Guard: avoid 'cannot change locked binding for data' ----
try({
  if (exists("data", envir = .GlobalEnv, inherits = FALSE) && bindingIsLocked("data", .GlobalEnv)) {
    unlockBinding("data", .GlobalEnv)
  }
}, silent = TRUE)
options(repos = c(CRAN="https://cloud.r-project.org"))
pkgs <- c("shiny","dplyr","rmarkdown","igraph","ggplot2","ggrepel","grid","readr")
miss <- pkgs[!vapply(pkgs, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))]
if (length(miss)) {
  stop("Missing required packages: ", paste(miss, collapse=", "),
       "
Please install them first, e.g.: install.packages(c(",
       paste(sprintf('"%s"', miss), collapse=", "), "))")
}
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(rmarkdown)
  library(igraph)
})


# ============================================================
# Kano plot renderer (beautiful + safe) - overrides utils.R
# Produces the style similar to the reference Kano plot.
# ============================================================
render_kano_png <- function(out_png, nodes, data, xcol = "value2", ycol = "value",
                            title = "Kano plot", xlab = "Edge(Influence)", ylab = "Density(Dominance)",
                            add_circle = TRUE) {
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

  visual_ratio <- 1 / 1.5
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
    geom_text_repel(aes(label = name), size = 3.5, max.overlaps = 100) +
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
    scale_y_continuous(limits = c(min_y - 3 * expand_y, max_y + 3 * expand_y)) +
    labs(title = title, x = xlab, y = ylab, size = "Dominance") +
    theme_minimal(base_family = "Microsoft JhengHei") +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), legend.position = "none")

  grDevices::png(out_png, width = 1200, height = 900, res = 130)
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
# ---- Load FLCA core in isolated env (minimize name collisions, but keep access to common functions) ----
flca_env <- new.env(parent = .GlobalEnv)
# ensure common functions exist even if something shadows search path
flca_env$head      <- utils::head
flca_env$tail      <- utils::tail
flca_env$setNames  <- stats::setNames
flca_env$readline  <- function(prompt = "") "1"
flca_env$scan      <- function(...) 1
flca_loaded   <- FALSE
flca_load_err <- NULL
tryCatch({
  sys.source("flca_core.R", envir = flca_env)
  if (exists("FLCA_run", envir = flca_env, inherits = FALSE)) {
    FLCA_run   <- flca_env$FLCA_run
    flca_loaded <- TRUE
  } else {
    flca_load_err <- "FLCA_run not found in flca_core.R"
  }
}, error = function(e) {
  flca_load_err <- paste0("Failed to source flca_core.R: ", conditionMessage(e))
})
if (!isTRUE(flca_loaded)) {
  message("[WARN] FLCA core not loaded: ", flca_load_err)
  FLCA_run <- function(...) {
    msg <- if (!is.null(flca_load_err) && nzchar(flca_load_err)) flca_load_err else "FLCA core not loaded"
    stop(msg)
  }
}

options(FLCA_SHINY_NO_SIDE_EFFECTS = TRUE)
source("renderSSplot.R", local = TRUE)  # provides render_panel()
ui <- fluidPage(
  titlePanel("FLCA Top20 Report (PNG-first, shinyapps.io ready)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload data (2 columns: Leader,Follower OR 3 columns: Leader,Follower,WCD)",
                accept = c(".csv", ".txt", ".tsv")),
      checkboxInput("use_demo", "Use demo (country.csv) if no upload", value = FALSE),
      numericInput("topn", "Top N (sampling after FLCA)", value = 20, min = 10, max = 50),
      numericInput("per_cluster", "Major sampling: per cluster", value = 4, min = 1, max = 10),
      actionButton("run", "Generate HTML report", class = "btn-primary"),
      br(), br(),
      uiOutput("report_link"),
      downloadButton("dl_report", "Download report.html")
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
            tags$div(style="padding:12px 14px;line-height:1.6;",
              tags$ol(
                tags$li(tags$b("Upload"), " a CSV/TXT/TSV with 2 columns (Leader, Follower) or 3 columns (Leader, Follower, WCD)."),
                tags$li(tags$b("Run"), " click 'Generate HTML report' to compute FLCA, major sampling, and figures."),
                tags$li(tags$b("View"), " figures in the Figures tab (Network, SSplot, Kano1, Kano2, PCA, Sankey)."),
                tags$li(tags$b("Download"), " tables/figures from the Downloads tab (includes demo data for learning)."),
                tags$li(tags$b("Report"), " open the self-contained HTML report, or download report.html.")
              ),
              tags$ul(
                tags$li("Top N controls how many nodes are kept after FLCA (default 20)."),
                tags$li("Major sampling per cluster controls balance across clusters."),
                tags$li("If you only want to learn the format, download the demo data first.")
              )
            )
          ),
          tags$ul(
            tags$li("Accepted formats: CSV / TXT / TSV"),
            tags$li("Columns: (Leader, follower) or (Leader, follower, WCD)"),
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
            )
          )
        ),

        tabPanel("Downloads",
          h4("Download demo data (for learning)"),
          downloadButton("dl_demo_data", "Download demo dataset (CSV)"),
          br(), br(),
          h4("Download tables"),
          downloadButton("dl_top20_nodes", "Download Top20 nodes (CSV)"),
          downloadButton("dl_top20_edges", "Download Top20 relations (CSV)"),
          br(), br(),
          h4("Download figures"),
          downloadButton("dl_fig_network", "Download Network PNG"),
          downloadButton("dl_fig_ssplot",  "Download SSplot PNG"),
          downloadButton("dl_fig_kano1",   "Download Kano1 PNG"),
          downloadButton("dl_fig_kano2",   "Download Kano2 PNG"),
          downloadButton("dl_fig_pca",     "Download PCA PNG"),
          downloadButton("dl_all_figs",    "Download ALL figures (ZIP)"),
          br(), br(),
          h5("Top20 nodes (preview)"),
          tableOutput("tbl_top20_nodes"),
          h5("Top20 relations (preview)"),
          tableOutput("tbl_top20_edges")
        ),

        tabPanel("Report",
          uiOutput("report_iframe")
        )
      )
    )
  )
)
server <- function(input, output, session) {
  rv <- reactiveValues(
    report_path   = NULL,
    report_prefix = NULL,
    fig_paths     = NULL,
    top20_nodes   = NULL,
    top20_edges   = NULL,
    sankey_url    = NULL,
    sankey_code   = NULL
  )
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
        # Prefer uploaded file if provided; fallback to demo only if no upload
        if (!is.null(input$file) && !is.na(input$file$datapath) && nzchar(input$file$datapath)) {
          # Use the uploaded file
          dat <- read_any_table(input$file$datapath)
        } else if (isTRUE(input$use_demo)) {
          # Use demo file from app directory
          demo_path <- file.path(app_dir, "demo", "demo_edges.csv")
          if (!file.exists(demo_path)) stop("Demo data not found: ", demo_path)
          dat <- read_any_table(demo_path)
        } else {
          stop("No data uploaded. Please upload a CSV file or select 'Use demo'.")
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
        n_pre_flca <- if (!is.null(net$nodes_base)) nrow(net$nodes_base) else 0
        # ---- Safety: force net$edges_full$WCD numeric ----
        if (!is.null(net$edges_full) && ("WCD" %in% names(net$edges_full))) {
          net$edges_full$WCD <- suppressWarnings(as.numeric(net$edges_full$WCD))
          net$edges_full$WCD[!is.finite(net$edges_full$WCD)] <- 1
        }
        incProgress(0.35, detail = "Running FLCA on full data")
        network <- list(nodes = net$nodes_base, data = net$edges_full)
        flca_out <- tryCatch(
          FLCA_run(network),
          error = function(e) {
            stop("[FLCA] FLCA_run failed: ", conditionMessage(e), call. = FALSE)
          }
        )
        nodes_full <- flca_out$nodes
        n_flca_out <- if (!is.null(nodes_full)) nrow(nodes_full) else 0
        if (exists('n_pre_flca') && is.finite(n_pre_flca) && n_pre_flca > 0 && n_flca_out > 0 && n_flca_out != n_pre_flca) {
          showNotification(sprintf('Warning: FLCA returned %d nodes but input had %d nodes. Check FLCA_run for unintended truncation.', n_flca_out, n_pre_flca), type='warning', duration = NULL)
        }
        # link metrics from pre-FLCA full links
        incProgress(0.45, detail = "Computing link metrics")
        nodes_full2 <- add_link_metrics(nodes_full, net$edges_full, net$two_col_input)
        # major sampling topN
        incProgress(0.55, detail = "Major sampling TopN")
        nodes20 <- major_sample_topN(nodes_full2, cap_limit = input$topn, per_cluster = input$per_cluster)
        n_pre_flca <- if (!is.null(net$nodes_base)) nrow(net$nodes_base) else 0
        n_post_flca <- if (!is.null(nodes_full2)) nrow(nodes_full2) else 0
        n_topN <- if (!is.null(nodes20)) nrow(nodes20) else 0
        tab_pre_flca  <- if (!is.null(net$nodes_base) && ('carac' %in% names(net$nodes_base))) as.data.frame(table(net$nodes_base$carac, useNA='ifany')) else data.frame(carac=NA, n=n_pre_flca)
        tab_post_flca <- if (!is.null(nodes_full2) && ('carac' %in% names(nodes_full2))) as.data.frame(table(nodes_full2$carac, useNA='ifany')) else data.frame(carac=NA, n=n_post_flca)
        tab_top20     <- if (!is.null(nodes20) && ('carac' %in% names(nodes20))) as.data.frame(table(nodes20$carac, useNA='ifany')) else data.frame(carac=NA, n=n_topN)
        # edges among top20
        edges_full20 <- net$edges_full %>%
          filter(Leader %in% nodes20$name, follower %in% nodes20$name)
        edges_one20 <- build_one_link_edges(edges_full20)
        # compute SS(i) and round
        incProgress(0.65, detail = "Computing SS(i) and AAC")
        # use full edge list for SS calculation to ensure proper penalties
        ss <- compute_ssi_top20(nodes20, net$edges_full)
        nodes20 <- ss$nodes20
        nodes20 <- round_numeric_df(nodes20, digits = 2)
        rv$top20_nodes <- nodes20
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
          pca     = file.path(out_dir, "pca_pc1_pc2.png")
        )
        rv$fig_paths <- fig_paths
        # render PNGs
        incProgress(0.9, detail = "Rendering PNG plots")
        render_network_png(fig_paths$network, nodes20, edges_one20)
     
        render_ssplot_png(fig_paths$ssplot, nodes20, net$edges_full)  # uses render_panel with sil_df adapter
        render_kano_png(fig_paths$kano1, nodes20, edges_one20,
                        xcol = "value2", ycol = "value",
                        title = "Kano1: value2 (x) vs value (y)",
                        xlab = "Collaboration (value2)", ylab = "Production (value)",
                        add_circle = TRUE)
        render_kano_png(fig_paths$kano2, nodes20, edges_one20,
                        xcol = "a_star1", ycol = "ssi",
                        title = "Kano2: a*(x) vs SS(i) (y)",
                        xlab = "a*(i)=1/(1+a(i))", ylab = "SS(i)",
                        add_circle = TRUE)
        render_pca_png(fig_paths$pca, nodes20, edges_one20, pca_obj)
        # render HTML report (self contained)
        incProgress(0.95, detail = "Rendering HTML report")
        report_rmd_path <- file.path(out_dir, "report.Rmd")
        ensure_report_rmd_template(report_rmd_path)

        message("[REPORT] rendering: ", report_rmd_path)
        ts2 <- gsub("[: ]", "", format(Sys.time(), "%Y%m%d_%H%M%OS3"))
            out_html <- file.path(out_dir, paste0("report_", ts2, ".html"))
# Prepare isolated env for report rendering (no params dependency)
        report_env <- new.env(parent = globalenv())

# ---- Defaults to avoid 'object not found' in report.Rmd ----
report_env$sankey_url  <- ""
report_env$sankey_code <- ""
        report_env$nodes20 <- as.data.frame(nodes20)
        report_env$aac_tbl <- as.data.frame(aac_tbl)
        report_env$tab_pre_flca <- tab_pre_flca
        report_env$tab_post_flca <- tab_post_flca
        report_env$tab_top20 <- tab_top20
        report_env$n_pre_flca <- n_pre_flca
        report_env$n_post_flca <- n_post_flca
        report_env$n_topN <- n_topN
        report_env$n_flca_out <- if (exists('n_flca_out')) n_flca_out else NA_integer_
        report_env$fig_network <- normalizePath(fig_paths$network, winslash="/", mustWork=FALSE)
        report_env$fig_ssplot  <- normalizePath(fig_paths$ssplot,  winslash="/", mustWork=FALSE)
        report_env$fig_kano1   <- normalizePath(fig_paths$kano1,   winslash="/", mustWork=FALSE)
        report_env$fig_kano2   <- normalizePath(fig_paths$kano2,   winslash="/", mustWork=FALSE)
        report_env$fig_pca     <- normalizePath(fig_paths$pca,     winslash="/", mustWork=FALSE)

# ---- Sankey (computed after figures, but before report render) ----
sank <- list(url = "", url_i = "", code = "")
if (!is.null(nodes20) && !is.null(edges_one20)) {
  sank <- sankey_build(nodes20, edges_one20)
}
report_env$sankey_url  <- safe1(sank$url)
report_env$sankey_code <- safe1(sank$code)
rv$sankey_url          <- safe1(sank$url)
rv$sankey_code         <- safe1(sank$code)


        rmarkdown::render(
          input = report_rmd_path,
          output_file = out_html,
          quiet = TRUE,
          envir = report_env
        )
        # unique resource prefix to avoid addResourcePath collisions
        prefix <- paste0("report_", as.integer(Sys.time()), "_", sample.int(1e9,1))
        addResourcePath(prefix, out_dir)
        rv$report_path <- out_html
        rv$report_prefix <- prefix
            updateTabsetPanel(session, "tabs", selected = "Report")
        showNotification("Report generated successfully.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", conditionMessage(e)), type = "error", duration = NULL)
        rv$report_path <- NULL
        rv$report_prefix <- NULL
      })
    })
  })
# ---- Figure previews for Figures tab ----
output$fig_network <- renderImage({
  req(rv$fig_paths$network)
  list(src = rv$fig_paths$network,
       contentType = "image/png",
       alt = "Network (Top20)")
}, deleteFile = FALSE)

output$fig_ssplot <- renderImage({
  req(rv$fig_paths$ssplot)
  list(src = rv$fig_paths$ssplot,
       contentType = "image/png",
       alt = "SS plot")
}, deleteFile = FALSE)

output$fig_kano1 <- renderImage({
  req(rv$fig_paths$kano1)
  list(src = rv$fig_paths$kano1,
       contentType = "image/png",
       alt = "Kano1")
}, deleteFile = FALSE)

output$fig_kano2 <- renderImage({
  req(rv$fig_paths$kano2)
  list(src = rv$fig_paths$kano2,
       contentType = "image/png",
       alt = "Kano2")
}, deleteFile = FALSE)

output$fig_pca <- renderImage({
  req(rv$fig_paths$pca)
  list(src = rv$fig_paths$pca,
       contentType = "image/png",
       alt = "PCA")
}, deleteFile = FALSE)
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


output$sankey_plot <- renderPlot({
  # 使用 igraph 畫出 Top20 節點與關係的「Sankey 風格」網路圖：
  # - 氣泡大小：依節點 value
  # - 顏色：依 cluster/carac
  # - 邊寬：依 WCD（邊權重）
  req(rv$top20_nodes, rv$top20_edges)
  nodes_df <- rv$top20_nodes
  edges_df <- rv$top20_edges

  # 丟棄 self-loop 關係，避免看到自己指向自己的大圈圈
  edges_df$Leader   <- as.character(edges_df$Leader)
  edges_df$follower <- as.character(edges_df$follower)
  edges_no_self <- edges_df[edges_df$Leader != edges_df$follower, , drop = FALSE]

  if (nrow(edges_no_self) == 0) {
    plot.new()
    text(0.5, 0.5, "No non-self edges for Sankey plot", cex = 1.2)
    return(invisible(NULL))
  }

  # 確保節點表有 name / carac / value 欄位
  if (!"name"  %in% names(nodes_df)) nodes_df$name  <- as.character(nodes_df$name)
  nodes_df$name <- as.character(nodes_df$name)
  if (!"carac" %in% names(nodes_df)) nodes_df$carac <- NA_integer_
  if (!"value" %in% names(nodes_df)) nodes_df$value <- 1

  # 節點全集：所有出現在節點表或邊上的名字
  node_names <- unique(c(
    as.character(nodes_df$name),
    as.character(edges_no_self$Leader),
    as.character(edges_no_self$follower)
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
    to   = as.character(edges_no_self$follower),
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
  w <- edge_df2$WCD
  w[!is.finite(w)] <- 0
  ewidth <- if (length(w) && any(w > 0)) {
    tryCatch(scales::rescale(w, to = c(1, 10)), error = function(e) 1)
  } else {
    1
  }
  igraph::E(g)$weight <- w

  # 只標註 Top3 節點名稱，避免畫面太擠
  top3 <- top3_names(nodes_df)
  igraph::V(g)$label <- ifelse(igraph::V(g)$name %in% top3, igraph::V(g)$name, NA)

  # 用 layout_as_tree 讓流向更像 Sankey
  set.seed(123)
  lay <- igraph::layout_as_tree(g, mode = "out")

  plot(
    g,
    layout = lay,
    vertex.size = vsize,
    vertex.label = igraph::V(g)$label,
    vertex.label.cex = 0.8,
    edge.width = ewidth,
    edge.arrow.size = 0.4,
    main = "Sankey-style flow (size = value, color = cluster, width = WCD)"
  )
})
output$dl_demo_data <- downloadHandler(
  filename = function() paste0("demo_dataset_", Sys.Date(), ".csv"),
  content = function(file) {
    demo_path <- file.path(app_dir, "demo", "demo_edges.csv")
    if (!file.exists(demo_path)) stop("Demo data not found: ", demo_path)
    file.copy(demo_path, file, overwrite = TRUE)
  }
)

output$dl_top20_nodes <- downloadHandler(
  filename = function() paste0("top20_nodes_", Sys.Date(), ".csv"),
  content = function(file) {
    dat <- rv$top20_nodes
    if (is.null(dat)) stop("No Top20 nodes; please run analysis first.")
    readr::write_csv(dat, file)
  }
)

output$dl_top20_edges <- downloadHandler(
  filename = function() paste0("top20_relations_", Sys.Date(), ".csv"),
  content = function(file) {
    dat <- rv$top20_edges
    if (is.null(dat)) stop("No Top20 relations; please run analysis first.")
    readr::write_csv(dat, file)
  }
)

# --- Figure downloads ---
.output_copy_png <- function(src, file) {
  if (is.null(src) || !nzchar(src) || !file.exists(src)) stop("Figure not available yet; please run analysis first.")
  file.copy(src, file, overwrite = TRUE)
}

output$dl_fig_network <- downloadHandler(
  filename = function() paste0("network_top20_", Sys.Date(), ".png"),
  content  = function(file) .output_copy_png(rv$fig_paths$network, file)
)
output$dl_fig_ssplot <- downloadHandler(
  filename = function() paste0("ssplot_top20_", Sys.Date(), ".png"),
  content  = function(file) .output_copy_png(rv$fig_paths$ssplot, file)
)
output$dl_fig_kano1 <- downloadHandler(
  filename = function() paste0("kano1_", Sys.Date(), ".png"),
  content  = function(file) .output_copy_png(rv$fig_paths$kano1, file)
)
output$dl_fig_kano2 <- downloadHandler(
  filename = function() paste0("kano2_", Sys.Date(), ".png"),
  content  = function(file) .output_copy_png(rv$fig_paths$kano2, file)
)
output$dl_fig_pca <- downloadHandler(
  filename = function() paste0("pca_", Sys.Date(), ".png"),
  content  = function(file) .output_copy_png(rv$fig_paths$pca, file)
)

output$dl_all_figs <- downloadHandler(
  filename = function() paste0("figures_", Sys.Date(), ".zip"),
  content = function(file) {
    req(rv$fig_paths)
    files <- unlist(rv$fig_paths, use.names = FALSE)
    files <- files[file.exists(files)]
    if (!length(files)) stop("No figures found; please run analysis first.")
    # zip: keep filenames only
    old <- setwd(dirname(files[[1]]))
    on.exit(setwd(old), add = TRUE)
    utils::zip(zipfile = file, files = basename(files))
  }
)


  output$report_iframe <- renderUI({
    req(rv$report_path, rv$report_prefix)
    tags$iframe(
      src = paste0(rv$report_prefix, "/", basename(rv$report_path), "?v=", as.integer(Sys.time())),
      style = "width: 100%; height: 900px; border: 1px solid #ccc;"
    )
  })
  output$report_link <- renderUI({
    req(rv$report_path, rv$report_prefix)
    tags$a("Open report in new tab", href = paste0(rv$report_prefix, "/", basename(rv$report_path), "?v=", as.integer(Sys.time())), target = "_blank")
  })
  output$dl_report <- downloadHandler(
    filename = function() { "report.html" },
    content = function(file) {
      req(rv$report_path)
      file.copy(rv$report_path, file, overwrite = TRUE)
    }
  )
}
shinyApp(ui, server)
