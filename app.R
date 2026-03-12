
# =========================
# CLEAN BOOTSTRAP (shared environment)
# Goal: avoid "could not find function XXX" by sourcing helper scripts into ONE env
# and exporting all functions/objects into .GlobalEnv.
# =========================
BOOT_DIR <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = TRUE), error = function(e) getwd())
sharedEnv <- new.env(parent = .GlobalEnv)

safe_source <- function(relpath) {
  fp <- file.path(BOOT_DIR, relpath)
  if (file.exists(fp)) {
    source(fp, local = sharedEnv, encoding = "UTF-8")
    message("[BOOT] sourced: ", relpath)
    TRUE
  } else {
    message("[BOOT] missing: ", relpath, " (skipped)")
    FALSE
  }
}

# Source core helpers (add more files here if your app uses them)
safe_source("utils.R")
safe_source("flca_core.R")
safe_source("pubmed_utils.R")
safe_source("sankey.R")
safe_source("renderSSplot.R")

# Export everything into global so app.R can call functions normally
try(list2env(as.list(sharedEnv, all.names = TRUE), envir = .GlobalEnv), silent = TRUE)

# Confirm key functions (prints TRUE/FALSE)
message("[BOOT] normalize_network: ", exists("normalize_network", mode="function"))
message("[BOOT] add_link_metrics: ", exists("add_link_metrics", mode="function"))
message("[BOOT] major_sample_topN: ", exists("major_sample_topN", mode="function"))
message("[BOOT] build_one_link_edges: ", exists("build_one_link_edges", mode="function"))



options(stringsAsFactors = FALSE)
options(repos = c(CRAN="https://cloud.r-project.org"))

# ---- Safe package loader: do NOT stop at sourcing time ----
safe_require <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    return(TRUE)
  } else {
    message("[WARN] Package not installed: ", pkg)
    return(FALSE)
  }
}

has_shiny    <- safe_require("shiny")
has_dplyr    <- safe_require("dplyr")
has_rmarkdown<- safe_require("rmarkdown")
has_igraph   <- safe_require("igraph")
has_scales   <- safe_require("scales")
has_readr    <- safe_require("readr")
has_DT       <- safe_require("DT")
has_htmltools<- safe_require("htmltools")
has_knitr    <- safe_require("knitr")

datatables_html <- function(df, pageLength = 10, caption = NULL, rownames = FALSE) {
  if (!is.data.frame(df)) df <- tryCatch(as.data.frame(df), error = function(e) data.frame())
  if (requireNamespace("DT", quietly = TRUE) && requireNamespace("htmltools", quietly = TRUE)) {
    w <- DT::datatable(df, rownames = rownames, options = list(pageLength = pageLength), caption = caption)
    return(as.character(htmltools::as.tags(w)))
  }
  if (requireNamespace("knitr", quietly = TRUE)) {
    return(as.character(knitr::kable(df, format = "html", escape = FALSE, caption = caption)))
  }
  out <- paste(utils::capture.output(print(df)), collapse = "\n")
  paste0("<pre>", out, "</pre>")
}
# ---- Always-available helper: cannot be missing ----
ensure_dir <- function(path) {
  if (is.null(path) || !isTRUE(nzchar(path))) stop("out_dir is empty.")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) stop("Failed to create out_dir: ", path)
  invisible(path)
}

# ---- Robust app_dir: resolve to folder containing app.R (fallback to getwd) ----
app_dir <- tryCatch({
  of <- sys.frame(1)$ofile
  if (!is.null(of) && isTRUE(nzchar(of))) normalizePath(dirname(of), winslash = "/", mustWork = TRUE) else normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}, error = function(e) normalizePath(getwd(), winslash = "/", mustWork = TRUE))



`%||%` <- function(a, b) if (!is.null(a) && length(a) && !all(is.na(a))) a else b

fetch_csv_to_temp <- function(url, timeout_sec = 30) {
  url <- trimws(url %||% "")
  if (!isTRUE(nzchar(url))) stop("csv_url is empty.")
  if (!grepl("^https?://", url, ignore.case = TRUE)) stop("csv_url must start with http:// or https://")
  tf <- tempfile(fileext = ".csv")
  if (requireNamespace("curl", quietly = TRUE)) {
    h <- curl::new_handle(timeout = timeout_sec)
    curl::curl_download(url, destfile = tf, handle = h, quiet = TRUE)
  } else {
    utils::download.file(url, destfile = tf, quiet = TRUE, mode = "wb")
  }
  if (!file.exists(tf) || file.info(tf)$size <= 0) stop("Downloaded file is empty.")
  head_txt <- tryCatch(paste(utils::head(readLines(tf, warn = FALSE), 5), collapse = "\n"), error = function(e) "")
  if (isTRUE(nzchar(head_txt)) && grepl("<html|<!doctype", head_txt, ignore.case = TRUE)) {
    stop("Downloaded content looks like HTML, not CSV. Use GitHub raw URL and ensure the file is public.")
  }
  tf
}

try(source(file.path(app_dir, "utils.R"),  local = TRUE), silent = TRUE)
try(source(file.path(app_dir, "pubmed_utils.R"), local = TRUE), silent = TRUE)
try(source(file.path(app_dir, "renderSSplot.R"), local = TRUE), silent = TRUE)
try(source(file.path(app_dir, "sankey.R"),       local = TRUE), silent = TRUE)


options(stringsAsFactors = FALSE)
# ---- Guard: avoid 'cannot change locked binding for data' ----
try({
  if (exists("data", envir = .GlobalEnv, inherits = FALSE) && bindingIsLocked("data", .GlobalEnv)) {
    unlockBinding("data", .GlobalEnv)
  }
}, silent = TRUE)
options(repos = c(CRAN="https://cloud.r-project.org"))

options(stringsAsFactors = FALSE)
options(repos = c(CRAN="https://cloud.r-project.org"))

# ---- Safe package loader: do NOT stop at sourcing time ----
safe_require <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    return(TRUE)
  } else {
    message("[WARN] Package not installed: ", pkg)
    return(FALSE)
  }
}

has_shiny    <- safe_require("shiny")
has_dplyr    <- safe_require("dplyr")
has_rmarkdown<- safe_require("rmarkdown")
has_igraph   <- safe_require("igraph")
has_scales   <- safe_require("scales")
has_readr    <- safe_require("readr")
has_DT       <- safe_require("DT")
has_htmltools<- safe_require("htmltools")
has_knitr    <- safe_require("knitr")

# ---- Report Rmd template helper (created on-demand) ----
ensure_report_rmd_template <- function(report_rmd_path) {
  if (is.null(report_rmd_path) || !isTRUE(nzchar(report_rmd_path))) return(invisible(FALSE))
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
    msg <- if (!is.null(flca_load_err) && isTRUE(nzchar(flca_load_err))) flca_load_err else "FLCA core not loaded"
    stop(msg)
  }
}

options(FLCA_SHINY_NO_SIDE_EFFECTS = TRUE)
source("renderSSplot.R", local = TRUE)  # provides render_panel()
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(tags$script(HTML("
    Shiny.addCustomMessageHandler(\'autoclick_run\', function(x){
      var btn = document.getElementById(\'run\');
      if(btn){ btn.click(); }
    });
  "))),
  titlePanel("Author Collaboration Coword Analytics (AC-SaaS)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload data (2 columns: Leader,Follower OR 3 columns: Leader,Follower,WCD)",
                accept = c(".csv", ".txt", ".tsv")),
      checkboxInput("use_demo", "Use demo (country.csv) if no upload", value = FALSE),
                textInput("cmc", "CMC or test", value = "", placeholder = "type test for 7-day trial, or enter 10-digit CMC"),
textInput("csv_url", "CSV URL (optional)", value = "https://raw.githubusercontent.com/smilechien/raschonline/main/drlai.csv", placeholder = "https://raw.githubusercontent.com/.../file.csv"),
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

      tags$div(style="color:red;font-weight:bold;margin:8px 0 12px 0;",
               "NOTE: type test for 7-day trial, or enter valid CMC (e.g., 57306**835) on request."),
h4("Welcome"),
      p("Please confirm your data format before running the analysis. The following five rules correspond one-to-one with how this App interprets inputs and applies priority:"),
      tags$ol(
        tags$li(
          tags$b("Multi-column (≥ 2) co-word data:"),
          " Treated as an edge list (from, to[, weight]). The original workflow remains unchanged."
        ),
        tags$li(
          tags$b("2-column WoS data (Fields B and Y):"),
          tags$ul(
            tags$li("B: Author list (with ';') → take the first author before ';', then extract Last, IN (e.g., Chang, WT)."),
            tags$li("Y: Corresponding author string (with 'correspond') → extract the first Last, IN (e.g., Kan, WC)."),
            tags$li("Output: Leader / follower / WCD = 1.")
          )
        ),
        tags$li(
          tags$b("Single-column WoS data (Fields A, F, T, or U):"),
          tags$ul(
            tags$li("If the content contains ';': treated as an author list → first author as Leader; remaining authors as followers (multiple edges per record, WCD = 1)."),
            tags$li("If ';' is not present: the App will request either a 2-column (B, Y) input or a single column containing ';' to avoid incorrect execution.")
          )
        ),
        tags$li(
          tags$b("No upload and demo not checked: URL will be used:"),
          " Input priority is: upload > demo (checked) > URL (demo unchecked and URL provided) > otherwise an error."
        ),
        tags$li(
          tags$b("CSV provided via URL still requires clicking Generate HTML report:"),
          " The URL specifies the data source only; the full pipeline runs only after clicking the button (no auto-run)."
        )
      ),
      hr(),
      h4("歡迎"),
      p("請先確認你上傳或提供的資料格式。以下五條規則與本 App 的資料判讀與優先順序逐條對應："),
      tags$ol(
        tags$li(
          tags$b("多欄（≥2）共字資料："),
          "仍視為 edge list（from, to[, weight]），原流程不改。"
        ),
        tags$li(
          tags$b("2 欄 WoS（欄 B 與 Y）："),
          tags$ul(
            tags$li("B：作者串（含 ;）→ 取 ; 前第一作者，再抽出 Last, IN（例如 Chang, WT）。"),
            tags$li("Y：通訊作者串（含 correspond）→ 抽出第一個 Last, IN（例如 Kan, WC）。"),
            tags$li("產出 Leader / follower / WCD = 1。")
          )
        ),
        tags$li(
          tags$b("單欄 WoS（A、F、T 或 U 欄）："),
          tags$ul(
            tags$li("若內容含 ;：視為作者串 → 第一作者為 Leader，其餘作者為 follower（每筆多條 edge，WCD = 1）。"),
            tags$li("若不含 ;：將提示需提供 2 欄（B, Y）或含 ; 的 1 欄，避免錯誤執行。")
          )
        ),
        tags$li(
          tags$b("未上傳且未勾選 demo 時，以 URL 為準："),
          "資料來源優先順序為：upload > demo（已勾選）> URL（未勾 demo 且有填）> 否則報錯。"
        ),
        tags$li(
          tags$b("URL 指向 CSV 時仍需點選 Generate HTML report："),
          "URL 僅作為資料來源，仍需按下按鈕才會執行完整流程（不自動跑）。"
        )
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
      h4("Download tables"),
      downloadButton("dl_top20_nodes", "Download Top20 nodes (CSV)"),
      downloadButton("dl_top20_edges", "Download Top20 relations (CSV)"),
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
,
tags$style(HTML("
  .contact-fab{position:fixed;right:18px;bottom:18px;z-index:9999;}
  .contact-fab .btn{background:#1f77b4;color:white;border:none;border-radius:999px;
    padding:10px 14px;font-weight:600;box-shadow:0 6px 18px rgba(0,0,0,.18);}
")),
tags$div(class="contact-fab", actionButton("contact_btn", "💬 Contact authors / Request CMC", class="btn"))
)
server <- function(input, output, session) {
  # ---- reactive state ----
  rv <- reactiveValues(
    report_path = NULL,
    report_prefix = NULL,
    fig_paths = list(),
    top20_nodes = NULL,
    top20_edges = NULL,
    sankey_code = "",
    sankey_url = "",
    owner_title = "",
    autorun_flag = "",
    autorun = "",
    autorun_sig = NULL,
    csv_url_param = "",
    term_param = "",
    mesh_param = "",
    title_param = ""
  )

  # Read URL params early (for autorun etc.)
  observe({
    qs <- session$clientData$url_search %||% ""
    if (!isTRUE(nzchar(qs))) return()
    q <- qs; if (startsWith(q, "?")) q <- substring(q, 2)
    parts <- strsplit(q, "&", fixed = TRUE)[[1]]
    kv <- strsplit(parts, "=", fixed = TRUE)
    keys <- vapply(kv, function(x) if (length(x) >= 1) x[[1]] else "", character(1))
    vals <- lapply(kv, function(x) if (length(x) >= 2) URLdecode(x[[2]]) else "")
    mp <- setNames(vals, keys)
    rv$autorun_flag <- as.character((mp[["autorun"]] %||% "")[[1]])
    rv$autorun <- rv$autorun_flag
    rv$cmc_param <- as.character((mp[["cmc"]] %||% "")[[1]])
    rv$title_param <- as.character((mp[["title"]] %||% "")[[1]])
    rv$csv_url_param <- as.character((mp[["csv_url"]] %||% mp[["url"]] %||% "")[[1]])
    rv$term_param <- as.character((mp[["term"]] %||% "")[[1]])
    rv$mesh_param <- as.character((mp[["mesh"]] %||% "")[[1]])
  })




# ---- Prefill CMC input box from query (?cmc=...) ----
observeEvent(rv$cmc_param, {
  v <- trimws(as.character(rv$cmc_param %||% ""))
  if (!isTRUE(nzchar(v))) return()
  # Only prefill if the user hasn't typed anything yet
  cur <- trimws(as.character(isolate(input$cmc) %||% ""))
  if (!isTRUE(nzchar(cur))) {
    updateTextInput(session, "cmc", value = v)
    message("[URL] Prefilled cmc into input box: ", v)
  }
}, ignoreInit = FALSE)
# ---- Floating contact button ----
observeEvent(input$contact_btn, {
  showModal(modalDialog(
    title = "Contact authors / Request CMC",
    easyClose = TRUE,
    footer = modalButton("Close"),
    tags$p("Please choose one of the following ways to contact the authors."),
    tags$h4("1) LINE account"),
    tags$p("LINE Official Account ID:"), tags$code("@onq5657t"),
    tags$p(tags$a("Open LINE add-friend page", href="https://line.me/R/ti/p/%40onq5657t",
                  target="_blank", rel="noopener noreferrer")),
    tags$hr(),
    tags$h4("2) Email"),
    tags$ul(
      tags$li(tags$code("rasch.smile@gmail.com")),
      tags$li(tags$code("codingpaperabc@gmail.com"))
    ),
    tags$hr(),
    tags$h4("3) Donation"),
    tags$p("Voluntary donations are appreciated to support ongoing maintenance and further development."),
    tags$a("🚀 Donate",
           href="https://payment.ecpay.com.tw/QuickCollect/PayData?D50jzB3Lqk68BhoyYtTffB90EJpM0b4XmYFUwS4pAMI%3d",
           target="_blank", rel="noopener noreferrer")
  ))
})



# Read URL parameters (do NOT prefill input boxes)

  # Autorun: trigger analysis once when ?autorun=1 (robust: wait until UI is flushed)
  session$onFlushed(function() {
    ar <- trimws(as.character(isolate(rv$autorun) %||% ""))
    if (!isTRUE(nzchar(ar)) || ar != "1") return(invisible(NULL))
    if (!is.null(isolate(rv$autorun_sig)) && identical(isolate(rv$autorun_sig), "done")) return(invisible(NULL))
    isolate({ rv$autorun_sig <- "done" })

    # Prefer shinyjs click (requires useShinyjs() in UI)
    ok <- TRUE
    tryCatch({
      shinyjs::click("run")
    }, error = function(e) { ok <<- FALSE })

    # Fallback: emulate button press by sending a new value
    if (!ok) {
      tryCatch({
        session$sendInputMessage("run", list(value = as.integer(Sys.time())))
      }, error = function(e) NULL)
    }

    invisible(NULL)
  }, once = TRUE)




  # ---- Prefill CSV URL from query string (?csv_url=... or ?url=...) ----
observeEvent(session$clientData$url_search, {
  raw_qs <- sub("^\\?", "", as.character(session$clientData$url_search %||% ""))
  if (!isTRUE(nzchar(raw_qs))) return()

  qs <- shiny::parseQueryString(raw_qs)
  if (!is.list(qs)) qs <- as.list(qs)

  v <- trimws(as.character(qs[["csv_url"]] %||% ""))
  if (!isTRUE(nzchar(v))) v <- trimws(as.character(qs[["url"]] %||% ""))

  if (isTRUE(nzchar(v))) {
    updateTextInput(session, "csv_url", value = v)
    showNotification(paste0("Loaded CSV URL from link: ", v), type = "message", duration = 5)
    message("[URL] Loaded csv_url from query: ", v)
  }
}, ignoreInit = FALSE)
  # ---- AUTO-RUN (autorun=1): click Generate HTML report when ready ----
  observe({
    # Auto-run only when autorun=1 AND (CMC ok) AND (some data source is ready).
    if (!identical(trimws(as.character(rv$autorun_flag %||% "")), "1")) return()

    cmc_raw <- trimws(as.character(input$cmc %||% ""))
    cmc_q   <- trimws(as.character(rv$cmc_param %||% ""))
    if (!isTRUE(nzchar(cmc_raw)) && isTRUE(nzchar(cmc_q))) cmc_raw <- cmc_q

    cmc_ok <- identical(tolower(cmc_raw), "test") || grepl("^[0-9]{10}$", cmc_raw)

    has_file <- (is.data.frame(input$file) && nrow(input$file) > 0 && !is.na(input$file$datapath[[1]]) && isTRUE(nzchar(input$file$datapath[[1]])))
    has_term <- isTRUE(nzchar(trimws(as.character(rv$term_param %||% ""))))
    has_demo <- isTRUE(input$use_demo)
    has_url  <- isTRUE(nzchar(trimws(isolate(input$csv_url) %||% ""))) || isTRUE(nzchar(trimws(as.character(rv$csv_url_param %||% ""))))
    has_data <- has_file || has_term || has_demo || has_url

    if (!(cmc_ok && has_data)) return()

    invalidateLater(450, session)

    sig <- paste0(
      "cmc=", cmc_raw,
      "|file=", if (is.data.frame(input$file) && nrow(input$file) > 0) (input$file$datapath[[1]] %||% "") else "",
      "|term=", trimws(as.character(rv$term_param %||% "")),
      "|mesh=", trimws(as.character(rv$mesh_param %||% "")),
      "|url=",  trimws(isolate(input$csv_url) %||% ""),
      "|topn=", input$top_n %||% "",
      "|percl=", input$per_cluster %||% "",
      "|demo=", input$use_demo %||% ""
    )
    if (!is.null(rv$autorun_sig) && identical(rv$autorun_sig, sig)) return()
    rv$autorun_sig <- sig

    session$sendCustomMessage("autoclick_run", list())
  })

observeEvent(input$run, {

# ---- Access control (CMC / test) ----
cmc_raw <- trimws(as.character(input$cmc %||% ""))
cmc_q <- trimws(as.character(rv$cmc_param %||% ""))
if (!isTRUE(nzchar(cmc_raw)) && isTRUE(nzchar(cmc_q))) cmc_raw <- cmc_q
is_valid_access <- identical(tolower(cmc_raw), "test") || grepl("^[0-9]{10}$", cmc_raw)
if (!is_valid_access) {
  showNotification("Invalid access. Type 'test' for 7-day trial, or enter valid CMC.", type = "error", duration = 8)
  return(invisible(NULL))
}

    
            # ---- Scalar-safe helper (prevents knitr 'text length zero') ----
            safe1 <- function(x) {
              if (is.null(x) || length(x) == 0) return("")
              x1 <- x[[1]]
              if (is.null(x1) || length(x1) == 0) return("")
              x1 <- as.character(x1)
              if (is.na(x1) || !isTRUE(nzchar(x1))) return("")
              x1
            }

# ---- PubMed first-last fallback (in case pubmed_utils returns empty) ----
pubmed_fetch_edges_first_last_fallback <- function(term, retmax = 200L) {
  if (!requireNamespace("rentrez", quietly = TRUE)) {
    stop("PubMed fallback requires package 'rentrez'. Install it via install.packages('rentrez').")
  }
  s <- rentrez::entrez_search(db = "pubmed", term = term, retmax = as.integer(retmax))
  pmids <- s$ids
  message("[POMER-FB] hits=", s$count, " fetched=", length(pmids))
  if (length(pmids) == 0) {
    return(data.frame(Leader=character(), Follower=character(), WCD=integer(), stringsAsFactors=FALSE))
  }
  xml_txt <- rentrez::entrez_fetch(db="pubmed", id=pmids, rettype="xml", parsed=FALSE)
  if (!isTRUE(nzchar(xml_txt))) {
    return(data.frame(Leader=character(), Follower=character(), WCD=integer(), stringsAsFactors=FALSE))
  }
  pick_tag <- function(x, tag) {
    m <- regexec(paste0("<", tag, ">([\\s\\S]*?)</", tag, ">"), x, perl=TRUE)
    r <- regmatches(x, m)[[1]]
    if (length(r) >= 2) r[2] else ""
  }
  get_authors <- function(article_xml) {
    a_blocks <- regmatches(article_xml, gregexpr("<Author\\b[\\s\\S]*?</Author>", article_xml, perl=TRUE))[[1]]
    if (length(a_blocks) == 0) return(character())
    authors <- vapply(a_blocks, function(b) {
      last <- pick_tag(b, "LastName")
      ini  <- pick_tag(b, "Initials")
      fore <- pick_tag(b, "ForeName")
      if (isTRUE(nzchar(last)) && isTRUE(nzchar(ini))) return(paste(last, ini))
      if (isTRUE(nzchar(last)) && isTRUE(nzchar(fore))) return(paste(last, fore))
      coll <- pick_tag(b, "CollectiveName")
      if (isTRUE(nzchar(coll))) return(coll)
      ""
    }, character(1))
    authors <- trimws(authors)
    authors <- authors[nzchar(authors)]
    unique(authors)
  }
  articles <- regmatches(xml_txt, gregexpr("<PubmedArticle\\b[\\s\\S]*?</PubmedArticle>", xml_txt, perl=TRUE))[[1]]
  if (length(articles) == 0) {
    return(data.frame(Leader=character(), Follower=character(), WCD=integer(), stringsAsFactors=FALSE))
  }
  author_lists <- lapply(articles, get_authors)
  author_lists <- author_lists[vapply(author_lists, length, integer(1)) >= 2]
  if (length(author_lists) == 0) {
    return(data.frame(Leader=character(), Follower=character(), WCD=integer(), stringsAsFactors=FALSE))
  }
  edges <- do.call(rbind, lapply(author_lists, function(a) {
    data.frame(Leader=a[1], Follower=a[length(a)], stringsAsFactors=FALSE)
  }))
  edges <- edges[edges$Leader != edges$Follower, , drop=FALSE]
  if (nrow(edges) == 0) {
    return(data.frame(Leader=character(), Follower=character(), WCD=integer(), stringsAsFactors=FALSE))
  }
  key <- paste(edges$Leader, edges$Follower, sep="|||")
  tab <- table(key)
  keys <- names(tab)
  data.frame(
    Leader = sub("\\|\\|\\|.*$", "", keys),
    Follower = sub("^.*\\|\\|\\|", "", keys),
    WCD = as.integer(tab),
    stringsAsFactors=FALSE
  )
}
    withProgress(message = "Generating report...", value = 0, {
      tryCatch({
        incProgress(0.1, detail = "Loading data")
        # Data source priority (AC-SaaS):
        # 1) Upload file
        # 2) PubMed term (mesh optional)
        # 3) Demo
        # 4) CSV URL (including homepage default)
        if (is.data.frame(input$file) && nrow(input$file) > 0 && !is.na(input$file$datapath[[1]]) && isTRUE(nzchar(input$file$datapath[[1]]))) {
          dat <- read_any_table(input$file$datapath[[1]])

        } else if (isTRUE(nzchar(trimws(as.character(rv$term_param %||% ""))))) {
          # PubMed term (and optional mesh mode)
          if (!exists("pubmed_fetch_edges_first_last", mode = "function")) {
            stop("PubMed helper not available: pubmed_utils.R not loaded.")
          }
          if (!requireNamespace("rentrez", quietly = TRUE)) {
            stop("PubMed requires the R package 'rentrez'. Install it via install.packages('rentrez').")
          }
          term_q  <- trimws(as.character(rv$term_param %||% ""))
          has_mesh <- isTRUE(nzchar(trimws(as.character(rv$mesh_param %||% ""))))
          if (has_mesh) {
            if (!exists("pubmed_fetch_mesh_coword_edges", mode = "function")) {
              stop("MeSH helper not available: pubmed_fetch_mesh_coword_edges not found.")
            }
            dat <- pubmed_fetch_mesh_coword_edges(term_q, retmax = 300L, per_article_cap = 25L, top_global = 200L)

rv$n_doc <- suppressWarnings(as.integer(attr(dat, "n_doc")))
if (!is.finite(rv$n_doc) || rv$n_doc <= 0) rv$n_doc <- NA_integer_
          } else {
            
message("[SRC] pubmed first-last term=", term_q)
dat <- pubmed_fetch_edges_first_last(term_q, retmax = 200L)

# record publication count for plotting (n=xx at bottom of SS plot)
rv$n_doc <- suppressWarnings(as.integer(attr(dat, "n_doc")))
if (!is.finite(rv$n_doc) || rv$n_doc <= 0) rv$n_doc <- NA_integer_
message("[SRC] pubmed first-last loaded: nrow=", if (is.data.frame(dat)) nrow(dat) else NA, " ncol=", if (is.data.frame(dat)) ncol(dat) else NA)
if (is.data.frame(dat) && nrow(dat) == 0) {
  message("[SRC] pubmed_utils returned 0 rows; trying fallback rentrez parser...")
  dat <- pubmed_fetch_edges_first_last_fallback(term_q, retmax = 200L)
  message("[SRC] fallback loaded: nrow=", if (is.data.frame(dat)) nrow(dat) else NA, " ncol=", if (is.data.frame(dat)) ncol(dat) else NA)
}}

        } else if (isTRUE(input$use_demo)) {
          demo_path <- file.path(app_dir, "country.csv")
          if (!file.exists(demo_path)) stop("country.csv not found in app folder: ", demo_path)
          dat <- read_any_table(demo_path)

        } else if (isTRUE(nzchar(trimws(isolate(input$csv_url) %||% ""))) || isTRUE(nzchar(trimws(as.character(rv$csv_url_param %||% ""))))) {
          url <- trimws(isolate(input$csv_url) %||% "")
          if (!isTRUE(nzchar(url))) url <- trimws(as.character(rv$csv_url_param %||% ""))
          showNotification(paste0("Downloading CSV from: ", url), type = "message", duration = 5)
          message("[URL] Downloading CSV: ", url)
          tf <- fetch_csv_to_temp(url)
          dat <- read_any_table(tf)

        } else {
          stop("No data. Upload a CSV, provide a PubMed term, use demo, or give a CSV URL.")
        }
        
        
# ---- 1-column ';' co-word list -> edge list (Leader/follower/WCD) ----
# If input is a single column and contains ';', treat each row as a bag of terms separated by ';'.
# Generate all unordered pairs within each row; WCD = co-occurrence count.
if (is.data.frame(dat) && ncol(dat) == 1) {
  v <- as.character(dat[[1]])
  has_semicolon <- any(grepl(";", v, fixed = TRUE), na.rm = TRUE)
  if (isTRUE(has_semicolon)) {
    make_pairs <- function(s) {
      s <- trimws(as.character(s))
      if (!isTRUE(nzchar(s))) return(NULL)
      parts <- trimws(strsplit(s, ";", fixed = TRUE)[[1]])
      parts <- parts[nzchar(parts)]
      if (length(parts) < 2) return(NULL)
      cmb <- t(combn(parts, 2))
      data.frame(Leader = cmb[,1], follower = cmb[,2], WCD = 1, stringsAsFactors = FALSE)
    }
    L <- lapply(v, make_pairs)
    L <- L[!vapply(L, is.null, logical(1))]
    if (length(L)) {
      edges <- do.call(rbind, L)
      # aggregate duplicate pairs across rows
      edges$Leader   <- trimws(as.character(edges$Leader))
      edges$follower <- trimws(as.character(edges$follower))
      edges <- edges[edges$Leader != "" & edges$follower != "", , drop = FALSE]
      if (nrow(edges)) {
        edges$key <- ifelse(edges$Leader <= edges$follower,
                            paste(edges$Leader, edges$follower, sep="||"),
                            paste(edges$follower, edges$Leader, sep="||"))
        agg <- aggregate(WCD ~ key, data = edges, FUN = sum)
        spl <- strsplit(as.character(agg$key), "\\|\\|", fixed = FALSE)
        Leader <- vapply(spl, function(x) x[1], character(1))
        follower <- vapply(spl, function(x) x[2], character(1))
        dat <- data.frame(Leader = Leader, follower = follower, WCD = agg$WCD, stringsAsFactors = FALSE)
      }
    }
  }
}
# ---- WoS 2-column ';' authors -> (Leader=1st author, follower=corresponding author) ----
        # Rule (as requested):
        # - Column 1: authors list separated by ';' -> take first author, then take "Last, IN" (first two tokens)
        # - Column 2: corresponding author line -> take the FIRST "Last, IN" match (e.g., "Kan, WC")
        if (is.data.frame(dat) && ncol(dat) == 2) {
          col1 <- as.character(dat[[1]])
          col2 <- as.character(dat[[2]])
          is_wos_like <- any(grepl(";", col1, fixed = TRUE), na.rm = TRUE) &&
            any(grepl("correspond", col2, ignore.case = TRUE), na.rm = TRUE)
          if (isTRUE(is_wos_like)) {
            # first author segment before first ';'
            first_seg <- trimws(sub(";.*$", "", col1))
            # extract "Last, IN" pattern from first segment / corresponding line
            extract_last_ini <- function(x) {
              x <- trimws(as.character(x))
              m <- regmatches(x, regexpr("^\\s*[^,;]+,\\s*[^\\s,;]+", x, perl = TRUE))
              if (length(m) == 0) "" else trimws(m[1])
            }
            Leader   <- vapply(first_seg, extract_last_ini, character(1))
            follower <- vapply(col2,     extract_last_ini, character(1))
            dat <- data.frame(Leader = Leader, follower = follower, WCD = 1, stringsAsFactors = FALSE)
          }
        }

# ---- Minimal numeric coercion (ensure WCD is numeric; NEVER assign dat[[3]] when input has <3 cols) ----
        if (ncol(dat) >= 3) {
          dat[[3]] <- suppressWarnings(as.numeric(dat[[3]]))
          dat[[3]][!is.finite(dat[[3]])] <- 1
        } else if (ncol(dat) == 2) {
          # keep 2-col inputs as edge list with implicit weight 1
          if (is.null(names(dat)) || any(is.na(names(dat)) | !nzchar(names(dat)))) {
            names(dat) <- c("Leader","follower")
          } else {
            names(dat)[1:2] <- c("Leader","follower")
          }
          dat[["WCD"]] <- 1
        } else {
          # ncol(dat) == 1 should be handled by the 1-col ';' co-word splitter BEFORE this block.
          # Leave as-is to avoid tibble assignment error.
        }
        incProgress(0.2, detail = "Normalizing network")
        net <- normalize_network(dat)
        if (!is.list(net)) stop("normalize_network() returned non-list; check input data format.")
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
        render_ssplot_png(fig_paths$ssplot, nodes20, net$edges_full, n_doc = rv$n_doc)  # uses render_panel with sil_df adapter
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

        # --- Inject owner title into the rendered HTML (from URL title/author/owner) ---
        owner_ttl <- trimws(as.character(rv$owner_title %||% ""))
        if (isTRUE(nzchar(owner_ttl)) && file.exists(out_html)) {
          html_lines <- tryCatch(readLines(out_html, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
          if (length(html_lines)) {
            badge <- paste0(
              "<div style='color:#c00000;font-weight:700;font-size:28px;line-height:1.2;margin:10px 0 14px 0;'>",
              htmltools::htmlEscape(owner_ttl),
              "</div>"
            )
            # Insert right after <body> if possible; otherwise prepend
            body_idx <- which(grepl("<body[^>]*>", html_lines, ignore.case = TRUE))[1]
            if (!is.na(body_idx)) {
              html_lines <- append(html_lines, badge, after = body_idx)
            } else {
              html_lines <- c(badge, html_lines)
            }
            tryCatch(writeLines(html_lines, out_html, useBytes = TRUE), error = function(e) NULL)
          }
        }

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
  if (!"value" %in% names(nodes_df)) nodes_df$value <- rep(1, nrow(nodes_df))# 節點全集：所有出現在節點表或邊上的名字
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
output$dl_top20_nodes <- downloadHandler(
  filename = function() {
    paste0("top20_relations_", Sys.Date(), ".csv")
  },
  content = function(file) {
    dat <- rv$top20_edges
    if (is.null(dat)) stop("No Top20 relations; please run analysis first.")
    readr::write_csv(dat, file)
  }
)



  
  observeEvent(rv$report_path, {
    req(!is.null(rv$report_path))
    tryCatch(updateTabsetPanel(session, "tabs", selected = "Report"), error = function(e) NULL)
  }, ignoreInit = TRUE)

output$report_iframe <- renderUI({
    req(!is.null(rv$report_path) && isTRUE(nzchar(rv$report_path)))
tags$iframe(
      src = paste0(rv$report_prefix, "/", basename(rv$report_path), "?v=", as.integer(Sys.time())),
      style = "width: 100%; height: 900px; border: 1px solid #ccc;"
    )
  })
  output$report_link <- renderUI({
    req(!is.null(rv$report_path) && isTRUE(nzchar(rv$report_path)))
tags$a("Open report in new tab", href = paste0(rv$report_prefix, "/", basename(rv$report_path), "?v=", as.integer(Sys.time())), target = "_blank")
  })
  output$dl_report <- downloadHandler(
    filename = function() { "report.html" },
    content = function(file) {
      req(rv$report_path)
      file.copy(rv$report_path, file, overwrite = TRUE)
    }
  )
  # Owner title (URL title=... wins; otherwise keep current)
  observe({
    ttl <- trimws(as.character(rv$title_param %||% ""))
    if (isTRUE(nzchar(ttl))) rv$owner_title <- ttl
  })
}
shinyApp(ui, server)
