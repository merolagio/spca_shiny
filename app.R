#=================================================P
#COMPANION APP TO THE SPCA TUTORIAL
#GIOVANNI M. MEROLA
# merolagio@gmail.com
#=================================================P
options(shiny.maxRequestSize = 200 * 1024^2)
library(shiny)
library(bslib)
library(DT)
library(spca)
# ADD read scales
# ADD plots
# ADD variable selection choice
#UI START=======================
`%||%` <- function(x, y) {if (is.null(x)) y else x}
vec2fac = function(v){
  u = unique(v)
  val = rep(0, length(v))
  for(i in 1:length(u)){
    val[v == u[i]] = i
  }
  factor(val, labels = u)
}
app_logo_file <- file.path(getwd(), "spca_Logo_bordered.png")
if (file.exists(app_logo_file)) {
  addResourcePath("spca_assets", normalizePath(dirname(app_logo_file), winslash = "/", mustWork = TRUE))
}
app_docs_dir <- file.path(getwd(), "docs")
if (dir.exists(app_docs_dir)) {
  addResourcePath("spca_docs", normalizePath(app_docs_dir, winslash = "/", mustWork = TRUE))
}
spca_logo_tag <- function() {
  if (!file.exists(app_logo_file)) return(NULL)
  tags$img(
    src = paste0("spca_assets/", basename(app_logo_file)),
    class = "spca-intro-logo",
    alt = "spca logo"
  )
}
spca_doc_href <- function(file) {
  if (dir.exists(app_docs_dir)) paste0("spca_docs/", file) else paste0("docs/", file)
}
app_css <- "
.intro-card-welcome {
  background-color: #f4f7fb;
  border-left: 5px solid #5b7dbb;
}
.intro-card-instructions {
  background-color: #f6fbf7;
  border-left: 5px solid #4f9d69;
}
.intro-card-references {
  background-color: #fbf8f1;
  border-left: 5px solid #c9902e;
}
.intro-card-welcome .card-header,
.intro-card-instructions .card-header,
.intro-card-references .card-header {
  font-weight: 700;
  color: #102a43;
}
.navbar .nav-link {
  font-weight: 700;
  border-radius: 0 0 6px 6px;
  margin: 0 2px;
  padding-left: 1rem !important;
  padding-right: 1rem !important;
}
.navbar-nav .nav-item:nth-child(1) .nav-link { background: #E69F00; color: #ffffff !important; }
.navbar-nav .nav-item:nth-child(2) .nav-link { background: #56B4E9; color: #ffffff !important; }
.navbar-nav .nav-item:nth-child(3) .nav-link { background: #009E73; color: #ffffff !important; }
.navbar-nav .nav-item:nth-child(4) .nav-link { background: #F0E442; color: #111111 !important; }
.navbar-nav .nav-item:nth-child(5) .nav-link { background: #0072B2; color: #ffffff !important; }
.navbar-nav .nav-item:nth-child(6) .nav-link { background: #D55E00; color: #ffffff !important; }
.navbar-nav .nav-item:nth-child(7) .nav-link { background: #CC79A7; color: #ffffff !important; }
.navbar .nav-link.active,
.navbar .nav-link:focus,
.navbar .nav-link:hover {
  filter: brightness(0.88);
}
.bslib-sidebar-layout > .sidebar,
.sidebar {
  background: #e9eef1 !important;
}
.bslib-sidebar-layout > .sidebar label,
.sidebar label,
.sidebar .control-label,
.sidebar .form-label,
.sidebar legend {
  font-weight: 700;
  color: #0d2b45;
}
.sidebar .btn,
.card .btn-primary {
  font-weight: 700;
}
.spca-title-row {
  display: flex;
  align-items: center;
  gap: 16px;
}
.spca-intro-logo {
  height: 72px;
  width: auto;
  object-fit: contain;
}
.manual-loadings-input textarea {
  font-family: Consolas, 'Courier New', monospace;
  font-size: 0.9rem;
}
.upload-drop-zone {
  border: 2px dashed #8aa0ad;
  border-radius: 8px;
  background: #f6f8f9;
  padding: 12px;
  margin-bottom: 12px;
}
.upload-drop-zone label {
  font-weight: 700;
}
.upload-drop-note {
  color: #435766;
  font-size: 0.9rem;
  margin-bottom: 8px;
}
.compare-status-line {
  min-height: 0;
  margin: 0 0 8px 0;
  padding: 4px 8px;
  line-height: 1.2;
  font-size: 0.9rem;
  font-family: inherit;
  white-space: normal;
}
.compare-table-wrap {
  width: 100%;
  max-width: 100%;
  overflow-x: auto;
}
.compare-table-wrap .datatables,
.compare-table-wrap .dataTables_wrapper {
  width: max-content;
  min-width: 100%;
}
"
ui <- page_navbar(
  title = "spca package web interface",
  theme = bs_theme(version = 5),
  navbar_options = navbar_options(underline = T),
  header = tags$head(tags$style(HTML(app_css))),
#  navbar_options = navbar_options(collapsible = FALSE),
#intro================
nav_panel(
  "Intro",
  layout_column_wrap(
    width = 1,
    
    card(
      class = "intro-card-welcome",
      card_header(
        tags$div(
          class = "spca-title-row",
          spca_logo_tag(),
          tags$div(
            class = "spca-title-text",
            tags$div("Author: Giovanni Maria Merola"),
            tags$div(
              "Issues and suggestions: ",
              tags$a(
                href = "https://github.com/merolagio/spca_shiny/issues",
                target = "_blank",
                "https://github.com/merolagio/spca_shiny/issues"
              )
            )
          )
        )
      ),
      p("This app provides a graphical interface to fit Least Squares Sparse Principal Components models using the spca package."),
      p(
        "It is a companion app for the SPCA tutorial. Functionalities are necessarily limited. ",
        "For more functionality, you can download the ",
        a("spca package from CRAN", href = "https://cran.r-project.org/package=spca", target = "_blank"),
        " or the latest development version from ",
        a("GitHub", href = "https://github.com/merolagio/spca", target = "_blank"),
        "."
      ),
        
      p("Some settings can be slow on large or wide data matrices, especially backward/stepwise variable selection, CVEXP objectives, and exact eigen-computations."),
      p("There are three datasets available: MSSCQ, Crime and Holzinger. The first two are used in the article and computing the sPCs is slow. Holzinger has only 12 variables and 144 observations, so it can be used to explore LSSPCA solutions. There is also the possibility to upload your own dataset (csv with only numerical values) and optionally with a vector containing the scale character for each variable (csv)."),
      p("Use the tabs in the navigation bar to upload data, run diagnostics, fit a model, and inspect results.")
    ),
    
    card(
      class = "intro-card-instructions",
      card_header("Quick instructions"),
      tags$ol(
        tags$li(tags$b("Data:"), " Upload a data file in CSV format, and, if needed, the scales, also in CSV format, or one of the existing datasets. You can select the variables to analyze and choose centering/scaling options as needed."),
        tags$li(tags$b("Diagnostics:"), " Inspect the scree plot and the Wachter QQ-plot."),
        tags$li(tags$b("Model:"), " Choose the SPCA variant, objective, variable-selection method, power-method options, and the number of sPCs to compute, then click ", tags$code("Run"), "."),
        tags$li(tags$b("Results:"), " Review the summary table, plots, and download the fitted (R) object or the loadings (csv) if needed.")
      ),
      p(tags$b("Note:"), " Any non-numeric columns in the uploaded data should be excluded from the variable selection.")
    ),
    
    card(
      card_header(
        class = "intro-card-references",
        "References"
      ),
      p(tags$b("Vignettes")),
      p(
        "Introductory vignette ",
        tags$a(href = spca_doc_href("spca_intro.html"), target = "_blank", "HTML"),
        " ",
        tags$a(href = spca_doc_href("spca_intro.pdf"), target = "_blank", "PDF")
      ),
      p(
        "Extended vignette ",
        tags$a(href = spca_doc_href("spca_extended_vignette.html"), target = "_blank", "HTML"),
        " ",
        tags$a(href = spca_doc_href("spca_extended_vignette.pdf"), target = "_blank", "PDF")
      ),
      p(tags$b("Refereed articles")),
      p(
        "Merola, G. M. (2015). Least squares sparse principal component analysis: a backward elimination approach to attain large loadings. Australia & New Zealand Journal of Statistics, 57, 391-429. ",
        tags$a(href = "https://arxiv.org/abs/1406.1381", target = "_blank", "Preprint")
      ),
      p(
        "Merola, G. M. and Chen, G. (2019). Projection sparse principal component analysis: An efficient least squares method. Journal of Multivariate Analysis, 173, 366-382. ",
        tags$a(href = "https://arxiv.org/abs/1612.00939", target = "_blank", "Preprint")
      )
    )
  )
), #end navpanel intro
  nav_panel(
    "Data",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons(
          "data_source",
          "Data source",
          choices = c(
            "Upload CSV" = "upload",
            "Holzinger (142 x 12)" = "holz",
            "MSSCQ (12,992 x 100)" = "msscq",
            "Crime (1,994 x 99)" = "crime"
          ),
          selected = "upload"
        ),
        conditionalPanel(
          condition = "input.data_source == 'msscq' || input.data_source == 'holz'",
          radioButtons(
            "use_builtin_scales",
            "Scales",
            choices = c("No" = "no", "Yes" = "yes"),
            selected = "yes",
            inline = TRUE
          )
        ),
        
        conditionalPanel(
          condition = "input.data_source == 'upload'",
          div(
            class = "upload-drop-zone",
            div(class = "upload-drop-note", "Drop a CSV data matrix here or use Browse."),
            fileInput("file", "Upload or drag/drop CSV data matrix", accept = c(".csv", ".txt"), buttonLabel = "Browse...")
          ),
          fileInput(
            "scale_csv",
            "Optional scale vector (CSV row or column)",
            accept = ".csv"
          ),
          textInput("sep", "Separator", value = ","),
          checkboxInput("header", "Header", TRUE)
        ),
        numericInput(
          "preview_digits",
          "Preview rounding digits",
          value = 2,
          min = 0,
          step = 1
        ),
        radioButtons(
          "show_vars",
          "Variable selection",
          choices = c("Hide" = "hide", "Show" = "show"),
          selected = "hide",
          inline = TRUE
        ),
        
        conditionalPanel(
          condition = "input.show_vars == 'show'",
          uiOutput("vars_ui")
        ),
        
        conditionalPanel(
          condition = "input.data_source == 'upload'",
          checkboxInput("center", "Center", FALSE),
          checkboxInput("scale", "Scale to unit variance", FALSE)
        ),
        
        width = 360
      ),
      card(card_header("Preview"), DTOutput("preview"))
    )
  ),
nav_panel(
  "Diagnostics",
  layout_sidebar(
    sidebar = sidebar(
      numericInput("nplot", "No. eigenvalues to plot", value = 20, min = 1, step = 1),
      checkboxInput("corr_mat", "Treat as correlation matrix (trace = p)", TRUE),
      
      # NEW: explicit refresh buttons
      actionButton("refresh_scree", "Refresh scree plot"),
      actionButton("refresh_wachter", "Refresh Wachter QQ-plot"),
      
      # FIX: allow negative values (remove min = 0)
      numericInput(
        "nfit_line",
        "Fit line using last k points (0 = none; negative allowed)",
        value = 0,
        min = NA,
        step = 1
      ),
      
      width = 360
    ),
    layout_column_wrap(
      width = 1/2,
      card(card_header("Scree plot"), plotOutput("scree", height = 420)),
      card(card_header("Wachter QQ-plot"), plotOutput("wachter", height = 420))
    )
  )
),
  nav_panel("Model",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "variant", "Variant",
          choices = c("cSPCA" = "cspca", "uSPCA" = "uspca", "pSPCA" = "pspca"),
          selected = "cspca"
        ),
        selectInput(
          "selection", "Variable selection",
          choices = c("Forward" = "fwd", "Forward-stepwise" = "step", "Backward" = "bkw"),
          selected = "fwd"
        ),
        selectInput(
          "objective", "Selection objective",
          choices = c("Squared correlation (r2)" = "r2", "Cumulative variance explained (CVEXP)" = "cvexp"),
          selected = "r2"
        ),
        checkboxInput("intensive", "Use intensive forward CVEXP selection", FALSE),
        checkboxInput("pm_loading", "Power method for PC/loadings", FALSE),
        checkboxInput("pm_varsel", "Power method inside variable selection", FALSE),
        numericInput("ncomp", "Components", value = 4, min = 1, step = 1),
        numericInput("alpha", "Target recovered variance (alpha)", value = 0.95, min = 0.50, max = 0.999, step = 0.01),
        actionButton("run", "Run", class = "btn-primary"),
        br(), br(),
        verbatimTextOutput("run_msg"),
        width = 360
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Status"),
          verbatimTextOutput("status")
        ))
    )
  ),
nav_panel("Results",
          layout_sidebar(
            sidebar = sidebar(
              checkboxInput("plotcontrib", "Plot contributions as %", TRUE),
              actionButton("refresh_fitplot", "Refresh plot"),
              uiOutput("plot_spca_controls_ui"),
              downloadButton("dl_rds", "Download fit (.rds)"), 
              downloadButton("dl_loadings_csv", "Download loadings (.csv)"),
              width = 360
            ),
            layout_column_wrap(
              width = 1/2,                        # side-by-side, you control this fraction
              heights_equal = "row",              # both cards same height in each row
              card(
                card_header("Summary"),
                DTOutput("sumtbl"),
                height = "500px"                  # explicit card height — tune this number
              ),
              card(
                card_header("Loadings / Contributions plot"),
                plotOutput("fitplot",
                           height = "450px",      # plot height = card height minus header ~50px
                           width  = "100%"),
                height = "500px"                  # same card height as the table card
              ),
              uiOutput("scale_card_ui")
            )
          )
),
nav_panel("Compare",
  layout_sidebar(
    sidebar = sidebar(
      radioButtons(
        "compare_source",
        "Comparison source",
        choices = c(
          "Fit another model with the current data" = "fit_new",
          "Create SPCA object from uploaded loadings" = "manual"
        ),
        selected = "fit_new"
      ),
      conditionalPanel(
        condition = "input.compare_source == 'fit_new'",
        selectInput(
          "compare_variant", "Variant",
          choices = c("cSPCA" = "cspca", "uSPCA" = "uspca", "pSPCA" = "pspca"),
          selected = "cspca"
        ),
        selectInput(
          "compare_selection", "Variable selection",
          choices = c("Forward" = "fwd", "Forward-stepwise" = "step", "Backward" = "bkw"),
          selected = "fwd"
        ),
        selectInput(
          "compare_objective", "Selection objective",
          choices = c("Squared correlation (r2)" = "r2", "Cumulative variance explained (CVEXP)" = "cvexp"),
          selected = "r2"
        ),
        checkboxInput("compare_intensive", "Use intensive forward CVEXP selection", FALSE),
        checkboxInput("compare_pm_loading", "Power method for PC/loadings", FALSE),
        checkboxInput("compare_pm_varsel", "Power method inside variable selection", FALSE),
        numericInput("compare_ncomp", "Components", value = 4, min = 1, step = 1),
        numericInput("compare_alpha", "Target recovered variance (alpha)", value = 0.95, min = 0.50, max = 0.999, step = 0.01),
        actionButton("compare_run", "Run comparison model", class = "btn-primary")
      ),
      conditionalPanel(
        condition = "input.compare_source == 'manual'",
        div(
          class = "upload-drop-zone",
          div(class = "upload-drop-note", "Drop a CSV/TXT loading matrix here or use Browse."),
          fileInput(
            "manual_loadings_file",
            "Loading matrix (variables x components)",
            accept = c(".csv", ".txt"),
            buttonLabel = "Browse..."
          )
        ),
        checkboxInput("manual_header", "First row contains component names", FALSE),
        textInput("manual_sep", "Separator for uploaded matrices", value = ","),
        div(
          class = "upload-drop-zone",
          div(class = "upload-drop-note", "Optionally drop the covariance/correlation matrix used by the loadings."),
          fileInput(
            "manual_s_file",
            "Optional covariance/correlation matrix",
            accept = c(".csv", ".txt"),
            buttonLabel = "Browse..."
          )
        ),
        checkboxInput("manual_s_header", "Covariance/correlation matrix has header", TRUE),
        actionButton("make_manual_spca", "Create SPCA object", class = "btn-secondary")
      ),
      br(), br(),
      downloadButton("dl_compare_rds", "Download comparison fit (.rds)"),
      downloadButton("dl_compare_loadings_csv", "Download comparison loadings (.csv)"),
      width = 360
    ),
    div(
      class = "compare-status-line",
      textOutput("compare_status", inline = TRUE)
    ),
    card(
      card_header("Comparative contribution plot"),
      plotOutput("manual_compare_plot", height = "450px", width = "100%")
    ),
    card(
      card_header("Comparative summary"),
      div(
        class = "compare-table-wrap",
        DTOutput("manual_summary_tbl")
      )
    )
  )
)
)
#funct  format summary===============
format_summary_matrix <- function(out) {
  percentage_rows <- c("Vexp", "Cvexp", "Rvexp", "Rcvexp")
  integer_rows <- "Card"
  correlation_rows <- "r"
  
  fx <- matrix("", nrow = nrow(out), ncol = ncol(out))
  rownames(fx) <- rownames(out)
  colnames(fx) <- colnames(out)
  
  for (i in seq_len(nrow(out))) {
    row_name <- rownames(out)[i]
    
    if (row_name %in% percentage_rows) {
      fx[i, ] <- paste0(
        format(
          round(100 * out[i, ], 1),
          nsmall = 1,
          drop0trailing = FALSE,
          justify = "right"
        ),
        "%"
      )
    } else if (row_name %in% integer_rows) {
      fx[i, ] <- format(round(out[i, ]), trim = TRUE)
    } else if (row_name %in% correlation_rows) {
      fx[i, ] <- format(
        round(out[i, ], 2),
        nsmall = 2,
        drop0trailing = FALSE,
        justify = "right"
      )
    } else {
      fx[i, ] <- format(
        round(out[i, ], 3),
        nsmall = 3,
        drop0trailing = FALSE,
        justify = "right"
      )
    }
  }
  
  fx
}
server <- function(input, output, session) {
  
  # ---------- Data ----------
  read_app_rds <- function(fname) {
    f <- file.path("data", fname)
    if (!file.exists(f)) stop("Missing app data file: ", f, call. = FALSE)
    readRDS(f)
  }
  read_scale_csv_vector <- function(path) {
    sc <- read.csv(path, header = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
    
    if (nrow(sc) == 1 && ncol(sc) >= 1) {
      v <- as.character(unlist(sc[1, ], use.names = FALSE))
    } else if (ncol(sc) == 1 && nrow(sc) >= 1) {
      v <- as.character(sc[[1]])
    } else {
      stop("Scale CSV must be a single row or a single column.", call. = FALSE)
    }
    
    v <- trimws(v)
    v <- v[nzchar(v)]
    if (length(v) == 0) return(NULL)
    v <- vec2fac(v)
#    factor(v)
  }
  
  dat <- reactive({
    src <- input$data_source
    if (is.null(src)) src <- "upload"
    
    if (identical(src, "upload")) {
      req(input$file)
      sep <- input$sep
      if (is.null(sep) || !nzchar(sep)) sep <- ","
      
      return(read.csv(
        input$file$datapath,
        sep = sep,
        header = isTRUE(input$header),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }
    
    if (identical(src, "msscq")) return(read_app_rds("msscq.rds"))
    if (identical(src, "crime")) return(read_app_rds("crime.rds"))
    if (identical(src, "holz"))  return(read_app_rds("holz.rds"))
    stop("Unknown data source.", call. = FALSE)
  })
  
  # helper: read an optional .rds factor; return NULL if missing
  read_optional_factor_rds <- function(fname) {
    f <- file.path("data", fname)
    if (!file.exists(f)) return(NULL)
    sc <- readRDS(f)
    if (!is.factor(sc)) sc <- vec2fac(sc)
    sc
  }
  read_package_data <- function(name, package = "spca") {
    env <- new.env(parent = emptyenv())
    utils::data(list = name, package = package, envir = env)
    if (!exists(name, envir = env, inherits = FALSE)) {
      stop("Package data object not found: ", name, call. = FALSE)
    }
    get(name, envir = env, inherits = FALSE)
  }
  
  scale_fac <- reactive({
    src <- input$data_source %||% "upload"
    
    if (identical(src, "msscq")) {
      if (!identical(input$use_builtin_scales %||% "yes", "yes")) return(NULL)
      sc <- readRDS(file.path("data", "ms_scalesh_fac.rds"))
      if (!is.factor(sc)) sc <- vec2fac(sc)
      return(sc)
    }
    
    if (identical(src, "crime")) {
      return(NULL)   # per your rule: no scale for crime
    }
    
    if (identical(src, "holz")) {
      if (!identical(input$use_builtin_scales %||% "yes", "yes")) return(NULL)
      sc <- tryCatch(
        read_package_data("holzinger_scales", package = "spca"),
        error = function(e) read_optional_factor_rds("holz_scalesh_fac.rds")
      )
      if (!is.factor(sc)) sc <- vec2fac(sc)
      return(sc)
    }
    # upload case: optional
    if (identical(src, "upload")) {
      if (is.null(input$scale_csv)) return(NULL)
      tryCatch(
        read_scale_csv_vector(input$scale_csv$datapath),
        error = function(e) NULL
      )
    } else {
      NULL
    }
  })
  
  scale_present <- reactive({
    !is.null(scale_fac())
  })
  output$vars_ui <- renderUI({
    df <- dat()
    is_num <- vapply(df, function(z) is.numeric(z) || is.integer(z), logical(1))
    num_cols <- names(df)[is_num]
    if (!length(num_cols)) {
      return(tags$div(class = "text-danger", "No numeric columns detected."))
    }
    selectInput("vars", "Variables", choices = num_cols, selected = num_cols, multiple = TRUE)
  })
  
  Xmat <- reactive({
    df <- dat()
    
    is_num <- vapply(df, function(z) is.numeric(z) || is.integer(z), logical(1))
    num_cols <- names(df)[is_num]
    req(length(num_cols) > 0)
    
    vars <- input$vars
    if (is.null(vars) || !length(vars)) vars <- num_cols
    
    X <- as.matrix(df[, vars, drop = FALSE])
    storage.mode(X) <- "double"
    
    # Apply center/scale only for uploaded CSV (per your earlier rule)
    src <- input$data_source %||% "upload"
    if (identical(src, "upload")) {
      if (isTRUE(input$center)) X <- scale(X, center = TRUE, scale = FALSE)
      if (isTRUE(input$scale))  X <- scale(X, center = FALSE, scale = TRUE)
    }
    
    X
  })
  
  output$preview <- renderDT({
    df_preview <- head(dat(), 50)
    digits <- input$preview_digits
    if (is.null(digits) || is.na(digits)) digits <- 2
    
    digits <- as.integer(digits)
    if (digits < 0) digits <- 0
    
    num_cols <- vapply(df_preview, is.numeric, logical(1))
    df_preview[num_cols] <- lapply(df_preview[num_cols], round, digits = digits)
    
    DT::datatable(df_preview, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---------- Diagnostics (eigenvalues) ----------
  eigvals <- reactive({
    X <- Xmat()
    # eigenvalues of sample covariance/correlation as appropriate
    S <- HelperSpcaShiny:::cov_R(X) 
    ev <- sort(spca:::eigenvalues_sym(S), decreasing = TRUE)
    ev
  })
  
  output$scree <- renderPlot({
    input$refresh_scree  
    req(eigvals())
    tryCatch({
    if (!requireNamespace("spca", quietly = TRUE)) {
      plot.new(); text(0.5, 0.5, "Package spca not available.")
      return()
    }
    ev <- eigvals()
    nplot <- min(length(ev), as.integer(input$nplot))
    pl <- spca::spca_screeplot(eigenvalues = ev, nplot = nplot, show_plot = FALSE, return_plot = TRUE)
    print(pl)}, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Scree plot error:\n", conditionMessage(e)))
    })
  })
  
  output$wachter <- renderPlot({
    input$refresh_wachter 
    req(eigvals())
    if (!requireNamespace("spca", quietly = TRUE)) {
      plot.new(); text(0.5, 0.5, "Package spca not available.")
      return()
    }
    ev <- eigvals()
    p <- ncol(Xmat()); n <- nrow(Xmat())
    nplot <- min(length(ev), as.integer(input$nplot))
    nfl <- as.integer(input$nfit_line)
    nfl <- if (is.na(nfl) || nfl == 0) NULL else nfl
    
    pl <- spca::wachter_qqplot(
      eigenvalues = ev, p = p, n = n, gamma = n / p,
      cor = isTRUE(input$corr_mat),
      nplot = nplot,
      n_fitline = nfl,
      show_plot = FALSE, return_plot = TRUE
    )
    suppressMessages({
    print(pl)
    })
  })
  
  # ---------- Fit ----------
  fit <- reactiveVal(NULL)
  run_err <- reactiveVal(NULL)
  
  observeEvent(input$run, {
    run_err(NULL)
    output$status <- renderText("Running...")
    
    if (!requireNamespace("spca", quietly = TRUE)) {
      run_err("Package 'spca' not available.")
      fit(NULL)
      output$status <- renderText("ERROR")
      return()
    }
    
    X <- Xmat()
    
    if (isTRUE(input$intensive) && (!identical(input$selection, "fwd") || !identical(input$objective, "cvexp"))) {
      run_err("Intensive selection requires Forward selection and CVEXP objective.")
      fit(NULL)
      output$status <- renderText("ERROR")
      return()
    }
    if (ncol(X) > nrow(X) && (!identical(input$selection, "fwd") || isTRUE(input$intensive))) {
      run_err("Wide data use the fat-matrix backend, which supports forward selection only and not intensive selection.")
      fit(NULL)
      output$status <- renderText("ERROR")
      return()
    }
    
    obj <- tryCatch({
      withProgress(message = "Fitting SPCA model...", value = 0, {
        incProgress(0.15, detail = "Preparing model")
        ans <- spca::spca(
          M = X,
          n_comps = as.integer(input$ncomp),
          alpha = input$alpha,
          method = input$variant,
          var_selection = input$selection,
          objective = input$objective,
          intensive = isTRUE(input$intensive),
          pm_loading = isTRUE(input$pm_loading),
          pm_varsel = isTRUE(input$pm_varsel)
        )
        incProgress(1, detail = "Done")
        ans
      })
    }, error = function(e) e)
    
    if (inherits(obj, "error")) {
      run_err(conditionMessage(obj))
      fit(NULL)
      output$status <- renderText(paste("ERROR:", conditionMessage(obj)))
    } else {
      fit(obj)
      output$status <- renderText("Done.")
    }
  })
  
  output$run_msg <- renderText({
    if (!is.null(run_err())) run_err() else ""
  })
  
  observeEvent(input$compare_run, {
    compare_err(NULL)
    compare_fit(NULL)
    
    if (!requireNamespace("spca", quietly = TRUE)) {
      compare_err("Package 'spca' not available.")
      return()
    }
    
    X <- Xmat()
    
    if (isTRUE(input$compare_intensive) && (!identical(input$compare_selection, "fwd") || !identical(input$compare_objective, "cvexp"))) {
      compare_err("Intensive selection requires Forward selection and CVEXP objective.")
      return()
    }
    if (ncol(X) > nrow(X) && (!identical(input$compare_selection, "fwd") || isTRUE(input$compare_intensive))) {
      compare_err("Wide data use the fat-matrix backend, which supports forward selection only and not intensive selection.")
      return()
    }
    
    obj <- tryCatch({
      withProgress(message = "Fitting comparison SPCA model...", value = 0, {
        incProgress(0.15, detail = "Preparing comparison")
        ans <- spca::spca(
          M = X,
          n_comps = as.integer(input$compare_ncomp),
          alpha = input$compare_alpha,
          method = input$compare_variant,
          var_selection = input$compare_selection,
          objective = input$compare_objective,
          intensive = isTRUE(input$compare_intensive),
          pm_loading = isTRUE(input$compare_pm_loading),
          pm_varsel = isTRUE(input$compare_pm_varsel)
        )
        incProgress(1, detail = "Done")
        ans
      })
    }, error = function(e) e)
    
    if (inherits(obj, "error")) {
      compare_err(conditionMessage(obj))
    } else {
      compare_fit(obj)
    }
  })
  
  manual_fit <- reactiveVal(NULL)
  manual_err <- reactiveVal(NULL)
  compare_fit <- reactiveVal(NULL)
  compare_err <- reactiveVal(NULL)
  
  read_uploaded_matrix <- function(file_info, header = FALSE, sep = ",", what = "matrix") {
    if (is.null(file_info)) stop("Upload a ", what, " first.", call. = FALSE)
    if (is.null(sep)) sep <- ","
    tab <- utils::read.table(
      file_info$datapath,
      header = isTRUE(header),
      sep = sep,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    row_lab <- NULL
    M <- suppressWarnings(as.matrix(data.frame(lapply(tab, as.numeric), check.names = FALSE)))
    if (ncol(M) > 1L && all(is.na(M[, 1])) && all(is.finite(M[, -1, drop = FALSE]))) {
      row_lab <- as.character(tab[[1]])
      M <- M[, -1, drop = FALSE]
    }
    storage.mode(M) <- "double"
    if (!all(is.finite(M))) stop("The ", what, " must contain only finite numeric values, except for an optional first column of row names.", call. = FALSE)
    if (!is.null(row_lab)) rownames(M) <- row_lab
    M
  }
  
  manual_context_matrix <- function(A) {
    sep <- input$manual_sep %||% ","
    
    if (!is.null(input$manual_s_file)) {
      S <- read_uploaded_matrix(input$manual_s_file, input$manual_s_header, sep, "covariance/correlation matrix")
      if (nrow(S) != ncol(S)) stop("The covariance/correlation matrix must be square.", call. = FALSE)
      if (nrow(A) != ncol(S)) {
        stop(
          "The loading matrix has ", nrow(A), " rows, but the covariance/correlation matrix is ",
          ncol(S), " by ", ncol(S), ".",
          call. = FALSE
        )
      }
      if (!is.null(colnames(S))) rownames(A) <- colnames(S)
      return(list(A = A, X = NULL, S = S))
    }
    
    X <- Xmat()
    if (nrow(A) != ncol(X)) {
      stop(
        "The loading matrix has ", nrow(A), " rows, but the current data have ",
        ncol(X), " variables.",
        call. = FALSE
      )
    }
    rownames(A) <- colnames(X)
    list(A = A, X = X, S = NULL)
  }
  
  observeEvent(input$make_manual_spca, {
    manual_err(NULL)
    manual_fit(NULL)
    obj <- tryCatch({
      A <- read_uploaded_matrix(input$manual_loadings_file, input$manual_header, input$manual_sep, "loading matrix")
      mats <- manual_context_matrix(A)
      spca::new_spca(A = mats$A, S = mats$S, X = mats$X, method_name = "Manual loadings")
    }, error = function(e) e)
    
    if (inherits(obj, "error")) {
      manual_err(conditionMessage(obj))
    } else {
      manual_fit(obj)
    }
  })
  
  compare_obj <- reactive({
    if (identical(input$compare_source %||% "fit_new", "manual")) {
      manual_fit()
    } else {
      compare_fit()
    }
  })
  compare_result <- reactive({
    req(fit(), compare_obj())
    n_compare <- min(ncol(fit()$loadings), ncol(compare_obj()$loadings))
    vg <- if (isTRUE(scale_present()) && identical(input$sep_scales, "yes")) {
      scale_fac()
    } else {
      NULL
    }
    spca::compare_spca(
      list(fit(), compare_obj()),
      n_comps = n_compare,
      contributions = TRUE,
      only_nonzero = input$only_nonzero_plot %||% TRUE,
      variable_groups = vg,
      plot_loadings = TRUE,
      plot_type = input$plot_type %||% "bars",
      methods_names = c("Main fit", "Comparison"),
      color_scale = input$color_scale %||% "ggplot",
      print_loadings = FALSE,
      return_tables = TRUE,
      print_tables = FALSE,
      return_plot = TRUE,
      show_plot = FALSE
    )
  })
  
  output$compare_status <- renderText({
    if (identical(input$compare_source %||% "fit_new", "manual")) {
      if (!is.null(manual_err())) return(paste("ERROR:", manual_err()))
      if (!is.null(manual_fit())) return("Manual SPCA object created.")
      return("Upload a loading matrix. Optionally upload its covariance/correlation matrix to speed evaluation.")
    }
    if (!is.null(compare_err())) return(paste("ERROR:", compare_err()))
    if (!is.null(compare_fit())) return("Comparison model fitted.")
    "Fit another model with the current Data-tab matrix, or switch to uploaded loadings."
  })
  
  output$manual_summary_tbl <- renderDT({
    out <- compare_result()
    s <- out$summary
    fx <- format_summary_matrix(s)
    DT::datatable(
      fx,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = "t"
      ),
      class = "stripe hover nowrap",
      rownames = TRUE
    )
  })
  
  output$manual_compare_plot <- renderPlot({
    req(compare_result())
    print(compare_result()$loadings_plot)
  })
  output$dl_compare_rds <- downloadHandler(
    filename = function() paste0("spca_comparison_fit_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"),
    content = function(file) {
      req(compare_obj())
      saveRDS(compare_obj(), file = file)
    }
  )
  output$dl_compare_loadings_csv <- downloadHandler(
    filename = function() {
      paste0("spca_comparison_loadings_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(compare_obj())
      obj <- compare_obj()
      L <- tryCatch({
        if (isS4(obj) && "loadings" %in% methods::slotNames(obj)) {
          methods::slot(obj, "loadings")
        } else if (is.list(obj) && "loadings" %in% names(obj)) {
          obj$loadings
        } else {
          stop("Cannot find loadings in comparison object (no slot/element named 'loadings').")
        }
      }, error = function(e) {
        data.frame(Error = conditionMessage(e), stringsAsFactors = FALSE)
      })
      utils::write.csv(L, file = file, row.names = TRUE)
    }
  )
    # ---------- Results ----------
#SUMMARY TABLE============  
  
  output$sumtbl <- renderDT({
    req(fit())
    summary_args <- list(
      object = fit(),
      contributions = isTRUE(input$plotcontrib),
      min_load = FALSE,
      cor_with_pc = TRUE,
      return_table = TRUE,
      print_table = FALSE
    )
    s <- do.call(summary, summary_args)
    fx <- format_summary_matrix(s)
    DT::datatable(fx, options = list(pageLength = 10, scrollX = TRUE), rownames = TRUE)
  })
  # ---- Results UI helpers  
  output$scale_card_ui <- renderUI({
    if (!isTRUE(scale_present())) return(NULL)
    if (!identical(input$show_scale_list, "show")) return(NULL)
    
    card(
      card_header("Scale list (for plot.spca variable_groups)"),
      DTOutput("scale_tbl")
    )
  })
  # ---- Results table (depends on scale_fac / Xmat) 
  output$scale_tbl <- renderDT({
    req(scale_fac())
    sc <- scale_fac()
    vars <- colnames(Xmat())
    req(vars)
    
    if (length(sc) != length(vars)) {
      return(DT::datatable(
        data.frame(
          Error = paste0(
            "Scale length (", length(sc), 
            ") does not match number of variables (", length(vars), ")."
          )
        ),
        options = list(dom = "t")
      ))
    }
    
    tab <- data.frame(
      variable = vars,
      scale = as.character(sc),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(tab, options = list(pageLength = 25, scrollX = TRUE))
  })
  # Loading plot ====================
  # interactive 
  output$plot_spca_controls_ui <- renderUI({
    nmax <- as.integer(input$ncomp)
    if (is.na(nmax) || nmax < 1L) nmax <- 1L
    
    tagList(
      radioButtons(
        "sep_scales",
        "Separate scales (variable groups)",
        choices = c("No" = "no", "Yes" = "yes"),
        selected = if (isTRUE(scale_present())) "yes" else "no",
        inline = TRUE
      ),
      if (isTRUE(scale_present())) {
        radioButtons(
          "show_scale_list",
          "Scale list box",
          choices = c("Hide" = "hide", "Show" = "show"),
          selected = "hide",
          inline = TRUE
        )
      },
      
      selectInput("plot_type", "Plot type", choices = c("Bars" = "bars", "Circular" = "circular", "Heatmap" = "heatmap"), selected = "bars"),
      selectInput("color_scale", "Color scale", choices = c("ggplot", "cbb", "printsafe", "bw"), selected = "ggplot"),
      checkboxInput("only_nonzero_plot", "Plot only nonzero variables", TRUE),
      # 2) varnames (always available)
      radioButtons(
        "varnames_plot",
        "Show variable names",
        choices = c("No" = "no", "Yes" = "yes"),
        selected = "no",
        inline = TRUE
      ),
      
      # 3) Component to plot (1..ncomp), default = ncomp
      numericInput(
        "plot_comp",
        "Component to plot",
        value = nmax,
        min = 1,
        max = nmax,
        step = 1
      )
    )
  })
  #end interactive
  
  output$fitplot <- renderPlot({
    input$refresh_fitplot  # manual refresh trigger
    
    # also rerun when controls change
    input$sep_scales
    input$varnames_plot
    input$plot_comp
    input$plotcontrib
    
    req(fit())
    obj <- fit()
    
    use_sep <- isTRUE(scale_present()) && identical(input$sep_scales, "yes")
    vg <- if (use_sep) scale_fac() else NULL
    vn <- identical(input$varnames_plot, "yes")
    var_names <- if (vn) colnames(Xmat()) else "none"
    kplot <- as.integer(input$plot_comp)
    if (is.na(kplot) || kplot < 1L) kplot <- 1L
    kplot <- min(kplot, ncol(obj$loadings))
    controls <- list(
      color_scale = input$color_scale %||% "ggplot",
      variable_names = var_names,
      legend_position = if (use_sep) "bottom" else "none",
      grid_type = "horizontal"
    )
    plot_args <- list(
      x = obj,
      n_plot = kplot,
      plot_type = input$plot_type %||% "bars",
      contributions = isTRUE(input$plotcontrib),
      only_nonzero = isTRUE(input$only_nonzero_plot),
      variable_groups = vg,
      controls = controls,
      return_plot = TRUE,
      show_plot = FALSE
    )
    tryCatch({
      pl <- do.call(plot, plot_args)
      if (vn && requireNamespace("ggplot2", quietly = TRUE)) {
        pl <- pl + ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
      }
      print(pl)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Plot failed:", conditionMessage(e)))
    })
  })
  
  output$dl_rds <- downloadHandler(
    filename = function() paste0("spca_fit_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"),
    content = function(file) saveRDS(fit(), file = file)
  )
  output$dl_loadings_csv <- downloadHandler(
  filename = function() {
    paste0("spca_loadings_", format(Sys.Date(), "%Y%m%d"), ".csv")
  },
  content = function(file) {
    obj <- fit()
    L <- tryCatch({
      if (isS4(obj) && "loadings" %in% methods::slotNames(obj)) {
        methods::slot(obj, "loadings")
      } else if (is.list(obj) && "loadings" %in% names(obj)) {
        obj$loadings
      } else {
        stop("Cannot find loadings in fit object (no slot/element named 'loadings').")
      }
    }, error = function(e) {
      data.frame(Error = conditionMessage(e), stringsAsFactors = FALSE)
    })
    # Write matrix/data.frame; keep rownames (variable names) if present
    utils::write.csv(L, file = file, row.names = TRUE)
  }
)
}
shinyApp(ui, server)
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
