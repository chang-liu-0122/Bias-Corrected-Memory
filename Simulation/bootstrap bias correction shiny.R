library(shiny)
library(tidyverse)
library(DT)
library(xtable)

############################################################
# Method labels and order
############################################################

method_order_raw <- c("d_LW","d_peng","d_R1","d_R2","d_R1_bc","d_R2_bc","d_avg")

# Plain Unicode subscripts — used in checkboxes and wide table column names
method_labels <- c(
  d_LW    = "LW",
  d_peng  = "DFA",
  d_R1    = "LPWN\u2081",
  d_R2    = "LPWN\u2082",
  d_R1_bc = "BC-LPWN\u2081",
  d_R2_bc = "BC-LPWN\u2082",
  d_avg   = "LW-LPWN-AVG"
)
method_levels <- unname(method_labels[method_order_raw])

# Plotmath labels — used inside ggplot for subscript rendering
method_labels_plot <- c(
  d_LW    = "LW",
  d_peng  = "DFA",
  d_R1    = "LPWN[1]",
  d_R2    = "LPWN[2]",
  d_R1_bc = "BC-LPWN[1]",
  d_R2_bc = "BC-LPWN[2]",
  d_avg   = "LW-LPWN-AVG"
)
method_levels_plot <- unname(method_labels_plot[method_order_raw])

############################################################
# CSS  — UNCHANGED from original
############################################################

app_css <- "
@import url('https://fonts.googleapis.com/css2?family=Playfair+Display:ital,wght@0,400;0,600;1,400&family=Source+Serif+4:opsz,wght@8..60,300;8..60,400;8..60,600&family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@300;400;500&display=swap');

:root {
  --bg:        #f8f7f4;
  --panel:     #ffffff;
  --dark:      #1e1e1e;
  --accent:    #1c3557;
  --red:       #7a1515;
  --mid:       #4a6278;
  --border:    #c9c5bc;
  --border-lt: #e5e1d8;
  --text:      #1a1a1a;
  --muted:     #6b7280;
  --tag:       #eeeae2;
  --tag-blue:  #e6eef5;
  --tag-red:   #f5e6e6;
}

* { box-sizing: border-box; }

body {
  background: var(--bg);
  font-family: 'Source Serif 4', Georgia, serif;
  color: var(--text);
  font-size: 22px;
  line-height: 1.8;
}

.masthead {
  background: var(--dark);
  border-bottom: 3px solid var(--accent);
  padding: 32px 52px 28px;
}
.masthead-kicker {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 1rem;
  letter-spacing: 0.22em;
  text-transform: uppercase;
  color: rgba(255,255,255,0.38);
  margin-bottom: 8px;
}
.masthead-title {
  font-family: 'Playfair Display', serif;
  font-size: 3.5rem;
  font-weight: 600;
  color: #fff;
  line-height: 1.3;
  margin-bottom: 8px;
}
.masthead-sub {
  font-family: 'IBM Plex Sans', sans-serif;
  font-size: 1.2rem;
  color: rgba(255,255,255,0.45);
  font-weight: 300;
  letter-spacing: 0.04em;
}

.nav-tabs {
  background: var(--panel);
  border-bottom: 1px solid var(--border);
  padding: 0 28px;
  margin-bottom: 0;
}
.nav-tabs > li > a {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 1rem;
  font-weight: 500;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--muted) !important;
  border: none !important;
  border-bottom: 2px solid transparent !important;
  border-radius: 0 !important;
  padding: 16px 22px;
  margin-bottom: -1px;
  background: transparent !important;
}
.nav-tabs > li.active > a {
  color: var(--accent) !important;
  border-bottom: 2px solid var(--accent) !important;
}
.nav-tabs > li > a:hover { color: var(--accent) !important; }
.tab-content {
  background: var(--panel);
  border: 1px solid var(--border);
  border-top: none;
  padding: 36px 40px;
  box-shadow: 0 1px 5px rgba(0,0,0,0.05);
}

.well {
  background: var(--panel);
  border: 1px solid var(--border);
  border-top: 3px solid var(--accent);
  border-radius: 0;
  box-shadow: 0 1px 5px rgba(0,0,0,0.05);
  padding: 26px 24px;
}
.s-head {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 1rem;
  font-weight: 600;
  letter-spacing: 0.22em;
  text-transform: uppercase;
  color: var(--accent);
  margin: 28px 0 12px;
  padding-bottom: 8px;
  border-bottom: 2px solid var(--border-lt);
}
.s-head:first-child { margin-top: 0; }
.f-lbl {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.95rem;
  letter-spacing: 0.12em;
  color: var(--muted);
  margin: 18px 0 10px;
}

.sel-row { display: flex; gap: 8px; margin-bottom: 10px; }
.sel-btn {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.9rem;
  letter-spacing: 0.05em;
  padding: 6px 12px;
  border: 1px solid var(--border);
  border-radius: 2px;
  background: var(--tag);
  color: var(--accent);
  cursor: pointer;
  transition: background 0.12s;
  display: inline-block;
}
.sel-btn:hover { background: #ddd8ce; }
.sel-btn.desel { color: var(--red); }
.sel-btn.desel:hover { background: #f0e0e0; }

.set-wrap select.form-control {
  font-family: 'Playfair Display', serif;
  font-size: 0.5rem;
  font-weight: 600;
  border: 1px solid var(--border);
  border-radius: 2px;
  background: var(--tag);
  color: var(--text);
  padding: 2px 8px;
  height: 20px;
  min-height: 20px;
}
.btn-dl {
  width: 100%;
  background: var(--accent) !important;
  color: #fff !important;
  border: none !important;
  border-radius: 2px !important;
  font-family: 'IBM Plex Mono', monospace !important;
  font-size: 0.95rem !important;
  letter-spacing: 0.08em !important;
  padding: 10px 0 !important;
  margin-top: 4px !important;
  transition: background 0.14s !important;
}
.btn-dl:hover { background: #254a72 !important; }

.checkbox label {
  font-family: 'IBM Plex Sans', sans-serif !important;
  font-size: 1.2rem !important;
  text-transform: none !important;
  letter-spacing: 0 !important;
  color: var(--text) !important;
  line-height: 2 !important;
}
.shiny-input-container > label {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.9rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--muted);
}
select.form-control, .selectize-input {
  font-family: 'IBM Plex Mono', monospace !important;
  font-size: 1.08rem !important;
  line-height: 1.35 !important;
  height: 46px !important;
  min-height: 46px !important;
  padding: 10px 16px !important;
  display: flex !important;
  align-items: center !important;
  border: 1px solid var(--border) !important;
  border-radius: 2px !important;
  background: #fff !important;
  color: var(--text) !important;
}
.selectize-input > div, .selectize-input input {
  font-size: 1.08rem !important;
  line-height: 1.3 !important;
}
.shiny-input-container { margin-bottom: 12px; }
.shiny-input-container > label {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.72rem !important;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--muted);
  margin-bottom: 5px;
}
.selectize-input {
  min-height: 34px !important;
  padding: 6px 10px !important;
  font-size: 0.78rem !important;
  font-family: 'IBM Plex Mono', monospace !important;
  line-height: 1.2 !important;
}
.selectize-input.items.not-full.has-options { font-size: 0.78rem !important; }
.selectize-dropdown .option {
  font-size: 0.76rem !important;
  padding: 6px 10px !important;
}
hr { border-color: var(--border-lt); }

.plot-rule { display: flex; align-items: center; gap: 14px; margin: 18px 0 14px; }
.plot-rule-line { flex: 1; height: 1px; background: var(--border); }
.plot-rule-lbl {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.8rem;
  letter-spacing: 0.18em;
  text-transform: uppercase;
  color: var(--muted);
}

table.dataTable thead th {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 1rem;
  letter-spacing: 0.04em;
  background: #eeeae2;
  color: var(--accent);
  border-bottom: 1px solid var(--border) !important;
}
table.dataTable tbody td {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 1rem;
  color: var(--text);
  padding: 12px 14px !important;
}
table.dataTable.stripe tbody tr.odd { background: #faf8f5; }

.about-hero {
  background: var(--dark);
  color: #fff;
  padding: 50px 56px;
  margin-bottom: 34px;
  border-left: 4px solid var(--accent);
}
.about-hero-eyebrow {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.85rem;
  letter-spacing: 0.2em;
  text-transform: uppercase;
  opacity: 0.42;
  margin-bottom: 10px;
}
.about-hero-title {
  font-family: 'Playfair Display', serif;
  font-size: 3rem;
  font-weight: 600;
  line-height: 1.35;
  margin-bottom: 12px;
}
.about-hero-meta {
  font-family: 'IBM Plex Sans', sans-serif;
  font-size: 1.1rem;
  opacity: 0.6;
  font-style: italic;
}
.about-card {
  border: 1px solid var(--border);
  border-top: 2px solid var(--accent);
  padding: 34px 38px;
  margin-bottom: 26px;
  background: var(--panel);
}
.about-card-label {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.82rem;
  letter-spacing: 0.2em;
  text-transform: uppercase;
  color: var(--red);
  margin-bottom: 14px;
  padding-bottom: 8px;
  border-bottom: 1px solid var(--border-lt);
}
.about-card p { font-size: 1.3rem; line-height: 1.9; color: #2e3a44; margin: 0; }
.e-grid { display: grid; grid-template-columns: 1fr; gap: 8px; margin-top: 8px; }
.e-chip { border: 1px solid var(--border); padding: 10px 14px; background: var(--tag); min-height: unset; }
.e-chip-name { font-family: 'IBM Plex Mono', monospace; font-size: 0.88rem; font-weight: 600; color: var(--accent); line-height: 1.2; }
.e-chip-desc { font-size: 0.78rem; color: var(--muted); font-family: 'IBM Plex Sans', sans-serif; margin-top: 3px; line-height: 1.35; }
.p-grid { display: flex; flex-wrap: wrap; gap: 10px; margin-top: 12px; }
.p-badge { background: var(--accent); color: #fff; font-family: 'IBM Plex Mono', monospace; font-size: 1rem; padding: 8px 16px; letter-spacing: 0.04em; }
"

############################################################
# Helper functions
############################################################

prepare_long_summary <- function(df, type = c("FAR","FARMA")) {
  type    <- match.arg(type)
  id_vars <- if (type=="FAR") c("T","ar_strength","d_true","method")
  else c("T","ar_strength","ma_strength","d_true","method")
  df %>%
    pivot_longer(cols=all_of(method_order_raw), names_to="method", values_to="d_hat") %>%
    mutate(error = d_hat - d_true) %>%
    group_by(across(all_of(id_vars))) %>%
    summarise(
      bias = round(mean(error, na.rm=TRUE), 4),
      sd   = round(sd(d_hat,  na.rm=TRUE), 4),
      mse  = round(mean(error^2, na.rm=TRUE), 4),
      .groups="drop"
    )
}

prepare_plot_df <- function(df_summary) {
  df_summary %>%
    mutate(
      d_true      = factor(d_true, levels=sort(unique(as.numeric(as.character(d_true))))),
      T           = factor(T),
      ar_strength = factor(ar_strength),
      ma_strength = if ("ma_strength" %in% names(.)) factor(ma_strength) else NULL,
      method      = recode(method, !!!method_labels_plot),
      method      = factor(method, levels=method_levels_plot)
    )
}

make_wide_table <- function(df_summary) {
  id_cols <- c("T","ar_strength","ma_strength","d_true")
  id_cols <- id_cols[id_cols %in% names(df_summary)]
  out <- df_summary %>%
    mutate(
      method       = factor(method, levels=method_order_raw),
      method_label = recode(as.character(method), !!!method_labels)
    ) %>%
    select(all_of(id_cols), method_label, bias, sd, mse) %>%
    pivot_wider(
      names_from  = method_label,
      values_from = c(bias, sd, mse),
      names_glue  = "{method_label}__{.value}"
    )
  desired <- c(
    id_cols,
    paste0(method_levels, "__bias"),
    paste0(method_levels, "__sd"),
    paste0(method_levels, "__mse")
  )
  out %>% select(all_of(desired[desired %in% names(out)]))
}

plot_perf_app <- function(df, methods_keep, yvar=c("bias","sd","mse"),
                          facet_row="T", facet_col="ar_strength",
                          title=NULL, show_legend=TRUE, show_xlab=FALSE) {
  yvar   <- match.arg(yvar)

  # convert checkbox labels -> plotmath labels
  methods_keep_plot <- recode(
    methods_keep,
    !!!setNames(method_levels_plot, method_levels)
  )

  df_sub <- df %>%
    filter(as.character(method) %in% methods_keep_plot) %>%
    mutate(method = factor(method, levels = methods_keep_plot))

  ylbl <- switch(yvar, bias="Bias", sd="SD", mse="MSE")

  strip_labeller <- labeller(
    T           = function(x) paste0("T = ", x),
    ar_strength = function(x) paste0("\u03c6 = ", x),
    ma_strength = function(x) paste0("\u03bd = ", x)
  )

  p <- ggplot(df_sub, aes(x=d_true, y=.data[[yvar]], group=method)) +
    geom_line(aes(linetype=method), linewidth=0.3, alpha=0.7) +
    geom_point(aes(shape=method), size=1.5) +
    facet_grid(
      rows     = vars(.data[[facet_row]]),
      cols     = vars(.data[[facet_col]]),
      labeller = strip_labeller
    ) +
    labs(
      title    = title,
      x        = if (show_xlab) "True memory parameter d" else NULL,
      y        = ylbl,
      linetype = "Estimators",
      shape    = "Estimators"
    ) +
    scale_linetype_discrete(labels = function(x) parse(text = x)) +
    scale_shape_discrete(labels    = function(x) parse(text = x)) +
    theme_classic(base_size=18) +
    theme(
      axis.text.x      = if (show_xlab) element_text(angle=45, hjust=1) else element_blank(),
      axis.ticks.x     = if (show_xlab) element_line() else element_blank(),
      legend.position  = if (show_legend) "top" else "none",
      strip.background = element_rect(fill="#eeeae2", colour="#c9c5bc"),
      strip.text       = element_text(colour="#1c3557", size=9, family="mono")
    )

  if (yvar == "bias")
    p <- p + geom_hline(yintercept=0, colour="red", linewidth=0.4, linetype="dashed")

  p
}

############################################################
# Load data
############################################################

LPWN_LW_sim_comparison_FAR <- readRDS("LPWN_LW_sim_comparison_FAR.rds")
peng_FAR                    <- readRDS("bootstrap_comparison_peng_FAR.rds")
LPWN_LW_sim_comparison_FAR <- LPWN_LW_sim_comparison_FAR %>%
  left_join(peng_FAR, by=c("T","ar_strength","d_true","seed")) %>%
  mutate(d_avg=(d_LW+d_R2_bc)/2)

LPWN_LW_sim_comparison_FARMA <- readRDS("LPWN_LW_sim_comparison_FARMA.rds")
peng_FARMA <- readRDS("bootstrap_comparison_peng_FARMA.rds") %>%
  rename(ar_strength=strength_ar, ma_strength=strength_ma, d_peng=sample_peng) %>%
  select(-sample_whittle)
LPWN_LW_sim_comparison_FARMA <- LPWN_LW_sim_comparison_FARMA %>%
  left_join(peng_FARMA, by=c("T","ar_strength","ma_strength","d_true","seed")) %>%
  mutate(d_avg=(d_LW+d_R2_bc)/2)

############################################################
# Pre-compute choice vectors
############################################################

FAR_choices <- list(
  T           = sort(unique(LPWN_LW_sim_comparison_FAR$T)),
  ar_strength = sort(unique(LPWN_LW_sim_comparison_FAR$ar_strength)),
  d_true      = sort(unique(LPWN_LW_sim_comparison_FAR$d_true))
)
FARMA_choices <- list(
  T           = sort(unique(LPWN_LW_sim_comparison_FARMA$T)),
  ar_strength = sort(unique(LPWN_LW_sim_comparison_FARMA$ar_strength)),
  ma_strength = sort(unique(LPWN_LW_sim_comparison_FARMA$ma_strength)),
  d_true      = sort(unique(LPWN_LW_sim_comparison_FARMA$d_true))
)

############################################################
# Reusable select/deselect widget
############################################################

sel_row_ui <- function(input_id) {
  div(class="sel-row",
      tags$button("Select All",   class="sel-btn",
                  onclick=sprintf("Shiny.setInputValue('_sel_%s', Math.random())", input_id)),
      tags$button("Deselect All", class="sel-btn desel",
                  onclick=sprintf("Shiny.setInputValue('_des_%s', Math.random())", input_id))
  )
}

############################################################
# UI
############################################################

ui <- fluidPage(

  tags$head(tags$style(HTML(app_css))),

  div(class="masthead",
      div(class="masthead-kicker", "Simulation Study · Functional Time Series"),
      div(class="masthead-title",  "Memory Estimator Simulation Explorer"),
      div(class="masthead-sub",
          "FARFIMA long-memory estimation · Bias & MSE comparison across estimators")
  ),

  br(),

  sidebarLayout(

    sidebarPanel(width=3,

                 div(class="s-head", "Simulation Setting"),
                 div(class="set-wrap",
                     selectInput("data_type", NULL,
                                 choices  = c("FAR — FARFIMA(1,d,0)"  = "FAR",
                                              "FARMA — FARFIMA(1,d,1)" = "FARMA"),
                                 selected = "FAR")
                 ),

                 conditionalPanel(
                   condition = "input.main_tabs == 'Summary Table'",
                   div(class="s-head", "Export"),
                   downloadButton("download_latex", "Download LaTeX Table", class="btn-dl")
                 ),

                 div(class="s-head", "Filters"),

                 div(class="f-lbl", "Sample Size T"),
                 sel_row_ui("T_filter"),
                 checkboxGroupInput("T_filter", NULL,
                                    choices=FAR_choices$T, selected=FAR_choices$T[1]),

                 div(class="f-lbl", HTML("AR Strength \u03c6")),
                 sel_row_ui("ar_filter"),
                 checkboxGroupInput("ar_filter", NULL,
                                    choices=FAR_choices$ar_strength, selected=FAR_choices$ar_strength[1]),

                 conditionalPanel(
                   condition = "input.data_type == 'FARMA'",
                   div(class="f-lbl", HTML("MA Strength \u03bd")),
                   sel_row_ui("ma_filter"),
                   checkboxGroupInput("ma_filter", NULL,
                                      choices=FARMA_choices$ma_strength, selected=FARMA_choices$ma_strength[1])
                 ),

                 div(class="f-lbl", "True Memory d"),
                 sel_row_ui("d_filter"),
                 checkboxGroupInput("d_filter", NULL,
                                    choices=FAR_choices$d_true, selected=FAR_choices$d_true[1]),

                 conditionalPanel(
                   condition = "input.main_tabs == 'Plot'",
                   div(class="s-head", "Estimators"),
                   sel_row_ui("methods"),
                   checkboxGroupInput("methods", NULL, choices=method_levels,
                                      selected=method_levels[c(1,2,4,6)]),
                   div(class="s-head", "Facets"),
                   uiOutput("facet_ui"),
                   div(class="s-head", "Export"),
                   downloadButton("download_plot", "Download Plot PDF", class="btn-dl")
                 )
    ),

    mainPanel(width=9,
              tabsetPanel(id="main_tabs",

                          # ── About ──
                          tabPanel("About",
                                   br(),
                                   div(class="about-hero",
                                       div(class="about-hero-eyebrow", "Functional Time Series"),
                                       div(class="about-hero-title",
                                           "Bias Correction of Long-Memory Estimator of Functional Time Series via the Prefiltered Sieve Bootstrap"),
                                       div(class="about-hero-meta", "Simulation study companion dashboard")
                                   ),
                                   fluidRow(
                                     column(7,
                                            div(class="about-card",
                                                div(class="about-card-label", "Abstract"),
                                                p("We investigate a sieve-bootstrap-based bias correction to estimate the long-memory
                  parameter d in fractionally integrated functional time series. The resampling method
                  implements a sieve bootstrap applied to data prefiltered by a preliminary estimate
                  of d. For the initial estimate, we recommend the local polynomial Whittle estimator
                  with noise (LPWN), which reduces bias induced by short-range dependence relative to
                  the standard local Whittle and DFA estimators. The bootstrap procedure further
                  corrects the remaining bias of the LPWN estimator, yielding the BC-LPWN estimators
                  evaluated in this simulation study.")
                                            ),
                                            div(class="about-card",
                                                div(class="about-card-label", "Simulation Design"),
                                                p(HTML("Functional FARFIMA(<i>p</i>,<i>d</i>,<i>q</i>) processes are simulated on [0,1]
                  across two settings — <b>Case 1 (FAR)</b>: FARFIMA(1,<i>d</i>,0) with a Gaussian AR kernel;
                  <b>Case 2 (FARMA)</b>: FARFIMA(1,<i>d</i>,1) augmented by a covariance-type MA kernel.
                  Operator norms ∈ {0.2, 0.5, 0.8} reflect weak, moderate, and strong short-run dependence.
                  Each configuration is replicated 100 times; performance is measured by average bias and MSE.")),
                                                div(class="about-card-label", style="margin-top:14px;", "Parameter Grid"),
                                                div(class="p-grid",
                                                    div(class="p-badge","T ∈ {500, 1000, 2000}"),
                                                    div(class="p-badge","d ∈ {0.1, …, 1.4}"),
                                                    div(class="p-badge","‖ϕ‖ ∈ {0.2, 0.5, 0.8}"),
                                                    div(class="p-badge","100 replications")
                                                )
                                            )
                                     ),
                                     column(5,
                                            div(class="about-card",
                                                div(class="about-card-label", "Estimators Compared"),
                                                div(class="e-grid",
                                                    div(class="e-chip",div(class="e-chip-name","LW"),
                                                        div(class="e-chip-desc","Local Whittle (benchmark)")),
                                                    div(class="e-chip",div(class="e-chip-name","DFA"),
                                                        div(class="e-chip-desc","Detrended Fluctuation Analysis (benchmark)")),
                                                    div(class="e-chip",div(class="e-chip-name",HTML("LPWN\u2081")),
                                                        div(class="e-chip-desc","Local Poly. Whittle with Order 1")),
                                                    div(class="e-chip",div(class="e-chip-name",HTML("LPWN\u2082")),
                                                        div(class="e-chip-desc","Local Poly. Whittle with Order 2")),
                                                    div(class="e-chip",div(class="e-chip-name",HTML("BC-LPWN\u2081")),
                                                        div(class="e-chip-desc",HTML("Bootstrap-corrected LPWN\u2081"))),
                                                    div(class="e-chip",div(class="e-chip-name",HTML("BC-LPWN\u2082")),
                                                        div(class="e-chip-desc",HTML("Bootstrap-corrected LPWN\u2082"))),
                                                    div(class="e-chip",div(class="e-chip-name","LW-LPWN-AVG"),
                                                        div(class="e-chip-desc",HTML("Average of LW and BC-LPWN\u2082")))
                                                )
                                            ),
                                            div(class="about-card",
                                                div(class="about-card-label", "How to Use"),
                                                p(HTML("Navigate to <b>Summary Table</b> to inspect bias and MSE filtered by sample
                  size, dependence strength, and true memory d. Use the <b>Plot</b> tab to compare
                  estimators visually across facetted configurations. Use <em>Select / Deselect All</em>
                  in the sidebar for quick filter adjustments. Export LaTeX tables or stacked PDF
                  plots from the sidebar."))
                                            )
                                     )
                                   )
                          ),

                          # ── Summary Table ──
                          tabPanel("Summary Table",
                                   br(),
                                   DTOutput("summary_table")
                          ),

                          # ── Plot ──
                          tabPanel("Plot",
                                   br(),
                                   div(class="plot-rule",
                                       div(class="plot-rule-line"),
                                       div(class="plot-rule-lbl","Bias"),
                                       div(class="plot-rule-line")
                                   ),
                                   plotOutput("perf_plot_bias", height="480px"),
                                   div(class="plot-rule",
                                       div(class="plot-rule-line"),
                                       div(class="plot-rule-lbl","Standard Deviation"),
                                       div(class="plot-rule-line")
                                   ),
                                   plotOutput("perf_plot_sd", height="420px"),
                                   div(class="plot-rule",
                                       div(class="plot-rule-line"),
                                       div(class="plot-rule-lbl","Mean Squared Error"),
                                       div(class="plot-rule-line")
                                   ),
                                   plotOutput("perf_plot_mse", height="420px")
                          )
              )
    )
  )
)

############################################################
# Server
############################################################

`%||%` <- function(a, b) if (!is.null(a)) a else b

server <- function(input, output, session) {

  observeEvent(input$data_type, {
    ch <- if (input$data_type=="FAR") FAR_choices else FARMA_choices
    updateCheckboxGroupInput(session,"T_filter",  choices=ch$T,           selected=ch$T[1])
    updateCheckboxGroupInput(session,"ar_filter", choices=ch$ar_strength, selected=ch$ar_strength[1])
    updateCheckboxGroupInput(session,"d_filter",  choices=ch$d_true,      selected=ch$d_true[1])
    if (input$data_type=="FARMA")
      updateCheckboxGroupInput(session,"ma_filter", choices=ch$ma_strength, selected=ch$ma_strength[1])
  }, ignoreInit=TRUE)

  # Select / Deselect All
  observeEvent(input[["_sel_T_filter"]], {
    ch <- if (input$data_type=="FAR") FAR_choices$T else FARMA_choices$T
    updateCheckboxGroupInput(session,"T_filter", choices=ch, selected=ch)
  }, ignoreNULL=TRUE, ignoreInit=TRUE)
  observeEvent(input[["_des_T_filter"]], {
    ch <- if (input$data_type=="FAR") FAR_choices$T else FARMA_choices$T
    updateCheckboxGroupInput(session,"T_filter", choices=ch, selected=character(0))
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  observeEvent(input[["_sel_ar_filter"]], {
    ch <- if (input$data_type=="FAR") FAR_choices$ar_strength else FARMA_choices$ar_strength
    updateCheckboxGroupInput(session,"ar_filter", choices=ch, selected=ch)
  }, ignoreNULL=TRUE, ignoreInit=TRUE)
  observeEvent(input[["_des_ar_filter"]], {
    ch <- if (input$data_type=="FAR") FAR_choices$ar_strength else FARMA_choices$ar_strength
    updateCheckboxGroupInput(session,"ar_filter", choices=ch, selected=character(0))
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  observeEvent(input[["_sel_ma_filter"]], {
    updateCheckboxGroupInput(session,"ma_filter",
                             choices=FARMA_choices$ma_strength, selected=FARMA_choices$ma_strength)
  }, ignoreNULL=TRUE, ignoreInit=TRUE)
  observeEvent(input[["_des_ma_filter"]], {
    updateCheckboxGroupInput(session,"ma_filter",
                             choices=FARMA_choices$ma_strength, selected=character(0))
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  observeEvent(input[["_sel_d_filter"]], {
    ch <- if (input$data_type=="FAR") FAR_choices$d_true else FARMA_choices$d_true
    updateCheckboxGroupInput(session,"d_filter", choices=ch, selected=ch)
  }, ignoreNULL=TRUE, ignoreInit=TRUE)
  observeEvent(input[["_des_d_filter"]], {
    ch <- if (input$data_type=="FAR") FAR_choices$d_true else FARMA_choices$d_true
    updateCheckboxGroupInput(session,"d_filter", choices=ch, selected=character(0))
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  observeEvent(input[["_sel_methods"]], {
    updateCheckboxGroupInput(session,"methods", choices=method_levels, selected=method_levels)
  }, ignoreNULL=TRUE, ignoreInit=TRUE)
  observeEvent(input[["_des_methods"]], {
    updateCheckboxGroupInput(session,"methods", choices=method_levels, selected=character(0))
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  output$facet_ui <- renderUI({
    fc <- if (input$data_type=="FAR") c("T","ar_strength")
    else c("T","ar_strength","ma_strength")
    tagList(
      selectInput("facet_row","Facet Rows",    choices=fc, selected="T"),
      selectInput("facet_col","Facet Columns", choices=fc, selected="ar_strength")
    )
  })

  raw_data <- reactive({
    if (input$data_type=="FAR") LPWN_LW_sim_comparison_FAR
    else LPWN_LW_sim_comparison_FARMA
  })

  summary_data <- reactive({
    prepare_long_summary(raw_data(), type=input$data_type)
  })

  filtered_summary <- reactive({
    df     <- summary_data()
    T_sel  <- input$T_filter  %||% character(0)
    ar_sel <- input$ar_filter %||% character(0)
    d_sel  <- input$d_filter  %||% character(0)
    df <- df %>%
      filter(as.character(T)           %in% as.character(T_sel),
             as.character(ar_strength) %in% as.character(ar_sel),
             as.character(d_true)      %in% as.character(d_sel))
    if (input$data_type=="FARMA") {
      ma_sel <- input$ma_filter %||% character(0)
      df <- df %>% filter(as.character(ma_strength) %in% as.character(ma_sel))
    }
    df
  })

  filtered_plot_df <- reactive({ prepare_plot_df(filtered_summary()) })
  wide_table       <- reactive({ make_wide_table(filtered_summary()) })

  output$summary_table <- renderDT({
    df <- wide_table()
    validate(need(nrow(df) > 0,
                  "No data matches the current filter selection. Please select at least one value per filter."))

    id_cols <- c("T","ar_strength","ma_strength","d_true")
    id_cols <- id_cols[id_cols %in% names(df)]
    n_id    <- length(id_cols)

    bias_cols <- grep("__bias$", names(df), value=TRUE)
    est_names <- gsub("__bias$","", bias_cols)
    n_est     <- length(est_names)

    # Greek display names for configuration columns
    id_display <- c(T="T", ar_strength="\u03c6", ma_strength="\u03bd", d_true="d")[id_cols]

    th_style <- function(bg, col, bord)
      sprintf("text-align:center;background:%s;color:%s;border-bottom:2px solid %s;
               font-family:IBM Plex Mono,monospace;font-size:0.67rem;
               letter-spacing:0.16em;text-transform:uppercase;padding:8px 4px;",
              bg, col, bord)

    # Row 1: config (rowspan=2) + Bias + SD + MSE super-headers
    # Config cells go in row 1 with rowspan=2; metric super-headers also in row 1
    config_cells <- paste(
      sprintf("<th rowspan='2' style='%s'>%s</th>",
              th_style("#eeeae2","#1c3557","#1c3557"), id_display),
      collapse="")

    bias_th <- sprintf("<th colspan='%d' style='%s'>Bias</th>",
                       n_est, th_style("#e6eef5","#1c3557","#1c3557"))
    sd_th   <- sprintf("<th colspan='%d' style='%s'>SD</th>",
                       n_est, th_style("#e8f2ec","#1a5c38","#1a5c38"))
    mse_th  <- sprintf("<th colspan='%d' style='%s'>MSE</th>",
                       n_est, th_style("#f5e6e6","#7a1515","#7a1515"))

    # Row 2: estimator names repeated 3x (bias, sd, mse)
    est_cells <- paste(
      sprintf("<th style='font-family:IBM Plex Mono,monospace;font-size:0.64rem;
               white-space:nowrap;text-align:center;padding:5px 6px;'>%s</th>",
              rep(est_names, 3)),
      collapse="")

    sketch <- htmltools::withTags(table(
      class = "display",
      thead(
        HTML(paste0("<tr>", config_cells, bias_th, sd_th, mse_th, "</tr>")),
        HTML(paste0("<tr>", est_cells, "</tr>"))
      )
    ))

    datatable(
      df, container=sketch,
      colnames=rep("", ncol(df)),
      rownames=FALSE,
      options=list(pageLength=20, scrollX=TRUE, ordering=TRUE, dom="lftip")
    ) %>% formatRound(columns=(n_id+1):ncol(df), digits=4)
  })

  output$perf_plot_bias <- renderPlot({
    req(input$methods, input$facet_row, input$facet_col)
    validate(need(nrow(filtered_plot_df())>0,"No data — adjust filters."))
    plot_perf_app(filtered_plot_df(), input$methods, "bias",
                  input$facet_row, input$facet_col,
                  NULL, show_legend=TRUE, show_xlab=FALSE)
  })

  output$perf_plot_sd <- renderPlot({
    req(input$methods, input$facet_row, input$facet_col)
    validate(need(nrow(filtered_plot_df())>0,"No data — adjust filters."))
    plot_perf_app(filtered_plot_df(), input$methods, "sd",
                  input$facet_row, input$facet_col,
                  NULL, show_legend=FALSE, show_xlab=FALSE)
  })

  output$perf_plot_mse <- renderPlot({
    req(input$methods, input$facet_row, input$facet_col)
    validate(need(nrow(filtered_plot_df())>0,"No data — adjust filters."))
    plot_perf_app(filtered_plot_df(), input$methods, "mse",
                  input$facet_row, input$facet_col,
                  NULL, show_legend=FALSE, show_xlab=TRUE)
  })

  output$download_latex <- downloadHandler(
    filename = function() paste0("summary_table_",input$data_type,".tex"),
    content  = function(file) {
      df <- wide_table()
      colnames(df) <- gsub("__bias$"," (Bias)", names(df))
      colnames(df) <- gsub("__sd$",  " (SD)",   colnames(df))
      colnames(df) <- gsub("__mse$", " (MSE)",  colnames(df))
      sink(file)
      print(xtable(df, digits=4), type="latex", include.rownames=FALSE)
      sink()
    }
  )

  output$download_plot <- downloadHandler(
    filename = function() paste0("plot_",input$data_type,"_bias_sd_mse.pdf"),
    content  = function(file) {
      pb <- plot_perf_app(filtered_plot_df(),input$methods,"bias",
                          input$facet_row,input$facet_col,NULL,TRUE,FALSE)
      ps <- plot_perf_app(filtered_plot_df(),input$methods,"sd",
                          input$facet_row,input$facet_col,NULL,FALSE,FALSE)
      pm <- plot_perf_app(filtered_plot_df(),input$methods,"mse",
                          input$facet_row,input$facet_col,NULL,FALSE,TRUE)
      if (requireNamespace("patchwork",quietly=TRUE)) {
        library(patchwork)
        ggsave(file, pb/ps/pm, width=10, height=16, dpi=300)
      } else {
        pdf(file,width=10,height=16); print(pb); print(ps); print(pm); dev.off()
      }
    }
  )
}

shinyApp(ui, server)
#' library(shiny)
#' library(tidyverse)
#' library(DT)
#' library(xtable)
#'
#' ############################################################
#' # Method labels and order
#' ############################################################
#'
#' method_order_raw <- c("d_LW","d_peng","d_R1","d_R2","d_R1_bc","d_R2_bc","d_avg")
#'
#' method_labels <- c(
#'   d_LW    = "LW",       d_peng  = "DFA",
#'   d_R1    = "LPWN(1)",  d_R2    = "LPWN(2)",
#'   d_R1_bc = "BC-LPWN(1)", d_R2_bc = "BC-LPWN(2)",
#'   d_avg   = "LW-LPWN-AVG"
#' )
#' method_levels <- unname(method_labels[method_order_raw])
#'
#' ############################################################
#' # CSS
#' ############################################################
#'
#' app_css <- "
#' @import url('https://fonts.googleapis.com/css2?family=Playfair+Display:ital,wght@0,400;0,600;1,400&family=Source+Serif+4:opsz,wght@8..60,300;8..60,400;8..60,600&family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@300;400;500&display=swap');
#'
#' :root {
#'   --bg:        #f8f7f4;
#'   --panel:     #ffffff;
#'   --dark:      #1e1e1e;
#'   --accent:    #1c3557;
#'   --red:       #7a1515;
#'   --mid:       #4a6278;
#'   --border:    #c9c5bc;
#'   --border-lt: #e5e1d8;
#'   --text:      #1a1a1a;
#'   --muted:     #6b7280;
#'   --tag:       #eeeae2;
#'   --tag-blue:  #e6eef5;
#'   --tag-red:   #f5e6e6;
#' }
#'
#' * { box-sizing: border-box; }
#'
#' body {
#'   background: var(--bg);
#'   font-family: 'Source Serif 4', Georgia, serif;
#'   color: var(--text);
#'   font-size: 22px;
#'   line-height: 1.8;
#' }
#'
#' /* Masthead */
#' .masthead {
#'   background: var(--dark);
#'   border-bottom: 3px solid var(--accent);
#'   padding: 32px 52px 28px;
#' }
#'
#' .masthead-kicker {
#'   font-family: 'IBM Plex Mono', monospace;
#'   font-size: 1rem;
#'   letter-spacing: 0.22em;
#'   text-transform: uppercase;
#'   color: rgba(255,255,255,0.38);
#'   margin-bottom: 8px;
#' }
#'
#' .masthead-title {
#'   font-family: 'Playfair Display', serif;
#'   font-size: 3.5rem;
#'   font-weight: 600;
#'   color: #fff;
#'   line-height: 1.3;
#'   margin-bottom: 8px;
#' }
#'
#' .masthead-sub {
#'   font-family: 'IBM Plex Sans', sans-serif;
#'   font-size: 1.2rem;
#'   color: rgba(255,255,255,0.45);
#'   font-weight: 300;
#'   letter-spacing: 0.04em;
#' }
#'
#' /* Nav tabs */
#' .nav-tabs {
#'   background: var(--panel);
#'   border-bottom: 1px solid var(--border);
#'   padding: 0 28px;
#'   margin-bottom: 0;
#' }
#'
#' .nav-tabs > li > a {
#'   font-family: 'IBM Plex Mono', monospace;
#'   font-size: 1rem;
#'   font-weight: 500;
#'   letter-spacing: 0.12em;
#'   text-transform: uppercase;
#'   color: var(--muted) !important;
#'   border: none !important;
#'   border-bottom: 2px solid transparent !important;
#'   border-radius: 0 !important;
#'   padding: 16px 22px;
#'   margin-bottom: -1px;
#'   background: transparent !important;
#' }
#'
#' .nav-tabs > li.active > a {
#'   color: var(--accent) !important;
#'   border-bottom: 2px solid var(--accent) !important;
#' }
#'
#' .nav-tabs > li > a:hover {
#'   color: var(--accent) !important;
#' }
#'
#' .tab-content {
#'   background: var(--panel);
#'   border: 1px solid var(--border);
#'   border-top: none;
#'   padding: 36px 40px;
#'   box-shadow: 0 1px 5px rgba(0,0,0,0.05);
#' }
#'
#' /* Sidebar */
#' .well {
#'   background: var(--panel);
#'   border: 1px solid var(--border);
#'   border-top: 3px solid var(--accent);
#'   border-radius: 0;
#'   box-shadow: 0 1px 5px rgba(0,0,0,0.05);
#'   padding: 26px 24px;
#' }
#'
#' .s-head {
#'   font-family: 'IBM Plex Mono', monospace;
#'   font-size: 1rem;
#'   font-weight: 600;
#'   letter-spacing: 0.22em;
#'   text-transform: uppercase;
#'   color: var(--accent);
#'   margin: 28px 0 12px;
#'   padding-bottom: 8px;
#'   border-bottom: 2px solid var(--border-lt);
#' }
#'
#' .s-head:first-child {
#'   margin-top: 0;
#' }
#'
#' .f-lbl {
#'   font-family: 'IBM Plex Mono', monospace;
#'   font-size: 0.95rem;
#'   letter-spacing: 0.12em;
#'   text-transform: uppercase;
#'   color: var(--muted);
#'   margin: 18px 0 10px;
#' }
#'
#' /* Select / Deselect All buttons */
#' .sel-row {
#'   display: flex;
#'   gap: 8px;
#'   margin-bottom: 10px;
#' }
#'
#' .sel-btn {
#'   font-family: 'IBM Plex Mono', monospace;
#'   font-size: 0.9rem;
#'   letter-spacing: 0.05em;
#'   padding: 6px 12px;
#'   border: 1px solid var(--border);
#'   border-radius: 2px;
#'   background: var(--tag);
#'   color: var(--accent);
#'   cursor: pointer;
#'   transition: background 0.12s;
#'   display: inline-block;
#' }
#'
#' .sel-btn:hover {
#'   background: #ddd8ce;
#' }
#'
#' .sel-btn.desel {
#'   color: var(--red);
#' }
#'
#' .sel-btn.desel:hover {
#'   background: #f0e0e0;
#' }
#'
#' /* Setting dropdown */
#' .set-wrap select.form-control {
#'   font-family: 'Playfair Display', serif;
#'   font-size: 0.5rem;
#'   font-weight: 600;
#'   border: 1px solid var(--border);
#'   border-radius: 2px;
#'   background: var(--tag);
#'   color: var(--text);
#'   padding: 2px 8px;
#'   height: 20px;
#'   min-height: 20px;
#' }
#'
#' /* Download button */
#' .btn-dl {
#'   width: 100%;
#'   background: var(--accent) !important;
#'   color: #fff !important;
#'   border: none !important;
#'   border-radius: 2px !important;
#'   font-family: 'IBM Plex Mono', monospace !important;
#'   font-size: 0.95rem !important;
#'   letter-spacing: 0.08em !important;
#'   padding: 10px 0 !important;
#'   margin-top: 4px !important;
#'   transition: background 0.14s !important;
#' }
#'
#' .btn-dl:hover {
#'   background: #254a72 !important;
#' }
#'
#' /* Checkbox */
#' .checkbox label {
#'   font-family: 'IBM Plex Sans', sans-serif !important;
#'   font-size: 1.2rem !important;
#'   text-transform: none !important;
#'   letter-spacing: 0 !important;
#'   color: var(--text) !important;
#'   line-height: 2 !important;
#' }
#'
#' .shiny-input-container > label {
#'   font-family: 'IBM Plex Mono', monospace;
#'   font-size: 0.9rem;
#'   letter-spacing: 0.1em;
#'   text-transform: uppercase;
#'   color: var(--muted);
#' }
#'
#' /* General dropdown */
#' /* Main dropdown */
#' /* Main dropdown */
#' select.form-control,
#' .selectize-input {
#'   font-family: 'IBM Plex Mono', monospace !important;
#'   font-size: 1.08rem !important;
#'   line-height: 1.35 !important;
#'
#'   height: 46px !important;
#'   min-height: 46px !important;
#'
#'   padding: 10px 16px !important;
#'
#'   display: flex !important;
#'   align-items: center !important;
#'
#'   border: 1px solid var(--border) !important;
#'   border-radius: 2px !important;
#'
#'   background: #fff !important;
#'   color: var(--text) !important;
#' }
#'
#' /* Selected text */
#' .selectize-input > div,
#' .selectize-input input {
#'   font-size: 1.08rem !important;
#'   line-height: 1.3 !important;
#' }
#'
#' /* Dropdown options */
#' .selectize-dropdown .option {
#'   font-size: 1rem !important;
#'   padding: 10px 14px !important;
#' }
#'
#' /* Dropdown menu items */
#' .selectize-dropdown .option {
#'   font-size: 0.9rem !important;
#'   padding: 8px 12px !important;
#' }
#'
#' /* Dropdown container */
#' .shiny-input-container {
#'   margin-bottom: 12px;
#' }
#'
#' /* Dropdown label */
#' .shiny-input-container > label {
#'   font-family: 'IBM Plex Mono', monospace;
#'   font-size: 0.72rem !important;
#'   letter-spacing: 0.12em;
#'   text-transform: uppercase;
#'   color: var(--muted);
#'   margin-bottom: 5px;
#' }
#'
#' /* Make selectize dropdown compact */
#' .selectize-input {
#'   min-height: 34px !important;
#'   padding: 6px 10px !important;
#'   font-size: 0.78rem !important;
#'   font-family: 'IBM Plex Mono', monospace !important;
#'   line-height: 1.2 !important;
#' }
#'
#' /* Reduce the giant selected text */
#' .selectize-input.items.not-full.has-options {
#'   font-size: 0.78rem !important;
#' }
#'
#' /* Dropdown menu options */
#' .selectize-dropdown .option {
#'   font-size: 0.76rem !important;
#'   padding: 6px 10px !important;
#' }
#'
#' hr {
#'   border-color: var(--border-lt);
#' }
#'
#' /* Plot divider */
#' .plot-rule {
#'   display: flex;
#'   align-items: center;
#'   gap: 14px;
#'   margin: 18px 0 14px;
#' }
#'
#' .plot-rule-line {
#'   flex: 1;
#'   height: 1px;
#'   background: var(--border);
#' }
#'
#' .plot-rule-lbl {
#'   font-family: 'IBM Plex Mono', monospace;
#'   font-size: 0.8rem;
#'   letter-spacing: 0.18em;
#'   text-transform: uppercase;
#'   color: var(--muted);
#' }
#'
#' /* DT */
#' table.dataTable thead th {
#'   font-family: 'IBM Plex Mono', monospace;
#'   font-size: 1rem;
#'   letter-spacing: 0.04em;
#'   background: #eeeae2;
#'   color: var(--accent);
#'   border-bottom: 1px solid var(--border) !important;
#' }
#'
#' table.dataTable tbody td {
#'   font-family: 'IBM Plex Mono', monospace;
#'   font-size: 1rem;
#'   color: var(--text);
#'   padding: 12px 14px !important;
#' }
#'
#' table.dataTable.stripe tbody tr.odd {
#'   background: #faf8f5;
#' }
#'
#' /* About */
#' .about-hero {
#'   background: var(--dark);
#'   color: #fff;
#'   padding: 50px 56px;
#'   margin-bottom: 34px;
#'   border-left: 4px solid var(--accent);
#' }
#'
#' .about-hero-eyebrow {
#'   font-family: 'IBM Plex Mono', monospace;
#'   font-size: 0.85rem;
#'   letter-spacing: 0.2em;
#'   text-transform: uppercase;
#'   opacity: 0.42;
#'   margin-bottom: 10px;
#' }
#'
#' .about-hero-title {
#'   font-family: 'Playfair Display', serif;
#'   font-size: 3rem;
#'   font-weight: 600;
#'   line-height: 1.35;
#'   margin-bottom: 12px;
#' }
#'
#' .about-hero-meta {
#'   font-family: 'IBM Plex Sans', sans-serif;
#'   font-size: 1.1rem;
#'   opacity: 0.6;
#'   font-style: italic;
#' }
#'
#' .about-card {
#'   border: 1px solid var(--border);
#'   border-top: 2px solid var(--accent);
#'   padding: 34px 38px;
#'   margin-bottom: 26px;
#'   background: var(--panel);
#' }
#'
#' .about-card-label {
#'   font-family: 'IBM Plex Mono', monospace;
#'   font-size: 0.82rem;
#'   letter-spacing: 0.2em;
#'   text-transform: uppercase;
#'   color: var(--red);
#'   margin-bottom: 14px;
#'   padding-bottom: 8px;
#'   border-bottom: 1px solid var(--border-lt);
#' }
#'
#' .about-card p {
#'   font-size: 1.3rem;
#'   line-height: 1.9;
#'   color: #2e3a44;
#'   margin: 0;
#' }
#'
#' /* Estimator grid */
#' .e-grid {
#'   display: grid;
#'   grid-template-columns: 1fr;
#'   gap: 8px;
#'   margin-top: 8px;
#' }
#'
#' /* Estimator cards */
#' .e-chip {
#'   border: 1px solid var(--border);
#'   padding: 10px 14px;
#'   background: var(--tag);
#'   min-height: unset;
#' }
#'
#' /* Estimator title */
#' .e-chip-name {
#'   font-family: 'IBM Plex Mono', monospace;
#'   font-size: 0.88rem;
#'   font-weight: 600;
#'   color: var(--accent);
#'   line-height: 1.2;
#' }
#'
#' /* Estimator description */
#' .e-chip-desc {
#'   font-size: 0.78rem;
#'   color: var(--muted);
#'   font-family: 'IBM Plex Sans', sans-serif;
#'   margin-top: 3px;
#'   line-height: 1.35;
#' }
#'
#' .p-grid {
#'   display: flex;
#'   flex-wrap: wrap;
#'   gap: 10px;
#'   margin-top: 12px;
#' }
#'
#' .p-badge {
#'   background: var(--accent);
#'   color: #fff;
#'   font-family: 'IBM Plex Mono', monospace;
#'   font-size: 1rem;
#'   padding: 8px 16px;
#'   letter-spacing: 0.04em;
#' }
#' "
#'
#' ############################################################
#' # Helper functions
#' ############################################################
#'
#' prepare_long_summary <- function(df, type = c("FAR","FARMA")) {
#'   type    <- match.arg(type)
#'   id_vars <- if (type=="FAR") c("T","ar_strength","d_true","method")
#'   else c("T","ar_strength","ma_strength","d_true","method")
#'   df %>%
#'     pivot_longer(cols=all_of(method_order_raw), names_to="method", values_to="d_hat") %>%
#'     mutate(error = d_hat - d_true) %>%
#'     group_by(across(all_of(id_vars))) %>%
#'     summarise(bias=round(mean(error,na.rm=TRUE),4),
#'               mse =round(mean(error^2,na.rm=TRUE),4), .groups="drop")
#' }
#'
#' prepare_plot_df <- function(df_summary) {
#'   df_summary %>%
#'     mutate(
#'       d_true      = factor(d_true, levels=sort(unique(as.numeric(as.character(d_true))))),
#'       T           = factor(T),
#'       ar_strength = factor(ar_strength),
#'       ma_strength = if ("ma_strength" %in% names(.)) factor(ma_strength) else NULL,
#'       method      = recode(method, !!!method_labels),
#'       method      = factor(method, levels=method_levels)
#'     )
#' }
#'
#' make_wide_table <- function(df_summary) {
#'   id_cols <- c("T","ar_strength","ma_strength","d_true")
#'   id_cols <- id_cols[id_cols %in% names(df_summary)]
#'   out <- df_summary %>%
#'     mutate(method       = factor(method, levels=method_order_raw),
#'            method_label = recode(as.character(method), !!!method_labels)) %>%
#'     select(all_of(id_cols), method_label, bias, mse) %>%
#'     pivot_wider(names_from=method_label, values_from=c(bias,mse),
#'                 names_glue="{method_label}__{.value}")
#'   desired <- c(id_cols, paste0(method_levels,"__bias"), paste0(method_levels,"__mse"))
#'   out %>% select(all_of(desired[desired %in% names(out)]))
#' }
#'
#' plot_perf_app <- function(df, methods_keep, yvar=c("bias","mse"),
#'                           facet_row="T", facet_col="ar_strength",
#'                           title=NULL, show_legend=TRUE) {
#'   yvar   <- match.arg(yvar)
#'   df_sub <- df %>% filter(method %in% methods_keep) %>%
#'     mutate(method=factor(method, levels=methods_keep))
#'   p <- ggplot(df_sub, aes(x=d_true, y=.data[[yvar]], group=method)) +
#'     geom_line(aes(linetype=method), linewidth=0.35, alpha=0.78) +
#'     geom_point(aes(shape=method), size=1.7) +
#'     facet_grid(rows=vars(.data[[facet_row]]), cols=vars(.data[[facet_col]]),
#'                labeller=label_both) +
#'     labs(title=title, x="True memory parameter  d",
#'          y=ifelse(yvar=="bias","Bias","MSE"),
#'          linetype="Estimator", shape="Estimator") +
#'     theme_classic(base_size=14) +
#'     theme(plot.title      =element_text(face="bold",size=12,colour="#1c3557"),
#'           axis.text.x     =element_text(angle=45,hjust=1),
#'           legend.position =if(show_legend) "top" else "none",
#'           strip.background=element_rect(fill="#eeeae2",colour="#c9c5bc"),
#'           strip.text      =element_text(colour="#1c3557",size=9,family="mono"))
#'   if (yvar=="bias")
#'     p <- p + geom_hline(yintercept=0, colour="#7a1515", linewidth=0.4, linetype="dashed")
#'   p
#' }
#'
#' ############################################################
#' # Load data
#' ############################################################
#'
#' LPWN_LW_sim_comparison_FAR <- readRDS("LPWN_LW_sim_comparison_FAR.rds")
#' peng_FAR                    <- readRDS("bootstrap_comparison_peng_FAR.rds")
#' LPWN_LW_sim_comparison_FAR <- LPWN_LW_sim_comparison_FAR %>%
#'   left_join(peng_FAR, by=c("T","ar_strength","d_true","seed")) %>%
#'   mutate(d_avg=(d_LW+d_R2_bc)/2)
#'
#' LPWN_LW_sim_comparison_FARMA <- readRDS("LPWN_LW_sim_comparison_FARMA.rds")
#' peng_FARMA <- readRDS("bootstrap_comparison_peng_FARMA.rds") %>%
#'   rename(ar_strength=strength_ar, ma_strength=strength_ma, d_peng=sample_peng) %>%
#'   select(-sample_whittle)
#' LPWN_LW_sim_comparison_FARMA <- LPWN_LW_sim_comparison_FARMA %>%
#'   left_join(peng_FARMA, by=c("T","ar_strength","ma_strength","d_true","seed")) %>%
#'   mutate(d_avg=(d_LW+d_R2_bc)/2)
#'
#' ############################################################
#' # Pre-compute choice vectors (stable — never change)
#' ############################################################
#'
#' FAR_choices <- list(
#'   T           = sort(unique(LPWN_LW_sim_comparison_FAR$T)),
#'   ar_strength = sort(unique(LPWN_LW_sim_comparison_FAR$ar_strength)),
#'   d_true      = sort(unique(LPWN_LW_sim_comparison_FAR$d_true))
#' )
#'
#' FARMA_choices <- list(
#'   T           = sort(unique(LPWN_LW_sim_comparison_FARMA$T)),
#'   ar_strength = sort(unique(LPWN_LW_sim_comparison_FARMA$ar_strength)),
#'   ma_strength = sort(unique(LPWN_LW_sim_comparison_FARMA$ma_strength)),
#'   d_true      = sort(unique(LPWN_LW_sim_comparison_FARMA$d_true))
#' )
#'
#' ############################################################
#' # Reusable select/deselect widget
#' ############################################################
#'
#' sel_row_ui <- function(input_id) {
#'   div(class="sel-row",
#'       tags$button("Select All",   class="sel-btn",
#'                   onclick=sprintf("Shiny.setInputValue('_sel_%s', Math.random())", input_id)),
#'       tags$button("Deselect All", class="sel-btn desel",
#'                   onclick=sprintf("Shiny.setInputValue('_des_%s', Math.random())", input_id))
#'   )
#' }
#'
#' ############################################################
#' # UI
#' ############################################################
#'
#' ui <- fluidPage(
#'
#'   tags$head(tags$style(HTML(app_css))),
#'
#'   div(class="masthead",
#'       div(class="masthead-kicker", "Simulation Study · Functional Time Series"),
#'       div(class="masthead-title",  "Memory Estimator Simulation Explorer"),
#'       div(class="masthead-sub",
#'           "FARFIMA long-memory estimation · Bias & MSE comparison across estimators")
#'   ),
#'
#'   br(),
#'
#'   sidebarLayout(
#'
#'     sidebarPanel(width=3,
#'
#'                  div(class="s-head", "Simulation Setting"),
#'                  div(class="set-wrap",
#'                      selectInput("data_type", NULL,
#'                                  choices  = c("FAR — FARFIMA(1,d,0)"  = "FAR",
#'                                               "FARMA — FARFIMA(1,d,1)" = "FARMA"),
#'                                  selected = "FAR")
#'                  ),
#'
#'                  conditionalPanel(
#'                    condition = "input.main_tabs == 'Summary Table'",
#'                    div(class="s-head", "Export"),
#'                    downloadButton("download_latex", "Download LaTeX Table", class="btn-dl")
#'                  ),
#'
#'                  div(class="s-head", "Filters"),
#'
#'                  # ── T filter (static UI, choices set in server) ──
#'                  div(class="f-lbl", "Sample Size T"),
#'                  sel_row_ui("T_filter"),
#'                  checkboxGroupInput("T_filter", NULL, choices=FAR_choices$T, selected=FAR_choices$T[1]),
#'
#'                  # ── AR filter ──
#'                  div(class="f-lbl", "AR Strength"),
#'                  sel_row_ui("ar_filter"),
#'                  checkboxGroupInput("ar_filter", NULL, choices=FAR_choices$ar_strength,
#'                                     selected=FAR_choices$ar_strength[1]),
#'
#'                  # ── MA filter — hidden for FAR ──
#'                  conditionalPanel(
#'                    condition = "input.data_type == 'FARMA'",
#'                    div(class="f-lbl", "MA Strength"),
#'                    sel_row_ui("ma_filter"),
#'                    checkboxGroupInput("ma_filter", NULL, choices=FARMA_choices$ma_strength,
#'                                       selected=FARMA_choices$ma_strength[1])
#'                  ),
#'
#'                  # ── d filter ──
#'                  div(class="f-lbl", "True Memory d"),
#'                  sel_row_ui("d_filter"),
#'                  checkboxGroupInput("d_filter", NULL, choices=FAR_choices$d_true,
#'                                     selected=FAR_choices$d_true[1]),
#'
#'                  # ── Plot controls ──
#'                  conditionalPanel(
#'                    condition = "input.main_tabs == 'Plot'",
#'                    div(class="s-head", "Estimators"),
#'                    sel_row_ui("methods"),
#'                    checkboxGroupInput("methods", NULL, choices=method_levels,
#'                                       selected=c("LW","DFA","LPWN(2)","BC-LPWN(2)")),
#'                    div(class="s-head", "Facets"),
#'                    uiOutput("facet_ui"),
#'                    div(class="s-head", "Export"),
#'                    downloadButton("download_plot", "Download Plot PDF", class="btn-dl")
#'                  )
#'     ),
#'
#'     mainPanel(width=9,
#'               tabsetPanel(id="main_tabs",
#'
#'                           # ── About ──
#'                           tabPanel("About",
#'                                    br(),
#'                                    div(class="about-hero",
#'                                        div(class="about-hero-eyebrow", "Functional Time Series"),
#'                                        div(class="about-hero-title",
#'                                            "Bias Correction of Long-Memory Estimator of Functional Time Series via the Prefiltered Sieve Bootstrap"),
#'                                        div(class="about-hero-meta", "Simulation study companion dashboard")
#'                                    ),
#'                                    fluidRow(
#'                                      column(7,
#'                                             div(class="about-card",
#'                                                 div(class="about-card-label", "Abstract"),
#'                                                 p("We investigate a sieve-bootstrap-based bias correction to estimate the long-memory
#'                   parameter d in fractionally integrated functional time series. The resampling method
#'                   implements a sieve bootstrap applied to data prefiltered by a preliminary estimate
#'                   of d. For the initial estimate, we recommend the local polynomial Whittle estimator
#'                   with noise (LPWN), which reduces bias induced by short-range dependence relative to
#'                   the standard local Whittle and DFA estimators. The bootstrap procedure further
#'                   corrects the remaining bias of the LPWN estimator, yielding the BC-LPWN estimators
#'                   evaluated in this simulation study.")
#'                                             ),
#'                                             div(class="about-card",
#'                                                 div(class="about-card-label", "Simulation Design"),
#'                                                 p(HTML("Functional FARFIMA(<i>p</i>,<i>d</i>,<i>q</i>) processes are simulated on [0,1]
#'                   across two settings — <b>Case 1 (FAR)</b>: FARFIMA(1,<i>d</i>,0) with a Gaussian AR kernel;
#'                   <b>Case 2 (FARMA)</b>: FARFIMA(1,<i>d</i>,1) augmented by a covariance-type MA kernel.
#'                   Operator norms ∈ {0.2, 0.5, 0.8} reflect weak, moderate, and strong short-run dependence.
#'                   Each configuration is replicated 100 times; performance is measured by average bias and MSE.")),
#'                                                 div(class="about-card-label", style="margin-top:14px;", "Parameter Grid"),
#'                                                 div(class="p-grid",
#'                                                     div(class="p-badge","T ∈ {500, 1000, 2000}"),
#'                                                     div(class="p-badge","d ∈ {0.1, …, 1.4}"),
#'                                                     div(class="p-badge","‖ϕ‖ ∈ {0.2, 0.5, 0.8}"),
#'                                                     div(class="p-badge","100 replications")
#'                                                 )
#'                                             )
#'                                      ),
#'                                      column(5,
#'                                             div(class="about-card",
#'                                                 div(class="about-card-label", "Estimators Compared"),
#'                                                 div(class="e-grid",
#'                                                     div(class="e-chip",div(class="e-chip-name","LW"),
#'                                                         div(class="e-chip-desc","Local Whittle (benchmark)")),
#'                                                     div(class="e-chip",div(class="e-chip-name","DFA"),
#'                                                         div(class="e-chip-desc","Detrended Fluctuation Analysis (benchmark)")),
#'                                                     div(class="e-chip",div(class="e-chip-name","LPWN(1)"),
#'                                                         div(class="e-chip-desc","Local Poly. Whittle with Order 1")),
#'                                                     div(class="e-chip",div(class="e-chip-name","LPWN(2)"),
#'                                                         div(class="e-chip-desc","Local Poly. Whittle with Order 2")),
#'                                                     div(class="e-chip",div(class="e-chip-name","BC-LPWN(1)"),
#'                                                         div(class="e-chip-desc","Bootstrap-corrected LPWN(1)")),
#'                                                     div(class="e-chip",div(class="e-chip-name","BC-LPWN(2)"),
#'                                                         div(class="e-chip-desc","Bootstrap-corrected LPWN(2)")),
#'                                                     div(class="e-chip",div(class="e-chip-name","LW-LPWN-AVG"),
#'                                                         div(class="e-chip-desc","Average of LW and BC-LPWN(2)"))
#'                                                 )
#'                                             ),
#'                                             div(class="about-card",
#'                                                 div(class="about-card-label", "How to Use"),
#'                                                 p(HTML("Navigate to <b>Summary Table</b> to inspect bias and MSE filtered by sample
#'                   size, dependence strength, and true memory d. Use the <b>Plot</b> tab to compare
#'                   estimators visually across facetted configurations. Use <em>Select / Deselect All</em>
#'                   in the sidebar for quick filter adjustments. Export LaTeX tables or stacked PDF
#'                   plots from the sidebar."))
#'                                             )
#'                                      )
#'                                    )
#'                           ),
#'
#'                           # ── Summary Table ──
#'                           tabPanel("Summary Table",
#'                                    br(),
#'                                    DTOutput("summary_table")
#'                           ),
#'
#'                           # ── Plot ──
#'                           tabPanel("Plot",
#'                                    br(),
#'                                    div(class="plot-rule",
#'                                        div(class="plot-rule-line"),
#'                                        div(class="plot-rule-lbl","Bias"),
#'                                        div(class="plot-rule-line")
#'                                    ),
#'                                    plotOutput("perf_plot_bias", height="480px"),
#'                                    div(class="plot-rule",
#'                                        div(class="plot-rule-line"),
#'                                        div(class="plot-rule-lbl","Mean Squared Error"),
#'                                        div(class="plot-rule-line")
#'                                    ),
#'                                    plotOutput("perf_plot_mse", height="420px")
#'                           )
#'               )
#'     )
#'   )
#' )
#'
#' ############################################################
#' # Server
#' ############################################################
#'
#' server <- function(input, output, session) {
#'
#'   # ── Update filter choices when data_type switches ──────
#'   observeEvent(input$data_type, {
#'     ch <- if (input$data_type=="FAR") FAR_choices else FARMA_choices
#'     updateCheckboxGroupInput(session,"T_filter",
#'                              choices=ch$T, selected=ch$T[1])
#'     updateCheckboxGroupInput(session,"ar_filter",
#'                              choices=ch$ar_strength, selected=ch$ar_strength[1])
#'     updateCheckboxGroupInput(session,"d_filter",
#'                              choices=ch$d_true, selected=ch$d_true[1])
#'     if (input$data_type=="FARMA")
#'       updateCheckboxGroupInput(session,"ma_filter",
#'                                choices=ch$ma_strength, selected=ch$ma_strength[1])
#'   }, ignoreInit=TRUE)
#'
#'   # ── Select / Deselect All observers ───────────────────
#'   # T
#'   observeEvent(input[["_sel_T_filter"]], {
#'     ch <- if (input$data_type=="FAR") FAR_choices$T else FARMA_choices$T
#'     updateCheckboxGroupInput(session,"T_filter", choices=ch, selected=ch)
#'   }, ignoreNULL=TRUE, ignoreInit=TRUE)
#'   observeEvent(input[["_des_T_filter"]], {
#'     ch <- if (input$data_type=="FAR") FAR_choices$T else FARMA_choices$T
#'     updateCheckboxGroupInput(session,"T_filter", choices=ch, selected=character(0))
#'   }, ignoreNULL=TRUE, ignoreInit=TRUE)
#'
#'   # AR
#'   observeEvent(input[["_sel_ar_filter"]], {
#'     ch <- if (input$data_type=="FAR") FAR_choices$ar_strength else FARMA_choices$ar_strength
#'     updateCheckboxGroupInput(session,"ar_filter", choices=ch, selected=ch)
#'   }, ignoreNULL=TRUE, ignoreInit=TRUE)
#'   observeEvent(input[["_des_ar_filter"]], {
#'     ch <- if (input$data_type=="FAR") FAR_choices$ar_strength else FARMA_choices$ar_strength
#'     updateCheckboxGroupInput(session,"ar_filter", choices=ch, selected=character(0))
#'   }, ignoreNULL=TRUE, ignoreInit=TRUE)
#'
#'   # MA
#'   observeEvent(input[["_sel_ma_filter"]], {
#'     updateCheckboxGroupInput(session,"ma_filter",
#'                              choices=FARMA_choices$ma_strength, selected=FARMA_choices$ma_strength)
#'   }, ignoreNULL=TRUE, ignoreInit=TRUE)
#'   observeEvent(input[["_des_ma_filter"]], {
#'     updateCheckboxGroupInput(session,"ma_filter",
#'                              choices=FARMA_choices$ma_strength, selected=character(0))
#'   }, ignoreNULL=TRUE, ignoreInit=TRUE)
#'
#'   # d
#'   observeEvent(input[["_sel_d_filter"]], {
#'     ch <- if (input$data_type=="FAR") FAR_choices$d_true else FARMA_choices$d_true
#'     updateCheckboxGroupInput(session,"d_filter", choices=ch, selected=ch)
#'   }, ignoreNULL=TRUE, ignoreInit=TRUE)
#'   observeEvent(input[["_des_d_filter"]], {
#'     ch <- if (input$data_type=="FAR") FAR_choices$d_true else FARMA_choices$d_true
#'     updateCheckboxGroupInput(session,"d_filter", choices=ch, selected=character(0))
#'   }, ignoreNULL=TRUE, ignoreInit=TRUE)
#'
#'   # Estimators
#'   observeEvent(input[["_sel_methods"]], {
#'     updateCheckboxGroupInput(session,"methods", choices=method_levels, selected=method_levels)
#'   }, ignoreNULL=TRUE, ignoreInit=TRUE)
#'   observeEvent(input[["_des_methods"]], {
#'     updateCheckboxGroupInput(session,"methods", choices=method_levels, selected=character(0))
#'   }, ignoreNULL=TRUE, ignoreInit=TRUE)
#'
#'   # ── Facet UI ──────────────────────────────────────────
#'   output$facet_ui <- renderUI({
#'     fc <- if (input$data_type=="FAR") c("T","ar_strength")
#'     else c("T","ar_strength","ma_strength")
#'     tagList(
#'       selectInput("facet_row","Facet Rows",    choices=fc, selected="T"),
#'       selectInput("facet_col","Facet Columns", choices=fc, selected="ar_strength")
#'     )
#'   })
#'
#'   # ── Raw data ──────────────────────────────────────────
#'   raw_data <- reactive({
#'     if (input$data_type=="FAR") LPWN_LW_sim_comparison_FAR
#'     else LPWN_LW_sim_comparison_FARMA
#'   })
#'
#'   # ── Summary (all rows — filtering happens below) ──────
#'   summary_data <- reactive({
#'     prepare_long_summary(raw_data(), type=input$data_type)
#'   })
#'
#'   # ── Filtered summary ──────────────────────────────────
#'   filtered_summary <- reactive({
#'     df <- summary_data()
#'
#'     # Safe fall-through: if inputs not yet ready, return empty
#'     T_sel  <- input$T_filter   %||% character(0)
#'     ar_sel <- input$ar_filter  %||% character(0)
#'     d_sel  <- input$d_filter   %||% character(0)
#'
#'     df <- df %>%
#'       filter(as.character(T)           %in% as.character(T_sel),
#'              as.character(ar_strength) %in% as.character(ar_sel),
#'              as.character(d_true)      %in% as.character(d_sel))
#'
#'     if (input$data_type=="FARMA") {
#'       ma_sel <- input$ma_filter %||% character(0)
#'       df <- df %>% filter(as.character(ma_strength) %in% as.character(ma_sel))
#'     }
#'     df
#'   })
#'
#'   filtered_plot_df <- reactive({ prepare_plot_df(filtered_summary()) })
#'   wide_table       <- reactive({ make_wide_table(filtered_summary()) })
#'
#'   # ── DT table ──────────────────────────────────────────
#'   output$summary_table <- renderDT({
#'     df      <- wide_table()
#'     validate(need(nrow(df) > 0,
#'                   "No data matches the current filter selection. Please select at least one value per filter."))
#'
#'     id_cols <- c("T","ar_strength","ma_strength","d_true")
#'     id_cols <- id_cols[id_cols %in% names(df)]
#'     n_id    <- length(id_cols)
#'
#'     bias_cols <- grep("__bias$", names(df), value=TRUE)
#'     est_names <- gsub("__bias$","", bias_cols)
#'     n_est     <- length(est_names)
#'
#'     id_display <- c(T="T", ar_strength="AR Strength",
#'                     ma_strength="MA Strength", d_true="d")[id_cols]
#'
#'     config_th <- sprintf(
#'       "<th colspan='%d' style='text-align:center;background:#eeeae2;color:#1c3557;
#'         border-bottom:2px solid #1c3557;font-family:IBM Plex Mono,monospace;
#'         font-size:0.67rem;letter-spacing:0.16em;text-transform:uppercase;
#'         padding:8px 4px;'>Configuration</th>", n_id)
#'
#'     bias_th <- sprintf(
#'       "<th colspan='%d' style='text-align:center;background:#e6eef5;color:#1c3557;
#'         border-bottom:2px solid #1c3557;font-family:IBM Plex Mono,monospace;
#'         font-size:0.67rem;letter-spacing:0.16em;text-transform:uppercase;
#'         padding:8px 4px;'>Bias</th>", n_est)
#'
#'     mse_th <- sprintf(
#'       "<th colspan='%d' style='text-align:center;background:#f5e6e6;color:#7a1515;
#'         border-bottom:2px solid #7a1515;font-family:IBM Plex Mono,monospace;
#'         font-size:0.67rem;letter-spacing:0.16em;text-transform:uppercase;
#'         padding:8px 4px;'>MSE</th>", n_est)
#'
#'     id_cells <- paste(
#'       sprintf("<th style='background:#eeeae2;color:#1c3557;vertical-align:middle;
#'                text-align:center;font-family:IBM Plex Mono,monospace;
#'                font-size:0.66rem;letter-spacing:0.06em;padding:6px 8px;'>%s</th>",
#'               id_display), collapse="")
#'
#'     est_cells <- paste(
#'       sprintf("<th style='font-family:IBM Plex Mono,monospace;font-size:0.64rem;
#'                white-space:nowrap;text-align:center;padding:5px 6px;'>%s</th>",
#'               rep(est_names, 2)), collapse="")
#'
#'     sketch <- htmltools::withTags(table(
#'       class="display",
#'       thead(
#'         HTML(paste0("<tr>", config_th, bias_th,  mse_th,  "</tr>")),
#'         HTML(paste0("<tr>", id_cells,  est_cells, "</tr>"))
#'       )
#'     ))
#'
#'     datatable(
#'       df, container=sketch,
#'       colnames=rep("", ncol(df)),
#'       rownames=FALSE,
#'       options=list(pageLength=20, scrollX=TRUE, ordering=TRUE, dom="lftip")
#'     ) %>% formatRound(columns=(n_id+1):ncol(df), digits=4)
#'   })
#'
#'   # ── Plots ─────────────────────────────────────────────
#'   output$perf_plot_bias <- renderPlot({
#'     req(input$methods, input$facet_row, input$facet_col)
#'     validate(need(nrow(filtered_plot_df())>0,"No data — adjust filters."))
#'     plot_perf_app(filtered_plot_df(), input$methods, "bias",
#'                   input$facet_row, input$facet_col,
#'                   paste(input$data_type,"— Bias"), TRUE)
#'   })
#'
#'   output$perf_plot_mse <- renderPlot({
#'     req(input$methods, input$facet_row, input$facet_col)
#'     validate(need(nrow(filtered_plot_df())>0,"No data — adjust filters."))
#'     plot_perf_app(filtered_plot_df(), input$methods, "mse",
#'                   input$facet_row, input$facet_col,
#'                   paste(input$data_type,"— MSE"), FALSE)
#'   })
#'
#'   # ── Downloads ─────────────────────────────────────────
#'   output$download_latex <- downloadHandler(
#'     filename = function() paste0("summary_table_",input$data_type,".tex"),
#'     content  = function(file) {
#'       df <- wide_table()
#'       colnames(df) <- gsub("__bias$"," (Bias)", names(df))
#'       colnames(df) <- gsub("__mse$", " (MSE)",  colnames(df))
#'       sink(file)
#'       print(xtable(df, digits=4), type="latex", include.rownames=FALSE)
#'       sink()
#'     }
#'   )
#'
#'   output$download_plot <- downloadHandler(
#'     filename = function() paste0("plot_",input$data_type,"_bias_mse.pdf"),
#'     content  = function(file) {
#'       pb <- plot_perf_app(filtered_plot_df(),input$methods,"bias",
#'                           input$facet_row,input$facet_col,
#'                           paste(input$data_type,"— Bias"),TRUE)
#'       pm <- plot_perf_app(filtered_plot_df(),input$methods,"mse",
#'                           input$facet_row,input$facet_col,
#'                           paste(input$data_type,"— MSE"),FALSE)
#'       if (requireNamespace("patchwork",quietly=TRUE)) {
#'         library(patchwork)
#'         ggsave(file, pb/pm, width=10, height=12, dpi=300)
#'       } else {
#'         pdf(file,width=10,height=12); print(pb); print(pm); dev.off()
#'       }
#'     }
#'   )
#' }
#'
#' # Null-coalescing helper
#' `%||%` <- function(a, b) if (!is.null(a)) a else b
#'
#' shinyApp(ui, server)
