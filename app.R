# ============================================================
# Neighbourhood Walkability & Cardiometabolic Health
# Data Story — Barbados SIDS Study (Rocke et al., 2023)
# ============================================================
# Run with: shiny::runApp("app.R")
# Required packages:
#   install.packages(c("shiny","shinydashboard","ggplot2",
#                      "plotly","dplyr","scales","bslib"))
# ============================================================

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(bslib)

# ── Colour palette ────────────────────────────────────────────
pal <- list(
  teal    = "#0D9488",
  teal_lt = "#5EEAD4",
  amber   = "#F59E0B",
  coral   = "#F43F5E",
  navy    = "#0F172A",
  slate   = "#334155",
  muted   = "#94A3B8",
  bg      = "#F0FDF9",
  card    = "#FFFFFF"
)

# ── Data ──────────────────────────────────────────────────────

# Table 1: Population characteristics by walkability tertile
pop_chars <- data.frame(
  walkability = factor(c("Low","Moderate","High"),
                       levels = c("Low","Moderate","High")),
  n           = c(331, 332, 302),
  age         = c(46.1, 46.4, 47.3),
  bmi         = c(29.1, 28.0, 27.9),
  car_own     = c(56.7, 61.6, 47.5),
  walking     = c(64.3, 64.7, 99.0),
  commuting   = c(7.9, 13.2, 26.7),
  mvpa        = c(220.6, 198.6, 245.5),
  total_act   = c(3684.4, 3820.0, 3654.3),
  hypertension= c(31.4, 28.8, 30.5),
  diabetes    = c(12.7, 11.9, 10.7),
  cvd_risk    = c(12.0, 11.2, 11.9)
)

# Table 2: PA regression results (per 10-pt walkability increase)
pa_assoc <- data.frame(
  outcome   = c("Overall Walking","Active Commuting","MVPA","Total Activity"),
  beta      = c(17.6, 50.2, 17.6, 76.9),
  ci_lo     = c(2.3, 19.1, 7.6, 28.5),
  ci_hi     = c(32.8, 81.3, 27.5, 125.2),
  p_value   = c(0.024, 0.002, 0.001, 0.002),
  unit      = c("mins/week","mins/week","mins/week","mins/week")
)
pa_assoc$outcome <- factor(pa_assoc$outcome,
                           levels = rev(pa_assoc$outcome))
pa_assoc$sig <- pa_assoc$p_value < 0.05

# Table 3: CVD outcomes
cvd_assoc <- data.frame(
  outcome  = c("10-yr CVD Risk (continuous)",
               "CVD Risk ≥7.5%","CVD Risk ≥10%","CVD Risk ≥20%",
               "Hypertension","Diabetes"),
  est      = c(-0.57, 0.96, 0.83, 0.87, 0.94, 0.81),
  ci_lo    = c(-0.88, 0.77, 0.68, 0.77, 0.85, 0.75),
  ci_hi    = c(-0.27, 1.20, 1.02, 0.99, 1.04, 0.88),
  p_value  = c(0.001, 0.720, 0.070, 0.031, 0.250, 0.001),
  type     = c("linear","logistic","logistic","logistic",
               "logistic","logistic")
)
cvd_assoc$outcome <- factor(cvd_assoc$outcome,
                            levels = rev(cvd_assoc$outcome))
cvd_assoc$sig <- cvd_assoc$p_value < 0.05

# IPEN Walkability index comparison
ipen <- data.frame(
  city    = c("Pooled IPEN","Adelaide, AUS","Ghent, BEL",
              "Curitiba, BRA","Bogota, COL","Olomouc, CZE",
              "Aarhus, DNK","Hong Kong, HKG","Cuernavaca, MEX",
              "Christchurch, NZL","Wellington, NZL",
              "Stoke-on-Trent, GBR","Baltimore, USA",
              "Seattle, USA","Barbados, BRB"),
  mean    = c(1.0, 0.2, 0.5, 1.5, 2.0, 1.8, 1.6, 5.5,
              0.3, -1.5, -0.5, 0.5, 0.2, 0.8, 1.2),
  iqr_lo  = c(-2.5,-2.5,-2.5,-0.5,-0.5, 0.5,-0.5, 2.5,
              -1.5,-3.5,-3.0,-2.5,-2.8,-2.0,-2.5),
  iqr_hi  = c( 7.0, 3.5, 3.5, 2.5, 4.0, 3.5, 3.5, 9.0,
               1.5, 0.0, 1.5, 3.5, 2.5, 3.5, 6.0),
  highlight = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE)
)
ipen$city <- factor(ipen$city, levels = rev(ipen$city))

# WHO PA guideline context
who_context <- data.frame(
  scenario     = c("Current avg (high walkability)",
                   "WHO minimum (MVPA)",
                   "10-pt walkability gain"),
  minutes      = c(245.5, 150, 17.6),
  category     = c("Observed","Guideline","Gain")
)

# ── Theme helpers ─────────────────────────────────────────────
theme_walk <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(
      plot.background  = element_rect(fill = pal$bg, colour = NA),
      panel.background = element_rect(fill = pal$card, colour = NA),
      panel.grid.major = element_line(colour = "#E2E8F0", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(colour = pal$slate, size = base - 1),
      axis.title       = element_text(colour = pal$navy, face = "bold",
                                      size = base - 0.5),
      plot.title       = element_text(colour = pal$navy, face = "bold",
                                      size = base + 2, margin = margin(b=6)),
      plot.subtitle    = element_text(colour = pal$muted, size = base - 1,
                                      margin = margin(b = 10)),
      legend.position  = "bottom",
      legend.text      = element_text(colour = pal$slate),
      plot.margin      = margin(15, 15, 10, 15)
    )
}

stat_card <- function(value, label, icon = "●", colour = pal$teal) {
  div(class = "stat-card",
      div(class = "stat-icon", style = paste0("color:", colour), icon),
      div(class = "stat-value", style = paste0("color:", colour), value),
      div(class = "stat-label", label)
  )
}

# ── Shared citation footer ────────────────────────────────────
citation_footer <- div(class = "footer",
                       tags$p("Data story based on:"),
                       tags$p(
                         tags$strong(
                           "Rocke KD et al. (2023). Neighbourhood Walkability and Its Influence on
       Physical Activity and Cardiometabolic Disease: A Cross-Sectional Study
       in a Caribbean Small Island Developing State."),
                         " Cureus 15(8): e44060.",
                         tags$a(href   = "https://doi.org/10.7759/cureus.44060",
                                target = "_blank",
                                style  = "color:#0D9488;",
                                " DOI: 10.7759/cureus.44060")
                       )
)

# ── UI ────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bs_theme(
    bg       = pal$bg,
    fg       = pal$navy,
    primary  = pal$teal,
    base_font = font_google("DM Sans"),
    heading_font = font_google("Fraunces")
  ),
  
  tags$head(
    tags$style(HTML(paste0("
      body { background:", pal$bg, "; font-family: 'DM Sans', sans-serif; }

      /* ── Hero ── */
      .hero {
        background: linear-gradient(135deg, ", pal$navy, " 0%, #1E3A5F 60%, #0D6B6E 100%);
        color: white;
        padding: 56px 48px 48px;
        border-radius: 0 0 32px 32px;
        margin-bottom: 36px;
        position: relative;
        overflow: hidden;
      }
      .hero::before {
        content: '';
        position: absolute;
        top: -60px; right: -60px;
        width: 300px; height: 300px;
        background: radial-gradient(circle, rgba(94,234,212,.15) 0%, transparent 70%);
        border-radius: 50%;
      }
      .hero-title {
        font-family: 'Fraunces', serif;
        font-size: 2.4rem;
        font-weight: 700;
        line-height: 1.15;
        margin-bottom: 14px;
      }
      .hero-sub {
        font-size: 1.05rem;
        opacity: 0.82;
        max-width: 620px;
        line-height: 1.6;
      }
      .hero-badge {
        display: inline-block;
        background: rgba(94,234,212,.18);
        border: 1px solid rgba(94,234,212,.4);
        color: ", pal$teal_lt, ";
        padding: 4px 14px;
        border-radius: 20px;
        font-size: 0.78rem;
        letter-spacing: .06em;
        text-transform: uppercase;
        margin-bottom: 18px;
      }

      /* ── Stat cards ── */
      .stats-row {
        display: flex;
        gap: 16px;
        margin-bottom: 32px;
        flex-wrap: wrap;
      }
      .stat-card {
        flex: 1;
        min-width: 140px;
        background: white;
        border-radius: 16px;
        padding: 22px 20px;
        box-shadow: 0 2px 12px rgba(0,0,0,.06);
        text-align: center;
        border-top: 4px solid ", pal$teal, ";
        transition: transform .2s, box-shadow .2s;
      }
      .stat-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 6px 20px rgba(13,148,136,.15);
      }
      .stat-icon { font-size: 1.4rem; margin-bottom: 6px; }
      .stat-value { font-size: 1.9rem; font-weight: 700; line-height: 1; margin-bottom: 6px; }
      .stat-label { font-size: 0.78rem; color: ", pal$muted, "; text-transform: uppercase;
                    letter-spacing: .05em; }

      /* ── Section ── */
      .section-title {
        font-family: 'Fraunces', serif;
        font-size: 1.55rem;
        font-weight: 600;
        color: ", pal$navy, ";
        margin: 36px 0 6px;
        padding-bottom: 8px;
        border-bottom: 3px solid ", pal$teal_lt, ";
        display: inline-block;
      }
      .section-desc {
        color: ", pal$slate, ";
        font-size: 0.92rem;
        line-height: 1.65;
        margin-bottom: 20px;
        max-width: 720px;
      }

      /* ── Plot card ── */
      .plot-card {
        background: white;
        border-radius: 16px;
        padding: 24px;
        box-shadow: 0 2px 12px rgba(0,0,0,.06);
        margin-bottom: 24px;
      }
      .plot-title {
        font-family: 'Fraunces', serif;
        font-size: 1.1rem;
        font-weight: 600;
        color: ", pal$navy, ";
        margin-bottom: 4px;
      }
      .plot-note {
        font-size: 0.78rem;
        color: ", pal$muted, ";
        margin-bottom: 14px;
        font-style: italic;
      }

      /* ── Insight box ── */
      .insight {
        background: linear-gradient(135deg, rgba(13,148,136,.08), rgba(94,234,212,.06));
        border-left: 4px solid ", pal$teal, ";
        border-radius: 0 12px 12px 0;
        padding: 16px 20px;
        margin: 16px 0 24px;
        font-size: 0.92rem;
        color: ", pal$slate, ";
        line-height: 1.65;
      }
      .insight strong { color: ", pal$teal, "; }

      /* ── Tabs ── */
      .nav-tabs .nav-link { color: ", pal$slate, "; }
      .nav-tabs .nav-link.active {
        color: ", pal$teal, ";
        border-bottom: 3px solid ", pal$teal, ";
        font-weight: 600;
      }

      /* ── Footer ── */
      .footer {
        text-align: center;
        color: ", pal$muted, ";
        font-size: 0.8rem;
        padding: 30px 0 20px;
        border-top: 1px solid #E2E8F0;
        margin-top: 40px;
      }

      /* ── Select controls ── */
      .control-row {
        display: flex; align-items: center; gap: 16px;
        margin-bottom: 16px; flex-wrap: wrap;
      }
      select.form-select, select.form-control {
        border-radius: 8px;
        border: 1px solid #CBD5E1;
        padding: 8px 14px;
        font-size: .88rem;
      }
    ")))
  ),
  
  # ── Hero banner ─────────────────────────────────────────────
  div(class = "hero",
      div(class = "hero-badge", "📍 Barbados · Cross-Sectional Study · 2023"),
      div(class = "hero-title",
          "Can your neighbourhood make you healthier?"),
      div(class = "hero-sub",
          "Exploring how walkable design shapes physical activity and
       cardiovascular risk in a Caribbean Small Island Developing State.
       Based on Rocke et al. (2023), Cureus 15(8): e44060.")
  ),
  
  # ── Main container ───────────────────────────────────────────
  div(style = "max-width:1100px; margin:0 auto; padding:0 20px;",
      
      # ── Key numbers row ─────────────────────────────────────
      div(class = "stats-row",
          stat_card("1,234",  "Adults surveyed",             "👤", pal$teal),
          stat_card("45",     "Neighbourhoods mapped",        "🗺️", pal$slate),
          stat_card("11.7%",  "Avg 10-yr CVD risk",           "❤️", pal$coral),
          stat_card("75 min", "Weekly walking (avg)",         "🚶", pal$teal),
          stat_card("−0.57%", "CVD risk drop per 10-pt WI ↑","📉", pal$amber)
      ),
      
      # ── TABS ──────────────────────────────────────────────
      tabsetPanel(id = "tabs", type = "tabs",
                  
                  # ══ TAB 1: Context ══════════════════════════════════
                  tabPanel("📖 Study Context",
                           div(class = "section-title", "Background & Methods"),
                           div(class = "section-desc",
                               "CVD is the leading cause of death globally.
           In the Caribbean, physical inactivity affects up to 50% of adults —
           above the global average of 27.5%. This study linked a nationally
           representative health survey of Barbados (Health of the Nation, 2011–2013)
           with spatial built-environment data to examine whether walkability
           protects against cardiometabolic disease."
                           ),
                           
                           fluidRow(
                             column(6,
                                    div(class = "plot-card",
                                        div(class = "plot-title", "Walkability Index Formula"),
                                        div(class = "plot-note",
                                            "Three components z-scored and summed; index rescaled 0–100"),
                                        div(style = paste0(
                                          "background:", pal$navy, "; color:white; border-radius:10px;
                 padding:20px; font-family:monospace; font-size:0.95rem;
                 text-align:center; line-height:2.2;"),
                                          HTML("WI = &Sigma; ( RD<sub>z</sub> + LUM<sub>z</sub> + 2 &times; ID<sub>z</sub> )"),
                                          br(),
                                          tags$small(style="color:#94A3B8; font-size:.75rem;",
                                                     "RD = Residential Density · LUM = Land Use Mix · ID = Intersection Density")
                                        ),
                                        br(),
                                        div(class = "insight",
                                            strong("Why these three?"),
                                            " Higher residential density means more people within walking distance of
                  destinations. Greater land use mix means shops, schools, and offices are
                  closer to homes. Better intersection density means fewer dead-ends and
                  more direct routes — all encouraging walking over driving."
                                        )
                                    )
                             ),
                             column(6,
                                    div(class = "plot-card",
                                        div(class = "plot-title", "Study Design at a Glance"),
                                        div(class = "plot-note", "Key methodological features"),
                                        tags$table(style = "width:100%; border-collapse:collapse;",
                                                   tags$tr(style="background:#F0FDF9;",
                                                           tags$th(style="padding:10px 12px; text-align:left; color:#0F172A; font-size:.82rem;",
                                                                   "Element"),
                                                           tags$th(style="padding:10px 12px; text-align:left; color:#0F172A; font-size:.82rem;",
                                                                   "Detail")),
                                                   lapply(list(
                                                     c("Design",      "Cross-sectional"),
                                                     c("Population",  "Adults ≥25 yrs, Barbados"),
                                                     c("Period",      "2011–2013"),
                                                     c("Sample",      "1,234 participants"),
                                                     c("Neighbourhoods","45 enumeration districts"),
                                                     c("PA measure",  "Recent PA Questionnaire (RPAQ)"),
                                                     c("CVD risk",    "ACC/AHA Pooled Cohort Equation"),
                                                     c("Stats model", "Multi-level mixed effects"),
                                                     c("Confounders", "Age, sex, BMI, car ownership, SES…")
                                                   ), function(r) {
                                                     tags$tr(
                                                       tags$td(style="padding:9px 12px; border-top:1px solid #E2E8F0;
                                   font-weight:600; color:#0D9488; font-size:.83rem;", r[1]),
                                                       tags$td(style="padding:9px 12px; border-top:1px solid #E2E8F0;
                                   color:#334155; font-size:.83rem;", r[2])
                                                     )
                                                   })
                                        )
                                    )
                             )
                           ),
                           
                           # IPEN comparison plot
                           div(class = "plot-card",
                               div(class = "plot-title",
                                   "Barbados walkability in international context (IPEN study)"),
                               div(class = "plot-note",
                                   "Box shows IQR; X marks mean. Barbados falls within the global range."),
                               plotlyOutput("ipenPlot", height = "400px")
                           ),
                           citation_footer
                  ),
                  
                  # ══ TAB 2: Physical Activity ═════════════════════════
                  tabPanel("🚶 Physical Activity",
                           div(class = "section-title", "Walkability & Physical Activity"),
                           div(class = "section-desc",
                               "Adults in more walkable neighbourhoods spent significantly more time
           walking, commuting actively, and engaging in MVPA — even after
           adjusting for age, sex, BMI, car ownership, and neighbourhood SES."
                           ),
                           
                           div(class = "insight",
                               strong("+10 points walkability = "),
                               "+17.6 min/week walking · +50.2 min/week active commuting · ",
                               "+17.6 min/week MVPA · +76.9 min/week total activity"
                           ),
                           
                           fluidRow(
                             column(7,
                                    div(class = "plot-card",
                                        div(class = "plot-title",
                                            "Adjusted association: walkability vs PA (per 10-pt increase)"),
                                        div(class = "plot-note",
                                            "Forest plot of multivariable tobit regression coefficients (95% CI)"),
                                        plotlyOutput("paForestPlot", height = "320px")
                                    )
                             ),
                             column(5,
                                    div(class = "plot-card",
                                        div(class = "plot-title", "Weekly minutes by walkability tertile"),
                                        div(class = "plot-note",
                                            "Select a physical activity outcome below"),
                                        div(class = "control-row",
                                            selectInput("pa_outcome", NULL,
                                                        choices = c(
                                                          "Overall Walking"   = "walking",
                                                          "Active Commuting"  = "commuting",
                                                          "MVPA"              = "mvpa",
                                                          "Total Activity"    = "total_act"
                                                        ), width = "100%")
                                        ),
                                        plotlyOutput("paBarPlot", height = "260px")
                                    )
                             )
                           ),
                           
                           # WHO guideline context
                           div(class = "plot-card",
                               div(class = "plot-title",
                                   "Putting gains in perspective: WHO guideline (150 min MVPA/week)"),
                               div(class = "plot-note",
                                   "A 10-point walkability gain equates to ~12% of the weekly WHO minimum"),
                               plotlyOutput("whoPlot", height = "240px")
                           ),
                           citation_footer
                  ),
                  
                  # ══ TAB 3: CVD Risk ═════════════════════════════════
                  tabPanel("❤️ Cardiovascular Risk",
                           div(class = "section-title",
                               "Walkability & Cardiometabolic Outcomes"),
                           div(class = "section-desc",
                               "Higher walkability was significantly associated with lower predicted
           10-year CVD risk and lower odds of diabetes. Hypertension showed
           no statistically significant association."
                           ),
                           
                           div(class = "insight",
                               strong("Key finding: "),
                               "Every 10-point increase in neighbourhood walkability was linked
           to a ", strong("0.57 percentage-point reduction"), " in 10-year CVD risk
           (p < 0.001). Adults in high-walkability areas were also ",
                               strong("19% less likely to have diabetes"), " (OR = 0.81, p < 0.001)."
                           ),
                           
                           fluidRow(
                             column(7,
                                    div(class = "plot-card",
                                        div(class = "plot-title",
                                            "Adjusted associations: walkability vs CVD outcomes"),
                                        div(class = "plot-note",
                                            "Linear model: β = % CVD risk change per 10-pt WI increase.
                 Logistic model: OR per 10-pt WI increase (null = 1.0).
                 Filled = p<0.05."),
                                        plotlyOutput("cvdForestPlot", height = "360px")
                                    )
                             ),
                             column(5,
                                    div(class = "plot-card",
                                        div(class = "plot-title",
                                            "CVD risk & diabetes by walkability tertile"),
                                        div(class = "plot-note",
                                            "Survey-weighted means (Table 1)"),
                                        plotlyOutput("cvdBarPlot", height = "175px"),
                                        br(),
                                        plotlyOutput("diabBarPlot", height = "155px")
                                    )
                             )
                           ),
                           
                           # Dose response visual
                           div(class = "plot-card",
                               div(class = "plot-title",
                                   "Simulated dose-response: walkability gain vs CVD risk reduction"),
                               div(class = "plot-note",
                                   "Extrapolated from the adjusted linear coefficient (−0.57% per 10-pt increase).
             Starting from population mean CVD risk of 11.7%."),
                               plotlyOutput("doseResponsePlot", height = "280px")
                           ),
                           citation_footer
                  ),
                  
                  # ══ TAB 4: Conclusions ═══════════════════════════════
                  tabPanel("💡 Implications",
                           div(class = "section-title", "What This Means"),
                           div(class = "section-desc",
                               "The first Caribbean study to link neighbourhood walkability with
           physical activity and cardiovascular risk — mirroring findings from
           North America, Europe, and Brazil in a very different SIDS context."
                           ),
                           
                           fluidRow(
                             column(4,
                                    div(class = "plot-card", style = "border-top:4px solid #0D9488;",
                                        tags$h5(style="color:#0D9488; font-weight:700;",
                                                "🏙️ Urban Planning"),
                                        tags$p(style="font-size:.88rem; color:#334155; line-height:1.65;",
                                               "Improving mixed land use, street connectivity, and residential
                 density could meaningfully increase PA at a population level.
                 Even small walkability gains accumulate to large public-health gains.")
                                    )
                             ),
                             column(4,
                                    div(class = "plot-card", style = "border-top:4px solid #F59E0B;",
                                        tags$h5(style="color:#F59E0B; font-weight:700;",
                                                "🔬 Microscale Interventions"),
                                        tags$p(style="font-size:.88rem; color:#334155; line-height:1.65;",
                                               "Because macroscale change is slow and costly, shorter-term
                 pedestrian-level improvements — sidewalks, crossings, shade —
                 offer quicker wins for walkability in resource-constrained SIDS.")
                                    )
                             ),
                             column(4,
                                    div(class = "plot-card", style = "border-top:4px solid #F43F5E;",
                                        tags$h5(style="color:#F43F5E; font-weight:700;",
                                                "🩺 NCD Prevention"),
                                        tags$p(style="font-size:.88rem; color:#334155; line-height:1.65;",
                                               "A walkable environment acts as an upstream lever for CVD and
                 diabetes prevention — complementing individual-level
                 interventions that have shown inconsistent long-term results.")
                                    )
                             )
                           ),
                           
                           div(class = "plot-card",
                               div(class = "plot-title", "Summary: strength of evidence"),
                               div(class = "plot-note",
                                   "All outcomes from fully adjusted multi-level models (Table 2 & 3)"),
                               plotlyOutput("summaryPlot", height = "340px")
                           ),
                           
                           div(class = "plot-card",
                               div(class = "plot-title", "Limitations to keep in mind"),
                               tags$ul(style = "color:#334155; font-size:.88rem; line-height:2;",
                                       tags$li(
                                         strong("Cross-sectional design: "),
                                         "Cannot establish causality or temporal sequence."),
                                       tags$li(
                                         strong("Self-reported PA: "),
                                         "RPAQ estimates may overstate actual activity levels."),
                                       tags$li(
                                         strong("Neighbourhood self-selection: "),
                                         "More active people may choose walkable neighbourhoods."),
                                       tags$li(
                                         strong("CVD algorithm: "),
                                         "ACC/AHA Pooled Cohort Equation validated in North American,
               not Caribbean, populations."),
                                       tags$li(
                                         strong("Single SIDS setting: "),
                                         "Barbados findings may not generalise to other Caribbean islands.")
                               )
                           ),
                           
                           citation_footer
                  )
      ) # end tabsetPanel
  ) # end main container
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Shared plotly config
  cfg <- list(displayModeBar = FALSE, responsive = TRUE)
  tc  <- list(family = "DM Sans, sans-serif", size = 12, color = pal$slate)
  
  # ─ IPEN comparison ─────────────────────────────────────
  output$ipenPlot <- renderPlotly({
    p <- ggplot(ipen, aes(x = mean, y = city,
                          colour = highlight, fill = highlight)) +
      geom_segment(aes(x = iqr_lo, xend = iqr_hi,
                       y = city, yend = city),
                   linewidth = 1.1, alpha = 0.6) +
      geom_point(size = 4, shape = 4, stroke = 2) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 colour = pal$muted, linewidth = 0.5) +
      scale_colour_manual(values = c("FALSE" = pal$slate,
                                     "TRUE"  = pal$coral),
                          guide = "none") +
      scale_fill_manual(values = c("FALSE" = pal$slate,
                                   "TRUE"  = pal$coral),
                        guide = "none") +
      labs(x = "Walkability Index (z-score scale)",
           y = NULL,
           title = NULL) +
      theme_walk()
    ggplotly(p, tooltip = c("x","y")) |>
      config(cfg) |>
      layout(font = tc,
             paper_bgcolor = pal$card,
             plot_bgcolor  = pal$card)
  })
  
  # ─ PA forest plot ──────────────────────────────────────
  output$paForestPlot <- renderPlotly({
    p <- ggplot(pa_assoc,
                aes(x = beta, y = outcome,
                    colour = sig, fill = sig,
                    text = paste0(outcome, "<br>β = ", beta,
                                  " (", ci_lo, "–", ci_hi, ")<br>p = ",
                                  p_value))) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 colour = pal$muted, linewidth = 0.5) +
      geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                     height = 0.25, linewidth = 1) +
      geom_point(size = 4, shape = 21, stroke = 1.5, colour = "white") +
      scale_colour_manual(
        values = c("TRUE" = pal$teal, "FALSE" = pal$muted),
        labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
        name = NULL) +
      scale_fill_manual(
        values = c("TRUE" = pal$teal, "FALSE" = pal$muted),
        guide = "none") +
      labs(x = "Additional minutes/week per 10-pt walkability increase",
           y = NULL, title = NULL) +
      theme_walk() +
      theme(legend.position = "top")
    ggplotly(p, tooltip = "text") |>
      config(cfg) |>
      layout(font = tc,
             paper_bgcolor = pal$card,
             plot_bgcolor  = pal$card,
             legend = list(orientation = "h", x = 0, y = 1.15))
  })
  
  # ─ PA bar plot ─────────────────────────────────────────
  output$paBarPlot <- renderPlotly({
    y_map <- c(
      walking   = "Overall Walking (min/wk)",
      commuting = "Active Commuting (min/wk)",
      mvpa      = "MVPA (min/wk)",
      total_act = "Total Activity (min/wk)"
    )
    sel <- if (is.null(input$pa_outcome)) "walking" else input$pa_outcome
    d <- data.frame(
      walkability = pop_chars$walkability,
      value       = pop_chars[[sel]]
    )
    p <- ggplot(d, aes(x = walkability, y = value,
                       fill = walkability,
                       text = paste0(walkability,
                                     " walkability: ", round(value, 1),
                                     " min/week"))) +
      geom_col(width = 0.6, alpha = 0.9) +
      scale_fill_manual(
        values = c("Low" = "#CBD5E1",
                   "Moderate" = pal$teal_lt,
                   "High" = pal$teal),
        guide = "none") +
      labs(x = "Walkability", y = y_map[sel], title = NULL) +
      theme_walk()
    ggplotly(p, tooltip = "text") |>
      config(cfg) |>
      layout(font = tc,
             paper_bgcolor = pal$card,
             plot_bgcolor  = pal$card)
  })
  
  # ─ WHO context ─────────────────────────────────────────
  output$whoPlot <- renderPlotly({
    d <- data.frame(
      label = c("Current avg MVPA\n(high walkability area)",
                "10-pt walkability gain",
                "WHO minimum\n(150 min/wk)"),
      value = c(245.5, 17.6, 150),
      type  = c("observed", "gain", "guideline")
    )
    d$label <- factor(d$label, levels = d$label)
    p <- ggplot(d, aes(x = label, y = value, fill = type,
                       text = paste0(label, ": ", value, " min/wk"))) +
      geom_col(width = 0.55, alpha = 0.9) +
      geom_hline(yintercept = 150, linetype = "dashed",
                 colour = pal$coral, linewidth = 0.8) +
      annotate("text", x = 3.45, y = 158,
               label = "WHO minimum", colour = pal$coral,
               size = 3, hjust = 1) +
      scale_fill_manual(
        values = c(observed  = pal$teal,
                   gain      = pal$amber,
                   guideline = "#CBD5E1"),
        guide = "none") +
      labs(x = NULL, y = "Minutes / week", title = NULL) +
      theme_walk()
    ggplotly(p, tooltip = "text") |>
      config(cfg) |>
      layout(font = tc,
             paper_bgcolor = pal$card,
             plot_bgcolor  = pal$card)
  })
  
  # ─ CVD forest plot ─────────────────────────────────────
  output$cvdForestPlot <- renderPlotly({
    d <- cvd_assoc
    # For continuous outcome use beta / null = 0; for logistic use OR / null = 1
    d$null_val <- ifelse(d$type == "linear", 0, 1)
    d$display_est <- d$est
    
    p <- ggplot(d, aes(x = display_est, y = outcome,
                       colour = sig, fill = sig,
                       text = paste0(
                         outcome, "<br>",
                         ifelse(type == "linear",
                                paste0("β = ", est, " (", ci_lo, "–", ci_hi, ")"),
                                paste0("OR = ", est, " (", ci_lo, "–", ci_hi, ")")),
                         "<br>p = ", p_value
                       ))) +
      # Null lines
      geom_vline(xintercept = 0, linetype = "dashed",
                 colour = pal$muted, linewidth = 0.4) +
      geom_vline(xintercept = 1, linetype = "dashed",
                 colour = "#CBD5E1", linewidth = 0.4) +
      geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                     height = 0.3, linewidth = 1.1) +
      geom_point(size = 4.5, shape = 21, stroke = 1.5,
                 colour = "white") +
      scale_colour_manual(
        values = c("TRUE" = pal$coral, "FALSE" = pal$muted),
        labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
        name = NULL) +
      scale_fill_manual(
        values = c("TRUE" = pal$coral, "FALSE" = pal$muted),
        guide = "none") +
      labs(x = "Estimate (β for continuous; OR for binary)",
           y = NULL, title = NULL) +
      theme_walk() +
      theme(legend.position = "top")
    ggplotly(p, tooltip = "text") |>
      config(cfg) |>
      layout(font = tc,
             paper_bgcolor = pal$card,
             plot_bgcolor  = pal$card,
             legend = list(orientation = "h", x = 0, y = 1.12))
  })
  
  # ─ CVD & Diabetes bars ─────────────────────────────────
  output$cvdBarPlot <- renderPlotly({
    p <- ggplot(pop_chars,
                aes(x = walkability, y = cvd_risk, fill = walkability,
                    text = paste0(walkability,
                                  ": ", cvd_risk, "% 10-yr CVD risk"))) +
      geom_col(width = 0.55, alpha = 0.9) +
      scale_fill_manual(
        values = c("Low" = "#CBD5E1",
                   "Moderate" = "#FDA4AF",
                   "High" = pal$coral),
        guide = "none") +
      labs(x = NULL, y = "10-yr CVD risk (%)",
           title = "10-yr CVD Risk") +
      theme_walk(base = 11)
    ggplotly(p, tooltip = "text") |>
      config(cfg) |>
      layout(font = tc,
             paper_bgcolor = pal$card,
             plot_bgcolor  = pal$card)
  })
  
  output$diabBarPlot <- renderPlotly({
    p <- ggplot(pop_chars,
                aes(x = walkability, y = diabetes, fill = walkability,
                    text = paste0(walkability,
                                  ": ", diabetes, "% prevalence"))) +
      geom_col(width = 0.55, alpha = 0.9) +
      scale_fill_manual(
        values = c("Low" = "#CBD5E1",
                   "Moderate" = pal$teal_lt,
                   "High" = pal$teal),
        guide = "none") +
      labs(x = "Walkability", y = "Diabetes (%)",
           title = "Diabetes Prevalence") +
      theme_walk(base = 11)
    ggplotly(p, tooltip = "text") |>
      config(cfg) |>
      layout(font = tc,
             paper_bgcolor = pal$card,
             plot_bgcolor  = pal$card)
  })
  
  # ─ Dose-response ───────────────────────────────────────
  output$doseResponsePlot <- renderPlotly({
    wi_gain <- seq(0, 50, by = 5)
    cvd_red <- 11.7 - (0.057 * wi_gain)   # 0.57% per 10-pt
    d <- data.frame(wi_gain, cvd_red)
    
    p <- ggplot(d, aes(x = wi_gain, y = cvd_red,
                       text = paste0("+", wi_gain, " WI points<br>",
                                     "CVD risk: ", round(cvd_red, 2), "%"))) +
      geom_ribbon(aes(ymin = cvd_red - 0.6, ymax = cvd_red + 0.6),
                  fill = pal$teal, alpha = 0.12) +
      geom_line(colour = pal$teal, linewidth = 1.6) +
      geom_point(colour = pal$teal, size = 3, alpha = 0.8) +
      geom_hline(yintercept = 11.7, linetype = "dashed",
                 colour = pal$muted) +
      annotate("text", x = 1, y = 12.05,
               label = "Baseline CVD risk (11.7%)", hjust = 0,
               colour = pal$muted, size = 3) +
      scale_y_continuous(labels = function(x) paste0(x, "%"),
                         limits = c(8, 13)) +
      labs(x = "Walkability index gain (points)",
           y = "Predicted 10-yr CVD risk",
           title = NULL) +
      theme_walk()
    ggplotly(p, tooltip = "text") |>
      config(cfg) |>
      layout(font = tc,
             paper_bgcolor = pal$card,
             plot_bgcolor  = pal$card)
  })
  
  # ─ Summary evidence chart ──────────────────────────────
  output$summaryPlot <- renderPlotly({
    d <- data.frame(
      outcome  = c("Overall Walking","Active Commuting","MVPA",
                   "Total Activity","10-yr CVD Risk",
                   "CVD Risk ≥20%","Hypertension","Diabetes"),
      domain   = c(rep("Physical Activity", 4),
                   rep("Cardiometabolic", 4)),
      sig      = c(TRUE, TRUE, TRUE, TRUE,
                   TRUE, TRUE, FALSE, TRUE),
      direction= c(1, 1, 1, 1, -1, -1, 0, -1),
      p_val    = c(0.024, 0.002, 0.001, 0.002,
                   0.001, 0.031, 0.250, 0.001)
    )
    d$neg_log_p <- -log10(d$p_val)
    d$outcome   <- factor(d$outcome, levels = rev(d$outcome))
    d$dir_label <- ifelse(d$direction == 1, "↑ Benefit (more PA)",
                          ifelse(d$direction == -1, "↓ Benefit (less risk)",
                                 "No significant effect"))
    d$col <- ifelse(!d$sig, pal$muted,
                    ifelse(d$direction >= 0, pal$teal, pal$coral))
    
    p <- ggplot(d, aes(x = neg_log_p, y = outcome, fill = col,
                       text = paste0(outcome,
                                     "<br>p = ", p_val,
                                     "<br>−log10(p) = ", round(neg_log_p,2),
                                     "<br>", dir_label))) +
      geom_col(width = 0.55, alpha = 0.9) +
      geom_vline(xintercept = -log10(0.05),
                 linetype = "dashed", colour = pal$amber, linewidth = 0.8) +
      annotate("text", x = -log10(0.05) + 0.05, y = 0.5,
               label = "p = 0.05", colour = pal$amber,
               size = 3, hjust = 0, vjust = 0) +
      scale_fill_identity() +
      scale_x_continuous(
        breaks = c(0,1,2,3),
        labels = c("1","0.1","0.01","0.001")) +
      facet_wrap(~domain, scales = "free_y", ncol = 1) +
      labs(x = "p-value (log scale; longer = more significant)",
           y = NULL, title = NULL) +
      theme_walk() +
      theme(strip.text = element_text(
        colour = pal$navy, face = "bold", size = 10),
        strip.background = element_rect(
          fill = "#E0F2F1", colour = NA))
    ggplotly(p, tooltip = "text") |>
      config(cfg) |>
      layout(font = tc,
             paper_bgcolor = pal$bg,
             plot_bgcolor  = pal$card)
  })
}

# ── Run ───────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
