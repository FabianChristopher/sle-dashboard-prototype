library(shiny)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "assets/styles.css")
  ),
  # Landing Page
  conditionalPanel(
    condition = "!input.enter_dashboard",
    tags$div(
      class = "sle-landing",
      tags$div(
        class = "sle-landing__content",
        tags$div(class = "sle-landing__icon", "🩺"),
        tags$h1(class = "sle-landing__title", "SLE Clinical Dashboard"),
        tags$p(class = "sle-landing__subtitle", "Systemic Lupus Erythematosus Patient Management Interface"),
        tags$p(class = "sle-landing__desc", 
          "A comprehensive dashboard for rheumatologists to review patient labs, organ involvement, medications, and biopsy results at a glance."
        ),
        tags$div(
          class = "sle-landing__features",
          tags$div(class = "sle-landing__feature", tags$span("📊"), "Lab Trends & Analysis"),
          tags$div(class = "sle-landing__feature", tags$span("🫀"), "Organ Domain Tracking"),
          tags$div(class = "sle-landing__feature", tags$span("💊"), "Medication Management"),
          tags$div(class = "sle-landing__feature", tags$span("🔬"), "Biopsy Summary")
        ),
        actionButton(
          "enter_dashboard",
          label = "Enter Dashboard",
          class = "sle-btn sle-btn--primary sle-btn--large"
        ),
        tags$p(class = "sle-landing__footer", "NYU Langone Health • Rheumatology Division • Renal Bx LLM-Digital Interface Pilot")
      )
    )
  ),
  # Main Dashboard (hidden until button clicked)
  conditionalPanel(
    condition = "input.enter_dashboard",
    tags$div(
      class = "sle-app",
      tags$div(
        class = "sle-header",
        tags$div(
          class = "sle-header__title",
          uiOutput("patient_header")
        ),
        tags$div(
          class = "sle-header__controls",
          selectInput("patient", NULL, choices = character(0), width = "220px")
        )
      ),
      tags$div(
        class = "sle-body",
        tabsetPanel(
          id = "main_tab",
          type = "tabs",
          tabPanel(
            "Overview & Trends",
            tags$div(
              class = "sle-card sle-card--tight",
              tags$div(class = "sle-card__title", "Organ Domains Involved"),
              uiOutput("domains_summary")
            ),
            tags$div(
              class = "sle-card",
              tags$div(
                class = "sle-card__title sle-card__title--row",
                tags$span("Key Labs & Trends"),
                selectInput(
                  "window_months",
                  NULL,
                  choices = c("3 months" = "3", "6 months" = "6", "1 year" = "12", "18 months" = "18"),
                  selected = "3",
                  width = "160px"
                )
              ),
              tags$div(
                class = "sle-legend",
                style = "font-size: 12px; color: rgba(255,255,255,0.7); margin: 8px 0 12px 0; padding: 0 16px;",
                tags$span("Trend colors: "),
                tags$span(style = "color: #4a9eff; font-weight: 600;", "Blue"),
                tags$span(" = improving, "),
                tags$span(style = "color: #ff6b6b; font-weight: 600;", "Red"),
                tags$span(" = worsening, "),
                tags$span(style = "color: #8a8f99; font-weight: 600;", "Grey"),
                tags$span(" = stable/unknown")
              ),
              tags$div(
                class = "sle-card--scrollable sle-sticky-table",
                style = "max-height: 300px; overflow-y: auto;",
                uiOutput("key_labs_table")
              ),
              tags$div(
                class = "sle-card__footer",
                actionButton(
                  "open_additional_labs",
                  label = "Additional Laboratory Parameters",
                  class = "sle-btn"
                )
              )
            ),
            tags$div(
              class = "sle-card",
              tags$div(class = "sle-card__title", "Current SLE Medications"),
              uiOutput("meds_current_sle")
            ),
            tags$div(
              class = "sle-card",
              tags$div(class = "sle-card__title", "Biologics / Immunosuppressives (Biomeds Tracker)"),
              uiOutput("biomeds_tracker")
            ),
            tags$div(
              class = "sle-grid sle-grid--2",
              tags$details(
                class = "sle-card sle-card--collapsible",
                open = NA,
                tags$summary(
                  class = "sle-card__title",
                  style = "cursor: pointer; user-select: none;",
                  "Steroid Info"
                ),
                uiOutput("steroid_info")
              ),
              tags$div(
                class = "sle-card sle-card--muted",
                tags$div(class = "sle-card__title", "Notes"),
                uiOutput("notes_text")
              )
            )
          ),
          tabPanel(
            "Active Disease & PE",
            tags$div(
              class = "sle-grid sle-grid--2",
              tags$div(
                class = "sle-card",
                tags$div(class = "sle-card__title", "Most Recent Visit"),
                uiOutput("most_recent_visit")
              ),
              tags$div(
                class = "sle-card",
                tags$div(class = "sle-card__title", "Active Domains (Current)"),
                uiOutput("active_domains_current")
              )
            ),
            tags$details(
              class = "sle-accordion",
              open = NA,
              tags$summary("Organ Involvement (Historical/Stratification)"),
              tags$div(
                class = "sle-card sle-card--flat",
                uiOutput("domains_historical")
              )
            )
          ),
          tabPanel(
            "Biopsy Summary",
            tags$div(
              class = "sle-card",
              tags$div(class = "sle-card__title", "Renal Biopsy Information"),
              uiOutput("biopsy_info")
            ),
            tags$div(
              class = "sle-grid sle-grid--2",
              tags$div(
                class = "sle-card",
                tags$div(class = "sle-card__title", "Biopsy Findings"),
                tags$div(
                  class = "sle-card--scrollable",
                  style = "max-height: 200px; overflow-y: scroll;",
                  uiOutput("biopsy_findings")
                )
              ),
              tags$div(
                class = "sle-card",
                tags$div(class = "sle-card__title", "Prognostic Indicators"),
                uiOutput("biopsy_prognosis")
              )
            ),
            tags$div(
              class = "sle-card sle-card--muted",
              tags$div(class = "sle-card__title", "Pathologist Notes"),
              uiOutput("biopsy_notes")
            )
          ),
          tabPanel(
            "Medications",
            tags$div(
              class = "sle-grid sle-grid--2",
              tags$div(
                class = "sle-card",
                tags$div(class = "sle-card__title", "Current BP Medications"),
                uiOutput("meds_current_bp")
              ),
              tags$div(
                class = "sle-card",
                tags$div(class = "sle-card__title", "Prior SLE/Cardiometabolic Medications"),
                uiOutput("meds_prior")
              )
            ),
            tags$div(
              class = "sle-card",
              tags$div(class = "sle-card__title", "Current Cardiometabolic Medications"),
              uiOutput("meds_current_cardio")
            )
          )
        )
      )
    )
  )
)
