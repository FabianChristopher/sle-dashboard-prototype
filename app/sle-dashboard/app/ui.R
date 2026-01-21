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
              tags$div(class = "sle-card__title", "Key Labs & Trends (Last 3 Visits)"),
              tags$div(
                class = "sle-card--scrollable",
                style = "max-height: 300px; overflow-y: scroll;",
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
              class = "sle-grid sle-grid--2",
              tags$div(
                class = "sle-card",
                tags$div(class = "sle-card__title", "Steroid Info"),
                uiOutput("steroid_info")
              ),
              tags$div(
                class = "sle-card sle-card--muted",
                tags$div(class = "sle-card__title", "Notes"),
                tags$div(
                  class = "sle-muted",
                  "Medication, steroid, and provider details are not present in the workbook."
                )
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
              class = "sle-grid sle-grid--3",
              tags$div(
                class = "sle-card",
                tags$div(class = "sle-card__title", "Current SLE Medications"),
                uiOutput("meds_current_sle")
              ),
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
