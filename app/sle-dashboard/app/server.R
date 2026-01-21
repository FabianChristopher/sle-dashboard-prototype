library(shiny)

format_date_short <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return("—")
  }
  format(as.Date(x), "%m/%d/%y")
}

timepoint_rank <- function(label) {
  label <- tolower(as.character(label))
  if (is.na(label) || nchar(label) == 0) return(0)
  if (grepl("after", label, fixed = TRUE)) return(3)
  if (grepl("biopsy", label, fixed = TRUE)) return(2)
  if (grepl("before", label, fixed = TRUE)) return(1)
  0
}

timepoint_short_label <- function(label) {
  label <- as.character(label)
  if (is.na(label) || nchar(label) == 0) return("Visit")
  l <- tolower(label)
  # Check the specific timepoint phrases before the generic "biopsy" match.
  if (grepl("3 months before biopsy", l, fixed = TRUE)) return("3M Before")
  if (grepl("3 months after biopsy", l, fixed = TRUE)) return("3M After")
  if (l == "biopsy" || grepl(" biopsy", l, fixed = TRUE) || grepl("biopsy ", l, fixed = TRUE)) return("Biopsy")
  label
}

as_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

normalize_lab_for_display <- function(lab_name, unit, value) {
  value <- as_num(value)
  unit <- if (!is.null(unit) && !is.na(unit) && nchar(unit) > 0) unit else NA_character_

  if (is.na(value)) {
    return(list(value = NA_real_, unit = unit))
  }

  # The loader parses strings like "6.6*10^3" into 6600.
  # For UI, match clinical display conventions.
  if (!is.na(unit) && grepl("K/", unit, fixed = TRUE)) {
    # e.g., Platelets (K/µL) should show 288 not 288000.
    if (value >= 1000) value <- value / 1000
    return(list(value = value, unit = unit))
  }

  # Some workbook labels are "cells/μL" but the mock UI uses k/μL.
  if (!is.na(unit) && grepl("cells/", unit, fixed = TRUE)) {
    if (value >= 1000) {
      value <- value / 1000
      unit <- "k/µL"
    }
    return(list(value = value, unit = unit))
  }

  list(value = value, unit = unit)
}

format_lab_value <- function(lab_name, unit, value, digits = 2) {
  norm <- normalize_lab_for_display(lab_name, unit, value)
  if (is.na(norm$value)) return("—")

  # Avoid showing 2 decimals for big integers.
  if (abs(norm$value) >= 100) {
    return(formatC(norm$value, format = "f", digits = 0))
  }

  formatC(norm$value, format = "f", digits = digits)
}

sparkline_svg <- function(values, stroke = "#7fb4ff") {
  values <- as_num(values)
  if (length(values) == 0 || all(is.na(values))) {
    return(tags$div(class = "sle-muted", "—"))
  }
  x <- seq_along(values)
  ok <- !is.na(values)
  x <- x[ok]
  y <- values[ok]
  if (length(y) < 2) {
    return(tags$div(class = "sle-muted", "—"))
  }

  w <- 120
  h <- 22
  pad <- 2
  min_y <- min(y)
  max_y <- max(y)
  if (min_y == max_y) {
    max_y <- max_y + 1
    min_y <- min_y - 1
  }
  sx <- function(xi) {
    pad + (xi - min(x)) / (max(x) - min(x)) * (w - 2 * pad)
  }
  sy <- function(yi) {
    pad + (1 - (yi - min_y) / (max_y - min_y)) * (h - 2 * pad)
  }

  pts <- paste(paste0(round(sx(x), 2), ",", round(sy(y), 2)), collapse = " ")
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    width = w,
    height = h,
    viewBox = paste(0, 0, w, h),
    tags$polyline(
      points = pts,
      fill = "none",
      stroke = stroke,
      `stroke-width` = 2,
      `stroke-linecap` = "round",
      `stroke-linejoin` = "round"
    )
  )
}

lab_flag <- function(lab_name, value) {
  value <- as_num(value)
  if (is.na(value)) {
    return(list(flag = NULL, class = NULL))
  }
  # Clinical reference thresholds for flagging abnormal values
  if (lab_name == "C3 complement" && value < 80) return(list(flag = "L", class = "is-low"))
  if (lab_name == "C4 complement" && value < 12) return(list(flag = "L", class = "is-low"))
  if (lab_name == "UPCR" && value > 0.5) return(list(flag = "H", class = "is-high"))
  if (lab_name == "Albumin" && value < 3.5) return(list(flag = "L", class = "is-low"))
  if (lab_name == "eGFR" && value < 60) return(list(flag = "L", class = "is-low"))
  if (lab_name == "ESR" && value > 20) return(list(flag = "H", class = "is-high"))
  if (lab_name == "CRP" && value > 1.0) return(list(flag = "H", class = "is-high"))
  if (lab_name == "Anti ds-DNA" && value > 10) return(list(flag = "H", class = "is-high"))
  list(flag = NULL, class = NULL)
}

sql_placeholders <- function(n, start_index = 1) {
  paste0("$", seq.int(start_index, length.out = n), collapse = ",")
}

as_checkbox_row <- function(label, value_text, checked) {
  tags$div(
    class = "sle-kv",
    tags$span(class = paste("sle-check", if (checked) "is-on" else "is-off")),
    tags$span(class = "sle-kv__label", label),
    tags$span(class = "sle-kv__value", value_text)
  )
}

server <- function(input, output, session) {
  con <- get_db()
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })

  domain_name_map <- c(
    "Arthritis" = "Joint",
    "Pulm" = "Pulm"
  )
  domain_order <- c("Skin", "Joint", "Renal", "Cardiac", "Pulm", "Gastrointestinal", "Neuro", "Oral Ulcers")

  patients <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "external_id,",
      "COALESCE(NULLIF(TRIM(CONCAT_WS(' ', first_name, last_name)), ''), external_id) AS display_name",
      "FROM patients",
      "ORDER BY external_id"
    )
  )

  patient_choices <- setNames(patients$external_id, patients$display_name)
  updateSelectInput(
    session,
    "patient",
    choices = patient_choices,
    selected = if (length(patients$external_id) > 0) patients$external_id[1] else NULL
  )

  patient_row <- reactive({
    req(input$patient)
    DBI::dbGetQuery(
      con,
      paste(
        "SELECT external_id, first_name, last_name, sex, diagnosis_date",
        "FROM patients",
        "WHERE external_id = $1"
      ),
      params = list(input$patient)
    )
  })

  visits_df <- reactive({
    req(input$patient)
    DBI::dbGetQuery(
      con,
      paste(
        "SELECT v.visit_id, v.visit_date, v.visit_type",
        "FROM visits v",
        "JOIN patients p ON p.patient_id = v.patient_id",
        "WHERE p.external_id = $1",
        "ORDER BY v.visit_date DESC"
      ),
      params = list(input$patient)
    )
  })

  last_visits <- reactive({
    v <- visits_df()
    if (nrow(v) == 0) return(v)
    v[seq_len(min(3, nrow(v))), , drop = FALSE]
  })

  # All 15 labs from the workbook, ordered by clinical relevance
  key_labs <- c(
    "Anti ds-DNA",
    "C3 complement",
    "C4 complement",
    "UPCR",
    "Urine Protein",
    "Urine Creatinine",
    "eGFR",
    "Albumin",
    "WBC",
    "ANC units",
    "ALC",
    "Platelets",
    "ESR",
    "CRP",
    "IgA"
  )
  lab_display <- c(
    "Anti ds-DNA" = "dsDNA",
    "C3 complement" = "C3",
    "C4 complement" = "C4",
    "UPCR" = "UPCR",
    "Urine Protein" = "U-Protein",
    "Urine Creatinine" = "U-Creat",
    "eGFR" = "eGFR",
    "Albumin" = "Albumin",
    "WBC" = "WBC",
    "ANC units" = "ANC",
    "ALC" = "ALC",
    "Platelets" = "Platelets",
    "ESR" = "ESR",
    "CRP" = "CRP",
    "IgA" = "IgA"
  )

  labs_df <- reactive({
    req(input$patient)
    v <- last_visits()
    if (nrow(v) == 0) {
      return(data.frame())
    }

    # Fetch labs for the patient's last 3 visits.
    visit_ids <- v$visit_id
    p1 <- sql_placeholders(length(visit_ids), start_index = 2)
    p2 <- sql_placeholders(length(key_labs), start_index = 2 + length(visit_ids))

    sql <- paste(
      "SELECT l.visit_id, l.collected_date, l.lab_name, l.lab_value, l.lab_unit",
      "FROM labs l",
      "JOIN patients p ON p.patient_id = l.patient_id",
      "WHERE p.external_id = $1",
      paste0("AND l.visit_id IN (", p1, ")"),
      paste0("AND l.lab_name IN (", p2, ")")
    )

    params <- c(list(input$patient), as.list(visit_ids), as.list(key_labs))
    DBI::dbGetQuery(con, sql, params = params)
  })

  domains_current_df <- reactive({
    req(input$patient)
    v <- visits_df()
    if (nrow(v) == 0) return(data.frame())
    latest_visit_id <- v$visit_id[1]
    DBI::dbGetQuery(
      con,
      paste(
        "SELECT d.domain_name, d.active, d.ever_involved",
        "FROM domains d",
        "JOIN patients p ON p.patient_id = d.patient_id",
        "WHERE p.external_id = $1",
        "AND d.visit_id = $2",
        "ORDER BY d.domain_name"
      ),
      params = list(input$patient, latest_visit_id)
    )
  })

  domains_ever_df <- reactive({
    req(input$patient)
    DBI::dbGetQuery(
      con,
      paste(
        "SELECT d.domain_name, BOOL_OR(d.active) AS ever_active, BOOL_OR(d.ever_involved) AS ever_involved",
        "FROM domains d",
        "JOIN patients p ON p.patient_id = d.patient_id",
        "WHERE p.external_id = $1",
        "GROUP BY d.domain_name",
        "ORDER BY d.domain_name"
      ),
      params = list(input$patient)
    )
  })

  output$patient_header <- renderUI({
    req(input$patient)
    pr <- patient_row()
    if (nrow(pr) == 0) {
      return(tags$span("Patient: —"))
    }
    # Workbook does not provide demographics; only render fields that exist.
    name <- paste(na.omit(c(pr$first_name[1], pr$last_name[1])), collapse = " ")
    if (is.null(name) || nchar(name) == 0) name <- pr$external_id[1]

    pieces <- list(tags$span(class = "sle-header__patient", paste0("Patient: ", name)))
    if (!is.na(pr$diagnosis_date[1])) {
      pieces[[length(pieces) + 1]] <- tags$span(
        class = "sle-header__meta",
        paste0("SLE Dx: ", format(as.Date(pr$diagnosis_date[1]), "%Y"))
      )
    }
    if (!is.na(pr$sex[1]) && nchar(pr$sex[1]) > 0) {
      pieces[[length(pieces) + 1]] <- tags$span(class = "sle-header__meta", paste0("Gender: ", pr$sex[1]))
    }
    tags$div(pieces)
  })

  output$domains_summary <- renderUI({
    req(input$patient)
    d <- domains_current_df()
    if (nrow(d) == 0) {
      return(tags$div(class = "sle-muted", "No domain data for this patient."))
    }
    d$display_name <- ifelse(d$domain_name %in% names(domain_name_map), domain_name_map[d$domain_name], d$domain_name)
    d$sort_key <- match(d$display_name, domain_order)
    d$sort_key[is.na(d$sort_key)] <- 999
    d <- d[order(d$sort_key, d$display_name), , drop = FALSE]

    items <- vapply(seq_len(nrow(d)), function(i) {
      paste0(d$display_name[i], " (", if (isTRUE(d$active[i])) "Y" else "N", ")")
    }, character(1))
    tags$div(class = "sle-domain-summary", paste(items, collapse = ", "))
  })

  output$key_labs_table <- renderUI({
    req(input$patient)
    v <- last_visits()
    if (nrow(v) == 0) {
      return(tags$div(class = "sle-muted", "No visits found."))
    }

    labs <- labs_df()

    # Use workbook timepoint labels for ordering and display (avoid synthetic dates).
    v_sorted <- v
    v_sorted$type <- ifelse(!is.na(v_sorted$visit_type), as.character(v_sorted$visit_type), NA_character_)
    v_sorted$rank <- vapply(v_sorted$type, timepoint_rank, numeric(1))
    v_sorted <- v_sorted[order(v_sorted$rank, decreasing = TRUE), , drop = FALSE]

    visit_cols <- lapply(seq_len(nrow(v_sorted)), function(i) {
      list(
        visit_id = v_sorted$visit_id[i],
        type = v_sorted$type[i],
        hdr = timepoint_short_label(v_sorted$type[i])
      )
    })

    row_ui <- lapply(key_labs, function(lab_name) {
      lab_rows <- labs[labs$lab_name == lab_name, , drop = FALSE]

      values_newest <- vapply(visit_cols, function(vc) {
        r <- lab_rows[lab_rows$visit_id == vc$visit_id, , drop = FALSE]
        if (nrow(r) == 0) return(NA_real_)
        norm <- normalize_lab_for_display(lab_name, r$lab_unit[1], r$lab_value[1])
        norm$value
      }, numeric(1))

      # For sparkline, use oldest -> newest so trend reads left->right
      values_oldest <- rev(values_newest)

      unit <- ""
      if (nrow(lab_rows) > 0) {
        unit <- lab_rows$lab_unit[1]
        unit <- normalize_lab_for_display(lab_name, unit, lab_rows$lab_value[1])$unit
        if (is.na(unit)) unit <- ""
      }

      label <- if (!is.na(lab_display[[lab_name]])) lab_display[[lab_name]] else lab_name
      lab_label <- if (nchar(unit) > 0) paste0(label, " (", unit, ")") else label

      vals_ui <- lapply(seq_along(visit_cols), function(i) {
        val <- values_newest[i]
        fl <- lab_flag(lab_name, val)

        val_txt <- if (is.na(val)) {
          "—"
        } else {
          format_lab_value(lab_name, unit, val)
        }

        badge <- if (!is.null(fl$flag)) paste0(" (", fl$flag, ")") else ""
        tags$div(
          class = paste("sle-lab-value", fl$class),
          paste0(val_txt, badge)
        )
      })

      stroke <- if (lab_name %in% c("C3 complement", "C4 complement", "UPCR")) "#ff6b6b" else "#7fb4ff"

      tags$tr(
        tags$td(class = "sle-lab-name", lab_label),
        tags$td(class = "sle-lab-spark", sparkline_svg(values_oldest, stroke = stroke)),
        lapply(vals_ui, function(x) tags$td(class = "sle-lab-col", x))
      )
    })

    header_dates <- lapply(seq_along(visit_cols), function(i) {
      vc <- visit_cols[[i]]
      tags$th(
        class = "sle-lab-col",
        tags$div(class = "sle-colhdr", vc$hdr)
      )
    })

    tags$table(
      class = "sle-table",
      tags$thead(
        tags$tr(
          tags$th(class = "sle-lab-name", tags$div(class = "sle-colhdr", "Lab")),
          tags$th(class = "sle-lab-spark", tags$div(class = "sle-colhdr", "Trend")),
          header_dates
        )
      ),
      tags$tbody(row_ui)
    )
  })

  output$most_recent_visit <- renderUI({
    req(input$patient)
    v <- visits_df()
    if (nrow(v) == 0) {
      return(tags$div(class = "sle-muted", "No visits found."))
    }
    # Do not display synthetic dates; show the workbook timepoint label instead.
    tp <- if (!is.na(v$visit_type[1])) as.character(v$visit_type[1]) else "(missing timepoint label)"
    tags$div(
      class = "sle-kv-list",
      tags$div(class = "sle-kv-line", tags$span("Most recent timepoint:"), tags$strong(tp)),
      tags$div(class = "sle-muted", "Visit calendar dates are not present in the workbook.")
    )
  })

  output$active_domains_current <- renderUI({
    req(input$patient)
    d <- domains_current_df()
    if (nrow(d) == 0) {
      return(tags$div(class = "sle-muted", "No domain data for the most recent visit."))
    }

    d$display_name <- ifelse(d$domain_name %in% names(domain_name_map), domain_name_map[d$domain_name], d$domain_name)
    d$sort_key <- match(d$display_name, domain_order)
    d$sort_key[is.na(d$sort_key)] <- 999
    d <- d[order(d$sort_key, d$display_name), , drop = FALSE]

    tags$div(
      class = "sle-grid sle-grid--2",
      lapply(seq_len(nrow(d)), function(i) {
        as_checkbox_row(d$display_name[i], if (isTRUE(d$active[i])) "Y" else "N", isTRUE(d$active[i]))
      })
    )
  })

  output$domains_historical <- renderUI({
    req(input$patient)
    d_ever <- domains_ever_df()
    d_cur <- domains_current_df()
    if (nrow(d_ever) == 0) {
      return(tags$div(class = "sle-muted", "No historical domain data."))
    }

    d_ever$display_name <- ifelse(d_ever$domain_name %in% names(domain_name_map), domain_name_map[d_ever$domain_name], d_ever$domain_name)
    d_ever$sort_key <- match(d_ever$display_name, domain_order)
    d_ever$sort_key[is.na(d_ever$sort_key)] <- 999
    d_ever <- d_ever[order(d_ever$sort_key, d_ever$display_name), , drop = FALSE]

    cur_map <- setNames(as.logical(d_cur$active), d_cur$domain_name)
    tags$div(
      class = "sle-grid sle-grid--2",
      lapply(seq_len(nrow(d_ever)), function(i) {
        nm <- d_ever$domain_name[i]
        label <- d_ever$display_name[i]
        ever <- isTRUE(d_ever$ever_involved[i]) || isTRUE(d_ever$ever_active[i])
        cur <- if (!is.null(cur_map[[nm]])) isTRUE(cur_map[[nm]]) else FALSE
        tags$div(
          class = "sle-domain-row",
          tags$div(class = "sle-domain-row__name", label),
          tags$div(class = "sle-domain-row__chips", 
                   tags$span(class = paste("sle-chip", if (cur) "is-on" else "is-off"), paste0("Current: ", if (cur) "Y" else "N")),
                   tags$span(class = paste("sle-chip", if (ever) "is-on" else "is-off"), paste0("Ever: ", if (ever) "Y" else "N"))
          )
        )
      })
    )
  })

  output$steroid_info <- renderUI({
    tags$div(
      class = "sle-kv-list",
      tags$div(class = "sle-muted", "Steroid data not present in the workbook.")
    )
  })

  # Biopsy Summary Tab outputs
  output$biopsy_info <- renderUI({
    req(input$patient)
    v <- visits_df()
    biopsy_visit <- v[grepl("biopsy", tolower(v$visit_type)), , drop = FALSE]
    
    if (nrow(biopsy_visit) == 0) {
      return(tags$div(class = "sle-muted", "No biopsy data available for this patient."))
    }
    
    tags$div(
      class = "sle-kv-list",
      tags$div(
        class = "sle-kv-line",
        tags$span("Biopsy Timepoint:"),
        tags$strong("Renal Biopsy")
      ),
      tags$div(
        class = "sle-kv-line",
        tags$span("Patient ID:"),
        tags$strong(input$patient)
      ),
      tags$div(
        class = "sle-muted",
        style = "margin-top: 10px;",
        "Detailed biopsy pathology data will be available when integrated with renal pathology reports."
      )
    )
  })
  
  output$biopsy_findings <- renderUI({
    req(input$patient)
    # Get renal-related labs at biopsy timepoint
    v <- visits_df()
    biopsy_visit <- v[grepl("biopsy", tolower(v$visit_type)) & !grepl("before|after", tolower(v$visit_type)), , drop = FALSE]
    
    if (nrow(biopsy_visit) == 0) {
      return(tags$div(class = "sle-muted", "No biopsy findings available."))
    }
    
    # Get key renal labs at biopsy
    renal_labs <- c("UPCR", "Urine Protein", "Urine Creatinine", "eGFR", "Albumin", "C3 complement", "C4 complement")
    
    # Build dynamic placeholders for the IN clause
    lab_placeholders <- sql_placeholders(length(renal_labs), start_index = 3)
    
    labs <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT l.lab_name, l.lab_value, l.lab_unit",
        "FROM labs l",
        "JOIN patients p ON p.patient_id = l.patient_id",
        "WHERE p.external_id = $1",
        "AND l.visit_id = $2",
        paste0("AND l.lab_name IN (", lab_placeholders, ")")
      ),
      params = c(list(input$patient, biopsy_visit$visit_id[1]), as.list(renal_labs))
    )
    
    if (nrow(labs) == 0) {
      return(tags$div(class = "sle-muted", "No renal-specific labs at biopsy timepoint."))
    }
    
    tags$div(
      class = "sle-biopsy-labs",
      tags$h4(style = "margin: 0 0 12px 0; font-size: 14px; color: rgba(255,255,255,0.7);", "Renal Labs at Biopsy:"),
      tags$table(
        class = "sle-table",
        tags$tbody(
          lapply(seq_len(nrow(labs)), function(i) {
            val <- format_lab_value(labs$lab_name[i], labs$lab_unit[i], labs$lab_value[i])
            unit <- if (!is.na(labs$lab_unit[i])) paste0(" (", labs$lab_unit[i], ")") else ""
            tags$tr(
              tags$td(style = "font-weight: 600;", paste0(labs$lab_name[i], unit)),
              tags$td(style = "text-align: right; font-weight: 700;", val)
            )
          })
        )
      )
    )
  })
  
  output$biopsy_prognosis <- renderUI({
    req(input$patient)
    d <- domains_current_df()
    renal_active <- any(d$domain_name == "Renal" & isTRUE(d$active))
    
    tags$div(
      class = "sle-prognosis-list",
      tags$div(
        class = paste("sle-prognosis-item", if (renal_active) "is-active" else "is-inactive"),
        tags$span(class = "sle-prognosis-icon", if (renal_active) "⚠️" else "✓"),
        tags$span("Renal Involvement: ", tags$strong(if (renal_active) "Active" else "Inactive"))
      ),
      tags$div(
        class = "sle-muted",
        style = "margin-top: 15px; font-size: 13px;",
        "Prognostic scoring (e.g., NIH Activity/Chronicity Index) will be available when pathology data is integrated."
      )
    )
  })
  
  output$biopsy_notes <- renderUI({
    tags$div(
      class = "sle-muted",
      tags$p("Pathologist notes and detailed biopsy interpretation will be displayed here once integrated with the renal pathology system."),
      tags$p(style = "margin-top: 10px;", 
        tags$em("This section is designed for collaboration with the renal pathology team to include ISN/RPS classification, activity index, chronicity index, and treatment recommendations.")
      )
    )
  })

  meds_df <- reactive({
    req(input$patient)
    DBI::dbGetQuery(
      con,
      paste(
        "SELECT m.medication_name, m.category, m.dose, m.route, m.frequency, m.current, m.stop_reason, m.end_date",
        "FROM medications m",
        "JOIN patients p ON p.patient_id = m.patient_id",
        "WHERE p.external_id = $1",
        "ORDER BY m.current DESC, m.category NULLS LAST, m.medication_name"
      ),
      params = list(input$patient)
    )
  })

  render_med_list <- function(df, empty_text) {
    if (nrow(df) == 0) {
      return(tags$div(class = "sle-muted", empty_text))
    }
    tags$ul(
      class = "sle-med-list",
      lapply(seq_len(nrow(df)), function(i) {
        line <- df$medication_name[i]
        if (!is.na(df$dose[i]) && nchar(df$dose[i]) > 0) line <- paste(line, df$dose[i])
        if (!is.na(df$frequency[i]) && nchar(df$frequency[i]) > 0) line <- paste(line, df$frequency[i])
        if (!isTRUE(df$current[i]) && !is.na(df$stop_reason[i]) && nchar(df$stop_reason[i]) > 0) {
          end_txt <- if (!is.na(df$end_date[i])) format_date_short(df$end_date[i]) else ""
          line <- paste0(line, " (Stopped", if (nchar(end_txt) > 0) paste0(": ", end_txt) else "", ": ", df$stop_reason[i], ")")
        }
        tags$li(line)
      })
    )
  }

  output$meds_current_sle <- renderUI({
    df <- meds_df()
    df <- df[isTRUE(df$current) & (is.na(df$category) | df$category %in% c("SLE", "sle")), , drop = FALSE]
    render_med_list(df, "Medication data not present in the workbook.")
  })

  output$meds_current_bp <- renderUI({
    df <- meds_df()
    df <- df[isTRUE(df$current) & (!is.na(df$category) & df$category %in% c("BP", "bp")), , drop = FALSE]
    render_med_list(df, "Medication data not present in the workbook.")
  })

  output$meds_current_cardio <- renderUI({
    df <- meds_df()
    df <- df[isTRUE(df$current) & (!is.na(df$category) & df$category %in% c("Cardiometabolic", "cardio", "metabolic")), , drop = FALSE]
    render_med_list(df, "Medication data not present in the workbook.")
  })

  output$meds_prior <- renderUI({
    df <- meds_df()
    df <- df[!isTRUE(df$current), , drop = FALSE]
    render_med_list(df, "Medication data not present in the workbook.")
  })

  observeEvent(input$open_additional_labs, {
    req(input$patient)
    all_labs <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT DISTINCT l.lab_name",
        "FROM labs l",
        "JOIN patients p ON p.patient_id = l.patient_id",
        "WHERE p.external_id = $1",
        "ORDER BY l.lab_name"
      ),
      params = list(input$patient)
    )$lab_name

    showModal(
      modalDialog(
        title = "Additional Laboratory Parameters",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tags$div(
          class = "sle-modal-content",
          tags$div(
            class = "sle-modal-select",
            selectInput("additional_lab", "Select Lab Parameter", choices = all_labs, selected = if (length(all_labs) > 0) all_labs[1] else NULL, width = "100%")
          ),
          plotOutput("additional_lab_plot", height = "400px")
        )
      )
    )
  })

  output$additional_lab_plot <- renderPlot({
    req(input$patient)
    req(input$additional_lab)
    v <- visits_df()
    if (nrow(v) == 0) return(NULL)
    df <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT v.visit_id, v.visit_type, l.lab_value",
        "FROM visits v",
        "JOIN patients p ON p.patient_id = v.patient_id",
        "LEFT JOIN labs l ON l.visit_id = v.visit_id AND l.lab_name = $2",
        "WHERE p.external_id = $1",
        "ORDER BY v.visit_date"
      ),
      params = list(input$patient, input$additional_lab)
    )
    y <- as_num(df$lab_value)

    df$type <- ifelse(!is.na(df$visit_type), as.character(df$visit_type), NA_character_)
    df$rank <- vapply(df$type, timepoint_rank, numeric(1))
    df <- df[order(df$rank), , drop = FALSE]

    # Normalize lab values for display (match table convention)
    lab_unit_row <- DBI::dbGetQuery(
      con,
      paste("SELECT lab_unit FROM labs WHERE lab_name = $1 LIMIT 1"),
      params = list(input$additional_lab)
    )
    lab_unit <- if (nrow(lab_unit_row) > 0) lab_unit_row$lab_unit[1] else NA_character_
    
    y_normalized <- sapply(y, function(val) {
      if (is.na(val)) return(NA_real_)
      norm <- normalize_lab_for_display(input$additional_lab, lab_unit, val)
      norm$value
    })

    x <- seq_len(nrow(df))
    
    # Dark theme styling with better margins
    par(bg = "#0b1420", col.axis = "#e0e0e0", col.lab = "#e0e0e0", col.main = "#ffffff", 
        fg = "#ffffff", col = "#ffffff", mar = c(5, 5, 4, 2))
    
    plot(
      x,
      y_normalized[order(df$rank)],
      type = "b",
      pch = 19,
      col = "#4a9eff",
      lwd = 3,
      cex = 2,
      xaxt = "n",
      xlab = "Timepoint",
      ylab = input$additional_lab,
      main = "",
      cex.lab = 1.3,
      cex.axis = 1.1
    )
    axis(1, at = x, labels = vapply(df$type, timepoint_short_label, character(1)), 
         col.axis = "#e0e0e0", col = "#606060", cex.axis = 1.1, lwd = 0, lwd.ticks = 1)
    grid(col = "#404040", lwd = 1, lty = 2)
  })
}
