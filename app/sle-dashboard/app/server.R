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
  # Handle dsDNA special format: "4+" -> 4, "2+" -> 2, "0" -> 0
  if (is.character(x) && grepl("\\+$", x)) {
    x <- sub("\\+$", "", x)
  }
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

# Lab configuration for clinical semantics
# Each lab has:
# - ref_low/ref_high: Normal reference range boundaries (NA if one-sided)
# - direction: Clinical preference when no range defined or both values in/out of range
#   * lower_better: Lower values indicate improvement (inflammatory markers, proteinuria)
#   * higher_better: Higher values indicate improvement (kidney function, proteins, immune cells)
#   * neutral: No inherent clinical direction (used for informational labs)
# - within_range_policy: Behavior when BOTH values are within normal range
#   * "directional": Apply direction rule even when both in range (disease markers, organ function)
#   * "stable": Return "same" when both in range (CBC/hematologic safety labs)
#   * "neutral": Always return "neutral" regardless of change (informational only)

LAB_CONFIG <- list(
  # SLE Disease Activity Markers
  "Anti ds-DNA" = list(
    ref_low = NA, 
    ref_high = 10, 
    direction = "lower_better",
    within_range_policy = "directional",
    note = "Elevated anti-dsDNA correlates with SLE disease activity, especially lupus nephritis"
  ),
  
  # Complement Levels (consumed during active disease)
  "C3 complement" = list(
    ref_low = 80, 
    ref_high = 160, 
    direction = "higher_better",
    within_range_policy = "directional",
    note = "Low C3 indicates complement consumption from active disease"
  ),
  "C4 complement" = list(
    ref_low = 12, 
    ref_high = 40, 
    direction = "higher_better",
    within_range_policy = "directional",
    note = "Low C4 indicates complement consumption from active disease"
  ),
  
  # Renal Function Markers
  "UPCR" = list(
    ref_low = NA, 
    ref_high = 0.5, 
    direction = "lower_better",
    within_range_policy = "directional",
    note = "Urine protein-to-creatinine ratio; elevated indicates proteinuria/renal damage"
  ),
  "Urine Protein" = list(
    ref_low = NA, 
    ref_high = 150, 
    direction = "lower_better",
    within_range_policy = "directional",
    note = "Elevated urine protein indicates glomerular damage"
  ),
  "Urine Creatinine" = list(
    ref_low = NA, 
    ref_high = NA, 
    direction = "neutral",
    within_range_policy = "neutral",
    note = "Used as denominator for UPCR calculation; no independent clinical significance"
  ),
  "eGFR" = list(
    ref_low = 60, 
    ref_high = NA, 
    direction = "higher_better",
    within_range_policy = "directional",
    note = "Estimated glomerular filtration rate; lower values indicate reduced kidney function"
  ),
  "Albumin" = list(
    ref_low = 3.5, 
    ref_high = 5.5, 
    direction = "higher_better",
    within_range_policy = "directional",
    note = "Serum albumin; low levels can indicate nephrotic syndrome or malnutrition"
  ),
  
  # Inflammatory Markers
  "ESR" = list(
    ref_low = NA, 
    ref_high = 20, 
    direction = "lower_better",
    within_range_policy = "directional",
    note = "Erythrocyte sedimentation rate; elevated indicates inflammation"
  ),
  "CRP" = list(
    ref_low = NA, 
    ref_high = 1.0, 
    direction = "lower_better",
    within_range_policy = "directional",
    note = "C-reactive protein; elevated indicates acute inflammation"
  ),
  
  # Hematologic Safety Markers (stable policy - within range = OK)
  "WBC" = list(
    ref_low = 4.0, 
    ref_high = 11.0, 
    direction = "higher_better",
    within_range_policy = "stable",
    note = "White blood cell count; low in SLE can indicate leukopenia"
  ),
  "ANC units" = list(
    ref_low = 1.5, 
    ref_high = NA, 
    direction = "higher_better",
    within_range_policy = "stable",
    note = "Absolute neutrophil count; low values indicate neutropenia and infection risk"
  ),
  "ALC" = list(
    ref_low = 1.0, 
    ref_high = 4.8, 
    direction = "higher_better",
    within_range_policy = "stable",
    note = "Absolute lymphocyte count; low values indicate lymphopenia common in SLE"
  ),
  "Platelets" = list(
    ref_low = 150, 
    ref_high = 400, 
    direction = "higher_better",
    within_range_policy = "stable",
    note = "Platelet count; low values can indicate thrombocytopenia in SLE"
  ),
  
  # Immunoglobulin (neutral - informational only)
  "IgA" = list(
    ref_low = 70, 
    ref_high = 400, 
    direction = "neutral",
    within_range_policy = "neutral",
    note = "Immunoglobulin A; can be elevated in some nephropathies but not primary SLE marker"
  )
)

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

trend_status <- function(lab_name, v1, v3, epsilon = 0.1) {
  # Determines trend status by comparing visit 1 (3M BEFORE) to visit 3 (3M AFTER)
  # Returns: "better", "worse", "same", or "neutral"
  # 
  # Clinician-Safe Logic:
  # 1. Neutral-policy labs (IgA, Urine Creat): always return "neutral"
  # 2. Check if values are clinically equivalent (within epsilon) -> "same"
  # 3. Prioritize crossing range boundaries (abnormal<->normal)
  # 4. For both abnormal: use distance-to-normal
  # 5. For both in range: respect within_range_policy
  #    - "stable" policy (WBC, ANC, ALC, Platelets): return "same"
  #    - "directional" policy (disease/organ markers): apply direction rules
  
  if (is.na(v1) || is.na(v3)) return("neutral")
  
  # Get lab configuration
  cfg <- LAB_CONFIG[[lab_name]]
  if (is.null(cfg)) cfg <- list(ref_low = NA, ref_high = NA, direction = "neutral", within_range_policy = "neutral")
  
  ref_low <- cfg$ref_low
  ref_high <- cfg$ref_high
  direction <- cfg$direction
  within_range_policy <- if (is.null(cfg$within_range_policy)) "directional" else cfg$within_range_policy
  
  # EARLY EXIT: Neutral-policy labs always return neutral
  if (within_range_policy == "neutral") return("neutral")
  
  # Check if values are clinically equivalent (within epsilon threshold)
  if (abs(v3 - v1) <= epsilon) return("same")
  
  # CASE 1: Double-sided reference range (e.g., C3: 80-160, Albumin: 3.5-5.5, WBC: 4-11)
  if (!is.na(ref_low) && !is.na(ref_high)) {
    # Calculate distance from normal range (0 if within range)
    distance <- function(x) {
      if (x < ref_low) return(ref_low - x)
      if (x > ref_high) return(x - ref_high)
      0
    }
    d1 <- distance(v1)
    d3 <- distance(v3)
    
    # Moving from abnormal to normal = BETTER (highest priority)
    if (d1 > 0 && d3 == 0) return("better")
    
    # Moving from normal to abnormal = WORSE (highest priority)
    if (d1 == 0 && d3 > 0) return("worse")
    
    # Both abnormal: closer to normal = better, further = worse
    if (d1 > 0 && d3 > 0) {
      if (d3 < d1) return("better")
      return("worse")
    }
    
    # Both in normal range: apply within_range_policy
    if (d1 == 0 && d3 == 0) {
      # Stable policy: within range = clinically stable (grey)
      if (within_range_policy == "stable") return("same")
      
      # Directional policy: assess trend direction even within range
      if (within_range_policy == "directional") {
        if (direction == "lower_better") return(if (v3 < v1) "better" else "worse")
        if (direction == "higher_better") return(if (v3 > v1) "better" else "worse")
      }
      
      return("same")
    }
  }
  
  # CASE 2: Single-sided reference range (e.g., eGFR >60, UPCR <0.5, ESR <20, ANC >1.5)
  if (!is.na(ref_low) || !is.na(ref_high)) {
    in_range1 <- (!is.na(ref_low) && v1 >= ref_low || is.na(ref_low)) && 
                 (!is.na(ref_high) && v1 <= ref_high || is.na(ref_high))
    in_range3 <- (!is.na(ref_low) && v3 >= ref_low || is.na(ref_low)) && 
                 (!is.na(ref_high) && v3 <= ref_high || is.na(ref_high))
    
    # Movement into normal range = BETTER
    if (!in_range1 && in_range3) return("better")
    
    # Movement out of normal range = WORSE
    if (in_range1 && !in_range3) return("worse")
    
    # Both in range: apply within_range_policy
    if (in_range1 && in_range3) {
      if (within_range_policy == "stable") return("same")
      
      if (within_range_policy == "directional") {
        if (direction == "lower_better") return(if (v3 < v1) "better" else "worse")
        if (direction == "higher_better") return(if (v3 > v1) "better" else "worse")
      }
      
      return("same")
    }
    
    # Both out of range: use direction rule to assess if moving toward normal
    if (!in_range1 && !in_range3) {
      if (direction == "lower_better") return(if (v3 < v1) "better" else "worse")
      if (direction == "higher_better") return(if (v3 > v1) "better" else "worse")
    }
  }
  
  # CASE 3: No reference range - use direction-based clinical rules
  if (direction != "neutral") {
    if (direction == "lower_better") return(if (v3 < v1) "better" else "worse")
    if (direction == "higher_better") return(if (v3 > v1) "better" else "worse")
  }
  
  # CASE 4: Neutral direction and no range - cannot determine better/worse
  "neutral"
}

trend_color <- function(status) {
  # Clinician-standard colors per Abhi: blue = better, red = worse, grey = stable/unknown
  if (status == "better") return("#4a9eff")  # blue
  if (status == "worse") return("#ff6b6b")   # red
  if (status == "same") return("#8a8f99")    # grey
  "#8a8f99"  # neutral = grey
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
  # No database connection needed - using embedded mock data!

  domain_name_map <- c(
    "Arthritis" = "Joint",
    "Pulm" = "Pulm"
  )
  domain_order <- c("Skin", "Joint", "Renal", "Cardiac", "Pulm", "Gastrointestinal", "Neuro", "Oral Ulcers")

  # Get patients from mock data
  patients <- get_mock_patients()

  patient_choices <- setNames(patients$external_id, patients$display_name)
  updateSelectInput(
    session,
    "patient",
    choices = patient_choices,
    selected = if (length(patients$external_id) > 0) patients$external_id[1] else NULL
  )

  patient_row <- reactive({
    req(input$patient)
    get_mock_patient_row(input$patient)
  })

  visits_df <- reactive({
    req(input$patient)
    get_mock_visits(input$patient)
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
    all_labs <- get_mock_all_labs(input$patient)
    if (nrow(all_labs) == 0) return(data.frame())
    all_labs[all_labs$lab_name %in% key_labs, , drop = FALSE]
  })

  domains_current_df <- reactive({
    req(input$patient)
    get_mock_domains_current(input$patient)
  })

  domains_ever_df <- reactive({
    req(input$patient)
    get_mock_domains_ever(input$patient)
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
    v_all <- visits_df()
    if (nrow(v_all) == 0) {
      return(tags$div(class = "sle-muted", "No visits found."))
    }

    labs <- labs_df()

    trend_visits <- v_all
    if (all(is.na(trend_visits$visit_date))) {
      trend_visits <- trend_visits[order(trend_visits$visit_id, decreasing = TRUE), , drop = FALSE]
      trend_visits <- trend_visits[seq_len(min(3, nrow(trend_visits))), , drop = FALSE]
      trend_visits <- trend_visits[order(trend_visits$visit_id), , drop = FALSE]
    } else {
      trend_visits <- trend_visits[order(trend_visits$visit_date, decreasing = TRUE), , drop = FALSE]
      trend_visits <- trend_visits[seq_len(min(3, nrow(trend_visits))), , drop = FALSE]
      trend_visits <- trend_visits[order(trend_visits$visit_date), , drop = FALSE]
    }

    biopsy_window_types <- c("3 Months Before Biopsy", "Biopsy", "3 Months After Biopsy")
    only_biopsy_window <- all(trend_visits$visit_type %in% biopsy_window_types)

    trend_cols <- lapply(seq_len(nrow(trend_visits)), function(i) {
      visit_date <- trend_visits$visit_date[i]
      hdr <- if (!is.na(visit_date)) format_date_short(visit_date) else paste0("Visit ", i)
      list(
        visit_id = trend_visits$visit_id[i],
        type = trend_visits$visit_type[i],
        hdr = hdr
      )
    })

    biopsy_type <- "Biopsy"
    window_months <- as.integer(input$window_months)
    
    # Map biopsy window to timepoint labels - PT4 has Month 0-3, 3-6, 6-9, 9-12, 12-15, 15-18
    # Biopsy is always Month 6-9 (labeled "Biopsy")
    # 3M: before=Month 3-6 ("3 Months Before Biopsy"), after=Month 9-12 ("3 Months After Biopsy")
    # 6M: before=Month 0-3, after=Month 12-15
    # 12M: before=N/A (no data), after=Month 15-18
    if (window_months == 3) {
      window_before <- "3 Months Before Biopsy"
      window_after <- "3 Months After Biopsy"
      window_label <- "3M"
    } else if (window_months == 6) {
      window_before <- "Month 0-3"
      window_after <- "Month 12-15"
      window_label <- "6M"
    } else if (window_months == 12) {
      window_before <- NA_character_  # No -6 to -3 data for PT4
      window_after <- "Month 15-18"
      window_label <- "1Y"
    } else {
      window_before <- NA_character_
      window_after <- NA_character_
      window_label <- "18M"
    }

    find_lab_value <- function(lab_name, visit_type) {
      if (is.na(visit_type) || nchar(visit_type) == 0) return(NA_real_)
      r <- labs[labs$lab_name == lab_name & labs$visit_type == visit_type, , drop = FALSE]
      if (nrow(r) == 0) return(NA_real_)
      norm <- normalize_lab_for_display(lab_name, r$lab_unit[1], r$lab_value[1])
      norm$value
    }

    row_ui <- lapply(key_labs, function(lab_name) {
      lab_rows <- labs[labs$lab_name == lab_name, , drop = FALSE]

      trend_values <- vapply(trend_cols, function(vc) {
        r <- lab_rows[lab_rows$visit_id == vc$visit_id, , drop = FALSE]
        if (nrow(r) == 0) return(NA_real_)
        norm <- normalize_lab_for_display(lab_name, r$lab_unit[1], r$lab_value[1])
        norm$value
      }, numeric(1))

      # For sparkline, use oldest -> newest so trend reads left->right
      values_oldest <- trend_values
      
      # For Recent Visits display, blank out if only biopsy window data
      display_trend_values <- if (only_biopsy_window) rep(NA_real_, length(trend_values)) else trend_values

      unit <- ""
      if (nrow(lab_rows) > 0) {
        unit <- lab_rows$lab_unit[1]
        unit <- normalize_lab_for_display(lab_name, unit, lab_rows$lab_value[1])$unit
        if (is.na(unit)) unit <- ""
      }

      label <- if (!is.na(lab_display[[lab_name]])) lab_display[[lab_name]] else lab_name
      lab_label <- if (nchar(unit) > 0) paste0(label, " (", unit, ")") else label

      vals_ui <- lapply(seq_along(trend_cols), function(i) {
        val <- display_trend_values[i]
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

      # Use new lab-specific trend logic
      status <- trend_status(lab_name, values_oldest[1], values_oldest[length(values_oldest)], epsilon = 0.05)
      stroke <- trend_color(status)

      biopsy_val <- find_lab_value(lab_name, biopsy_type)
      before_val <- if (!is.na(window_before)) find_lab_value(lab_name, window_before) else NA_real_
      after_val <- if (!is.na(window_after)) find_lab_value(lab_name, window_after) else NA_real_

      render_window_val <- function(val) {
        if (is.na(val)) return("—")
        format_lab_value(lab_name, unit, val)
      }

      render_optional_dup <- function(val, compare_val) {
        if (is.na(val)) return("—")
        if (!is.na(compare_val) && isTRUE(all.equal(val, compare_val))) return("↔")
        format_lab_value(lab_name, unit, val)
      }

      tags$tr(
        tags$td(class = "sle-lab-name", lab_label),
        tags$td(class = "sle-lab-spark", sparkline_svg(values_oldest, stroke = stroke)),
        lapply(vals_ui, function(x) tags$td(class = "sle-lab-col", x)),
        tags$td(class = "sle-lab-col", render_optional_dup(biopsy_val, display_trend_values[2])),
        tags$td(class = "sle-lab-col", render_optional_dup(before_val, display_trend_values[1])),
        tags$td(class = "sle-lab-col", render_optional_dup(after_val, display_trend_values[length(display_trend_values)]))
      )
    })

    header_dates <- lapply(seq_along(trend_cols), function(i) {
      vc <- trend_cols[[i]]
      tags$th(
        class = "sle-lab-col",
        tags$div(class = "sle-colhdr", vc$hdr)
      )
    })

    tags$table(
      class = "sle-table",
      tags$thead(
        tags$tr(
          tags$th(class = "sle-lab-name", ""),
          tags$th(class = "sle-lab-spark", ""),
          tags$th(class = "sle-lab-col", colspan = length(trend_cols), tags$div(class = "sle-colhdr", "RECENT VISITS (LAST 3)")),
          tags$th(class = "sle-lab-col", colspan = 3, tags$div(class = "sle-colhdr", paste0("BIOPSY WINDOW (", window_label, ")")))
        ),
        tags$tr(
          tags$th(class = "sle-lab-name", tags$div(class = "sle-colhdr", "Lab")),
          tags$th(class = "sle-lab-spark", tags$div(class = "sle-colhdr", "Trend")),
          header_dates,
          tags$th(class = "sle-lab-col", tags$div(class = "sle-colhdr", "At Biopsy")),
          tags$th(class = "sle-lab-col", tags$div(class = "sle-colhdr", paste0(window_label, " Before"))),
          tags$th(class = "sle-lab-col", tags$div(class = "sle-colhdr", paste0(window_label, " After")))
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
    req(input$patient)
    window_months <- as.integer(input$window_months)
    
    # Map biopsy window to timepoint labels - same as lab table
    if (window_months == 3) {
      window_before <- "3 Months Before Biopsy"
      window_after <- "3 Months After Biopsy"
      window_label <- "3M"
    } else if (window_months == 6) {
      window_before <- "Month 0-3"
      window_after <- "Month 12-15"
      window_label <- "6M"
    } else if (window_months == 12) {
      window_before <- NA_character_
      window_after <- "Month 15-18"
      window_label <- "1Y"
    } else {
      window_before <- NA_character_
      window_after <- NA_character_
      window_label <- "18M"
    }

    df_biopsy <- get_mock_steroid_exposure(input$patient, "Biopsy")
    df_before <- if (!is.na(window_before)) get_mock_steroid_exposure(input$patient, window_before) else data.frame()
    df_after <- if (!is.na(window_after)) get_mock_steroid_exposure(input$patient, window_after) else data.frame()

    if (nrow(df_biopsy) == 0) {
      return(tags$div(class = "sle-muted", "Steroid data not available for this patient."))
    }

    na_or_value <- function(x) {
      if (is.na(x) || nchar(as.character(x)) == 0) return("N/A (not provided yet)")
      as.character(x)
    }

    cell_val <- function(df, field) {
      if (nrow(df) == 0) return("—")
      val <- df[[field]][1]
      if (is.na(val) || nchar(as.character(val)) == 0) return("N/A (not provided yet)")
      as.character(val)
    }

    rows <- list(
      list(label = "Alert: >5mg Pred >8 weeks", field = "alert_5mg_8w"),
      list(label = "Alert: >6mg Pred >4 weeks + no decrease", field = "alert_6mg_4w_no_decrease"),
      list(label = "Total mg prednisone equivalents", field = "total_mg_pred_equiv"),
      list(label = "Avg mg/day", field = "avg_mg_pred_per_day"),
      list(label = "1y cumulative", field = "cumulative_1y_mg"),
      list(label = "5y cumulative", field = "cumulative_5y_mg"),
      list(label = "Last IV steroid note", field = "last_iv_steroid_note"),
      list(label = "SLICC damage note", field = "slicc_damage_note")
    )

    tags$table(
      class = "sle-table",
      tags$thead(
        tags$tr(
          tags$th(class = "sle-lab-name", ""),
          tags$th(class = "sle-lab-col", tags$div(class = "sle-colhdr", "At Biopsy")),
          tags$th(class = "sle-lab-col", tags$div(class = "sle-colhdr", paste0(window_label, " Before"))),
          tags$th(class = "sle-lab-col", tags$div(class = "sle-colhdr", paste0(window_label, " After")))
        )
      ),
      tags$tbody(
        lapply(rows, function(r) {
          tags$tr(
            tags$td(class = "sle-lab-name", r$label),
            tags$td(class = "sle-lab-col", cell_val(df_biopsy, r$field)),
            tags$td(class = "sle-lab-col", cell_val(df_before, r$field)),
            tags$td(class = "sle-lab-col", cell_val(df_after, r$field))
          )
        })
      )
    )
  })

  output$biomeds_tracker <- renderUI({
    req(input$patient)
    df <- get_mock_biomeds(input$patient)
    if (nrow(df) == 0) {
      return(tags$div(class = "sle-muted", "Not provided in workbook yet."))
    }
    tags$table(
      class = "sle-table",
      tags$thead(
        tags$tr(
          tags$th(class = "sle-lab-name", tags$div(class = "sle-colhdr", "Medication")),
          tags$th(class = "sle-lab-col", tags$div(class = "sle-colhdr", "Class")),
          tags$th(class = "sle-lab-col", tags$div(class = "sle-colhdr", "Start")),
          tags$th(class = "sle-lab-col", tags$div(class = "sle-colhdr", "Stop")),
          tags$th(class = "sle-lab-col", tags$div(class = "sle-colhdr", "Status")),
          tags$th(class = "sle-lab-col", tags$div(class = "sle-colhdr", "Notes"))
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            tags$td(class = "sle-lab-name", df$med_name[i]),
            tags$td(class = "sle-lab-col", df$class[i]),
            tags$td(class = "sle-lab-col", format_date_short(df$start_date[i])),
            tags$td(class = "sle-lab-col", format_date_short(df$stop_date[i])),
            tags$td(class = "sle-lab-col", df$status[i]),
            tags$td(class = "sle-lab-col", df$notes[i])
          )
        })
      )
    )
  })

  output$notes_text <- renderUI({
    req(input$patient)
    has_steroids <- nrow(get_mock_steroid_exposure(input$patient)) > 0
    if (has_steroids) {
      tags$div(
        class = "sle-muted",
        "Medication and provider details are not present in the current mock workbook. Steroid exposure fields are included; 1y/5y cumulative dose fields are not provided yet in mock data."
      )
    } else {
      tags$div(
        class = "sle-muted",
        "Medication and provider details are not present in the current mock workbook. Steroid exposure details are not present for this patient."
      )
    }
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
    
    labs <- get_mock_labs(input$patient, biopsy_visit$visit_id[1], renal_labs)
    
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
      "Pathology notes and recommendations will appear here when biopsy reports are integrated."
    )
  })

  render_med_list <- function(df, empty_msg) {
    if (nrow(df) == 0) {
      return(tags$div(class = "sle-muted", empty_msg))
    }
    tags$ul(
      class = "sle-med-list",
      lapply(seq_len(nrow(df)), function(i) {
        tags$li(
          tags$strong(df$medication_name[i]),
          if (!is.na(df$dose[i])) paste0(" - ", df$dose[i]) else ""
        )
      })
    )
  }

  output$meds_current_sle <- renderUI({
    render_med_list(data.frame(), "Medication data not present in the workbook.")
  })

  output$meds_current_bp <- renderUI({
    render_med_list(data.frame(), "Medication data not present in the workbook.")
  })

  output$meds_current_cardio <- renderUI({
    render_med_list(data.frame(), "Medication data not present in the workbook.")
  })

  output$meds_prior <- renderUI({
    render_med_list(data.frame(), "Medication data not present in the workbook.")
  })

  observeEvent(input$open_additional_labs, {
    req(input$patient)
    all_labs <- get_mock_all_labs(input$patient)
    lab_names <- unique(all_labs$lab_name)
    lab_names <- lab_names[order(lab_names)]

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
            selectInput("additional_lab", "Select Lab Parameter", choices = lab_names, selected = if (length(lab_names) > 0) lab_names[1] else NULL, width = "100%")
          ),
          plotOutput("additional_lab_plot", height = "400px")
        )
      )
    )
  })

  output$additional_lab_plot <- renderPlot({
    req(input$patient)
    req(input$additional_lab)
    
    all_labs <- get_mock_all_labs(input$patient)
    df <- all_labs[all_labs$lab_name == input$additional_lab, ]
    
    if (nrow(df) == 0) return(NULL)

    df$rank <- vapply(df$visit_type, timepoint_rank, numeric(1))
    df <- df[order(df$rank), , drop = FALSE]

    y <- as_num(df$lab_value)
    lab_unit <- if (nrow(df) > 0) df$lab_unit[1] else NA_character_
    
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
      y_normalized,
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
    axis(1, at = x, labels = vapply(df$visit_type, timepoint_short_label, character(1)), 
         col.axis = "#e0e0e0", col = "#606060", cex.axis = 1.1, lwd = 0, lwd.ticks = 1)
    grid(col = "#404040", lwd = 1, lty = 2)
  })
}
