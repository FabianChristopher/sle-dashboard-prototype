# Mock data embedded in the app - no database required
# This file contains all patient, visit, lab, and domain data as R data frames

# ============================================================================
# PATIENTS DATA
# ============================================================================
MOCK_PATIENTS <- data.frame(
  patient_id = 1:4,
  external_id = c("PT1", "PT2", "PT3", "PT4"),
  first_name = c(NA_character_, NA_character_, NA_character_, NA_character_),
  last_name = c(NA_character_, NA_character_, NA_character_, NA_character_),
  sex = c(NA_character_, NA_character_, NA_character_, NA_character_),
  diagnosis_date = as.Date(c(NA, NA, NA, NA)),
  stringsAsFactors = FALSE
)

# ============================================================================
# VISITS DATA
# ============================================================================
base_date <- as.Date("2024-01-01")
timepoints <- c("3 Months Before Biopsy", "Biopsy", "3 Months After Biopsy")
offsets <- c(-90, 0, 90)

# PT1-PT3: 3 visits each (biopsy window only)
# PT4: 6 visits (Month 0-3, biopsy window, and 3 post-biopsy visits)
MOCK_VISITS <- data.frame(
  visit_id = 1:15,
  patient_id = c(
    rep(1, 3), rep(2, 3), rep(3, 3),  # PT1-PT3: 3 visits each
    rep(4, 6)  # PT4: 6 visits
  ),
  visit_date = c(
    # PT1-PT3: standard 3-visit pattern
    rep(base_date + offsets, 3),
    # PT4: 6 visits with placeholder dates
    as.Date("2023-01-01"),  # Month 0-3
    as.Date("2023-04-01"),  # 3M Before Biopsy
    as.Date("2023-07-01"),  # Biopsy
    as.Date("2023-10-01"),  # 3M After Biopsy
    as.Date("2024-01-01"),  # Month 12-15
    as.Date("2024-04-01")   # Month 15-18
  ),
  visit_type = c(
    # PT1-PT3: standard timepoints
    rep(timepoints, 3),
    # PT4: 6 visit types
    "Month 0-3",
    "3 Months Before Biopsy",
    "Biopsy",
    "3 Months After Biopsy",
    "Month 12-15",
    "Month 15-18"
  ),
  display_label = c(
    # PT1-PT3: standard labels
    rep(timepoints, 3),
    # PT4: 6 labels
    "Month 0-3",
    "3 Months Before Biopsy",
    "Biopsy",
    "3 Months After Biopsy",
    "Month 12-15",
    "Month 15-18"
  ),
  notes = NA_character_,
  stringsAsFactors = FALSE
)

# ============================================================================
# LABS DATA
# ============================================================================
# Helper to build lab rows
build_labs <- function() {
  lab_data <- list(
    PT1 = list(
      "3 Months Before Biopsy" = list(
        "Platelets" = list(value = 288000, unit = "K/µL"),
        "WBC" = list(value = 6400, unit = "K/µL"),
        "ALC" = list(value = 1300, unit = "cells/µL"),
        "IgA" = list(value = 362, unit = "mg/dL"),
        "ANC units" = list(value = 4400, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = 4, unit = NA),
        "ESR" = list(value = 26, unit = "mm/hr"),
        "CRP" = list(value = 0.5, unit = "mg/dl"),
        "C3 complement" = list(value = 70.8, unit = "mg/dL"),
        "C4 complement" = list(value = 14.7, unit = "mg/dL"),
        "Urine Protein" = list(value = 36, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 38, unit = "mg/dL"),
        "UPCR" = list(value = 0.95, unit = NA),
        "Albumin" = list(value = 5.2, unit = NA),
        "eGFR" = list(value = 132, unit = NA)
      ),
      "Biopsy" = list(
        "Platelets" = list(value = 232000, unit = "K/µL"),
        "WBC" = list(value = 6900, unit = "K/µL"),
        "ALC" = list(value = 1300, unit = "cells/µL"),
        "IgA" = list(value = 340, unit = "mg/dL"),
        "ANC units" = list(value = 4700, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = 2, unit = NA),
        "ESR" = list(value = 20, unit = "mm/hr"),
        "CRP" = list(value = 0.5, unit = "mg/dl"),
        "C3 complement" = list(value = 84.1, unit = "mg/dL"),
        "C4 complement" = list(value = 33.5, unit = "mg/dL"),
        "Urine Protein" = list(value = 28, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 40, unit = "mg/dL"),
        "UPCR" = list(value = 0.7, unit = NA),
        "Albumin" = list(value = 4.9, unit = NA),
        "eGFR" = list(value = 143, unit = NA)
      ),
      "3 Months After Biopsy" = list(
        "Platelets" = list(value = 256000, unit = "K/µL"),
        "WBC" = list(value = 6600, unit = "K/µL"),
        "ALC" = list(value = 1000, unit = "cells/µL"),
        "IgA" = list(value = 350, unit = "mg/dL"),
        "ANC units" = list(value = 4900, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = 4, unit = NA),
        "ESR" = list(value = 16, unit = "mm/hr"),
        "CRP" = list(value = 0.5, unit = "mg/dl"),
        "C3 complement" = list(value = 84.7, unit = "mg/dL"),
        "C4 complement" = list(value = 28.6, unit = "mg/dL"),
        "Urine Protein" = list(value = 29, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 43, unit = "mg/dL"),
        "UPCR" = list(value = 0.67, unit = NA),
        "Albumin" = list(value = 4.6, unit = NA),
        "eGFR" = list(value = 140, unit = NA)
      )
    ),
    PT2 = list(
      "3 Months Before Biopsy" = list(
        "Platelets" = list(value = 288000, unit = "K/µL"),
        "WBC" = list(value = 6400, unit = "K/µL"),
        "ALC" = list(value = 1300, unit = "cells/µL"),
        "IgA" = list(value = 362, unit = "mg/dL"),
        "ANC units" = list(value = 4400, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = 4, unit = NA),
        "ESR" = list(value = 26, unit = "mm/hr"),
        "CRP" = list(value = 0.5, unit = "mg/dl"),
        "C3 complement" = list(value = 70.8, unit = "mg/dL"),
        "C4 complement" = list(value = 12.4, unit = "mg/dL"),
        "Urine Protein" = list(value = 102, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 49, unit = "mg/dL"),
        "UPCR" = list(value = 2.1, unit = NA),
        "Albumin" = list(value = 3.3, unit = NA),
        "eGFR" = list(value = 80, unit = NA)
      ),
      "Biopsy" = list(
        "Platelets" = list(value = 200000, unit = "K/µL"),
        "WBC" = list(value = 6000, unit = "K/µL"),
        "ALC" = list(value = 1000, unit = "cells/µL"),
        "IgA" = list(value = 413, unit = "mg/dL"),
        "ANC units" = list(value = 3900, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = 4, unit = NA),
        "ESR" = list(value = 42, unit = "mm/hr"),
        "CRP" = list(value = 1, unit = "mg/dl"),
        "C3 complement" = list(value = 64.3, unit = "mg/dL"),
        "C4 complement" = list(value = 10.1, unit = "mg/dL"),
        "Urine Protein" = list(value = 110, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 50, unit = "mg/dL"),
        "UPCR" = list(value = 2.2, unit = NA),
        "Albumin" = list(value = 3.1, unit = NA),
        "eGFR" = list(value = 74, unit = NA)
      ),
      "3 Months After Biopsy" = list(
        "Platelets" = list(value = 180000, unit = "K/µL"),
        "WBC" = list(value = 5200, unit = "K/µL"),
        "ALC" = list(value = 800, unit = "cells/µL"),
        "IgA" = list(value = 500, unit = "mg/dL"),
        "ANC units" = list(value = 3400, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = 4, unit = NA),
        "ESR" = list(value = 54, unit = "mm/hr"),
        "CRP" = list(value = 1, unit = "mg/dl"),
        "C3 complement" = list(value = 59.2, unit = "mg/dL"),
        "C4 complement" = list(value = 11.2, unit = "mg/dL"),
        "Urine Protein" = list(value = 120, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 61, unit = "mg/dL"),
        "UPCR" = list(value = 2, unit = NA),
        "Albumin" = list(value = 3.5, unit = NA),
        "eGFR" = list(value = 78, unit = NA)
      )
    ),
    PT3 = list(
      "3 Months Before Biopsy" = list(
        "Platelets" = list(value = 288000, unit = "K/µL"),
        "WBC" = list(value = 6400, unit = "K/µL"),
        "ALC" = list(value = 1300, unit = "cells/µL"),
        "IgA" = list(value = 223, unit = "mg/dL"),
        "ANC units" = list(value = 4400, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = 4, unit = NA),
        "ESR" = list(value = 14, unit = "mm/hr"),
        "CRP" = list(value = 0.5, unit = "mg/dl"),
        "C3 complement" = list(value = 84.3, unit = "mg/dL"),
        "C4 complement" = list(value = 18.3, unit = "mg/dL"),
        "Urine Protein" = list(value = 30, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 48, unit = "mg/dL"),
        "UPCR" = list(value = 0.63, unit = NA),
        "Albumin" = list(value = 4.8, unit = NA),
        "eGFR" = list(value = 132, unit = NA)
      ),
      "Biopsy" = list(
        "Platelets" = list(value = 300000, unit = "K/µL"),
        "WBC" = list(value = 6900, unit = "K/µL"),
        "ALC" = list(value = 1500, unit = "cells/µL"),
        "IgA" = list(value = 246, unit = "mg/dL"),
        "ANC units" = list(value = 4700, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = 2, unit = NA),
        "ESR" = list(value = 20, unit = "mm/hr"),
        "CRP" = list(value = 0.5, unit = "mg/dl"),
        "C3 complement" = list(value = 88.9, unit = "mg/dL"),
        "C4 complement" = list(value = 24.7, unit = "mg/dL"),
        "Urine Protein" = list(value = 24, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 40, unit = "mg/dL"),
        "UPCR" = list(value = 0.57, unit = NA),
        "Albumin" = list(value = 4.9, unit = NA),
        "eGFR" = list(value = 143, unit = NA)
      ),
      "3 Months After Biopsy" = list(
        "Platelets" = list(value = 356000, unit = "K/µL"),
        "WBC" = list(value = 7000, unit = "K/µL"),
        "ALC" = list(value = 1900, unit = "cells/µL"),
        "IgA" = list(value = 287, unit = "mg/dL"),
        "ANC units" = list(value = 4900, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = 0, unit = NA),
        "ESR" = list(value = 16, unit = "mm/hr"),
        "CRP" = list(value = 0.5, unit = "mg/dl"),
        "C3 complement" = list(value = 86.7, unit = "mg/dL"),
        "C4 complement" = list(value = 28.6, unit = "mg/dL"),
        "Urine Protein" = list(value = 21, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 43, unit = "mg/dL"),
        "UPCR" = list(value = 0.49, unit = NA),
        "Albumin" = list(value = 5, unit = NA),
        "eGFR" = list(value = 140, unit = NA)
      )
    ),
    PT4 = list(
      "Month 0-3" = list(
        "Platelets" = list(value = 256000, unit = "K/µL"),
        "WBC" = list(value = 6600, unit = "K/µL"),
        "ALC" = list(value = 1000, unit = "cells/µL"),
        "IgA" = list(value = 350, unit = "mg/dL"),
        "ANC units" = list(value = 4900, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = "4+", unit = NA),
        "ESR" = list(value = 16, unit = "mm/hr"),
        "CRP" = list(value = 0.5, unit = "mg/dl"),
        "C3 complement" = list(value = 84.7, unit = "mg/dL"),
        "C4 complement" = list(value = 11.7, unit = "mg/dL"),
        "Urine Protein" = list(value = 165, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 50, unit = "mg/dL"),
        "UPCR" = list(value = 3.24, unit = NA),
        "Albumin" = list(value = 4.5, unit = NA),
        "eGFR" = list(value = 113, unit = NA)
      ),
      "3 Months Before Biopsy" = list(
        "Platelets" = list(value = 288000, unit = "K/µL"),
        "WBC" = list(value = 6400, unit = "K/µL"),
        "ALC" = list(value = 1300, unit = "cells/µL"),
        "IgA" = list(value = 362, unit = "mg/dL"),
        "ANC units" = list(value = 4400, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = "4+", unit = NA),
        "ESR" = list(value = 26, unit = "mm/hr"),
        "CRP" = list(value = 0.5, unit = "mg/dl"),
        "C3 complement" = list(value = 70.8, unit = "mg/dL"),
        "C4 complement" = list(value = 11.9, unit = "mg/dL"),
        "Urine Protein" = list(value = 110, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 52.3, unit = "mg/dL"),
        "UPCR" = list(value = 2.10, unit = NA),
        "Albumin" = list(value = 4.4, unit = NA),
        "eGFR" = list(value = 123, unit = NA)
      ),
      "Biopsy" = list(
        "Platelets" = list(value = 200000, unit = "K/µL"),
        "WBC" = list(value = 6000, unit = "K/µL"),
        "ALC" = list(value = 1000, unit = "cells/µL"),
        "IgA" = list(value = 413, unit = "mg/dL"),
        "ANC units" = list(value = 3900, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = "4+", unit = NA),
        "ESR" = list(value = 42, unit = "mm/hr"),
        "CRP" = list(value = 1, unit = "mg/dl"),
        "C3 complement" = list(value = 64.3, unit = "mg/dL"),
        "C4 complement" = list(value = 10.1, unit = "mg/dL"),
        "Urine Protein" = list(value = 110, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 50, unit = "mg/dL"),
        "UPCR" = list(value = 2.20, unit = NA),
        "Albumin" = list(value = 4.2, unit = NA),
        "eGFR" = list(value = 115, unit = NA)
      ),
      "3 Months After Biopsy" = list(
        "Platelets" = list(value = 288000, unit = "K/µL"),
        "WBC" = list(value = 6400, unit = "K/µL"),
        "ALC" = list(value = 1300, unit = "cells/µL"),
        "IgA" = list(value = 223, unit = "mg/dL"),
        "ANC units" = list(value = 4400, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = "4+", unit = NA),
        "ESR" = list(value = 14, unit = "mm/hr"),
        "CRP" = list(value = 0.5, unit = "mg/dl"),
        "C3 complement" = list(value = 84.3, unit = "mg/dL"),
        "C4 complement" = list(value = 18.3, unit = "mg/dL"),
        "Urine Protein" = list(value = 110, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 174.3, unit = "mg/dL"),
        "UPCR" = list(value = 0.63, unit = NA),
        "Albumin" = list(value = 4.5, unit = NA),
        "eGFR" = list(value = 115, unit = NA)
      ),
      "Month 12-15" = list(
        "Platelets" = list(value = 300000, unit = "K/µL"),
        "WBC" = list(value = 6900, unit = "K/µL"),
        "ALC" = list(value = 1500, unit = "cells/µL"),
        "IgA" = list(value = 246, unit = "mg/dL"),
        "ANC units" = list(value = 4700, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = "2+", unit = NA),
        "ESR" = list(value = 20, unit = "mm/hr"),
        "CRP" = list(value = 0.5, unit = "mg/dl"),
        "C3 complement" = list(value = 88.9, unit = "mg/dL"),
        "C4 complement" = list(value = 24.7, unit = "mg/dL"),
        "Urine Protein" = list(value = 98, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 172.2, unit = "mg/dL"),
        "UPCR" = list(value = 0.57, unit = NA),
        "Albumin" = list(value = 4.3, unit = NA),
        "eGFR" = list(value = 143, unit = NA)
      ),
      "Month 15-18" = list(
        "Platelets" = list(value = 356000, unit = "K/µL"),
        "WBC" = list(value = 7000, unit = "K/µL"),
        "ALC" = list(value = 1900, unit = "cells/µL"),
        "IgA" = list(value = 287, unit = "mg/dL"),
        "ANC units" = list(value = 4900, unit = "cells/µL"),
        "Anti ds-DNA" = list(value = "0", unit = NA),
        "ESR" = list(value = 16, unit = "mm/hr"),
        "CRP" = list(value = 0.5, unit = "mg/dl"),
        "C3 complement" = list(value = 86.7, unit = "mg/dL"),
        "C4 complement" = list(value = 28.6, unit = "mg/dL"),
        "Urine Protein" = list(value = 170, unit = "mg/dL"),
        "Urine Creatinine" = list(value = 343.1, unit = "mg/dL"),
        "UPCR" = list(value = 0.49, unit = NA),
        "Albumin" = list(value = 4.4, unit = NA),
        "eGFR" = list(value = 140, unit = NA)
      )
    )
  )
  
  pt_map <- c("PT1" = 1L, "PT2" = 2L, "PT3" = 3L, "PT4" = 4L)
  
  # PT4 has 6 timepoints, PT1-PT3 have 3
  pt4_timepoints <- c("Month 0-3", "3 Months Before Biopsy", "Biopsy", "3 Months After Biopsy", "Month 12-15", "Month 15-18")
  timepoints <- c("3 Months Before Biopsy", "Biopsy", "3 Months After Biopsy")
  base_date <- as.Date("2024-01-01")
  offsets <- c(-90, 0, 90)
  
  # PT4 visit dates
  pt4_dates <- c(
    as.Date("2023-01-01"),  # Month 0-3
    as.Date("2023-04-01"),  # 3M Before Biopsy
    as.Date("2023-07-01"),  # Biopsy
    as.Date("2023-10-01"),  # 3M After Biopsy
    as.Date("2024-01-01"),  # Month 12-15
    as.Date("2024-04-01")   # Month 15-18
  )
  
  rows <- list()
  lab_id <- 1
  for (ext_id in names(lab_data)) {
    pid <- pt_map[[ext_id]]
    
    # Determine timepoints for this patient
    if (ext_id == "PT4") {
      patient_timepoints <- pt4_timepoints
      visit_id_offset <- 9  # PT1-PT3 use visit_ids 1-9, PT4 starts at 10
    } else {
      patient_timepoints <- timepoints
      visit_id_offset <- (pid - 1) * 3
    }
    
    for (tp_idx in seq_along(patient_timepoints)) {
      tp <- patient_timepoints[tp_idx]
      
      # Calculate visit_id and date
      if (ext_id == "PT4") {
        vid <- visit_id_offset + tp_idx
        vdate <- pt4_dates[tp_idx]
      } else {
        vid <- visit_id_offset + tp_idx
        vdate <- base_date + offsets[tp_idx]
      }
      
      for (lab_name in names(lab_data[[ext_id]][[tp]])) {
        lab <- lab_data[[ext_id]][[tp]][[lab_name]]
        rows[[lab_id]] <- data.frame(
          lab_id = lab_id,
          patient_id = pid,
          visit_id = vid,
          collected_date = vdate,
          lab_name = lab_name,
          lab_value = lab$value,
          lab_unit = if (is.null(lab$unit) || is.na(lab$unit)) NA_character_ else lab$unit,
          reference_range_low = NA_real_,
          reference_range_high = NA_real_,
          abnormal_flag = NA,
          stringsAsFactors = FALSE
        )
        lab_id <- lab_id + 1
      }
    }
  }
  do.call(rbind, rows)
}

MOCK_LABS <- build_labs()

# ============================================================================
# DOMAINS DATA
# ============================================================================
build_domains <- function() {
  domain_data <- list(
    PT1 = list(
      "3 Months Before Biopsy" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = TRUE),
      "Biopsy" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = TRUE),
      "3 Months After Biopsy" = list(Arthritis = TRUE, Renal = FALSE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = TRUE)
    ),
    PT2 = list(
      "3 Months Before Biopsy" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = TRUE, Pulm = TRUE, Gastrointestinal = FALSE, Neuro = FALSE),
      "Biopsy" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = TRUE, Pulm = TRUE, Gastrointestinal = FALSE, Neuro = FALSE),
      "3 Months After Biopsy" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = TRUE, Pulm = TRUE, Gastrointestinal = FALSE, Neuro = FALSE)
    ),
    PT3 = list(
      "3 Months Before Biopsy" = list(Arthritis = FALSE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = TRUE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = FALSE),
      "Biopsy" = list(Arthritis = FALSE, Renal = FALSE, Skin = TRUE, `Oral Ulcers` = TRUE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = FALSE),
      "3 Months After Biopsy" = list(Arthritis = FALSE, Renal = FALSE, Skin = TRUE, `Oral Ulcers` = TRUE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = FALSE)
    ),
    PT4 = list(
      # Note: Domain values provided in workbook for all 6 timepoints
      "Month 0-3" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = TRUE),
      "3 Months Before Biopsy" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = TRUE),
      "Biopsy" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = TRUE),
      "3 Months After Biopsy" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = TRUE),
      "Month 12-15" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = TRUE),
      "Month 15-18" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = TRUE)
    )
  )
  
  pt_map <- c("PT1" = 1L, "PT2" = 2L, "PT3" = 3L, "PT4" = 4L)
  pt4_timepoints <- c("Month 0-3", "3 Months Before Biopsy", "Biopsy", "3 Months After Biopsy", "Month 12-15", "Month 15-18")
  timepoints <- c("3 Months Before Biopsy", "Biopsy", "3 Months After Biopsy")
  base_date <- as.Date("2024-01-01")
  offsets <- c(-90, 0, 90)
  
  # PT4 visit dates
  pt4_dates <- c(
    as.Date("2023-01-01"),  # Month 0-3
    as.Date("2023-04-01"),  # 3M Before Biopsy
    as.Date("2023-07-01"),  # Biopsy
    as.Date("2023-10-01"),  # 3M After Biopsy
    as.Date("2024-01-01"),  # Month 12-15
    as.Date("2024-04-01")   # Month 15-18
  )
  
  rows <- list()
  domain_id <- 1
  for (ext_id in names(domain_data)) {
    pid <- pt_map[[ext_id]]
    
    # Determine timepoints for this patient
    if (ext_id == "PT4") {
      patient_timepoints <- pt4_timepoints
      visit_id_offset <- 9  # PT1-PT3 use visit_ids 1-9, PT4 starts at 10
    } else {
      patient_timepoints <- timepoints
      visit_id_offset <- (pid - 1) * 3
    }
    
    for (tp_idx in seq_along(patient_timepoints)) {
      tp <- patient_timepoints[tp_idx]
      
      # Calculate visit_id and date
      if (ext_id == "PT4") {
        vid <- visit_id_offset + tp_idx
        vdate <- pt4_dates[tp_idx]
      } else {
        vid <- visit_id_offset + tp_idx
        vdate <- base_date + offsets[tp_idx]
      }
      
      for (domain_name in names(domain_data[[ext_id]][[tp]])) {
        active <- domain_data[[ext_id]][[tp]][[domain_name]]
        # Check if ever involved across all timepoints for this patient
        ever <- any(sapply(domain_data[[ext_id]], function(tp_data) tp_data[[domain_name]]))
        rows[[domain_id]] <- data.frame(
          domain_id = domain_id,
          patient_id = pid,
          visit_id = vid,
          assessed_date = vdate,
          domain_name = domain_name,
          domain_score = NA_real_,
          active = active,
          ever_involved = ever,
          stringsAsFactors = FALSE
        )
        domain_id <- domain_id + 1
      }
    }
  }
  do.call(rbind, rows)
}

MOCK_DOMAINS <- build_domains()

# ============================================================================
# MEDICATIONS DATA (empty for now, can be populated later)
# ============================================================================
MOCK_MEDICATIONS <- data.frame(
  medication_id = integer(0),
  patient_id = integer(0),
  medication_name = character(0),
  category = character(0),
  dose = character(0),
  route = character(0),
  frequency = character(0),
  start_date = as.Date(character(0)),
  end_date = as.Date(character(0)),
  current = logical(0),
  stop_reason = character(0),
  indication = character(0),
  stringsAsFactors = FALSE
)

# ==========================================================================
# BIOMEDS DATA (empty for now)
# ==========================================================================
MOCK_BIOMEDS <- data.frame(
  biomeds_id = integer(0),
  patient_id = integer(0),
  med_name = character(0),
  class = character(0),
  start_date = as.Date(character(0)),
  stop_date = as.Date(character(0)),
  status = character(0),
  notes = character(0),
  stringsAsFactors = FALSE
)

# ==========================================================================
# STEROID EXPOSURE DATA (v2 mock)
# ==========================================================================
MOCK_STEROID_EXPOSURE <- data.frame(
  patient_external_id = c(
    "PT1","PT1","PT1",
    "PT2","PT2","PT2",
    "PT3","PT3","PT3",
    "PT4","PT4","PT4"  # PT4: steroid data not provided in workbook
  ),
  timepoint = c(
    "3 Months Before Biopsy","Biopsy","3 Months After Biopsy",
    "3 Months Before Biopsy","Biopsy","3 Months After Biopsy",
    "3 Months Before Biopsy","Biopsy","3 Months After Biopsy",
    "3 Months Before Biopsy","Biopsy","3 Months After Biopsy"
  ),
  alert_5mg_8w = c(
    "Y","Y","N",
    "Y","Y","Y",
    "N","Y","N",
    NA, NA, NA  # PT4: not provided
  ),
  alert_6mg_4w_no_decrease = c(
    "Y","N","N",
    "Y","Y","Y",
    "N","N","N",
    NA, NA, NA  # PT4: not provided
  ),
  total_mg_pred_equiv = c(
    450, 900, 0,
    600, 750, 600,
    0, 900, 0,
    NA, NA, NA  # PT4: not provided
  ),
  avg_mg_pred_per_day = c(
    5, 10, 0,
    20, 25, 20,
    0, 10, 0,
    NA, NA, NA  # PT4: not provided
  ),
  cumulative_1y_mg = c(
    NA, NA, NA,
    NA, NA, NA,
    NA, NA, NA,
    NA, NA, NA  # PT4: not provided
  ),
  cumulative_5y_mg = c(
    NA, NA, NA,
    NA, NA, NA,
    NA, NA, NA,
    NA, NA, NA  # PT4: not provided
  ),
  last_iv_steroid_note = c(
    NA, "3 days post Bx", "3 days post Bx",
    "3 days prior to 3 month visit", "3 days post Bx", NA,
    NA, NA, NA,
    NA, NA, NA  # PT4: not provided
  ),
  slicc_damage_note = c(
    "Y-osteoporotic fractures", NA, NA,
    "Y-DM and osteoporotic fractures", NA, NA,
    NA, NA, NA,
    NA, NA, NA  # PT4: not provided
  ),
  stringsAsFactors = FALSE
)

# ============================================================================
# HELPER FUNCTIONS TO QUERY MOCK DATA
# ============================================================================

# Get all patients
get_mock_patients <- function() {
  df <- MOCK_PATIENTS
  df$display_name <- ifelse(
    is.na(df$first_name) & is.na(df$last_name),
    df$external_id,
    paste(df$first_name, df$last_name)
  )
  df[order(df$external_id), c("external_id", "display_name")]
}

# Get patient row by external_id
get_mock_patient_row <- function(external_id) {
  MOCK_PATIENTS[MOCK_PATIENTS$external_id == external_id, 
                c("external_id", "first_name", "last_name", "sex", "diagnosis_date")]
}

# Get visits for a patient by external_id
get_mock_visits <- function(external_id) {
  pid <- MOCK_PATIENTS$patient_id[MOCK_PATIENTS$external_id == external_id]
  if (length(pid) == 0) return(data.frame())
  v <- MOCK_VISITS[MOCK_VISITS$patient_id == pid, c("visit_id", "visit_date", "visit_type", "display_label")]
  v[order(v$visit_date, decreasing = TRUE), ]
}

# Get labs for patient by external_id and visit_ids
get_mock_labs <- function(external_id, visit_ids = NULL, lab_names = NULL) {
  pid <- MOCK_PATIENTS$patient_id[MOCK_PATIENTS$external_id == external_id]
  if (length(pid) == 0) return(data.frame())
  
  labs <- MOCK_LABS[MOCK_LABS$patient_id == pid, ]
  
  if (!is.null(visit_ids) && length(visit_ids) > 0) {
    labs <- labs[labs$visit_id %in% visit_ids, ]
  }
  
  if (!is.null(lab_names) && length(lab_names) > 0) {
    labs <- labs[labs$lab_name %in% lab_names, ]
  }
  
  labs[, c("visit_id", "collected_date", "lab_name", "lab_value", "lab_unit")]
}

# Get all labs for a patient (for trends)
get_mock_all_labs <- function(external_id) {
  pid <- MOCK_PATIENTS$patient_id[MOCK_PATIENTS$external_id == external_id]
  if (length(pid) == 0) return(data.frame())
  
  labs <- MOCK_LABS[MOCK_LABS$patient_id == pid, ]
  visits <- MOCK_VISITS[MOCK_VISITS$patient_id == pid, ]
  
  # Join to get visit_type
  merged <- merge(labs, visits[, c("visit_id", "visit_type", "visit_date", "display_label")], by = "visit_id")
  merged[, c("visit_id", "collected_date", "lab_name", "lab_value", "lab_unit", "visit_type", "visit_date", "display_label")]
}

# Get a single lab unit for display
get_mock_lab_unit <- function(external_id, lab_name) {
  pid <- MOCK_PATIENTS$patient_id[MOCK_PATIENTS$external_id == external_id]
  if (length(pid) == 0) return(data.frame())
  
  labs <- MOCK_LABS[MOCK_LABS$patient_id == pid & MOCK_LABS$lab_name == lab_name, ]
  if (nrow(labs) == 0) return(data.frame())
  labs[1, c("lab_unit")]
}

# Get current domains for a patient at latest visit
get_mock_domains_current <- function(external_id) {
  pid <- MOCK_PATIENTS$patient_id[MOCK_PATIENTS$external_id == external_id]
  if (length(pid) == 0) return(data.frame())
  
  visits <- get_mock_visits(external_id)
  if (nrow(visits) == 0) return(data.frame())
  
  latest_vid <- visits$visit_id[1]
  d <- MOCK_DOMAINS[MOCK_DOMAINS$patient_id == pid & MOCK_DOMAINS$visit_id == latest_vid, ]
  d[order(d$domain_name), c("domain_name", "active", "ever_involved")]
}

# Get ever-active domains for a patient
get_mock_domains_ever <- function(external_id) {
  pid <- MOCK_PATIENTS$patient_id[MOCK_PATIENTS$external_id == external_id]
  if (length(pid) == 0) return(data.frame())
  
  d <- MOCK_DOMAINS[MOCK_DOMAINS$patient_id == pid, ]
  
  # Aggregate by domain_name
  agg <- aggregate(
    cbind(active, ever_involved) ~ domain_name,
    data = d,
    FUN = function(x) any(x)
  )
  names(agg) <- c("domain_name", "ever_active", "ever_involved")
  agg[order(agg$domain_name), ]
}

# Get domains at specific visits
get_mock_domains_at_visits <- function(external_id, visit_ids) {
  pid <- MOCK_PATIENTS$patient_id[MOCK_PATIENTS$external_id == external_id]
  if (length(pid) == 0) return(data.frame())
  
  d <- MOCK_DOMAINS[MOCK_DOMAINS$patient_id == pid & MOCK_DOMAINS$visit_id %in% visit_ids, ]
  
  visits <- MOCK_VISITS[MOCK_VISITS$visit_id %in% visit_ids, c("visit_id", "visit_type")]
  
  merged <- merge(d, visits, by = "visit_id")
  merged[, c("domain_name", "active", "ever_involved", "visit_type")]
}

# Get biomeds tracker data for a patient
get_mock_biomeds <- function(external_id) {
  pid <- MOCK_PATIENTS$patient_id[MOCK_PATIENTS$external_id == external_id]
  if (length(pid) == 0) return(data.frame())
  MOCK_BIOMEDS[MOCK_BIOMEDS$patient_id == pid, ]
}

# Get steroid exposure data for a patient/timepoint
get_mock_steroid_exposure <- function(external_id, timepoint = NULL) {
  df <- MOCK_STEROID_EXPOSURE[MOCK_STEROID_EXPOSURE$patient_external_id == external_id, ]
  if (!is.null(timepoint)) {
    df <- df[df$timepoint == timepoint, ]
  }
  df
}

message("[MOCK DATA] Loaded embedded mock data - no database required")
