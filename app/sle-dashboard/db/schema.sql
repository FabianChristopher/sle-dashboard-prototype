-- Postgres schema for SLE dashboard data.

CREATE TABLE IF NOT EXISTS patients (
  patient_id BIGSERIAL PRIMARY KEY,
  external_id TEXT UNIQUE NOT NULL,
  first_name TEXT,
  last_name TEXT,
  date_of_birth DATE,
  sex TEXT,
  diagnosis_date DATE
);

CREATE TABLE IF NOT EXISTS visits (
  visit_id BIGSERIAL PRIMARY KEY,
  patient_id BIGINT NOT NULL REFERENCES patients(patient_id) ON DELETE CASCADE,
  visit_date DATE NOT NULL,
  visit_type TEXT,
  notes TEXT
);

CREATE TABLE IF NOT EXISTS labs (
  lab_id BIGSERIAL PRIMARY KEY,
  patient_id BIGINT NOT NULL REFERENCES patients(patient_id) ON DELETE CASCADE,
  visit_id BIGINT REFERENCES visits(visit_id) ON DELETE SET NULL,
  collected_date DATE NOT NULL,
  lab_name TEXT NOT NULL,
  lab_value NUMERIC,
  lab_unit TEXT,
  reference_range_low NUMERIC,
  reference_range_high NUMERIC,
  abnormal_flag BOOLEAN
);

CREATE TABLE IF NOT EXISTS domains (
  domain_id BIGSERIAL PRIMARY KEY,
  patient_id BIGINT NOT NULL REFERENCES patients(patient_id) ON DELETE CASCADE,
  visit_id BIGINT REFERENCES visits(visit_id) ON DELETE SET NULL,
  assessed_date DATE NOT NULL,
  domain_name TEXT NOT NULL,
  domain_score NUMERIC,
  active BOOLEAN,
  ever_involved BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE TABLE IF NOT EXISTS medications (
  medication_id BIGSERIAL PRIMARY KEY,
  patient_id BIGINT NOT NULL REFERENCES patients(patient_id) ON DELETE CASCADE,
  medication_name TEXT NOT NULL,
  category TEXT,
  dose TEXT,
  route TEXT,
  frequency TEXT,
  start_date DATE,
  end_date DATE,
  current BOOLEAN NOT NULL DEFAULT FALSE,
  stop_reason TEXT,
  indication TEXT
);

CREATE INDEX IF NOT EXISTS idx_visits_patient_date ON visits(patient_id, visit_date);
CREATE INDEX IF NOT EXISTS idx_labs_patient_name_date ON labs(patient_id, lab_name, collected_date);
CREATE INDEX IF NOT EXISTS idx_domains_patient_name_date ON domains(patient_id, domain_name, assessed_date);
CREATE INDEX IF NOT EXISTS idx_meds_patient_current ON medications(patient_id, current);
CREATE INDEX IF NOT EXISTS idx_meds_category ON medications(category);
