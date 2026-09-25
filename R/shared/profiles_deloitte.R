fallbacks_list_1 <- list(
  GenMed = c("Surge")
)

# Shared default: ICU stays are assumed far more variable than other units.
# unit may be NULL (ambulatory), a single unit, or an ordered vector of units.
default_cv_for_unit <- function(unit) {
  if (is.null(unit)) return(NULL)
  ifelse(unit == "ICU", 1, 0.24)
}

# Example 1: the original Deloitte 3-unit test profiles
# Only the GenMed<->Surge fallback
# link changes; no patient_profiles step uses this unit directly.
deloitte_test_profile_config <- function() {
  patient_profiles <- list(
    medsurg_3 = list(unit = "GenMed", los = 3),
    medsurg_5 = list(unit = "GenMed", los = 5),
    medsurg_7 = list(unit = "GenMed", los = 7),
    medsurg_15 = list(unit = "GenMed", los = 15),
    medsurg_18 = list(unit = "GenMed", los = 18),
    icu_1_medsurg_4 = list(unit = c("ICU", "GenMed"), los = c(1, 4)),
    icu_2_medsurg_4 = list(unit = c("ICU", "GenMed"), los = c(2, 4)),
    icu_3_medsurg_7 = list(unit = c("ICU", "GenMed"), los = c(3, 7)),
    icu_5_medsurg_7 = list(unit = c("ICU", "GenMed"), los = c(5, 7)),
    icu_10_medsurg_10 = list(unit = c("ICU", "GenMed"), los = c(10, 10)),
    icu_30 = list(unit = "ICU", los = 30)
  )
  # CV = 1 for ICU steps, 0.24 for every other unit.
  patient_profiles <- lapply(patient_profiles, function(profile) {
    profile$cv <- default_cv_for_unit(profile$unit)
    profile
  })

  profile_counts <- c(
    medsurg_3 = 24,
    medsurg_5 = 201,
    medsurg_7 = 10,
    medsurg_15 = 9,
    medsurg_18 = 4,
    icu_1_medsurg_4 = 7,
    icu_2_medsurg_4 = 78,
    icu_3_medsurg_7 = 15,
    icu_5_medsurg_7 = 15,
    icu_10_medsurg_10 = 6,
    icu_30 = 4
  )

  list(
    source = "deloitte_test",
    source_label = "Deloitte test profiles (reduced; UC Davis units)",
    units = c("Surge", "GenMed", "ICU"),
    patient_profiles = patient_profiles,
    profile_prob = profile_counts / sum(profile_counts),
    fallbacks = fallbacks_list_1
  )
}

# UC Davis calibrated profiles: reads the CSV built by paper/Cleaning.Rmd's
# "Build UC Davis profiles for the app" section (build_uc_davis_profiles()),
# which follows the 2026_Assessing_the_hospital_occupancy_under_a_surge_event
# paper's Section 2.2 flow structure. Every patient is modeled as
# ED -> unit[-> second unit] -- a single entry type, since the Davis extract
# has no per-unit split between ED-origin and direct/transfer admissions
# (Admit_<unit> is a combined total; see Cleaning.Rmd's "What the data does
# NOT support"), so the paper's A_S/A_M/A_I cannot be calibrated separately
# from ED admissions here. Each profile has at most one post-admission
# transfer, chosen by the paper's Section 2.5 competing-exponential
# transfer/discharge rates (varphi/psi/epsilon). Every value is calibrated
# from 2025 UC Davis operational data only -- no external/illustrative
# values are substituted in. The Surge <-> GenMed transfer (both directions)
# is not observable in that data at all and is therefore not modeled (rate
# 0), rather than filled in from elsewhere. See Cleaning.Rmd for the full
# derivation and caveats (notably: ICU-linked transfer counts did not
# reconcile with a Little's-Law check there, so treat those specific rates
# as approximate).
uc_davis_profile_config <- function(csv_path = file.path("data", "baseline_civilian_profiles_uc_davis.csv")) {
  rows <- utils::read.csv(csv_path, check.names = FALSE, colClasses = "character",
                           fileEncoding = "UTF-8-BOM", strip.white = TRUE)
  split_steps <- function(value) trimws(strsplit(value, ",", fixed = TRUE)[[1]])

  patient_profiles <- stats::setNames(lapply(seq_len(nrow(rows)), function(index) {
    list(
      unit = split_steps(rows$Pathway[[index]]),
      los = as.numeric(split_steps(rows$Mean_stays_days[[index]])),
      cv = as.numeric(split_steps(rows$CV_values[[index]]))
    )
  }), rows$Profile)

  profile_counts <- stats::setNames(as.numeric(rows$Patients_per_day), rows$Profile)

  list(
    source = "uc_davis",
    source_label = "UC Davis calibrated profiles (ICU/GenMed/Surge; ED-origin admissions)",
    units = c("Surge", "GenMed", "ICU"),
    patient_profiles = patient_profiles,
    profile_prob = profile_counts / sum(profile_counts),
    fallbacks = fallbacks_list_1
  )
}

# Deloitte's granular Wounded-In-Action / Disease-and-Non-Battle-Injury (WIA/
# DNBI) pathway data: one entry per injury/illness subtype, each an ordered
# unit sequence with a mean length of stay per step (NULL unit = ambulatory,
# no bed at all). Shared, read-only source data for every
# deloitte_injury_path_profile_config() variant below -- individual hospital
# examples only change which units these subtypes are routed through
# (unit_map) and which fallback network applies, never these proportions.
deloitte_injury_paths <- list(
  Amputation1 = list(unit = c("ICU", "GenMed"), los = c(3, 7)),
  Amputation2 = list(unit = c("ICU", "GenMed"), los = c(5, 7)),
  Amputation3 = list(unit = "GenMed", los = 5),
  Amputation4 = list(unit = "GenMed", los = 7),
  Burn1 = list(unit = NULL, los = NULL),
  Burn2 = list(unit = "GenMed", los = 15),
  Burn3 = list(unit = c("BurnBed", "GenMed"), los = c(10, 10)),
  Burn4 = list(unit = "BurnBed", los = 30),
  Fract1 = list(unit = NULL, los = NULL),
  Fract2 = list(unit = c("ICU", "PhysicalMed"), los = c(2, 4)),
  Fract3 = list(unit = "GenMed", los = 5),
  Intr1 = list(unit = c("ICU", "GenMed"), los = c(3, 12)),
  Intr2 = list(unit = c("ICU", "GenMed"), los = c(3, 7)),
  NS1 = list(unit = c("ICU", "TransitionalCare"), los = c(3, 7)),
  NS2 = list(unit = "GenMed", los = 5),
  MS1 = list(unit = c("ICU", "GenMed"), los = c(1, 5)),
  MS2 = list(unit = "GenMed", los = 5),
  TOW1 = list(unit = c("CardiacICU", "GenMed", "TransitionalCare"), los = c(3, 7, 8)),
  TOW2 = list(unit = c("CardiacICU", "GenMed"), los = c(5, 10)),
  TOW3 = list(unit = "GenMed", los = 10),
  MOW1 = list(unit = c("ICU", "GenMed"), los = c(3, 12)),
  MOW2 = list(unit = c("ICU", "GenMed"), los = c(3, 7)),
  MOW3 = list(unit = "GenMed", los = 5),
  MUSF1 = list(unit = "PhysicalMed", los = 5),
  MUSF2 = list(unit = c("GenMed", "PhysicalMed"), los = c(2, 3)),
  ISS1 = list(unit = "PhysicalMed", los = 3),
  ISS2 = list(unit = NULL, los = NULL),
  DIG1 = list(unit = "PhysicalMed", los = 5),
  DIG2 = list(unit = NULL, los = NULL),
  MDN1 = list(unit = "Psychiatric", los = 18),
  MDN2 = list(unit = NULL, los = NULL),
  OMC1 = list(unit = "GenMed", los = 5),
  OMC2 = list(unit = NULL, los = NULL),
  OSR1 = list(unit = c("ICU", "GenMed"), los = c(1, 4)),
  OSR2 = list(unit = "GenMed", los = 3)
)

deloitte_wia_type_probabilities <- c(
  Amputation = 0.05,
  Burn = 0.038,
  Fracture = 0.196,
  Intracranial = 0.016,
  NervousSystem = 0.022,
  Musculoskeletal = 0.018,
  ThoracicOpenWound = 0.101,
  MultiOpenWound = 0.229
)
deloitte_wia_type_probabilities <- deloitte_wia_type_probabilities / sum(deloitte_wia_type_probabilities)

deloitte_dnbi_type_probabilities <- c(
  MusculoskeletalFracture = 0.061,
  InjurySprains = 0.078,
  Digestive = 0.036,
  MentalDisorder = 0.039,
  OtherMedical = 0.094,
  OtherSurgical = 0.023
)
deloitte_dnbi_type_probabilities <- deloitte_dnbi_type_probabilities / sum(deloitte_dnbi_type_probabilities)

deloitte_subtype_probabilities <- list(
  Amputation = c(Amputation1 = 0.3, Amputation2 = 0.3, Amputation3 = 0.2, Amputation4 = 0.2),
  Burn = c(Burn1 = 0.5, Burn2 = 0.25, Burn3 = 0.15, Burn4 = 0.1),
  Fracture = c(Fract1 = 0.1, Fract2 = 0.4, Fract3 = 0.5),
  Intracranial = c(Intr1 = 0.4, Intr2 = 0.6),
  NervousSystem = c(NS1 = 0.1, NS2 = 0.9),
  Musculoskeletal = c(MS1 = 0.1, MS2 = 0.9),
  ThoracicOpenWound = c(TOW1 = 0.7, TOW2 = 0.1, TOW3 = 0.2),
  MultiOpenWound = c(MOW1 = 0.7, MOW2 = 0.1, MOW3 = 0.2),
  MusculoskeletalFracture = c(MUSF1 = 0.1, MUSF2 = 0.9),
  InjurySprains = c(ISS1 = 0.1, ISS2 = 0.9),
  Digestive = c(DIG1 = 0.3, DIG2 = 0.7),
  MentalDisorder = c(MDN1 = 0.3, MDN2 = 0.7),
  OtherMedical = c(OMC1 = 0.3, OMC2 = 0.7),
  OtherSurgical = c(OSR1 = 0.3, OSR2 = 0.7)
)

deloitte_expand_probabilities <- function(type_probabilities, population_probability) {
  expanded <- lapply(names(type_probabilities), function(type_name) {
    population_probability *
      type_probabilities[[type_name]] *
      deloitte_subtype_probabilities[[type_name]]
  })
  probabilities <- unlist(expanded, use.names = TRUE)
  names(probabilities) <- unlist(
    lapply(
      names(type_probabilities),
      function(type_name) names(deloitte_subtype_probabilities[[type_name]])
    ),
    use.names = FALSE
  )
  probabilities
}

# Short profile-name codes, keyed by unit; used to build a readable profile
# name (e.g. "ICU_GM") from a pathway's unit sequence. An unrecognized unit
# (e.g. a future hospital-specific unit) falls back to its own first 3
# letters, upper-cased, so this never errors on new units.
deloitte_unit_short_code <- c(
  ICU = "ICU", GenMed = "GM", BurnBed = "BB", PhysicalMed = "PM",
  CardiacICU = "CARD", Cardiology = "CARD", TransitionalCare = "TC",
  Psychiatric = "PSY", ED = "ED", Surge = "IPSURGE"
)
deloitte_abbreviate_units <- function(units) {
  if (is.null(units)) return("AMB")
  codes <- unname(deloitte_unit_short_code[units])
  missing <- is.na(codes)
  if (any(missing)) codes[missing] <- toupper(substr(units[missing], 1, 3))
  paste(codes, collapse = "_")
}

fallbacks_list_2 <- list(
  BurnBed = c("ICU"),
  CardiacICU = c("ICU"),
  GenMed = c("PhysicalMed", "TransitionalCare"),
  ICU = c("CardiacICU"),
  PhysicalMed = c("GenMed", "TransitionalCare"),
  Psychiatric = c("GenMed", "PhysicalMed", "TransitionalCare"),
  TransitionalCare = c("GenMed", "PhysicalMed")
)

# Regional and Tertiary UC Davis hospitals share the same specialty units as
# the original Deloitte data except CardiacICU, which UC Davis calls
# Cardiology; the Regional hospital additionally has no BurnBed or
# Psychiatric unit at all.
tertiary_hospital_fallbacks <- list(
  BurnBed = c("ICU"),
  Cardiology = c("ICU"),
  GenMed = c("PhysicalMed", "TransitionalCare"),
  ICU = c("Cardiology"),
  PhysicalMed = c("GenMed", "TransitionalCare"),
  Psychiatric = c("GenMed", "PhysicalMed", "TransitionalCare"),
  TransitionalCare = c("GenMed", "PhysicalMed")
)

regional_hospital_fallbacks <- list(
  Cardiology = c("ICU"),
  GenMed = c("PhysicalMed", "TransitionalCare"),
  ICU = c("Cardiology"),
  PhysicalMed = c("GenMed", "TransitionalCare"),
  TransitionalCare = c("GenMed", "PhysicalMed")
)

# Shared builder behind injury_path_test_profile_config() and the
# hospital-specific examples below. Reuses the same Deloitte WIA/DNBI
# proportions (deloitte_injury_paths/deloitte_*_type_probabilities) for all of
# them; unit_map only renames or reroutes pathway units to match a target
# hospital's actual unit set (e.g. rerouting BurnBed patients to ICU for a
# hospital with no dedicated burn unit). A step's length of stay travels
# unchanged with a reroute -- a simplifying modeling assumption to adapt the
# Deloitte proportions to a different unit set, not a clinically validated
# adjustment; treat rerouted-unit examples as illustrative, not calibrated.
deloitte_injury_path_profile_config <- function(source, source_label, fallbacks,
                                                 unit_map = character(0),
                                                 capacities = NULL,
                                                 wia_prob = 0.67) {
  stopifnot(
    length(wia_prob) == 1,
    is.finite(wia_prob),
    wia_prob >= 0,
    wia_prob <= 1
  )

  remap_units <- function(units) {
    if (is.null(units)) return(NULL)
    matched <- units %in% names(unit_map)
    units[matched] <- unname(unit_map[units[matched]])
    units
  }
  injury_paths <- lapply(deloitte_injury_paths, function(path) {
    path$unit <- remap_units(path$unit)
    path
  })

  subtype_probability <- c(
    deloitte_expand_probabilities(deloitte_wia_type_probabilities, wia_prob),
    deloitte_expand_probabilities(deloitte_dnbi_type_probabilities, 1 - wia_prob)
  )
  subtype_probability <- subtype_probability / sum(subtype_probability)

  path_signature <- vapply(injury_paths, function(path) {
    if (is.null(path$unit)) "ambulatory" else paste(path$unit, collapse = "__")
  }, character(1))
  grouped_subtypes <- split(names(injury_paths), path_signature)

  patient_profiles <- list()
  profile_prob <- numeric()
  profile_members <- list()

  for (signature in names(grouped_subtypes)) {
    # Ambulatory subtypes (unit = NULL) are excluded rather than kept as a
    # zero-bed profile: profile_prob is renormalized over the remaining,
    # bed-occupying profiles below, so removing this group redistributes its
    # probability mass rather than shrinking the effective patient count.
    if (signature == "ambulatory") next
    members <- grouped_subtypes[[signature]]
    representative_path <- injury_paths[[members[[1]]]]
    profile_name <- deloitte_abbreviate_units(representative_path$unit)
    # Two different original signatures can remap to the same unit sequence
    # (e.g. BurnBed rerouted to ICU merges with paths already routed through
    # ICU); make the merged group's name unique by appending a counter.
    if (profile_name %in% names(patient_profiles)) {
      suffix <- 2L
      while (paste0(profile_name, "_", suffix) %in% names(patient_profiles)) suffix <- suffix + 1L
      profile_name <- paste0(profile_name, "_", suffix)
    }
    member_probabilities <- subtype_probability[members]
    grouped_probability <- sum(member_probabilities)

    if (is.null(representative_path$unit)) {
      grouped_los <- NULL
    } else {
      weighted_los <- vapply(seq_along(representative_path$unit), function(index) {
        sum(vapply(members, function(member) {
          injury_paths[[member]]$los[[index]] * subtype_probability[[member]]
        }, numeric(1))) / grouped_probability
      }, numeric(1))
      grouped_los <- round(weighted_los, 3)
    }

    patient_profiles[[profile_name]] <- list(
      unit = representative_path$unit,
      los = grouped_los
    )
    profile_prob[[profile_name]] <- grouped_probability
    profile_members[[profile_name]] <- members
  }

  profile_prob <- profile_prob / sum(profile_prob)
  configured_units <- unique(c(
    unlist(lapply(patient_profiles, `[[`, "unit"), use.names = FALSE),
    names(fallbacks),
    unlist(fallbacks, use.names = FALSE)
  ))

  list(
    source = source,
    source_label = source_label,
    units = configured_units,
    patient_profiles = patient_profiles,
    profile_prob = profile_prob,
    fallbacks = fallbacks,
    capacities = capacities,
    profile_members = profile_members,
    wia_prob = wia_prob
  )
}

# The original, most granular Deloitte example: all 7 units as Deloitte
# defined them (CardiacICU, BurnBed, Psychiatric included), no rerouting.
injury_path_test_profile_config <- function(wia_prob = 0.67) {
  deloitte_injury_path_profile_config(
    source = "injury_path_test",
    source_label = "Grouped injury-path test profiles (WIA 67%)",
    fallbacks = fallbacks_list_2,
    wia_prob = wia_prob
  )
}

# Example 2: UC Davis Tertiary hospital -- same specialty units as the
# original Deloitte data, only CardiacICU relabeled to Cardiology. Bed counts
# are UC Davis's own published Tertiary capacities, not derived from Deloitte.
tertiary_hospital_test_profile_config <- function(wia_prob = 0.67) {
  deloitte_injury_path_profile_config(
    source = "tertiary_hospital_test",
    source_label = "Tertiary hospital test profiles (Deloitte-derived; UC Davis Tertiary unit set)",
    fallbacks = tertiary_hospital_fallbacks,
    unit_map = c(CardiacICU = "Cardiology"),
    capacities = c(GenMed = 400, ICU = 72, BurnBed = 12, Cardiology = 18,
                   PhysicalMed = 24, Psychiatric = 20, TransitionalCare = 24),
    wia_prob = wia_prob
  )
}

# Example 3: UC Davis Regional hospital -- no BurnBed or Psychiatric unit, so
# those Deloitte pathways are rerouted to the nearest unit the hospital does
# have (BurnBed -> ICU, Psychiatric -> GenMed); CardiacICU relabeled to
# Cardiology as in the Tertiary example. Bed counts are UC Davis's own
# published Regional capacities, not derived from Deloitte.
regional_hospital_test_profile_config <- function(wia_prob = 0.67) {
  deloitte_injury_path_profile_config(
    source = "regional_hospital_test",
    source_label = "Regional hospital test profiles (Deloitte-derived; UC Davis Regional unit set)",
    fallbacks = regional_hospital_fallbacks,
    unit_map = c(CardiacICU = "Cardiology", BurnBed = "ICU", Psychiatric = "GenMed"),
    capacities = c(GenMed = 240, ICU = 20, Cardiology = 12,
                   PhysicalMed = 12, TransitionalCare = 20),
    wia_prob = wia_prob
  )
}
