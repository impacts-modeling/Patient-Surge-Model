fallbacks_list_1 <- list(
  GenMed = c("Surge")
)

# Shared default: ICU stays are assumed far more variable than other units.
# unit may be NULL (ambulatory), a single unit, or an ordered vector of units.
default_cv_for_unit <- function(unit) {
  if (is.null(unit)) return(NULL)
  ifelse(unit == "ICU", 1, 0.24)
}

# UC Davis calibrated profiles: the original Deloitte 3-unit test profiles
# used in the manuscript (paper/ scripts only; not offered in the app).
# The internal source ID stays "deloitte_test" so saved runs remain comparable.
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

  # Display names for the manuscript tables only. The engine, saved
  # configurations and outputs keep using the profile keys above, so renaming
  # a label never affects results.
  profile_labels <- c(
    medsurg_3 = "Ward, short stay",
    medsurg_5 = "Ward, standard stay",
    medsurg_7 = "Ward, extended stay",
    medsurg_15 = "Ward, long stay",
    medsurg_18 = "Ward, prolonged stay",
    icu_1_medsurg_4 = "ICU brief, then ward",
    icu_2_medsurg_4 = "ICU short, then ward",
    icu_3_medsurg_7 = "ICU medium, then ward",
    icu_5_medsurg_7 = "ICU extended, then ward",
    icu_10_medsurg_10 = "ICU long, then ward",
    icu_30 = "ICU prolonged stay"
  )
  stopifnot(setequal(names(profile_labels), names(patient_profiles)))

  list(
    source = "deloitte_test",
    source_label = "UC Davis calibrated profiles (NDMS-based; IP Surge/GenMed/ICU)",
    units = c("Surge", "GenMed", "ICU"),
    patient_profiles = patient_profiles,
    profile_prob = profile_counts / sum(profile_counts),
    profile_labels = profile_labels,
    fallbacks = fallbacks_list_1
  )
}

# Deloitte's granular Wounded-In-Action / Disease-and-Non-Battle-Injury (WIA/
# DNBI) pathway data: one entry per injury/illness subtype, each an ordered
# unit sequence with a mean length of stay per step (NULL unit = ambulatory,
# no bed at all). Shared, read-only source data for
# deloitte_injury_path_profile_config(): a hospital only changes which
# subtypes are kept (those whose units it has) and which fallbacks apply,
# never these pathways or proportions.
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
  TOW1 = list(unit = c("Cardiology", "GenMed", "TransitionalCare"), los = c(3, 7, 8)),
  TOW2 = list(unit = c("Cardiology", "GenMed"), los = c(5, 10)),
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
  Cardiology = "CARD", TransitionalCare = "TC",
  Psychiatric = "PSY", ED = "ED", Surge = "IPSURGE"
)
deloitte_abbreviate_units <- function(units) {
  if (is.null(units)) return("AMB")
  codes <- unname(deloitte_unit_short_code[units])
  missing <- is.na(codes)
  if (any(missing)) codes[missing] <- toupper(substr(units[missing], 1, 3))
  paste(codes, collapse = "_")
}

# Master fallback network; deloitte_profiles_for_hospital() keeps only the
# rules and units a given hospital has.
fallbacks_list <- list(
  BurnBed = c("ICU"),
  Cardiology = c("ICU"),
  GenMed = c("Surge","PhysicalMed", "TransitionalCare"),
  ICU = c("Cardiology"),
  PhysicalMed = c("GenMed", "TransitionalCare"),
  Psychiatric = c("GenMed", "PhysicalMed", "TransitionalCare"),
  TransitionalCare = c("GenMed", "PhysicalMed")
)

# Builds grouped surge profiles from the Deloitte WIA/DNBI subtypes
# (deloitte_injury_paths / deloitte_*_type_probabilities): subtypes with the
# same pathway are merged, with probability-weighted mean stays.
# available_units (optional): subtypes whose pathway uses any other unit are
# dropped -- never rerouted -- and profile_prob is renormalized over what
# remains; excluded_subtypes / excluded_share record what was removed.
deloitte_injury_path_profile_config <- function(source, source_label, fallbacks,
                                                 capacities = NULL,
                                                 wia_prob = 0.67,
                                                 available_units = NULL) {
  stopifnot(
    length(wia_prob) == 1,
    is.finite(wia_prob),
    wia_prob >= 0,
    wia_prob <= 1
  )

  injury_paths <- deloitte_injury_paths

  subtype_probability <- c(
    deloitte_expand_probabilities(deloitte_wia_type_probabilities, wia_prob),
    deloitte_expand_probabilities(deloitte_dnbi_type_probabilities, 1 - wia_prob)
  )
  subtype_probability <- subtype_probability / sum(subtype_probability)

  excluded_subtypes <- character()
  excluded_share <- 0
  if (!is.null(available_units)) {
    inpatient <- names(injury_paths)[!vapply(injury_paths, function(path) is.null(path$unit), logical(1))]
    fits <- vapply(injury_paths[inpatient], function(path) all(path$unit %in% available_units), logical(1))
    excluded_subtypes <- inpatient[!fits]
    # Share of bed-occupying surge patients whose pathway cannot be served.
    excluded_share <- sum(subtype_probability[excluded_subtypes]) / sum(subtype_probability[inpatient])
    if (!any(fits)) {
      stop("No Deloitte pathway uses only the available units: ",
           paste(available_units, collapse = ", "), call. = FALSE)
    }
    injury_paths <- injury_paths[setdiff(names(injury_paths), excluded_subtypes)]
  }

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
    # Safety net: if two pathways ever abbreviate to the same name (units
    # sharing a short code), keep names unique by appending a counter.
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
    wia_prob = wia_prob,
    excluded_subtypes = excluded_subtypes,
    excluded_share = excluded_share
  )
}

# Deloitte (NDMS-based) surge profiles for an arbitrary hospital.
# capacities: named bed counts, e.g. c(GenMed = 80, ICU = 12, TransitionalCare = 12).
# Units with zero beds count as absent; ED, which no Deloitte pathway uses, is
# ignored. Assumptions:
# - A subtype is kept only if every unit of its pathway is available; pathways
#   are never truncated or rerouted. Excluded patients are not modeled: arrival
#   probabilities are renormalized over the kept profiles, so the surge
#   patients/day are spread over the pathways the hospital can serve (see
#   excluded_share for the removed fraction).
# - Fallbacks keep each rule of `fallbacks` whose primary unit is available,
#   with only its available fallback units (order preserved); rules left
#   empty are dropped.
deloitte_profiles_for_hospital <- function(capacities, fallbacks = fallbacks_list,
                                           wia_prob = 0.67,
                                           source = "deloitte_for_hospital",
                                           source_label = "NDMS-based profiles for the selected hospital units") {
  stopifnot(is.numeric(capacities), length(capacities) > 0, !is.null(names(capacities)),
            all(is.finite(capacities)), all(capacities >= 0))
  available_units <- setdiff(names(capacities)[capacities > 0], "ED")
  hospital_fallbacks <- lapply(fallbacks[intersect(names(fallbacks), available_units)],
                               function(units) intersect(units, available_units))
  hospital_fallbacks <- Filter(length, hospital_fallbacks)

  config <- deloitte_injury_path_profile_config(
    source = source, source_label = source_label, fallbacks = hospital_fallbacks,
    capacities = capacities, wia_prob = wia_prob, available_units = available_units
  )
  # The hospital's own units, including ones no kept pathway uses (e.g. Surge,
  # reachable only as a fallback).
  config$units <- available_units
  config
}
