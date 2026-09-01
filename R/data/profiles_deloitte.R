fallbacks_list_1 <- list(
  BurnBed = c("ICU"),
  CardiacICU = c("ICU"),
  GenMed = c("PhysicalMed", "TransitionalCare"),
  ICU = c("CardiacICU"),
  PhysicalMed = c("GenMed", "TransitionalCare"),
  Psychiatric = c("GenMed", "PhysicalMed", "TransitionalCare"),
  TransitionalCare = c("GenMed", "PhysicalMed")
)

deloitte_test_profile_config <- function() {
  patient_profiles <- list(
    ambulatory = list(unit = NULL, los = NULL),
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

  profile_counts <- c(
    ambulatory = 242,
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
    source_label = "Deloitte test profiles",
    units = c("GenMed", "ICU"),
    patient_profiles = patient_profiles,
    profile_prob = profile_counts / sum(profile_counts),
    fallbacks = list()
  )
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

injury_path_test_profile_config <- function(wia_prob = 0.67) {
  stopifnot(
    length(wia_prob) == 1,
    is.finite(wia_prob),
    wia_prob >= 0,
    wia_prob <= 1
  )

  injury_paths <- list(
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

  wia_type_probabilities <- c(
    Amputation = 0.05,
    Burn = 0.038,
    Fracture = 0.196,
    Intracranial = 0.016,
    NervousSystem = 0.022,
    Musculoskeletal = 0.018,
    ThoracicOpenWound = 0.101,
    MultiOpenWound = 0.229
  )
  wia_type_probabilities <- wia_type_probabilities / sum(wia_type_probabilities)

  dnbi_type_probabilities <- c(
    MusculoskeletalFracture = 0.061,
    InjurySprains = 0.078,
    Digestive = 0.036,
    MentalDisorder = 0.039,
    OtherMedical = 0.094,
    OtherSurgical = 0.023
  )
  dnbi_type_probabilities <- dnbi_type_probabilities / sum(dnbi_type_probabilities)

  subtype_probabilities <- list(
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

  expand_probabilities <- function(type_probabilities, population_probability) {
    expanded <- lapply(names(type_probabilities), function(type_name) {
      population_probability *
        type_probabilities[[type_name]] *
        subtype_probabilities[[type_name]]
    })
    probabilities <- unlist(expanded, use.names = TRUE)
    names(probabilities) <- unlist(
      lapply(
        names(type_probabilities),
        function(type_name) names(subtype_probabilities[[type_name]])
      ),
      use.names = FALSE
    )
    probabilities
  }

  subtype_probability <- c(
    expand_probabilities(wia_type_probabilities, wia_prob),
    expand_probabilities(dnbi_type_probabilities, 1 - wia_prob)
  )
  subtype_probability <- subtype_probability / sum(subtype_probability)

  path_signature <- vapply(injury_paths, function(path) {
    if (is.null(path$unit)) "ambulatory" else paste(path$unit, collapse = "__")
  }, character(1))
  grouped_subtypes <- split(names(injury_paths), path_signature)

  profile_names <- c(
    ambulatory = "AMB",
    ICU__GenMed = "ICU_GM",
    GenMed = "GM",
    BurnBed__GenMed = "BB_GM",
    BurnBed = "BB",
    ICU__PhysicalMed = "ICU_PM",
    ICU__TransitionalCare = "ICU_TC",
    CardiacICU__GenMed__TransitionalCare = "CARD_GM_TC",
    CardiacICU__GenMed = "CARD_GM",
    PhysicalMed = "PM",
    GenMed__PhysicalMed = "GM_PM",
    Psychiatric = "PSY"
  )

  patient_profiles <- list()
  profile_prob <- numeric()
  profile_members <- list()

  for (signature in names(grouped_subtypes)) {
    members <- grouped_subtypes[[signature]]
    profile_name <- unname(profile_names[[signature]])
    member_probabilities <- subtype_probability[members]
    grouped_probability <- sum(member_probabilities)
    representative_path <- injury_paths[[members[[1]]]]

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
    names(fallbacks_list_2),
    unlist(fallbacks_list_2, use.names = FALSE)
  ))

  list(
    source = "injury_path_test",
    source_label = "Grouped injury-path test profiles (WIA 67%)",
    units = configured_units,
    patient_profiles = patient_profiles,
    profile_prob = profile_prob,
    fallbacks = fallbacks_list_2,
    profile_members = profile_members,
    wia_prob = wia_prob
  )
}