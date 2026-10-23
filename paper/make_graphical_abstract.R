# Graphical abstract of the manuscript (PNG at 300 dpi and PDF).
#
# Usage, from the repository root:
#   source("paper/make_graphical_abstract.R")
#   make_graphical_abstract()
#
# The bar chart reads the accepted bed additions from the capacity searches
# (expansion.csv of each optimized_* output folder), so the figure follows the
# latest runs. The text describes the implemented model; it makes no claim of
# validation or global optimality. Requires ggplot2 only.
make_graphical_abstract <- function(
    optimized_dirs = c(reference = "outputs/optimized_reference",
                       volume_20 = "outputs/optimized_volume20",
                       concentration_5 = "outputs/optimized_concentration5"),
    scenario_labels = c(reference = "15/day\n10 days", volume_20 = "20/day\n10 days",
                        concentration_5 = "30/day\n5 days"),
    output_dir = "outputs/graphical_abstract",
    width = 12, height = 5.3, dpi = 300) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE),
            setequal(names(optimized_dirs), names(scenario_labels)))
  # ---- results: one accepted row per scenario ----------------------------
  results <- do.call(rbind, lapply(names(optimized_dirs), function(id) {
    path <- file.path(optimized_dirs[[id]], "expansion.csv")
    if (!file.exists(path)) stop("Missing ", path, "; run the optimizations first.")
    e <- utils::read.csv(path)
    e <- e[e$scenario_id == id & e$accepted %in% TRUE, , drop = FALSE]
    if (nrow(e) != 1L) stop("Expected one accepted search for '", id, "' in ", path)
    data.frame(scenario = scenario_labels[[id]], GenMed = e$GenMed_added, ICU = e$ICU_added)
  }))
  # Achieved joint compliance of each accepted search (returned for reference; the panel text
  # states the search criterion, reliability_level in R/01_config.R, which is 75%).
  reliability <- vapply(optimized_dirs, function(d) {
    e <- utils::read.csv(file.path(d, "expansion.csv")); min(e$joint_compliance[e$accepted %in% TRUE])
  }, numeric(1))

  # ---- palette (colour-blind safe) ----------------------------------------
  col_civ <- "#0072B2"; col_surge <- "#D55E00"; col_model <- "#4D4D4D"
  col_gen <- "#009E73"; col_icu <- "#CC79A7"; light <- "#F3F4F6"

  box <- function(x0, y0, x1, y1, fill = light, border = "grey55", lw = 0.5)
    ggplot2::annotate("rect", xmin = x0, xmax = x1, ymin = y0, ymax = y1,
                      fill = fill, colour = border, linewidth = lw)
  txt <- function(x, y, label, size = 3.4, face = "plain", colour = "black", hjust = 0.5)
    ggplot2::annotate("text", x = x, y = y, label = label, size = size, fontface = face,
                      colour = colour, hjust = hjust, lineheight = 0.95)
  arrow_big <- function(x0, x1, y)
    ggplot2::annotate("segment", x = x0, xend = x1, y = y, yend = y, linewidth = 1.3,
                      colour = "grey40", arrow = grid::arrow(length = grid::unit(0.22, "cm"), type = "closed"))
  unit_box <- function(x, y, label, fill)
    list(box(x - 2.8, y - 2.1, x + 2.8, y + 2.1, fill = fill, border = "grey30", lw = 0.6),
         txt(x, y, label, size = 3.2, face = "bold"))

  p <- ggplot2::ggplot() + ggplot2::xlim(0, 100) + ggplot2::ylim(0, 44) +
    ggplot2::coord_cartesian(expand = FALSE) + ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.margin = ggplot2::margin(4, 4, 4, 4))

  # title
  p <- p + txt(50, 41.3, "How many extra hospital beds does a military patient surge require?",
               size = 5.6, face = "bold") +
    txt(50, 38.3, "Discrete-event simulation of civilian hospital patient flow during large-scale combat operations (LSCO)",
        size = 3.5, colour = "grey30")

  # 1 inputs
  p <- p + txt(12.5, 34.3, "1  Inputs", size = 4.2, face = "bold", colour = col_model) +
    box(1, 19.5, 24, 32.2, fill = "#E8F1FA", border = col_civ, lw = 0.8) +
    txt(12.5, 30.4, "Routine civilian flow", size = 3.7, face = "bold", colour = col_civ) +
    txt(12.5, 26.2, "Pathways, arrival rates and\nlengths of stay from\nUC Davis Medical Center\npatient data (2024-2025)", size = 3.1) +
    box(1, 5.5, 24, 18.3, fill = "#FCEBDD", border = col_surge, lw = 0.8) +
    txt(12.5, 16.5, "Military surge scenarios", size = 3.7, face = "bold", colour = col_surge) +
    txt(12.5, 12.2, "Profile-specific care\npathways; surge volume\nand duration varied", size = 3.1)

  # 2 simulation model
  p <- p + arrow_big(24.6, 28.4, 18.5) +
    txt(40, 34.3, "2  Simulation model", size = 4.2, face = "bold", colour = col_model) +
    box(29, 5.5, 51, 32.2, fill = light, border = col_model, lw = 0.8) +
    unit_box(33.8, 27.5, "ED", "#DDE7F3") + unit_box(40, 27.5, "ICU", "#F3D3E5") +
    unit_box(46.2, 27.5, "GenMed", "#CDEBDD") +
    txt(40, 22.8, "Finite beds; patients keep their\ncurrent bed while boarding", size = 3.0) +
    txt(40, 18.6, "Boarding time credited toward\nthe next stay", size = 3.0) +
    txt(40, 14.6, "Ordered alternative placements\n(fallback units)", size = 3.0) +
    txt(40, 9.4, "Civilian warm-up, then surge;\nadded beds activated at surge onset", size = 3.0, colour = "grey25")

  # 3 capacity search
  p <- p + arrow_big(51.6, 55.4, 18.5) +
    txt(63, 34.3, "3  Capacity search", size = 4.2, face = "bold", colour = col_model) +
    box(56, 5.5, 71.5, 32.2, fill = light, border = col_model, lw = 0.8) +
    txt(63.75, 29.4, "Find added GenMed\nand ICU beds", size = 3.3, face = "bold") +
    txt(63.75, 22.8, "Joint waiting-time limits\nmet in at least 75%\nof replications", size = 3.0) +
    txt(63.75, 15.2, "Selected configuration\nconfirmed on independent\nreplications", size = 3.0) +
    txt(63.75, 8.6, "Occupancy, queues,\nwaiting times", size = 2.9, colour = "grey25")

  # 4 results (bars drawn with annotate so the whole figure shares one coordinate system)
  p <- p + arrow_big(72, 75.8, 18.5) +
    txt(88, 34.3, "4  Additional beds required", size = 4.2, face = "bold", colour = col_model) +
    box(76.5, 5.5, 99, 32.2, fill = "white", border = col_model, lw = 0.8)
  x0 <- 80.2; slot <- 6.4; bw <- 2.4; base <- 11.2
  scale_y <- 15 / max(c(results$GenMed, results$ICU))   # tallest bar = 15 plot units
  for (i in seq_len(nrow(results))) {
    xc <- x0 + (i - 1) * slot + 2.6
    p <- p +
      ggplot2::annotate("rect", xmin = xc - bw, xmax = xc, ymin = base, ymax = base + results$GenMed[i] * scale_y, fill = col_gen) +
      ggplot2::annotate("rect", xmin = xc, xmax = xc + bw, ymin = base, ymax = base + results$ICU[i] * scale_y, fill = col_icu) +
      txt(xc - bw / 2, base + results$GenMed[i] * scale_y + 0.9, results$GenMed[i], size = 3.1, face = "bold", colour = col_gen) +
      txt(xc + bw / 2, base + results$ICU[i] * scale_y + 0.9, results$ICU[i], size = 3.1, face = "bold", colour = col_icu) +
      txt(xc, 8.6, results$scenario[i], size = 2.6)
  }
  p <- p + ggplot2::annotate("segment", x = 78.4, xend = 98, y = base, yend = base, linewidth = 0.4, colour = "grey40") +
    ggplot2::annotate("rect", xmin = 78.6, xmax = 79.8, ymin = 29.1, ymax = 30.1, fill = col_gen) +
    txt(80.4, 29.6, "GenMed", size = 2.9, hjust = 0) +
    ggplot2::annotate("rect", xmin = 85.6, xmax = 86.8, ymin = 29.1, ymax = 30.1, fill = col_icu) +
    txt(87.4, 29.6, "ICU", size = 2.9, hjust = 0)

  # takeaway
  p <- p + box(1, 0.6, 99, 4.0, fill = "#FFF8E1", border = "#E0B000", lw = 0.7) +
    txt(50, 2.3, "Requirements depend on both total surge volume and how concentrated the arrivals are",
        size = 3.6, face = "bold")

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(file.path(output_dir, "graphical_abstract.png"), p, width = width, height = height,
                  dpi = dpi, bg = "white")
  ggplot2::ggsave(file.path(output_dir, "graphical_abstract.pdf"), p, width = width, height = height,
                  bg = "white")
  invisible(list(plot = p, results = results, accepted_joint_compliance = reliability))
}
