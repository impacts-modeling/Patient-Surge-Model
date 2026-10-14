source("paper/run_manuscript_scenarios.R")
load_study_functions()

# The warm-up length is explicit: the model default is 30 days, the manuscript runs use 60.
study <- make_study_config(warmup = list(warmup_days = 80))


# Unlimited-capacity occupancy: simulated (DES) and analytic, stored in the same folder.
demand <- run_unlimited_demand_study(study, capacity = 10000L, output_dir = "outputs/unlimited_demand_01")

# Figure and tables contrasting the analytic and the simulated unlimited-capacity occupancy,
# and checking the warm-up length (no simulation; reads the runs saved above).
source("paper/make_unlimited_comparison.R")
unlimited_comparison <- make_unlimited_comparison("outputs/unlimited_demand_01",
                                                  output_dir = "outputs/unlimited_comparison_01")
unlimited_comparison$figures$trajectories   # analítico contra DES, todos los escenarios
unlimited_comparison$figures$warmup         # comprobación del warm-up
unlimited_comparison$peak_table             # picos DES contra analítico
unlimited_comparison$warmup_reached         # primer día en 95% y 99% del equilibrio

# cbind( unlimited_comparison$trajectory |> dplyr::filter(time ==0) |>
#   dplyr::select(analytic_mean ) ,
# 
# unlimited_comparison$trajectory |> dplyr::filter(time ==50) |>
#   dplyr::select(analytic_mean ))
   

# 
volume_and_concentration <- run_volume_and_concentration_study(study,
                                                               output_dir = "outputs/volume_concentration_01")

# The saved study carries the engine signature of the code that produced it; the engine
# has since changed (analytic initialization), so reloading it would be rejected by
# run_scenario_study(). Use the object computed above, or rerun the first stage.
volume_and_concentration <- readRDS("outputs/volume_concentration_01/study.rds")

# Corer de nuevo
optimized_ref <- optimize_study_scenarios(volume_and_concentration, c("reference"),
                                          search = list(initialization = "analytic", analytic_start_level = 0.75,
                                                        refinement_margin = 0.1,
                                                        max_evaluations = 50,
                                                        num_sims = 40, final_num_sims= 40,
                                                        acceptance_rule = "point_estimate"),
                                          output_dir = "outputs/optimized_reference")

                                      
optimized_vol20 <- optimize_study_scenarios(volume_and_concentration, c("volume_20"),
                                            search = list(initialization = "analytic", analytic_start_level = 0.75,
                                                          refinement_margin = 0.1,
                                                          num_sims = 40, final_num_sims= 40,
                                                          acceptance_rule = "point_estimate"),
                                      output_dir = "outputs/optimized_volume20")

optimized_con <- optimize_study_scenarios(volume_and_concentration, c("concentration_5"),
                                      search = list(initialization = "analytic", analytic_start_level = 0.75,
                                                    refinement_margin = 0.1,
                                                    num_sims = 40, final_num_sims= 40,
                                                    acceptance_rule = "point_estimate"),
                                      output_dir = "outputs/optimized_concentration5")

combined <- combine_optimized_studies(
  c("outputs/optimized_reference", "outputs/optimized_volume20", "outputs/optimized_concentration5"),
  out_file = "outputs/figs/trajectories_optim.png"
)

#rm(optimized_ref)
#trajectories_optim
h <- optimized_con$tables$search_evaluation_history
#h <- h[h$scenario_id == "optimized", ]
print(h[, c("stage", "stage_evaluation", "GenMed", "ICU", "replications", "target_replications",
            "GenMed_reliability", "ICU_reliability", "joint_reliability", "passes")])




combined$figures$trajectories   # ver la figura
combined$tables$design          # tabla combinada de diseño
combined$tables$expansion       # tabla combinada de resultados de expansión
