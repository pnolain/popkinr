browsing_root <- ifelse(env_nmtoolbox_root != "",
                        env_nmtoolbox_root,
                        "/")

if(user_initial_selection == "/" && browsing_root != "/")
  user_initial_selection <- browsing_root

# Error with filenames with special encoding
safe_file_ext <- safely(tools::file_ext)

is_nm_run_folder <- function(dir_path){
  if(is.null(dir_path) || !dir.exists(dir_path)) return(FALSE)
  dir_files <- list.files(dir_path, ignore.case = TRUE)

  exts <- safe_file_ext(dir_files)

  all(c("xml", "ext") %in% exts$result)
}

run_browser_formatting <- function(id, text){
  # `id` is the path of the tree node
  is_nm_run <- is_nm_run_folder(id)
  if(is_nm_run) return(sprintf("<span style='color:red;'>%s</span>", text))

  has_archives <- (length(list.files(id, pattern = "\\.tar\\.gz$", ignore.case = TRUE)) > 0)
  if(has_archives) return(sprintf("<strong>%s</strong>", text))

  text
}

run_browser <- callModule(popkinr::serverBrowser, "run_browser",
                          root_directory = browsing_root,
                          initial_selection = user_initial_selection,
                          file_extensions = "tar.gz",
                          folder_shortcuts = reactive({
                            if(nrow(rv$previous_runs) > 0) return(dirname(rv$previous_runs$path))
                          }),
                          formatting_function = run_browser_formatting)

metadata_browser <- callModule(popkinr::serverBrowser, "metadata_browser",
                               root_directory = browsing_root,
                               initial_selection = user_initial_selection,
                               file_extensions = "tar.gz",
                               folder_shortcuts = reactive({
                                 if(nrow(rv$previous_runs) > 0) return(dirname(rv$previous_runs$path))
                               }),
                               formatting_function = run_browser_formatting)

comparison_browser <- callModule(popkinr::serverBrowser, "comparison_browser",
                                 root_directory = browsing_root,
                                 initial_selection = user_initial_selection,
                                 file_extensions = "tar.gz",
                                 folder_shortcuts = reactive({
                                   if(nrow(rv$previous_runs) > 0) return(dirname(rv$previous_runs$path))
                                 }),
                                 formatting_function = run_browser_formatting)

# table modules----
callModule(extendedDTnew, "thetas_table", reactive_table = reactive(run_thetas_table()), filename = "THETAs")
callModule(extendedDTnew, "omega_table", reactive_table = reactive(run_omega_table()), filename = "OMEGA")
callModule(extendedDTnew, "eta_bars_table", reactive_table = reactive(run_eta_bars_table()), filename = "ETA-bars")
callModule(extendedDTnew, "sigma_table", reactive_table = reactive(run_sigma_table()), filename = "SIGMA")
callModule(extendedDTnew, "eta_shrinkage_table", reactive_table = reactive(run_eta_shrinkage_table()), filename = "ETA-shrinkage")
callModule(extendedDTnew, "eps_shrinkage_table", reactive_table = reactive(run_eps_shrinkage_table()), filename = "EPS-shinkage")


callModule(extendedDT, "parameters_individual_table", reactive_table = reactive(individual_parameters()),
           rownames = FALSE,
           filename = "individual_parameters", options = list(pageLength = 20, dom = 'ip'))
callModule(extendedDTnew, "parameters_distributions_summary", reactive_table = reactive(run_parameters_summary_table()),
           rownames = FALSE,
           filename = "parameters_summary", options = list(paging = FALSE))

callModule(extendedDT, "continuous_covariates_individual_table", reactive_table = reactive(continuous_covariates()),
           rownames = FALSE,
           filename = "individual_continuous_covariates", options = list(pageLength = 20, dom = 'ip'))
callModule(extendedDT, "continuous_covariates_distributions_summary",
           reactive_table = reactive(run_continuous_covariates_summary_table()),
           rownames = FALSE,
           filename = "continuous_covariates_summary", options = list(paging = FALSE))

callModule(extendedDT, "categorical_covariates_individual_table", reactive_table = reactive(categorical_covariates()),
           rownames = FALSE,
           filename = "individual_categorical_covariates", options = list(pageLength = 20, dom = 'ip'))
callModule(extendedDT, "categorical_covariates_distributions_summary", reactive_table = reactive(run_categorical_covariates_summary_table()),
           rownames = FALSE,
           filename = "categorical_covariates_summary", options = list(paging = FALSE))

callModule(extendedDT, "comparison_table", reactive_table = reactive(run_comparison_table()),
           filename = "comparison",
           digits = 6)

callModule(extendedDT, "comparison_stats_table", reactive_table = reactive(comparison_statistics()),
           filename = "comparison_statistics", escape = FALSE, rownames = FALSE,
           options = list(scrollX = TRUE, paging = FALSE))


callModule(extendedDT, "run_table_output", reactive_table = reactive(run_selected_table()),
           filename = "run_table", escape = FALSE, rownames = FALSE,
           options = list(pageLength = 50, scrollX = TRUE, dom = 'ip'))

callModule(extendedDT, "outliers_grubbs_table",
           reactive_table = reactive(outliers_grubbs_detection()$outliers),
           filename = "outliers_grubbs", options = list(pageLength = 20, dom = 'ip'))

callModule(extendedDT, "outliers_boxplot_table",
           reactive_table = reactive(outliers_boxplot_detection()$outliers),
           filename = "outliers_boxplot", options = list(pageLength = 20, dom = 'ip'))


callModule(extendedDT, "vpc_sim_table",
           reactive_table = reactive(vpc_sim_table()),
           filename = "vpc_simulated_ouput", rownames = FALSE,
           options = list(scrollX = TRUE, pageLength = 10, dom = 'ip'))

callModule(extendedDT, "vpc_obs_table",
           reactive_table = reactive(vpc_obs_table()),
           filename = "vpc_obs_ouput", rownames = FALSE,
           options = list(scrollX = TRUE, pageLength = 10, dom = 'ip'))

callModule(extendedDT, "vpc_db_table",
           reactive_table = reactive(vpc_db_table()),
           filename = "vpc_summary", rownames = FALSE,
           options = list(scrollX = TRUE, pageLength = 10, dom = 'ip'))


# plot modules----
callModule(extendedPlot, "ofv_convergence_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_ofv_convergence_plot()))
callModule(extendedPlot, "individual_ofv_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_individual_ofv()))
callModule(extendedPlot, "pop_parameters_convergence_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_parameters_convergence_plot()))

callModule(extendedPlot, "dv_vs_pred_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_dv_vs_pred_plot()),
           plot_extra = reactive(dv_vs_pred_plot_extra()))
callModule(extendedPlot, "spaghetti_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_spaghetti_plot()))
callModule(extendedPlot, "residuals_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_residuals_plot()),
           plot_extra = reactive(residuals_plot_extra()))
callModule(extendedPlot, "individuals_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_individuals_plot()))
callModule(extendedPlot, "vpc_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_vpc_plot()),
           r_code = FALSE)

callModule(extendedPlot, "parameters_distributions_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_parameters_distributions_plot()))
callModule(extendedPlot, "continuous_covariates_distributions_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_continuous_covariates_distributions_plot()))
callModule(extendedPlot, "categorical_covariates_distributions_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_categorical_covariates_distributions_plot()))

callModule(extendedPlot, "parameters_correlations_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_parameters_correlations_plot()),
           plot_extra = reactive(parameters_correlations_plot_extra()))
callModule(extendedPlot, "p_p_correlation_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(selected_p_p_correlation_plot()))

callModule(extendedPlot, "covariates_correlations_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_covariates_correlations_plot()),
           plot_extra = reactive(covariates_correlations_plot_extra()))
callModule(extendedPlot, "c_c_correlation_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(selected_c_c_correlation_plot()))

callModule(extendedPlot, "parameters_covariates_correlations_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_parameters_covariates_correlations_plot()),
           plot_extra = reactive(parameters_covariates_correlations_plot_extra()))
callModule(extendedPlot, "p_c_correlation_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(selected_p_c_correlation_plot()))

callModule(extendedPlot, "parameters_cat_covs_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_parameters_vs_categorical_covariates_plot()))

callModule(extendedPlot, "outliers_qqplot", reactive_run = reactive(rv$run), reactive_plot = reactive(outliers_qqplot()),
           r_code = FALSE)
callModule(extendedPlot, "outliers_boxplot", reactive_run = reactive(rv$run), reactive_plot = reactive(outliers_boxplot()),
           r_code = FALSE)

callModule(extendedPlot, "comparison_param_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_comparison_param_plot()),
           r_code = FALSE)
callModule(extendedPlot, "comparison_ofv_plot", reactive_run = reactive(rv$run), reactive_plot = reactive(run_comparison_ofv_plot()),
           r_code = FALSE)
