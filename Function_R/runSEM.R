run_sem_analysis <- function(country_id) {
  
  
  create_resume_table <- function(model_data, country_id, model_number) {
    data.frame(
      Country = as.numeric(country_id),
      Model = model_number,
      N = model_data[["data"]][["nobs"]],
      NFP = model_data[["fit"]][["npar"]],
      YB_chiSquared = round(model_data[["fit"]][["chisq"]], 3),
      df = model_data[["fit"]][["df"]],
      p = round(model_data[["fit"]][["pvalue.scaled"]], 3),
      SCR = round(model_data[["fit"]][["chisq.scaling.factor"]], 3),
      CFI = round(model_data[["fit"]][["cfi.scaled"]], 3),
      TLI = round(model_data[["fit"]][["tli.scaled"]], 3),
      RMSEA = round(model_data[["fit"]][["rmsea.scaled"]], 3),
      SRMR = round(model_data[["fit"]][["srmr"]], 3),
      AIC = round(model_data[["fit"]][["aic"]], 3),
      BIC = round(model_data[["fit"]][["bic"]], 3),
      ABIC = round(model_data[["fit"]][["bic2"]], 3)
    )
  }
  
  
  # Fit Model 1
  
  fit1 <- sem(
    sem_m1,
    data = data_mlSEM[data_mlSEM$CNTRYID==country_id, ],
    missing = "ML",
    estimator = "MLR"
    # cluster = "CNTRYID"
  )
  
  # Summary Model 1
  
  m1_sem_country <- summary(
    fit1,
    fit.measures = TRUE,
    rsquare=TRUE,
    standardized = TRUE
  )
  
  # Resume Table Fit Model 1
  
  resume_table_m1 <- create_resume_table(m1_sem_country, country_id, 1)
  
  
  # Fit Model 2
  
  fit2 <- sem(
    sem_m2,
    data = data_mlSEM[data_mlSEM$CNTRYID==country_id, ],
    missing = "ML",
    estimator = "MLR"
    # cluster = "CNTRYID"
  )
  
  # Summary Model 2
  
  m2_sem_country <- summary(
    fit2,
    fit.measures = TRUE,
    rsquare=TRUE,
    standardized = TRUE
  )
  
  # Resume Table Fit Model 2
  
  resume_table_m2 <- create_resume_table(m2_sem_country, country_id, 2)
  
  # Bind Model 1 and 2
  
  res_table <- rbind(resume_table_m1,resume_table_m2)
  
  # Add model comparison
  
  res_table2 <- cbind(
    res_table,
    as_tibble(
      lavTestLRT(fit2, fit1, method = "satorra.bentler.2001")
    ) %>%
      dplyr::select(`Chisq diff`, `Df diff`, `Pr(>Chisq)`) %>% 
      rename(
        SBdelta_chiSq = `Chisq diff`,
        delta_df = `Df diff`,
        delta_p = `Pr(>Chisq)`
      ) %>% 
      mutate(
        SBdelta_chiSq = round(SBdelta_chiSq, 3),
        delta_p = round(delta_p, 3)
      )
  )  
  
  
  
  # PARAMETERS
  
  # Extract path a, path b, and R^2
  
  param_table <- m2_sem_country[["pe"]] %>% 
    filter(label == "a" | label == "b" | op == "r2") %>% 
    filter(lhs == "Math_ach" & rhs == "Math_ach" | label == "a" | label == "b")
  
  # Put all necessary parameters in Table
  
  param_table2 <- data.frame(
    path_a = round(param_table[param_table$label == "a", "est"], 3),
    std_err_a = round(param_table[param_table$label == "a", "se"], 3),
    path_a_std = round(param_table[param_table$label == "a", "std.all"], 3),
    path_a_pval = round(param_table[param_table$label == "a", "pvalue"], 3),
    path_b = round(param_table[param_table$label == "b", "est"], 3),
    std_err_b = round(param_table[param_table$label == "b", "se"], 3),
    path_b_std = round(param_table[param_table$label == "b", "std.all"], 3),
    path_b_pval = round(param_table[param_table$label == "b", "pvalue"], 3),
    Ach_r_square = round(param_table[param_table$op == "r2", "est"]*100, 2)
  ) %>%
    add_row(., .before = 1)
  
  
  # Bind resume table and parameters table
  
  final_table_fit_param <- cbind(res_table2, param_table2)
  
  ###################################
  
  # Mediation
  
  final_table_med <- med_mcmc(
    a = final_table_fit_param[final_table_fit_param$Model==2, "path_a"],
    b = final_table_fit_param[final_table_fit_param$Model==2, "path_b"],
    se_a = final_table_fit_param[final_table_fit_param$Model==2, "std_err_a"],
    se_b = final_table_fit_param[final_table_fit_param$Model==2, "std_err_b"],
    cov_ab_number = 0,
    D = 0
  )%>%
    add_row(., .before = 1)
  
  
  ###########################
  # Bind everything
  
  
  final_table <- cbind(final_table_fit_param, final_table_med)
  

# RMESEA_D creating -------------------------------------------------------

  
  final_table$RMESEA_D <- sqrt((final_table$SBdelta_chiSq - final_table$delta_df) / 
                                          (final_table$delta_df * (final_table$N - 1)))
  final_table$RMESEA_D[is.nan(final_table$RMESEA_D)] <- 0
  
  
  return(as.data.frame(final_table))
}
