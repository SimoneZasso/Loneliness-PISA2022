run_analysis_and_save <- function(country_id, output_df) {
  result <- run_sem_analysis(country_id = country_id)
  output_df <- rbind(output_df, result)
  return(output_df)
}


run_analysis_and_save_PartMed <- function(country_id, output_df) {
  result <- run_sem_analysis_PART_MED(country_id = country_id)
  output_df <- rbind(output_df, result)
  return(output_df)
}
