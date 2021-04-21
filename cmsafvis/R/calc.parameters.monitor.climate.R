# save parameters in csv for further processing (e.g. display in Toolbox)
calc.parameters.monitor.climate <- function(final_values, ranking.values = NULL)
{
  # save parameters
  outfile_path <- file.path(tempdir(),"mc_parameters.csv")
  # unlink tmp files
  if(file.exists(outfile_path)){
    unlink(outfile_path)
  }
  utils::write.csv(final_values, file = outfile_path, row.names = FALSE)
  
  # save ranking results
  if(!(is.null(ranking.values))){
    outfile_ranking_path <- file.path(tempdir(),"mc_ranking.csv")
    # unlink tmp files
    if(file.exists(outfile_ranking_path)){
      unlink(outfile_ranking_path)
    }
    utils::write.csv(ranking.values, file = outfile_ranking_path, row.names = FALSE)
  }
}