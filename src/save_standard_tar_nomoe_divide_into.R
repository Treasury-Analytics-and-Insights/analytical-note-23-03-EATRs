save_standard_tar_nomoe_divide_into <- function(output_dt, output_path, template_path, divide_into = 1) 
{
  # if (split_threshold < 20000) {
  #   msg <- sprintf("split threshold: %f too low, must be greater than 20000", 
  #                  split_threshold)
  #   logerror(msg)
  #   stop(msg)
  # }
  output_dt <- copy(output_dt)
  output_dt[, `:=`(Index, .I)]
  values_dt <- output_dt[Sheet == "Values", .(Index, Value, 
                                              Population, Sample)]
  quantiles_dt <- output_dt[Sheet == "Quantiles", .(Index, 
                                                    Quantile, Value, Population, Sample)]
  descriptors_dt <- output_dt[, .SD, .SDcol = !c("Value", 
                                                 "Population", "Sample")]
  setcolorder(descriptors_dt, c("Index", "Sheet", "Scenario", 
                                "Variable"))
  if (divide_into == 1) {
    TAWApost::save_stats_output(values_dt, quantiles_dt, descriptors_dt, 
                      output_path, template_path = template_path)
  } else {
    # loginfo(
    #   "File: %s larger than threshold (%d > %d), splitting data and retrying.",
    #   basename(output_path), file.size(output_path), split_threshold
    # )
    loginfo("Splitting")
    filename_bits <- strsplit(basename(output_path), "\\.")[[1]]
    name <- filename_bits[[1]]
    ext <- filename_bits[[2]]
    part1 <- output_dt[1:(nrow(output_dt)%/%2)]
    part1_path = file.path(dirname(output_path), paste0(name, 
                                                        "_part1.", ext))
    save_standard_tar_nomoe_divide_into(part1, part1_path, template_path, 
                            divide_into = divide_into / 2)
    part2 <- output_dt[(nrow(output_dt)%/%2 + 1):nrow(output_dt)]
    part2_path = file.path(dirname(output_path), paste0(name, 
                                                        "_part2.", ext))
    save_standard_tar_nomoe_divide_into(part2, part2_path, template_path, 
                            divide_into = divide_into / 2)
  }
}
