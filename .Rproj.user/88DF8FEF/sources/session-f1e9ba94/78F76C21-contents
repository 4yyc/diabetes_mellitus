wil_test <- function(var, data_table) {
  # Check if the number of obs. meet the required minimum (3)
  obs_by_gender_gp <- data_table %>%
    group_by(gender, delta_hba1c_conv_gp) %>%
    summarise(observation_count = sum(!is.na(.data[[var]]))) %>%
    filter(observation_count >= 3)
  
  genders_with_multiple_obs <- obs_by_gender_gp %>%
    group_by(gender) %>%
    filter(n() >= 2) %>%
    pull(gender)
  # requirement check
  req_check <- length(genders_with_multiple_obs) > 0 & (is.numeric(data_table[[var]]))
  #print(req_check)
  result<-list()
  # todo: haven't handled numeric type categorical data!! like client type
  if (req_check ) {
    message(paste(var, ": Apply Wilcox Test"))
    
    # Check if variable has more than 70% zeros
    zero_percentage <- sum(data_table[[var]] == 0, na.rm = TRUE) / length(data_table[[var]])
    
    if (zero_percentage <= 0.669 | zero_percentage==0) {
      var_result <- data_table %>%
        filter(gender %in% genders_with_multiple_obs) %>%
        group_by(gender) %>%
        rstatix::wilcox_test(as.formula(paste(var, "delta_hba1c_conv_gp", sep = " ~ "))) %>%
        rstatix::add_significance() %>%
        rstatix::add_xy_position(x = "gender", fun = "mean_se", dodge = 0.8)
      
      result[[var]] <- var_result
    } else {
      message(paste(var, ": Skipped, more than 70% zeros."))
    }
  } else {
    message(paste(var, ": Skipped, non-numeric variable or no enough observations."))
  }
}


## plot function ##
stratified_plot <- function(data_table, var_stat_test, var) {
  data_table %>%
    ggbarplot(x = "gender", y = var, fill = "delta_hba1c_conv_gp", alpha = 0.5,
              add = "mean_se", add.params = list(group = "delta_hba1c_conv_gp"),
              position = position_dodge(0.8), legend = "right", legend.title = "") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "", y = "Mean ± SE", title = var) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 15)
    ) +
    stat_pvalue_manual(
      var_stat_test, label = "p.adj.signif", tip.length = 0.0,
      bracket.nudge.y = 1, step.increase = 0.05, hide.ns = FALSE
    )
}

