rm(list=ls())
# one_var_dd <-dd[,.(tbwffm_baseline, bmi_baseline, delta_hba1c_conv_gp, gender, protein_ep_meal,fruits_day_deficit,
# delta_eAG_pt,urine_ep_baseline)]
# vector_list<-  c("delta_eAG_pt","tbwffm_baseline", "bmi_baseline", "protein_ep_meal","fruits_day_deficit","delta_eAG_pt")
if (!exists("setup_completed")) {




#### Load data funcs, and libraries ####
load("/Users/ando/Downloads/WSpace_0808.RData")
source("~/Desktop/work/cofit/rd/01r/diabetes_mellitus/config.R")
#source(paste0(work_dir, "rscript/lin_function.R"))

library(pacman)
pacman::p_load(magrittr, knitr, kableExtra, dplyr, readr, readxl, tibble, showtext, 
               ggvenn, ggplot2,knitr, kableExtra, openxlsx, lubridate, 
              ggpubr, webshot, stringr, googleVis, rstatix, rlang, data.table)
#extraInserts,cowplot

font_add(family = "CHN_character", regular = font_stheiti)
showtext_auto(enable = TRUE)
#---------------

#### clean source data ####
setDT(stat_table_1st_dm)
dd <- stat_table_1st_dm
names(dd) <- gsub("∆", "delta_", names(dd))
names(dd) <- gsub("%", "_pt", names(dd))

dd[ ,hba1c_endpoint_conv:=(GA_endpoint*0.216)+2.978  ]
dd[is.na(hba1c_endpoint_conv) ,hba1c_endpoint_conv:= hba1c_endpoint]
dd[ ,delta_hba1c_conv:= hba1c_endpoint_conv - hba1c_baseline]
dd[ ,delta_hba1c_conv_pt:= (delta_hba1c_conv/hba1c_baseline)*100]


#### creating cutoff point ####
#by gender and by sample size

#by delta_hba1c_conv
quantiles <- quantile(dd$delta_hba1c_conv, probs = c(1/3, 2/3)) #-1.92824  -0.87360 
dd$delta_hba1c_conv_gp <- cut(dd$delta_hba1c_conv, breaks = c(-Inf, quantiles[[1]], quantiles[[2]], Inf), labels = c("Good", "Medium", "Poor"))
#table(dd$delta_hba1c_conv_gp)

#by delta_hba1c_conv_pt
quantiles <- quantile(dd$delta_hba1c_conv_pt, probs = c(1/3, 2/3)) #-24.39024 -12.84706
dd$delta_hba1c_conv_pt_gp <- cut(dd$delta_hba1c_conv_pt, breaks = c(-Inf, quantiles[[1]], quantiles[[2]], Inf), labels = c("Good", "Medium", "Poor"))
#table(dd$delta_hba1c_conv_pt_gp)


# by age gp
quantiles <- quantile(dd$age, probs = c(1/3, 2/3)) # 47,56 
dd$age_gp <- cut(dd$age, breaks = c(-Inf, quantiles[[1]], quantiles[[2]], Inf), labels = c("<47", "47-56", ">56"))
#table(dd$age_gp)

#by finer age gp
#dd$age_gp_finer <- cut(dd$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
dd$age_gp_finer <- cut(dd$age, c(0,29.5,39.5,49.5,59.5,100), c("<29", "30-39","40-49","50-59",">60"))
#table(dd$age_gp_finer) 
#<25 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64 65-69   >70 
#1     4     9     5    14    19    20    21    14     8     9 

setup_completed <- TRUE
}


one_var_dd <-dd[,.(tbwffm_baseline, bmi_baseline, delta_hba1c_conv_gp, gender, protein_ep_meal,fruits_day_deficit,
                   delta_eAG_pt,urine_ep_baseline)]

vector_list<-  c("delta_eAG_pt","tbwffm_baseline", "bmi_baseline", "protein_ep_meal","fruits_day_deficit")





#讚 Apply Wilcox_Test to get stat table
stat.test <- lapply(vector_list, function(var) {
  wil_test(var, one_var_dd)
}) %>% bind_rows() %>% as.data.table()

# Plotting
plot_var_list <- unique(stat.test$`.y.`)
plot_collection <- list()
for (var in plot_var_list) {
  one_var_stat_test_filtered <- stat.test[.y. == var]
  plot <- stratified_plot(dd, one_var_stat_test_filtered, var)
  plot_collection[[var]] <- plot
}
#print(plot_collection)
#grouping them
#combined_plot <- ggarrange(plotlist = plot_collection, ncol = 2, nrow = 2, common.legend = TRUE)

####
  # Define the plot groups and their corresponding plot ranges
  # plot_groups <- list(
  #   fruits = c("delta_eAG_pt", "tbwffm_baseline", "bmi_baseline"),
  #   water = c("protein_ep_meal", "fruits_day_deficit")
  # )
  # Iterate over each plot group and display the plots within each group

  
  # for (group_name in names(plot_groups)) {
  #   group_plots <- plot_groups[[group_name]]
  #   
  #   # Create a list to store plots within the group
  #   group_plot_list <- list()
  #   
  #   for (plot_name in group_plots) {
  #     group_plot_list[[plot_name]] <- plot_collection[[plot_name]]
  #   }
  #   
  #   # Combine plots within the same group
  #   combined_group_plot <- ggarrange(plotlist = group_plot_list, ncol = 2)
  #   grand_plot_list[[group_name]] <- combined_group_plot
  #   
  #   # Use invisible() to suppress output
  #   invisible(print(combined_group_plot))
  #   
  #   cat("\n")
  # }
##########






#####
# Define the plot groups and their corresponding plot ranges
# plot_groups <- list(
#   fruits = c("delta_eAG_pt" ,      "tbwffm_baseline"  ,  "bmi_baseline" ),
#   water = c( "protein_ep_meal"   , "fruits_day_deficit")
# )
# 
# 
# # Iterate over each plot group and display the plots within each group
# for (group_name in names(plot_groups)) {
#   cat(paste("<", group_name, ">"))
#   group_plots <- plot_groups[[group_name]]
#   
#   for (plot_name in group_plots) {
#     cat(paste("plot", plot_name))
#     print(plot_collection[[plot_name]])
#   }
#   cat("\n")
# }
# 
# 
# 
# 
# #gridExtra::grid.arrange(grobs = plot_collection,ncol = 2, nrow = 3, common.legend = TRUE,legend="top")
# 
# # Combine the plots into a single grid
# combined_plot <- ggarrange(plotlist = plot_collection, ncol = 2, nrow = 2 ,common.legend = TRUE)
# #print(combined_plot)
# for (plot in seq_along(combined_plot)) {
#   print(combined_plot[[plot]])
# }
# 
# 
# 
# 


# Export the combined plot
#ggexport(combined_plot, filename = "/Users/ando/Downloads/stratified_plot_all.pdf")
#stat.test.copy <- apply(stat.test,2,as.character)
#write.csv(stat.test.copy, "/Users/ando/Downloads/gender_stratified_wilcox_by_delta_hba1c_conv_gp_stat_test.csv", row.names = FALSE, fileEncoding = "UTF-8")



# var="amh_baseline"
# dd %>%
#   group_by(gender, delta_hba1c_conv_gp) %>%
#   summarise(observation_count = sum(!is.na(.data[[var]]))) %>%
#   filter(observation_count >= 3)
# 
# genders_with_multiple_obs <- obs_by_gender_gp %>%
#   group_by(gender) %>%
#   filter(n() >= 2) %>%
#   pull(gender)
# 
# ################# dd ###################
# vector_list <- names(dd) %>% unique()
# dd[,c(703)]<-NULL
# # Apply Wilcox_Test to get stat table
# stat.test <- lapply(vector_list, function(var) {
#   wil_test(var, dd)
# }) %>% bind_rows() %>% as.data.table()
# 
# # Plotting
# plot_var_list <- unique(stat.test$`.y.`)
# plot_collection <- list()
# for (var in vector_list) {
#   one_var_stat_test_filtered <- stat.test[.y. == var]
#   plot <- stratified_plot(one_var_dd, one_var_stat_test_filtered, var)
#   plot_collection[[var]] <- plot
# }
# print(plot_collection)
# 
# 
# var="amh_baseline"
# dd %>%
#   group_by(gender, delta_hba1c_conv_gp) %>%
#   summarise(observation_count = sum(!is.na(.data[[var]]))) %>%
#   filter(observation_count >= 3)
# 
# genders_with_multiple_obs <- obs_by_gender_gp %>%
#   group_by(gender) %>%
#   filter(n() >= 2) %>%
#   pull(gender)
# 
# 