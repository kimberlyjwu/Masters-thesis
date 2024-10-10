library(dplyr)

# Assigning individuals to grids based on the total area of the grid
points_areal <- function(df){
  df %>% 
    mutate(
      points = ifelse(is.na(Total_Length_mean), 
                            0, 
                            round((TotalArea/ sum(TotalArea[!is.na(Total_Length_mean)], na.rm = TRUE)) * 3680000)),
      NumPoints = points
    )
}

# Assigning individuals to grids based on the total urban area of the grid

points_binary <- function(df){
  df %>% 
    mutate(
      urban_points = ifelse(is.na(Total_Length_mean), 
                            0, 
                            round((UrbanArea/ sum(UrbanArea[!is.na(Total_Length_mean)], na.rm = TRUE)) * 3680000)),
      NumPoints = urban_points
    )
}

# Assigning individuals to grids based on the total urban, recreational and agricultural area of the grid

points_three <- function(df){
  df %>% 
    mutate(
      urban_points = ifelse(is.na(Total_Length_mean), 
                            0, 
                            round((UrbanArea/ sum(UrbanArea[!is.na(Total_Length_mean)], na.rm = TRUE)) * 3710000 * 0.7)),
      agri_points = ifelse(is.na(Total_Length_mean), 
                           0, 
                           round((RecArea / sum(RecArea[!is.na(Total_Length_mean)], na.rm = TRUE)) * 3710000 * 0.20)),
      forest_points = ifelse(is.na(Total_Length_mean), 
                             0, 
                             round((AgriArea / sum(AgriArea[!is.na(Total_Length_mean)], na.rm = TRUE)) * 3710000 * 0.10)),
      NumPoints = urban_points + agri_points + forest_points
    )
}

# calculating the mse for the distance variable, sum the number of individuals per municipality, sum the the weighted
# total length mean and divide the total weighted length mean / the number of individuals per municipality to find the
# average distance to nearest high school per municipality

calculate_mse <- function(df, truth) {
  library(dplyr)
  
  # Replace NA values with 0
  df$Total_Length_mean <- ifelse(is.na(df$Total_Length_mean), 0, df$Total_Length_mean)
  df$NumPoints_2 <- ifelse(is.na(df$NumPoints_2), 0, df$NumPoints_2)
  
  df$Weighted_TL <- df$Total_Length_mean * df$NumPoints_2
  
  sum_numpoints <- df %>%
    group_by(GM_NAAM) %>%
    summarise(sum_value = sum(NumPoints_2), .groups = "drop")
  
  
  sum_weighted_TL <- df %>%
    group_by(GM_NAAM) %>%
    summarise(sum_value = sum(Weighted_TL), .groups = "drop")
  
  # join the summaries and calculate the ratio
  result_data <- sum_numpoints %>%
    inner_join(sum_weighted_TL, by = "GM_NAAM", suffix = c(".numpoints", ".weightedTL")) %>%
    mutate(ratio = sum_value.weightedTL / sum_value.numpoints) %>%
    dplyr::select(GM_NAAM, ratio)
  
  mse_points <- data.frame(
    mse = (result_data$ratio - truth$Total_Length_mean)^2,
    me =(result_data$ratio - truth$Total_Length_mean),
    GM_NAAM = truth$GM_NAAM
  )
  
  # Return the mean of the MSE
  print(mean(mse_points$mse))
  print(sqrt(mean(mse_points$mse)))
  return(mse_points)
}

# mse for the count variable: make sure that the total number of individuals in the province match up to the truth, then sum

calculate_metrics <- function(point_data, df, truth_numpoints, truth) {
  change <- sum(point_data$AANTPERS_v) / sum(df$NumPoints_2)
  
  df$NumPoints_3 <- round(df$NumPoints_2 * change)
  
  numpoints_gem <- df %>%
    group_by(GM_NAAM) %>%
    summarise(sum_value = sum(NumPoints_3))
  
  mse_points <- data.frame(
    mse = (numpoints_gem$sum_value - truth_numpoints$Count)^2,
    me = (numpoints_gem$sum_value - truth_numpoints$Count),
    GM_NAAM = truth$GM_NAAM
  )
  
  print(sum(mse_points$mse) / 50)
  print(sqrt(sum(mse_points$mse) / 50))
  print(sum(df$NumPoints_3))
  return(mse_points)
}

# for analysis

# confidence interval using the t-distribution
calculate_ci = function(data, confidence_level = 0.95) {
  std_error = sd(data) / sqrt(length(data))
  
  df = length(data) - 1
  
  t_value = qt((1 - confidence_level) / 2 + confidence_level, df)
  
  margin_of_error = t_value * std_error
  
  ci_lower = mean(data) - margin_of_error
  ci_upper = mean(data) + margin_of_error
  
  return(c(ci_lower, ci_upper))
}

# includes grouping by position
avg_rmse <- function(dataframes) {
  results <- lapply(names(dataframes), function(df_name) {
    df <- dataframes[[df_name]]
    
    df %>%
      group_by(dataframe = df_name, size, position) %>%
      summarize(avg_rmse = mean(rmse),
                ci = list(calculate_ci(rmse))) %>%
      mutate(ci_lower = sapply(ci, `[`, 1),
             ci_upper = sapply(ci, `[`, 2)) %>%
      select(-ci)
  })
  
  return(bind_rows(results))
}

# only includes grouping by size
avg_avg_rmse <- function(dataframes) {
  results <- lapply(names(dataframes), function(df_name) {
    df <- dataframes[[df_name]]
    
    df %>%
      group_by(dataframe = df_name, size) %>%
      summarize(avg_rmse = mean(rmse),
                ci = list(calculate_ci(rmse))) %>%
      mutate(ci_lower = sapply(ci, `[`, 1),
             ci_upper = sapply(ci, `[`, 2)) %>%
      select(-ci)
  })
  
  return(bind_rows(results))
}

# to calculate cv based on the estimations

calculate_cv <- function(df) {
  cv_df <- df %>%
    group_by(GM_NAAM) %>%
    summarise(mean_value = mean(sum_value, na.rm = TRUE),
              sd_value = sd(sum_value, na.rm = TRUE)) %>%
    mutate(cv = (sd_value / mean_value)*100)
  
  return(cv_df)
}

calculate_cv2 <- function(df) {
  cv_df <- df %>%
    group_by(GM_NAAM) %>%
    summarise(mean_value = mean(ratio, na.rm = TRUE),
              sd_value = sd(ratio, na.rm = TRUE)) %>%
    mutate(cv = (sd_value / mean_value)*100)
  
  return(cv_df)
}

# to calculate the cv based on the rmse (not used)

calc_cv <- function(df) {
  cv_df <- df %>%
    group_by(GM_NAAM) %>%
    summarise(mean_rmse = mean(rmse, na.rm = TRUE),
              sd_rmse = sd(rmse, na.rm = TRUE)) %>%
    mutate(cv = (sd_rmse / mean_rmse)*100)
  
  return(cv_df)
}

# calculate only the estimations

calculate_distance <- function(df) {
  library(dplyr)
  
  # Replace NA values with 0
  df$Total_Length_mean <- ifelse(is.na(df$Total_Length_mean), 0, df$Total_Length_mean)
  df$NumPoints_2 <- ifelse(is.na(df$NumPoints_2), 0, df$NumPoints_2)
  
  df$Weighted_TL <- df$Total_Length_mean * df$NumPoints_2
  
  sum_numpoints <- df %>%
    group_by(GM_NAAM) %>%
    summarise(sum_value = sum(NumPoints_2), .groups = "drop")
  
  
  sum_weighted_TL <- df %>%
    group_by(GM_NAAM) %>%
    summarise(sum_value = sum(Weighted_TL), .groups = "drop")
  
  # Join the summaries and calculate the ratio
  result_data <- sum_numpoints %>%
    inner_join(sum_weighted_TL, by = "GM_NAAM", suffix = c(".numpoints", ".weightedTL")) %>%
    mutate(ratio = sum_value.weightedTL / sum_value.numpoints) %>%
    dplyr::select(GM_NAAM, ratio)
  
  return(result_data)
}

calculate_points <- function(point_data, df) {
  change <- sum(point_data$AANTPERS_v) / sum(df$NumPoints_2)
  
  df$NumPoints_3 <- round(df$NumPoints_2 * change)
  
  numpoints_gem <- df %>%
    group_by(GM_NAAM) %>%
    summarise(sum_value = sum(NumPoints_3))
  
  return(numpoints_gem)
}



