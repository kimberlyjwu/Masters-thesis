# from book Negative Binomial Regression (not used)
modelfit = function(x){
  obs = x$df.null + 1
  aic = x$aic
  xvars = x$rank
  rdof = x$df.residual
  aic_n = aic/obs
  ll = xvars - aic/2
  bic_r = x$deviance - (rdof * log(obs))
  bic_l = -2*ll + xvars * log(obs)
  bic_qh = -2*(ll - xvars * log(xvars))/obs
  c(AICn = aic_n, AIC = aic, BICqh = bic_qh, BICl = bic_l)
}



calculate_mse_d <- function(df, truth) {
  library(dplyr)
  df$Total_Length_mean <- ifelse(is.na(df$Total_Length_mean), 0, df$Total_Length_mean)
  
  df$Weighted_TL <- df$Total_Length_mean * df$NumPoints
  
  sum_numpoints <- df %>%
    group_by(GM_NAAM) %>%
    summarise(sum_numpoints = sum(NumPoints), .groups = "drop")
  
  sum_weighted_TL <- df %>%
    group_by(GM_NAAM) %>%
    summarise(sum_weighted_TL = sum(Weighted_TL), .groups = "drop")
  
  # Join the summaries and calculate the ratio
  result_data <- sum_numpoints %>%
    inner_join(sum_weighted_TL, by = "GM_NAAM") %>%
    mutate(ratio = ifelse(sum_numpoints == 0, NA, sum_weighted_TL / sum_numpoints)) %>%
    dplyr::select(GM_NAAM, ratio)
  
  mse_points <- data.frame(
    mse = (result_data$ratio - truth$Total_Length_mean)^2,
    rmse = sqrt((result_data$ratio - truth$Total_Length_mean)^2),
    me =(result_data$ratio - truth$Total_Length_mean),
    GM_NAAM = truth$GM_NAAM
  )
  
  # Return the mean of the MSE
  print(mean(mse_points$mse))
  print(mean(mse_points$rmse))
  return(mse_points)
}


calculate_metrics_d <- function(point_data, df, truth_numpoints, truth) {
  change <- sum(point_data$AANTPERS_v) / sum(df$NumPoints)
  
  df$NumPoints_1 <- round(df$NumPoints * change)
  
  numpoints_gem <- df %>%
    group_by(GM_NAAM) %>%
    summarise(sum_value = sum(NumPoints_1))
  
  mse_points <- data.frame(
    mse = (numpoints_gem$sum_value - truth_numpoints$Count)^2,
    rmse = sqrt((numpoints_gem$sum_value - truth_numpoints$Count)^2),
    me = (numpoints_gem$sum_value - truth_numpoints$Count),
    GM_NAAM = truth$GM_NAAM
  )
  
  print(mean(mse_points$mse))
  print(mean(mse_points$rmse))
  return(mse_points)
}


# for the results

calculate_distance_d <- function(df, truth) {
  library(dplyr)
  df$Total_Length_mean <- ifelse(is.na(df$Total_Length_mean), 0, df$Total_Length_mean)
  
  df$Weighted_TL <- df$Total_Length_mean * df$NumPoints
  
  sum_numpoints <- df %>%
    group_by(GM_NAAM) %>%
    summarise(sum_numpoints = sum(NumPoints), .groups = "drop")
  
  sum_weighted_TL <- df %>%
    group_by(GM_NAAM) %>%
    summarise(sum_weighted_TL = sum(Weighted_TL), .groups = "drop")
  
  # Join the summaries and calculate the ratio
  result_data <- sum_numpoints %>%
    inner_join(sum_weighted_TL, by = "GM_NAAM") %>%
    mutate(ratio = ifelse(sum_numpoints == 0, NA, sum_weighted_TL / sum_numpoints)) %>%
    dplyr::select(GM_NAAM, ratio)
  
  return(result_data)
}

calculate_points_d <- function(point_data, df, truth_numpoints, truth) {
  change <- sum(point_data$AANTPERS_v) / sum(df$NumPoints)
  
  df$NumPoints_1 <- round(df$NumPoints * change)
  
  numpoints_gem <- df %>%
    group_by(GM_NAAM) %>%
    summarise(sum_value = sum(NumPoints_1))
  
  return(numpoints_gem)
}

