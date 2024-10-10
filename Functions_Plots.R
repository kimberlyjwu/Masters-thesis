library(gridExtra)
library(ggplot2)
generate_plot <- function(df, xlim_val, title) {
  p <- ggplot(df, aes(x = avg_rmse, y = position)) +
    geom_line() +
    geom_point() +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
    labs(x = "Average RMSE", y = "Position") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    ggtitle(title) +
    facet_wrap(~ dataframe, nrow = 2, scales = "free", labeller = as_labeller(setNames(subplot_titles, unique(df$dataframe)))) +
    coord_cartesian(xlim = xlim_val)
  
  print(p)
}

generate_plot2 <- function(df, xlim_val, title) {
  p <- ggplot(df, aes(x = avg_rmse, y = factor(method))) +
    geom_line() +
    geom_point() +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
    labs(x = "Average RMSE", y = "Method") +
    theme_minimal() +
    facet_wrap(~ size, nrow = 2, scales = "free") +
    coord_cartesian(xlim = xlim_val)
  
  return(p)
}


map_plot_me_points <- function(data, top_values = TRUE,  x_offset = -20000, y_offset = 5000, title = "RMSE Map Plot") {
  library(ggrepel)
  if (top_values) {
    selected_municipalities <- data %>%
      arrange(desc(avg_me)) %>%
      slice(1:5)
  } else {
    selected_municipalities <- data %>%
      arrange(avg_me) %>%
      slice(1:5)
  }
  
  selected_municipalities <- selected_municipalities %>%
    st_as_sf() %>%
    mutate(centroid = st_centroid(geom),
           x = st_coordinates(centroid)[, 1],
           y = st_coordinates(centroid)[, 2])
  
  ggplot(data = data) +
    geom_sf(aes(fill = avg_me)) + 
    geom_sf(data = selected_municipalities, color = "red", fill = NA) +  # Highlight top 5
    
    geom_label_repel(data = selected_municipalities, aes(x = x, y = y, label = avg_me),
                     fill = "white", color = "black", fontface = "bold", 
                     size = 3, label.padding = unit(0.3, "lines"), 
                     nudge_x = 0, nudge_y = 0) +
    
    geom_label_repel(data = selected_municipalities, aes(x = x, y = y, label = GM_NAAM),
                     fill = "lightgreen", color = "black", fontface = "bold",
                     size = 3, label.padding = unit(0.3, "lines"), 
                     nudge_x = x_offset , nudge_y = y_offset) +
    
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Average Error",
                        limits = c(-150000, 60000), oob = scales::squish) +
    labs(title = title) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}

map_plot_me <- function(data, top_values = TRUE,  x_offset = -20000, y_offset = 5000, title = "RMSE Map Plot") {
  library(ggrepel)
  if (top_values) {
    selected_municipalities <- data %>%
      arrange(desc(avg_me)) %>%
      slice(1:5)
  } else {
    selected_municipalities <- data %>%
      arrange(avg_me) %>%
      slice(1:5)
  }
  
  selected_municipalities <- selected_municipalities %>%
    st_as_sf() %>%
    mutate(centroid = st_centroid(geom),
           x = st_coordinates(centroid)[, 1],
           y = st_coordinates(centroid)[, 2])
  
  ggplot(data = data) +
    geom_sf(aes(fill = avg_me)) + 
    geom_sf(data = selected_municipalities, color = "red", fill = NA) +  # Highlight top 5
    
    geom_label_repel(data = selected_municipalities, aes(x = x, y = y, label = avg_me),
                     fill = "white", color = "black", fontface = "bold", 
                     size = 3, label.padding = unit(0.3, "lines"), 
                     nudge_x = 0, nudge_y = 0) +
    
    geom_label_repel(data = selected_municipalities, aes(x = x, y = y, label = GM_NAAM),
                     fill = "lightgreen", color = "black", fontface = "bold",
                     size = 3, label.padding = unit(0.3, "lines"), 
                     nudge_x = x_offset , nudge_y = y_offset) +
    
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Average Error",
                        limits = c(-200, 200), oob = scales::squish) +
    labs(title = title) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}



calc_summary <- function(data) {
  stats <- data %>%
    group_by(GM_NAAM) %>%
    summarize(
      avg_me = round(mean(me, na.rm = TRUE)),
      avg_rmse = round(mean(rmse, na.rm = TRUE)),  
      sd_rmse = round(sd(rmse, na.rm = TRUE),2), 
      cv_rmse = round(((sd_rmse / avg_rmse) * 100), 2)
    )
  return(stats)
}

map_plot_cv <- function(data, top_values = TRUE,  x_offset = -20000, y_offset = 5000, title = "RMSE Map Plot") {
  library(ggrepel)
  if (top_values) {
    selected_municipalities <- data %>%
      arrange(desc(cv_rmse)) %>%
      slice(1:5)
  } else {
    selected_municipalities <- data %>%
      arrange(cv_rmse) %>%
      slice(1:5)
  }
  
  selected_municipalities <- selected_municipalities %>%
    st_as_sf() %>%
    mutate(centroid = st_centroid(geom),
           x = st_coordinates(centroid)[, 1],
           y = st_coordinates(centroid)[, 2])
  
  ggplot(data = data) +
    geom_sf(aes(fill = cv_rmse)) + 
    geom_sf(data = selected_municipalities, color = "red", fill = NA) +  # Highlight top 5
    
    geom_label_repel(data = selected_municipalities, aes(x = x, y = y, label = cv_rmse),
                     fill = "white", color = "black", fontface = "bold", 
                     size = 3, label.padding = unit(0.3, "lines"), 
                     nudge_x = 0, nudge_y = 0) +
    
    geom_label_repel(data = selected_municipalities, aes(x = x, y = y, label = GM_NAAM),
                     fill = "lightgreen", color = "black", fontface = "bold",
                     size = 3, label.padding = unit(0.3, "lines"), 
                     nudge_x = x_offset , nudge_y = y_offset) +
    
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Coefficient of Variation (CV)") +
    labs(title = title) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}


map_plot_se <- function(data, top_values = TRUE,  x_offset = -20000, y_offset = 5000, title = "RMSE Map Plot") {
  library(ggrepel)
  if (top_values) {
    selected_municipalities <- data %>%
      arrange(desc(sd_rmse)) %>%
      slice(1:5)
  } else {
    selected_municipalities <- data %>%
      arrange(sd_rmse) %>%
      slice(1:5)
  }
  
  selected_municipalities <- selected_municipalities %>%
    st_as_sf() %>%
    mutate(centroid = st_centroid(geom),
           x = st_coordinates(centroid)[, 1],
           y = st_coordinates(centroid)[, 2])
  
  ggplot(data = data) +
    geom_sf(aes(fill = sd_rmse)) + 
    geom_sf(data = selected_municipalities, color = "red", fill = NA) +  # Highlight top 5
    
    geom_label_repel(data = selected_municipalities, aes(x = x, y = y, label = sd_rmse),
                     fill = "white", color = "black", fontface = "bold", 
                     size = 3, label.padding = unit(0.3, "lines"), 
                     nudge_x = 0, nudge_y = 0) +
    
    geom_label_repel(data = selected_municipalities, aes(x = x, y = y, label = GM_NAAM),
                     fill = "lightgreen", color = "black", fontface = "bold",
                     size = 3, label.padding = unit(0.3, "lines"), 
                     nudge_x = x_offset , nudge_y = y_offset) +
    
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Standard Deviation") +
    labs(title = title) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}

cv_plot = function(data, ylimit, title){
  ggplot(data, aes(x = reorder(GM_NAAM, cv), y = cv)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = title,
         x = "Municipality",
         y = "CV (%)") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 5),
          plot.title = element_text(hjust = 0.5)) +
    ylim(ylimit)
}

