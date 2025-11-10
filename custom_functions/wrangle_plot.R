# Data Wrangling and Plotting Functions

#' Create summary statistics for individual participants
#' @param data Data frame with experimental data
#' @param id_var Participant ID variable
#' @param group_vars Character vector of grouping variables
#' @param response_var Response variable to summarize
#' @param scale Scaling factor (e.g., 100 for percentages)
summarise_individual <- function(
    data,
    id_var,
    group_vars,
    response_var,
    scale = 1
) {
  data |>
    group_by({{ id_var }}, across(all_of(group_vars))) |>
    summarise(
      mean_val = mean({{ response_var }}, na.rm = TRUE) * scale,
      sd_val = sd({{ response_var }}, na.rm = TRUE) * scale,
      .groups = "drop"
    )
}

#' Create summary statistics at the group level directly from raw data
#' @param data Raw data frame with trial-level data
#' @param group_vars Character vector of grouping variables
#' @param response_var Response variable to summarize
#' @param id_var Participant ID variable
#' @param scale Scaling factor (e.g., 100 for percentages)
summarise_group <- function(
    data,
    id_var = pid,
    group_vars,
    response_var,
    scale = 1
) {
  data |>
    group_by(across(all_of(group_vars))) |>
    summarise(
      n = n_distinct({{ id_var }}),
      mean_val = mean({{ response_var }}, na.rm = TRUE) * scale,
      sd_val = sd({{ response_var }}, na.rm = TRUE) * scale,
      sem_val = sd_val / sqrt(n),
      .groups = "drop"
    )
}

#' Count trials with flexible grouping
#' @param data Data frame with trial data
#' @param ... Variables to group by
count_trials <- function(data, ...) {
  data |>
    group_by(...) |>
    tally() |>
    arrange(n)
}

# Visualization Functions

#' Create a simple, readable theme for consistent plots
create_clean_theme <- function(base_size = 28) {
  theme_bw(base_size = base_size) +
    theme(
      text = element_text(face = "bold"),
      title = element_text(face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
}

#' Create a raincloud plot using the gghalves package
#' @param data_ind Individual-level summary data
#' @param data_group Group-level summary data
#' @param x_var Variable to plot on y-axis (factors, conditions etc.)
#' @param y_var Variable to plot on x-axis (data values)
#' @param facet_var Variable to facet by (optional). Can be a single variable name or a vector of variable names for facet_grid
#' @param facet_rows Variable(s) to use for facet grid rows (optional, only used if facet_type = "grid")
#' @param facet_cols Variable(s) to use for facet grid columns (optional, only used if facet_type = "grid")
#' @param facet_type Type of faceting: "wrap" (default) or "grid"
#' @param facet_scales Scales parameter for faceting ("fixed", "free", "free_x", "free_y")
#' @param x_label Label for group categories axis
#' @param y_label Label for data values axis
#' @param title Plot title
#' @param alpha Transparency of the distributions
#' @param violin_side "l" or "r"
#' @param point_side "l" or "r"
#' @param point_spread point width or height for jitterring
#' @param flip Whether to flip the coordinates (TRUE = vertical orientation, FALSE = horizontal)
#' @param trim Whether to trim the density to the range of the data (like trim in geom_violin)
#' @param value_limits Vector of min and max values for the data value axis
#' @param value_breaks Vector of break points for the data value axis
#' @param x_spacing controls white space at the edge of the x axis
plot_rain <- function(
    data_ind,
    data_group,
    x_var = "GROUP",
    y_var = "mean_val",
    facet_var = NULL,
    facet_rows = NULL,
    facet_cols = NULL,
    facet_type = "wrap",
    facet_scales = "free_x",
    x_label = "Training Condition",
    y_label = "Score",
    title = NULL,
    alpha = 0.5,
    violin_side = "r",
    point_side = "l",
    point_spread = .05,
    flip = TRUE,
    trim = TRUE,
    value_limits = NULL,
    value_breaks = NULL,
    x_spacing = c(0.4, 0.4)
) {
  # Create base plot - always in vertical orientation
  p <- ggplot(
    data_ind,
    aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[x_var]])
  )
  
  # Add the half_violin plot
  p <- p +
    gghalves::geom_half_violin(
      side = violin_side,
      trim = trim,
      alpha = alpha
    )
  
  # Add points with jitter
  p <- p +
    gghalves::geom_half_point(
      aes(colour = .data[[x_var]]),
      side = point_side,
      alpha = alpha,
      size = 2,
      transformation = position_jitter(width = point_spread, height = 0)
    )
  
  # Add mean points and error bars
  p <- p +
    geom_point(
      data = data_group,
      size = 3,
      colour = "black"
    ) +
    geom_errorbar(
      data = data_group,
      aes(ymin = .data[[y_var]] - sem_val, ymax = .data[[y_var]] + sem_val),
      colour = "black",
      width = 0.2,
      linewidth = 1
    )
  
  # Handle line grouping for different faceting scenarios
  if (facet_type == "grid" && (!is.null(facet_rows) || !is.null(facet_cols))) {
    # For facet_grid, create grouping variable from all faceting variables
    group_vars <- c(facet_rows, facet_cols)
    group_vars <- group_vars[!is.null(group_vars)]
    
    if (length(group_vars) == 1) {
      p <- p +
        geom_line(
          data = data_group,
          aes(group = .data[[group_vars[1]]]),
          linewidth = 0.7,
          color = "black"
        )
    } else if (length(group_vars) > 1) {
      # Create interaction for multiple grouping variables
      p <- p +
        geom_line(
          data = data_group,
          aes(group = interaction(!!!syms(group_vars))),
          linewidth = 0.7,
          color = "black"
        )
    }
  } else if (!is.null(facet_var)) {
    # Original facet_wrap behavior
    if (length(facet_var) == 1) {
      p <- p +
        geom_line(
          data = data_group,
          aes(group = .data[[facet_var]]),
          linewidth = 0.7,
          color = "black"
        )
    } else {
      # Multiple variables for facet_wrap
      p <- p +
        geom_line(
          data = data_group,
          aes(group = interaction(!!!syms(facet_var))),
          linewidth = 0.7,
          color = "black"
        )
    }
  } else {
    p <- p +
      geom_line(
        data = data_group,
        group = 1,
        linewidth = 0.7,
        color = "black"
      )
  }
  
  # Select color palette
  p <- p +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2")
  
  # Add faceting based on type
  if (facet_type == "grid") {
    if (!is.null(facet_rows) || !is.null(facet_cols)) {
      # Build facet_grid formula
      row_formula <- if (!is.null(facet_rows)) {
        if (length(facet_rows) == 1) {
          facet_rows[1]
        } else {
          paste(facet_rows, collapse = " + ")
        }
      } else {
        "."
      }
      
      col_formula <- if (!is.null(facet_cols)) {
        if (length(facet_cols) == 1) {
          facet_cols[1]
        } else {
          paste(facet_cols, collapse = " + ")
        }
      } else {
        "."
      }
      
      formula_string <- paste(row_formula, "~", col_formula)
      p <- p + facet_grid(as.formula(formula_string), scales = facet_scales)
    }
  } else if (facet_type == "wrap" && !is.null(facet_var)) {
    # facet_wrap can handle multiple variables
    p <- p + facet_wrap(vars(!!!syms(facet_var)), scales = facet_scales)
  }
  
  # Add axis scaling with limits if provided
  if (!is.null(value_limits) || !is.null(value_breaks)) {
    p <- p +
      scale_y_continuous(
        limits = value_limits,
        breaks = value_breaks,
        expand = expansion(mult = 0.05, add = c(0.1, 0.1))
      ) +
      scale_x_discrete(expand = expansion(mult = 0.01, add = x_spacing))
  } else {
    p <- p +
      scale_y_continuous(
        expand = expansion(mult = 0.05, add = c(0.1, 0.1))
      ) +
      scale_x_discrete(expand = expansion(mult = 0.01, add = x_spacing))
  }
  
  # Add labels
  p <- p +
    labs(
      title = title,
      x = x_label,
      y = y_label
    )
  
  # Apply coord_flip if needed
  if (!flip) {
    p <- p + coord_flip()
  }
  
  # Apply theme updates to remove stuff
  p <- p +
    theme(
      # axis.text.x = element_blank(),
      # axis.ticks.x = element_blank(),
      # legend.title = element_blank(),
      legend.position = "none"
    )

  return(p)
}
# Create an LBA schematic plot - single panel with two accumulators
library(tidyverse)

# Single panel with two accumulators
create_lba_schematic <- function() {
  # Set common end time (both arrows end at same x position)
  common_end_time <- 1.8
  
  # Accumulator 1 parameters
  drift1 <- 0.4
  start1 <- 0.2
  end_evidence1 <- start1 + drift1 * common_end_time
  
  # Accumulator 2 parameters
  drift2 <- 0.2
  start2 <- 0.2
  end_evidence2 <- start2 + drift2 * common_end_time
  
  # Calculate arrow start point for accumulator 2 (very short distance before end)
  arrow_start_distance <- 0.05
  acc2_arrow_start_x <- common_end_time - arrow_start_distance
  acc2_arrow_start_y <- start2 + drift2 * acc2_arrow_start_x
  
  ggplot() +
    # Starting point region (to the left of y-axis)
    annotate("rect", xmin = -0.35, xmax = -0.05, ymin = 0, ymax = 0.35,
             fill = "steelblue", alpha = 0.3) +
    # Axes with arrows
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1.15), 
                 linewidth = 0.7, color = "black",
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    geom_segment(aes(x = 0, y = 0, xend = 2.7, yend = 0), 
                 linewidth = 0.7, color = "black",
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    # Threshold line (starting at x=0)
    geom_segment(aes(x = 0, y = 1, xend = 2.7, yend = 1), 
                 linetype = "dashed", linewidth = 0.8, color = "black") +
    # Accumulator 1 line with arrow
    geom_segment(aes(x = 0, y = start1, xend = common_end_time, yend = end_evidence1),
                 linewidth = 1.2, color = "#1B9E77FF", linetype = "solid",
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    # Accumulator 2 - dotted line WITHOUT arrow
    geom_segment(aes(x = 0, y = start2, xend = common_end_time, yend = end_evidence2),
                 linewidth = 1.2, color = "#D95F02FF", linetype = "dotted") +
    # Accumulator 2 - very short solid arrow at the end
    geom_segment(aes(x = acc2_arrow_start_x, y = acc2_arrow_start_y, 
                     xend = common_end_time, yend = end_evidence2),
                 linewidth = 1.2, color = "#D95F02FF", linetype = "solid",
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    # Manual axis labels as annotations
    annotate("text", x = -0.15, y = 0.6, label = "Evidence", 
             size = 4, fontface = "bold", angle = 90) +
    annotate("text", x = 1.3, y = -0.1, label = "Time", 
             size = 4, fontface = "bold") +
    # Parameter labels (bold + italic)
    annotate("text", x = -0.2, y = 0.175, label = "Start point (A)", 
             size = 4, fontface = "bold.italic", angle = 90) +
    annotate("text", x = 1.5, y = 1.05, label = "Threshold (b)", 
             size = 4, fontface = "bold.italic", hjust = 1) +
    annotate("text", x = common_end_time * 0.4, y = start1 + drift1 * common_end_time * 0.4 + 0.2, 
             label = "Drift rate (v)", size = 4, fontface = "bold.italic") +
    # Accumulator labels at arrow tips
    annotate("text", x = common_end_time + 0.05, y = end_evidence1, 
             label = "Accumulator 1", size = 4, fontface = "bold.italic", 
             color = "black", hjust = 0) +
    annotate("text", x = common_end_time + 0.05, y = end_evidence2, 
             label = "Accumulator 2", size = 4, fontface = "bold.italic", 
             color = "black", hjust = 0) +
    # Styling
    scale_x_continuous(breaks = NULL, limits = c(-0.5, 2.8)) +
    scale_y_continuous(breaks = NULL, limits = c(-0.1, 1.2)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.margin = margin(0, 0, 0, 0)
    )
}

# Create the plot
create_lba_schematic()