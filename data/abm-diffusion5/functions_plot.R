##################################################################
#                                                                #
# Functions for ...                                              #
# Diffusion                                                      #
#                                                                # 
# Version Diffusion-5                                            #
#                                                                #
##################################################################

library(rlang)

get_type_colors <- function() {
  c(S = "#a5d875",  # Susceptible
    I = "#eb6a6a",  # Infectious
    R = "#73bcde",  # Removed (Immune)
    D = "#000000"   # Dead
    )
}

plot_avg_SIR <- function(df) {
  # 1) determine number of replicates
  if (!"replications" %in% names(df)) {
    stop("`df` must contain a `replications` column with the total number of runs.")
  }
  n_rep <- max(df$replications, na.rm = TRUE)
  
  # 2) pivot longer on *_mean and *_sd, extract series, compute CIs
  df_long <- df |>
    pivot_longer(
      cols = matches("_(mean|sd)$"),
      names_to = c("series","stat"),
      names_pattern = "(.+?)_(mean|sd)$",
      values_to = "value"
    ) |>
    pivot_wider(names_from = stat, values_from = value) |>
    mutate(
      Type = substr(series, 1, 1),
      ci_lower = mean - 1.96 * (sd / sqrt(n_rep)),
      ci_upper = mean + 1.96 * (sd / sqrt(n_rep))
    )
  
  type_colors <- get_type_colors()
  
  # 3) plot
  ggplot(df_long, aes(x = step, y = mean, color = Type, fill = Type, group = Type)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
    geom_line() +
    scale_color_manual(values = type_colors) +
    scale_fill_manual(values  = type_colors) +
    labs(
      x = "Step",
      y = "Proportion"
    ) +
    theme_minimal()
}

plot_avg_type <- function(df,
                         show       = "all",    # "all" or a character vector of series, e.g. c("S","I")
                         group_var  = NULL,     # unquoted name to facet by
                         replications = NULL    # if NULL, tries to read df$replications
) {
  # capture the grouping variable
  group_q <- enquo(group_var)
  
  # find or check replications
  if (is.null(replications) && "replications" %in% names(df)) {
    replications <- max(df$replications, na.rm = TRUE)
  }
  if (is.null(replications) || replications <= 0) {
    stop("Please supply a positive `replications` or have a column `replications` in df.")
  }
  
  # 1) gather all cols ending in _mean/_sd
  series_cols <- grep("_(mean|sd)$", names(df), value = TRUE)
  
  df_long <- df |>
    pivot_longer(
      cols = all_of(series_cols),
      names_to   = c("series", "stat"),
      names_pattern = "(.+)_([^_]+)$",
      values_to  = "value"
    ) |>
    pivot_wider(names_from = stat, values_from = value) |>
    # extract the short series name (prefix up to first underscore)
    mutate(
      series_short = sub("_.*", "", series),
      ci_lower = mean - 1.96 * (sd / sqrt(replications)),
      ci_upper = mean + 1.96 * (sd / sqrt(replications))
    )
  
  # 2) filter which series to show
  if (!identical(show, "all")) {
    df_long <- df_long |>
      filter(series_short %in% show)
  }
  
  type_colors <- get_type_colors()
  
  # 3) build the ggplot
  p <- ggplot(df_long, aes(
    x = step,
    y = mean,
    color = series_short,
    fill  = series_short,
    group = series_short
  )) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                alpha = 0.2, color = NA) +
    geom_line() +
    scale_color_manual(values = type_colors) +
    scale_fill_manual(values  = type_colors)
  
  # 4) facet if requested
  if (!quo_is_null(group_q)) {
    p <- p + facet_wrap(vars(!!group_q))
  }
  
  # 5) labels
  p + labs(
    x     = "Step",
    y     = "Proportion",
    color = NULL,
    fill  = NULL
  ) +
    theme_minimal()
}

plot_heatmap <- function(df, 
                         x_var, 
                         y_var, 
                         fill_var, 
                         color =c ("grey95", "grey50", "grey0")
) {
  ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill_var]])) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = color[1], mid = color[2], high = color[3], 
      midpoint = mean(range(pull(df, !!ensym(fill_var)), na.rm = TRUE)),
      limits = range(pull(df, !!ensym(fill_var)), na.rm = TRUE),
      name = fill_var
    ) +
    labs(
      title = paste("Heatmap of", fill_var),
      x = x_var,
      y = y_var
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(color = "black")
    )
}

plot_SIR_shares <- function(df,
                                 x_var,
                                 y_var) {
  type_colors <- get_type_colors()
  
  col_S <- col2rgb(type_colors["S"])
  col_I <- col2rgb(type_colors["I"])
  col_R <- col2rgb(type_colors["R"])
  
  df_blended <- df |>
    rowwise() |>
    mutate(
      r = S_share_mean * col_S[1,1] + I_share_mean * col_I[1,1] + R_share_mean * col_R[1,1],
      g = S_share_mean * col_S[2,1] + I_share_mean * col_I[2,1] + R_share_mean * col_R[2,1],
      b = S_share_mean * col_S[3,1] + I_share_mean * col_I[3,1] + R_share_mean * col_R[3,1],
      fill_color = rgb(r / 255, g / 255, b / 255),
      tooltip = paste0(
        "<b>", x_var, ":</b> ", .data[[x_var]], "<br>",
        "<b>", y_var, ":</b> ", .data[[y_var]], "<br>",
        "<b>S:</b> ", round(S_share_mean, 3), "<br>",
        "<b>I:</b> ", round(I_share_mean, 3), "<br>",
        "<b>R:</b> ", round(R_share_mean, 3)
      )
    ) |>
    ungroup()
  
  ggplot(df_blended, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_tile(aes(fill = fill_color, text = tooltip),
              color = "white",
              show.legend = FALSE) +
    scale_fill_identity() +
    labs(
      title = "SIR Proportion Heatmap",
      x = x_var,
      y = y_var
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(color = "black")
    )
}

plot_SIR_shares_interactive <- function(df, x_var, y_var) {
  p <- plot_SIR_shares(df, x_var, y_var)
  
  ggplotly(p, tooltip = "text") |> layout(showlegend = FALSE)
}

plot_world_shiny <- function(world, plot_layout){
  g    <- world$agents
  type_colors <- get_type_colors()
  plot(g,
       vertex.color = type_colors[V(g)$type],
       vertex.frame.color = NA,           
       vertex.size  = 186 / sqrt(length(world$agents)) / 2,
       vertex.label = NA,
       edge.color   = "grey80",
       layout       = plot_layout())
}

