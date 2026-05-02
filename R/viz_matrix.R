if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "record_id",
    "tactic",
    "technique_id",
    "technique_label",
    "technique_name",
    "tool_count",
    "total"
  ))
}

.viz_require_columns <- function(data, required_columns, data_name) {
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      sprintf("%s is missing required columns: %s", data_name, paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }
}

.viz_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#e8dcc7", linewidth = 0.35),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", size = 16, color = "#1f1f1f"),
      plot.subtitle = ggplot2::element_text(color = "#6d6258", margin = ggplot2::margin(b = 10)),
      axis.text = ggplot2::element_text(color = "#453d36"),
      axis.title = ggplot2::element_text(color = "#564d45"),
      legend.title = ggplot2::element_text(color = "#564d45"),
      legend.text = ggplot2::element_text(color = "#453d36"),
      plot.background = ggplot2::element_rect(fill = "#fbf7ef", color = NA),
      panel.background = ggplot2::element_rect(fill = "#fbf7ef", color = NA)
    )
}

plot_mitre_heatmap <- function(visualization_matrix, top_n_techniques = 25L) {
  if (!is.data.frame(visualization_matrix)) {
    stop("visualization_matrix must be a data frame.", call. = FALSE)
  }

  .viz_require_columns(
    visualization_matrix,
    c("record_id", "technique_id", "technique_name", "tactic"),
    "visualization_matrix"
  )

  if (nrow(visualization_matrix) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::labs(title = "MITRE heatmap", subtitle = "No matrix rows available")
    )
  }

  heatmap_source <- unique(visualization_matrix[c("record_id", "tactic", "technique_id", "technique_name")])
  heatmap_data <- stats::aggregate(
    record_id ~ tactic + technique_id + technique_name,
    data = heatmap_source,
    FUN = length
  )
  names(heatmap_data)[names(heatmap_data) == "record_id"] <- "tool_count"
  heatmap_order <- order(-heatmap_data[["tool_count"]], heatmap_data[["tactic"]], heatmap_data[["technique_name"]])
  heatmap_data <- heatmap_data[heatmap_order, , drop = FALSE]

  if (!is.null(top_n_techniques) && nrow(heatmap_data) > top_n_techniques) {
    top_techniques <- stats::aggregate(
      tool_count ~ technique_id + technique_name,
      data = heatmap_data,
      FUN = sum
    )
    top_order <- order(-top_techniques[["tool_count"]], top_techniques[["technique_id"]], top_techniques[["technique_name"]])
    top_techniques <- utils::head(top_techniques[top_order, , drop = FALSE], as.integer(top_n_techniques))
    top_keys <- paste(top_techniques[["technique_id"]], top_techniques[["technique_name"]], sep = "||")
    heatmap_keys <- paste(heatmap_data[["technique_id"]], heatmap_data[["technique_name"]], sep = "||")
    heatmap_data <- heatmap_data[heatmap_keys %in% top_keys, , drop = FALSE]
  }

  tactic_totals <- stats::aggregate(tool_count ~ tactic, data = heatmap_data, FUN = sum)
  tactic_levels <- tactic_totals[["tactic"]][order(tactic_totals[["tool_count"]])]
  heatmap_data[["tactic"]] <- factor(heatmap_data[["tactic"]], levels = tactic_levels)

  heatmap_data[["technique_label"]] <- paste0(heatmap_data[["technique_id"]], " ", heatmap_data[["technique_name"]])
  technique_totals <- stats::aggregate(tool_count ~ technique_label, data = heatmap_data, FUN = sum)
  technique_levels <- technique_totals[["technique_label"]][order(technique_totals[["tool_count"]])]
  heatmap_data[["technique_label"]] <- factor(heatmap_data[["technique_label"]], levels = technique_levels)

  ggplot2::ggplot(heatmap_data, ggplot2::aes_string(x = "tactic", y = "technique_label", fill = "tool_count")) +
    ggplot2::geom_tile(color = "#f3efe6", linewidth = 0.4) +
    ggplot2::scale_fill_gradient(low = "#f5e6bf", high = "#b44f24") +
    ggplot2::labs(
      title = "MITRE ATT&CK Heatmap",
      subtitle = "Количество релевантных инструментов на tactic/technique",
      x = NULL,
      y = NULL,
      fill = "Tools"
    ) +
    .viz_plot_theme() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 25, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 9)
    )
}

plot_tactic_distribution <- function(visualization_matrix) {
  if (!is.data.frame(visualization_matrix)) {
    stop("visualization_matrix must be a data frame.", call. = FALSE)
  }

  .viz_require_columns(visualization_matrix, c("record_id", "tactic"), "visualization_matrix")

  if (nrow(visualization_matrix) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::labs(title = "MITRE tactics", subtitle = "No matrix rows available")
    )
  }

  distribution_source <- unique(visualization_matrix[c("record_id", "tactic")])
  distribution <- stats::aggregate(record_id ~ tactic, data = distribution_source, FUN = length)
  names(distribution)[names(distribution) == "record_id"] <- "tool_count"
  distribution <- distribution[order(distribution[["tool_count"]]), , drop = FALSE]
  distribution[["tactic_label"]] <- factor(distribution[["tactic"]], levels = distribution[["tactic"]])

  ggplot2::ggplot(distribution, ggplot2::aes_string(x = "tactic_label", y = "tool_count", fill = "tool_count")) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_gradient(low = "#d8ecd5", high = "#1b6b52") +
    ggplot2::labs(
      title = "MITRE Tactic Distribution",
      subtitle = "Сколько инструментов связано с каждой тактикой",
      x = NULL,
      y = "Tools"
    ) +
    .viz_plot_theme()
}