if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("tool_count"))
}

.viz_tools_require_columns <- function(data, required_columns, data_name) {
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      sprintf("%s is missing required columns: %s", data_name, paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }
}

.viz_tools_plot_theme <- function() {
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

plot_tools_by_source <- function(visualization_tools) {
  if (!is.data.frame(visualization_tools)) {
    stop("visualization_tools must be a data frame.", call. = FALSE)
  }

  .viz_tools_require_columns(visualization_tools, c("source", "entity_type"), "visualization_tools")

  if (nrow(visualization_tools) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::labs(title = "Sources", subtitle = "No visualization rows available")
    )
  }

  source_data <- visualization_tools |>
    dplyr::transmute(
      source = as.character(.data[["source"]]),
      entity_type = as.character(.data[["entity_type"]])
    ) |>
    dplyr::count(.data[["source"]], .data[["entity_type"]], name = "tool_count") |>
    tibble::as_tibble()

  ggplot2::ggplot(source_data, ggplot2::aes_string(x = "source", y = "tool_count", fill = "entity_type")) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(
      title = "Tools by Source",
      subtitle = "Распределение релевантных инструментов по источникам",
      x = NULL,
      y = "Tools",
      fill = "Entity type"
    ) +
    .viz_tools_plot_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 18, hjust = 1))
}

plot_top_tools <- function(visualization_tools, top_n = 15L) {
  if (!is.data.frame(visualization_tools)) {
    stop("visualization_tools must be a data frame.", call. = FALSE)
  }

  .viz_tools_require_columns(
    visualization_tools,
    c("assessed_name", "visualization_score", "entity_type"),
    "visualization_tools"
  )

  if (nrow(visualization_tools) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::labs(title = "Top tools", subtitle = "No visualization rows available")
    )
  }

  ranking_order <- order(-visualization_tools[["visualization_score"]], visualization_tools[["assessed_name"]])
  ranking <- visualization_tools[ranking_order, , drop = FALSE]
  ranking <- utils::head(ranking, as.integer(top_n))
  ranking <- ranking[order(ranking[["visualization_score"]]), , drop = FALSE]
  ranking[["assessed_name_ordered"]] <- factor(ranking[["assessed_name"]], levels = ranking[["assessed_name"]])

  ggplot2::ggplot(ranking, ggplot2::aes_string(x = "visualization_score", y = "assessed_name_ordered", fill = "entity_type")) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", ranking[["visualization_score"]])),
      hjust = -0.15,
      size = 3.4,
      color = "#453d36"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::expand_limits(x = max(ranking$visualization_score, na.rm = TRUE) + 0.08) +
    ggplot2::labs(
      title = "Top Ranked Tools",
      subtitle = "Порядок для верхней части визуализации после LLM",
      x = "Visualization score",
      y = NULL,
      fill = "Entity type"
    ) +
    .viz_tools_plot_theme()
}

plot_confidence_distribution <- function(visualization_tools) {
  if (!is.data.frame(visualization_tools)) {
    stop("visualization_tools must be a data frame.", call. = FALSE)
  }

  .viz_tools_require_columns(visualization_tools, c("confidence_score"), "visualization_tools")

  if (nrow(visualization_tools) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::labs(title = "Confidence", subtitle = "No visualization rows available")
    )
  }

  ggplot2::ggplot(visualization_tools, ggplot2::aes_string(x = "confidence_score")) +
    ggplot2::geom_histogram(binwidth = 0.05, fill = "#355c7d", color = "#fbf7ef", linewidth = 0.4) +
    ggplot2::labs(
      title = "Confidence Distribution",
      subtitle = "Распределение post-LLM confidence",
      x = "Confidence score",
      y = "Tools"
    ) +
    .viz_tools_plot_theme()
}