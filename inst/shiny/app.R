resolve_project_root <- function() {
  candidates <- unique(c(
    getwd(),
    normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = FALSE)
  ))

  for (candidate in candidates) {
    if (
      file.exists(file.path(candidate, "DESCRIPTION")) &&
        dir.exists(file.path(candidate, "inst", "shiny"))
    ) {
      extdata_path <- file.path(candidate, "inst", "extdata")
      if (!dir.exists(extdata_path)) {
        dir.create(extdata_path, recursive = TRUE, showWarnings = FALSE)
      }

      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop("Project root with DESCRIPTION and inst/shiny was not found.", call. = FALSE)
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "active_in_ui",
    "already_mapped",
    "assessed_name",
    "confidence",
    "confidence_score",
    "count",
    "date_found",
    "duration_seconds",
    "entity_type",
    "error_message",
    "finished_at",
    "first_ui_added_at",
    "label",
    "last_ui_seen_at",
    "last_visualization_rank",
    "llm_error",
    "llm_processed_at",
    "llm_status",
    "mapped_confidence",
    "matched_terms",
    "mitre_tactic_count",
    "mitre_tactics",
    "mitre_technique_count",
    "mitre_technique_ids",
    "name",
    "pre_llm_score",
    "pre_llm_should_process",
    "queue_status",
    "record_id",
    "retrieval_rank",
    "retrieval_score",
    "rows",
    "run_id",
    "stage",
    "started_at",
    "status",
    "tactic",
    "tactic_names",
    "table_name",
    "technique_id",
    "technique_name",
    "visualization_rank",
    "visualization_score"
  ))
}

library(shiny)

project_root <- resolve_project_root()

`%||%` <- function(left, right) {
  if (is.null(left) || length(left) == 0 || is.na(left[[1]])) {
    return(right)
  }

  left
}

source(file.path(project_root, "R", "utils_core.R"))
source(file.path(project_root, "R", "normalize.R"))
source(file.path(project_root, "R", "llm_provider.R"))
source(file.path(project_root, "R", "llm_contracts.R"))
source(file.path(project_root, "R", "storage_duckdb.R"))
source(file.path(project_root, "R", "visualization_data.R"))
source(file.path(project_root, "R", "viz_matrix.R"))
source(file.path(project_root, "R", "viz_tools.R"))

extdata_dir <- file.path(project_root, "inst", "extdata")
asset_dir <- file.path(project_root, "inst", "shiny", "www")
duckdb_path <- file.path(extdata_dir, "offensive_tool_mapper.duckdb")

visualization_tools <- .visualization_empty_tools()
visualization_matrix <- .visualization_empty_matrix()

app_theme <- bslib::bs_theme(
  version = 5,
  bg = "#0f1720",
  fg = "#f5f1e8",
  primary = "#ff7a3d",
  secondary = "#40b9b4",
  success = "#3fba7c",
  base_font = bslib::font_google("Space Grotesk"),
  heading_font = bslib::font_google("Manrope"),
  code_font = bslib::font_google("IBM Plex Mono")
)

app_shell <- function(..., class = NULL) {
  tags$div(class = trimws(paste("app-shell", class)), ...)
}

metric_card <- function(title, value, subtitle) {
  bslib::card(
    class = "metric-card shell-card",
    bslib::card_body(
      tags$div(class = "metric-title", title),
      tags$div(class = "metric-value", value),
      tags$div(class = "metric-subtitle", subtitle)
    )
  )
}

hero_panel <- function(tool, tools, matrix) {
  if (nrow(tool) == 0) {
    return(
      tags$section(
        class = "hero-panel",
        tags$div(
          class = "hero-copy shell-card",
          tags$div(class = "eyebrow", "Offensive Tool Intelligence"),
          tags$h1("Visualization dataset is empty"),
          tags$p(class = "hero-lead", "Сначала пересобери visualization-ready слой, затем UI автоматически подхватит данные.")
        )
      )
    )
  }

  preview_tags <- utils::head(tool$filter_tags[[1]], 6)
  top_score_value <- if (nrow(tools) > 0) {
    max(tools$visualization_score, na.rm = TRUE)
  } else {
    NA_real_
  }

  tags$section(
    class = "hero-panel",
    tags$div(
      class = "hero-copy shell-card",
      tags$div(class = "eyebrow", "Offensive Tool Intelligence"),
      tags$h1("Современный обзор offensive tooling с упором на полезность, качество описания и MITRE coverage"),
      tags$p(
        class = "hero-lead",
        "Это не просто список репозиториев. Верхняя зона ранжируется уже после LLM-оценки и учитывает confidence, насыщенность описания, MITRE coverage и тип сущности."
      ),
      tags$div(
        class = "hero-chip-row",
        tags$span(class = "hero-chip", sprintf("%s UI-ready tools", nrow(tools))),
        tags$span(class = "hero-chip", sprintf("%s MITRE rows", nrow(matrix))),
        tags$span(class = "hero-chip", sprintf("Top score %s", if (is.na(top_score_value)) "n/a" else sprintf("%.2f", top_score_value)))
      )
    ),
    tags$aside(
      class = "hero-spotlight shell-card",
      tags$div(class = "spotlight-label", "Featured tool"),
      tags$h3(tool$assessed_name[[1]]),
      tags$p(class = "spotlight-summary", tool$short_description_ru[[1]]),
      tags$div(
        class = "spotlight-meta",
        sprintf(
          "Rank #%s | Confidence %.2f | %s | %s techniques",
          tool$visualization_rank[[1]],
          tool$confidence_score[[1]],
          tool$entity_type[[1]],
          tool$mitre_technique_count[[1]]
        )
      ),
      tags$div(
        class = "spotlight-tags",
        lapply(preview_tags, function(tag) tags$span(class = "tag-chip", tag))
      )
    )
  )
}

section_intro <- function(title, text) {
  tags$div(
    class = "section-intro",
    tags$div(class = "section-kicker", "Dashboard section"),
    tags$h2(title),
    tags$p(text)
  )
}

featured_tool_cards <- function(tools) {
  if (nrow(tools) == 0) {
    return(NULL)
  }

  tags$div(
    class = "featured-grid",
    lapply(seq_len(nrow(tools)), function(index) {
      tool <- tools[index, , drop = FALSE]

      tags$article(
        class = "featured-card shell-card",
        tags$div(class = "featured-rank", sprintf("#%s", tool$visualization_rank[[1]])),
        tags$div(class = "featured-entity", tool$entity_type[[1]]),
        tags$h3(tool$assessed_name[[1]]),
        tags$p(class = "featured-summary", tool$short_description_ru[[1]]),
        tags$div(
          class = "featured-meta-row",
          tags$span(class = "score-pill", sprintf("score %.2f", tool$visualization_score[[1]])),
          tags$span(class = "score-pill muted", sprintf("confidence %.2f", tool$confidence_score[[1]]))
        )
      )
    })
  )
}

.artifact_error <- function(message) {
  structure(list(message = message), class = "artifact_error")
}

.is_artifact_error <- function(value) {
  inherits(value, "artifact_error")
}

safe_read_artifact <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(
    readRDS(path),
    error = function(error) .artifact_error(conditionMessage(error))
  )
}

load_visualization_snapshot <- function(data_dir) {
  tools <- safe_read_artifact(file.path(data_dir, "visualization_tools.rds"))
  matrix <- safe_read_artifact(file.path(data_dir, "visualization_tool_matrix.rds"))

  if (is.null(tools) || .is_artifact_error(tools) || !is.data.frame(tools)) {
    tools <- .visualization_empty_tools()
  }

  if (is.null(matrix) || .is_artifact_error(matrix) || !is.data.frame(matrix)) {
    matrix <- .visualization_empty_matrix()
  }

  list(tools = tools, matrix = matrix)
}

collapse_multivalue <- function(value) {
  if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    return(NA_character_)
  }

  paste(unique(as.character(unlist(value, use.names = FALSE))), collapse = ", ")
}

count_records <- function(data, columns) {
  if (!is.data.frame(data) || nrow(data) == 0) {
    empty <- as.data.frame(setNames(vector("list", length(columns) + 1L), c(columns, "count")))

    for (column in columns) {
      empty[[column]] <- character()
    }

    empty[["count"]] <- integer()
    return(tibble::as_tibble(empty))
  }

  counted <- data |>
    dplyr::select(dplyr::all_of(columns)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), function(value) {
      if (is.factor(value)) {
        return(as.character(value))
      }

      if (is.list(value)) {
        return(vapply(value, collapse_multivalue, character(1)))
      }

      value
    })) |>
    dplyr::count(dplyr::across(dplyr::everything()), name = "count") |>
    tibble::as_tibble()

  if (nrow(counted) > 0) {
    order_args <- c(list(-counted[["count"]]), unname(lapply(columns, function(column) counted[[column]])))
    counted <- counted[do.call(order, order_args), , drop = FALSE]
  }

  tibble::as_tibble(counted)
}

empty_refinement_candidates <- function() {
  tibble::tibble(
    record_id = character(),
    assessed_name = character(),
    already_mapped = logical(),
    technique_id = character(),
    technique_name = character(),
    tactic_names = I(list()),
    is_subtechnique = logical(),
    retrieval_score = numeric(),
    retrieval_rank = integer(),
    matched_terms = I(list()),
    mapped_confidence = numeric(),
    tactic_label = character(),
    matched_terms_label = character(),
    mapping_status = character()
  )
}

load_refinement_candidates <- function(data_dir) {
  path <- file.path(data_dir, "mitre_refinement_candidates.rds")
  value <- safe_read_artifact(path)

  if (is.null(value) || .is_artifact_error(value) || !is.data.frame(value) || nrow(value) == 0) {
    return(empty_refinement_candidates())
  }

  value[["already_mapped"]] <- dplyr::coalesce(as.logical(value[["already_mapped"]]), FALSE)
  value[["retrieval_score"]] <- as.numeric(value[["retrieval_score"]])
  value[["retrieval_rank"]] <- as.integer(value[["retrieval_rank"]])
  value[["mapped_confidence"]] <- as.numeric(value[["mapped_confidence"]])
  value[["tactic_label"]] <- vapply(value[["tactic_names"]], collapse_multivalue, character(1))
  value[["matched_terms_label"]] <- vapply(value[["matched_terms"]], collapse_multivalue, character(1))
  value[["mapping_status"]] <- ifelse(value[["already_mapped"]], "already_mapped", "needs_review")
  value <- value[order(value[["already_mapped"]], -value[["retrieval_score"]], value[["retrieval_rank"]]), , drop = FALSE]
  tibble::as_tibble(value)
}

modern_accent_palette <- c("#b42318", "#40b9b4", "#f59e0b", "#2563eb", "#7c3aed", "#0f766e")

tactic_details <- c(
  Reconnaissance = "Сбор первичной информации о целях, инфраструктуре, пользователях и внешнем периметре до активного доступа.",
  "Resource Development" = "Подготовка ресурсов для операции: домены, инфраструктура, учётки, полезные нагрузки и вспомогательные сервисы.",
  "Initial Access" = "Получение начальной точки входа в среду через уязвимость, публичный сервис, фишинг или внешний доступ.",
  Execution = "Запуск команд, скриптов или полезной нагрузки на целевой системе.",
  Persistence = "Закрепление доступа, чтобы вернуться в систему после перезапуска, смены сессии или устранения первичного входа.",
  "Privilege Escalation" = "Повышение прав, переход от обычного пользователя к более привилегированному контексту.",
  "Defense Evasion" = "Снижение заметности: обход защит, маскировка активности, отключение или затруднение детекта.",
  "Credential Access" = "Получение паролей, токенов, хэшей и других материалов для дальнейшего доступа.",
  Discovery = "Разведка уже внутри среды: пользователи, процессы, хосты, сети, домены, сервисы и права.",
  "Lateral Movement" = "Перемещение между системами внутри сети с использованием доступов, удалённого выполнения или доверенных каналов.",
  Collection = "Сбор данных внутри среды перед выгрузкой или дальнейшей обработкой.",
  Exfiltration = "Вывод собранных данных из среды наружу через сетевые, облачные или иные каналы.",
  "Command and Control" = "Поддержание канала управления между оператором и инструментом внутри среды.",
  Impact = "Действия, которые нарушают доступность, целостность или бизнес-процесс цели."
)

technique_hints <- c(
  T1003 = "Получение учётных данных из памяти, хранилищ ОС или связанных credential-артефактов.",
  T1018 = "Поиск удалённых систем, узлов и доступных направлений для дальнейшего движения.",
  T1021 = "Использование удалённых сервисов для перехода на другие машины или выполнения действий внутри сети.",
  T1041 = "Передача данных через тот же канал, который используется для управления или связи.",
  T1055 = "Встраивание кода в другой процесс, чтобы скрыть выполнение или получить контекст процесса.",
  T1059 = "Запуск команд и скриптов через shell, PowerShell, Python, JavaScript или другие интерпретаторы.",
  T1105 = "Передача файлов и полезной нагрузки между внешней инфраструктурой и целевой средой.",
  T1133 = "Использование VPN, RDP, SSH, внешних панелей или других exposed remote access сервисов.",
  T1190 = "Эксплуатация уязвимого публичного приложения или сервиса для начального входа.",
  T1548 = "Злоупотребление механизмами повышения прав или обхода контроля привилегий.",
  T1562 = "Отключение, обход или ослабление защитных механизмов и средств мониторинга.",
  T1573 = "Защита C2-канала шифрованием или нестандартным туннелированием связи.",
  T1583 = "Создание или аренда инфраструктуры: доменов, серверов, учёток, сертификатов и связанных ресурсов.",
  T1588 = "Получение готовых возможностей: инструментов, эксплойтов, сертификатов, аккаунтов или инфраструктуры.",
  T1595 = "Активное сканирование внешнего периметра, сервисов или адресов для поиска целей и уязвимостей."
)

row_value <- function(row, column, default = NA_character_) {
  if (!is.data.frame(row) || !(column %in% names(row)) || length(row[[column]]) == 0) {
    return(default)
  }

  value <- row[[column]][[1]]

  if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    return(default)
  }

  value
}

normalise_value_array <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(character())
  }

  flattened <- unlist(value, recursive = TRUE, use.names = FALSE)
  flattened <- as.character(flattened)
  flattened <- flattened[!is.na(flattened)]
  flattened <- trimws(flattened)
  flattened[nzchar(flattened)]
}

split_mitre_values <- function(value) {
  items <- normalise_value_array(value)

  if (length(items) == 0) {
    return(character())
  }

  items <- unlist(strsplit(items, ",", fixed = TRUE), use.names = FALSE)
  items <- trimws(items)
  items <- gsub("^c\\(", "", items)
  items <- gsub("\\)$", "", items)
  items <- gsub("^[\"'`]+|[\"'`]+$", "", items)
  unique(items[nzchar(items)])
}

clean_display_tags <- function(value) {
  tags_value <- normalise_value_array(value)
  tags_value <- tags_value[!grepl("^(mitre|source|entity|category):", tags_value, ignore.case = TRUE)]
  unique(tags_value)
}

format_score <- function(value) {
  number <- suppressWarnings(as.numeric(value[[1]] %||% NA_real_))

  if (is.na(number)) {
    return("н/д")
  }

  sprintf("%.2f", number)
}

rank_label <- function(value) {
  rank <- suppressWarnings(as.integer(value[[1]] %||% NA_integer_))

  if (is.na(rank)) {
    return("н/д")
  }

  sprintf("#%s", rank)
}

format_meta_value <- function(value, fallback = "н/д") {
  if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    return(fallback)
  }

  value <- paste(normalise_value_array(value), collapse = ", ")

  if (!nzchar(value)) {
    return(fallback)
  }

  value
}

tactic_description <- function(tactic) {
  description <- tactic_details[[tactic]]

  if (is.null(description) || is.na(description)) {
    return("Тактика описывает крупную цель поведения: зачем инструмент использует связанные техники в этой части цепочки.")
  }

  description
}

describe_technique <- function(row) {
  technique_id <- format_meta_value(row_value(row, "technique_id"), fallback = "")
  normalized_id <- strsplit(technique_id, ".", fixed = TRUE)[[1]][[1]]

  if (technique_id %in% names(technique_hints)) {
    return(technique_hints[[technique_id]])
  }

  if (normalized_id %in% names(technique_hints)) {
    return(technique_hints[[normalized_id]])
  }

  sprintf(
    "Техника описывает действие или возможность из MITRE ATT&CK: %s.",
    format_meta_value(row_value(row, "technique_name"), fallback = "название пока не указано")
  )
}

build_selected_ttp_groups <- function(matrix_rows) {
  if (!is.data.frame(matrix_rows) || nrow(matrix_rows) == 0) {
    return(list())
  }

  groups <- list()

  for (index in seq_len(nrow(matrix_rows))) {
    row <- matrix_rows[index, , drop = FALSE]
    tactics <- split_mitre_values(row_value(row, "tactic"))

    if (length(tactics) == 0) {
      tactics <- "Тактика не указана"
    }

    for (tactic in tactics) {
      if (is.null(groups[[tactic]])) {
        groups[[tactic]] <- list(
          tactic = tactic,
          description = tactic_description(tactic),
          techniques = list()
        )
      }

      groups[[tactic]][["techniques"]] <- append(groups[[tactic]][["techniques"]], list(row))
    }
  }

  groups <- lapply(groups, function(group) {
    confidences <- vapply(group$techniques, function(row) {
      suppressWarnings(as.numeric(row_value(row, "confidence", 0)))
    }, numeric(1))

    group$avg_confidence <- if (length(confidences) > 0) mean(confidences, na.rm = TRUE) else NA_real_
    group
  })

  groups[order(
    -vapply(groups, function(group) length(group$techniques), integer(1)),
    vapply(groups, function(group) group$tactic, character(1))
  )]
}

tool_card_onclick <- function(record_id) {
  sprintf(
    "Shiny.setInputValue('tool_card_select', %s, {priority: 'event'})",
    jsonlite::toJSON(as.character(record_id), auto_unbox = TRUE)
  )
}

tool_list_cards <- function(tools, selected_id = NULL) {
  if (!is.data.frame(tools) || nrow(tools) == 0) {
    return(
      tags$div(
        class = "empty-detail utility-empty",
        "Нет инструментов для текущих фильтров."
      )
    )
  }

  tags$div(
    class = "utility-list-scroll",
    lapply(seq_len(nrow(tools)), function(index) {
      tool <- tools[index, , drop = FALSE]
      record_id <- as.character(row_value(tool, "record_id", ""))
      active_class <- if (identical(record_id, selected_id)) " is-active" else ""

      tags$button(
        type = "button",
        class = paste0("utility-list-item", active_class),
        onclick = tool_card_onclick(record_id),
        tags$div(
          class = "utility-list-topline",
          tags$span(rank_label(row_value(tool, "visualization_rank"))),
          tags$span(format_meta_value(row_value(tool, "source")))
        ),
        tags$h4(format_meta_value(row_value(tool, "assessed_name"))),
        tags$p(format_meta_value(row_value(tool, "short_description_ru"), fallback = "Описание недоступно.")),
        tags$div(
          class = "utility-list-footer",
          tags$span(format_meta_value(row_value(tool, "entity_type"))),
          tags$span(sprintf("%s MITRE", format_meta_value(row_value(tool, "mitre_technique_count"), fallback = "0"))),
          tags$span(sprintf("%s уверенность", format_score(row_value(tool, "confidence_score"))))
        )
      )
    })
  )
}

modern_metric_card <- function(label, value, meta) {
  tags$article(
    class = "metric-card-modern",
    tags$div(class = "metric-icon-shell", substr(label, 1, 1)),
    tags$div(
      tags$div(class = "metric-label-modern", label),
      tags$div(class = "metric-value-modern", value),
      tags$div(class = "metric-meta-modern", meta)
    )
  )
}

modern_section_heading <- function(kicker, title, text = NULL, class = NULL) {
  tags$div(
    class = trimws(paste("section-heading heading-light", class)),
    tags$div(
      tags$div(class = "section-kicker-modern", kicker),
      tags$h3(title)
    ),
    if (!is.null(text)) tags$p(text)
  )
}

modern_top_tool_cards <- function(tools, limit = 4L) {
  if (!is.data.frame(tools) || nrow(tools) == 0) {
    return(tags$div(class = "empty-detail utility-empty", "Данных для карточек пока нет."))
  }

  visible <- utils::head(tools[order(tools[["visualization_rank"]]), , drop = FALSE], limit)

  tags$div(
    class = "featured-ribbon-grid",
    lapply(seq_len(nrow(visible)), function(index) {
      tool <- visible[index, , drop = FALSE]

      tags$article(
        class = "featured-ribbon-card glass-panel",
        tags$div(
          class = "featured-ribbon-meta",
          tags$span(rank_label(row_value(tool, "visualization_rank"))),
          tags$span(format_meta_value(row_value(tool, "source")))
        ),
        tags$h4(format_meta_value(row_value(tool, "assessed_name"))),
        tags$p(format_meta_value(row_value(tool, "short_description_ru"), fallback = "Описание недоступно.")),
        tags$div(
          class = "featured-ribbon-footer",
          tags$span(format_meta_value(row_value(tool, "entity_type"))),
          tags$span(sprintf("%s MITRE", format_meta_value(row_value(tool, "mitre_technique_count"), fallback = "0")))
        )
      )
    })
  )
}

modern_count_records <- function(data, column, value_name = column, limit = 8L) {
  if (!is.data.frame(data) || nrow(data) == 0 || !(column %in% names(data))) {
    return(tibble::tibble(!!value_name := character(), count = integer()))
  }

  values <- data[[column]]
  if (is.list(values)) {
    values <- unlist(lapply(values, split_mitre_values), use.names = FALSE)
  }

  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(values)]

  if (length(values) == 0) {
    return(tibble::tibble(!!value_name := character(), count = integer()))
  }

  counted <- sort(table(values), decreasing = TRUE)
  counted <- utils::head(counted, limit)
  tibble::tibble(
    !!value_name := names(counted),
    count = as.integer(counted)
  )
}

modern_coverage_summary <- function(tools, matrix) {
  mapped_tools <- if (is.data.frame(tools) && nrow(tools) > 0) {
    sum(tools[["mitre_technique_count"]] > 0, na.rm = TRUE)
  } else {
    0L
  }
  unique_tactics <- if (is.data.frame(matrix) && nrow(matrix) > 0) {
    length(unique(unlist(lapply(matrix[["tactic"]], split_mitre_values), use.names = FALSE)))
  } else {
    0L
  }
  unique_techniques <- if (is.data.frame(matrix) && nrow(matrix) > 0) {
    dplyr::n_distinct(matrix[["technique_id"]])
  } else {
    0L
  }
  avg_techniques <- if (is.data.frame(tools) && nrow(tools) > 0 && mapped_tools > 0) {
    mean(tools[["mitre_technique_count"]][tools[["mitre_technique_count"]] > 0], na.rm = TRUE)
  } else {
    0
  }

  list(
    list(label = "Инструменты со связями", value = mapped_tools, meta = "Утилиты с хотя бы одной MITRE-связью"),
    list(label = "Уникальные тактики", value = unique_tactics, meta = "Разные тактики в текущей матрице"),
    list(label = "Уникальные техники", value = unique_techniques, meta = "Уникальные technique_id в текущем слое"),
    list(label = "Среднее техник", value = sprintf("%.2f", avg_techniques), meta = "Среднее число техник на связанный инструмент")
  )
}

modern_refinement_summary <- function(refinement) {
  if (!is.data.frame(refinement) || nrow(refinement) == 0) {
    refinement <- empty_refinement_candidates()
  }

  gaps <- refinement[!refinement[["already_mapped"]], , drop = FALSE]

  list(
    list(label = "Кандидаты", value = nrow(refinement), meta = "Все дополнительные кандидаты из слоя уточнения"),
    list(label = "На проверку", value = nrow(gaps), meta = "Новые связи, которых ещё нет в основной матрице"),
    list(label = "Инструменты с зазорами", value = dplyr::n_distinct(gaps[["record_id"]]), meta = "Сколько профилей получили новые MITRE-кандидаты"),
    list(label = "Уже подтверждено", value = sum(refinement[["already_mapped"]], na.rm = TRUE), meta = "Кандидаты, совпавшие с текущей матрицей"),
    list(label = "Новые техники", value = dplyr::n_distinct(gaps[["technique_id"]]), meta = "Уникальные техники среди новых кандидатов")
  )
}

modern_summary_cards <- function(items, class = "coverage-summary-grid") {
  tags$div(
    class = class,
    lapply(items, function(item) {
      tags$article(
        class = "glass-panel coverage-summary-card",
        tags$div(class = "detail-section-kicker", item$label),
        tags$div(class = "coverage-summary-value", item$value),
        tags$p(item$meta)
      )
    })
  )
}

format_file_size <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }

  size_bytes <- file.info(path)$size[[1]]
  if (is.na(size_bytes)) {
    return(NA_character_)
  }

  if (size_bytes < 1024) {
    return(sprintf("%s B", size_bytes))
  }

  if (size_bytes < 1024^2) {
    return(sprintf("%.1f KB", size_bytes / 1024))
  }

  sprintf("%.2f MB", size_bytes / (1024^2))
}

format_file_mtime <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }

  format(file.info(path)$mtime[[1]], "%Y-%m-%d %H:%M:%S")
}

artifact_row_count <- function(value) {
  if (is.null(value) || .is_artifact_error(value)) {
    return(NA_integer_)
  }

  if (is.data.frame(value)) {
    return(nrow(value))
  }

  if (is.list(value)) {
    return(length(value))
  }

  NA_integer_
}

empty_github_query_log <- function() {
  tibble::tibble(
    executed_at = character(),
    request_index = integer(),
    source_query = character(),
    query_family = character(),
    query_tier = character(),
    query_min_stars = integer(),
    search_query = character(),
    search_sort = character(),
    search_page = integer(),
    request_status = character(),
    returned_rows = integer(),
    unique_candidates_after_request = integer()
  )
}

empty_pipeline_batch_summary <- function() {
  tibble::tibble(
    run_index = integer(),
    started_at = character(),
    finished_at = character(),
    duration_seconds = numeric(),
    github_new_rows = integer(),
    github_rows = integer(),
    github_request_offset = integer(),
    github_next_request_offset = integer(),
    github_query_plan_size = integer(),
    github_rotation_progress_percent = numeric(),
    github_logged_requests = integer(),
    github_query_families = character()
  )
}

empty_pipeline_backlog <- function() {
  tibble::tibble(
    name = character(),
    source = character(),
    pre_llm_score = numeric(),
    date_found = character(),
    llm_status = character(),
    backlog_reason = character(),
    llm_error = character()
  )
}

empty_pipeline_history <- function() {
  tibble::tibble(
    assessed_name = character(),
    source = character(),
    entity_type = character(),
    first_ui_added_at = character(),
    last_ui_seen_at = character(),
    active_in_ui = logical(),
    last_visualization_rank = integer()
  )
}

empty_pipeline_queue <- function() {
  tibble::tibble(
    name = character(),
    source = character(),
    pre_llm_score = numeric(),
    date_found = character(),
    queue_status = character(),
    llm_status = character(),
    llm_processed_at = character(),
    llm_error = character(),
    pre_llm_should_process = logical()
  )
}

empty_pipeline_stage_history <- function() {
  tibble::tibble(
    stage = character(),
    status = character(),
    started_at = character(),
    finished_at = character(),
    duration_seconds = numeric(),
    error_message = character()
  )
}

empty_pipeline_discovery_summary <- function() {
  tibble::tibble(
    metric = character(),
    value = character()
  )
}

pipeline_parse_details_json <- function(value) {
  if (is.null(value) || length(value) == 0 || !nzchar(value[[1]] %||% "")) {
    return(list())
  }

  tryCatch(
    jsonlite::fromJSON(value[[1]], simplifyVector = TRUE),
    error = function(error) list(parse_error = conditionMessage(error))
  )
}

pipeline_gcd <- function(left, right) {
  left <- abs(as.integer(left %||% 0L))
  right <- abs(as.integer(right %||% 0L))

  while (right != 0L) {
    next_value <- left %% right
    left <- right
    right <- next_value
  }

  left
}

build_pipeline_stage_history <- function(status_table) {
  if (!is.data.frame(status_table) || nrow(status_table) == 0) {
    return(empty_pipeline_stage_history())
  }

  status_table <- status_table[order(-xtfrm(status_table[["finished_at"]]), -xtfrm(status_table[["started_at"]])), , drop = FALSE]
  tibble::as_tibble(status_table[c("stage", "status", "started_at", "finished_at", "duration_seconds", "error_message")])
}

build_pipeline_discovery_summary <- function(status_table, github_search_state = NULL) {
  if (!is.data.frame(status_table) || nrow(status_table) == 0) {
    return(empty_pipeline_discovery_summary())
  }

  collect_row <- status_table[status_table[["stage"]] == "collect", , drop = FALSE]
  if (nrow(collect_row) > 0) {
    collect_row <- collect_row[order(-xtfrm(collect_row[["finished_at"]]), -xtfrm(collect_row[["started_at"]])), , drop = FALSE]
    collect_row <- utils::head(collect_row, 1L)
  }

  if (nrow(collect_row) == 0) {
    return(empty_pipeline_discovery_summary())
  }

  details <- pipeline_parse_details_json(collect_row$details_json)
  query_plan_size <- as.integer(details$github_query_plan_size %||% github_search_state$query_plan_size %||% 0L)
  selected_requests <- as.integer(details$github_selected_requests %||% github_search_state$last_selected_requests %||% 0L)
  request_offset <- as.integer(details$github_request_offset %||% github_search_state$last_start_offset %||% 0L)
  total_new_rows <- sum(c(
    as.integer(details$github_new_rows %||% 0L),
    as.integer(details$rss_new_rows %||% 0L),
    as.integer(details$packetstorm_new_rows %||% 0L)
  ))

  full_coverage_runs <- if (query_plan_size > 0L && selected_requests > 0L) {
    ceiling(query_plan_size / selected_requests)
  } else {
    NA_integer_
  }

  rotation_cycle_runs <- if (query_plan_size > 0L && selected_requests > 0L) {
    as.integer(query_plan_size / pipeline_gcd(query_plan_size, selected_requests))
  } else {
    NA_integer_
  }

  tibble::tribble(
    ~metric, ~value,
    "collect_mode", as.character(details$collect_mode %||% "unknown"),
    "new_rows_total", as.character(total_new_rows),
    "github_new_rows", as.character(details$github_new_rows %||% NA_integer_),
    "rss_new_rows", as.character(details$rss_new_rows %||% NA_integer_),
    "packetstorm_new_rows", as.character(details$packetstorm_new_rows %||% NA_integer_),
    "github_rows_total", as.character(details$github_rows %||% NA_integer_),
    "rss_rows_total", as.character(details$rss_rows %||% NA_integer_),
    "packetstorm_rows_total", as.character(details$packetstorm_rows %||% NA_integer_),
    "github_selected_requests", as.character(selected_requests),
    "github_request_offset", as.character(request_offset),
    "github_next_request_offset", as.character(details$github_next_request_offset %||% github_search_state$next_request_offset %||% NA_integer_),
    "github_query_plan_size", as.character(query_plan_size),
    "github_selection_share_percent", as.character(details$github_selection_share_percent %||% NA_real_),
    "github_rotation_progress_percent", as.character(details$github_rotation_progress_percent %||% NA_real_),
    "github_remaining_runs_estimate", as.character(details$github_remaining_runs_estimate %||% NA_integer_),
    "github_remaining_request_slots", as.character(details$github_remaining_request_slots %||% NA_integer_),
    "github_logged_requests", as.character(details$github_logged_requests %||% NA_integer_),
    "github_logged_query_families", as.character(details$github_logged_query_families %||% NA_character_),
    "github_logged_query_tiers", as.character(details$github_logged_query_tiers %||% NA_character_),
    "estimated_full_coverage_runs", as.character(full_coverage_runs %||% NA_integer_),
    "rotation_cycle_runs", as.character(rotation_cycle_runs %||% NA_integer_),
    "search_state_updated_at", as.character(github_search_state$updated_at %||% NA_character_),
    "collect_status", as.character(collect_row$status[[1]] %||% NA_character_),
    "collect_started_at", as.character(collect_row$started_at[[1]] %||% NA_character_),
    "collect_finished_at", as.character(collect_row$finished_at[[1]] %||% NA_character_),
    "collect_duration_seconds", as.character(collect_row$duration_seconds[[1]] %||% NA_real_)
  )
}

pipeline_discovery_metric_value <- function(discovery_summary, metric, default = NA_character_) {
  if (!is.data.frame(discovery_summary) || nrow(discovery_summary) == 0) {
    return(default)
  }

  values <- discovery_summary$value[discovery_summary$metric == metric]
  if (length(values) == 0) {
    return(default)
  }

  values[[1]]
}

build_pipeline_queue <- function(normalized_data, assessment_results, queue_data = NULL) {
  if (is.data.frame(queue_data) && nrow(queue_data) > 0) {
    return(tibble::as_tibble(queue_data[c(
      "name",
      "source",
      "pre_llm_score",
      "date_found",
      "queue_status",
      "llm_status",
      "llm_processed_at",
      "llm_error",
      "pre_llm_should_process"
    )]))
  }

  if (!is.data.frame(normalized_data) || nrow(normalized_data) == 0) {
    return(empty_pipeline_queue())
  }

  assessment_index <- if (is.data.frame(assessment_results) && nrow(assessment_results) > 0) {
    assessment_index <- assessment_results[!duplicated(assessment_results[["record_id"]]), , drop = FALSE]
    tibble::as_tibble(assessment_index[c("record_id", "llm_status", "llm_error", "llm_processed_at")])
  } else {
    tibble::tibble(
      record_id = character(),
      llm_status = character(),
      llm_error = character(),
      llm_processed_at = character()
    )
  }

  queue_view <- dplyr::left_join(normalized_data, assessment_index, by = "record_id")
  queue_view[["queue_status"]] <- ifelse(
    !(queue_view[["pre_llm_should_process"]] %in% TRUE),
    "filtered_out_pre_llm",
    ifelse(
      is.na(queue_view[["llm_status"]]),
      "pending_not_sent",
      ifelse(
        queue_view[["llm_status"]] == "error",
        "retry_after_error",
        ifelse(
          queue_view[["llm_status"]] == "success",
          "processed_success",
          ifelse(
            queue_view[["llm_status"]] == "skipped_pre_filter",
            "filtered_out_pre_llm",
            paste0("status_", queue_view[["llm_status"]])
          )
        )
      )
    )
  )
  queue_view <- queue_view[
    order(queue_view[["queue_status"]], -queue_view[["pre_llm_score"]], -xtfrm(queue_view[["date_found"]]), queue_view[["name"]]),
    ,
    drop = FALSE
  ]
  tibble::as_tibble(queue_view[c(
    "name",
    "source",
    "pre_llm_score",
    "date_found",
    "queue_status",
    "llm_status",
    "llm_processed_at",
    "llm_error",
    "pre_llm_should_process"
  )])
}

build_pipeline_backlog <- function(queue_data) {
  if (!is.data.frame(queue_data) || nrow(queue_data) == 0) {
    return(empty_pipeline_backlog())
  }

  backlog <- queue_data[queue_data[["queue_status"]] %in% c("pending_not_sent", "retry_after_error"), , drop = FALSE]
  backlog[["backlog_reason"]] <- backlog[["queue_status"]]
  tibble::as_tibble(backlog[c("name", "source", "pre_llm_score", "date_found", "llm_status", "backlog_reason", "llm_error")])
}

build_pipeline_history <- function(history_data) {
  if (!is.data.frame(history_data) || nrow(history_data) == 0) {
    return(empty_pipeline_history())
  }

  history_data <- history_data[
    order(-xtfrm(history_data[["first_ui_added_at"]]), history_data[["assessed_name"]]),
    ,
    drop = FALSE
  ]

  tibble::as_tibble(history_data[c(
    "assessed_name",
    "source",
    "entity_type",
    "first_ui_added_at",
    "last_ui_seen_at",
    "active_in_ui",
    "last_visualization_rank"
  )])
}

safe_runtime_config <- function() {
  tryCatch(
    get_llm_runtime_config(),
    error = function(error) {
      tibble::tibble(
        provider = NA_character_,
        model = NA_character_,
        base_url = NA_character_,
        api_key_present = FALSE,
        error = conditionMessage(error)
      )
    }
  )
}

pipeline_duckdb_summary <- function(db_path) {
  empty_summary <- tibble::tibble(
    table_name = character(),
    rows = integer()
  )

  if (!file.exists(db_path)) {
    return(empty_summary)
  }

  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("duckdb", quietly = TRUE)) {
    return(empty_summary)
  }

  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  tables <- DBI::dbListTables(connection)
  if (length(tables) == 0) {
    return(empty_summary)
  }

  summary <- dplyr::bind_rows(lapply(
    tables,
    function(table_name) {
      row_count <- DBI::dbGetQuery(connection, sprintf("SELECT COUNT(*) AS n FROM %s", table_name))$n[[1]]
      tibble::tibble(table_name = table_name, rows = as.integer(row_count))
    }
  ))

  summary[order(-summary[["rows"]], summary[["table_name"]]), , drop = FALSE]
}

empty_pipeline_run_catalog <- function() {
  tibble::tibble(
    run_id = character(),
    started_at = character(),
    finished_at = character(),
    duration_seconds = numeric(),
    status = character(),
    stages_completed = integer(),
    discovery_new_rows = integer(),
    github_new_rows = integer(),
    github_requests = integer(),
    relevant_tools = integer(),
    mitre_rows = integer()
  )
}

empty_pipeline_run_compare_summary <- function() {
  tibble::tibble(
    metric = character(),
    baseline = character(),
    current = character(),
    delta = character()
  )
}

empty_pipeline_run_stage_comparison <- function() {
  tibble::tibble(
    stage = character(),
    baseline_status = character(),
    current_status = character(),
    baseline_duration_seconds = numeric(),
    current_duration_seconds = numeric(),
    duration_delta_seconds = numeric()
  )
}

empty_pipeline_run_tool_diff <- function() {
  tibble::tibble(
    change_type = character(),
    assessed_name = character(),
    source = character(),
    entity_type = character(),
    baseline_confidence_score = numeric(),
    current_confidence_score = numeric(),
    baseline_mitre_technique_count = integer(),
    current_mitre_technique_count = integer()
  )
}

empty_pipeline_run_mitre_diff <- function() {
  tibble::tibble(
    change_type = character(),
    assessed_name = character(),
    technique_id = character(),
    technique_name = character(),
    tactic = character(),
    baseline_confidence = numeric(),
    current_confidence = numeric()
  )
}

empty_pipeline_sanity_checks <- function() {
  tibble::tibble(
    run_id = character(),
    check_name = character(),
    status = character(),
    severity = character(),
    observed_value = character(),
    expected_value = character(),
    details = character(),
    checked_at = character()
  )
}

pipeline_parse_datetime <- function(values) {
  suppressWarnings(as.POSIXct(as.character(values), format = "%Y-%m-%d %H:%M:%S", tz = ""))
}

pipeline_format_number <- function(value, digits = 0L) {
  if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
    return("n/a")
  }

  value <- as.numeric(value[[1]])

  if (is.na(value)) {
    return("n/a")
  }

  if (digits <= 0L) {
    return(as.character(as.integer(round(value))))
  }

  sprintf(paste0("%.", digits, "f"), value)
}

pipeline_format_delta <- function(current_value, baseline_value, digits = 0L) {
  if (is.null(current_value) || is.null(baseline_value) || length(current_value) == 0 || length(baseline_value) == 0) {
    return("n/a")
  }

  current_value <- as.numeric(current_value[[1]])
  baseline_value <- as.numeric(baseline_value[[1]])

  if (is.na(current_value) || is.na(baseline_value)) {
    return("n/a")
  }

  delta <- current_value - baseline_value

  if (digits <= 0L) {
    return(sprintf("%+d", as.integer(round(delta))))
  }

  sprintf(paste0("%+.", digits, "f"), delta)
}

pipeline_row_value <- function(row, column_name, default = NA) {
  if (!is.data.frame(row) || nrow(row) == 0 || !(column_name %in% names(row))) {
    return(default)
  }

  row[[column_name]][[1]] %||% default
}

pipeline_lookup_run_count <- function(count_table, run_id) {
  if (!is.data.frame(count_table) || nrow(count_table) == 0) {
    return(0L)
  }

  value <- count_table$count[count_table$run_id == run_id]

  if (length(value) == 0) {
    return(0L)
  }

  as.integer(value[[1]])
}

pipeline_count_rows_by_run <- function(table_name, db_path) {
  empty_counts <- tibble::tibble(run_id = character(), count = integer())
  table_data <- read_duckdb_table(table_name, db_path = db_path, required = FALSE, restore = FALSE)

  if (!is.data.frame(table_data) || nrow(table_data) == 0 || !("run_id" %in% names(table_data))) {
    return(empty_counts)
  }

  run_ids <- as.character(table_data[["run_id"]])
  run_ids <- run_ids[!is.na(run_ids) & nzchar(run_ids)]

  if (length(run_ids) == 0) {
    return(empty_counts)
  }

  counted <- as.data.frame(table(run_ids), stringsAsFactors = FALSE)
  names(counted) <- c("run_id", "count")
  counted[["count"]] <- as.integer(counted[["count"]])
  tibble::as_tibble(counted[order(-counted[["count"]], counted[["run_id"]]), , drop = FALSE])
}

pipeline_build_run_label <- function(run_row) {
  if (!is.data.frame(run_row) || nrow(run_row) == 0) {
    return("No runs available")
  }

  sprintf(
    "%s | %s | relevant %s | MITRE %s",
    run_row$started_at[[1]] %||% run_row$run_id[[1]],
    run_row$status[[1]] %||% "unknown",
    run_row$relevant_tools[[1]] %||% 0L,
    run_row$mitre_rows[[1]] %||% 0L
  )
}

build_pipeline_run_catalog <- function(db_path) {
  stage_history <- read_duckdb_table("pipeline_stage_history", db_path = db_path, required = FALSE, restore = FALSE)

  if (!is.data.frame(stage_history) || nrow(stage_history) == 0 || !("run_id" %in% names(stage_history))) {
    return(empty_pipeline_run_catalog())
  }

  stage_history <- stage_history[!is.na(stage_history[["run_id"]]) & nzchar(stage_history[["run_id"]]), , drop = FALSE]

  if (nrow(stage_history) == 0) {
    return(empty_pipeline_run_catalog())
  }

  relevant_counts <- pipeline_count_rows_by_run("relevant_tools_history", db_path)
  mitre_counts <- pipeline_count_rows_by_run("mitre_mappings_history", db_path)
  github_request_counts <- pipeline_count_rows_by_run("github_search_log_history", db_path)

  run_catalog <- lapply(unique(stage_history[["run_id"]]), function(run_id) {
    run_rows <- stage_history[stage_history[["run_id"]] == run_id, , drop = FALSE]
    run_rows <- run_rows[order(xtfrm(run_rows[["finished_at"]]), xtfrm(run_rows[["started_at"]])), , drop = FALSE]
    started_at_values <- pipeline_parse_datetime(run_rows[["started_at"]])
    finished_at_values <- pipeline_parse_datetime(run_rows[["finished_at"]])

    run_started_at <- if (all(is.na(started_at_values))) {
      NA_character_
    } else {
      format(min(started_at_values, na.rm = TRUE), "%Y-%m-%d %H:%M:%S")
    }

    run_finished_at <- if (all(is.na(finished_at_values))) {
      NA_character_
    } else {
      format(max(finished_at_values, na.rm = TRUE), "%Y-%m-%d %H:%M:%S")
    }

    run_duration_seconds <- if (!all(is.na(started_at_values)) && !all(is.na(finished_at_values))) {
      as.numeric(difftime(max(finished_at_values, na.rm = TRUE), min(started_at_values, na.rm = TRUE), units = "secs"))
    } else {
      sum(suppressWarnings(as.numeric(run_rows[["duration_seconds"]])), na.rm = TRUE)
    }

    last_status <- utils::tail(stats::na.omit(as.character(run_rows[["status"]])), 1L)
    run_status <- if (any(as.character(run_rows[["status"]]) == "error", na.rm = TRUE)) {
      "error"
    } else if (length(last_status) > 0) {
      last_status[[1]]
    } else {
      "unknown"
    }

    collect_rows <- run_rows[run_rows[["stage"]] == "collect", , drop = FALSE]
    collect_row <- if (nrow(collect_rows) > 0) utils::tail(collect_rows, 1L) else collect_rows
    collect_details <- if (nrow(collect_row) > 0) pipeline_parse_details_json(collect_row[["details_json"]]) else list()

    tibble::tibble(
      run_id = run_id,
      started_at = run_started_at,
      finished_at = run_finished_at,
      duration_seconds = round(run_duration_seconds, 3),
      status = run_status,
      stages_completed = dplyr::n_distinct(run_rows[["stage"]]),
      discovery_new_rows = as.integer(collect_details$new_rows_total %||% sum(c(
        as.integer(collect_details$github_new_rows %||% 0L),
        as.integer(collect_details$rss_new_rows %||% 0L),
        as.integer(collect_details$packetstorm_new_rows %||% 0L)
      ))),
      github_new_rows = as.integer(collect_details$github_new_rows %||% 0L),
      github_requests = pipeline_lookup_run_count(github_request_counts, run_id),
      relevant_tools = pipeline_lookup_run_count(relevant_counts, run_id),
      mitre_rows = pipeline_lookup_run_count(mitre_counts, run_id)
    )
  }) |>
    dplyr::bind_rows()

  if (nrow(run_catalog) == 0) {
    return(empty_pipeline_run_catalog())
  }

  run_catalog[order(-xtfrm(run_catalog[["finished_at"]]), -xtfrm(run_catalog[["started_at"]])), , drop = FALSE]
}

pipeline_select_stage_rows <- function(stage_history, run_id) {
  if (!is.data.frame(stage_history) || nrow(stage_history) == 0) {
    return(stage_history[0, , drop = FALSE])
  }

  run_rows <- stage_history[stage_history[["run_id"]] == run_id, , drop = FALSE]
  if (nrow(run_rows) == 0) {
    return(run_rows)
  }

  run_rows <- run_rows[order(xtfrm(run_rows[["finished_at"]]), xtfrm(run_rows[["started_at"]])), , drop = FALSE]
  run_rows[!duplicated(run_rows[["stage"]], fromLast = TRUE), , drop = FALSE]
}

pipeline_build_tool_diff_row <- function(change_type, baseline_row = NULL, current_row = NULL) {
  source_row <- if (is.data.frame(current_row) && nrow(current_row) > 0) current_row else baseline_row

  tibble::tibble(
    change_type = change_type,
    assessed_name = pipeline_row_value(source_row, "assessed_name", pipeline_row_value(source_row, "name", "unknown")),
    source = pipeline_row_value(source_row, "source", NA_character_),
    entity_type = pipeline_row_value(source_row, "entity_type", NA_character_),
    baseline_confidence_score = suppressWarnings(as.numeric(pipeline_row_value(baseline_row, "confidence_score", NA_real_))),
    current_confidence_score = suppressWarnings(as.numeric(pipeline_row_value(current_row, "confidence_score", NA_real_))),
    baseline_mitre_technique_count = suppressWarnings(as.integer(pipeline_row_value(baseline_row, "mitre_technique_count", NA_integer_))),
    current_mitre_technique_count = suppressWarnings(as.integer(pipeline_row_value(current_row, "mitre_technique_count", NA_integer_)))
  )
}

pipeline_build_mitre_diff_row <- function(change_type, baseline_row = NULL, current_row = NULL) {
  source_row <- if (is.data.frame(current_row) && nrow(current_row) > 0) current_row else baseline_row

  tibble::tibble(
    change_type = change_type,
    assessed_name = pipeline_row_value(source_row, "assessed_name", pipeline_row_value(source_row, "name", "unknown")),
    technique_id = pipeline_row_value(source_row, "technique_id", NA_character_),
    technique_name = pipeline_row_value(source_row, "technique_name", NA_character_),
    tactic = pipeline_row_value(source_row, "tactic", NA_character_),
    baseline_confidence = suppressWarnings(as.numeric(pipeline_row_value(baseline_row, "confidence", NA_real_))),
    current_confidence = suppressWarnings(as.numeric(pipeline_row_value(current_row, "confidence", NA_real_)))
  )
}

build_pipeline_run_comparison <- function(db_path, baseline_run_id, current_run_id) {
  empty_result <- list(
    summary = empty_pipeline_run_compare_summary(),
    stages = empty_pipeline_run_stage_comparison(),
    tool_diff = empty_pipeline_run_tool_diff(),
    mitre_diff = empty_pipeline_run_mitre_diff()
  )

  if (!file.exists(db_path) || is.null(baseline_run_id) || is.null(current_run_id) || !nzchar(baseline_run_id) || !nzchar(current_run_id)) {
    return(empty_result)
  }

  run_catalog <- build_pipeline_run_catalog(db_path)
  baseline_run <- run_catalog[run_catalog[["run_id"]] == baseline_run_id, , drop = FALSE]
  current_run <- run_catalog[run_catalog[["run_id"]] == current_run_id, , drop = FALSE]

  if (nrow(baseline_run) == 0 || nrow(current_run) == 0) {
    return(empty_result)
  }

  comparison_summary <- tibble::tribble(
    ~metric, ~baseline, ~current, ~delta,
    "run_id", baseline_run_id, current_run_id, if (identical(baseline_run_id, current_run_id)) "same run" else "different runs",
    "status", baseline_run$status[[1]] %||% "n/a", current_run$status[[1]] %||% "n/a", "n/a",
    "started_at", baseline_run$started_at[[1]] %||% "n/a", current_run$started_at[[1]] %||% "n/a", "n/a",
    "duration_seconds", pipeline_format_number(baseline_run$duration_seconds[[1]], 3), pipeline_format_number(current_run$duration_seconds[[1]], 3), pipeline_format_delta(current_run$duration_seconds[[1]], baseline_run$duration_seconds[[1]], 3),
    "stages_completed", pipeline_format_number(baseline_run$stages_completed[[1]]), pipeline_format_number(current_run$stages_completed[[1]]), pipeline_format_delta(current_run$stages_completed[[1]], baseline_run$stages_completed[[1]]),
    "discovery_new_rows", pipeline_format_number(baseline_run$discovery_new_rows[[1]]), pipeline_format_number(current_run$discovery_new_rows[[1]]), pipeline_format_delta(current_run$discovery_new_rows[[1]], baseline_run$discovery_new_rows[[1]]),
    "github_new_rows", pipeline_format_number(baseline_run$github_new_rows[[1]]), pipeline_format_number(current_run$github_new_rows[[1]]), pipeline_format_delta(current_run$github_new_rows[[1]], baseline_run$github_new_rows[[1]]),
    "github_requests", pipeline_format_number(baseline_run$github_requests[[1]]), pipeline_format_number(current_run$github_requests[[1]]), pipeline_format_delta(current_run$github_requests[[1]], baseline_run$github_requests[[1]]),
    "relevant_tools", pipeline_format_number(baseline_run$relevant_tools[[1]]), pipeline_format_number(current_run$relevant_tools[[1]]), pipeline_format_delta(current_run$relevant_tools[[1]], baseline_run$relevant_tools[[1]]),
    "mitre_rows", pipeline_format_number(baseline_run$mitre_rows[[1]]), pipeline_format_number(current_run$mitre_rows[[1]]), pipeline_format_delta(current_run$mitre_rows[[1]], baseline_run$mitre_rows[[1]])
  )

  stage_history <- read_duckdb_table("pipeline_stage_history", db_path = db_path, required = FALSE, restore = FALSE)
  comparison_stages <- empty_pipeline_run_stage_comparison()

  if (is.data.frame(stage_history) && nrow(stage_history) > 0) {
    baseline_stages <- pipeline_select_stage_rows(stage_history, baseline_run_id)
    current_stages <- pipeline_select_stage_rows(stage_history, current_run_id)
    stage_names <- sort(unique(c(as.character(baseline_stages$stage), as.character(current_stages$stage))))
    stage_names <- stage_names[!is.na(stage_names) & nzchar(stage_names)]

    if (length(stage_names) > 0) {
      comparison_stages <- lapply(stage_names, function(stage_name) {
        baseline_stage <- baseline_stages[baseline_stages[["stage"]] == stage_name, , drop = FALSE]
        current_stage <- current_stages[current_stages[["stage"]] == stage_name, , drop = FALSE]

        tibble::tibble(
          stage = stage_name,
          baseline_status = pipeline_row_value(baseline_stage, "status", NA_character_),
          current_status = pipeline_row_value(current_stage, "status", NA_character_),
          baseline_duration_seconds = suppressWarnings(as.numeric(pipeline_row_value(baseline_stage, "duration_seconds", NA_real_))),
          current_duration_seconds = suppressWarnings(as.numeric(pipeline_row_value(current_stage, "duration_seconds", NA_real_))),
          duration_delta_seconds = suppressWarnings(as.numeric(pipeline_row_value(current_stage, "duration_seconds", NA_real_))) - suppressWarnings(as.numeric(pipeline_row_value(baseline_stage, "duration_seconds", NA_real_)))
        )
      }) |>
        dplyr::bind_rows()
    }
  }

  relevant_history <- read_duckdb_table("relevant_tools_history", db_path = db_path, required = FALSE, restore = TRUE)
  comparison_tool_diff <- empty_pipeline_run_tool_diff()
  comparison_mitre_diff <- empty_pipeline_run_mitre_diff()

  if (is.data.frame(relevant_history) && nrow(relevant_history) > 0 && "record_id" %in% names(relevant_history) && "run_id" %in% names(relevant_history)) {
    baseline_tools <- relevant_history[relevant_history[["run_id"]] == baseline_run_id, , drop = FALSE]
    current_tools <- relevant_history[relevant_history[["run_id"]] == current_run_id, , drop = FALSE]

    if (nrow(baseline_tools) > 0) {
      baseline_tools <- baseline_tools[!duplicated(baseline_tools[["record_id"]], fromLast = TRUE), , drop = FALSE]
    }

    if (nrow(current_tools) > 0) {
      current_tools <- current_tools[!duplicated(current_tools[["record_id"]], fromLast = TRUE), , drop = FALSE]
    }

    baseline_ids <- as.character(baseline_tools[["record_id"]] %||% character())
    current_ids <- as.character(current_tools[["record_id"]] %||% character())
    new_ids <- setdiff(current_ids, baseline_ids)
    removed_ids <- setdiff(baseline_ids, current_ids)
    common_ids <- intersect(current_ids, baseline_ids)

    changed_ids <- common_ids[vapply(common_ids, function(record_id) {
      baseline_row <- baseline_tools[baseline_tools[["record_id"]] == record_id, , drop = FALSE]
      current_row <- current_tools[current_tools[["record_id"]] == record_id, , drop = FALSE]

      !identical(pipeline_row_value(baseline_row, "entity_type", NA_character_), pipeline_row_value(current_row, "entity_type", NA_character_)) ||
        !identical(suppressWarnings(as.numeric(pipeline_row_value(baseline_row, "confidence_score", NA_real_))), suppressWarnings(as.numeric(pipeline_row_value(current_row, "confidence_score", NA_real_)))) ||
        !identical(suppressWarnings(as.integer(pipeline_row_value(baseline_row, "mitre_technique_count", NA_integer_))), suppressWarnings(as.integer(pipeline_row_value(current_row, "mitre_technique_count", NA_integer_))))
    }, logical(1))]

    comparison_tool_diff <- dplyr::bind_rows(
      lapply(new_ids, function(record_id) {
        current_row <- current_tools[current_tools[["record_id"]] == record_id, , drop = FALSE]
        pipeline_build_tool_diff_row("new", current_row = current_row)
      }),
      lapply(changed_ids, function(record_id) {
        baseline_row <- baseline_tools[baseline_tools[["record_id"]] == record_id, , drop = FALSE]
        current_row <- current_tools[current_tools[["record_id"]] == record_id, , drop = FALSE]
        pipeline_build_tool_diff_row("changed", baseline_row = baseline_row, current_row = current_row)
      }),
      lapply(removed_ids, function(record_id) {
        baseline_row <- baseline_tools[baseline_tools[["record_id"]] == record_id, , drop = FALSE]
        pipeline_build_tool_diff_row("removed", baseline_row = baseline_row)
      })
    )

    if (nrow(comparison_tool_diff) > 0) {
      comparison_tool_diff[["change_type"]] <- factor(comparison_tool_diff[["change_type"]], levels = c("new", "changed", "removed"))
      comparison_tool_diff <- comparison_tool_diff[order(comparison_tool_diff[["change_type"]], comparison_tool_diff[["assessed_name"]]), , drop = FALSE]
      comparison_tool_diff[["change_type"]] <- as.character(comparison_tool_diff[["change_type"]])
    }
  }

  mitre_history <- read_duckdb_table("mitre_mappings_history", db_path = db_path, required = FALSE, restore = TRUE)

  if (is.data.frame(mitre_history) && nrow(mitre_history) > 0 && all(c("run_id", "record_id", "technique_id") %in% names(mitre_history))) {
    baseline_mitre <- mitre_history[mitre_history[["run_id"]] == baseline_run_id, , drop = FALSE]
    current_mitre <- mitre_history[mitre_history[["run_id"]] == current_run_id, , drop = FALSE]

    if (nrow(baseline_mitre) > 0) {
      baseline_mitre[["mitre_key"]] <- paste(
        as.character(baseline_mitre[["record_id"]]),
        as.character(baseline_mitre[["technique_id"]]),
        as.character(baseline_mitre[["tactic"]] %||% ""),
        sep = "::"
      )
      baseline_mitre <- baseline_mitre[!duplicated(baseline_mitre[["mitre_key"]], fromLast = TRUE), , drop = FALSE]
    }

    if (nrow(current_mitre) > 0) {
      current_mitre[["mitre_key"]] <- paste(
        as.character(current_mitre[["record_id"]]),
        as.character(current_mitre[["technique_id"]]),
        as.character(current_mitre[["tactic"]] %||% ""),
        sep = "::"
      )
      current_mitre <- current_mitre[!duplicated(current_mitre[["mitre_key"]], fromLast = TRUE), , drop = FALSE]
    }

    baseline_keys <- as.character(baseline_mitre[["mitre_key"]] %||% character())
    current_keys <- as.character(current_mitre[["mitre_key"]] %||% character())
    new_keys <- setdiff(current_keys, baseline_keys)
    removed_keys <- setdiff(baseline_keys, current_keys)
    common_keys <- intersect(current_keys, baseline_keys)

    changed_keys <- common_keys[vapply(common_keys, function(mitre_key) {
      baseline_row <- baseline_mitre[baseline_mitre[["mitre_key"]] == mitre_key, , drop = FALSE]
      current_row <- current_mitre[current_mitre[["mitre_key"]] == mitre_key, , drop = FALSE]

      !identical(
        suppressWarnings(as.numeric(pipeline_row_value(baseline_row, "confidence", NA_real_))),
        suppressWarnings(as.numeric(pipeline_row_value(current_row, "confidence", NA_real_)))
      ) || !identical(
        pipeline_row_value(baseline_row, "reasoning_ru", NA_character_),
        pipeline_row_value(current_row, "reasoning_ru", NA_character_)
      )
    }, logical(1))]

    comparison_mitre_diff <- dplyr::bind_rows(
      lapply(new_keys, function(mitre_key) {
        current_row <- current_mitre[current_mitre[["mitre_key"]] == mitre_key, , drop = FALSE]
        pipeline_build_mitre_diff_row("new", current_row = current_row)
      }),
      lapply(changed_keys, function(mitre_key) {
        baseline_row <- baseline_mitre[baseline_mitre[["mitre_key"]] == mitre_key, , drop = FALSE]
        current_row <- current_mitre[current_mitre[["mitre_key"]] == mitre_key, , drop = FALSE]
        pipeline_build_mitre_diff_row("changed", baseline_row = baseline_row, current_row = current_row)
      }),
      lapply(removed_keys, function(mitre_key) {
        baseline_row <- baseline_mitre[baseline_mitre[["mitre_key"]] == mitre_key, , drop = FALSE]
        pipeline_build_mitre_diff_row("removed", baseline_row = baseline_row)
      })
    )

    if (nrow(comparison_mitre_diff) > 0) {
      comparison_mitre_diff[["change_type"]] <- factor(comparison_mitre_diff[["change_type"]], levels = c("new", "changed", "removed"))
      comparison_mitre_diff <- comparison_mitre_diff[order(comparison_mitre_diff[["change_type"]], comparison_mitre_diff[["assessed_name"]], comparison_mitre_diff[["technique_id"]]), , drop = FALSE]
      comparison_mitre_diff[["change_type"]] <- as.character(comparison_mitre_diff[["change_type"]])
    }
  }

  list(
    summary = comparison_summary,
    stages = comparison_stages,
    tool_diff = comparison_tool_diff,
    mitre_diff = comparison_mitre_diff
  )
}

build_pipeline_snapshot <- function(data_dir) {
  artifact_specs <- tibble::tribble(
    ~label, ~file_name, ~kind,
    "Raw GitHub", "raw_github.rds", "rds",
    "Raw PacketStorm", "raw_packetstorm.rds", "rds",
    "Raw RSS", "raw_rss.rds", "rds",
    "Normalized candidates", "normalized_tools.rds", "rds",
    "Assessment results", "tool_assessment_results.rds", "rds",
    "LLM processing queue", "llm_processing_queue.rds", "rds",
    "Relevant tools", "relevant_tools.rds", "rds",
    "MITRE mappings", "tool_mitre_mappings.rds", "rds",
    "MITRE refinement index", "mitre_refinement_index.rds", "rds",
    "MITRE refinement candidates", "mitre_refinement_candidates.rds", "rds",
    "Visualization tools", "visualization_tools.rds", "rds",
    "Visualization matrix", "visualization_tool_matrix.rds", "rds",
    "Visualization history", "visualization_tool_history.rds", "rds",
    "Pipeline sanity checks", "pipeline_sanity_checks.rds", "rds",
    "Pipeline status", "pipeline_status.rds", "rds",
    "GitHub search state", "github_search_state.rds", "rds",
    "GitHub search log", "github_search_log.rds", "rds",
    "Pipeline batch summary", "pipeline_batch_summary.rds", "rds",
    "DuckDB", "offensive_tool_mapper.duckdb", "duckdb"
  )

  artifacts <- lapply(seq_len(nrow(artifact_specs)), function(index) {
    spec <- artifact_specs[index, , drop = FALSE]
    path <- file.path(data_dir, spec$file_name[[1]])
    value <- if (identical(spec$kind[[1]], "rds")) safe_read_artifact(path) else NULL
    status <- if (!file.exists(path)) {
      "missing"
    } else if (.is_artifact_error(value)) {
      "error"
    } else {
      "ready"
    }

    tibble::tibble(
      dataset = spec$label[[1]],
      file_name = spec$file_name[[1]],
      status = status,
      rows = artifact_row_count(value),
      size = format_file_size(path),
      modified_at = format_file_mtime(path)
    )
  }) |>
    dplyr::bind_rows()

  normalized_snapshot <- safe_read_artifact(file.path(data_dir, "normalized_tools.rds"))
  assessment_results <- safe_read_artifact(file.path(data_dir, "tool_assessment_results.rds"))
  llm_queue_snapshot <- safe_read_artifact(file.path(data_dir, "llm_processing_queue.rds"))
  visualization_tools_snapshot <- safe_read_artifact(file.path(data_dir, "visualization_tools.rds"))
  visualization_matrix_snapshot <- safe_read_artifact(file.path(data_dir, "visualization_tool_matrix.rds"))
  visualization_history_snapshot <- safe_read_artifact(file.path(data_dir, "visualization_tool_history.rds"))
  pipeline_sanity_checks_snapshot <- safe_read_artifact(file.path(data_dir, "pipeline_sanity_checks.rds"))
  pipeline_status_snapshot <- safe_read_artifact(file.path(data_dir, "pipeline_status.rds"))
  github_search_state_snapshot <- safe_read_artifact(file.path(data_dir, "github_search_state.rds"))
  github_query_log_snapshot <- safe_read_artifact(file.path(data_dir, "github_search_log.rds"))
  pipeline_batch_summary_snapshot <- safe_read_artifact(file.path(data_dir, "pipeline_batch_summary.rds"))

  if (!is.data.frame(github_query_log_snapshot)) {
    github_query_log_snapshot <- empty_github_query_log()
  }

  if (!is.data.frame(pipeline_batch_summary_snapshot)) {
    pipeline_batch_summary_snapshot <- empty_pipeline_batch_summary()
  }

  if (!is.data.frame(pipeline_sanity_checks_snapshot)) {
    pipeline_sanity_checks_snapshot <- empty_pipeline_sanity_checks()
  }

  llm_queue <- build_pipeline_queue(normalized_snapshot, assessment_results, llm_queue_snapshot)
  llm_backlog <- build_pipeline_backlog(llm_queue)
  ui_history <- build_pipeline_history(visualization_history_snapshot)
  stage_history <- build_pipeline_stage_history(pipeline_status_snapshot)
  discovery_summary <- build_pipeline_discovery_summary(pipeline_status_snapshot, github_search_state_snapshot)

  assessment_status <- if (is.data.frame(assessment_results) && "llm_status" %in% names(assessment_results)) {
    count_records(assessment_results, "llm_status")
  } else {
    tibble::tibble(llm_status = character(), count = integer())
  }

  recent_tools <- if (is.data.frame(visualization_tools_snapshot) && nrow(visualization_tools_snapshot) > 0) {
    recent_tools_view <- visualization_tools_snapshot[
      order(visualization_tools_snapshot[["visualization_rank"]]),
      ,
      drop = FALSE
    ]
    recent_tools_view <- utils::head(recent_tools_view, 8)

    if ("first_ui_added_at" %in% names(recent_tools_view)) {
      tibble::as_tibble(recent_tools_view[c(
        "assessed_name",
        "source",
        "entity_type",
        "visualization_rank",
        "confidence_score",
        "mitre_technique_count",
        "first_ui_added_at"
      )])
    } else {
      tibble::as_tibble(recent_tools_view[c(
        "assessed_name",
        "source",
        "entity_type",
        "visualization_rank",
        "confidence_score",
        "mitre_technique_count"
      )])
    }
  } else {
    tibble::tibble(
      assessed_name = character(),
      source = character(),
      entity_type = character(),
      visualization_rank = integer(),
      confidence_score = numeric(),
      mitre_technique_count = integer(),
      first_ui_added_at = character()
    )
  }

  metrics <- list(
    normalized = artifacts$rows[artifacts$file_name == "normalized_tools.rds"][[1]],
    assessed = artifacts$rows[artifacts$file_name == "tool_assessment_results.rds"][[1]],
    relevant = artifacts$rows[artifacts$file_name == "relevant_tools.rds"][[1]],
    matrix = artifacts$rows[artifacts$file_name == "visualization_tool_matrix.rds"][[1]],
    llm_candidates = if (is.data.frame(llm_queue)) sum(llm_queue$pre_llm_should_process %in% TRUE, na.rm = TRUE) else NA_integer_,
    llm_backlog = nrow(llm_backlog),
    llm_success = if (is.data.frame(llm_queue)) sum(llm_queue$queue_status == "processed_success", na.rm = TRUE) else NA_integer_,
    llm_errors = if (is.data.frame(llm_queue)) sum(llm_queue$queue_status == "retry_after_error", na.rm = TRUE) else NA_integer_,
    ui_history = nrow(ui_history),
    github_new_rows = suppressWarnings(as.integer(pipeline_discovery_metric_value(discovery_summary, "github_new_rows", NA_character_))),
    discovery_new_rows = suppressWarnings(as.integer(pipeline_discovery_metric_value(discovery_summary, "new_rows_total", NA_character_))),
    github_selected_requests = suppressWarnings(as.integer(pipeline_discovery_metric_value(discovery_summary, "github_selected_requests", NA_character_))),
    github_request_offset = suppressWarnings(as.integer(pipeline_discovery_metric_value(discovery_summary, "github_request_offset", NA_character_))),
    github_next_request_offset = suppressWarnings(as.integer(pipeline_discovery_metric_value(discovery_summary, "github_next_request_offset", NA_character_))),
    github_query_plan_size = suppressWarnings(as.integer(pipeline_discovery_metric_value(discovery_summary, "github_query_plan_size", NA_character_))),
    github_selection_share_percent = suppressWarnings(as.numeric(pipeline_discovery_metric_value(discovery_summary, "github_selection_share_percent", NA_character_))),
    github_rotation_progress_percent = suppressWarnings(as.numeric(pipeline_discovery_metric_value(discovery_summary, "github_rotation_progress_percent", NA_character_))),
    github_remaining_runs_estimate = suppressWarnings(as.integer(pipeline_discovery_metric_value(discovery_summary, "github_remaining_runs_estimate", NA_character_))),
    github_logged_requests = suppressWarnings(as.integer(pipeline_discovery_metric_value(discovery_summary, "github_logged_requests", NA_character_))),
    estimated_full_coverage_runs = suppressWarnings(as.integer(pipeline_discovery_metric_value(discovery_summary, "estimated_full_coverage_runs", NA_character_))),
    sanity_failed_checks = if (is.data.frame(pipeline_sanity_checks_snapshot)) sum(pipeline_sanity_checks_snapshot$status == "fail", na.rm = TRUE) else NA_integer_,
    sanity_warning_checks = if (is.data.frame(pipeline_sanity_checks_snapshot)) sum(pipeline_sanity_checks_snapshot$status == "warn", na.rm = TRUE) else NA_integer_,
    unique_tactics = if (is.data.frame(visualization_matrix_snapshot)) dplyr::n_distinct(visualization_matrix_snapshot$tactic) else NA_integer_,
    unique_techniques = if (is.data.frame(visualization_matrix_snapshot)) dplyr::n_distinct(visualization_matrix_snapshot$technique_id) else NA_integer_
  )

  list(
    artifacts = artifacts,
    assessment_status = assessment_status,
    runtime = safe_runtime_config(),
    duckdb = pipeline_duckdb_summary(file.path(data_dir, "offensive_tool_mapper.duckdb")),
    duckdb_path = file.path(data_dir, "offensive_tool_mapper.duckdb"),
    run_catalog = build_pipeline_run_catalog(file.path(data_dir, "offensive_tool_mapper.duckdb")),
    recent_tools = recent_tools,
    llm_queue = llm_queue,
    llm_backlog = llm_backlog,
    ui_history = ui_history,
    stage_history = stage_history,
    pipeline_sanity_checks = pipeline_sanity_checks_snapshot,
    discovery_summary = discovery_summary,
    github_query_log = github_query_log_snapshot,
    pipeline_batch_summary = pipeline_batch_summary_snapshot,
    metrics = metrics,
    data_dir = data_dir
  )
}

app_ui <- bslib::page_navbar(
  theme = app_theme,
  title = tags$div(
    class = "brand-lockup",
    tags$span(class = "brand-mark", "OTM"),
    tags$div(
      tags$span(class = "brand-title", "OffensiveToolMapper"),
      tags$span(class = "brand-subtitle", "ranked offensive tooling intelligence")
    )
  ),
  id = "main_nav",
  header = tags$head(
    shiny::includeCSS(file.path(asset_dir, "app.css"))
  ),
  bslib::nav_panel(
    "Обзор",
    app_shell(
      class = "shiny-tools-root modern-overview-root",
      shiny::uiOutput("modern_overview_panel")
    )
  ),
  bslib::nav_panel(
    "Инструменты",
    app_shell(
      class = "app-shell-tools shiny-tools-root",
      tags$section(
        class = "explorer-shell",
        id = "tool-explorer",
        tags$div(
          class = "section-heading heading-light explorer-heading",
          tags$div(
            tags$div(class = "section-kicker-modern", "Инструменты"),
            tags$h3("Полноценный браузер по слою разведданных")
          ),
          tags$p(
            "Здесь можно работать как в отдельном продукте: быстро фильтровать, искать и читать полный профиль инструмента без старой табличной раскладки."
          )
        ),
        tags$div(
          class = "explorer-controls glass-panel",
          tags$div(
            class = "control-block search-block",
            tags$span("Поиск"),
            shiny::textInput("tool_search", label = NULL, placeholder = "название, теги, описание...")
          ),
          tags$div(
            class = "control-block",
            tags$span("Источник"),
            shiny::selectInput("source_filter", label = NULL, choices = c("Все" = "All"), selected = "All", selectize = FALSE)
          ),
          tags$div(
            class = "control-block",
            tags$span("Тактика MITRE"),
            shiny::selectInput("tactic_filter", label = NULL, choices = c("Все" = "All"), selected = "All", selectize = FALSE)
          )
        ),
        tags$div(
          class = "explorer-grid",
          tags$section(
            class = "utility-list-panel glass-panel",
            tags$div(
              class = "panel-heading-row",
              tags$div(
                tags$div(class = "section-kicker-modern", "Список утилит"),
                shiny::uiOutput("tool_list_count")
              ),
              tags$span(class = "panel-chip", "По рангу")
            ),
            shiny::uiOutput("tool_list_cards")
          ),
          tags$section(
            class = "detail-panel glass-panel",
            shiny::uiOutput("tool_detail_panel")
          )
        )
      )
    )
  ),
  bslib::nav_panel(
    "Аналитика",
    app_shell(
      class = "shiny-tools-root modern-analytics-root",
      shiny::uiOutput("modern_analytics_panel")
    )
  ),
  bslib::nav_panel(
    "Пайплайн",
    app_shell(
      tags$section(
        class = "app-section",
        tags$div(
          class = "pipeline-toolbar",
          section_intro(
            "Пайплайн и мониторинг",
            "Операционная панель: статусы обработки инструментов, текущая конфигурация нейросети, содержимое базы данных и команды для ручного перезапуска нужного этапа."
          ),
          shiny::actionButton("pipeline_refresh", "Обновить", class = "pipeline-refresh-button")
        ),
        shiny::uiOutput("pipeline_metric_grid"),
        tags$div(
          class = "plot-grid pipeline-grid",
          bslib::card(
            class = "shell-card plot-shell",
            full_screen = TRUE,
            bslib::card_header("Статус оценки инструментов"),
            bslib::card_body(plotOutput("pipeline_status_plot", height = 360))
          ),
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Конфигурация нейросети"),
            bslib::card_body(DT::dataTableOutput("pipeline_runtime_table"))
          )
        ),
        tags$div(
          class = "plot-grid pipeline-grid",
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Артефакты данных"),
            bslib::card_body(DT::dataTableOutput("pipeline_artifact_table"))
          ),
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Таблицы базы данных"),
            bslib::card_body(DT::dataTableOutput("pipeline_duckdb_table"))
          )
        ),
        tags$div(
          class = "plot-grid pipeline-grid",
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Обнаружение и ротация запросов"),
            bslib::card_body(DT::dataTableOutput("pipeline_discovery_table"))
          ),
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Этапы последнего запуска"),
            bslib::card_body(DT::dataTableOutput("pipeline_stage_history_table"))
          )
        ),
        bslib::card(
          class = "shell-card info-card",
          bslib::card_header("Сравнение запусков"),
          bslib::card_body(
            tags$p("Сравнение двух запусков пайплайна по идентификатору run_id: изменения времени выполнения по этапам, сдвиги в наборе инструментов и MITRE-маппингах."),
            tags$div(
              class = "pipeline-compare-controls",
              shiny::selectInput("pipeline_compare_current_run", "Текущий запуск", choices = c("Нет доступных запусков" = ""), selected = ""),
              shiny::selectInput("pipeline_compare_baseline_run", "Базовый запуск", choices = c("Нет доступных запусков" = ""), selected = "")
            ),
            shiny::uiOutput("pipeline_run_compare_metric_grid")
          )
        ),
        tags$div(
          class = "plot-grid pipeline-grid",
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Доступные запуски"),
            bslib::card_body(DT::dataTableOutput("pipeline_run_catalog_table"))
          ),
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Сводное сравнение запусков"),
            bslib::card_body(DT::dataTableOutput("pipeline_run_compare_summary_table"))
          )
        ),
        tags$div(
          class = "plot-grid pipeline-grid",
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Сравнение по этапам"),
            bslib::card_body(DT::dataTableOutput("pipeline_run_compare_stage_table"))
          ),
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Изменения в инструментах"),
            bslib::card_body(DT::dataTableOutput("pipeline_run_compare_tools_table"))
          )
        ),
        tags$div(
          class = "plot-grid pipeline-grid",
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Изменения в MITRE-маппингах"),
            bslib::card_body(DT::dataTableOutput("pipeline_run_compare_mitre_table"))
          ),
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Диагностика пайплайна"),
            bslib::card_body(DT::dataTableOutput("pipeline_sanity_checks_table"))
          )
        ),
        tags$div(
          class = "plot-grid pipeline-grid",
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Последние поисковые запросы GitHub"),
            bslib::card_body(DT::dataTableOutput("pipeline_github_query_log_table"))
          ),
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Пакетные запуски пайплайна"),
            bslib::card_body(DT::dataTableOutput("pipeline_batch_summary_table"))
          )
        ),
        tags$div(
          class = "plot-grid pipeline-grid",
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Топ инструментов (текущие данные)"),
            bslib::card_body(DT::dataTableOutput("pipeline_recent_tools_table"))
          ),
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Очередь и повторные попытки"),
            bslib::card_body(DT::dataTableOutput("pipeline_backlog_table"))
          )
        ),
        tags$div(
          class = "plot-grid pipeline-grid",
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("История добавления инструментов"),
            bslib::card_body(DT::dataTableOutput("pipeline_ui_history_table"))
          ),
          bslib::card(
            class = "shell-card table-shell",
            full_screen = TRUE,
            bslib::card_header("Снимок очереди нейросети"),
            bslib::card_body(DT::dataTableOutput("pipeline_queue_table"))
          )
        ),
        bslib::card(
          class = "shell-card info-card code-block-card",
          bslib::card_header("Быстрые действия"),
          bslib::card_body(
            tags$p("Директория данных приложения:"),
            tags$pre(class = "command-block", extdata_dir),
            tags$p("Пересобрать слой визуализации (запускать после изменения данных):"),
            tags$pre(class = "command-block", 'Rscript data-raw/build_visualization_data.R'),
            tags$p("Просмотр очереди нейросети (текущее состояние задач):"),
            tags$pre(class = "command-block", 'Rscript data-raw/inspect_llm_queue.R'),
            tags$p("Запустить оценку инструментов нейросетью:"),
            tags$pre(class = "command-block", 'Rscript data-raw/run_unified_tool_assessment.R'),
            tags$p("Запустить полный пайплайн 5 раз подряд (пакетный режим):"),
            tags$pre(class = "command-block", 'Rscript data-raw/run_full_pipeline_batch.R 5'),
            tags$p("Запустить этап уточнения MITRE-маппингов:"),
            tags$pre(class = "command-block", 'Rscript data-raw/run_mitre_refinement.R'),
            tags$p("Запустить MCP-сервер в режиме stdio:"),
            tags$pre(class = "command-block", 'Rscript inst/mcp/run_server.R')
          )
        )
      )
    )
  )
)

app_server <- function(input, output, session) {
  selected_record_id <- shiny::reactiveVal(NULL)

  viz_poll <- shiny::reactivePoll(
    intervalMillis = 5000,
    session = session,
    checkFunc = function() {
      files <- c(
        file.path(extdata_dir, "visualization_tools.rds"),
        file.path(extdata_dir, "visualization_tool_matrix.rds")
      )
      sapply(files, function(f) if (file.exists(f)) as.numeric(file.mtime(f)) else -1)
    },
    valueFunc = function() {
      load_visualization_snapshot(extdata_dir)
    }
  )

  visualization_snapshot <- shiny::reactive({
    input$pipeline_refresh
    viz_poll()
  })

  pipeline_snapshot <- shiny::reactive({
    input$pipeline_refresh
    build_pipeline_snapshot(extdata_dir)
  })

  pipeline_run_comparison <- shiny::reactive({
    snapshot <- pipeline_snapshot()

    build_pipeline_run_comparison(
      db_path = snapshot$duckdb_path,
      baseline_run_id = input$pipeline_compare_baseline_run,
      current_run_id = input$pipeline_compare_current_run
    )
  })

  refinement_poll <- shiny::reactivePoll(
    intervalMillis = 5000,
    session = session,
    checkFunc = function() {
      path <- file.path(extdata_dir, "mitre_refinement_candidates.rds")
      if (file.exists(path)) as.numeric(file.mtime(path)) else -1
    },
    valueFunc = function() {
      load_refinement_candidates(extdata_dir)
    }
  )

  refinement_candidates <- shiny::reactive({
    input$pipeline_refresh
    refinement_poll()
  })

  shiny::observe({
    snapshot <- visualization_snapshot()
    tools_data <- snapshot$tools
    matrix_data <- snapshot$matrix

    current_source <- input$source_filter %||% "All"
    current_tactic <- input$tactic_filter %||% "All"

    source_choices <- c("All", sort(unique(stats::na.omit(as.character(tools_data$source)))))
    tactic_choices <- c("All", sort(unique(unlist(lapply(matrix_data$tactic, split_mitre_values), use.names = FALSE))))

    shiny::updateSelectInput(
      session,
      "source_filter",
      choices = stats::setNames(source_choices, ifelse(source_choices == "All", "Все", source_choices)),
      selected = if (current_source %in% source_choices) current_source else "All"
    )
    shiny::updateSelectInput(
      session,
      "tactic_filter",
      choices = stats::setNames(tactic_choices, ifelse(tactic_choices == "All", "Все", tactic_choices)),
      selected = if (current_tactic %in% tactic_choices) current_tactic else "All"
    )
  })

  shiny::observe({
    snapshot <- pipeline_snapshot()
    run_catalog <- snapshot$run_catalog

    if (!is.data.frame(run_catalog) || nrow(run_catalog) == 0) {
      shiny::updateSelectInput(session, "pipeline_compare_current_run", choices = c("No runs available" = ""), selected = "")
      shiny::updateSelectInput(session, "pipeline_compare_baseline_run", choices = c("No runs available" = ""), selected = "")
      return()
    }

    choice_values <- run_catalog$run_id
    choice_labels <- vapply(seq_len(nrow(run_catalog)), function(index) {
      pipeline_build_run_label(run_catalog[index, , drop = FALSE])
    }, character(1))
    choices <- stats::setNames(choice_values, choice_labels)

    default_current <- choice_values[[1]]
    default_baseline <- if (length(choice_values) >= 2) choice_values[[2]] else choice_values[[1]]
    selected_current <- input$pipeline_compare_current_run %||% default_current
    selected_baseline <- input$pipeline_compare_baseline_run %||% default_baseline

    if (!(selected_current %in% choice_values)) {
      selected_current <- default_current
    }

    if (!(selected_baseline %in% choice_values)) {
      selected_baseline <- default_baseline
    }

    shiny::updateSelectInput(session, "pipeline_compare_current_run", choices = choices, selected = selected_current)
    shiny::updateSelectInput(session, "pipeline_compare_baseline_run", choices = choices, selected = selected_baseline)
  })

  filtered_matrix <- shiny::reactive({
    matrix_data <- visualization_snapshot()$matrix
    tactic_filter <- input$tactic_filter %||% "All"
    technique_filter <- input$technique_filter %||% "All"

    if (!identical(tactic_filter, "All")) {
      matrix_data <- matrix_data[
        vapply(matrix_data[["tactic"]], function(value) tactic_filter %in% split_mitre_values(value), logical(1)),
        ,
        drop = FALSE
      ]
    }

    if (!identical(technique_filter, "All")) {
      matrix_data <- matrix_data[matrix_data[["technique_id"]] == technique_filter, , drop = FALSE]
    }

    matrix_data
  })

  filtered_tools <- shiny::reactive({
    tools_data <- visualization_snapshot()$tools
    search_query <- tolower(trimws(input$tool_search %||% ""))
    source_filter <- input$source_filter %||% "All"
    tactic_filter <- input$tactic_filter %||% "All"

    if (!identical(source_filter, "All")) {
      tools_data <- tools_data[tools_data[["source"]] == source_filter, , drop = FALSE]
    }

    if (!identical(tactic_filter, "All")) {
      tools_data <- tools_data[
        vapply(tools_data[["mitre_tactics"]], function(value) tactic_filter %in% split_mitre_values(value), logical(1)),
        ,
        drop = FALSE
      ]
    }

    if (nzchar(search_query)) {
      tools_data <- tools_data[
        vapply(seq_len(nrow(tools_data)), function(index) {
          tool <- tools_data[index, , drop = FALSE]
          haystack <- paste(
            format_meta_value(row_value(tool, "assessed_name")),
            format_meta_value(row_value(tool, "short_description_ru")),
            format_meta_value(row_value(tool, "long_description_ru")),
            format_meta_value(row_value(tool, "category_ru")),
            format_meta_value(row_value(tool, "entity_type")),
            paste(clean_display_tags(row_value(tool, "filter_tags")), collapse = " "),
            sep = " "
          )
          grepl(search_query, tolower(haystack), fixed = TRUE)
        }, logical(1)),
        ,
        drop = FALSE
      ]
    }

    tools_data <- tools_data[order(tools_data[["visualization_rank"]]), , drop = FALSE]
    tools_data
  })

  shiny::observeEvent(input$tool_card_select, {
    selected_record_id(as.character(input$tool_card_select))
  }, ignoreInit = TRUE)

  shiny::observe({
    tools_data <- filtered_tools()
    current_id <- selected_record_id()

    if (nrow(tools_data) == 0) {
      selected_record_id(NULL)
      return()
    }

    available_ids <- as.character(tools_data[["record_id"]])

    if (is.null(current_id) || !(current_id %in% available_ids)) {
      selected_record_id(available_ids[[1]])
    }
  })

  selected_tool <- shiny::reactive({
    tools_data <- filtered_tools()
    current_id <- selected_record_id()

    if (nrow(tools_data) == 0) {
      return(tools_data[0, , drop = FALSE])
    }

    if (is.null(current_id) || !(current_id %in% as.character(tools_data[["record_id"]]))) {
      return(tools_data[1, , drop = FALSE])
    }

    tools_data[as.character(tools_data[["record_id"]]) == current_id, , drop = FALSE]
  })

  selected_tool_matrix <- shiny::reactive({
    tool <- selected_tool()
    matrix_data <- visualization_snapshot()$matrix

    if (nrow(tool) == 0) {
      return(matrix_data[0, , drop = FALSE])
    }

    matrix_data <- matrix_data[matrix_data[["record_id"]] == tool[["record_id"]][[1]], , drop = FALSE]
    matrix_data[order(-matrix_data[["confidence"]], matrix_data[["tactic"]], matrix_data[["technique_id"]]), , drop = FALSE]
  })

  selected_tool_refinement <- shiny::reactive({
    tool <- selected_tool()
    refinement_data <- refinement_candidates()

    if (nrow(tool) == 0 || nrow(refinement_data) == 0) {
      return(refinement_data[0, , drop = FALSE])
    }

    refinement_data <- refinement_data[refinement_data[["record_id"]] == tool[["record_id"]][[1]], , drop = FALSE]
    refinement_data[order(refinement_data[["already_mapped"]], -refinement_data[["retrieval_score"]], refinement_data[["retrieval_rank"]]), , drop = FALSE]
  })

  output$modern_overview_panel <- shiny::renderUI({
    snapshot <- visualization_snapshot()
    tools_data <- snapshot$tools
    matrix_data <- snapshot$matrix
    featured_tool <- if (nrow(tools_data) > 0) tools_data[1, , drop = FALSE] else tools_data[0, , drop = FALSE]

    tags$div(
      tags$section(
        class = "hero-shell",
        tags$div(
          class = "hero-copy-modern glass-panel",
          tags$div(class = "eyebrow-modern", "Разведка по наступательным инструментам"),
          tags$h1("Каталог утилит, сигналов и MITRE-связей в продуктовой оболочке"),
          tags$p(
            "Здесь собраны уже отфильтрованные и оценённые инструменты: удобнее смотреть, какие утилиты выходят наверх, из каких источников они приходят и как покрывают MITRE ATT&CK."
          ),
          tags$div(
            class = "hero-strip",
            modern_metric_card("Готовые карточки", nrow(tools_data), "LLM-оценка, описание, MITRE-связи"),
            modern_metric_card("MITRE-связи", nrow(matrix_data), "Инструмент -> тактика -> техника"),
            modern_metric_card(
              "Первое место",
              if (nrow(featured_tool) > 0) rank_label(row_value(featured_tool, "visualization_rank")) else "н/д",
              if (nrow(featured_tool) > 0) format_meta_value(row_value(featured_tool, "assessed_name")) else "Нет данных"
            )
          )
        ),
        tags$aside(
          class = "hero-spotlight-modern glass-panel",
          tags$div(
            class = "spotlight-topline",
            tags$span(class = "eyebrow-modern", "Главный сигнал"),
            tags$span(class = "spotlight-rank", if (nrow(featured_tool) > 0) rank_label(row_value(featured_tool, "visualization_rank")) else "н/д")
          ),
          tags$h2(if (nrow(featured_tool) > 0) format_meta_value(row_value(featured_tool, "assessed_name")) else "Нет данных"),
          tags$p(if (nrow(featured_tool) > 0) format_meta_value(row_value(featured_tool, "short_description_ru"), fallback = "Описание недоступно.") else "Описание недоступно."),
          tags$div(
            class = "spotlight-stats-grid",
            tags$div(tags$span("Источник"), tags$strong(if (nrow(featured_tool) > 0) format_meta_value(row_value(featured_tool, "source")) else "н/д")),
            tags$div(tags$span("Уверенность"), tags$strong(if (nrow(featured_tool) > 0) format_score(row_value(featured_tool, "confidence_score")) else "н/д")),
            tags$div(tags$span("Тип"), tags$strong(if (nrow(featured_tool) > 0) format_meta_value(row_value(featured_tool, "entity_type")) else "н/д")),
            tags$div(tags$span("MITRE"), tags$strong(if (nrow(featured_tool) > 0) sprintf("%s техник", format_meta_value(row_value(featured_tool, "mitre_technique_count"), fallback = "0")) else "0 техник"))
          ),
          if (nrow(featured_tool) > 0 && length(clean_display_tags(row_value(featured_tool, "filter_tags"))) > 0) {
            tags$div(
              class = "tag-ribbon",
              lapply(utils::head(clean_display_tags(row_value(featured_tool, "filter_tags")), 7), function(tag) tags$span(tag))
            )
          }
        )
      ),
      tags$section(
        class = "featured-ribbon-shell",
        modern_section_heading("Лучшие утилиты", "Короткий срез лучших записей из текущего слоя"),
        modern_top_tool_cards(tools_data, limit = 4L)
      ),
      tags$section(
        class = "analytics-grid",
        tags$article(
          class = "glass-panel chart-card chart-card-wide",
          modern_section_heading("Рейтинг", "Топ инструментов по итоговому score"),
          plotOutput("top_tools_plot", height = 420)
        ),
        tags$article(
          class = "glass-panel chart-card",
          modern_section_heading("Источники", "Где чаще всего находятся инструменты"),
          plotOutput("source_plot", height = 320)
        ),
        tags$article(
          class = "glass-panel chart-card",
          modern_section_heading("Качество сигнала", "Распределение уверенности"),
          plotOutput("confidence_plot", height = 320)
        )
      )
    )
  })

  output$modern_analytics_panel <- shiny::renderUI({
    snapshot <- visualization_snapshot()
    tools_data <- snapshot$tools
    matrix_data <- snapshot$matrix
    refinement_data <- refinement_candidates()

    tags$section(
      class = "analytics-shell",
      modern_section_heading(
        "Покрытие MITRE",
        "Как текущий набор инструментов покрывает тактики и техники",
        "Ниже показан слой покрытия: сколько утилит реально связаны с MITRE, какие тактики плотнее всего заполнены и какие кандидаты стоит проверить следующими.",
        class = "analytics-shell-heading"
      ),
      modern_summary_cards(modern_coverage_summary(tools_data, matrix_data)),
      tags$article(
        class = "glass-panel refinement-summary-panel",
        tags$div(
          class = "section-heading",
          tags$div(
            tags$div(class = "section-kicker-modern", "Очередь уточнения"),
            tags$h3("Где слой уточнения нашёл дополнительные MITRE-возможности")
          ),
          tags$p("Этот слой показывает не подтверждённые связи, а самые правдоподобные кандидаты для ручной проверки или следующего прохода.")
        ),
        modern_summary_cards(modern_refinement_summary(refinement_data), class = "refinement-summary-grid")
      ),
      tags$div(
        class = "analytics-grid",
        tags$article(
          class = "glass-panel chart-card",
          modern_section_heading("Покрытие", "Нагрузка по тактикам MITRE"),
          plotOutput("tactic_plot", height = 360)
        ),
        tags$article(
          class = "glass-panel chart-card",
          modern_section_heading("Кандидаты уточнения", "Какие техники чаще всплывают как новые кандидаты"),
          plotOutput("mitre_refinement_plot", height = 360)
        ),
        tags$article(
          class = "glass-panel chart-card chart-card-wide mitre-heatmap-panel",
          modern_section_heading("Тепловая карта MITRE", "Компактный вид матрицы тактик и техник"),
          plotOutput("heatmap_plot", height = 680)
        ),
        tags$article(
          class = "glass-panel chart-card chart-card-wide",
          modern_section_heading("Матрица", "Технические строки MITRE"),
          DT::dataTableOutput("matrix_table")
        ),
        tags$article(
          class = "glass-panel chart-card chart-card-wide",
          modern_section_heading("Очередь проверки", "Новые MITRE-кандидаты"),
          DT::dataTableOutput("mitre_refinement_table")
        )
      )
    )
  })

  output$tool_list_count <- shiny::renderUI({
    tags$h3(sprintf("Показано: %s", nrow(filtered_tools())))
  })

  output$tool_list_cards <- shiny::renderUI({
    current_tool <- selected_tool()
    current_id <- if (nrow(current_tool) > 0) as.character(current_tool$record_id[[1]]) else NULL
    tool_list_cards(filtered_tools(), current_id)
  })

  output$tool_detail_panel <- shiny::renderUI({
    tool <- selected_tool()

    if (nrow(tool) == 0) {
      return(tags$div(class = "empty-detail", "Нет данных для текущих фильтров."))
    }

    selected_matrix <- selected_tool_matrix()
    selected_refinement <- selected_tool_refinement()
    selected_ttp_groups <- build_selected_ttp_groups(selected_matrix)
    selected_display_tags <- clean_display_tags(row_value(tool, "filter_tags"))
    tool_name <- format_meta_value(row_value(tool, "assessed_name"))
    tool_url <- format_meta_value(row_value(tool, "url"), fallback = "#")

    tags$div(
      tags$div(
        class = "panel-heading-row",
        tags$div(
          tags$div(class = "section-kicker-modern", "Профиль инструмента"),
          tags$h3(tool_name)
        ),
        tags$a(
          class = "detail-link",
          href = tool_url,
          target = "_blank",
          rel = "noreferrer",
          "Открыть источник",
          tags$span(class = "detail-link-arrow", "->")
        )
      ),
      tags$div(
        class = "detail-stat-grid-modern",
        tags$div(tags$span("Ранг"), tags$strong(rank_label(row_value(tool, "visualization_rank")))),
        tags$div(tags$span("Уверенность"), tags$strong(format_score(row_value(tool, "confidence_score")))),
        tags$div(tags$span("Источник"), tags$strong(format_meta_value(row_value(tool, "source")))),
        tags$div(tags$span("Тип"), tags$strong(format_meta_value(row_value(tool, "entity_type"))))
      ),
      tags$div(
        class = "detail-copy-grid",
        tags$article(
          tags$div(class = "detail-section-kicker", "Краткое описание"),
          tags$p(format_meta_value(row_value(tool, "short_description_ru"), fallback = "Описание недоступно."))
        ),
        tags$article(
          tags$div(class = "detail-section-kicker", "Контекст записи"),
          tags$div(
            class = "tool-context-grid",
            tags$span(tags$strong("source"), format_meta_value(row_value(tool, "source"))),
            tags$span(tags$strong("type"), format_meta_value(row_value(tool, "entity_type"))),
            tags$span(tags$strong("category"), format_meta_value(row_value(tool, "category_ru"))),
            tags$span(tags$strong("MITRE"), sprintf("%s связей", nrow(selected_matrix)))
          ),
          if (length(selected_display_tags) > 0) {
            tags$div(
              class = "tag-ribbon compact-tags readable-tags",
              lapply(utils::head(selected_display_tags, 10), function(tag) tags$span(tag))
            )
          }
        )
      ),
      tags$article(
        class = "detail-longform",
        tags$div(class = "detail-section-kicker", "Полное описание"),
        tags$p(format_meta_value(row_value(tool, "long_description_ru"), fallback = "Полное описание недоступно."))
      ),
      tags$section(
        class = "matrix-section-modern ttp-map-section",
        tags$div(
          class = "panel-heading-row minor-heading",
          tags$div(
            tags$div(class = "detail-section-kicker", "MITRE-связи инструмента"),
            tags$h4("Карта связей: инструмент -> тактики -> техники")
          ),
          tags$span(
            class = "panel-chip",
            sprintf("%s тактик · %s техник", length(selected_ttp_groups), nrow(selected_matrix))
          )
        ),
        if (length(selected_ttp_groups) > 0) {
          tags$div(
            class = "ttp-map-layout",
            tags$div(
              class = "ttp-overview-band",
              tags$div(
                class = "ttp-tool-node",
                tags$span("Инструмент"),
                tags$strong(tool_name),
                tags$em(format_meta_value(row_value(tool, "category_ru")))
              ),
              tags$div(class = "ttp-overview-arrow", "->"),
              tags$div(
                class = "ttp-overview-stat",
                tags$strong(length(selected_ttp_groups)),
                tags$span("тактик")
              ),
              tags$div(class = "ttp-overview-arrow", "->"),
              tags$div(
                class = "ttp-overview-stat",
                tags$strong(nrow(selected_matrix)),
                tags$span("техник")
              )
            ),
            tags$div(
              class = "ttp-tactic-stack",
              lapply(seq_along(selected_ttp_groups), function(group_index) {
                group <- selected_ttp_groups[[group_index]]
                accent <- modern_accent_palette[((group_index - 1) %% length(modern_accent_palette)) + 1]

                tags$article(
                  class = "ttp-tactic-block",
                  tags$div(
                    class = "ttp-tactic-node",
                    style = sprintf("--tactic-accent: %s;", accent),
                    tags$div(class = "ttp-tactic-number", sprintf("%02d", group_index)),
                    tags$div(
                      tags$span("Тактика MITRE"),
                      tags$h5(group$tactic),
                      tags$p(group$description)
                    ),
                    tags$div(
                      class = "ttp-tactic-metrics",
                      tags$strong(length(group$techniques)),
                      tags$span("техник"),
                      tags$em(sprintf("средняя уверенность %s", format_score(group$avg_confidence)))
                    )
                  ),
                  tags$div(class = "ttp-flow-divider", tags$span("Техники внутри тактики")),
                  tags$div(
                    class = "ttp-technique-grid",
                    lapply(seq_along(group$techniques), function(row_index) {
                      row <- group$techniques[[row_index]]
                      reasoning <- format_meta_value(row_value(row, "reasoning_ru"), fallback = "")

                      tags$div(
                        class = "ttp-technique-card",
                        tags$div(
                          class = "ttp-technique-topline",
                          tags$div(class = "ttp-technique-id", format_meta_value(row_value(row, "technique_id"))),
                          tags$span(sprintf("Уверенность %s", format_score(row_value(row, "confidence"))))
                        ),
                        tags$h5(format_meta_value(row_value(row, "technique_name"))),
                        tags$div(
                          class = "ttp-technique-explain",
                          tags$strong("Что делает"),
                          tags$p(describe_technique(row))
                        ),
                        if (nzchar(reasoning)) {
                          tags$div(
                            class = "ttp-technique-explain is-reason",
                            tags$strong("Почему связали"),
                            tags$p(reasoning)
                          )
                        },
                        tags$div(
                          class = "ttp-technique-meta",
                          tags$span(sprintf("source:%s", format_meta_value(row_value(row, "source")))),
                          tags$span(format_meta_value(row_value(row, "category_ru")))
                        )
                      )
                    })
                  )
                )
              })
            )
          )
        } else {
          tags$div(class = "empty-detail mitre-empty", "Для этого инструмента MITRE-связи пока не рассчитаны.")
        }
      ),
      tags$section(
        class = "matrix-section-modern refinement-section-modern compact-review-section",
        tags$div(
          class = "panel-heading-row minor-heading",
          tags$div(
            tags$div(class = "detail-section-kicker", "Уточнение MITRE"),
            tags$h4(sprintf("%s кандидатов на проверку", nrow(selected_refinement)))
          )
        ),
        if (nrow(selected_refinement) > 0) {
          tags$div(
            class = "refinement-highlight-grid refinement-highlight-grid-detail",
            lapply(seq_len(min(8, nrow(selected_refinement))), function(index) {
              row <- selected_refinement[index, , drop = FALSE]
              status_is_mapped <- isTRUE(row_value(row, "already_mapped", FALSE))
              status_class <- if (status_is_mapped) " is-mapped" else ""
              status_label <- if (status_is_mapped) "Уже в матрице" else "На проверку"
              tactics <- split_mitre_values(row_value(row, "tactic_names"))
              matched_terms <- split_mitre_values(row_value(row, "matched_terms"))

              tags$article(
                class = "refinement-highlight-card refinement-highlight-card-detail",
                tags$div(
                  class = "refinement-card-topline",
                  tags$span(class = paste0("refinement-status-pill", status_class), status_label),
                  tags$span(sprintf("%s оценка", format_score(row_value(row, "retrieval_score"))))
                ),
                tags$h4(sprintf(
                  "%s · %s",
                  format_meta_value(row_value(row, "technique_id")),
                  format_meta_value(row_value(row, "technique_name"))
                )),
                tags$div(
                  class = "refinement-meta-row",
                  tags$span(sprintf("очередь #%s", format_meta_value(row_value(row, "retrieval_rank")))),
                  tags$span(if (is.na(row_value(row, "mapped_confidence", NA_real_))) "новый кандидат" else sprintf("%s в матрице", format_score(row_value(row, "mapped_confidence"))))
                ),
                if (length(tactics) > 0) {
                  tags$div(
                    class = "tag-ribbon compact-tags refinement-tag-ribbon",
                    lapply(utils::head(tactics, 4), function(tactic) tags$span(tactic))
                  )
                },
                if (length(matched_terms) > 0) {
                  tags$div(
                    class = "refinement-token-row",
                    lapply(utils::head(matched_terms, 6), function(term) tags$span(class = "refinement-token", term))
                  )
                }
              )
            })
          )
        } else {
          tags$div(class = "empty-detail refinement-empty", "Для этой утилиты слой уточнения пока не нашёл дополнительных кандидатов.")
        }
      ),
      tags$section(
        class = "matrix-section-modern",
        tags$div(
          class = "panel-heading-row minor-heading",
          tags$div(
            tags$div(class = "detail-section-kicker", "Техническая таблица MITRE"),
            tags$h4(sprintf("%s подтверждённых связей", nrow(selected_matrix)))
          )
        ),
        if (nrow(selected_matrix) > 0) {
          tags$div(
            class = "matrix-table-modern",
            tags$div(
              class = "matrix-table-head",
              tags$span("Техника"),
              tags$span("Название"),
              tags$span("Тактика"),
              tags$span("Уверенность")
            ),
            tags$div(
              class = "matrix-table-body",
              lapply(seq_len(nrow(selected_matrix)), function(index) {
                row <- selected_matrix[index, , drop = FALSE]

                tags$div(
                  class = "matrix-table-row",
                  tags$span(format_meta_value(row_value(row, "technique_id"))),
                  tags$span(format_meta_value(row_value(row, "technique_name"))),
                  tags$span(format_meta_value(row_value(row, "tactic"))),
                  tags$span(format_score(row_value(row, "confidence")))
                )
              })
            )
          )
        } else {
          tags$div(class = "empty-detail mitre-empty", "Подтверждённых MITRE-связей пока нет.")
        }
      )
    )
  })

  output$overview_hero_panel <- shiny::renderUI({
    snapshot <- visualization_snapshot()
    tools_data <- snapshot$tools
    matrix_data <- snapshot$matrix
    featured_tool <- if (nrow(tools_data) > 0) tools_data[1, , drop = FALSE] else tools_data[0, , drop = FALSE]

    hero_panel(featured_tool, tools_data, matrix_data)
  })

  output$overview_featured_cards <- shiny::renderUI({
    featured_tool_cards(utils::head(visualization_snapshot()$tools, 3))
  })

  output$overview_metric_grid <- shiny::renderUI({
    snapshot <- visualization_snapshot()
    tools_data <- snapshot$tools
    matrix_data <- snapshot$matrix
    top_score_value <- if (nrow(tools_data) > 0) max(tools_data$visualization_score, na.rm = TRUE) else NA_real_

    tags$div(
      class = "metric-grid",
      metric_card("Visualization tools", nrow(tools_data), "Готовые карточки под UI"),
      metric_card("MITRE mappings", nrow(matrix_data), "Связи tactic/technique"),
      metric_card("Top score", if (is.na(top_score_value)) "n/a" else sprintf("%.2f", top_score_value), "Лучший post-LLM ranking")
    )
  })

  output$top_tools_plot <- shiny::renderPlot({
    tools_data <- visualization_snapshot()$tools

    if (nrow(tools_data) == 0) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Visualization tools are not available yet.")
      return(invisible(NULL))
    }

    plot_top_tools(tools_data)
  })

  output$confidence_plot <- shiny::renderPlot({
    tools_data <- visualization_snapshot()$tools

    if (nrow(tools_data) == 0) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Visualization tools are not available yet.")
      return(invisible(NULL))
    }

    plot_confidence_distribution(tools_data)
  })

  output$source_plot <- shiny::renderPlot({
    tools_data <- visualization_snapshot()$tools

    if (nrow(tools_data) == 0) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Visualization tools are not available yet.")
      return(invisible(NULL))
    }

    plot_tools_by_source(tools_data)
  })

  output$tactic_plot <- shiny::renderPlot({
    matrix_data <- filtered_matrix()

    if (nrow(matrix_data) == 0) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "MITRE matrix is not available yet.")
      return(invisible(NULL))
    }

    plot_tactic_distribution(matrix_data)
  })

  output$heatmap_plot <- shiny::renderPlot({
    matrix_data <- filtered_matrix()

    if (nrow(matrix_data) == 0) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "MITRE matrix is not available yet.")
      return(invisible(NULL))
    }

    plot_mitre_heatmap(matrix_data)
  })

  output$mitre_refinement_metric_grid <- shiny::renderUI({
    refinement_data <- refinement_candidates()
    refinement_gaps <- refinement_data[!refinement_data[["already_mapped"]], , drop = FALSE]

    tags$div(
      class = "metric-grid refinement-metric-grid",
      metric_card("Candidate rows", nrow(refinement_data), "Все retrieval-first кандидаты для MITRE refinement"),
      metric_card("Needs review", nrow(refinement_gaps), "Новые links, которых пока нет в текущем mapping-layer"),
      metric_card("Tools with gaps", dplyr::n_distinct(refinement_gaps$record_id), "Сколько tool profiles получили новые MITRE suggestions"),
      metric_card("Gap techniques", dplyr::n_distinct(refinement_gaps$technique_id), "Уникальные techniques среди unmapped candidates"),
      metric_card("Mapped confirmations", sum(refinement_data$already_mapped, na.rm = TRUE), "Кандидаты, уже совпавшие с существующими mapping rows")
    )
  })

  output$mitre_refinement_plot <- shiny::renderPlot({
    refinement_plot_data <- refinement_candidates()
    refinement_plot_data <- refinement_plot_data[!refinement_plot_data[["already_mapped"]], , drop = FALSE]
    refinement_plot_data <- count_records(refinement_plot_data, c("technique_id", "technique_name"))
    refinement_plot_data <- utils::head(refinement_plot_data, 10)
    refinement_plot_data[["label"]] <- sprintf(
      "%s %s",
      refinement_plot_data[["technique_id"]],
      refinement_plot_data[["technique_name"]]
    )
    refinement_plot_data[["label_ordered"]] <- factor(
      refinement_plot_data[["label"]],
      levels = refinement_plot_data[["label"]][order(refinement_plot_data[["count"]])]
    )

    if (nrow(refinement_plot_data) == 0) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "MITRE refinement candidates are not available yet.")
      return(invisible(NULL))
    }

    plot <- ggplot2::ggplot(
      refinement_plot_data,
      ggplot2::aes_string(x = "label_ordered", y = "count")
    ) +
      ggplot2::geom_col(fill = "#ff7a3d", width = 0.72) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Candidate rows") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = "#191d24"),
        axis.title = ggplot2::element_text(color = "#5f646d")
      )

    print(plot)
  })

  output$mitre_refinement_table <- DT::renderDataTable({
    refinement_table <- refinement_candidates()
    refinement_table <- refinement_table[!refinement_table[["already_mapped"]], , drop = FALSE]
    refinement_table <- tibble::as_tibble(refinement_table[c(
      "assessed_name",
      "technique_id",
      "technique_name",
      "tactic_label",
      "retrieval_score",
      "retrieval_rank"
    )])
    refinement_table <- utils::head(refinement_table, 20)
    names(refinement_table) <- c("utility", "technique_id", "technique_name", "tactic", "score", "rank")

    if (nrow(refinement_table) == 0) {
      refinement_table <- tibble::tibble(
        utility = "No refinement candidates available",
        technique_id = NA_character_,
        technique_name = NA_character_,
        tactic = NA_character_,
        score = NA_real_,
        rank = NA_integer_
      )
    }

    DT::datatable(
      refinement_table,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$matrix_table <- DT::renderDataTable({
    matrix_table <- filtered_matrix()
    matrix_table <- tibble::as_tibble(matrix_table[c("assessed_name", "technique_id", "technique_name", "tactic", "confidence")])

    DT::datatable(
      matrix_table,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$tools_table <- DT::renderDataTable({
    tools_table <- filtered_tools()
    tools_table <- tibble::as_tibble(tools_table[c(
      "assessed_name",
      "category_ru",
      "confidence_score",
      "mitre_technique_count"
    )])
    names(tools_table) <- c("utility", "category", "confidence", "mitre")

    DT::datatable(
      tools_table,
      rownames = FALSE,
      selection = "single",
      class = "stripe hover cell-border",
      options = list(pageLength = 14, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$selected_tool_matrix_table <- DT::renderDataTable({
    selected_matrix_table <- selected_tool_matrix()
    selected_matrix_table <- tibble::as_tibble(selected_matrix_table[c("technique_id", "technique_name", "tactic", "confidence")])

    DT::datatable(
      selected_matrix_table,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$tool_detail_header <- shiny::renderUI({
    tool <- selected_tool()
    if (nrow(tool) == 0) {
      return(tags$div("Нет данных для выбранных фильтров."))
    }

    tags$div(
      tags$div(class = "detail-kicker", "Selected tool"),
      tags$h3(class = "detail-title", tool$assessed_name[[1]]),
      tags$div(class = "tool-meta", sprintf("Rank #%s | %s", tool$visualization_rank[[1]], tool$entity_type[[1]]))
    )
  })

  output$tool_detail_stats <- shiny::renderUI({
    tool <- selected_tool()
    if (nrow(tool) == 0) {
      return(NULL)
    }

    tags$div(
      class = "detail-stats-grid",
      tags$div(class = "detail-stat", tags$span("Visualization score"), tags$strong(sprintf("%.2f", tool$visualization_score[[1]]))),
      tags$div(class = "detail-stat", tags$span("Confidence"), tags$strong(sprintf("%.2f", tool$confidence_score[[1]]))),
      tags$div(class = "detail-stat", tags$span("MITRE techniques"), tags$strong(tool$mitre_technique_count[[1]])),
      tags$div(class = "detail-stat", tags$span("MITRE tactics"), tags$strong(tool$mitre_tactic_count[[1]]))
    )
  })

  output$tool_detail_meta <- shiny::renderUI({
    tool <- selected_tool()
    if (nrow(tool) == 0) {
      return(NULL)
    }

    tags$div(
      class = "tool-meta-stack",
      tags$div(class = "tool-meta-line", tags$span("Source"), tags$strong(tool$source[[1]])),
      tags$div(class = "tool-meta-line", tags$span("Category"), tags$strong(tool$category_ru[[1]])),
      tags$div(class = "tool-meta-line", tags$span("MITRE tactics"), tags$strong(paste(tool$mitre_tactics[[1]], collapse = ", "))),
      tags$div(class = "tool-meta-line", tags$span("URL"), tags$a(href = tool$url[[1]], target = "_blank", tool$url[[1]]))
    )
  })

  output$tool_detail_tags <- shiny::renderUI({
    tool <- selected_tool()
    if (nrow(tool) == 0) {
      return(NULL)
    }

    tags$div(
      class = "chip-row",
      lapply(tool$filter_tags[[1]], function(tag) tags$span(class = "tag-chip", tag))
    )
  })

  output$tool_detail_body <- shiny::renderUI({
    tool <- selected_tool()
    if (nrow(tool) == 0) {
      return(NULL)
    }

    tags$div(class = "tool-detail", tool$long_description_ru[[1]])
  })

  output$tool_detail_refinement_cards <- shiny::renderUI({
    refinement_rows <- selected_tool_refinement()

    if (nrow(refinement_rows) == 0) {
      return(
        tags$div(
          class = "refinement-empty-shell",
          tags$span(class = "score-pill muted", "No candidates"),
          tags$p("Для этой утилиты retrieval-first слой пока не нашёл дополнительных MITRE suggestions.")
        )
      )
    }

    tags$div(
      class = "refinement-card-grid",
      lapply(seq_len(min(6, nrow(refinement_rows))), function(index) {
        row <- refinement_rows[index, , drop = FALSE]
        matched_terms <- row$matched_terms[[1]]
        if (is.null(matched_terms) || length(matched_terms) == 0 || all(is.na(matched_terms))) {
          matched_terms <- character()
        }

        tags$article(
          class = "refinement-card",
          tags$div(
            class = "refinement-card-topline",
            tags$span(
              class = sprintf("refinement-status-pill %s", if (isTRUE(row$already_mapped[[1]])) "is-mapped" else "is-gap"),
              if (isTRUE(row$already_mapped[[1]])) "Already mapped" else "Needs review"
            ),
            tags$span(class = "score-pill", sprintf("score %.3f", row$retrieval_score[[1]]))
          ),
          tags$h4(sprintf("%s %s", row$technique_id[[1]], row$technique_name[[1]])),
          tags$p(class = "refinement-copy", row$tactic_label[[1]] %||% "MITRE tactic is not available"),
          tags$div(
            class = "refinement-meta-row",
            tags$span(class = "score-pill muted", sprintf("rank #%s", row$retrieval_rank[[1]] %||% "n/a")),
            if (!is.na(row$mapped_confidence[[1]])) tags$span(class = "score-pill muted", sprintf("mapped %.2f", row$mapped_confidence[[1]]))
          ),
          if (length(matched_terms) > 0) {
            tags$div(
              class = "refinement-token-row",
              lapply(utils::head(as.character(matched_terms), 6), function(term) tags$span(class = "tag-chip refinement-token", term))
            )
          }
        )
      })
    )
  })

  output$pipeline_metric_grid <- shiny::renderUI({
    snapshot <- pipeline_snapshot()

    tags$div(
      class = "metric-grid pipeline-metric-grid",
      metric_card("Нормализовано", snapshot$metrics$normalized %||% "n/a", "Инструменты после нормализации и дедупликации"),
      metric_card("Оценено", snapshot$metrics$assessed %||% "n/a", "Инструменты, прошедшие оценку нейросетью"),
      metric_card("Релевантных", snapshot$metrics$relevant %||% "n/a", "Инструменты, признанные релевантными"),
      metric_card("Новых найдено", snapshot$metrics$discovery_new_rows %||% "n/a", "Новые записи из последнего сбора данных"),
      metric_card("Новых с GitHub", snapshot$metrics$github_new_rows %||% "n/a", "Новые репозитории GitHub за последний запуск"),
      metric_card("Кандидатов для оценки", snapshot$metrics$llm_candidates %||% "n/a", "Инструменты, отправленные на оценку нейросетью"),
      metric_card("В очереди", snapshot$metrics$llm_backlog %||% "n/a", "Ещё не обработано или требует повтора"),
      metric_card("Ошибок оценки", snapshot$metrics$llm_errors %||% "n/a", "Записи с ошибкой при оценке нейросетью"),
      metric_card("Записей MITRE", snapshot$metrics$matrix %||% "n/a", "Связи инструмент — тактика / техника"),
      metric_card("Добавлено в интерфейс", snapshot$metrics$ui_history %||% "n/a", "Все инструменты, когда-либо показанные в интерфейсе"),
      metric_card("Запросов в срезе", snapshot$metrics$github_selected_requests %||% "n/a", "Число поисковых запросов в текущем срезе"),
      metric_card("Смещение запросов", snapshot$metrics$github_request_offset %||% "n/a", "Смещение, с которого начался последний запуск"),
      metric_card("Следующее смещение", snapshot$metrics$github_next_request_offset %||% "n/a", "Смещение для следующего запуска"),
      metric_card("Размер плана", snapshot$metrics$github_query_plan_size %||% "n/a", "Общий размер плана поисковых запросов"),
      metric_card("Доля среза", if (is.na(snapshot$metrics$github_selection_share_percent)) "n/a" else sprintf("%.1f%%", snapshot$metrics$github_selection_share_percent), "Доля плана, охватываемая за один запуск"),
      metric_card("Прогресс обхода", if (is.na(snapshot$metrics$github_rotation_progress_percent)) "n/a" else sprintf("%.1f%%", snapshot$metrics$github_rotation_progress_percent), "Прогресс инкрементального обхода плана запросов"),
      metric_card("Отправлено запросов", snapshot$metrics$github_logged_requests %||% "n/a", "Реально отправлено поисковых запросов в последнем запуске"),
      metric_card("Запусков для покрытия", snapshot$metrics$estimated_full_coverage_runs %||% "n/a", "Оценочное число запусков для полного обхода плана"),
      metric_card("Осталось запусков", snapshot$metrics$github_remaining_runs_estimate %||% "n/a", "Осталось запусков до конца текущего обхода"),
      metric_card("Критических ошибок", snapshot$metrics$sanity_failed_checks %||% "n/a", "Критические проверки, не прошедшие после запуска"),
      metric_card("Предупреждений", snapshot$metrics$sanity_warning_checks %||% "n/a", "Предупреждения, возникшие после запуска"),
      metric_card("Уникальных тактик", snapshot$metrics$unique_tactics %||% "n/a", "Уникальные тактики в матрице визуализации"),
      metric_card("Уникальных техник", snapshot$metrics$unique_techniques %||% "n/a", "Уникальные техники MITRE в матрице визуализации")
    )
  })

  output$pipeline_run_compare_metric_grid <- shiny::renderUI({
    comparison <- pipeline_run_comparison()
    tool_diff <- comparison$tool_diff
    mitre_diff <- comparison$mitre_diff
    summary <- comparison$summary
    summary_lookup <- if (is.data.frame(summary) && nrow(summary) > 0) stats::setNames(summary$current, summary$metric) else character()

    tags$div(
      class = "metric-grid pipeline-compare-metric-grid",
      metric_card("Текущий запуск", input$pipeline_compare_current_run %||% "n/a", summary_lookup[["status"]] %||% "статус недоступен"),
      metric_card("Базовый запуск", input$pipeline_compare_baseline_run %||% "n/a", if (is.data.frame(summary) && nrow(summary) > 0) summary$baseline[summary$metric == "status"][[1]] %||% "статус недоступен" else "статус недоступен"),
      metric_card("Новых инструментов", if (is.data.frame(tool_diff)) sum(tool_diff$change_type == "new", na.rm = TRUE) else 0L, "Инструменты, которых не было в базовом запуске"),
      metric_card("Изменённых инструментов", if (is.data.frame(tool_diff)) sum(tool_diff$change_type == "changed", na.rm = TRUE) else 0L, "Те же инструменты, но с изменёнными данными"),
      metric_card("Удалённых инструментов", if (is.data.frame(tool_diff)) sum(tool_diff$change_type == "removed", na.rm = TRUE) else 0L, "Инструменты, пропавшие по сравнению с базовым запуском"),
      metric_card("Новых MITRE-связей", if (is.data.frame(mitre_diff)) sum(mitre_diff$change_type == "new", na.rm = TRUE) else 0L, "Новые MITRE-маппинги по сравнению с базовым запуском"),
      metric_card("Изменённых MITRE-связей", if (is.data.frame(mitre_diff)) sum(mitre_diff$change_type == "changed", na.rm = TRUE) else 0L, "Те же маппинги с изменёнными данными"),
      metric_card("Удалённых MITRE-связей", if (is.data.frame(mitre_diff)) sum(mitre_diff$change_type == "removed", na.rm = TRUE) else 0L, "Маппинги, пропавшие в текущем запуске"),
      metric_card("Изменение MITRE", if (is.data.frame(summary) && any(summary$metric == "mitre_rows")) summary$delta[summary$metric == "mitre_rows"][[1]] else "n/a", "Изменение общего числа MITRE-маппингов")
    )
  })

  output$pipeline_status_plot <- shiny::renderPlot({
    status_data <- pipeline_snapshot()$assessment_status

    if (!is.data.frame(status_data) || nrow(status_data) == 0) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Assessment status data is not available yet.")
      return(invisible(NULL))
    }

    status_data[["llm_status_ordered"]] <- factor(
      status_data[["llm_status"]],
      levels = status_data[["llm_status"]][order(status_data[["count"]])]
    )

    plot <- ggplot2::ggplot(
      status_data,
      ggplot2::aes_string(x = "llm_status_ordered", y = "count", fill = "llm_status")
    ) +
      ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c(
        success = "#ff7a3d",
        skipped_pre_filter = "#40b9b4",
        error = "#c75015"
      )) +
      ggplot2::labs(x = NULL, y = "Rows") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = "#191d24"),
        axis.title = ggplot2::element_text(color = "#5f646d")
      )

    print(plot)
  })

  output$pipeline_runtime_table <- DT::renderDataTable({
    DT::datatable(
      pipeline_snapshot()$runtime,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE)
    )
  })

  output$pipeline_artifact_table <- DT::renderDataTable({
    DT::datatable(
      pipeline_snapshot()$artifacts,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$pipeline_duckdb_table <- DT::renderDataTable({
    duckdb_summary <- pipeline_snapshot()$duckdb

    if (!is.data.frame(duckdb_summary) || nrow(duckdb_summary) == 0) {
      duckdb_summary <- tibble::tibble(
        table_name = "No DuckDB tables available",
        rows = NA_integer_
      )
    }

    DT::datatable(
      duckdb_summary,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE)
    )
  })

  output$pipeline_recent_tools_table <- DT::renderDataTable({
    DT::datatable(
      pipeline_snapshot()$recent_tools,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 8, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$pipeline_discovery_table <- DT::renderDataTable({
    discovery_summary <- pipeline_snapshot()$discovery_summary

    if (!is.data.frame(discovery_summary) || nrow(discovery_summary) == 0) {
      discovery_summary <- tibble::tibble(
        metric = "No collect stage has been recorded yet",
        value = NA_character_
      )
    }

    DT::datatable(
      discovery_summary,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE)
    )
  })

  output$pipeline_stage_history_table <- DT::renderDataTable({
    stage_history <- pipeline_snapshot()$stage_history

    if (!is.data.frame(stage_history) || nrow(stage_history) == 0) {
      stage_history <- tibble::tibble(
        stage = "Pipeline stage history is not available yet",
        status = NA_character_,
        started_at = NA_character_,
        finished_at = NA_character_,
        duration_seconds = NA_real_,
        error_message = NA_character_
      )
    }

    DT::datatable(
      utils::head(stage_history, 12),
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 12, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$pipeline_run_catalog_table <- DT::renderDataTable({
    run_catalog <- pipeline_snapshot()$run_catalog

    if (!is.data.frame(run_catalog) || nrow(run_catalog) == 0) {
      run_catalog <- tibble::tibble(
        run_id = "No DuckDB runs available yet",
        started_at = NA_character_,
        finished_at = NA_character_,
        duration_seconds = NA_real_,
        status = NA_character_,
        stages_completed = NA_integer_,
        discovery_new_rows = NA_integer_,
        github_new_rows = NA_integer_,
        github_requests = NA_integer_,
        relevant_tools = NA_integer_,
        mitre_rows = NA_integer_
      )
    }

    DT::datatable(
      run_catalog,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 8, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$pipeline_run_compare_summary_table <- DT::renderDataTable({
    comparison_summary <- pipeline_run_comparison()$summary

    if (!is.data.frame(comparison_summary) || nrow(comparison_summary) == 0) {
      comparison_summary <- tibble::tibble(
        metric = "Run comparison is not available yet",
        baseline = NA_character_,
        current = NA_character_,
        delta = NA_character_
      )
    }

    DT::datatable(
      comparison_summary,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(dom = "t", scrollX = TRUE, autoWidth = FALSE)
    )
  })

  output$pipeline_run_compare_stage_table <- DT::renderDataTable({
    stage_comparison <- pipeline_run_comparison()$stages

    if (!is.data.frame(stage_comparison) || nrow(stage_comparison) == 0) {
      stage_comparison <- tibble::tibble(
        stage = "Stage comparison is not available yet",
        baseline_status = NA_character_,
        current_status = NA_character_,
        baseline_duration_seconds = NA_real_,
        current_duration_seconds = NA_real_,
        duration_delta_seconds = NA_real_
      )
    }

    DT::datatable(
      stage_comparison,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 8, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$pipeline_run_compare_tools_table <- DT::renderDataTable({
    tool_diff <- pipeline_run_comparison()$tool_diff

    if (!is.data.frame(tool_diff) || nrow(tool_diff) == 0) {
      tool_diff <- tibble::tibble(
        change_type = "No relevant tool diff available yet",
        assessed_name = NA_character_,
        source = NA_character_,
        entity_type = NA_character_,
        baseline_confidence_score = NA_real_,
        current_confidence_score = NA_real_,
        baseline_mitre_technique_count = NA_integer_,
        current_mitre_technique_count = NA_integer_
      )
    }

    DT::datatable(
      tool_diff,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$pipeline_run_compare_mitre_table <- DT::renderDataTable({
    mitre_diff <- pipeline_run_comparison()$mitre_diff

    if (!is.data.frame(mitre_diff) || nrow(mitre_diff) == 0) {
      mitre_diff <- tibble::tibble(
        change_type = "No MITRE diff available yet",
        assessed_name = NA_character_,
        technique_id = NA_character_,
        technique_name = NA_character_,
        tactic = NA_character_,
        baseline_confidence = NA_real_,
        current_confidence = NA_real_
      )
    }

    DT::datatable(
      mitre_diff,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$pipeline_sanity_checks_table <- DT::renderDataTable({
    sanity_checks <- pipeline_snapshot()$pipeline_sanity_checks

    if (!is.data.frame(sanity_checks) || nrow(sanity_checks) == 0) {
      sanity_checks <- tibble::tibble(
        run_id = NA_character_,
        check_name = "No sanity checks available yet",
        status = NA_character_,
        severity = NA_character_,
        observed_value = NA_character_,
        expected_value = NA_character_,
        details = NA_character_,
        checked_at = NA_character_
      )
    }

    DT::datatable(
      sanity_checks,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$pipeline_github_query_log_table <- DT::renderDataTable({
    github_query_log <- pipeline_snapshot()$github_query_log

    if (!is.data.frame(github_query_log) || nrow(github_query_log) == 0) {
      github_query_log <- tibble::tibble(
        executed_at = "No GitHub search log available yet",
        request_index = NA_integer_,
        query_family = NA_character_,
        search_sort = NA_character_,
        search_page = NA_integer_,
        returned_rows = NA_integer_,
        unique_candidates_after_request = NA_integer_,
        search_query = NA_character_
      )
    } else {
      github_query_log <- github_query_log[order(-github_query_log[["request_index"]]), , drop = FALSE]
      github_query_log <- utils::head(github_query_log, 25)
      github_query_log <- tibble::as_tibble(github_query_log[c(
        "executed_at",
        "request_index",
        "query_family",
        "search_sort",
        "search_page",
        "returned_rows",
        "unique_candidates_after_request",
        "search_query"
      )])
    }

    DT::datatable(
      github_query_log,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$pipeline_batch_summary_table <- DT::renderDataTable({
    batch_summary <- pipeline_snapshot()$pipeline_batch_summary

    if (!is.data.frame(batch_summary) || nrow(batch_summary) == 0) {
      batch_summary <- tibble::tibble(
        run_index = NA_integer_,
        started_at = "No batch summary available yet",
        duration_seconds = NA_real_,
        github_new_rows = NA_integer_,
        github_request_offset = NA_integer_,
        github_next_request_offset = NA_integer_,
        github_rotation_progress_percent = NA_real_,
        github_query_families = NA_character_
      )
    } else {
      batch_summary <- batch_summary[order(-batch_summary[["run_index"]]), , drop = FALSE]
      batch_summary <- utils::head(batch_summary, 20)
      batch_summary <- tibble::as_tibble(batch_summary[c(
        "run_index",
        "started_at",
        "duration_seconds",
        "github_new_rows",
        "github_request_offset",
        "github_next_request_offset",
        "github_rotation_progress_percent",
        "github_query_families"
      )])
    }

    DT::datatable(
      batch_summary,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$pipeline_backlog_table <- DT::renderDataTable({
    backlog <- pipeline_snapshot()$llm_backlog

    if (!is.data.frame(backlog) || nrow(backlog) == 0) {
      backlog <- tibble::tibble(
        name = "No pending LLM backlog",
        source = NA_character_,
        pre_llm_score = NA_real_,
        date_found = NA_character_,
        llm_status = NA_character_,
        backlog_reason = NA_character_,
        llm_error = NA_character_
      )
    }

    DT::datatable(
      backlog,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 8, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$pipeline_ui_history_table <- DT::renderDataTable({
    ui_history <- pipeline_snapshot()$ui_history

    if (!is.data.frame(ui_history) || nrow(ui_history) == 0) {
      ui_history <- tibble::tibble(
        assessed_name = "UI history is not available yet",
        source = NA_character_,
        entity_type = NA_character_,
        first_ui_added_at = NA_character_,
        last_ui_seen_at = NA_character_,
        active_in_ui = NA,
        last_visualization_rank = NA_integer_
      )
    }

    DT::datatable(
      ui_history,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })

  output$pipeline_queue_table <- DT::renderDataTable({
    llm_queue <- pipeline_snapshot()$llm_queue

    if (!is.data.frame(llm_queue) || nrow(llm_queue) == 0) {
      llm_queue <- tibble::tibble(
        name = "LLM queue is not available yet",
        source = NA_character_,
        pre_llm_score = NA_real_,
        date_found = NA_character_,
        queue_status = NA_character_,
        llm_status = NA_character_,
        llm_processed_at = NA_character_,
        llm_error = NA_character_,
        pre_llm_should_process = NA
      )
    }

    DT::datatable(
      llm_queue,
      rownames = FALSE,
      class = "stripe hover cell-border",
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip", autoWidth = FALSE)
    )
  })
}

shiny::shinyApp(app_ui, app_server)
