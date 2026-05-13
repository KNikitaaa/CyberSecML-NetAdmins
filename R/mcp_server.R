#' Prepare a compact tool table for MCP responses
#'
#' @param tools Visualization tools data frame.
#'
#' @return Tibble with user-facing tool fields.
.mcp_compact_tools <- function(tools) {
  if (!is.data.frame(tools) || nrow(tools) == 0) {
    return(tibble::tibble(
      record_id = character(),
      assessed_name = character(),
      source = character(),
      entity_type = character(),
      category_ru = character(),
      short_description_ru = character(),
      confidence_score = numeric(),
      visualization_rank = integer(),
      mitre_tactic_count = integer(),
      mitre_technique_count = integer(),
      url = character()
    ))
  }

  tibble::tibble(
    record_id = as.character(tools[["record_id"]]),
    assessed_name = as.character(tools[["assessed_name"]]),
    source = as.character(tools[["source"]]),
    entity_type = as.character(tools[["entity_type"]]),
    category_ru = as.character(tools[["category_ru"]]),
    short_description_ru = as.character(tools[["short_description_ru"]]),
    confidence_score = as.numeric(tools[["confidence_score"]]),
    visualization_rank = as.integer(tools[["visualization_rank"]]),
    mitre_tactic_count = as.integer(tools[["mitre_tactic_count"]]),
    mitre_technique_count = as.integer(tools[["mitre_technique_count"]]),
    url = as.character(tools[["url"]])
  )
}

.mcp_load_visualization_artifacts <- function(data_dir = get_default_data_dir()) {
  list(
    tools = load_pipeline_rds(file.path(data_dir, "visualization_tools.rds"), required = TRUE),
    matrix = load_pipeline_rds(file.path(data_dir, "visualization_tool_matrix.rds"), required = TRUE)
  )
}

.mcp_normalize_scalar <- function(value) {
  normalized <- .normalize_string(value)
  if (is.na(normalized)) {
    return(NA_character_)
  }

  tolower(normalized)
}

.mcp_normalize_limit <- function(limit, default = 10L, max_limit = 100L) {
  if (is.null(limit) || length(limit) == 0 || is.na(limit[[1]])) {
    return(default)
  }

  normalized <- suppressWarnings(as.integer(limit[[1]]))
  if (is.na(normalized) || normalized <= 0) {
    return(default)
  }

  min(normalized, max_limit)
}

.mcp_match_query <- function(tool, query) {
  normalized_query <- .mcp_normalize_scalar(query)

  if (is.na(normalized_query) || !nzchar(normalized_query)) {
    return(TRUE)
  }

  haystack <- c(
    tool$assessed_name,
    tool$short_description_ru,
    tool$long_description_ru,
    tool$category_ru,
    tool$entity_type,
    unlist(tool$filter_tags, use.names = FALSE)
  )
  haystack <- haystack[!is.na(haystack) & nzchar(haystack)]

  if (length(haystack) == 0) {
    return(FALSE)
  }

  stringr::str_detect(
    string = tolower(paste(haystack, collapse = " ")),
    pattern = stringr::fixed(normalized_query)
  )
}

.mcp_match_tactic <- function(tool_tactics, tactic) {
  normalized_tactic <- .mcp_normalize_scalar(tactic)

  if (is.na(normalized_tactic) || !nzchar(normalized_tactic)) {
    return(TRUE)
  }

  tool_tactics <- .normalize_value_or(tool_tactics, character(0))
  tool_tactics <- tolower(as.character(unlist(tool_tactics, use.names = FALSE)))

  normalized_tactic %in% tool_tactics
}

.mcp_select_tool <- function(tools, tool_name = NULL, record_id = NULL) {
  normalized_record_id <- .normalize_string(record_id)
  normalized_name <- .mcp_normalize_scalar(tool_name)

  if (is.na(normalized_record_id) && is.na(normalized_name)) {
    stop("Either tool_name or record_id must be provided.", call. = FALSE)
  }

  matched <- tools

  if (!is.na(normalized_record_id)) {
    matched <- matched[matched[["record_id"]] == normalized_record_id, , drop = FALSE]
  } else {
    exact_matches <- matched[tolower(matched[["assessed_name"]]) == normalized_name, , drop = FALSE]

    matched <- if (nrow(exact_matches) > 0) {
      exact_matches
    } else {
      matched[stringr::str_detect(tolower(matched[["assessed_name"]]), stringr::fixed(normalized_name)), , drop = FALSE]
    }
  }

  if (nrow(matched) == 0) {
    stop("Tool was not found in the current visualization layer.", call. = FALSE)
  }

  matched <- matched[order(matched[["visualization_rank"]], -matched[["confidence_score"]], matched[["assessed_name"]]), , drop = FALSE]
  matched[1, , drop = FALSE]
}

.mcp_json_text <- function(value) {
  jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    dataframe = "rows",
    null = "null",
    na = "null",
    pretty = TRUE
  )
}

.mcp_response <- function(value) {
  mcpr::response_text(.mcp_json_text(value))
}

.mcp_safe_handler <- function(expr) {
  tryCatch(
    expr(),
    error = function(error) {
      .mcp_response(list(
        success = FALSE,
        error = conditionMessage(error)
      ))
    }
  )
}

#' Search mapped offensive tools for MCP clients
#'
#' @param query Optional search query.
#' @param source Optional source filter.
#' @param tactic Optional MITRE tactic filter.
#' @param limit Maximum number of rows to return.
#' @param data_dir Directory with visualization artifacts.
#'
#' @return Tibble with matching tools.
search_tools <- function(
  query = NULL,
  source = NULL,
  tactic = NULL,
  limit = 10L,
  data_dir = get_default_data_dir()
) {
  artifacts <- .mcp_load_visualization_artifacts(data_dir)
  normalized_source <- .mcp_normalize_scalar(source)
  normalized_limit <- .mcp_normalize_limit(limit)

  query_mask <- vapply(seq_len(nrow(artifacts$tools)), function(index) {
    .mcp_match_query(artifacts$tools[index, , drop = FALSE], query)
  }, logical(1))
  source_mask <- if (is.na(normalized_source)) {
    rep(TRUE, nrow(artifacts$tools))
  } else {
    tolower(artifacts$tools[["source"]]) == normalized_source
  }
  tactic_mask <- vapply(artifacts$tools[["mitre_tactics"]], .mcp_match_tactic, logical(1), tactic = tactic)

  filtered <- artifacts$tools[query_mask & source_mask & tactic_mask, , drop = FALSE]
  if (nrow(filtered) > 0) {
    filtered <- filtered[order(filtered[["visualization_rank"]], -filtered[["confidence_score"]], filtered[["assessed_name"]]), , drop = FALSE]
    filtered <- utils::head(filtered, normalized_limit)
  }

  .mcp_compact_tools(filtered)
}

#' Get MITRE mappings for a selected tool
#'
#' @param tool_name Optional assessed tool name.
#' @param record_id Optional record identifier.
#' @param data_dir Directory with visualization artifacts.
#'
#' @return Named list with the selected tool and MITRE mappings.
get_tool_ttps <- function(tool_name = NULL, record_id = NULL, data_dir = get_default_data_dir()) {
  artifacts <- .mcp_load_visualization_artifacts(data_dir)
  tool <- .mcp_select_tool(artifacts$tools, tool_name = tool_name, record_id = record_id)

  mappings <- artifacts$matrix[artifacts$matrix[["record_id"]] == tool[["record_id"]][[1]], , drop = FALSE]
  if (nrow(mappings) > 0) {
    mappings <- mappings[order(-mappings[["confidence"]], mappings[["technique_id"]], mappings[["tactic"]]), c("technique_id", "technique_name", "tactic", "confidence", "reasoning_ru"), drop = FALSE]
  }

  list(
    tool = .mcp_compact_tools(tool),
    mitre_mappings = mappings
  )
}

#' Find tools associated with a MITRE technique
#'
#' @param technique_id MITRE technique identifier such as `T1055`.
#' @param limit Maximum number of tools to return.
#' @param data_dir Directory with visualization artifacts.
#'
#' @return Tibble with tools linked to the requested technique.
get_technique_tools <- function(technique_id, limit = 20L, data_dir = get_default_data_dir()) {
  normalized_technique <- toupper(.normalize_string(technique_id))

  if (is.na(normalized_technique) || !nzchar(normalized_technique)) {
    stop("technique_id must be provided.", call. = FALSE)
  }

  artifacts <- .mcp_load_visualization_artifacts(data_dir)
  normalized_limit <- .mcp_normalize_limit(limit, default = 20L)

  linked_rows <- artifacts$matrix[toupper(artifacts$matrix[["technique_id"]]) == normalized_technique, , drop = FALSE]

  if (nrow(linked_rows) == 0) {
    return(tibble::tibble(
      record_id = character(),
      assessed_name = character(),
      source = character(),
      entity_type = character(),
      confidence_score = numeric(),
      visualization_rank = integer(),
      technique_id = character(),
      technique_name = character(),
      tactic = character(),
      mapping_confidence = numeric(),
      url = character()
    ))
  }

  record_groups <- split(seq_len(nrow(linked_rows)), linked_rows[["record_id"]])
  summary_rows <- tibble::tibble(
    record_id = names(record_groups),
    technique_id = vapply(record_groups, function(indexes) linked_rows[["technique_id"]][[indexes[[1]]]], character(1)),
    technique_name = vapply(record_groups, function(indexes) linked_rows[["technique_name"]][[indexes[[1]]]], character(1)),
    tactic = vapply(record_groups, function(indexes) paste(sort(unique(linked_rows[["tactic"]][indexes])), collapse = ", "), character(1)),
    mapping_confidence = vapply(record_groups, function(indexes) max(linked_rows[["confidence"]][indexes], na.rm = TRUE), numeric(1))
  )
  tool_columns <- artifacts$tools[c("record_id", "assessed_name", "source", "entity_type", "confidence_score", "visualization_rank", "url")]
  combined <- merge(summary_rows, tool_columns, by = "record_id", all.x = TRUE, sort = FALSE)
  combined <- combined[order(combined[["visualization_rank"]], -combined[["mapping_confidence"]], combined[["assessed_name"]]), , drop = FALSE]
  combined <- utils::head(combined, normalized_limit)
  tibble::as_tibble(combined[c("record_id", "assessed_name", "source", "entity_type", "confidence_score", "visualization_rank", "technique_id", "technique_name", "tactic", "mapping_confidence", "url")])
}

#' Summarize current visualization-layer statistics
#'
#' @param data_dir Directory with visualization artifacts.
#'
#' @return Named list with counts and breakdown tables.
get_statistics <- function(data_dir = get_default_data_dir()) {
  artifacts <- .mcp_load_visualization_artifacts(data_dir)

  source_breakdown <- tibble::as_tibble(sort(table(artifacts$tools[["source"]]), decreasing = TRUE), .name_repair = "minimal")
  names(source_breakdown) <- c("source", "count")

  entity_breakdown <- tibble::as_tibble(sort(table(artifacts$tools[["entity_type"]]), decreasing = TRUE), .name_repair = "minimal")
  names(entity_breakdown) <- c("entity_type", "count")

  tactic_breakdown <- tibble::as_tibble(sort(table(artifacts$matrix[["tactic"]]), decreasing = TRUE), .name_repair = "minimal")
  names(tactic_breakdown) <- c("tactic", "count")

  list(
    tool_count = nrow(artifacts$tools),
    mapping_count = nrow(artifacts$matrix),
    mapped_tool_count = sum(artifacts$tools$mitre_technique_count > 0, na.rm = TRUE),
    source_count = dplyr::n_distinct(artifacts$tools$source),
    technique_count = dplyr::n_distinct(artifacts$matrix$technique_id),
    tactic_count = dplyr::n_distinct(artifacts$matrix$tactic),
    source_breakdown = source_breakdown,
    entity_breakdown = entity_breakdown,
    tactic_breakdown = tactic_breakdown
  )
}

#' Classify a manually provided tool through the live LLM path
#'
#' @param name Candidate tool name.
#' @param description Candidate description or raw text.
#' @param source Source label to attach to the temporary record.
#' @param source_type Source type to attach to the temporary record.
#' @param url Optional reference URL.
#' @param provider LLM provider name.
#' @param model Provider model name.
#' @param base_url Provider base URL.
#' @param api_key Provider API key.
#'
#' @return Named list with heuristic metadata, assessed tool row, and MITRE mappings.
classify_new_tool <- function(
  name,
  description,
  source = "manual",
  source_type = "manual_input",
  url = NA_character_,
  provider = get_default_llm_provider(),
  model = get_default_llm_model(provider),
  base_url = get_default_llm_base_url(provider),
  api_key = get_llm_api_key(provider)
) {
  normalized_name <- .normalize_string(name)
  normalized_description <- .normalize_text_block(description, max_chars = 3500L)

  if (is.na(normalized_name) || !nzchar(normalized_name)) {
    stop("name must be provided.", call. = FALSE)
  }

  if (is.na(normalized_description) || !nzchar(normalized_description)) {
    stop("description must be provided.", call. = FALSE)
  }

  if (!nzchar(api_key)) {
    stop(sprintf("API key for provider '%s' is required.", provider), call. = FALSE)
  }

  metadata <- list(manual_input = TRUE)
  priority <- .normalize_compute_priority(
    source = source,
    source_type = source_type,
    name = normalized_name,
    raw_text = normalized_description,
    metadata = metadata,
    url = url
  )

  priority$should_process <- TRUE
  priority$reasons <- unique(c(priority$reasons, "manual_override:forced_llm"))

  record <- tibble::tibble(
    record_id = .normalize_make_record_id(source, url, normalized_name),
    name = normalized_name,
    source = source,
    source_type = source_type,
    url = .normalize_string(url),
    raw_description = normalized_description,
    raw_text = normalized_description,
    date_found = as.character(Sys.Date()),
    pre_llm_score = priority$score,
    pre_llm_priority = priority$priority,
    pre_llm_candidate_type = priority$candidate_type,
    pre_llm_should_process = TRUE,
    pre_llm_reasons = list(priority$reasons),
    metadata = list(metadata)
  )

  prompt <- build_unified_tool_assessment_prompt(record)
  json_text <- .openai_chat_completion(
    system_prompt = .assessment_system_prompt(),
    user_prompt = prompt,
    model = model,
    api_key = api_key,
    provider = provider,
    base_url = base_url,
    schema_name = "unified_tool_assessment",
    schema = get_unified_tool_assessment_schema()
  )
  parsed <- parse_unified_tool_assessment_json(json_text, fallback_name = normalized_name)

  list(
    heuristic = list(
      score = priority$score,
      priority = priority$priority,
      candidate_type = priority$candidate_type,
      reasons = priority$reasons
    ),
    assessment = .assessment_success_row(record, parsed, provider = provider, base_url = base_url, model = model),
    mitre_mappings = .assessment_mitre_rows(record$record_id[[1]], parsed$assessment$name[[1]], parsed)
  )
}

#' Build the OffensiveToolMapper MCP server
#'
#' @param data_dir Directory with visualization artifacts.
#'
#' @return An `mcpr` server object.
build_mcp_server <- function(data_dir = get_default_data_dir()) {
  if (!requireNamespace("mcpr", quietly = TRUE)) {
    stop("Package 'mcpr' is required to build the MCP server.", call. = FALSE)
  }

  server <- mcpr::new_server(
    name = "OffensiveToolMapper MCP Server",
    description = "Expose offensive tool intelligence and MITRE ATT&CK mappings from OffensiveToolMapper.",
    version = "0.0.1"
  )

  search_tool <- mcpr::new_tool(
    name = "search_tools",
    description = "Search the current offensive tool catalog by query, source, or MITRE tactic.",
    input_schema = mcpr::schema(
      properties = mcpr::properties(
        query = mcpr::property_string("query", "Free-text search query", required = FALSE),
        source = mcpr::property_string("source", "Optional source filter such as github or packetstorm", required = FALSE),
        tactic = mcpr::property_string("tactic", "Optional MITRE tactic filter", required = FALSE),
        limit = mcpr::property_number("limit", "Maximum number of rows to return", required = FALSE)
      )
    ),
    handler = function(params) {
      .mcp_safe_handler(function() {
        .mcp_response(list(
          success = TRUE,
          results = search_tools(
            query = params$query,
            source = params$source,
            tactic = params$tactic,
            limit = params$limit,
            data_dir = data_dir
          )
        ))
      })
    }
  )

  tool_ttps_tool <- mcpr::new_tool(
    name = "get_tool_ttps",
    description = "Return MITRE ATT&CK mappings for a selected tool.",
    input_schema = mcpr::schema(
      properties = mcpr::properties(
        tool_name = mcpr::property_string("tool_name", "Assessed tool name", required = FALSE),
        record_id = mcpr::property_string("record_id", "Exact OffensiveToolMapper record_id", required = FALSE)
      )
    ),
    handler = function(params) {
      .mcp_safe_handler(function() {
        .mcp_response(c(list(success = TRUE), get_tool_ttps(
          tool_name = params$tool_name,
          record_id = params$record_id,
          data_dir = data_dir
        )))
      })
    }
  )

  technique_tools_tool <- mcpr::new_tool(
    name = "get_technique_tools",
    description = "List tools mapped to a specific MITRE technique ID.",
    input_schema = mcpr::schema(
      properties = mcpr::properties(
        technique_id = mcpr::property_string("technique_id", "MITRE technique identifier such as T1055", required = TRUE),
        limit = mcpr::property_number("limit", "Maximum number of tools to return", required = FALSE)
      )
    ),
    handler = function(params) {
      .mcp_safe_handler(function() {
        .mcp_response(list(
          success = TRUE,
          results = get_technique_tools(
            technique_id = params$technique_id,
            limit = params$limit,
            data_dir = data_dir
          )
        ))
      })
    }
  )

  statistics_tool <- mcpr::new_tool(
    name = "get_statistics",
    description = "Summarize the current offensive tool catalog and MITRE coverage.",
    input_schema = mcpr::schema(properties = mcpr::properties()),
    handler = function(params) {
      .mcp_safe_handler(function() {
        .mcp_response(c(list(success = TRUE), get_statistics(data_dir = data_dir)))
      })
    }
  )

  classify_tool <- mcpr::new_tool(
    name = "classify_new_tool",
    description = "Run the live unified LLM assessment for a manually supplied tool description.",
    input_schema = mcpr::schema(
      properties = mcpr::properties(
        name = mcpr::property_string("name", "Candidate tool name", required = TRUE),
        description = mcpr::property_string("description", "Candidate description or raw text", required = TRUE),
        source = mcpr::property_string("source", "Optional source label", required = FALSE),
        source_type = mcpr::property_string("source_type", "Optional source type", required = FALSE),
        url = mcpr::property_string("url", "Optional source URL", required = FALSE)
      )
    ),
    handler = function(params) {
      .mcp_safe_handler(function() {
        .mcp_response(c(list(success = TRUE), classify_new_tool(
          name = params$name,
          description = params$description,
          source = dplyr::coalesce(params$source, "manual"),
          source_type = dplyr::coalesce(params$source_type, "manual_input"),
          url = params$url
        )))
      })
    }
  )

  for (tool in list(search_tool, tool_ttps_tool, technique_tools_tool, statistics_tool, classify_tool)) {
    server <- mcpr::add_capability(server, tool)
  }

  server
}

#' Run the OffensiveToolMapper MCP server
#'
#' @param transport MCP transport, either `stdio` or `http`.
#' @param data_dir Directory with visualization artifacts.
#' @param port HTTP port when `transport = "http"`.
#'
#' @return Invokes the server transport and does not return.
#' @export
run_mcp_server <- function(transport = c("stdio", "http"), data_dir = get_default_data_dir(), port = 3000L) {
  if (!requireNamespace("mcpr", quietly = TRUE)) {
    stop("Package 'mcpr' is required to run the MCP server.", call. = FALSE)
  }

  transport <- match.arg(transport)
  server <- build_mcp_server(data_dir = data_dir)

  if (identical(transport, "http")) {
    return(mcpr::serve_http(server, port = as.integer(port)))
  }

  mcpr::serve_io(server)
}
