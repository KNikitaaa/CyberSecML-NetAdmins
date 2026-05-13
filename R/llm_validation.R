.validation_empty_results <- function() {
  tibble::tibble(
    record_id = character(),
    name = character(),
    source = character(),
    source_type = character(),
    url = character(),
    raw_description = character(),
    raw_text = character(),
    date_found = character(),
    pre_llm_score = numeric(),
    pre_llm_priority = character(),
    pre_llm_candidate_type = character(),
    pre_llm_should_process = logical(),
    pre_llm_reasons = list(),
    metadata = list(),
    llm_provider = character(),
    llm_base_url = character(),
    llm_model = character(),
    llm_processed_at = character(),
    llm_status = character(),
    llm_error = character(),
    is_tool = logical(),
    validated_name = character(),
    purpose = character(),
    capabilities = list(),
    target_platforms = list(),
    category = character(),
    reason = character()
  )
}

.validation_default_fresh_discovery_slots <- function() {
  value <- suppressWarnings(as.integer(Sys.getenv("OTM_FRESH_DISCOVERY_LLM_SLOTS", unset = "5")))

  if (is.na(value) || value < 0L) {
    return(5L)
  }

  value
}

.validation_is_fresh_discovery_candidate <- function(candidates) {
  if (!is.data.frame(candidates) || nrow(candidates) == 0) {
    return(logical(0))
  }

  type_values <- if ("pre_llm_candidate_type" %in% names(candidates)) {
    as.character(candidates[["pre_llm_candidate_type"]])
  } else {
    rep(NA_character_, nrow(candidates))
  }

  reason_values <- if ("pre_llm_reasons" %in% names(candidates) && is.list(candidates[["pre_llm_reasons"]])) {
    vapply(candidates[["pre_llm_reasons"]], function(reasons) {
      values <- unlist(.normalize_value_or(reasons, character(0)), use.names = FALSE)
      paste(as.character(values), collapse = " ")
    }, character(1))
  } else {
    rep("", nrow(candidates))
  }

  type_values == "fresh_discovery" |
    stringr::str_detect(reason_values, stringr::fixed("candidate:fresh_discovery"))
}

.validation_order_candidates <- function(candidates) {
  if (!is.data.frame(candidates) || nrow(candidates) == 0) {
    return(candidates)
  }

  score_values <- candidates[["pre_llm_score"]]
  score_values[is.na(score_values)] <- -Inf
  date_values <- candidates[["date_found"]]
  date_values[is.na(date_values)] <- ""
  name_values <- candidates[["name"]]
  order_index <- order(-score_values, -xtfrm(date_values), name_values, na.last = TRUE)
  candidates[order_index, , drop = FALSE]
}

.validation_limit_candidates <- function(candidates, max_records, fresh_discovery_slots = .validation_default_fresh_discovery_slots()) {
  if (is.null(max_records)) {
    return(candidates)
  }

  max_records <- as.integer(max_records)
  if (is.na(max_records) || max_records < 1L || nrow(candidates) == 0) {
    return(utils::head(candidates, 0L))
  }

  fresh_discovery_slots <- max(0L, as.integer(fresh_discovery_slots))
  if (fresh_discovery_slots == 0L) {
    return(utils::head(candidates, max_records))
  }

  fresh_mask <- .validation_is_fresh_discovery_candidate(candidates)
  fresh_candidates <- candidates[fresh_mask, , drop = FALSE]
  baseline_candidates <- utils::head(candidates, max_records)

  if (nrow(fresh_candidates) == 0) {
    return(baseline_candidates)
  }

  fresh_take <- min(max_records, fresh_discovery_slots, nrow(fresh_candidates))
  selected <- dplyr::bind_rows(
    utils::head(fresh_candidates, fresh_take),
    baseline_candidates
  )
  selected <- selected[!duplicated(selected[["record_id"]]), , drop = FALSE]

  if (nrow(selected) <= max_records) {
    return(selected)
  }

  selected_fresh <- .validation_is_fresh_discovery_candidate(selected)
  fresh_selected <- utils::head(selected[selected_fresh, , drop = FALSE], fresh_take)
  regular_selected <- selected[!selected_fresh, , drop = FALSE]
  regular_take <- max(0L, max_records - nrow(fresh_selected))

  dplyr::bind_rows(fresh_selected, utils::head(regular_selected, regular_take))
}

.validation_system_prompt <- function() {
  paste(
    "You validate whether a candidate is a real offensive security utility.",
    "Focus on tools, frameworks, implants, scanners, proxies, toolkits, and well-known offensive script collections.",
    "Do not treat vulnerability news, advisories, bulletins, CVE posts, training material, blog posts, generic libraries, cheat sheets, playbooks, exam prep, walkthroughs, reference repositories, or Markdown-only note collections without executable artifacts as tools.",
    "Return only valid JSON matching the provided schema.",
    sep = " "
  )
}

#' Build a validation/enrichment prompt for a normalized candidate
#'
#' @param record One-row tibble from the normalized candidate queue.
#'
#' @return Character scalar prompt for the LLM.
build_validation_enrichment_prompt <- function(record) {
  if (!is.data.frame(record) || nrow(record) != 1) {
    stop("record must be a one-row data frame.", call. = FALSE)
  }

  metadata <- record$metadata[[1]]
  llm_text <- .clean_candidate_text(record$raw_text, max_chars = 3500L)
  metadata_json <- jsonlite::toJSON(
    .normalize_value_or(metadata, list()),
    auto_unbox = TRUE,
    null = "null"
  )

  sprintf(
    paste(
      "Candidate name: %s",
      "Source: %s",
      "Source type: %s",
      "URL: %s",
      "Heuristic candidate type: %s",
      "Heuristic score: %s",
      "Heuristic reasons: %s",
      "Metadata JSON: %s",
      "Raw text:",
      "%s",
      "",
      "Task:",
      "1. Decide whether this candidate is a real offensive utility or a meaningful offensive script collection.",
      "2. Reject plain news, advisories, writeups, educational content, cheat sheets, playbooks, exam-prep repositories, command references, and generic non-offensive libraries.",
      "3. Markdown-only helper or notes repositories without clear signs of executable scripts, binaries, or runnable tooling should be rejected.",
      "4. If it is a tool, produce a concise structured purpose, capabilities, target platforms, and category.",
      "5. Keep reason brief and concrete.",
      sep = "\n"
    ),
    .llm_optional_string(record$name),
    .llm_optional_string(record$source),
    .llm_optional_string(record$source_type),
    .llm_optional_string(record$url),
    .llm_optional_string(record$pre_llm_candidate_type),
    ifelse(is.na(record$pre_llm_score[[1]]), "NA", as.character(record$pre_llm_score[[1]])),
    paste(.normalize_value_or(record$pre_llm_reasons[[1]], character(0)), collapse = "; "),
    metadata_json,
    .llm_optional_string(llm_text)
  )
}

.openai_extract_content <- function(response_body) {
  choices <- response_body$choices

  if (is.null(choices) || length(choices) == 0) {
    stop("LLM response did not contain choices.", call. = FALSE)
  }

  message <- choices[[1]]$message
  content <- message$content

  if (is.character(content) && length(content) > 0) {
    return(content[[1]])
  }

  if (is.list(content) && length(content) > 0) {
    text_parts <- vapply(
      content,
      function(part) {
        if (!is.null(part$text) && nzchar(as.character(part$text))) {
          return(as.character(part$text))
        }

        if (!is.null(part$value) && nzchar(as.character(part$value))) {
          return(as.character(part$value))
        }

        NA_character_
      },
      character(1)
    )

    text_parts <- text_parts[!is.na(text_parts) & nzchar(text_parts)]

    if (length(text_parts) > 0) {
      return(paste(text_parts, collapse = "\n"))
    }
  }

  stop("LLM response did not contain message content.", call. = FALSE)
}

.openai_extract_tool_arguments <- function(response_body, expected_tool_name = NULL) {
  choices <- response_body$choices

  if (is.null(choices) || length(choices) == 0) {
    stop("LLM response did not contain choices.", call. = FALSE)
  }

  tool_calls <- choices[[1]]$message$tool_calls
  if (is.null(tool_calls) || length(tool_calls) == 0) {
    stop("LLM response did not contain tool calls.", call. = FALSE)
  }

  selected_call <- tool_calls[[1]]
  if (!is.null(expected_tool_name) && nzchar(expected_tool_name)) {
    matching_calls <- Filter(
      function(tool_call) identical(.llm_optional_string(tool_call[["function"]][["name"]]), expected_tool_name),
      tool_calls
    )
    if (length(matching_calls) > 0) {
      selected_call <- matching_calls[[1]]
    }
  }

  arguments <- selected_call[["function"]][["arguments"]]
  if (is.null(arguments) || length(arguments) == 0 || !nzchar(as.character(arguments[[1]]))) {
    stop("LLM tool call did not contain function arguments.", call. = FALSE)
  }

  as.character(arguments[[1]])
}

.openai_tool_definition <- function(schema_name, schema) {
  list(
    type = "function",
    `function` = list(
      name = schema_name,
      description = sprintf("Return the %s payload that matches the provided JSON schema.", schema_name),
      parameters = schema
    )
  )
}

.openai_chat_completion <- function(
  system_prompt,
  user_prompt,
  model,
  api_key,
  provider = get_default_llm_provider(),
  base_url = get_default_llm_base_url(provider),
  schema_name = "validation_enrichment",
  schema = get_validation_enrichment_schema()
) {
  provider_config <- .llm_provider_config(provider = provider, base_url = base_url)
  call_mode <- get_default_llm_call_mode(provider_config$provider)

  request_body <- c(provider_config$extra_body, list(
    model = model,
    temperature = 0,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    )
  ))

  use_tool_calls <- identical(call_mode, "tools") && isTRUE(provider_config$supports_tool_calls)

  if (isTRUE(use_tool_calls)) {
    request_body$tools <- list(.openai_tool_definition(schema_name = schema_name, schema = schema))
    request_body$tool_choice <- list(
      type = "function",
      `function` = list(name = schema_name)
    )
  } else {
    request_body$response_format <- if (identical(provider_config$provider, "openai")) {
      list(
        type = "json_schema",
        json_schema = list(
          name = schema_name,
          strict = TRUE,
          schema = schema
        )
      )
    } else {
      provider_config$response_format
    }
  }

  response_body <- safe_run(
    expr = function() {
      request <- httr2::request(paste0(provider_config$base_url, provider_config$endpoint)) |>
        httr2::req_headers(
          Authorization = paste("Bearer", api_key),
          "Content-Type" = "application/json"
        ) |>
        httr2::req_body_json(data = request_body, auto_unbox = TRUE)

      safe_json_request(
        request = request,
        simplify_data_frame = FALSE,
        error_context = sprintf("%s validation request failed", tools::toTitleCase(provider_config$provider))
      )
    },
    error_context = sprintf("Failed to execute %s validation call", provider_config$provider)
  )

  if (isTRUE(use_tool_calls)) {
    return(.openai_extract_tool_arguments(response_body, expected_tool_name = schema_name))
  }

  .openai_extract_content(response_body)
}

.validation_success_row <- function(record, parsed, provider, base_url, model) {
  tibble::tibble(
    record_id = record$record_id,
    name = record$name,
    source = record$source,
    source_type = record$source_type,
    url = record$url,
    raw_description = record$raw_description,
    raw_text = record$raw_text,
    date_found = record$date_found,
    pre_llm_score = record$pre_llm_score,
    pre_llm_priority = record$pre_llm_priority,
    pre_llm_candidate_type = record$pre_llm_candidate_type,
    pre_llm_should_process = record$pre_llm_should_process,
    pre_llm_reasons = record$pre_llm_reasons,
    metadata = record$metadata,
    llm_provider = provider,
    llm_base_url = base_url,
    llm_model = model,
    llm_processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    llm_status = "success",
    llm_error = NA_character_,
    is_tool = parsed$is_tool,
    validated_name = parsed$name,
    purpose = parsed$purpose,
    capabilities = parsed$capabilities,
    target_platforms = parsed$target_platforms,
    category = parsed$category,
    reason = parsed$reason
  )
}

.validation_skipped_row <- function(record, provider, base_url, llm_status, llm_error = NA_character_) {
  tibble::tibble(
    record_id = record$record_id,
    name = record$name,
    source = record$source,
    source_type = record$source_type,
    url = record$url,
    raw_description = record$raw_description,
    raw_text = record$raw_text,
    date_found = record$date_found,
    pre_llm_score = record$pre_llm_score,
    pre_llm_priority = record$pre_llm_priority,
    pre_llm_candidate_type = record$pre_llm_candidate_type,
    pre_llm_should_process = record$pre_llm_should_process,
    pre_llm_reasons = record$pre_llm_reasons,
    metadata = record$metadata,
    llm_provider = provider,
    llm_base_url = base_url,
    llm_model = NA_character_,
    llm_processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    llm_status = llm_status,
    llm_error = llm_error,
    is_tool = NA,
    validated_name = NA_character_,
    purpose = NA_character_,
    capabilities = list(character(0)),
    target_platforms = list(character(0)),
    category = NA_character_,
    reason = NA_character_
  )
}

#' Select normalized candidates for the first LLM stage
#'
#' @param normalized_data Optional normalized tibble.
#' @param data_dir Directory containing normalized artifacts.
#' @param max_records Optional maximum number of candidates to return.
#'
#' @return Tibble ordered by descending pre-LLM priority.
get_validation_candidates <- function(
  normalized_data = NULL,
  data_dir = get_default_data_dir(),
  max_records = NULL
) {
  inputs <- normalized_data

  if (is.null(inputs)) {
    inputs <- load_pipeline_table("normalized_candidates", rds_path = file.path(data_dir, "normalized_tools.rds"), required = TRUE)
  }

  candidates <- inputs[inputs[["pre_llm_should_process"]] %in% TRUE, , drop = FALSE]

  candidates <- .validation_order_candidates(candidates)
  .validation_limit_candidates(candidates, max_records)
}

#' Run the first LLM stage for validation and enrichment
#'
#' @param normalized_data Optional normalized tibble.
#' @param data_dir Directory containing normalized artifacts.
#' @param output_path Output path for all validation results.
#' @param enriched_output_path Output path for validated tool-only rows.
#' @param model LLM model name.
#' @param provider LLM provider name.
#' @param base_url Optional provider base URL override.
#' @param max_records Optional limit on the number of processed candidates.
#' @param api_key Provider API key.
#'
#' @return Named list with `validation_results` and `enriched_tools` tibbles.
run_validation_enrichment <- function(
  normalized_data = NULL,
  data_dir = get_default_data_dir(),
  output_path = file.path(data_dir, "validation_enrichment_results.rds"),
  enriched_output_path = file.path(data_dir, "enriched_tools.rds"),
  provider = get_default_llm_provider(),
  model = get_default_llm_model(provider),
  base_url = get_default_llm_base_url(provider),
  max_records = NULL,
  api_key = get_llm_api_key(provider)
) {
  inputs <- normalized_data

  if (is.null(inputs)) {
    inputs <- load_pipeline_table("normalized_candidates", rds_path = file.path(data_dir, "normalized_tools.rds"), required = TRUE)
  }

  if (!is.data.frame(inputs)) {
    stop("normalized_data must be a data frame.", call. = FALSE)
  }

  candidates <- get_validation_candidates(
    normalized_data = inputs,
    data_dir = data_dir,
    max_records = max_records
  )

  skipped_inputs <- inputs[!(inputs[["record_id"]] %in% candidates[["record_id"]]), , drop = FALSE]

  if (nrow(candidates) > 0 && !nzchar(api_key)) {
    stop(sprintf("API key for provider '%s' is required.", provider), call. = FALSE)
  }

  log_message(sprintf("Validation runtime: provider=%s model=%s base_url=%s api_key_present=%s", provider, model, base_url, nzchar(api_key)))
  log_message(sprintf("Running validation enrichment for %s candidates", nrow(candidates)))

  processed_rows <- lapply(
    seq_len(nrow(candidates)),
    function(index) {
      record <- candidates[index, , drop = FALSE]

      tryCatch(
        {
          prompt <- build_validation_enrichment_prompt(record)
          json_text <- .openai_chat_completion(
            system_prompt = .validation_system_prompt(),
            user_prompt = prompt,
            model = model,
            api_key = api_key,
            provider = provider,
            base_url = base_url
          )
          parsed <- parse_validation_enrichment_json(json_text)
          .validation_success_row(record, parsed, provider = provider, base_url = base_url, model = model)
        },
        error = function(error) {
          .validation_skipped_row(
            record,
            provider = provider,
            base_url = base_url,
            llm_status = "error",
            llm_error = conditionMessage(error)
          )
        }
      )
    }
  )

  skipped_rows <- lapply(
    seq_len(nrow(skipped_inputs)),
    function(index) {
      .validation_skipped_row(
        skipped_inputs[index, , drop = FALSE],
        provider = provider,
        base_url = base_url,
        llm_status = "skipped_pre_filter"
      )
    }
  )

  validation_results <- dplyr::bind_rows(c(processed_rows, skipped_rows))

  if (nrow(validation_results) == 0) {
    validation_results <- .validation_empty_results()
  } else {
    process_values <- as.integer(validation_results[["pre_llm_should_process"]] %in% TRUE)
    score_values <- validation_results[["pre_llm_score"]]
    score_values[is.na(score_values)] <- -Inf
    name_values <- validation_results[["name"]]
    order_index <- order(-process_values, -score_values, name_values, na.last = TRUE)
    validation_results <- validation_results[order_index, , drop = FALSE]
  }

  enriched_tools <- validation_results[
    validation_results[["llm_status"]] == "success" & validation_results[["is_tool"]] %in% TRUE,
    ,
    drop = FALSE
  ]

  if (nrow(enriched_tools) > 0) {
    score_values <- enriched_tools[["pre_llm_score"]]
    score_values[is.na(score_values)] <- -Inf
    validated_name_values <- enriched_tools[["validated_name"]]
    order_index <- order(-score_values, validated_name_values, na.last = TRUE)
    enriched_tools <- enriched_tools[order_index, , drop = FALSE]
  }

  save_pipeline_rds(validation_results, output_path)
  save_pipeline_rds(enriched_tools, enriched_output_path)

  log_message(sprintf("Saved %s validation results to %s", nrow(validation_results), output_path))
  log_message(sprintf("Saved %s enriched tool rows to %s", nrow(enriched_tools), enriched_output_path))

  list(
    validation_results = validation_results,
    enriched_tools = enriched_tools
  )
}
