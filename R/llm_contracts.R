.llm_string <- function(value, field_name) {
  if (is.null(value) || length(value) == 0) {
    stop(sprintf("Field '%s' is required.", field_name), call. = FALSE)
  }

  first_value <- value[[1]]

  if (is.null(first_value) || is.na(first_value)) {
    stop(sprintf("Field '%s' cannot be NA.", field_name), call. = FALSE)
  }

  first_value <- stringr::str_squish(as.character(first_value))

  if (!nzchar(first_value)) {
    stop(sprintf("Field '%s' cannot be empty.", field_name), call. = FALSE)
  }

  first_value
}

.llm_optional_string <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_character_)
  }

  first_value <- value[[1]]

  if (is.null(first_value) || is.na(first_value)) {
    return(NA_character_)
  }

  first_value <- stringr::str_squish(as.character(first_value))

  if (!nzchar(first_value)) {
    return(NA_character_)
  }

  first_value
}

.llm_logical <- function(value, field_name) {
  if (is.null(value) || length(value) == 0) {
    stop(sprintf("Field '%s' is required.", field_name), call. = FALSE)
  }

  first_value <- value[[1]]

  if (is.logical(first_value) && !is.na(first_value)) {
    return(first_value)
  }

  if (is.character(first_value)) {
    lowered <- tolower(stringr::str_squish(first_value))

    if (lowered %in% c("true", "false")) {
      return(identical(lowered, "true"))
    }
  }

  stop(sprintf("Field '%s' must be boolean.", field_name), call. = FALSE)
}

.llm_numeric <- function(value, field_name) {
  if (is.null(value) || length(value) == 0) {
    stop(sprintf("Field '%s' is required.", field_name), call. = FALSE)
  }

  first_value <- suppressWarnings(as.numeric(value[[1]]))

  if (is.na(first_value)) {
    stop(sprintf("Field '%s' must be numeric.", field_name), call. = FALSE)
  }

  first_value
}

.llm_character_vector <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(character(0))
  }

  values <- as.character(unlist(value, use.names = FALSE))
  values <- stringr::str_squish(values)
  values <- values[nzchar(values)]
  unique(values)
}

.llm_parse_json <- function(json_text, error_context) {
  safe_run(
    expr = function() {
      jsonlite::fromJSON(json_text, simplifyVector = FALSE)
    },
    error_context = error_context
  )
}

.llm_first_present <- function(container, field_names) {
  for (field_name in field_names) {
    if (!is.null(container[[field_name]])) {
      return(container[[field_name]])
    }
  }

  NULL
}

#' Get the JSON schema for LLM validation and enrichment output
#'
#' @return Named list representing the target JSON schema.
get_validation_enrichment_schema <- function() {
  list(
    type = "object",
    additionalProperties = FALSE,
    required = c("is_tool", "name", "purpose", "capabilities", "target_platforms", "category", "reason"),
    properties = list(
      is_tool = list(type = "boolean"),
      name = list(type = "string"),
      purpose = list(type = "string"),
      capabilities = list(type = "array", items = list(type = "string")),
      target_platforms = list(type = "array", items = list(type = "string")),
      category = list(type = "string"),
      reason = list(type = "string")
    )
  )
}

#' Get the JSON schema for LLM MITRE classification output
#'
#' @return Named list representing the target JSON schema.
get_mitre_classification_schema <- function() {
  list(
    type = "object",
    additionalProperties = FALSE,
    required = c("tool_name", "classifications"),
    properties = list(
      tool_name = list(type = "string"),
      classifications = list(
        type = "array",
        items = list(
          type = "object",
          additionalProperties = FALSE,
          required = c("technique_id", "technique_name", "tactic", "confidence", "reasoning"),
          properties = list(
            technique_id = list(type = "string"),
            technique_name = list(type = "string"),
            tactic = list(type = "string"),
            confidence = list(type = "number"),
            reasoning = list(type = "string")
          )
        )
      )
    )
  )
}

#' Parse and validate LLM validation/enrichment JSON
#'
#' @param json_text JSON string returned by the validation/enrichment LLM step.
#'
#' @return Tibble with one validated enrichment row.
parse_validation_enrichment_json <- function(json_text) {
  parsed <- .llm_parse_json(
    json_text = json_text,
    error_context = "Failed to parse validation/enrichment JSON"
  )

  tibble::tibble(
    is_tool = .llm_logical(parsed$is_tool, "is_tool"),
    name = .llm_string(parsed$name, "name"),
    purpose = .llm_string(parsed$purpose, "purpose"),
    capabilities = list(.llm_character_vector(parsed$capabilities)),
    target_platforms = list(.llm_character_vector(parsed$target_platforms)),
    category = .llm_string(parsed$category, "category"),
    reason = .llm_optional_string(parsed$reason)
  )
}

#' Parse and validate LLM MITRE classification JSON
#'
#' @param json_text JSON string returned by the MITRE classification LLM step.
#'
#' @return Tibble with validated technique classifications.
parse_mitre_classification_json <- function(json_text) {
  parsed <- .llm_parse_json(
    json_text = json_text,
    error_context = "Failed to parse MITRE classification JSON"
  )

  tool_name <- .llm_string(parsed$tool_name, "tool_name")
  classifications <- parsed$classifications

  if (is.null(classifications) || length(classifications) == 0) {
    return(tibble::tibble(
      tool_name = character(),
      technique_id = character(),
      technique_name = character(),
      tactic = character(),
      confidence = numeric(),
      reasoning = character()
    ))
  }

  rows <- lapply(
    classifications,
    function(item) {
      tibble::tibble(
        tool_name = tool_name,
        technique_id = .llm_string(item$technique_id, "technique_id"),
        technique_name = .llm_string(item$technique_name, "technique_name"),
        tactic = .llm_string(item$tactic, "tactic"),
        confidence = .llm_numeric(item$confidence, "confidence"),
        reasoning = .llm_string(item$reasoning, "reasoning")
      )
    }
  )

  dplyr::bind_rows(rows)
}

#' Get the JSON schema for unified LLM tool assessment output
#'
#' @return Named list representing the target JSON schema.
get_unified_tool_assessment_schema <- function() {
  list(
    type = "object",
    additionalProperties = FALSE,
    required = c(
      "is_relevant",
      "entity_type",
      "is_tool",
      "name",
      "summary_ru",
      "purpose_ru",
      "capabilities_ru",
      "target_platforms",
      "category_ru",
      "overall_confidence",
      "reason_ru",
      "mitre_classifications"
    ),
    properties = list(
      is_relevant = list(type = "boolean"),
      entity_type = list(type = "string"),
      is_tool = list(type = "boolean"),
      name = list(type = "string"),
      summary_ru = list(type = "string"),
      purpose_ru = list(type = "string"),
      capabilities_ru = list(type = "array", items = list(type = "string")),
      target_platforms = list(type = "array", items = list(type = "string")),
      category_ru = list(type = "string"),
      overall_confidence = list(type = "number"),
      reason_ru = list(type = "string"),
      mitre_classifications = list(
        type = "array",
        items = list(
          type = "object",
          additionalProperties = FALSE,
          required = c("technique_id", "technique_name", "tactic", "confidence", "reasoning_ru"),
          properties = list(
            technique_id = list(type = "string"),
            technique_name = list(type = "string"),
            tactic = list(type = "string"),
            confidence = list(type = "number"),
            reasoning_ru = list(type = "string")
          )
        )
      )
    )
  )
}

#' Parse and validate unified LLM tool assessment JSON
#'
#' @param json_text JSON string returned by the unified one-call LLM stage.
#'
#' @return Named list with validated assessment row and MITRE mapping tibble.
parse_unified_tool_assessment_json <- function(json_text, fallback_name = NA_character_) {
  parsed <- .llm_parse_json(
    json_text = json_text,
    error_context = "Failed to parse unified tool assessment JSON"
  )

  is_relevant_value <- .llm_first_present(parsed, c("is_relevant", "relevant"))
  entity_type_value <- .llm_first_present(parsed, c("entity_type", "type"))
  is_relevant <- .llm_logical(is_relevant_value, "is_relevant")
  entity_type <- .llm_string(entity_type_value, "entity_type")

  is_tool_value <- .llm_first_present(parsed, c("is_tool", "tool"))
  if (is.null(is_tool_value)) {
    is_tool_value <- !(tolower(entity_type) %in% c("news_or_advisory", "non_tool", "library"))
  }

  name_value <- .llm_first_present(parsed, c("name", "tool_name", "assessed_name"))
  if (is.null(name_value) && !is.na(fallback_name) && nzchar(as.character(fallback_name))) {
    name_value <- fallback_name
  }

  overall_confidence_value <- .llm_first_present(parsed, c("overall_confidence", "confidence"))
  if (is.null(overall_confidence_value)) {
    overall_confidence_value <- if (isTRUE(is_relevant)) 0.75 else 0.25
  }

  assessment_row <- tibble::tibble(
    is_relevant = is_relevant,
    entity_type = entity_type,
    is_tool = .llm_logical(is_tool_value, "is_tool"),
    name = .llm_string(name_value, "name"),
    summary_ru = .llm_string(parsed$summary_ru, "summary_ru"),
    purpose_ru = .llm_string(parsed$purpose_ru, "purpose_ru"),
    capabilities_ru = list(.llm_character_vector(parsed$capabilities_ru)),
    target_platforms = list(.llm_character_vector(parsed$target_platforms)),
    category_ru = .llm_string(parsed$category_ru, "category_ru"),
    overall_confidence = .llm_numeric(overall_confidence_value, "overall_confidence"),
    reason_ru = .llm_string(parsed$reason_ru, "reason_ru")
  )

  classifications <- .llm_first_present(parsed, c("mitre_classifications", "mitre_attack", "mitre_mappings"))

  mapping_rows <- if (is.null(classifications) || length(classifications) == 0) {
    tibble::tibble(
      technique_id = character(),
      technique_name = character(),
      tactic = character(),
      confidence = numeric(),
      reasoning_ru = character()
    )
  } else {
    dplyr::bind_rows(lapply(
      classifications,
      function(item) {
        item_confidence <- .llm_first_present(item, c("confidence", "score"))
        if (is.null(item_confidence)) {
          item_confidence <- overall_confidence_value
        }

        item_reasoning <- .llm_first_present(item, c("reasoning_ru", "reasoning", "reason_ru"))
        if (is.null(item_reasoning)) {
          item_reasoning <- parsed$reason_ru
        }

        tibble::tibble(
          technique_id = .llm_string(item$technique_id, "technique_id"),
          technique_name = .llm_string(item$technique_name, "technique_name"),
          tactic = .llm_string(item$tactic, "tactic"),
          confidence = .llm_numeric(item_confidence, "confidence"),
          reasoning_ru = .llm_string(item_reasoning, "reasoning_ru")
        )
      }
    ))
  }

  list(
    assessment = assessment_row,
    mitre_classifications = mapping_rows
  )
}