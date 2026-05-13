get_default_llm_provider <- function() {
  provider <- get_runtime_env_value("LLM_PROVIDER", unset = "deepseek")
  provider <- tolower(stringr::str_squish(provider))

  if (!provider %in% c("deepseek", "openai", "openai_compatible")) {
    stop(sprintf("Unsupported LLM provider '%s'.", provider), call. = FALSE)
  }

  provider
}

get_default_llm_model <- function(provider = get_default_llm_provider()) {
  provider <- tolower(stringr::str_squish(provider))
  explicit_model <- get_runtime_env_value("LLM_MODEL", unset = "")

  if (nzchar(explicit_model)) {
    return(explicit_model)
  }

  switch(
    provider,
    deepseek = "deepseek-chat",
    openai = "gpt-4o",
    openai_compatible = "openai-compatible",
    stop(sprintf("Unsupported LLM provider '%s'.", provider), call. = FALSE)
  )
}

get_default_llm_base_url <- function(provider = get_default_llm_provider()) {
  provider <- tolower(stringr::str_squish(provider))
  explicit_base_url <- get_runtime_env_value("LLM_BASE_URL", unset = "")

  if (nzchar(explicit_base_url)) {
    return(explicit_base_url)
  }

  switch(
    provider,
    deepseek = "https://api.deepseek.com",
    openai = "https://api.openai.com/v1",
    openai_compatible = "http://127.0.0.1:8000/v1",
    stop(sprintf("Unsupported LLM provider '%s'.", provider), call. = FALSE)
  )
}

get_llm_api_key <- function(provider = get_default_llm_provider()) {
  provider <- tolower(stringr::str_squish(provider))
  env_name <- switch(
    provider,
    deepseek = "DEEPSEEK_API_KEY",
    openai = "OPENAI_API_KEY",
    openai_compatible = "LLM_API_KEY",
    stop(sprintf("Unsupported LLM provider '%s'.", provider), call. = FALSE)
  )

  api_key <- get_runtime_env_value(env_name, unset = "")
  if (!nzchar(api_key) && identical(provider, "openai_compatible")) {
    api_key <- get_runtime_env_value("OPENAI_API_KEY", unset = "")
  }

  api_key
}

get_llm_runtime_config <- function(provider = get_default_llm_provider()) {
  normalized_provider <- tolower(stringr::str_squish(provider))
  api_key <- get_llm_api_key(normalized_provider)

  tibble::tibble(
    provider = normalized_provider,
    model = get_default_llm_model(normalized_provider),
    base_url = get_default_llm_base_url(normalized_provider),
    api_key_present = nzchar(api_key)
  )
}

get_default_llm_max_records <- function() {
  raw_value <- get_runtime_env_value("LLM_MAX_RECORDS", unset = "")

  if (!nzchar(raw_value)) {
    return(NULL)
  }

  numeric_value <- suppressWarnings(as.integer(raw_value))
  if (is.na(numeric_value) || numeric_value <= 0) {
    stop("Environment variable 'LLM_MAX_RECORDS' must be a positive integer.", call. = FALSE)
  }

  numeric_value
}

get_default_llm_call_mode <- function(provider = get_default_llm_provider()) {
  explicit_mode <- tolower(stringr::str_squish(get_runtime_env_value("LLM_CALL_MODE", unset = "")))
  if (!nzchar(explicit_mode)) {
    return("tools")
  }

  if (!explicit_mode %in% c("tools", "json")) {
    stop("Environment variable 'LLM_CALL_MODE' must be either 'tools' or 'json'.", call. = FALSE)
  }

  explicit_mode
}

.llm_provider_config <- function(provider = get_default_llm_provider(), base_url = NULL) {
  normalized_provider <- tolower(stringr::str_squish(provider))
  resolved_base_url <- if (!is.null(base_url) && nzchar(base_url)) {
    as.character(base_url)
  } else {
    get_default_llm_base_url(normalized_provider)
  }
  response_format_mode <- tolower(stringr::str_squish(get_runtime_env_value("LLM_RESPONSE_FORMAT", unset = "")))

  switch(
    normalized_provider,
    deepseek = list(
      provider = "deepseek",
      base_url = resolved_base_url,
      endpoint = "/chat/completions",
      supports_tool_calls = TRUE,
      response_format = list(type = "json_object"),
      extra_body = list(stream = FALSE)
    ),
    openai = list(
      provider = "openai",
      base_url = resolved_base_url,
      endpoint = "/chat/completions",
      supports_tool_calls = TRUE,
      response_format = NULL,
      extra_body = list()
    ),
    openai_compatible = list(
      provider = "openai_compatible",
      base_url = resolved_base_url,
      endpoint = "/chat/completions",
      supports_tool_calls = TRUE,
      response_format = if (identical(response_format_mode, "none")) NULL else list(type = "json_object"),
      extra_body = list()
    ),
    stop(sprintf("Unsupported LLM provider '%s'.", normalized_provider), call. = FALSE)
  )
}
