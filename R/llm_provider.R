.llm_supported_providers <- function() {
  c("openai", "deepseek", "anthropic", "xai", "groq", "mistral", "ollama")
}

get_default_llm_provider <- function() {
  provider <- get_runtime_env_value("LLM_PROVIDER", unset = "openai")
  provider <- tolower(stringr::str_squish(provider))

  supported <- .llm_supported_providers()
  if (!provider %in% supported) {
    stop(
      sprintf(
        "Unsupported LLM provider '%s'. Supported: %s",
        provider,
        paste(supported, collapse = ", ")
      ),
      call. = FALSE
    )
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
    openai    = "gpt-4.1-mini",
    deepseek  = "deepseek-chat",
    anthropic = "claude-sonnet-4-6",
    xai       = "grok-3-mini",
    groq      = "llama-3.3-70b-versatile",
    mistral   = "mistral-small-latest",
    ollama    = "qwen3:8b",
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
    openai    = "https://api.openai.com/v1",
    deepseek  = "https://api.deepseek.com",
    anthropic = "https://api.anthropic.com/v1",
    xai       = "https://api.x.ai/v1",
    groq      = "https://api.groq.com/openai/v1",
    mistral   = "https://api.mistral.ai/v1",
    ollama    = "http://localhost:11434/v1",
    stop(sprintf("Unsupported LLM provider '%s'.", provider), call. = FALSE)
  )
}

.llm_normalize_base_url <- function(base_url) {
  normalized <- stringr::str_squish(as.character(base_url))
  gsub("/+$", "", normalized)
}

get_llm_api_key <- function(provider = get_default_llm_provider()) {
  provider <- tolower(stringr::str_squish(provider))

  if (identical(provider, "ollama")) {
    # Ollama — локальный сервер без аутентификации; возвращаем placeholder
    key <- get_runtime_env_value("OLLAMA_API_KEY", unset = "")
    return(if (nzchar(key)) key else "ollama")
  }

  env_name <- switch(
    provider,
    openai    = "OPENAI_API_KEY",
    deepseek  = "DEEPSEEK_API_KEY",
    anthropic = "ANTHROPIC_API_KEY",
    xai       = "XAI_API_KEY",
    groq      = "GROQ_API_KEY",
    mistral   = "MISTRAL_API_KEY",
    stop(sprintf("Unsupported LLM provider '%s'.", provider), call. = FALSE)
  )

  get_runtime_env_value(env_name, unset = "")
}

get_llm_runtime_config <- function(provider = get_default_llm_provider()) {
  normalized_provider <- tolower(stringr::str_squish(provider))
  api_key <- get_llm_api_key(normalized_provider)

  tibble::tibble(
    provider     = normalized_provider,
    model        = get_default_llm_model(normalized_provider),
    base_url     = get_default_llm_base_url(normalized_provider),
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

.llm_provider_config <- function(provider = get_default_llm_provider(), base_url = NULL) {
  normalized_provider <- tolower(stringr::str_squish(provider))
  resolved_base_url <- if (!is.null(base_url) && nzchar(base_url)) {
    as.character(base_url)
  } else {
    get_default_llm_base_url(normalized_provider)
  }
  resolved_base_url <- .llm_normalize_base_url(resolved_base_url)

  # Все провайдеры, кроме openai, используют json_object (OpenAI-compatible формат)
  json_object_format <- list(type = "json_object")

  switch(
    normalized_provider,
    openai = list(
      provider        = "openai",
      base_url        = resolved_base_url,
      endpoint        = "/chat/completions",
      response_format = NULL,
      extra_body      = list()
    ),
    deepseek = list(
      provider        = "deepseek",
      base_url        = resolved_base_url,
      endpoint        = "/chat/completions",
      response_format = json_object_format,
      extra_body      = list(stream = FALSE)
    ),
    anthropic = list(
      provider        = "anthropic",
      base_url        = resolved_base_url,
      endpoint        = "/chat/completions",
      response_format = json_object_format,
      extra_body      = list()
    ),
    xai = list(
      provider        = "xai",
      base_url        = resolved_base_url,
      endpoint        = "/chat/completions",
      response_format = json_object_format,
      extra_body      = list()
    ),
    groq = list(
      provider        = "groq",
      base_url        = resolved_base_url,
      endpoint        = "/chat/completions",
      response_format = json_object_format,
      extra_body      = list()
    ),
    mistral = list(
      provider        = "mistral",
      base_url        = resolved_base_url,
      endpoint        = "/chat/completions",
      response_format = json_object_format,
      extra_body      = list()
    ),
    ollama = list(
      provider        = "ollama",
      base_url        = resolved_base_url,
      endpoint        = "/chat/completions",
      response_format = json_object_format,
      extra_body      = list()
    ),
    stop(sprintf("Unsupported LLM provider '%s'.", normalized_provider), call. = FALSE)
  )
}
