project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "R", "utils_core.R"))
source(file.path(project_root, "R", "text_processing.R"))
source(file.path(project_root, "R", "normalize.R"))
source(file.path(project_root, "R", "llm_provider.R"))
source(file.path(project_root, "R", "llm_contracts.R"))
source(file.path(project_root, "R", "llm_validation.R"))
source(file.path(project_root, "R", "llm_assessment.R"))

provider <- get_default_llm_provider()
model <- get_default_llm_model(provider)
base_url <- get_default_llm_base_url(provider)
api_key <- get_llm_api_key(provider)

if (!nzchar(api_key)) {
  stop(sprintf("API key for provider '%s' is required.", provider), call. = FALSE)
}

normalized <- load_pipeline_rds(file.path(project_root, "inst", "extdata", "normalized_tools.rds"), required = TRUE)
candidates <- get_assessment_candidates(normalized_data = normalized, max_records = 1)

if (nrow(candidates) == 0) {
  stop("No assessment candidates found.", call. = FALSE)
}

record <- candidates[1, , drop = FALSE]
prompt <- build_unified_tool_assessment_prompt(record)
raw_response <- .openai_chat_completion(
  system_prompt = .assessment_system_prompt(),
  user_prompt = prompt,
  model = model,
  api_key = api_key,
  provider = provider,
  base_url = base_url,
  schema_name = "unified_tool_assessment",
  schema = get_unified_tool_assessment_schema()
)

debug_path <- file.path(project_root, "inst", "extdata", "debug_deepseek_unified_response.txt")
writeLines(raw_response, con = debug_path, useBytes = TRUE)

cat("debug_record_id=", record$record_id[[1]], "\n", sep = "")
cat("debug_name=", record$name[[1]], "\n", sep = "")
cat("debug_output_path=", debug_path, "\n", sep = "")