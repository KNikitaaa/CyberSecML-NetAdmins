project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "R", "utils_core.R"))
source(file.path(project_root, "R", "etl_mitre.R"))

log_message("Refreshing local MITRE ATT&CK dataset")

mitre_attack <- save_mitre_attack_dataset(
  output_path = file.path(project_root, "data", "mitre_attack.rda")
)

log_message(sprintf("MITRE ATT&CK dataset refreshed with %s rows", nrow(mitre_attack)))
