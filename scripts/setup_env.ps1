param(
  [string]$Provider = "",
  [string]$Model = "",
  [string]$OpenAiApiKey = "",
  [string]$AnthropicApiKey = "",
  [string]$DeepSeekApiKey = "",
  [string]$XaiApiKey = "",
  [string]$GroqApiKey = "",
  [string]$MistralApiKey = "",
  [string]$GithubPat = "",
  [string]$OpenAiOrgId = "",
  [string]$OpenAiProjectId = "",
  [string]$LlmBaseUrl = "",
  [string]$LlmMaxRecords = "",
  [string]$GithubMaxResults = "",
  [string]$GithubMaxSearchRequests = "",
  [string]$RssFeeds = "",
  [string]$PacketStormApiSecret = "",
  [string]$PacketStormUrls = "",
  [string]$DockerImage = "",
  [string]$OutputPath = "",
  [switch]$Force,
  [switch]$NonInteractive
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = Resolve-Path -LiteralPath (Join-Path $PSScriptRoot "..")
if (-not $OutputPath) {
  $OutputPath = Join-Path $repoRoot ".env"
}

$SupportedProviders = @("openai", "anthropic", "deepseek", "xai", "groq", "mistral", "ollama")

function Get-EnvValue {
  param([string]$Name)
  if (Test-Path -LiteralPath $OutputPath) {
    foreach ($line in Get-Content -LiteralPath $OutputPath -Encoding UTF8) {
      $trimmed = $line.Trim()
      if (-not $trimmed -or $trimmed.StartsWith("#")) { continue }

      $parts = $trimmed.Split("=", 2)
      if ($parts.Count -lt 2) { continue }

      $key = $parts[0].Trim()
      if ($key -ne $Name) { continue }

      return $parts[1].Trim().Trim('"').Trim("'")
    }
  }

  $value = [Environment]::GetEnvironmentVariable($Name, "Process")
  if (-not $value) { $value = [Environment]::GetEnvironmentVariable($Name, "User") }
  if (-not $value) { $value = [Environment]::GetEnvironmentVariable($Name, "Machine") }
  if ($value) { return $value.Trim() }
  ""
}

function Write-SetupHeader {
  Write-Host ""
  Write-Host "OffensiveToolMapper .env setup"
  Write-Host "Creates a local .env file for Docker, Shiny, MCP and the data pipeline."
  Write-Host ""
  Write-Host "Supported providers: openai, anthropic, deepseek, xai, groq, mistral, ollama"
  Write-Host ""
  Write-Host "How to answer:"
  Write-Host "  - For LLM_PROVIDER enter only the provider name (see list above)."
  Write-Host "  - API keys are requested later; hidden input is normal."
  Write-Host "  - Press Enter to accept the value in [brackets]."
  Write-Host "  - Optional fields can stay empty."
  Write-Host ""
}

function Write-SetupStep {
  param([string]$Title, [string]$Text = "")
  Write-Host ""
  Write-Host "== $Title =="
  if ($Text) { Write-Host $Text }
}

function Read-PlainValue {
  param([string]$Prompt, [string]$Default = "")
  if ($Default) {
    $value = Read-Host "$Prompt [$Default]"
    if (-not $value) { return $Default }
    return $value.Trim()
  }
  return (Read-Host $Prompt).Trim()
}

function Read-SecretValue {
  param([string]$Prompt, [string]$ExistingValue = "")
  $effectivePrompt = if ($ExistingValue) { "$Prompt [already set, press Enter to keep]" } else { $Prompt }
  $secure = Read-Host $effectivePrompt -AsSecureString
  if ($secure.Length -eq 0) { return $ExistingValue }
  $ptr = [Runtime.InteropServices.Marshal]::SecureStringToBSTR($secure)
  try { return [Runtime.InteropServices.Marshal]::PtrToStringBSTR($ptr) }
  finally { [Runtime.InteropServices.Marshal]::ZeroFreeBSTR($ptr) }
}

function Test-LooksLikeApiKey {
  param([string]$Value)
  if (-not $Value) { return $false }
  $trimmed = $Value.Trim().Trim('"').Trim("'")
  return $trimmed -match '^(sk-|sk_|github_pat_|ghp_)'
}

function Get-DefaultModel {
  param([string]$ProviderName)
  switch ($ProviderName) {
    "openai"    { return "gpt-4.1-mini" }
    "anthropic" { return "claude-sonnet-4-6" }
    "deepseek"  { return "deepseek-chat" }
    "xai"       { return "grok-3-mini" }
    "groq"      { return "llama-3.3-70b-versatile" }
    "mistral"   { return "mistral-small-latest" }
    "ollama"    { return "qwen3:8b" }
    default     { return "gpt-4.1-mini" }
  }
}

function Add-EnvLine {
  param([System.Collections.Generic.List[string]]$Lines, [string]$Key, [string]$Value = "")
  $safeValue = if ($null -eq $Value) { "" } else { $Value.Trim() }
  $Lines.Add("$Key=$safeValue")
}

if ((Test-Path -LiteralPath $OutputPath) -and -not $Force -and -not $NonInteractive) {
  $answer = Read-PlainValue "$OutputPath already exists. Overwrite? y/N" "N"
  if ($answer.ToLowerInvariant() -notin @("y", "yes")) {
    Write-Host "Existing .env was left unchanged."
    exit 0
  }
}

if ((Test-Path -LiteralPath $OutputPath) -and -not $Force -and $NonInteractive) {
  throw "$OutputPath already exists. Pass -Force to overwrite."
}

$EnvProvider = Get-EnvValue "LLM_PROVIDER"

if (-not $NonInteractive) { Write-SetupHeader }

if (-not $Provider) {
  $Provider = if ($EnvProvider) { $EnvProvider } else { "openai" }
}

if (-not $NonInteractive) {
  Write-SetupStep "1. LLM provider" "Choose the AI provider. openai=ChatGPT, anthropic=Claude, deepseek=DeepSeek, xai=Grok, groq=Groq, mistral=Mistral, ollama=local Ollama."
  $Provider = Read-PlainValue "LLM_PROVIDER (provider name, not an API key)" $Provider
}

$Provider = $Provider.Trim().Trim('"').Trim("'").ToLowerInvariant()
if (Test-LooksLikeApiKey $Provider) {
  throw "You pasted an API key into LLM_PROVIDER. Enter only the provider name: openai, anthropic, deepseek, xai, groq, mistral or ollama."
}

if ($Provider -notin $SupportedProviders) {
  throw "LLM_PROVIDER must be one of: $($SupportedProviders -join ', ')."
}

if (-not $Model) {
  $EnvModel = Get-EnvValue "LLM_MODEL"
  if ($EnvModel -and $EnvProvider -and ($Provider -eq $EnvProvider.Trim().Trim('"').Trim("'").ToLowerInvariant())) {
    $Model = $EnvModel
  }
}
if (-not $Model) { $Model = Get-DefaultModel $Provider }

if (-not $NonInteractive) {
  Write-SetupStep "2. LLM model" "Default model is fine for a first run."
  $Model = Read-PlainValue "LLM_MODEL" $Model
}

if (-not $OpenAiApiKey)    { $OpenAiApiKey    = Get-EnvValue "OPENAI_API_KEY"    }
if (-not $AnthropicApiKey) { $AnthropicApiKey = Get-EnvValue "ANTHROPIC_API_KEY" }
if (-not $DeepSeekApiKey)  { $DeepSeekApiKey  = Get-EnvValue "DEEPSEEK_API_KEY"  }
if (-not $XaiApiKey)       { $XaiApiKey       = Get-EnvValue "XAI_API_KEY"       }
if (-not $GroqApiKey)      { $GroqApiKey      = Get-EnvValue "GROQ_API_KEY"      }
if (-not $MistralApiKey)   { $MistralApiKey   = Get-EnvValue "MISTRAL_API_KEY"   }
if (-not $GithubPat)            { $GithubPat            = Get-EnvValue "GITHUB_PAT"                }
if (-not $OpenAiOrgId)          { $OpenAiOrgId          = Get-EnvValue "OPENAI_ORG_ID"            }
if (-not $OpenAiProjectId)      { $OpenAiProjectId      = Get-EnvValue "OPENAI_PROJECT_ID"        }
if (-not $LlmBaseUrl)           { $LlmBaseUrl           = Get-EnvValue "LLM_BASE_URL"             }
if (-not $RssFeeds)             { $RssFeeds             = Get-EnvValue "OTM_RSS_FEEDS"            }
if (-not $PacketStormApiSecret) { $PacketStormApiSecret = Get-EnvValue "PACKETSTORM_API_SECRET"   }
if (-not $PacketStormUrls)      { $PacketStormUrls      = Get-EnvValue "PACKETSTORM_URLS"         }
if (-not $DockerImage)          { $DockerImage          = Get-EnvValue "OTM_DOCKER_IMAGE"         }

if (-not $NonInteractive) {
  Write-SetupStep "3. API keys" "Enter the key for your selected provider. Other keys are optional."

  switch ($Provider) {
    "openai"    { $OpenAiApiKey    = Read-SecretValue "OPENAI_API_KEY (required for openai)"        $OpenAiApiKey    }
    "anthropic" { $AnthropicApiKey = Read-SecretValue "ANTHROPIC_API_KEY (required for anthropic)"  $AnthropicApiKey }
    "deepseek"  { $DeepSeekApiKey  = Read-SecretValue "DEEPSEEK_API_KEY (required for deepseek)"    $DeepSeekApiKey  }
    "xai"       { $XaiApiKey       = Read-SecretValue "XAI_API_KEY (required for xai/Grok)"         $XaiApiKey       }
    "groq"      { $GroqApiKey      = Read-SecretValue "GROQ_API_KEY (required for groq)"            $GroqApiKey      }
    "mistral"   { $MistralApiKey   = Read-SecretValue "MISTRAL_API_KEY (required for mistral)"      $MistralApiKey   }
    "ollama"    { Write-Host "Ollama runs locally, no API key needed." }
  }

  Write-Host ""
  Write-Host "Additional keys (optional, press Enter to skip):"
  if ($Provider -ne "openai")    { $OpenAiApiKey    = Read-SecretValue "OPENAI_API_KEY (optional)"    $OpenAiApiKey    }
  if ($Provider -ne "anthropic") { $AnthropicApiKey = Read-SecretValue "ANTHROPIC_API_KEY (optional)" $AnthropicApiKey }
  if ($Provider -ne "deepseek")  { $DeepSeekApiKey  = Read-SecretValue "DEEPSEEK_API_KEY (optional)"  $DeepSeekApiKey  }
  if ($Provider -ne "xai")       { $XaiApiKey       = Read-SecretValue "XAI_API_KEY (optional)"       $XaiApiKey       }
  if ($Provider -ne "groq")      { $GroqApiKey      = Read-SecretValue "GROQ_API_KEY (optional)"      $GroqApiKey      }
  if ($Provider -ne "mistral")   { $MistralApiKey   = Read-SecretValue "MISTRAL_API_KEY (optional)"   $MistralApiKey   }

  Write-SetupStep "4. GitHub token" "GITHUB_PAT is recommended for repository collection. Without it GitHub limits are much stricter."
  $GithubPat = Read-SecretValue "GITHUB_PAT (recommended)" $GithubPat

  Write-SetupStep "5. Optional OpenAI/account settings" "Usually leave empty unless you use a custom OpenAI-compatible endpoint or multi-account org."
  $OpenAiOrgId     = Read-PlainValue "OPENAI_ORG_ID (optional)"                    $OpenAiOrgId
  $OpenAiProjectId = Read-PlainValue "OPENAI_PROJECT_ID (optional)"                $OpenAiProjectId
  $LlmBaseUrl      = Read-PlainValue "LLM_BASE_URL (optional, custom endpoint)"    $LlmBaseUrl

  Write-SetupStep "6. Run limits" "LLM_MAX_RECORDS is empty by default, which means no LLM record limit."
  $LlmMaxRecords = Read-PlainValue "LLM_MAX_RECORDS (optional; leave empty = no limit)" $LlmMaxRecords
  $githubResultsDefault = "30"
  $GithubMaxResults = Read-PlainValue "OTM_GITHUB_MAX_RESULTS" $githubResultsDefault
  $githubRequestDefault = "1"
  $GithubMaxSearchRequests = Read-PlainValue "OTM_GITHUB_MAX_SEARCH_REQUESTS" $githubRequestDefault

  Write-SetupStep "7. Additional data sources (optional)" "RSS and PacketStorm run automatically if configured."
  Write-Host "  RSS feeds: leave empty to use defaults (Exploit-DB, TheHackersNews, BleepingComputer)."
  Write-Host "             Set to 'disabled' to turn off. Or enter URLs separated by semicolons."
  $RssFeeds = Read-PlainValue "OTM_RSS_FEEDS (optional)" $RssFeeds
  Write-Host ""
  Write-Host "  PacketStorm: provide an API secret, OR paste page URLs separated by semicolons."
  Write-Host "               Leave both empty to skip PacketStorm collection."
  $PacketStormApiSecret = Read-SecretValue "PACKETSTORM_API_SECRET (optional)" $PacketStormApiSecret
  $PacketStormUrls      = Read-PlainValue  "PACKETSTORM_URLS (optional, semicolon-separated)" $PacketStormUrls

  Write-SetupStep "8. Docker image" "For normal startup use the prebuilt image. Maintainers can override this with their own GHCR path."
  $dockerImageDefault = if ($DockerImage) { $DockerImage } else { "ghcr.io/knikitaaa/cybersecml-netadmins:latest" }
  $DockerImage = Read-PlainValue "OTM_DOCKER_IMAGE" $dockerImageDefault
}

if ($Provider -ne "ollama") {
  $requiredKey = switch ($Provider) {
    "openai"    { $OpenAiApiKey    }
    "anthropic" { $AnthropicApiKey }
    "deepseek"  { $DeepSeekApiKey  }
    "xai"       { $XaiApiKey       }
    "groq"      { $GroqApiKey      }
    "mistral"   { $MistralApiKey   }
    default     { "" }
  }
  if (-not $requiredKey) {
    $keyName = ($Provider.ToUpper() -replace "OPENAI", "OPENAI") + "_API_KEY"
    Write-Warning "$keyName is empty: UI and MCP will start, but LLM assessment will not work."
  }
}

if (-not $GithubPat) {
  Write-Warning "GITHUB_PAT is empty: GitHub collection may hit API rate limits."
}

if (-not $GithubMaxResults) { $GithubMaxResults = "30" }
if (-not $GithubMaxSearchRequests) { $GithubMaxSearchRequests = "1" }
if (-not $DockerImage) { $DockerImage = "ghcr.io/knikitaaa/cybersecml-netadmins:latest" }

$lines = [System.Collections.Generic.List[string]]::new()
$lines.Add("# OffensiveToolMapper local environment")
$lines.Add("# Created by scripts/setup_env.ps1. Do not commit this file.")
$lines.Add("")
$lines.Add("# LLM")
Add-EnvLine $lines "LLM_PROVIDER"   $Provider
Add-EnvLine $lines "LLM_MODEL"      $Model
Add-EnvLine $lines "LLM_BASE_URL"   $LlmBaseUrl
Add-EnvLine $lines "LLM_MAX_RECORDS" $LlmMaxRecords
$lines.Add("")
$lines.Add("# API keys (fill in the one you need)")
Add-EnvLine $lines "OPENAI_API_KEY"     $OpenAiApiKey
Add-EnvLine $lines "OPENAI_ORG_ID"      $OpenAiOrgId
Add-EnvLine $lines "OPENAI_PROJECT_ID"  $OpenAiProjectId
Add-EnvLine $lines "ANTHROPIC_API_KEY"  $AnthropicApiKey
Add-EnvLine $lines "DEEPSEEK_API_KEY"   $DeepSeekApiKey
Add-EnvLine $lines "XAI_API_KEY"        $XaiApiKey
Add-EnvLine $lines "GROQ_API_KEY"       $GroqApiKey
Add-EnvLine $lines "MISTRAL_API_KEY"    $MistralApiKey
$lines.Add("")
$lines.Add("# GitHub collection")
Add-EnvLine $lines "GITHUB_PAT"                     $GithubPat
Add-EnvLine $lines "OTM_COLLECT_MODE"               "incremental"
Add-EnvLine $lines "OTM_GITHUB_MIN_STARS"           "0"
Add-EnvLine $lines "OTM_GITHUB_MAX_RESULTS"         $GithubMaxResults
Add-EnvLine $lines "OTM_GITHUB_MAX_SEARCH_REQUESTS" $GithubMaxSearchRequests
$lines.Add("")
$lines.Add("# Additional data sources")
Add-EnvLine $lines "OTM_RSS_FEEDS"           $RssFeeds
Add-EnvLine $lines "PACKETSTORM_API_SECRET"  $PacketStormApiSecret
Add-EnvLine $lines "PACKETSTORM_URLS"        $PacketStormUrls
$lines.Add("")
$lines.Add("# Shiny/MCP")
Add-EnvLine $lines "OTM_DOCKER_IMAGE"              $DockerImage
Add-EnvLine $lines "OTM_MCP_TRANSPORT"            "http"
Add-EnvLine $lines "OTM_MCP_PORT"                 "3000"
Add-EnvLine $lines "OFFENSIVETOOLMAPPER_DATA_DIR" ""

Set-Content -LiteralPath $OutputPath -Value $lines -Encoding UTF8

Write-Host "Done: created $OutputPath"
Write-Host "Next command:"
Write-Host "  docker compose pull"
Write-Host "  docker compose up -d"
Write-Host ""
Write-Host "After startup:"
Write-Host "  Shiny app:  http://localhost:8788"
Write-Host "  MCP server: http://localhost:3000"
