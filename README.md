# OffensiveToolMapper

Система мониторинга и классификации наступательных инструментов безопасности. Автоматически собирает инструменты с GitHub, оценивает их нейросетью и строит MITRE ATT&CK-маппинги.

## Что запускается

`docker compose up -d` поднимает два сервиса:

| Сервис | URL | Назначение |
| --- | --- | --- |
| Shiny-приложение | `http://localhost:8788` | основной интерфейс: обзор, аналитика, инструменты, MITRE-связи и пайплайн |
| MCP-сервер | `http://localhost:3000` | HTTP MCP-сервер для подключения к нейросетевым агентам |

Первый запуск возможен без собранных данных: интерфейсы откроются, но список инструментов будет пустым. Реальные записи появляются после запуска пайплайна.

## Требования

Для запуска через Docker:

- Docker Desktop;
- включённый Docker daemon;
- доступ к интернету для установки Docker/R-зависимостей при первой сборке.

Проверка Docker:

```powershell
docker info
docker compose version
```

Если `docker info` не подключается к Docker API, сначала запусти Docker Desktop.

Для локальных проверок без Docker дополнительно нужен R.

## Быстрый старт

```powershell
git clone https://github.com/KNikitaaa/CyberSecML-NetAdmins.git
cd CyberSecML-NetAdmins
.\scripts\setup_env.ps1
docker compose pull
docker compose up -d
```

If PowerShell blocks direct `.ps1` execution:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/setup_env.ps1
docker compose pull
docker compose up -d
```

Local Docker build is only needed for development:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/build_local.ps1
```

Maintainers publish the prebuilt image with `.github/workflows/docker-image.yml`. The default startup image is controlled by `OTM_DOCKER_IMAGE`; for this repository it should be `ghcr.io/knikitaaa/cybersecml-netadmins:latest`.

Если PowerShell запрещает запуск `.ps1`:

```powershell
powershell -ExecutionPolicy Bypass -File .\scripts\setup_env.ps1
docker compose pull
docker compose up -d
```

## Тестовый запуск (быстрая проверка)

Чтобы убедиться, что пайплайн работает, не тратя деньги на LLM и время на GitHub — задай в `.env` минимальные лимиты:

```env
LLM_PROVIDER=deepseek
LLM_MODEL=deepseek-chat
DEEPSEEK_API_KEY=sk-...
GITHUB_PAT=github_pat_...

# Лимиты для быстрой проверки
LLM_MAX_RECORDS=
OTM_GITHUB_MAX_SEARCH_REQUESTS=1
OTM_GITHUB_MAX_RESULTS=30
OTM_RSS_FEEDS=disabled
```

Такой профиль удобен для быстрой проверки: GitHub делает один search-запрос (`GitHub request 1/1`), сохраняет до 30 уникальных репозиториев, затем normalize/pre-filter отбирает кандидатов для LLM. `LLM_MAX_RECORDS=` оставлен пустым, поэтому искусственного лимита LLM нет; фактическое число LLM-запросов зависит от того, сколько кандидатов пройдет фильтр.

Для более полного запуска, который работает дольше и собирает шире, можно увеличить GitHub-лимиты:

```env
LLM_MAX_RECORDS=
OTM_GITHUB_MAX_SEARCH_REQUESTS=15
OTM_GITHUB_MAX_RESULTS=300
OTM_RSS_FEEDS=
```

Такой профиль сделает до 15 GitHub Search API-запросов (`GitHub request 1/15` ... `15/15`), сохранит до 300 уникальных репозиториев и оставит RSS включенным. Он лучше подходит для полноценного наполнения каталога, но потребует больше времени, GitHub rate limit и LLM-запросов.

> Вместо DeepSeek подойдёт любой дешёвый провайдер: `groq` (llama-3.3-70b-versatile — бесплатный тариф), `mistral` (mistral-small-latest).

Запуск через CLI:

```powershell
# Только сбор данных, без LLM — мгновенно, бесплатно
Rscript data-raw/run_collect_only.R

# Полный пайплайн с лимитами — 1–3 минуты вместо десятков
Rscript data-raw/run_full_pipeline.R
```

Следить за ходом в реальном времени:

```powershell
Get-Content inst\extdata\pipeline_shiny.log -Wait -Tail 30
```

Или через Docker:

```powershell
docker compose run --rm shiny-app Rscript data-raw/run_full_pipeline.R
```

После запуска в Shiny-приложении (`http://localhost:8788`) на вкладке **Инструменты** должны появиться первые карточки.

## Настройка `.env`

Проект читает настройки из `.env` в корне репозитория. Этот файл не коммитится.

Самый удобный способ создать `.env` на Windows:

```powershell
.\scripts\setup_env.ps1
```

Скрипт спросит:

- `LLM_PROVIDER`: имя провайдера (openai, anthropic, deepseek, xai, groq, mistral, ollama);
- `LLM_MODEL`: можно оставить значение по умолчанию;
- API-ключ выбранного провайдера: ввод скрытый, символы не отображаются;
- дополнительные ключи других провайдеров (необязательно);
- `GITHUB_PAT`: желательно указать, чтобы не упираться в низкие GitHub-лимиты;
- лимиты запусков: по умолчанию `LLM_MAX_RECORDS` пустой и LLM обрабатывает все подходящие записи; для теста можно поставить, например, `LLM_MAX_RECORDS=20`.

Если нужные переменные уже установлены в системе, скрипт подхватит их и предложит оставить текущие значения через Enter.

Ручной способ:

```powershell
Copy-Item .env.example .env
notepad .env
```

Linux/macOS:

```bash
cp .env.example .env
```

## Поддерживаемые нейросети

Проект поддерживает 7 провайдеров через единый OpenAI-совместимый интерфейс:

| Провайдер | `LLM_PROVIDER` | Модель по умолчанию | Переменная ключа |
| --- | --- | --- | --- |
| OpenAI (ChatGPT) | `openai` | `gpt-4.1-mini` | `OPENAI_API_KEY` |
| Anthropic (Claude) | `anthropic` | `claude-sonnet-4-6` | `ANTHROPIC_API_KEY` |
| DeepSeek | `deepseek` | `deepseek-chat` | `DEEPSEEK_API_KEY` |
| xAI (Grok) | `xai` | `grok-3-mini` | `XAI_API_KEY` |
| Groq | `groq` | `llama-3.3-70b-versatile` | `GROQ_API_KEY` |
| Mistral AI | `mistral` | `mistral-small-latest` | `MISTRAL_API_KEY` |
| Ollama (локальный) | `ollama` | `qwen3:8b` | не нужен |

Пример для Claude:

```env
LLM_PROVIDER=anthropic
LLM_MODEL=claude-sonnet-4-6
ANTHROPIC_API_KEY=sk-ant-...
GITHUB_PAT=github_pat_...
```

Пример для Grok:

```env
LLM_PROVIDER=xai
LLM_MODEL=grok-3-mini
XAI_API_KEY=xai-...
GITHUB_PAT=github_pat_...
```

Пример для локального Ollama:

```env
LLM_PROVIDER=ollama
LLM_MODEL=qwen3:8b
# LLM_BASE_URL=http://localhost:11434/v1  # по умолчанию
GITHUB_PAT=github_pat_...
```

## Обязательные ключи

Для полноценного пайплайна нужны:

- `GITHUB_PAT` — GitHub Personal Access Token;
- один API-ключ выбранного LLM-провайдера.

Обычно можно оставить пустыми:

- `OPENAI_ORG_ID`;
- `OPENAI_PROJECT_ID`;
- `LLM_BASE_URL`;
- `LLM_MAX_RECORDS`.

Обычно не нужно менять:

- `OTM_MCP_TRANSPORT`;
- `OTM_MCP_PORT`;
- `OFFENSIVETOOLMAPPER_DATA_DIR`.

## Все переменные `.env`

| Переменная | Когда нужна |
| --- | --- |
| `LLM_PROVIDER` | всегда: имя провайдера |
| `LLM_MODEL` | всегда: модель выбранного провайдера (пустое = по умолчанию) |
| `LLM_BASE_URL` | только для кастомного OpenAI-compatible endpoint |
| `LLM_MAX_RECORDS` | для ограничения LLM-запросов в тестовом запуске |
| `OPENAI_API_KEY` | если `LLM_PROVIDER=openai` |
| `ANTHROPIC_API_KEY` | если `LLM_PROVIDER=anthropic` |
| `DEEPSEEK_API_KEY` | если `LLM_PROVIDER=deepseek` |
| `XAI_API_KEY` | если `LLM_PROVIDER=xai` |
| `GROQ_API_KEY` | если `LLM_PROVIDER=groq` |
| `MISTRAL_API_KEY` | если `LLM_PROVIDER=mistral` |
| `OPENAI_ORG_ID` | только если требуется OpenAI-организацией |
| `OPENAI_PROJECT_ID` | только если требуется OpenAI-организацией |
| `GITHUB_PAT` | для GitHub-сбора |
| `OTM_COLLECT_MODE` | `incremental` или `snapshot` |
| `OTM_GITHUB_MIN_STARS` | глобальный минимум stars для GitHub discovery; по умолчанию `0` |
| `OTM_GITHUB_MAX_RESULTS` | максимум уникальных GitHub-репозиториев, которые collect-этап сохранит после дедупликации |
| `OTM_GITHUB_MAX_SEARCH_REQUESTS` | максимум GitHub Search API-запросов за один запуск collect-этапа |
| `OTM_MCP_TRANSPORT` | транспорт MCP, в Docker используется `http` |
| `OTM_MCP_PORT` | порт MCP HTTP-сервера |
| `OFFENSIVETOOLMAPPER_DATA_DIR` | директория артефактов; в Docker задаётся автоматически |

## Запуск сервисов

```powershell
docker compose up -d
```

Открыть:

- `http://localhost:8788`;
- `http://localhost:3000`.

Остановить:

```powershell
docker compose down
```

## Запуск пайплайна из интерфейса

В Shiny-приложении (вкладка **Пайплайн**) есть кнопка **«Запустить пайплайн»**.

По нажатию открывается диалог, где можно настроить:

- провайдер нейросети и модель;
- лимит записей LLM (0 = без лимита);
- режим сбора (инкрементальный / снапшот);
- количество запусков подряд (batch).

После подтверждения пайплайн запускается в фоновом режиме, а таблицы на странице обновляются автоматически.

## Источники данных

Пайплайн собирает данные из трёх источников.

### GitHub (основной)

Поиск репозиториев по ключевым словам, языку и количеству звёзд. Управляется через `.env`:

```env
OTM_GITHUB_MIN_STARS=0
OTM_GITHUB_MAX_RESULTS=30
OTM_GITHUB_MAX_SEARCH_REQUESTS=1
```

`OTM_GITHUB_MAX_SEARCH_REQUESTS` ограничивает расход GitHub API: один search-запрос = один запрос к `/search/repositories` для конкретного query/sort/page. `OTM_GITHUB_MAX_RESULTS` ограничивает уже итоговый набор уникальных репозиториев после объединения и дедупликации ответов.

Пример: при `OTM_GITHUB_MAX_SEARCH_REQUESTS=1` и `OTM_GITHUB_MAX_RESULTS=30` в логе должно быть `GitHub request 1/1`. GitHub может вернуть больше репозиториев, но пайплайн сохранит максимум 30 уникальных, затем пропустит их через normalize/pre-filter, и к LLM уйдут только кандидаты, которые прошли фильтр и ещё не были успешно обработаны раньше.

### RSS-ленты

По умолчанию подключены три открытых ленты безопасности:

| Лента | URL |
| --- | --- |
| Exploit-DB | `https://www.exploit-db.com/rss.xml` |
| The Hacker News | `https://feeds.feedburner.com/TheHackersNews` |
| BleepingComputer | `https://www.bleepingcomputer.com/feed/` |

Чтобы задать другие ленты, укажи их через точку с запятой в `.env`:

```env
OTM_RSS_FEEDS=https://www.exploit-db.com/rss.xml;https://example.com/feed.xml
```

Чтобы отключить RSS: `OTM_RSS_FEEDS=disabled`

### PacketStorm

Доступен в двух режимах:

**Ручные URL** (без API-ключа):

```env
PACKETSTORM_URLS=https://packetstormsecurity.com/files/180000/tool-name.tgz;https://...
```

**API-доступ** (нужен ключ):

```env
PACKETSTORM_API_SECRET=ваш-ключ
```

Если оба варианта пустые — PacketStorm отключён.

## Сбор данных

Есть два режима запуска пайплайна из CLI:

**Только сбор** — без LLM, без расходов, для проверки источников данных:

```powershell
Rscript data-raw/run_collect_only.R
```

Выводит количество строк из GitHub, RSS и PacketStorm. LLM-этап не запускается.

**Полный пайплайн** с ограничениями для тестирования:

```powershell
Rscript data-raw/run_full_pipeline.R
```

Для короткого прогона задай в `.env`:

```env
LLM_MAX_RECORDS=
OTM_GITHUB_MAX_SEARCH_REQUESTS=1
OTM_GITHUB_MAX_RESULTS=30
OTM_RSS_FEEDS=disabled
```

Запуск внутри Docker:

```powershell
docker compose run --rm shiny-app Rscript data-raw/run_full_pipeline.R
docker compose up -d
```

После пайплайна в Shiny-приложении должны появиться карточки инструментов, тактики и техники MITRE.

## Этапы пайплайна

Пайплайн состоит из пяти этапов, которые запускаются последовательно:

1. **collect** — поиск новых инструментов на GitHub (по ключевым словам, stars, языку);
2. **normalize** — дедупликация, извлечение метаданных, предварительная оценка по эвристикам;
3. **assessment** — оценка каждого инструмента нейросетью: категория, тактики MITRE, уверенность;
4. **refine_mitre** — уточнение MITRE-маппингов для записей с высокой уверенностью;
5. **visualize** — построение слоя визуализации из DuckDB-данных в `.rds`-снапшоты для Shiny.

Каждый запуск фиксируется в DuckDB с уникальным `run_id`. Это позволяет:

- сравнивать запуски в Shiny (вкладка «Пайплайн» → «Сравнение запусков»);
- отслеживать прирост новых инструментов между запусками;
- видеть изменения в MITRE-маппингах со временем.

**Одиночный запуск** (`run_full_pipeline.R`) проходит все 5 этапов один раз.

**Пакетный запуск** (`run_full_pipeline_batch.R N`) повторяет полный цикл N раз подряд. Каждый цикл получает отдельный `run_id` и отдельную запись в `batch_summary`. Используется для постепенного накопления данных с GitHub (ротация поисковых запросов не позволяет собрать всё за один проход) или для проверки стабильности LLM-оценок.

## MCP-сервер

MCP (Model Context Protocol) — стандарт Anthropic для подключения инструментов к нейросетевым агентам. Проект реализует полноценный MCP-сервер с пятью инструментами:

| Инструмент | Назначение |
| --- | --- |
| `search_tools` | поиск инструментов по ключевым словам |
| `get_tool_ttps` | получение MITRE TTP-маппингов для конкретного инструмента |
| `get_technique_tools` | поиск инструментов по технике MITRE (T-код) |
| `get_statistics` | статистика базы: число инструментов, покрытие тактик |
| `classify_new_tool` | классификация произвольного инструмента нейросетью |

MCP-сервер поддерживает два транспорта:
- **stdio** — для встраивания в Claude Desktop / Claude Code как локального MCP-сервера;
- **HTTP** — для удалённого подключения (в Docker запускается на порту 3000).

### Подключение к Claude Desktop (stdio)

Добавь в конфиг Claude Desktop:

```json
{
  "mcpServers": {
    "OffensiveToolMapper": {
      "command": "Rscript",
      "args": ["inst/mcp/run_server.R"],
      "cwd": "/path/to/CyberSecML-NetAdmins",
      "env": {
        "OTM_MCP_TRANSPORT": "stdio",
        "OFFENSIVETOOLMAPPER_DATA_DIR": "/path/to/CyberSecML-NetAdmins/inst/extdata"
      }
    }
  }
}
```

### Запуск MCP-сервера вручную

```bash
# stdio
Rscript inst/mcp/run_server.R

# HTTP (порт 3000)
OTM_MCP_TRANSPORT=http OTM_MCP_PORT=3000 Rscript inst/mcp/run_server.R
```

## R-пакет

Проект оформлен как полноценный R-пакет (`DESCRIPTION`, `NAMESPACE`, `R/`, `tests/`). Это позволяет:

- устанавливать пакет через `devtools::install()` или `pak::pkg_install()`;
- использовать функции пайплайна напрямую из R-сессии;
- запускать тесты через `testthat`.

### Установка пакета

```r
# из локальной директории
devtools::install("/path/to/CyberSecML-NetAdmins")

# или через pak
pak::pkg_install("local::/path/to/CyberSecML-NetAdmins")
```

### Использование без Docker

Если R установлен локально, все компоненты работают без Docker:

```r
library(OffensiveToolMapper)

# Запуск Shiny-приложения
shiny::runApp("inst/shiny/app.R")

# Запуск MCP-сервера
run_mcp_server(transport = "stdio")
```

Пайплайн запускается через Rscript:

```bash
Rscript data-raw/run_full_pipeline.R
```

Зависимости устанавливаются автоматически через `renv` (если он активен) или вручную:

```r
install.packages(c("shiny", "bslib", "dplyr", "duckdb", "httr2", "jsonlite", "DT"))
```
