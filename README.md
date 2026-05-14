# OffensiveToolMapper

OffensiveToolMapper — локальный R-пайплайн для сбора, нормализации, LLM-оценки и отображения offensive security tooling с привязкой к MITRE ATT&CK. Основной интерфейс проекта — Shiny dashboard, а основное хранилище аналитических таблиц — DuckDB.

## Что делает проект

- собирает кандидатов из GitHub API, Packet Storm и RSS/Atom;
- нормализует разные источники в единый candidate layer;
- отсекает documentation-only проекты, cheat sheets, walkthroughs и нерелевантные записи до LLM;
- отправляет подходящие записи в unified LLM assessment;
- получает structured output через OpenAI-compatible function calling;
- строит MITRE mappings с confidence-порогами;
- формирует visualization-ready слой для Shiny;
- хранит состояние, историю запусков и аналитические таблицы в DuckDB;
- поднимает Shiny dashboard и MCP server.

## Архитектура

```text
GitHub / Packet Storm / RSS
        ↓
collect
        ↓
normalize
        ↓
pre-LLM filtering
        ↓
LLM assessment
        ↓
MITRE mappings + refinement candidates
        ↓
visualize
        ↓
DuckDB prepared tables
        ↓
Shiny dashboard / MCP server
```

## Основные стадии pipeline

| Стадия | Назначение | Основной результат |
| --- | --- | --- |
| `collect` | собирает raw-кандидатов из источников | raw source tables, search logs |
| `normalize` | приводит записи к единой структуре и считает pre-LLM признаки | normalized candidates |
| `assessment` | отправляет кандидатов в LLM и получает structured assessment | assessments, LLM queue, MITRE mappings |
| `refine_mitre` | ищет дополнительные candidate-техники через retrieval | refinement candidates |
| `visualize` | готовит слой для Shiny | visualization tools, matrix, modules |
| `sanity_checks` | проверяет качество результата после запуска | sanity checks |

## Сбор инструментов

GitHub является основным discovery-источником. Логика сбора ориентирована не только на популярные проекты, но и на свежие малоизвестные утилиты:

- минимальный порог звёзд не используется как жёсткий барьер;
- fresh discovery lane позволяет новым проектам доходить до LLM;
- используются разные search modes, recency windows и language shards;
- query rotation проходит большой search plan по частям;
- Packet Storm и RSS могут дополнять GitHub внешними источниками.

Pre-LLM слой не пытается идеально классифицировать инструмент. Его задача — убрать очевидный шум и не тратить LLM-запросы на нерелевантные записи.

## LLM assessment

LLM получает не всю MITRE matrix, а описание конкретного кандидата:

```text
Candidate name
Source
URL
Heuristic candidate type
Heuristic score
Heuristic reasons
Metadata JSON
Cleaned raw text / README
```

DeepSeek используется как OpenAI-compatible provider. Ответ запрашивается через function calling: модель должна вернуть структурированный объект, а не свободный текст.

Упрощённая форма ответа:

```json
{
  "is_relevant": true,
  "entity_type": "framework",
  "is_tool": true,
  "name": "example-c2",
  "summary_ru": "Краткое описание инструмента.",
  "purpose_ru": "Для чего используется инструмент.",
  "capabilities_ru": ["регистрация агентов", "выдача команд"],
  "target_platforms": ["Linux", "Windows"],
  "category_ru": "C2 framework",
  "overall_confidence": 0.86,
  "reason_ru": "Почему запись похожа на реальный инструмент.",
  "mitre_classifications": [
    {
      "technique_id": "T1071.001",
      "technique_name": "Web Protocols",
      "tactic": "Command and Control",
      "confidence": 0.82,
      "reasoning_ru": "В описании явно указаны HTTP callbacks."
    }
  ]
}
```

MITRE mappings назначаются консервативно. Если в описании нет явных признаков поведения, модель должна вернуть пустой список техник. Для финальной матрицы применяется confidence threshold, по умолчанию `0.7`.

## MITRE refinement

Полная MITRE ATT&CK matrix не отправляется в каждый LLM-запрос. Она хранится локально и используется retrieval-слоем:

```text
tool description
        ↓
local MITRE retrieval
        ↓
top-k candidate techniques
        ↓
review / confidence gate
```

Такой подход экономит токены и снижает риск случайных общих связей вроде автоматического назначения Reconnaissance всем security-инструментам.

## DuckDB

DuckDB используется как primary storage для аналитических таблиц:

- normalized candidates;
- LLM assessments;
- LLM queue;
- MITRE mappings;
- refinement candidates;
- visualization tables;
- pipeline status;
- run history;
- storage snapshots;
- sanity checks.

RDS-файлы могут использоваться как совместимый локальный cache/snapshot для R-скриптов, но основной смысловой слой проекта — DuckDB.

## Shiny dashboard

Shiny запускается на `http://127.0.0.1:8788`.

Основные вкладки:

- `Overview` — общие метрики, top ranked tools, распределения по источникам и MITRE tactics;
- `Инструменты` — каталог инструментов с фильтрами, поиском, сортировкой и подробной карточкой по клику;
- `MITRE` — confidence filter, heatmap, matrix rows, refinement hotspots и review queue;
- `Pipeline` — состояние collect/normalize/assessment/visualize, LLM queue, DuckDB tables, artifacts и run comparison.

## MCP server

MCP server запускается из `inst/mcp/run_server.R` и может работать в `stdio` или HTTP-режиме. В Docker Compose он поднимается на порту `3000`.

## Быстрый запуск

### Подготовка ключей

Секреты не хранятся в проекте. Каждый пользователь создаёт локальный `.env` из шаблона и вписывает свои ключи:

```powershell
copy .env.example .env
notepad .env
```

Минимально для LLM-оценки нужен ключ провайдера:

- `DEEPSEEK_API_KEY` для режима по умолчанию `LLM_PROVIDER=deepseek`;
- или `LLM_API_KEY`, `LLM_BASE_URL`, `LLM_MODEL` при `LLM_PROVIDER=openai_compatible`;
- `GITHUB_PAT` не обязателен, но нужен для устойчивого GitHub API discovery без быстрого rate limit.

Файл `.env` добавлен в `.gitignore` и `.dockerignore`: он остаётся только на машине пользователя и не попадает в образ Docker.

### Shiny локально

```powershell
powershell -ExecutionPolicy Bypass -File .\data-raw\run_shiny_dashboard.ps1
```

### Docker Compose

```powershell
docker compose up -d --build shiny-app mcp-server
```

Команда скачает базовый R/Shiny image, установит R-зависимости и поднимет оба сервиса. Перед первым запуском убедитесь, что `.env` создан из `.env.example`.

После запуска:

- Shiny: `http://127.0.0.1:8788`
- MCP HTTP: `http://127.0.0.1:3000`

### Первый сбор данных

После настройки ключей можно запустить полный pipeline из контейнера:

```powershell
docker compose run --rm shiny-app Rscript data-raw/run_full_pipeline.R
```

Артефакты pipeline пишутся в `inst/extdata/`. Эта директория игнорируется git, поэтому локальные результаты, DuckDB и очереди LLM не попадут в репозиторий.

## Основные переменные окружения

| Переменная | Назначение |
| --- | --- |
| `GITHUB_PAT` | GitHub token для API search и README fetch |
| `DEEPSEEK_API_KEY` | API key для DeepSeek |
| `OPENAI_API_KEY` | fallback key для OpenAI-compatible режима |
| `LLM_PROVIDER` | LLM provider, по умолчанию DeepSeek |
| `LLM_MODEL` | модель LLM |
| `LLM_CALL_MODE` | режим вызова, например `tools` для function calling |
| `LLM_MAX_RECORDS` | лимит записей на один LLM-run |
| `OTM_GITHUB_MIN_STARS` | глобальный floor по звёздам, обычно `0` |
| `OTM_GITHUB_MAX_SEARCH_REQUESTS` | бюджет GitHub search-запросов на запуск |
| `OTM_MITRE_MAPPING_MIN_CONFIDENCE` | порог MITRE mappings для visualization layer |
| `OFFENSIVETOOLMAPPER_DATA_DIR` | путь к runtime data directory |
| `OTM_MCP_TRANSPORT` | `stdio` или `http` |
| `OTM_MCP_PORT` | порт MCP HTTP server |

## Публикация на GitHub

Перед первым push проверьте, что в индекс не попали секреты или runtime-данные:

```powershell
git status --short
git ls-files .env .env.local .Renviron inst/extdata
```

Обе команды не должны показывать реальные `.env`-файлы или содержимое `inst/extdata`. В репозиторий должен попадать только `.env.example` с пустыми значениями.

Рекомендуемый публичный набор:

- исходный код `R/`, `inst/shiny/`, `inst/mcp/`, `data-raw/`;
- `Dockerfile`, `docker-compose.yml`, `.env.example`;
- `data/mitre_attack.rda`, если хотите дать готовую локальную MITRE matrix без первого скачивания;
- `README.md`, `LICENSE`, тесты.

Не публикуйте:

- `.env`, `.env.local`, `.Renviron`;
- `inst/extdata/` с DuckDB, RDS, очередями LLM и search logs;
- debug-файлы с сырыми ответами моделей или API.

## Тесты

Примеры точечных проверок:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "testthat::test_file('tests/testthat/test-encoding.R')"
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "testthat::test_file('tests/testthat/test-pipeline.R')"
```

Проверка Shiny-парсинга:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "source('inst/shiny/app.R')"
```

## Структура проекта

```text
R/                 package modules: ETL, normalize, LLM, MITRE, pipeline, DuckDB
data-raw/          operational scripts
data/              static datasets, including MITRE ATT&CK
inst/shiny/        Shiny dashboard
inst/mcp/          MCP launcher
tests/testthat/    regression and unit tests
man/               generated R documentation
```
