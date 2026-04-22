# ===============================================================================
# КОНВЕЙЕР ОЦИФРОВКИ АРХИВНЫХ МАТЕРИАЛОВ
# Проект: Горнозаводские корни Белорецка
#
# Как работает:
#   1. Фото из музея → папка 4.Data/raw/images/
#   2. OCR читает текст с каждого фото
#   3. GPT анализирует фото + текст, находит факты
#   4. Факты сохраняются в таблицу facts.csv
#   5. Из фактов строится справочник сущностей (entities.csv)
#   6. Из сущностей строится граф для Shiny дашборда
# ===============================================================================

library(targets)

# Загрузка настроек и функций
source("1.R/04.config.R")
source("1.R/03.functions.R")

# Пакеты которые нужны на всех шагах
tar_option_set(
  packages = c(
    "tesseract",    # OCR — чтение текста с фото
    "httr",         # HTTP запросы к GPT API
    "jsonlite",     # работа с JSON
    "base64enc",    # кодирование фото для API
    "dplyr",        # работа с таблицами
    "readr",        # чтение/запись CSV
    "readxl",       # чтение Excel
    "writexl"       # запись Excel
  ),
  format = "qs"
)

list(

  # ════════════════════════════════════════════════════════════════════════════
  # ШАГ 1: НАСТРОЙКИ
  # Что: загружаем ключ GPT API из переменной окружения
  # Зачем: чтобы не хранить секретный ключ прямо в коде
  # ════════════════════════════════════════════════════════════════════════════

  tar_target(
    name    = gpt_api_key,
    command = {
      key <- Sys.getenv("OPENAI_API_KEY")
      if (nchar(key) == 0) stop("❌ Не задан OPENAI_API_KEY. Добавь его в .Renviron")
      message("✅ GPT API ключ найден")
      key
    }
  ),

  # ════════════════════════════════════════════════════════════════════════════
  # ШАГ 2: НАХОДИМ ФОТО
  # Что: смотрим в папку 4.Data/raw/images/ и берём все файлы
  # Зачем: targets запомнит список — если добавится новое фото, пересчитает только его
  # ════════════════════════════════════════════════════════════════════════════

  tar_target(
    name    = image_files,
    command = {
      images_dir <- file.path(PATHS$data_raw, "images")
      files      <- list_new_images(images_dir)
      if (length(files) == 0) message("⚠️ Фото не найдены. Положи их в 4.Data/raw/images/")
      files
    }
  ),

  # ════════════════════════════════════════════════════════════════════════════
  # ШАГ 3: OCR — ЧИТАЕМ ТЕКСТ С КАЖДОГО ФОТО
  # Что: Tesseract смотрит на каждое фото и переводит картинку в текст
  # pattern = map() означает: делай это для каждого фото отдельно
  # ════════════════════════════════════════════════════════════════════════════

  tar_target(
    name    = ocr_results,
    command = run_ocr(image_files),
    pattern = map(image_files)
  ),

  # ════════════════════════════════════════════════════════════════════════════
  # ШАГ 4: GPT — АНАЛИЗИРУЕМ КАЖДОЕ ФОТО
  # Что: отправляем фото + OCR текст в GPT-4, получаем факты в JSON
  # ════════════════════════════════════════════════════════════════════════════

  tar_target(
    name    = gpt_responses,
    command = analyze_with_gpt(image_files, ocr_results, gpt_api_key),
    pattern = map(image_files, ocr_results)
  ),

  # ════════════════════════════════════════════════════════════════════════════
  # ШАГ 5: РАЗБИРАЕМ ОТВЕТЫ GPT → СТРОКИ ТАБЛИЦЫ
  # Что: JSON от GPT превращаем в строки таблицы (кто, что, связь, дата)
  # ════════════════════════════════════════════════════════════════════════════

  tar_target(
    name    = parsed_facts,
    command = parse_gpt_response(gpt_responses, image_files),
    pattern = map(gpt_responses, image_files)
  ),

  # ════════════════════════════════════════════════════════════════════════════
  # ШАГ 6: СОБИРАЕМ ВСЕ ФАКТЫ В ОДНУ ТАБЛИЦУ И СОХРАНЯЕМ
  # Что: объединяем факты со всех фото в одну таблицу, сохраняем в CSV
  # Файл: 4.Data/processed/facts.csv
  # ════════════════════════════════════════════════════════════════════════════

  tar_target(
    name    = facts_table,
    command = {
      # Объединяем все факты в одну таблицу
      all_facts <- dplyr::bind_rows(parsed_facts)
      message(paste("📋 Всего фактов:", nrow(all_facts)))

      # Добавляем уникальный ID каждому факту
      if (nrow(all_facts) > 0) {
        all_facts$id <- seq_len(nrow(all_facts))
      }

      # Сохраняем в CSV
      output_path <- file.path(PATHS$data_processed, "facts.csv")
      readr::write_csv(all_facts, output_path)
      message(paste("💾 Сохранено в", output_path))

      all_facts
    }
  ),

  # ════════════════════════════════════════════════════════════════════════════
  # ШАГ 7: СТРОИМ СПРАВОЧНИК СУЩНОСТЕЙ
  # Что: собираем всех уникальных людей и объекты из фактов
  # Файл: 4.Data/processed/entities.csv
  # ════════════════════════════════════════════════════════════════════════════

  tar_target(
    name    = entities_table,
    command = {
      entities <- build_entities(facts_table)

      output_path <- file.path(PATHS$data_processed, "entities.csv")
      readr::write_csv(entities, output_path)
      message(paste("💾 Сущностей сохранено:", nrow(entities)))

      entities
    }
  ),

  # ════════════════════════════════════════════════════════════════════════════
  # ШАГ 8: СТРОИМ ДАННЫЕ ДЛЯ ГРАФА
  # Что: из фактов и сущностей делаем nodes.xlsx и links.xlsx
  # Эти файлы читает Shiny и рисует интерактивный граф
  # ════════════════════════════════════════════════════════════════════════════

  tar_target(
    name    = graph_data,
    command = {
      graph <- build_graph_data(facts_table, entities_table)

      # Сохраняем в Excel (как было в оригинальном проекте)
      writexl::write_xlsx(graph$nodes, file.path(PATHS$data_raw, "nodes.xlsx"))
      writexl::write_xlsx(graph$edges, file.path(PATHS$data_raw, "links.xlsx"))

      message(paste("🕸️ Граф готов:", nrow(graph$nodes), "узлов,", nrow(graph$edges), "связей"))
      graph
    }
  )

)
