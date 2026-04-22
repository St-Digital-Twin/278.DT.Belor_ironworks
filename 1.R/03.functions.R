# ===============================================================================
# ФУНКЦИИ КОНВЕЙЕРА ОЦИФРОВКИ АРХИВНЫХ МАТЕРИАЛОВ
# Проект: Горнозаводские корни Белорецка
# ===============================================================================

# ── ШАГ 2: OCR — чтение текста с фото ──────────────────────────────────────────
# Что делает: смотрит на фото документа и переводит картинку букв в текст
# Программа: Tesseract (бесплатная, ставится один раз)

run_ocr <- function(image_path) {
  message(paste("🔍 Читаю текст с:", basename(image_path)))

  if (!file.exists(image_path)) {
    warning(paste("Файл не найден:", image_path))
    return(NA_character_)
  }

  result <- tryCatch({
    # Загружаем русский + английский языки для распознавания
    engine <- tesseract::tesseract(language = "rus+eng")
    text   <- tesseract::ocr(image_path, engine = engine)
    message(paste("✅ Текст извлечён, символов:", nchar(text)))
    text
  }, error = function(e) {
    warning(paste("❌ OCR не сработал для", basename(image_path), ":", e$message))
    NA_character_
  })

  result
}

# ── ШАГ 3: GPT Vision — анализ документа ───────────────────────────────────────
# Что делает: отправляет фото + текст в ChatGPT и просит найти
#             людей, объекты, даты и связи между ними
# Возвращает: список фактов в структурированном виде

SYSTEM_PROMPT <- "
Ты эксперт по истории Белорецка и горнозаводского Урала.
Твоя задача — извлекать исторические факты из архивных документов.

Документы могут быть: газетные вырезки, рукописи, фотографии, списки,
приказы, письма — всё что связано с историей Белорецкого железоделательного
завода и городa Белорецк (Республика Башкортостан).

Извлеки все факты и верни их ТОЛЬКО в формате JSON массива:
[
  {
    \"subject\": \"имя персоны или название объекта\",
    \"object\": \"имя персоны или название объекта\",
    \"relation_type\": \"основатель|владелец|управляющий|построил|работал|родился|умер|женился|основал|продал|купил\",
    \"date_start\": \"год или дата (ГГГГ или ГГГГ-ММ-ДД)\",
    \"date_end\": null,
    \"description\": \"краткое описание факта одним предложением\",
    \"confidence\": 0.9
  }
]

Если поле неизвестно — пиши null.
Если фактов нет — верни пустой массив [].
ТОЛЬКО JSON, без пояснений.
"

analyze_with_gpt <- function(image_path, ocr_text, api_key) {
  message(paste("🤖 Анализирую через GPT:", basename(image_path)))

  # Кодируем фото в base64 для отправки в API
  image_base64 <- base64enc::base64encode(image_path)
  image_ext    <- tolower(tools::file_ext(image_path))
  mime_type    <- if (image_ext == "jpg" || image_ext == "jpeg") "image/jpeg" else "image/png"

  # Формируем запрос к GPT-4 Vision
  body <- list(
    model = "gpt-4o",
    max_tokens = 2000,
    messages = list(
      list(
        role    = "system",
        content = SYSTEM_PROMPT
      ),
      list(
        role = "user",
        content = list(
          list(
            type = "image_url",
            image_url = list(
              url = paste0("data:", mime_type, ";base64,", image_base64)
            )
          ),
          list(
            type = "text",
            text = if (!is.na(ocr_text) && nchar(ocr_text) > 10) {
              paste("OCR текст документа:\n", ocr_text)
            } else {
              "Проанализируй документ на изображении."
            }
          )
        )
      )
    )
  )

  response <- tryCatch({
    resp <- httr::POST(
      url    = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type"  = "application/json"
      ),
      body   = jsonlite::toJSON(body, auto_unbox = TRUE),
      encode = "raw",
      httr::timeout(60)
    )

    if (httr::status_code(resp) != 200) {
      warning(paste("❌ GPT вернул ошибку:", httr::status_code(resp)))
      return(NULL)
    }

    content <- httr::content(resp, "parsed")
    raw_text <- content$choices[[1]]$message$content
    message("✅ GPT ответил")
    raw_text

  }, error = function(e) {
    warning(paste("❌ Ошибка запроса к GPT:", e$message))
    NULL
  })

  response
}

# ── ШАГ 4: Разбор ответа GPT → таблица фактов ─────────────────────────────────
# Что делает: берёт JSON от GPT и превращает в строки таблицы facts.csv

parse_gpt_response <- function(gpt_text, image_path, source_name = "Краеведческий музей Белорецка") {
  if (is.null(gpt_text) || is.na(gpt_text)) {
    return(data.frame())
  }

  # Извлекаем JSON из ответа (GPT иногда добавляет лишний текст)
  json_match <- regmatches(gpt_text, regexpr("\\[.*\\]", gpt_text, perl = TRUE))

  if (length(json_match) == 0) {
    warning("GPT не вернул JSON в ожидаемом формате")
    return(data.frame())
  }

  facts <- tryCatch({
    parsed <- jsonlite::fromJSON(json_match, simplifyDataFrame = TRUE)
    if (nrow(parsed) == 0) return(data.frame())

    parsed$source      <- source_name
    parsed$image_file  <- basename(image_path)
    parsed$created_at  <- as.character(Sys.Date())
    parsed
  }, error = function(e) {
    warning(paste("Не удалось разобрать JSON от GPT:", e$message))
    data.frame()
  })

  facts
}

# ── ШАГ 5: Создание справочника сущностей ──────────────────────────────────────
# Что делает: собирает всех уникальных людей и объекты из таблицы фактов
# Результат: entities.csv — справочник кто есть кто

build_entities <- function(facts_df) {
  if (nrow(facts_df) == 0) return(data.frame())

  subjects <- data.frame(name = facts_df$subject, stringsAsFactors = FALSE)
  objects  <- data.frame(name = facts_df$object,  stringsAsFactors = FALSE)

  all_names <- unique(rbind(subjects, objects))
  all_names <- all_names[!is.na(all_names$name) & nchar(trimws(all_names$name)) > 0, , drop = FALSE]

  entities <- data.frame(
    id          = seq_len(nrow(all_names)),
    name        = all_names$name,
    type        = NA_character_,   # person / facility / place — заполнить вручную
    description = NA_character_,
    stringsAsFactors = FALSE
  )

  message(paste("✅ Найдено сущностей:", nrow(entities)))
  entities
}

# ── ШАГ 6: Подготовка данных для графа ─────────────────────────────────────────
# Что делает: из таблиц фактов и сущностей делает nodes и links
# для visNetwork — программы рисующей граф в Shiny

build_graph_data <- function(facts_df, entities_df) {
  if (nrow(facts_df) == 0 || nrow(entities_df) == 0) {
    return(list(
      nodes = data.frame(id = integer(), label = character()),
      edges = data.frame(from = integer(), to = integer(), label = character())
    ))
  }

  # Узлы (nodes) — каждая персона или объект
  nodes <- data.frame(
    id    = entities_df$id,
    label = entities_df$name,
    title = entities_df$description,  # подсказка при наведении
    group = ifelse(is.na(entities_df$type), "unknown", entities_df$type),
    stringsAsFactors = FALSE
  )

  # Рёбра (edges) — связи между узлами
  name_to_id <- setNames(entities_df$id, entities_df$name)

  edges <- data.frame(
    from  = name_to_id[facts_df$subject],
    to    = name_to_id[facts_df$object],
    label = facts_df$relation_type,
    title = facts_df$description,
    stringsAsFactors = FALSE
  )
  edges <- edges[!is.na(edges$from) & !is.na(edges$to), ]

  message(paste("📊 Граф:", nrow(nodes), "узлов,", nrow(edges), "связей"))
  list(nodes = nodes, edges = edges)
}

# ── Вспомогательная: список необработанных фото ────────────────────────────────

list_new_images <- function(images_dir) {
  if (!dir.exists(images_dir)) {
    dir.create(images_dir, recursive = TRUE)
    message(paste("📁 Создана папка для фото:", images_dir))
    return(character(0))
  }

  all_images <- list.files(
    images_dir,
    pattern     = "\\.(jpg|jpeg|png|tif|tiff)$",
    ignore.case = TRUE,
    full.names  = TRUE
  )

  message(paste("📷 Найдено фото:", length(all_images)))
  all_images
}
