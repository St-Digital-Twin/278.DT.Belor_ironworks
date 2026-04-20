# ===============================================================================
# УПРАВЛЕНИЕ БИБЛИОТЕКАМИ - МЕТАЛЛУРГИЧЕСКИЙ ПРОЕКТ
# ===============================================================================
# Автор: Н.Бураков
# Компания: Цифровой Двойник  
# Обновлено: Апрель 2026 (адаптация к типовому шаблону)
# ===============================================================================

# Утилитарные операторы
`%!in%` <- Negate(`%in%`)

# ===============================================================================
# СПИСОК НЕОБХОДИМЫХ БИБЛИОТЕК
# ===============================================================================

# Основные библиотеки для работы с данными
data_libs <- c(
  "data.table",        # Быстрая работа с большими данными
  "dplyr",            # Манипуляции с данными
  "tidyr",            # Приведение данных в порядок
  "tibble",           # Современные data.frame
  "purrr",            # Функциональное программирование
  "stringr",          # Работа со строками
  "stringi",          # Расширенная работа со строками
  "lubridate",        # Работа с датами и временем
  "jsonlite"          # Работа с JSON
)

# Библиотеки для чтения/записи данных
io_libs <- c(
  "readr",            # Быстрое чтение CSV
  "readxl",           # Чтение Excel файлов
  "openxlsx",         # Запись Excel файлов
  "qs",               # Быстрое сохранение R объектов
  "fst",              # Быстрое сохранение data.frame
  "httr",             # HTTP запросы
  "httr2",            # HTTP запросы (новая версия)
  "DBI",              # Интерфейс к базам данных
  "RSQLite"           # SQLite база данных
)

# Targets и пайплайны
pipeline_libs <- c(
  "targets",          # Управление пайплайнами
  "tarchetypes",      # Расширенные типы targets
  "future",           # Асинхронные вычисления
  "future.callr",     # Параллельные процессы
  "callr",            # Внешние R процессы
  "parallel"          # Параллельные вычисления
)

# Shiny и веб-интерфейс
shiny_libs <- c(
  "shiny",            # Основной Shiny фреймворк
  "bs4Dash",          # Bootstrap 4 дашборды
  "shinydashboard",   # Классические дашборды
  "shinydashboardPlus", # Расширенные дашборды
  "shinyWidgets",     # Дополнительные виджеты
  "shinyFeedback",    # Обратная связь пользователю
  "shinycssloaders",  # Индикаторы загрузки
  "shinyBS",          # Bootstrap компоненты
  "bsplus",           # Дополнительные Bootstrap элементы
  "bslib",            # Современные Bootstrap темы
  "shinythemes",      # Темы для Shiny
  "fresh"             # Кастомизация интерфейса
)

# Визуализация данных
viz_libs <- c(
  "ggplot2",          # Грамматика графиков
  "plotly",           # Интерактивные графики
  "visNetwork",       # Сетевые графики
  "networkD3",        # D3 сетевые визуализации
  "leaflet",          # Интерактивные карты
  "mapdeck",          # 3D карты
  "DT",               # Интерактивные таблицы
  "rhandsontable",    # Редактируемые таблицы
  "ggrepel",          # Улучшенные подписи на графиках
  "cowplot",          # Композиция графиков
  "magick"            # Обработка изображений
)

# API и веб-сервисы
api_libs <- c(
  "plumber",          # REST API
  "swagger",          # API документация
  "httpuv",           # HTTP сервер
  "mime"              # MIME типы
)

# Статистика и машинное обучение
stats_libs <- c(
  "caret",            # Классификация и регрессия
  "brms",             # Байесовские модели
  "pracma",           # Практическая математика
  "factoextra",       # Факторный анализ
  "EMD",              # Эмпирическая модовая декомпозиция
  "slider"            # Скользящие окна
)

# Специализированные библиотеки
special_libs <- c(
  "reticulate",       # Интеграция с Python
  "keras",            # Глубокое обучение
  "geojsonsf",        # GeoJSON для простых features
  "gepaf",            # Google Encoded Polyline Algorithm
  "fuzzyjoin",        # Нечеткое соединение таблиц
  "lpSolve",          # Линейное программирование
  "emphatic",         # Расцветка таблиц в консоли
  "archive"           # Работа с архивами
)

# Системные и вспомогательные
system_libs <- c(
  "devtools",         # Инструменты разработчика
  "rstudioapi",       # API RStudio
  "R.utils",          # Системные утилиты
  "config",           # Управление конфигурацией
  "yaml",             # Работа с YAML файлами
  "here",             # Определение корня проекта
  "usethis",          # Автоматизация разработки
  "testthat"          # Unit тестирование
)

# Интеграции и внешние сервисы
integration_libs <- c(
  "googlesheets4",    # Google Sheets API
  "googledrive",      # Google Drive API
  "RSelenium",        # Автоматизация браузера
  "netstat",          # Сетевая диагностика
  "mailR",            # Отправка email
  "pagedown",         # R Markdown в PDF
  "psycModel",        # Психометрические модели
  "stripe.pay",       # Интеграция с платежами
  "sysfonts"          # Системные шрифты
)

# Объединение всех библиотек
all_libs <- c(
  data_libs,
  io_libs, 
  pipeline_libs,
  shiny_libs,
  viz_libs,
  api_libs,
  stats_libs,
  special_libs,
  system_libs,
  integration_libs
)

# Удаление дубликатов
all_libs <- unique(all_libs)

# ===============================================================================
# ФУНКЦИЯ УСТАНОВКИ БИБЛИОТЕК
# ===============================================================================

install_if_missing <- function(packages) {
  missing <- packages[!packages %in% installed.packages()[,"Package"]]
  
  if(length(missing) > 0) {
    message("📦 Устанавливаю недостающие пакеты: ", paste(missing, collapse = ", "))
    
    # Попытка установки из CRAN
    tryCatch({
      install.packages(missing, dependencies = TRUE, repos = "https://cloud.r-project.org/")
      message("✅ Установка завершена")
    }, error = function(e) {
      warning("⚠️ Ошибка установки некоторых пакетов: ", e$message)
    })
  } else {
    message("✅ Все необходимые пакеты уже установлены")
  }
}

# ===============================================================================
# УСТАНОВКА И ЗАГРУЗКА БИБЛИОТЕК
# ===============================================================================

# Установка недостающих пакетов
install_if_missing(all_libs)

# Загрузка библиотек с подавлением сообщений
message("📚 Загружаю библиотеки...")

successfully_loaded <- c()
failed_to_load <- c()

for(lib in all_libs) {
  result <- suppressMessages(suppressWarnings(
    require(lib, character.only = TRUE, quietly = TRUE)
  ))
  
  if(result) {
    successfully_loaded <- c(successfully_loaded, lib)
  } else {
    failed_to_load <- c(failed_to_load, lib)
  }
}

# Отчет о загрузке
message(paste("✅ Успешно загружено:", length(successfully_loaded), "библиотек"))

if(length(failed_to_load) > 0) {
  warning("⚠️ Не удалось загрузить: ", paste(failed_to_load, collapse = ", "))
  message("Попробуйте переустановить проблемные пакеты")
}

# ===============================================================================
# НАСТРОЙКИ И ОПЦИИ
# ===============================================================================

# Настройки для работы с данными
options(
  scipen = 999,                    # Отключить научную нотацию
  stringsAsFactors = FALSE,        # Строки как строки, не факторы
  digits = 3,                      # Количество отображаемых цифр
  max.print = 1000,               # Максимум строк для печати
  width = 120                      # Ширина консоли
)

# Настройки для параллельных вычислений
if("future" %in% successfully_loaded) {
  future::plan(future::multisession, workers = parallel::detectCores() - 1)
}

# Настройки Shiny
if("shiny" %in% successfully_loaded) {
  options(shiny.maxRequestSize = 100*1024^2)  # 100MB для загрузки файлов
}

# Настройка кэширования для targets
if("targets" %in% successfully_loaded) {
  Sys.setenv(TAR_PROJECT = basename(getwd()))
}

message("🚀 Инициализация библиотек завершена!")
message("📊 Проект готов к работе с металлургическими данными")




