# ===============================================================================
# КОНФИГУРАЦИЯ ПРОЕКТА - МЕТАЛЛУРГИЧЕСКОЕ ПРЕДПРИЯТИЕ БЕЛАРУСИ
# ===============================================================================
# Автор: Н.Бураков
# Компания: Цифровой Двойник
# Создано: Апрель 2026 (адаптация к типовому шаблону)
# Описание: Центральные настройки для всех компонентов проекта
# ===============================================================================

# ===============================================================================
# БАЗОВЫЕ НАСТРОЙКИ ПРОЕКТА
# ===============================================================================

# Определение корня проекта
if (require(here, quietly = TRUE)) {
  BASE_PATH <- here::here()
} else {
  BASE_PATH <- getwd()
}

# Основная информация о проекте
PROJECT_INFO <- list(
  name = "Digital Twin Belor Ironworks",
  code = "278.DT.Belor_ironworks", 
  version = "2.0",
  author = "Н.Бураков",
  company = "Цифровой Двойник",
  description = "Цифровой двойник белорусского металлургического предприятия"
)

message(paste("🏭 Инициализация проекта:", PROJECT_INFO$name))
message(paste("📍 Рабочая директория:", BASE_PATH))

# ===============================================================================
# СТРУКТУРА ПУТЕЙ
# ===============================================================================

# Основные пути проекта
PATHS <- list(
  base = BASE_PATH,
  r_scripts = file.path(BASE_PATH, "1.R"),
  targets = file.path(BASE_PATH, "2.Targets"),
  global = file.path(BASE_PATH, "3.Global"),
  data = file.path(BASE_PATH, "4.Data"),
  data_raw = file.path(BASE_PATH, "4.Data", "raw"),
  data_processed = file.path(BASE_PATH, "4.Data", "processed"),
  data_output = file.path(BASE_PATH, "4.Data", "output"),
  shiny = file.path(BASE_PATH, "7.shiny"),
  api = file.path(BASE_PATH, "9.API"),
  docs = file.path(BASE_PATH, "docs"),
  tests = file.path(BASE_PATH, "tests"),
  temp = file.path(BASE_PATH, "temp"),
  logs = file.path(BASE_PATH, "logs")
)

# Создание необходимых папок если они не существуют
create_dirs_if_missing <- function(paths) {
  for(path in paths) {
    if(!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
      message(paste("📁 Создана папка:", path))
    }
  }
}

create_dirs_if_missing(PATHS)

# Вспомогательная функция для построения путей
get_data_path <- function(type = "raw", filename = NULL) {
  base_path <- switch(type,
    "raw" = PATHS$data_raw,
    "processed" = PATHS$data_processed,
    "output" = PATHS$data_output,
    PATHS$data
  )
  
  if(is.null(filename)) {
    return(base_path)
  } else {
    return(file.path(base_path, filename))
  }
}

# ===============================================================================
# ПРОИЗВОДСТВЕННЫЕ НАСТРОЙКИ
# ===============================================================================

# Конфигурация металлургического производства
PRODUCTION_CONFIG <- list(
  # Основные параметры производства
  plant_name = "Белорусский металлургический комбинат",
  plant_code = "BMK_001",
  
  # Производственные мощности
  furnaces_count = 4,
  daily_capacity_tons = 2500,
  max_monthly_capacity = 75000,
  
  # Качественные параметры
  steel_grades = c("St3", "St20", "St45", "40X", "65G"),
  quality_threshold = 0.95,
  defect_rate_max = 0.05,
  
  # Энергетические показатели
  power_consumption_kwh_per_ton = 450,
  gas_consumption_m3_per_ton = 180,
  
  # Временные параметры
  shift_hours = 8,
  shifts_per_day = 3,
  working_days_per_month = 22,
  
  # Температурные режимы
  melting_temperature = 1650,    # Celsius
  casting_temperature = 1550,
  cooling_temperature = 800
)

# KPI показатели
KPI_CONFIG <- list(
  # Операционные показатели
  oee_target = 0.85,             # Overall Equipment Effectiveness
  yield_target = 0.92,           # Выход годного
  capacity_utilization_target = 0.88,
  
  # Качественные показатели
  first_pass_yield = 0.95,
  customer_complaints_max = 10,  # в месяц
  
  # Финансовые показатели
  cost_per_ton_target = 450,     # USD
  energy_cost_share_max = 0.25,
  
  # Экологические показатели
  co2_emissions_max = 2.1,       # тонн CO2 на тонну стали
  water_consumption_max = 3.5,   # м3 на тонну стали
  
  # Безопасность
  incidents_max = 0,             # целевое значение инцидентов
  near_miss_reports_min = 20     # минимум сообщений о рисках
)

# ===============================================================================
# НАСТРОЙКИ ПОДКЛЮЧЕНИЙ К ДАННЫМ
# ===============================================================================

# Настройки базы данных
DATABASE_CONFIG <- list(
  # Основная производственная БД
  production = list(
    type = "postgresql",
    host = Sys.getenv("PROD_DB_HOST", "localhost"),
    port = as.numeric(Sys.getenv("PROD_DB_PORT", "5432")),
    dbname = Sys.getenv("PROD_DB_NAME", "belor_ironworks"),
    user = Sys.getenv("PROD_DB_USER", "postgres"),
    password = Sys.getenv("PROD_DB_PASS", ""),
    schema = "production"
  ),
  
  # Аналитическая БД
  analytics = list(
    type = "postgresql", 
    host = Sys.getenv("ANALYTICS_DB_HOST", "localhost"),
    port = as.numeric(Sys.getenv("ANALYTICS_DB_PORT", "5432")),
    dbname = Sys.getenv("ANALYTICS_DB_NAME", "analytics"),
    user = Sys.getenv("ANALYTICS_DB_USER", "analyst"),
    password = Sys.getenv("ANALYTICS_DB_PASS", ""),
    schema = "dwh"
  ),
  
  # Локальная SQLite для разработки
  local = list(
    type = "sqlite",
    path = get_data_path("processed", "local_data.sqlite")
  )
)

# Внешние API и интеграции
EXTERNAL_APIS <- list(
  # ERP система предприятия
  erp = list(
    base_url = Sys.getenv("ERP_API_URL", "https://erp.company.by/api/v1"),
    api_key = Sys.getenv("ERP_API_KEY", ""),
    timeout_seconds = 30
  ),
  
  # Система управления качеством
  qms = list(
    base_url = Sys.getenv("QMS_API_URL", "https://qms.company.by/api"),
    username = Sys.getenv("QMS_USER", ""),
    password = Sys.getenv("QMS_PASS", ""),
    timeout_seconds = 15
  ),
  
  # Погодный сервис (влияет на потребление энергии)
  weather = list(
    api_key = Sys.getenv("WEATHER_API_KEY", ""),
    base_url = "https://api.openweathermap.org/data/2.5",
    location = "Minsk,BY"
  )
)

# ===============================================================================
# НАСТРОЙКИ SHINY ПРИЛОЖЕНИЯ
# ===============================================================================

SHINY_CONFIG <- list(
  # Основные параметры
  title = "Цифровой двойник металлургического предприятия",
  host = Sys.getenv("SHINY_HOST", "127.0.0.1"),
  port = as.numeric(Sys.getenv("SHINY_PORT", "3838")),
  
  # UI настройки
  theme = "blue",
  skin = "blue",
  primary_color = "#3498db",
  secondary_color = "#2c3e50", 
  accent_color = "#e74c3c",
  
  # Производительность
  max_file_size = 100 * 1024^2,  # 100MB
  session_timeout = 3600,        # 1 час
  
  # Обновления данных
  refresh_interval_sec = 30,
  cache_duration_min = 5,
  
  # Уведомления
  enable_notifications = TRUE,
  notification_duration = 5000,   # миллисекунды
  
  # Экспорт данных
  export_formats = c("csv", "xlsx", "pdf"),
  max_export_rows = 50000
)

# ===============================================================================
# НАСТРОЙКИ API
# ===============================================================================

API_CONFIG <- list(
  # Основные параметры
  host = Sys.getenv("API_HOST", "127.0.0.1"),
  port = as.numeric(Sys.getenv("API_PORT", "8000")),
  title = "Belor Ironworks API",
  version = "1.0.0",
  
  # Безопасность
  enable_cors = TRUE,
  api_keys_enabled = as.logical(Sys.getenv("API_KEYS_ENABLED", "FALSE")),
  rate_limit_per_minute = 100,
  
  # Логирование
  log_requests = TRUE,
  log_file = file.path(PATHS$logs, "api.log"),
  log_level = Sys.getenv("LOG_LEVEL", "INFO"),
  
  # Производительность
  max_request_size = "50mb",
  timeout_seconds = 60,
  
  # Документация
  enable_swagger = TRUE,
  swagger_url = "/docs"
)

# ===============================================================================
# НАСТРОЙКИ ОБРАБОТКИ ДАННЫХ
# ===============================================================================

DATA_PROCESSING_CONFIG <- list(
  # Общие настройки
  encoding = "UTF-8",
  decimal_separator = ".",
  date_format = "%Y-%m-%d",
  datetime_format = "%Y-%m-%d %H:%M:%S",
  
  # Валидация данных
  max_missing_rate = 0.1,        # 10% пропусков максимум
  outlier_threshold = 3,         # Z-score для выбросов
  
  # Временные рамки данных
  min_date = as.Date("2020-01-01"),
  max_date = Sys.Date() + 365,   # год в будущее для планов
  
  # Производственные ограничения
  min_temperature = 0,           # Celsius
  max_temperature = 2000,
  min_pressure = 0,              # bar
  max_pressure = 50,
  min_tonnage = 0,
  max_daily_tonnage = 3000,
  
  # Настройки параллельной обработки
  parallel_processing = TRUE,
  max_cores = parallel::detectCores() - 1,
  chunk_size = 10000,            # строк в chunk'е
  
  # Кэширование
  enable_cache = TRUE,
  cache_duration_hours = 4
)

# ===============================================================================
# СПЕЦИФИЧЕСКИЕ НАСТРОЙКИ МЕТАЛЛУРГИИ
# ===============================================================================

# Химический состав сталей
STEEL_CHEMISTRY <- list(
  St3 = list(C = c(0.14, 0.22), Mn = c(0.40, 0.65), Si = c(0.15, 0.30)),
  St20 = list(C = c(0.17, 0.24), Mn = c(0.35, 0.65), Si = c(0.17, 0.37)),
  St45 = list(C = c(0.42, 0.50), Mn = c(0.50, 0.80), Si = c(0.17, 0.37)),
  "40X" = list(C = c(0.36, 0.44), Mn = c(0.50, 0.80), Cr = c(0.80, 1.10)),
  "65G" = list(C = c(0.62, 0.70), Mn = c(0.90, 1.20), Si = c(0.17, 0.37))
)

# Коды технологических операций
OPERATION_CODES <- list(
  melting = "OP001",
  refining = "OP002", 
  casting = "OP003",
  rolling = "OP004",
  heat_treatment = "OP005",
  quality_control = "OP006",
  finishing = "OP007",
  packaging = "OP008"
)

# Классификация дефектов
DEFECT_TYPES <- list(
  surface = c("scale", "scratches", "dents", "corrosion"),
  internal = c("porosity", "inclusions", "cracks", "segregation"),
  dimensional = c("thickness", "width", "length", "shape"),
  chemical = c("carbon_content", "alloy_content", "sulfur", "phosphorus")
)

# ===============================================================================
# ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
# ===============================================================================

#' Получение конфигурации для определенной среды
get_config <- function(env = Sys.getenv("R_CONFIG_ACTIVE", "development")) {
  config_file <- file.path(PATHS$global, "config.yaml")
  
  if(file.exists(config_file) && require(config, quietly = TRUE)) {
    return(config::get(config = env))
  } else {
    warning("Файл config.yaml не найден, используются значения по умолчанию")
    return(list())
  }
}

#' Проверка соединения с базой данных
test_db_connection <- function(config_name = "local") {
  db_config <- DATABASE_CONFIG[[config_name]]
  
  tryCatch({
    if(db_config$type == "sqlite") {
      con <- DBI::dbConnect(RSQLite::SQLite(), db_config$path)
    } else if(db_config$type == "postgresql") {
      con <- DBI::dbConnect(
        RPostgreSQL::PostgreSQL(),
        host = db_config$host,
        port = db_config$port,
        dbname = db_config$dbname,
        user = db_config$user,
        password = db_config$password
      )
    }
    
    DBI::dbDisconnect(con)
    message(paste("✅ Соединение с БД", config_name, "успешно"))
    return(TRUE)
    
  }, error = function(e) {
    warning(paste("❌ Ошибка соединения с БД", config_name, ":", e$message))
    return(FALSE)
  })
}

#' Проверка качества данных
validate_production_data <- function(data) {
  issues <- c()
  
  # Проверка на пропуски
  missing_rate <- sum(is.na(data)) / length(data)
  if(missing_rate > DATA_PROCESSING_CONFIG$max_missing_rate) {
    issues <- c(issues, paste("Слишком много пропусков:", round(missing_rate * 100, 1), "%"))
  }
  
  # Проверка временных меток
  if("timestamp" %in% names(data)) {
    if(any(data$timestamp < DATA_PROCESSING_CONFIG$min_date, na.rm = TRUE)) {
      issues <- c(issues, "Данные содержат даты из прошлого")
    }
  }
  
  # Проверка производственных показателей
  if("tonnage" %in% names(data)) {
    if(any(data$tonnage > DATA_PROCESSING_CONFIG$max_daily_tonnage, na.rm = TRUE)) {
      issues <- c(issues, "Превышена максимальная суточная производительность")
    }
  }
  
  if(length(issues) == 0) {
    message("✅ Данные прошли валидацию")
    return(TRUE)
  } else {
    warning(paste("⚠️ Проблемы с данными:", paste(issues, collapse = "; ")))
    return(FALSE)
  }
}

#' Форматирование числовых значений для отображения
format_number <- function(x, type = "auto") {
  if(is.na(x) || is.null(x)) return("—")
  
  switch(type,
    "tonnage" = paste(formatC(x, format = "f", digits = 1), "т"),
    "temperature" = paste(formatC(x, format = "f", digits = 0), "°C"),
    "percentage" = paste(formatC(x * 100, format = "f", digits = 1), "%"),
    "currency" = paste("$", formatC(x, format = "f", digits = 2)),
    "auto" = {
      if(x >= 1000000) {
        paste0(formatC(x/1000000, format = "f", digits = 1), "М")
      } else if(x >= 1000) {
        paste0(formatC(x/1000, format = "f", digits = 1), "К")
      } else {
        formatC(x, format = "f", digits = 1)
      }
    }
  )
}

# ===============================================================================
# ИНИЦИАЛИЗАЦИЯ И ПРОВЕРКИ
# ===============================================================================

# Проверка критических компонентов
message("🔍 Проверка системных компонентов...")

# Проверка доступности R пакетов
critical_packages <- c("targets", "shiny", "data.table", "ggplot2")
missing_packages <- critical_packages[!critical_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  warning(paste("⚠️ Отсутствуют критические пакеты:", paste(missing_packages, collapse = ", ")))
  message("Установите их командой: install.packages(c(", paste0('"', paste(missing_packages, collapse = '", "'), '"'), "))")
}

# Проверка переменных окружения
required_env_vars <- c()  # Добавить при необходимости
missing_env_vars <- required_env_vars[Sys.getenv(required_env_vars) == ""]

if(length(missing_env_vars) > 0) {
  warning(paste("⚠️ Не установлены переменные окружения:", paste(missing_env_vars, collapse = ", ")))
}

# Финальное сообщение
message("🚀 Конфигурация проекта загружена успешно!")
message(paste("🏭 Готов к работе с данными предприятия:", PRODUCTION_CONFIG$plant_name))
message("📊 Используйте функции get_config(), get_data_path() для работы с настройками")
