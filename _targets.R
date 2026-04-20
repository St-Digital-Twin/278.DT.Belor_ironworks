# ===============================================================================
# TARGETS КОНВЕЙЕР - МЕТАЛЛУРГИЧЕСКОЕ ПРЕДПРИЯТИЕ БЕЛАРУСИ
# ===============================================================================
# Автор: Н.Бураков  
# Компания: Цифровой Двойник
# Создано: Апрель 2026 (адаптация к типовому шаблону)
# Описание: Главный файл управления пайплайном обработки данных
# ===============================================================================

# ===============================================================================
# ИНИЦИАЛИЗАЦИЯ И НАСТРОЙКА
# ===============================================================================

# Загрузка основных библиотек
library(targets)
library(tarchetypes)

# Загрузка конфигурации проекта
source("1.R/01.libraries.R")
source("1.R/04.config.R")

# Загрузка пользовательских функций
if(file.exists("1.R/03.functions.R")) {
  source("1.R/03.functions.R")
}

# Загрузка targets определений
source("2.Targets/01.SourceAlltargets.R")

# ===============================================================================
# НАСТРОЙКИ TARGETS
# ===============================================================================

# Настройки параллельных вычислений
tar_option_set(
  packages = c(
    "data.table", "dplyr", "ggplot2", "plotly", "lubridate",
    "readr", "readxl", "qs", "DT", "shiny", "bs4Dash"
  ),
  format = "qs",  # Быстрый формат сохранения
  
  # Параллельные вычисления
  controller = crew::crew_controller_local(
    workers = DATA_PROCESSING_CONFIG$max_cores,
    seconds_idle = 10
  ),
  
  # Настройки хранения
  storage = "worker",
  retrieval = "worker",
  
  # Обработка ошибок
  error = "continue",
  memory = "transient",
  
  # Отладка и логирование
  debug = "metallurgy_project",
  workspaces = c("metallurgy_failed")
)

# ===============================================================================
# ОПРЕДЕЛЕНИЕ ПАЙПЛАЙНА
# ===============================================================================

# Главный список всех targets в проекте
targets_list <- list(
  
  # ============================================================================
  # БЛОК 1: МЕТАДАННЫЕ И КОНФИГУРАЦИЯ
  # ============================================================================
  
  # Загрузка конфигурации проекта
  tar_target(
    name = project_config,
    command = {
      message("📋 Загрузка конфигурации проекта...")
      list(
        production = PRODUCTION_CONFIG,
        kpi = KPI_CONFIG,
        data_processing = DATA_PROCESSING_CONFIG,
        paths = PATHS,
        steel_chemistry = STEEL_CHEMISTRY,
        operation_codes = OPERATION_CODES,
        defect_types = DEFECT_TYPES,
        timestamp = Sys.time()
      )
    }
  ),
  
  # ============================================================================
  # БЛОК 2: ЗАГРУЗКА ИСХОДНЫХ ДАННЫХ
  # ============================================================================
  
  # Загрузка данных из Excel файлов
  tar_target(
    name = raw_agent_list,
    command = {
      message("📊 Загружаю список агентов производства...")
      file_path <- get_data_path("raw", "agent_list.xlsx")
      
      if(file.exists(file_path)) {
        data <- readxl::read_xlsx(file_path) %>%
          data.table::as.data.table()
        message(paste("✅ Загружено", nrow(data), "записей агентов"))
        return(data)
      } else {
        warning("⚠️ Файл agent_list.xlsx не найден, генерирую демо-данные")
        return(generate_demo_agents(n = 50))
      }
    },
    format = "qs"
  ),
  
  tar_target(
    name = raw_nodes,
    command = {
      message("🔗 Загружаю узлы производственной сети...")
      file_path <- get_data_path("raw", "nodes.xlsx")
      
      if(file.exists(file_path)) {
        data <- readxl::read_xlsx(file_path) %>%
          data.table::as.data.table()
        message(paste("✅ Загружено", nrow(data), "узлов"))
        return(data)
      } else {
        warning("⚠️ Файл nodes.xlsx не найден, генерирую демо-данные")
        return(generate_demo_nodes(n = 30))
      }
    },
    format = "qs"
  ),
  
  tar_target(
    name = raw_links,
    command = {
      message("↔️ Загружаю связи производственной сети...")
      file_path <- get_data_path("raw", "links.xlsx")
      
      if(file.exists(file_path)) {
        data <- readxl::read_xlsx(file_path) %>%
          data.table::as.data.table()
        message(paste("✅ Загружено", nrow(data), "связей"))
        return(data)
      } else {
        warning("⚠️ Файл links.xlsx не найден, генерирую демо-данные")  
        return(generate_demo_links(n = 45))
      }
    },
    format = "qs"
  ),
  
  # Загрузка данных показателей (из qs файлов)
  tar_target(
    name = raw_indicators,
    command = {
      message("📈 Загружаю показатели производства...")
      file_path <- get_data_path("processed", "ind.qs")
      
      if(file.exists(file_path)) {
        data <- qs::qread(file_path)
        message(paste("✅ Загружено", nrow(data), "показателей"))
        return(data)
      } else {
        warning("⚠️ Файл ind.qs не найден, генерирую демо-данные")
        return(generate_demo_indicators(n = 100))
      }
    },
    format = "qs"
  ),
  
  tar_target(
    name = raw_data_for_sd,
    command = {
      message("📊 Загружаю данные для системной динамики...")
      file_path <- get_data_path("processed", "data_for_sd.qs")
      
      if(file.exists(file_path)) {
        data <- qs::qread(file_path) %>%
          data.table::as.data.table()
        
        # Фильтрация по годам как в оригинале
        if("year" %in% names(data)) {
          data <- data[year < 2031]
        }
        
        # Переименование колонок как в оригинале
        if("id" %in% names(data) && "pr_fact" %in% names(data)) {
          data.table::setnames(data, c("id", "pr_fact"), c("location", "fact"))
        }
        
        message(paste("✅ Загружено", nrow(data), "записей для системной динамики"))
        return(data)
      } else {
        warning("⚠️ Файл data_for_sd.qs не найден, генерирую демо-данные")
        return(generate_demo_system_dynamics(n = 1000))
      }
    },
    format = "qs"
  ),
  
  # ============================================================================
  # БЛОК 3: ОЧИСТКА И ВАЛИДАЦИЯ ДАННЫХ
  # ============================================================================
  
  tar_target(
    name = clean_agents,
    command = {
      message("🧹 Очистка данных агентов...")
      
      data <- raw_agent_list %>%
        # Стандартизация названий колонок
        janitor::clean_names() %>%
        # Удаление пустых строк
        filter(!is.na(name) | !is.na(id)) %>%
        # Приведение к стандартному формату
        mutate(
          created_at = Sys.time(),
          updated_at = Sys.time()
        )
      
      message(paste("✅ После очистки:", nrow(data), "агентов"))
      return(data)
    },
    format = "qs"
  ),
  
  tar_target(
    name = clean_network,
    command = {
      message("🕸️ Очистка сетевых данных...")
      
      nodes_clean <- raw_nodes %>%
        janitor::clean_names() %>%
        filter(!is.na(id))
      
      links_clean <- raw_links %>%
        janitor::clean_names() %>%
        filter(!is.na(source), !is.na(target))
      
      # Валидация связей (источники и цели должны существовать в узлах)
      valid_node_ids <- nodes_clean$id
      links_clean <- links_clean %>%
        filter(
          source %in% valid_node_ids,
          target %in% valid_node_ids
        )
      
      result <- list(
        nodes = nodes_clean,
        links = links_clean
      )
      
      message(paste("✅ Сеть после очистки:", nrow(nodes_clean), "узлов,", nrow(links_clean), "связей"))
      return(result)
    },
    format = "qs"
  ),
  
  # ============================================================================
  # БЛОК 4: АНАЛИТИЧЕСКИЕ РАСЧЕТЫ
  # ============================================================================
  
  tar_target(
    name = production_kpis,
    command = {
      message("📊 Расчет производственных KPI...")
      
      # Базовые KPI на основе конфигурации
      kpis <- calculate_production_kpis(
        agents_data = clean_agents,
        indicators_data = raw_indicators,
        config = project_config$production
      )
      
      message("✅ KPI рассчитаны")
      return(kpis)
    },
    format = "qs"
  ),
  
  tar_target(
    name = network_analysis,
    command = {
      message("🔗 Анализ производственной сети...")
      
      analysis <- analyze_production_network(
        nodes = clean_network$nodes,
        links = clean_network$links
      )
      
      message("✅ Сетевой анализ выполнен")
      return(analysis)
    },
    format = "qs"
  ),
  
  # ============================================================================
  # БЛОК 5: ПОДГОТОВКА ДАННЫХ ДЛЯ ВИЗУАЛИЗАЦИИ
  # ============================================================================
  
  tar_target(
    name = dashboard_data,
    command = {
      message("📊 Подготовка данных для дашборда...")
      
      dashboard_dataset <- prepare_dashboard_data(
        kpis = production_kpis,
        network = network_analysis,
        agents = clean_agents,
        system_dynamics = raw_data_for_sd
      )
      
      message("✅ Данные для дашборда готовы")
      return(dashboard_dataset)
    },
    format = "qs"
  ),
  
  # ============================================================================
  # БЛОК 6: API DATA ENDPOINTS
  # ============================================================================
  
  tar_target(
    name = api_endpoints_data,
    command = {
      message("🔌 Подготовка данных для API...")
      
      api_data <- list(
        summary = production_kpis$summary,
        agents = clean_agents,
        network_stats = network_analysis$stats,
        latest_indicators = tail(raw_indicators, 100),
        timestamp = Sys.time()
      )
      
      message("✅ Данные для API подготовлены")
      return(api_data)
    },
    format = "qs"
  ),
  
  # ============================================================================
  # БЛОК 7: ОТЧЕТЫ И ЭКСПОРТ
  # ============================================================================
  
  tar_target(
    name = daily_report,
    command = {
      message("📋 Генерация ежедневного отчета...")
      
      report <- generate_daily_production_report(
        date = Sys.Date(),
        kpis = production_kpis,
        network = network_analysis,
        config = project_config
      )
      
      # Сохранение отчета
      report_file <- get_data_path("output", paste0("daily_report_", Sys.Date(), ".html"))
      writeLines(report$html, report_file)
      
      message(paste("✅ Отчет сохранен:", report_file))
      return(report)
    },
    format = "qs"
  ),
  
  # ============================================================================
  # БЛОК 8: ВАЛИДАЦИЯ И КАЧЕСТВО ДАННЫХ
  # ============================================================================
  
  tar_target(
    name = data_quality_checks,
    command = {
      message("🔍 Проверка качества данных...")
      
      checks <- list(
        agents_quality = validate_production_data(clean_agents),
        indicators_quality = validate_production_data(raw_indicators),
        network_integrity = validate_network_integrity(clean_network),
        timestamp = Sys.time()
      )
      
      # Запись результатов проверки
      quality_file <- get_data_path("output", paste0("data_quality_", Sys.Date(), ".json"))
      jsonlite::write_json(checks, quality_file, pretty = TRUE)
      
      message("✅ Проверка качества завершена")
      return(checks)
    },
    format = "qs"
  )
)

# ===============================================================================
# ЭКСПОРТ TARGETS
# ===============================================================================

# Возвращаем список всех targets
targets_list
