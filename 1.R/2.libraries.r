`%!in%` <- Negate(`%in%`)
libs <- c("tarchetypes", "caret", 'Dtwin', 'expm', 'rsdmx',
          "qs", "readxl", "fst", "dplyr", "stringr", 'stringi',
          "archive","factoextra","keras","EMD","callr",
          "shiny", "bs4Dash","shinydashboard","bslib", "shinythemes", "sysfonts",
          "shinydashboardPlus","shinyWidgets",
          "devtools","readr","lpSolve","DT",
          "jsonlite", "readxl", "tidyr", "tibble", "tidyselect",
          "emphatic", # Расцветка таблиц в консоли
          "devtools","rstudioapi",
          "foreach","shinyFeedback","googlesheets4","googledrive",
          "R.utils", "future.callr", "future",
          "RSelenium", "netstat", # Необходимо для парсинга сайтов
          "lubridate", "slider", # Работа со временем
          "purrr","visNetwork", "ggrepel",
          "plotly", "ggplot2", "openxlsx",
          "parallel",
          "brms","reticulate", 'magick', 'cowplot', "data.table", "pracma","targets",
          "geojsonsf","gepaf","fuzzyjoin","fresh","mapdeck","shinycssloaders",
          "rhandsontable","networkD3","httr2","shinyBS","bsplus","psycModel","pagedown","mailR", 'stripe.pay')

# Включение библиотек
suppressMessages(lapply(libs, require, character.only = TRUE))




