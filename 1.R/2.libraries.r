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


# Проверка наличия и установкана библиотек ЦД и devtools

tryCatch({
  if("devtools" %!in% installed.packages()[,"Package"]) install.packages("devtools")
  if("Dtwin"    %!in% installed.packages()[,"Package"]) devtools::install_github("St-Digital-Twin/133.DtwinLib", auth_token = "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")
  if("DTwinDW"  %!in% installed.packages()[,"Package"]) devtools::install_github("St-Digital-Twin/DTwinDW", auth_token = "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")
  if("stripe.pay"  %!in% installed.packages()[,"Package"]) devtools::install_github("St-Digital-Twin/218.Payment", auth_token = "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")

  
  # Обновление библиотек из гитхаба при наличии
  update_github_pkgs <- function() {
    
    # check/load necessary packages
    # devtools package
    if (!("package:devtools" %in% search())) {
      tryCatch(require(devtools), error = function(x) {warning(x); cat("Cannot load devtools package \n")})
      on.exit(detach("package:devtools", unload = TRUE))
    }
    
    pkgs <- installed.packages(fields = "RemoteType")
    github_pkgs <- pkgs[pkgs[, "RemoteType"] %in% "github", "Package"]
    
    print(github_pkgs)
    lapply(github_pkgs, function(pac) {
      message("Updating ", pac, " from GitHub...")
      
      repo = packageDescription(pac, fields = "GithubRepo")
      username = packageDescription(pac, fields = "GithubUsername")
      
      install_github(repo = paste0(username, "/", repo), auth_token = "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")
    })
  }
  
  # update_github_pkgs()
  
}, error=function(cond) {
  message('Не удалось установить библиотеку Dtwin.\nПопробуйте установить её вручную по инструкции по ссылке: https://docs.google.com/document/d/1VHMOmU0sHxwrQ0wb74inNxWKcPPc3W6SNXlaTYdwY3A/edit?usp=sharing')
})


# Инсталляция отсутствующих не гитхаб библиотек
new.packages <- libs[!(libs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Включение библиотек
suppressMessages(lapply(libs, require, character.only = TRUE))




