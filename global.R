# Автор: Н.Бураков
# Компания: Цифровой Двойник 
# Дата: Сентябрь 2023
# 1. Библиотеки                                         ####
source("1.R/1.SourceAll.R")
# 2. Справочники                                        #### 

# locations <- qread(paste0(path_221,"/2.Masterdata/1.Model_city/locations")) %>% 
#   .[str_count(oktmo) <= 8]

ind <- as.data.table(read_all_prosha(sheet = c("indicator"))$dwh_prod$indicator)


project_type <- data.table(name = c("library","parks","hospital","school","shop"), 
                           name_RU = c("Строительство бибиотеки","Создание парка", "Строительство больницы","Строительство школы","Создание торгового объекта"),
                           from = c("C2698,C182,C2696","C1115,C1188","C2699,C165,C2697","C2701,C2700","C2703,C2702"),
                           to = c("C1013","C732","C743","C741","C746"),
                           max = c("500,50,100","300,150","2500,400,10000","1000,800","1500,15000"))

index_fact <- data.table(location = c("19701000","65701000","42701000","44701000","50701000","60737000"), 
                         index = c(56.68, 56.61545,56.55545,38.19455,54.80727,49.24818),
                         mean_index = c(52.57, 54.50455,52.79818,46.89,54.50455,53.09364),
                         claster = c("С достатком","Космополиты","Региональные центры", "Холодные","Космополиты","Спутники космополитов"))


button_style <- 
  "color: #1A1814; 
  background-color:#FFA500;
  border: 1px solid #FFA500;
  border-radius: 6px;
  box-shadow: rgba(0, 0, 0, 0.1) 1px 2px 4px;
  display: inline-block;
  outline: 0;
  text-align: center;
  text-rendering: geometricprecision;
  text-transform: none;
  user-select: none;
  -webkit-user-select: none;
  touch-action: manipulation;
  vertical-align: middle;
  cursor: pointer;
  font-size: 16px; 
  font-weight: 600; 
  font-family: Panton;"
# 3. Данные                                             ####
id_to_c_code <- qread("3.Data/id_to_c_code")

data_for_sd <- qread("3.Data/data_for_sd") %>% 
  .[year < 2031] %>% 
  setnames(c("id","pr_fact"),c("location","fact")) %>% 
  .[,-c("source")]


result_diagram_json <- jsonlite::fromJSON("3.Data/diagram/diagram.json")


# 4. Функции                                            ####
bs4ValueBox_NB <- function (value, subtitle, icon = NULL, color = NULL, width = 3, 
                            href = NULL, footer = NULL, gradient = FALSE, elevation = NULL) 
{
  if (!is.null(icon)) {
    tagAssert(icon, type = "i")
  }
  if (is.null(color) && gradient) {
    stop("color cannot be NULL when gradient is TRUE. \n         fill cannot be TRUE when color is NULL.")
  }
  if (!is.null(width)) {
    stopifnot(is.numeric(width))
    stopifnot(width <= 12)
    stopifnot(width >= 0)
  }
  if (!is.null(elevation)) {
    stopifnot(is.numeric(elevation))
    stopifnot(elevation < 6)
    stopifnot(elevation >= 0)
  }
  if (!is.null(footer) & !is.null(href)) {
    stop("Choose either href or footer.")
  }
  valueBoxCl <- "small-box"
  if (!is.null(color)) {
    validateStatusPlus(color)
    if (gradient) {
      valueBoxCl <- paste0(valueBoxCl, " bg-gradient-", 
                           color)
    }
    else {
      valueBoxCl <- paste0(valueBoxCl, " bg-", color)
    }
  }
  if (!is.null(elevation)) 
    valueBoxCl <- paste0(valueBoxCl, " elevation-", elevation)
  innerTag <- shiny::tags$div(class = "inner", value, shiny::tags$p(class = "small-box-subtitle", 
                                                                    subtitle))
  iconTag <- if (!is.null(icon)) {
    shiny::tags$div(class = "icon", icon)
  }
  else {
    NULL
  }
  footerTag <- if (!is.null(footer)) {
    shiny::tags$div(class = "small-box-footer", footer)
  }
  else {
    if (!is.null(href)) {
      shiny::tags$a(href = href, target = "_blank", class = "small-box-footer", 
                    "More info", shiny::icon("circle-arrow-right"))
    }
    else {
      shiny::tags$div(class = "small-box-footer", style = "height: 0px;")
    }
  }
  valueBoxTag <- shiny::tags$div(class = valueBoxCl)
  valueBoxTag <- shiny::tagAppendChildren(valueBoxTag, innerTag, 
                                          iconTag, footerTag)
  shiny::tags$div(class = if (!is.null(width)) 
    paste0("col-sm-", width), valueBoxTag)
}
js <- "
Shiny.addCustomMessageHandler('anim',
 function(x){

    var $box = $('#' + x.id + ' div.small-box');
    var value = x.value;

    var $s = $box.find('div.inner h3');
    var o = {value: 0};
    $.Animation( o, {
        value: value
      }, {
        duration: 1200
      }).progress(function(e) {
          $s.text((e.tweens[0].now).toFixed(1) + ' баллов');
    });

  }
);"

# 5. Запуск                                             #### 
source("ui.R", encoding = "UTF-8")
source("server.R", encoding = "UTF-8")
shinyApp(ui, server)


