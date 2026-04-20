server <- function(input, output, session) {
# Реактивные переменные                       ####
  ind_vis = reactiveVal("C712")
  
  need_project = reactiveVal("Создание парка")
  need_project_from = reactiveVal(project_type[name_RU == "Создание парка"]$from)
# Логотип                                     ####
  output$logo <- renderImage({
    list(src = '2.Pic/19.png',
         width = 250,
         vspace = 5,
         alt = "www.dtwin.ru")
  }, deleteFile = FALSE)
# Динамично изменяемый интерфейс              ####
# Выбор параметров проекта                    ####  
  output$ui_project <- renderUI({
      fluidRow(
        column(12,
               selectizeInput(
                 'type_of_project', label = "Тип инвестиционного проекта", choices = project_type$name_RU,
                 options = list(create = TRUE), selected = "Создание парка"),
               actionButton("reshet", "Рассчитать",status = "warning",
                            style = button_style)))
  })
# Страница цифровой двойник города            ####
  output$ui_dt <- renderUI({
    fluidPage(
      fluidRow(
        bs4TabCard(width = 12,solidHeader = FALSE,title = "",
                   collapsible = TRUE,background = 'white', 
                   tabPanel(title = "Укрупненная модель города",
                            fluidRow(
                              column(width = 8,visNetworkOutput("visnetwork_modal", height = "650px") %>% withSpinner(type = getOption("spinner.type", default = 6),color = "#FFA500")),
                              column(width = 4,plotlyOutput("map_plot"),htmlOutput("small_city"))
                            )),
                   tabPanel(title = "Агентное взаимодействие",
                            column(width = 6 ,htmlOutput("agent_anal")),
                            plotlyOutput("plotsankey_agents",width = "90%", height = "800px") %>% withSpinner(type = getOption("spinner.type", default = 6),color = "#FFA500"))
        )
      ))
  })
# Страница оценки влияния проекта             ####
  output$ui_pi <- renderUI({
    fluidPage(
      fluidRow(
        column(width = 4,tagAppendAttributes(bs4ValueBox_NB(h3(""),width = 12,
                                                            icon = icon("person"),
                                                            h5('Индекс ВЭБ.РФ'),
                                                            color = "white"),id = "invest11") %>%
                 bs_embed_tooltip(title = "Фактический Индекс города с сайта Индекс ВЭБ.РФ")),
        column(width = 4, tagAppendAttributes(bs4ValueBox_NB(h3(""),width = 12,
                                                             icon = icon("money-bill"),
                                                             h5('Вклад проекта к росту Индекса ВЭБ.РФ'),
                                                             color = "warning"),id = "invest12") %>%
                 bs_embed_tooltip(title = "Прирост к Индексу ВЭБ.РФ города благодаря реализации инвест проекта")),
        column(width = 4, tagAppendAttributes(bs4ValueBox_NB(h3(""),width = 12,
                                                                icon = icon("chart-simple"),
                                                                h5(paste0('Медианный по кластеру Индекс ВЭБ.РФ')),
                                                                color = "gray-dark"),id = "invest13") %>%
                 bs_embed_tooltip(title = "Медианный по кластеру Индекс города с сайта Индекс ВЭБ.РФ")),
        bs4Dash::bs4TabCard(width = 12,title = "",background = 'white',solidHeader = FALSE)
      ))
  })
  
  
# Текст для фактического индекса              ####
  observeEvent({input$type_of_project},{
    if(!is.null(need_project_from())){
      poten_pay_costs <- index_fact[location == "19701000"]$index
      session$sendCustomMessage("anim", list(id = "invest11", value = round(poten_pay_costs,2)))
    }
  })
  
# Текст для фактического индекса              ####
  observeEvent({input$type_of_project},{
    if(!is.null(need_project_from())){
      poten_pay_costs <- index_fact[location == "19701000"]$index
      session$sendCustomMessage("anim", list(id = "invest12", value = round(poten_pay_costs,2)))
    }
  })
  
# Текст для фактического индекса              ####
  observeEvent({input$type_of_project},{
    if(!is.null(need_project_from())){
      poten_pay_costs <- index_fact[location == "19701000"]$index
      session$sendCustomMessage("anim", list(id = "invest13", value = round(poten_pay_costs,2)))
    }
  })
# Сохрание проекта                            ####
  observeEvent({input$enter_name},{
    projects_rea(qread(paste0(path_221, "/1.Data/projects")))
    to_save <- rbindlist(list(projects_rea(),
                              cbind(data.table(id = last(projects_rea()$id) + 1,
                                               name = input$save_name_pr,
                                               location = need_loc_code(),
                                               type = project_type[name_RU == input$type_of_project]$name,
                                               effect = index_fact[location == need_loc_code()]$index*index_add()/100),impact_data() %>% 
                                      pivot_wider(names_from  = indicator, values_from = impact) %>% 
                                      as.data.table())
    ),use.names = T, fill = T)
    projects_rea(to_save)
    qsave(to_save,paste0(path_221, "/1.Data/projects"))
    resetLoadingButton("enter_name")
    sendSweetAlert(
      session = session,
      title =  "Сохранено",
      btn_labels = "Хорошо",
      btn_colors = "#FFA500",
      text = tags$div(h6(paste0("Ваш проект успешно сохранен и будет отображаться во вкладке Приоритезация проектов"))),
      type = "success",
      width = "20%"
    )
    removeModal(session = getDefaultReactiveDomain())
    
  })
# Текст для медианного по кластеру индекса    ####
  observeEvent({input$city_choosen},{
    if(!is.null(need_loc_code())){
      poten_pay_costs <- index_fact[location == need_loc_code()]$mean_index
      session$sendCustomMessage("anim", list(id = "invest13", value = round(poten_pay_costs,2)))
    }
  })
# Обновление реактивов проекта                ####
  observeEvent({input$type_of_project},{
    if(!is.null(input$type_of_project)){
      if(input$type_of_project != ""){
        need_project(input$type_of_project)
        need_project_from(project_type[name_RU == input$type_of_project]$from)
      }
    }
  })
# Обновление идикатора по клику на схему      ####
  observeEvent({input$click},{
    ind_vis(id_to_c_code[id == input$click]$code)
  })
# Sankey Агетов люди - объекты                ####
  output$plotsankey_agents  <- renderPlotly({
    if(TRUE){
      
      nodes0 <- read.xlsx("agent_list.xlsx",sheet = "nodes")
      
      ind2 <- data.table(code = c(13, 14, 15, 16, 17, 18, 19, 20, 21),
                         color = c("#D75A00","#6740B4","#785549","#D0A200","#50AE55","#D0A200","#F2453D","#2C98F0","#4054B3"))
      
      links <- read.xlsx("agent_list.xlsx",sheet = "links",sep.names = " ") %>% 
        as.data.table() %>% 
        melt(id.vars = c("source"), variable.name = "target") %>% 
        merge(nodes0[,c("name", "id")], by.x = "source", by.y = "name",all.x = TRUE) %>%
        merge(nodes0[,c("name", "id")], by.x = "target", by.y = "name",all.x = TRUE) %>%
        .[,source := id.x] %>%
        .[,target := id.y] %>% 
        .[!is.na(value)] %>% 
        merge(ind2,by.y = "code",by.x = "target",all.x =T) %>% 
        .[!is.na(color),color := paste0(color,"55")] %>% 
        .[is.na(color),color := "#00AAA755"] %>% 
        .[,-c("id.x", "id.y")]
    
      nodes <- copy(nodes0) %>%
        as.data.table() %>% 
        setnames("id","code") %>% 
        merge(ind2,by = "code",all.x =T) %>% 
        .[is.na(color),color :="#00AAA7"]
      
      plot_ly(type = "sankey",
              arrangement = "snap",
              valueformat = ".2f",
              valuesuffix = "",
              node = list(
                label = nodes$name,
                color = nodes$color,
                pad = 40,
                thickness = 30,
                line = list(
                  color = "black",
                  width = 0.5)
              ),
              link = list(
                source = links$source,
                target = links$target,
                value  =  links$value,
                color  = links$color,
                hovertemplate = paste0('Размер взаимосвязи %{value}')
              )) %>% 
        layout(
          title = list(text = "",
                       x = 0.03,
                       y = 0.96),
          font = list(
            size = 16,
            family = "Panton"
          ),
          xaxis = list(showgrid = F, zeroline = F,visible = F),
          yaxis = list(showgrid = F, zeroline = F,visible = F),
          margin = list(l=50, r=50, t=50, b=50))
      
    }
  })
# Тексты подписи городов                      ####
  output$text3 <- renderText({paste0("В разрезе Город - Показатель, т.е самый лучший проект выбраннго типа в выбранном городе из созданных")})
  output$texttitle <- renderText({paste0("Агент-ориентированая модель цифрового двойника города (прототип)")})
# Кнопка вызова формы обратной связи          ####
  observeEvent({input$contact},{
    showModal(modalDialog(
      div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px; font-family: Panton;",
          wellPanel(
            tags$h2("Форма обратная связи", class = "text-center", 
                    style = "padding-top: 0;color:#0F0F0F; font-weight:600; font-family: Panton;"),
            textInput("feedbackName", placeholder="Ваше имя", label = tagList(icon("user"), "Имя")),
            textInput("feedbackcontact", placeholder="Как с вами связаться?", label = tagList(icon("address-book"), "Email/Telegram/другое")),
            textAreaInput("feedbacktext", placeholder="Ваше сообщение", label = tagList(icon("pen"), "Сообщение")),
            br(),
            div(
              style = "text-align: center;",
              actionButton("send", 
                           "Отправить", 
                           style = button_style),
              br()))), 
      footer = modalButton("Отмена")))
  })
# Кнопка отпраквки обратной связи             ####
  observeEvent(priority = 3,{input$send},{
    if(input$feedbackName != ""){
      if(input$feedbackcontact != ""){
        if(input$feedbacktext != ""){
          # MobiDickMessage(chat_id = "232289398", text = paste0("Feedback from site Dtwin.city","\n",
          #                                                      "Name: ",input$feedbackName,"\n",
          #                                                      "Contact: ",input$feedbackcontact,"\n",
          #                                                      "Text: ",input$feedbacktext,"\n"))
          
          MobiDickMessage(chat_id = "-1001791298919", text = paste0("Feedback from site INDEX WEB","\n",
                                                                    "Name: ",input$feedbackName,"\n",
                                                                    "Contact: ",input$feedbackcontact,"\n",
                                                                    "Text: ",TRANSLATE(text = input$feedbacktext , to_lang = "RU"),"\n"))
          
          # options(gargle_oauth_cache = ".secrets")
          # drive_auth(cache = ".secrets", email = "nikbrown35@gmail.com")
          # gs4_auth(token = drive_token())
          # sheet_append("1HHe-gPjAn1dYczyZMrSE5vpodxB1qbWpoG_oxrZJSy0", rbindlist(list(as.data.table(googledrive::drive_get("Tilda_Leads_EN") %>% 
          #                        read_sheet()), data.table(Name = input$feedbackName, Email = input$feedbackcontact,
          #                                                  Describe_your_application = input$feedbacktext, referer = "smart_search",
          #                                                  sent = as.character(Sys.time()))), 
          #                  fill = TRUE, use.names = TRUE)[.N], sheet = "Лист1")
          
          sendSweetAlert(
            session = session,
            title =  "Отправлено",
            btn_labels = "Хорошо",
            btn_colors = "#FFA500",
            text = tags$div(h6(paste0("Благодарим Вас за оставленный отзыв, который очень важен для нас. Мы делаем все возможное, чтобы обслуживание было максимально удобным."))),
            type = "success",
            width = "25%"
          )
          removeModal(session = getDefaultReactiveDomain())
        }else{
          sendSweetAlert(
            session = session,
            title =  "Вы не заполнили текст отзыва",
            btn_labels = "Хорошо",
            btn_colors = "#FFA500",
            text = tags$div(h6("Пожалуйста, напишите текст Вашего отзыва")),
            type = "error",
            width = "25%"
          )
        }}else{
          sendSweetAlert(
            session = session,
            title =  "Вы не оставили никаких контактных данных",
            btn_labels = "Хорошо",
            btn_colors = "#FFA500",
            text = tags$div(h6("Пожалуйста, напишите Ваши контактные данные")),
            type = "error",
            width = "25%"
          )
        }
    }else{
      sendSweetAlert(
        session = session,
        title =  "Вы не заполнили имя",
        btn_labels = "Хорошо",
        btn_colors = "#FFA500",
        text = tags$div(h6("Пожалуйста, напишите свое имя")),
        type = "error",
        width = "25%"
      )
    }
  })
# Visnetwork Модели города                    ####
  output$visnetwork_modal <- renderVisNetwork({
    
    nodes_test0 <- data.table::as.data.table(result_diagram_json$blocks) %>% 
      .[,id:=0:(.N-1)] %>% 
      .[,c("id","text","type","x","y")] %>% 
      .[text == "Infrastructure", y := y + 50] %>% 
      .[text == "Immigration", y := y - 60] %>% 
      .[text == "Loses (B)", y := y - 75] %>% 
      .[id == 9, y := y + 75] %>% 
      .[id == 20, y := y - 75] %>% 
      # .[text == "Import", y := y - 60] %>% 
      data.table::setnames(c("text","type"),c("label","group")) %>% 
      .[group == "Ввод / вывод",group := "Gathering nodes"] %>% 
      .[group == "Блок",group := "Influencing nodes"] %>% 
      .[group == unique(group)[1], `:=`(shape = "box",
                                        icon.color = "#FFA500")] %>% 
      .[group == unique(group)[2], `:=`(shape = "dot",
                                        icon.color = "#1F1F1F")]
    
    c_to_names <- data.table(label = nodes_test0$label, code = str_replace(c("C013", "C757", "C108", "C1120",
                                                                 "C035", "C037","C034",
                                                                 "C699","C087","C892","С1300","C261","С041",
                                                                 "C221","C409","C038","C1049","C897","C896","C1115",
                                                                 "C109","C713","C149","C796","С499","C2705","C049",
                                                                 "C711","C850","С706","C475","C065"), "С","C")) #%>% #, id = nodes_test0$id
      # .[code %in% unique(c(impact_NN_max$from,impact_NN_max$to))]
    # qsave(c_to_names,paste0(path_211, "/1.Data/id_to_c_code"))
    nodes_test <- copy(nodes_test0) %>% 
      merge(c_to_names, by = "label", all.x = TRUE) %>% 
      # .[is.na(code), icon.color := "#CCCCCC"] %>% 
      .[, size := 5] %>% 
      merge(ind[,c("code","long_name")],by.x = "code",by.y = "code",all.x =TRUE) %>% 
      .[,label := long_name] %>% 
      .[,-c("long_name")]
    
    edges_test <- data.table::as.data.table(result_diagram_json$arrows) %>% 
      data.table::setnames(c("startIndex","endIndex"),c("from", "to")) %>% 
      .[,c("from", "to")]
    
    
    visNetwork(nodes_test, edges_test) %>%
      visPhysics(enabled = FALSE) %>% 
      visEvents(startStabilizing = "function() {
    this.moveTo({scale:0.1})}") %>% 
      visEdges  (arrows = "middle",smooth = list(type = "discrete"),font = list(face = "Panton",size = 30)) %>% 
      visNodes  (shadow = TRUE, font = list(face  = "Panton",  color = "black",size = 16)) %>% #fixed = TRUE,
      visLayout (randomSeed = 123, improvedLayout = TRUE) %>% 
      visOptions(highlightNearest = list(enabled   = TRUE,
                                         hover     = TRUE, # подсвечивание узлов графа при проведении мышкой
                                         algorithm = "hierarchical", # подсвечивание последователей по направлению графа или опция "all"
                                         degree    = 1 # глубина подсвечивание
      ), # цвет не выбранных узлов
      autoResize       = TRUE,
      collapse   = list(enabled        = TRUE,
                        fit            = TRUE,
                        resetHighlight = TRUE,
                        keepCoord      = TRUE,
                        labelSuffix    = "A coiled knot"),
      clickToUse   = TRUE
      ) %>%  # Опция редактирования графа
      visEvents(type = "on", doubleClick = "networkOpenCluster", click = "function(node){
                  Shiny.onInputChange('click', node.nodes[0]);
                  ;}") %>% # При двойном нажатии на узел открывается кластер
      visInteraction(hover                = TRUE,
                     hoverConnectedEdges  = TRUE,
                     multiselect          = TRUE,
                     navigationButtons    = TRUE,
                     tooltipDelay         = 50,
                     selectable           = TRUE,
                     selectConnectedEdges = TRUE)
  })
# График временного ряда у схемы              ####
  output$map_plot <- renderPlotly({
    if(!is.null(ind_vis())){
      
      df <- copy(data_for_sd) %>% 
        .[indicator == ind_vis() & location == "19701000"]
      
      plot_ly() %>%
        add_trace(x = as.numeric(df[year > 2022]$year), y = as.numeric(df[year > 2022]$dyn_calc), line = list(dash = 'dash'), color = I("#1c1c1c"), name = "Прогнозные значения", mode = 'lines+markers') %>%
        add_trace(x = as.numeric(df[year <= 2023]$year), y = as.numeric(df[year <= 2023]$dyn_calc), color = I("#1c1c1c"), name = "Модельный ряд данных", mode = 'lines+markers') %>%
        add_trace(x = as.numeric(df$year), y = as.numeric(df$fact), color = I("#FFA500"), name = "Фактические значения", mode = 'lines+markers') %>%
        layout(font = list(family = "Panton", size = 14)) %>%
        layout(legend = list(
          orientation = "h",y = -0.45))  %>%
        layout(title = FALSE, xaxis = list(title = "Год")) %>% 
        layout(
          images = list(
            source = base64enc::dataURI(file = '2.Pic/15.png'),
            x = 0.8, y = 1.1,
            sizex = 0.18, sizey = 0.1,
            xref = "paper", yref = "paper",
            xanchor = "left", yanchor = "bottom"
          ),
          margin = list(t = 50)
        )%>% 
        config(scrollZoom = TRUE, displaylogo = FALSE, 
               toImageButtonOptions = list(format= 'svg', # one of png, svg, jpeg, webp
                                           filename= 'custom_image',
                                           height= 1080,
                                           width= 1920,
                                           scale= 1 ), 
               modeBarButtonsToRemove = c('lasso2d','zoomIn','zoomOut','zoom','box')) %>% 
        add_annotations(
          text = paste0(ind[code == ind_vis()]$long_name[1]),
          x = 0,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(
            size = 16)
        ) %>% 
        layout(hovermode = "x unified", hoverlabel = list(namelength = -1),
               margin = list(l=50, r=50, t=120, b=50))
    }
  })
# Текст вызова кнопки помощи                  ####
  observeEvent({input$help_button},{
    if(input$sbMenu == "dt"){
      showModal(modalDialog(
        p('Раздел "Цифровой двойник города"', style = "color:#1A1814;"),
        p("В данном разделе представлен методологический подход к модели города, приведены прогнозные и фактические значения показателей при оценке основных компонентов и начальных этапов агент-ориентированного моделирования.", style = "color:#1A1814;"),
        br(),
        p("Раздел содержит следующие вкладки:", style = "color:#1A1814;"),
        p(" • Укрупненная модель города — модель связи показателей города, созданная экспертным путем", style = "color:#1A1814;"),
        p(" • Полная модель города — модель связи показателей города, созданная на основе анализа взаимного влияния рядов данных", style = "color:#1A1814;"),
        p(" • Главные компоненты — на вкладке показано как обширный объем показателей складывается в пять главных компонент", style = "color:#1A1814;"),
        p(" • Агентное взаимодействие — на вкладке показаны значимые зависимости между группами агентов-людей и агентами-объектами инфраструктуры.", style = "color:#1A1814;"),
        br(),
        span('Ссылка на лендинг "Цифровой двойник города": ', style = "color:#1A1814;"),
        span('https://dtwin.ru/city_soft', style = "color:#FFA500;",onclick ="location.href='https://dtwin.ru/city_soft';"),
        br(),
        span('Ссылка на презентацию к данному стенду: ', style = "color:#1A1814;"),
        span('https://seafiles.dtwin.ru/f/0ce15a0b3a17416c8f30/', style = "color:#FFA500;",onclick ="location.href='https://seafiles.dtwin.ru/f/0ce15a0b3a17416c8f30/';"),
        br(),
        title = tags$p(paste0("Инструкция по работе с интерактивным стендом"), style = "color:#1A1814; font-weight: bold;"),
        footer = modalButton("Ок"),
        size = 'l',
        easyClose = TRUE
      ))
      
    }else if(input$sbMenu == "project_impact"){
      showModal(modalDialog(
        p('Раздел "Влияние проекта на качество жизни"', style = "color:#1A1814;"),
        br(),
        p("В данном разделе представлены интерактивные инструменты для оценки эффектов пользовательского проекта на показатели города и Индекс качества жизни ВЭБ.РФ.", style = "color:#1A1814;"),
        p("Пользователь может ввести параметры оцениваемого проекта (город, объем инвестиций и т.д.) на вкладке слева.", style = "color:#1A1814;"),
        br(),
        p("Раздел содержит следующие вкладки:", style = "color:#1A1814;"),
        p(" • Эффекты — санкей-диаграмма показывает потоки эффектов от пользовательского проекта, а также влияние на 11 направлений и итоговую оценку Индекса ВЭБ.РФ. Графики под санкей-диаграммой показывают влияние выбранного проекта на ключевые показатели города, и оценку по направлениям Индекса ВЭБ.РФ.", style = "color:#1A1814;"),
        p(" • Что надо чтобы — данный раздел позволяет пользователю выбрать целевой показатель для максимизации и объем имеющихся в распоряжении инвестиций, после чего система распределит инвестиции по наиболее оптимальным отраслям для вложений.  ", style = "color:#1A1814;"),
        p(" • Сравнение проектов — пользователь может провести сравнение в разрезе всех созданных проектов, а также отфильтровать и отсортировать таблицу по интересующим его атрибутам.", style = "color:#1A1814;"),
        p(" • Подбор города для проекта — данный раздел позволяет пользователю выбрать любой проект из библиотеки проектов, либо использовать собственный проект (задается во вкладке слева), после чего система выдаст перечень наиболее подходящих городов для реализации этого проекта. В качестве критерия для ранжирования городов используется наибольший прирост Индекса ВЭБ.РФ от реализации проекта.", style = "color:#1A1814;"),
        br(),
        span('Ссылка на лендинг "Цифровой двойник города": ', style = "color:#1A1814;"),
        span('https://dtwin.ru/city_soft', style = "color:#FFA500;",onclick ="location.href='https://dtwin.ru/city_soft';"),
        br(),
        span('Ссылка на презентацию к данному стенду: ', style = "color:#1A1814;"),
        span('https://seafiles.dtwin.ru/f/0ce15a0b3a17416c8f30/', style = "color:#FFA500;",onclick ="location.href='https://seafiles.dtwin.ru/f/0ce15a0b3a17416c8f30/';"),
        br(),
        title = tags$p(paste0("Инструкция по работе с интерактивным стендом"), style = "color:#1A1814; font-weight: bold;"),
        footer = modalButton("Ок"),
        size = 'l',
        easyClose = TRUE
      ))
      
    }
  })
# Текст сравниения проектов                   ####
  output$brbr <- renderUI({
    HTML(as.character(paste0(
      span("Главные компоненты — на вкладке показано как обширный объем показателей складывается в пять главных компонент.", style = "font-size: 14px; color:#1A1814;"),
      br(),
      span("Каждый столбец отвечает за один показатель, с помощью цветовой пропорции показано на какие главные компоненты он оказывает влияние.", style = "font-size: 14px; color:#1A1814;"),
      br(),
      span("Например, если столбец состоит из двух цветов, это означает что представленный показатель влияет на две главные компоненты соответствующего цвета.", style = "font-size: 14px; color:#1A1814;")
    )))
  })
}
