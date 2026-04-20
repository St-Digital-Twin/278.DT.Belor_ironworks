# 1. UI                                 ####
ui <- bs4DashPage(
  dark = NULL,
  help = NULL,
  title = "Цифровой двойник города",
  # 1.1. Header                         ####
  header = bs4DashNavbar(
    tags$head(
      includeCSS("www/CSS.css")
    ),
    tags$head(tags$link(rel="shortcut icon",
                        href="https://static.tildacdn.com/tild3863-3066-4836-b439-373366623330/favicon.ico",
                        type="image/vnd.microsoft.icon")),
    tags$style(HTML(
      "<script>
           $( document ).ready(function() {
              $(\".tab-content [type='checkbox']\").on('click', function(){
                var is_checked = this.checked;
                if(typeof is_checked === 'boolean' && is_checked === true){
                  setTimeout(function() {
                    window.scrollTo(0,document.body.scrollHeight);
                  }, 200)
                }
              })
           })
        </script>")),
    status = 'light',
    controlbarIcon = shiny::icon("info"),
    textOutput("texttitle"),
    rightUi = tagList(tags$li(class='dropdown', bs4Dash::actionButton("contact", "Обратная связь", icon = icon("comment"),status = "danger", style = paste0(button_style,""))))
   ),
  # 1.2. Sidebar                        ####
  sidebar = bs4DashSidebar(
    collapsed = F,
    minified = F,
    expandOnHover = FALSE,
    elevation = 5,
    customArea = actionButton("help_button", "Помощь",status = "warning",
                              style = button_style),
    
    imageOutput("logo", height = "70px"),
    bs4SidebarMenu(
      id = "sbMenu",
      tags$style(HTML('.sidebar{font-family: Panton;
                                 color: #1A1814}}
                      ')),
      bs4SidebarMenuItem(text = "Цифровой двойник города", icon = icon("project-diagram"), tabName = 'dt'),
      bs4SidebarMenuItem(text = "Влияние проекта на качество жизни", icon = icon("chart-column"), tabName = 'project_impact',selected = TRUE,startExpanded = T,
                         fluidRow(
                           uiOutput("ui_project"),
                           uiOutput("ui_output1"))
      ),
      setSliderColor(c(rep("black",100)), c(1:100)),
      chooseSliderSkin("Flat")
    )
  ),
  # 1.3. Controlbar (left)              ####
  controlbar = bs4DashControlbar(
    tags$ol(
      style = "font-size:14px;",
      tags$h5 (tags$b("Ссылки"), style = "margin-top: 15px; margin-bottom: 15px;"),
      tags$li( a(href = "https://www.dtwin.ru"       , target = "_blank", "Сайт компании")),  # tags$li(
      tags$li( a(href = "https://openbook.dtwin.ru"  , target = "_blank", "Открытая книга ЦД")),  # tags$li(
      tags$li( a(href = "https://dtwin.city"         , target = "_blank", "Платформа анализа городов")),  # tags$li(
      tags$li( a(href = "https://t.me/DT_Prosha_Bot" , target = "_blank", "Чат бот телеграмм")),  # tags$li(
      tags$li( a(href = "https://teb.dtwin.ru/"      , target = "_blank", "Энергетика РФ")),  # tags$li(
      tags$li( a(href = "https://asset.dtwin.ru"     , target = "_blank", "Анализ Промышленного преприятия")),  # tags$li(
      tags$li( a(href = "https://t.me/sergeygumerov" , target = "_blank", "Телеграм CEO"))  # tags$li(
    )
  ),
  
  # 1.4. Body                           ####
  body = bs4DashBody(
    use_theme(create_theme(
      bs4dash_status(light = "#787878"),
      bs4dash_status(light = "#1A1814", primary = "#FFA500"),
      bs4dash_vars(
        navbar_light_color = "orange",
        navbar_light_active_color = "white",
        navbar_light_hover_color = "white"
      ),
      bs4dash_font(
        size_base = "0.85rem"
      ),
      bs4dash_sidebar_light(
        bg = "white",
        color = '#1A1814',
        hover_color  = 'gray',
        submenu_active_bg  = 'orange'
      ),
      bs4dash_sidebar_dark(
        bg = "white",
        color = '#1A1814',
        hover_color  = 'gray',
        submenu_active_bg   = 'orange'
      ),
      bs4dash_layout(
        sidebar_width = "340px",
        main_bg = "#15120F"
      ),
      bs4dash_color(
        gray_900 = "#FFF"
      )
    )),
    tags$head(tags$style(HTML('
        .dropdown-menu {
          font-weight: bold;
          font-family: Panton;
          color: #0F0F0F;
                        }'))),
    tags$head(tags$style(HTML('
        .modal-content {
          font-weight: bold;
          font-family: Panton;
          color: #0F0F0F;
                        }'))),
    bs4TabItems(
      bs4TabItem(tabName = "dt",             uiOutput("ui_dt")),
      bs4TabItem(tabName = "project_impact", uiOutput("ui_pi"))
    ),
    tags$head(tags$style("#texttitle{
                                 font-size: 20px;
                                 font-weight: bold;
                                 }")),
    tags$head(tags$style(HTML('
        .nav-pills .nav-link.active {
          font-weight: bold;
          font-family: Panton;
          color: #0F0F0F;
                        }'))),
    tags$head(tags$script(HTML(js))),
  )
)

