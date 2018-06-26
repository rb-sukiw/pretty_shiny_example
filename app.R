source('modules.R')
source('config.R')

ui <- dashboardPage(
  dashboardHeader(title = "Making Shiny Dashboards Shine",
                  titleWidth = 550, headerUI("earthquakes")),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    conditionalPanel(condition = "!output['earthquakes-logged_in']",
                     loginUI("earthquakes")),
    conditionalPanel(condition = "output['earthquakes-logged_in']",
                     dashboardUI("earthquakes")
    ) #End conditionalPanel
  ) #End dashboardBody
)

server <- function(input, output, session){
  callModule(login, "earthquakes")
  callModule(display_table, "earthquakes", quakes)
  callModule(quake_info, "earthquakes", quakes)
}

shinyApp(ui, server)