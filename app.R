source('modules.R')
source('config.R')

ui <- dashboardPage(
  dashboardHeader(title = "Making Shiny Dashboards Shine", titleWidth = 550, headerUI("quakes")),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    conditionalPanel(condition = "!output['quakes-logged_in']",
                     loginUI("quakes")),
    conditionalPanel(condition = "output['quakes-logged_in']",
                     dashboardUI("quakes")
    ) #End conditionalPanel
  ) #End dashboardBody
)

server <- function(input, output, session){
  callModule(login, "quakes")
  callModule(display_table, "quakes", quakes)
  callModule(quake_info, "quakes", quakes)
}

shinyApp(ui, server)