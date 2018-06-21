source('modules.R')
source('config.R')

ui <- dashboardPage(
  dashboardHeader(
    title = "Making Shiny Dashboards Shine",
    titleWidth = 450,
    headerUI("example")
  ),
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "custom.css")),
    bodyUI("example"),
    conditionalPanel(
      condition = "!output['example-logged_in']",
      loginUI("example")
    ),
    conditionalPanel(
      condition = "output['example-logged_in']",
      tabItems(
        tabItem(
          tabName = "example",
          fluidPage(
            dashboardUI("example")
          ) #End fluidPage
        ) #End tabItem
      ) #End tabItems
    ) #End conditionalPanel
  ) #End dashboardBody
)

server <- function(input, output, session) {
  callModule(login, "example")
  callModule(display_table, "example", quakes)
  callModule(info, "example", quakes)
  callModule(quake_map, "example", quakes)
}

shinyApp(ui, server)