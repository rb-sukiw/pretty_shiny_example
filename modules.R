################################# LOGIN SCREEN #################################
loginUI <- function(id){
  ns <- NS(id)
  tagList(
    column(
      width = 4,
      offset = 4,
      wellPanel(
        tags$head(tags$link(rel = "stylesheet", type = "text/css",
                            href = "custom.css")),
        h4(textOutput(ns("login_info")), align = "center"),
        h3(textInput(ns("user"), label = "Username")),
        tagAppendAttributes(
          h3(passwordInput(ns("pw"), label = "Password")),
          `data-proxy-click` = ns("login")
        ),
        actionButton(inputId = ns("login"), label = "Login",
                     class = "loginButton")
      )
    )
  )
}

login <- function(input, output, session){
  approved_users <- data.frame(users,passwords)

  login_attempt <- reactiveValues(
    message = "Please enter your login credentials", status = FALSE
    )
  output$logged_in <- reactive({return(login_attempt$status)})
  outputOptions(output, "logged_in", suspendWhenHidden = FALSE)

  observeEvent(
    eventExpr = input$login,
    handlerExpr = {
      validate(
        need(input$user, "Please enter your username"),
        need(input$pw, "Please enter your password"),
        need(approved_users, "Problem with the vertica connection")
      )
      is_approved <- approved_users %>%
        filter(users == input$user, passwords == input$pw)
      if(nrow(is_approved) > 0){
        login_attempt$status <- TRUE
        login_attempt$message <- paste("You are logged in as", input$user)
      } else {
        login_attempt$status <- FALSE
        showModal(modalDialog(
          title = "Invalid credentials",
          "Please check your login information and try again.",
          easyClose = TRUE
        ))
      }
    } # end handlerExpr
  ) # end observEvent

  observeEvent(
    eventExpr = input$logout,
    handlerExpr = {
      login_attempt$status <- FALSE
      login_attempt$message <- paste("Logged out")
    }
  )
  output$login_info <- reactive({
    return(login_attempt$message)
  })
}

################################### HEADER UI ##################################
headerUI <- function(id){
  ns <- NS(id)
  tags$li(a(
    href = 'http://www.massmutual.com',
    img(
      src = 'massmutual_logo.png',
      title = "Company Home",
      height = "50px",
      width = "200px"
    ),
    style = "padding-top: 0px; padding-bottom: 0px;"),
    class = "dropdown")
}

################################# TAB PANELS UI ################################
dashboardUI <- function(id){
  ns <- NS(id)
  tagList(
    column(width = 12,
      fluidRow(
        tags$style(".nav-tabs-custom .nav-tabs li.active {border-top-color: #213469;}"),
        tabBox(
          width = "100%",
          tabPanel(
            title = "Earthquake Info",
            div(uiOutput(ns("info")))
      )
        )#End tabBox
      ),#End fluidRow
      br(),
      div(
        span(
          tags$style(HTML('table.dataTable tr.selected td, table.dataTable
                          td.selected {background-color: rgba(90, 91, 91, 0.6)
                          !important;}')),
          DT::dataTableOutput(ns("main_table")),
          style = "height:40%;"
        )
      )
    ) #End column
  ) #End tagList
}

################################ DATA TABLE ###################################
tableUI <- function(data){
  df <- quakes %>%
    rename("LATITUDE" = lat,
           "LONGITUDE" = long,
           "DEPTH" = depth,
           "MAGNITUDE" = mag,
           "REPORTING STATIONS" = stations
          )
  #Data table UI
  out <- datatable(
    df, selection = "single", rownames = FALSE,
    options = list(
              pageLength = 10,
              autoWidth = FALSE,
              scrollX = FALSE,
              columnDefs = list(
                list(
                  width = '200px',
                  targets = c(0,1,2,3,4)
                )
              ),
      searching = TRUE
    ),
    class = 'cell-border stripe'
  )
  return(out)
}

display_table <- function(input, output, session, data){
  output$main_table <- renderDataTable({tableUI(data())})
}

################################# QUAKE INFO TAB ###############################
info <- function(input, output, session, data){
  output$quake_info <- renderUI({
    validate(
      need(
        try(input$main_table_rows_selected),
        "Select a row to see a summary of the earthquake"
      )
    )
    num_rows <- length(input$main_table_rows_selected)
    if(num_rows > 0){
      index <- input$main_table_rows_selected
      active_lat <- data()$main_table[index,]$lat
      active_long <- data()$main_table[index,]$long
      selected_df <- quakes %>% filter(lat == active_lat, long == active_long)
      lat <- selected_df$lat
      long <- selected_df$long
      depth <- selected_df$depth
      mag <- selected_df$mag
      stations <- selected_df$stations
      out_html <- HTML(
        "<ul style = 'list-style-type:none'>",
        "<li><b>Location: </b>", lat, long, "</li>",
        "<li><b>Depth: </b>", depth, "</li>",
        "<li><b>Magnitude: </b>", mag, "</li>",
        "<li><b>Number of reporting stations: </b>", stations, "</li>",
        "</ul>"
      )
      return(out_html)
    }
  })
}