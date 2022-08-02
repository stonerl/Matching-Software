library(shiny)
library(shinybusy)
library(shinyFiles)
library(shinydashboard)
library(shinyjs)
library(tools)
library(DT)
library(data.table)
library(xlsx)
library(dplyr)

source('matching_algorithm.R')
source('matching_table.R')

filetypes <-
  c(
    '.xlsx',
    'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
    '.csv',
    'text/csv'
  )

ui <- dashboardPage(
  dashboardHeader(title = 'Buddy-Matching'),
  dashboardSidebar(
    disabled(
      actionButton('start_matching', 'Start matching', style = 'width: 87%; margin-top: 10px; margin-bottom: 10px')
    ),
    fileInput('file_incomings', 'Incomings', accept = filetypes),
    fileInput('file_tuebinger', 'Tübinger', accept = filetypes),
    fileInput('file_matching', 'Load previous matching data', accept = filetypes),
    div(
      img(src = 'UT_Logo.png', style = 'position: absolute; bottom: 20px; left: 7%; width: 87%')
    )
  ),
  dashboardBody(
    tags$head(tags$style(
      HTML('.shiny-notification {right: 20px; bottom: 20 px;}')
    )),
    tags$head(tags$style(
      HTML('.skin-blue .main-sidebar {background-color: #bba976;}')
    )),
    tags$head(tags$style(
      HTML('.skin-blue .main-header .logo {background-color: #b4a069;}')
    )),
    tags$head(tags$style(
      HTML(
        '.skin-blue .main-header .logo:hover {background-color: #b4a069;}'
      )
    )),
    tags$head(tags$style(
      HTML('.skin-blue .main-header .navbar {background-color: #a51e37;}')
    )),
    tags$head(tags$style(
      HTML(
        '.skin-blue .main-header .navbar .sidebar-toggle:hover {background-color: #b44d50;}'
      )
    )),
    useShinyjs(),
    DTOutput('table_results', height = "90vh")
  )
)

server <- function(input, output, session) {
  file_incomings <- reactive(input$file_incomings)
  file_tuebinger <- reactive(input$file_tuebinger)
  file_matching <- reactive(input$file_matching)
  
  observeEvent(list(file_incomings(), file_tuebinger()), {
    req(file_incomings())
    req(file_tuebinger())
    enable('start_matching')
  })
  
  observeEvent(file_matching(), {
    req(file_matching())
    output$table_results <-
      renderDT(matching_table(file_matching()$datapath, NULL))
  })
  
  observeEvent(input$start_matching, {
    output$table_results <-
      renderDT(matching_table(file_incomings()$datapath, file_tuebinger()$datapath))
  })
}

shinyApp(ui, server)
