library(shiny)
library(shinybusy)
library(shinyFiles)
library(shinydashboard)
library(shinyjs)
library(tools)
library(DT)
library(data.table)

source('matching_algorithm.R')

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
    fileInput('file_incomings', 'Incomings', accept = filetypes),
    fileInput('file_tuebinger', 'Tübinger', accept = filetypes),
    
    disabled(
      actionButton('start_matching', 'Matching beginnen', style = 'width: 87%; margin-top: 55px')
    ),
    div(
      img(src = 'UT_Logo.png', style = 'position: absolute; bottom: 20px; left: 7%; width: 87%')
    )
  ),
  dashboardBody(
    tags$head(tags$style(
      HTML('.skin-blue .main-sidebar {background-color: #b4a069;}')
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
  
  observeEvent(list(file_incomings(), file_tuebinger()), {
    # used to check file extensions later
    ext_incomings <- file_ext(file_incomings()$datapath)
    ext_tuebinger <- file_ext(file_incomings()$datapath)
    
    if (isTruthy(file_incomings()) &&
        isTruthy(file_tuebinger())) {
      enable('start_matching')
    }
  })
  observeEvent(input$start_matching, {
    # initialize empty object to hold the dataframe
    table_results_df <- NULL
    
    show_modal_spinner(spin = 'fading-circle',
                       color = '#a51e37',
                       text = 'Das Matching kann bis zu einer Stunde dauern. Bitte warten…')
    
    table_results_df <-
      matching_programm(req(as.character(file_incomings()$datapath)),
                        req(as.character(file_tuebinger()$datapath)),
                        FALSE,
                        TRUE)
    
    while (is.null(table_results_df)) {
      Sys.sleep(5)
    }
    setDT(table_results_df)
    setorder(table_results_df, Last.Name.1, na.last = FALSE)
    table_results_df <-
      datatable(
        table_results_df,
        options = list(pageLength = 15),
        fillContainer = TRUE,
        plugins = 'natural',
        colnames = c(
          'Name',
          'Alter',
          'Geschlecht',
          'Studienfach',
          'Sprachen',
          'Ankunft',
          'Name',
          'Alter',
          'Geschlecht',
          'Studienfach',
          'Sprachen',
          'Ankunft'
        )
      )
    remove_modal_spinner()
    output$table_results <- renderDT(table_results_df)
    addClass(selector = "body", class = "sidebar-collapse")
  })
}

shinyApp(ui, server)
