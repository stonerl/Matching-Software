library(shiny)
library(shinybusy)
library(shinyFiles)
library(shinydashboard)
library(shinyjs)
library(DT)
library(data.table)

source('matching_algorithm.R')

ui <- dashboardPage(
  dashboardHeader(title = 'Buddy-Matching'),
  dashboardSidebar(
    shinyFilesButton(
      'file_incoming',
      'Datei für Incoming',
      'Incoming-Datei wählen',
      multiple = FALSE,
      style = 'width: 87%; margin-top: 25px'
    ),
    div(
      style = 'width: 87%; margin: auto' ,
      verbatimTextOutput('path_incoming', placeholder = TRUE)
    ),
    shinyFilesButton(
      'file_tuebinger',
      'Datei für Tübinger',
      'Tübinger-Datei wählen',
      multiple = FALSE,
      style = 'width: 87%; margin-top: 25px'
    ),
    div(
      style = 'width: 87%; margin: auto' ,
      verbatimTextOutput('path_tuebinger', placeholder = TRUE)
    ),
    shinyDirButton(
      'dir_output',
      'Ausgabeordner',
      'Speicherort für Matchingergebisse wählen',
      style = 'width: 87%; margin-top: 25px'
    ),
    div(style = 'width: 87%; margin: auto',
        verbatimTextOutput('path_output', placeholder = TRUE)),
    actionButton('start_matching', 'Matching beginnen', style = 'width: 87%; margin-top: 50px'),
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
  roots = c(home = '~')
  filetypes = c('csv', 'xlsx')
  
  shinyFileChoose(
    input,
    'file_incoming',
    roots = roots,
    session = session,
    filetypes = filetypes
  )
  
  path_to_incoming <<- reactive(input$file_incoming)
  output$path_incoming <-
    renderText({
      as.character(parseFilePaths(roots, path_to_incoming())$datapath)
    })
  
  shinyFileChoose(
    input,
    'file_tuebinger',
    roots = roots,
    session = session,
    filetypes = filetypes
  )
  
  path_to_tuebinger <<- reactive(input$file_tuebinger)
  
  output$path_tuebinger <- renderText({
    as.character(parseFilePaths(roots, path_to_tuebinger())$datapath)
  })
  
  shinyDirChoose(
    input,
    'dir_output',
    roots = roots,
    session = session,
    filetypes = filetypes
  )
  
  dir_to_output <- reactive(input$dir_output)
  
  output$path_output <- renderText({
    parseDirPath(roots, dir_to_output())
  })
  
  observeEvent(input$start_matching, {
    if (!(isTruthy(input$file_incoming)) |
        !(isTruthy(input$file_tuebinger)) |
        !(isTruthy(input$dir_output))) {
      showModal(
        modalDialog(
          title = "Dateien und/oder Ordner fehlen",
          "Es wurden entweder nicht alle Dateien für das Matching angegeben, oder es fehlt der Ausgabeordner."
        )
      )
    } else {
      # initialize empty object to hold the dataframe
      table_results_df <- NULL
      
      show_modal_spinner(spin = 'fading-circle',
                         color = '#a51e37',
                         text = 'Das Matching kann bis zu einer Stunde dauern. Bitte warten…')
      
      table_results_df <-
        matching_programm(req(as.character(
          parseFilePaths(roots, path_to_incoming())$datapath
        )),
        req(as.character(
          parseFilePaths(roots, path_to_tuebinger())$datapath
        )),
        req(as.character(
          parseDirPath(roots, dir_to_output())
        )),
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
    }
  })
}

shinyApp(ui, server)
