library(shiny)
library(shinybusy)
library(shinyFiles)
library(shinydashboard)
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
    div(
      style = 'width: 87%; margin: auto' ,
      verbatimTextOutput('path_output', placeholder = TRUE)
    ),
    actionButton('start_matching', 'Matching beginnen', style = 'width: 87%; margin-top: 50px'),
    div(
      style= 'position: absolute; bottom: 1%; left: 14%', "© Toni Förster - Version: 0.2 "
    )
  ),
  dashboardBody(DTOutput('table_results'))
  
)

server <- function(input, output, session) {
  roots = c(home = '~')
  filetypes = c('csv')
  
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
      table_results_df <- datatable(table_results_df, colnames = c('Name', 'Alter', 'Geschlecht', 'Sprachen', 'Ankunft', 'Name', 'Alter', 'Geschlecht', 'Sprachen', 'Ankunft'))
      remove_modal_spinner()
      output$table_results <-
        renderDT(table_results_df, options = list(lengthChange = FALSE))
    }
  })
}

shinyApp(ui, server)
