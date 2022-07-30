library(shiny)
library(shinybusy)
library(shinyFiles)
library(shinydashboard)
library(DT)
library(data.table)

source('matching_algorithm.R')

ui <- fluidPage(
  titlePanel(
    title = div(
      img(src = 'UT_Logo.png', style = 'width: 15%; height: 15%'),
      'Buddy-Matching-Programm'
    ),
    'Buddy-Matching-Programm'
  ),
  sidebarLayout(
    sidebarPanel(
      shinyFilesButton(
        'file_incoming',
        'Datei für Incoming',
        'Incoming-Datei wählen',
        multiple = FALSE
      ),
      verbatimTextOutput('path_incoming', placeholder = TRUE),
      br(),
      shinyFilesButton(
        'file_tuebinger',
        'Datei für Tübinger',
        'Tübinger-Datei wählen',
        multiple = FALSE
      ),
      verbatimTextOutput('path_tuebinger', placeholder = TRUE),
      br(),
      shinyDirButton(
        'dir_output',
        'Ausgabeordner',
        'Speicherort für Matchingergebisse wählen'
      ),
      verbatimTextOutput('path_output', placeholder = TRUE),
      br(),
      br(),
      actionButton('start_matching', 'Matching beginnen'),
      width = 3,
    ),
    mainPanel(DTOutput('table_results'),
              width = 9),
    position = c('right')
  ),
  lang = 'de'
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
                         color = '#A51E37' ,
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
      table_results_df <- datatable(table_results_df, colnames = c('Name', 'Alter', 'Genus', 'Sprachen', 'Ankunft', 'Name', 'Alter', 'Genus', 'Sprachen', 'Ankunft'))
      remove_modal_spinner()
      output$table_results <-
        renderDT(table_results_df, options = list(lengthChange = FALSE))
    }
  })
}

shinyApp(ui, server)
