library(shiny)
library(shinyFiles)

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
    mainPanel(fluidRow(tableOutput('table_results')),
              width = 9),
    position = c('left')
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
  
  dir_to_output <<- reactive(input$dir_output)
  
  output$path_output <- renderText({
    parseDirPath(roots, dir_to_output())
  })
  
  observeEvent(input$start_matching, {
    showModal(
      modalDialog(
        title = 'Matching wurde gestartet',
        'Dieser Vorgang kann mehrere Minuten dauern.',
        footer = NULL,
        easyClose = FALSE,
        fade = TRUE
      ),
      session = session
    )
    output$table_results <<-
      renderTable(data.frame(matching_programm(
        req(as.character(
          parseFilePaths(roots, path_to_incoming())$datapath
        )),
        req(as.character(
          parseFilePaths(roots, path_to_tuebinger())$datapath
        )),
        req(as.character(parseDirPath(
          roots, dir_to_output()
        ))),
        TRUE
      )),
      colnames = FALSE,
      spacing = c('s'))
    
    removeModal(session = session)
    
  })
}

shinyApp(ui, server)

