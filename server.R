source('helpers.R')
source('libraries.R')
source('matching/matching_algorithm.R')
source('matching/matching_table.R')

server <- function(input, output, session) {
  file_incomings <- reactive(input$file_incomings)
  file_tuebinger <- reactive(input$file_tuebinger)
  file_matching <- reactive(input$file_matching)
  
  observeEvent(list(file_incomings(), file_tuebinger()), {
    req(file_incomings())
    req(file_tuebinger())
    enable('start_matching')
  })
  
  observeEvent(input$start_matching, {
    show_modal_spinner(spin = 'fading-circle',
                       color = '#a51e37',
                       text = 'Matching will take several minutes. Please wait…')
    table_results <-
      matching_programm(file_incomings()$datapath,
                        file_tuebinger()$datapath,
                        FALSE,
                        TRUE)
    table_results <-
      matching_table(table_results)
    
    output$table_results <- renderDT(table_results)
    
    addClass(selector = "body", class = "sidebar-collapse")
    remove_modal_spinner()
    resetButtons()
  })
  
  observeEvent(file_matching(), {
    req(file_matching())
    show_modal_spinner(spin = 'fading-circle',
                       color = '#a51e37',
                       text = 'Loading matching data. Please wait…')
    
    table_results <<- matching_table(
      read.xlsx2(
        file = file_matching()$datapath,
        sheetIndex = 1,
        startRow = 2,
        header = TRUE,
        as.data.frame = TRUE
      )
    )
    
    output$table_results <- renderDT(table_results)
    
    addClass(selector = "body", class = "sidebar-collapse")
    remove_modal_spinner()
    resetButtons()
  })
  
}
