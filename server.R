source('helpers.R')
source('libraries.R')
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
