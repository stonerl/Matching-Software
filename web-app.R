library(shiny)
library(shinybusy)
library(shinyFiles)
library(shinydashboard)
library(shinyjs)
library(tools)
library(DT)
library(data.table)

source('matching_algorithm.R')

resetTable <- c(
  "function(e, dt, node, config){",
  "  dt.iterator('table', function(s){",
  "    s.aaSorting.length = 0;",
  "    s.aiDisplay.sort(function(a,b){",
  "       return a-b;",
  "    });",
  "    s.aiDisplayMaster.sort(function(a,b){",
  "       return a-b;",
  "    });",
  "  }).draw();",
  "}"
)

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
      actionButton('start_matching', 'Start matching', style = 'width: 87%; margin-top: 55px')
    ),
    div(
      img(src = 'UT_Logo.png', style = 'position: absolute; bottom: 20px; left: 7%; width: 87%')
    )
  ),
  dashboardBody(
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
                       text = 'Matching can take up to one hour. Please wait…')
    
    table_results_df <-
      matching_programm(req(as.character(file_incomings()$datapath)),
                        req(as.character(file_tuebinger()$datapath)),
                        FALSE,
                        TRUE)
    
    while (is.null(table_results_df)) {
      Sys.sleep(5)
    }
    setDT(table_results_df)
    setorder(table_results_df, Would.you.like.to.receive.ECTS.credit.points.for.the.Studium.Professionale..Key.Qualifications...Intercultural.Competency...., na.last = FALSE)
    table_results_df <-
      datatable(
        table_results_df,
        options = list(
          pageLength = 25,
          lengthMenu = list(c(10, 25, 50, 75, 100, -1), c(10, 25, 50, 75, 100, 'All')),
          dom = "Bftip",
          buttons = list(
            'pageLength',
            list(
              extend = 'collection',
              text = 'Reset sorting',
              action = JS(resetTable)
            ),
            list(extend = 'excel',
                 text = 'Export to Excel')
          ),
          columnDefs = list(list(
            visible = FALSE,
            targets = c(
              # Incomings
              # First name
              2,
              # Email
              3,
              # 2nd Major
              7,
              # Degree
              8,
              # University
              10,
              # University free text
              11,
              # Communication Language
              12,
              # Hobby 1
              13,
              # Hobby 2
              14,
              # Hobby 3
              15,
              # Pre-Course
              17,
              # Inform Buddy
              19,
              # Comments
              20,
              # Tuebinger
              # First name
              22,
              # Email
              23,
              # 2nd Major
              27,
              # Degree
              28,
              # University
              30,
              # University free text
              31,
              # Hobby 1
              32,
              # Hobby 2
              33,
              # Hobby 3
              34,
              # 2 Buddies
              36,
              # Inform Buddy
              39,
              # Comments
              40
            )
          ))
        ),
        fillContainer = TRUE,
        extensions = 'Buttons',
        plugins = 'natural',
        colnames = c(
          'Last name',
          'First name',
          'Email',
          'Age',
          'Gender',
          'Major',
          '2nd Major',
          'Degree',
          'Country',
          'University',
          'University free text',
          'Communication Language',
          'Hobby 1',
          'Hobby 2',
          'Hobby 3',
          'Languages',
          'Pre-Course',
          'Arrival',
          'Inform Buddy',
          'Comments',
          'Last name',
          'First name',
          'Email',
          'Age',
          'Gender',
          'Major',
          '2nd Major',
          'Degree',
          'Country',
          'University',
          'University free text',
          'Hobby 1',
          'Hobby 2',
          'Hobby 3',
          'Languages',
          '2 Buddies',
          'Available',
          'ECTS',
          'Inform Buddy',
          'Comments'
        )
      )
    remove_modal_spinner()
    output$table_results <- renderDT(table_results_df)
    addClass(selector = "body", class = "sidebar-collapse")
  })
}

shinyApp(ui, server)
