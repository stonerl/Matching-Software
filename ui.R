source('helpers.R')
source('libraries.R')

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
