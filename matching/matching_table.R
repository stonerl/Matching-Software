source('matching/matching_algorithm.R')

# ----------------------------------------------------------------------------------------------------------------------
# FUNCTION MATCHING_TABLE
#-----------------------------------------------------------------------------------------------------------------------

# This function outputs a datatable, that can then be drawn in the UI.
#
# file_1 needs to be a datapath
# file_2 can either be a datapath or an NULL object
#
# If a NULL object is passed we expect that file_1 contains a proper formatted matching result table

matching_table <- function(file_1, file_2) {
  # initialize a NULL object to hold the datatable
  table_results <- NULL
  
  tryCatch({
    if (is.null(file_2)) {
      show_modal_spinner(spin = 'fading-circle',
                         color = '#a51e37',
                         text = 'Loading matching data. Please wait…')
      
      table_results <- read.xlsx2(
        file = file_1,
        sheetIndex = 1,
        startRow = 2,
        header = TRUE,
        colClasses = NA,
        as.data.frame = TRUE
      )
      
      setDT(table_results)
      
      setorder(table_results,
               na.last = FALSE)
      
      Sys.sleep(3)
    } else {
      show_modal_spinner(spin = 'fading-circle',
                         color = '#a51e37',
                         text = 'Matching can take up to one hour. Please wait…')
      
      table_results <-
        matching_programm(file_1,
                          file_2,
                          FALSE,
                          TRUE)
      
      while (is.null(table_results)) {
        Sys.sleep(5)
      }
      
      setDT(table_results)
      
      setorder(table_results,
               na.last = FALSE)
    }
    
    table_results <-
      datatable(
        table_results,
        options = list(
          pageLength = 25,
          lengthMenu = list(c(10, 25, 50, 75, 100, -1), c(10, 25, 50, 75, 100, 'All')),
          keys = TRUE,
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
        extensions = c('Buttons', 'KeyTable'),
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
    addClass(selector = "body", class = "sidebar-collapse")
    return(table_results)
  },
  error = function(err) {
    showNotification(paste0(err),
                     duration = 30,
                     type = 'err')
  },
  finally = c(resetButtons(), remove_modal_spinner()))
}