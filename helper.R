# ----------------------------------------------------------------------------------------------------------------------------------------
# emptyDT
#-----------------------------------------------------------------------------------------------------------------------------------------

# create an empty datatable with all headers that are displayed later on
emptyDT <- data.table(
  'Last name' = numeric(),
  'Age' = numeric(),
  'Gender' = numeric(),
  'Major' = numeric(),
  'Country' = numeric(),
  'Languages' = numeric(),
  'Arrival' = numeric(),
  'Last name' = numeric(),
  'Age' = numeric(),
  'Gender' = numeric(),
  'Country' = numeric(),
  'Languages' = numeric(),
  'Available' = numeric(),
  'ECTS' = numeric()
)

# ----------------------------------------------------------------------------------------------------------------------------------------
# resetTable
#-----------------------------------------------------------------------------------------------------------------------------------------

# helper to draw a additional Button in the DTOutput view

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

# ----------------------------------------------------------------------------------------------------------------------------------------
# resetButtons
#-----------------------------------------------------------------------------------------------------------------------------------------

# helper function to reset all buttons

resetButtons <- function() {
  reset('file_incomings')
  reset('file_tuebinger')
  reset('file_matching')
  disable('start_matching')
}