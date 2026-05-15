# Test if the R code in the file has any obvious issues
library(shiny)

# Check the column structure
tryCatch({
  # Simulate the fluidRow with 4 columns of 3 units each
  test_ui <- fluidRow(
    style = "margin: 0; padding: 15px 0; width: 100%;", id = "header-row",
    column(3,
      tags$label(strong("Workbook name"))
    ),
    column(3,
      tags$label(strong("Import workbook"))
    ),
    column(3,
      tags$label(strong("Export workbook"))
    ),
    column(3,
      tags$label(strong("Display"))
    )
  )
  print("UI structure is valid!")
}, error = function(e) {
  print(paste("ERROR:", e$message))
})
