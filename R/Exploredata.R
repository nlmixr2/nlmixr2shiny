#' Get the data for Exploring the model
#'
#' @return A character vector of the names of the data sets.
#' @noRd
#' @author Dyani Peterson 
#' @examples
#' getDataNamesForExploration()
getDataNamesForExploration <- function() {
  v <- vapply(ls(envir=globalenv()),
              function(v) {
                if (inherits(get(v, envir=globalenv()), "data.frame")) {
                  v
                } else {
                  ""
                }
              },
              character(1),
              USE.NAMES = FALSE)
  v <- v[v != ""]
# Add nlmixr2 datasets 
c(v, "theo_sd", "theo_md")
}
getDataForExploration <- function(d) {
  get(d, envir = globalenv())
}

# Assume the global environment contains data.frames df1, df2
data_names <- getDataNamesForExploration()
print(data_names)

ui <- fluidPage(
  titlePanel("Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", choices = getDataNamesForExploration()),
      actionButton("loadData", "Load Data")
    ),
    mainPanel(
      tableOutput("dataTable")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store the selected data
  selectedData <- reactiveVal(NULL)
  
  # Observe when to load the selected dataset
  observeEvent(input$loadData, {
    selectedData(getDataForExploration(input$dataset))
  })
  
  # Render the table of the selected dataset
  output$dataTable <- renderTable({
    req(selectedData())  # Ensure selectedData is not NULL
    head(selectedData())  # Display the first few rows
  })
}

shinyApp(ui, server)