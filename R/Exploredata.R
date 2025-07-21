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
  ))
  
# Ensure required columns exist before making predictions
if (!("dataset" %in% colnames(data)) || !("TIME" %in% colnames(data))) {
  stop("Dataset must contain 'dataset' and 'TIME'.")
}
# Dynamically calculate predictions based on 'dataset' column
data$estimate <- ifelse(
  data$dataset == "Single-Dose",
  12 * exp(-0.3 * data$TIME),  # Model for Single-Dose
  10 * exp(-0.2 * data$TIME)   # Model for Multi-Dose
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
# Add data identifiers for single-dose and multi-dose
theo_sd$dataset <- "Single-Dose"
theo_md$dataset <- "Multi-Dose"

# Combine the two datasets into one
combined_data <- rbind(theo_sd, theo_md)
head(combined_data)  # View the combined dataset structure
# Calculate total pages needed
num_ids <- length(unique(combined_data$ID))  # Count unique IDs
facets_per_page <- 4  # Number of facets per page (2x2 grid)
num_pages <- ceiling(num_ids / facets_per_page)

# Add model predictions 
combined_data$estimate <- ifelse(
  combined_data$dataset == "Single-Dose",
  12 * exp(-0.3 * combined_data$TIME),
  10 * exp(-0.2 * combined_data$TIME)
)
# Create the ggplot
for (i in 1:num_pages) {
  p <- ggplot(combined_data, aes(x = TIME)) +
    geom_point(aes(y = DV, color = dataset), size = 3, alpha = 0.8) +
    geom_line(aes(y = estimate, color = dataset), linetype = "dashed", linewidth = 1.2) +
    facet_wrap_paginate(~ID, ncol = 2, nrow = 2, page = i) +  # Pagination for facets
    labs(
      title = paste("Observed vs Model Estimates: Page", i),
      x = "Time (hours)",
      y = "Concentration (mg/L)",
      color = "Dataset"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "top"
    )
  
  # Print the current page
  print(p)
}
shinyApp(ui, server)