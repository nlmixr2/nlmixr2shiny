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

# Obtain a dataset by its name dynamically
getDataForExploration <- function(dataset_name) {
  if (exists(dataset_name, envir = globalenv())) {
    return(get(dataset_name, envir = globalenv()))
  } else {
    stop(paste("Dataset not found:", dataset_name))
  }
}

# Define the `results` list with `forCov` integrated
results <- list(
  forCov = function(data) {
    # Model predictions dynamically based on TIME
    if (!"TIME" %in% names(data)) {
      stop("Dataset must include a `TIME` column.")
    }
    # Dynamic prediction logic
    ifelse(data$dataset == "Single-Dose",
           12 * exp(-0.3 * data$TIME),  # Single-Dose decay model
           10 * exp(-0.2 * data$TIME))  # Multi-Dose decay model
  }
)

# UI definition
ui <- fluidPage(
  titlePanel("Dynamic Exploratory Data Analysis with forCov Integration"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select a dataset:", choices = getDataNamesForExploration()),  # Dataset selector
      actionButton("loadData", "Load Dataset"),  # Button to load dataset
      sliderInput("page", "Page:", value = 1, min = 1, max = 1, step = 1)  # Pagination slider
    ),
    mainPanel(
      tableOutput("dataTable"),  # Render selected dataset in a table
      plotOutput("dataPlot")     # Render paginated ggplot
    )
  )
)

# Server definition
server <- function(input, output, session) {
  # Reactive value for processed dataset
  selectedData <- reactiveVal(NULL)
  
  # Observe dataset loading event
  observeEvent(input$loadData, {
    req(input$dataset)
    
    # Dynamically load the selected dataset
    data <- getDataForExploration(input$dataset)
    
    # Validation: Ensure essential columns exist
    if (!"TIME" %in% names(data) || !"ID" %in% names(data)) {
      stop("Dataset must contain columns `TIME` and `ID`.")
    }
    
    # Label the dataset based on its type (Single-Dose or Multi-Dose)
    if (input$dataset == "theo_sd") {
      data$dataset <- "Single-Dose"
    } else if (input$dataset == "theo_md") {
      data$dataset <- "Multi-Dose"
    }
    
    # Generate model predictions dynamically using `results$forCov`
    predictions <- results$forCov(data)
    if (length(predictions) != nrow(data)) {
      stop("Error: Number of predictions does not match number of rows in the dataset.")
    }
    data$estimate <- predictions
    
    # Store the processed dataset in a reactive value
    selectedData(data)
    
    # Update slider for pagination
    num_ids <- length(unique(data$ID))
    facets_per_page <- 4  # Configured as 2x2 grid
    num_pages <- ceiling(num_ids / facets_per_page)
    updateSliderInput(session, "page", min = 1, max = num_pages, value = 1)
  })
  
  # Render data table
  output$dataTable <- renderTable({
    req(selectedData())
    head(selectedData())
  })
  
  # Render paginated plot
  output$dataPlot <- renderPlot({
    req(selectedData())
    data <- selectedData()
    
    ggplot(data, aes(x = TIME)) +
      geom_point(aes(y = DV, color = "Observed Data"), size = 3) +
      geom_line(aes(y = estimate, color = "Model Prediction"), linetype = "dashed", size = 1) +
      facet_wrap_paginate(~ID, ncol = 2, nrow = 2, page = input$page) +
      labs(
        title = paste("Exploratory Data Analysis - Page", input$page),
        x = "Time (hours)",
        y = "Concentration (mg/L)",
        color = "Legend"
      ) +
      scale_color_manual(values = c("Observed Data" = "blue", "Model Prediction" = "red")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "top"
      )
  })
}

# Launch the app
shinyApp(ui, server)