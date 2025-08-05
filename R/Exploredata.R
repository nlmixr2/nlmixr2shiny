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

# Define the plot module UI
plotModuleUI <- function(id) {
  ns <- NS(id) # Namespace for the module
  tagList(
    plotOutput(ns("plot")) # Output placeholder for the plot
  )
}

# Define the plot module server 
plotModuleServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$plot <- renderPlot({
      ggplot(data, aes(x = TIME, y = effect)) +
        geom_point(color = "blue", size = 3, alpha = 0.8) +
        facet_wrap_paginate(~ID, ncol = 2, nrow = 2, page = input$page) +
        labs(
          title = paste("Explore PK/PD Data - Page", input$page),
          x = "Time",
          y = "Effect"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    })
  })
}
# UI Module for Explore Data
expUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        selectInput(
          ns("dataset"), "Select Dataset:",
          choices = getDataNamesForExploration(),
          selected = NULL
        )
      ),
      column(
        4,
        actionButton(ns("loadData"), "Load Dataset", class = "btn-primary")
      ),
      column(
        4,
        sliderInput(
          ns("page"), "Select Plot Page:", value = 1, min = 1, max = 1, step = 1
        )
      )
    ),
    fluidRow(
      column(12, tableOutput(ns("dataPreview")))
    ),
    fluidRow(
      column(12, plotOutput(ns("dataPlot")))
    ),
    fluidRow(
      column(12, plotModuleUI(ns("dataModulePlot")))
    )
  )
}

# Server Module for Explore Data
expServer <- function(id, results, UI) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store the selected dataset
    selectedData <- reactiveVal(NULL)
    
    # Observe the Load Dataset event
    observeEvent(input$loadData, {
      req(input$dataset)  # Ensure a dataset is selected
      
      # Dynamically fetch the selected dataset
      dataset <- getDataForExploration(input$dataset)
      
      # Validate that the dataset has required columns
      if (!"TIME" %in% names(dataset) || !"ID" %in% names(dataset)) {
        stop("The dataset must include `TIME` and `ID` columns to continue.")
      }
      # browser()
      # Store the dataset in a reactive value
      selectedData(dataset)
      print(selectedData)
      
      # Calculate the number of pages required for pagination
      num_ids <- length(unique(dataset$ID))
      facets_per_page <- 4  # Number of facets per page (e.g., 2x2 grid)
      num_pages <- ceiling(num_ids / facets_per_page)
      updateSliderInput(session, "page", min = 1, max = num_pages, value = 1)
      NULL
    })
    
    # Dynamically calculate PK/PD model outputs
    modelData <- reactive({
      req(selectedData())
      req(results$forCov)  # Ensure results$forCov is a valid function
      
      dataset <- selectedData()
      
      # Apply model function to dataset dynamically
      dataset <- rxSolve(results$forCov,selectedData())
      
      dataset  # Return modified dataset
    })
    
    
    
    # Render table preview
    output$dataPreview <- renderTable({
      req(modelData())  # Ensure model data exists
      head(modelData())  # Display the first few rows of augmented data
    })
    
    
    # Render paginated plot
    output$dataPlot <- renderPlot({
      req(modelData())
      augmentedDataset <- modelData()
      
      ggplot(augmentedDataset, aes(x = TIME, y = effect)) +
        geom_point(color = "blue", size = 3, alpha = 0.8) +
        facet_wrap_paginate(~ID, ncol = 2, nrow = 2, page = input$page) +
        labs(
          title = paste("Explore PK/PD Data - Page", input$page),
          x = "Time",
          y = "Effect"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
        
      
    })
  })
}
