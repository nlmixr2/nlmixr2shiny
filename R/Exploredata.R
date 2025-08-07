#' Get the data for Exploring the model
#'
#' @return A character vector of the names of the data sets.
#' @noRd
#' @author Dyani Peterson 
#' @examples
#' getDataNamesForExploration()

#--------------------------------------------------
# 2. Utility Functions
#--------------------------------------------------
getDataNamesForExploration <- function() {
  v <- vapply(ls(envir = globalenv()),
              function(v) {
                if (inherits(get(v, envir = globalenv()), "data.frame")) v else ""
              },
              character(1),
              USE.NAMES = FALSE)
  v <- v[v != ""]
  c(v, "theo_sd", "theo_md") # Add built-in datasets
}

getDataForExploration <- function(dataset_name) {
  if (exists(dataset_name, envir = globalenv())) {
    return(get(dataset_name, envir = globalenv()))
  } else {
    stop(paste("Dataset not found:", dataset_name))
  }
}

#--------------------------------------------------
# 3. UI Module
#--------------------------------------------------
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
        sliderInput(ns("page"), "Select Plot Page:", value = 1, min = 1, max = 1, step = 1)
      )
    ),
    br(),
    fluidRow(
      column(12, plotOutput(ns("dataPlot")))
    ),
    br(),
    fluidRow(
      column(12, DTOutput(ns("dataPreview"))) # Show first 50 rows of rxSolve result
    )
  )
}

#--------------------------------------------------
# 4. Server Module
#--------------------------------------------------
expServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive storage for selected dataset
    selectedData <- reactiveVal(NULL)
    
    # Load dataset on button click
    observeEvent(input$loadData, {
      req(input$dataset)
      tryCatch({
        dataset <- getDataForExploration(input$dataset)
        
        # Validate columns
        requiredCols <- c("ID", "TIME", "AMT", "EVID", "CMT", "DV")
        if (!all(requiredCols %in% names(dataset))) {
          stop(paste("Dataset must include columns:", paste(requiredCols, collapse = ", ")))
        }
        
        # Solve the dataset
        print(results$forCov)
        s = rxSolve(results$forCov, dataset, keep = "DV")
        
        # Store the results 
        results$s = s
        # Save dataset
        selectedData(dataset)
        
        # Update pagination slider
        num_ids <- length(unique(dataset$ID))
        facets_per_page <- 4
        num_pages <- ceiling(num_ids / facets_per_page)
        updateSliderInput(session, "page", min = 1, max = num_pages, value = 1)
        
      }, error = function(e) {
        showNotification(paste("Error loading dataset:", e$message), type = "error")
      })
    })
    
  
    # Reactive: Solve the model
    # augmentedDataset <- reactive({
    #   req(selectedData())
    #   dataset <- selectedData()
    #   
    #   tryCatch({
    #     suppressMessages(rxSolve(model, dataset))
    #   }, error = function(e) {
    #     showNotification(paste("Model error:", e$message), type = "error")
    #     NULL
    #   })
    # })
    
    
    # Render the PK/PD plot
    output$dataPlot <- renderPlot({
      req(results$s)
      print(results$s)
      gg = ggplot(results$s, aes(x = time, y = DV)) +
        geom_point() +
        ggforce::facet_wrap_paginate(~id, ncol = 2, nrow = 2, page = input$page) +
        geom_line(aes(x = time, y = ipredSim)) +
        rxode2::rxTheme()
     # gg = ggplot(results$s, aes(x = time, y = ipredSim )) +
     #    geom_point(aes(x= time, y = DV), size = 3, alpha = 0.8) +
     #    geom_line() +
     #    facet_wrap_paginate(id, ncol = 2, nrow = 2, page = input$page) +
     #    labs(
     #      title = paste("Explore PK/PD Data - Page", input$page),
     #      x = "Time",
     #      y = "ipredSim",
     #      
     #    ) +
     #    theme_minimal() +
     #    theme(plot.title = element_text(hjust = 0.5))
     print(gg)
     gg
    })
    
    
  })
}