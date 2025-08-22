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
      column(12, uiOutput(ns("parameterSliders")))
    ),
    actionButton(ns("updateModel"), "Update Model with Sliders", class = "btn-success"),
    
    br(),
    
    fluidRow(
      column(12, plotOutput(ns("dataPlot")))
    ),
    
    br(),
    
    fluidRow(
      column(12, DTOutput(ns("dataPreview")))
  )
)}

#--------------------------------------------------
# 4. Server Module
#--------------------------------------------------
expServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for storing model and simulation output
    rxState <- reactiveValues(model = NULL, solved = NULL)
    
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
        
        print(results$forCov$iniDf)
        # Render UI for parameter sliders
        output$parameterSliders <- renderUI({
          req(results$forCov)
          iniDf <- results$forCov$iniDf
          
          validate(need(nrow(iniDf) > 0, "Model contains no parameters."))
          
          sliders <- lapply(seq_len(nrow(iniDf)), function(i) {
            row <- iniDf[i, ]
            
            if (is.null(row$est) || is.na(row$est)) return(NULL)  # skip if no initial value
            
            # Rule-based bounds if missing or infinite
            lower <- if (!is.na(row$lower) && is.finite(row$lower)) row$lower else row$est * 0.5
            upper <- if (!is.na(row$upper) && is.finite(row$upper)) row$upper else row$est * 1.5
            
            # Catch case where est = 0 (e.g., propSd)
            if (row$est == 0) {
              lower <- 0.01
              upper <- 1
            }
            
            # Force fallback values if still invalid
            if (!is.finite(lower) || !is.finite(upper) || lower >= upper) {
              lower <- 0.1
              upper <- 10
            }
            sliderInput(
              inputId = ns(paste0("slider_", row$name)),
              label = paste("Parameter:", row$name),
              min = 0.9 * row$est,
              max = 1.1 * row$est,
              value = row$est,
              step = 0.01 * row$est
            )
          })
          
          do.call(tagList, sliders)
        })
        
        observeEvent(input$updateModel, {
          req(results$forCov)
          req(selectedData())
          
          iniDf <- results$forCov$iniDf
          
          # Create a copy to modify 
          newIni <- iniDf
          
          for (i in seq_len(nrow(newIni))) {
            paramName <- newIni$name[i]
            sliderId <- paste0("slider_", paramName)
            if (!is.null(input[[sliderId]])) {
              newIni$est[i] <- input[[sliderId]]
            }
          }
          
          # Update the model parameters 
          ini(results$forCov) <- newIni
          
          # Rerun simulation 
          newSim <- rxSolve(results$forCov, selectedData(), keep = "DV")
          results$s <- newSim
        })   
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