
#' The genUI function
#'
#' @description The main UI function for the nlmixr2Shiny app.
#'
#' @return A Shiny UI object.
#' @import shiny
#' @import shinyjs
#' @import waiter
#' @import shinyWidgets
#' @import shinydashboard
#' @import rhandsontable
#' @import DT
#' @import waiter
genUi <- function() {
  ui <- fluidPage(
    useShinyjs(),
    useWaiter(),
    titlePanel("nlmixr2Shiny"),
    tabsetPanel(
      id = "mainTabs",
      tabPanel("PKPD Model", pkUI("pkpdModel")),
      tabPanel("Model Property", pkprUI("modelProperty")),
      tabPanel("Parameter Estimate", ParEstUI("parameterEstimate")),
      tabPanel("Statistical Model", covUI("covariancEstimate"))
      # tabPanel("Simulation", pksimUI("simulation"))
    )
  )
  
  server <- function(input, output, session) {
    results <- reactiveValues(pkpdpipe = character(0),
                              pkpdm = NULL,
                              modProp = NULL,
                              parEst = NULL,
                              covarianceMat=NULL
    )
    
    # Call the PKPD model server
    pkServer("pkpdModel", results)
    
    # Call the Model Property server
    pkprServer("modelProperty", results)
    
    # Call the Parameter Estimate server
    ParEstServer("parameterEstimate", results)
    
    # Call the covariance server
    covServer("covariancEstimate", results)
    
    # Monitor active tab and update results$pkpdm when "Model Property" is active
    observeEvent(input$mainTabs, {
      tab <- input$mainTabs
      
      if (tab == "Model Property") {
        
        
        waiter_show(html = tagList(
          spin_fading_circles(),  
          h4("Calculating, please wait...")
        ))
        
        
        # Retrieve the stored PKPD inputs from results
        pk_values <- list(
          absorption_method = results$absorption_method,
          distribution_model = results$distribution_model,
          elimination_method = results$elimination_method,
          parameterization = results$parameterization
        )
        
        if (results$absorption_method == "Transit") {
          pk_values$transit_compartment <- results$transit_compartment
        }
        
        pd_values <- list(
          response_type = results$response_type,
          drug_action = results$drug_action
        )
        
        if (results$response_type %in% c("Direct/Immediate", "Effect Compartment")) {
          pd_values$baseline <- results$baseline
        } else if (results$response_type == "Indirect/Turnover") {
          pd_values$type_of_model <- results$type_of_model
        }
        
        if (length(results$drug_action) == 1 && results$drug_action %in% c("Emax", "Imax")) {
          pd_values$sigmoidicity <- results$sigmoidicity
        }
        if (length(results$response_type) == 1 && results$response_type == "Indirect/Turnover") {
          pd_values$par_bas <- results$par_bas
        }
        
        # # Compute the PK and PD models
        pk_output <- ifelse(results$pk_switch, do.call(PKph, pk_values), "")
        pd_output <- ifelse(results$pd_switch, do.call(PDph, pd_values),"")
        pkpdmod <- ifelse(results$pk_switch || results$pd_switch, jPh(pk_output, pd_output), "")
        results$pkpdpipe <- pkpdmod
        
        #Evaluate the pipeline to get results$pkpdm
        results$pkpdm <- eval(str2lang(results$pkpdpipe))
        
        waiter_hide()
        
        
        # } else {
        #   # If the pkpdpipe is empty, reset pkpdm
        #   results$pkpdm <- list(state = character(0))
        # }
      }
      
      # Reset pkpdm and modProp when switching to PKPD Model tab
      if (tab == "PKPD Model") {
        results$modProp <- NULL
        results$pkpdm <- NULL
      } else if (tab == "Model Property") {
        results$parEst <- NULL
      } else if(tab == "Statistical Model"){
        results$covarianceMat <- NULL
      }
    })
  }
  
  
  
  
  
  # Create the Shiny app
  shinyApp(ui = ui, server = server)
}