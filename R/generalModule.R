#' The genUI function
#' 
#' @description The main UI function for the nlmixr2Shiny app using Shiny (no miniUI).
#' 
#' @return A Shiny UI object.
#' @import shiny
#' @import shinyjs
#' @import waiter
#' @import shinyWidgets
#' @import rhandsontable
#' @import DT
genUi <- function() {
  ui <- fluidPage(
    useShinyjs(),
    useWaiter(),
    
    # Custom CSS to manage margins and enhance the display
    tags$style(HTML("
      .content { 
        margin: 15px;  /* Adds margin around the content */
      }
      .navbar { 
        background-color: #f7f7f7;
        border-bottom: 2px solid #e5e5e5;
      }
      .navbar-brand {
        display: flex;
        align-items: center;
      }
      .app-logo {
        max-height: 40px;  /* Logo size adjustment */
        margin-right: 10px;
      }
    ")),
    
    # Navigation bar with the title and logo
    navbarPage(
      title = div(
        tags$img(src=paste0("data:image/png;base64,",xfun::base64_encode(system.file("logonlmixr.png", package = "nlmixr2shiny"))),height=40)
      ),
      id = "mainTabs",
      
      # Tab for PKPD Model
      tabPanel("PKPD Model", icon = icon("cogs"), pkUI("pkpdModel")),
      
      # Tab for Model Property
      tabPanel("Model Property", icon = icon("wrench"), pkprUI("modelProperty")),
      
      # Tab for Parameter Estimate
      tabPanel("Parameter Estimate", icon = icon("calculator"), ParEstUI("parameterEstimate")),
      
      # Tab for Statistical Model
      tabPanel("Statistical Model", icon = icon("chart-bar"), covUI("covariancEstimate"))
      
      # Additional tab for Simulation if needed
      # tabPanel("Simulation", icon = icon("play"), pksimUI("simulation"))
    )
  )
  
  server <- function(input, output, session) {
    # Reactive values to store the intermediate results
    results <- reactiveValues(
      pkpdpipe = character(0),
      pkpdm = NULL,
      modProp = NULL,
      parEst = NULL,
      covarianceMat = NULL
    )
    
    # Call the respective server modules
    pkServer("pkpdModel", results)
    pkprServer("modelProperty", results)
    ParEstServer("parameterEstimate", results)
    covServer("covariancEstimate", results)
    
    # Monitor active tab and update results based on the selected tab
    observeEvent(input$mainTabs, {
      tab <- input$mainTabs
      
      if (tab == "Model Property") {
        waiter_show(html = tagList(
          spin_fading_circles(),  # A nice spinning loading indicator
          h4("Calculating, please wait...")
        ))
        
        # Retrieve stored PKPD inputs from results and process them
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
        
        pk_output <- ifelse(results$pk_switch, do.call(PKph, pk_values), "")
        pd_output <- ifelse(results$pd_switch, do.call(PDph, pd_values), "")
        pkpdmod <- ifelse(results$pk_switch || results$pd_switch, jPh(pk_output, pd_output), "")
        results$pkpdpipe <- pkpdmod
        
        # Evaluate pipeline for results$pkpdm
        results$pkpdm <- eval(str2lang(results$pkpdpipe))
        waiter_hide()
      }
      
      if (tab == "PKPD Model") {
        
        results$modProp <- NULL
        results$pkpdm <- NULL
      } else if (tab == "Model Property") {
        results$parEst <- NULL
      } else if (tab == "Parameter Estimate") {
        waiter_show(html = tagList(
          spin_fading_circles(),  # A nice spinning loading indicator
          h4("Calculating, please wait...")
        ))
        req(results$pkpdm)
        results$parEstim <- eval(str2lang(paste(c("results$pkpdm", results$modProp), collapse = "|>\n\t")))
        waiter_hide()
      } else if (tab == "Statistical Model") {
        waiter_show(html = tagList(
          spin_fading_circles(),  # A nice spinning loading indicator
          h4("Calculating, please wait...")
        ))
        req(results$parEstim)
        results$forCov <- eval(str2lang(paste(c("results$parEstim", results$ParEstimates), collapse = "|>\n\t")))
        waiter_hide()
      }
    })
  }
  
  # Create the Shiny app
  #shinyApp(ui = ui, server = server)
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("NLMixR2Shiny", width = 1200, height = 1200))
  
}
