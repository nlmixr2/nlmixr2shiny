#' This function is supposed to calculate the initial model.
#'
#' @param results The Shiny lists that contains the results.
#'
#' @return nothing called for side effects
#' @noRd
#'
#'
calculatingInitialModel <-function(results){
  if (is.null(results$pkpdm)){

    waiter_show(html = tagList(
      spin_fading_circles(),  # A nice spinning loading indicator
      h4("Calculating initial model...")
    ))

    # Evaluate pipeline for results$pkpdm

    results$pkpdm <- eval(str2lang(results$pkpdpipe))

    waiter_hide()
  }
}


#' This function is supposed to calculate the statistical model.
#'
#' @param results This Shiny list contains the results.
#'
#' @return nothing called for side effects
#' @noRd
#'
#'
calculatingStatisticalModel <-function(results){
  if (is.null(results$forCov)){
    waiter_show(html = tagList(
      spin_fading_circles(),  # A nice spinning loading indicator
      h4("Calculating statistical model...")
    ))

    # Evaluate the pipeline for results$forCov
    results$forCov <- eval(str2lang(paste(c("results$parEstim", results$ParEstimates), collapse = "|>\n\t")))

    waiter_hide()
  }
  
  calculatingExploreData <-function(results) {
    if(is.null(results$forCov)){
      waiter_show(html = tagList(
        spin_seven_circle(), # A nice spinning loading indicator
        h4("Calculating explore data...")
      ))
      
      # Evaluate the pipeline for results$forCov
      results$forCov <- eval(str2lang(paste(c("results$parEstim", results$ParEstimates), collapse = "|>\n\t")))
      
      waiter_hide()
    }
  }
}

calculatingParameterEstimate <-function(results) {
  if (is.null(results$ParaEstim)) {
    waiter_show(html = tagList(
      spin_fading_circles(),  # A nice spinning loading indicator
      h4("Calculating parameter estimates...")
    ))

    # Evaluate the pipeline for results$ParaEstim
    results$parEstim <- eval(str2lang(paste(c("results$pkpdm", results$modProp), collapse = "|>\n\t")))

    waiter_hide()
  }

}


#' The nlmixr2model function
#'
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
#' @import nlmixr2lib
#' @export
nlmixr2model <- function() {
  library(nlmixr2lib)(
    useShinyjs(),
    useWaiter(),
    
    # Custom CSS for styling margins and layout
    tags$style(HTML("
      .content {
        margin: 15px;  /* Adds margin to the content */
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
        max-height: 40px;  /* Adjust logo size */
        margin-right: 10px;
      }
    ")),
    
    navbarPage(
      title = div(
        tags$img(
          src = paste0("data:image/png;base64,", xfun::base64_encode(system.file("logonlmixr.png", package = "nlmixr2shiny"))),
          height = 40
        )
      ),
      id = "mainTabs",
      
      # Tab for PKPD Model
      tabPanel("PKPD Model", icon = icon("cogs"), pkUI("pkpdModel")),
      
      # Tab for Model Property
      tabPanel("Model Property", icon = icon("wrench"), pkprUI("modelProperty")),
      
      # Tab for Parameter Estimate
      tabPanel("Parameter Estimate", icon = icon("calculator"), ParEstUI("parameterEstimate")),
      
      # Tab for Statistical Model
      tabPanel("Statistical Model", icon = icon("chart-bar"), covUI("covariancEstimate")),
      
      # Tab for Explore Data
      tabPanel("Explore Data", icon = icon("play"), expUI("exploreData"))
    )
  )
  
  return(ui)
}
     
server <- function(input, output, session) {
  # Reactive values to store intermediate results
  results <- reactiveValues(
    pkpdpipe = character(0),
    pkpdm = NULL,
    modProp = NULL,
    parEst = NULL,
    covarianceMat = NULL,
    forCov = NULL
  )
  
  # Call server modules with placeholder logic
  pkServer("pkpdModel", results)
  pkprServer("modelProperty", results)
  ParEstServer("parameterEstimate", results)
  covServer("covariancEstimate", results)
  
  # Monitor active tabs and update reactive results
  observeEvent(input$mainTabs, {
    tab <- input$mainTabs
    if (tab == "PKPD Model") {
      results$parEst <- NULL
      results$modProp <- NULL
      results$pkpdm <- NULL
    } else if (tab == "Model Property") {
      results$modProp <- "Model Property Results"
    } else if (tab == "Parameter Estimate") {
      results$parEst <- "Parameter Estimate Results"
    } else if (tab == "Statistical Model") {
      results$covarianceMat <- "Statistical Model Results"
    }
  })
}

pkServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    output$pkpdOutput <- renderText("PKPD Model logic goes here!")
  })
}

pkprServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    output$modelPropOutput <- renderText("Model Property logic goes here!")
  })
}

ParEstServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    output$parEstimateOutput <- renderText("Parameter Estimate logic goes here!")
  })
}

covServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    output$covEstimateOutput <- renderText("Statistical Model logic goes here!")
  })
}

expUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Explore Data"),
    fluidRow(
      column(12, textOutput(ns("exploreDataOutput")))
    )
  )
}


  shiny::runGadget(ui, server, viewer = shiny::dialogViewer(
    dialogName = "nlmixr2shiny",
    width = 4500,
    height = 3500
  ))



