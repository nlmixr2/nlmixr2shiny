#' This function is supposed to calculate the initial model.
#'
#' @param results The Shiny lists that contains the results.
#'
#' @return nothing called for side effects
#'
#' @noRd
calculatingInitialModel <-function(results) {
  if (!is.null(results$ace)) {
    # Save model, but require an evaluation/parse
    if (results$ace != "") {
      message("non empty ace")
      .env <- new.env(parent=globalenv())
      eval(str2lang(results$ace), envir = .env)
      .ls <- ls(.env)
      if (length(.ls) == 1L) {
        message("evaluates to one model, save model")
        resetInitialModel(results)
        results$pkpdm <- rxode2::rxode2(get(.ls, envir = .env))
      }
    }
    results$ace <- NULL
  }
  if (is.null(results$pkpdm)) {
    waiter::waiter_show(html = tagList(
      waiter::spin_fading_circles(),  # A nice spinning loading indicator
      h4("Calculating initial model...")
    ))
    if (isTRUE(results$modelModified)) {
    } else if (results$modelTypeSwitch == "Model Builder") {
      # Evaluate pipeline for results$pkpdm
      results$pkpdm <- eval(str2lang(results$pkpdpipe))
      results$modelModified <- TRUE
      results$modelTypeSwitch <- "Current Model"
    } else if (results$modelTypeSwitch == "Model Library") {
      # Evaluate the model from the model library
      .mod <- nlmixr2lib::readModelDb(results$modlibInput)
      if (!inherits(.mod, "rxUi")){
        .mod <- rxode2::rxode2(.mod)
      }
      results$pkpdm <- .mod
      results$modelModified <- TRUE
      results$modelTypeSwitch <- "Current Model"
    }

    # Reset the other results that depend on the initial model
    results$parEstim <- NULL
    results$forCov <- NULL
    waiter::waiter_hide()
  }
}
#' This resets the initial model calculations
#'
#'
#' @param results The Shiny lists that contains the results.
#'
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
resetInitialModel <- function(results) {
  if (!isTRUE(results$modelModified)) {
    results$pkpdm <- NULL
  }
  results$modProp <- NULL
  results$parEstim <- NULL
  results$forCov <- NULL
}

#' This function is supposed to calculate the statistical model.
#'
#' @param results This Shiny list contains the results.
#'
#' @return nothing called for side effects
#' @noRd
calculatingStatisticalModel <-function(results) {
  if (is.null(results$forCov)){
    waiter::waiter_show(html = tagList(
      waiter::spin_fading_circles(),  # A nice spinning loading indicator
      h4("Calculating statistical model...")
    ))

    # Evaluate the pipeline for results$forCov
    results$forCov <- eval(str2lang(paste(c("results$parEstim", results$ParEstimates), collapse = "|>\n\t")))

    waiter::waiter_hide()
  }
}
#' Calculate the parameter estimates table
#'
#' @param results results that allow the calculation of the parameter estimates table
#' @return nothing, called for side effects
#' @noRd
calculatingParameterEstimate <-function(results) {
  if (is.null(results$ParaEstim)) {
    waiter::waiter_show(html = tagList(
      waiter::spin_fading_circles(),  # A nice spinning loading indicator
      h4("Calculating parameter estimates...")
    ))

    # Evaluate the pipeline for results$ParaEstim
    results$parEstim <- eval(str2lang(paste(c("results$pkpdm", results$modProp), collapse = "|>\n\t")))
    results$forCov <- NULL
    waiter::waiter_hide()
  }
}
#' Calculate what is needed to explore the model with the data in the R environment
#'
#' @param results the results that need to be caluclated
#' @return
#' @export
#' @author Matthew L. Fidler
calculatingExploreData <-function(results) {
  if(is.null(results$forCov)) {
    waiter::waiter_show(html = tagList(
      waiter::spin_seven_circle(), # A nice spinning loading indicator
      h4("Calculating explore data...")
    ))

    # Evaluate the pipeline for results$forCov
    results$forCov <- eval(str2lang(paste(c("results$parEstim", results$ParEstimates), collapse = "|>\n\t")))

    waiter::waiter_hide()
  }
}



#' The nlmixr2model function
#'
#'
#' @description The main UI function for the nlmixr2Shiny app using Shiny (no miniUI).
#'
#' @return A Shiny UI object.
#' @import shiny
#' @import ggplot2
#' @import nlmixr2lib
#' @export
nlmixr2model <- function() {
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    waiter::useWaiter(),

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
      tabPanel("Population Estimates", icon = icon("calculator"),
               ParEstUI("parameterEstimate")),

      # Tab for Statistical Model
      tabPanel("Statistical Model", icon = icon("chart-bar"), covUI("covariancEstimate")) ,

      # Tab for model
      tabPanel("Edit/Insert", icon = icon("file-pen"), aceUI("editModel"))

      # Tab for Explore Data
      ## tabPanel("Explore Data", icon = icon("play"), expUI("exploreData"))

      # Additional tab for Simulation if needed
      # tabPanel("Simulation", icon = icon("play"), pksimUI("simulation"))
    )
  )

  server <- function(input, output, session) {
    # Reactive values to store the intermediate results
    ns <- session$ns
    results <- reactiveValues(
      pkpdpipe = character(0),
      pkpdm = NULL,
      modProp = NULL,
      parEst = NULL,
      covarianceMat = NULL,
      forCov = NULL # This is supposed to match calculations in 'calculatingStatisticalModel
    )

    # Call the respective server modules
    pkServer("pkpdModel", results)
    pkprServer("modelProperty", results)
    ParEstServer("parameterEstimate", results)
    covServer("covariancEstimate", results)
    #expServer("exploreData", results)
    aceServer("editModel", results)


    # Monitor active tab and update results based on the selected tab
    observeEvent(input$mainTabs, {
      tab <- input$mainTabs

      if (tab == "PKPD Model") {
        resetInitialModel(results)
      } else if (tab == "Model Property") {
        calculatingInitialModel(results)
      } else if (tab == "Population Estimates") {
        calculatingInitialModel(results)
        calculatingParameterEstimate(results)
        req(results$pkpdm)
      } else if (tab == "Statistical Model") {
        calculatingInitialModel(results)
        calculatingParameterEstimate(results)
        calculatingStatisticalModel(results)
        req(results$parEstim)
      } else if (tab == "Explore Data") {
        calculatingInitialModel(results)
        calculatingParameterEstimate(results)
        calculatingStatisticalModel(results)
        req(results$parEstim)
      } else if (tab == "Edit/Insert") {
        calculatingInitialModel(results)
        calculatingParameterEstimate(results)
        calculatingStatisticalModel(results)
        req(results$parEstim)
      }
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer(
    dialogName = "nlmixr2shiny",
    width = 4500,
    height = 3500
  ))


}
