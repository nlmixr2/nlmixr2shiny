#' This function is supposed to calculate the initial model.
#'
#' @param results The Shiny lists that contains the results.
#'
#' @return nothing called for side effects
#'
#' @noRd
calculatingInitialModel <-function(results) {
  .saved <- FALSE
  if (!is.null(results$ace)) {
    # Save model, but require an evaluation/parse
    if (results$ace != "") {
      .env <- new.env(parent=globalenv())
      eval(str2lang(results$ace), envir = .env)
      .ls <- ls(.env)
      if (length(.ls) == 1L) {
        resetInitialModel(results)
        .mod <- try(rxode2::rxode2(get(.ls, envir = .env)), silent=TRUE)
        if (!inherits(.mod, "try-error")) {
          .saved <- TRUE
          results$pkpdm <- results$parEstim <- .mod
        }
      }
      if (!.saved) {
        showNotification("Error parsing model from editor. Please check the syntax.", type = "error")
      }
    }
    results$ace <- NULL
  }
  if (is.null(results$pkpdm)) {
    waiter::waiter_show(html = tagList(
      waiter::spin_fading_circles(),  # A nice spinning loading indicator
      h4("Calculating initial model...")
    ))
    on.exit({waiter::waiter_hide()})
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
  results$rxsolve <- NULL
  results$iniDf <- NULL
  results$paramNames <- NULL
  results$backTransform <- NULL
  results$modProp <- NULL
  results$parEstim <- NULL
  results$fullOmegaShiny <- NULL
}

#' Calculate the parameter estimates table
#'
#' @param results results that allow the calculation of the parameter estimates table
#' @return nothing, called for side effects
#' @noRd
calculatingParameterEstimate <-function(results) {
  if (is.null(results$parEstim)) {
    waiter::waiter_show(html = tagList(
      waiter::spin_fading_circles(),  # A nice spinning loading indicator
      h4("Calculating parameter estimates...")
    ))
    on.exit({waiter::waiter_hide()})

    # Evaluate the pipeline for results$ParaEstim
    results$parEstim <- eval(str2lang(paste(c("results$pkpdm", results$modProp), collapse = "|>\n\t")))
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
#' @importFrom stats setNames
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

      # Tab for Statistical Model
      tabPanel("Statistical Model", icon = icon("chart-bar"), covUI("covariancEstimate")) ,

      # Tab for Parameter Estimate
      tabPanel("Population Estimates", icon = icon("calculator"),
               ParEstUI("parameterEstimate")),

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
      covarianceMat = NULL
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
        updateOmegaInModel(results)
        updateParEstimWithEsts(results)
        updateResidInModel(results)
      } else if (tab == "Model Property") {
        calculatingInitialModel(results)
        updateOmegaInModel(results)
        updateParEstimWithEsts(results)
        updateResidInModel(results)
      } else if (tab == "Population Estimates") {
        calculatingInitialModel(results)
        updateOmegaInModel(results)
        calculatingParameterEstimate(results)
        updateParEstimWithEsts(results)
        updateResidInModel(results)
        req(results$pkpdm)
      } else if (tab == "Statistical Model") {
        calculatingInitialModel(results)
        updateOmegaInModel(results)
        calculatingParameterEstimate(results)
        updateParEstimWithEsts(results)
        updateResidInModel(results)
      } else if (tab == "Edit/Insert") {
        calculatingInitialModel(results)
        calculatingParameterEstimate(results)
        updateOmegaInModel(results)
        updateParEstimWithEsts(results)
        updateResidInModel(results)
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

.dummy <- function() {
  # This is a dummy function to import nlmixr2est explicitly
  nlmixr2est::nlmixr()
}
