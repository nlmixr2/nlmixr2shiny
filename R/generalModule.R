#' This function is supposed to calculate the initial model.
#'
#' @param results The Shiny lists that contains the results.
#'
#' @return nothing called for side effects
#'
#' @noRd
calculatingInitialModel <-function(results){
  if (is.null(results$pkpdm)){
    waiter::waiter_show(html = tagList(
      waiter::spin_fading_circles(),  # A nice spinning loading indicator
      h4("Calculating initial model...")
    ))

    # Evaluate pipeline for results$pkpdm
    results$pkpdm <- eval(str2lang(results$pkpdpipe))

    waiter::waiter_hide()
  }
}

#' This function is supposed to calculate the statistical model.
#'
#' @param results This Shiny list contains the results.
#'
#' @return nothing called for side effects
#' @noRd
calculatingStatisticalModel <-function(results){
  if (is.null(results$forCov)){
    waiter::waiter_show(html = tagList(
      waiter::spin_fading_circles(),  # A nice spinning loading indicator
      h4("Calculating statistical model...")
    ))

    # Evaluate the pipeline for results$forCov
    results$forCov <- eval(str2lang(paste(c("results$parEstim", results$ParEstimates), collapse = "|>\n\t")))

    waiter::waiter_hide()
  }

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
}

calculatingParameterEstimate <-function(results) {
  if (is.null(results$ParaEstim)) {
    waiter::waiter_show(html = tagList(
      waiter::spin_fading_circles(),  # A nice spinning loading indicator
      h4("Calculating parameter estimates...")
    ))

    # Evaluate the pipeline for results$ParaEstim
    results$parEstim <- eval(str2lang(paste(c("results$pkpdm", results$modProp), collapse = "|>\n\t")))

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
      tabPanel("Parameter Estimate", icon = icon("calculator"),
               ParEstUI("parameterEstimate")),

      # Tab for Statistical Model
      tabPanel("Statistical Model", icon = icon("chart-bar"), covUI("covariancEstimate")),

      # Tab for Explore Data
      tabPanel("Explore Data", icon = icon("play"), expUI("exploreData"))

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
      covarianceMat = NULL,
      forCov = NULL # This is supposed to match calculations in 'calculatingStatisticalModel
    )

    # Call the respective server modules
    pkServer("pkpdModel", results)
    pkprServer("modelProperty", results)
    ParEstServer("parameterEstimate", results)
    covServer("covariancEstimate", results)
    expServer("exploreData", results)

    # Monitor active tab and update results based on the selected tab
    observeEvent(input$mainTabs, {
      tab <- input$mainTabs


      if (tab == "PKPD Model") {
        results$parEst <- NULL
        results$modProp <- NULL
        results$pkpdm <- NULL
      } else if (tab == "Model Property") {
        calculatingInitialModel(results)
        results$parEst <- NULL
      } else if (tab == "Parameter Estimate") {
        calculatingInitialModel(results)
        calculatingParameterEstimate(results)
        results$ParaEstim <- NULL
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
      }
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer(
    dialogName = "nlmixr2shiny",
    width = 4500,
    height = 3500
  ))


}
