
# pkSelectionUI function
pkSelectionUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("PK Model"),
    selectInput(ns("pk_input"), "Choose PK Parameter", choices = c("Option 1", "Option 2"))
  )
}

# pkSelection server logic
pkSelection <- function(input, output, session) {
  observeEvent(input$pk_input, {
    print(paste("PK Parameter selected:", input$pk_input))
  })
}



# pdSelectionUI function
pdSelectionUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("PD Selection"),
    selectInput(ns("pd_input"), "Choose PD Parameter", choices = c("Option A", "Option B"))
  )
}

# pdSelection server logic
pdSelection <- function(input, output, session) {
  observeEvent(input$pd_input, {
    print(paste("PD Parameter selected:", input$pd_input))
  })
}



# modelPropertiesUI function
modelPropertiesUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Model Properties and Theta Estimates"),
    numericInput(ns("theta1"), "Theta 1", value = 0),
    numericInput(ns("theta2"), "Theta 2", value = 0)
  )
}

# modelProperties server logic
modelProperties <- function(input, output, session) {
  observe({
    print(paste("Theta 1:", input$theta1, "Theta 2:", input$theta2))
  })
}


etaEstimatesUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Eta Estimates"),
    numericInput(ns("eta1"), "Eta 1", value = 0),
    numericInput(ns("eta2"), "Eta 2", value = 0)
  )
}

etaEstimates <- function(input, output, session) {
  observe({
    print(paste("Eta 1:", input$eta1, "Eta 2:", input$eta2))
  })
}



library(shiny)

ui <- fluidPage(
  titlePanel("nlmixR2shiny with tabset skeletal view"),
    mainPanel(
      tabsetPanel(
        tabPanel("PK Selection", pkSelectionUI("pk_selection")),
        tabPanel("PD Selection", pdSelectionUI("pd_selection")),
        tabPanel("Model Properties", modelPropertiesUI("model_properties")),
        tabPanel("Eta Estimates", etaEstimatesUI("eta_estimates")),
        tabPanel("Model Name", etaEstimatesUI("model_name")),
    )
  )
)

server <- function(input, output, session) {
  callModule(pkSelection, "pk_selection")
  callModule(pdSelection, "pd_selection")
  callModule(modelProperties, "model_properties")
  callModule(etaEstimates, "eta_estimates")
}

shinyApp(ui = ui, server = server)
