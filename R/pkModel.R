
# library(shiny)
# library(bslib)
# library(shinydashboard)



# PK UI Module
pkUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             selectInput(ns("Absorption"), "Absorption Method",
                         choices = c("IV/Infusion/Bolus", "First Order", "Transit"),
                         selectize = FALSE,
                         size = 3)
      ),
      column(4,
             selectInput(ns("Distribution"), "Distribution Model",
                         choices = c("1 compartment", "2 compartment", "3 compartment"),
                         selectize = FALSE,
                         size = 3)
      ),
      column(4,
             selectInput(ns("Elimination"), "Elimination Method",
                         choices = c("linear", "Michealis-Menton"),
                         selectize = FALSE,
                         size = 2)
      )
    ),
    fluidRow(
      column(3, numericInput(ns("ntransit"), "Number of Transit Compartments", value = 3, min = 1)),
      #column(3, checkboxInput(ns("linCmt"), "Linear Compartment", value = FALSE)),
      column(3, checkboxInput(ns("lag"), "Lag Time", value = FALSE)),
      column(3, checkboxInput(ns("fdepot"), "Fraction in Depot", value = FALSE))
    ),
    fluidRow(
      column(3, checkboxInput(ns("returnUi"), "Return UI", value = FALSE)),
      column(3, actionButton(ns("submit"), "Submit"))
    ),
    fluidRow(
      column(12, verbatimTextOutput(ns("final_summary"), placeholder = FALSE))
    )
  )
}

# PK Server Module
pkServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$submit, {
      output$final_summary <- renderPrint({
        rxode2::rxode2(readModelDb("PK_3cmt_des"))
      })
    })
  })
}




# main UI

ui <- dashboardPage(
  dashboardHeader(title = "Non Linear Mix Effect Model in R, NLMixR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("PK", tabName = "pk", icon = icon("flask")),
      menuItem("PD", tabName = "pd", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    theme = bs_theme(
      version = 4,
      bootswatch = "minty",
      bg = "#1a1d11",
      fg = "#f8f9fa",
      primary = "#3498ab",
      base_font = font_google("Roboto"),
      code_font = font_google("Source Code Pro")
    ),
    tabItems(
      tabItem(tabName = "pk",
              pkUI("pkModule")
      ),
      tabItem(tabName = "pd",
              pdUI("pdModule")
      )
      )
    )
  )


server <- function(input, output, session) {
  pkServer("pkModule")
}

shinyApp(ui, server)
