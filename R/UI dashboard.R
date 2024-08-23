#' This function runs the model builder
#'
#' @return nothing, called for side effects
#' @export
#' 
#' @import shinydashboard
#' @import shiny
#'
#'
#' 
runModLib <- function(){

  ui <- dashboardPage(
    dashboardHeader(title = "nlmixr2Shiny"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("PK", tabName = "pk", icon = icon("flask")),
        menuItem("PD", tabName = "pd", icon = icon("heartbeat")),
        menuItem("PKPD", tabName = "pkpd", icon = icon("project-diagram"))
      )
    ),
    dashboardBody(
      tags$head(
        tags$style(HTML("
        .shiny-input-container label, 
        .shiny-input-container select,
        .shiny-input-container input {
          font-size: 16px;
        }
        .shiny-input-container input[type='checkbox'] {
          width: 30px;
          height: 20px;
        }
        .shiny-input-container .control-label {
          font-size: 16px;
        }
      "))
      ),
      tabItems(
        tabItem(tabName = "pk",
                pkUI("pkModule")
        ),
        tabItem(tabName = "pd",
                pdUI("pdModule")
        ),
        tabItem(tabName = "pkpd",
                pkpdUI("pkpdModule")
        )
      )
    )
  )
  
  # Server
  server <- function(input, output, session) {
    pkServer("pkModule")
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}
