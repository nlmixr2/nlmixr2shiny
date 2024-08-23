
vars <- new.env(parent = emptyenv())
vars$table <- data.frame(Compartment = character(0), Property = character(0))


params <- data.frame(
  parameter = "ka",
  #`Estimate Name` = "lka",
  `Trans.` = factor("None"),
  # `Trans. Low` = -Inf,
  # `Trans. Hi` = Inf,
  `Low` = -Inf,
  `Est` = 1,
  `Hi` = Inf,
  `fixed` = TRUE,
  label=c("params"),
  # `eta` = FALSE,
  # `eta est` = 0.0,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

params <- params
vars$params <- params


pkprUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),  # Include shinyjs for JavaScript functionalities
    titlePanel("nlmixr2shiny"),
    mainPanel(
      fluidRow(
        column(1, actionButton(ns("add_row"), label = HTML("&nbsp;"), icon = icon("plus")),
               style="column-width:150px; vertical-align: middle; align: center"),
        column(6, uiOutput(ns("compartment_ui")),
               style="column-width:55%; vertical-align: middle; align: center"),
        column(5, uiOutput(ns("property_ui")),
               style="column-width:45%; vertical-align: middle; align: center")
      ),
      fluidRow(column(12,
               DTOutput(ns("table_output"))
      )
      ),
      fluidRow(
        column(12, HTML("&nbsp;"))
      ),
      fluidRow(
        column(12, rHandsontableOutput(ns("parameters")))
      )
    )
  )
}


# Server function for the module
pkprServer <- function(id) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    table_data <- reactiveVal(vars$table)
    
    
    # Render UI elements
    output$compartment_ui <- renderUI({
      selectInput(ns("compartment"), "Compartment", choices =rxode2::rxode2(readModelDb("PK_3cmt_des"))$state, width="300px")
    })
    
    output$property_ui <- renderUI({
      selectInput(ns("property"), "Property", choices = c("initial value", "bioavailability", "rate", "duration", "lag time"), width="300px")
    })
    
    # Add row to the table
    observeEvent(input$add_row, {
      new_row <- data.frame(Compartment = input$compartment, Property = input$property, stringsAsFactors = FALSE)
      # Check for duplicates
      if (nrow(subset(table_data(), Compartment == new_row$Compartment & Property == new_row$Property)) > 0) {
        showModal(modalDialog(
          title = "Error",
          "This Compartment-Property combination already exists.",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        vars$table <- rbind(vars$table, new_row)
        table_data(vars$table)
      }
    })
    
    output$table_output <- renderDT({
      td <- table_data()
      if(inherits(td,"data.frame")&&nrow(td)>0){
       td <- cbind(table_data(), Remove = sprintf('<button class="btn btn-danger btn-sm delete" id="%s">-</button>', 1:nrow(table_data())))
      } else{
        td <- data.frame()
      }
      datatable(td,
        escape = FALSE, selection = 'none', rownames = FALSE,
        options = list(dom = 't', ordering = FALSE, paging = FALSE)
      )
    })
    
    output$parameters <- renderRHandsontable({
      rhandsontable(params, width = 600, height = 300) %>%
        hot_col(col="Trans.", type = "dropdown", source = c("None", "Lognormal","Logit", "Probit"))
    })
    
    observeEvent(input$table_output_cell_clicked, {
      info <- input$table_output_cell_clicked
      if (!is.null(info$value) && grepl("delete", info$value)) {
        row_id <- as.numeric(gsub("\\D", "", info$value))
        vars$table <- vars$table[-row_id, ]
        table_data(vars$table)
      }
    })
  })
}




