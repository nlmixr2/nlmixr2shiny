# Fixed preset properties
addprop <- function(property=c("initial value", "bioavailability", "rate", "duration", "lag time"),
                    compartment){
  property <- match.arg(property)
  switch(property, "initial value"=paste("addIni(",compartment,")"),
         "bioavailability"=paste("addBioavailability(",compartment,")"),
         "rate"=paste("addRate(",compartment,")"),
         "duration"=paste("addDur(",compartment,")"),
         "lag time"=paste("addLag(",compartment,")"))
}
#' Convert model property to full property name in the UI
#'
#'
#' @param prop A character vector of model properties, e.g., "ini", "f", "alag", "dur", "rate".
#' @return A character vector of full property names.
#' @noRd
#' @author Matthew L. Fidler
modelPropToFullProp <- function(prop) {
  vapply(prop,
         function(v) {
           switch(v,
                  "ini"="initial value",
                  "f"="bioavailability",
                  "alag"="lag time",
                  "dur"="duration",
                  "rate"="rate")
         }, character(1), USE.NAMES = FALSE)
}
#'  Model Property Initial values (based on cmtProp)
#'
#'
#' @param cmtProp cmtProp from ui that tells what properties are in the model
#' @return An initial data frame with compartments and properties (fixed)
#' @noRd
#' @author Matthew L. Fidler
modelPropIni <- function(cmtProp) {
  if (is.null(cmtProp)) {
    data.frame(Compartment = character(0),
               Property = character(0),
               Remove = character(0),
               stringsAsFactors = FALSE)
  } else {
    data.frame(Compartment = cmtProp$Compartment,
                      Property = modelPropToFullProp(cmtProp$Property),
                      Remove = "Fixed",
                      stringsAsFactors = FALSE)
  }
}


pipeAllProp <- function(df){
  checkmate::assertDataFrame(df)
  df <- df[df$Property != "Fixed",, drop=FALSE ]
  if(nrow(df)==0){
    character(0)
  } else {
    vapply(1:nrow(df), function(i){
      addprop(df$Property[i],df$Compartment[i])},character(1))
  }
}


pkprUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    titlePanel("Add model Properties"),
    mainPanel(
      fluidRow(
        column(1, actionButton(ns("add_row"), label = HTML("&nbsp;"), icon = icon("plus")),
               style="column-width:150px; vertical-align: middle; align: center"),
        column(6, uiOutput(ns("compartment_ui")),
               style="column-width:55%; vertical-align: middle; align: center"),
        column(5, uiOutput(ns("property_ui")),
               style="column-width:45%; vertical-align: middle; align: center")
      ),
      fluidRow(column(12, DT::DTOutput(ns("table_output")))),
      fluidRow(
        column(12, HTML("&nbsp;"))
      ),
      fluidRow(
        column(12, actionButton(ns("copy_code"), "Copy Model Code"))
      )
    )
  )
}

pkprServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize table data to track compartment-property combinations
    table_data <- reactiveVal(
      data.frame(
        Compartment = character(0),
        Property = character(0),
        stringsAsFactors = FALSE
      )
    )

    # Dynamically determine fixed properties based on the model library excluding specific properties for central compartment
    determineFixedProperties <- function() {
      req(results$pkpdm)  # Ensure pkpdm is available

      fixed_rows <- data.frame(
        Compartment = character(0),
        Property = character(0),
        stringsAsFactors = FALSE
      )
      return(fixed_rows)
    }

    # Initialize fixed properties for the first compartment when applicable
    observe({
      fixed_rows <- determineFixedProperties()
      if (nrow(fixed_rows) > 0) {
        table_data(fixed_rows)
      }
    })

    # Update pipeline output
    updatePipeOutput <- function() {
      results$modProp <- pipeAllProp(
        table_data()  # Include all rows
      )
    }

    # Compartment selection UI
    output$compartment_ui <- renderUI({
      req(results$pkpdm)  # Ensure pkpdm is available
      selectInput(ns("compartment"), "Compartment", choices = results$pkpdm$state, width = "300px", selectize = FALSE, size = 5)
    })

    # Property selection UI
    output$property_ui <- renderUI({
      req(results$pkpdm)  # Ensure pkpdm is available
      selectInput(ns("property"), "Property", choices = c("initial value", "bioavailability", "rate", "duration", "lag time"), width = "300px", selectize = FALSE, size = 5)
    })

    # Adding rows to the table
    observeEvent(input$add_row, {
      new_row <- data.frame(Compartment = input$compartment, Property = input$property, stringsAsFactors = FALSE)

      td <- rbind(table_data(),
                  modelPropIni(results$pkpdm$props$cmtProp)[,1:2])
      if (nrow(subset(td,
                      Compartment == new_row$Compartment &
                        Property == new_row$Property)) > 0) {
        showModal(modalDialog(
          title = "Error",
          "This Compartment-Property combination already exists.",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        table_data(rbind(table_data(), new_row))
        updatePipeOutput()  # Update pipeline output when a row is added
      }
    })

    # Render data table
    output$table_output <- DT::renderDT({
      td <- table_data()
      td0 <- modelPropIni(results$pkpdm$props$cmtProp)
      if (inherits(td, "data.frame") && nrow(td) > 0) {
        # Add column Conditional Fixed or Remove Button
        td <- rbind(cbind(td, Remove = sprintf('<button class="btn btn-danger btn-sm delete" id="%s">-</button>', 1:nrow(td))),
                    td0)
      } else {
        td <- td0
      }

      # Check for properties no longer part of the model
      w <- which(!(td$Compartment %in% results$pkpdm$state))
      if (length(w) > 0) {
        showModal(modalDialog(
          title = "Warning",
          "Dropping compartment properties no longer in the model!",
          easyClose = TRUE,
          footer = NULL
        ))
        td <- td[-w,,drop=FALSE]
        if (nrow(td) == 0) {
          td <- data.frame(Compartment = character(0), Property = character(0), Remove = character(0), stringsAsFactors = FALSE)
        }
      }
      DT::datatable(td, escape = FALSE, selection = 'none',
                    rownames = FALSE,
                    options = list(dom = 't',
                                   ordering = FALSE,
                                   paging = FALSE))
    })

    # Handling delete button click in the table
    observeEvent(input$table_output_cell_clicked, {
      info <- input$table_output_cell_clicked
      if (!is.null(info$value) && grepl("delete", info$value)) {
        row_id <- as.numeric(gsub("\\D", "", info$value))
        table_data(table_data()[-row_id, ])
        updatePipeOutput()
      }
    })

    # Copy model code button functionality
    observeEvent(input$copy_code, {
      waiter::waiter_show(html = tagList(
        waiter::spin_fading_circles(),  # A nice spinning loading indicator
        h4("Calculating, please wait...")
      ))
      model_code <- paste("mod1 <- ",deparse(as.function(eval(str2lang(paste(c("results$pkpdm", results$modProp), collapse = "|>\n\t"))))), collapse="\n")
      rstudioapi::insertText(model_code)

      waiter::waiter_hide()
      stopApp()  # Close the app after the code is copied
    })
  })
}
