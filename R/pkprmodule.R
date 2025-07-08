# Fixed preset properties 
FIXED_PROPERTIES <- c("initial value", "rate") # List of fixed properties that can't be modified 

addprop <- function(property=c("initial value", "bioavailability", "rate", "duration", "lag time"),
                    compartment){
  property <- match.arg(property)
  switch(property, "initial value"=paste("addIni(",compartment,")"),
         "bioavailability"=paste("addBioavailability(",compartment,")"),
         "rate"=paste("addRate(",compartment,")"),
         "duration"=paste("addDur(",compartment,")"),
         "lag time"=paste("addLag(",compartment,")"))
}

pipeAllProp <- function(df){
  checkmate::assertDataFrame(df)
  if(nrow(df)==0){
    character(0)
  } else {
    vapply(1:nrow(df), function(i){
      addprop(df$Property[i],df$Compartment[i])},character(1))
    df <- df[df$Property != "Fixed", ]
  }
}


pkprUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
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
      fluidRow(column(12, DTOutput(ns("table_output")))),
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
      print(results$pkpdm$props)
      
      fixed_rows <- data.frame(
        Compartment = character(0),
        Property = character(0),
        stringsAsFactors = FALSE
      )
      
      if (nrow(table_data()) == 0) {
        fixed_props <- results$pkpdm$props[results$pkpdm$props %in% FIXED_PROPERTIES]
        
        if (length(fixed_props) > 0 && results$pkpdm$state[1] != "central") {
          fixed_rows <- data.frame(
            Compartment = rep(results$pkpdm$state[1], length(fixed_props)),  # Assign fixed properties to the initial compartment
            Property = fixed_props,
            stringsAsFactors = FALSE
          )
        } else if (length(fixed_props) > 0) {
          fixed_props <- fixed_props[!(fixed_props %in% c("initial value", "rate"))] # Remove specific properties for central compartment
          
          if (length(fixed_props) > 0) {
            fixed_rows <- data.frame(
              Compartment = rep(results$pkpdm$state[1], length(fixed_props)),  # Assign fixed properties to the initial compartment
              Property = fixed_props,
              stringsAsFactors = FALSE
            )
          }
        }
      }
      return(fixed_rows)
    }
    
    # Initialize fixed properties for the first compartment when applicable
    observe({
      fixed_rows <- determineFixedProperties()
      print(fixed_rows)
      if (nrow(fixed_rows) > 0) {
        table_data(fixed_rows)
      }
    })
    
    # Update pipeline output
    updatePipeOutput <- function() {
      results$modProp <- pipeAllProp(
        table_data(),  # Include all rows
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
      if (nrow(subset(table_data(), Compartment == new_row$Compartment & Property == new_row$Property)) > 0) {
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
    output$table_output <- renderDT({
      td <- table_data()
      if (inherits(td, "data.frame") && nrow(td) > 0) {
        # Add column Conditional Fixed or Remove Button
        # td <- cbind(td, Remove = ifelse(td$Property %in% FIXED_PROPERTIES & !(td$Property %in% c("initial value", "rate") & td$Compartment == "central"), "Fixed", sprintf('<button class="btn btn-danger btn-sm delete" id="%s">-</button>', 1:nrow(td))))
      } else {
       
        if (is.null(results$pkpdm$props$cmtProp)) {
          td <- data.frame(Compartment = character(0), Property = character(0), Remove = character(0), stringsAsFactors = FALSE)
        } else {
          td <- data.frame(Compartment = results$pkpdm$props$cmtProp$Compartment, 
                           Property = results$pkpdm$props$cmtProp$Property, Remove = "Fixed", stringsAsFactors = FALSE)
        }
          
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
      
      datatable(td, escape = FALSE, selection = 'none', rownames = FALSE,
                options = list(dom = 't', ordering = FALSE, paging = FALSE))
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
      waiter_show(html = tagList(
        spin_fading_circles(),  # A nice spinning loading indicator
        h4("Calculating, please wait...")
      ))
      model_code <- paste("mod1 <- ",deparse(as.function(eval(str2lang(paste(c("results$pkpdm", results$modProp), collapse = "|>\n\t"))))), collapse="\n")
      rstudioapi::insertText(model_code)
      
      waiter_hide()
      stopApp()  # Close the app after the code is copied
    })
  })
}

 
 
 