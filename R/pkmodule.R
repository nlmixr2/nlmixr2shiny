
pkUI <- function(id) {
  ns <- NS(id)
  tagList(
    materialSwitch(ns("pk_switch"), label = "PK", value = TRUE),
    conditionalPanel(
      condition = paste0("input['", ns("pk_switch"), "']"),
      fluidRow(
        column(3,
               selectInput(ns("absorption_method"), "Absorption Method",
                           choices = c("IV/Infusion/Bolus", "First order", "Transit","Weibull"), selectize = FALSE, size = 4),
               conditionalPanel(
                 condition = paste0("input['", ns("absorption_method"), "'] == 'Transit'"),
                 sliderInput(ns("transit_compartment"), "Number of Transit Compartments",
                             min = 1, max = 50, value = 1)
               )
        ),
        column(3,
               selectInput(ns("distribution_model"), "Distribution Model",
                           choices = c("1 compartment", "2 compartment", "3 compartment"), selectize = FALSE, size = 3)
        ),
        column(3,
               selectInput(ns("elimination_method"), "Elimination Method",
                           choices = c("Linear", "Michaelis-Menton"), selectize = FALSE, size = 2)
        ),
        column(3,
               uiOutput(ns("parameterization_ui"))
        )
      )
    ),
    materialSwitch(ns("pd_switch"), label = "PD", value = FALSE),
    conditionalPanel(
      condition = paste0("input['", ns("pd_switch"), "']"),
      fluidRow(
        column(4,
               selectInput(ns("response_type"), "Response",
                           choices = c("Direct/Immediate", "Indirect/Turnover", "Effect Compartment"), selectize = FALSE, size = 3)
        ),
        column(4,
               uiOutput(ns("second_dropdown_ui"))
        ),
        column(4,
               uiOutput(ns("third_dropdown_ui"))
        )
      ),
      fluidRow(
        column(12,
               uiOutput(ns("sigmoidicity_ui"))
        )
      ),
      fluidRow(
        column(12,
               uiOutput(ns("parameter_base_ui"))
        )
      )
    ),
    verbatimTextOutput(ns("combined_output"))
  )
}


pkServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$parameterization_ui <- renderUI({
      choices <- switch(input$distribution_model,
                        "1 compartment" = c("Cl/V", "kel", "alpha"),
                        "2 compartment" = c("Cl/V", "Cl/Vss", "alpha", "k21", "aob", "kel"),
                        "3 compartment" = c("Cl/V", "alpha", "k21", "kel"))
      selectInput(ns("parameterization"), "Parameterization", choices = choices, selectize = FALSE, size = length(choices))
    })

    output$second_dropdown_ui <- renderUI({
      if (input$response_type %in% c("Direct/Immediate", "Effect Compartment")) {
        selectInput(ns("drug_action"), "Drug Action",
                    choices = c("Emax", "Imax", "linear", "logarithmic", "quadratic"), selectize = FALSE, size = 5)
      } else if(input$response_type == "Indirect/Turnover"){
        selectInput(ns("type_of_model"), "Type of Model",
                    choices = c("stimulation of input", "stimulation of output",
                                "inhibition of input", "inhibition of output"), selectize = FALSE, size = 4)
      }

    })
    output$parameter_base_ui <- renderUI({
      if (length(input$response_type)==1 && input$response_type == "Indirect/Turnover") {
        fluidRow(
          column(12, checkboxInput(ns("par_bas"), label="parameterize baseline instead of kin", value = FALSE))
        )
      }
    })

    output$third_dropdown_ui <- renderUI({
      if (input$response_type %in% c("Direct/Immediate", "Effect Compartment")) {
        selectInput(ns("baseline"), "Baseline",
                    choices = c("baseline = 0","constant", "1-exp", "linear","exp"), selectize = FALSE, size = 5)
      } else if (input$response_type == "Indirect/Turnover") {
        selectInput(ns("drug_action"), "Drug Action",
                    choices = c("Emax", "Imax", "linear", "logarithmic", "quadratic"), selectize = FALSE, size = 5)

      }
    })

    output$sigmoidicity_ui <- renderUI({
      if (length(input$drug_action)==1 && input$drug_action %in% c("Emax", "Imax")) {
        fluidRow(
          column(12, checkboxInput(ns("sigmoidicity"), "Sigmoidicity or Hill Constant", value = FALSE))
        )
      }
    })


    # observeEvent(input$distribution_model,{
    #   results$distribution_model <- input$distribution_model
    # })
    # observeEvent(input$response_type,{
    #   results$response_type <- input$response_type
    # })
    # observeEvent(input$response_type,{
    #   results$response_type <- input$response_type
    # })



    observe({
      results$absorption_method <- input$absorption_method
      results$distribution_model <- input$distribution_model
      results$elimination_method <- input$elimination_method
      results$parameterization <- input$parameterization
      results$pk_switch <- input$pk_switch
      results$pd_switch <- input$pd_switch

      if (input$absorption_method == "Transit") {
        results$transit_compartment <- input$transit_compartment
       }

      results$response_type <- input$response_type
      results$drug_action <- input$drug_action

       if (input$response_type %in% c("Direct/Immediate", "Effect Compartment")) {
        results$baseline <- input$baseline

         } else if (input$response_type == "Indirect/Turnover") {
        results$type_of_model <- input$type_of_model
       }

       if (!is.null(input$drug_action) && input$drug_action %in% c("Emax", "Imax")) {
        results$sigmoidicity <- input$sigmoidicity
       }

       if (input$response_type == "Indirect/Turnover") {
        results$par_bas <- input$par_bas
       }
    })




    # output$combined_output <- renderPrint({


      # cat("PK Model:\n", pk_output, sep = "\n")
      # cat("\nPD Model:\n", pd_output, sep = "\n")
      # cat("\nPKPD Model:\n", pkpdmod, sep = "\n")
    # })
  })
}



# pkServer <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#
#     # Your existing code...
#
#     output$combined_output <- renderPrint({
#       # Your existing code to create pk_output and pd_output...
#
#       pkpdmod <- jPh(pk_output, pd_output)
#
#       cat("PK Model:\n")
#       cat(pk_output, sep = "\n")
#       cat("\nPD Model:\n")
#       cat(pd_output, sep = "\n")
#       cat("\nPKPD Model:\n")
#       print(pkpdmod)
#
#       # Return pkpdmod for use in pkprServer
#       list(pkpdmod = pkpdmod)
#     })
#   })
# }

