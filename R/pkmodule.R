# 
# pkUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     materialSwitch(ns("pk_switch"), label = "PK", value = TRUE),
#     conditionalPanel(
#       condition = paste0("input['", ns("pk_switch"), "']"),
#       fluidRow(
#         column(3,
#                selectInput(ns("absorption_method"), "Absorption Method",
#                            choices = c("IV/Infusion/Bolus", "First order", "Transit","Weibull"), selectize = FALSE, size = 4),
#                conditionalPanel(
#                  condition = paste0("input['", ns("absorption_method"), "'] == 'Transit'"),
#                  sliderInput(ns("transit_compartment"), "Number of Transit Compartments",
#                              min = 1, max = 50, value = 1)
#                )
#         ),
#         column(3,
#                selectInput(ns("distribution_model"), "Distribution Model",
#                            choices = c("1 compartment", "2 compartment", "3 compartment"), selectize = FALSE, size = 3)
#         ),
#         column(3,
#                selectInput(ns("elimination_method"), "Elimination Method",
#                            choices = c("Linear", "Michaelis-Menton"), selectize = FALSE, size = 2)
#         ),
#         column(3,
#                uiOutput(ns("parameterization_ui"))
#         )
#       )
#     ),
#     materialSwitch(ns("pd_switch"), label = "PD", value = FALSE),
#     conditionalPanel(
#       condition = paste0("input['", ns("pd_switch"), "']"),
#       fluidRow(
#         column(4,
#                selectInput(ns("response_type"), "Response",
#                            choices = c("Direct/Immediate", "Indirect/Turnover", "Effect Compartment"), selectize = FALSE, size = 3)
#         ),
#         column(4,
#                uiOutput(ns("second_dropdown_ui"))
#         ),
#         column(4,
#                uiOutput(ns("third_dropdown_ui"))
#         )
#       ),
#       fluidRow(
#         column(12,
#                uiOutput(ns("sigmoidicity_ui"))
#         )
#       ),
#       fluidRow(
#         column(12,
#                uiOutput(ns("parameter_base_ui"))
#         )
#       )
#     ),
#     verbatimTextOutput(ns("combined_output"))
#     # actionButton(ns("show_code_btn"), "Show Model Code"),
#     # uiOutput(ns("code_output_ui")) 
#   )
# }
# 
# 
# pkServer <- function(id, results) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     output$parameterization_ui <- renderUI({
#       choices <- switch(input$distribution_model,
#                         "1 compartment" = c("Cl/V", "kel", "alpha"),
#                         "2 compartment" = c("Cl/V", "Cl/Vss", "alpha", "k21", "aob", "kel"),
#                         "3 compartment" = c("Cl/V", "alpha", "k21", "kel"))
#       selectInput(ns("parameterization"), "Parameterization", choices = choices, selectize = FALSE, size = length(choices))
#     })
# 
#     output$second_dropdown_ui <- renderUI({
#       if (input$response_type %in% c("Direct/Immediate", "Effect Compartment")) {
#         selectInput(ns("drug_action"), "Drug Action",
#                     choices = c("Emax", "Imax", "linear", "logarithmic", "quadratic"), selectize = FALSE, size = 5)
#       } else if(input$response_type == "Indirect/Turnover"){
#         selectInput(ns("type_of_model"), "Type of Model",
#                     choices = c("stimulation of input", "stimulation of output",
#                                 "inhibition of input", "inhibition of output"), selectize = FALSE, size = 4)
#       }
# 
#     })
#     output$parameter_base_ui <- renderUI({
#       if (length(input$response_type)==1 && input$response_type == "Indirect/Turnover") {
#         fluidRow(
#           column(12, checkboxInput(ns("par_bas"), label="parameterize baseline instead of kin", value = FALSE))
#         )
#       }
#     })
# 
#     output$third_dropdown_ui <- renderUI({
#       if (input$response_type %in% c("Direct/Immediate", "Effect Compartment")) {
#         selectInput(ns("baseline"), "Baseline",
#                     choices = c("baseline = 0","constant", "1-exp", "linear","exp"), selectize = FALSE, size = 5)
#       } else if (input$response_type == "Indirect/Turnover") {
#         selectInput(ns("drug_action"), "Drug Action",
#                     choices = c("Emax", "Imax", "linear", "logarithmic", "quadratic"), selectize = FALSE, size = 5)
# 
#       }
#     })
# 
#     output$sigmoidicity_ui <- renderUI({
#       if (length(input$drug_action)==1 && input$drug_action %in% c("Emax", "Imax")) {
#         fluidRow(
#           column(12, checkboxInput(ns("sigmoidicity"), "Sigmoidicity or Hill Constant", value = FALSE))
#         )
#       }
#     })
# 
# 
# 
#     observe({
#       results$absorption_method <- input$absorption_method
#       results$distribution_model <- input$distribution_model
#       results$elimination_method <- input$elimination_method
#       results$parameterization <- input$parameterization
#       results$pk_switch <- input$pk_switch
#       results$pd_switch <- input$pd_switch
# 
#       if (input$absorption_method == "Transit") {
#         results$transit_compartment <- input$transit_compartment
#        }
# 
#       results$response_type <- input$response_type
#       results$drug_action <- input$drug_action
# 
#        if (input$response_type %in% c("Direct/Immediate", "Effect Compartment")) {
#         results$baseline <- input$baseline
# 
#          } else if (input$response_type == "Indirect/Turnover") {
#         results$type_of_model <- input$type_of_model
#        }
# 
#        if (!is.null(input$drug_action) && input$drug_action %in% c("Emax", "Imax")) {
#         results$sigmoidicity <- input$sigmoidicity
#        }
# 
#        if (input$response_type == "Indirect/Turnover") {
#         results$par_bas <- input$par_bas
#        }
# 
#       # Retrieve stored PKPD inputs from results and process them
#       pk_values <- list(
#         absorption_method = results$absorption_method,
#         distribution_model = results$distribution_model,
#         elimination_method = results$elimination_method,
#         parameterization = results$parameterization
#       )
#       if (results$absorption_method == "Transit") {
#         pk_values$transit_compartment <- results$transit_compartment
#       }
# 
#       pd_values <- list(
#         response_type = results$response_type,
#         drug_action = results$drug_action
#       )
#       if (results$response_type %in% c("Direct/Immediate", "Effect Compartment")) {
#         pd_values$baseline <- results$baseline
#       } else if (results$response_type == "Indirect/Turnover") {
#         pd_values$type_of_model <- results$type_of_model
#       }
#       if (length(results$drug_action) == 1 && results$drug_action %in% c("Emax", "Imax")) {
#         pd_values$sigmoidicity <- results$sigmoidicity
#       }
# 
#       pk_output <- ifelse(results$pk_switch, do.call(PKph, pk_values), "")
#       pd_output <- ifelse(results$pd_switch, do.call(PDph, pd_values), "")
#       pkpdmod <- ifelse(results$pk_switch || results$pd_switch, jPh(pk_output, pd_output), "")
#       results$pkpdpipe <- pkpdmod
#     })
# 
#   })
# }




pkUI <- function(id) {
  ns <- NS(id)
  tagList(
    materialSwitch(ns("pk_switch"), label = "PK", value = TRUE),
    conditionalPanel(
      condition = paste0("input['", ns("pk_switch"), "']"),
      fluidRow(
        column(3,
               selectInput(ns("absorption_method"), "Absorption Method",
                           choices = c("IV/Infusion/Bolus", "First order", "Transit", "Weibull"), selectize = FALSE, size = 4),
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
    # Replaced "Show Model Code" with "Copy Model Code" button
    actionButton(ns("copy_code"), "Export Model")  # Button to directly copy model code
    
  )
}



pkServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # UI for parameterization based on distribution model
    output$parameterization_ui <- renderUI({
      choices <- switch(input$distribution_model,
                        "1 compartment" = c("Cl/V", "kel", "alpha"),
                        "2 compartment" = c("Cl/V", "Cl/Vss", "alpha", "k21", "aob", "kel"),
                        "3 compartment" = c("Cl/V", "alpha", "k21", "kel"))
      selectInput(ns("parameterization"), "Parameterization", choices = choices, selectize = FALSE, size = length(choices))
    })
    
    # UI for second dropdown based on response type
    output$second_dropdown_ui <- renderUI({
      if (input$response_type %in% c("Direct/Immediate", "Effect Compartment")) {
        selectInput(ns("drug_action"), "Drug Action",
                    choices = c("Emax", "Imax", "linear", "logarithmic", "quadratic"), selectize = FALSE, size = 5)
      } else if (input$response_type == "Indirect/Turnover") {
        selectInput(ns("type_of_model"), "Type of Model",
                    choices = c("stimulation of input", "stimulation of output",
                                "inhibition of input", "inhibition of output"), selectize = FALSE, size = 4)
      }
    })
    
    # UI for parameter baseline checkbox based on response type
    output$parameter_base_ui <- renderUI({
      if (length(input$response_type) == 1 && input$response_type == "Indirect/Turnover") {
        fluidRow(
          column(12, checkboxInput(ns("par_bas"), label = "Parameterize baseline instead of kin", value = FALSE))
        )
      }
    })
    
    # UI for third dropdown based on drug action and response type
    output$third_dropdown_ui <- renderUI({
      if (input$response_type %in% c("Direct/Immediate", "Effect Compartment")) {
        selectInput(ns("baseline"), "Baseline",
                    choices = c("baseline = 0", "constant", "1-exp", "linear", "exp"), selectize = FALSE, size = 5)
      } else if (input$response_type == "Indirect/Turnover") {
        selectInput(ns("drug_action"), "Drug Action",
                    choices = c("Emax", "Imax", "linear", "logarithmic", "quadratic"), selectize = FALSE, size = 5)
      }
    })
    
    # UI for sigmoidicity checkbox based on drug action
    output$sigmoidicity_ui <- renderUI({
      if (length(input$drug_action) == 1 && input$drug_action %in% c("Emax", "Imax")) {
        fluidRow(
          column(12, checkboxInput(ns("sigmoidicity"), "Sigmoidicity or Hill Constant", value = FALSE))
        )
      }
    })
    
    # Observe changes in the input values and update results
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
      
      # Generate PK and PD model outputs
      pk_values <- list(
        absorption_method = results$absorption_method,
        distribution_model = results$distribution_model,
        elimination_method = results$elimination_method,
        parameterization = results$parameterization
      )
      if (results$absorption_method == "Transit") {
        pk_values$transit_compartment <- results$transit_compartment
      }
      
      pd_values <- list(
        response_type = results$response_type,
        drug_action = results$drug_action
      )
      if (results$response_type %in% c("Direct/Immediate", "Effect Compartment")) {
        pd_values$baseline <- results$baseline
      } else if (results$response_type == "Indirect/Turnover") {
        pd_values$type_of_model <- results$type_of_model
      }
      if (length(results$drug_action) == 1 && results$drug_action %in% c("Emax", "Imax")) {
        pd_values$sigmoidicity <- results$sigmoidicity
      }
      
      pk_output <- ifelse(results$pk_switch, do.call(PKph, pk_values), "")
      pd_output <- ifelse(results$pd_switch, do.call(PDph, pd_values), "")
      pkpdmod <- ifelse(results$pk_switch || results$pd_switch, jPh(pk_output, pd_output), "")
      results$pkpdpipe <- pkpdmod
    })
    
    
    observeEvent(input$copy_code, {
      waiter_show(html = tagList(
        spin_fading_circles(),  # A nice spinning loading indicator
        h4("Calculating, please wait...")
      ))
      req(results$pkpdpipe)
      
      # Generate the model code
      code_to_copy <- paste(deparse(as.function(eval(str2lang(results$pkpdpipe)))), collapse = "\n")
      
      # Copy code to the R script using rstudioapi::insertText
      rstudioapi::insertText(text =paste("mod1 <- ", code_to_copy))
      
      waiter_hide()  # Hide the spinner after the code is copied
      
      # Close the app
      stopApp()
    })
  })
}




