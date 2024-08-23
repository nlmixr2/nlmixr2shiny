PKph <- function(absorption_method,
                 distribution_model,
                 elimination_method,
                 parameterization,
                 transit_compartment = NULL) {
  cmt <- switch(distribution_model, "1 compartment"="readModelDb('PK_1cmt_des')",
                "2 compartment"="readModelDb('PK_2cmt_des')",
                "3 compartment"="readModelDb('PK_3cmt_des')")
  pkpipe <- character(0)
  pkpipe <- c(pkpipe, cmt)
  parme <- ifelse(parameterization=="Cl/V", "",paste("pkTrans(","'",parameterization,"'",")",sep=""))
  pkpipe <- c(pkpipe, parme)
  Elim <- ifelse(elimination_method=="Linear","","convertMM()")
  pkpipe <- c(pkpipe, Elim)

  if(absorption_method=="Transit"){

    tran <- paste("addTransit(",transit_compartment,")",sep="")
  } else if(absorption_method=="First Order"){
    tran <- ""
  }else if(absorption_method=="IV/Infusion/Bolus"){

    tran <- "removeDepot()"
  } else{
    tran <- "addWeibullAbs()"
  }
  pkpipe <- c(pkpipe, tran)

  pkpipe <- pkpipe[pkpipe != ""]
  pkpipe <- paste(pkpipe, collapse="|>\n\t")

  #model <- eval(str2lang(pkpipe))

  return(pkpipe)
}



PDph <- function(response_type=c("Direct/Immediate", "Indirect/Turnover", "Effect Compartment"),
                 drug_action = c("Emax", "Imax", "linear", "logarithmic", "quadratic"),
                 baseline = c("baseline = 0","constant", "1-exp", "linear","exp"),
                 type_of_model = c("stimulation of input", "stimulation of output",
                                   "inhibition of input", "inhibition of output"),
                 sigmoidicity =FALSE) {
  checkmate::assertLogical(sigmoidicity,any.missing = FALSE, len=1)
  #checkmate::assertLogical(par_bas,any.missing = FALSE, len=1)
  type_of_model <- match.arg(type_of_model)
  drug_action <- match.arg(drug_action)
  baseline <- match.arg(baseline)
  pdpipe <- character(0)
  if(response_type == "Direct/Immediate"){
    resp <- "addDirectLin()"
  } else if(response_type == "Effect Compartment"){
    resp <- "addEffectCmtLin()"
  }else if (response_type == "Indirect/Turnover" && type_of_model == "stimulation of input"){
    resp <- 'addIndirectLin(stim="in")'
  }else if(response_type == "Indirect/Turnover" && type_of_model == "stimulation of output"){
    resp <- 'addIndirectLin(stim="out")'
  }else if(response_type == "Indirect/Turnover" && type_of_model == "inhibition of input"){
    resp <- 'addIndirectLin(inhib="in")'
  }else if(response_type == "Indirect/Turnover" && type_of_model == "inhibition of output"){
    resp <- 'addIndirectLin(inhib="out")'
  }

  pdpipe <- c(pdpipe,resp)

  basel <- switch (baseline,
                   "Baseline=0" = "",
                   "constant" = "addBaselineConst()",
                   "1-exp" = "addBaseline1exp()",
                   "linear" = "addBaselineLin()",
                   "exp" = "addBaselineExp()"
  )

  pdpipe <- c(pdpipe, basel)

  if(drug_action=="linear"){
    drAc <- ""
  }else if(drug_action=="Emax" && sigmoidicity==TRUE){
    drAc <- "convertEmaxHill()"
  }else if(drug_action=="Emax" && sigmoidicity==FALSE){
    drAc <- "convertEmax()"
  }else if(drug_action=="Imax" && sigmoidicity==TRUE){
    drAc <- 'convertEmaxHill(emax="Imax", ec50="IC50")'
  }else if(drug_action=="Imax" && sigmoidicity==FALSE){
    drAc <- 'convertEmax(emax="Imax", ec50="IC50")'
  }else if(drug_action=="logarithmic"){
    drAc <- 'convertLogLin()'
  }else if(drug_action=="quadratic"){
    drAc <- 'convertQuad()'
  }
  pdpipe <- c(pdpipe, drAc)

  pdpipe <- pdpipe[pdpipe!= ""]
  pdpipe <- paste(pdpipe, collapse = "|>\n\t")
  #pdmodel <- eval(str2lang(pdpipe))
  return(pdpipe)
}


jPh <- function(pkO,pdO){
  assertthat::is.string(pkO)
  assertthat::is.string(pdO)
  joined <- c(pkO,pdO)
  joined <- joined[joined!=""]
  joined <- paste(joined,collapse = "|>\n\t")
  pipEnv$step1=NULL
  #pkpdmodel <- eval(str2lang(joined))
  return(joined)
}


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
               uiOutput(ns("parameter_bas_ui"))
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
    output$parameter_base_ui <- renderUI({
      if (length(input$response_type)==1 && input$response_type == "Indirect/Turnover") {
        fluidRow(
          column(12, checkboxInput(ns("par_bas"), "parameterize baseline instead of kin", value = FALSE))
        )
      }
    })
    output$combined_output <- renderPrint({
      pk_values <- list(
        absorption_method = input$absorption_method,
        distribution_model = input$distribution_model,
        elimination_method = input$elimination_method,
        parameterization = input$parameterization
      )

      if (input$absorption_method == "Transit") {
        pk_values$transit_compartment <- input$transit_compartment
      }

      pd_values <- list(
        response_type = input$response_type,
        drug_action = input$drug_action
      )

      if (input$response_type %in% c("Direct/Immediate", "Effect Compartment")) {
        pd_values$baseline <- input$baseline
      } else if (input$response_type == "Indirect/Turnover") {
        pd_values$type_of_model <- input$type_of_model
      }

      if (length(input$drug_action)==1 && input$drug_action %in% c("Emax", "Imax")) {
        pd_values$sigmoidicity <- input$sigmoidicity
      }
      if (length(input$response_type)==1 && input$response_type =="Indirect/Turnover") {
        pd_values$par_bas <- input$par_bas
      }
      if(input$pk_switch==FALSE){
        pk_output <- ""
      } else{
        pk_output <- do.call(PKph, pk_values)
      }
      if(input$pd_switch==FALSE){
        pd_output <- ""
      } else {
        pd_output <- do.call(PDph, pd_values)
      }

      if(input$pk_switch==FALSE && input$pd_switch==FALSE){
        pkpdmod <- ""
      } else {
        pkpdmod <- jPh(pk_output,pd_output)
      }

      cat("PK Model:\n")
      cat(pk_output, sep = "\n")
      cat("\nPD Model:\n")
      cat(pd_output, sep = "\n")
      cat("\nPKPD Model:\n")
      results$pkpdpipe <- pkpdmod
      if(input$pk_switch==FALSE && input$pd_switch==FALSE){
        results$pkpdm <- list(state=character(0))
      } else {
        results$pkpdm <- eval(str2lang(pkpdmod))
      }

      print(pkpdmod)
      #list(pkpdmod = pkpdmod)
    })
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

