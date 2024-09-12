PKph <- function(absorption_method,
                 distribution_model,
                 elimination_method,
                 parameterization,
                 transit_compartment = NULL) {
  parme <- ""
  if(length(parameterization)==1){
    if(distribution_model=="1 compartment" && length(distribution_model)==1){
      parme <- switch(parameterization, "kel"='pkTrans("k")',
                      "Cl/V"="",
                      "alpha"='pkTrans("alpha")')
    } else if(distribution_model=="2 compartment" && length(distribution_model)==1){
      parme <- switch(parameterization, "kel"='pkTrans("k")',
                      "Cl/Vss"='pkTrans("vss")',
                      "Cl/V"="",
                      "alpha"='pkTrans("alpha")',
                      "aob"='pkTrans("aob")',
                      "k21"='pkTrans("k21")'
      )
    } else if(distribution_model=="3 compartment" && length(distribution_model)==1){
      parme <- switch(parameterization, "kel"='pkTrans("k")',
                      "Cl/V"="",
                      "alpha"='pkTrans("alpha")',
                      "k21"='pkTrans("k21")'
      )
    }
  }

  cmt <- switch(distribution_model, "1 compartment"="readModelDb('PK_1cmt_des')",
                "2 compartment"="readModelDb('PK_2cmt_des')",
                "3 compartment"="readModelDb('PK_3cmt_des')")



  pkpipe <- character(0)
  pkpipe <- c(pkpipe, cmt)
  #parme <- ifelse(parameterization=="Cl/V", "",paste("pkTrans(","'",parameterization,"'",")",sep=""))
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



PDph <- function(response_type = c("Direct/Immediate", "Indirect/Turnover", "Effect Compartment"),
                 drug_action = c("Emax", "Imax", "linear", "logarithmic", "quadratic"),
                 baseline = NULL,
                 type_of_model = c("stimulation of input", "stimulation of output",
                                   "inhibition of input", "inhibition of output"),
                 sigmoidicity = FALSE,
                 par_bas = FALSE) {

  # Check input validity for logical parameters
  checkmate::assertLogical(sigmoidicity, any.missing = FALSE, len = 1)
  checkmate::assertLogical(par_bas, any.missing = FALSE, len = 1)

  # Match the type_of_model and drug_action arguments
  type_of_model <- match.arg(type_of_model)
  drug_action <- match.arg(drug_action)

  # Initialize pdpipe
  pdpipe <- character(0)

  # Determine response type
  if (response_type == "Direct/Immediate") {
    resp <- "addDirectLin()"
  } else if (response_type == "Effect Compartment") {
    resp <- "addEffectCmtLin()"
  } else if (response_type == "Indirect/Turnover" && type_of_model == "stimulation of input") {
    resp <- 'addIndirectLin(stim="in")'
  } else if (response_type == "Indirect/Turnover" && type_of_model == "stimulation of output") {
    resp <- 'addIndirectLin(stim="out")'
  } else if (response_type == "Indirect/Turnover" && type_of_model == "inhibition of input") {
    resp <- 'addIndirectLin(inhib="in")'
  } else if (response_type == "Indirect/Turnover" && type_of_model == "inhibition of output") {
    resp <- 'addIndirectLin(inhib="out")'
  }

  pdpipe <- c(pdpipe, resp)

  # Handle baseline only if it is not NULL
  if (!is.null(baseline)) {
    baseline <- match.arg(baseline, c("baseline = 0", "constant", "1-exp", "linear", "exp"))

    basel <- switch(baseline,
                    "baseline = 0" = "",  # No action for baseline = 0
                    "constant" = "addBaselineConst()",
                    "1-exp" = "addBaseline1exp()",
                    "linear" = "addBaselineLin()",
                    "exp" = "addBaselineExp()"
    )

    pdpipe <- c(pdpipe, basel)
  }

  # Handle drug action
  if (drug_action == "linear") {
    drAc <- ""
  } else if (drug_action == "Emax" && sigmoidicity == TRUE) {
    drAc <- "convertEmaxHill()"
  } else if (drug_action == "Emax" && sigmoidicity == FALSE) {
    drAc <- "convertEmax()"
  } else if (drug_action == "Imax" && sigmoidicity == TRUE) {
    drAc <- 'convertEmaxHill(emax="Imax", ec50="IC50")'
  } else if (drug_action == "Imax" && sigmoidicity == FALSE) {
    drAc <- 'convertEmax(emax="Imax", ec50="IC50")'
  } else if (drug_action == "logarithmic") {
    drAc <- 'convertLogLin()'
  } else if (drug_action == "quadratic") {
    drAc <- 'convertQuad()'
  }

  pdpipe <- c(pdpipe, drAc)

  # Handle par_bas logic
  if (par_bas == TRUE) {
    pdpipe <- c(pdpipe, "convertKinR0()")
  }

  # Remove empty elements from pdpipe and concatenate with a pipe separator
  pdpipe <- pdpipe[pdpipe != ""]
  pdpipe <- paste(pdpipe, collapse = "|>\n\t")

  # Return the final pipe string
  return(pdpipe)
}



jPh <- function(pkO,pdO){
  assertthat::is.string(pkO)
  assertthat::is.string(pdO)
  joined <- c(pkO,pdO)
  joined <- joined[joined!=""]
  joined <- paste(joined,collapse = "|>\n\t")
  #pkpdmodel <- eval(str2lang(joined))
  return(joined)
}
