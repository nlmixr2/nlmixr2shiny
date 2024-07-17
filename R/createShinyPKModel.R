#' This function creates a nlmixr2 model by specifying the absorption, distribution and elimination
#' 
#' In general this is used in shiny to create the base PK model
#'
#' @param absorption This specifies the absorption method and can be:
#'   - IV/Infusion/Bolus -- no changes to the base model
#'   - First Order -- adds a depot compartment
#'   - Transit -- adds a transit compartment of size `ntransit`
#' @param distribution number of compartments in the model;  Can be:
#' - 1 compartment
#' - 2 compartment
#' - 3 compartment
#' @param elimination the type of elimination of PK.  Currently supports:
#' - linear
#' - Michaelis Menton
#' @param ntransit number of transit compartments (by default 3)
#' @param linCmt boolean, is this going to use a linear compartment solution
#'
#' @return a PK model according to the specifications above;  This will return a list of string
#' @export
#'
#' @examples
#' 
#' # By defualt this creates a one comparmtent ODE model from the `nlmixr2lib` model library
#' createShinyPKModel()
#' 
#' createShinyPKModel(absorption="First Order")
createShinyPKModel <- function(absorption=c("IV/Infusion/Bolus",
                                            "First Order",
                                            "Transit"), 
                               distribution=c("1 compartment",
                                              "2 compartment",
                                              "3 compartment"), 
                               elimination=c("linear",
                                             "Michealis-Menton"), 
                               ntransit=3) {
  absorption <- match.arg(absorption)
  distribution <- match.arg(distribution)
  elimination <- match.arg(elimination)
  checkmate::assertIntegerish(ntransit, lower=1, any.missing=FALSE, len=1)
  ini <- rxode2::ini
  if (!linCmt) {
    .lib <- loadNamespace("nlmixr2lib")
    .f <- switch(distribution,
                "1 compartment"=nlmixr2lib::readModelDb("PK_1cmt_des"),
                "2 compartment"=nlmixr2lib::readModelDb("PK_2cmt_des"),
                "3 compartment"=nlmixr2lib::readModelDb("PK_3cmt_des"))
    .f <- .f()
    if (absorption == "First Order") {
      .f <- nlmixr2lib::addDepot(.f,lag = lag, fdepot=fdepot)
    } else {
      if (absorption == "Transit") {
        .f <- nlmixr2lib::addTransit(.f, transit = ntransit)
      } else {
        .f <- nlmixr2lib::removeDepot(.f)
      }
    }
  } else {
    stop("not implented", call.=FALSE)
  }
  .f <- rxode2::rxUiDecompress(.f)
  rm("description", envir=.f$meta)
  rxode2::rxUiCompress(.f)
}
