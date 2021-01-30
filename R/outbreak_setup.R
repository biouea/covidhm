#' Set up initial cases for network model
#' @author Lewis Spurgin
#' @author Joel Hellewell
#'
#' @param net network in pairwise list format.
#' @param num.initial.cases integer number of initial cases
#' @param incfn function that samples from incubation period Weibull distribution; generated using dist_setup
#' @param delayfn function that samples from the onset-to-hospitalisation delay Weibull distribution; generated using dist_setup
#' @param prop.asym numeric proportion of cases that are sublinical (between 0 and 1)
#' @param isolation logical - do you want indiviuals to self-isolate on infection?
#'
#' @return data.frame of cases in outbreak so far
#' @export
#' @importFrom tibble tibble
#'
#' @examples
#'
#'\dontrun{
#' load("data-raw/am_list.RData")
#' haslemere <- format_network(am_list[[1]])
#' # incubation period sampling function
#' incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
#' # delay distribution sampling function
#' delayfn <- dist_setup(dist_shape = 1,dist_scale = 1.4)
#' outbreak_setup(haslemere, num.initial.cases = 1,incfn,delayfn,prop.asym=0, isolation = TRUE)
#'}
#'
#'
outbreak_setup <- function(net, num.initial.cases, incfn, delayfn, prop.asym, isolation) {

  # Set up table of population
  popsize <- length(unique(c(net$Var1,net$Var2)))
  case_data <- tibble(exposure = NA, # Exposure time of 0 for all initial cases
                      asym = purrr::rbernoulli(popsize, p = prop.asym),
                      caseid = unique(c(net$Var1,net$Var2)), # set case id
                      infector = NA,
                      onset = NA,
                      isolated_time = Inf,
                      quarantine_time = Inf,
                      test_time = Inf,
                      release_time = NA,
                      recovery_time = NA,
                      status = "S",
                      isolated = FALSE,
                      quarantined = FALSE)

  # Set up initial cases
  initial_cases <- sample(1:popsize,num.initial.cases)
  case_data$exposure[initial_cases] <- 0
  case_data$onset[initial_cases] <- incfn(num.initial.cases)
  case_data$recovery_time[initial_cases] <- case_data$onset[initial_cases] + 7
  case_data$status[initial_cases] <- "I"

  if(isolation){
    #Isolation times for symptomatic cases: onset + delay
    sym_cases <- initial_cases[!case_data$asym[initial_cases]]
    case_data$isolated_time[sym_cases] <- case_data$onset[sym_cases] +
      delayfn(length(sym_cases))
    case_data$release_time[sym_cases] <- case_data$isolated_time[sym_cases] + 14
  }


  # return
  return(case_data)
}
