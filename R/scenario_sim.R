#' Run a specified number of simulations with identical parameters
#' @author Joel Hellewell
#' @author Lewis Spurgin
#' @param n.sim number of simulations to run
#' @param net network from which to sample cases - square matrix
#' @param num.initial.cases Initial number of cases
#' @param prop.ascertain Probability that cases are ascertained by contact tracing
#' @param cap_max_days Maximum number of days to run process for
#' @param delay_shape shape of distribution for delay between symptom onset and isolation
#' @param delay_scale scale of distribution for delay between symptom onset and isolation
#' @param scenario character - must be either ("nothing","isolation","primary_quarantine", or "secondary_quarantine")
#' @param R scaling factor for infection probability
#' @param presymrate rate of presymptomatic transmission (must be 0 <= x <= 1)
#' @param prop.asym proportion of asymptomatic cases (must be 0 <= x <= 1)
#' @param outside infection rate from outside the network (must be 0 <= x <= 1)
#' @param testing logical
#' @param distancing proportion of rare contacts to remove (must be 0 <= x <= 1)
#' @param dist_func how do you want to do social distancing? (must be dist_all or dist_no_reall - see aux functions)
#' @param null.net do you want to simulate on a null network? Must be 'none', 'edge', 'deg', 'latt' or 'clust'
#' @param test_neg false negative rate for testing
#' @param cap_max_tests integer - max number of daily tests. Only use if testing == TRUE
#'
#' @importFrom purrr safely map
#' @importFrom dplyr bind_rows mutate
#' @return data.frame of weekly cases, isolations, quarantines and tests for each simulation
#' @export
#'
#' @examples
#' \dontrun{
#' #Load association matrices
#' load("data-raw/am_list.RData")
#'
#' #First item in the list is data across all days
#' m <- am_list[[1]]
#'
#' res <- scenario_sim(net = m, n.sim = 10, num.initial.cases = 1,prop.asym=0.4,
#'                     prop.ascertain = 0.9, cap_max_days = 70,
#'                     delay_shape = 1, delay_scale = 1.4, R = 0.8, presymrate = 0.2, scenario = "nothing",
#'                     testing = FALSE, outside = 0.001, distancing = 0)
#'                     }


scenario_sim <- function(n.sim, net, prop.ascertain, cap_max_days, R, presymrate,
                         delay_shape, delay_scale, num.initial.cases, prop.asym, scenario,
                         outside, distancing, dist_func = NULL, null.net = "none", testing,
                         cap_max_tests = NULL, test_neg = 0.1) {


  # Check input parameters --------------------------------------------------

  if(floor(prop.ascertain) != 0) stop("prop.ascertain must between 0 and 1")
  if(floor(presymrate) != 0) stop("presymrate must between 0 and 1")
  if(floor(prop.asym) != 0) stop("prop.asym must between 0 and 1")
  if(floor(outside) != 0) stop("outside must between 0 and 1")



  # Set up networks ---------------------------------------------------------

  message("Setting up networks...")

  if(null.net == "none")
  {
    amlist <- rep(list(net),n.sim)
  } else
  {
    amlist <- replicate(n.sim, network_null(net,returns = "matrix",null = null.net), simplify = FALSE)
  }

  if(distancing > 0)
  {
    amlist <- purrr::map(amlist, ~ dist_func(., "matrix", distancing))
  }

  netlist <- purrr::map(amlist, ~format_network(.))

  # Set up scenarios --------------------------------------------------------

  if(scenario == "nothing") {
    isolation <- FALSE
    tracing <- FALSE
    quarantine <- FALSE
    secondary <- FALSE
  } else {
    if(scenario == "isolation") {
      isolation <- TRUE
      tracing <- FALSE
      quarantine <- FALSE
      secondary <- FALSE
    } else {
      if(scenario == "primary_quarantine") {
        isolation <- TRUE
        tracing <- TRUE
        quarantine <- TRUE
        secondary <- FALSE
      } else {
        if(scenario == "secondary_quarantine") {
          isolation <- TRUE
          tracing <- TRUE
          quarantine <- TRUE
          secondary <- TRUE
        } else {
          stop('Scenario must be either "nothing","isolation","primary_quarantine", or "secondary_quarantine"')
        }
      }
    }
  }


  message("Running epidemic model...")

  # Run n.sim number of model runs and put them all together in a big data.frame
  res <- purrr::map(netlist, ~ outbreak_model(num.initial.cases = num.initial.cases,
                                              net = .,
                                              prop.ascertain = prop.ascertain,
                                              cap_max_days = cap_max_days,
                                              delay_shape = delay_shape,
                                              delay_scale = delay_scale,
                                              R = R,
                                              presymrate = presymrate,
                                              prop.asym = prop.asym,
                                              quarantine = quarantine,
                                              secondary = secondary,
                                              tracing = tracing,
                                              isolation = isolation,
                                              outside = outside,
                                              testing = testing,
                                              cap_max_tests = cap_max_tests,
                                              test_neg = test_neg))


  # bind output together and add simulation index
  res <- dplyr::bind_rows(res) %>%
    dplyr::mutate(sim = rep(1:n.sim, rep(floor(cap_max_days / 7) + 1, n.sim)))
  message("Done!")
  return(res)
}
