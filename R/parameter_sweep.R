#' Sweep across parameters
#'
#' @description Explore scenarios using gridding with sampling for parameters not in the grid. Parameters that
#' are included in the grid are currently hard coded. Use the `future` package to control parallisation
#' outside of the function.
#'
#' @param scenarios A dataframe containing all gridded  scenarios - see the examples for the required structure.
#' Defaults to NULL.
#' @param samples Numeric, defaults to 1. The number of samples to take.
#' @param sim_fn Function, defaults to NULL. The vectorised model simulation function - see the examples
#' for usage.
#' @param show_progress Logical, defaults to `TRUE`. Show the progress of the parameter sweep.
#' @author Sam Abbott
#'
#' @return A nested tibble containing the parameters for each scenario and a nested list of output
#' @export
#' @importFrom dplyr group_by mutate ungroup sample_frac
#' @importFrom tidyr nest unnest
#' @importFrom furrr future_map future_options
#' @importFrom purrr safely map
#' @examples
#'\dontrun{
#' library(covidhm)
#' library(tibble)
#' library(dplyr)
#'
#' nreps = 1
#' # Parameter sweep for sensitivity testing ---------------------------------
#'
#' scenarios <- tidyr::expand_grid(
#'   ## Put parameters that are grouped by disease into this data.frame
#'   delay_group = list(tibble::tibble(
#'     delay = c("Medium", "Short"),
#'     delay_shape = c(1.651524, 1),
#'     delay_scale = c(4.287786, 1.4)
#'   )),
#'   presymrate = c(0.2,0.4),
#'   prop.asym = c(0.2, 0.4),
#'   control_effectiveness = c(0.3, 0.6, 0.9),
#'   num.initial.cases = c(1, 5),
#'   scenario = c("primary_quarantine", "secondary_quarantine"),
#'   R = c(0.5,0.8,2)) %>%
#'   tidyr::unnest("delay_group") %>%
#'   dplyr::mutate(scenarioID = 1:dplyr::n())
#'
#' ## Parameterise fixed paramters
#' sim_with_params <- purrr::partial(scenario_sim,
#'                                   net = am_list[[1]],
#'                                   cap_max_days = 69,
#'                                   outside = 0.001,
#'                                   testing = FALSE,
#'                                   distancing = 0,
#'                                   cap_max_tests = Inf)
#'
#' ## Set up multicore if using see ?future::plan for details
#' ## Use the workers argument to control the number of cores used.
#' future::plan("multiprocess")
#'
#' ## Run paramter sweep
#' sweep_results <- parameter_sweep(scenarios,
#'                                  sim_fn = sim_with_params,
#'                                  samples = nreps,
#'                                  show_progress = TRUE)
#'
#' sweep_results
#' }

parameter_sweep <- function(scenarios = NULL, samples = 1,
                            sim_fn = NULL, show_progress = TRUE) {

  safe_sim_fn <- purrr::safely(sim_fn, quiet = FALSE)

  scenario_sims <- scenarios %>%
    dplyr::group_by(scenarioID) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    ##Randomise the order of scenarios - helps share the load across cores
    dplyr::sample_frac(size = 1, replace = FALSE) %>%
    dplyr::mutate(sims = purrr::map(
      data,
      ~ safe_sim_fn(n.sim = samples,
                    num.initial.cases = .$num.initial.cases,
                    presymrate = .$presymrate,
                    delay_shape = .$delay_shape,
                    delay_scale = .$delay_scale,
                    prop.ascertain = .$control_effectiveness,
                    prop.asym = .$prop.asym,
                    scenario = .$scenario,
                    R = .$R
      )[[1]],
      .progress = show_progress)) %>%
    tidyr::unnest(cols = "data")

  return(scenario_sims)
}
