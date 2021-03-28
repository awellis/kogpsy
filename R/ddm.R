#' Generate a discrete-time diffusion process
#'
#' @param bias Starting point \[0, 1\]. 0.5 means no bias.
#' @param driftrate Average rate of evidence accumulation.
#' @param decision_boundary Symmetric absorbing boundaries.
#' @param ndt Non-decision time, in seconds.
#' @param diffvar Diffusion variance---must be > 0.
#' @param max_time Maximum duration of trial.
#' @return A tibble containing \code{time}, the decision variable \code{dv} and the step number \code{steps}.
#' @examples
#' drift_diffusion(bias = 0.3, driftrate = 0.8)
#' drift_diffusion(bias = 0.5, driftrate = -1.0, decision_boundary = 2.5, diffvar = 0.8)
#'
#' @export
drift_diffusion <- function(bias = 0.5,
                            driftrate = 1.0,
                            decision_boundary = 2,
                            ndt = 0.5,
                            diffvar = 0.1,
                            dt = 0.001,
                            max_time = 6,
                            ndt_noise = FALSE) {

    assertthat::assert_that(diffvar > 0)

    # rescale bias so that 0.5 lies halfway between upper and lower bound
    bias <- as.numeric(2 * decision_boundary * bias - decision_boundary)

    # initialize time_steps and dv
    time_steps <- max_time/dt
    dv <- array(dim = time_steps)

    # start acumulating from bias (starting point)
    dv[1] <- rnorm(1, mean = bias, sd = sqrt(dt))

    for (j in 2:time_steps) {

        # non-decision time
        if (j <= ndt/dt) {
            dv[j] <- ifelse(ndt_noise,
                                    rnorm(1, mean = bias, sd = sqrt(dt)),
                                    bias)
        }
        else {
            error <- rnorm(1, 0, sqrt(diffvar * dt))
            dv[j] <- dv[j-1] + driftrate * dt + error  # Cobb & Zacks (1985), Eq. 1.14
            if (abs(dv[j]) > decision_boundary) {
                dv[j] <- dplyr::if_else(dv[j] > 0,
                                 min(dv[j], decision_boundary),
                                 max(dv[j], -decision_boundary))
                break()
            }
        }
    }
    # dv <- dv[!is.na(dv)]
    d <- dplyr::tibble(time = round(seq_along(dv) * dt, 2),
                         dv = dv,
                         steps = seq_along(dv),
                         driftrate = driftrate,
                         decision_boundary = decision_boundary,
                         bias = bias,
                         ndt = ndt)
    # invisible(dv)
    return(d)
}


#' @export
get_response_rt <- function(d) {
    time <- pull(d, time)
    dv <- pull(d, dv)
    decision_boundary <- pull(d, decision_boundary)[1]

    index <- tail(which(!is.na(dv)), 1)
    rt <- time[index]
    response <- sign(dv[index])
    list(response = response, rt = rt)
}
