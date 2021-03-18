
drift_diffusion <- function(bias = 0.5,
                            driftrate = 1.0,
                            decision_boundary = 40,
                            ndt = 0.5,
                            dt = 0.01) {
    
    # Outputs a sequence of Brownian motion data
    bias <- as.integer(2 * decision_boundary * bias - decision_boundary)
    
    # make sure time_steps is big enough
    time_steps <- 6000
    dv <- array(dim = time_steps)

    diffvar <- 2

    # start acumulating from bias (starting point)
    dv[1] <- rnorm(1, mean = bias, sd = sqrt(dt))

    for (j in 2:time_steps) {

        # non-decision time
        if (j < ndt/dt) {
            dv[j] <- rnorm(1, mean = bias, sd = sqrt(dt))
        }
        else {
        error <- rnorm(1, 0, sqrt(diffvar * dt))
        if (abs(dv[j-1]) > decision_boundary) break()
        # dv[j:time_steps] <-  NA
        else {
            dv[j] <- dv[j-1] + driftrate * dt + error  # Cobb & Zacks (1985), Eq. 1.14
        }
        }
    }
    # dv <- dv[!is.na(dv)]
    out <- dplyr::tibble(time = round(seq_along(dv)/60, 2), 
                         dv = dv,
                         steps = seq_along(dv))
    # invisible(dv)
    return(out)
}
