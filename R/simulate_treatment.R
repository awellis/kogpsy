#' Generate a multilevel data for a treatment factor with two levels
#'
#' @param a Average pre-treatment score
#' @param b Average difference between pre and post
#' @param sigma_a Std dev in intercepts
#' @param sigma_b Std dev in slopes -> set to 0.5
#' @param rho Correlation between intercepts and slopes
#' @param n_subjects Number of subjects
#' @param n_trials Number of trials per subjetc per treatment condition
#' @param sigma  Std dev within subjects
#' @return A tibble \code{d} containing the parameters, the linear predictor and the response for \code{n_trials} repetitions.
#' @examples
#' d <- simulate_data()
#' d_linpred <- d |>
#'     group_by(subject, treatment) |> distinct(mu, .keep_all = TRUE)
#' @import dplyr
#' @import tidyr
#'
#' @export
simulate_treament <- function(a = 3.5,
                          b = -1,
                          sigma_a = 1,
                          sigma_b = 0.8,
                          rho = -0.7,
                          n_subjects = 10,
                          n_trials = 20,
                          sigma = 0.5) {

    # Ccombine the terms
    mu <- c(a, b)
    cov_ab <- sigma_a * sigma_b * rho
    SD  <- matrix(c(sigma_a^2, cov_ab,
                       cov_ab, sigma_b^2), ncol = 2)

    varying_effects <-
        MASS::mvrnorm(n_subjects, mu, SD) |>
        # as_tibble(.name_repair = "unique") |>
        data.frame() |>
        purrr::set_names("a_j", "b_j")

    d_linpred <-
        varying_effects |>
        mutate(subject  = 1:n_subjects) |>
        expand(nesting(subject, a_j, b_j), post = c(0, 1)) |>
        mutate(mu = a_j + b_j * post,
               sigma = sigma) |>
        mutate(treatment = ifelse(post == 0, "pre", "post"),
               treatment = factor(treatment, levels = c("pre", "post")))

    d <- d_linpred |>
        slice(rep(1:n(), each = n_trials)) |>
        mutate(response = rnorm(n = n(), mean = mu, sd = sigma))

   d
}
