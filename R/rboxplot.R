#' @importFrom sn rsn
#' @importFrom tibble tibble
#' @importFrom dplyr mutate slice_sample
#' @importFrom purrr map
#' @importFrom forcats fct_inorder
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom stats rbinom rnorm median
NULL

#' Randomly sample a set of 4 boxplots for interpretation testing
#' @export
#' @param n the sample size per group, per boxplot
#' @param mu the difference in center
#' @param sd the difference in spread
#' @param skew the difference in skewness
#' @return a `data.frame` suitable for plotting with `ggplot2`
rboxplot <- function(n = 100, mu = 0.6, sd = 1.5, skew = 10){

  same <- function(n) {
    data.frame(A=rnorm(n, mean=0, sd=1),
               B=rnorm(n, mean=0, sd=1))
  }

  different_shape <- function(n, skewness) {
    if (rbinom(1,1,0.5)) { skewness = -skewness }

    # This recenters and rescales so that it has mean=mu, sd=sigma even if alpha != 0
    my_rsn <- function(n, mu, sigma, alpha) {
      delta <- alpha/sqrt(1 + alpha^2)
      sigma_sn <- sigma / sqrt(1 - 2*delta^2/pi)
      mu_sn <- mu - sigma_sn * delta * sqrt(2/pi)
      #  mu_z <- sqrt(2/pi)*delta
      #  gamma_1 <- (4-pi)/2 * (delta*sqrt(2/pi))^3/(1 - 2*delta^2/pi)^(3/2)
      #  m0 = mu_z - gamma_1*sqrt(1-mu_z^2)/2 - sign(alpha)/2*exp(-2*pi/abs(alpha))
      #  mu_sn <- mu - sigma_sn*delta
      sn::rsn(n,mu_sn,sigma_sn,alpha)
    }

    s1 = my_rsn(n,0,1,skewness)
    s2 = my_rsn(n,0,1,-skewness)
    diff_median = median(s2)-median(s1)
    s1 = s1 + diff_median*0.4
    s2 = s2 - diff_median*0.4
    data.frame(A = s1, B = s2)
  }

  different_spread <- function(n, spread) {
    if (rbinom(1,1,0.5)) { sd = 1/sd }
    data.frame(A = rnorm(n, mean=0, sd=1),
               B = rnorm(n, mean=0, sd=sd))
  }

  different_center <- function(n, center) {
    if (rbinom(1,1,0.5)) { center = -center }
    data.frame(A = rnorm(n, mean=0, sd=1),
               B = rnorm(n, mean=center, sd=1))
  }

  # generate our data
  tibble::tibble(evaluate = c("none", "center", "shape", "spread"),
                 data = list(same(n),
                             different_center(n, center=mu),
                             different_shape(n, skewness=skew),
                             different_spread(n, spread=sd))) %>%
    dplyr::mutate(data = purrr::map(data, ~tidyr::pivot_longer(., cols=A:B, names_to = 'group', values_to = 'value'))) %>%
    dplyr::slice_sample(n=4) %>%
    dplyr::mutate(evaluate = forcats::fct_inorder(evaluate))
}
