# inverse-gamma distribution for fitdist.R
#
# uploaded to github: 28.09.2019: done
#
# R version 3.6.1 (2019-07-05) -- "Action of the Toes"
# Platform: x86_64-w64-mingw32/x64 (64-bit)
#
# test: OK

library(fitdistrplus)  # fitdist()
library(invgamma)      # dinvgamma(), pinvgamma(), qinvgamma()


dat1 <- c(0.1729, 0.1789, 0.1929, 0.1938, 0.2032, 0.2034, 0.2111, 0.2151, 0.2159, 0.2171,
          0.2172, 0.2195, 0.2205, 0.2310, 0.2317, 0.2344, 0.2419, 0.2483, 0.2529, 0.2556,
          0.2620, 0.2621, 0.2667, 0.2804, 0.2814, 0.2888, 0.2933, 0.3073, 0.3138, 0.3228)


# Inverse-gamma distribution for fitdist()
# shape = alpha = a, rate = beta = b

# define the functions for the inverse-gamma distribution:
# probability density function = PDF, x > 0
dinvgamma2 <- function(x, a, b) dinvgamma(x, a, b)

# cumulative distribution function = CDF
pinvgamma2 <- function(q, a, b) pinvgamma(q, a, b)

# quantile function = inverse CDF
qinvgamma2 <- function(p, a, b) qinvgamma(p, a, b)


fit_dat1_invgamma <- fitdist(dat1, "invgamma2", start = list(a = 1.0, b = 1.0))

summary(fit_dat1_invgamma)

plot(fit_dat1_invgamma)

