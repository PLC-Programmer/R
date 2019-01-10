# compute_pi_b.R
#
# computes digits of Pi
#
# check digits from here: http://www.geom.uiuc.edu/~huberty/math5337/groupe/digits.html
#
#
# source: A Spigot Algorithm for the Digits of Pi
#         by Stanley Rabinowitz and Stan Wagon
#         The American Mathematical Monthly, 102 (1995), 195–203
#
# 10.01.2019 07:21:30
#
# test: OK
#
# env.: R version 3.5.2 (2018-12-20) -- "Eggshell Igloo"; Platform: x86_64-w64-mingw32/x64 (64-bit)

file_name <- "DIGITS.txt"
DIGITS <- "The first digits of number pi are:"
write(DIGITS, file = file_name, append = FALSE)

n   <- 10010
len <- (10 * n) %/% 3

a   <- NULL
for (j in 1:len) {
  a[j] <- 2
}

nines    <- 0
predigit <- 0

DIGITS <- ""
nbr    <- 0

for (j in 1:n) {
  q <- 0

  for (i in seq(from = len, to = 1, by = -1)) {
    x    <- 10 * a[i] + q * i
    a[i] <- x %% (2 * i - 1)
    q    <- x %/% (2 * i - 1)
  }

  a[1] <- q %% 10
  q    <- q %/% 10

  if (q == 9) {
    nines <- nines + 1
  }
  else {
    if (q == 10) {
      DIGITS <- paste(DIGITS, predigit + 1, sep = "")

      if (nines > 0) {
        for (k in 1:nines) {
          DIGITS <- paste(DIGITS, "0", sep = "")
        }
      }
      nines    <- 0
      predigit <- 0
    }
    else {
      DIGITS <- paste(DIGITS, predigit, sep = "")
      predigit <- q

      if (nines != 0) {
        if (nines > 0) {
          for (k in 1:nines) {
            DIGITS <- paste(DIGITS, "9", sep = "")
          }
        }
        nines <- 0
      }
    }
  }

  if (nchar(DIGITS) >= 50) {
    nbr <- nbr + nchar(DIGITS)
    print(DIGITS)
    write(DIGITS, file = file_name, append = TRUE)

    if (nbr <= 50) {
      first_digits <- DIGITS
    }

    DIGITS <- ""
  }
}

nbr <- nbr + nchar(DIGITS) - 1

write(DIGITS, file = file_name, append = TRUE)
print(paste("Result written to file:", file_name))
print(paste("Approximate number of calculated digits of number pi:", nbr))
print(paste("First 49 digits of pi are:", substr(first_digits, 2, 51)))


# output:
# [1] "Result written to file: DIGITS.txt"
# [1] "Approximate number of calculated digits of number pi: 10009"
# [1] "First 49 digits of pi are: 3141592653589793238462643383279502884197169399375"

# end of compute_pi_b.R
