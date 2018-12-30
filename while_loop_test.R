# while_loop_test.R
#
# PLC-Programmer, 30.12.2018
#
# idea:
# - while-loops are more flexible than for-loops
#   => so this is only a very simple exercise
#
# env.: R version 3.5.1 (2018-07-02) -- "Feather Spray"; Platform: x86_64-w64-mingw32/x64 (64-bit)
#
# test: OK

i <- 1
while (TRUE) {
 print("------")
 print(sprintf("**i = %i**", i))
 if (i >= 10) {break}

 j <- 1
 while (TRUE) {
   print(sprintf("j = %i", j))
   if (j >= 10) {break}
   j <- j + 1
 }

 i <- i + 1
}
