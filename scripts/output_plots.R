
# output plots
#
#

# marginal model ----
# cure fraction

xx <- extract(out)
hist(xx$curefrac,
     breaks = 30,
     freq = FALSE,
     xlim = c(0,1),
     xlab = "probability",
     main = "cure fraction")
lines(density(xx$pmean_cf), col = "red")
lines(density(xx$curefrac), col = "blue")







