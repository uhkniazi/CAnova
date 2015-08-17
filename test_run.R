source('CAnova.R')
ivPlacebo = c(3, 2, 1, 1, 4)
ivLow = c(5, 2, 4, 2, 3)
ivHigh = c(7, 4, 5, 3, 6)

f = gl(3, 5, labels = c('Placebo', 'Low', 'High'))
d = c(ivPlacebo, ivLow, ivHigh)
ob = CAnova(d, f)
print.canova(ob)
plot.canova(ob)

