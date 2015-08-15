ivPlacebo = c(3, 2, 1, 1, 4)
ivLow = c(5, 2, 4, 2, 3)
ivHigh = c(7, 4, 5, 3, 6)

fGroups = gl(3, 5, labels = c('Placebo', 'Low', 'High'))


fit_model = function(dat, fac){
  return(lm(dat ~ fac))
}


ivDat = c(ivPlacebo, ivLow, ivHigh)
fit = lm(ivDat ~ fGroups)
col.type = rainbow(length(levels(fGroups)))
col.dat = col.type[as.numeric(fGroups)]
grand.mean = mean(ivDat)
grp.mean = tapply(ivDat, fGroups, mean)
grp.size = tapply(ivDat, fGroups, length)
plot(1:length(ivDat), ivDat, col=col.dat, xaxt='n', xlab='Index', ylab='Value')
axis(side = 1, at = 1:length(ivDat))
abline(h = grand.mean)
sapply(seq_along(grp.mean), function(x) {
  y.len = rep(grp.mean[x], times=grp.size[x])
  x.len = which(fGroups == levels(fGroups)[x])
  lines(x.len, y.len, col=col.type[x])  
} )

for (i in seq_along(ivDat)){
  y.start = ivDat[i]
  y.end = grp.mean[fGroups[i]]
  x.start = i
  x.end = i
  lines(c(x.start, x.end), c(y.start, y.end), lty=2, col=col.dat[i])
}

for (i in 2:length(coef(fit))){
  x.start = which(fGroups == levels(fGroups)[i])[1]
  x.start = x.start - 0.5
  y.start = grp.mean[1]
  y.end = grp.mean[i]
  arrows(x.start, y.start, x.start, y.end, code=0, lwd=2, angle=50)
}

########## sum of squares
calculateSST = function(dat){
  return(var(dat)*(length(dat)-1))
}

plot(1:length(ivDat), ivDat, col=col.dat, xaxt='n', xlab='Index', ylab='Value')
axis(side = 1, at = 1:length(ivDat))
abline(h = grand.mean)
for (i in seq_along(ivDat)){
  y.start = ivDat[i]
  y.end = grand.mean
  x.start = i
  x.end = i
  lines(c(x.start, x.end), c(y.start, y.end), lty=2, col=col.dat[i])
}


calculateSSM = function(dat, grp){
  x.grand = mean(dat)
  x.grp = tapply(dat, grp, mean)
  x.len = tapply(dat, grp, length)
  ssm = rep(NA, times=length(x.grp))
  for (i in seq_along(x.grp)){
    s = (x.grp[i] - x.grand)^2
    s = s * x.len[i]
    ssm[i] = s
  }
  return(sum(ssm))
}

plot(1:length(ivDat), ivDat, col=col.dat, xaxt='n', xlab='Index', ylab='Value')
axis(side = 1, at = 1:length(ivDat))
abline(h = grand.mean)
sapply(seq_along(grp.mean), function(x) {
  y.len = rep(grp.mean[x], times=grp.size[x])
  x.len = which(fGroups == levels(fGroups)[x])
  lines(x.len, y.len, col=col.type[x])  
} )

for (i in seq_along(ivDat)){
  y.start = grp.mean[fGroups[i]]
  y.end = grand.mean
  x.start = i
  x.end = i
  lines(c(x.start, x.end), c(y.start, y.end), lty=2, col=col.dat[i])
}
