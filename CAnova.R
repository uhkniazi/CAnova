# Name: CAnova.R
# Auth: Umar Niazi u.niazi@imperial.ac.uk
# Date: 15/08/2015
# Desc: Class to perform anova and plot results visually

library(methods)

############################################################
## class declaration
# Name: CAnova
# Desc: Parent class to hold data and perform anova

# declaration
setClass('CAnova', slots=list(ivDat='numeric', fGroups='factor', col.type='character', col.dat='character', grand.mean='numeric',
                              grp.mean='array', grp.size='array', ssm='numeric', sst='numeric',
                              ssr='numeric', msm='numeric', msr='numeric', fratio='numeric', pvalue='numeric'))

# constructor
CAnova = function(ivDat, fGroups){
  ###### internal functions
  calculateSST = function(dat){
    return(var(dat)*(length(dat)-1))
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
  
  calculateSSR = function(dat, grp){
    x.grp = tapply(dat, grp, mean)
    #x.len = tapply(dat, grp, length)
    ssr = rep(NA, times=length(x.grp))
    for (i in seq_along(x.grp)){
      l = names(x.grp)[i]
      d = dat[grp == l]
      s = (d - x.grp[i])^2
      s = sum(s)
      ssr[i] = s
    }
    return(sum(ssr))
  }
  
  calculateMeanSquaresM = function(dat, grp){
    ssm = calculateSSM(dat, grp)
    df = length(levels(grp)) - 1
    return(ssm/df)
  }
  
  calculateMeanSquaresR = function(dat, grp){
    ssr = calculateSSR(dat, grp)
    df = tapply(dat, grp, length)
    df = sum(df - 1)
    return(ssr/df)
  }
  
  getFRatio = function(dat, grp){
    msm = calculateMeanSquaresM(dat, grp)
    msr = calculateMeanSquaresR(dat, grp)
    return(msm/msr)
  }
  
  getPValue = function(dat, grp){
    f = getFRatio(dat, grp)
    d1 = length(levels(grp)) - 1
    d2 = tapply(dat, grp, length)
    d2 = sum(d2 - 1)
    return(1-pf(f, d1, d2))
  }
  ###### end internal functions
  ## sort data on factors
  ivDat = ivDat[order(fGroups)]
  fGroups = fGroups[order(fGroups)]
  
  ###### perform calculations
  # choose colour
  col.type = rainbow(length(levels(fGroups)))
  col.dat = col.type[as.numeric(fGroups)]
  grand.mean = mean(ivDat)
  grp.mean = tapply(ivDat, fGroups, mean)
  grp.size = tapply(ivDat, fGroups, length)
  sst = calculateSST(ivDat)
  ssm = calculateSSM(ivDat, fGroups)
  ssr = calculateSSR(ivDat, fGroups)
  msm = calculateMeanSquaresM(ivDat, fGroups)
  msr = calculateMeanSquaresR(ivDat, fGroups)
  fratio = getFRatio(ivDat, fGroups)
  pvalue = getPValue(ivDat, fGroups)
  
  # create object
  obj = new('CAnova', ivDat=ivDat, fGroups=fGroups, col.type=col.type, col.dat=col.dat, grand.mean=grand.mean, 
            grp.mean=grp.mean, grp.size=grp.size, ssm=ssm, sst=sst, ssr=ssr, msm=msm, msr=msr, fratio=fratio, 
            pvalue=pvalue)
  return(obj)  
} # constructor


### plotting and summary functions
setGeneric('plot.canova', function(obj)standardGeneric('plot.canova'))
setMethod('plot.canova', signature = 'CAnova', definition = function(obj){
  # plot the diagnostics for the anova
  p.old = par(mar=c(3, 4, 2, 1)+0.1)
  
  ## plot no.1 - model with regression coefficients
  plot(1:length(obj@ivDat), obj@ivDat, col=obj@col.dat, xaxt='n', xlab='Index', ylab='Data', main='Visual Anova', pch=20)
  axis(side = 1, at = 1:length(obj@ivDat))
  abline(h = obj@grand.mean)
  # lines for group means
  sapply(seq_along(obj@grp.mean), function(x) {
    y.len = rep(obj@grp.mean[x], times=obj@grp.size[x])
    x.len = which(obj@fGroups == levels(obj@fGroups)[x])
    lines(x.len, y.len, col=obj@col.type[x], lwd=2)  
  } )
  
  # residuals for each group
  for (i in seq_along(obj@ivDat)){
    y.start = obj@ivDat[i]
    y.end = obj@grp.mean[obj@fGroups[i]]
    x.start = i
    x.end = i
    lines(c(x.start, x.end), c(y.start, y.end), lty=2, col=obj@col.dat[i], lwd=2)
  }
  # calculate regression coefficients i.e. differences in groups
  fit = lm(obj@ivDat ~ obj@fGroups)
  for (i in 2:length(coef(fit))){
    x.start = which(obj@fGroups == levels(obj@fGroups)[i])[1]
    x.start = x.start - 0.5
    y.start = obj@grp.mean[1]
    y.end = obj@grp.mean[i]
    arrows(x.start, y.start, x.start, y.end, code=0, lwd=2, angle=50)
  }
  # wait for next plot
  cat('Press Enter for next plot')
  readline()
  
  ## Sum of squares for null model
  plot(1:length(obj@ivDat), obj@ivDat, col=obj@col.dat, xaxt='n', xlab='Index', ylab='Data', main='Total Sum of Squares - NULL Model',
       pch=20)
  axis(side = 1, at = 1:length(obj@ivDat))
  abline(h = obj@grand.mean)
  # lines for residuals of each data point agasint grand mean
  for (i in seq_along(obj@ivDat)){
    y.start = obj@ivDat[i]
    y.end = obj@grand.mean
    x.start = i
    x.end = i
    lines(c(x.start, x.end), c(y.start, y.end), lty=2, col=obj@col.dat[i], lwd=2)
  }
  
  ## Sum of squares for model
  # wait for next plot
  cat('Press Enter for next plot')
  readline()
  plot(1:length(obj@ivDat), obj@ivDat, col=obj@col.dat, xaxt='n', xlab='Index', ylab='Value', 
       main='Model Sum of Squares - Explained Variance', pch=20)
  axis(side = 1, at = 1:length(obj@ivDat))
  abline(h = obj@grand.mean)
  # lines for group means
  sapply(seq_along(obj@grp.mean), function(x) {
    y.len = rep(obj@grp.mean[x], times=obj@grp.size[x])
    x.len = which(obj@fGroups == levels(obj@fGroups)[x])
    lines(x.len, y.len, col=obj@col.type[x], lwd=2)  
  } )
  
  # Difference between grand mean and group means i.e. predicted values
  for (i in seq_along(obj@ivDat)){
    y.start = obj@grp.mean[obj@fGroups[i]]
    y.end = obj@grand.mean
    x.start = i
    x.end = i
    lines(c(x.start, x.end), c(y.start, y.end), lty=2, col=obj@col.dat[i], lwd=2)
  }
  
  cat('Press Enter for next plot')
  readline()
  ## Sum of squares for residuals
  plot(1:length(obj@ivDat), obj@ivDat, col=obj@col.dat, xaxt='n', xlab='Index', ylab='Value',
       main='Residuals Sum of Squares - Unexplained Variance', pch=20)
  axis(side = 1, at = 1:length(obj@ivDat))
  abline(h = obj@grand.mean)
  sapply(seq_along(obj@grp.mean), function(x) {
    y.len = rep(obj@grp.mean[x], times=obj@grp.size[x])
    x.len = which(obj@fGroups == levels(obj@fGroups)[x])
    lines(x.len, y.len, col=obj@col.type[x], lwd=2)  
  } )
  # lines for residuals in each group
  y.end = rep(obj@grp.mean, times=obj@grp.size)
  for (i in seq_along(obj@ivDat)){
    y.start = obj@ivDat[i]
    x.start = i
    x.end = i
    lines(c(x.start, x.end), c(y.start, y.end[i]), lty=2, col=obj@col.dat[i], lwd=2)
  }
})

## print function
setGeneric('print.canova', function(obj) standardGeneric('print.canova'))
setMethod('print.canova', signature = 'CAnova', definition = function(obj){
  print(paste('Grand Mean =', round(obj@grand.mean, 3)))
  print(paste('Null model Sum of Squares =', round(obj@sst, 3)))
  print(paste('Model Sum of Squares =', round(obj@ssm, 3)))
  print(paste('Residual Sum of Squares =', round(obj@ssr, 3)))
  print(paste('Mean Squares Model =', round(obj@msm, 3)))
  print(paste('Mean Squares Residual =', round(obj@msr, 3)))
  print(paste('FRatio = MSm/MSr =', round(obj@fratio, 3)))
  print(paste('PValue =', signif(obj@pvalue, 3)))
  fit = lm(obj@ivDat ~ obj@fGroups)
  print(summary(fit))
  print(anova(fit))
})

