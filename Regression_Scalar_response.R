#install.packages("fda")
library(fda)
print(names(CanadianWeather))
annualprec <- log10(apply(CanadianWeather$dailyAv[,,"Precipitation.mm"], 2,sum))
smallbasis  <- create.fourier.basis(c(0, 365), 25)
## The covariate is the temperature curve for each station
tempfd <- smooth.basis(day.5, CanadianWeather$dailyAv[,,"Temperature.C"], smallbasis)$fd
## smooths the daily temperature data for each station using the Fourier basis created earlier and stores it in the variable "tempfd"
precip.Temp1 <- fRegress(annualprec ~ tempfd, method="fRegress")
## performs functional regression using the annual precipitation as the response variable and the smoothed temperature data as the predictor variable.
#names(precip.Temp1)
annualprec.fit1 <- precip.Temp1$yhatfdobj
plot(annualprec.fit1, annualprec, type="p", pch="o")
lines(annualprec.fit1, annualprec.fit1, lty=2)
## plots the observed annual precipitation against the fitted values obtained from the regression
RMSE <- round(sqrt(mean((annualprec-annualprec.fit1)^2)),3) 
#print(paste("RMSE =",RMSE))
plot(precip.Temp1$betaestlist[[2]]) #plots the estimated regression function obtained from the functional regression



##
## Get the default setup and modify it
## the "model" value of the method argument causes the analysis
## to produce a list vector of arguments for calling the
## fRegress function
##
precip.Temp.mdl1 <- fRegress(annualprec ~ tempfd, method="model")
# First confirm we get the same answer as above by calling function fRegress() with these arguments:
precip.Temp.m <- do.call('fRegress', precip.Temp.mdl1)
all.equal(precip.Temp.m, precip.Temp1)
#outputs 'TRUE'



#  set up a smaller basis for beta2 than for temperature so that we get a more parsimonious fit to the data
nbetabasis2 <- 21  #  not much less, but we add some roughness penalization
betabasis2  <- create.fourier.basis(c(0, 365), nbetabasis2)
betafd2     <- fd(rep(0, nbetabasis2), betabasis2)
# add smoothing
betafdPar2  <- fdPar(betafd2, lambda=10)
# replace the regress coefficient function with this fdPar object
precip.Temp.mdl2 <- precip.Temp.mdl1
precip.Temp.mdl2[['betalist']][['tempfd']] <- betafdPar2
# Now do re-fit the data
precip.Temp2 <- do.call('fRegress', precip.Temp.mdl2)


# Compare the two fits:
#  degrees of freedom
precip.Temp1[['df']] # 26
precip.Temp2[['df']] # 22
#  root-mean-squared errors:
RMSE1 <- round(sqrt(mean(with(precip.Temp1, (yhatfdobj-yvec)^2))),3)
RMSE2 <- round(sqrt(mean(with(precip.Temp2, (yhatfdobj-yvec)^2))),3)
#print(c(RMSE1, RMSE2))
annualprec.fit2 <- precip.Temp2$yhatfdobj
plot(annualprec.fit2, annualprec, type="p", pch="o", xlab= "Predicted Log10 Annual Precipitation", ylab="Observed Log10 Annual Precipitation")
lines(annualprec.fit2, annualprec.fit2, lty=2)
#plots the observed annual precipitation against the fitted values from the more parsimonious model.
plot(precip.Temp2$betaestlist[[2]], xlab="Day of Year", ylab="Estimated Coefficients")
#plots the estimated regression function obtained from the more parsimonious model.


#it is primarily the temperatures in the early winter that provide the fit to log precipitation by temperature

## Manual construction of xfdlist and betalist
xfdlist <- list(const=rep(1, 35), tempfd=tempfd)

# The intercept must be constant for a scalar response
betabasis1 <- create.constant.basis(c(0, 365))
betafd1    <- fd(0, betabasis1)
betafdPar1 <- fdPar(betafd1)

betafd2     <- fd(matrix(0,7,1), create.bspline.basis(c(0, 365),7))
# convert to an fdPar object
betafdPar2  <- fdPar(betafd2)

betalist <- list(const=betafdPar1, tempfd=betafdPar2)

precip.Temp3   <- fRegress(annualprec, xfdlist, betalist)
annualprec.fit3 <- precip.Temp3$yhatfdobj
#  plot the data and the fit
plot(annualprec.fit3, annualprec, type="p", pch="o", xlab="Predicted Log10 Annual Precipitation", ylab="Observed Log10 Annual Precipitation")
lines(annualprec.fit3, annualprec.fit3)
plot(precip.Temp3$betaestlist[[2]], xlab="Day of Year", ylab="Estimated Coefficients")
