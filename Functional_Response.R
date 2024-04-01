
###  functional response with vector explanatory variables  

## simplest:  formula interface

daybasis65 <- create.fourier.basis(rangeval=c(0, 365), nbasis=65,
                                   axes=list('axesIntervals'))
Temp.fd <- with(CanadianWeather, smooth.basisPar(day.5,
                                                 dailyAv[,,'Temperature.C'], daybasis65)$fd)
TempRgn.f <- fRegress(Temp.fd ~ region, CanadianWeather)

## Get the default setup and possibly modify it
TempRgn.mdl <- fRegress(Temp.fd ~ region, CanadianWeather, method='model')

# make desired modifications here then run

TempRgn.m <- do.call('fRegress', TempRgn.mdl)

# no change, so match the first run

all.equal(TempRgn.m, TempRgn.f)


##
## More detailed set up
##

region.contrasts <- model.matrix(~factor(CanadianWeather$region))
rgnContr3 <- region.contrasts
dim(rgnContr3) <- c(1, 35, 4)
dimnames(rgnContr3) <- list('', CanadianWeather$place, c('const',
                                                         paste('region', c('Atlantic', 'Continental', 'Pacific'), sep='.')) )

const365 <- create.constant.basis(c(0, 365))
region.fd.Atlantic <- fd(matrix(rgnContr3[,,2], 1), const365)
# str(region.fd.Atlantic)
region.fd.Continental <- fd(matrix(rgnContr3[,,3], 1), const365)
region.fd.Pacific <- fd(matrix(rgnContr3[,,4], 1), const365)
region.fdlist <- list(const=rep(1, 35),
                      region.Atlantic=region.fd.Atlantic,
                      region.Continental=region.fd.Continental,
                      region.Pacific=region.fd.Pacific)
# str(TempRgn.mdl$betalist)

###
###
###  functional response with functional explanatory variable  
###
###

##
##  predict knee angle from hip angle;  
##     from demo('gait', package='fda')

##
## formula interface
##
gaittime   <- as.matrix((1:20)/21)
gaitrange  <- c(0,20)
gaitbasis  <- create.fourier.basis(gaitrange, nbasis=21)
gaitnbasis <- gaitbasis$nbasis
gaitcoef   <- matrix(0,gaitnbasis,dim(gait)[2])
harmaccelLfd <- vec2Lfd(c(0, (2*pi/20)^2, 0), rangeval=gaitrange)
gaitfd     <- smooth.basisPar(gaittime, gait, gaitbasis, 
                              Lfdobj=harmaccelLfd, lambda=1e-2)$fd
hipfd  <- gaitfd[,1]
kneefd <- gaitfd[,2]

knee.hip.f <- fRegress(kneefd ~ hipfd)

##
## manual set-up
##

#  set up the list of covariate objects
const  <- rep(1, dim(kneefd$coef)[2])
xfdlist  <- list(const=const, hipfd=hipfd)

beta0 <- with(kneefd, fd(gaitcoef, gaitbasis, fdnames))
beta1 <- with(hipfd,  fd(gaitcoef, gaitbasis, fdnames))

betalist  <- list(const=fdPar(beta0), hipfd=fdPar(beta1))

fRegressout <- fRegress(kneefd, xfdlist, betalist)
par(oldpar)