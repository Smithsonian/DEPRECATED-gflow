##====================================================================
## This program illstrates how to do basic overlap calculations, using
## Tiger and Muntjac as an example.
##
## Refer to the following paper for details of the methodology:
## Ridout, M.S. and Linkie, M. (2009) 
## Estimating overlap of daily activity patterns from camera trap data
## Journal of Agricultural, Biological and Environmental Statistics,
## 14, 322-337.
##====================================================================

install.packages( c("boot", "circular"))

##-------------------------------
##Read the contents of a file that contains various 
##functions that are used in the analysis
##-------------------------------

require(boot)
require(circular)

#=====================================
 ovlest.circular <- function( x, y, c) 
#=====================================
{
        ##-----------------------------------------------
        ## Calculates the various estimators of overlap
        ## between two samples of circular data, x and y.
        ## c is a parameter used in calculating the 
        ## bandwidth for kernel density estimation.

        ## First calculate the bandwidths. These depend
        ## on (the size and variance of) the sample.
        ##-----------------------------------------------
    bwx = c * var.circular(x) / (length(x)^0.4)
    bwy = c * var.circular(y) / (length(y)^0.4)

        ##---------------------------------------
        ## Calculate the kernel density estimates
        ##---------------------------------------
    c0 = circular(0);  c2pi = circular(2*pi)
    dcx = density.circular(x, bw=1/bwx, from=c0, to=c2pi)
    dcy = density.circular(y, bw=1/bwy, from=c0, to=c2pi)

        ##---------------------------------------------
        ## Calculate ordinary (non-circular) vectors 
        ## containing the estimated densities (dx, dy)
        ## and the regular grids of 512 points at which 
        ## these are evaluated (grid)
        ##---------------------------------------------
    dx = as.vector(dcx$y) * 2 * pi / 511
    dy = as.vector(dcy$y) * 2 * pi / 511
    grid = as.vector(dcx$x)
#print(cbind(sum(dx), sum(dy)))
#print(cbind(dy[1],dy[512]))
    ovlcalc.circular(grid, dx, dy, x, y)
}

#======================================
 ovlest.robust <- function( x, y, kmax) 
#======================================
{
        ##-----------------------------------------------
        ## Calculates the various estimators of overlap
        ## between two samples of circular data, x and y,
        ## using the plug-in estimator of bandwidth with
        ## robust estimation of kappa (determined by the
        ## value of k)
        ## First calculate the bandwidths using the robust
        ## approach. 
        ##-----------------------------------------------

    Afun <- function(kappa, trigmom, k) {
       besselI(kappa,k)/besselI(kappa,0) - trigmom
    }
    estkappax = numeric(kmax)
    estkappay = numeric(kmax)

    for (k in 1:kmax) {
        trigmomx = trigonometric.moment(x, k, center=TRUE)$rho
        trigmomy = trigonometric.moment(y, k, center=TRUE)$rho
        estkappax[k] = uniroot(Afun, c(0.001,100), trigmomx, k)$root
        estkappay[k] = uniroot(Afun, c(0.001,100), trigmomy, k)$root
    }
    kappahat = max(estkappax[1:kmax])
    bwx = ( 3*length(y)*kappahat^2*besselI(2*kappahat,2) / 
          (4*sqrt(pi)*besselI(kappahat,0)^2) ) ^(2/5)
    kappahat = max(estkappay[1:kmax])
    bwy = ( 3*length(y)*kappahat^2*besselI(2*kappahat,2) / 
          (4*sqrt(pi)*besselI(kappahat,0)^2) ) ^(2/5)

        ##---------------------------------------
        ## Calculate the kernel density estimates
        ##---------------------------------------
    c0 = circular(0);  c2pi = circular(2*pi)
    dcx = density.circular(x, bw=bwx, from=c0, to=c2pi)
    dcy = density.circular(y, bw=bwy, from=c0, to=c2pi)

        ##---------------------------------------------
        ## Calculate ordinary (non-circular) vectors 
        ## containing the estimated densities (dx, dy)
        ## and the regular grids of 512 points at which 
        ## these are evaluated (grid)
        ##---------------------------------------------
    dx = as.vector(dcx$y) * 2 * pi / 511
    dy = as.vector(dcy$y) * 2 * pi / 511
    grid = as.vector(dcx$x)
#print(cbind(sum(dx), sum(dy)))
#print(cbind(dy[1],dy[512]))
    ovlcalc.circular(grid, dx, dy, x, y)
}

#==========================================
 ovlest.robust2 <- function( x, y, kmax, c) 
#==========================================
{
        ##-----------------------------------------------
        ## Calculates the various estimators of overlap
        ## between two samples of circular data, x and y,
        ## using the plug-in estimator of bandwidth with
        ## robust estimation of kappa (determined by the
        ## value of k)
        ## First calculate the bandwidths using the robust
        ## approach. 
        ##-----------------------------------------------

    Afun <- function(kappa, trigmom, k) {
       besselI(kappa,k)/besselI(kappa,0) - trigmom
    }
    estkappax = numeric(kmax)
    estkappay = numeric(kmax)

    for (k in 1:kmax) {
        trigmomx = trigonometric.moment(x, k, center=TRUE)$rho
        trigmomy = trigonometric.moment(y, k, center=TRUE)$rho
  if (Afun(0.0001,trigmomx,k) * Afun(500,trigmomx,k) >= 0) {
     print(x)
     stop("Uniroot Err x")
  }
  if (Afun(0.0001,trigmomy,k) * Afun(500,trigmomy,k) >= 0) {
     print(y)
     stop("Uniroot Err y")  
  }
        estkappax[k] = uniroot(Afun, c(0.0001,500), trigmomx, k)$root
        estkappay[k] = uniroot(Afun, c(0.0001,500), trigmomy, k)$root
    }
    kappahat = max(estkappax[1:kmax])
    bwx = ( 3*length(x)*kappahat^2*besselI(2*kappahat,2) / 
          (4*sqrt(pi)*besselI(kappahat,0)^2) ) ^(2/5)
    kappahat = max(estkappay[1:kmax])
    bwy = ( 3*length(y)*kappahat^2*besselI(2*kappahat,2) / 
          (4*sqrt(pi)*besselI(kappahat,0)^2) ) ^(2/5)
    bwx = c*bwx
    bwy = c*bwy

        ##---------------------------------------
        ## Calculate the kernel density estimates
        ##---------------------------------------
    c0 = circular(0);  c2pi = circular(2*pi)
    dcx = density.circular(x, bw=bwx, from=c0, to=c2pi)
    dcy = density.circular(y, bw=bwy, from=c0, to=c2pi)

        ##---------------------------------------------
        ## Calculate ordinary (non-circular) vectors 
        ## containing the estimated densities (dx, dy)
        ## and the regular grids of 512 points at which 
        ## these are evaluated (grid)
        ##---------------------------------------------
    dx = as.vector(dcx$y) * 2 * pi / 511
    dy = as.vector(dcy$y) * 2 * pi / 511
    grid = as.vector(dcx$x)
#print(cbind(sum(dx), sum(dy)))
#print(cbind(dy[1],dy[512]))
    ovlcalc.circular(grid, dx, dy, x, y)
}


#=================================================
 ovlcalc.circular <- function( grid, dx, dy, x, y) 
#=================================================
{
        ##---------------------------------------------
        ## Calculate various estimators of overlap from
        ## two estimated densities, dx and dy at a set
        ## of grid points grid. All these arguments are 
        ## vectors of the same length. The actual data 
        ## are given in arguments x and y.
        ##
        ## First calculate the estimated densities at
        ## the sample values of x and y. 
        ## dx.x = density for x from estimated pdf of x
        ## dx.y = density for y from estimated pdf of x
        ## dy.x = density for x from estimated pdf of y
        ## dy.y = density for y from estimated pdf of y
        ## This uses spline interpolation. 
        ##---------------------------------------------
    dx.x = approx( grid, dx, as.vector(x))$y
    dx.y = approx( grid, dx, as.vector(y))$y
    dy.x = approx( grid, dy, as.vector(x))$y
    dy.y = approx( grid, dy, as.vector(y))$y

        ##-----------------------------------------
        ## Now calculate the various estimators of 
        ## overlap. OVL[3] not defined for circular
        ## data Note the endpoint corrections for 
        ## ovl[1] andovl[2] (trapezoidal rule).
        ##-----------------------------------------

#plot(grid, dx, type="l")
#lines(grid, dy, col="red")
#plot(grid, dy, type="l")
#lines(grid, dx, col="red")
    ovl = numeric(5)
    ovl[1] = sum(pmin(dx,dy)) - pmin(dx[1],dy[1])
    ovl[2] = 1 - 1/2 * (sum(abs(dx-dy)) - abs(dx[1]-dy[1]))
    ovl[3] = NA
    ovl[4] = (mean(pmin(1, dy.x/dx.x)) + mean(pmin(1, dx.y/dy.y))) / 2
    ovl[5] = (mean(dx.x < dy.x) + mean(dy.y <= dx.y))

    ovl
}


#==========================
 trigpdf <- function(param)
#==========================
{
        ##-----------------------------------------------
        ## Calculate the pdf of the trigonometric series
        ## distribution with a given set of parameter
        ## values. (The order of trigonometric series is
        ## determined by the length of the argument param
        ##-----------------------------------------------

    lp = length(param)
    n = (lp) / 2

    rpart = c(param[1:n],0)
    c.r =  sqrt( exp(rpart)/sum(exp(rpart)) / (2*pi) )
    c.theta = c(0, param[(n+1):lp])
    c = c.r * exp(1i*c.theta)

    ab = numeric(n)
    multiplier = 2 * pi
    for (k in 1:n) {
         for (nu in 0: (n-k)) {
             ab[k] = ab[k] + multiplier * c[nu+k+1] * Conj(c[nu+1])
         }
    }
    a = Re(ab)
    b = -Im(ab)

    kk = c(1:n)
    theta = seq(0, 2*pi, length.out=512)

    f = 1/(2*pi)
    for (k in 1:n) {
        f = f +  1 / pi * ( a[k] * cos(k*theta) 
                          + b[k] * sin(k*theta))
    }
    list(theta = theta, f = f)
}


#=============================
 AICtrig <- function(param, x)
#=============================
{
        ##-----------------------------------------------
        ## Calculate the AIC of the trigonometric series
        ## distribution with a given set of parameter
        ## values and a given set of data x. (The order 
        ## of trigonometric series is determined by the 
        ## length of the argument param
        ##-----------------------------------------------

    lp = length(param)
    n = (lp) / 2

    rpart = c(param[1:n],0)
    c.r =  sqrt( exp(rpart)/sum(exp(rpart)) / (2*pi) )
    c.theta = c(0, param[(n+1):lp] )
    c = c.r * exp(1i*c.theta)

    ab = numeric(n)
    multiplier = 2 * pi
    for (k in 1:n) {
        for (nu in 0: (n-k)) {
            ab[k] = ab[k] + multiplier * c[nu+k+1] *
                    Conj(c[nu+1])
        }
    }
    a = Re(ab)
    b = -Im(ab)

    f = 1/(2*pi)
    for (k in 1:n) {
        f = f +  1/pi * ( a[k] * cos(k*x) + 
                          b[k] * sin(k*x))
    }

       ## BIC not used at present
   #BIC = -2 * sum(log(f)) + log(length(x)) * length(param)
    AIC = -2 * (sum(log(f)) - length(param))
    AIC
}

#===================================
 fit.trigseries <- function(x, nrep)
#===================================
{
        ##--------------------------------------------------
        ## Fit trigonometric series distribution to circular
        ## data x using minimum AIC criterion. Try number of 
        ## terms in series equal to 1, 2, ..., length(nrep).
        ## Use multiple starting points for optimisation,
        ## the number to use being set by the elements of
        ## nrep. Returns fitted density for the best AIC
        ## value.
        ##--------------------------------------------------

    bestn = 0
    nmax = length(nrep)

    allfv <- matrix(0, nrow=512, ncol=nmax)
    fv = numeric(512)

    yvar = as.vector(x)
    best = 99.99E49

    for (n in 1:nmax) {
        minaic = 99.99E49
        aic = numeric(nrep[n])
        for (jj in 1:nrep[n]) {
            param = c(rnorm(n, sd=3), runif(n, 0, 2*pi))
            fitted = optim(param, AICtrig,, yvar,
                           control=list(maxit=5000))
            fitted = optim(fitted$par, AICtrig, , yvar)
            aic[jj] = fitted$val
            if (aic[jj] < minaic) {
               bestaic = fitted
               minaic = aic[jj]
            }
        }
#        print(cbind(n, min(aic)))
 #       aic = sort(aic)
        ctrig = trigpdf(bestaic$par)
        allfv[,n] = 2 * pi * ctrig$f
        if (bestaic$value < best) {
           best = bestaic$value
           bestn = n
        }
    }
#    cat("Number of components by AIC = ", bestn,"\n")
    allfv[,bestn] / 511
}



#==================================================
  bootstrap.ovl <- function(x, y, kmax, c, nboot) {
#==================================================

nx = length(x)
ny = length(y)
boot.sample = array(0, dim=c(nboot,3))

for (iboot in 1:nboot) {
    xx = sample(x, replace=TRUE)
    yy = sample(y, replace=TRUE)
    ovl.val = ovlest.robust2( xx, yy, kmax, c)
    boot.sample[iboot,1] = ovl.val[1] 
    boot.sample[iboot,2] = ovl.val[4] 
    boot.sample[iboot,3] = ovl.val[5] 
}

boot.sample
}


#==========================================
  bootstrap.trig <- function(x, y, nboot) {
#==========================================

nx = length(x)
ny = length(y)
boot.sample = array(0, dim=c(nboot,3))

grid = seq(0, 2*pi, length.out=512)
fitxx = numeric(512)
fityy = numeric(512)

for (iboot in 1:nboot) {

    xx = sample(x, replace=TRUE)
    yy = sample(y, replace=TRUE)

    fitxx = fit.trigseries(xx, nrep=c(5,5,5,5))
    fityy = fit.trigseries(yy, nrep=c(5,5,5,5))

    ovl.val = ovlcalc.circular(grid, fitxx, fityy, xx, yy)
    boot.sample[iboot,1] = ovl.val[1] 
    boot.sample[iboot,2] = ovl.val[4] 
    boot.sample[iboot,3] = ovl.val[5] 
}

boot.sample
}




##
#===================================================
  bootstrap.both <- function(x, y, kmax, c, nboot) 
#===================================================
{
nx = length(x)
ny = length(y)

grid = seq(0, 2*pi, length.out=512)
fitxx = numeric(512)
fityy = numeric(512)

boot.sample = array(0, dim=c(nboot,6))

for (iboot in 1:nboot) {
    xx = sample(x, replace=TRUE)
    yy = sample(y, replace=TRUE)
    ovl.val = ovlest.robust2( xx, yy, kmax, c)
    boot.sample[iboot,1] = ovl.val[1] 
    boot.sample[iboot,2] = ovl.val[4] 
    boot.sample[iboot,3] = ovl.val[5] 

    fitxx = fit.trigseries(xx, nrep=c(5,5,5,5))
    fityy = fit.trigseries(yy, nrep=c(5,5,5,5))

    ovl.val = ovlcalc.circular(grid, fitxx, fityy, xx, yy)
    boot.sample[iboot,4] = ovl.val[1] 
    boot.sample[iboot,5] = ovl.val[4] 
    boot.sample[iboot,6] = ovl.val[5] 

}

boot.sample
}



#=================================
  kplot <- function( x, kmax, c=1)
#=================================
{
    ## Calculate kernel density estimates for plotting

    Afun <- function(kappa, trigmom, k) {
       besselI(kappa,k)/besselI(kappa,0) - trigmom
    }
    estkappa = numeric(kmax)

    for (k in 1:kmax) {
        trigmom = trigonometric.moment(x, k, center=TRUE)$rho
        estkappa[k] = uniroot(Afun, c(0.001,100), trigmom, k)$root
    }
    kappahat = max(estkappa[1:kmax])

    bw = ( 3*length(x)*kappahat^2*besselI(2*kappahat,2) / 
         (4*sqrt(pi)*besselI(kappahat,0)^2) ) ^(2/5)

    dc = density.circular(x, bw=c*bw)
    dx = as.vector(dc$x)/2/pi
    dy = as.vector(dc$y)*2*pi
    list(x = dx, y = dy)
}

##-------------------------------
##The .txt data file contains a nameless column for observations of each species
##Read in the raw data and create a variable for each species
##in a circular format and with missing values removed
##-------------------------------

rawtimes <- read.table("C:/Montgomery/My_Work/STATISTICS/My R Files/CameraTrap/Activity_Overlap_Code/traptimes.txt")
times <- list(1)
for (j in c(1,2,3,4,5,6,7,8)) {
    tmp <- circular(2*pi*na.omit(rawtimes[,j]))
    times[[j]] <- tmp
}

ow <- options("warn")
options(warn = -1)
tiger = as.circular(na.omit(as.vector(times[[1]])))
muntjac = as.circular(na.omit(as.vector(times[[2]])))
sambar = as.circular(na.omit(as.vector(times[[3]])))
tapir = as.circular(na.omit(as.vector(times[[4]])))
boar = as.circular(na.omit(as.vector(times[[5]])))
clouded = as.circular(na.omit(as.vector(times[[6]])))
golden = as.circular(na.omit(as.vector(times[[7]])))
macaque = as.circular(na.omit(as.vector(times[[8]])))
options(ow) # reset

##-------------------------------
##Set up the site indicator for each species
##(camera trap records come from 4 sites)
##-------------------------------

ftiger = c(rep(1,15), rep(2,83), rep(3,52), rep(4,51))
fmuntjac = c(rep(1,11), rep(2,99), rep(3,61), rep(4,29))
fsambar = c(rep(1,1), rep(2,14), rep(3,5), rep(4,5))
ftapir = c(rep(1,13), rep(2,61), rep(3,42), rep(4,65))
fboar = c(rep(2,7), rep(3,6), rep(4,15))
fclouded = c(rep(1,27), rep(2,10), rep(3,17), rep(4,32))
fgolden = c(rep(1,14), rep(2,26), rep(3,38), rep(4,26))
fmacaque = c(rep(1,23), rep(2,125), rep(3,59), rep(4,66))


    ##-------------------------------
    ## Set up for two figures on page
    ##-------------------------------
par(mfrow=c(2,1))

    ##----------------------------------------------------
    ## Basic kernel density estimation. You need to choose
    ## values for the parameters K and c discussed in the
    ## paper by Ridout & Linkie.
    ##
    ## These commands do the estimation and plot the 
    ## estimates (tiger is the dashed line). You can ignore 
    ## the warning messages that are generated by the 
    ## circular package.
    ##----------------------------------------------------
K = 3
c = 1
kernel.tiger = kplot( tiger, K, c)
kernel.muntjac = kplot( muntjac, K, c)

    ##--------------------------------------------------------------
    ## Note the /24 needed on the y-axis, to get the correct scaling
    ##--------------------------------------------------------------
plot(kernel.muntjac$x, kernel.muntjac$y/24, type="l", xaxt="n", 
     xlab="Tiger (dashed), Muntjac (solid)", ylab="Activity density")
lines(kernel.tiger$x, kernel.tiger$y/24, lty="dashed", col="red") 
axis(1, at=c(0,0.25,0.5,0.75,1), labels=c("0:00", "6:00", "12:00", "18:00", "24:00"))

    ##----------------------------------------------------------
    ##--------------------------------
    ## Calculate the ovlerap estimates. Produces the estimates
    ## Delta_1_hat ... Delta_5_hat. Note that for circular data
    ## (like time of day), Delta_3_hat is not defined (hence the 
    ## value is given as NA, and that Delta_1_hat = Delta_2_hat.
    ## So there are only 3 distinct estimates, Delta_1_hat,
    ## Delta_4_hat and Delta_5_hat (and these are the only ones
    ## that we print here. See the Ridout & Linkie 
    ## paper for recommendations about which one to use.
    ##----------------------------------------------------------
kernel.overlap = ovlest.robust2( tiger, muntjac, K, c)
cat("\nEstimates of overlap based on kernel estimates of density\n")
print(kernel.overlap[c(1,4,5)])

    ##-----------------------------------------------------------
    ## Alternatively, we can fit trigonometric sum distributions.
    ## This is much slower, because of the need to do lots of
    ## fits. The second term
    ##    nrep = c(10,10,10,10)
    ## means that models of order 1, 2, 3, 4 will be fitted
    ## with 10 starting points for each (because of possible
    ## problems with multiple optima). The best model is
    ## selected automatically by AIC.
    ##-----------------------------------------------------------
u = seq(0,1, length.out=512)
tsd.tiger = fit.trigseries(tiger, nrep=c(10,10,10,10))
tsd.muntjac = fit.trigseries(muntjac, nrep=c(10,10,10,10))

    ##--------------------------------------------------------------
    ## Note that the conversion to 24-hour period is different here,
    ## need to MULTIPLY by 24 to get the correct scaling
    ##--------------------------------------------------------------
plot(u, tsd.muntjac*24, type="l", xaxt="n",
     xlab="Tiger (dashed), Muntjac (solid)", ylab="Activity density")
lines(u, tsd.tiger*24, lty="dashed", col="red") 
axis(1, at=c(0,0.25,0.5,0.75,1), labels=c("0:00", "6:00", "12:00", "18:00", "24:00"))

    ##---------------------------------------------------------------
    ## Here is a function to calculate overlap based on trigonometric
    ## sum distributions. 
    ##---------------------------------------------------------------
grid = seq(0, 2*pi, length.out=512)
trig.overlap = ovlcalc.circular(grid, tsd.tiger, tsd.muntjac, tiger, muntjac)
cat("\nEstimates of overlap (1,4,5) based on trigonometric sum estimates of density\n")
print(trig.overlap[c(1,4,5)])

    ##-----------------------------------------------------
    ## Bootstrap based confidence limits (95%)
    ## nboot = number of bootstrap samples. Set to a fairly
    ##         small value here, for speed. Papers used a 
    ##         value of nboot = 500
    ##-----------------------------------------------------
nboot = 50
boot.sample = bootstrap.ovl(tiger, muntjac, K, c, nboot)

    ##-----------------------------------------
    ## Print the individual bootstrap estimates
    ##-----------------------------------------
print(boot.sample)

    ##--------------------------------------------
    ## Calculate and display the confidence limits
    ##--------------------------------------------
lower = numeric(3)
upper = numeric(3)
lower.percentile = 0.025
upper.percentile = 0.975
for (j in 1: 3) {
    lower[j] = quantile(boot.sample[,j], lower.percentile)
    upper[j] = quantile(boot.sample[,j], upper.percentile)
}
cat("\nLower confidence limit for estimates 1, 4 and 5\n")
print(lower)
cat("\nUpper confidence limit for estimates 1, 4 and 5\n")
print(upper)

   ##----------------------------------------------------
   ## Note that we haven't calculated bootstrap intervals
   ## for overlap estimates based on trigonometric sum
   ## distributions. This can be done (see the function
   ## bootstrap.trig in ovlcode.r, or the function 
   ## bootstrap.both, which does both kernel and trig
   ## sum-based estimates at once. However, bootstrap
   ## calculations that involve fitting trigonometric
   ## sum distributions are VERY slow. Since they give
   ## similar results, in our experience, to using
   ## kernel density estimates, they may not often be
   ## needed.
   ##----------------------------------------------------
