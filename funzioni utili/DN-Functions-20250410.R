

library(splines2)


################################################################################
## Spline functions
################################################################################

.date.2.POSIXct <- function(date, hour = "00:00:00", tz = "GMT")
{
  x1 <- paste0(date, " ", hour)
  strptime(x = x1, format = "%Y-%m-%d %H:%M:%S", tz = tz)
}
# ------------------------------------------------------------------------------


.date.2.t <- function(date)
{
  x1 <- as.numeric( format(date[1], "%j") )
  seq(from = x1 / 365.25, by = 1 / 365.25, length.out = NROW(date))
}
# ------------------------------------------------------------------------------


.is.leapyear <- function(year)
{
  ( (year %% 4 == 0) & (year %% 100 != 0) ) | (year %% 400 == 0)
}
# ------------------------------------------------------------------------------


.ndaysInYear <- 
function(year)
{
  #### Number of days by year
  365 + .is.leapyear(year = year)
}
# ------------------------------------------------------------------------------


.xreg.trspl <- 
function(time, nk, degree = 3, demean = FALSE)
{
  #### 'time' must be equally spaced
  tz <- format(x = time[1], "%Z")
  time  <- as.POSIXct(x = time, tz = tz)
  timeN <- as.numeric(time)
  binN <- abs(diff(timeN[1:2]))
  #### Settings
  nobs <- NROW(time)
  #### Extend date up the end of the year
  dateL <- time[nobs]
  x1 <- paste0(format(dateL, "%Y"), "-12-31 23:59:59")
  x1 <- strptime(x = x1, format = "%Y-%m-%d %H:%M:%S", tz = tz)
  eoy <- as.numeric(x1)
  timeN1 <- c(timeN, seq(from = timeN[nobs], to = eoy, by = binN)[-1])
  nobs1 <- NROW(timeN1)
  #### Equally spaced knots
  # by <- (timeN1[nobs1] - timeN1[1]) / nk
  # knots1 <- seq(from = timeN1[1] + 0.5*by, by = by, length.out = nk)
  by <- (timeN1[nobs1] - timeN1[1]) / (nk + 1)
  knots1 <- seq(from = timeN1[1] + by, by = by, length.out = nk)
  #### Make
  x1 <- splines2::naturalSpline(x = timeN1, knots = knots1, degree = degree)
  colnames(x1) <- paste0("tspl.", 1 : NCOL(x1))
  #### Restore attributes
  attr <- attributes(x1)
  x1 <- x1[1:nobs, , drop = FALSE]
  attr$dim <- dim(x1)
  attributes(x1) <- attr
  #### Demean
  if (demean[1])
  {
    x1 <- x1 - matrix(colMeans(x1), NROW(x1), NCOL(x1), TRUE)
  }
  
  #### Answer
  x1
}
# ------------------------------------------------------------------------------


.year.frac <- 
function(time)
{
  #### Number of seconds by year
  tz <- format(x = time[1], "%Z")
  year <- format(x = time, format = "%Y")
  x1 <- as.numeric( unique(year) )
  x2 <- .ndaysInYear(year = x1) * 86400
  ####
  rec <- paste(paste0(x1, "=", x2), collapse = ";")
  nsy <- car::recode(var = year, recodes = rec)
  #### Number of periods in each day
  time  <- as.POSIXct(x = time, tz = tz)
  time0 <- strptime(x = paste0(year, "-01-01 00:00:00"), 
    format = "%Y-%m-%d %H:%M:%S", tz = tz)
  timeN  <- as.numeric(time)
  timeN0 <- as.numeric(time0)
  #### Answer
  (timeN - timeN0) / nsy
}
# ------------------------------------------------------------------------------


.xreg.perspl <- 
function(time, knot, degree = 3, demean = FALSE)
{
  #### x1 = fraction of year
  x1 <- .year.frac(time = time)
  #### knots1 = equally spaced knots
  knots1 <- if ( NROW(knot) == 1 )
  {
    by <- 1 / knot
    # seq(from = 0.5 * by, by = by, length.out = knot)
    seq(from = 0, by = by, length.out = knot)[-1]
  }
  else
  {
    knots1 <- knot
  }
  #### Make
  x1 <- splines2::mSpline(x = x1, knots = knots1, degree = degree,
    intercept = FALSE, Boundary.knots = c(0,1), periodic = TRUE)
  colnames(x1) <- paste0("pspl.", colnames(x1))
  #### Demean
  if (demean[1])
  {
    x1 <- x1 - matrix(colMeans(x1), NROW(x1), NCOL(x1), TRUE)
  }
  #### Answer
  x1
}
# ------------------------------------------------------------------------------


.xreg.perfou <- 
function(date, K)
{
  #### Settings
  year <- format(x = date, format = "%Y")
  date <- split(x = date, f = year)
  ndy1 <- .ndaysInYear(year = as.numeric(year[1]))
  
  #### Function
  fun <- function(date, K)
  {
    x1 <- .dayInYear.startEnd(date = date)
    nd <- x1[2] - x1[1] + 1
    ndy <- .ndaysInYear(year = as.numeric(format(x = date[1], format = "%Y")))
    fourier( 
      x = ts(data = numeric(nd), start = x1[1] / ndy, deltat = 1 / ndy), 
      K = K, h = NULL)
  }
  
  #### Make
  x1 <- mapply(FUN = fun, date = date, MoreArgs = list(K = K))
  
  #### Join
  x1 <- do.call( what = rbind, args = x1)
  colnames(x1) <- paste0("pfou.", 
    gsub(x = colnames(x1), pattern = paste0("-", ndy1), replacement = "", 
      fixed = TRUE))
  
  #### Answer
  x1
}
# ------------------------------------------------------------------------------



.xreg.spl <- 
function(type, x, knot, degree = 3, Boundary.knots = NULL, demean = FALSE)
{
  #### Settings
  nobs <- NROW(x)
  #### Boundary.knots
  if ( NROW(Boundary.knots) == 0 )
  {
    Boundary.knots <- range(x, na.rm = TRUE)
  }
  #### knots1 = equally spaced knots
  knots1 <- if ( NROW(knot) == 1 )
  {
    nk <- knot
    by <- (Boundary.knots[2] - Boundary.knots[1]) / (nk + 1)
    seq(from = Boundary.knots[1] + by, by = by, length.out = nk)
  }
  else
  {
    knot
  }
  #### Make
  if (type == "b")
  {
    x1 <- splines2::bSpline(x = x, knots = knots1, degree = degree, 
      intercept = FALSE, Boundary.knots = Boundary.knots)
    colnames(x1) <- paste0("bspl.", 1 : NCOL(x1))
  }
  else if (type == "m")
  {
    x1 <- splines2::mSpline(x = x, knots = knots1, degree = degree, 
      intercept = FALSE, Boundary.knots = Boundary.knots, periodic = TRUE)
    colnames(x1) <- paste0("mspl.", 1 : NCOL(x1))
  }
  else if (type == "i")
  {
    x1 <- splines2::iSpline(x = x, knots = knots1, degree = degree, 
      intercept = FALSE, Boundary.knots = Boundary.knots)
    colnames(x1) <- paste0("ispl.", 1 : NCOL(x1))
  }
  else if (type == "n")
  {
    x1 <- splines2::naturalSpline(x = x, knots = knots1, degree = degree, 
      intercept = FALSE, Boundary.knots = Boundary.knots)
    colnames(x1) <- paste0("nspl.", 1 : NCOL(x1))
  }
  else
  {
    stop("Only 'b', 'm', 'i' and 'n' splines are implemented")
  }
  ####
  if (demean[1])
  {
    x1 <- x1 - matrix(colMeans(x1), NROW(x1), NCOL(x1), TRUE)
  }
  
  #### Answer
  x1
}
# ------------------------------------------------------------------------------


################################################################################
## Dummy functions
################################################################################

.xreg.daily.dummies <- function(date)
{
  #### Calendar effects
  to <- date[NROW(date)]
  #### Day
  x1  <- .calendarEffects.2(from=date[1], to = to, easter.len = 3, 
    country = "it")
  cal <- model.matrix(object= ~ 0 + x1)
  colnames(cal)=substr(colnames(cal), 3, 100)
  ## Sunday taken as reference
  ind <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "lh", "sh", "eh")
  cal[, ind, drop = FALSE]
}
# ----------------------------------------------------------------------


.xreg.monthly.dummies <- function(date)
{
  #### 
  mon <- format(x = date, format = "%m")
  #### Make
  x1 <- model.matrix( ~ 0 + mon)
  #### Remove Dec
  ind <- colnames(x1) != "mon12"
  x1 <- x1[, ind, drop = FALSE]
  #### Adjust colnames
  colnames(x1) <- gsub(x = colnames(x1), pattern = "mon", replacement = "m", 
    fixed = TRUE)
  #### Answer
  x1   
}
# ------------------------------------------------------------------------------


# #### Vector of dates (as and example) 
# date <- seq(from = as.Date("2015-01-01"), to = as.Date("2024-12-01"), 
#   by = "1 day")
# 
# #### Dummies (Sun and December are the references)
# dd <- .xreg.daily.dummies(date = date)
# md <- .xreg.monthly.dummies(date = date)
# #### Other ways to capture components
# trspl <- .xreg.trspl(time = date, nk = 5, degree = 3,
#   demean = TRUE)
# perspl <- .xreg.perspl(time = date, knot = 5, degree = 3,
#   demean = TRUE)
# ####
# par(mfrow = c(2,2), mar = c(4, 4, 4, .5))
# matplot(y = dd, x = date, type = "l", xlab = "", ylab = "", main = "Daily dummies")
# matplot(y = md, x =  date, type = "l", xlab = "", ylab = "", main = "Monthly dummies")
# matplot(y = trspl, x = date, type = "l", xlab = "", ylab = "", main = "Spline trend")
# matplot(y = perspl, x =  date, type = "l", xlab = "", ylab = "", main = "Periodic spline")