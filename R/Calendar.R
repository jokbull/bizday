#' @title Creates a calendar
#' 
#' @description
#' The \code{Calendar} stores all information necessary to compute business days.
#' This works like a helper class for many of \code{bizdays}' methods.
#' 
#' @param holidays a vector of Dates which contains the holidays
#' @param start.date the date which the calendar starts
#' @param end.date the date which the calendar ends
#' @param name calendar's name
#' @param dib a single numeric variable which indicates the amount of days
#' within a year (\code{dib} stands for days in base).
#' 
#' @details
#' The arguments \code{start.date} and \code{end.date} can be set but once they aren't and \code{holidays}
#' is set, \code{start.date} is defined to \code{min(holidays)} and \code{end.date} to \code{max(holidays)}.
#' If holidays isn't set \code{start.date} is set to \code{'1970-01-01'} and \code{end.date} to \code{'2071-01-01'}.
#' 
#' \code{weekdays} is controversial but it is only a sequence of nonworking weekdays.
#' In the great majority of situations it refers to the weekend but it is also possible defining
#' it differently.
#' \code{weekdays} accepts a \code{character} sequence with lower case weekdays (
#' \code{sunday}, \code{monday}, \code{thuesday}, \code{wednesday}, \code{thursday},
#' \code{friday}, \code{saturday}).
#' This argument defaults to \code{NULL} because the default intended behavior for 
#' \code{Calendar} returns an \emph{actual} calendar, so calling \code{Calendar(dib=365)} 
#' returns a \emph{actual/365} calendar and \code{Calendar(dib=360)} and \emph{actual/360}
#' (for more calendars see \href{http://en.wikipedia.org/wiki/Day_count_convention}{Day Count Convention})
#' To define the weekend as the nonworking weekdays one could simply
#' use \code{weekdays=c("saturday", "sunday")}.
#' 
#' \code{dib} reffers to \emph{days in base} and represents the amount of days within a year.
#' That is necessary for defining Day Count Conventions and for accounting annualized periods 
#' (see \code{\link{bizyears}}).
#' 
#' The arguments \code{adjust.from} and \code{adjust.to} are used to adjust \code{bizdays}' arguments
#' \code{from} and \code{to}, respectively.
#' These arguments need to be adjusted when nonworking days are provided.
#' The default behavior, setting \code{adjust.from=adjust.previous} and \code{adjust.to=adjust.next},
#' works like Excel's function NETWORKDAYS, since that is fairly used by a great number of practitioners.
#' 
#' \code{Calendar} doesn't have to be named, but it helps identifying the calendars once many are instantiated.
#' You name a \code{Calendar} by setting the argument \code{name}.
#' 
#' @export
#' @examples
#' data(holidaysCN)
#' that <- Calendar(name="CN", holidays = holidaysCN)
#' options(calendar=that)
#' 
#' # ACTUAL calendar
#' cal <- Calendar(name="Actual", dib=365)
#' # calendar default name is gregorian
#' cal <- Calendar(start.date="1976-07-12", end.date="2013-10-28")
#' is.null(cal$name) # TRUE
Calendar <- function (holidays=integer(0),
                      startDate="2005-01-01", endDate="2015-12-31", 
                      pattern = c("%Y-%m-%d","%Y%m%d"), name="gregorian", dib=NULL) {
  
  # check the parameters
  startDate <- match.arg(startDate)
  endDate   <- match.arg(endDate)
  pattern   <- match.arg(pattern)
  
  
  # convert to POSIX-date
  startDate <- as.Date(startDate, format = pattern)
  endDate <- as.Date(endDate, format = pattern)
  
  # 
  dates <- seq(from=startDate, to=endDate, by='day')
  n.dates <- as.integer(dates)
  
  n.holidays <- as.integer(as.Date(holidays))
  
  .is.bizday <- !n.dates %in% n.holidays 

  # bizdays index
  n.bizdays <- n.dates[.is.bizday]
  index.bizdays <- seq_along(n.bizdays)
  index <- cumsum(.is.bizday)
    
  
  
  that <- list(name = name, dib = dib, startDate = startDate, endDate = endDate, 
      index = index, maxindex = max(index.bizdays), mindate = min(n.dates), maxdate = max(n.dates),
      bizdays = dates[.is.bizday], n.bizdays = n.dates[.is.bizday],
      holidays = dates[!.is.bizday], n.holidays = dates[!.is.bizday],
      n.dates = n.dates
  ) 

  # set class attribute
  class(that) <- 'Calendar'
  return(that)
}


#' @export
print.Calendar <- function(cal, ...) {
  cat('Calendar:', cal$name,
      '\nRange:', cal$startDate,
      'to', cal$endDate,
      '\ndib:', cal$dib,
      '\n')
  invisible(x)
}




#' Adjusts the given dates to the next/previous business day
#'
#' If the given dates are business days it returns the given dates, but once it
#' is not, it returns the next/previous business days.
#'
#' @param lhs dates to be adjusted
#' @param rhs offset days
#' @param cal an instance of \code{Calendar}
#' 
#' @section Date types accepted:
#' 
#' The argument \code{dates} accepts \code{Date} objects and any
#' object that returns a valid \code{Date} object when passed through
#' \code{as.Date}, which include all \code{POSIX*} classes and \code{character}
#' objects with ISO formatted dates.
#' 
#' @return
#' \code{Date} objects adjusted accordingly.
#' 
#' @rdname adjust.date
#' 
#' @export 
`%+%` <-  function(lhs, rhs, method = c("next","previous"), cal = getOption("calendar"),...) UseMethod("%+%")

#' @export 
`%+%.default` <- function(lhs, rhs, method = c("next","previous"), cal = getOption("calendar"),...) {
  lhs = as.Date(lhs)
  `%+%.Date`(lhs, rhs, method, cal)
}

#' @export 
`%+%.Date` <-  function(lhs, rhs, method = c("next","previous"), cal = getOption("calendar"),...) {
  # check rhs  
  stopifnot(is.numeric(rhs))
  
  # check the lengths
  stopifnot(length(rhs) == 1 | length(lhs)==length(rhs))
  
  method = match.arg(method)
  offset = switch(method,
                  "next" = 1,
                  "previous" = -1)
 
  n.lhs <- as.integer(lhs)
  idx <- match(n.lhs, cal$n.bizdays)
  idx[is.na(idx)] <- FALSE
  while(!all(idx)) {
    n.lhs[!idx] <- n.lhs[!idx] + offset
    if (any(n.lhs>cal$maxdate)) stop("Exceed the calendar max date")
    idx <- match(n.lhs, cal$n.bizdays)
    idx[is.na(idx)] <- FALSE
  }
  
  if (any(idx+rhs<=0)) stop("Exceed the calendar min date")
  if (any(idx+rhs>cal$maxindex)) stop("Exceed the calendar max date")
  cal$bizdays[idx + rhs]
}

#' next biz date
#' 
#' @rdname adjust.date
#' @export
nextbiz <- function(lhs,  method = "previous", cal = getOption("calendar") ) `%+%`(lhs,1,method,cal)


#' previous biz date
#'
#' @rdname adjust.date
#' @export
prevbiz <- function(lhs, method = "next", cal = getOption("calendar") ) `%+%`(lhs,-1,method,cal)

#' Computes business days between two dates.

#' @export
bizdays <- function(from, to, cal=getOption("calendar")) UseMethod('bizdays')

bizdays.default <- function(from, to, cal=getOption("calendar")) {
  from <- as.Date(from)
  bizdays.Date(from, to, cal)
}

bizdays.Date <- function(from, to, cal=getOption("calendar")) {
  tryCatch({to <- as.Date(to)}, error=function(e) e)
  # ---
  if (all(is.na(to))) return( rep(NA, max(length(to), length(from))) )
  if ( ! any(from >= cal$startDate & from <= cal$endDate) )
    stop('Given "from" date out of range.')
  if ( ! any(to >= cal$startDate & to <= cal$endDate) )
    stop('Given "to" date out of range.')
  lengths <- c(length(from), length(to))
  if (max(lengths) %% min(lengths) != 0)
    stop("from's length must be multiple of to's length and vice-versa.")
  if ( ! all(from <= to, na.rm=TRUE) )
    stop('All from dates must be greater than all to dates.')
  from <- `%+%`(from, rhs = 0)
  to <- `%+%`(to, rhs = 0, method="previous")
  from.idx <- cal$index[match(as.integer(from), cal$n.dates)]
  to.idx <- cal$index[match(as.integer(to), cal$n.dates)]
  to.idx - from.idx + 1
}


#' @export
is.bizday <- function(dates,cal=getOption("calendar")) UseMethod("is.bizday")

#' @export
is.bizday.default <- function(dates,cal=getOption("calendar")) {
  dates <- as.Date(dates)
  is.bizday(dates, cal)
}
#' @export
is.bizday.Date <- function(dates, cal=getOption("calendar")) {
  if ( ! any(dates >= cal$startDate & dates <= cal$endDate) )
    stop('Given date out of range.')
  as.integer(dates) %in% cal$n.bizdays 
}
  
#' @export
bizseq <- function(from, to, cal=getOption("calendar")) UseMethod('bizseq')

#' @export
bizseq.default <- function(from, to, cal=getOption("calendar")) {
  from <- as.Date(from)
  bizseq(from, to, cal)
}
#' @export
bizseq.Date <- function(from, to, cal=getOption("calendar")) {
  to <- as.Date(to)
  if ( ! any(from >= cal$startDate & from <= cal$endDate) )
    stop('Given "from" date out of range.')
  if ( ! any(to >= cal$startDate & to <= cal$endDate) )
    stop('Given "to" date out of range.')
  if ( ! all(from <= to) )
    stop('All from dates must be greater than all to dates.')
  from <- as.integer(from)
  to <- as.integer(to)
  
  as.Date(cal$n.bizdays[which(cal$n.bizdays >= from & cal$n.bizdays <= to)], origin='1970-01-01')
}
