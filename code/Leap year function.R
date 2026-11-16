#leap year fxn

is_leap_year <- function(year) {if(!is.numeric(year)) stop("Please enter a numeric value for year.") else(year %%4 == 0) & (year %% 100 != 0) | (year %%400 == 0) 
}