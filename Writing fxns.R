##Writing functions

install.packages("cli")
library(tidyverse)


is_leap_year <- function(year) {if(!is.numeric(year)) stop("Please enter a numeric value for year.") else(year %%4 == 0) & (year %% 100 != 0) | (year %%400 == 0) 
}


is_leap_year(2004)
is_leap_year(c(2004, 2008, 2009))

is_leap_year(20)
is_leap_year(20.1)
is_leap_year(1900)
is_leap_year(2000)
is_leap_year(2004)
is_leap_year(2023)
is_leap_year("two thousand")

test_years <- c(1900, 2000, 2004, 2023, 2024, 2100)
is_leap_year(test_years)

#calling functions from other scripts
source("code/Leap year function.R")

