rm(list =ls())

scaled_power <- function(x, power = 2, method = c("simple", "precise"), scale = 10, plot = T, ...)
  {
  if(!is.numeric(x)) {stop("Input 'x' must be numeric.", call. = FALSE)}
  
  method = match.arg(method)#check that user input is from the specified options
  
  pow_res <- x^power
  final_res <- pow_res*scale
  
  if (method == "simple") {final_res = round(final_res)}
  
  if(plot){
    plot(x, final_res, xlab = "input (x)", ...) #use triple dot to represent plot function arguments
  }
  
  return(final_res)
}

scaled_power(x = c(1,2,3), power = 3, method = "simple")
scaled_power(x = c(1,2,3), power = 3, method = "Simple")
scaled_power(x = c(1.1,2,3), power = 3, method = "simp")
scaled_power(x = c(1.1,2,3), power = 3, method = "precise")
scaled_power(c(1, 2, 3), power = 3, scale = 5)

scaled_power("three")

res <- scaled_power(
  x = seq(-3, 3, by = 0.5), 
  power = 2, 
  scale = 5, 
  method = "precise",
  col = "firebrick", 
  pch = 19, 
  type = "b"
)


library(purrr)
library(tibble)
library(dplyr)
?map_lgl()
leap_flags = map_lgl(years_vec, is_leap_year)


Sample_groups <- list(
  group_a = rnorm(30, mean = 10, sd = 2),
  group_b = rnorm(30, mean = 15, sd = 4),
  group_c = rnorm(30, mean = 5,  sd = 1)
)

summarize_group <- function(vec, group_name){ tibble(#group = group_name, 
  mean = mean(vec), sd = sd(vec), median = median(vec))}

imap(Sample_groups, ~summarize_group(.x, .y)) |>
  list_rbind()

tapply(iris$Sepal.Length, iris$Species, summarize_group)

dirty_years <- list(1996, "year_2000", 2004, TRUE, 2024)

safe_leap_check <- possibly(.f = is_leap_year, otherwise = NA)

tmp = map(dirty_years, safe_leap_check)
list_c(tmp)


library(cli)

# Base R validation reference
is_leap_year <- function(year) {
  if (!is.numeric(year)) {
    stop("Argument 'year' must be a numeric vector.", call. = FALSE)
  }
  (year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0))
}

scaled_power <- function(x, power = 2, scale = 10, 
                         method = c("simple", "precise"), 
                         plot = TRUE, verbose = TRUE, ...) {
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric vector.", call. = FALSE)
  }
  method <- match.arg(method)
  res <- (x^power) * scale
  if (method == "simple") res <- round(res)
  if (verbose) message("Method selected: '", method, "' | Processing ", length(x), " elements.")
  if (plot) plot(x, res, ...)
  return(res)
}


is_leap_year_cli <- function(year) {
  if (!is.numeric(year)) {
    cli::cli_abort(c(
      "x" = "Argument {.arg year} must be a numeric vector.", "i" = "You supplied an object of class {.cls {class(year)}}."), call. = FALSE)
  }
  (year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0))
}
?cli::cli_abort

is_leap_year_cli("2024")

scaled_power_cli <- function(x, power = 2, scale = 10, 
                         method = c("simple", "precise"), 
                         plot = TRUE, verbose = TRUE, ...) {
  if (!is.numeric(c(x, power, scale))) {
    cli::cli_abort(c(
      "x" = "Argument {.arg x}, {.arg power}, {.arg scale} must be numeric.", "i" = "You supplied objects of class {.cls {class(x)}}, {.cls {class(power)}}, and {.cls {class(scale)}}, respectively."), call. = FALSE)
  }
  if (!is.logical(c(plot, verbose))) {
    cli::cli_abort(c(
      "x" = "Argument {.arg plot}, {.arg verbose} must be logical.", "i" = "You supplied objects of class {.cls {class(plot)}} and {.cls {class(verbose)}} respectively."), call. = FALSE)
  }
  method <- match.arg(method)
  res <- (x^power) * scale
  if (method == "simple") res <- round(res)
  if (verbose) cli::cli_inform(c(
    "v" = "Method selected: {.val {method}}", "i"= "scale is {.val {scale}}", "*" = "Processed {length(x)} element{?s} using scale = {.val {scale}}."))
  if (plot) plot(x, res, ...)
  return(res)
}
scaled_power_cli("twenty", 2, "three", method = "simple", plot = "True", verbose = 1)

scaled_power_cli(3, 2, 1, method = "simple", plot = "True", verbose = 1)

scaled_power_cli(3, 2, 1, method = "simple", plot = T, verbose = F)

scaled_power_cli(x = 5, verbose = TRUE, plot = FALSE)
scaled_power_cli(x = 1:5, verbose = TRUE, plot = FALSE)

