library(rvest)
library(purrr) # for the keep() and pluck() functions

url <- "https://en.wikipedia.org/wiki/Washington,_D.C."
page <- read_html(url)

# Extract all html tables and find the population table
pop_table <- page |>
  html_elements("table") |>
  html_table() |>
  keep(~ any(grepl("Census", names(.x)))) |>
  pluck(1)

head(pop_table)



names(pop_table)
library(dplyr)
pop = pop_table |> #rename 
  rename(Year = Census, 
         Population  = Pop.,
         Change_pct = `%±`) |>
  slice(-n()) |>
  select(-3) |>
  #replace commas with space
  mutate(Population = gsub(",","", Population), 
         Change_pct = gsub("%", "", Change_pct)) |>
  mutate(Change_pct = gsub("—", "-", Change_pct)) |>
  mutate(Year = as.numeric(substr(Year, 1, 4)), 
         Population = as.numeric(Population), 
         Change_pct = as.numeric(Change_pct)) #as numeric
##substr says to only look at first four characters as last row had characters in it 

#as.numeric does not work on population because of commas

summary(pop)

write.csv(pop, "data/derived/popDC.csv", row.names=F)

