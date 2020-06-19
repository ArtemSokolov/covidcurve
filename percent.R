## Computing percent positive cases over the past few weeks
##  for a selected set of states
##
## by Artem Sokolov

library( tidyverse )
library( lubridate )
library( kableExtra )

## States of interest
soi <- c( "CA", "NC", "AZ", "TX", "FL", "GA", "NV" )

## Select the relevant slice of the raw data
X <- read_csv("https://covidtracking.com/api/v1/states/daily.csv",
              col_types=cols(date=col_character())) %>%
  transmute( Date = ymd(date), State = state,
             Total = totalTestResultsIncrease,
             Positive = positiveIncrease ) %>%
  filter( State %in% soi )

## Aggregate across the past four weeks
sd <- today() - weeks(4)                  # Start date
wseq <- sd + (0:3)*weeks(1)               # Anchor for each week
Y <- X %>% filter( Date >= sd ) %>%
  mutate( Week = wseq[as.integer((Date-sd) / dweeks(1))+1] ) %>%
  group_by( State, Week ) %>%
  summarize_at( vars(Total, Positive), sum ) %>%
  ungroup() %>% 
  mutate( Percent = Positive / Total * 100 )

## Reshape to wide format
Z <- Y %>% select( State, Week, Percent ) %>% 
  mutate( Percent = str_c( round(Percent, 1), "%" ) ) %>%
  spread( Week, Percent )

## Display the table
Z %>% knitr::kable("html") %>%
  kable_styling(bootstrap_options = c("striped"), 
                full_width=FALSE) %>%
  add_header_above(c(" ", "Week Of" = 4))
