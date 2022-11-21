fname <- read.csv(file = "~/Documents/info201/data/incarceration_trends.csv")

Viewlibrary("tidyverse")
library("dplyr")
library("ggplot2")


# The functions might be useful for A4
# source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
#test_query1 <- function() {
 # return ("Hello world")
#}

# Return a vector of numbers
#test_query2 <- function(num=6) {
 # v <- seq(1:num)
#  return(v)
#}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>

#What is the average value of the jailed population of black or African American from age 15 to 64?
#What are the average value of the jailed population of white from age 15 to 64?
#What are the average value of the jailed population of latino from age 15 to 64?

#Section 2: Data summary 
#Write a paragraph of summary information, citing at least three values calculated from the data.  Your goal is to summarize some of the key variables that you are interested in. Report the values and explain why they are important; that is, how do the variables and value help you to understand patterns of inequality in the prison system.

#This values will likely be calculated using your DPLYR skills. You might answer such questions as: 
  
 # What is the average value of my variable across all the counties (in a given year)? 
  #Where is my variable the highest or lowest?  
 # How much has my variable change over the last N years?
  #Requirements 
#[ ] Complete: At least three values are included 
#[ ] Complete: Values clarify chosen variables related to patterns of inequality 
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>

# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  jail_pop <- fname %>%
    select(total_jail_pop, year)
  return(jail_pop)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function(){
  jail_pop_df <- get_year_jail_pop()
  graph_jail <- ggplot(data = fname) + 
  geom_col(mapping = aes(x = year, y = total_jail_pop)) + 
  ggtitle( "Increase of Jail Population in U.S. (1970-2018)") +  
  labs(y = "Total Jail Population", x = "Year")
  return(graph_jail)   
  }
#----------------------------------------------------------------------------#

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>

get_jail_pop_by_states <- function(states){
  jail_pop_state <- fname %>%
    select(total_jail_pop, year)
  return(jail_pop_state)
}


plot_jail_pop_by_states <- function(state){
  df_two <- get_jail_pop_by_states(states)
  graph_by_states <- ggplot(data = fname) + 
    geom_line(
      mapping = aes(x = year, y = total_jail_pop)) + 
    ggtitle( "U.S. prison population from 1970 to 2018 in Washington, California, and Oregon") +  
    labs(y = "Total Jail Population by..", x = "Year")
  return(graph_by_states)   
}


----
  
  selected_states <-data.frame(
    state = c("WA", "OR", "CA")
  )

  
  This data wrangling function should return a data frame that is suitable for visualization. The parameter states should be a vector of states.
plot_jail_pop_by_states(states)

: This plotting function should return the chart. The parameter states should be a vector of states. This function should call the data wrangling function.
If plot_jail_pop_by_states(c("WA", "OR", "CA")) is called it will produce a line chart with three lines, one for each of the states.  Show more than three states but fewer than 10 states.  

For the report, include the following in this section: 
  
Chart caption. Include a caption that names and briefly describes the chart.
Summary paragraph. Include a brief paragraph (50 words or more) that summarizes the key patterns that appear to be revealed in the chart. Explain the reason for the states that you show.

# This is  a line chart that shows the growth of the U.S. prison population from 1970 to 2018 in Washington, California, and Oregon. 
#I chose these states as they are west coast where I live in. We can see how ....

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


