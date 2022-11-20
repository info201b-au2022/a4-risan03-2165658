fname <- read.csv(file = "~/Documents/info201/data/incarceration_trends.csv")

library("tidyverse")
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
# See Canvas
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


