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
fname_sub <- fname %>%
  select(year, state, aapi_jail_pop, black_jail_pop, latinx_jail_pop, 
         native_jail_pop, white_jail_pop, total_jail_pop, aapi_pop_15to64, 
         black_pop_15to64, latinx_pop_15to64, native_pop_15to64, white_pop_15to64)

fname_sub_2018 <- fname_sub %>%
  filter(year == 2018) %>%
  summarise(mean_aapi_jail_pop = mean(aapi_jail_pop, na.rm = TRUE),
            mean_black_jail_pop = mean(black_jail_pop, na.rm = TRUE),
            mean_latinx_jail_pop = mean(latinx_jail_pop, na.rm = TRUE),
            mean_native_jail_pop = mean(native_jail_pop, na.rm = TRUE),
            mean_white_jail_pop = mean(white_jail_pop, na.rm = TRUE),
            mean_aapi_pop_15to64 = mean(aapi_pop_15to64, na.rm = TRUE),
            mean_black_pop_15to64 = mean(black_pop_15to64, na.rm = TRUE),
            mean_latinx_pop_15to64 = mean(latinx_pop_15to64, na.rm = TRUE),
            mean_native_pop_15to64 = mean(native_pop_15to64, na.rm = TRUE),
            mean_white_pop_15to64 = mean(white_pop_15to64, na.rm = TRUE),
                  )

#ratio of mean population to jail in 2018 (Race includes Asian American, Native American,
#Black, White, and Latinx)

ratio_pop_to_jail_2018 <- fname_sub_2018 %>%
  summarise(ratio_aapi = mean_aapi_jail_pop/mean_aapi_pop_15to64,
            ratio_black = mean_black_jail_pop/ mean_black_pop_15to64,
            ratio_latinx = mean_latinx_jail_pop/mean_aapi_pop_15to64,
            ratio_native = mean_native_jail_pop/mean_native_pop_15to64,
            ratio_white = mean_white_jail_pop/ mean_white_pop_15to64
            )

overall <- list()

overall$ratio_aapi_2018 <- pull(ratio_pop_to_jail_2018, ratio_aapi)
overall$ratio_black_2018 <- pull(ratio_pop_to_jail_2018, ratio_black)
overall$ratio_latinx_2018 <- pull(ratio_pop_to_jail_2018, ratio_latinx)
overall$ratio_native_2018 <- pull(ratio_pop_to_jail_2018, ratio_native)
overall$ratio_white_2018 <- pull(ratio_pop_to_jail_2018, ratio_white)

#What is the average value of the jailed population of black or African American from age 15 to 64?
#What are the average value of the jailed population of white from age 15 to 64?
#What are the average value of the jailed population of latinx from age 15 to 64?

#Section 2: Data summary 
#Write a paragraph of summary information, citing at least three values calculated from the data.  Your goal is to summarize some of the key variables that you are interested in. 
#Report the values and explain why they are important; that is, how do the variables and value help you to understand patterns of inequality in the prison system.

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

fname_section_five <- fname %>%
  select(year, state, female_adult_jail_pop, male_adult_jail_pop) %>%
  group_by(state) %>%
  filter(year == 2018)

fname_section_five[is.na(fname_section_five)] <- 0 

section_five_graph <- function(state){}
  ggplot(data = fname_section_five) +
  geom_point(
    mapping = aes(x = female_adult_jail_pop, y = male_adult_jail_pop)) + 
  ggtitle( "Variable Comparison of Jailed Population by Gender (in 2018 by States)") +  
  labs(y = "Male Jailed Population", x = "Women Jailed Population")
return(graph_jail)   
}

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Defining a minimalist theme for maps
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

get_black_jail_map <- fname %>%
  select(state, year, black_jail_pop, total_pop)%>%
  filter(year == 2018) %>%
  group_by(state) %>%
  mutate(black_ratio = black_jail_pop / total_pop)
         
get_black_jail_map$state <- state.name[match(get_black_jail_map$state, state.abb)]

state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(get_black_jail_map, by ="state")

map_one <- ggplot(state_shape) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, 
                             fill = black_ratio, size = .1, alpha = .3)
               ) +
  scale_fill_gradient2(
    "# of Black jailed population",
    low = "white",
    mid = "yellow",
    high = "red"
  ) +
  labs(title="Jailed Black Population in the United States, 2018") +
  theme(plot.margin = margin(.3, 0, 0, 0, "cm")) +
  blank_theme

---
  

Section 6: <a map shows potential patterns of inequality that vary geographically>
  In this section, your goal is to produce a map that reveals a potential inequality.
Specifically, the map should show show show how a variable is distributed geographically. 
Again, think carefully about how a "geographic comparison" (e.g., counties in a state, counties in division, or counties in across regions) might reveal an inequality. 
Your first step should be to find potential trends in the dataset.  Recommendation: See reading on maps and (1)  Use a map based coordinate system to set the aspect ratio of your map; 
and (2) Use a minimalist theme for the map (see reading). 

Structuring your code. As in the previous sections, you should write two functions (or more, if useful), one for data wrangling and one for plotting.


#----------------------------------------------------------------------------#

## Load data frame ---- 


