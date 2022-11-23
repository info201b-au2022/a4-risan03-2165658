fname <- read.csv(url("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"))

library("tidyverse")
library("dplyr")
library("ggplot2")

## Section 2  ---- 
#----------------------------------------------------------------------------#
#selecting all the relevant values using select() in order to draw insights, making into a smaller dataset
fname_sub <- fname %>%
  select(year, state, aapi_jail_pop, black_jail_pop, latinx_jail_pop, 
         native_jail_pop, white_jail_pop, total_jail_pop, aapi_pop_15to64, 
         black_pop_15to64, latinx_pop_15to64, native_pop_15to64, white_pop_15to64)

#filtering data to 2018 and finding mean of the values using mean() in summarise function
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

#Finding the ratio of mean jail population by the population of the race in 2018 by (Race includes Asian American, Native American,
#Black, White, and Latinx)

ratio_pop_to_jail_2018 <- fname_sub_2018 %>%
  summarise(ratio_aapi = mean_aapi_jail_pop/mean_aapi_pop_15to64,
            ratio_black = mean_black_jail_pop/ mean_black_pop_15to64,
            ratio_latinx = mean_latinx_jail_pop/mean_aapi_pop_15to64,
            ratio_native = mean_native_jail_pop/mean_native_pop_15to64,
            ratio_white = mean_white_jail_pop/ mean_white_pop_15to64
            )

overall <- list()

#pulling the value of the ratio so that it can be inputed on the index.Rmd
overall$ratio_aapi_2018 <- pull(ratio_pop_to_jail_2018, ratio_aapi)
overall$ratio_black_2018 <- pull(ratio_pop_to_jail_2018, ratio_black)
overall$ratio_latinx_2018 <- pull(ratio_pop_to_jail_2018, ratio_latinx)
overall$ratio_native_2018 <- pull(ratio_pop_to_jail_2018, ratio_native)
overall$ratio_white_2018 <- pull(ratio_pop_to_jail_2018, ratio_white)
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

#Function for data wrangling on the growth of prison population by state
fname <- replace(fname, is.na(fname),0)
# choses on these 3 states (Washington, Oregon, California) out of all the other states 
states = c("WA", "OR", "CA")
get_jail_pop_by_states <- function(states){
  jail_pop_state <- fname %>%
  filter(state %in% states) %>%
    #selects the relevant values which includes the county name, state, year (1970 to 2018), 
    # and the total jail population
    select(total_jail_pop, year, county_name, state) %>%
    group_by(state, year) %>%
    summarise(
      state,
      year,
      pop = sum(total_jail_pop)
    )
  return(unique(jail_pop_state))
}

#Function for creating a chart  and return the chart. It is a line chart presenting the U.S. prison population in WA,CA,OR
#where x-axis shows the year and the y-axis represents the jail population

plot_jail_pop_by_states <- function(jail_pop_state){
  df_two <- get_jail_pop_by_states(states)
  graph_by_states <- ggplot(
    data = jail_pop_state,
    mapping = aes(x = year, y = pop, group = state)) + 
    geom_line(aes(color = state)) +
    ggtitle( "U.S. prison population in WA, CA, OR (1970-2018)") +  
    labs(y = "Total Jail Population", x = "Year")
  return(graph_by_states)   
}

testdf <- get_jail_pop_by_states(states)
plot1<- plot_jail_pop_by_states(testdf)
plot1
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
#Function for data wrangling on the variable comparson between women and men on the jailed population
sub_fname_five <- fname %>%
  select(year, state, female_adult_jail_pop, male_adult_jail_pop) %>%
  group_by(state) %>%
  filter(year == 2018)

#Function for creating a chart  and return the chart. It is a point chart (scatter plot) 
#which takes in(calls) the data wrangling function above
section_five_graph <- function(state){
  ggplot(data = sub_fname_five) +
  geom_point(
    mapping = aes(x = female_adult_jail_pop, y = male_adult_jail_pop)) + 
  ggtitle( "Variable Comparison of Jailed Population by Gender by States (2018)") +  
  labs(y = "Male Jailed Population", x = "Women Jailed Population")
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
#Function for data wrangling which selects the relevant value (state, year, black jailed population, and total population)
#and which only uses the data from 2018 (through filtering), and find the ratio of black jail population to the total population
get_black_jail_map <- fname %>%
  select(state, year, black_jail_pop, total_pop) %>%
  filter(year == 2018) %>% # keep only 2018 data
  mutate(black_ratio = black_jail_pop / total_pop) %>%
  #Multiplied it by 100 to show the ratio in numbers that are easier to understand for the map rendering (as number is very small)
  mutate(black_ratio = black_ratio * 100)

# "tolower" fixes every character in the state name string to be lowercase to match the state_map data set
get_black_jail_map$state <- tolower(state.name[match(get_black_jail_map$state, state.abb)])

# Join data to the U.S. shapefile
state_shape <- map_data("state") %>% # load state shapefile
  rename(state = region) %>% # rename for joining
  left_join(get_black_jail_map)

# Function which produce map of the jailed population in the united states (2018). Scale fill continuous
#changes the color on the map depending on the number of the jailed black population in each state
map_one <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_ratio),
    color = "black", # show state outlines
    size = 0.1 # thinly stroked
    ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(limits = c(0, max(0.5)), 
                        na.value = "light gray", low = "white", high = "Blue") +
  labs(fill = "Jailed Black Population in the United States, 2018") +
  blank_theme # variable containing map styles (defined in next code snippet)

