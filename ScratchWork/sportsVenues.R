library(rvest)
library(tidyverse)

# Defines a function to pull html content from a specified URL
getHTMLContent = function(url){
    download.file(url, destfile = 'scrapedpage.html', quiet = T)
    content = read_html('scrapedpage.html')
    content
}

# We will pull our venue data from four different URLs
urls = c('https://en.wikipedia.org/wiki/List_of_current_Major_League_Baseball_stadiums',
         'https://en.wikipedia.org/wiki/List_of_National_Basketball_Association_arenas',
         'https://en.wikipedia.org/wiki/List_of_current_National_Football_League_stadiums',
         'https://en.wikipedia.org/wiki/List_of_National_Hockey_League_arenas')

# MLB Data

mlbVenues = getHTMLContent(urls[1])

mlbVenues = mlbVenues %>%
    html_nodes('table') %>%
    .[[2]] %>%
    html_table() %>%
    as_tibble()

mlbVenues = mlbVenues %>%
    select(Name, Capacity, Location, Team, Opened) %>%
    mutate(League = 'MLB')

# NBA Data

nbaVenues = getHTMLContent(urls[2])

nbaVenues = nbaVenues %>%
    html_nodes('table') %>%
    .[[1]] %>%
    html_table() %>%
    as_tibble()

nbaVenues = nbaVenues %>%
    select(Arena, Capacity, Location, `Team(s)`, Opened) %>%
    mutate(League = 'NBA') %>%
    rename(Team = `Team(s)`, Name = Arena)

# NFL Data

nflVenues = getHTMLContent(urls[3])

nflVenues = nflVenues %>%
    html_nodes('table') %>%
    .[[2]] %>%
    html_table() %>%
    as_tibble()

nflVenues = nflVenues %>%
    select(Name, Capacity, Location, `Team(s)`, Opened) %>%
    mutate(League = 'NFL') %>%
    rename(Team = `Team(s)`)

# NHL Data

nhlVenues = getHTMLContent(urls[4])

nhlVenues = nhlVenues %>%
    html_nodes('table') %>%
    .[[1]] %>%
    html_table() %>%
    as_tibble()

nhlVenues = nhlVenues %>%
    select(Arena, Capacity, Location, `Team(s)`, Opened) %>%
    mutate(League = 'NHL') %>%
    rename(Team = `Team(s)`, Name = Arena)

# Binds the information into a single tibble

venues = rbind(mlbVenues, nbaVenues, nflVenues, nhlVenues)

# Creates city and state features and coerces data into correct types

venues = venues %>%
    separate(Location, c('City', 'State'), ', ') %>%
    mutate(Name = str_replace(Name, '\\[.*\\]', ''),
           Team = str_replace(Team, '\\[.*\\]', ''),
           Capacity = as.integer(str_replace_all(str_replace(Capacity, '\\[.*\\]', ''), ',', '')),
           Opened = as.integer(str_sub(Opened, 1, 4)))

# Summarizes the venue counts by League for sanity checks

venues %>% 
    group_by(League) %>%
    summarise(VenueCount = n())

# The venue counts do not match the number of teams exactly

# Two nfl teams share the same stadium
venues %>% 
    filter(League == 'NFL') %>%
    mutate(TeamLength = sapply(Team, str_length)) %>%
    arrange(desc(TeamLength)) %>%
    top_n(1) %>%
    select(-TeamLength)

# Cleans up names of New York Teams
venues = venues %>%
    mutate(Team = ifelse(Name == 'MetLife Stadium', 
                         'New York Giants & New York Jets', 
                         Team))

# One NHL team has two stadiums
venues %>%
    group_by(Team) %>%
    summarise(NumberOfVenues = n()) %>%
    filter(NumberOfVenues > 1) %>%
    arrange(desc(NumberOfVenues))

# Reads csv of coordinate data
coordURL = 'C:/Users/Rick/Documents/Projects/Sports_Venues/venueCoordinates.csv'
coordinates = read.csv(coordURL)

venues = venues %>% 
    left_join(coordinates, by = c('Team' = 'team_name')) %>%
    rename(Latitude = lat, Longitude = lon) %>%
    select(League, Name, Capacity, City, State, Team, Opened, Latitude, Longitude)

# Checks for missing latitude and longitude data
venues %>%
    filter(is.na(Latitude) | is.na(Longitude))

output = 'C:/Users/Rick/Documents/Projects/Sports_Venues/venues.csv'
write.csv(venues, file = output, row.names = F)
