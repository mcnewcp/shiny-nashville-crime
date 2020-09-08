### this script loads and preps crime data
###
###
###     Created by:         Coy McNew         2020-03-07
###     Last edited by:     Coy McNew         2020-09-07

### load data -------------------------------------------------------------------------------------
#data.nashville.gov crime data urls
urls <- data.frame(
  year = c(
    2020
  ),
  url = c(
    "https://data.nashville.gov/resource/sie3-y9k4.csv"
  )
)

#socrata APIs allow for SoQL Clauses to be appended to the url for limiting, filtering etc.
#https://dev.socrata.com/docs/queries/
#a couple useful options:
#### $select returns specified columns only
#### $where filters rows based on column expression
#### $limt returns a maximum number of rows
#### $query lets you combine clauses
####### the syntax for $query seems to be broken in RSocrata as of writing this

#query the n most recent incidents
n <- 100

#RSocrata app token
app_token <- read_tsv("app_token.txt", col_names = FALSE) %>%
  pull("X1")

rawDF <- urls %>%
  #SoQL to sort by incident_occurred and pull n most recent incidents
  mutate_at(vars(url), ~paste0(.x, "?$order=incident_occurred DESC &$limit=", n)) %>%
  #map is unneccesary with just one url, but will be needed when pulling from multiple sources
  #and combining, like 2020 and 2021 data
  mutate(
    data = map(url, ~read.socrata(.x, app_token = app_token)),
    rows = map_int(data, ~nrow(.x))
  ) %>% 
  unnest(data)
rm(urls, app_token)  

### clean and prep data ---------------------------------------------------------------------------
mainDF <- rawDF %>%
  #drop url info
  select(-year, -url) %>%
  #combine street and zip
  mutate(address = paste(
    incident_location, ifelse(is.na(zip_code), "", zip_code)
  )) %>%
  #fix numeric
  mutate_at(vars(victim_number, longitude, latitude), as.numeric) %>%
  #combine offense description and NIBRS code
  mutate_at(
    vars(offense_description),
    ~paste(offense_nibrs, "-", .x)
  ) %>% 
  #factorize by alphabetical
  mutate_at(
    vars(offense_description, location_description),
    ~factor(.x, ordered = TRUE)
  ) %>%
  #choose columns
  select(
    incident_occurred, incident_reported, 
    address, location_description,
    incident_status, investigation_status,
    offense_description, weapon_description,
    victim_number, victim_description, victim_gender, victim_race, victim_ethnicity, 
    victim_county_resident,
    domestic_related,
    latitude, longitude
  ) %>%
  #factorize by frequency
  mutate_at(vars( 
    incident_status, investigation_status,
    weapon_description,
    victim_description, victim_gender, victim_race, victim_ethnicity, 
    victim_county_resident,
    domestic_related
    ), ~fct_infreq(factor(.x, ordered = TRUE))
  )

# observations with lat/lon
mapDF <- mainDF %>%
  filter(!(is.na(longitude)|is.na(latitude)))
#observations without lat/lon
blankDF <- mainDF %>%
  filter(is.na(longitude)|is.na(latitude)) %>%
  select(-latitude, -longitude)
rm(rawDF, mainDF)

#save data as RDS
saveRDS(
  list("mapDF" = mapDF, "blankDF" = blankDF),
  file.path('NashvilleCrime-WebApp', 'data.RDS')
)

rm(blankDF, mapDF)
