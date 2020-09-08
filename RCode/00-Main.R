### this program runs a shiny app to visualize Nashville crime data
###
###
###       Created by:       Coy McNew           2020-03-07
###       Last edited by:   Coy McNew           2020-03-07
if (!require("pacman")) install.packages("pacman")
p_load(shiny, tidyverse, lubridate, RSocrata)

### Operators -------------------------------------------------------------------------------------

### Data ------------------------------------------------------------------------------------------
source(file.path('RCode', '01-Data.R'))

### Shiny -----------------------------------------------------------------------------------------
#running the app
runApp(file.path('NashvilleCrime-WebApp'))

#deploying to shinyapps.io
rsconnect::deployApp(file.path('NashvilleCrime-WebApp'))
