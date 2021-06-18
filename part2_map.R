#===============================================================================
# 2021-06-17 -- MPIDR dataviz
# Maps
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com

# Last revised by: Dr Melissa Bedinger, m.bedinger@hw.ac.uk
# Last revised: 2021-06-18
#===============================================================================

# Prepare the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set working directory to the script's folder
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Load the required packages
pacman::p_load(tidyverse, janitor, sf, ggthemes, plotly, extrafont, htmlwidgets)

# Ensure fonts downloaded
# loadfonts(device = "win")
# If not
# font_import()


# read data ---------------------------------------------------------------

# read in the shapefile as sf object
gd_scot <- read_sf("pub_las.shp")

# read in tweet data
tweets <- readRDS("data_scotland_final.RDS")
# summarise tweet data
tweets_summary <- tweets %>% count(local_auth)

# read in population count
population <- read.csv("scot_pop.csv")

# join
gd <-
    gd_scot %>%
    left_join(tweets_summary, by = "local_auth") %>% 
    left_join(population, by = "local_auth") %>%
    mutate(quo_100k = (pop / 100000)) %>%
    mutate(n_100k = round(n / quo_100k), 0)


# A bit of magic: interactive plots with PLOTLY ---------------------------

# transform the projection for the one suitable for Europe
gdtr <- gd %>% st_transform(crs = 3035)

# ok plot but needs some learning/fiddling with 
# for reformatting relative positions and sizes of title, legend, annotation
gdtr %>%
    plot_ly() %>% 
    add_sf(
        split = ~local_auth, # group by our regions
        alpha = 1, # non-transparent colors
        stroke = I("#333333"), # color of the boundaries
        span = I(.1), # thickness of the boundaries
        color = ~n_100k, # the variable defining colors
        colors = "Blues",
        text = ~paste(
            "Local Authority:", local_auth, "\n", 
            "Tweets per 100k people:", n_100k
        ), # hover box info
        hoveron = "fills",
        hoverinfo = "text",
        showlegend = FALSE # try TRUE to see what happens
    ) %>%
    colorbar(title = "Tweets \n per 100k people") %>%
    layout(
        title = list(text = "Where People Tweet about Whisky the Most", 
                     yshift = -40), # need to lower the title!
        font = list(family = "Bahnschrift", face = "bold", color = "#000000"),
        annotations = 
            list(x = 1, y = -0.1, 
                 text = 
                     '*in Scotland from 2021-06-09 through 2021-06-17 <br>Population Data: <a href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2019">National Records of Scotland</a> | Tweet Data: <a href= "https://github.com/mbedinger/whisky">GitHub</a> | Shapefile: <a href="https://data.spatialhub.scot/dataset/local_authority_boundaries-is/resource/d24c5735-0f1c-4819-a6bd-dbfeb93bd8e4">Spatial Hub</a>', 
                 showarrow = F, xref='paper', yref='paper', align = 'left',
                 xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 50,
                 font = list(family = "Bahnschrift", color = "#999999", size = 12)))

pl <- plotly::last_plot()

htmlwidgets::saveWidget(pl, "ggplotly_whiskyTweetsv2.html")
