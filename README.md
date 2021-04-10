# Census-Mapper
A free `shiny` application to interactively visualize detailed census data with different base maps and spatial disaggregations.
It currently visualizes the Uganda 2014 Population and Housing Census. 

**Running Application:** [https://sebkrantz.shinyapps.io/Census-Mapper/](https://sebkrantz.shinyapps.io/Census-Mapper/)

***
To adapt to a different census dataset, use a census and supporting shapefile, create an `sf` data frame and aggregate to different spatial resolutions with `aggregate.sf()`. Use `rmapshaper::ms_simplify` to simplify the shapefile for improved `leaflet` performance at high spatial resolutions. 

*Note:* You need an API key if you want to use the [Jawg](<https://www.jawg.io/en/>) base maps. 