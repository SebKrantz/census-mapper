##############################
# Uganda 2014 Census Map
##############################

suppressPackageStartupMessages({
  library(shiny)
  library(shinyBS)
  library(collapse) # > 1.8.0
  library(leaflet)
  library(sf) 
  library(scales)
  library(htmltools)
  library(RColorBrewer)
})

# rm(list = ls())
# load("app_DATA.RData")
qs::qreadm("app_DATA.qs", nthreads = 2)

idvars <- c("Region", "District",  "County",  "Subcounty", "Parish")

names(cens_vars_list)[1L] <- "Composite Indices"

agglabs <- setNames(idvars, paste0(idvars, " (", 
                                   c(fnrow(DATA_Region), fnrow(DATA_District), fnrow(DATA_County), 
                                     fnrow(DATA_Subcounty), fnrow(DATA)), ")"))

# rsconnect::showLogs()
# Todo: still accomodate per area !
genpopup <- function(data, colvar = "POP_M", oth = c("POP", "HDI"), 
                     idvars = c("Region", "District",  "County",  "Subcounty", "Parish")) { # , coldata
  ids <- idvars[idvars %in% names(data)]
  if(colvar %!in% oth) oth <- c(oth, colvar)
  vars <- c(ids, oth) 
  lhs <- paste0("<br/><strong>", vars, ":</strong>") 
  lhs[1L] <- substr(lhs[1L], 6L, 10000L)
  pop <- c(as.vector(lhs, "list"), .subset(data, vars))
  if(length(nint <- vtypes(pop[oth], FALSE) %!=% "integer")) 
    pop[oth][nint] <- lapply(pop[oth][nint], round, 3L)
  o <- seq(1L, 2L*length(vars), 2L)
  o <- radixorder(c(o, o + 1L))
  do.call(paste, pop[o]) # lapply(, htmltools::HTML)
}


ui <- source(file.path("geoUI.R"), local = TRUE)$value 

server <- function(input, output, session) {
  source(file.path("geoSERVER.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)

