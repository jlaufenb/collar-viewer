#' @title Collar viewer
#' 
#' @description  An R shiny app for visualizing and summarizing GPS collar data.
#' @author McCrea Cobb \email{mccrea_cobb@@fws.gov}
#' 
#' @example shiny::runGitHub( "collar-viewer", "USFWS", launch.browser=T)


# Install the required packages if they aren't already installed
packages <- c("shiny", "shinyjs", "tidyverse", "leaflet", "shinyWidgets", "DT", "sp", 
              "rgdal","adehabitatLT", "adehabitatHR", "lubridate", "geojsonio",
              "maptools", "dplyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
}

# Load the required packages
library(shiny)
library(shinyjs)
library(tidyverse)
library(leaflet)
library(shinyWidgets)
library(DT)
library(sp)
library(rgdal)
library(adehabitatLT)
library(adehabitatHR)
library(lubridate)
library(geojsonio)
library(maptools)
library(dplyr)

# Source custom functions
source("code/functions/global.R")

# Define UI for application that draws a histogram
ui <- navbarPage("CollarViewer v.0.1.0-alpha", id="nav",
                 
                 # Tab 1: Load data
                 tabPanel("Load data",
                          # Sidebar layout
                          fluidPage(
                              fileInput(inputId = "file", 
                                        label = "Select collar data:",
                                        multiple = FALSE,
                                        accept = ".RData")), # Get the file upload control option
                          htmlOutput("load_msg")
                 ),
                 
                 # Tab 2: Summary tables
                 navbarMenu("Summary tables",
                            tabPanel("Collars",
                                     DT::DTOutput("collar.tbl")
                            ),
                            tabPanel("Animals",
                                     DT::DTOutput("animal.tbl")
                            )
                 )
)


#-------------------------------------------------------------------------------

# Define server logic 
server <- function(input, output, session) {
    
    options(shiny.maxRequestSize=30*1024^2)
    
    #----
    ## Tab 1: Load data
    
    # Define the uploaded .Rdata file
    dat <- reactive({
        req(input$file)
        if (is.null(input$file)) 
            return(NULL)
        inFile <- input$file
        file <- inFile$datapath
        # Load the file into new environment and get it from there
        e = new.env()
        name <- load(file, envir = e)
        dat <- e[[name]]
        dat$locations <- dat$locations %>%  
            filter(!is.na(lat))  # Filter out rows that are missing latitude values
        dat
    })

    cvdat <- reactive({
        req(input$file)
        dat = dat()
        if(!is.null(dat$collars))
            if(!is.null(dat$animals)){
                aids = dat$animals$animal_id
                dat$collars = dat$collars[dat$collars$animal_id %in% aids,]
            }
        dat$locations = dat$locations[dat$locations$collar_id %in% dat$collars$collar_id,]
        cvdat = dat
        cvdat
    })

    load_msg <- reactive({
        req(input$file)
        dat = dat()
        cvdat = cvdat()
        if(is.null(cvdat$locations)){
            msg = "No location data uploaded"
        }else{
            if(is.null(cvdat$collars)){
                if(!is.null(cvdat$animals)){
                    msg = "Must provide collar data if providing animal data"
                }else{
                    msg = "Only location data was uploaded"
                }
            }else{
                if(!is.null(cvdat$animals)){
                    aids = cvdat$animals$animal_id
                    collars2rm_a = cvdat$collars$collar_id[!cvdat$collars$animal_id %in% aids]
                    if(length(collars2rm_a)==0){
                        msg_a = "All collar IDs occurring in collar input data also occur in animal data."
                    }else{
                        msg_a = paste(paste0("Of ", nrow(cvdat$collars), " collar IDs occurring in the collar input data,"),
                                       paste0(length(collars2rm_a), " collar IDs did not occur in the"),
                                       "animal input data and were thus removed", sep = "\n")
                    }
                }
                lcids = unique(cvdat$locations$collar_id)
                collars2rm_b = lcids[!lcids %in% cvdat$collars$collar_id & !lcids %in% collars2rm_a]
                if(length(collars2rm_b)==0){
                    msg_b = "All collar IDs occurring in collar input data also occur in animal data."
                }else{
                    msg_b = paste(paste0("Of ", length(lcids), " collar IDs occurring in the location input data,"),
                                  paste(length(collars2rm_b), " collar IDs did not occur in the"),
                                  "collar input data and were thus removed", sep = "\n")
                }
                msg = c(msg_a, msg_b)
            }
        }
        msg
    })
    
    
    output$load_msg <- renderText({load_msg()})
    
    #----
    ## Tab 2: Summarize data
    
    

    # Summarize data for the collar summary table
    collar.tbl <- reactive({
        req(input$file)
        x = dat()
        tbl = x$collars
        tbl
    })
    
    # Create a DT table of the summarized collar data
    output$collar.tbl <- DT::renderDT(collar.tbl(), options = list(pageLength = 20))
    

    # Summarize data for the animal summary table
    animal.tbl <- reactive({
        req(input$file)
        x = dat()
        tbl = x$animals
        tbl
    })
    
    # Create a DT table of the summarized animal data
    output$animal.tbl <- DT::renderDT(animal.tbl(), options = list(pageLength = 20))
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
