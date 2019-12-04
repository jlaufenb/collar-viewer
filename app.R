#
# A work in progress updating the shiny app
#
# Tried to add a panel to upload a RData file of collar data and display 
# these data in the second panel as a leaflet map.

library(shiny)
library(tidyverse)
library(leaflet)
library(shinyWidgets)

# Load the data
# load("./data/dat_short.Rdata")

# Script the global file:
source("code/global.R")

# Define UI for application that draws a histogram
ui <- navbarPage("Lynx Collar Viewer", id="nav",
                 
                 tabPanel("Load data",
                          # Sidebar layout
                          sidebarLayout(
                            sidebarPanel(
                              fileInput(inputId="file", 
                                        label="Select collar data",
                                        multiple = FALSE,
                                        accept = ".RData"), # used to get the file upload control option
                              helpText("Max file size is 32 MB")),
                            mainPanel(
                              uiOutput("validate")
                            )
                          )),
                 
                 
                 tabPanel("Interactive map",
                          div(class="outer",
                              
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              ),
                              
                              leafletOutput("map", width="100%", height="100%"),
                              
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                                            width = 300, height = "auto",
                                            
                                            h3("Collar explorer"),
                                            
                                            selectizeGroupUI(
                                              id = "my-filters",
                                              inline = FALSE,
                                              params = list(
                                                site = list(inputId = "site", label = "Select site", placeholder = 'select'),
                                                sex = list(inputId = "sex", label = "Select sex", placeholder = 'select'),
                                                id = list(inputId = "id", label = "Select id", placeholder = 'select')
                                              )
                                            )
                              )
                          )
                 )
)






# Define server logic 
server <- function(input, output, session) {
  
  # Define the uploaded Rdata file
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
    dat
  })
  
  
  dat.sub <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = dat,
    vars = c("site", "sex", "id")
  )
  
  
  
  
  
  
  
  # Create a map of the subsetted data:
  output$map <- renderLeaflet({
    CollarMap(dat.sub())
  })
}





# Run the application 
shinyApp(ui = ui, server = server)

