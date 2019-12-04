#library(DT)

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
                          fluidPage(
                              fileInput(inputId="file", 
                                        label="Select collar data",
                                        multiple = FALSE,
                                        accept = ".RData")), # used to get the file upload control option

                              DT::DTOutput("sum.tbl")
                          ),
                 
                 
                 tabPanel("Interactive map",
                          div(class="outer",
                              
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("style.css"),
                                includeScript("gomap.js")
                              ),
                              
                              leafletOutput("map", width="100%", height="100%"),
                              
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                                            width = 300, height = "auto",
                                            
                                            selectizeGroupUI(
                                              id = "my-filters",
                                              inline = FALSE,
                                              params = list(
                                                site = list(inputId = "site", title = "Select site", placeholder = 'select'),
                                                sex = list(inputId = "sex", title = "Select sex", placeholder = 'select'),
                                                age = list(inputId = "age", title = "Select age class", placeholder = 'select'),
                                                id = list(inputId = "id", title = "Select id", placeholder = 'select')
                                              )
                                            )
                              )
                          )
                 )
)






# Define server logic 
server <- function(input, output, session) {
  
  ## LOAD DATA
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
  
  # Subset data for summary table:
  dat.tbl <- reactive({
    req(input$file)
    dat() %>% 
      group_by("ID" = id, 
               "Study site" = site,
               "AgeClass" = age,
               "Sex" = sex) %>%  # Creates a summary table of the data
      summarise(
        "Capture date" = as.Date(first(capture_date)),
        "Fix schedule" = last(fixsched),
        "Current fix rate" = last(fixrate),
        "First fix" = min(date),
        "Last fix" = max(date)
      )
  })
  
  # Displays a summary table of the data:
  output$sum.tbl <- DT::renderDT(dat.tbl(), options = list(pageLength = 20))
  
  
  ## MAP
  
  # Subset the data based on user input:
  dat.sub <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = dat,
    vars = c("site", "sex", "age", "id")
  )

  # Create a map of the subsetted data:
  output$map <- renderLeaflet({
    CollarMap(dat.sub())
  })
  
}





# Run the application 
shinyApp(ui = ui, server = server)

