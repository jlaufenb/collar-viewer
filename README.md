

The United States Fish and Wildlife Service (FWS) GitHub project code is provided on 
an "as is" basis and the user assumes responsibility for its use. FWS has relinquished 
control of the information and no longer has responsibility to protect the integrity, 
confidentiality, or availability of the information. Any reference to specific 
commercial products, processes, or services by service mark, trademark, manufacturer, 
or otherwise, does not constitute or imply their endorsement, recommendation or 
favoring by FWS. The FWS seal and logo shall not be used in any manner to imply 
endorsement of any commercial product or activity by FWS or the United States 
Government.
_____________________________

# GPS Collar Viewer

## Use
`if (!require("shiny")) install.packages("shiny")  
shiny::runGitHub( "collar-viewer", "USFWS", launch.browser=T)`

## Description
An R shiny app for visualizing and summarizing GPS collar data from lynx in Alaska. 
Currently, users upload a RData file that contains a dataframe named "dat". This dataframe at a minimum contains unprojected GPS fixes 
(lat/long), collarIDs and dates. These data are imported and mapped using leaflet.
A summary table is produced that includes includes refuges and fist and last fix
dates, by collarID. Users can subset the data by study site, id, and sex.
