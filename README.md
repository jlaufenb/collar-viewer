# boreal_lynx

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

This is a shiny app for visualizing and summarizing GPS collar data from lynx in Alaska. 
Currently, users upload a .csv file that at a minimum contains unprojected GPS fixes 
(lat/long), collarIDs and dates. See "Data.csv" as an example. These are read in and mapped using leaflet.
A summary table is produced that includes includes refuges and fist and last fix
dates, by collarID. Users can subset the data by date range, collarID, and Refuge.
