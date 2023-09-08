library(rsconnect)

# rsconnect::setAccountInfo(name='jtillil',
#                           token='EFA4D794F7F31BC6C468A23F58A685A8',
#                           secret='iM2jaeVtIQus7UnVKd9DUhLaZz5eGeB2GNB5OILF')

setwd("/Users/jtm2/Documents/GitHub/QSPanalyse")
rsconnect::deployApp(getwd())