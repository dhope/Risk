library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

# Define UI for application that draws a histogram
ui <-  navbarPage("Risk Assessment", id = "tabs",
                  tabPanel("Home",
                           
                           # Application title
                           titlePanel("Trip preparation risk assessment"),
                           
                           # Sidebar with a slider input for number of bins 
                           # km_driven_highway=970*2,
                           # Floatplane = 2,
                           # N_helicopter_ARU_sites = 35,
                           # number_nights_hotel =3,
                           # number_nights_cabin =6,
                           # avg_segment_length = 300)
                           # sidebarLayout(
                           flowLayout(
                             numericInput('km_highway', "Number of km driven on highway",
                                          value =  500,min = 0, step = 100),
                             # numericInput('km_primary', "Number of km driven on primary roads",
                             #              value =  0,min = 0, step = 100),
                             # numericInput('km_secondary', "Number of km driven on secondary roads",
                             #              value =  0,min = 0, step = 100),
                             numericInput('number_ARUs', "Number of ARU sites",
                                          value =  10,min = 0, step = 1),
                             numericInput('number_clusters', "Number of PC Clusters",
                                          value =  10,min = 0, step = 1),
                             numericInput('float_plane_hours', "Hours of flight in float plane",
                                          value =  10,min = 0, step = 1),
                             numericInput('heli_hours', "Hours of flight in helicopter",
                                          value =  10,min = 0, step = 1),
                             sliderInput("percent_heli_access", label = "Percentage of Helicopter Access",
                                         value = 50, min = 0, max = 100, step =1),       
                             sliderInput("number_nights_hotel", label = "Nights in a hotel",
                                         value = 3, min = 0, max = 40, step =1),
                             sliderInput("number_nights_cabin", label = "Nights in a cabin",
                                         value = 6, min = 0, max = 40, step =1),
                             sliderInput("avg_segment_lengthARUs", label = "Average distance walked for ARUs",
                                         value = 300, min = 0, max = 10000, step =10),
                             sliderInput("avg_segment_lengthcluster", label = "Average distance walked for a cluster",
                                         value = 300, min = 0, max = 10000, step =10),
                             selectInput('bear_density', "Bear Density", 
                                         choices = c("None", "Low", "Medium", "High")),
                             actionButton("assignToBaseline", "Assign to Baseline")
                             # actionButton("genplot", "Update figure")
                           ),
                           
                           # Show a plot of the generated distribution
                           verticalLayout(
                             plotOutput("riskPlot"),
                             actionButton("runscen", "Roll the dice"),
                             textOutput("Death")
                           )
                           
                  ),
                  tabPanel("Settings",
                           flowLayout(
                             numericInput("risk_drive_highway", "Risk highway per 1000km",min = 0, max = 0.01, value = 2*5.1/(1.60934*1000000000)*1000),
                             # numericInput("risk_drive_primary", "Risk primary road per 1000km",min = 0, max = 0.01, value = 1e-8),
                             # numericInput("risk_drive_secondary", "Risk secondary road per 1000km",min = 0, max = 0.01, value = 1e-7),
                             # numericInput("risk_flight_per_flight", "Risk per flight commercial air",min = 0, max = 0.01, value = 1e-6),
                             # numericInput("risk_flight_per_flight_wheel", "Risk per flight small aircraft",min = 0, max = 0.01, value = 1e-6),
                             numericInput("risk_flight_per_hour_float", "Risk per hour float plane",min = 0, max = 0.01, value = 1e-6),
                             numericInput("risk_per_heli_hour", "Risk per hour of helicopter flight",min = 0, max = 0.01, value = 1e-6),
                             numericInput("risk_per_heli_landing", "Risk per helicopter landing",min = 0, max = 0.01, value = 1e-6),
                             numericInput("risk_per_100m_walking", "Risk of walking 100m",min = 0, max = 0.01, value = 1e-6),
                             numericInput("risk_night_hotel", "Risk of night in hotel",min = 0, max = 0.01, value = 1e-6),
                             numericInput("risk_night_cabin", "Risk of night in cabin",min = 0, max = 0.01, value = 1e-6),
                             numericInput("risk_bears_low", "Risk multiplier for low bear density",min = 0, max = 100, value = 2),
                             numericInput("risk_bears_med", "Risk multiplier for medium bear density",min = 0, max = 100, value = 5),
                             numericInput("risk_bears_high", "Risk multiplier for high bear high",min = 0, max = 100, value = 10)
                             # sliderInput("risk_night_cabin", "Risk of night in cabin",min = 0, max = 0.01, value = 1e-6),
                             
                             # actionButton("updateSettings", "Update Settings")
                           )
                           
                           # risk_list <- list(
                           #     risk_drive_per_1000km = list(highway =   2*5.1/(1.60934*1000000000)*1000, #Doubled because driving in moose country
                           #                                  primary = 1e-8, secondary = 1e-7), 
                           #     # https://en.wikipedia.org/wiki/Transportation_safety_in_the_United_States
                           #     risk_flight_per_flight = list(commercial =117/1e9, wheeled = 11700/1e9, float=117000/1e9),
                           #     risk_per_heli_landing = 1e-5,
                           #     risk_night =list(hotel = 1e-10, cabin = 1e-9, airbnb = 1e-10),
                           #     risk_per_100m_walking = 1e-6
                           # )
                           
                  )
)