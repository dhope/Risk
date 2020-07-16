#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

# Define UI for application that draws a histogram
ui <-  navbarPage("Navbar page", id = "tabs",
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
            numericInput('km_primary', "Number of km driven on primary roads",
                         value =  0,min = 0, step = 100),
            numericInput('km_secondary', "Number of km driven on secondary roads",
                         value =  0,min = 0, step = 100),
            numericInput('number_ARUs', "Number of ARU sites",
                         value =  10,min = 0, step = 1),
            numericInput('number_clusters', "Number of PC Clusters",
                         value =  10,min = 0, step = 1),
            numericInput('number_floatflights', "Number of Float Plane Flights",
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
            checkboxInput('bears_present', "Are bears likely present in area?"),
            actionButton("assignToBaseline", "Assign to Baseline")
            # actionButton("genplot", "Update figure")
        ),

        # Show a plot of the generated distribution
        verticalLayout(
           plotOutput("riskPlot")
        )
    
),
tabPanel("Settings",
         flowLayout(
             numericInput("risk_drive_highway", "Risk highway per 1000km",min = 0, max = 0.01, value = 2*5.1/(1.60934*1000000000)*1000),
             numericInput("risk_drive_primary", "Risk primary road per 1000km",min = 0, max = 0.01, value = 1e-8),
             numericInput("risk_drive_secondary", "Risk secondary road per 1000km",min = 0, max = 0.01, value = 1e-7),
             numericInput("risk_flight_per_flight", "Risk per flight commercial air",min = 0, max = 0.01, value = 1e-6),
             numericInput("risk_flight_per_flight_wheel", "Risk per flight small aircraft",min = 0, max = 0.01, value = 1e-6),
             numericInput("risk_flight_per_flight_float", "Risk per flight float plane",min = 0, max = 0.01, value = 1e-6),
             numericInput("risk_per_heli_landing", "Risk per helicopter landing",min = 0, max = 0.01, value = 1e-6),
             numericInput("risk_per_100m_walking", "Risk of walking 100m",min = 0, max = 0.01, value = 1e-6),
             numericInput("risk_night_hotel", "Risk of night in hotel",min = 0, max = 0.01, value = 1e-6),
             numericInput("risk_night_cabin", "Risk of night in cabin",min = 0, max = 0.01, value = 1e-6),
             numericInput("risk_bears", "Risk multiplier for bear presence",min = 0, max = 100, value = 2)
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
# Define server logic required to calculate risk and generate plot
server <- function(input, output) {
    
    calculate_risk <- function(risk_per_event, n_events) {
        1-(1-risk_per_event)**n_events
    }
    
    
    getRisks <- function(tripinfo){
        list2env(tripinfo, envir = environment())
        
        # browser()
        driverisk <- calculate_risk(input$risk_drive_highway, input$km_highway/1000)
        
        flightrisk <- calculate_risk(input$risk_flight_per_flight_float, input$number_floatflights)
        helirisk <- calculate_risk(input$risk_per_heli_landing, (input$percent_heli_access/100*(input$number_ARUs+input$number_clusters)*2)+
                                       2*input$number_nights_cabin +2) 
        accom_risk <- 1-((1-calculate_risk(input$risk_night_hotel, 
                                           input$number_nights_hotel))*
                             (1-calculate_risk(input$risk_night_cabin,
                                               input$number_nights_cabin)))

        if(isTRUE(input$bears_present)){
            cat("BEARS! OH MY!")
            walking_risk <- 1-((1-calculate_risk(input$risk_per_100m_walking*input$risk_bears,
                                                 input$avg_segment_lengthARUs*input$number_ARUs)) *
                                   (1-calculate_risk(input$risk_per_100m_walking*input$risk_bears,
                                                     input$avg_segment_lengthcluster*input$number_clusters)) )
        } else{
            walking_risk <- 1-((1-calculate_risk(input$risk_per_100m_walking,
                                                 input$avg_segment_lengthARUs*input$number_ARUs)) *
                                   (1-calculate_risk(input$risk_per_100m_walking,
                                                     input$avg_segment_lengthcluster*input$number_clusters)) )
        }
        
        totalrisk <- 1-((1-driverisk)*(1-flightrisk)*(1-helirisk)*(1-accom_risk)*(1-walking_risk))
        
        data.frame(accom_risk, driverisk, flightrisk, helirisk, walking_risk, totalrisk) %>% 
            pivot_longer(cols = everything(),names_to = "risk", values_to = "pInc") %>% 
            mutate(risk_lab = factor(case_when(
                risk == "accom_risk"~"Risk from overnight stay",
                risk == "driverisk"~"Risk from driving",
                risk == "flightrisk"~"Risk from flying",
                risk == "helirisk"~"Risk from helicopter landings",
                risk == "walking_risk"~"Risk from walking",
                risk == "totalrisk"~"Total risk"
                       ), levels = c("Risk from overnight stay",
                                     "Risk from driving",
                                     "Risk from flying",
                                     "Risk from helicopter landings",
                                     "Risk from walking",
                                     "Total risk")))
    }
    # km_highway
    # km_primary
    # km_secondary
    # number_ARUs
    # number_clusters
    # number_floatflights
    # percent_heli_access
    # bears_present
    re <- eventReactive(input$assignToBaseline, {getRisks(input) %>% mutate(group = "Baseline")})
    # output$baseline <- re()
    output$riskPlot <- renderPlot({
        df_ <- df1 <- getRisks(input) %>% 
            mutate(group = "Scenario")
        try({df_ <- bind_rows(df1, re())})
        
        # gr <- getRisks(input)
        ggplot(df_, aes(risk_lab, pInc, colour = group, shape = group)) + 
            geom_point(position = position_dodge(width = 0.2), size = 3) +
            labs(x = "", y = "Probability of Incident", colour = "", shape = "") +
            scale_colour_grey() +
            theme(axis.text.x = element_text(angle =45, hjust = 1, vjust = 1))
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)







