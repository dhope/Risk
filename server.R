# Define server logic required to calculate risk and generate plot
server <- function(input, output) {
  
  calculate_risk <- function(risk_per_event, n_events) {
    1-(1-risk_per_event)**n_events
  }
  
  
  getRisks <- function(tripinfo){
    list2env(tripinfo, envir = environment())
    
    # browser()
    driverisk <- calculate_risk(input$risk_drive_highway, input$km_highway/1000)
    
    flightrisk <- calculate_risk(input$risk_flight_per_hour_float, input$float_plane_hours)
    helirisklandings <- calculate_risk(input$risk_per_heli_landing, (input$percent_heli_access/100*(input$number_ARUs+input$number_clusters)*2)+
                                         2*input$number_nights_cabin +2) 
    helirisk_flight <- calculate_risk(input$risk_per_heli_hour, input$heli_hours)
    accom_risk <- 1-((1-calculate_risk(input$risk_night_hotel, 
                                       input$number_nights_hotel))*
                       (1-calculate_risk(input$risk_night_cabin,
                                         input$number_nights_cabin)))
    
    # if(isTRUE(input$bears_present)){
    #     cat("BEARS! OH MY!")
    bear_multiplier <- switch (input$bear_density,
                               'None' = 1,
                               "Low" = input$risk_bears_low,
                               "Medium" = input$risk_bears_med,
                               "High" = input$risk_bears_high
    )
    walking_risk <- 1-(
      (1-calculate_risk(input$risk_per_100m_walking*bear_multiplier,
                        input$avg_segment_lengthARUs*input$number_ARUs)) *
        (1-calculate_risk(input$risk_per_100m_walking*bear_multiplier,
                          input$avg_segment_lengthcluster*input$number_clusters)) )
    # } else{
    #     walking_risk <- 1-((1-calculate_risk(input$risk_per_100m_walking,
    #                                          input$avg_segment_lengthARUs*input$number_ARUs)) *
    #                            (1-calculate_risk(input$risk_per_100m_walking,
    #                                              input$avg_segment_lengthcluster*input$number_clusters)) )
    # }
    
    totalrisk <- 1-((1-driverisk)*(1-flightrisk)*(1-helirisklandings)*(1-accom_risk)*(1-walking_risk)*(1-helirisk_flight))
    
    data.frame(accom_risk, driverisk, flightrisk, helirisklandings,helirisk_flight, walking_risk, totalrisk) %>% 
      pivot_longer(cols = everything(),names_to = "risk", values_to = "pInc") %>% 
      mutate(risk_lab = factor(case_when(
        risk == "accom_risk"~"Risk from overnight stay",
        risk == "driverisk"~"Risk from driving",
        risk == "flightrisk"~"Risk from plane flights",
        risk == "helirisk_flight"~"Risk from helicopter flying",
        risk == "helirisklandings"~"Risk from helicopter landings",
        risk == "walking_risk"~"Risk from walking",
        risk == "totalrisk"~"Total risk"
      ), levels = c("Risk from overnight stay",
                    "Risk from driving",
                    "Risk from plane flights",
                    "Risk from helicopter flying",
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
  re <- eventReactive(input$assignToBaseline, {getRisks(input) %>% mutate(group = "Baseline")}, ignoreNULL = F)
  # output$baseline <- re()
  output$riskPlot <- renderPlot({
    df_ <- df1 <- getRisks(input) %>% 
      mutate(group = "Scenario")
    df_ <- bind_rows(df1, re())
    
    # gr <- getRisks(input)
    ggplot(df_, aes(risk_lab, pInc, colour = group, shape = group)) + 
      geom_point(position = position_dodge(width = 0.2), size = 3) +
      geom_line(aes(group = risk_lab),colour = 'black', linetype =2) +
      labs(x = "", y = "Probability of Incident", colour = "", shape = "") +
      scale_colour_manual(values = c("Baseline" = "blue", "Scenario" = "red" )) +
      theme(axis.text.x = element_text(angle =45, hjust = 1, vjust = 1))
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2]
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    # # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  roll <- eventReactive(input$runscen, {
    df1 <- getRisks(input)
    ifelse( (runif(1)<=df1$pInc[df1$risk == "totalrisk"]),
            ("Oh no, time to fill out a HOIR"),
            ("Everyone returned safely")) })
  output$Death <- renderText(roll())
  
}