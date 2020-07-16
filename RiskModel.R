risk_incident_minute_walking <- function(topo, tree_den, bear_den, windy)
risk_incident_minute_PC <- case_when(windy~1,
                                   not_windy~2)
risk_incident_minute_helicopter_flying
# US per 100k flying hours crash, and fatalities
# https://rotor.org/Portals/0/2018%20A%20Release%20Full%20Year%20US%20Accident.pdf
# 2018 3.62 0.72 0.65
# 2017 3.70 0.60 0.69
# 2016 3.48 0.54 0.73
# 2015 3.67 0.52
# 2014 4.26 0.65
# 2013 4.95 1.02


risk_incident_each_heli_exit_entrance
risk_incident_minute_driving <- function(age, roadtype)
risk_incident_overnight <- function(camptype, bear_den)
  
  
risk_injury_incident <- function(incidentType)

  
risk_death_incident <- function(incidentType)
  
  
  


tripRisk <- function(min_walking, minPC, min_heli, min_driving){
  pinicent * 
  
  
  
}

risk_drive_per_1000km$highway <- 1e-6
risk_flight_per_flight$float

tripExample <- list(km_driven_highway=970*2,
                    Floatplane = 2,
                    N_helicopter_ARU_sites = 35,
                    number_nights_hotel =3,
                    number_nights_cabin =6,
                    avg_segment_length = 300)

risk_list <- list(
risk_drive_per_1000km = list(highway =   2*5.1/(1.60934*1000000000)*1000, #Doubled because driving in moose country
                             primary = 1e-8, secondary = 1e-7), 
# https://en.wikipedia.org/wiki/Transportation_safety_in_the_United_States
risk_flight_per_flight = list(commercial =117/1e9, wheeled = 11700/1e9, float=117000/1e9),
risk_per_heli_landing = 1e-5,
risk_night =list(hotel = 1e-10, cabin = 1e-9, airbnb = 1e-10),
risk_per_100m_walking = 1e-6
)
calculate_risk <- function(risk_per_event, n_events) {
  1-(1-risk_per_event)**n_events
}


getRisks <- function(tripinfo, risk_list){
  list2env(risk_list, envir = environment())
  list2env(tripinfo, envir = environment())
  
# browser()
driverisk <- calculate_risk(risk_drive_per_1000km$highway, km_driven_highway/1000)
 
flightrisk <- calculate_risk(risk_flight_per_flight$float, Floatplane)
helirisk <- calculate_risk(risk_per_heli_landing, N_helicopter_ARU_sites*2) 
accom_risk <- 1-((1-calculate_risk(risk_night$hotel, number_nights_hotel))*(1-calculate_risk(risk_night$cabin, number_nights_cabin)))
walking_risk <- calculate_risk(risk_per_100m_walking, avg_segment_length/100*N_helicopter_ARU_sites)
 
totalrisk <- 1-((1-driverisk)*(1-flightrisk)*(1-helirisk)*(1-accom_risk)*(1-walking_risk))
  
data.frame(accom_risk, driverisk, flightrisk, helirisk, walking_risk, totalrisk) %>% 
  pivot_longer(cols = everything(),names_to = "risk", values_to = "pInc")
}



tripExample2 <- list(km_driven_highway=970*2,
                    Floatplane = 2,
                    N_helicopter_ARU_sites = 17,
                    number_nights_hotel =3,
                    number_nights_cabin =6,
                    avg_segment_length = 3000)
twotrips <- 
bind_rows(
  getRisks(tripExample, risk_list) %>% mutate(ex = "ARUs"),
  getRisks(tripExample2, risk_list) %>% mutate(ex = "Clusters")
) %>% mutate(risk_f = forcats::fct_reorder(factor(risk), pInc)) 


ggplot(twotrips, aes(risk_f, pInc, colour = ex)) + 
  geom_point(position = position_dodge(width = 0.2))
library(tidyverse)

data.frame(accom_risk, driverisk, flightrisk, helirisk, walking_risk, totalrisk) %>% 
  pivot_longer(cols = everything(),names_to = "risk", values_to = "pInc") %>% 
  mutate(risk_f = forcats::fct_reorder(factor(risk), pInc)) %>% 
  ggplot(aes(risk_f, pInc)) + geom_point()



