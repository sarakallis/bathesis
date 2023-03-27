###Title: Replication Data BA Thesis
###Author: Sara Kallis
###Date: 31 May 2021

setwd("/Users/sara/Documents/FS21/Bachelor Thesis/Analysis")
getwd()
library(stargazer)
library(fansi)
library(sjPlot) 
library(sjmisc) 
library(ggplot2) 
library(car)
library(dplyr)
library(sensemakr)

data <- read.csv("/Users/sara/Documents/FS21/Bachelor Thesis/Analysis/Data For Analysis/data_27.05.imputed.csv")

##1. Descriptive Statistics####
  ##1.1: Spatial Distribution Graphs
    ##made in excel

  ##1.2Temporal Distribution Graphs####
    map <- st_read("/Users/sara/Documents/FS21/Bachelor Thesis/Data/Geo/caf_admbnda_adm3_200k_sigcaf_reach_itos_ocha.shp")
       map$admin3Name <- gsub("é", "e", map$admin3Name)
       map$admin3Name <- gsub("è", "e", map$admin3Name)
       map$admin3Name <- gsub("-", " ", map$admin3Name)
       map$admin3Name <- gsub("ï", "i", map$admin3Name)
        map$admin3Name <- gsub("'", "", map$admin3Name)
    data_geomapping <- data[, c("commune", "idp", "pbactivity", "multilevel")]
        names(data:geomapping)[names(data_geomapping) == "commune"] <- "admin3Name"
    map_idp_pb_ml <- full_join(map, data_geomapping)
    
    ##Plot 1: Number of IDPs per commune
    ggplot(map_idp_pb_ml) + 
      geom_sf(aes(fill = idp)) +
      scale_fill_gradient(low = "honeydew3", high = "brown4", na.value = "gray100") 
    
    ##Plot 2: Number of IDP-inclusive p& multilevel eacebuilding activities
    ggplot(map_idp_pb_ml) + 
      geom_sf(aes(fill = pbactivity)) +
      scale_fill_gradient(low = "honeydew3", high = "brown4", na.value = "gray100") 
    
    ggplot(map_idp_pb_ml) + 
      geom_sf(aes(fill = multilevel)) +
      scale_fill_gradient(low = "honeydew3", high = "brown4", na.value = "gray100") 
    

    #Plot Test
    
  #1.3 Variables Descriptive Statistics (selection of variables from process below, section 2)####
    data.analysis <- data[, c("commune", "year", "month", "Admin1", "Admin2", "idp", "pbactivity", "multilevel",
                           "roads", "radio_nat", "radio_loc", "roadblocks", "justice_tribunal", 
                          "violence_sum", "pop_2015", "femme", "tel_networks", "route_to_prefcapital", "FACA")]
    stargazer(data.analysis, type = "html", title="Descriptive Statistics", 
          digits=1, out="/Users/sara/Documents/FS21/Bachelor Thesis/Analysis/Tables/desc2.doc")

    
    
  
##2. Regression Analysis#####
  ###Establish which controls to use:####
    summary(lm(idp ~ urban + mine + asphalt_roads + roads + rural_track + waterways + route_to_prefcapital
           + tel_networks + radio_nat + radio_loc + ele_network + edu_pri_lack_schools + police
           + gendarmerie + prison + FACA + armed_groups + roadblocks + justice_tribunal, data = data))
    ##***: roads + radio_nat + radio_loc + prison + roadblocks + justice_tribunal
    ##**: tel_networks / *: route_to_prefcapital + police / . : edu_pri_lack_schools +FACA
    ##no effect: rural_tracks; waterways + ele_network + gendarmerie + armed_groups, urban

    summary(lm(idp ~ violence_dummy + violence_sum + pop_2015 + femme, data = data))
    ##***: violence_sum + pop + femme
    ##no effect: violence_dummy

    ##after adding all *** from above, prison was no longer significant
    ##iteratively adding ** variables and checking if model fit improves (R^2 > 0.4622, adj > 0.4554)
    summary(lm(idp ~ roads + radio_nat + radio_loc + roadblocks + + justice_tribunal + violence_sum + pop_2015 + femme
           + VARIABLE , data = data))
    ##yes, model fit improves: tel_networks + route_to_prefcapital + FACA
    ##re-run model with all these controls:
    summary(lm(idp ~ roads + radio_nat + radio_loc + roadblocks + + justice_tribunal + violence_sum + pop_2015 + femme
           + tel_networks + route_to_prefcapital + FACA , data = data)) 
    ##950-312 Observations, R^2 = 0.478, Adj. = 0.4688
    
###2.1 OLS Models####
    
    ###Main Models
    
    ols1 <- lm(idp ~ pbactivity + multilevel, data = data)
    ols2 <- lm(idp ~ pbactivity + multilevel + violence_sum + FACA + roadblocks
               + roads + radio_nat + radio_loc + tel_networks
               + justice_tribunal  + pop_2015 + femme
               + route_to_prefcapital, data = data)
    summary(ols2)
    
    ols3 <- lm(log1p(idp) ~ pbactivity + multilevel, data = data.analysis)
    
    ols4 <- lm(log1p(idp) ~ pbactivity + multilevel + violence_sum + FACA + roadblocks
               + roads + radio_nat + radio_loc + tel_networks
               + justice_tribunal  + pop_2015 + femme
               + route_to_prefcapital, data = data)
    
     ols5 <- lm(log1p(idp) ~ pbactivity + multilevel + violence_sum + FACA + roadblocks +
                 + roads + radio_nat + radio_loc + urban + rural_track +
                 + justice_tribunal
               , data = data) ###best fit
     summary(ols5)
     ####Present Results
     stargazer(ols1, ols2, ols3, ols4, ols5, type = "html", out = "/Users/sara/Documents/FS21/Bachelor Thesis/Analysis/Tables/main_regression_3.doc")
     #Forest-Coefficient Plots
     plot_model(ols2,type = "est",show.values = T, value.offset = 0.35, value.size = 3.2, 
                colors = c("darkseagreen4", "firebrick"), axis.labels = c("km to prefecture capital", "pop: female", "population",
                                                                          "court", "telephone network", "local radio", "national radio",
                                                                          "roads", "roadblocks", "FACA", "violent events", "multilevel", 
                                                                          "inclusive peacebuilding"),title = "Estimated Coefficients (Model2)") 

     plot_model(ols5, type = "est", show.values = T, value.offset = 0.35, value.size = 3.2, 
                colors = c("darkseagreen4", "firebrick"), axis.labels = c("court", "rural tracks", "urban",  "local radio",
                                                                          "national radio", "roads", "roadblocks", "FACA",
                                                                          "violent events", "multilevel", "inclusive peacebuilding"),
                                                                           title = "Estimated Coefficients (Model 5)") 
                                                                          
   
### 2.2 Robustness Checks#####
     ####2.3.1 Violence as proxy DV####
     ols6 <- lm(log1p(idp) ~ violence_sum, data = data)

     ols7 <- lm(violence_sum ~ pbactivity*multilevel + FACA + roadblocks
                  + rural_track
                  + justice_tribunal + log1p(pop_2015) + log1p(femme)
                , data = data) 
     
     glm8 <- glm(violence_dummy ~ pbactivity*multilevel + rural_track + ele_network 
                 + gendarmerie + FACA + armed_groups + justice_tribunal , family = "binomial", data = data)
   
     
     ols9 <- lm(violence_dummy ~ pbactivity*multilevel + rural_track + ele_network
                 + gendarmerie + FACA + armed_groups + justice_tribunal, data = data)
     
     ols10 <- lm(violence_sum ~ idp + pbactivity*multilevel + roadblocks
                 + rural_track + FACA
                 + justice_tribunal + log1p(pop_2015) + log1p(femme)
                 , data = data)

    stargazer(ols6, glm8, ols9, ols7, ols10, type = "html", out = "/Users/sara/Documents/FS21/Bachelor Thesis/Analysis/Tables/robustness_violence.doc")
    
    
    ####2.3.2 Limited to low violence, (violence_sum < 10) ####
    data.lowv <- read.csv("/Users/sara/Documents/FS21/Bachelor Thesis/Analysis/Data For Analysis/data_27.05.lowviolence.csv")
    
    ols11 <- lm(log1p(idp) ~ pbactivity + multilevel, data = data.lowv)
    
    ols12 <- lm(log1p(idp) ~ pbactivity + multilevel + violence_sum + FACA + roadblocks
               + roads + radio_nat + radio_loc + tel_networks
               + justice_tribunal  + pop_2015 + femme
               , data = data.lowv)
    
    ols13 <- lm(log1p(idp) ~ pbactivity + multilevel + violence_sum + FACA + roadblocks +
                 + roads + radio_nat + radio_loc + urban + rural_track +
                 + justice_tribunal
               , data = data.lowv) ###best fit
    
    stargazer(ols11, ols12, ols13, type = "html", out = "/Users/sara/Documents/FS21/Bachelor Thesis/Analysis/Tables/robustness_lowv.doc")
    
    ####2.3.3 Other control variables####
    
    ols14 <- lm(log1p(idp) ~ pbactivity + multilevel + rural_track + waterways + ele_network + gendarmerie + armed_groups + urban, data = data)
    summary(ols14)
    
    ####2.3.4 Time-Lagged IVs & Violence####
    
    datalag1 <- data %>% arrange(commune, Admin1, Admin2, year, month) %>% 
      group_by(commune, Admin1, Admin2) %>%
      mutate(violence_dummy = dplyr::lag(violence_dummy,1)
             ,violence_sum = dplyr::lag(violence_sum,1)
             ,pbactivity = dplyr::lag(pbactivity,1)
             ,multilevel = dplyr::lag(multilevel,1) )
    
    datalag2 <- data %>% arrange(commune, Admin1, Admin2, year, month) %>% 
      group_by(commune, Admin1, Admin2) %>%
      mutate(violence_dummy = dplyr::lag(violence_dummy,2)
             ,violence_sum = dplyr::lag(violence_sum,2)
             ,pbactivity = dplyr::lag(pbactivity,2)
             ,multilevel = dplyr::lag(multilevel,2) )
    
    datalag3 <- data %>% arrange(commune, Admin1, Admin2, year, month) %>% 
      group_by(commune, Admin1, Admin2) %>%
      mutate(violence_dummy = dplyr::lag(violence_dummy,3)
             ,violence_sum = dplyr::lag(violence_sum,3)
             ,pbactivity = dplyr::lag(pbactivity,3)
             ,multilevel = dplyr::lag(multilevel,3) )
    
    datalag4 <- data %>% arrange(commune, Admin1, Admin2, year, month) %>% 
      group_by(commune, Admin1, Admin2) %>%
      mutate(violence_dummy = dplyr::lag(violence_dummy,4)
             ,violence_sum = dplyr::lag(violence_sum,4)
             ,pbactivity = dplyr::lag(pbactivity,4)
             ,multilevel = dplyr::lag(multilevel,4) )
    
    datalag5 <- data %>% arrange(commune, Admin1, Admin2, year, month) %>% 
      group_by(commune, Admin1, Admin2) %>%
      mutate(violence_dummy = dplyr::lag(violence_dummy,5)
             ,violence_sum = dplyr::lag(violence_sum,5)
             ,pbactivity = dplyr::lag(pbactivity,5)
             ,multilevel = dplyr::lag(multilevel,5) )

    
    ols15 <- lm(log1p(idp) ~ pbactivity + multilevel + violence_sum + FACA + roadblocks +
                 + roads + radio_nat + radio_loc + urban + rural_track +
                 + justice_tribunal
               , data = datalag1) 
    summary(ols15)
    
    ols16 <- lm(log1p(idp) ~ pbactivity + multilevel + violence_sum + FACA + roadblocks +
                 + roads + radio_nat + radio_loc + urban + rural_track +
                 + justice_tribunal
               , data = datalag2) 
    summary(ols16)
    
    ols17 <- lm(log1p(idp) ~ pbactivity + multilevel + violence_sum + FACA + roadblocks +
                 + roads + radio_nat + radio_loc + urban + rural_track +
                 + justice_tribunal
               , data = datalag3) 
    summary(ols17)
    
    ols18 <- lm(log1p(idp) ~ pbactivity + multilevel + violence_sum + FACA + roadblocks +
                 + roads + radio_nat + radio_loc + urban + rural_track +
                 + justice_tribunal
               , data = datalag4) 
    summary(ols18)
    
    ols19 <- lm(log1p(idp) ~ pbactivity + multilevel + violence_sum + FACA + roadblocks +
                 + roads + radio_nat + radio_loc + urban +
                 + justice_tribunal
               , data = datalag5) 
    summary(ols19)
    
    stargazer(ols15, ols16, ols17, ols18, ols19, type = "html", out = "/Users/sara/Documents/FS21/Bachelor Thesis/Analysis/Tables/robustness_lags.doc")
    
    ####2.3.5 Sensitivity Analysis: Omitted Variable Bias####
    #install.packages("sensemakr")
    ##Parameter for an unobserved confounding variable that I am quantifying as a 'worst case' scenario is: should have 1:1 effect on x and y like benchmark variable 'violence_sum'
    ##Theoretically this means: "If there is an omitted variable equally an infuential as violence in influencing conflict displacement, how strong would it need to be to nullify my findings?
      
    
    sensitivity1 <- sensemakr(model = ols2, 
                              treatment = "pbactivity",
                              benchmark_covariates = "violence_sum",
                              kd = 1:1,
                              ky = 1:1, 
                              q = 1,
                              alpha = 0.05, 
                              reduce = TRUE)
    
        summary(sensitivity1)##Partial R2: 4.11% / R.V. 1: 18.68% / R.V.2: 7.99%
    
    ovb_minimal_reporting(sensitivity1)
    ovb_minimal_reporting(sensitivity2)
    ovb_minimal_reporting(sensitivity3)
    ovb_minimal_reporting(sensitivity4)
    
    
    
   
                        
    
    sensitivity2 <- sensemakr(model = ols2, 
                              treatment = "multilevel",
                              benchmark_covariates = "violence_sum",
                              kd = 1:1,
                              ky = 1:1, 
                              q = 1,
                              alpha = 0.05, 
                              reduce = FALSE)
   
     
        summary(sensitivity2) ##Partial R2: 2.48% / R.V. 1: 14.72% / R.V.2: 3.49%,R2dz.x = 0.0101, R2yz.dx = 0.0102

    sensitivity3 <- sensemakr(model = ols5, 
                              treatment = "pbactivity",
                              benchmark_covariates = "violence_sum",
                              kd = 1:1,
                              ky = 1:1, 
                              q = 1,
                              alpha = 0.05, 
                              reduce = TRUE)
        summary(sensitivity3) #0.05%, 2.12%, 0%, R2dz.x = 0.0057, R2yz.dx = 0.0144
    
    sensitivity4 <- sensemakr(model = ols5, 
                              treatment = "multilevel",
                              benchmark_covariates = "violence_sum",
                              kd = 1:1,
                              ky = 1:1, 
                              q = 1,
                              alpha = 0.05, 
                              reduce = FALSE)
    
        summary(sensitivity4) #0%, 0.64%, 0.05%, 1x violence: R2dz.x = 0.0089, R2yz.dx = 0.0145
    