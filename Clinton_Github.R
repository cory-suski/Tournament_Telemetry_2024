## Install Required packages -----------------------------------------------
install.packages("remotes")
install.packages("ggfortify")
install.packages("knitr")
install.packages("xtable")
install.packages("forcats")
install.packages("ggtext")
install.packages("gtsummary")
install.packages("remotes")
remove.packages("pkgconfig")
install.packages("pkgconfig")
install.packages("lme4")
install.packages("dplyr")
install.packages("devtools")
# This will install actel's and RSP's development version:
# The displayed version should now be 1.2.1.9016
# If that is not the case, unload actel and load it again.
remotes::install_github("hugomflavio/actel",
                        build_opts = c("--no-resave-data", "--no-manual"), 
                        build_vignettes = TRUE, force=TRUE)

remotes::install_github("YuriNiella/RSP", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
# Libraries ---------------------------------------------------------------
library(AICcmodavg)
library(ggpubr)
library(Matrix)
library("actel")
library("RSP")
library("raster")
library(remotes)
library(dplyr)
library(readr)
library(chron)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(survival)
library(condSURV)
library(ggfortify)
library(knitr)
library(xtable)
library(broom)
library(forcats)
library(ggtext)
library(gtsummary)
library(lme4)
library(emmeans)
library(car)
library(gt)
library(optimx)
library(rms)
library(MuMIn)

# Explore the data with Actel ---------------------------------------------

#clean data to remove invalid events/speeds based on desired metrics. Manually inspect warnings
ClintonNov<-explore(tz='America/Chicago', report = TRUE, auto.open= TRUE,save.detections = TRUE,
                    jump.warning = 1,GUI=c("never"), min.per.event =  2, speed.method = "last to first", 
                    speed.warning = .9 ,speed.error=1 , discard.orphans = TRUE,  detections.y.axis = c("arrays"), max.interval = 60 )
#explore data through residency function, same restrictions as explore
ClintonNovRes<- residency(tz='America/Chicago', report = TRUE, auto.open= TRUE,save.detections = TRUE,
                          jump.warning = 1,GUI=c("never"), min.per.event =  2, speed.method = "last to first", 
                          speed.warning = .9 ,speed.error=1 , discard.orphans = TRUE,  detections.y.axis = c("arrays"), 
                          max.interval = 60 )
#to view valid detections
valid.detections<-ClintonNov$valid.detections
# Loading Raster ----------------------------------------------------------

#make sure the dbf, shx, and shp file are all in work directory

water.shape <- shapeToRaster(shape = "Polygons.shp",spatial = "spatial.csv",buffer = .05, coord.x = "Longitude", coord.y = "Latitude", size = 0.0001)  
raster::plot(water.shape)
water.transition <- transitionLayer(water.shape, directions = 16)

#need to define water.shape as a raster and not a spatraster - that is done with the raster() function below
ClintonRaster<- plotRaster(input = ClintonNov, base.raster = water.shape %>% raster(), coord.x = "Longitude", coord.y = "Latitude",
                           land.col = "gray")

ClintonRaster
# distance matrix ---------------------------------------------------------
rsp.data <- runRSP(input = ClintonNov, t.layer = water.transition, coord.x = "Longitude", coord.y = "Latitude")
#create a distance matrix of receivers 
dist.mat <- distancesMatrix(water.transition, coord.x = "Longitude", coord.y = "Latitude")
y
raster::plot(ClintonRaster)
## Extract Detections for Return analysis ----------------------------------
#valid detections from actel explore functions
valid.detections<- ClintonNov$valid.detections
#create DF from all tags in study area.
tag2466<-valid.detections$'A69-9002-2466' 
tag2465<-valid.detections$'A69-9002-2465' 
tag2462<-valid.detections$'A69-9002-2462' 
tag2461<-valid.detections$'A69-9002-2461' 
tag2472<-valid.detections$'A69-9002-2472' 
tag2471<-valid.detections$'A69-9002-2471' 
tag2464<-valid.detections$'A69-9002-2464' 
tag2468<-valid.detections$'A69-9002-2468'  
tag2463<-valid.detections$'A69-9002-2463' 
tag2431<-valid.detections$'A69-9002-2431' 
tag2432<-valid.detections$'A69-9002-2432' 
tag2433<-valid.detections$'A69-9002-2433'  
tag2437<-valid.detections$'A69-9002-2437' 
tag2438<-valid.detections$'A69-9002-2438'  
tag2439<-valid.detections$'A69-9002-2439'  
tag2440<-valid.detections$'A69-9002-2440' 
tag2441<-valid.detections$'A69-9002-2441' 
tag2442<-valid.detections$'A69-9002-2442' 
tag2443<-valid.detections$'A69-9002-2443' 
tag2434<-valid.detections$'A69-9002-2434' 
tag2435<-valid.detections$'A69-9002-2435'  
tag2436<-valid.detections$'A69-9002-2436'  
tag2430<-valid.detections$'A69-9002-2430'  
tag2429<-valid.detections$'A69-9002-2429' 
tag2428<-valid.detections$'A69-9002-2428' 
tag2427<-valid.detections$'A69-9002-2427'  
tag2426<-valid.detections$'A69-9002-2426'  
tag2425<-valid.detections$'A69-9002-2425'  
tag2424<-valid.detections$'A69-9002-2424' 
tag2405<-valid.detections$'A69-9002-2405' 
tag2406<-valid.detections$'A69-9002-2406'  
tag2404<-valid.detections$'A69-9002-2404'  
tag2403<-valid.detections$'A69-9002-2403'  
tag2402<-valid.detections$'A69-9002-2402'  
tag2401<-valid.detections$'A69-9002-2401'
tag2400<-valid.detections$'A69-9002-2400'  
tag2399<-valid.detections$'A69-9002-2399'  
tag2398<-valid.detections$'A69-9002-2398' 
tag2397<-valid.detections$'A69-9002-2397'  
tag10845<-valid.detections$'A69-9002-10845'
tag10846<-valid.detections$'A69-9002-10846'
tag10847<-valid.detections$'A69-9002-10847'
tag10848<-valid.detections$'A69-9002-10848'
tag10849<-valid.detections$'A69-9002-10849'
tag10850<-valid.detections$'A69-9002-10850'
tag10855<-valid.detections$'A69-9002-10855'
tag10856<-valid.detections$'A69-9002-10856'
tag13236<-valid.detections$'A69-9002-13236'
tag13237<-valid.detections$'A69-9002-13237'
tag2390<-valid.detections$'A69-9002-2390'
tag2391<-valid.detections$'A69-9002-2391'
tag2392<-valid.detections$'A69-9002-2392'
tag2393<-valid.detections$'A69-9002-2393'
tag2394<-valid.detections$'A69-9002-2394'
tag2395<-valid.detections$'A69-9002-2395'
tag2396<-valid.detections$'A69-9002-2396'
tag2386<-valid.detections$'A69-9002-2386'
tag2387<-valid.detections$'A69-9002-2387'
tag2388<-valid.detections$'A69-9002-2388'
tag2389<-valid.detections$'A69-9002-2389'
tag2385<-valid.detections$'A69-9002-2385'
tag2384<-valid.detections$'A69-9002-2384'
tag2383<-valid.detections$'A69-9002-2383'
tag13569<-valid.detections$'A69-1604-13569'
tag13568<-valid.detections$'A69-1604-13568'
tag13567<-valid.detections$'A69-1604-13567'
tag13566<-valid.detections$'A69-1604-13566'
tag13565<-valid.detections$'A69-1604-13565'
tag13564<-valid.detections$'A69-1604-13564'
tag13236<-valid.detections$'A69-9002-13236'
tag13238<-valid.detections$'A69-9002-13238'
tag13239<-valid.detections$'A69-9002-13239'
tag13240<-valid.detections$'A69-9002-13240'
tag13241<-valid.detections$'A69-9002-13241'
tag13242<-valid.detections$'A69-9002-13242'
tag13243<-valid.detections$'A69-9002-13243'
tag13244<-valid.detections$'A69-9002-13244'
tag13245<-valid.detections$'A69-9002-13245'
tag13246<-valid.detections$'A69-9002-13246'
tag13247<-valid.detections$'A69-9002-13247'
#bind all dataframes 
tag.detections<-rbind(
  tag2397,
  tag2398,
  tag2399,
  tag2400,
  tag2401,
  tag2402,
  tag2403,
  tag2404,
  tag2405,
  tag2406,
  tag2424,
  tag2425,
  tag2426,
  tag2427,
  tag2428,
  tag2429,
  tag2430,
  tag2431,
  tag2432,
  tag2433,
  tag2434,
  tag2435,
  tag2436,
  tag2437,
  tag2438,
  tag2439,
  tag2440,
  tag2441,
  tag2442,
  tag2443,
  tag2461,
  tag2462,
  tag2463,
  tag2464,
  tag2465,
  tag2466,
  tag2468,
  tag2471,
  tag2472,
  tag10845,
  tag10846,
  tag10847,
  tag10848,
  tag10849,
  tag10850,
  tag10850,
  tag10855,
  tag10856,
  tag13236,
  tag13237,
  tag2390,
  tag2391,
  tag2392,
  tag2393,
  tag2394,
  tag2395,
  tag2396,
  tag2386,
  tag2387,
  tag2388,
  tag2389,
  tag2385,
  tag2384,
  tag2383,
  tag13569,
  tag13568,
  tag13567,
  tag13566,
  tag13565,
  tag13564,
  tag13238,
  tag13239,
  tag13240,
  tag13241,
  tag13242,
  tag13243,
  tag13244,
  tag13245,
  tag13246,
  tag13247
)
unique(tag.detections$Signal)
#convert to Sensor data to Celsius based on tag slope and intercept (from tag specification sheet)
tag.detections$Sensor.Value<-(.1596*(tag.detections$Sensor.Value)-5)
# RETURN ANALYSIS ---------------------------------------------------------
## Data Manipulation for Return Analysis -----------------------------------
#import biometric file to merge on signal 
biometrics_RA <- read_csv("biometrics.csv",
                          col_types = cols(Signal = col_character()))
RA_Merge<-merge(tag.detections,biometrics_RA,by= intersect(x = "Signal", y= "Signal"))
#select for cols used in analysis
RA_df= RA_Merge %>% select(Timestamp, Receiver, Signal, Transmitter,Group, Release.date, Array, Section)
#Rename columns for analysis
RA_df = RA_df %>% rename(Detect.Time = 'Timestamp',Release.Time = "Release.date")
#pull mortalities
Morts<-subset(RA_df, Signal %in% c("2441","13566","13568","2400",
                                   "2384","2396","10845","10847","10856",
                                   "13236","13237","13239","13241","13244",
                                   "13247","2392"))
#filter out deaths or tags with weird detection histories
RA_df<-RA_df[!(RA_df$Signal %in% c(2441,13566,13568,2400,2384,2396,10845,
                                   10847,10856,13236,13237,13239,
                                   13241,13244,13247,2392)),]
#confirm all tags are accounted for in analysis - should be 79
unique(RA_df$Signal)
unique(Morts$Signal)
#split df into tournaments, df too big to save to excel
tag.tournament<-split(RA_df, RA_df$Group)
#save to excel - No need to if explore results havent changed
write.xlsx(tag.tournament, "C:/Users/YOUR_WD_HERE") #saved as csv - change Release and detection columns to Datetime
#import with corrected assignments of date/time - save as CSV and Name appropriately
#since we split, we will likely have to import each tournament's csv individually which is super annoying
#but could be useful for later analyses or plots
C1 <- read_csv("C1.csv", col_types = cols(Detect.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                          Release.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) 
C2 <- read_csv("C2.csv", col_types = cols(Detect.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                          Release.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) 
T1 <- read_csv("T1.csv", col_types = cols(Detect.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                          Release.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) 
T2 <- read_csv("T2.csv", col_types = cols(Detect.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                          Release.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S")))  
T3 <- read_csv("T3.csv", col_types = cols(Detect.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                          Release.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S")))  
T4 <- read_csv("T4.csv", col_types = cols(Detect.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                          Release.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) 
T5 <- read_csv("T5.csv", col_types = cols(Detect.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                          Release.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
T6<- read_csv("T6.csv", col_types = cols(Detect.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                         Release.Time = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
#bind  these DF - two df to run Coxph against both controls
## Coxph for Controls ------------------------------------------------------
#test controls agains eachother to see if there are differences
cntrlCox<-rbind(C1,C2)
#split date and time into two columns for both Detections and Release
cntrlCox['Detect.Date'] <-format(as.POSIXct(cntrlCox$Detect.Time,format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d") 
cntrlCox$Detect.Time <- format(as.POSIXct(cntrlCox$Detect.Time,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S")
#now for release
cntrlCox['Release.Date'] <-format(as.POSIXct(cntrlCox$Release.Time,format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d") 
cntrlCox$Release.Time <- format(as.POSIXct(cntrlCox$Release.Time,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S")
#reorder the columns so they make sense - Keep time for now, maybe get rid of later
cntrlCox<- cntrlCox %>% select(Release.Date,Release.Time,Signal, Group, Detect.Date, Detect.Time, Section, Receiver, Transmitter, Array) %>% 
  #Add a column to calculate days between Release and detection
  mutate(
    Elapsed.T = as.duration(Release.Date %--% Detect.Date) / ddays(1)
  )
unique(cntrlCox$Release.Date)
unique(cntrlCox$Release.Time)
#add a column for "status" 0 = not in lake, 1 = in Lake (0=non event, 1= event)
cntrlCox<-cntrlCox %>% dplyr::mutate(Return = ifelse(Section == "Marina", "0", "1"))
cntrlCox$Return <- as.numeric(as.character(cntrlCox$Return))
#subset data by lake section
cntrlCox1<-subset(cntrlCox, Section=='Lake')
cntrlCox2<-subset(cntrlCox, Section=='Marina')
#bind df
cntrlcoxph<-rbind(cntrlCox1,cntrlCox2)
#make sure dates are considered dates
cntrlcoxph$Release.Date <-ymd(cntrlcoxph$Release.Date)
cntrlcoxph$Detect.Date <-ymd(cntrlcoxph$Detect.Date)
#double check format is noted as date
class(cntrlcoxph$Release.Date)
class(cntrlcoxph$Detect.Date)
#sorting data to select the first detection on Lake receiver
sorted_cntrl<- cntrlcoxph %>% 
  group_by(Signal) %>% 
  arrange(Section) %>% 
  slice(1) %>% 
  ungroup()
#create a survival object, individuals with a (+) did not make it
surv<-Surv(sorted_cntrl$Elapsed.T, sorted_cntrl$Return)
#creating the survival curve for all groups
#(also known as Kaplan-Meier curve) displays the probability of survival over time for a group of subjects.
surv_objcntrl <- with(sorted_cntrl, Surv(Elapsed.T, Return))
surv_fitcntrl <- survfit(surv_objcntrl ~ Group, data = sorted_cntrl)
surv_dfcntrl<-fortify(surv_fitcntrl)
#survival curves using ggplot
ggplot(surv_dfcntrl, aes(x = time, y = surv, color = strata)) +
  geom_step(linewidth = 1.2) +
  labs(x = "Time", y = "Survival Probability",
       title = "Survival Curves for Multiple Groups") +
  scale_color_discrete(name = "Group") +
  theme_minimal()

#log-rank test, comparing survival distributions between different groups specified by the variable Group in the dataset sorted_cntrl.
resultcntrl <- survdiff(surv_objcntrl ~ Group, data = sorted_cntrl)
summary(resultcntrl)
print(resultcntrl)
#now running the cox propotional hazard
cox_modelcntrl<-coxph(surv_objcntrl ~ Group, data = sorted_cntrl)
summary(cox_modelcntrl)
coef_infocntrl <- summary(cox_modelcntrl)$coefficients
coef_dfcntrl <- data.frame(
  Variable = rownames(coef_infocntrl),
  Coefficient = coef_infocntrl[, "coef"],
  HR = exp(coef_infocntrl[, "coef"]),  # Hazard Ratio
  CI_lower = exp(confint(cox_modelcntrl)[, 1]),  # Lower bound of the confidence interval
  CI_upper = exp(confint(cox_modelcntrl)[, 2]),  # Upper bound of the confidence interval
  P_value = coef_info[, "Pr(>|z|)"]
)
print(coef_infocntrl)

### Coxph all ---------------------------------------------------------------
#bind dataframes
RA_df<-rbind(C1,C2,T1,T2,T3,T4,T5,T6)
#RA_df<-subset(RA_df, Signal %in% c(2464) - this line is used to remove outlier signals in df
RA_df<-RA_df %>% mutate(Season = case_when(Group == 'T1'|Group =='T2' ~ 'Early',Group =='C1'~ 'Control',
                                           Group== 'C2' ~'Control', Group== 'T3'|Group== 'T4'|
                                             Group== 'T5'|Group== 'T6' ~ 'Late'))
#merge the two controls together using mutate - all C2 fish are now with the C1
RA_df <- RA_df %>%  
  mutate(Group = fct_recode(Group,
                            "C1" = "C2")) 
unique(RA_df$Group)
#split date and time into two columns for both Detections and Release
RA_df['Detect.Date'] <-format(as.POSIXct(RA_df$Detect.Time,format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d") 
RA_df$Detect.Time <- format(as.POSIXct(RA_df$Detect.Time,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S")
#now for release
RA_df['Release.Date'] <-format(as.POSIXct(RA_df$Release.Time,format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d") 
RA_df$Release.Time <- format(as.POSIXct(RA_df$Release.Time,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S")
#reorder the columns so they make sense - Keep time for now, maybe get rid of later
RA_df<- RA_df %>% select(Release.Date,Release.Time,Signal, Group, Detect.Date, Detect.Time, Section,Season, Receiver, Transmitter, Array) %>% 
  #Add a column to calculate days between Release and detection
  mutate(
    Elapsed.T = as.duration(Release.Date %--% Detect.Date) / ddays(1)
  )
#add a column for "status" 0 = not in lake, 1 = in Lake (0=non event, 1= event)
RA_df<-RA_df %>% dplyr::mutate(Return = ifelse(Section == "Marina", "0", "1"))
RA_df$Return <- as.numeric(as.character(RA_df$Return))

### Return analysis Model ---------------------------------------------------
#select only for lake and Marina sections
RA_subset1<-subset(RA_df, Section=='Lake')
RA_subset2<-subset(RA_df, Section=='Marina')
RA_subset<-rbind(RA_subset1,RA_subset2)
RA_subset$Release.Date <-ymd(RA_subset$Release.Date)
RA_subset$Detect.Date <-ymd(RA_subset$Detect.Date)
#double check format is noted as date
class(RA_subset$Release.Date)
class(RA_subset$Detect.Date)
#sorting data to select the first detection on Lake receiver
sorted_RA<- RA_subset %>% 
  group_by(Signal) %>% 
  arrange(Section) %>% 
  slice(1) %>% 
  ungroup()
#create a survival object, individuals with a (+) did not make it
surv<-Surv(sorted_RA$Elapsed.T, sorted_RA$Return)
print(surv)
summary(sorted_RA)
#creating the survival curve for all groups
surv_obj <- with(sorted_RA, Surv(Elapsed.T, Return))
surv_fit <- survfit(surv_obj ~ Group, data = sorted_RA)
surv_df<-fortify(surv_fit)
# Coxph -------------------------------------------------------------------
#fit a cox proportional hazard model using a survival object
cox_model1<-coxph(surv_obj ~ Group, data = sorted_RA)
summary(cox_model1)
coef_info <- summary(cox_model1)$coefficients
coef_df <- data.frame(
  Variable = rownames(coef_info),
  Coefficient = coef_info[, "coef"],
  HR = exp(coef_info[, "coef"]),  # Hazard Ratio
  CI_lower = exp(confint(cox_model1)[, 1]),  # Lower bound of the confidence interval
  CI_upper = exp(confint(cox_model1)[, 2]),  # Upper bound of the confidence interval
  P_value = coef_info[, "Pr(>|z|)"]
)
coef_info
coef_df
coefinfogroup<-coef_info
coefdfgroup<-coef_df
schoenfeld_resid <- cox.zph(cox_model1)
#Same thing as above but for seasons
surv_fit2 <- survfit(surv_obj ~ Season, data = sorted_RA)
surv_df2<-fortify(surv_fit2)
#fit a cox proportional hazard model using a survival object
cox_model2<-coxph(surv_obj ~ Season, data = sorted_RA)
coef_info2 <- summary(cox_model2)$coefficients
coef_df2 <- data.frame(
  Variable = rownames(coef_info2),
  Coefficient = coef_info2[, "coef"],
  HR = exp(coef_info2[, "coef"]),  # Hazard Ratio
  CI_lower = exp(confint(cox_model2)[, 1]),  # Lower bound of the confidence interval
  CI_upper = exp(confint(cox_model2)[, 2]),  # Upper bound of the confidence interval
  P_value = coef_info2[, "Pr(>|z|)"]
)
schoenfeld_resid <- cox.zph(cox_model2)
coefinfoseason<-coef_info2
coefinfoseason
coefdfseasondf<-coef_df2
coefdfseasondf
##HR < 1: If the hazard ratio is less than 1, it suggests that the first group has a 
#lower risk of the event compared to the second group. 
#HR > 1: If the hazard ratio is greater than 1, it indicates that the first group has a higher risk
#of the event compared to the second group.
# Plots -------------------------------------------------------------------
#survival curves using ggplot
ggplot(surv_df, aes(x = time, y = surv, color = strata)) +
  geom_step(size = 1.2) +
  labs(x = "Time", y = "Probability to return to Lake",
       title = "Return analysis by Season") +
  scale_color_discrete(name = "Group") +
  theme_minimal()


# glm for return time -----------------------------------------------------
#remove those that didn't return
returntimes<-sorted_RA[!(sorted_RA$Signal %in% c(13567,13569,2398,2391)),]

interceptonly<- lmer(formula = Elapsed.T~ 1+(1|Group),
                     data    = returntimes)

anova(interceptonly)
Anova(interceptonly)
summary(interceptonly)
par(mfrow = c(2, 2))
plot(lm(interceptonly))
Model1<-lmer(formula = Elapsed.T~Season+(1|Group),
             data = returntimes,
             control = lmerControl(optimizer ='optimx', optCtrl=list(method='bobyqa')))
Model2<-lmer(formula = Elapsed.T~Group+(1|Group),
             data = returntimes,
             control = lmerControl(optimizer ='optimx', optCtrl=list(method='bobyqa')))
anova(Model1)
Anova(Model1)
Model1emmeans<-emmeans(Model1, ~ Season)
Model1contrast<- pairs(Model1emmeans)
summary(Model1contrast)
par(mfrow = c(2, 2))
plot(lm(Model1))
r_squared <- r.squaredGLMM(Model1)

anova(Model2)
Anova(Model2)
Model2emmeans<-emmeans(Model2, ~ Group)
Model2contrast<- pairs(Model2emmeans)
summary(Model2contrast)
r_squared <- r.squaredGLMM(Model2)
par(mfrow = c(2, 2))
plot(lm(Model2))
## Survival/Return plots (aesthetically pleasing) -----------------------------------------
#bar plot for successful return
returncsv <- read_csv("returncsv.csv", col_types = cols(nonreturn = col_number(), 
                                                        return = col_number()))
return<-returncsv %>% group_by(Group) %>% mutate(Percent=return/total)


returnplot<-ggplot(return,aes(y=Percent,x=Group,
                              group_by= Group))+
  geom_col(aes(fill=Group))+
  scale_fill_manual(name= "Group", values = c("#9F2B00", "#FFCD58","#D37506"))+
  #scale_x_discrete(labels=c("Surgery", "Expected","Tournament","Total"))+
  labs(x = NULL, y = "Proportion of Successful Returns",
       title = str_wrap("Late Season Tournament Fish May be Less Likely to Leave the Release Point",45))+
  annotate("text", x =1.05, y = 1.04, label = "100%", fontface = "bold", colour = "black", size = 10)+
  annotate("text", x =2.05, y = 1.04, label = "100%", fontface = "bold", colour = "black", size = 10)+
  annotate("text", x =3.05, y = .90, label = "86.7%", fontface = "bold", colour = "black", size = 10)+
  #annotate("text", x =4.08, y = 23, label = "22.03%", fontface = "bold", colour = "black", size = 10)+
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    plot.title = element_text(face = "bold", colour= "black", size = 28,hjust = .0125),
    axis.title.y = element_text(face = "bold", colour = "black", size = 20),
    axis.title.x = element_text(face = "bold", colour = "black", size = 20),
    axis.ticks.length = unit(.40,"cm"),
    axis.ticks = element_line(size = 1.00, colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black", size = 12),
    axis.line = element_line(size = 1.00, color = "black"),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    panel.grid.major.x = element_line(size = .15, colour = "#D3D3CB"),
    panel.grid.minor = element_line(size = .15, color = "#D3D3CB"),
    panel.grid.major = element_line(size = .15, color = "#D3D3CB"),
    panel.background = element_rect(fill = "#E9EAEC"))
returnplot

surv_df<-surv_df %>% mutate(Season = case_when(strata == 'T1'|strata =='T2' ~ 'Early',strata =='C1'~ 'Control',
                                               strata== 'T3'|strata== 'T4'|strata== 'T5'|strata== 'T6' ~ 'Late'))
#my_colors <- c("#FFC50D", "#5B9883", "#84D4B7","#BEE8D9","#99F2D1")
survplot2<-ggplot(surv_df, aes(x = time, y = surv, group_by= strata ,color = Season)) +
  scale_color_manual(name = "Season", values = c("#9F2B00", "#D37506","#FFCD58"))+
  geom_step(size = 2.3) +
  labs(x = "Time (Days)", y = "Proportion of Fish At Release",
       title = "Time to Return to Main Lake")+  theme(
         legend.background = element_blank(),
         legend.key = element_blank(),
         legend.text = element_text(colour = "black", size = 20),
         legend.title = element_text(face = "bold", colour = "black", size = 25),
         plot.title = element_text(face = "bold", colour= "black", size = 35,hjust = .0125),
         axis.title.y = element_text(face = "bold", colour = "black", size = 20),
         axis.title.x = element_text(face = "bold", colour = "black", size = 20),
         axis.ticks.length = unit(.50,"cm"),
         axis.ticks = element_line(size = 1.00, colour = "black"),
         axis.text.y = element_text(face = "bold", colour = "black", size = 18),
         axis.line = element_line(size = 1.00, color = "black"),
         axis.text = element_text(size = 18, face = "bold", color = "black"),
         panel.grid.major.x = element_line(size = .15, colour = "#D3D3CB"),
         panel.grid.minor = element_line(size = .15, color = "#D3D3CB"),
         panel.grid.major = element_line(size = .15, color = "#D3D3CB"),
         panel.background = element_rect(fill = "#ADA7A7"),
         panel.spacing = unit(.05, "lines"),
         panel.border = element_rect(color = "#D3D3CB", fill = "NA", linewidth =  1), 
         strip.background = element_rect(color = "#D3D3CB", fill ="#315C4F", linewidth = 1)) 
survplot2

##boxplot for days to return to lake --------------------------------------
tournamentrename<- as_labeller(c("C1" ="CONTROL","T1" ="EARLY 1","T2" ="EARLY 2","T3" ="LATE 1","T4" ="LATE 2","T5" ="LATE 3","T6" ="LATE 4"))
returnplot2<-ggplot(surv_df, aes(x = strata, y = time,group_by= strata ,color = Season)) +
  geom_boxplot(aes(fill=Season))+
  scale_fill_manual(name= "Season", values = c("#9F2B00", "#D37506","#FFCD58"))+
  scale_color_manual(name = "Season", values = c("black", "black","black"))+
  geom_point(size= 4)+
  labs(x = "Group", y = "Time (days)",
       title = "No Difference in Time to Leave Release Point")+
  theme( legend.position = "none",
         #legend.background = element_blank(),
         #legend.key = element_blank(),
         #legend.text = element_text(colour = "black", size = 20),
         #legend.title = element_text(face = "bold", colour = "black", size = 25),
         plot.background = element_blank(),
         plot.title = element_text(face = "bold", colour= "black", size = 28,hjust = .0125),
         axis.title.y = element_text(face = "bold", colour = "black", size = 20),
         axis.title.x = element_text(face = "bold", colour = "black", size = 20),
         axis.ticks.length = unit(.50,"cm"),
         axis.ticks = element_line(size = 1.00, colour = "black"),
         axis.text.y = element_text(face = "bold", colour = "black", size = 18),
         axis.line = element_line(size = 1.00, color = "black"),
         axis.text = element_text(size = 18, face = "bold", color = "black"),
         panel.grid.major.x = element_blank(),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         panel.background = element_blank())+
  scale_x_discrete(labels=tournamentrename)
returnplot2
#equally weighs observations over the entire follow up time (assumes proportional hazards)
#The test produces a chi-squared statistic, which is a measure of the difference in survival curves among the groups.
#The associated p-value indicates whether the observed differences are statistically significant.
#A small p-value suggests that there is evidence to reject the null hypothesis of no difference in survival among groups.
# Perform the log-rank test using survdiff
result <- survdiff(surv_obj ~ Season, data = sorted_RA)
summary(result)
print(result)
result$pvalue


#pull quantitative fish data from each group
summary(biometrics_RA)
sd(biometrics_RA$Length)
C1new<- C1 %>% 
  group_by(Signal) %>% 
  slice(1) %>% 
  ungroup()
summary(C1new)
C1sub<-subset(biometrics_RA, Group=='C1')
summary(C1sub)
sd(C1sub$Length)

C2new<- C2 %>% 
  group_by(Signal) %>% 
  slice(1) %>% 
  ungroup()
summary(C2new)
C2sub<-subset(biometrics_RA, Group=='C2')
summary(C2sub)
sd(C2sub$Length)

T1new<- T1 %>% 
  group_by(Signal) %>% 
  slice(1) %>% 
  ungroup()
summary(T1new)
T1sub<-subset(biometrics_RA, Group=='T1')
summary(T1sub)
sd(T1sub$Length)

T2new<- T2 %>% 
  group_by(Signal) %>% 
  slice(1) %>% 
  ungroup()
summary(T2new)
T2sub<-subset(biometrics_RA, Group=='T2')
summary(T2sub)
sd(T2sub$Length)

T3new<- T3 %>% 
  group_by(Signal) %>% 
  slice(1) %>% 
  ungroup()
summary(T3new)
T3sub<-subset(biometrics_RA, Group=='T3')
summary(T3sub)
sd(T3sub$Length)

T4new<- T4 %>% 
  group_by(Signal) %>% 
  slice(1) %>% 
  ungroup()
summary(T4new)
T4sub<-subset(biometrics_RA, Group=='T4')
summary(T4sub)
sd(T4sub$Length)

T5new<- T5 %>% 
  group_by(Signal) %>% 
  slice(1) %>% 
  ungroup()
summary(T5new)
T5sub<-subset(biometrics_RA, Group=='T5')
summary(T5sub)
sd(T5sub$Length)

T6new<- T6 %>% 
  group_by(Signal) %>% 
  slice(1) %>% 
  ungroup()
summary(T6new)
T6sub<-subset(biometrics_RA, Group=='T6')
summary(T6sub)
sd(T6sub$Length)
# Create a gt table
coefdfseasonoutlierdf<-coefdfseasonoutlierdf %>% rename(Season="Variable")
coefdfseasonoutlierdf<-coefdfseasonoutlierdf%>% mutate(Season = case_when(Season == 'SeasonEarly'~ 'Early', Season =='SeasonLate' ~ 'Late'))
table <- coefdfseasonoutlierdf %>%
  gt() %>%
  tab_spanner(
    label = "Group",
    columns = c(Season)
  ) %>%
  tab_spanner(
    label = "Summary Statistics",
    columns = c(Coefficient,HR,CI_lower,CI_upper,P_value)
  ) %>%
  fmt_number(
    columns = c(Coefficient,CI_lower,CI_upper,P_value, HR),
    decimals = 3
  )
table
#save table
table %>%
  gtsave("table.png") 
# RESIDENCY ---------------------------------------------------------------
# data wrangling ----------------------------------------------------------
#status df includes number of valid detectionss
resdf=status.df %>% select(Transmitter, Release.date,Signal, Group, Length, Weight,Valid.detections)
resdf$Signal<-as.character(resdf$Signal)
resdf<-merge(tag.detections,resdf,by= intersect(x = "Signal", y= "Signal"))
resdf=resdf %>% select(Transmitter.x, Release.date,Signal, Group, Length, Weight,Valid.detections, Array,Valid, Timestamp, Receiver,Section,Sensor.Value)
#Add elapsed time from release
resdf=resdf %>% mutate(
  Elapsed.T = as.duration(Release.date %--% Timestamp) / ddays(1))
#trim anything past 50 days
resdf<-resdf %>% filter(Elapsed.T < 50)
#Fix total detections to match timeframe
resdf<-resdf %>% add_count(Signal,Valid)
resdf=resdf %>% rename(olddet=Valid.detections,T.valid.det="n")
#create a column summing all valid detections within a group
resdf<-resdf %>% add_count(Group,Valid)
#add column for valid detections by group at each array
resdf<-resdf %>% add_count(Array,Group,Valid)
#do it for section too because why not
resdf<-resdf %>% add_count(Section,Group,Valid)
#for individual tags
resdf<-resdf %>% add_count(Array,Signal,Valid)
resdf<-resdf %>% add_count(Section,Signal,Valid)
#check to make sure the numbers are right
resdf=resdf %>% rename(total.by.group="n", total.by.array="nn",
                       total.by.section="nnn",signal.by.array="nnnn", signal.by.section ="nnnnn")
#column to determine proportion of time spent in each array over total detections
resdf<-resdf %>% group_by(Group) %>% mutate(Percent=total.by.array/total.by.group)
resdf=resdf %>% rename(group.ratios.array="Percent")
resdf<-resdf %>% group_by(Group) %>% mutate(Percent=total.by.section/total.by.group)
resdf=resdf %>% rename(group.ratios.section ="Percent")
resdf<-resdf %>% group_by(Signal) %>% mutate(Percent=signal.by.array/T.valid.det)
resdf=resdf %>% rename(indv.ratios.array="Percent")
resdf<-resdf %>% group_by(Signal) %>% mutate(Percent=signal.by.section/T.valid.det)
resdf=resdf %>% rename(indv.ratios.sections ="Percent")

#reorder things because it got a little crazy up there
resdf=resdf %>% select(Release.date,Signal,signal.by.array, signal.by.section,
                       Group, T.valid.det,indv.ratios.sections,indv.ratios.array, 
                       Array, Timestamp, Receiver,Section,total.by.group, 
                       total.by.array,group.ratios.array,
                       total.by.section,group.ratios.section,Elapsed.T,Valid,Sensor.Value)

# 7 day -------------------------------------------------------------------
#this code is to determine array ratios within the timebins
res7day<-resdf %>% select(Signal,Section,Valid, Group,Receiver, Array, Elapsed.T,Sensor.Value)
#trim dataframe for only detections before 7 days
res7day<-res7day %>% filter(Elapsed.T < 7.01) %>% 
  mutate(Bin = factor("7"),Season = case_when(Group == 'T1'|Group =='T2' ~ 'Early',Group =='C1'~ 'EarlyControl',
                                              Group== 'C2' ~'LateControl', Group== 'T3'|Group== 'T4'|
                                                Group== 'T5'|Group== 'T6' ~ 'Late'))
res7day$Elapsed.T<-as.integer(res7day$Elapsed.T)
#Fix total detections to match timeframe
res7day<-res7day %>% add_count(Signal,Valid)
res7day=res7day %>% rename(valid.det.7 ="n")
#create a column summing all valid detections within a group
res7day<-res7day %>% add_count(Group,Valid)
#add column for valid detections by group at each array
res7day<-res7day %>% add_count(Array,Group,Valid)
#do it for section too because why not
res7day<-res7day %>% add_count(Section,Group,Valid)
#for individual tags
res7day<-res7day %>% add_count(Array,Signal,Valid)
res7day<-res7day %>% add_count(Section,Signal,Valid)
res7day=res7day %>% rename(total7.by.group="n", total7.by.array="nn",
                           total7.by.section="nnn",signal7.by.array="nnnn", signal7.by.section ="nnnnn")
#column to determine proportion of time spent in each array over total detections
res7day<-res7day %>% group_by(Group) %>% mutate(Percent=total7.by.array/total7.by.group)
res7day=res7day %>% rename(group.ratios.array="Percent")
res7day<-res7day %>% group_by(Group) %>% mutate(Percent=total7.by.section/total7.by.group)
res7day=res7day %>% rename(group.ratios.section ="Percent")
res7day<-res7day %>% group_by(Signal) %>% mutate(Percent=signal7.by.array/valid.det.7)
res7day=res7day %>% rename(indv.ratios.array="Percent")
res7day<-res7day %>% group_by(Signal) %>% mutate(Percent=signal7.by.section/valid.det.7)
res7day=res7day %>% rename(indv.ratios.sections ="Percent")

#select necessary columns (just proportions and identifying info)
res7day<-res7day %>% select(Signal,Group,Elapsed.T,Section,Array,indv.ratios.sections,
                            indv.ratios.array,group.ratios.section,group.ratios.array,Bin,Sensor.Value,Season)
res7day<- res7day %>%
  group_by(Signal,Array) %>% 
  slice(1:2) 

# 14day -------------------------------------------------------------------

res14day<-resdf %>% select(Signal,Section,Valid, Group,Receiver, Array, Elapsed.T,Sensor.Value)
#trim dataframe for only detections before 14 days
res14day<-res14day %>% filter(between(Elapsed.T, 7,14)) %>% 
  mutate(Bin = factor("14"),Season = case_when(Group == 'T1'|Group =='T2' ~ 'Early',Group =='C1'~ 'EarlyControl',
                                               Group== 'C2' ~'LateControl', Group== 'T3'|Group== 'T4'|
                                                 Group== 'T5'|Group== 'T6' ~ 'Late'))

#Fix total detections to match timeframe
res14day<-res14day %>% add_count(Signal,Valid)
res14day=res14day %>% rename(valid.det.14 ="n")
#create a column summing all valid detections within a group
res14day<-res14day %>% add_count(Group,Valid)
#add column for valid detections by group at each array
res14day<-res14day %>% add_count(Array,Group,Valid)
#do it for section too because why not
res14day<-res14day %>% add_count(Section,Group,Valid)
#for individual tags
res14day<-res14day %>% add_count(Array,Signal,Valid)
res14day<-res14day %>% add_count(Section,Signal,Valid)
res14day=res14day %>% rename(total14.by.group="n", total14.by.array="nn",
                             total14.by.section="nnn",signal14.by.array="nnnn", signal14.by.section ="nnnnn")
#column to determine proportion of time spent in each array over total detections
res14day<-res14day %>% group_by(Group) %>% mutate(Percent=total14.by.array/total14.by.group)
res14day=res14day %>% rename(group.ratios.array="Percent")
res14day<-res14day %>% group_by(Group) %>% mutate(Percent=total14.by.section/total14.by.group)
res14day=res14day %>% rename(group.ratios.section ="Percent")
res14day<-res14day %>% group_by(Signal) %>% mutate(Percent=signal14.by.array/valid.det.14)
res14day=res14day %>% rename(indv.ratios.array="Percent")
res14day<-res14day %>% group_by(Signal) %>% mutate(Percent=signal14.by.section/valid.det.14)
res14day=res14day %>% rename(indv.ratios.sections ="Percent")
res14day<-res14day %>% select(Signal,Elapsed.T,Group,Section,Array,indv.ratios.sections,
                              indv.ratios.array,group.ratios.section,group.ratios.array,Bin,Sensor.Value,Season)
res14day<- res14day %>%
  group_by(Signal,Array) %>% 
  slice(1:2) 

# 25 day -------------------------------------------------------------------

res25day<-resdf %>% select(Signal,Section,Valid, Group,Receiver, Array, Elapsed.T,Sensor.Value)
#trim dataframe for only detections before 14 days
res25day<-res25day %>% filter(between(Elapsed.T, 14,25)) %>% 
  mutate(Bin = factor("25"),Season = case_when(Group == 'T1'|Group =='T2' ~ 'Early',Group =='C1'~ 'EarlyControl',
                                               Group== 'C2' ~'LateControl', Group== 'T3'|Group== 'T4'|
                                                 Group== 'T5'|Group== 'T6' ~ 'Late'))
#Fix total detections to match timeframe
res25day<-res25day %>% add_count(Signal,Valid)
res25day=res25day %>% rename(valid.det.30 ="n")
#create a column summing all valid detections within a group
res25day<-res25day %>% add_count(Group,Valid)
#add column for valid detections by group at each array
res25day<-res25day %>% add_count(Array,Group,Valid)
#do it for section too because why not
res25day<-res25day %>% add_count(Section,Group,Valid)
#for individual tags
res25day<-res25day %>% add_count(Array,Signal,Valid)
res25day<-res25day %>% add_count(Section,Signal,Valid)
res25day=res25day %>% rename(total30.by.group="n", total30.by.array="nn",
                             total30.by.section="nnn",signal30.by.array="nnnn", signal30.by.section ="nnnnn")
#column to determine proportion of time spent in each array over total detections
res25day<-res25day %>% group_by(Group) %>% mutate(Percent=total30.by.array/total30.by.group)
res25day=res25day %>% rename(group.ratios.array="Percent")
res25day<-res25day %>% group_by(Group) %>% mutate(Percent=total30.by.section/total30.by.group)
res25day=res25day %>% rename(group.ratios.section ="Percent")
res25day<-res25day %>% group_by(Signal) %>% mutate(Percent=signal30.by.array/valid.det.30)
res25day=res25day %>% rename(indv.ratios.array="Percent")
res25day<-res25day %>% group_by(Signal) %>% mutate(Percent=signal30.by.section/valid.det.30)
res25day=res25day %>% rename(indv.ratios.sections ="Percent")
res25day<-res25day %>% select(Signal,Elapsed.T,Group,Section,Array,indv.ratios.sections,indv.ratios.array,
                              group.ratios.section,group.ratios.array,Bin,Sensor.Value,Season)
res25day<- res25day %>%
  group_by(Signal,Array) %>% 
  slice(1:2) 


# 40 day -------------------------------------------------------------------

res40day<-resdf %>% select(Signal,Section,Valid, Group,Receiver, Array, Elapsed.T,Sensor.Value)
#trim dataframe for only detections before 14 days
res40day<-res40day %>% filter(between(Elapsed.T, 25,40))%>% 
  mutate(Bin = factor("40"),Season = case_when(Group == 'T1'|Group =='T2' ~ 'Early',Group =='C1'~ 'EarlyControl',
                                               Group== 'C2' ~'LateControl', Group== 'T3'|Group== 'T4'|
                                                 Group== 'T5'|Group== 'T6' ~ 'Late'))

#Fix total detections to match timeframe
res40day<-res40day %>% add_count(Signal,Valid)
res40day=res40day %>% rename(valid.det.45 ="n")
#create a column summing all valid detections within a group
res40day<-res40day %>% add_count(Group,Valid)
#add column for valid detections by group at each array
res40day<-res40day %>% add_count(Array,Group,Valid)
#do it for section too because why not
res40day<-res40day %>% add_count(Section,Group,Valid)
#for individual tags
res40day<-res40day %>% add_count(Array,Signal,Valid)
res40day<-res40day %>% add_count(Section,Signal,Valid)
res40day=res40day %>% rename(total45.by.group="n", total45.by.array="nn",
                             total45.by.section="nnn",signal45.by.array="nnnn", signal45.by.section ="nnnnn")
#column to determine proportion of time spent in each array over total detections
res40day<-res40day %>% group_by(Group) %>% mutate(Percent=total45.by.array/total45.by.group)
res40day=res40day %>% rename(group.ratios.array="Percent")
res40day<-res40day %>% group_by(Group) %>% mutate(Percent=total45.by.section/total45.by.group)
res40day=res40day %>% rename(group.ratios.section ="Percent")
res40day<-res40day %>% group_by(Signal) %>% mutate(Percent=signal45.by.array/valid.det.45)
res40day=res40day %>% rename(indv.ratios.array="Percent")
res40day<-res40day %>% group_by(Signal) %>% mutate(Percent=signal45.by.section/valid.det.45)
res40day=res40day %>% rename(indv.ratios.sections ="Percent")
res40day<-res40day %>% select(Signal,Elapsed.T,Group,Section,Array,indv.ratios.sections,
                              indv.ratios.array,group.ratios.section,group.ratios.array,Bin,Sensor.Value,Season)
res40day<- res40day %>%
  group_by(Signal,Array) %>% 
  slice(1:2) 


# 50 day -------------------------------------------------------------------

res50day<-resdf %>% select(Signal,Section,Valid, Group,Receiver, Array, Elapsed.T,Sensor.Value)
#trim dataframe for only detections before 14 days
res50day<-res50day %>% filter(between(Elapsed.T, 40,50)) %>% 
  mutate(Bin = factor("50"),Season = case_when(Group == 'T1'|Group =='T2' ~ 'Early',Group =='C1'~ 'EarlyControl',
                                               Group== 'C2' ~'LateControl', Group== 'T3'|Group== 'T4'|
                                                 Group== 'T5'|Group== 'T6' ~ 'Late'))

#Fix total detections to match timeframe
res50day<-res50day %>% add_count(Signal,Valid)
res50day=res50day %>% rename(valid.det.60 ="n")
#create a column summing all valid detections within a group
res50day<-res50day %>% add_count(Group,Valid)
#add column for valid detections by group at each array
res50day<-res50day %>% add_count(Array,Group,Valid)
#do it for section too because why not
res50day<-res50day %>% add_count(Section,Group,Valid)
#for individual tags
res50day<-res50day %>% add_count(Array,Signal,Valid)
res50day<-res50day %>% add_count(Section,Signal,Valid)
res50day=res50day %>% rename(total60.by.group="n", total60.by.array="nn",
                             total60.by.section="nnn",signal60.by.array="nnnn", signal60.by.section ="nnnnn")
#column to determine proportion of time spent in each array over total detections
res50day<-res50day %>% group_by(Group) %>% mutate(Percent=total60.by.array/total60.by.group)
res50day=res50day %>% rename(group.ratios.array="Percent")
res50day<-res50day %>% group_by(Group) %>% mutate(Percent=total60.by.section/total60.by.group)
res50day=res50day %>% rename(group.ratios.section ="Percent")
res50day<-res50day %>% group_by(Signal) %>% mutate(Percent=signal60.by.array/valid.det.60)
res50day=res50day %>% rename(indv.ratios.array="Percent")
res50day<-res50day %>% group_by(Signal) %>% mutate(Percent=signal60.by.section/valid.det.60)
res50day=res50day %>% rename(indv.ratios.sections ="Percent")
res50day<-res50day %>% select(Signal,Elapsed.T,Group,Section,Array,indv.ratios.sections,
                              indv.ratios.array,group.ratios.section,group.ratios.array,Bin,Sensor.Value,Season)
res50day<- res50day %>%
  group_by(Signal,Array) %>% 
  slice(1:2) 

#merge these? I think
binnedresdf<-rbind(res7day,res14day,res25day,res40day,res50day)
#remove controls
binnedresdf<-binnedresdf[!(binnedresdf$Group %in% c("C1","C2")),]
res7day<-res7day[!(res7day$Group %in% c("C1","C2")),]
# plot --------------------------------------------------------------------
#This is the winner plot
binnedresrename<-binnedresdf %>% mutate(Array = case_when(Array == 'C_1'~ 'Cold 1', Array =='C_2' ~ 'Cold 2', Array =='C_3'~ 'Cold 3',
                                                          Array== 'W_1' ~'Warm 1', Array== 'W_3'~ 'Warm 3', Array== 'M_1'~ 'Release',Array== 'W_4'~ 'Warm 4',))
resplot1<-  ggplot(binnedresrename, aes(x=Bin,
                                        y= indv.ratios.array,
                                        group_by=Array)) +
  geom_boxplot(aes(fill=Array))+
  facet_grid(Season~.)+
  scale_fill_manual(name= "Zone", values = c("#9F2B00", "#FFCD58","#5C5847","#4C5EA9","#D37506","#5A0001","#29335C"))+
  scale_color_manual(name = "Array", values = c("black", "black","black", "black","black", "black","black"))+
  labs(x = "Days Since Release", y =  str_wrap("Proportional Time (Detections at array/all detections)",28),
       title = str_wrap("Differences in Occupancy are in the First 7 Days Following Release",38))+
  theme(
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 15),
    legend.title = element_text(face = "bold", colour = "black", size = 18),
    plot.background = element_blank(),
    plot.title = element_text(face = "bold", colour= "black", size = 35,hjust = .0125),
    axis.title.y = element_text(face = "bold", colour = "black", size = 20),
    axis.title.x = element_text(face = "bold", colour = "black", size = 20),
    axis.ticks.length = unit(.50,"cm"),
    axis.ticks = element_line(size = 1.00, colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black", size = 15),
    axis.line = element_line(size = 1.00, color = "black"),
    axis.text = element_text(size = 15, face = "bold", color = "black"),
    panel.grid.major.x = element_line(size = .15, colour = "#D3D3CB"),
    panel.grid.minor = element_line(size = .15, color = "#D3D3CB"),
    panel.grid.major = element_line(size = .15, color = "#D3D3CB"),
    panel.background = element_rect(fill = "#E9EAEC"),
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(color = "black", fill = "NA", linewidth =  1), 
    strip.background = element_rect(color = "black", fill ="#B8BBC1", linewidth = 1),
    strip.text = element_text(color="black",size = 15))+
  stat_summary(
    fun = mean, geom = "point", shape = 5, size = 1.25, stroke = 2, na.rm = TRUE,
    color = "black", show.legend = FALSE
  )
resplot1

#alternative resplot
resplot1<-  ggplot(binnedresdf, aes(x=Array,
                                    y= indv.ratios.array,
                                    group_by=Bin)) +
  geom_boxplot(aes(fill=Season))+
  facet_grid(Bin~.)+
  labs(x = "Bin", y = "Proportional Time (Detections at array/all detections)",
       title = "Seasonal Occupancy ratios at each Array")+
  theme(
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 20),
    legend.title = element_text(face = "bold", colour = "black", size = 25),
    plot.background = element_blank(),
    plot.title = element_text(face = "bold", colour= "black", size = 38,hjust = .0125),
    axis.title.y = element_text(face = "bold", colour = "black", size = 20),
    axis.title.x = element_text(face = "bold", colour = "black", size = 20),
    axis.ticks.length = unit(1.00,"cm"),
    axis.ticks = element_line(size = 1.00, colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black", size = 18),
    axis.line = element_line(size = 1.00, color = "black"),
    axis.text = element_text(size = 18, face = "bold", color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank())
resplot1

#plot with only bin 7 
res7dayrename<-binnedresrename%>% filter(Elapsed.T < 7.01)
res7dayplot<-ggplot(res7dayrename, aes(x=Array,
                                       y= indv.ratios.array,
                                       group_by=Season)) +
  geom_boxplot(aes(fill=Season))+
  labs(x = "Zone", y = str_wrap("Proportional Time (Detections at array/all detections)",28),
       title = str_wrap("Late Season Fish are Spending More Time in the Marina Than Early Season Fish",40))+
  scale_fill_manual(name= "Season", values = c("#9F2B00", "#FFCD58"))+
  scale_color_manual(name = "Season", values = c("black", "black"))+
  theme(
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 15),
    legend.title = element_text(face = "bold", colour = "black", size = 20),
    plot.background = element_blank(),
    plot.title = element_text(face = "bold", colour= "black", size = ,hjust = .0125),
    axis.title.y = element_text(face = "bold", colour = "black", size = 20),
    axis.title.x = element_text(face = "bold", colour = "black", size = 20),
    axis.ticks.length = unit(.50,"cm"),
    axis.ticks = element_line(size = 1.00, colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black", size = 12),
    axis.line = element_line(size = 1.00, color = "black"),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    panel.grid.major.x = element_line(size = .15, colour = "#D3D3CB"),
    panel.grid.minor = element_line(size = .15, color = "#D3D3CB"),
    panel.grid.major = element_line(size = .15, color = "#D3D3CB"),
    panel.background = element_rect(fill = "#E9EAEC"))+
  stat_summary(
    fun = mean, geom = "point", shape = 5, size = 1.25, stroke = 2, na.rm = TRUE,
    color = "black", show.legend = TRUE
  ) 
res7dayplot


# Figures and Tables for Papers -------------------------------------------
####Mosaic chart########################
resdfcountplot<-resdfcount %>% mutate(Array = case_when(Array == 'C_1'~ 'Lake 1', Array =='C_2' ~ 'Dam',
                                                        Array =='C_3'~ 'Intake',
                                                        Array== 'W_1' ~'Lake 2', Array== 'W_3'~ 'Bridge', 
                                                        Array== 'M_1'~ 'Release',Array== 'W_4'~ 'Outflow',))
resdfcountplot <- resdfcountplot %>% group_by(Season,Array)
resdfcountplot$Array <- factor(resdfcountplot$Array, levels=c("Release","Lake 1" ,"Lake 2","Dam" ,
                                                              "Bridge","Intake" ,"Outflow")) 

ggplot(resdfcountplot, aes(x = Array, y = Percent, fill = Season)) +
  geom_col(position = "fill") +
  labs(x = "Receiver Location", y = "Time Spent (days) Since Release")+
  theme(
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text( colour = "#413F31", size = 18),
    legend.text = element_text( colour = "#413F31", size = 18),
    plot.title = element_text( colour= "#413F31", size = 28,hjust = .0125),
    axis.title.y = element_text( colour = "#413F31", size = 18),
    axis.title.x = element_text( colour = "#413F31", size = 18),
    axis.ticks = element_blank(),
    axis.text.y = element_text( colour = "#413F31", size = 15),
    axis.line = element_line(size = 1.00, color = "#413F31"),
    axis.text = element_text(angle = 35,vjust = 0.5, hjust=1,size = 15,  color = "#413F31"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank())+
  scale_fill_manual(name= "Season", values = c("#413F31", "#C5C5C5","#999578","#525252"))+
  scale_y_continuous(labels = scales::percent)

ggsave("mosaicspatial2.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 11,
       height = 8,
       units = c("in"),
       dpi = 2000,
       limitsize = FALSE,
       bg = NULL)

# models ---------------------------------------

interceptonlymodel <- glmer(formula = indv.ratios.array ~ 1 + (1|Signal),
                            data    = binnedresdf,family = binomial, control = glmerControl(optimizer = "bobyqa"),
                            nAGQ = 10)
#all signals by season
Seasonaloccupancy<- glmer(formula = indv.ratios.array ~ Bin*Season + (1|Season:Signal),
                          data    = binnedresdf,family = binomial, control = glmerControl(optimizer = "bobyqa"),
                          nAGQ = 10)
summary(Seasonaloccupancy)
anova(Seasonaloccupancy)
Anova(Seasonaloccupancy)
Seasonaloccupancycontrast <-emmeans(Seasonaloccupancy, ~ Bin:Season)
pwpp(Seasonaloccupancycontrast)
Seasonaloccupancycontrast<-pairs(Seasonaloccupancycontrast)
Seasonaloccupancycontrast
r_squared <- r.squaredGLMM(Seasonaloccupancy)
par(mfrow = c(2, 2))
plot(lm(Seasonaloccupancy))

write.xlsx(Seasonaloccupancycontrast, 'C:/Users/YOUR_WD_HERE') 

###7 Day occupancy model
#trim receiver arrays to only have bin 7
W_1<-W_1%>% filter(Bin=="7")
C_1<-C_1%>% filter(Bin=="7")
M_1<-M_1%>% filter(Bin=="7")
C_2<-C_2%>% filter(Bin=="7")
W_2<-w_2%>% filter(Bin=="7")
W_3<-W_3%>% filter(Bin=="7")
C_3<-C_3%>% filter(Bin=="7")
W_4<-W_4%>% filter(Bin=="7")
res7dayoccupancy <- glmer(formula = indv.ratios.array ~ Season + (1|Season:Signal),
                          data    = M_1, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                          nAGQ = 10)

summary(res7dayoccupancy)
anova(res7dayoccupancy)
Anova(res7dayoccupancy)
res7dayoccupancycontrast <-emmeans(res7dayoccupancy, ~ Season)
pwpp(res7dayoccupancycontrast)
res7dayoccupancycontrast<-pairs(res7dayoccupancycontrast)
res7dayoccupancycontrast
r_squared <- r.squaredGLMM(Seasonaloccupancy)
par(mfrow = c(2, 2))
plot(lm(Seasonaloccupancy))
#plot to see the difference
M_1plot<-ggplot(M_1,aes(x=Season,
                        y=indv.ratios.array,
                        group_by= Season))+
  geom_boxplot()
M_1plot
write.xlsx(res7dayoccupancycontrast, "C:/Users/Allison/Box/Hay_Allison/Data/analyses/Actel/Clinton Code") 
# TEMPERATURE -------------------------------------------------------------
hourlytemp=status.df %>% select(Transmitter, Release.date,Signal, Group, Length, Weight,Valid.detections)
hourlytemp$Signal<-as.character(hourlytemp$Signal)
hourlytemp<-merge(tag.detections,hourlytemp,by= intersect(x = "Signal", y= "Signal"))
hourlytemp=hourlytemp %>% select(Transmitter.x, Release.date,Signal, Group, Length,
                                 Weight,Valid.detections, Array,Valid, Timestamp, Receiver,Section,Sensor.Value)
#Add elapsed time from release
hourlytemp=hourlytemp %>% mutate(
  Elapsed.T = as.duration(Release.date %--% Timestamp) / ddays(1))
#convert decimal days to hours, make this plot by hours 
hourlytemp<-hourlytemp %>% mutate(Elapsed.T=Elapsed.T*24)
#trim anything past 50 days
hourlytemp<-hourlytemp %>% filter(Elapsed.T < 2)
hourlytemp<- dplyr::filter(hourlytemp,  !is.na(Sensor.Value))
#attempt to create a df that include change in temperature from start temp as a column
hourlytempfirst<- hourlytemp %>% 
  group_by(Signal) %>% 
  arrange(Timestamp) %>% 
  slice(1)
hourlytempfirst<-hourlytempfirst %>% rename(start.temp = "Sensor.Value")
hourlytempfirst<-hourlytempfirst %>% select(start.temp,Signal)
tempselectdf<-merge(hourlytemp,hourlytempfirst,by= intersect(x = "Signal", y= "Signal"))
tempselectdf<-tempselectdf %>% mutate(difference=start.temp-Sensor.Value )
#bins!
tempselectdf <- tempselectdf %>% mutate(bin = case_when(Elapsed.T <= .25 ~ '.25',
                                                        Elapsed.T > .25  & Elapsed.T <= .5 ~ '.5',
                                                        Elapsed.T > .5  & Elapsed.T <= .75 ~ '.75',
                                                        Elapsed.T > .75  & Elapsed.T <= 1 ~ '1',
                                                        Elapsed.T > 1  & Elapsed.T <= 1.25 ~ '1.25',
                                                        Elapsed.T > 1.25  & Elapsed.T <= 1.5 ~ '1.5',
                                                        Elapsed.T > 1.5  & Elapsed.T <= 1.75 ~ '1.75',
                                                        Elapsed.T > 1.75  & Elapsed.T <= 2 ~ '2',
                                                        Elapsed.T > 2  & Elapsed.T <= 2.25 ~ '2.25',
                                                        Elapsed.T > 2.25  & Elapsed.T <= 2.50 ~ '2.5',
                                                        Elapsed.T > 2.50  & Elapsed.T <= 2.75 ~ '2.75',
                                                        Elapsed.T > 2.75  & Elapsed.T <= 3 ~ '3',
                                                        Elapsed.T > 3  & Elapsed.T <= 3.25 ~ '3.25',
                                                        Elapsed.T > 3.25  & Elapsed.T <= 3.5 ~ '3.5',
                                                        Elapsed.T > 3.5  & Elapsed.T <= 3.75 ~ '3.75',
                                                        Elapsed.T > 3.75  & Elapsed.T <= 4 ~ '4',
                                                        Elapsed.T > 4  & Elapsed.T <= 4.25 ~ '4.25',
                                                        Elapsed.T > 4.25  & Elapsed.T <= 4.5 ~ '4.5',
                                                        Elapsed.T > 4.5  & Elapsed.T <= 4.75 ~ '4.75',
                                                        Elapsed.T > 4.75  & Elapsed.T <= 5 ~ '5')) # end function
tempselectdf<-tempselectdf %>% mutate(Season = case_when(Group == 'T1'|Group =='T2' ~ 'Early',Group =='C1'~ 'EarlyControl',
                                                         Group== 'C2' ~'LateControl', Group== 'T3'|Group== 'T4'|
                                                           Group== 'T5'|Group== 'T6' ~ 'Late')) %>% 
  rename(Date = 'Release.date')
tempselectdf$Date <- format(as.POSIXct(tempselectdf$Date,format="%m/%d/%Y %H:%M:%S"),"%Y-%m-%d")

#remove non-paired controls
tempselectdfcontrol<-tempselectdf[!(tempselectdf$Group %in% c("T1","T2","T3","T4","T5","T6")),]
#combine controls
#import marina temperatue
MarinaTemp<- read_csv("Marina 2023-11-22 15_52_59 CST (Data CST).csv", 
                      col_types = cols(`Date-Time (CST/CDT)` = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
                                       `Ch:1 - Temperature   (°C)` = col_number())) %>% 
  rename(Date = 'Date-Time (CST/CDT)',
         Temperature = `Ch:1 - Temperature   (°C)`)
MarinaTemp['Time'] <-format(as.POSIXct(MarinaTemp$Date,format="%m/%d/%Y %H:%M:%S"),"%H:%M:%S") 
MarinaTemp$Date <- format(as.POSIXct(MarinaTemp$Date,format="%m/%d/%Y %H:%M:%S"),"%Y-%m-%d")
#assign time as a time
MarinaTemp$Time <- chron(times. = (MarinaTemp$Time))
summary(MarinaTemp)
#select for dates tournaments occurred
MarinaTemp<-MarinaTemp%>% filter(Date == "2023-05-06"|Date=="2023-05-11"|Date == "2023-06-10"|Date=="2023-06-25"|
                                   Date == "2023-07-08"|Date=="2023-09-28"|Date == "2023-09-30")
MarinaTemp<-MarinaTemp %>%
  group_by(Date) %>%
  summarize(avg_daily_temp= mean(Temperature, na.rm = TRUE))
tempselectdf<- merge(MarinaTemp, tempselectdf, by= intersect(x = "Date", y= "Date"))
# plots -------------------------------------------------------------------
#plot to investigate
tempplot1<-ggplot(hourlytemp,aes(x=Elapsed.T,
                                 y=Sensor.Value,
                                 group_by=Group))+
  geom_point(aes(fill=Group), position = "jitter")+
  facet_wrap(Group~.)
tempplot1

tempplot2<-ggplot(tempselectdf,aes(x=Elapsed.T,
                                   y=Sensor.Value,
                                   colour=Group))+
  scale_fill_manual(name= "Group", values = c("#9F2B00", "#FFCD58","#5C5847","#4C5EA9","#D37506","#5A0001","#29335C","black"))+
  geom_point(position = "jitter")+
  
  facet_wrap(Season~.)+
  labs(x = "Elapsed Time (Hours)", y = "Temperature (C)",
       title = "Temperature Following Release")+
  theme(
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 15),
    legend.title = element_text(face = "bold", colour = "black", size = 20),
    plot.background = element_blank(),
    plot.title = element_text(face = "bold", colour= "black", size = 35,hjust = .0125),
    axis.title.y = element_text(face = "bold", colour = "black", size = 20),
    axis.title.x = element_text(face = "bold", colour = "black", size = 20),
    axis.ticks.length = unit(.50,"cm"),
    axis.ticks = element_line(size = 1.00, colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black", size = 15),
    axis.line = element_line(size = 1.00, color = "black"),
    axis.text = element_text(size = 15, face = "bold", color = "black"),
    panel.grid.major.x = element_line(size = .15, colour = "#D3D3CB"),
    panel.grid.minor = element_line(size = .15, color = "#D3D3CB"),
    panel.grid.major = element_line(size = .15, color = "#D3D3CB"),
    panel.background = element_rect(fill = "#E9EAEC"),
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(color = "black", fill = "NA", linewidth =  1),
    strip.background = element_rect(color = "black", fill ="#B8BBC1", linewidth = 1),
    strip.text = element_text(color="black",size = 15))
tempplot2

#change group names
tempselectdf<-tempselectdf%>% mutate(Group = case_when(Group == 'C1'~ 'Control (5/11/2023)', Group =='C2' ~ 'Control (9/28/2023)',
                                                       Group =='T1'~ 'Early1 (4/23/2023)',Group== 'T2' ~'Early2 (5/06/2023)', Group== 'T3'~ 'Late1 (6/10/2023)',
                                                       Group== 'T4'~ 'Late2 (6/25/2023)',Group== 'T5'~ 'Late3 (7/8/2023)', Group== 'T6'~ 'Late4 (9/30/2023)'))

tempplotall<-ggplot(tempselectdf,aes(x=bin,
                                     y=difference,
                                     group_by=Season))+
  geom_boxplot(aes(fill=Season))+
  scale_fill_manual(name= "Season", values = c("#FFCD58" ,"#9F2B00","#5C5828","#D37506","#4C5EA9","#5A0001","#D3D3CB", "pink"))+
  #facet_grid(Season~.)+
  labs(x = "Time Since Release (hrs)", y = "Degrees Changed from Release Temperature",
       title = str_wrap("All Fish, Regardless of Treatment or Season, Exhibit Temperature Variation Following Release",47))+
  #subtitle = "Early Season Fish Temperature Variation is Significantly Different from Early Control Fish")+
  theme(
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 15),
    legend.title = element_text(face = "bold", colour = "black", size = 18),
    plot.background = element_blank(),
    plot.title = element_text(face = "bold", colour= "black", size = 28,hjust = .0125),
    plot.subtitle = element_text(face = "bold", colour = "black", size =15),
    axis.title.y = element_text(face = "bold", colour = "black", size = 20),
    axis.title.x = element_text(face = "bold", colour = "black", size = 20),
    axis.ticks.length = unit(.50,"cm"),
    axis.ticks = element_line(size = 1.00, colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black", size = 15),
    axis.line = element_line(size = 1.00, color = "black"),
    axis.text = element_text(size = 15, face = "bold", color = "black"),
    panel.grid.major.x = element_line(size = .15, colour = "#D3D3CB"),
    panel.grid.minor = element_line(size = .15, color = "#D3D3CB"),
    panel.grid.major = element_line(size = .15, color = "#D3D3CB"),
    panel.background = element_rect(fill = "#E9EAEC"),
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(color = "black", fill = "NA", linewidth =  1), 
    strip.background = element_rect(color = "black", fill ="#B8BBC1", linewidth = 1),
    strip.text = element_text(color="black",size = 15)) 
tempplotall
#subset for two plots
tempplotearlydf<-subset(tempselectdf, Season=='Early'| Season=='EarlyControl')
tempplotlatedf<-subset(tempselectdf, Season=='Late' | Season=='LateControl')
tempplotearly<-ggplot(tempplotearlydf,aes(x=bin,
                                          y=difference,
                                          group_by=Group))+
  geom_boxplot(aes(fill=Group))+
  scale_fill_manual(name= "Group", values = c("#FFCD58" ,"#9F2B00","#5C5828","#D37506","#4C5EA9","#5A0001","#D3D3CB", "#E8C45E"))+
  #facet_grid(Season~.)+
  labs(x = "Time Since Release (hrs)", y = "Degrees Changed from Release Temperature",
       title = str_wrap("Early Season Fish Temperature
                        Variation is Significantly Different from Early Control Fish",48))+
  theme(
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 15),
    legend.title = element_text(face = "bold", colour = "black", size = 20),
    plot.background = element_blank(),
    plot.title = element_text(face = "bold", colour= "black", size = 25,hjust = .0125),
    axis.title.y = element_text(face = "bold", colour = "black", size = 20),
    axis.title.x = element_text(face = "bold", colour = "black", size = 20),
    axis.ticks.length = unit(.50,"cm"),
    axis.ticks = element_line(size = 1.00, colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black", size = 15),
    axis.line = element_line(size = 1.00, color = "black"),
    axis.text = element_text(size = 15, face = "bold", color = "black"),
    panel.grid.major.x = element_line(size = .15, colour = "#D3D3CB"),
    panel.grid.minor = element_line(size = .15, color = "#D3D3CB"),
    panel.grid.major = element_line(size = .15, color = "#D3D3CB"),
    panel.background = element_rect(fill = "#E9EAEC"),
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(color = "black", fill = "NA", linewidth =  1), 
    strip.background = element_rect(color = "black", fill ="#B8BBC1", linewidth = 1),
    strip.text = element_text(color="black",size = 15)) 
tempplotearly

tempplotlate<-ggplot(tempplotlatedf,aes(x=bin,
                                        y=difference,
                                        group_by=Group))+
  geom_boxplot(aes(fill=Group))+
  scale_fill_manual(name= "Group", values = c("#FFCD58" ,"#9F2B00","#5C5828","#D37506","#4C5EA9","#5A0001","#D3D3CB", "#E8C45E"))+
  #facet_grid(Season~.)+
  labs(x = "Time Since Release (hrs)", y = "Degrees Changed from Release Temperature",
       title = "Late Tournament Temperature Differences")+
  theme(
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 15),
    legend.title = element_text(face = "bold", colour = "black", size = 15),
    plot.background = element_blank(),
    plot.title = element_text(face = "bold", colour= "black", size = 20,hjust = .0125),
    axis.title.y = element_text(face = "bold", colour = "black", size = 10),
    axis.title.x = element_text(face = "bold", colour = "black", size = 10),
    axis.ticks.length = unit(.50,"cm"),
    axis.ticks = element_line(size = 1.00, colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black", size = 15),
    axis.line = element_line(size = 1.00, color = "black"),
    axis.text = element_text(size = 15, face = "bold", color = "black"),
    panel.grid.major.x = element_line(size = .15, colour = "#D3D3CB"),
    panel.grid.minor = element_line(size = .15, color = "#D3D3CB"),
    panel.grid.major = element_line(size = .15, color = "#D3D3CB"),
    panel.background = element_rect(fill = "#E9EAEC"),
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(color = "black", fill = "NA", linewidth =  1), 
    strip.background = element_rect(color = "black", fill ="#B8BBC1", linewidth = 1),
    strip.text = element_text(color="black",size = 15)) 
tempplotlate

# figures and tables for papers -------------------------------------------
#all temps
tempplotallpaper<-ggplot(tempselectdf,aes(x=bin,
                                          y=difference,
                                          group_by=Season))+
  geom_boxplot(aes(fill=Season))+
  scale_fill_manual(name= "Season", values = c("#0A0708" ,"#444444","#747474","#B1B1B1"))+
  #facet_grid(Season~.)+
  labs(x = "Time Since Release (hrs)", y = "Degrees Changed from Release Temperature")+
  theme(
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 10),
    legend.title = element_text(face = "bold", colour = "black", size = 12),
    plot.background = element_blank(),
    axis.title.y =  element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(.50,"cm"),
    axis.ticks = element_line(size = 1.00, colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black", size = 10),
    axis.line = element_line(size = 1.00, color = "black"),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor =  element_blank(),
    panel.grid.major =  element_blank(),
    panel.background =  element_blank(),
    panel.spacing = unit(1, "lines"),
    panel.border =  element_blank(), 
    strip.background = element_rect(color = "black", fill ="#B8BBC1", linewidth = 1),
    strip.text = element_text(color="black",size = 15)) 
tempplotallpaper

#seasonal
#subset for two plots
tempplotearlydf<-subset(tempselectdf, Season=='Early'| Season=='EarlyControl')
tempplotlatedf<-subset(tempselectdf, Season=='Late' | Season=='LateControl')
tempplotearly<-ggplot(tempplotearlydf,aes(x=bin,
                                          y=difference,
                                          group_by=Group))+
  geom_boxplot(aes(fill=Group))+
  scale_fill_manual(name= "Group", values = c("#0A0708","#747474","#B1B1B1"))+
  #facet_grid(Season~.)+
  theme(
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 10),
    legend.title = element_text(face = "bold", colour = "black", size = 12),
    plot.background = element_blank(),
    axis.title.y =  element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(.50,"cm"),
    axis.ticks = element_line(size = 1.00, colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black", size = 10),
    axis.line = element_line(size = 1.00, color = "black"),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor =  element_blank(),
    panel.grid.major =  element_blank(),
    panel.background =  element_blank(),
    panel.spacing = unit(1, "lines"),
    panel.border =  element_blank(), 
    strip.background = element_rect(color = "black", fill ="#B8BBC1", linewidth = 1),
    strip.text = element_text(color="black",size = 15)) 
tempplotearly

tempplotlate<-ggplot(tempplotlatedf,aes(x=bin,
                                        y=difference,
                                        group_by=Group))+
  geom_boxplot(aes(fill=Group))+
  scale_fill_manual(name= "Group", values = c("#0A0708" ,"#444444","white","#747474","#B1B1B1","#D6D6D6"))+
  #facet_grid(Season~.)+
  theme(
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 10),
    legend.title = element_text(face = "bold", colour = "black", size = 12),
    plot.background = element_blank(),
    axis.title.y =  element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(.50,"cm"),
    axis.ticks = element_line(size = 1.00, colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black", size = 10),
    axis.line = element_line(size = 1.00, color = "black"),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor =  element_blank(),
    panel.grid.major =  element_blank(),
    panel.background =  element_blank(),
    panel.spacing = unit(1, "lines"),
    panel.border =  element_blank(), 
    strip.background = element_rect(color = "black", fill ="#B8BBC1", linewidth = 1),
    strip.text = element_text(color="black",size = 15)) 
tempplotlate

# models ------------------------------------------------------------------


#subset data for each paired treatment/control group
early<-subset(tempselectdfpaired, Group=='C1'| Group=='T2') 
late<-subset(tempselectdfpaired, Group=='C2'|Group=='T6') 

emm_options(lmerTest.limit = 5000)
#model for each paired group
earlytempmodel<-lmer(difference~Group*bin+(1|Signal), data = early)
summary(earlytempmodel)
anova(earlytempmodel)
Anova(earlytempmodel)
earlytempmodelcontrast <-emmeans(earlytempmodel,~Group:bin)
pwpp(earlytempmodelcontrast)
earlytempmodelcontrast<-pairs(earlytempmodelcontrast)
earlytempmodelcontrast
r_squared <- r.squaredGLMM(earlytempmodel)
par(mfrow = c(2, 2))
plot(lm(earlytempmodel))
write.xlsx(earlytempmodelcontrast, 'C:/Users/aahay2/Box/Hay_Allison/Data/analyses/Actel/Clinton Code/')

latetempmodel<-lmer(difference~Group*bin+(1|Signal), data = late)
summary(latetempmodel)
anova(latetempmodel)
Anova(latetempmodel)
latetempmodelcontrast <-emmeans(latetempmodel,~Group:bin)
pwpp(latetempmodelcontrast)
latetempmodelcontrast<-pairs(latetempmodelcontrast)
latetempmodelcontrast
r_squared <- r.squaredGLMM(latetempmodel)
par(mfrow = c(2, 2))
plot(lm(latetempmodel))
write.xlsx(latetempmodelcontrast, 'C:/Users/aahay2/Box/Hay_Allison/Data/analyses/Actel/Clinton Code/')

#all groups
tempmodel<-lmer(difference~Season+bin+(1|Signal), data = tempselectdf)
summary(tempmodel)
anova(tempmodel)
Anova(tempmodel)
tempmodelcontrast <-emmeans(tempmodel,~Season:bin)
pwpp(tempmodelcontrast)
tempmodelcontrast<-pairs(tempmodelcontrast)
tempmodelcontrast
r_squared <- r.squaredGLMM(tempmodel)
par(mfrow = c(2, 2))
plot(lm(tempmodel))
write.xlsx(tempmodelcontrast, 'C:/Users/Allison/Box/Hay_Allison/Data/analyses/Actel/Clinton Code/') 

#seasonal variation - controls are included but noted in season

tempmodelseason<-lmer(difference~Season*bin+(1|Signal), data = tempselectdf)
summary(tempmodelseason)
anova(tempmodelseason)
Anova(tempmodelseason)
tempmodelseasoncontrast <-emmeans(tempmodelseason,~Group:bin)
pwpp(tempmodelseasoncontrast)
tempmodelseasoncontrast<-pairs(tempmodelseasoncontrast)
tempmodelseasoncontrast
write.xlsx(tempmodelseasoncontrast, 'C:/Users/aahay2/Box/Hay_Allison/Data/analyses/Actel/Clinton Code/') 

# MORTALITY ---------------------------------------------------------------
Morts_tag<-Morts %>% mutate(Season = case_when(Group == 'T1'|Group =='C1'|Group =='T2' ~ 'Early',
                                               Group== 'C2'|Group== 'T3'|Group== 'T4'|
                                                 Group== 'T5'|Group== 'T6' ~ 'Late'))
Morts_tag<- Morts_tag %>%
  group_by(Signal) %>% 
  slice(1)
#remove tags that are not true mortalities - just weird detection plots
Morts_tag<- Morts_tag[!(Morts_tag$Signal %in% c(13236,13237,13241)),]
Morts_tag<-data.frame(append(Morts_tag, c(Count='1')))
Morts_tag<-Morts_tag %>% 
  group_by(Season) %>% 
  mutate(count = (1))
Morts_tag$count<-as.numeric(Morts_tag$count)
Morts_exp <- read_csv("Mortality.csv", col_types = cols(fishid = col_character(), 
                                                        date = col_date(format = "%m/%d/%Y")))

# plots -------------------------------------------------------------------
mortexpplot<-ggplot(Morts_exp,aes(x=Season,
                                  y=mort,
                                  group_by= Season))+
  geom_col()
mortexpplot

#add column that has percentages 
morttagplot<-ggplot(Morts_tag,aes(x=Season,
                                  group_by= Season))+
  geom_bar(stat="count", position = "dodge",aes(fill=Season))+
  scale_y_continuous(limits = NULL, breaks = c(2,4,6,8,10,12))+
  scale_fill_manual(name= "Array", values = c("#FFCD58","#9F2B00"))+
  scale_x_discrete(labels=c("Early", "Late"))+
  labs(x = "Season", y = "Mortalities",
       title = "Study Mortalities")+
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    plot.title = element_text(face = "bold", colour= "black", size = 32,hjust = .0125),
    axis.title.y = element_text(face = "bold", colour = "black", size = 20),
    axis.title.x = element_text(face = "bold", colour = "black", size = 20),
    axis.ticks.length = unit(.40,"cm"),
    axis.ticks = element_line(size = 1.00, colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black", size = 12),
    axis.line = element_line(size = 1.00, color = "black"),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    panel.grid.major.x = element_line(size = .15, colour = "#D3D3CB"),
    panel.grid.minor = element_line(size = .15, color = "#D3D3CB"),
    panel.grid.major = element_line(size = .15, color = "#D3D3CB"),
    panel.background = element_rect(fill = "#E9EAEC"))

morttagplot

mort_percent <- read_csv("mort_percent.csv", 
                         col_types = cols(`percent_mortality` = col_number()))
totalmortplot<-ggplot(mort_percent,aes(x=mort_type,
                                       y=percent_mortality))+
  geom_col(aes(fill=mort_type))+
  scale_fill_manual(name= "Array", values = c("#9F2B00", "#FFCD58","#D37506","#29335C"))+
  #scale_x_discrete(labels=c("Surgery", "Expected","Tournament","Total"))+
  labs(x = NULL, y = "Mortality Rate (%)",
       title = str_wrap("Late Season Tournaments Experience Higher Mortality Rates Than Early Season Tournaments",38))+
  annotate("text", x =1.12, y = 2.6, label = "1.70%", fontface = "bold", colour = "black", size = 10)+
  annotate("text", x =2.06, y = 21.5, label = "20.34%", fontface = "bold", colour = "black", size = 10)+
  annotate("text", x =3.07, y = 10.00, label = "8.9%", fontface = "bold", colour = "black", size = 10)+
  annotate("text", x =4.08, y = 23, label = "22.03%", fontface = "bold", colour = "black", size = 10)+
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    plot.title = element_text(face = "bold", colour= "black", size = 28,hjust = .0125),
    axis.title.y = element_text(face = "bold", colour = "black", size = 20),
    axis.title.x = element_text(face = "bold", colour = "black", size = 20),
    axis.ticks.length = unit(.40,"cm"),
    axis.ticks = element_line(size = 1.00, colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black", size = 12),
    axis.line = element_line(size = 1.00, color = "black"),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    panel.grid.major.x = element_line(size = .15, colour = "#D3D3CB"),
    panel.grid.minor = element_line(size = .15, color = "#D3D3CB"),
    panel.grid.major = element_line(size = .15, color = "#D3D3CB"),
    panel.background = element_rect(fill = "#E9EAEC"))

totalmortplot

# figures and tables for manuscript ---------------------------------------
mort_percent <- read_csv("mort_percent.csv", 
                         col_types = cols(`percent_mortality` = col_number()))
totalmortplot<-ggplot(mort_percent,aes(x=mort_type,
                                       y=percent_mortality))+
  geom_col(aes(fill=mort_type))+
  scale_fill_manual(name= "Array", values = c("#0A0708" ,"#444444","#D6D6D6","#747474"))+
  #scale_x_discrete(labels=c("Surgery", "Expected","Tournament","Total"))+
  labs(x = NULL, y = NULL,
       title = NULL)+
  annotate("text", x =1.12, y = 2.6, label = "1.70%", fontface = "bold", colour = "black", size = 7)+
  annotate("text", x =2.06, y = 21.5, label = "20.34%", fontface = "bold", colour = "black", size = 7)+
  annotate("text", x =3.07, y = 10.00, label = "8.9%", fontface = "bold", colour = "black", size = 7)+
  annotate("text", x =4.08, y = 23, label = "22.03%", fontface = "bold", colour = "black", size = 7)+
  theme(
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 10),
    legend.title = element_text(face = "bold", colour = "black", size = 12),
    plot.background = element_blank(),
    axis.title.y =  element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.length = unit(.50,"cm"),
    axis.ticks = element_line(size = 1.00, colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black", size = 10),
    axis.line = element_line(size = 1.00, color = "black"),
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor =  element_blank(),
    panel.grid.major =  element_blank(),
    panel.background =  element_blank(),
    panel.spacing = unit(1, "lines"),
    panel.border =  element_blank(), 
    strip.background = element_rect(color = "black", fill ="#B8BBC1", linewidth = 1),
    strip.text = element_text(color="black",size = 15)) 
totalmortplot



# models ------------------------------------------------------------------
#models for experiment
one.way <- aov(mort ~ Season, data = Morts_exp)
two.way <- aov(mort ~ Season+length, data = Morts_exp)
summary(one.way)
r_squared <- r.squaredGLMM(one.way)
summary(two.way)
model.set<- list(two.way,one.way)
model.names<-c("two.way","one.way")
aictab(model.set, modnames = model.names)
anova(one.way)
Anova(one.way)
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way

#tag detection models
tag.detections


tagsall<- merge(tag.detections,biometrics_RA, by= intersect(x = "Signal", y= "Signal"))
tagsall<-tagsall %>% mutate(Mort = case_when(Signal == '2441'~ 1, Signal =='13566'~ 1,
                                             Signal== '13568'~ 1, Signal== '2400'~ 1,
                                             Signal== '2384'~ 1, Signal== '2396' ~ 1, Signal == '10845'~ 1,Signal== '10847'~ 1,
                                             Signal== '10856'~ 1, Signal== '13236'~ 1, Signal== '13239'~ 1, Signal== '13241'~ 1,
                                             Signal== '13247'~ 1, Signal== '13244'~ 1,Signal== '2392'~ 1))
tagsall[is.na(tagsall)] <- 0
tagsall <- tagsall %>% na.replace(0)

tagsall<-tagsall %>% mutate(Season = case_when(Group == 'T1'|Group =='T2' ~ 'Early',
                                               Group =='C1'~ 'EarlyControl',
                                               Group== 'C2' ~'LateControl', Group== 'T3'|Group== 'T4'|
                                                 Group== 'T5'|Group== 'T6' ~ 'Late'))
write.xlsx(tagsall, "C:/Users/ YOUR_WD_HERE")
tagsall <- read_csv("tagsallcsv.csv", 
                    col_types = cols(Timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                     Release.date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
class(tagsall$Timestamp)
tagsall['Timestamp'] <-format(as.POSIXct(tagsall$Timestamp,format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d") 
tagsall$Detect.Time <- format(as.POSIXct(tagsall$Timestamp,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S")
#now for release
tagsall['Release.date'] <-format(as.POSIXct(tagsall$Release.date,format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d") 
tagsall$Release.Time <- format(as.POSIXct(tagsall$Release.date,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S")
#reorder the columns so they make sense - Keep time for now, maybe get rid of later
tagsall<- tagsall %>%
  #Add a column to calculate days between Release and detection
  mutate(
    Elapsed.T = as.duration(Release.date %--% Timestamp) / ddays(1)
  )
tagsall<-tagsall %>% filter(Elapsed.T < 4)
tagsall<- tagsall %>% 
  group_by(Signal) %>% 
  slice(1) %>% 
  ungroup()
#trim for only the first 3 days after tournament!
tagsall<- tagsall[!(tagsall$Signal %in% c(13236,13237,13241)),]
tagsall<-tagsall[!(tagsall$Season %in% c('LateControl','EarlyControl')),]

one.waytag <- aov(Mort ~ Season, data = tagsall)
summary(one.waytag)
r_squared <- r.squaredGLMM(one.waytag)
tukeytags<-TukeyHSD(one.waytag)
tukeytags