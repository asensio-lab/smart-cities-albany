if (!require(c("tidyverse", "ggalt","gdata","gridExtra","rgenoud","Matching","MatchIt",
               "plm","Hmisc","lmtest","sandwich","multiwayvcov","cem","devtools","did","DIDmultiplegt","TwoWayFEWeights")))
  install.packages(c("tidyverse","ggalt","gdata","gridExtra","rgenoud","Matching","MatchIt",
                     "plm","Hmisc","lmtest","sandwich","multiwayvcov","cem","devtools","did","DIDmultiplegt","TwoWayFEWeights"))

library(tidyverse)
library(ggalt)
library(gdata)
library(gridExtra)
library(rgenoud)
library(Matching)
library(MatchIt)
library(plm)
library(Hmisc)
library(lmtest)
library(sandwich)
library(multiwayvcov)
library(cem)
library(devtools)
library(did)
library(DIDmultiplegt)
library(TwoWayFEWeights)

install_github('ChristopherLucas/MatchingFrontier')
library(MatchingFrontier)


ELC <- read.csv(file = "ELC.csv", header = TRUE)
ELC <- ELC %>% 
  mutate(yearELC = as.factor(yearELC),
         monthELC = as.factor(monthELC),
         quarterELC = as.factor(quarterELC))

PropertyStats <- read.csv(file = "PropertyStats.csv", header = TRUE)
PropertyStats <- PropertyStats %>% 
  mutate(baths = as.numeric(baths),
         beds = as.numeric(beds),
         assessment = as.numeric(assessment),
         market = as.numeric(market),
         size = as.numeric(size),
         PropertyAge = as.numeric(PropertyAge),
         BaselineConsumption = as.numeric(BaselineConsumption),
         InitialPeriod = as.numeric(InitialPeriod))

# transformation of the log-linear regression coefficients and CIs (based on Wooldridge,2008)
# SE are updated using the recalculated coefficients and CIs as (CI.upper_bound-coef)/1.96
resexp <- function(cluster,m,n){
  x_list=NA
  for(r in seq(1:m))
    {
    for(c in seq(1:n))
      {
      x <- exp(cluster[r,c])-1
      x_list <- c(x_list,x)
      }
    }
  print(x_list)
}

## Fixed Effects Model

ELC_property <- merge(ELC, PropertyStats, by = 'ID')
ELC_property <- ELC_property %>%
  mutate(NormConsumption = Consumption/size,
         Treatment = ifelse(InitialPeriod<=Period,1,0)) # Treatment variable encompass both the fact of being treated and the time of treatment - turns out 1 when the treated unit gets treatment
ELC_property <- ELC_property %>% mutate(Treatment = ifelse(is.na(Treatment),0,Treatment))
table(ELC_property$Treatment)
ELC_property <- ELC_property %>% filter(NormConsumption <= quantile(NormConsumption, c(0.9999), na.rm = TRUE)) # outliers

# by group
fe <- plm(log(NormConsumption) ~ Treatment + yearELC + monthELC + CoolingDays + HeatingDays, data = ELC_property, model='within', index = c('ID','Period')) # fixed effects to remove unobserved heterogeneity between properties and periods
summary(fe) 
coeftest(fe, vcov=function(x) vcovHC(x, cluster="group", type="HC0")) # cluster-adjusted standard error account for within-cluster correlation
resexp(coeftest(fe, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),1,1) # exponential transformation of log-linear coefficients
confint(coeftest(fe, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(fe, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),1,2)

# placebo testing
ELC_property <- ELC_property %>% mutate(Treatment_placebo = ifelse(Group==1 & 20<Period,1,0)) # Treatment assigned to the middle of the pre-treatment period
fe.pl <- plm(log(NormConsumption) ~ Treatment_placebo + yearELC + monthELC + CoolingDays + HeatingDays, data = ELC_property, model='within', index = c('ID','Period'))
summary(fe.pl) 
coeftest(fe.pl, vcov=function(x) vcovHC(x, cluster="group", type="HC0")) # not sign
resexp(coeftest(fe.pl, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),1,1) 
confint(coeftest(fe.pl, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(fe.pl, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),1,2)

# by program
ELC_property <- ELC_property %>% 
  mutate(TreatProgram = case_when(Treatment==0 ~ 'None', 
                                  Treatment==1 & Program == 'CDBG' ~ 'CDBG', 
                                  Treatment==1 & Program == 'HOME' ~ 'HOME'))
ELC_property$TreatProgram <- factor(ELC_property$TreatProgram, levels = c('None', 'CDBG', 'HOME'))
fe.program <- plm(log(NormConsumption) ~ TreatProgram + yearELC + monthELC + CoolingDays + HeatingDays, data = ELC_property, model='within', index = c('ID','Period'))  
summary(fe.program)
coeftest(fe.program, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))
resexp(coeftest(fe.program, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),2,1) 
confint(coeftest(fe.program, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(fe.program, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),2,2)

# by project 
ELC_property <- ELC_property %>%
  mutate(TreatProject = case_when(Treatment==0 ~ "None", 
                                  Treatment==1 & Project == "Energy Efficiency" ~ "Energy Efficiency",
                                  Treatment==1 & Project == "Emergency Repairs" ~ "Emergency Repairs",
                                  Treatment==1 & Program == "CDBG" & Project == "Homeowner Rehabilitation" ~ "Homeowner Rehabilitation - CDBG",
                                  Treatment==1 & Program == "CDBG" & Project == "Rental Rehabilitation" ~ "Rental Rehabilitation - CDBG",
                                  Treatment==1 & Program == "HOME" & Project == "Homeowner Rehabilitation" ~ "Homeowner Rehabilitation - HOME",
                                  Treatment==1 & Program == "HOME" & Project == "Rental Rehabilitation" ~ "Rental Rehabilitation - HOME",
                                  Treatment==1 & Project == "Acquisition" ~ "Acquisition",
                                  Treatment==1 & Project == "CHDO" ~ "CHDO",
                                  Treatment==1 & Project == "Beautification" ~ "Beautification",
                                  Treatment==1 & Project == "Demolition" ~ "Demolition",
                                  Treatment==1 & Project == "Hudson Lane Sewer Connections" ~ "Hudson Lane Sewer Connections",
                                  Treatment==1 & Project == "Lead-Based Paint Remediations" ~ "Lead-Based Paint Remediations",
                                  Treatment==1 & Project == "Public Facilities" ~ "Public Facilities",
                                  Treatment==1 & Project == "Public Service" ~ "Public Service",
                                  Treatment==1 & Project == "Relocation" ~ "Relocation",
                                  Treatment==1 & Project %in% c("Tenant Based Rental Assistance","New Construction","Homebuyer Rehab","Acquisition/Rehabilitation","AHOP Financing","Acquisition/Rental Rehabilitation") ~ "Non-energy"))

ELC_property$TreatProject <- factor(ELC_property$TreatProject, levels = c("None","Acquisition","CHDO","Beautification","Demolition","Emergency Repairs","Energy Efficiency",
                                                                          "Homeowner Rehabilitation - CDBG","Homeowner Rehabilitation - HOME","Public Facilities",
                                                                          "Hudson Lane Sewer Connections","Lead-Based Paint Remediations","Public Service","Relocation",
                                                                          "Rental Rehabilitation - CDBG","Rental Rehabilitation - HOME","Non-energy"))

fe.project <- plm(log(NormConsumption) ~ TreatProject + yearELC + monthELC + CoolingDays + HeatingDays, data = ELC_property, model='within', index = c('ID','Period'))  
summary(fe.project)
coeftest(fe.project, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))
resexp(coeftest(fe.project, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),15,1) 
confint(coeftest(fe.project, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(fe.project, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),15,2)

# non-energy for placebo
ELC_property_NE <- ELC_property %>% filter(!TreatProject %in% c("Energy Efficiency","Emergency Repairs",
                                                                "Homeowner Rehabilitation - CDBG","Rental Rehabilitation - CDBG",
                                                                "Homeowner Rehabilitation - HOME","Rental Rehabilitation - HOME"))
fe.plm.ne <- plm(NormConsumption ~ Treatment + yearELC + monthELC + CoolingDays + HeatingDays, data = ELC_property_NE, model='within', index = c('ID','TimePeriod'))
summary(fe.ne) 
coeftest(fe.ne, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))
resexp(coeftest(fe.ne, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),1,1) 
confint(coeftest(fe.ne, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(fe.ne, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),1,2)
                        
## GenMatch - Genetic Matching 

c1 = makePSOCKcluster(100) # enhance computing 
set.seed(123)

# calibrate ratio only to keep all treated
r_list = 1
mean_std_diff_list = 73.81971
G_list= 0.4225865
N_list = 407

for(r in seq(2,100,1))
{
  gen_match <- matchit(Group ~ BaselineConsumption + size + beds + baths + PropertyAge + market, method='genetic',data=PropertyStats, replace = TRUE, ratio=r, cluster=c1)
  r_list <- c(r_list,r)
  mean_std_diff_list <- c(mean_std_diff_list,mean(summary(gen_match)$reduction[-1,1]))
  N_list <- c(N_list,summary(gen_match)$nn[4,1])
  G <- L1.meas(match.data(gen_match)$Group, match.data(gen_match), drop=c("X","lot_size","zoningType","siteZoningIdent","propClass","yearBuilt","rooms","floors","condition","foundationType","roofCover","wallType","improvementYear","PrimaryId","Plan.Year",
                                                                          "Project","Program","InitialDate","initialmonth","initialyear","InitialPeriod","InitialQuarter","Group","Longitude","Latitude","FIPS","Interview","distance","weights",
                                                                          "assessment","MedianIncome","PovertyBelow","FemaleHouseholder","Black","MedianAge","RentAsIncome35","OccupantsRoom","SNAP"), breaks = NULL, weights=match.data(gen_match)$weights, grouping = NULL)
  G_list <- c(G_list,0.5*(G[["L1"]]))
}

plot(r_list,mean_std_diff_list,xlab='Ratio',ylab='Average Percent Reduction in Std. Mean Diff.') # Figure S1
T_list <- 16680 - (N_list + 549)
plot(T_list,G_list,xlim=c(0,17500),ylim=c(0.05,0.5),col = '#ba7f27', xlab = 'Number of Observations Pruned' , ylab = 'Absolute Loss Function') # Figure S5
plot(T_list,mean_std_diff_list,xlab = 'Number of Observations Pruned' , ylab = 'Average Percent Reduction in Std. Mean Diff.') # Figure S2
stopCluster(c1)

# best solution
gen_match <- matchit(Group ~ BaselineConsumption + size + beds + baths + PropertyAge + market, method='genetic', pop.size=100, data=PropertyStats, replace = TRUE, ratio=19, cluster=c1) # pop.size=100 for faster processing
summary(gen_match, data = PropertyStats, addlvariables = c("assessment","MedianIncome","FemaleHouseholder","Black","PovertyBelow",
                                                           "RentAsIncome35","SNAP"))
stopCluster(c1)

# bias reduction in standardized percent bias (Figure 2)
gen_mean_treated_bef <- summary(gen_match, data = PropertyStats, 
                                addlvariables = c("assessment","MedianIncome","FemaleHouseholder","Black","PovertyBelow",
                                                  "RentAsIncome35","SNAP"))$sum.all[2:14,1]

gen_mean_control_bef <- summary(gen_match, data = PropertyStats, 
                                addlvariables = c("assessment","MedianIncome","FemaleHouseholder","Black","PovertyBelow",
                                                  "RentAsIncome35","SNAP"))$sum.all[2:14,2]

gen_avg_var_bef <- c(sqrt((var(PropertyStats$BaselineConsumption[PropertyStats$Group==1]) + var(PropertyStats$BaselineConsumption[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$size[PropertyStats$Group==1]) + var(PropertyStats$size[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$beds[PropertyStats$Group==1]) + var(PropertyStats$beds[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$baths[PropertyStats$Group==1]) + var(PropertyStats$baths[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$PropertyAge[PropertyStats$Group==1]) + var(PropertyStats$PropertyAge[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$market[PropertyStats$Group==1]) + var(PropertyStats$market[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$assessment[PropertyStats$Group==1]) + var(PropertyStats$assessment[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$MedianIncome[PropertyStats$Group==1]) + var(PropertyStats$MedianIncome[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$PovertyBelow[PropertyStats$Group==1]) + var(PropertyStats$PovertyBelow[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$FemaleHouseholder[PropertyStats$Group==1]) + var(PropertyStats$FemaleHouseholder[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$Black[PropertyStats$Group==1]) + var(PropertyStats$Black[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$RentAsIncome35[PropertyStats$Group==1]) + var(PropertyStats$RentAsIncome35[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$SNAP[PropertyStats$Group==1]) + var(PropertyStats$SNAP[PropertyStats$Group==0]))/2))

gen_std_mean_dif_bef <- 100*(gen_mean_treated_bef-gen_mean_control_bef)/gen_avg_var_bef

gen_mean_treated_aft <- summary(gen_match, data = PropertyStats, 
                                addlvariables = c("assessment","MedianIncome","FemaleHouseholder","Black","PovertyBelow",
                                                  "RentAsIncome35","SNAP"))$sum.matched[2:14,1]
gen_mean_control_aft <- summary(gen_match, data = PropertyStats, 
                                addlvariables = c("assessment","MedianIncome","FemaleHouseholder","Black","PovertyBelow",
                                                  "RentAsIncome35","SNAP"))$sum.matched[2:14,2]

gen_avg_var_aft <- c(sqrt((var(gen_matched_data$BaselineConsumption[gen_matched_data$Group==1]) + var(gen_matched_data$BaselineConsumption[gen_matched_data$Group==0]))/2),
                     sqrt((var(gen_matched_data$size[gen_matched_data$Group==1]) + var(gen_matched_data$size[gen_matched_data$Group==0]))/2),
                     sqrt((var(gen_matched_data$beds[gen_matched_data$Group==1]) + var(gen_matched_data$beds[gen_matched_data$Group==0]))/2),
                     sqrt((var(gen_matched_data$baths[gen_matched_data$Group==1]) + var(gen_matched_data$baths[gen_matched_data$Group==0]))/2),
                     sqrt((var(gen_matched_data$PropertyAge[gen_matched_data$Group==1]) + var(gen_matched_data$PropertyAge[gen_matched_data$Group==0]))/2),
                     sqrt((var(gen_matched_data$market[gen_matched_data$Group==1]) + var(gen_matched_data$market[gen_matched_data$Group==0]))/2),
                     sqrt((var(gen_matched_data$assessment[gen_matched_data$Group==1]) + var(gen_matched_data$assessment[gen_matched_data$Group==0]))/2),
                     sqrt((var(gen_matched_data$MedianIncome[gen_matched_data$Group==1]) + var(gen_matched_data$MedianIncome[gen_matched_data$Group==0]))/2),
                     sqrt((var(gen_matched_data$PovertyBelow[gen_matched_data$Group==1]) + var(gen_matched_data$PovertyBelow[gen_matched_data$Group==0]))/2),
                     sqrt((var(gen_matched_data$FemaleHouseholder[gen_matched_data$Group==1]) + var(gen_matched_data$FemaleHouseholder[gen_matched_data$Group==0]))/2),
                     sqrt((var(gen_matched_data$Black[gen_matched_data$Group==1]) + var(gen_matched_data$Black[gen_matched_data$Group==0]))/2),
                     sqrt((var(gen_matched_data$RentAsIncome35[gen_matched_data$Group==1]) + var(gen_matched_data$RentAsIncome35[gen_matched_data$Group==0]))/2),
                     sqrt((var(gen_matched_data$SNAP[gen_matched_data$Group==1]) + var(gen_matched_data$SNAP[gen_matched_data$Group==0]))/2))

gen_std_mean_dif_aft <- 100*(gen_mean_treated_aft-gen_mean_control_aft)/gen_avg_var_aft

gen_reduction <- round(summary(gen_match, data = PropertyStats, 
                               addlvariables = c("assessment","MedianIncome","FemaleHouseholder","Black","PovertyBelow",
                                                 "RentAsIncome35","SNAP"))$reduction[2:14,1],2)                             

gen_bias_df <- data.frame('Covariates' = Covariates,                          
                          'Before' = gen_std_mean_dif_bef,
                          'After' = gen_std_mean_dif_aft, 
                          'Reduction' = gen_reduction)

gg_gen <- ggplot(gen_bias_df, aes(x=After, xend=Before, y=Covariates, group=Covariates)) +
  geom_dumbbell(color = '#e8ac51',
                size=2, 
                colour_x = '#ba7f27',
                colour_xend ='#fccd86',
                size=2, 
                dot_guide=TRUE, dot_guide_size=0.25) + 
  labs(x="", y="", title="") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#ffffff"),
        panel.background=element_rect(fill="#ffffff"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(linetype=2),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg_gen)

# extract matched data (gen_match)
gen_matched_data <- match.data(gen_match) 
gen_matched_data$Index <- 1:nrow(gen_matched_data)

# merge data and create factors
genPanel <- merge(ELC, gen_matched_data, by = 'ID')
genPanel <- genPanel %>% 
  mutate(NormConsumption = Consumption/size,
         Treatment = ifelse(InitialPeriod<=Period,1,0)) 
genPanel$Treatment <- ifelse(is.na(genPanel$Treatment),0,genPanel$Treatment)
genPanel <- genPanel %>% filter(NormConsumption <= quantile(NormConsumption, c(0.9999), na.rm = TRUE))

# by group
gen <- plm(log(NormConsumption) ~ Treatment + yearELC + monthELC + CoolingDays + HeatingDays, data = genPanel, model='within', index = c('ID','Period'))
summary(gen)
coeftest(gen, vcov=function(x) vcovHC(x, cluster="group", type="HC0")) 
resexp(coeftest(gen, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),1,1) 
confint(coeftest(gen, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(gen, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),1,2)

# placebo testing
genPanel <- genPanel %>% mutate(Treatment_placebo = ifelse(Group==1 & 20<Period,1,0))
gen.pl <- plm(log(NormConsumption) ~ Treatment_placebo + yearELC + monthELC + CoolingDays + HeatingDays, data = genPanel, model='within', index = c('ID','Period'))
summary(gen.pl) # (not sign)
coeftest(gen.pl, vcov=function(x) vcovHC(x, cluster="group", type="HC0")) 
resexp(coeftest(gen.pl, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),1,1) 
confint(coeftest(gen.pl, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(gen.pl, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),1,2)

# by program
genPanel <- genPanel %>%
  mutate(TreatProgram = case_when(Treatment==0 ~ 'None',
                                  Treatment==1 & Program == 'CDBG' ~ 'CDBG',
                                  Treatment==1 & Program == 'HOME' ~ 'HOME')) 
genPanel$TreatProgram <- factor(genPanel$TreatProgram, levels = c("None", "CDBG", "HOME"))
gen.program <- plm(log(NormConsumption) ~ TreatProgram + yearELC + monthELC + CoolingDays + HeatingDays, data = genPanel,model='within', index = c('ID','Period'))
summary(gen.program)
coeftest(gen.program, vcov=function(x) vcovHC(x, cluster="group", type="HC0")) 
resexp(coeftest(gen.program, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),2,1) 
confint(coeftest(gen.program, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(gen.program, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),2,2)

# by project
genPanel <- genPanel %>%
  mutate(TreatProject = case_when(Treatment==0 ~ "None", 
                                  Treatment==1 & Project == "Energy Efficiency" ~ "Energy Efficiency",
                                  Treatment==1 & Project == "Emergency Repairs" ~ "Emergency Repairs",
                                  Treatment==1 & Program == "CDBG" & Project == "Homeowner Rehabilitation" ~ "Homeowner Rehabilitation - CDBG",
                                  Treatment==1 & Program == "CDBG" & Project == "Rental Rehabilitation" ~ "Rental Rehabilitation - CDBG",
                                  Treatment==1 & Program == "HOME" & Project == "Homeowner Rehabilitation" ~ "Homeowner Rehabilitation - HOME",
                                  Treatment==1 & Program == "HOME" & Project == "Rental Rehabilitation" ~ "Rental Rehabilitation - HOME",
                                  Treatment==1 & Project == "Acquisition" ~ "Acquisition",
                                  Treatment==1 & Project == "CHDO" ~ "CHDO",
                                  Treatment==1 & Project == "Beautification" ~ "Beautification",
                                  Treatment==1 & Project == "Demolition" ~ "Demolition",
                                  Treatment==1 & Project == "Hudson Lane Sewer Connections" ~ "Hudson Lane Sewer Connections",
                                  Treatment==1 & Project == "Lead-Based Paint Remediations" ~ "Lead-Based Paint Remediations",
                                  Treatment==1 & Project == "Public Facilities" ~ "Public Facilities",
                                  Treatment==1 & Project == "Public Service" ~ "Public Service",
                                  Treatment==1 & Project == "Relocation" ~ "Relocation",
                                  Treatment==1 & Project %in% c("Tenant Based Rental Assistance","New Construction","Homebuyer Rehab","Acquisition/Rehabilitation","AHOP Financing","Acquisition/Rental Rehabilitation") ~ "Non-energy"))

genPanel$TreatProject <- factor(genPanel$TreatProject, levels = c("None","Acquisition","CHDO","Beautification","Demolition","Emergency Repairs","Energy Efficiency",
                                                                  "Homeowner Rehabilitation - CDBG","Homeowner Rehabilitation - HOME","Public Facilities",
                                                                  "Hudson Lane Sewer Connections","Lead-Based Paint Remediations","Public Service","Relocation",
                                                                  "Rental Rehabilitation - CDBG","Rental Rehabilitation - HOME","Non-energy"))

gen.project <- plm(log(NormConsumption) ~ TreatProject + yearELC + monthELC + CoolingDays + HeatingDays, data = genPanel, model='within', index = c('ID','Period'))
summary(gen.project)
coeftest(gen.project, vcov=function(x) vcovHC(x, cluster="group", type="HC0")) 
resexp(coeftest(gen.project, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),15,1) 
confint(coeftest(gen.project, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(gen.project, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),15,2)

# non-energy for placebo
genPanel_NE<- psmPanel %>% filter(!TreatProject %in% c("Energy Efficiency","Emergency Repairs",
                                                       "Homeowner Rehabilitation - CDBG","Rental Rehabilitation - CDBG",
                                                       "Homeowner Rehabilitation - HOME","Rental Rehabilitation - HOME"))
gen.ne <- plm(log(NormConsumption) ~ Treatment + yearELC + monthELC + CoolingDays + HeatingDays, data = genPanel_NE, model='within', index = c('ID','Period'))
summary(gen.ne) # (+) sign after clustering
coeftest(gen.ne, vcov=function(x) vcovHC(x, cluster="group", type="HC0")) 
resexp(coeftest(gen.ne, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),1,1) 
confint(coeftest(gen.ne, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(gen.ne, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),1,2)                       

## PSM - Propensity Score Matching

# calibrate ratio only to keep all treated
r_list = 1
mean_std_diff_list = 73.81971
P_list= 0.495329
N_list = 478

for(r in seq(2,100,1))
{
  psm_match <- matchit(Group ~ BaselineConsumption + size + beds + baths + PropertyAge + market, method='nearest',data=PropertyStats, replace = TRUE, ratio=r)
  r_list <- c(r_list,r)
  mean_std_diff_list <- c(mean_std_diff_list,mean(summary(psm_match)$reduction[-1,1]))
  N_list <- c(N_list,summary(psm_match)$nn[4,1])
  P <- L1.meas(match.data(psm_match)$Group, match.data(psm_match), drop=c("X","lot_size","zoningType","siteZoningIdent","propClass","yearBuilt","rooms","floors","condition","foundationType","roofCover","wallType","improvementYear","PrimaryId","Plan.Year",
                                                                          "Project","Program","InitialDate","initialmonth","initialyear","InitialPeriod","InitialQuarter","Group","Longitude","Latitude","FIPS","Interview","distance","weights",
                                                                          "assessment","MedianIncome","PovertyBelow","FemaleHouseholder","Black","MedianAge","RentAsIncome35","OccupantsRoom","SNAP"), breaks = NULL, weights=match.data(psm_match)$weights, grouping = NULL)
  P_list <- c(P_list,0.5*(P[["L1"]]))
}

plot(r_list,mean_std_diff_list,xlab='Ratio',ylab='Average Percent Reduction in Std. Mean Diff.') # Figure S3
H_list <- 16680 - (N_list + 549)
plot(H_list,P_list,xlim=c(0,17500),ylim=c(0.05,0.5),col ='#0e668b',xlab = 'Number of Observations Pruned' , ylab = 'Absolute Loss Function') # Figure S5
plot(H_list,mean_std_diff_list,xlab = 'Number of Observations Pruned' , ylab = 'Average Percent Reduction in Std. Mean Diff.') # Figure S4

# best solution
psm_match <- matchit(Group ~ BaselineConsumption + size + beds + baths + PropertyAge + market, method='nearest', data=PropertyStats, replace = TRUE, ratio=21)
summary(psm_match, data = PropertyStats, addlvariables = c("assessment","MedianIncome","FemaleHouseholder","Black","PovertyBelow",
                                                           "RentAsIncome35","SNAP"))
# bias reduction in standardized percent bias (Figure 2)
psm_mean_treated_bef <- summary(psm_match, data = PropertyStats, 
                                addlvariables = c("assessment","MedianIncome","FemaleHouseholder","Black","PovertyBelow",
                                                  "RentAsIncome35","SNAP"))$sum.all[2:14,1]

psm_mean_control_bef <- summary(psm_match, data = PropertyStats, 
                                addlvariables = c("assessment","MedianIncome","FemaleHouseholder","Black","PovertyBelow",
                                                  "RentAsIncome35","SNAP"))$sum.all[2:14,2]

psm_avg_var_bef <- c(sqrt((var(PropertyStats$BaselineConsumption[PropertyStats$Group==1]) + var(PropertyStats$BaselineConsumption[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$size[PropertyStats$Group==1]) + var(PropertyStats$size[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$beds[PropertyStats$Group==1]) + var(PropertyStats$beds[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$baths[PropertyStats$Group==1]) + var(PropertyStats$baths[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$PropertyAge[PropertyStats$Group==1]) + var(PropertyStats$PropertyAge[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$market[PropertyStats$Group==1]) + var(PropertyStats$market[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$assessment[PropertyStats$Group==1]) + var(PropertyStats$assessment[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$MedianIncome[PropertyStats$Group==1]) + var(PropertyStats$MedianIncome[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$PovertyBelow[PropertyStats$Group==1]) + var(PropertyStats$PovertyBelow[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$FemaleHouseholder[PropertyStats$Group==1]) + var(PropertyStats$FemaleHouseholder[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$Black[PropertyStats$Group==1]) + var(PropertyStats$Black[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$RentAsIncome35[PropertyStats$Group==1]) + var(PropertyStats$RentAsIncome35[PropertyStats$Group==0]))/2),
                     sqrt((var(PropertyStats$SNAP[PropertyStats$Group==1]) + var(PropertyStats$SNAP[PropertyStats$Group==0]))/2))

psm_std_mean_dif_bef <- 100*(psm_mean_treated_bef-psm_mean_control_bef)/psm_avg_var_bef


psm_mean_treated_aft <- summary(psm_match, data = PropertyStats, 
                                addlvariables = c("assessment","MedianIncome","FemaleHouseholder","Black","PovertyBelow",
                                                  "RentAsIncome35","SNAP"))$sum.matched[2:14,1]
psm_mean_control_aft <- summary(psm_match, data = PropertyStats, 
                                addlvariables = c("assessment","MedianIncome","FemaleHouseholder","Black","PovertyBelow",
                                                  "RentAsIncome35","SNAP"))$sum.matched[2:14,2]

psm_avg_var_aft <- c(sqrt((var(psm_matched_data$BaselineConsumption[psm_matched_data$Group==1]) + var(psm_matched_data$BaselineConsumption[psm_matched_data$Group==0]))/2),
                     sqrt((var(psm_matched_data$size[psm_matched_data$Group==1]) + var(psm_matched_data$size[psm_matched_data$Group==0]))/2),
                     sqrt((var(psm_matched_data$beds[psm_matched_data$Group==1]) + var(psm_matched_data$beds[psm_matched_data$Group==0]))/2),
                     sqrt((var(psm_matched_data$baths[psm_matched_data$Group==1]) + var(psm_matched_data$baths[psm_matched_data$Group==0]))/2),
                     sqrt((var(psm_matched_data$PropertyAge[psm_matched_data$Group==1]) + var(psm_matched_data$PropertyAge[psm_matched_data$Group==0]))/2),
                     sqrt((var(psm_matched_data$market[psm_matched_data$Group==1]) + var(psm_matched_data$market[psm_matched_data$Group==0]))/2),
                     sqrt((var(psm_matched_data$assessment[psm_matched_data$Group==1]) + var(psm_matched_data$assessment[psm_matched_data$Group==0]))/2),
                     sqrt((var(psm_matched_data$MedianIncome[psm_matched_data$Group==1]) + var(psm_matched_data$MedianIncome[psm_matched_data$Group==0]))/2),
                     sqrt((var(psm_matched_data$PovertyBelow[psm_matched_data$Group==1]) + var(psm_matched_data$PovertyBelow[psm_matched_data$Group==0]))/2),
                     sqrt((var(psm_matched_data$FemaleHouseholder[psm_matched_data$Group==1]) + var(psm_matched_data$FemaleHouseholder[psm_matched_data$Group==0]))/2),
                     sqrt((var(psm_matched_data$Black[psm_matched_data$Group==1]) + var(psm_matched_data$Black[psm_matched_data$Group==0]))/2),
                     sqrt((var(psm_matched_data$RentAsIncome35[psm_matched_data$Group==1]) + var(psm_matched_data$RentAsIncome35[psm_matched_data$Group==0]))/2),
                     sqrt((var(psm_matched_data$SNAP[psm_matched_data$Group==1]) + var(psm_matched_data$SNAP[psm_matched_data$Group==0]))/2))

psm_std_mean_dif_aft <- 100*(psm_mean_treated_aft-psm_mean_control_aft)/psm_avg_var_aft

psm_reduction <- round(summary(psm_match, data = PropertyStats, 
                               addlvariables = c("assessment","MedianIncome","FemaleHouseholder","Black","PovertyBelow",
                                                 "RentAsIncome35","SNAP"))$reduction[2:14,1],2)                             

names <- c("Average Baseline Consumption", "Property Size", "No. Beds", "No. Baths", "Property Age", "Market Property Value", "Assessment Property Value","Median Income", "Female Head of the Household", "Black Population", "Population below Poverty Level", "Gross Rent more than 35% of Household Incomes", "Population on SNAP")

Covariates <- factor(names, ordered = TRUE, levels = rev(names))

psm_bias_df <- data.frame('Covariates' = Covariates,
                          'Before' = psm_std_mean_dif_bef,
                          'After' = psm_std_mean_dif_aft, 
                          'Reduction' = psm_reduction)

gg_psm <- ggplot(psm_bias_df, aes(x=After, xend=Before, y=Covariates, group=Covariates)) +
  geom_vline(colour='#787878',xintercept = 0) +
  geom_dumbbell(color='#8ab7db', 
                #color = '#e8ac51',
                size=2, 
                colour_x = '#0e668b',
                #colour_x = '#ba7f27',
                colour_xend ='#d2e8fa',
                #colour_xend = '#fccd86',
                dot_guide=TRUE, dot_guide_size=0.25) + 
  labs(x="", y="", title="") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#ffffff"),
        panel.background=element_rect(fill="#ffffff"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_line(linetype=2),
        panel.grid.major.x=element_blank(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg_psm)

# extract matched data
psm_matched_data <- match.data(psm_match) #6767
psm_matched_data$Index <- 1:nrow(psm_matched_data)

# merge data and create factors
psmPanel <- merge(ELC, psm_matched_data, by = 'ID') #1,170,765
psmPanel <- psmPanel %>% 
  mutate(NormConsumption = Consumption/size,
         Treatment = ifelse(InitialPeriod<=Period,1,0)) # Treatment variable encompass both the fact of being treated and the time of treatment - turns out 1 when the treated unit gets treatment
psmPanel <- psmPanel %>% mutate(Treatment = ifelse(is.na(Treatment),0,Treatment))
table(psmPanel$Treatment)
psmPanel <- psmPanel %>% filter(NormConsumption <= quantile(NormConsumption, c(0.9999), na.rm = TRUE)) 

# by group
psm <- plm(log(NormConsumption) ~ Treatment + yearELC + monthELC + CoolingDays + HeatingDays, data = psmPanel, model = 'within', index = c('ID','Period')) # no weights included as the results are almost identical (-0.0565 vs.-0.0562) but no clustering allowed for a weighted panel regression with fixed effects 
summary(psm) 
coeftest(psm, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))
resexp(coeftest(psm, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),1,1) 
confint(coeftest(psm, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(psm, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),1,2)

# placebo testing
psmPanel <- psmPanel %>% mutate(Treatment_placebo = ifelse(Group==1 & 20<Period,1,0)) # Treatment assigned to the middle of the pre-treatment period
psm.pl <- plm(log(NormConsumption) ~ Treatment_placebo + yearELC + monthELC + CoolingDays + HeatingDays, data = psmPanel, model = 'within', index = c('ID','Period'))
summary(psm.pl) 
coeftest(psm.pl, vcov=function(x) vcovHC(x, cluster="group",type="HC0")) # not sign
resexp(coeftest(psm.pl, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),1,1) 
confint(coeftest(psm.pl, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(psm.pl, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),1,2)

# by program
psmPanel <- psmPanel %>% 
  mutate(TreatProgram = case_when(Treatment==0 ~ 'None', 
                                  Treatment==1 & Program == 'CDBG' ~ 'CDBG', 
                                  Treatment==1 & Program == 'HOME' ~ 'HOME'))

psmPanel$TreatProgram <- factor(psmPanel$TreatProgram, levels = c("None", "CDBG", "HOME"))
psm.program <- plm(log(NormConsumption) ~ TreatProgram + yearELC + monthELC + CoolingDays + HeatingDays, data = psmPanel, model='within', index = c('ID','Period'))  
summary(psm.program)
coeftest(psm.program, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))
resexp(coeftest(psm.program, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),2,1) 
confint(coeftest(psm.program, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(psm.program, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),2,2)

# by project 
psmPanel <- psmPanel %>%
  mutate(TreatProject = case_when(Treatment==0 ~ "None", 
                                  Treatment==1 & Project == "Energy Efficiency" ~ "Energy Efficiency",
                                  Treatment==1 & Project == "Emergency Repairs" ~ "Emergency Repairs",
                                  Treatment==1 & Program == "CDBG" & Project == "Homeowner Rehabilitation" ~ "Homeowner Rehabilitation - CDBG",
                                  Treatment==1 & Program == "CDBG" & Project == "Rental Rehabilitation" ~ "Rental Rehabilitation - CDBG",
                                  Treatment==1 & Program == "HOME" & Project == "Homeowner Rehabilitation" ~ "Homeowner Rehabilitation - HOME",
                                  Treatment==1 & Program == "HOME" & Project == "Rental Rehabilitation" ~ "Rental Rehabilitation - HOME",
                                  Treatment==1 & Project == "Acquisition" ~ "Acquisition",
                                  Treatment==1 & Project == "CHDO" ~ "CHDO",
                                  Treatment==1 & Project == "Beautification" ~ "Beautification",
                                  Treatment==1 & Project == "Demolition" ~ "Demolition",
                                  Treatment==1 & Project == "Hudson Lane Sewer Connections" ~ "Hudson Lane Sewer Connections",
                                  Treatment==1 & Project == "Lead-Based Paint Remediations" ~ "Lead-Based Paint Remediations",
                                  Treatment==1 & Project == "Public Facilities" ~ "Public Facilities",
                                  Treatment==1 & Project == "Public Service" ~ "Public Service",
                                  Treatment==1 & Project == "Relocation" ~ "Relocation",
                                  Treatment==1 & Project %in% c("Tenant Based Rental Assistance","New Construction","Homebuyer Rehab","Acquisition/Rehabilitation","AHOP Financing","Acquisition/Rental Rehabilitation") ~ "Non-energy"))

psmPanel$TreatProject <- factor(psmPanel$TreatProject, levels = c("None","Acquisition","CHDO","Beautification","Demolition","Emergency Repairs","Energy Efficiency",
                                                                  "Homeowner Rehabilitation - CDBG","Homeowner Rehabilitation - HOME","Public Facilities",
                                                                  "Hudson Lane Sewer Connections","Lead-Based Paint Remediations","Public Service","Relocation",
                                                                  "Rental Rehabilitation - CDBG","Rental Rehabilitation - HOME","Non-energy"))

psm.project <- plm(log(NormConsumption) ~ TreatProject + yearELC + monthELC + CoolingDays + HeatingDays, data = psmPanel, model = 'within', index = c('ID','Period'))
summary(psm.project)
coeftest(psm.project, vcov=function(x) vcovHC(x, cluster="group", type="HC0")) 
resexp(coeftest(psm.project, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),15,1) 
confint(coeftest(psm.project, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(psm.project, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),15,2)
 
# non-energy for placebo
psmPanel_NE <- psmPanel %>% filter(!TreatProject %in% c("Energy Efficiency","Emergency Repairs",
                                                        "Homeowner Rehabilitation - CDBG","Rental Rehabilitation - CDBG",
                                                        "Homeowner Rehabilitation - HOME","Rental Rehabilitation - HOME"))
psm.ne <- plm(log(NormConsumption) ~ Treatment + yearELC + monthELC + CoolingDays + HeatingDays, data = psmPanel_NE, model='within', index = c('ID','Period'))
summary(psm.ne) # (+) sign after clustering
coeftest(psm.ne, vcov=function(x) vcovHC(x, cluster="group", type="HC0")) 
resexp(coeftest(psm.ne, vcov=function(x) vcovHC(x, cluster="group", type="HC0")),1,1)
confint(coeftest(psm.ne, vcov=function(x) vcovHC(x, cluster="group", type="HC0")))
resexp(confint(coeftest(psm.ne, vcov=function(x) vcovHC(x, cluster="group", type="HC0"))),1,2)

## MatchingFrontier 

Property_frontier <- subset(PropertyStats, select = c('BaselineConsumption','size','baths','beds','market','PropertyAge', 'Group', 'InitialPeriod', 'InitialQuarter','ID', 'Program', 'Project'))
Property_frontier <- Property_frontier %>%
  mutate(InitialPeriod = replace(InitialPeriod,is.na(InitialPeriod),500), # assign '500' to never-treated units (NA's) to construct Treatment variable next
         Program = replace(Program,is.na(Program),'None'),
         Project = replace(Project,is.na(Project),'None'))
match.on <- colnames(Property_frontier)[!(colnames(Property_frontier) %in% c('Group', 'InitialPeriod', 'InitialQuarter','ID', 'Program', 'Project'))]
frontier <- makeFrontier(dataset=Property_frontier, treatment = 'Group', match.on = match.on, QOI = 'SATT', metric = 'L1') 
frontier_matched_data <- generateDataset(frontier,15824) 
plot(frontier, type = 'l',xlim=c(0,17500),ylim=c(0.05,0.5),ylab = 'Absolute Loss Function',main = '') # Figure S5

## Estimation of the weights and measure of robustness to treatment effect heterogeneity

Y = "log"
G = "ID"
T = "Period"
D = "Treatment"
                        
ELC_property$log <- log(ELC_property$NormConsumption)
genPanel$log <- log(genPanel$NormConsumption)
psmPanel$log <- log(psmPanel$NormConsumption)
                        
twowayfeweights(ELC_property, Y, G, T, D, cmd_type="feTR")
twowayfeweights(genPanel, Y, G, T, D, cmd_type="feTR")
twowayfeweights(psmPanel, Y, G, T, D, cmd_type="feTR")
                        
## Group by quarter: with and without matching

ELC_full <- ELC_property %>% 
                        filter((TreatProject=="None" & Group==0) | Project=="Energy Efficiency" | Project=="Emergency Repairs" | (Project=="Homeowner Rehabilitation" & Program=="HOME")) # without matching
ELC_quarter <- ELC_full %>% 
                        group_by(ID,quarterELC) %>% 
                        summarise(sum=sum(NormConsumption),
                                  Group=max(Group),
                                  yearELC=max(yearELC),
                                  InitialQuarter=max(InitialQuarter),
                                  Treatment=max(Treatment),
                                  sumHeatingDays=sum(HeatingDays),
                                  sumCoolingDays=sum(CoolingDays)) %>%
                        mutate(log=log(sum))

gen_full <- genPanel %>% 
                        filter((TreatProject=="None" & Group==0) | Project=="Energy Efficiency" | Project=="Emergency Repairs" | (Project=="Homeowner Rehabilitation" & Program=="HOME")) # after genetic matching
gen_quarter <- gen_full %>% 
                        group_by(ID,quarterELC) %>% 
                        summarise(sum=sum(NormConsumption),
                                  Group=max(Group),
                                  yearELC=max(yearELC),
                                  InitialQuarter=max(InitialQuarter),
                                  Treatment=max(Treatment),
                                  sumHeatingDays=sum(HeatingDays),
                                  sumCoolingDays=sum(CoolingDays)) %>%
                        mutate(log=log(sum))
                        
psm_full <- psmPanel %>% 
                        filter((TreatProject=="None" & Group==0) | Project=="Energy Efficiency" | Project=="Emergency Repairs" | (Project=="Homeowner Rehabilitation" & Program=="HOME")) # after propensity score matching
psm_quarter <- psm_full %>% 
                        group_by(ID,quarterELC) %>% 
                        summarise(sum=sum(NormConsumption),
                                  Group=max(Group),
                                  yearELC=max(yearELC),
                                  InitialQuarter=max(InitialQuarter),
                                  Treatment=max(Treatment),
                                  sumHeatingDays=sum(HeatingDays),
                                  sumCoolingDays=sum(CoolingDays)) %>%
                        mutate(log=log(sum))

## Difference-in-Differences design by de Chaisemartin & D'Haultfoeuille (2020)
 
set.seed(1)
T = "quarterELC"
controls = c("yearELC","sumCoolingDays","sumHeatingDays")
elc_quarter_log <- DIDmultiplegt::did_multiplegt(ELC_quarter, Y, G, T, D, cluster="ID", controls, brep=50, placebo=14, dynamic=47, covariance=TRUE, average_effect = "simple", parallel=TRUE) # without matching
gen_quarter_log <- DIDmultiplegt::did_multiplegt(gen_quarter, Y, G, T, D, cluster="ID", controls, brep=50, placebo=14, dynamic=47, covariance=TRUE, average_effect = "simple", parallel=TRUE) # after getenic matching
psm_quarter_log <- DIDmultiplegt::did_multiplegt(psm_quarter, Y, G, T, D, cluster="ID", controls, brep=50, placebo=14, dynamic=47, covariance=TRUE, average_effect = "simple", parallel=TRUE) # after propensity score matching

## Difference-in-Differences design by Callaway & Sant'Anna (2020)
                        
elc_full <- att_gt(yname = "log",
                   gname = "InitialQuarter",
                   idname = "ID",
                   tname = "quarterELC",
                   xformla = ~sumCoolingDays+sumHeatingDays,
                   data = ELC_quarter,
                   est_method = "reg", 
                   clustervars = "ID",
                   panel = TRUE)
elc_effects <- aggte(elc_full, type = "dynamic",na.rm = TRUE)
ggdid(elc_effects) # without matching
                        
gen_full <- att_gt(yname = "log",
                 gname = "InitialQuarter",
                 idname = "ID",
                 tname = "quarterELC",
                 xformla = ~sumCoolingDays+sumHeatingDays,
                 data = gen_quarter,
                 est_method = "reg", 
                 clustervars = "ID",
                 panel = TRUE)
gen_effects <- aggte(gen_full, type = "dynamic",na.rm = TRUE)
ggdid(gen_effects) # after genetic matching

psm_full <- att_gt(yname = "log",
                   gname = "InitialQuarter",
                   idname = "ID",
                   tname = "quarterELC",
                   xformla = ~sumCoolingDays+sumHeatingDays,
                   data = psm_quarter,
                   est_method = "reg", 
                   clustervars = "ID",
                   panel = TRUE)
psm_effects <- aggte(psm_full, type = "dynamic",na.rm = TRUE)
ggdid(psm_effects) # after propensity score matching
                        
## Parallel trends assumption - Figure S7

show_plot <- function(dat,label="", show.means=TRUE) {
  gdat<- dat %>%
    group_by(Group, Period, Treatment) %>%
    summarise(Consumption = NormConsumption) %>%
    mutate(Group=ifelse(Group==1,"Treated","Untreated"))
  
  gg <- ggplot(gdat, aes(y=Consumption, x=Period, group=Group, color=factor(Group))) +
    geom_smooth(size=2,aes(fill = Group)) + 
    geom_vline(xintercept=46,linetype="dashed",size=1) + 
    scale_x_continuous(expand = c(0, 0), breaks = seq(0,180,12)) +
    theme_classic() + 
    theme(legend.position="bottom") + 
    xlab("Period") + 
    ylab("Monthly Consumption, KWh/sqft")
  gg
}  
show_plot(psmPanel,show.means = FALSE) 

## Comparison of TWFE and staggered DiD estimators - Figure S6
                        
gen_coef_CS <- as.data.frame(gen_effects$att.egt[58:103])
gen_se_CS <- as.data.frame(gen_effects$se.egt[58:103])
gen_t <- as.data.frame(gen_effects$egt[58:103])
gen_CS <- cbind(gen_t,gen_coef_CS,gen_se_CS)
gen_CS_CM <- cbind(gen_CS,gen_CM) # gen_CM extracted as Coef. and S.E. vectors from DiD results by Chaisemartin & D'Haultfoeuille (2020)
names(gen_CS_CM) <- c("t_CS","coef_CS","se_CS","t_CM","coef_CM","se_CM")

gen_comparison <- ggplot(gen_CS_CM, aes(t_CS)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  geom_hline(yintercept=-8.65, color = '#e8ac51', size=3) +
  geom_hline(yintercept=-12.26, color = '#e8ac51', size=0.5) +
  geom_hline(yintercept=-4.89, color = '#e8ac51', size=0.5) +
  geom_line(aes(y = 100*coef_CS, colour = "coef_CS",size = 0.25)) +
  geom_line(aes(y = 100*(coef_CS+1.96*se_CS), colour = "coef_CS",linetype = "dashed")) +
  geom_line(aes(y = 100*(coef_CS-1.96*se_CS), colour = "coef_CS",linetype = "dashed")) +
  geom_line(aes(y = 100*coef_CM, colour = "coef_CM",size = 0.25)) +
  geom_line(aes(y = 100*(coef_CM+1.96*se_CM), colour = "coef_CM",linetype = "dashed")) +
  geom_line(aes(y = 100*(coef_CM-1.96*se_CM), colour = "coef_CM",linetype = "dashed")) +
  scale_x_continuous(expand = c(0, 0),breaks=seq(0,50,5)) +
  scale_y_continuous(expand = c(0, 0),breaks=seq(-100,25,5)) +
  xlab("Quarter") +
  ylab("Estimates") +
  scale_color_brewer(palette="Paired")
