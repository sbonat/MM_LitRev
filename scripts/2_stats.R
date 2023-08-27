#Load libraries ####
#Data manipulation
library(readxl)
library(tidyverse)
library(broom)

#Graphics
library(ggplot2)

#Statistics
library(fitdistrplus)
library(MuMIn)
library(predictmeans)
library(lmPerm)

library(vegan)
library(cluster)

#Load data####
lit <- read_excel("data/review.xlsx")
lit1 <- lit %>% 
  dplyr::select(-Title, -SearchN, -Notes, -Authors)
journal_trend <- read_excel("data/review.xlsx", sheet = "pub_year_journals") %>% 
  slice(9)

#Data manipulation: MME numbers####

#Get the number of publications per year of publication
mme_numbers <- lit1 %>% 
  group_by(PublicationYear, Year, Group, Region, Cause) %>% 
  tally() %>% #Get the number of MMEs for each publication year
  # filter(Year != "NA") %>% 
  mutate(Year = as.numeric(PublicationYear)) %>% 
  #Get a before/after 2015 category
  mutate(PublicationYear = as.numeric(PublicationYear),
         Region = gsub("Australia/New Zealand", "Australasia", Region),
         Region = gsub("Pacific Islands", "Australasia", Region),
         PubTime = case_when(
           PublicationYear < 1996 ~ "Before 1995",
           PublicationYear > 1995 & PublicationYear < 2001 ~ "1996 to 2000",
           PublicationYear > 2000 & PublicationYear < 2006 ~ "2001 to 2005",
           PublicationYear > 2005 & PublicationYear < 2011 ~ "2006 to 2010",
           PublicationYear > 2010 & PublicationYear < 2016 ~ "2011 to 2015",
           PublicationYear > 2015~ "2016 to present",
           TRUE ~ "NA")
  ) %>% 
  ungroup() %>% 
  dplyr::select(-PublicationYear) %>% #Year is not necessary, we are interested in Before/After 2015
  group_by(Group, Region, Cause, PubTime) %>% 
  tally() %>% 
  mutate(Group = as.factor(Group),
         Region = as.factor(Region),
         Cause = as.factor(Cause),
         PubTime = as.factor(PubTime))


#Statistical analysis: mme numbers ####

#Check distribution
hist(mme_numbers$n) #very little variation. 

#Poisson GLM for lag
hist(mme_lag$Pub_lag)
lag_m <- glm(PublicationYear ~ Pub_lag,
             family = poisson,
            data = mme_lag)
summary(lag_m)
#validation
residplot(lag_m, newwd = F) #These look super weird. Maybe poisson GLM not good

aovp_lag <- aovp(PublicationYear ~Pub_lag, data = mme_lag)
summary(aovp_lag)

#PERMUTATIONAL ANOVAs ####
#Cannot do normal ANOVAs as the residuals are not normally distributed. See below
anova1 <- aov(n ~ PubTime, data = mme_numbers)
summary.aovp(anova1)
residplot(anova1, newwd = F) #residuals are not normal. Assumptions are not met

#Need to adopt a permutational approach

#AOVP and model selection####

aovp1 <- aovp(n ~ (Cause + Region + Group + PubTime)^2,
              data = mme_numbers)
summary.aovp(aovp1)

drop1

#Drop Cause:Region
aovp2 <- aovp(n ~ (Cause + Region + Group + PubTime)^2 - Cause:Region,
              data = mme_numbers)
summary.aovp(aovp2)

drop1(aovp2)

#Drop Group:PubTime
aovp3 <- aovp(n ~ (Cause + Region + Group + PubTime)^2 - Cause:Region - Group:PubTime,
              data = mme_numbers)
summary.aovp(aovp3)

drop1(aovp3)

#Drop Region:Group
aovp4 <- aovp(n ~ (Cause + Region + Group + PubTime)^2 - Cause:Region - Group:PubTime - Region:Group,
              data = mme_numbers)
summary.aovp(aovp4)

drop1(aovp4)

#Drop Cause:PubTime
aovp5 <- aovp(n ~ Cause + Region + Group + PubTime + Cause*Group + Region*PubTime,
              data = mme_numbers)
summary.aovp(aovp5)

drop1(aovp5)

#Drop Region:PubTime
aovp6 <- aovp(n ~ Cause + Region + Group + PubTime + Cause*Group,
              data = mme_numbers)
summary.aovp(aovp6)

drop1(aovp6)

#Drop Cause:Group
aovp7 <- aovp(n ~ Cause + Region + Group + PubTime,
              data = mme_numbers)
summary.aovp(aovp7)

drop1(aovp7)

#Drop Region
aovp8 <- aovp(n ~ Cause + Group + PubTime,
              data = mme_numbers)
summary.aovp(aovp8)

#Drop Group and keep Region
aovp9 <- aovp(n ~ Cause + Region + PubTime,
              data = mme_numbers)
summary.aovp(aovp9)

#Drop Group and Region
aovp10 <- aovp(n ~Cause + PubTime,
             data = mme_numbers)
summary.aovp(aovp10)

AICc(aovp7, aovp8, aovp9, aovp10)  #aovp8 is the best fit, although pretty close to aovp10

aovp_results <- tidy(aovp8) %>% 
  mutate(across(3:6, ~round(.x, digits = 3)))



# AOVP Magnitude and year####
#Get mme_mag1 data frame from the 1_litreview script
mme_magnitude <- mme_mag1 %>% 
  group_by(PublicationYear, Magnitude) %>% 
  tally() %>% 
  mutate(PublicationYear = as.numeric(PublicationYear),
         PubTime = case_when(
           PublicationYear < 1996 ~ "Before 1995",
           PublicationYear > 1995 & PublicationYear < 2001 ~ "1996 to 2000",
           PublicationYear > 2000 & PublicationYear < 2006 ~ "2001 to 2005",
           PublicationYear > 2005 & PublicationYear < 2011 ~ "2006 to 2010",
           PublicationYear > 2010 & PublicationYear < 2016 ~ "2011 to 2015",
           PublicationYear > 2015~ "2016 to present",
           TRUE ~ "NA")
  ) %>% 
  ungroup() %>% 
  dplyr::select(-PublicationYear)

#GLM with poisson distribution
a6a <- aovp(n ~ PubTime*Magnitude,
           data = mme_magnitude)
summary.aovp(a6a)

#Drop interaction
a6b <- aovp(n ~ PubTime+Magnitude,
           data = mme_magnitude)
summary.aovp(a6b) #magnitude significant

#Trend analysis####
journal_trend1 <- journal_trend %>% 
  pivot_longer(cols = -Journal,
               names_to = "year",
               values_to = "avg_pub_n") %>% 
  select(-1) %>% 
  mutate(avg_pub_n = round(avg_pub_n))

mme_per_year <- lit1 %>% 
  group_by(PublicationYear) %>% 
  tally() %>% 
  rename("year" = "PublicationYear") %>% 
  ungroup()

trends <- merge(journal_trend1, mme_per_year, by = "year") %>% 
  mutate(normalised = (n/avg_pub_n)*100, 
         year = as.integer(year))
str(trends)

regression <-lm(formula = year ~ normalised,
                data=trends)                      
summary(regression)

trend_plot <- trends %>% 
  ggplot(aes(year, normalised))+
  geom_point() +
  labs(x= "Year",
       y = "Percentage of MME studies over average total publications")+
  theme_bw()+
  theme(axis.text.x = element_text(colour = "grey20", 
                                   size = 10, angle = 90, 
                                   hjust = 0.5, vjust = 0.5)) +
  geom_smooth(method = "lm", se = F)+
  scale_x_continuous(breaks = trends$year, labels = trends$year)

trend_plot

ggsave("figures/trend_plot.png", trend_plot,
       height = 5,
       width = 10,
      bg = "white")

