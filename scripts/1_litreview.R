#Load packages####

#Data manipulation
library(readxl)
library(tidyverse)

#Plotting
library(ggplot2)
library(ggpubr)
library(maps) #spatial data
library(viridis)
library(plotly)
library(scatterpie)
library(ggrepel)
library(classInt)
library(RColorBrewer)


#Load data####
lit <- read_excel("data/review.xlsx")
lit1 <- lit %>% 
        select(-Title, -SearchN, -Notes, -Authors)

# Plot a time series of the Publication number per year by taxonomic group ####
timeseries1 <- lit1 %>%
  count(PublicationYear, Group) %>%
  ggplot(mapping = aes(x = PublicationYear, y = n, color = Group)) +
         geom_point()+
  #geom_smooth(method = "glm", method.args = list(family = "poisson"))+ #
   facet_wrap(facets = vars(Group))+
  labs(title = "",
       x = "Publication Year",
       y = "Count of publications",
       color = "Animal group")+
  theme_bw()+
  theme(text = element_text(size = 15),
        axis.text.x = element_text(colour = "grey20", 
                                   size = 10, angle = 90, 
                                   hjust = 0.5, vjust = 0.5),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(n.breaks = 7)

ggsave("figures/fig_time_taxa.png", 
       plot = timeseries1,
       height = 4.25,
       width = 8)

#Plot time series of publications, show MMEs that have repeated studies ####
#Timeseries of MMEs ####
timeseries_mme <- lit1 %>% 
  filter(Year != "NA") %>% 
  group_by(PublicationYear, Individual_or_not) %>% 
  tally() %>% 
  mutate(indiv = case_when(Individual_or_not == "DA-Caribbean" ~ "Sea urchin 1983-1984",
                           Individual_or_not == "1984 sika deer MME" ~ "Deer Japan",
                           Individual_or_not == "Frog MME 1993" ~ "Frogs 1993",
                           Individual_or_not == "Pilchard MME 1994-95 and 1998" ~ "Pilchards 90s",
                           Individual_or_not == "Monk seal 1997" ~ "Monk seal 1997",
                           Individual_or_not == "Fish kills Northern NSW" ~ "Fish kills Northern NSW",
                           Individual_or_not == "2003 Europe heatwave" ~        "2003 Europe heatwave",
                           Individual_or_not == "2004 amphibian MME"    ~ "2004 amphibian MME",
                           Individual_or_not == "Mediterranean invertebrates" ~ "Mediterranean invertebrates",
                           Individual_or_not == "2011 inverts North America" ~ "Invertebrates North America",
                           Individual_or_not == "2013 Sea star wasting disease" ~ "Sea star MME",
                           Individual_or_not == "Auklets 2014 MME"  ~  "Seabird 2014",
                           Individual_or_not == "2015 Murre MME"  ~    "Seabird 2015",                      
                           Individual_or_not == "Pinna nobilis MMEs" ~    "Pinna nobilis MMEs",
                           Individual_or_not == "Menindee fish kills" ~ "Fish kills AUS 2018-2019"  ,                   
                           Individual_or_not == "MME experiment USA" ~ "Experiment USA",
                           TRUE ~ "MME Studied once"
                           
  ))

colourCount = length(unique(timeseries_mme$indiv))

getPalette = colorRampPalette(brewer.pal(8, "Accent"))

timeseries_pub <- timeseries_mme %>% 
  ggplot(aes(factor(PublicationYear), n, fill = indiv))+
  geom_col(col = "black")+
  theme_classic()+
  labs(x = "Publication Year",
       y = "Count of publications",
       title = "",
       fill = "Studied once vs multiple times"
  )+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_y_continuous(n.breaks = 7, limits = c(0, 30))+
  theme(axis.text.x = element_text(colour = "grey20", 
                                   size = 10, angle = 90, 
                                   hjust = 0.5, vjust = 0.5))

timeseries_pub

ggsave("figures/fig_time_pub.png", 
       plot = timeseries_pub,
       height = 5,
       width = 10)

#Plot publication years and lag between MME and publication####
#Focus on research articles, calculate publication lag
mme_research <- lit1 %>% 
  # filter(Year != "Multiple", #Exclude studies that look at multiple MMEs
  #        ArticleType == "Research article" #Only look at research articles
  # ) %>% 
  mutate(Pub_lag = as.numeric(PublicationYear) - as.numeric(Year)) #get publication lag

#Exclude studies for which lag could not be calculated (NAs)
mme_lag <- mme_research %>% 
  filter(Pub_lag != "NA") %>% #312 publications
  mutate(Individual_or_not = case_when(Individual_or_not == "DA-Caribbean" ~ "Sea urchin 1983-1984",
                           Individual_or_not == "1984 sika deer MME" ~ "Deer Japan",
                           Individual_or_not == "Frog MME 1993" ~ "Frogs 1993",
                           Individual_or_not == "Pilchard MME 1994-95 and 1998" ~ "Pilchards 90s",
                           Individual_or_not == "Monk seal 1997" ~ "Monk seal 1997",
                           Individual_or_not == "Fish kills Northern NSW" ~ "Fish kills Northern NSW",
                           Individual_or_not == "2003 Europe heatwave" ~        "2003 Europe heatwave",
                           Individual_or_not == "2004 amphibian MME"    ~ "2004 amphibian MME",
                           Individual_or_not == "Mediterranean invertebrates" ~ "Mediterranean invertebrates",
                           Individual_or_not == "2011 inverts North America" ~ "Invertebrates North America",
                           Individual_or_not == "2013 Sea star wasting disease" ~ "Sea star MME",
                           Individual_or_not == "Auklets 2014 MME"  ~  "Seabird 2014",
                           Individual_or_not == "2015 Murre MME"  ~    "Seabird 2015",                      
                           Individual_or_not == "Pinna nobilis MMEs" ~    "Pinna nobilis MMEs",
                           Individual_or_not == "Menindee fish kills" ~ "Fish kills AUS 2018-2019"  ,                   
                           Individual_or_not == "MME experiment USA" ~ "Experiment USA",
                           TRUE ~ "MME Studied once"
                           
  ))
#plot Publication lag agains year of publication

lag_plot <- mme_lag %>% 
  ggplot(aes(PublicationYear, Pub_lag))+
  geom_point(aes(col = Individual_or_not), size = 2.5)+
  theme_classic()+
  labs(title = "",
       x = "Publication year",
       y = "Publication lag (years)",
       col = "Studied once vs multiple times")+
  # geom_smooth(aes(PublicationYear, Pub_lag), method = "glm", method.args = list(family = "poisson"), col = "black", se = F)+
  scale_colour_manual(values = getPalette(colourCount))+
  theme(legend.key.height = unit(0.5,"cm"))

lag_plot

ggsave("figures/fig_pub_lag.png", 
       plot = lag_plot,
       height = 4,
       width = 8)

# # Plot map of publications####
require(maps)
require(viridis)
#get the data needed. World map, and names of continents for each region
world_map <- map_data("world")
continent.df <-  read.csv("useful/map-data_litrev.csv")

#Select the data you need to plot on the map.
#I decided to exclude Antarctica because there's no publications to show there
lit_region_data <- left_join(continent.df, world_map, by = "region") %>%
                  filter(Continent != "Indian Ocean islands",
                         Continent != "Atlantic islands")

# Count publications for data grouped by region, and clean up
region_data <- lit1 %>%
  mutate(Region = gsub("Australia and New Zealand", "Australasia", Region),
         Region = gsub("Pacific Islands", "Australasia", Region)) %>% 
  count(Region) %>%
  rename(Continent = Region)
  

#Calc centroids for the different geographical groups
continent.centroids <- lit_region_data %>%
  group_by(Continent) %>%
  summarise(long = mean(long), lat = mean(lat)) %>%
  filter(Continent != "Australasia",
         Continent != "Antarctica") %>% 
  mutate(Continent = gsub("Australia and New Zealand", "Australasia", Continent))
# #Calc centroid for Pacific Islands based on PNG
# pacific.centroid <- lit_region_data %>%
#   group_by(Continent) %>%
#   filter(Continent == "Pacific Islands",
#          region == "Solomon Islands")%>%
#   summarise(long = mean(long), lat = mean(lat))

#Calculate a centroid in Atlantic Ocean to place "Global" papers
global.centroid <- lit_region_data %>%
  group_by(Continent) %>%
  filter(Continent == "Antarctica" | Continent == "Africa" | Continent == "South America")%>%
  mutate(Continent = gsub("Antarctica", "Global", Continent),
         Continent = gsub("Africa", "Global", Continent),
         Continent = gsub("South America", "Global", Continent)) %>%
  summarise(long = mean(long), lat = mean(lat))

#Merge all centroids into one dataframe
centroids <-  rbind(continent.centroids, global.centroid)

#Now, left join centroids with publication data.
data_byregion <- left_join(centroids, region_data, by = "Continent")

#Count the number of publication by cause and region
cause_byregion <- lit1 %>%
    mutate(Region = gsub("Australia and New Zealand", "Australasia", Region),
         Region = gsub("Pacific Islands", "Australasia", Region)) %>%
   count(Region, Cause) %>%
   rename(Continent = Region) %>% 
  filter(Continent != "Antarctica")

# cause_byregion_before2015 <- lit1 %>%
#   filter(PublicationYear < 2015) %>% 
#   count(Region, Cause) %>%
#   rename(Continent = Region) %>%
#   mutate(Continent = gsub("Australasia", "Australia and New Zealand", Continent)) %>%
#   filter(Continent != "Antarctica")
# 
# cause_byregion_after2015 <- lit1 %>%
#   filter(PublicationYear >= 2015) %>% 
#   count(Region, Cause) %>%
#   rename(Continent = Region) %>%
#   mutate(Continent = gsub("Australasia", "Australia and New Zealand", Continent)) %>%
#   filter(Continent != "Antarctica")


#Join the dataframe above with the centroids one
#This is all the data I need to put in the map for now
cause_region_data <- left_join(centroids, cause_byregion, by = "Continent")

# cause_region_data_before2015 <- left_join(centroids, cause_byregion_before2015, by = "Continent")
# cause_region_data_after2015 <- left_join(centroids, cause_byregion_after2015, by = "Continent")

#Remove Antarctica from the map
lit_region_data <- lit_region_data %>%
              filter(Continent != "Antarctica")

#Get the data ready for pie charts. Needs to be pivoted wider
pie_data <- pivot_wider(cause_region_data,
                        names_from = Cause,
                        values_from = n)

pie_data[is.na(pie_data)] = 0 #replace all NA values with zeroes
pie_data <- pie_data %>% rename("Theoretical papers, no cause"="NA") #rename NA column
pie_data <- left_join(pie_data, region_data)
pie_data <- pie_data %>%
            mutate(radius = 2*log(n))

# pie_data_before2015 <- pivot_wider(cause_region_data_before2015,
#                         names_from = Cause,
#                         values_from = n)
# pie_data_before2015[is.na(pie_data_before2015)] = 0 #replace all NA values with zeroes
# pie_data_before2015 <- pie_data_before2015 %>% rename("Theoretical papers, no cause"="NA") #rename NA column
# pie_data_before2015 <- pie_data_before2015 %>%
#   mutate(n = rowSums(pie_data_before2015[, c(4:16)]),
#          radius = 2*log(n))
# 
# 
# pie_data_after2015 <- pivot_wider(cause_region_data_after2015,
#                                    names_from = Cause,
#                                    values_from = n)
# pie_data_after2015[is.na(pie_data_after2015)] = 0 #replace all NA values with zeroes
# pie_data_after2015 <- pie_data_after2015 %>% rename("Theoretical papers, no cause"="NA") #rename NA column
# pie_data_after2015 <- pie_data_after2015 %>%
#   mutate(n = rowSums(pie_data_after2015[, c(4:16)]),
#     radius = 2*log(n))


# Remove all the unnecessary items from environment
rm(cause_byregion, continent.df, centroids, region_data,
   world_map, data_byregion, continent.centroids,
   global.centroid)

# use this code to determine the palette and extend it to the data needed
colourCount = length(colnames(pie_data[,4:17])) #this is to select the number of colours needed
getPalette = colorRampPalette(brewer.pal(10, "Paired"))

theme_set(theme_void())
ggplot(lit_region_data, aes(x = long, y = lat, group = group))+
  geom_polygon(fill= "dark grey", color = "black") + #remember that fill or color need to be outside aes()
  geom_scatterpie(data = pie_data,
                  mapping = aes(x = long, y = lat,
                                group = Continent,
                                r = radius),
                  cols = colnames(pie_data[,4:16]))+
   scale_fill_manual(values = getPalette(colourCount))+ #apply different palette to pie charts
    theme(legend.position = "right",
          plot.background = element_rect(fill = "white"))+
    labs(fill = "Cause of MME",
       title = "")+
  geom_text(data = pie_data,
                  aes(x= long, y = lat,
                      group = Continent,
                      label = n,
                      fontface = "bold"),
            color = "white", #fill or color need to be outside aes() to work properly
            size = 3.5,
            show.legend = FALSE
                  )+
  annotate("text",label = "Global", x= -19, y = -8)
ggsave("figures/fig_map.png", width = 11.5, height = 4)

# theme_set(theme_void())
# ggplot(lit_region_data, aes(x = long, y = lat, group = group))+
#   geom_polygon(fill= "dark grey", color = "black") + #remember that fill or color need to be outside aes()
#   geom_scatterpie(data = pie_data_before2015,
#                   mapping = aes(x = long, y = lat,
#                                 group = Continent,
#                                 r = radius),
#                   cols = colnames(pie_data_before2015[,4:16]))+
#   scale_fill_manual(values = getPalette(colourCount))+ #apply different palette to pie charts
#   theme(legend.position = "right",
#         plot.background = element_rect(fill = "white"))+
#   labs(fill = "Cause of MME",
#        title = "Publications about MMEs grouped by region and cause before 2015")+
#   geom_text(data = pie_data_before2015,
#             aes(x= long, y = lat,
#                 group = Continent,
#                 label = n,
#                 fontface = "bold"),
#             color = "white", #fill or color need to be outside aes() to work properly
#             size = 3.5,
#             show.legend = FALSE
#   )+
#   annotate("text",label = "Global", x= -19, y = -8)
# ggsave("figures/fig_map_pre2015.png", width = 10.5, height = 4.4)
# 
# theme_set(theme_void())
# ggplot(lit_region_data, aes(x = long, y = lat, group = group))+
#   geom_polygon(fill= "dark grey", color = "black") + #remember that fill or color need to be outside aes()
#   geom_scatterpie(data = pie_data_after2015,
#                   mapping = aes(x = long, y = lat,
#                                 group = Continent,
#                                 r = radius),
#                   cols = colnames(pie_data_after2015[,4:16]))+
#   scale_fill_manual(values = getPalette(colourCount))+ #apply different palette to pie charts
#   theme(legend.position = "right",
#         plot.background = element_rect(fill = "white"))+
#   labs(fill = "Cause of MME",
#        title = "Publications about MMEs grouped by region and cause after 2015")+
#   geom_text(data = pie_data_after2015,
#             aes(x= long, y = lat,
#                 group = Continent,
#                 label = n,
#                 fontface = "bold"),
#             color = "white", #fill or color need to be outside aes() to work properly
#             size = 3.5,
#             show.legend = FALSE
#   )+
#   annotate("text",label = "Global", x= -19, y = -8)
# 
# ggsave("figures/fig_map_post2015.png", width = 10.5, height = 4.4)


#MME Magnitudes over time, and by region####

#Magnitude vs time
mme_mag <- lit1 %>%
  filter(MME_Magnitude != "NA") %>% #Exclude all the publications without data on magnitude
  mutate(Region = gsub("Australasia", "Australia/New Zealand", Region),
         Magnitude = case_when(MME_Magnitude == "Up to 22% mortality"|MME_Magnitude == "30% phylogenetic loss" |
                                 MME_Magnitude == "Up to 30% decline" |MME_Magnitude == "Up to 40% decline" ~ "0 to 30%",
                               MME_Magnitude == "Up to 44.5% mortality"|MME_Magnitude == "Up to 50% declines" |
                                 MME_Magnitude == "Up to 50% mortality" ~ "31 to 60% ",
                               MME_Magnitude == "Up to 65% decline" |MME_Magnitude == "Up to 75% decline" |MME_Magnitude == "Up to 80% mortality" ~ "61 to 80%",
                               MME_Magnitude == "Up to 88% decline" |MME_Magnitude == "Up to 89% decline" |
                                 MME_Magnitude == "Up to 90% decline" |MME_Magnitude == "Up to 90% mortality" |MME_Magnitude == "Up to 95% mortality" |
                                 MME_Magnitude == "Up to 93% mortality" |MME_Magnitude == "Up to 99% mortality" |MME_Magnitude == "Up to 100% mortality" |
                                 MME_Magnitude == "Up to 100% decline" ~ "81 to 100%",
                               MME_Magnitude == "Tens" ~ "10s",
                               MME_Magnitude == "Hundreds" ~ "100s",
                               MME_Magnitude == "Hundreds to Thousands"~ "100s to 1,000s",
                               MME_Magnitude == "Tens to hundreds" ~ "10s to 100s",
                               MME_Magnitude == "Hundreds of thousands"~ "100,000s",
                               MME_Magnitude == "Tens of Thousands" ~ "10,000s",
                               MME_Magnitude == "Tens of thousands" ~ "10,000s",
                               MME_Magnitude == "Thousands" ~ "1,000s",
                               MME_Magnitude == "Millions" ~ "1,000,000s",
                               TRUE ~ "Unknown"))
#364 publications
#Now, group by individual_or_not and keep first row for each group, so that the studies that are
#about the same MME are not counted as individual

indiv_events <- mme_mag %>%
  group_by(Year, Group, Individual_or_not) %>%
  filter(Individual_or_not == "Individual") %>%
  ungroup()
#publications regarding the same event: discard all but one
multiple_papers <- mme_mag %>%
  group_by(Group, Individual_or_not) %>%
  filter(Individual_or_not != "NA",
         Individual_or_not != "MME experiment USA",
         Individual_or_not != "Individual") %>%
  slice(1) %>%
  ungroup()

mme_mag1 <- rbind(indiv_events, multiple_papers) %>%  #315 papers
  mutate(Region = gsub("Australia/New Zealand", "Australasia", Region),
         Region = gsub("Pacific Islands", "Australasia", Region))
#Now, plot
mag_plot_data1 <- mme_mag1 %>% 
  filter(
    Magnitude == "10s"| 
      Magnitude == "100s" |
      Magnitude == "100s to 1,000s"|
      Magnitude == "10s to 100s"|
      Magnitude == "100,000s"|
      Magnitude == "10,000s"|
      Magnitude == "10,000s"|
      Magnitude == "1,000s"|
      Magnitude == "1,000,000s"
  ) %>%   
  mutate(Year = as.numeric(Year),
         Region = as.factor(Region),
         Magnitude = as.factor(Magnitude)) %>% 
  group_by(Year, Region, Magnitude) %>% 
  tally() %>%
  filter(Year != "NA") #remove all records that don't have a MME year


mag_plot1 <- mag_plot_data1 %>% 
  filter(Region != "Global") %>% 
  ggplot(aes(Year, n,
             col = Magnitude)
  )+
  geom_point()+
  theme_bw()+
  scale_color_brewer(palette = "RdYlBu")+
  scale_y_continuous(n.breaks = 7) +
  scale_x_continuous(n.breaks = 8) +
  labs(title = "",
       x = "MME year",
       y = "Count of publications",
       fill = "MME magnitude (n)") +
  facet_wrap(facets = vars(Region))+
  theme(axis.text.x = element_text(colour = "grey20", 
                                   size = 8, angle = 90, 
                                   hjust = 0.5, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
mag_plot1


mag_plot_data2 <- mme_mag1 %>% 
  filter(
    Magnitude == "0 to 30%"|
      Magnitude == "31 to 60% "|
      Magnitude == "61 to 80%"|
      Magnitude == "81 to 100%"|
      Magnitude == "Unknown"
  ) %>%   
  #remove all records that don't have a MME year
  mutate(Year = as.numeric(Year),
         Region = as.factor(Region),
         Magnitude = as.factor(Magnitude)) %>% 
  group_by(Year, Region, Magnitude) %>% 
  tally()%>%
  filter(Year != "NA") 

mag_plot2 <- mag_plot_data2 %>% 
  filter(Region != "South America") %>% 
  ggplot(aes(Year, n,
             col = Magnitude)
  )+theme_bw()+
  theme(axis.text.x = element_text(colour = "grey20", 
                                   size = 8, angle = 90, 
                                   hjust = 0.5, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_point()+
  scale_color_brewer(palette = "RdYlBu")+
  scale_y_continuous(n.breaks = 2) +
  scale_x_continuous(n.breaks = 8) +
  labs(title = "",
       x = "MME year",
       y = "Count of publications",
       fill = "MME magnitude (n)") +
  facet_wrap(facets = vars(Region)) 
mag_plot2

mag_plot <- ggarrange(mag_plot1, mag_plot2
)


ggsave("figures/fig_time_mag.png", mag_plot,
       width = 14,
       height = 5.5
)

mag2_data <- mme_mag1 %>% 
  group_by(Year, Magnitude) %>% 
  tally()


time_mag1 <- mag2_data %>% 
  filter(
    Magnitude == "10s"| 
      Magnitude == "100s" |
      Magnitude == "100s to 1,000s"|
      Magnitude == "10s to 100s"|
      Magnitude == "100,000s"|
      Magnitude == "10,000s"|
      Magnitude == "10,000s"|
      Magnitude == "1,000s"|
      Magnitude == "1,000,000s"
  ) %>% 
  ggplot(aes(Year, n,
              fill = Magnitude))+
  geom_col(col = "black")+ theme_classic()+
  labs(title = "",
       x = "MME year",
       y = "Count of publications",
       fill = "MME magnitude (n)")+
  theme(axis.text.x = element_text(angle = 90,
                      hjust = 0.5, vjust = 0.5))+
  scale_fill_brewer(palette = "RdYlBu")
time_mag1


time_mag2 <- mag2_data %>% 
  filter(
    Magnitude == "0 to 30%"|
    Magnitude == "31 to 60% "|
    Magnitude == "61 to 80%"|
    Magnitude == "81 to 100%"|
    Magnitude == "Unknown"
    ) %>% 
  ggplot(aes(Year, n,
             fill = Magnitude))+
  geom_col(col = "black")+ theme_classic()+
  labs(title = "",
       x = "MME year",
       y = "Count of publications",
       fill = "MME magnitude (n)")+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 0.5, vjust = 0.5))+
  scale_fill_brewer(palette = "RdYlBu")
time_mag2

time_mag <- ggarrange(time_mag1, time_mag2,
                      labels = "AUTO"
                      )


ggsave("figures/time_mag.png", time_mag,
       width = 12,
       height = 5
)
