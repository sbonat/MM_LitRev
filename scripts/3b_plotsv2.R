#Load libraries ####
#Data manipulation
library(readxl)
library(tidyverse)

#Graphics
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(RColorBrewer)

#Load data####
lit <- read_excel("data/review.xlsx")
lit1 <- lit %>% 
  select(-Title, -SearchN, -Notes, -Authors)
journal_trend <- read_excel("data/review.xlsx", sheet = "pub_year_journals") %>% 
  slice(9)

#Pie charts for topic, studies done once vs multiple times, causes

topic_pie <- lit1 %>% 
mutate(ArticleTopic = case_when(ArticleTopic == "Dispute of other article"|
                                  ArticleTopic == "Estimation of pre-MME population"|
                                  ArticleTopic == "Estimation of post-MME population"|
                                  ArticleTopic == "General observations"|
                                  ArticleTopic == "Public survey" ~ "Other",
                                ArticleTopic == "Flow-on effects"~ "Ecosystem responses",
                                TRUE ~ ArticleTopic))%>% 
  group_by(ArticleTopic) %>% 
  tally() %>% 
  # mutate(avg_n = round(n/33, digits = 2)) %>% #average per year
  ungroup() 

pie_topic <- ggplot(topic_pie, aes(x = 1, y = n, fill = ArticleTopic)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
   theme_void()+
  labs(fill = "Study topic")+
  scale_fill_brewer(palette = "RdYlBu")+
  geom_label(
    aes(label = n, x = 1.5),
    position = position_stack(vjust = 0.8),
    show.legend = FALSE
  )
pie_topic

ggsave("figures/pie_topic.png", pie_topic,
       width = 7,
       height = 4,
       bg = "white"
)


# #Studied once vs multiple times
# studied_pie <- lit1 %>% 
#   group_by(Individual_or_not) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   mutate(Individual_or_not = gsub("NA", "Other", Individual_or_not))

# pie_studied <- studied_pie %>% 
#   ggplot(aes(x = 1, y = n, fill = Individual_or_not)) +
#   geom_bar(stat = "identity", width = 1, color = "black") +
#   coord_polar(theta = "y") +
#   theme_void() +
#   labs(fill = "MMEs studied once vs multiple studies")#+
# # geom_label(
# #   aes(label = n, x = 1.2),
# #   position = position_stack(vjust = 0.5),
# #   show.legend = FALSE
# # )
# pie_studied
# 
# type <- lit1 %>% 
#   group_by(ArticleType) %>% 
#   tally() %>% 
#   mutate(ArticleType = case_when(ArticleType == "General article" | 
#                                    ArticleType == "Scientific correspondence" |
#                                    ArticleType == "Scientific correspondence" |
#                                    ArticleType == "Opinion article" |
#                                    ArticleType == "Commentary" |
#                                    ArticleType == "Letter" |
#                                    ArticleType == "Newsletter" ~ "Other",
#                                  TRUE ~ ArticleType)) %>% 
#   ungroup() %>% 
#   mutate(avg_n = round(n/33, digits = 2))
# 
# pie_type <- type %>% 
#   ggplot(aes(x = 1, y = avg_n, fill = ArticleType)) +
#   geom_bar(stat = "identity", width = 1, color = "black") +
#   coord_polar(theta = "y") +
#   theme_void() +
#   labs(fill = "Article type")+
#   scale_fill_manual(values = getPalette(colourCount))+
#   geom_label(
#     aes(label = avg_n, x = 1.5),
#     position = position_stack(vjust = 0.8),
#     show.legend = FALSE
#   )
# pie_type


#MME causes on average per year
causes_pie <- lit1 %>% 
  mutate(Cause = gsub("NA", "Theory", Cause)) %>% 
  group_by(Cause) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(avg_n = round(n/33, digits = 2))#Average per year

colourCount = length(unique(causes_pie$Cause))
getPalette = colorRampPalette(brewer.pal(8, "RdYlBu"))


pie_causes <- causes_pie %>% 
  ggplot(aes(x = 1, y = avg_n, fill = Cause)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(fill = "Cause")+
  scale_fill_manual(values = getPalette(colourCount))+
  geom_label(
    aes(label = avg_n, x = 1.5),
    position = position_stack(vjust = 0.8),
    show.legend = FALSE
  )
pie_causes

pie_topic_causes <- ggarrange(pie_topic, pie_causes)
pie_topic_causes
ggsave("figures/pie_topic_causes.png", pie_topic_causes,
       width = 13,
       height = 5.5,
       bg = "white"
)

#Magnitude pie charts####

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
  group_by(Magnitude) %>% 
  tally() %>% 
  ungroup()
    
mag_plot1 <- mag_plot_data1 %>% 
  ggplot(aes(x = 1, y = n, fill = Magnitude)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(fill = "Magnitude (Estimated n)")+
  scale_fill_brewer(palette = "RdYlBu")+
  geom_label(
    aes(label = n, x = 1.5),
    position = position_stack(vjust = 0.5),
    show.legend = FALSE
  )
mag_plot1


mag_plot_data2 <- mme_mag1 %>% 
  filter(
    Magnitude == "0 to 30%"|
      Magnitude == "31 to 60% "|
      Magnitude == "61 to 80%"|
      Magnitude == "81 to 100%"|
      Magnitude == "Unknown"
  ) %>%  
  group_by(Magnitude) %>% 
  tally() %>% 
  ungroup() 
  

mag_plot2 <- mag_plot_data2 %>% 
  ggplot(aes(x = 1, y = n, fill = Magnitude)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(fill = "Magnitude (% dead)")+
  scale_fill_brewer(palette = "RdYlBu")+
  geom_label(
    aes(label = n, x = 1.5),
    position = position_stack(vjust = 0.5),
    show.legend = FALSE
  )

mag_plot2

mag_plot <- ggarrange(mag_plot1, mag_plot2,
                      labels = c("A", "B")
)


ggsave("figures/pie_mag.png", mag_plot,
       width = 11,
       height = 5.5,
       bg = "white"
       )

