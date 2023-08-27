#Load libraries ####
#Data manipulation
library(readxl)
library(tidyverse)

#Graphics
library(ggplot2)
library(RColorBrewer)

#Load data####
lit <- read_excel("data/review.xlsx")
lit1 <- lit %>% 
  select(-Title, -SearchN, -Notes, -Authors)


#Calculate general numbers of articles by type and do barplot

type <- lit1 %>% 
  group_by(ArticleType) %>% 
  tally()

type <- lit1 %>% 
  group_by(PublicationYear, ArticleType) %>% 
  tally() %>% 
  mutate(ArticleType = case_when(ArticleType == "General article" | 
                                   ArticleType == "Scientific correspondence" |
                                   ArticleType == "Scientific correspondence" |
                                   ArticleType == "Opinion article" |
                                   ArticleType == "Commentary" |
                                   ArticleType == "Letter" |
                                   ArticleType == "Newsletter" ~ "Other",
                                 TRUE ~ ArticleType))

type_plot <- type %>% 
  ggplot(aes(factor(PublicationYear), n, fill = ArticleType))+
  geom_col(col = "black")+ theme_classic()+
  labs(title = "",
       x = "Publication Year",
       y = "Count of publications",
       fill = "Article type") +
  scale_fill_brewer(palette = "RdYlBu")+
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0.5, 
                                   vjust = 0.5)) 

type_plot


ggsave("figures/time_type.png", type_plot,
       width = 10,
       height = 6
       )


topic <- lit1 %>% 
  group_by(ArticleTopic) %>% 
  tally() %>% 
  mutate(percent = round((n/440)*100, digits =1))

topic <- lit1 %>% 
  group_by(PublicationYear, ArticleTopic) %>% 
  tally() %>%
  mutate(ArticleTopic = case_when(ArticleTopic == "Dispute of other article"|
                                    ArticleTopic == "Estimation of pre-MME population"|
                                    ArticleTopic == "Estimation of post-MME population"|
                                    ArticleTopic == "General observations"|
                                    ArticleTopic == "Public survey" ~ "Other",
                                    ArticleTopic == "Flow-on effects"~ "Ecosystem responses",
                                  TRUE ~ ArticleTopic))

topic_plot <- topic %>% 
  ggplot(aes(factor(PublicationYear), n, fill = ArticleTopic))+
  geom_col(col = "black")+
  labs(title = "",
       x = "Publication Year",
       y = "Count of publications",
       fill = "Article topic")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0.5, 
                                   vjust = 0.5)) +
  scale_fill_brewer(palette = "RdYlBu")
topic_plot

ggsave("figures/time_topic.png", topic_plot,
       width = 10,
       height = 6
      )

#First, look in general
causes <- lit1 %>% 
  group_by(Cause) %>% 
  tally()%>% 
  mutate(percent = round((n/440)*100, digits =1))



#Then, look by year
causes1 <- lit1 %>% 
  group_by(PublicationYear, Cause, Individual_or_not) %>% 
  tally() %>% 
  #Remove duplicates using studies from the MME
  mutate(n = case_when(Individual_or_not != "Individual" ~ as.numeric("1"),
                       TRUE ~ as.numeric(n))) %>% 
  group_by(PublicationYear, Cause) %>% 
  tally() %>% 
  mutate(Cause = gsub("NA", "Theory, no cause", Cause))

cause_plot <- causes %>% 
  ggplot(aes(Cause, n))+
  geom_col()+
  labs(title = "",
       x = "Cause",
       y = "Count of publications")

time_causes <- causes1 %>% 
        ggplot(aes(factor(PublicationYear), n, fill = Cause),
               )+
  geom_col(col = "black")+
  labs(title = "",
       x = "Publication year",
       y = "Count of publications",
       fill = "Cause of MME")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0.5, 
                                   vjust = 0.5))+
  scale_fill_brewer(palette = "Paired")
time_causes

ggsave("figures/time_cause.png", time_causes,
       width = 10,
       height = 6)


location <- lit1 %>% 
  mutate(Region = gsub("Pacific Islands", "Australasia", Region)) %>% 
  group_by(Year, Region) %>% 
  tally()

time_location <- location %>% 
  ggplot(aes(Year, n, fill = Region))+
  geom_col(col = "black")+
  labs(title = "",
       x = "MME Year",
       y = "Publication count",
       fill = "MME Location")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0.5, 
                                   vjust = 0.5)) +
  scale_fill_brewer(palette = "RdYlBu")

time_location

taxa <- lit1 %>% 
  group_by(Group) %>% 
  tally() %>% 
  mutate(percent = round((n/440)*100, digits =1))

magnitudes <- mag2_data %>%
  group_by(Magnitude) %>% 
  tally()
mag_tot <- sum(magnitudes$n)

mmen <- mme_numbers %>% group_by(PubTime) %>% tally() %>% 
  mutate(percent = (n/440)*100)

