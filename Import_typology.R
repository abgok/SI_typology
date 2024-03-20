library(tidyverse)
library(readxl)
library("xlsx")
library(janitor)


####Projects####
projects<-read_tsv("data/999/projects.csv", guess_max=14487)%>%
  select(idProjects, ProjectName, ProjectWebpage, FirstDataSource, URL_Reported, DateOfLastCrawl, Translated)%>%
  rename(Project_id=idProjects)

####Summary####
projects_summary<-read_tsv("data/999/project_summary.csv")%>%
  rename(Project_id=ProjectID)


projects_summary%>%filter(SummarizerModelName=="MergedDescription")%>%distinct(Project_id)

####Scores####
scores<-read_tsv("data/999/social_innov_scores.csv")%>%
  mutate(total_score=CriterionActors+CriterionInnovativeness+CriterionObjectives+CriterionOutputs)%>%
  filter(total_score>0)%>%
  rename(Project_id=ProjectID)%>%
  left_join(read_excel("data/types_new_2024.xlsx"))

scores%>%
  filter(AnnSource=="MACHINE")%>%
  ggplot(aes(x = total_score )) +
  geom_histogram()

scores%>%
  group_by(AnnSource)%>%
  summarise(total_score   =mean(total_score   ))


scores%>%
  group_by(total_score)%>%
  tally()

#some checks#

projects%>%left_join(scores)%>%
  mutate(total_score=CriterionActors+CriterionInnovativeness+CriterionObjectives+CriterionOutputs)%>%
  filter(total_score==-4, FirstDataSource!="Kickstarter")%>%
  left_join(projects_summary)%>%
  filter(!is.na(TextLength))%>%
  arrange(desc(TextLength ))

projects%>%left_join(scores)%>%
  mutate(total_score=CriterionActors+CriterionInnovativeness+CriterionObjectives+CriterionOutputs)%>%
  left_join(projects_summary)%>%
  group_by(FirstDataSource)%>%
  summarise(avg_Score=mean(total_score))%>%
  arrange(avg_Score)


projects%>%left_join(scores)%>%
  mutate(total_score=CriterionActors+CriterionInnovativeness+CriterionObjectives+CriterionOutputs)%>%
  filter(FirstDataSource=="Digital Social Innovation Database")%>%
  left_join(projects_summary)%>%
  select(Project_id, ProjectName, ProjectWebpage, URL_Reported, DateOfLastCrawl, starts_with("Criterion"), AnnSource, total_score, ProjectSummary)%>%
  distinct()%>%
  write.xlsx("data/999/dsi_manual.xlsx")


check<-projects%>%left_join(read_tsv("data/999/social_innov_scores.csv"))%>%
  mutate(total_score=CriterionActors+CriterionInnovativeness+CriterionObjectives+CriterionOutputs)%>%
  filter(FirstDataSource=="Digital Social Innovation Database", total_score==4)
  

projects%>%left_join(read_tsv("data/999/social_innov_scores.csv")%>%rename(Project_id=ProjectID))%>%
  mutate(total_score=CriterionActors+CriterionInnovativeness+CriterionObjectives+CriterionOutputs)%>%
  filter(FirstDataSource=="Digital Social Innovation Database")%>%
  group_by(total_score)%>%
  tally()

read_tsv("data/999/project_topic.csv")%>%
  group_by(Sources)%>%
  tally()

####MongoDB####

text_amount <- read_tsv("data/999/mongodb_projects_spec.csv")

####Topics####
Topic_info <- read_delim("data/knowmak.tree-simple_new") %>% filter(TopLevel=="SDG")
topic_levels <- unique(Topic_info$Topics)
#raw <- read.csv("data/999/project_topic.csv")
Key_Topics <- read_tsv("data/999/project_topic.csv") %>%
   mutate(TopicName = str_remove_all(TopicName, "http://www.gate.ac.uk/ns/ontologies/knowmak/")) %>%
  left_join(Topic_info, by=c("TopicName" = "Subtopics")) %>%
  #filter(TopLevel=="SDG", Sources=="Title,Summary")%>%
  filter(TopLevel=="SDG", Sources=="Title,Summary,FullText")%>%
  rename(Project_id=ProjectID)

# calculate deciles for setting a threshold. for each topicname (= subtopics), calculate deciles for each project ID.
Key_Topics <- Key_Topics %>% filter(Topics %in% topic_levels)
# Key_Topics$Decile <- NA
# 
# df <- Key_Topics[-c(1:nrow(Key_Topics)),]
# 
# for(i in seq_along(topic_levels)){
#   df_sub <- Key_Topics %>% filter(Topics == topic_levels[i])
#   df_sub$Decile <- ntile(df_sub$TopicScore2, 100) # change last arg to 100 if wanting centile
#   df <- rbind(df, df_sub)
# }
# Key_Topics <- df

# set a threshold for deciles 

threshold <- 75

Key_Topics_2 <- Key_Topics %>% 
  select(c(TopicName, TopicScore2, KeyWords, Project_id, Topics)) %>% 
  distinct()%>%
  left_join(Key_Topics%>% 
              select(c(TopicName, TopicScore2, KeyWords, Project_id, Topics)) %>% 
              distinct()%>%
              group_by(Topics, TopicName)%>%
              reframe(TopicScore2, Decile_2=ntile(TopicScore2, 100))
            )




#calculate metrics#
Topics_table<- Key_Topics_2 %>% 
  group_by(Project_id, Topics) %>%
  summarise("Mean_Score" = mean(TopicScore2),
            "Mean_Decile" = mean(Decile_2),
            "Max_Score" = max(TopicScore2),
            "Max_Decile" = max(Decile_2))

Topics_table<-Topics_table%>%
  left_join(Key_Topics%>%
              group_by(Project_id, Topics)%>%
              tally()%>%
              rename(n_subtopics=n)
            )%>%
    mutate(n_subtopics=case_when(is.na(n_subtopics)~0,
                               TRUE~n_subtopics))


Topics_flat<- Topics_table%>%
  #add filter here, either as Max_Decile or n_subtopics or both#
  filter(Max_Decile>=threshold)%>%
  filter(n_subtopics>=2)%>%
  right_join(scores%>%select(Project_id))%>%
  mutate(Topics=case_when(is.na(Topics)~"A_topic_not_related_to_an_SDG",
                          TRUE~Topics))%>%
  group_by(Project_id, Topics)%>%
  tally()%>%
  pivot_wider(id_cols=Project_id, names_from = Topics, values_fill = 0, values_fn = length, values_from = Topics)%>%
  ungroup()%>%
  mutate(Number_SDG_Topics= rowSums(.[setdiff(names(.),c("Project_id", "A_topic_not_related_to_an_SDG"))]))

           

           
#some checks#

Topics_flat%>%
  group_by(Number_SDG_Topics)%>%
  tally()

Topics_table%>%
  group_by(Max_Decile)%>%
  tally()%>%
  arrange(desc(n))

Topics_flat%>%
  group_by(A_topic_not_related_to_an_SDG)%>%
  tally()

####Location####
EU_member <- read_csv("data/eu_members.csv")


#country and continent codes ISO-3166#

continent_codes<-read_csv("data/country_continent_codes.csv")%>%
  rename(CountryISOName=`alpha-3`,
         Continent_Name=region)%>%
  mutate(Continent_Name=case_when(`sub-region`=="Northern America"~"Northern America",
                                  `sub-region`=="Latin America and the Caribbean"~"Latin America and the Caribbean",
                                  TRUE~Continent_Name))%>%
    select(Continent_Name, CountryISOName)

#WIPO Country Inattentiveness#
wipo_iix <-read_excel("data/wipo-pub-2000-2022-tech1.xlsx", sheet="Data")%>%
  filter(NAME=="Global Innovation Index")%>%
  rename(WIPO_Innov_Score=SCORE)%>%
  mutate(WIPO_Innov_quantile = ntile(desc(WIPO_Innov_Score), 4),
         WIPO_Innov_group=case_when(WIPO_Innov_quantile==1~"H_Innov",
                                    WIPO_Innov_quantile==2~"UM_Innov",
                                    WIPO_Innov_quantile==3~"LM_Innov",
                                    WIPO_Innov_quantile==4~"L_Innov")
  )

wipo_classification <-read_excel("data/wipo-pub-2000-2022-tech1.xlsx", sheet="Economies")

#SPI Country Data#
spi<- read_csv("data/spi.csv")%>%filter(year==2020)%>%
  dplyr::select(-year)%>%
  filter(!is.na(spi))%>%
  mutate(spi_quantile = ntile(desc(spi), 4),
         spi_group=case_when(spi_quantile==1~"HI",
                             spi_quantile==2~"UM",
                             spi_quantile==3~"LM",
                             spi_quantile==4~"LI")
  )

spi_extended<- read_excel("data/2022_social_progress_index_dataset-1663250262.xlsx", 
                          sheet="2011-2022 SPI data", 
                          range="A2:CB2366")%>%
  filter(spiyear==2020, status=="Ranked")%>%
  select(spicountrycode,
         score_spi,
         score_bhn,	
         score_fow,	
         score_opp, 
         score_nbmc,
         score_ws,
         score_sh,
         score_ps,
         score_abk,
         score_aic,
         score_hw,
         score_eq,
         score_pr,
         score_pfc,
         score_incl,
         score_aae)%>%
  rename_with(.fn = ~ paste0("spi_", .x))%>%
  mutate(across(where(is.numeric),
                .fns = ~ ntile(desc(.x), 10),
                .names = "{.col}_decile"))%>%
  mutate(across(ends_with("decile"),
                .fns = ~ case_when(.x==1~1,
                                   .x==2|.x==3~2,
                                   .x==4|.x==5~3,
                                   .x==6|.x==7~4,
                                   .x==8|.x==9~5,
                                   .x==10~6),
                .names = "{.col}_tier"))


# Country_info<-read_tsv("data/999/project_location_UPDATED_24022024.csv") %>%
#   rename(Project_id=ProjectID)%>%
#   filter(DataTrace%in%c("Enriched by Location AlgorithmV2.0", "Predicted by Location Algorithm V2.0"))%>%
#   filter(CountryISOName!="NULL")%>%
#   #add Palestinian Territories#
#   mutate(CountryISOName=case_when(CountryName=="Palestinian Territories"~"PSE",
#                                   TRUE~CountryISOName))%>%
#   left_join(continent_codes)%>%
#   mutate(Continent_Name_2=case_when(CountryISOName%in%EU_member$`ISO 3166-1 alpha-3 code`|Continent_Name=="European Union"~"EU",
#                                     Continent_Name=="Europe" ~ "Non-EU Europe",
#                                     Continent_Name=="NULL"|is.na(Continent_Name)~"Unknown/Global",
#                                     #add Kosovo to Europe#
#                                     CountryISOName=="XXK"~"Europe",
#                               TRUE~Continent_Name))%>%
#   select(Project_id, CountryName, CountryISOName, Continent_Name, Continent_Name_2, CountryLatitude, CountryLongitude)%>%
#   left_join(wipo_iix, by=join_by(CountryISOName==ISO3))%>%
#   left_join(wipo_classification, by=join_by(CountryISOName==ISO3))%>%
#   left_join(spi_extended, by=join_by(CountryISOName==spi_spicountrycode))%>%
#   left_join(spi, by=join_by(CountryISOName==country_code))%>%
#   right_join(scores%>%select(Project_id))%>%
#   mutate(Continent_Name_2=case_when(is.na(Continent_Name_2)~"Unknown/Global",
#                           TRUE~Continent_Name_2))
#   
# 
# Country_info_old<-read_csv("data/New_location_model_091122.csv", guess_max=100000)  %>%
#   rename(Project_id=Projects_idProjects)%>%
#   filter(DataTrace%in%c("Enriched by Location AlgorithmV2.0", "Predicted by Location Algorithm V2.0"))%>%
#   filter(CountryISOName!="NULL")%>%
#   #add Palestinian Territories#
#   mutate(CountryISOName=case_when(CountryName=="Palestinian Territories"~"PSE",
#                                   CountryName=="European Union, bul. Cherni vrah, ж.к. Лозенец, Lozenets, Sofia, Sofia City, Sofia-City, 1421, Bulgaria"~"BGR",
#                                   CountryName=="Hong Kong Island, Hong Kong, China"~"HKG",
#                                   TRUE~CountryISOName))%>%
#   left_join(continent_codes)%>%
#   mutate(Continent_Name_2=case_when(CountryISOName%in%EU_member$`ISO 3166-1 alpha-3 code`|Continent_Name=="European Union"~"EU",
#                                     Continent_Name=="Europe" ~ "Non-EU Europe",
#                                     Continent_Name=="NULL"|is.na(Continent_Name)~"Unknown/Global",
#                                     #add Kosovo to Europe#
#                                     CountryISOName=="XXK"~"Europe",
#                                     TRUE~Continent_Name))%>%
#   select(Project_id, CountryName, CountryISOName, Continent_Name, Continent_Name_2, CountryLatitude, CountryLongitude)%>%
#   left_join(wipo_iix, by=join_by(CountryISOName==ISO3))%>%
#   left_join(wipo_classification, by=join_by(CountryISOName==ISO3))%>%
#   left_join(spi_extended, by=join_by(CountryISOName==spi_spicountrycode))%>%
#   left_join(spi, by=join_by(CountryISOName==country_code))%>%
#   right_join(scores%>%select(Project_id))%>%
#   mutate(Continent_Name_2=case_when(is.na(Continent_Name_2)~"Unknown/Global",
#                                     TRUE~Continent_Name_2))
# 
# 
# 
# read_csv("data/New_location_model_091122.csv", guess_max=100000)  %>%
#   rename(Project_id=Projects_idProjects )%>%
#   filter(DataTrace%in%c("Enriched by Location AlgorithmV2.0", "Predicted by Location Algorithm V2.0"))%>%
#   filter(CountryISOName=="NULL")%>%
#   group_by(CountryName)%>%
#   tally()%>%
#   arrange(desc(n))
# 
# 
# 
# anti_join(Country_info_old%>%select(Project_id), Country_info%>%select(Project_id))
# 
# Country_info_augmented <-
#   read_tsv("data/999/project_location.csv") %>%
#   rename(Project_id=ProjectID)%>%
#   filter(DataTrace%in%c("Enriched by Location AlgorithmV2.0", "Predicted by Location Algorithm V2.0"))%>%
#   filter(!is.na(CountryISOName)) %>%
#   bind_rows(
#           read_csv("data/New_location_model_091122.csv", guess_max=100000) %>%
#           select(Projects_idProjects, CountryName, CountryISOName, DataTrace)%>%
#           rename(Project_id = Projects_idProjects) %>%
#           mutate(Project_id = as.numeric(Project_id)) %>%
#           filter(
#             DataTrace %in% c(
#               "Enriched by Location AlgorithmV2.0",
#               "Predicted by Location Algorithm V2.0"
#             ),
#             !is.na(CountryISOName)
#           )%>%
#             right_join(
#               scores%>%select(Project_id)%>%
#                 anti_join(read_tsv("data/999/project_location.csv") %>%
#                             rename(Project_id=ProjectID) %>%
#                             filter(!is.na(CountryISOName)) %>%
#                             select(Project_id)) 
#             )
#           )%>%
#   #add Palestinian Territories#
#   mutate(CountryISOName=case_when(CountryName=="Palestinian Territories"~"PSE",
#                                   CountryName=="European Union, bul. Cherni vrah, ж.к. Лозенец, Lozenets, Sofia, Sofia City, Sofia-City, 1421, Bulgaria"~"BGR",
#                                   CountryName=="Hong Kong Island, Hong Kong, China"~"HKG",
#                                   TRUE~CountryISOName))%>%
#   left_join(continent_codes)%>%
#   mutate(Continent_Name_2=case_when(CountryISOName%in%EU_member$`ISO 3166-1 alpha-3 code`|Continent_Name=="European Union"~"EU",
#                                     Continent_Name=="Europe" ~ "Non-EU Europe",
#                                     #Continent_Name=="NULL"|is.na(Continent_Name)~"Unknown/Global",
#                                     #add Kosovo to Europe#
#                                     CountryISOName=="XXK"~"Non-EU Europe",
#                                     Continent_Name=="Asia"|Continent_Name=="Oceania"~"Asia and Ocenia",
#                                     TRUE~Continent_Name))%>%
#   select(Project_id, CountryName, CountryISOName, Continent_Name, Continent_Name_2)%>%
#   left_join(wipo_iix, by=join_by(CountryISOName==ISO3))%>%
#   left_join(wipo_classification, by=join_by(CountryISOName==ISO3))%>%
#   left_join(spi_extended, by=join_by(CountryISOName==spi_spicountrycode))%>%
#   left_join(spi, by=join_by(CountryISOName==country_code))%>%
#   right_join(scores%>%select(Project_id))%>%
#   mutate(Continent_Name_2=case_when(is.na(Continent_Name_2)~"Unknown/Global",
#                                     TRUE~Continent_Name_2))



Country_info_2<-read_tsv("data/999/project_location_UPDATED_24022024.csv") %>%
  rename(Project_id=ProjectID)%>%
  filter(DataTrace%in%c("Enriched by Location AlgorithmV2.0", "Predicted by Location Algorithm V2.0"))%>%
  filter(CountryISOName!="NULL")%>%
  #add Palestinian Territories#
  mutate(CountryISOName=case_when(CountryName=="Palestinian Territories"~"PSE",
                                  CountryName=="European Union, bul. Cherni vrah, ж.к. Лозенец, Lozenets, Sofia, Sofia City, Sofia-City, 1421, Bulgaria"~"BGR",
                                  CountryName=="Hong Kong Island, Hong Kong, China"~"HKG",
                                  TRUE~CountryISOName))%>%
  left_join(continent_codes)%>%
  mutate(Continent_Name_2=case_when(CountryISOName%in%EU_member$`ISO 3166-1 alpha-3 code`|Continent_Name=="European Union"~"EU",
                                    Continent_Name=="Europe" | CountryName=="Kosovo" ~ "Non-EU Europe",
                                    #Continent_Name=="NULL"|is.na(Continent_Name)~"Unknown/Global",
                                    #add Kosovo to Europe#
                                    CountryISOName=="XKX"~"Non-EU Europe",
                                    Continent_Name=="Africa"|Continent_Name=="Oceania"|Continent_Name=="Latin America and the Caribbean"~"RoW",
                                    TRUE~Continent_Name))%>%
  select(Project_id, CountryName, CountryISOName, Continent_Name, Continent_Name_2, CountryLatitude, CountryLongitude)%>%
  left_join(wipo_iix, by=join_by(CountryISOName==ISO3))%>%
  left_join(wipo_classification, by=join_by(CountryISOName==ISO3))%>%
  left_join(spi_extended, by=join_by(CountryISOName==spi_spicountrycode))%>%
  left_join(spi, by=join_by(CountryISOName==country_code))%>%
  right_join(scores%>%select(Project_id))%>%
  mutate(Continent_Name_2=case_when(is.na(Continent_Name_2)~"Unknown/Global",
                                    TRUE~Continent_Name_2))


Country_info_2%>%
  filter(Continent_Name_2=="Unknown/Global")%>%
  #filter(is.na(CountryISOName))%>%
  group_by(CountryLatitude)%>%
  tally()%>%
  arrange(desc(n))


Country_flat <- Country_info_2%>%
  select(Project_id, Continent_Name_2)%>%
  pivot_wider(id_cols=Project_id, names_prefix="Continent_", names_from = Continent_Name_2, values_fill = 0, values_fn = length, values_from = Continent_Name_2)%>%
  mutate(across(starts_with("Continent"), ~case_when(.x>=1~1,
                                                     TRUE~0)))%>%
  left_join(  Country_info_augmented%>%
                dplyr::select(Project_id, WIPO_Innov_Score, POP, PPPGDP, PPPPC)%>%
                filter(!is.na(WIPO_Innov_Score), !is.na(POP), !is.na(PPPGDP), !is.na(PPPPC))%>%
                group_by(Project_id)%>%
                reframe(mean_SCORE=mean(WIPO_Innov_Score, .na.RM=TRUE),
                        mean_POP=mean(POP, .na.RM=TRUE),
                        mean_PPPGDP=mean(PPPGDP, .na.RM=TRUE),
                        mean_PPPPC=mean(PPPPC, .na.RM=TRUE)
                )%>%
                left_join(
                  Country_info_augmented%>%
                    group_by(Project_id)%>%
                    tally()%>%
                    filter(n>1)%>%
                    mutate(multi_country=1)%>%
                    dplyr::select(-n))
  )%>%
  select(-starts_with("mean"))%>%
  ungroup()%>%
  mutate(multi_country=tidyr::replace_na(multi_country, 0))%>%
  left_join(Country_info_augmented%>%
              filter(!is.na(INCOME), !is.na(spi_group))%>%
              unite(INCOMEvsSPI, 
                    c("INCOME", "spi_group"), 
                    sep="_"
              )%>%
              pivot_wider(id_cols=Project_id, 
                          names_prefix= "INCOMEvsSPI_", 
                          names_from = INCOMEvsSPI, 
                          values_fill = "No", 
                          values_from = INCOMEvsSPI, 
                          values_fn = max)%>%
              mutate(across(
                starts_with("INCOMEvsSPI_"), 
                ~case_when(.x!="No"~"Yes",
                           TRUE~.x)
              )
              )
            )%>%
  left_join(Country_info_augmented%>%
               filter(!is.na(INCOME))%>%
               pivot_wider(id_cols=Project_id, names_prefix= "INCOME_", names_from = INCOME, values_fill = "No", values_from = INCOME, values_fn = max)%>%
               mutate(across(starts_with("INCOME_"), ~case_when(.x!="No"~"Yes",
                                                                TRUE~.x)
               )
               ))   %>%
  left_join(Country_info_augmented%>%
               filter(!is.na(spi_group))%>%
               pivot_wider(id_cols=Project_id, names_prefix= "SPI_", names_from = spi_group, values_fill = "No", values_from = spi_group, values_fn = max)%>%
               mutate(across(starts_with("SPI_"), ~case_when(.x!="No"~"Yes",
                                                                TRUE~.x)
               )
               )
             )%>%
  left_join(Country_info_augmented%>%
               filter(!is.na(WIPO_Innov_group))%>%
               pivot_wider(id_cols=Project_id, names_prefix= "INNOV_", names_from = WIPO_Innov_group, values_fill = "No", values_from = WIPO_Innov_group, values_fn = max)%>%
               mutate(across(starts_with("INNOV_"), ~case_when(.x!="No"~"Yes",
                                                                TRUE~.x)
               )
               )
             
             
             )
  #%>%
  # left_join(Country_info_augmented%>%
  #             pivot_wider(id_cols=Project_id, 
  #                         names_from = spi_score_spi_decile_tier, 
  #                         values_from = spi_score_spi_decile_tier, 
  #                         names_prefix = "spi_score_spi_decile_tier_",
  #                         values_fn=first)%>%
  #             mutate(
  #               across(everything(), ~replace_na(.x, 0))
  #             )%>%
  #             mutate(
  #               across(setdiff(names(select(., where(is.numeric))), 'Project_id'), ~case_when(.x>=1~1,
  #                                                                                             TRUE~0))
  #             )%>%
  #             select(-ends_with("_NA"))
  #           )%>%
  # left_join(Country_info_augmented%>%
  #             pivot_wider(id_cols=Project_id, 
  #                         names_from = spi_score_bhn_decile_tier, 
  #                         values_from = spi_score_bhn_decile_tier, 
  #                         names_prefix = "spi_score_bhn_decile_tier_",
  #                         values_fn=first)%>%
  #             mutate(
  #               across(everything(), ~replace_na(.x, 0))
  #             )%>%
  #             mutate(
  #               across(setdiff(names(select(., where(is.numeric))), 'Project_id'), ~case_when(.x>=1~1,
  #                                                                                             TRUE~0))
  #             )%>%
  #             select(-ends_with("_NA"))
  #           )%>%
  # left_join(Country_info_augmented%>%
  #             pivot_wider(id_cols=Project_id, 
  #                         names_from = spi_score_fow_decile_tier, 
  #                         values_from = spi_score_fow_decile_tier, 
  #                         names_prefix = "spi_score_fow_decile_tier_",
  #                         values_fn=first)%>%
  #             mutate(
  #               across(everything(), ~replace_na(.x, 0))
  #             )%>%
  #             mutate(
  #               across(setdiff(names(select(., where(is.numeric))), 'Project_id'), ~case_when(.x>=1~1,
  #                                                                                             TRUE~0))
  #             )%>%
  #             select(-ends_with("_NA"))
  #           )%>%
  # left_join(Country_info_augmented%>%
  #             pivot_wider(id_cols=Project_id, 
  #                         names_from = spi_score_opp_decile_tier, 
  #                         values_from = spi_score_opp_decile_tier, 
  #                         names_prefix = "spi_score_opp_decile_tier_",
  #                         values_fn=first)%>%
  #             mutate(
  #               across(everything(), ~replace_na(.x, 0))
  #             )%>%
  #             mutate(
  #               across(setdiff(names(select(., where(is.numeric))), 'Project_id'), ~case_when(.x>=1~1,
  #                                                                                             TRUE~0))
  #             )%>%
  #             select(-ends_with("_NA"))
  #           )


###some checks###

Country_info%>%
  group_by(Continent_Name_2)%>%
  tally()%>%
  arrange(desc(n))


Country_flat%>%
  group_by(`Continent_Unknown/Global`)%>%
  tally()


Country_info%>%
  dplyr::select(Project_id, Continent_Name_2)%>%
  pivot_wider(id_cols=Project_id, 
              names_prefix="Continent_", 
              names_from = Continent_Name_2, 
              values_fill = 0,
              values_fn = length, 
              values_from = Continent_Name_2)%>%
  mutate(across(starts_with("Continent"), ~case_when(.x>=1~1,
                                                     TRUE~0)))%>%
  group_by(Continent_EU)%>%
  tally()






####merge to a flat file####
Flat_data <- scores%>%
  left_join(Topics_flat)%>%
  left_join(Country_flat)%>%
  left_join(actor_merged_flat)%>%
  mutate(across(everything(), ~factor(.x)),
         Number_actor_types=as.numeric(Number_actor_types),
         Project_id=as.character(Project_id),
         total_score=as.numeric(total_score),
         Number_SDG_Topics=as.numeric(Number_SDG_Topics),
         across(c(all_of(topic_levels), 
                  A_topic_not_related_to_an_SDG,
                  starts_with("Continent"),
                  multi_country,
                  ends_with("INCOME"),
                  starts_with("INCOMEvsSPI_"),
                  contains("_SPI_"),
                  ends_with("_ANY")
                  ), ~ recode(., "0" = "No", "1" = "Yes")),
         across(ends_with("_ANY"), ~as.character(.x)),
         #`ActorType_Inter-governmental_Organisation_ANY`=tidyr::replace_na(`ActorType_Inter-governmental_Organisation_ANY`, "No"),
         across(ends_with("_ANY"), ~tidyr::replace_na(.x, "No")),
         Number_actor_types=tidyr::replace_na(Number_actor_types, 0)
         )%>%
  rename( "Dimension: Innovativeness"=CriterionInnovativeness,
          "Dimension: Objectives"=CriterionObjectives,
          "Dimension: Actors"=CriterionActors,
          "Dimension: Outputs"=CriterionOutputs,
          "Includes a project location in Region: North America"=`Continent_Northern America`,
          #"Includesa project location in Region: South and Central America"=`Continent_Latin America and the Caribbean`,
          "Includes a project location in Region: EU"=Continent_EU,
          "Includes a project location in Region: Non-EU Europe"=`Continent_Non-EU Europe`,
          "Includes a project location in Region: RoW"=`Continent_RoW`,
          "Includes a project location in Region: Asia"=`Continent_Asia`,
          "Includes a project location in Region: Unknown/Global"=`Continent_Unknown/Global`,
          "Project location is in more than one country"=multi_country,
          "Project location includes a country with Income Level: High & SPI Level: High"=INCOMEvsSPI_HI_HI,
          "Project location includes a country with Income Level: Lower-Middle & SPI Level: Lower-Middle"=INCOMEvsSPI_LM_LM,
          "Project location includes a country with Income Level: Upper-Middle & SPI Level: Upper-Middle"=INCOMEvsSPI_UM_UM,
          "Project location includes a country with Income Level: Upper-Middle & SPI Level: Lower-Middle"=INCOMEvsSPI_UM_LM,
          "Project location includes a country with Income Level: Low & SPI Level: Low"=INCOMEvsSPI_LI_LI,
          "Project location includes a country with Income Level: Lower-Middle & SPI Level: Low"=INCOMEvsSPI_LM_LI,
          "Project location includes a country with Income Level: Upper-Middle & SPI Level: High"=INCOMEvsSPI_UM_HI,
          "Project location includes a country with Income Level: Lower-Middle & SPI Level: Upper-Middle"=INCOMEvsSPI_LM_UM,
          "Project location includes a country with Income Level: High & SPI Level: Upper-Middle"=INCOMEvsSPI_HI_UM,
          "Project location includes a country with Income Level: High & SPI Level: Lower-Middle"=INCOMEvsSPI_HI_LM,
          "SDG: Industry, innovation and infrastructure"= industry_innovation_and_infrastructure,
          "SDG: Quality education"=quality_education,
          "SDG: Decent work and economic growth"=decent_work_and_economic_growth,
          "SDG: Good health and well being"=good_health_and_well_being,
          "SDG: Reduced inequalities"=reduced_inequalities,
          "SDG: No poverty"=no_poverty,
          "SDG: Sustainable cities and communities"= sustainable_cities_and_communities,
          "SDG: Peace justice and strong institutions"=peace_justice_and_strong_institutions,
          "SDG: Responsible consumption and production"=responsible_consumption_and_production,
          "SDG: Climate action"=climate_action,
          "SDG: Zero hunger"=zero_hunger,
          "SDG: Life on land"=life_on_land,
          "SDG: Affordable and clean energy"=affordable_and_clean_energy,
          "SDG: Life below water"=life_below_water,
          "SDG: Clean water and sanitation"=clean_water_and_sanitation,
          "SDG: On a topic not related to SDGs"=A_topic_not_related_to_an_SDG,
          "Total number of related SDGs"= Number_SDG_Topics,
          "Project location includes a country with Income Level: Low"= INCOME_LI,
          "Project location includes a country with Income Level: Lower-Middle"=INCOME_LM,
          "Project location includes a country with Income Level: Upper-Middle"=INCOME_UM,
          "Project location includes a country with Income Level: High"=INCOME_HI,
          "Project location includes a country with SPI Level: Low"=SPI_LI,
          "Project location includes a country with SPI Level: Lower-Middle"=SPI_LM,
          "Project location includes a country with SPI Level: Upper-Middle"=SPI_UM,
          "Project location includes a country with SPI Level: High"=SPI_HI,
          "Has an actor of type: Third Sector"=ActorType_Third_Sector_ANY,
          "Has an actor of type: Non-Tertiary Education"="ActorType_Non-Tertiary_Education_ANY",
          "Has an actor of type: University"=ActorType_University_ANY,                     
          "Has an actor of type: Public Sector"=ActorType_Public_Sector_ANY,
          "Has an actor of type: For-profit Company"="ActorType_For-profit_company_ANY",
          "Has an actor of type: Inter-governmental Organisation"="ActorType_Inter-governmental_Organisation_ANY",
          "Has an actor of type: Religious Organisation"=ActorType_Religious_Organisation_ANY,
          "Has an actor of type: Other Tertiary Education"=ActorType_Other_Tertiary_Education_ANY,
          "Has an actor of type: Political Organisation"=ActorType_Political_Organisation_ANY,
          "Total number of types of actors"=Number_actor_types  
          )%>%
  mutate(across(starts_with("Project location includes a country"), ~tidyr::replace_na(.x, "No")),
         `Total number of related SDGs`=`Total number of related SDGs`-1,
         `Total number of related SDGs (Grouped)`=factor(`Total number of related SDGs`),
         `Total number of related SDGs (Grouped)`=fct_recode(`Total number of related SDGs (Grouped)`,
           "2" = "2",
           "3" = "3",
           "4" = "4",
           "5+" = "5",
           "5+" = "6",
           "5+" = "7",
          "5+" = "8",
          "5+" = "9",
          "5+" = "10",
          "5+" = "11",
          "5+" = "12",
          "5+" = "13",
           #"5+" = "14"
          ),
         `Covers Multiple SDGs`=as.factor(case_when(`Total number of related SDGs`<2~"No",
                                          TRUE~"Yes"))
         )

Flat_data$`Total number of related SDGs (Grouped)`

Flat_data%>%
  group_by(`Total number of related SDGs (Grouped)`)%>%
  tally()
  

Flat_data_filtered<-  Flat_data%>%
  filter(`Dimension: Innovativeness` %in% c("1", "2"))%>%
  filter(`Dimension: Objectives` %in% c("1", "2") | `Dimension: Actors` %in% c("1", "2") | `Dimension: Outputs` %in% c("1", "2"))


Flat_data_filtered_2<-
  Flat_data_filtered%>%
  filter(`Includes a project location in Region: Asia`=="Yes"| 
         `Includes a project location in Region: EU`=="Yes"|
         `Includes a project location in Region: Non-EU Europe`=="Yes"|
         `Includes a project location in Region: North America`=="Yes"|
         `Includes a project location in Region: RoW`=="Yes")%>%
  select(-`Includes a project location in Region: Unknown/Global`)




save.image(file = "Imported_data_v2.RData")
