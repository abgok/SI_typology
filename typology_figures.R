library(tidyverse)
library(ggalluvial)
library(viridis)
library(ggsci)
library(crosstable)
library(officer)
library(corrplot)
library(scales)
library(ggpubr)
library(ggrepel)
library(DescTools)
library(nnet)
library(broom)


load(file = "Imported_data_v2.RData")


####Figure 2####


###alluvial###

alluvial_graph<-
  Flat_data_filtered_2 %>%
  mutate(across(.cols=starts_with("Dimension"), ~factor(.x, levels=c(2,1,0))),
         type5_top=factor(type5_top, levels=c("Extensive SI", "Oriented SI", "Unoriented SI", "Limited SI")))%>%
  group_by(`Dimension: Objectives`,`Dimension: Actors`,`Dimension: Outputs`,`Dimension: Innovativeness`, type5_top) %>% 
  summarise(n=n()) %>%
  ggplot(aes(
    axis1 = `Dimension: Objectives`, 
    axis2 = `Dimension: Actors`, 
    axis3 = `Dimension: Outputs`, 
    axis4 = `Dimension: Innovativeness`,
    y = n)
  )+
  scale_x_discrete(limits = c("Dimension: Objectives", "Dimension: Actors", "Dimension: Outputs", "Dimension: Innovativeness"), 
                   expand = c(.1, .05),
                   labels = function(x) stringr::str_wrap(x, width = 15))+
  geom_flow(aes(fill = type5_top), width=1/4) +
  geom_stratum(fill="grey", 
               width=1/4,
  )+
  #geom_lode()+
  geom_label_repel(
    stat = "stratum",
    aes(label = paste0(
      "Score ", stratum,
      "\n",
      round(after_stat(prop), 3) *
        100, "%"
    )),
    size = 3,
    #fill = alpha("white", 0.60),
    # lineheight = 0.75,
    #direction = "x",
    nudge_x = .5,
    min.segment.length = 0,
    seed = 42, 
    box.padding = 0,
    #color = "white"
  ) +
  labs(y="Number of Projects",
       fill="Type of SI"
  )+
  scale_fill_brewer(palette = "Pastel1")+
  theme(legend.position = "none")
 # theme(legend.key.size = unit(1.5, 'cm'))
#scale_fill_d3(palette="category20c")


###alluvial sub###
options(ggrepel.max.overlaps = 5)

alluvial_graph_sub <- Flat_data_filtered_2 %>%
  mutate(across(.cols = starts_with("Dimension"), ~ factor(.x, levels =
                                                             c(2, 1, 0))),
         type5_top = factor(
           type5_top,
           levels = c("Extensive SI", "Oriented SI", "Unoriented SI", "Limited SI")
         )) %>%
  group_by(
    `Dimension: Objectives`,
    `Dimension: Actors`,
    `Dimension: Outputs`,
    `Dimension: Innovativeness`,
    type5_top
  ) %>%
  summarise(n = n()) %>%
  group_by(type5_top) %>%
  mutate(percentage = n / sum(n)) %>%
  ungroup() %>%
  ggplot(
    aes(
      axis1 = `Dimension: Objectives`,
      axis2 = `Dimension: Actors`,
      axis3 = `Dimension: Outputs`,
      axis4 = `Dimension: Innovativeness`,
      y = percentage
    )
  ) +
  scale_x_discrete(
    limits = c(
      "Dimension: Objectives",
      "Dimension: Actors",
      "Dimension: Outputs",
      "Dimension: Innovativeness"
    ),
    expand = c(.15, .25),
    labels = function(x)
      stringr::str_wrap(x, width = 15)
  ) +
  geom_flow(aes(fill = type5_top), width = 1 / 4) +
  geom_stratum(fill = "grey",
               width = 1 / 4) +
  # geom_label(stat = "stratum", aes(label = paste0("Score ", stratum,
  #                                                 "\n",
  #                                                 round(after_stat(prop), 3)*100, "%")
  # ),
  # size=3.5,
  # fill=alpha("white", 0.60),
  # lineheight=0.75
  # )+
  geom_label_repel(
    stat = "stratum",
    aes(label = paste0(
      "Score ", stratum,
      "\n",
      round(after_stat(prop), 3) *
        100, "%"
    )),
    size = 2.5,
    #fill = alpha("white", 0.60),
    # lineheight = 0.75,
    #direction = "x",
    nudge_x = .5,
    min.segment.length = 0,
    seed = 42, 
    box.padding = 0.1
  ) +
  labs(y = "Number of Projects",
       fill = "Type of SI") +
  scale_fill_brewer(palette = "Pastel1") +
  scale_y_continuous(labels = scales::percent)+
  facet_wrap( ~ type5_top)+
  theme(legend.position = "none")
#scale_fill_d3(palette="category20c")


###bar chart###

pie_chart_df<-Flat_data_filtered_2%>%
  group_by(type5_top)%>%
  tally()%>%
  mutate(Percentage=n*100/sum(n))%>%
  mutate(lab_ypos = cumsum(n) - 1.5 * n) 

bar_chart<-   pie_chart_df%>%
  mutate(type5_top=factor(type5_top, levels=c("Extensive SI", "Oriented SI", "Unoriented SI", "Limited SI")))%>%
  ggplot(aes(x="", y=n, fill=type5_top)) + 
  geom_col(width=0.5)+
  geom_text(aes(label = paste0(type5_top, "\n", n, " (", round(Percentage, 1), "%)")), 
            position = position_stack(vjust = 0.5),
            size=3) +
  scale_fill_brewer(palette = "Pastel1")+
  labs(x="",
       y="Number of Projects",
       fill="Type of SI"
  )+
  theme(legend.position = "none")+
  scale_x_discrete(expand = c(.15, .15))
#+coord_flip()








figure_2 <- ggarrange(
  ggarrange(alluvial_graph, alluvial_graph_sub, ncol=1, heights=c(0.7, 1), labels= c("a.", "b.")), 
  ggarrange(bar_chart, labels= c("c.")), 
  ncol=2, 
  widths =  c(1, 0.20))

ggsave("figure_2.png", figure_2, dpi=900, units="mm", width=245, height = 139)

####Figure 3####

###Region by Type###
figure_3<- 
  Flat_data_filtered_2%>%
  select(type5_top, starts_with("Includes a project location in Region"))%>%
  pivot_longer(cols=starts_with("Includes a project location in Region"),
               names_to = "Region",
               values_to = "value")%>%
  group_by(type5_top, Region, value)%>%
  tally()%>%
  bind_rows(Flat_data_filtered_2%>%
              select(starts_with("Includes a project location in Region"))%>%
              pivot_longer(cols=starts_with("Includes a project location in Region"),
                           names_to = "Region",
                           values_to = "value")%>%
              group_by(Region, value)%>%
              tally()%>%
              mutate(type5_top="All Projects")
  )%>%
  ungroup()%>%
  group_by(type5_top, Region)%>%
  mutate(percentage=n/sum(n))%>%
  filter(value=="Yes")%>%
  select(-value)%>%
  ungroup()%>%
  mutate(Region=str_replace_all(Region, "Includes a project location in Region: ", ""),
         Region=factor(Region, levels=c("EU", "Non-EU Europe", "North America", "Asia", "RoW"), ordered=TRUE),
         #SDG=relevel(as.factor(SDG),ref="SDG: On a topic not related to SDGs")
  )%>%
  mutate(type5_top=
           factor(type5_top, levels=c("Extensive SI", "Oriented SI", "Unoriented SI", "Limited SI", "All Projects")
           )
  )%>%
  # bind_rows(Flat_data_filtered_2%>%
  #             group_by(type5_top)%>%
  #             tally()%>%
  #             mutate(SDG="All Projects", percentage=n/sum(n)))%>%
  ggplot(aes(y=percentage, x=Region, fill=Region )) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(
    aes(label = scales::percent(percentage, accuracy = 0.1)),
    colour = "black", size = 3.5,
    vjust = 1.25, 
    position = position_dodge(.5)
  )+
  facet_wrap(~type5_top, labeller = label_wrap_gen(width=25), ncol = 1)+
  scale_y_continuous(limits=c(0, 0.5), labels = scales::percent)+
  #theme_light()+
  theme(#axis.text.x=element_blank(),
    #axis.text.x = element_text(angle = 0, vjust = 0, hjust=0),
    legend.position="none",
    strip.text = element_text(face="bold"))+
  labs(x="Includes a project location in Region",
       y="Percentage of Projects",
       fill="")+
  scale_fill_brewer(palette = "Pastel1")
#scale_fill_d3(palette="category20c")

ggsave("figure_3.png", figure_3, dpi=900, units="mm", width=200, height = 160)



####Figure 4####

###SDG by Type###
figure_4 <-
  Flat_data_filtered_2%>%
  select(type5_top, starts_with("SDG"))%>%
  pivot_longer(cols=starts_with("SDG"),
               names_to = "SDG",
               values_to = "value")%>%
  group_by(type5_top, SDG, value)%>%
  tally()%>%
  bind_rows(Flat_data_filtered_2%>%
              select(starts_with("SDG"))%>%
              pivot_longer(cols=starts_with("SDG"),
                           names_to = "SDG",
                           values_to = "value")%>%
              group_by(SDG, value)%>%
              tally()%>%
              mutate(type5_top="All Projects")
            )%>%
  ungroup()%>%
  group_by(type5_top, SDG)%>%
  mutate(percentage=n/sum(n))%>%
  filter(value=="Yes")%>%
  select(-value)%>%
  ungroup()%>%
  mutate(SDG=str_replace_all(SDG, "SDG: ", ""),
         SDG=factor(SDG, levels=unique(SDG[order(percentage,SDG, decreasing = T)]), ordered=TRUE),
         #SDG=relevel(as.factor(SDG),ref="SDG: On a topic not related to SDGs")
         SDG=fct_relevel(SDG,"On a topic not related to SDGs", after = 0L)
  )%>%
  mutate(type5_top=
           factor(type5_top, levels=c("Extensive SI", "Oriented SI", "Unoriented SI", "Limited SI", "All Projects")
                  )
         )%>%
  # bind_rows(Flat_data_filtered_2%>%
  #             group_by(type5_top)%>%
  #             tally()%>%
  #             mutate(SDG="All Projects", percentage=n/sum(n)))%>%
  ggplot(aes(y=percentage, x=SDG, fill=SDG )) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(
    aes(label = scales::percent(percentage, accuracy = 0.1)),
    colour = "black", size = 3,
    vjust = -0.25, position = position_dodge(.5)
  )+
  # geom_text_repel(aes(label = scales::percent(percentage, accuracy = 0.1)),
  #                 position = position_dodge())+
  facet_wrap(~type5_top, labeller = label_wrap_gen(width=25), ncol = 1)+
  scale_y_continuous(limits=c(0, 0.5), labels = scales::percent)+
  theme(#axis.text.x=element_blank(),
    axis.text.x = element_text(angle = 40, vjust = 1, hjust=1),
    legend.position="none",
    strip.text = element_text(face="bold"))+
  labs(x="SDG",
       y="Percentage of Projects",
       fill="")+
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 24))


ggsave("figure_4.png", figure_4, dpi=900, units="mm", width=260, height = 140)



####Descriptives Table####

crosstable_options(crosstable_fishertest_B = 1e+05)
crosstable(
  Flat_data_filtered_2,
  cols = c(
    -everything(),
    -Project_id,
    "Includes a project location in Region: EU",
    "Includes a project location in Region: Non-EU Europe",
    "Includes a project location in Region: North America",
    "Includes a project location in Region: Asia",
    "Includes a project location in Region: RoW",
    "Project location is in more than one country",
    "SDG: Industry, innovation and infrastructure",
    "SDG: Quality education",
    "SDG: Decent work and economic growth",
    "SDG: Good health and well being",
    "SDG: Reduced inequalities",
    "SDG: No poverty",
    "SDG: Sustainable cities and communities",
    "SDG: Peace justice and strong institutions",
    "SDG: Responsible consumption and production",
    "SDG: Climate action",
    "SDG: Zero hunger",
    "SDG: Life on land",
    "SDG: Affordable and clean energy",
    "SDG: Life below water",
    "SDG: Clean water and sanitation",
    "SDG: On a topic not related to SDGs",
    "Total number of related SDGs",
    "Total number of related SDGs (Grouped)",
    "Covers Multiple SDGs"
     ),
  by = "type5_top",
  total = "both",
  percent_pattern = "{n} ({p_col})",
  showNA = "ifany",
  test = T
) %>%
  flextable::as_flextable() %>%
  flextable::autofit()%>%
  font(fontname = "Arial")%>%
  fontsize(size=9)%>%
  line_spacing(space = 1, part = "all")%>%
  set_table_properties(layout = "autofit")%>%
  flextable::save_as_docx(path = "descriptives_crosstable.docx")


####model####
typology_mn_07<- multinom(type5_top ~
                            `Includes a project location in Region: North America`+
                            #`Includesa project location in Region: South and Central America`+
                            `Includes a project location in Region: EU`+
                            `Includes a project location in Region: Non-EU Europe`+
                            `Includes a project location in Region: RoW`+
                            #`Includes a project location in Region: Oceania`+
                            `Includes a project location in Region: Asia`+
                            #`Includes a project location in Region: Asia`+
                            #`Includes a project location in Region: Unknown/Global`+
                            `Project location is in more than one country`+
                            `SDG: Industry, innovation and infrastructure`+
                            `SDG: Quality education`+
                            `SDG: Decent work and economic growth`+
                            `SDG: Good health and well being`+
                            `SDG: Reduced inequalities`+
                            `SDG: No poverty`+
                            `SDG: Sustainable cities and communities`+
                            `SDG: Peace justice and strong institutions`+
                            `SDG: Responsible consumption and production`+
                            `SDG: Climate action`+
                            `SDG: Zero hunger`+
                            `SDG: Life on land`+
                            `SDG: Affordable and clean energy`+
                            `SDG: Life below water`+
                            `SDG: Clean water and sanitation`+
                            `SDG: On a topic not related to SDGs`+
                            `Covers Multiple SDGs`,
                          #`Total number of related SDGs`,
                          #`Project location includes a country with Income Level: Low`+
                          #`Project location includes a country with Income Level: Lower-Middle`+
                          #`Project location includes a country with Income Level: Upper-Middle`+
                          #`Project location includes a country with Income Level: High`+
                          # `Project location includes a country with SPI Level: Low`+
                          # `Project location includes a country with SPI Level: Lower-Middle`+
                          # `Project location includes a country with SPI Level: Upper-Middle`+
                          # `Project location includes a country with SPI Level: High`,
                          # `Has an actor of type: Third Sector`+
                          # #`Has an actor of type: Non-Tertiary Education`,
                          # `Has an actor of type: University`+
                          # `Has an actor of type: Public Sector`+
                          # `Has an actor of type: For-profit Company`+
                          # `Has an actor of type: Inter-governmental Organisation`+
                          # #`Has an actor of type: Religious Organisation`,
                          # #`Has an actor of type: Other Tertiary Education`,
                          # #`Has an actor of type: Political Organisation`,
                          # #`Has an actor of type: Crowdfunding Platform`,
                          # `Total number of types of actors`,
                          # `Project location includes a country with Income Level: High & SPI Level: High`+
                          # `Project location includes a country with Income Level: Lower-Middle & SPI Level: Lower-Middle`+
                          # `Project location includes a country with Income Level: Upper-Middle & SPI Level: Upper-Middle`+
                          # `Project location includes a country with Income Level: Upper-Middle & SPI Level: Lower-Middle`+
                          # `Project location includes a country with Income Level: Low & SPI Level: Low`+
                          # `Project location includes a country with Income Level: Lower-Middle & SPI Level: Low`+
                          # `Project location includes a country with Income Level: Upper-Middle & SPI Level: High`+
                          # `Project location includes a country with Income Level: Lower-Middle & SPI Level: Upper-Middle`+
                          # `Project location includes a country with Income Level: High & SPI Level: Upper-Middle`+
                          # `Project location includes a country with Income Level: High & SPI Level: Lower-Middle`,
                          data=Flat_data_filtered_2,
                          maxit=1000)

tt_typology_mn_07 <- broom::tidy(typology_mn_07,conf.int=TRUE, conf.level = 0.90)%>%
  filter(term!="(Intercept)")%>%
  mutate(RRR=exp(estimate),
         Significance=case_when(p.value<=0.01~"99%",
                                p.value>0.01&p.value<=0.05~"95%",
                                p.value>0.05&p.value<=0.1~"90%",
                                TRUE~"Not Significant"),
         Association=case_when(RRR==1~"Zero",
                               RRR<1~"Negative",
                               RRR>1~"Positive",
                               TRUE~"NA"),
         conf.low.RRR=exp(conf.low),
         conf.high.RRR=exp(conf.high)
  )



typology_mn_level_order <- c(  
  "Includes a project location in Region: EU",
  "Includes a project location in Region: Non-EU Europe",
  "Includes a project location in Region: North America",
  #"Includesa project location in Region: South and Central America",
  "Includes a project location in Region: Asia",
  #"Includes a project location in Region: Africa",
  #"Includes a project location in Region: Oceania",
  "Includes a project location in Region: RoW",
  #"Includes a project location in Region: Unknown/Global",
  "Project location is in more than one country",
  "SDG: Industry, innovation and infrastructure",
  "SDG: Quality education",
  "SDG: Decent work and economic growth",
  "SDG: Good health and well being",
  "SDG: Reduced inequalities",
  "SDG: No poverty",
  "SDG: Sustainable cities and communities",
  "SDG: Peace justice and strong institutions",
  "SDG: Responsible consumption and production",
  "SDG: Climate action",
  "SDG: Zero hunger",
  "SDG: Life on land",
  "SDG: Affordable and clean energy",
  "SDG: Life below water",
  "SDG: Clean water and sanitation",
  "SDG: On a topic not related to SDGs",
  "Covers Multiple SDGs"
  #"Project location includes a country with Income Level: High & SPI Level: High",
  # "Project location includes a country with Income Level: Lower-Middle & SPI Level: Lower-Middle",
  #"Project location includes a country with Income Level: Upper-Middle & SPI Level: Upper-Middle",
  #"Project location includes a country with Income Level: Upper-Middle & SPI Level: Lower-Middle",
  #"Project location includes a country with Income Level: Low & SPI Level: Low",
  #  "Project location includes a country with Income Level: Lower-Middle & SPI Level: Low",
  # "Project location includes a country with Income Level: Upper-Middle & SPI Level: High",
  #"Project location includes a country with Income Level: Lower-Middle & SPI Level: Upper-Middle",
  #"Project location includes a country with Income Level: High & SPI Level: Upper-Middle",
  #"Project location includes a country with Income Level: High & SPI Level: Lower-Middle",
  # "Project location includes a country with SPI Level: Low",
  # "Project location includes a country with SPI Level: Lower-Middle",
  # "Project location includes a country with SPI Level: Upper-Middle",
  # "Project location includes a country with SPI Level: High"
  # # "Has an actor of type: Third Sector",
  #     "Has an actor of type: University",
  #   "Has an actor of type: Public Sector",
  #   "Has an actor of type: For-profit Company",
  #   "Has an actor of type: Inter-governmental Organisation",
  #    "Total number of types of actors"
)




figure_1 <- tt_typology_mn_07%>%
  mutate(y.level=factor(y.level, levels=c("Extensive SI",
                                          "Limited SI",
                                          "Oriented SI")),
         term=str_replace_all(term, "`Yes", ""),
         term=str_replace_all(term, "`", ""),
  )%>%
  mutate(term=factor(term, levels=typology_mn_level_order))%>%
  #filter(significance!="Not Significant")%>%
  ggplot(aes(x=RRR,y=
               term
             #factor(term, levels=multinom_level_order)
             , colour=Significance))+
  geom_point(aes(shape=Association), size=3)+
  #scale_x_continuous(trans = 'log10')+
  #scale_shape_manual(values=c(4, 3))+
  #xlim(-15,25)+
  xlab("Odds Ratio")+
  ylab("Independent Variable")+
  geom_line(aes(group=term)) + 
  geom_linerange(aes(xmin = conf.low.RRR, xmax=  conf.high.RRR)) +
  geom_vline(xintercept=1, linetype="dotted", alpha=0.7, color="black")+
  scale_y_discrete(limits=rev)+
  facet_wrap(~y.level, ncol=6, scales="free_x", labeller = label_wrap_gen(width=13))+
  theme(legend.position = "bottom",
        legend.box="vertical",
        legend.box.just = "left",
        legend.margin=margin(),
        legend.box.spacing=unit(0.1, "cm"),
        legend.spacing = unit(0.1, "cm"),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10, face="bold"),
        axis.title.y = element_text(size = 10, face="bold"),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10, face="bold"),
        strip.text.x.bottom=element_text(angle=45))+
  geom_text_repel(aes(label=round(RRR, 2)), arrow = arrow(length = unit(0.1, 'cm'), type = 'closed'))+
  #geom_text(aes(label=round(RRR, 2)), size=3, check_overlap = TRUE, vjust=-1, hjust="outward")+
  scale_color_manual(values=c("Not Significant"="grey", "99%"="red3", "95%"="darkorchid4", "90%"="violet"))




ggsave("figure_1.png", figure_1, dpi=900, units="mm", width=200, height = 240)


tt_typology_mn_07%>%
  mutate(y.level=factor(y.level, levels=c("Extensive SI",
                                          "Limited SI",
                                          "Oriented SI")),
         term=str_replace_all(term, "`Yes", ""),
         term=str_replace_all(term, "`", ""),
  )%>%
  mutate(term=factor(term, levels=typology_mn_level_order))%>%
  write.csv("S1_model_table.csv")




####unused####

###correlation between scores###

Flat_data_filtered_2%>%
  select(starts_with("Dimension:"))%>%
  mutate(across(dplyr::everything(), ~as.numeric(.x)))%>%
  cor()%>%
  corrplot(method="color", addCoef.col = "black", insig="blank", sig.level = 0.01, type="upper", tl.col="black", tl.srt=45, diag=F)




