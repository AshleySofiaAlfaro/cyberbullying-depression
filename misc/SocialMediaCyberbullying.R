## ——— Libraries ——— ##
install.packages("haven")
install.packages("ggforce")
library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggforce)

## ——— Importing Dataset ——— ##
DepressionAnxietyStress <- read_sav("Datasets/DepressionAnxietyStress.sav")
View(DepressionAnxietyStress)
names(DepressionAnxietyStress)

# Looking through Data: Selecting Columns
DepressionAnxietyStress$GENDER
DepressionAnxietyStress$AGE

DepressionAnxietyStress$ACTIVE_SOCIAL_MEDIA
DepressionAnxietyStress$TIME_SPENT_SOCIAL_MEDIA

DepressionAnxietyStress$DEPRESSLEVELS
DepressionAnxietyStress$ANXIETYLEVELS
DepressionAnxietyStress$STRESSLEVELS
DepressionAnxietyStress$CVPUBLICHUMILIATION
DepressionAnxietyStress$CVTOTAL

DepressionAnxietyStress$MALICE_2LEVELS
DepressionAnxietyStress$UNWANTEDCONTACT_2LEVELS
DepressionAnxietyStress$DECEPTION_2LEVELS
DepressionAnxietyStress$PUBLICHUMILIATION_2LEVELS

DepressionAnxietyStress %>% select(ACTIVE_SOCIAL_MEDIA, TIME_SPENT_SOCIAL_MEDIA, DEPRESSLEVELS)

#onlyMale <- filter(DepressionAnxietyStress, GENDER==1) - Total: 240
#onlyFemale <- filter(DepressionAnxietyStress, GENDER==2) - Total: 321
#rm(onlyMale, onlyFemale)

## ——— Subsetting Dataset ——— ##
#DEPRESSION
AllDepress <- DepressionAnxietyStress %>%
  select(ID, GENDER, TIME_SPENT_SOCIAL_MEDIA, CVTOTAL, DEPRESSLEVELS, CVPUBLICHUMILIATION, 
         CVMALICE, CVUNWANTEDCONTACT, CVDECEPTION, PUBLICHUMILIATION_2LEVELS, MALICE_2LEVELS,
         UNWANTEDCONTACT_2LEVELS, DECEPTION_2LEVELS, MEANPUBLICHUMILIATION, MEANMALICE,
         MEANDECEPTION, MEANUNWANTEDCONTACT) %>%
  filter(DEPRESSLEVELS!=1)

VeryHighDepress <- DepressionAnxietyStress %>%
  select(ID, GENDER, TIME_SPENT_SOCIAL_MEDIA, CVTOTAL, DEPRESSLEVELS, CVPUBLICHUMILIATION, 
         CVMALICE, CVUNWANTEDCONTACT, CVDECEPTION) %>%
  filter(DEPRESSLEVELS==5) 

HighDepress <- DepressionAnxietyStress %>%
  select(ID, GENDER, TIME_SPENT_SOCIAL_MEDIA, CVTOTAL, DEPRESSLEVELS) %>%
  filter(DEPRESSLEVELS==4) 

ModerateDepress <- DepressionAnxietyStress %>%
  select(ID, GENDER, TIME_SPENT_SOCIAL_MEDIA, CVTOTAL, DEPRESSLEVELS) %>%
  filter(DEPRESSLEVELS==3)

MildDepress <- DepressionAnxietyStress %>%
  select(ID, GENDER, TIME_SPENT_SOCIAL_MEDIA, CVTOTAL, DEPRESSLEVELS) %>%
  filter(DEPRESSLEVELS==2) 

NormalDepress <- DepressionAnxietyStress %>%
  select(ID, GENDER, TIME_SPENT_SOCIAL_MEDIA, CVTOTAL, DEPRESSLEVELS) %>%
  filter(DEPRESSLEVELS==1)
#BOTH
AllDepressAnxiety <- DepressionAnxietyStress %>%
  select(ID, GENDER, TIME_SPENT_SOCIAL_MEDIA, DEPRESSLEVELS, ANXIETYLEVELS, CVTOTAL, 
         CVPUBLICHUMILIATION, CVMALICE, CVUNWANTEDCONTACT, CVDECEPTION) %>%
  filter(ANXIETYLEVELS!=1, DEPRESSLEVELS!=1)

## ——— Remove Cells w/ NA ——— ##
AllDepressAnxietySM <- AllDepressAnxiety %>%
  drop_na((TIME_SPENT_SOCIAL_MEDIA))
rm(AllDepressAnxiety) #To get rid of other dataset w/ NA

AllDepressSM <- AllDepress %>%
  drop_na((TIME_SPENT_SOCIAL_MEDIA))
rm(AllDepress)

VeryHighDepressSM <- VeryHighDepress %>%
  drop_na((TIME_SPENT_SOCIAL_MEDIA))
rm(VeryHighDepress)

HighDepressSM <- HighDepress %>%
  drop_na((TIME_SPENT_SOCIAL_MEDIA))
rm(HighDepress)

ModerateDepressSM <- ModerateDepress %>%
  drop_na(TIME_SPENT_SOCIAL_MEDIA)
rm(ModerateDepress)

MildDepressSM <- MildDepress %>%
  drop_na(TIME_SPENT_SOCIAL_MEDIA)
rm(MildDepress)

AllDepressAnxietySM <- AllDepressAnxiety %>%
  drop_na(TIME_SPENT_SOCIAL_MEDIA)
rm(AllDepressAnxiety)


## ——— BAR PLOT - Total Depression Levels ——— ##
DepressionAnxietyStress$UNIVERSITY_NAME <- haven::as_factor(DepressionAnxietyStress$UNIVERSITY_NAME) 
AllDepressSM$DEPRESSLEVELS <- haven::as_factor(AllDepressSM$DEPRESSLEVELS) 
#Doing this can mess up the graphs (boxplot, violin)
AllDepressSM$TIME_SPENT_SOCIAL_MEDIA <- haven::as_factor(AllDepressSM$TIME_SPENT_SOCIAL_MEDIA)
AllDepressSM$GENDER <- haven::as_factor(AllDepressSM$GENDER)
AllDepressSM$PUBLICHUMILIATION_2LEVELS <- haven::as_factor(AllDepressSM$PUBLICHUMILIATION_2LEVELS)
AllDepressSM$MALICE_2LEVELS <- haven::as_factor(AllDepressSM$MALICE_2LEVELS)
AllDepressSM$UNWANTEDCONTACT_2LEVELS <- haven::as_factor(AllDepressSM$UNWANTEDCONTACT_2LEVELS)
AllDepressSM$CVDECEPTION <- haven::as_factor(AllDepressSM$CVDECEPTION)


AllDepressSM %>%
  ggplot(aes(x=DEPRESSLEVELS,
             fill=DEPRESSLEVELS))+
  geom_bar(stat="count")+
  stat_count(geom = "text", color = "white", size = 4.5,
             aes(label = after_stat(count)),position=position_stack(vjust=0.5))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  labs(title="Number of Individuals with Depression",
       x="Depression Levels",
       y="Total")


## ——— BOX PLOT - Comparing CVTOTAL and DEPRESSLEVELS ——— ##
# Box Plot + Violin Ver 
AllDepressSM %>%
  ggplot(aes(x=DEPRESSLEVELS,
             y=CVTOTAL,
             color=DEPRESSLEVELS,
             fill=DEPRESSLEVELS))+
  geom_violin(alpha=0.6, linewidth=0.8)+
  geom_boxplot(alpha=1, linewidth=0.6, width=0.1, fill="white", color="black")+
  theme_bw()+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme(text = element_text(family = "Century"),
        plot.title = element_text(hjust=0.5, face="bold", size=22),
        plot.caption = element_text(size=10),
        plot.subtitle = element_text(hjust=0.5, size=14),
        axis.title.y = element_text(face="bold", size=15),
        axis.title.x = element_text(face="bold", size=15),
        axis.text.x.bottom = element_text(size=12, color="black"),
        axis.text.y.left = element_text(size=12, color="black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.position = "bottom",
        legend.background = element_rect(linewidth=0.5, color="light gray"))+
  labs(title="Impacts of Cyberbullying on Depression",
       x="Depression Levels",
       y="Cyberbullying Victimization Score",
       fill="Depression Levels",
       color="Depression Levels",
       caption="
NOTE: Scores came from a survey the participants had filled out.
      The higher the score, the more instances of cyberbullying they have encountered.")


# Box Plot + Violin Ver - Grouped by GENDER
AllDepressSM %>%
  ggplot(aes(x=DEPRESSLEVELS,
             y=CVTOTAL,
             fill=DEPRESSLEVELS,
             color=DEPRESSLEVELS))+
  #geom_col(alpha=1, formula = y ~ x, method="lm")+
  geom_violin(alpha=0.6, size=0.8, width=1)+
  geom_boxplot(alpha=0.9, size=0.4, width=0.2, fill="white", color="black")+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  theme(panel.spacing = unit(2, "lines"))+
  labs(x=NULL,
       y="Cyberbullying Victimization Score",
       fill="Depression Levels",
       color="Depression Levels")+
  facet_wrap(~GENDER)


## BAR PLOT - Comparing CV Groups w/ DEPRESSLEVELS ##
# - Public Humiliation
AllDepressSM %>%
  ggplot(aes(y=CVPUBLICHUMILIATION,
             fill=DEPRESSLEVELS))+
  geom_bar(stat="count", width=0.9)+
  stat_count(geom = "text", colour = "white", size = 2.5,
             aes(label = after_stat(count)),position=position_stack(vjust=0.5))+
  facet_wrap(~DEPRESSLEVELS)+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_light()+
  labs(title="Public Humiliation",
       subtitle="Figure #",
       x="Total",
       y="Public Humiliation Score")+
  theme(plot.title = element_text(face="bold", size=18, hjust=0.5), 
        plot.subtitle = element_text(hjust=0.5, face="italic", size=14),
        axis.title.x.bottom = element_text(face="bold", size=12),
        axis.title.y.left = element_text(face="bold", size=12),
        panel.spacing = unit(0.5, "line"),
        strip.text = element_text(size=10, face="bold"),
        legend.title = element_text(size=10),
        legend.position = "bottom",
        legend.background = element_rect(color="light gray", linewidth=0.5))+
  guides(
    fill=guide_legend("Depression Levels"))
#theme(axis.text.x = element_text(margin = margin(t = -0.1, unit = "in")))

#Ver 2 - Violin & Box Plot - CVPUBLICHUMILIATION 
AllDepressSM %>%
  ggplot(aes(x=DEPRESSLEVELS,
             y=CVPUBLICHUMILIATION,
             color=DEPRESSLEVELS,
             fill=DEPRESSLEVELS))+
  geom_violin(alpha=0.65, linewidth=0.8)+
  geom_boxplot(alpha=1, linewidth=0.7, width=0.1, fill="white", color="black")+
  theme_bw()+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme(text = element_text(family = "Century"),
        plot.title = element_text(hjust=0.5, face="bold", size=22),
        plot.subtitle = element_text(hjust=0.5, size=14),
        axis.title.y = element_text(face="bold", size=15),
        axis.title.x = element_text(face="bold", size=15),
        axis.text.x.bottom = element_text(size=12, color="black"),
        axis.text.y.left = element_text(size=12, color="black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.position = "bottom",
        legend.background = element_rect(linewidth=0.5, color="light gray"))+
  labs(title="Public Humiliation",
       x="Depression Levels",
       y="Public Humiliation Score",
       fill="Depression Levels",
       color="Depression Levels")

#Ver 4 - Using PUBLICHUMILATION_2LEVELS
AllDepressSM %>%
  ggplot(aes(x=PUBLICHUMILIATION_2LEVELS,
             fill=DEPRESSLEVELS))+
  geom_bar(stat="count")+
  stat_count(geom="text", color="white", size=3.5,
             aes(label=after_stat(count)), position=position_stack(vjust=0.5))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  facet_wrap(~DEPRESSLEVELS)

# - Malice
# Using MALICE_2LEVELS
AllDepressSM %>%
  ggplot(aes(x=MALICE_2LEVELS,
             fill=DEPRESSLEVELS))+
  geom_bar(stat="count")+
  stat_count(geom="text", color="white", size=3.5,
             aes(label=after_stat(count)), position=position_stack(vjust=0.5))+
  facet_wrap(~DEPRESSLEVELS)+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  labs(title="Malice",
       x=NULL,
       y="Total")

# Ver 2 - Using CVMALICE, Violin + Box Plot
AllDepressSM %>%
  ggplot(aes(x=DEPRESSLEVELS,
             y=CVMALICE,
             color=DEPRESSLEVELS,
             fill=DEPRESSLEVELS))+
  geom_violin(alpha=0.65, linewidth=0.8)+
  geom_boxplot(alpha=1, linewidth=0.7, width=0.1, fill="white", color="black")+
  theme_bw()+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme(text = element_text(family = "Century"),
        plot.title = element_text(hjust=0.5, face="bold", size=22),
        plot.subtitle = element_text(hjust=0.5, size=14),
        axis.title.y = element_text(face="bold", size=15),
        axis.title.x = element_text(face="bold", size=15),
        axis.text.x.bottom = element_text(size=12, color="black"),
        axis.text.y.left = element_text(size=12, color="black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.position = "bottom",
        legend.background = element_rect(linewidth=0.5, color="light gray"))+
  labs(title="Malice",
       x="Depression Levels",
       y="Malice Score",
       fill="Depression Levels",
       color="Depression Levels")

# - Unwanted Contact
AllDepressSM %>%
  ggplot(aes(x=DEPRESSLEVELS,
             y=CVUNWANTEDCONTACT,
             color=DEPRESSLEVELS,
             fill=DEPRESSLEVELS))+
  geom_violin(alpha=0.6, linewidth=0.8)+
  geom_boxplot(alpha=1, linewidth=0.6, width=0.1, fill="white", color="black")+
  theme_bw()+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme(text = element_text(family = "Century"),
        plot.title = element_text(hjust=0.5, face="bold", size=22),
        plot.subtitle = element_text(hjust=0.5, size=14),
        axis.title.y = element_text(face="bold", size=15),
        axis.title.x = element_text(face="bold", size=15),
        axis.text.x.bottom = element_text(size=12, color="black"),
        axis.text.y.left = element_text(size=12, color="black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.position = "bottom",
        legend.background = element_rect(linewidth=0.5, color="light gray"))+
  labs(title="Unwanted Contact",
       x="Depression Levels",
       y="Unwanted Contact Score",
       fill="Depression Levels",
       color="Depression Levels")

# - Deception
AllDepressSM %>%
  ggplot(aes(y=CVDECEPTION,
             fill=DEPRESSLEVELS))+
  geom_bar(stat="count", width=0.9)+
  stat_count(geom = "text", colour = "white", size = 2.8,
             aes(label = after_stat(count)),position=position_stack(vjust=0.5))+
  facet_wrap(~DEPRESSLEVELS)+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  #theme(title = element_text(face="bold"))+
  labs(title="Deception",
       subtitle="Type of Cyberbullying Victimization",
       x="Total",
       y="Deception Score")

# Ver 2 - VIOLIN + BOX PLOT
AllDepressSM %>%
  ggplot(aes(x=DEPRESSLEVELS,
             y=CVDECEPTION,
             color=DEPRESSLEVELS,
             fill=DEPRESSLEVELS))+
  geom_violin(alpha=0.6, linewidth=0.8)+
  geom_boxplot(alpha=1, linewidth=0.6, width=0.1, fill="white", color="black")+
  theme_bw()+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme(text = element_text(family = "Century"),
        plot.title = element_text(hjust=0.5, face="bold", size=22),
        plot.subtitle = element_text(hjust=0.5, size=14),
        axis.title.y = element_text(face="bold", size=15),
        axis.title.x = element_text(face="bold", size=15),
        axis.text.x.bottom = element_text(size=12, color="black"),
        axis.text.y.left = element_text(size=12, color="black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.position = "bottom",
        legend.background = element_rect(linewidth=0.5, color="light gray"))+
  labs(title="Deception",
       x="Depression Levels",
       y="Deception Score",
       fill="Depression Levels",
       color="Depression Levels")


## ——— BAR PLOT - Depression Lvls and Time Spent on SM ——— ##
# - Grouped by DEPRESSLEVELS
AllDepressSM %>%
  ggplot(aes(x=TIME_SPENT_SOCIAL_MEDIA,
             fill=DEPRESSLEVELS))+
  #scale_color_brewer(palette = "Blues")+
  #scale_fill_brewer(palette = "Blues")+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  geom_bar(stat="count")+
  stat_count(geom = "text", colour = "white", size = 3,
             aes(label = after_stat(count)),position=position_stack(vjust=0.5))+
  theme_bw()+
  facet_wrap(~DEPRESSLEVELS)+
  guides(fill=guide_legend(
    title="Depression Levels",
    order=1))+
  labs(title="Time Spent on Social Media",
       subtitle="Depression - Figure #",
       x="Time Spent on Social Media",
       y="Total")

AllDepressSM %>%
  ggplot(aes(x=DEPRESSLEVELS,
             fill=TIME_SPENT_SOCIAL_MEDIA))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  geom_bar(stat="count")+
  stat_count(geom = "text", colour = "white", size = 3,
             aes(label = ..count..),position=position_stack(vjust=0.5))+
  theme_bw()+
  #facet_wrap(~TIME_SPENT_SOCIAL_MEDIA)+
  theme(panel.spacing = unit(1, "lines"))+
  theme(plot.title = element_text(hjust=0.5))+
  labs(title="Time Spent on Social Media",
       subtitle="Figure #",
       x="Depression Levels",
       y="Total",
       fill="Time")