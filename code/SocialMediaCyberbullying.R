## ——— Libraries ——— ##
library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(stats)
library(cluster)
library(factoextra)
library(ggalt)
library(ggpubr)
install.packages("ggalt")

## ——— Importing Dataset ——— ##
DepressionAnxietyStress <- read_sav("Datasets/DepressionAnxietyStress.sav")
View(DepressionAnxietyStress)
names(DepressionAnxietyStress)

# Looking through Data: Selecting Columns
DepressionAnxietyStress$DEPRESSLEVELS
DepressionAnxietyStress$CVPUBLICHUMILIATION
DepressionAnxietyStress$CVTOTAL
DepressionAnxietyStress$CV1

#onlyMale <- filter(DepressionAnxietyStress, GENDER==1) - Total: 240
#onlyFemale <- filter(DepressionAnxietyStress, GENDER==2) - Total: 321
#rm(onlyMale, onlyFemale)

## ——— Subsetting Dataset ——— ##
#DEPRESSION
AllDepress <- DepressionAnxietyStress %>%
  select(ID, GENDER, TIME_SPENT_SOCIAL_MEDIA, CVTOTAL, DEPRESSLEVELS, CVPUBLICHUMILIATION, 
         CVMALICE, CVUNWANTEDCONTACT, CVDECEPTION, SUMDEPRESS, DEPRESSMULTIPLIED2, 
         MEANPUBLICHUMILIATION, MEANMALICE, MEANDECEPTION, MEANUNWANTEDCONTACT) %>%
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
AllDepressSM$CVDECEPTION <- haven::as_factor(AllDepressSM$CVDECEPTION)
AllDepressSM$ACTIVE_SOCIAL_MEDIA <- haven::as_factor(AllDepressSM$ACTIVE_SOCIAL_MEDIA)


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


## ——— K-MEANS - Depression Levels, Scores, Individual ——— ##
# PUBLIC HUMILIATION (4 is CVTOTAL, 10 is SUMDEPRESS)

# Select relevant columns and scale the data
public_scaled <- scale(AllDepressSM[c("CVPUBLICHUMILIATION","CVMALICE")])

set.seed(123)
# Perform k-means clustering on the scaled data
kmeans_result <- kmeans(public_scaled, centers=4, nstart=25)

# Create a data frame that includes the original data and the cluster assignments
AllDepressSM$cluster <- factor(kmeans_result$cluster)

# Plot the clustering results
# PUBLIC + MALICE
ggplot(AllDepressSM, aes(x=CVPUBLICHUMILIATION, y=CVMALICE, 
                         color=cluster, shape=DEPRESSLEVELS))+
  geom_point(size=3)+
  geom_encircle(aes(group=cluster,fill=cluster),alpha=0.15,s_shape=1,expand=0.02)+
  scale_shape_manual(values=c(19, 17, 15, 8))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  labs(title="K-Means Clustering of Public Humiliation and Malice Scores",
       x="Public Humiliation Score", y="Malice Score",
       color="Cluster", fill="Cluster", shape="Depression Levels")+
  theme(text = element_text(family = "Century"),
        plot.title=element_text(size=20, face="bold", hjust=0.5),
        axis.title.y = element_text(face="bold", size=16),
        axis.title.x = element_text(face="bold", size=16),
        axis.text.x.bottom = element_text(size=14, color="black"),
        axis.text.y.left = element_text(size=14, color="black"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.position="bottom",
        legend.background = element_rect(linewidth=0.4, color="light gray"))+
  scale_x_continuous(n.breaks=8)+
  scale_y_continuous(n.breaks=8)

# PUBLIC + UNWANTED CONTACT
public2_scaled <- scale(AllDepressSM[c("CVPUBLICHUMILIATION", "CVUNWANTEDCONTACT")])
kmeans_result2 <- kmeans(public2_scaled, centers=4, nstart=25)
AllDepressSM$cluster <- factor(kmeans_result2$cluster)

ggplot(AllDepressSM, aes(x=CVPUBLICHUMILIATION, y=CVUNWANTEDCONTACT, 
                         color=cluster, shape=DEPRESSLEVELS))+
  geom_point(size=3)+
  geom_encircle(aes(group=cluster,fill=cluster),alpha=0.15,s_shape=1,expand=0.02)+
  scale_shape_manual(values=c(19, 17, 15, 8))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  labs(title="K-Means Clustering of Public Humiliation and Unwanted Contact Scores",
       x="Public Humiliation Score", y="Unwanted Contact Score",
       color="Cluster", fill="Cluster", shape="Depression Levels")+
  theme(text = element_text(family = "Century"),
        plot.title=element_text(size=20, face="bold", hjust=0.5),
        axis.title.y = element_text(face="bold", size=16),
        axis.title.x = element_text(face="bold", size=16),
        axis.text.x.bottom = element_text(size=14, color="black"),
        axis.text.y.left = element_text(size=14, color="black"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.position="bottom",
        legend.background = element_rect(linewidth=0.4, color="light gray"))+
  scale_x_continuous(n.breaks=8)+
  scale_y_continuous(n.breaks=7)

# PUBLIC + DECEPTION
public3_scaled <- scale(AllDepressSM[c("CVPUBLICHUMILIATION", "CVDECEPTION")])
kmeans_result3 <- kmeans(public3_scaled, centers=4, nstart=25)
AllDepressSM$cluster <- factor(kmeans_result3$cluster)

ggplot(AllDepressSM, aes(x=CVPUBLICHUMILIATION, y=CVDECEPTION, 
                         color=cluster, shape=DEPRESSLEVELS))+
  geom_point(size=3)+
  geom_encircle(aes(group=cluster,fill=cluster),alpha=0.15,s_shape=1,expand=0.02)+
  scale_shape_manual(values=c(19, 17, 15, 8))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  labs(title="K-Means Clustering of Public Humiliation and Deception Scores",
       x="Public Humiliation Score", y="Deception Score",
       color="Cluster", fill="Cluster", shape="Depression Levels")+
  theme(text = element_text(family = "Century"),
        plot.title=element_text(size=20, face="bold", hjust=0.5),
        axis.title.y = element_text(face="bold", size=16),
        axis.title.x = element_text(face="bold", size=16),
        axis.text.x.bottom = element_text(size=14, color="black"),
        axis.text.y.left = element_text(size=14, color="black"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.position="bottom",
        legend.background = element_rect(linewidth=0.4, color="light gray"))+
  scale_x_continuous(n.breaks=8)+
  scale_y_continuous(n.breaks=8)


# MALICE + PUBLIC
malice_scaled <- scale(AllDepressSM[c("CVMALICE", "CVPUBLICHUMILIATION")])
kmeans_result <- kmeans(malice_scaled, centers=4, nstart=25)
AllDepressSM$cluster <- factor(kmeans_result$cluster)

ggplot(AllDepressSM, aes(x=CVMALICE, y=CVPUBLICHUMILIATION, 
                         color=cluster, shape=DEPRESSLEVELS))+
  geom_point(size=3)+
  geom_encircle(aes(group=cluster,fill=cluster),alpha=0.15,s_shape=1,expand=0.02)+
  scale_shape_manual(values=c(19, 17, 15, 8))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  labs(title="K-Means Clustering of Malice and Public Humiliation Scores",
       x="Malice Score", y="Public Humiliation Score",
       color="Cluster", fill="Cluster", shape="Depression Levels")+
  theme(text = element_text(family = "Century"),
        plot.title=element_text(size=20, face="bold", hjust=0.5),
        axis.title.y = element_text(face="bold", size=16),
        axis.title.x = element_text(face="bold", size=16),
        axis.text.x.bottom = element_text(size=14, color="black"),
        axis.text.y.left = element_text(size=14, color="black"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.position="bottom",
        legend.background = element_rect(linewidth=0.4, color="light gray"))+
  scale_x_continuous(n.breaks=8)+
  scale_y_continuous(n.breaks=8)

# MALICE + UNWANTED CONTACT
malice2_scaled <- scale(AllDepressSM[c("CVMALICE", "CVUNWANTEDCONTACT")])
kmeans_result2 <- kmeans(malice2_scaled, centers=4, nstart=25)
AllDepressSM$cluster <- factor(kmeans_result2$cluster)

ggplot(AllDepressSM, aes(x=CVMALICE, y=CVUNWANTEDCONTACT, 
                         color=cluster, shape=DEPRESSLEVELS))+
  geom_point(size=3)+
  geom_encircle(aes(group=cluster,fill=cluster),alpha=0.15,s_shape=1,expand=0.02)+
  scale_shape_manual(values=c(19, 17, 15, 8))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  labs(title="K-Means Clustering of Malice and Unwanted Contact Scores",
       x="Malice Score", y="Unwanted Contact Score",
       color="Cluster", fill="Cluster", shape="Depression Levels")+
  theme(text = element_text(family = "Century"),
        plot.title=element_text(size=20, face="bold", hjust=0.5),
        axis.title.y = element_text(face="bold", size=16),
        axis.title.x = element_text(face="bold", size=16),
        axis.text.x.bottom = element_text(size=14, color="black"),
        axis.text.y.left = element_text(size=14, color="black"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.position="bottom",
        legend.background = element_rect(linewidth=0.4, color="light gray"))+
  scale_x_continuous(n.breaks=8)+
  scale_y_continuous(n.breaks=7)

# MALICE + DECEPTION
malice3_scaled <- scale(AllDepressSM[c("CVMALICE", "CVDECEPTION")])
kmeans_result3 <- kmeans(malice3_scaled, centers=4, nstart=25)
AllDepressSM$cluster <- factor(kmeans_result3$cluster)

ggplot(AllDepressSM, aes(x=CVMALICE, y=CVDECEPTION, 
                         color=cluster, shape=DEPRESSLEVELS))+
  geom_point(size=3)+
  geom_encircle(aes(group=cluster,fill=cluster),alpha=0.15,s_shape=1,expand=0.02)+
  scale_shape_manual(values=c(19, 17, 15, 8))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  labs(title="K-Means Clustering of Malice and Deception Scores",
       x="Malice Score", y="Deception Score",
       color="Cluster", fill="Cluster", shape="Depression Levels")+
  theme(text = element_text(family = "Century"),
        plot.title=element_text(size=20, face="bold", hjust=0.5),
        axis.title.y = element_text(face="bold", size=16),
        axis.title.x = element_text(face="bold", size=16),
        axis.text.x.bottom = element_text(size=14, color="black"),
        axis.text.y.left = element_text(size=14, color="black"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.position="bottom",
        legend.background = element_rect(linewidth=0.4, color="light gray"))+
  scale_x_continuous(n.breaks=8)+
  scale_y_continuous(n.breaks=7)

# UNWANTED + PUBLIC
unwanted_scaled <- scale(AllDepressSM[c("CVUNWANTEDCONTACT", "CVPUBLICHUMILIATION")])
kmeans_result <- kmeans(unwanted_scaled, centers=4, nstart=25)
AllDepressSM$cluster <- factor(kmeans_result$cluster)

ggplot(AllDepressSM, aes(x=CVUNWANTEDCONTACT, y=CVPUBLICHUMILIATION, 
                         color=cluster, shape=DEPRESSLEVELS))+
  geom_point(size=3)+
  geom_encircle(aes(group=cluster,fill=cluster),alpha=0.15,s_shape=1,expand=0.02)+
  scale_shape_manual(values=c(19, 17, 15, 8))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  labs(title="K-Means Clustering of Unwanted Contact and Public Humiliation Scores",
       x="Unwanted Contact Score", y="Public Humiliation Score",
       color="Cluster", fill="Cluster", shape="Depression Levels")+
  theme(text = element_text(family = "Century"),
        plot.title=element_text(size=20, face="bold", hjust=0.5),
        axis.title.y = element_text(face="bold", size=16),
        axis.title.x = element_text(face="bold", size=16),
        axis.text.x.bottom = element_text(size=14, color="black"),
        axis.text.y.left = element_text(size=14, color="black"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.position="bottom",
        legend.background = element_rect(linewidth=0.4, color="light gray"))+
  scale_x_continuous(n.breaks=7)+
  scale_y_continuous(n.breaks=8)

# UNWANTED + MALICE
unwanted2_scaled <- scale(AllDepressSM[c("CVUNWANTEDCONTACT", "CVMALICE")])
kmeans_result2 <- kmeans(unwanted2_scaled, centers=4, nstart=25)
AllDepressSM$cluster <- factor(kmeans_result2$cluster)

ggplot(AllDepressSM, aes(x=CVUNWANTEDCONTACT, y=CVMALICE, 
                         color=cluster, shape=DEPRESSLEVELS))+
  geom_point(size=3)+
  geom_encircle(aes(group=cluster,fill=cluster),alpha=0.15,s_shape=1,expand=0.02)+
  scale_shape_manual(values=c(19, 17, 15, 8))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  labs(title="K-Means Clustering of Unwanted Contact and Malice Scores",
       x="Unwanted Contact Score", y="Malice Score",
       color="Cluster", fill="Cluster", shape="Depression Levels")+
  theme(text = element_text(family = "Century"),
        plot.title=element_text(size=20, face="bold", hjust=0.5),
        axis.title.y = element_text(face="bold", size=16),
        axis.title.x = element_text(face="bold", size=16),
        axis.text.x.bottom = element_text(size=14, color="black"),
        axis.text.y.left = element_text(size=14, color="black"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.position="bottom",
        legend.background = element_rect(linewidth=0.4, color="light gray"))+
  scale_x_continuous(n.breaks=7)+
  scale_y_continuous(n.breaks=8)

# UNWANTED + DECEPTION
unwanted3_scaled <- scale(AllDepressSM[c("CVUNWANTEDCONTACT", "CVDECEPTION")])
kmeans_result3 <- kmeans(unwanted3_scaled, centers=4, nstart=30)
AllDepressSM$cluster <- factor(kmeans_result3$cluster)

ggplot(AllDepressSM, aes(x=CVUNWANTEDCONTACT, y=CVDECEPTION, 
                         color=cluster, shape=DEPRESSLEVELS))+
  geom_point(size=3)+
  geom_encircle(aes(group=cluster,fill=cluster),alpha=0.15,s_shape=1,expand=0.02)+
  scale_shape_manual(values=c(19, 17, 15, 8))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  labs(title="K-Means Clustering of Unwanted Contact and Deception Scores",
       x="Unwanted Contact Score", y="Deception Score",
       color="Cluster", fill="Cluster", shape="Depression Levels")+
  theme(text = element_text(family = "Century"),
        plot.title=element_text(size=20, face="bold", hjust=0.5),
        axis.title.y = element_text(face="bold", size=16),
        axis.title.x = element_text(face="bold", size=16),
        axis.text.x.bottom = element_text(size=14, color="black"),
        axis.text.y.left = element_text(size=14, color="black"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.position="bottom",
        legend.background = element_rect(linewidth=0.4, color="light gray"))+
  scale_x_continuous(n.breaks=7)+
  scale_y_continuous(n.breaks=8)

# DECEPTION + PUBLIC
deception_scaled <- scale(AllDepressSM[c("CVDECEPTION", "CVPUBLICHUMILIATION")])
kmeans_result <- kmeans(deception_scaled, centers=4, nstart=25)
AllDepressSM$cluster <- factor(kmeans_result$cluster)

ggplot(AllDepressSM, aes(x=CVDECEPTION, y=CVPUBLICHUMILIATION, 
                         color=cluster, shape=DEPRESSLEVELS))+
  geom_point(size=3)+
  geom_encircle(aes(group=cluster,fill=cluster),alpha=0.15,s_shape=1,expand=0.02)+
  scale_shape_manual(values=c(19, 17, 15, 8))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  labs(title="K-Means Clustering of Deception and Public Humiliation Scores",
       x="Deception Score", y="Public Humiliation Score",
       color="Cluster", fill="Cluster", shape="Depression Levels")+
  theme(text = element_text(family = "Century"),
        plot.title=element_text(size=20, face="bold", hjust=0.5),
        axis.title.y = element_text(face="bold", size=16),
        axis.title.x = element_text(face="bold", size=16),
        axis.text.x.bottom = element_text(size=14, color="black"),
        axis.text.y.left = element_text(size=14, color="black"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.position="bottom",
        legend.background = element_rect(linewidth=0.4, color="light gray"))+
  scale_x_continuous(n.breaks=7)+
  scale_y_continuous(n.breaks=8)

# DECEPTION + MALICE
deception2_scaled <- scale(AllDepressSM[c("CVDECEPTION", "CVMALICE")])
kmeans_result2 <- kmeans(deception2_scaled, centers=4, nstart=25)
AllDepressSM$cluster <- factor(kmeans_result2$cluster)

ggplot(AllDepressSM, aes(x=CVDECEPTION, y=CVMALICE, 
                         color=cluster, shape=DEPRESSLEVELS))+
  geom_point(size=3)+
  geom_encircle(aes(group=cluster,fill=cluster),alpha=0.15,s_shape=1,expand=0.02)+
  scale_shape_manual(values=c(19, 17, 15, 8))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  labs(title="K-Means Clustering of Deception and Malice Scores",
       x="Deception Score", y="Malice Score",
       color="Cluster", fill="Cluster", shape="Depression Levels")+
  theme(text = element_text(family = "Century"),
        plot.title=element_text(size=20, face="bold", hjust=0.5),
        axis.title.y = element_text(face="bold", size=16),
        axis.title.x = element_text(face="bold", size=16),
        axis.text.x.bottom = element_text(size=14, color="black"),
        axis.text.y.left = element_text(size=14, color="black"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.position="bottom",
        legend.background = element_rect(linewidth=0.4, color="light gray"))+
  scale_x_continuous(n.breaks=7)+
  scale_y_continuous(n.breaks=8)

# DECEPTION + UNWANTED  
deception3_scaled <- scale(AllDepressSM[c("CVDECEPTION", "CVUNWANTEDCONTACT")])
kmeans_result3 <- kmeans(deception3_scaled, centers=4, nstart=25)
AllDepressSM$cluster <- factor(kmeans_result3$cluster)

ggplot(AllDepressSM, aes(x=CVDECEPTION, y=CVUNWANTEDCONTACT, 
                         color=cluster, shape=DEPRESSLEVELS))+
  geom_point(size=3)+
  geom_encircle(aes(group=cluster,fill=cluster),alpha=0.15,s_shape=1,expand=0.02)+
  scale_shape_manual(values=c(19, 17, 15, 8))+
  scale_fill_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  scale_color_manual(values=c("#adb886", "#5f9f80", "#00797f", "#005076", "#082455"))+
  theme_bw()+
  labs(title="K-Means Clustering of Deception and Unwanted Contact Scores",
       x="Deception Score", y="Unwanted Contact Score",
       color="Cluster", fill="Cluster", shape="Depression Levels")+
  theme(text = element_text(family = "Century"),
        plot.title=element_text(size=20, face="bold", hjust=0.5),
        axis.title.y = element_text(face="bold", size=16),
        axis.title.x = element_text(face="bold", size=16),
        axis.text.x.bottom = element_text(size=14, color="black"),
        axis.text.y.left = element_text(size=14, color="black"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.position="bottom",
        legend.background = element_rect(linewidth=0.4, color="light gray"))+
  scale_x_continuous(n.breaks=7)+
  scale_y_continuous(n.breaks=7)

## ——— VIOLIN + BOX PLOT - Comparing CVTOTAL and DEPRESSLEVELS ——— ##
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

# - Malice
# Using CVMALICE, Violin + Box Plot
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
  geom_violin(alpha=0.6, linewidth=0.8, width=1.1)+
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
# VIOLIN + BOX PLOT
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
       color="Depression Levels")+
  scale_y_continuous(n.breaks=4)
