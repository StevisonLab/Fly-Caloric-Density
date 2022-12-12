##Analysis of weight at day 7

#Set working directory -- You can type out the path but I like to 
#go to Session --> Set working directory --> Choose working directory...
setwd("~/")

#Grab the data file from your computer. I sometimes having issues working from Box,so you may want to download
#the file to your computer. When you run the below, a File explorer will appear and you can select the appropriate csv file
#datum is an object -- we can choose any name we want -- it's just what we're going to call our data file from now on.
datum=read.csv(file.choose())
datum$Weight<-1000*(datum$Weight)

plot(Weight~as.factor(Strain),data=datum)
results=lm(Weight~as.factor(Treatment)+as.factor(Strain)+as.factor(Sex),data=datum)

##Overall
resultsAOV=aov(Weight~Treatment+Strain*Sex,data=datum)
summary(resultsAOV)
TukeyHSD(resultsAOV)

library(ggplot2)
ggplot(data = datum, aes(Treatment,Weight)) +
  labs(title="Body Weight Across Strains",y="Weight (mg)", x="Caloric Density")+
  geom_boxplot(size = 1) +
  geom_boxplot(color="black", fill="gray", alpha=0.4) +
  theme_bw() +
  facet_wrap(~ Strain)+
  stat_compare_means(method="anova", color="red")

#Females
Bey=read.csv(file.choose())
resultsAOV=aov(Weight~Treatment+Strain,data=Bey)
summary(resultsAOV)
TukeyHSD(resultsAOV)

ggplot(data = Bey, aes(Treatment,Weight)) +
  geom_boxplot(size = 1) +
  #labs(title = "Caloric Density Across Strains",
  #  y = "Body Weight (g)", x = "Caloric Density") + 
  facet_wrap(~ Strain) +
  stat_compare_means(method="anova", color="red")

Bey=read.csv(file.choose())
resultsAOV=aov(Weight~Treatment+Strain,data=datum)
summary(resultsAOV)
TukeyHSD(resultsAOV)

#Males
Dudes=read.csv(file.choose())
resultsAOV=aov(Weight~Treatment+Strain,data=Dudes)
summary(resultsAOV)
TukeyHSD(resultsAOV)

ggplot(data = Dudes, aes(Treatment,Weight)) +
  geom_boxplot(size = 1) +
  labs(title = "Caloric Density Across Strains",
   y = "Body Weight (g)", x = "Caloric Density") + 
  facet_wrap(~ Strain)

#OVERALL FAT
fatresultsAOV=aov(Percent~Treatment*Strain*Sex,data=datum)
summary(fatresultsAOV)
TukeyHSD(fatresultsAOV)

fatresultsAOV2=aov(Percent~Treatment,data=datum)
summary(fatresultsAOV2)
TukeyHSD(fatresultsAOV2)

ggplot(data = datum, aes(Treatment,Percent)) +
  geom_boxplot(size = 1) +
  labs(title = "Body Fat Across Caloric Density",
    y = "Body Fat (g)", x = "Caloric Density") + 
  facet_wrap(~ Strain)

library(ggplot2)