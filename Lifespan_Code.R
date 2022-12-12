##Analysis of weight at day 7

#Set working directory -- You can type out the path but I like to 
#go to Session --> Set working directory --> Choose working directory...
setwd("~/")

#Grab the data file from your computer. I sometimes having issues working from Box,so you may want to download
#the file to your computer. When you run the below, a File explorer will appear and you can select the appropriate csv file
#datum is an object -- we can choose any name we want -- it's just what we're going to call our data file from now on.
life=read.csv(file.choose())

##Overall
resultsAOV=aov(Age~Treatment+Sex,data=life)
summary(resultsAOV)
TukeyHSD(resultsAOV)

plot(Age~as.factor(Treatment),data=life,xlab="Caloric Density", ylab="Lifespan (Days)", 
     title="Average Age")


library(ggplot2)
ggplot(data = life, aes(Treatment,Age)) +
  geom_boxplot(size = 1) +
  geom_boxplot(color="black", fill="gray", alpha=0.4) +
  labs(title = "Age Across Strains",
       y = "Age (D)", x = "Caloric Density") + 
  theme_bw()+
  facet_wrap(~ Strain) +
  stat_compare_means(method="anova", color="red")