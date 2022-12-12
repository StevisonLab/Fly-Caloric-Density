##Analysis of weight at day 7

#Set working directory -- You can type out the path but I like to 
#go to Session --> Set working directory --> Choose working directory...
setwd("~/")

#Grab the data file from your computer. I sometimes having issues working from Box,so you may want to download
#the file to your computer. When you run the below, a File explorer will appear and you can select the appropriate csv file
#datum is an object -- we can choose any name we want -- it's just what we're going to call our data file from now on.
fec=read.csv(file.choose())

##Overall
resultsAOV=aov(Total~Treatment+Strain,data=fec)
resultsAOV=aov(Total~Treatment,data=fec)
summary(resultsAOV)
TukeyHSD(resultsAOV)

library(ggplot2)
ggplot(data = fec, aes(Treatment,Total)) +
  geom_boxplot(size = 1) +
  geom_boxplot(color="black", fill="gray", alpha=0.4) +
  theme_bw() +
  labs(title = "Fecundity Across Strains",
       y = "Number of Offspring", x = "Caloric Density") + 
  facet_wrap(~ Strain)+
  stat_compare_means(method="anova", color="red")
