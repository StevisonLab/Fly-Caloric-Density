##Analysis of weight at day 7

#Set working directory -- You can type out the path but I like to 
#go to Session --> Set working directory --> Choose working directory...
setwd("~/")

#Grab the data file from your computer. I sometimes having issues working from Box,so you may want to download
#the file to your computer. When you run the below, a File explorer will appear and you can select the appropriate csv file
#datum is an object -- we can choose any name we want -- it's just what we're going to call our data file from now on.
datum=read.csv(file.choose())

resultsAOV=aov(Treatment~as.factor(Total),data=datum)
summary(resultsAOV)
TukeyHSD(resultsAOV)

library(ggplot2)
library(ggpubr)
ggplot(data = datum, aes(Treatment,Total)) +
  geom_boxplot(size = 1) +
  geom_boxplot(color="black", fill="gray", alpha=0.4) +
  labs(title = "MV2-25 Offspring per Female across Caloric Densities",
       y = "Number of Offspring", x = "Caloric Density") +
  theme_bw()+
 stat_compare_means(method="anova",label.y=400,color="red")

