#Set working directory -- You can type out the path but I like to 
#go to Session --> Set working directory --> Choose working directory...
setwd("~/Library/CloudStorage/Box-Box/Caloric Density Manuscript/code/")

#Grab the data file from your computer. I sometimes having issues working from Box,so you may want to download
#the file to your computer. When you run the below, a File explorer will appear and you can select the appropriate csv file
#datum is an object -- we can choose any name we want -- it's just what we're going to call our data file from now on.
fat=read.csv(file = "../data/MV225_Compiled_Weight.csv",header = T,stringsAsFactors = T)

#load libraries
library(ggplot2)
library(ggthemes)
library(ggprism)
library(ggpubr)
library(multcompView)
library(ragg)

##Overall strains ANOVA - note this part of the code was abandoned because the full model did not show either
##significant strain effect or an interaction term that was significant.
resultsAOV=aov(Percent~Treatment,data=fat)
summary(resultsAOV)

fat_plot=ggplot(data = fat, aes(Treatment,Percent)) +
  #  geom_boxplot(size = 1) + # commented out since it is duplicate of line below
  geom_boxplot(color="black", fill="gray") + #, alpha=0.4) + commented out alpha to make boxplots not transparent
  #  theme_bw() +  # commented out since it is overwritten by theme command below
  labs(title = "MV2-25 Body Fat",
       y = "Percent Body Fat", x = "Caloric Density") + 
  theme_base()

fat_plot