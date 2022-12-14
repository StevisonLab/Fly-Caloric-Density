##Analysis of weight at day 7

#Set working directory -- You can type out the path but I like to 
#go to Session --> Set working directory --> Choose working directory...
#setwd("~/")
setwd("~/Library/CloudStorage/Box-Box/Caloric Density Manuscript/code/")

#load libraries
library(ggplot2)
library(ggthemes)
library(ggprism)
library(ggpubr)
library(multcompView)
library(ragg)

#Grab the data file from your computer. I sometimes having issues working from Box,so you may want to download
#the file to your computer. When you run the below, a File explorer will appear and you can select the appropriate csv file
#datum is an object -- we can choose any name we want -- it's just what we're going to call our data file from now on.
intake=read.csv(file = "../data/Intake.csv",header = T,stringsAsFactors = T)

#get labels for Tukey's result in plot
#function from here: https://r-graph-gallery.com/84-tukey-test.html
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}


##Overall strains ANOVA - note this part of the code was abandoned because the full model did not show either
##significant strain effect or an interaction term that was significant.
resultsAOV=aov(Spec~Treatment,data=intake)
summary(resultsAOV)
TukeyHSD(resultsAOV)

TUKEY <- TukeyHSD(resultsAOV)
labels<-generate_label_df(TUKEY , "Treatment") #generate labels using function
names(labels)<-c('Letters','Treatment')#rename columns for merging
yvalue<-aggregate(.~Treatment, data=intake, mean)# obtain letter position for y axis using means
final<-merge(labels,yvalue) #merge dataframes
#remove vial and strain from final
final=final[,c(1:2,5)]


intake_plot=ggplot(data = intake, aes(Treatment,Spec)) +
  #geom_boxplot(size = 1) +
  geom_boxplot(color="black", fill="gray") + #, alpha=0.4) +
  labs(title = "MV2-25 Intake",
       y = "Spec", x = "Caloric Density") +
  theme_base()+
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format))),method="anova",label.y=0.2,color="red") +
  geom_text(data = final, aes(x = Treatment, y = Spec, label = Letters),vjust=-7.5,hjust=-0.2) 

intake_plot 