#Set working directory -- You can type out the path but I like to 
#go to Session --> Set working directory --> Choose working directory...
setwd("~/Library/CloudStorage/Box-Box/Caloric Density Manuscript/code/")

#load libraries
library(ggplot2)
library(ggthemes)
library(ggprism)
library(ggpubr)
library(multcompView)
library(ragg)

#Grab the data file from your computer.
weight=read.csv(file = "../data/MV225_Compiled_Weight.csv",header = T,stringsAsFactors = T)
weight$Pre<-1000*(weight$Pre)
stwt=read.csv(file = "../data/Strain_Weights.csv",header = T,stringsAsFactors = T)
stwt$Weight<-1000*(stwt$Weight)

generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

#resultsAOV=aov(Total~Treatment+Strain,data=fec)
resultsAOV=aov(Total~Pre,data=weight)
summary(resultsAOV)
TukeyHSD(resultsAOV)

#plotting strain weight
stwt_plot=ggplot(data = stwt, aes(Treatment,Weight)) +
  geom_boxplot(color="black", fill="gray") + #, alpha=0.4) + commented out alpha to make boxplots not transparent
  labs(title = "Weight Across Strains",
       y = "Dry Weight (mg)", x = "Caloric Density") + 
  facet_wrap(~ Strain)+
  theme_base()

stwt_plot  


##Overall MV2-25 ANOVA
resultsAOV=aov(Pre~Treatment,data=weight)
summary(resultsAOV)
TukeyHSD(resultsAOV)

#tukey's for MV2-25 fecundity
TUKEY <- TukeyHSD(resultsAOV)
labels<-generate_label_df(TUKEY , "Treatment") #generate labels using function
names(labels)<-c('Letters','Treatment')#rename columns for merging
yvalue<-aggregate(.~Treatment, data=weight, mean)# obtain letter position for y axis using means
final<-merge(labels,yvalue) #merge dataframes
#remove vial and strain from final
final=final[,c(1:2,5)]


bw_plot=ggplot(data = weight, aes(Treatment,Pre)) +
  geom_boxplot(color="black", fill="gray") + #, alpha=0.4) +
  labs(title = "MV2-25 Body Weight",
       y = "Body Weight (mg)", x = "Caloric Density") +
  theme_base()+
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format))),method="anova",label.y=1,color="red") +
  geom_text(data = final, aes(x = Treatment, y = Pre, label = Letters),vjust=-7.5,hjust=-0.2) 

bw_plot

combo2=ggarrange(bw_plot, stwt_plot,ncol = 2,nrow = 1,labels=c("A","B"))
combo2

#Make TIFF plot -- https://www.christophenicault.com/post/understand_size_dimension_ggplot2/
ragg::agg_png(file="../figures/Figure2.tiff", width = 400,height = 150, units = "mm", res = 300, pointsize=5) 
combo2
dev.off()

ragg::agg_tiff(file="../figures/Figure2scale.tiff", width = 140,height = 53, units = "mm", res = 300, scaling = 0.5) 
combo2
dev.off()