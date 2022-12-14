##Analysis of weight at day 7

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

#Grab the data file from your computer. I sometimes having issues working from Box,so you may want to download
#the file to your computer. When you run the below, a File explorer will appear and you can select the appropriate csv file
#datum is an object -- we can choose any name we want -- it's just what we're going to call our data file from now on.
fec=read.csv(file = "../data/Strain_Offspring_Count_Calories.csv",header = T,stringsAsFactors = T)
datum=read.csv(file = "../data/MV225_Offspring_Count.csv",header = T,stringsAsFactors = T)
#fec=read.csv(file.choose())
#datum=read.csv(file.choose())

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
#resultsAOV=aov(Total~Treatment+Strain,data=fec)
resultsAOV=aov(Total~Treatment,data=fec)
summary(resultsAOV)
TukeyHSD(resultsAOV)

#tukey's for strain fecundity
TUKEY <- TukeyHSD(resultsAOV)
labels<-generate_label_df(TUKEY , "Treatment") #generate labels using function
names(labels)<-c('Letters','Treatment')#rename columns for merging
yvalue<-aggregate(.~Treatment, data=fec, mean)# obtain letter position for y axis using means
final<-merge(labels,yvalue) #merge dataframes
#remove vial and strain from final
final=final[,c(1:2,4)]


st_fec_plot=ggplot(data = fec, aes(Treatment,Total)) +
  #  geom_boxplot(size = 1) + # commented out since it is duplicate of line below
  geom_boxplot(color="black", fill="gray") + #, alpha=0.4) + commented out alpha to make boxplots not transparent
  #  theme_bw() +  # commented out since it is overwritten by theme command below
  labs(title = "Fecundity Across Strains",
       y = "Number of Offspring", x = "Caloric Density") + 
  facet_wrap(~ Strain)+
  theme_base()
#abandoned stats due to inconsistencies with reported model
#  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format))), method="anova", color="red")+  #gives incorrect submodel p-values
#  geom_text(data = final, aes(x = Treatment, y = Total, label = Letters),vjust=-7.5,hjust=-0.2) #adds tukey results, but not correct per strain

st_fec_plot  
##Overall MV2-25 ANOVA
#resultsAOV=aov(Total~Treatment+Strain,data=fec)
resultsAOV=aov(Total~Treatment,data=datum)
summary(resultsAOV)
TukeyHSD(resultsAOV)

#tukey's for MV2-25 fecundity
TUKEY <- TukeyHSD(resultsAOV)
labels<-generate_label_df(TUKEY , "Treatment") #generate labels using function
names(labels)<-c('Letters','Treatment')#rename columns for merging
yvalue<-aggregate(.~Treatment, data=datum, mean)# obtain letter position for y axis using means
final<-merge(labels,yvalue) #merge dataframes
#remove vial and strain from final
final=final[,c(1:2,4)]


fec_plot=ggplot(data = datum, aes(Treatment,Total)) +
  #geom_boxplot(size = 1) +
  geom_boxplot(color="black", fill="gray") + #, alpha=0.4) +
  labs(title = "MV2-25 Fecundity",
       y = "Number of Offspring", x = "Caloric Density") +
  #theme_bw()+
  theme_base()+
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format))),method="anova",label.y=400,color="red") +
  geom_text(data = final, aes(x = Treatment, y = Total, label = Letters),vjust=-7.5,hjust=-0.2) 

fec_plot

#Grab the data file from your computer. I sometimes having issues working from Box,so you may want to download
#the file to your computer. When you run the below, a File explorer will appear and you can select the appropriate csv file
#datum is an object -- we can choose any name we want -- it's just what we're going to call our data file from now on.
long=read.csv(file = "../data/MV225_Long.csv",header = T,stringsAsFactors = T)
stlong=read.csv(file = "../data/Longevity_Strains.csv",header = T,stringsAsFactors = T)

generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}


#plotting strain weight
stlong_plot=ggplot(data = stlong, aes(Treatment,Age)) +
  geom_boxplot(color="black", fill="gray") + #, alpha=0.4) + commented out alpha to make boxplots not transparent
  labs(title = "Lifespan Across Strains",
       y = "Lifespan (d)", x = "Caloric Density") + 
  facet_wrap(~ Strain)+
  theme_base()

stlong_plot  

##Overall MV2-25 ANOVA
resultsAOV=aov(Age~Treatment,data=long)
summary(resultsAOV)
TukeyHSD(resultsAOV)

#tukey's for MV2-25 fecundity
TUKEY <- TukeyHSD(resultsAOV)
labels<-generate_label_df(TUKEY , "Treatment") #generate labels using function
names(labels)<-c('Letters','Treatment')#rename columns for merging
yvalue<-aggregate(.~Treatment, data=long, mean)# obtain letter position for y axis using means
final<-merge(labels,yvalue) #merge dataframes
#remove vial and strain from final
final=final[,c(1:2,5)]


long_plot=ggplot(data = long, aes(Treatment,Age)) +
  geom_boxplot(color="black", fill="gray") + #, alpha=0.4) +
  labs(title = "MV2-25 Lifespan",
       y = "Lifespan (d)", x = "Caloric Density") +
  theme_base()+
  stat_compare_means(aes(label = paste0("p = ", after_stat(p.format))),method="anova",label.y=150,color="red") +
  geom_text(data = final, aes(x = Treatment, y = Age, label = Letters),vjust=-7.5,hjust=-0.2) 

long_plot

combo4=ggarrange(fec_plot, st_fec_plot,long_plot, stlong_plot,ncol = 2,nrow = 2,labels=c("A","B","C","D"))
combo4

#Make TIFF plot -- https://www.christophenicault.com/post/understand_size_dimension_ggplot2/
ragg::agg_png(file="../figures/Figure2.tiff", width = 400,height = 300, units = "mm", res = 300, pointsize=5) 
combo4
dev.off()

ragg::agg_tiff(file="../figures/Figure3.tiff", width = 140,height = 105, units = "mm", res = 300, scaling = 0.5) 
combo4
dev.off()