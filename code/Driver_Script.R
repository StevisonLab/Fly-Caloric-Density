#set working directory to code directory
setwd("~/Library/CloudStorage/Box-Box/Caloric Density Manuscript/code/")

#run script Intake.R to plot Figure 1A
source("Intake.R")

#run script BW_Fat.R to plot Figure 1B
source("Body_Fat.R")

#combine Figure 1 plots
combo1=ggarrange(intake_plot, fat_plot,ncol = 2,nrow = 1,labels=c("A","B"))
ragg::agg_tiff(file="../figures/Fig1.tiff", width = 140,height = 100, units = "mm", res = 300, scaling = 0.5) 
combo1
dev.off()

#run script for figure 2 -- MV2-25 BW & Strain BW -- Merged in script
source("Body_Weight.R")

#run scripts for figure 3 -- MV2-25 Fec & Strain Fec, MV2-25 Long & Strain Long
#run script Strain_Fertility_Code.R to plot Figure2.tiff
source("Fecundity_Longevity.R")

