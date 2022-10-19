##Analysis of weight at day 7

#Set working directory -- You can type out the path but I like to 
#go to Session --> Set working directory --> Choose working directory...
setwd("~/")

#Grab the data file from your computer. I sometimes having issues working from Box,so you may want to download
#the file to your computer. When you run the below, a File explorer will appear and you can select the appropriate csv file
#datum is an object -- we can choose any name we want -- it's just what we're going to call our data file from now on.
datum=read.csv(file.choose())

#Post-hoc -- purpose=artificially inflate p-values to maintain the experiment-wide type 1 error rate.
resultsAOV=aov(Pre~Treatment,data=datum)

#send the results for lines 39 & 40 to Taylor
summary(resultsAOV)
TukeyHSD(resultsAOV)

plot(Pre~as.factor(Treatment),data=datum,xlab="Treatment",ylab="Weight (g)",main="Body Weight Across Caloric Densities")





