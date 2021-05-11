
datum=read.csv(file.choose())
library(ggplot2)

ggplot(aes(x=Concentration, y=X..Floating, colour=Treatment, group=Treatment), data=datum)