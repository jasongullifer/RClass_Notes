library(ez)
library(reshape)
library(plyr)

data(ToothGrowth) #put a data.frame ToothGrowth on the workspace

data = ToothGrowth #copy the dataset to data

data$pig = rep(seq(1,10),6) #add a column to track subject number
data$dose <- as.factor(data$dose) #factor dose

## Exporting to SPSS
   #Melt the data specifying len as the measure.vars
   data_melted = melt(data, measure.vars="len")

   #Cast the data to SPSS repeated measures format
   data_casted = cast(data_melted, pig ~ supp + dose, mean)

   #Write out the data to csv
   write.csv(data_casted,"pig_cast.csv")

## Run ezANOVA with R

   #The general format
   # ezANOVA(data ,          #your dataset
   #        dv=,             #your dependent measure
   #        wid =,           #your subject/item id column
   #        within =  .( ),  #within subject/item factors
   #        between = .( ),  #between subject/item factors
   #       )

   ezANOVA(data = data, wid=pig, dv=len, within= .(supp, dose))

## Summary tables
   #Using ddply, kind of ugly for reading
   data.summary = ddply(data, .(supp, dose), summarise, meanLen = mean(len))
   
   #Using reshape allows us more control of the format
   data.melted.summary <- melt(data, measure.vars="len")
   data.summary.reshape = cast(data.melted.summary, supp ~ dose, mean)
