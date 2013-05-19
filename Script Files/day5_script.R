# Load packages
library(plyr)
library(reshape)

# Constants for outlier detection
absolute_outlier_high_cut = 2000
absolute_outlier_low_cut = 200

# Load Data
data_unfiltered <- read.csv("Cari_Simon_Data.csv")

# Remove practice
data <- data_unfiltered[data_unfiltered$Running!="PracTrials",]

# Assign group names 
data$Group[data$Subject > 100 & data$Subject < 200] = "Monolinguals"
data$Group[data$Subject > 200 & data$Subject < 300] = "SpanEng_Bilinguals"
data$Group[data$Subject > 300 & data$Subject < 400] = "ChineseEng_Bilinguals"
data$Group[data$Subject > 400 & data$Subject < 500] = "EngSpan_Bilinguals"

# Remove the participant whose number fall outside of these windows
data = data[is.na(data$Group)==F,]


# Preserve Stimulus.ACC as a new variable in data called "original_acc".
# Make Stimulus.ACC a factor, labeling 0s as "incorrect" and 1s as
# "correct". Also add the relative outliers column.

data$original_acc = data$Stimulus.ACC
data$Stimulus.ACC = as.factor(data$Stimulus.ACC)
levels(data$Stimulus.ACC) = c("incorrect", "correct", "ab_outliers", "rel_outliers")

# Identify absolute outliers before relative outliers.
data$Stimulus.ACC[data$Stimulus.RT > absolute_outlier_high_cut |
data$Stimulus.RT < absolute_outlier_low_cut] = "ab_outliers"


# Using ddply(), identify FROM THE CORRECT TRIALS ONLY the trials 
# that fall outside 2.5 standard deviations above and below the mean 
# for each subject. Make sure to only use correct trials in that mean! 
# Label these trials as "rel_outliers". Also use a fileorder vector to 
# keep track of the original order othe dataset.

data$FileOrder = seq(1,nrow(data))

data = ddply(data, .(Subject, Stimulus.ACC), transform, 
             high_cutoff = mean(Stimulus.RT) + 2.5*sd(Stimulus.RT),
             low_cutoff = mean(Stimulus.RT - 2.5*sd(Stimulus.RT)))

data$Stimulus.ACC[data$Stimulus.ACC=="correct" & 
                    (data$Stimulus.RT > data$high_cutoff | 
                       data$Stimulus.RT < data$low_cutoff)]="rel_outliers"

data = data[order(data$FileOrder),]

# Calculate mean RT and Accuracy (taken from original acc) 
# per Subject, per Group, and per condition (Congruency). For the mean RTs,
# take only those those trials that were correct. Create a dataframe 
# entitled "summary" that contains this information.

# Using plyr

summary = ddply(data[data$Stimulus.ACC=="correct",], 
                 .(Subject, Group, Congruency), summarise,
                 Mean.RT = mean(Stimulus.RT))

block_summary = ddply(data[data$Stimulus.ACC=="correct",], 
                      .(Subject, Group, Congruency, Running), summarise,
                      Mean.RT = mean(Stimulus.RT))


# Reshaping the summary frame
summary_melt <- melt(summary,  #we're going to melt down summary
                     measure.vars="Mean.RT") #our measure var is mean RT

blocksummary_melt <- melt(block_summary, measure.vars="Mean.RT")

summary_recasted <- cast(summary_melt, #melted data to recast
                         Subject + Group ~ Congruency, mean) #the formula then the aggregation

blocksummary_recasted <- cast(blocksummary_melt, 
                              Subject+Group~Congruency+Running,mean)


# Melting and casting our trial level data without using ddply
data.melted <- melt(data[data$Stimulus.ACC=="correct",],
     measure.vars="Stimulus.RT")

data.casted <- cast(data.melted,
                         Subject + Group ~ Congruency, mean) #the formula then the aggregation

#Add block if we want it
data.casted <- cast(data.melted,
                         Subject + Group ~ Running+ Congruency, mean) #the formula then the aggregation
