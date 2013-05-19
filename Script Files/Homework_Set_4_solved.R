##############################################################################
##
##					                  Homework Set #4
##
##############################################################################
##
##	Simon Task Data: If you are unfamiliar with the Simon task, watch this
##	video: https://www.youtube.com/watch?v=qF-urz8adVI . In the file
##	"Cari Simon Data" is a lot of data from a Simon task that Cari ran
##	on four groups of participants: Monolinguals (Subject #s: 100s),
##	Spanish-English bilinguals (Subject #s: 200s), Chinese-English bilinguals
##	(Subject #s: 300s), and English-Spanish-bilinguals (Subject #s: 400s).
##	There are practice trials and experimental trials (identified in the
##	Running column). Accuracy is in Stimulus.ACC, and reaction times are in
##	Stimulus.RT. The different conditions are in Congruency (congruent,
##	incongruent, and central).
##
##	Problem #1
##	Using the Simon task data, load the data in R. 
##	Call the dataframe "data_unfiltered".
##

##  I know I need to use ddply() later on, so I'm going to install it up here.

install.packages("plyr")
library(plyr)
install.packages("reshape")
library(reshape)
install.packages("reshape2")
library(reshape2)

data_unfiltered <- read.csv("Cari_Simon_Data.csv")

##
##	Problem #2
##	Create a new dataframe consisting of only those trials that are NOT
##	practice trials. Call this dataframe "data".
##

data <- data_unfiltered[data_unfiltered$Running!="PracTrials",]

##	
##	Problem #3
##	How many subjects are in the data? How many trials per subject per
##	condition?
##

length(xtabs(~data$Subject))
##  There are 91 subjects in this data file.

xtabs(~data$Subject + data$Congruency)
##  There are 42 trials in each condition (3 conditions: central,
##  congruent, and incongruent) per subject.

##
##	Problem #4a
##	Create a new factor variable in data called "Group". Label monolinguals
##	(anyone whose Subject ID is in the 100s) as "Monolinguals", Spanish-
##	English Bilinguals (anyone whose Subject ID is in the 200s) as 
##	"SpanEng_Bilinguals", Chinese-English Bilinguals (anyone whose Subject 
##	ID is in the 300s) as "ChineseEng_Bilinguals", and the English-Spanish
##	Biliguals (anyone whose Subject ID is in the 400s) as "EngSpan_Biliguals".
##

data$Group[data$Subject > 100 & data$Subject < 200] = "Monolinguals"
data$Group[data$Subject > 200 & data$Subject < 300] = "SpanEng_Bilinguals"
data$Group[data$Subject > 300 & data$Subject < 400] = "ChineseEng_Bilinguals"
data$Group[data$Subject > 400 & data$Subject < 500] = "EngSpan_Bilinguals"

##
##  Problem #4b
##  There is one participant whose number falls outside of those windows.
##  That data is from a research assistant who tested the script. Delete
##  that participant from the data frame.
##

data = data[is.na(data$Group)==F,]

##  You don't need to know what the participant's number is. The fact that
##  the number is outside these windows means that the Group variable will
##  be NA for this person, and can be deleted that way.

##
##	Problem #5
##	Preserve Stimulus.ACC as a new variable in data called "original_acc".
##	Make Stimulus.ACC a factor, labeling 0s as "incorrect" and 1s as
##	"correct".
##

data$original_acc = data$Stimulus.ACC
data$Stimulus.ACC = as.factor(data$Stimulus.ACC)
levels(data$Stimulus.ACC) = c("incorrect", "correct", "rel_outliers")

##	You have to add the level for relative outliers for Problem #6!

##	
##	Problem #6
##	Using ddply(), identify FROM THE CORRECT TRIALS ONLY the trials 
##	that fall outside 2.5 standard deviations above and below the mean 
##	for each subject. Make sure to only use correct trials in that mean! 
##	Label these trials as "rel_outliers".
##

data$FileOrder = seq(1,nrow(data))

data = ddply(data, .(Subject), transform, 
             high_cutoff = mean(Stimulus.RT) + 2.5*sd(Stimulus.RT),
             low_cutoff = mean(Stimulus.RT - 2.5*sd(Stimulus.RT)))

data$Stimulus.ACC[data$Stimulus.ACC=="correct" & 
                    (data$Stimulus.RT > data$high_cutoff | 
                       data$Stimulus.RT < data$low_cutoff)]="rel_outliers"

data = data[order(data$FileOrder),]

##  xtabs(~data$Stimulus.ACC)

##	The file order stuff is to preserve the original data order, in case
##	ddply() did anything weird.

##
##	Problem #7
##	The researcher wants mean RT and Accuracy (taken from original acc) 
##	per Subject, per Group, and per condition (Congruency). For the mean RTs,
##	take only those those trials that were correct. Create a dataframe 
##	entitled "summary" that contains this information.
##

summary = ddply(data[data$Stimulus.ACC=="correct",], 
		.(Subject, Group, Congruency), summarise,
		Mean.RT = mean(Stimulus.RT))

##	Create a new summary, also broken down by block
block_summary = ddply(data[data$Stimulus.ACC=="correct",], 
		.(Subject, Group, Congruency, Running), summarise,
		Mean.RT = mean(Stimulus.RT))

##	BONUS!!: Using "reshape" package

summary_melt <- melt(summary, measure.vars="Mean.RT")
summary_melt <- summary_melt[!is.na(summary_melt$value),]
summary_recasted <- cast(summary_melt, Subject+Group~Congruency,mean)

##	BONUS BONUS!!: Reshaping the by block data

blocksummary_melt <- melt(block_summary, measure.vars="Mean.RT")
blocksummary_melt <- blocksummary_melt[!is.na(blocksummary_melt$value),]
blocksummary_recasted <- cast(blocksummary_melt, Subject+Group~Congruency+Running,mean)

##
##############################################################################