# Read the csv, you might haver to change your file or folder name
monodata<-read.csv("monolingual_naming.csv")

#remove practice from data
monodata.noprac = monodata[monodata$Procedure.Block. != "PracBlockProc",]

#Remove none from cognate status, and remove practice level from Procedure.Block.
monodata.noprac$CognateStatus <- droplevels(monodata.noprac$CognateStatus)
monodata.noprac$Procedure.Block. <- droplevels(monodata.noprac$Procedure.Block.)
monodata.noprac$Animacy <- droplevels(monodata.noprac$Animacy)

#make subject a factor
monodata.noprac$Subject <- as.factor(monodata.noprac$Subject)

#Fix errors in Accuracy
monodata.noprac$Accuracy[!(monodata.noprac$Accuracy==0 | monodata.noprac$Accuracy==1)] <- 1

#Preserve original accuracy data as numeric
monodata.noprac$originalacc = monodata.noprac$Accuracy

#Make Accuracy column a factor
monodata.noprac$Accuracy = as.factor(monodata.noprac$Accuracy)
levels(monodata.noprac$Accuracy) = c("incorrect","correct")

#Make sure to only include correct trials.
monodata.correct <- monodata.noprac[monodata.noprac$Accuracy=="correct",]

#Aggregate our data using tapply(). 
data.applied <- tapply(monodata.correct$TargetWord.RT, 
	list(monodata.correct$Subject,
	monodata.correct$CognateStatus),
	mean)
data.applied <- as.data.frame(data.applied)

#Aggregate our data using tapply(). 
data.applied2 <- tapply(monodata.correct$TargetWord.RT, 
	list(monodata.correct$Subject,
	monodata.correct$CognateStatus,
	monodata.correct$Animacy),
	mean)
data.applied2 <- as.data.frame(data.applied2)

data.applied$Subject <- rownames(data.applied)
colnames(data.applied)<-c("Cog.RT","NCog.RT","Subject")




sumdata<-ddply(monodata.correct, 
	.(Subject, CognateStatus, Animacy),summarise ,meanRT=mean(TargetWord.RT), 
	cuthigh = mean(TargetWord.RT) + 2.5 * sd(TargetWord.RT),
	cutlow = mean(TargetWord.RT) - 2.5 * sd(TargetWord.RT))



transformeddata<-ddply(monodata.correct, 
	.(Subject, CognateStatus, Animacy),transform ,meanRT=mean(TargetWord.RT), 
	cuthigh = mean(TargetWord.RT) + 2.5 * sd(TargetWord.RT),
	cutlow = mean(TargetWord.RT) - 2.5 * sd(TargetWord.RT))

transformeddata$Accuracy4Outliers <- transformeddata$Accuracy

levels(transformeddata$Accuracy) <- c("incorrect", "correct", "rel_outlier")

transformeddata$Accuracy[transformeddata$TargetWord.RT > transformeddata$cuthigh |
	transformeddata$TargetWord.RT < transformeddata$cutlow] = "rel_outlier"





