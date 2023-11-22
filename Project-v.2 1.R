#Read data
data<- read.csv("D:/Data.csv",header = TRUE,sep = ",")
data

#structure of the data set
str(data)

#summary() Function
summary(data)

mydata <-data
mydata


#Outlier Detection as a missing value
mydata$gender[mydata$gender == ""] <- 'NA'
mydata$hypertension[mydata$hypertension == ""] <- 'NA'
mydata$smoking_history[mydata$smoking_history == "No Info"] <- 'NA'
mydata

#Annotate Male as 1, Female as 0  from “gender” attribute
mydata$gender <- factor(mydata$gender,
                        levels = c("Female","Male"),
                        labels = c(0,1))
mydata

#Annotate never as 1, ever as 2, former as 3,current as 4 ,not current as 5  from "smoking_history” attribute
mydata$smoking_history <- factor(mydata$smoking_history,
                                 levels = c("never","ever","former","current","not current"),
                                 labels = c(1,2,3,4,5))
mydata


#finding missing values
colSums(is.na(mydata))

#HANDLING MISSING VALUES

#Delete the rows with missing values
removedata<- na.omit(mydata)
removedata

meanData <-mydata
meanData

#Recovering Missing Values With Mean
meanData$gender=as.numeric(meanData$gender)
Gendermean=mean(meanData$gender , na.rm = TRUE)
missingGenderMean= meanData$gender[is.na(meanData$gender)] <- Gendermean
meanData

Agemean=mean(meanData$age , na.rm = TRUE)
missingAgeMean= meanData$age[is.na(meanData$age)] <- Agemean
meanData

meanData$hypertension=as.numeric(meanData$hypertension)
Hypertensionmean=mean(meanData$hypertension , na.rm = TRUE)
missingHypertensionMean= meanData$hypertension[is.na(meanData$hypertension)] <- Hypertensionmean
meanData

meanData$smoking_history=as.numeric(meanData$smoking_history)
Smokinghistorymean=mean(meanData$smoking_history , na.rm = TRUE)
SmokinghistoryMean= meanData$smoking_history[is.na(meanData$smoking_history)] <- Smokinghistorymean
meanData



medianData <-mydata
medianData

#Recovering Missing Values With Median
medianData$gender=as.numeric(medianData$gender)
Gendermedian=median(medianData$gender , na.rm = TRUE)
missingGenderMedian= medianData$gender[is.na(medianData$gender)] <- Gendermedian
medianData

Agemedian=median(medianData$age , na.rm = TRUE)
missingAgeMedian= medianData$age[is.na(medianData$age)] <- Agemedian
medianData

medianData$hypertension=as.numeric(medianData$hypertension)
Hypertensionmedian=median(medianData$hypertension , na.rm = TRUE)
missingHypertensionMedian= medianData$hypertension[is.na(medianData$hypertension)] <- Hypertensionmedian
medianData

medianData$smoking_history=as.numeric(medianData$smoking_history)
Smokinghistorymedian=median(medianData$smoking_history , na.rm = TRUE)
SmokinghistoryMedian= medianData$smoking_history[is.na(medianData$smoking_history)] <- Smokinghistorymedian
medianData


modeData <-mydata
modeData

#Recovering missing values with Mode
mode <- function(x){
  unique_x <- unique(x)
  tabulate_x <-tabulate(match(x,unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}


modeData$gender=as.numeric(modeData$gender)
genderMode = mode(modeData$gender)
missingGenderMode= modeData$gender[is.na(modeData$gender)] <- genderMode
modeData

ageMode = mode(modeData$age)
missingAgeMode= modeData$age[is.na(modeData$age)] <- ageMode
modeData

modeData$hypertension=as.numeric(modeData$hypertension)
hypertensionMode = mode(modeData$hypertension)
missingHypertensionMode= modeData$hypertension[is.na(modeData$hypertension)] <- hypertensionMode
modeData

smoking_historyMode = mode(modeData$smoking_history)
missingSmoking_historyMode= modeData$smoking_history[is.na(modeData$smoking_history)] <- smoking_historyMode
modeData

#categorical value check for Mode

data3<- read.csv("D:/Data.csv",header = TRUE,sep = ",")
data3

colSums(is.na(data3))
data3$gender[data3$gender == ""] <- 'NA'
data3
data3$gender <- factor(data3$gender,
                       levels = c("Male","Female"),
                       labels = c(1,2))
data3

mode <- function(v) {
  uniquevalue <- na.omit(unique(v))
  uniquevalue[which.max(tabulate(match(v, uniquevalue)))] }
Gendermode = mode(data3$gender)
missinggenderMode= data3$gender[is.na(data3$gender)] <- Gendermode
data3

data3$gender <- factor(data3$gender,
                       levels = c(1,2),
                       labels = c("Male","Female"))
data3



#Correcting Invalid Values
options(max.print=10000)
data1<- read.csv("D:/Data.csv",header = TRUE,sep = ",")
data1
data1$age[data1$age == "290"] <- "29"
data1$age[data1$age == "280"] <- "28"
data1$bmi[data1$bmi == "-27.32"] <- "27.32"
data1

#Deleting Invalid Values
data2<- read.csv("D:/Data.csv",header = TRUE,sep = ",")
delete_row <-data2[c(1,2,4:51,53:118,120),]
delete_row
updateData <-delete_row
updateData
#Measure of Central Tendency

# Calculate the mean for each attribute
means <- colMeans(meanData)
print(means)

# Calculate the median for each attribute using apply
medians <- apply(medianData, 2, median)
print(medians)

# Calculate the mode for each attribute using apply
modes <- apply(modeData, 2, mode)
print(modes)

newData <-medianData
newData

# Measure of Spread 
numeric_data <- newData[, sapply(newData, is.numeric)]
ranges <- sapply(numeric_data, range)
print(ranges)
# Calculate the variance for numerical columns
variances <- sapply(numeric_data, var)
print(variances)
# Calculate the standard deviation for numerical columns
std_devs <- sapply(numeric_data, sd)
print(std_devs)

#Histogram

hist(modeData$age, main = "Age Distribution", xlab = "Age", ylab = "Frequency")
hist(modeData$bmi, main = "bmi Distribution", xlab = "bmi", ylab = "Frequency" , ylim = c(0,100))
hist(modeData$ HbA1c_level, main = " HbA1c_level Distribution", xlab = " HbA1c_level", ylab = "Frequency", xlim = c(0,10), ylim = c(0,30))
hist(modeData$blood_glucose_level, main = "blood_glucose_level Distribution", xlab = "blood_glucose_level", ylab = "Frequency", xlim = c(50,300), ylim = c(0,60))


#Bargraph

barplot(table(medianData$smoking_history ), main = " smoking_history  Distribution", xlab = "Smoking History", ylab = "Frequency")
barplot(table(medianData$gender), main = "Gender Distribution", xlab = "Gender", ylab = "Frequency", ylim = c(0, 80))
barplot(table(medianData$heart_disease ), main = "heart_disease  Distribution", xlab = "heart_disease ", ylab = "Frequency",ylim = c(0, 150))
barplot(table(medianData$diabetes), main = "diabetes Distribution", xlab = "diabetes", ylab = "Frequency",ylim = c(0, 80))


#Boxplot

plotData<-medianData
plotData

#visalixzing outliers via boxplot
boxplot(plotData$age, 
        main = "Age Distribution",
        ylab = "Age",
        col = "lightblue")

boxplot(plotData$bmi, 
        main = "bmi Distribution",
        ylab = "bmi",
        col = "lightblue")

boxplot(plotData$blood_glucose_level, 
        main = "glucose level Distribution",
        ylab = "glucose level",
        col = "lightblue")

boxplot(plotData$HbA1c_level, 
        main = "HbA1c_level Distribution",
        ylab = "HbA1c_level",
        col = "lightblue")



#removing outliers 
data <- plotData$age
length(data)
boxplot(data, plot = FALSE)$out
outliers <- boxplot(data, plot = FALSE)$out
data_no_outlier <- data[-which(data %in% outliers)] 
boxplot(data_no_outlier, plot = FALSE)$out
length(data_no_outlier)




data <- plotData$bmi
length(data)
boxplot(data, plot = FALSE)$out
outliers <- boxplot(data, plot = FALSE)$out
data_no_outlier <- data[-which(data %in% outliers)] 
boxplot(data_no_outlier, plot = FALSE)$out
length(data_no_outlier)





data <- plotData$blood_glucose_level
length(data)
boxplot(data, plot = FALSE)$out
outliers <- boxplot(data, plot = FALSE)$out
data_no_outlier <- data[-which(data %in% outliers)] 
boxplot(data_no_outlier, plot = FALSE)$out
length(data_no_outlier)


data <- plotData$HbA1c_level
length(data)
boxplot(data, plot = FALSE)$out
outliers <- boxplot(data, plot = FALSE)$out
data_no_outlier <- data[-which(data %in% outliers)] 
boxplot(data_no_outlier, plot = FALSE)$out
length(data_no_outlier)

#handling duplicated values

duplicated(mydata)
which(duplicated(mydata))


mydata_noDuplicate<-mydata[!duplicated(mydata),]
mydata_noDuplicate