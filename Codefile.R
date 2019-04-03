library(ggplot2)
library(dplyr)
library(caTools)
library(e1071)
library(MASS)
library(data)
library(class)
"Ïntroduction of our project"
"Within past years chicago has whitnessed a dramatic resurgence of crimes and violence, these conflicts,robberies 
and tragedies are happening every day around the city, and to tackle this issue i am going to analyze the historical 
crime data and conducting predictive analysis so we can identify the pattern in the crimes data and informed the police
departments so they can allocate police forcies on desired areas and try to prevent tragedies from happening"




"First part will cover the following points
	Articulate what patterns/relationships you are seeing in the data"

"Now lets start our project before going on the predictive analysis lets first
analyze the datasets through visualizations
"
#Read data from the file and save that in the variable
Chicago_data <- read.csv(file="Chicago_Crimes_2012_to_2017.csv", header=TRUE)
Chicago_data <- na.omit(Chicago_data)

"Lets summarize the data first"
summary(Chicago_data)


"We are using dataset from year 2012 to 2017"

"Now lets move towards the First Part of our Project"
"Exploratory Data Analysis"

"Visual No 1"
"Lets find the Total Number of crimes going down across years"
plot_1 <- ggplot(data=Chicago_data,aes(x=Year, group=Primary.Type, fill=Primary.Type)) +geom_histogram()
plot_1 + coord_fixed(ratio = 0.2)

"Here is the first plot of our data and it gives us very clear insights that Theft Type crimes are the main crimes
from year 2012 to year 2017 "
"One more thing we came to know that in 2017 crimes rate reduced drastically as compared to other years moreover 2012
has most crimes "

"Visual No 2"

"Now find the crime rates per month for this we have to change the format of date so that it is being understood 
by R"
"Below are the steps that are used to convert date into a format which is being understood by R"
summary(Chicago_data$Date)


"This part covers the following point
Exploratory Data Analysis/Transformation:"
date<- as.POSIXlt(Chicago_data$Date, format = "%m/%d/%Y %I:%M:%S %p")
Chicago_data$Date <- date
Chicago_data$day <- as.factor(Chicago_data$Date$mday)
Chicago_data$hour <- as.factor(Chicago_data$Date$hour)
Chicago_data$month <- as.factor(Chicago_data$Date$mon+1)
Chicago_data$year <- as.factor(Chicago_data$Date$year+1900)
Chicago_data$weekday <- as.factor(Chicago_data$Date$wday+1)

"Convert the date variable to type Date"
Chicago_data$Date <- as.Date(Chicago_data$Date, format = "%m/%d/%Y %I:%M:%S %p")

write.csv(Chicago_data, file = "MyData.csv")

"Now we have seperate columns for months we can visualize the crimes rates with respect to different months"
plot_2 <- ggplot(data=Chicago_data,aes(x=month,fill=month)) +geom_histogram(stat = 'count')
plot_2

"Lets change numeric of plot and add month names instead of numbers"
Chicago_data$month <- month.abb[Chicago_data$month]

"Now again make a plot and see the results"

plot_2 <- ggplot(data=Chicago_data,aes(x=month,fill=month)) +geom_histogram(stat = 'count')
plot_2
"From this we can see that Month of July has the most number of crimes as compared to other months "


"Now look at the trends of different crimes types for this we have to filter the orignal data and for that 
lets make a new data frame"

"As there are many types of crimes we are focusing on the important ones here "

Type_of_crimes <- Chicago_data %>% 
  group_by(Primary.Type, Date) %>% 
  summarize(total = n()) %>%
  filter(Primary.Type != "CONCEALED CARRY LICENSE VIOLATION", 
         Primary.Type!= "NON-CRIMINAL",
         Primary.Type!= "NON-CRIMINAL (SUBJECT SPECIFIED)", 
         Primary.Type!= "NON - CRIMINAL", 
         Primary.Type!= "OTHER NARCOTIC VIOLATION", 
         Primary.Type!= "OTHER OFFENSE")


ggplot(data=Type_of_crimes,aes(x = Date, y = total)) + 
  geom_line(color = "red") + 
  facet_wrap(~Primary.Type, scales = "free") + 
  ggtitle(label = "Breakdown of Crime Trends by Type of Offense") +
  scale_y_continuous(name = "Reports of Violations")

"These are the major crimes rates you can see that narcotics crimes are reduced as we compared to other crimes over 
the years"


"So our analysis are being done and we conclude some important analysis which are explained below"
"1)  Theft crimes are the most prominent crimes in the chicago over the years and police should trained itselt to 
tackle with Theft crimes and always be prepared to tackle such an event one, police should install security cameras
where Theft crimes are mostly reported or in rushy areas and identify the culprits through this way police will be 
able to reduce the Theft crimes"
"2) July has the most crimes reported so in that month there will be more police and petrolling police in the city 
Chicago as compared to other months"

"3) Narcotics crime rate has reduced over the years and we should focus on other crimes and give less time to 
narcotics as compared to other because it's already controlled"



"Second Part of our project will cover the following points of the requirements
1) Training/test/validation:
2) Modelling"


"Part (i)"

"Lets split the data into two parts First Training Data and second Test Part"

"Our dataset is very big contains about 1.4 million rows and we would require GPU to process that so i am 
restricting the rows to Tewenty Thousand so we can train our model effeciently"
Model_data <- tail(Chicago_data,n=20000)


"Here our data is splitted into two parts (Segmentation) 
Training Dataset and Test DataSet"
split = sample.split(Model_data, SplitRatio = 2/3)
train = subset(Model_data, split == TRUE)
test = subset(Model_data, split == TRUE)

"Our main technique is Supervised Learning"


"We are applying here KNN model here"

trainKNN = train[,c("Arrest","Beat","District","Ward","Community.Area")]
trainKNN$Arrest = as.logical(trainKNN$Arrest)
testKNN = test[,c("Arrest","Beat","District","Ward","Community.Area")]
testKNN$Arrest = as.logical(testKNN$Arrest)
#Training and testing the model
length(trainKNN)
length(trainKNN$Arrest)
testautoknn = knn(train = trainKNN, cl = trainKNN$Arrest, test = testKNN, k = 3)
trainautoknn = knn(train = trainKNN, cl = trainKNN$Arrest, test = trainKNN, k = 3)
table(testKNN$Arrest,testautoknn)


"As we can see from the table it is quite good observation if we compared with True False and True TRUE so this model
outperforms our analysis"


"Second model we are using is Linear Regression"
summary(lm(as.numeric(Arrest)~Beat+District+Ward+Community.Area,data=train))
"We look from this model that linear regression model is not a good choice as it shows us very poor results"