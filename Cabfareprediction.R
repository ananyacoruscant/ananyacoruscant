#Loading required libraries
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(corrplot)
library(scales)
library(psych)

#Gettings the files
train = read.csv("C:/Users/Ananya Verma/Documents/traincab.csv")
test = read.csv("C:/Users/Ananya Verma/Documents/testcab.csv")

#Making a copy of train variable for ease 
qwe = train


#Trying to understand the data better
dim(train)
dim(test)
head(train)
head(test)

#Trying to understand the fare_amount in train set
summary(train$fare_amount)

#The fare_amount variable has some wierd values like "(Other)"

#Finding the column names
colnames(train)
colnames(test)

##Dealing with pickup_datetime variable## 
#Seperating pickupdatetime variable into seperate variables
train = train %>% 
  mutate(
    pickup_datetime = ymd_hms(pickup_datetime),
    month = month(pickup_datetime),
    year = year(pickup_datetime),
    day = day(pickup_datetime),
    dayOfWeek = wday(pickup_datetime),
    hour = hour(pickup_datetime)
  )

#Similarly for test
test = test %>% 
  mutate(
    pickup_datetime = ymd_hms(pickup_datetime),
    month = month(pickup_datetime),
    year = year(pickup_datetime),
    day = day(pickup_datetime),
    dayOfWeek = wday(pickup_datetime),
    hour = hour(pickup_datetime)
  )

#Deleting pickup_datetime variable as it is of now use now
train$pickup_datetime = NULL

#Verifying the newly added columns
colnames(train)
colnames(test)

#Finding Missing values in data
missingval_train= data.frame(apply(train,2,function(x){sum(is.na(train))}))
missingval_train
#Since passenger_Count, month, year,day,hour,dayOfWeek has missing values, let us impute missing value with median
train$passenger_count[is.na(train$passenger_count)] = median(train$passenger_count, na.rm = T)
train$month[is.na(train$month)] = median(train$month, na.rm = T)
train$year[is.na(train$year)] = median(train$year, na.rm = T)
train$day[is.na(train$day)] = median(train$day, na.rm = T)
train$dayOfWeek[is.na(train$dayOfWeek)] = median(train$dayOfWeek, na.rm = T)
train$hour[is.na(train$hour)] = median(train$hour, na.rm = T)


#Checking if missing values in passenger count were treated right
missingval_train= data.frame(apply(train,2,function(x){sum(is.na(train))}))
missingval_train

View(train)
#0 missing values

#Moving forward with test data
missingval_test= data.frame(apply(test,2,function(x){sum(is.na(test))}))
missingval_test
#We see that test data has zero missing values

#Finding NA's in data
cat('NAs in train dataset:', sum(is.na(train)), '\nNAs in train dataset:', sum(is.na(train)), sep = ' ')
cat('NAs in test dataset:', sum(is.na(test)), '\nNAs in test dataset:', sum(is.na(test)), sep = ' ')

#Finding type of data
class(train)
class(test)

#The row 1124 has fare_amount "430-" hence we will try to remove it
train[1124,]
train = train[-c(1124), ]
##we have deleted "430-" value and then stored the new data in "train"

##Treating Longitudes and Latitudes##
#Trying to understand the latitudes and longitude values in train set
summary(train$pickup_latitude)
summary(train$dropoff_latitude)
del_Lat_max = which(train$pickup_latitude > 90 )
train = train[-del_Lat_max,]
del_Lat_min = which(train$pickup_latitude < -90)
#no values satisfying above condition

#Removing the rows where drop off latitude is > 90 and < -90.
del_Lat_max = which(train$dropoff_latitude > 90)
#no values satisfying above condition
del_Lat_min = which(train$dropoff_latitude < -90)
#no values satisfying above condition

# Getting the summary of pick up and Drop off latitude after clean up 
summary(train$pickup_latitude)
summary(train$dropoff_latitude)


#Checking if any longitude is < -180 or > 180
summary(train$pickup_longitude)
summary(train$dropoff_longitude)

#Removing the rows where pick up longitude is < -180 and > 180
pick_longi_min = which(train$pickup_longitude < -180)
#no values satisfying above condition
pick_longi_max = which(train$pickup_longitude > 180)
#no values satisfying above condition

#Removing the rows where drop off longitude is < -180 and > 180
drop_longi_min = which(train$dropoff_longitude < -180)
drop_longi_max = which(train$dropoff_longitude > 180)
#no values satisfying above conditions

# Getting the summary of pick up and Drop off longitude after clean up 
summary(train$pickup_longitude)
summary(train$dropoff_longitude)


#Checking train and test set with new variables
head(train)
head(test)

#The train dataset has negative values in fare_amount which is not practical, hence removing those values
train$fare_amount = as.numeric(train$fare_amount)
str(train$fare_amount)
negativefare = which(train$fare_amount < 0)
summary(train$fare_amount)
#Checking fare_amount for negative values, min value is 1 which means no negative value existing.
#train_data = train_data[-negativefare, ]

class(train) #data.frame

#--------------------VIsualization------------------------------------------------------
colnames(train)

ggplot(train, aes(x=day,y = fare_amount))+geom_bar(stat = "identity",na.rm = FALSE, fill="steelblue",show.legend = T, width = 0.9)+xlab("Day")+ylab("fare_amount")+scale_x_discrete(breaks=pretty_breaks(n=10), )+scale_y_discrete(breaks=pretty_breaks(n=10))+theme(text = element_text(size = 15))+geom_text(aes(label=train$day, vjust=-0.3))
ggplot(train, aes(x=dayOfWeek,y = fare_amount)) +geom_bar(stat = "identity",na.rm = FALSE, fill="steelblue",show.legend = T, width = 0.9)+xlab("Day Of week")+ylab("fare_amount")+scale_x_discrete(breaks=pretty_breaks(n=10), )+scale_y_discrete(breaks=pretty_breaks(n=10))+theme(text = element_text(size = 15))+geom_text(aes(label=train$dayOfWeek, vjust=-0.3))
ggplot(train, aes(x=hour,y = fare_amount)) +geom_bar(stat = "identity",na.rm = FALSE, fill="steelblue",show.legend = T, width = 0.9)+xlab("Hour")+ylab("fare_amount")+scale_x_discrete(breaks=pretty_breaks(n=10), )+scale_y_discrete(breaks=pretty_breaks(n=10))+theme(text = element_text(size = 15))+geom_text(aes(label=train$hour, vjust=-0.3))
ggplot(train, aes(x=year,y = fare_amount)) +geom_bar(stat = "identity",na.rm = FALSE, fill="steelblue",show.legend = T, width = 0.9)+xlab("Year")+ylab("fare_amount")+scale_x_discrete(breaks=pretty_breaks(n=10), )+scale_y_discrete(breaks=pretty_breaks(n=10))+theme(text = element_text(size = 15))+geom_text(aes(label=train$year, vjust=-0.3))
ggplot(train, aes(x=month,y = fare_amount)) +geom_bar(stat = "identity",na.rm = FALSE, fill="steelblue",show.legend = T, width = 0.9)+xlab("Month")+ylab("fare_amount")+scale_x_discrete(breaks=pretty_breaks(n=10), )+scale_y_discrete(breaks=pretty_breaks(n=10))+theme(text = element_text(size = 15))+geom_text(aes(label=train$month, vjust=-0.3))
#ggplot(train, aes(x=passenger_count,y = fare_amount)) +geom_bar(stat = "identity", fill="green")+scale_x_discrete(breaks=5)
ggplot(train, aes(x=passenger_count,y = fare_amount)) +geom_point(aes_string(color=train$passenger_count))+theme_bw()+ylab("fare_Amount")+xlab("Passenger Count")+theme(text=element_text(size=10))+scale_x_continuous(breaks=pretty_breaks(n=10))+scale_y_continuous(breaks=pretty_breaks(n=10))
ggplot(train, aes(x=dropoff_latitude,y = fare_amount)) +geom_point(aes_string(color=train$dropoff_latitude))+theme_bw()+ylab("fare_Amount")+xlab("Dropoff Latitude")+theme(text=element_text(size=10))+scale_x_continuous(breaks=pretty_breaks(n=10))+scale_y_continuous(breaks=pretty_breaks(n=10))
ggplot(train, aes(x=dropoff_longitude,y = fare_amount)) +geom_point(aes_string(color=train$dropoff_longitude))+theme_bw()+ylab("fare_Amount")+xlab("Dropoff Longitude")+theme(text=element_text(size=10))+scale_x_continuous(breaks=pretty_breaks(n=10))+scale_y_continuous(breaks=pretty_breaks(n=10))
ggplot(train, aes(x=pickup_latitude,y = fare_amount)) +geom_point(aes_string(color=train$pickup_latitude))+theme_bw()+ylab("fare_Amount")+xlab("Pickupp Latitude")+theme(text=element_text(size=10))+scale_x_continuous(breaks=pretty_breaks(n=10))+scale_y_continuous(breaks=pretty_breaks(n=10))
ggplot(train, aes(x=pickup_longitude,y = fare_amount)) +geom_point(aes_string(color=train$pickup_longitude))+theme_bw()+ylab("fare_Amount")+xlab("Pickup Longitude")+theme(text=element_text(size=10))+scale_x_continuous(breaks=pretty_breaks(n=10))+scale_y_continuous(breaks=pretty_breaks(n=10))



#Outlier analysis
cnames= colnames(train)
cnames

#Outliers Visualization
for (i in 1:length(cnames))
{
  assign(paste0("an",i), ggplot(aes_string(y = (cnames[i]), x = "fare_amount"), data = subset(train))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="fare_amount")+
           ggtitle(paste("Box plot of responded for",cnames[i])))
}

library(gridExtra)
grid.arrange(an3, an5,an4, an6, an7, an8, an9,an10, an11,an1,an2, ncol=4)


#Treating Outliers

val = train$pickup_longitude[train$pickup_longitude %in% boxplot.stats(train$pickup_longitude)$out]
length(val)
train = train[which(!train$pickup_longitude %in% val),]
View(train)

#@@@@@@@@@@

val1 = train$passenger_count[train$passenger_count %in% boxplot.stats(train$passenger_count)$out]
length(val1)
train = train[which(!train$passenger_count %in% val1),]
View(train)

#@@@@@@@@@@@@@@@@@@@@@@@@
val2 = train$dropoff_longitude[train$dropoff_longitude %in% boxplot.stats(train$dropoff_longitude)$out]
length(val2)
train = train[which(!train$dropoff_longitude %in% val2),]
View(train)
#@@@@@@@@@@@@@@@@@@@@@@@@@@
val3 = train$dropoff_latitude[train$dropoff_latitude %in% boxplot.stats(train$dropoff_latitude)$out]
length(val3)
train = train[which(!train$dropoff_latitude %in% val3),]
View(train)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@
val4 = train$pickup_latitude[train$pickup_latitude %in% boxplot.stats(train$pickup_latitude)$out]
length(val4)
train = train[which(!train$pickup_latitude %in% val4),]
View(train)

#We have treated the outliers, We shall now move forward with linear regression model


colnames(train)

#Visualizations
#plotting bar graph to observe variable day
ggplot(train, aes(x=day,y = fare_amount)) +geom_bar(stat = "identity", fill="navyblue")+scale_x_continuous(breaks = pretty_breaks(n=10))+scale_y_continuous(breaks = pretty_breaks(n=10))

#plotting bar graph to observe variable year
ggplot(train, aes(x=year,y = fare_amount)) +geom_bar(stat = "identity", fill="lightblue")+scale_x_continuous(breaks = pretty_breaks(n=10))+scale_y_continuous(breaks = pretty_breaks(n=10))

#plotting bar graph to observe month
ggplot(train, aes(x=month,y = fare_amount)) +geom_bar(stat = "identity", fill="green")+scale_x_continuous(breaks = pretty_breaks(n=10))+scale_y_continuous(breaks = pretty_breaks(n=10))

#plotting bar graph to observe hout
ggplot(train, aes(x=hour,y = fare_amount)) +geom_bar(stat = "identity", fill="skyblue")+scale_x_continuous(breaks = pretty_breaks(n=10))+scale_y_continuous(breaks = pretty_breaks(n=10))
#plotting bar graph to observe pickup_latitude
ggplot(train,aes(x=pickup_latitude))+
  geom_histogram(fill="black",alpha=0.5) +
  ggtitle("Histogram of pickup_latitude")

#plotting bar graph to observe pickup_longitude
ggplot(train,aes(x=pickup_longitude))+
  geom_histogram(fill="yellow",alpha=0.5) +
  ggtitle("Histogram of pickup_longitude")

#plotting bar graph to observe day

ggplot(train, aes(x=day,y = passenger_count)) +geom_bar(stat = "identity")

#plotting bar graph to observe dropoff_latitude
ggplot(train,aes(x=dropoff_latitude))+
  geom_histogram(fill="brown",alpha=0.5) +
  ggtitle("Histogram of dropoff_latitude")


#plotting bar graph to observe dayOfWeek
ggplot(train,aes(x=dayOfWeek))+
  geom_histogram(fill="orange",alpha=0.5) +
  ggtitle("Histogram of dayOfWeek")


#plotting bar graph to observe dropoff_longitude
ggplot(train,aes(x=dropoff_longitude))+
  geom_histogram(fill="green",alpha=0.5) +
  ggtitle("Histogram of dropoff_longitude")


######################  Decision Tree Model   #################

fit = rpart(fare_amount~ ., data = train, method = "anova")
summary(fit)

#Predict for new test cases
predictions_DT = predict(fit, test)
predictions_DT

y=train$fare_amount#Storing actual values in a object for ease

#CHeck Performance of model
#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}
MAPE(y, predictions_DT)
#Error rate : 3.752733
#Accuracy : 96.25



########Linear Regression Model#############
#Check multicolinearity

library(usdm)
library(rpart)

#CHecking Multicollinearity
library(corrplot)
qwe= as.numeric(qwe)

vif(qwe)
lm_model = lm(fare_amount ~., data = train)

#Summary of the model
summary(lm_model)

#Predict
predictions_LR = predict(lm_model, test)

#Calculate MAPE
MAPE(train$fare_amount, predictions_LR)
#Error rate : 3.926267
#Accuracy : 96.07



