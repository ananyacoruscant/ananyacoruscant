import numpy as np
import pandas as pd 
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error
from math import sqrt
from sklearn.neighbors import KNeighborsRegressor
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import GradientBoostingRegressor
from sklearn import tree
from collections import Counter
from sklearn.svm import SVR
from sklearn.linear_model import Lasso
from sklearn.linear_model import Ridge
import seaborn as sns
from sklearn.naive_bayes import GaussianNB
import statsmodels.api as sm
from sklearn.tree import DecisionTreeRegressor


#Calling Train and Test datasets
train = pd.read_csv("C:/Users/Ananya Verma/Documents/traincab.csv")
test = pd.read_csv("C:/Users/Ananya Verma/Documents/testcab.csv")

#Looking at first 5 entries of train and test dataset
print(train.head())
print(test.head())

print(train['fare_amount'].describe())

#checking datatypes in train dataset
print(train.dtypes)
print(test.dtypes)

#we will try to find the index of '430-' value in fare_amount and will drop the value
print(train.index[train.fare_amount == '430-'])

#converting fare_amount to float 
train['fare_amount'] = pd.to_numeric(train['fare_amount'],errors='coerce')
train['fare_amount'].dtype

#Checking for Multicollinearity
corr = train.corr()
print(corr)
cmap = sns.diverging_palette(220, 10, as_cmap=True)
sns.heatmap(corr, cmap=cmap, vmax=.3, center=0, square=True, linewidths=.5, cbar_kws={"shrink": .5})
plt.show()

#Removing the negative values from the fare_amount variable
print('Old count: %d' % len(train))
train = train[train.fare_amount>=0]
print('New count: %d' % len(train))


#Checking the missing values in train and test dataset
missvaltrain = pd.DataFrame(train.isnull().sum())
missvaltest = pd.DataFrame(test.isnull().sum())

#Imputing missing values in passenger_count variable with median
train['passenger_count'] = train['passenger_count'].fillna(train["passenger_count"].median())

#Verifying the passenger count variable
print(train['passenger_count'])
print(pd.DataFrame(train.isnull().sum()))


#Split our Datetime into individual columns year, month, day,
#weekday, hour and then dropping datetime column
def aligndatetime(df):
    df["pickup_datetime"] = df["pickup_datetime"].map(lambda x: str(x)[:-3])
    df["pickup_datetime"] = pd.to_datetime(df["pickup_datetime"], format='%Y-%m-%d %H:%M:%S')
    df['year'] = df.pickup_datetime.dt.year
    df['month'] = df.pickup_datetime.dt.month
    df['day'] = df.pickup_datetime.dt.day
    df['weekday'] = df.pickup_datetime.dt.weekday
    df['hour'] = df.pickup_datetime.dt.hour
    return(df["pickup_datetime"].head())    
print(aligndatetime(train))
print(aligndatetime(test))

train.drop('pickup_datetime', axis=1, inplace=True)
test.drop('pickup_datetime', axis=1, inplace=True)

#Checking the datasets for new columns added in the last step
print(train.head())
print(test.head())

#Checking for NA in dataset
print(train.isnull().any())
print(test.isnull().any())
print(train.isnull().sum())
train=train.fillna(train.mean())
train.isnull().sum()

print(train.isnull().sum())

#Dealing with the logitude and latitude variables, we consider Manhattan distance
train['abs_long']=abs(train['pickup_longitude']-train['dropoff_longitude'])
train['abs_lat']=abs(train['pickup_latitude']-train['dropoff_latitude'])

#Checking the new variables abs_long and abs_lat
print(train.head())

#Performing the same activity for the test dataset
test['abs_long']=abs(test['pickup_longitude']-test['dropoff_longitude'])
test['abs_lat']=abs(test['pickup_latitude']-test['dropoff_latitude'])

#Checking the new variables abs_long and abs_lat
print(test.head())
print(train.columns)
print(test.columns)

train['fare_amount'] =  train['fare_amount'].astype(int)
train['pickup_longitude'] =  train['pickup_longitude'].astype(int)
train['pickup_latitude'] =  train['pickup_latitude'].astype(int)
train['dropoff_longitude'] =  train['dropoff_longitude'].astype(int)
train['dropoff_latitude'] =  train['dropoff_latitude'].astype(int)
train['year'] =  train['year'].astype(int)
train['passenger_count'] =  train['passenger_count'].astype(int)
train['month'] =  train['month'].astype(int)
train['day'] =  train['day'].astype(int)
train['hour'] =  train['hour'].astype(int)
train['weekday'] =  train['weekday'].astype(int)
train['abs_long'] =  train['abs_long'].astype(int)
train['abs_lat'] =  train['abs_lat'].astype(int)
train['pickup_longitude'] =  train['pickup_longitude'].astype(int)
###-----------Data Visualization--------------

sns.barplot(x='year',y='fare_amount',data=train)
plt.show()
#After closing the plot that opens in the last step,another plot opens automatically for the next step.
#This happens till the last plt.show() occurs.
sns.barplot(x='month',y='fare_amount',data=train)
plt.show()
sns.barplot(x='day',y='fare_amount',data=train)
plt.show()
sns.barplot(x='weekday',y='fare_amount',data=train)
plt.show()
sns.barplot(x='hour',y='fare_amount',data=train)
plt.show()

sns.barplot(x='month',y='passenger_count',data=train)
plt.show()
sns.barplot(x='day',y='passenger_count',data=train)
plt.show
sns.barplot(x='hour',y='passenger_count',data=train)
plt.show

print(test.shape)

###Detect outliers and dropping outliers
plt.boxplot(train['fare_amount'])
plt.show()
plt.boxplot(train['passenger_count'])
plt.show()
cnames=train.columns
def outliers_analysis(oa):
    for i in oa.columns:
        q75,q25 = np.percentile(oa.loc[:,i],[75,25])
        iqr=q75-q25
        min=q25-(iqr*1.5)
        max=q75+(iqr*1.5)
        oa = oa.drop(oa[oa.loc[:,i] < min].index)
        oa = oa.drop(oa[oa.loc[:,i] > max].index)
        return(oa)

train = outliers_analysis(train)
test = outliers_analysis(test)

print(test.shape)

#Splitting dataset keeping fare_amount variable aside
train_X = train.iloc[:,1:13] #all the independent variables
train_Y = train['fare_amount'] #only the dependent variable


##----------------------LINEAR REGRESSION-------------------------------------

import statsmodels.api as sm
LRmodel = sm.OLS(train_Y,train_X).fit()

print(LRmodel.summary())
#Check R-squared, adjusted R-squared etc.

predictions_LR = LRmodel.predict(test)

#MAPE
def MAPE(train_Y, predictions_LR):
    mapetest= np.mean(np.abs((train_Y - predictions_LR)/train_Y))*100
    return mapetest
print(MAPE(train_Y, predictions_LR))
    
#Since it gives infinite results we will use MAE and RMSE

#MAE
MAE = np.mean(abs(train_Y - predictions_LR))
print(MAE)
#3.2667889809326742


#RMSE
def RMSE(train_Y, predictions_LR):
    return np.sqrt(((predictions_LR - train_Y) ** 2).mean())
print(RMSE(train_Y, predictions_LR))
#4.1014448355035364

sns.barplot(x='hour',y='fare_amount',data=train)
plt.show()

##----------------------------Decision Tree--------------------------------

DTmodel = DecisionTreeRegressor(max_depth=2).fit(train_X, train_Y)
print(DTmodel)

predictions_DT = DTmodel.predict(test)
print(predictions_DT)

#Since it will give values in array let us convert the predictions_DT to dataframe in order to compute the MAE and RMSE
predictions_DT = pd.DataFrame(predictions_DT)
#MAE
MAEdt = np.mean(abs(train_Y - predictions_DT))
print(MAEdt)
#5.375968
#Accuracy :94.63

#RMSE
def RMSE(train_Y, predictions_DT):
    return np.sqrt(((predictions_DT - train_Y) ** 2).mean())
print(RMSE(train_Y, predictions_DT))

