import os
import pandas as pd
import matplotlib.pyplot as plt
#from matplotlib import animation.
os.getcwd()

data=pd.read_csv("NOW_AssignmentData_train.csv")
dataTest=pd.read_csv("NOW_AssignmentData_test.csv")
#checking foe na values..turns out a lot of the columns have unfilled ones
data1=data.dropna()

#this was executed in second attempt to check if the na values actually had an effect
#data=data1

#Checking if there are duplicate with some variables
data['DeviceType'].unique()
data['MessageType'].unique()
#since it's a clean and organised data i move ahead

#contact column is the predictor variable

#data exploration
data['Contact'].value_counts()      #263691 0s and 5107 1s

contact_resulted=len(data[data['Contact']==1])
contact_not_resulted=len(data[data['Contact']==0])
print('Percentage conact made',(contact_resulted/(contact_not_resulted+contact_resulted)*100))
#so contact made is 1.899 %!!!

#finding the mean and the behaviour of predictor vaiable for  all numerical variables
summ=data.groupby('Contact').mean()      #TOTAL VISITS SEEMS TO BE A VERY NICE INDICATOR!!


data.columms      #get the names for checking
#BIT of visualizations
table=pd.crosstab(data.DeviceType,data.Contact)
table.div(table.sum(1).astype(float),axis=0).plot(kind='bar',stacked=False)


plt.title('Total  vs Contact')
plt.xlabel('Total visits')
plt.ylabel('Proportion of Contacts')


#checking if i should further categorize these variables into groups??!!
data.TotalVisits.hist()
data.ClicksBeforeThisDisplay.hist()
data.PreviousDisplaysThisSession.hist()
#country,number of visits,messagetype,ResponsePromise(ASAP),DisplayReason(higher ofr higher ID)
#devise type (very litle)


#NOW BEGINS THE TRAINING...!st i will split the train data set into 2 parts itself
#later after getting the good accuracy, I shall use it on test data set!!


#desktop=1,Tablet=2,mobile=3
cat_columns = data.select_dtypes(['object']).columns

change_to_cat={'TrafficSource','Country','MessageType','ResponsePromise','DisplayReason','CallCenterStatus'}
# for x in change_to_cat:
#     data[x} =data[x].astype('category')

data["DeviceType"] = data['DeviceType'].astype('category')
data["TrafficSource"] = data['TrafficSource'].astype('category')
data["Country"] = data['Country'].astype('category')
data["MessageType"] = data['MessageType'].astype('category')
data["ResponsePromise"] = data['ResponsePromise'].astype('category')
data["DisplayReason"] = data['DisplayReason'].astype('category')
data["CallCenterStatus"] = data['CallCenterStatus'].astype('category')


data['DeviceType_cat'] = data['DeviceType'].cat.codes
data["TrafficSource_cat"] = data['TrafficSource'].cat.codes
data["Country_cat"] = data['Country'].cat.codes
data["MessageType_cat"] = data['MessageType'].cat.codes
data["ResponsePromise_cat"] = data['ResponsePromise'].cat.codes
data["DisplayReason_cat"] = data['DisplayReason'].cat.codes
data["CallCenterStatus_cat"] = data['CallCenterStatus'].cat.codes

#data['DeviceType'] = pd.Categorical.from_array(data.DeviceType).codes
#desktop 0 tablet 2 mobile 1

#data['DeviceType']=data['DeviceType'].apply(lambda x: x.cat.codes)
from sklearn.model_selection import train_test_split
X=data.iloc[:,[7,10,11,21,22,23,24,25,26,27]]
#not needed
#Y=data.iloc[:,data.columns=='Contact']


#data['DeviceType'] = data['DeviceType'] == 'desktop'

X_train, X_test, y_train, y_test = train_test_split(X, Y, test_size=0.2, random_state=1)

#1st method, logistic regression
from sklearn.linear_model import LogisticRegression
#LR #scikit learn only handles numerical ones..categorical becomes bit complicated
l_clf=LogisticRegression()
result=l_clf.fit(X_train,y_train)
#print(result.summary())
l_prediction = l_clf.predict(X_test)
print(l_prediction)

# import statsmodels.api as sm
# logit_model=sm.Logit(y_train,X_train)
# result=logit_model.fit()
# print(result.summary2())
#
# hitta=result.predict(X_test)
# print(hitta)






from sklearn.metrics import accuracy_score
print(accuracy_score(y_true=y_test,y_pred=l_prediction))        #Accuracy of 98%???!!!

# flag=0
# if l_prediction==1:
#     flag=flag+1

#now repeat the same steps for test data set too

dataTest["DeviceType"] = dataTest['DeviceType'].astype('category')
dataTest["TrafficSource"] = dataTest['TrafficSource'].astype('category')
dataTest["Country"] = dataTest['Country'].astype('category')
dataTest["MessageType"] = dataTest['MessageType'].astype('category')
dataTest["ResponsePromise"] = dataTest['ResponsePromise'].astype('category')
dataTest["DisplayReason"] = dataTest['DisplayReason'].astype('category')
dataTest["CallCenterStatus"] = dataTest['CallCenterStatus'].astype('category')


dataTest['DeviceType_cat'] = dataTest['DeviceType'].cat.codes
dataTest["TrafficSource_cat"] = dataTest['TrafficSource'].cat.codes
dataTest["Country_cat"] = dataTest['Country'].cat.codes
dataTest["MessageType_cat"] = dataTest['MessageType'].cat.codes
dataTest["ResponsePromise_cat"] = dataTest['ResponsePromise'].cat.codes
dataTest["DisplayReason_cat"] = dataTest['DisplayReason'].cat.codes
dataTest["CallCenterStatus_cat"] = dataTest['CallCenterStatus'].cat.codes

dataTest['Contact']=99



X_new=dataTest.iloc[:,[7,10,11,20,21,22,23,24,25,26]]
Y_new=dataTest.iloc[:,data.columns=='Contact']

l_clf2=LogisticRegression()
l_clf2.fit(X,Y)
l_prediction2 = l_clf2.predict(X_new)
print(l_prediction2)

dataTest['Contact']=l_prediction2        #predictor values generated
dataTest['Contact'].value_counts()

#CONTACT HAS ALL 0 VALUES...(29947 ZEROES)

# from sklearn.naive_bayes import GaussianNB
# nb=GaussianNB()
# nb.fit(X,Y)
# y_predicted=nb.predict(X_new)
# dataTest['Contact']=y_predicted
# dataTest['Contact'].value_counts()

#Support Vector Classifier
# from sklearn.svm import SVC
# s_clf = SVC()
# s_clf.fit(X,Y)
# s_prediction = s_clf.predict(X_new)
# print (s_prediction)
#
# if 1 in s_prediction:
#     print('HI')
# else:
#     print('jai')