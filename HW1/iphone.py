# -*- coding: utf-8 -*-
"""
Created on Thu Mar 18 16:59:56 2021

@author: andre
"""

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.api as sm
from statsmodels.formula.api import ols
from sklearn.linear_model import LinearRegression
import math

def getStatAndPlot(df,x,y):
    model = ols(f'{y} ~ {x}', data=df).fit()
    # print(model.summary())
    print(model.summary2())
          
    fig = plt.figure(figsize=(12,10))
    fig = sm.graphics.plot_regress_exog(model, f'{x}', fig=fig)
    plt.show()
    print('rsquared_adj:',model.rsquared_adj)
    # print('durbin_watson:',durbin_watson(model.resid))
    return model


# Importing the dataset
dataset = pd.read_csv('iphone.csv')
dataset.set_index('Time',inplace=True)
dataset.dropna(inplace=True)
dataset['T1']=np.arange(1,len(dataset)+1)


X_train=dataset.loc['Q3/07':'Q4/17',['T1']]
y_train=dataset.loc['Q3/07':'Q4/17','iPhone']

X_test=dataset.loc['Q1/18':'Q4/18',['T1']]
y_test=dataset.loc['Q1/18':'Q4/18','iPhone']



reg1 = LinearRegression()
reg1.fit(X_train, y_train)

Database=dataset.loc['Q3/07':'Q4/17']
model1=getStatAndPlot(Database,'T1','iPhone')

print('model1-','b1:',reg1.coef_[0],'b0:',reg1.intercept_)


y_pred1_train= reg1.predict(X_train)
y_pred1_test= reg1.predict(X_test)
RMSE1_train=math.sqrt((np.mean((y_pred1_train-y_train)**2)))

RMSE1_test=math.sqrt((np.mean((y_pred1_test-y_test)**2)))
print('RMSE1_train:',RMSE1_train)
print('RMSE1_test:',RMSE1_test)

