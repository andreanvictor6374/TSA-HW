# -*- coding: utf-8 -*-
"""
Created on Thu Mar 18 10:54:09 2021

@author: andre
task: simple linear regression
"""
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.api as sm
from statsmodels.formula.api import ols
from sklearn.linear_model import LinearRegression
# from statsmodels.stats.stattools import durbin_watson
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

sesonalised=False

# Importing the dataset
dataset = pd.read_csv('Gap-regression.csv')
dataset['Date']=pd.to_datetime(dataset['Date'],format='%Y/%m/%d')
dataset.set_index('Date',inplace=True)
dataset['Dsales']=dataset['Gap_Sales']/dataset['Seasonal']


Database={'train':dataset.loc['1985-03':'2005-12'],
          'test':dataset.loc['2006-03':'2007-03']}


y_train=Database['train']['Dsales']
y_test=Database['test']['Dsales']

# Fitting Simple Linear Regression to the Training set
reg1 = LinearRegression()
reg1.fit(Database['train'][['T1']], Database['train']['Dsales'])
model1=getStatAndPlot(Database['train'],'T1','Dsales')


reg2 = LinearRegression()
reg2.fit(Database['train'][['DPI']], Database['train']['Dsales'])
model2=getStatAndPlot(Database['train'],'DPI','Dsales')

print('model1-','b1:',reg1.coef_[0],'b0:',reg1.intercept_)
print('model2-','b1:',reg2.coef_[0],'b0:',reg2.intercept_)

if sesonalised:
    # Predicting the Test set results
    y_pred1_train = reg1.predict(Database['train'][['T1']])*Database['train']['Seasonal']
    y_pred2_train = reg2.predict(Database['train'][['DPI']])*Database['train']['Seasonal']
    y_pred1_test = reg1.predict(Database['test'][['T1']])*Database['test']['Seasonal']
    y_pred2_test = reg2.predict(Database['test'][['DPI']])*Database['test']['Seasonal']
    
    RMSE1_train=math.sqrt((np.mean((y_pred1_train-y_train*Database['train']['Seasonal'])**2)))
    RMSE1_test=math.sqrt((np.mean((y_pred1_test-y_test*Database['test']['Seasonal'])**2)))
    
    RMSE2_train=math.sqrt((np.mean((y_pred2_train-y_train*Database['train']['Seasonal'])**2)))
    RMSE2_test=math.sqrt((np.mean((y_pred2_test-y_test*Database['test']['Seasonal'])**2)))

else:
    # Predicting the Test set results
    y_pred1_train = reg1.predict(Database['train'][['T1']])
    y_pred2_train = reg2.predict(Database['train'][['DPI']])
    y_pred1_test = reg1.predict(Database['test'][['T1']])
    y_pred2_test = reg2.predict(Database['test'][['DPI']])
    
    
    RMSE1_train=math.sqrt((np.mean((y_pred1_train-y_train)**2)))
    RMSE1_test=math.sqrt((np.mean((y_pred1_test-y_test)**2)))
    
    RMSE2_train=math.sqrt((np.mean((y_pred2_train-y_train)**2)))
    RMSE2_test=math.sqrt((np.mean((y_pred2_test-y_test)**2)))
    

print('RMSE1_train:',RMSE1_train)
print('RMSE1_test:',RMSE1_test)
print('RMSE2_train:',RMSE2_train)
print('RMSE2_test:',RMSE2_test)

