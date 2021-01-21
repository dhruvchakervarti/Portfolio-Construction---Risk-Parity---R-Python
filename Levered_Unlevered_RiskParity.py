#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
from datetime import datetime 
import matplotlib.pyplot as plt
from scipy import stats
from scipy.stats import skew, kurtosis
plt.style.use('fivethirtyeight')


# In[2]:


CRSP_stocks = pd.read_csv('QAM1.csv',header = 0)


# In[3]:


def stocks(data):
    #data = CRSP_stocks.copy()
    data = data.copy()
    data = data[(data.SHRCD == 10) | (data.SHRCD == 11)]
    data = data[(data.EXCHCD == 1) | (data.EXCHCD == 2) | (data.EXCHCD == 3)]
    data.date = data['date'].astype(str)
    data.date = pd.to_datetime(data.date)
    data['month'] = data['date'].dt.month
    data['year'] = data['date'].dt.year
    data['RET'] =pd.to_numeric(data.RET,errors = 'coerce')
    data['DLRET'] = pd.to_numeric(data.DLRET, errors = 'coerce')
    data['Stock_lag_MV'] = abs(data['PRC'] * data['SHROUT'])
    data['Stock_lag_MV'] = data.groupby('PERMNO')['Stock_lag_MV'].shift(1)
    data['CumReturns'] = 0
    data.loc[(data.RET.notna() & data.DLRET.notna() == True),'CumReturns'] = ((1 + data.loc[(data.RET.notna() & data.DLRET.notna() == True),'RET'])*(1 + data.loc[(data.RET.notna() & data.DLRET.notna() == True),'DLRET'])-1)
    data.loc[(data.RET.isna() & data.DLRET.notna() == True),'CumReturns'] = data.DLRET
    data.loc[(data.RET.notna() & data.DLRET.isna() == True),'CumReturns'] = data.RET
    data['weights'] = data.groupby(['year','month'])['Stock_lag_MV'].transform(lambda x: (x/np.sum(x)))
    data['Vw'] = data['weights']*data['CumReturns']
    Data_1 = pd.DataFrame()
    Data_1['Stock_lag_MV'] = data.groupby(['year','month'])['Stock_lag_MV'].sum()
    Data_1 = Data_1.reset_index()
    Data_1['Stock_Ew_Ret'] = data.groupby(['year','month'])['CumReturns'].transform(lambda x: np.nanmean(x))
    Data_1['Stock_Vw_Ret'] = data.groupby(['year','month'])['Vw'].transform(lambda x: np.nansum(x))
    Data_1 = Data_1[Data_1['year']>=1926]
    Data_1 = Data_1[['year','month','Stock_lag_MV','Stock_Ew_Ret','Stock_Vw_Ret']]
    return Data_1


# In[4]:


Monthly_CRSP_Stocks = stocks(CRSP_stocks).reset_index(drop=True)
Monthly_CRSP_Stocks


# In[5]:


CRSP_Bonds = pd.read_csv('Bonddata.csv')
CRSP_Bonds


# In[6]:


def bonds(data_bond):
    #data_bond = CRSP_Bonds.copy()
    data_bond = data_bond.copy()
    data_bond['MCALDT'] = data_bond['MCALDT'].astype(str)
    data_bond['MCALDT'] = pd.to_datetime(data_bond['MCALDT'])
    data_bond['month'] = data_bond['MCALDT'].dt.month
    data_bond['year'] = data_bond['MCALDT'].dt.year
    data_bond['Bond_lag_MV'] = data_bond['TMTOTOUT']
    data_bond['Bond_lag_MV'] = data_bond.groupby('KYCRSPID')['Bond_lag_MV'].shift(1)
    data_bond['weights'] = data_bond.groupby(['MCALDT'])['Bond_lag_MV'].transform(lambda x: (x/np.nansum(x)))
    data_bond['Vw'] = data_bond['weights']*data_bond['TMRETNUA']
    Data_bond = pd.DataFrame()
    Data_bond['Bond_lag_MV'] = data_bond.groupby(['year','month'])['Bond_lag_MV'].sum()
    Data_bond = Data_bond.reset_index()
    Data_bond['Bond_Ew_Ret'] = data_bond.groupby(['year','month'])['TMRETNUA'].transform(lambda x: np.nanmean(x))
    Data_bond['Bond_Vw_Ret'] = data_bond.groupby(['year','month'])['Vw'].transform(lambda x: np.nansum(x))
    Data_bond = Data_bond[Data_bond['year']>=1926]
    Data_bond = Data_bond[['year','month','Bond_lag_MV','Bond_Ew_Ret','Bond_Vw_Ret']]
    return Data_bond


# In[7]:


Monthly_CRSP_Bonds = bonds(CRSP_Bonds)
Monthly_CRSP_Bonds


# In[8]:


Monthly_CRSP_Riskless = pd.read_csv('Treasurydata.csv')
Monthly_CRSP_Riskless


# In[9]:


def universe(Stocks, Bonds, Riskless):
    Stocks = Stocks.copy()
    Bonds = Bonds.copy()
    Riskless = Riskless.copy()
    
    Riskless['caldt'] = Riskless['caldt'].astype(str)
    Riskless['caldt'] = pd.to_datetime(Riskless['caldt'])
    Riskless['month'] = Riskless['caldt'].dt.month
    Riskless['year'] = Riskless['caldt'].dt.year
    
    Data = Stocks[['year','month','Stock_lag_MV']]
    temp = Stocks.merge(Riskless, on=["year","month"])
    temp['Stock_Excess_Vw_Ret'] = temp['Stock_Vw_Ret'] - temp['t90ret']
    Data['Stock_Excess_Vw_Ret'] = temp['Stock_Excess_Vw_Ret']
    
    temp2 = Bonds.merge(Riskless, on=["year","month"])
    temp2['Bond_Excess_Vw_Ret']= temp2['Bond_Vw_Ret'] - temp2['t30ret']
    
    Data['Bond_lag_MV'] = temp2['Bond_lag_MV']
    Data['Bond_Excess_Vw_Ret'] = temp2['Bond_Excess_Vw_Ret']
    return(Data)


# In[10]:


Monthly_CRSP_Universe = universe(Monthly_CRSP_Stocks,Monthly_CRSP_Bonds,Monthly_CRSP_Riskless)


# In[11]:


def port_returns(data):
    data = data.copy()
    data['Total_MV'] = data['Stock_lag_MV'] + data['Bond_lag_MV']
    data['Excess_Vw_Ret']= ((data['Stock_Excess_Vw_Ret']*data['Stock_lag_MV']) + (data['Bond_lag_MV']*data['Bond_Excess_Vw_Ret']))/(data['Total_MV'])
    #data.loc[0,'Excess_Vw_Ret'] = 0
    data['Excess_60_40_Ret'] = 0.6*data['Stock_Excess_Vw_Ret'] + 0.4*data['Bond_Excess_Vw_Ret']
    data['Stock_inverse_sigma_hat'] = (1/data['Stock_Excess_Vw_Ret'].transform(lambda x: x.shift(1).rolling(window=36).std()))
    data['Bond_inverse_sigma_hat'] = (1/data['Bond_Excess_Vw_Ret'].transform(lambda x: x.shift(1).rolling(window=36).std())) 
    data['Unlevered_k'] = 1/(data['Stock_inverse_sigma_hat'] + data['Bond_inverse_sigma_hat'] )
    data['Excess_Unlevered_RP_Ret'] = data['Unlevered_k']*data['Stock_inverse_sigma_hat']*(data['Stock_Excess_Vw_Ret']) + data['Unlevered_k']*data['Bond_inverse_sigma_hat']*(data['Bond_Excess_Vw_Ret'])
    levered_k = np.sqrt((np.std(data['Excess_Vw_Ret']))**2/(2*(1+data['Stock_Excess_Vw_Ret'].corr(data['Bond_Excess_Vw_Ret']))))
    data['Levered_k']= levered_k
    data['Excess_Levered_RP_Ret'] = (data['Levered_k']*data['Stock_inverse_sigma_hat']*(data['Stock_Excess_Vw_Ret'])) +  data['Bond_inverse_sigma_hat']*data['Levered_k']*(data['Bond_Excess_Vw_Ret'])

    Data = pd.DataFrame()
    Data = data[['year','month','Stock_Excess_Vw_Ret', 'Bond_Excess_Vw_Ret', 'Excess_Vw_Ret', 'Excess_60_40_Ret', 'Stock_inverse_sigma_hat',
             'Bond_inverse_sigma_hat', 'Unlevered_k', 'Excess_Unlevered_RP_Ret', 'Levered_k', 'Excess_Levered_RP_Ret']]
    return Data


# In[12]:


Port_Rets = port_returns(Monthly_CRSP_Universe)
Port_Rets


# In[13]:


def comparative_stats(data):
    data1 = Port_Rets.copy()
    data1 = data1[data1['year'] >=1930]
    data1 = data1[data1['year'] <= 2010]

    Annualized_Mean = [np.mean(data1['Stock_Excess_Vw_Ret'])*12, np.mean(data1['Bond_Excess_Vw_Ret'])*12, 
                       np.mean(data1['Excess_Vw_Ret'])*12, np.mean(data1['Excess_60_40_Ret'])*12, 
                       np.mean(data1['Excess_Unlevered_RP_Ret'])*12, np.mean(data1['Excess_Levered_RP_Ret'])*12]

    t_stat = [stats.ttest_1samp(data1['Stock_Excess_Vw_Ret'].fillna(0),0).statistic, 
              stats.ttest_1samp(data1['Bond_Excess_Vw_Ret'].fillna(0),0).statistic,
              stats.ttest_1samp(data1['Excess_Vw_Ret'].fillna(0),0).statistic,
              stats.ttest_1samp(data1['Excess_60_40_Ret'].fillna(0),0).statistic,
              stats.ttest_1samp(data1['Excess_Unlevered_RP_Ret'].fillna(0),0).statistic,
              stats.ttest_1samp(data1['Excess_Levered_RP_Ret'].fillna(0),0).statistic]

    Annualized_Standard_Deviation = [np.std(data1['Stock_Excess_Vw_Ret'])*np.sqrt(12), 
                                     np.std(data1['Bond_Excess_Vw_Ret'])*np.sqrt(12), 
                                     np.std(data1['Excess_Vw_Ret'])*np.sqrt(12),
                                     np.std(data1['Excess_60_40_Ret'])*np.sqrt(12), 
                                     np.std(data1['Excess_Unlevered_RP_Ret'])*np.sqrt(12), 
                                     np.std(data1['Excess_Levered_RP_Ret'])*np.sqrt(12)]
    
    skewness = [skew(data1['Stock_Excess_Vw_Ret'].fillna(0)), skew(data1['Bond_Excess_Vw_Ret'].fillna(0)), 
        skew(data1['Excess_Vw_Ret'].fillna(0)), skew(data1['Excess_60_40_Ret'].fillna(0)), 
        skew(data1['Excess_Unlevered_RP_Ret'].fillna(0)), skew(data1['Excess_Levered_RP_Ret'].fillna(0))] 
    
    Excess_Kurtosis = [kurtosis(data1['Stock_Excess_Vw_Ret'].fillna(0))-3, kurtosis(data1['Bond_Excess_Vw_Ret'].fillna(0))-3, 
                   kurtosis(data1['Excess_Vw_Ret'].fillna(0))-3, kurtosis(data1['Excess_60_40_Ret'].fillna(0))-3, 
                   kurtosis(data1['Excess_Unlevered_RP_Ret'].fillna(0))-3, kurtosis(data1['Excess_Levered_RP_Ret'].fillna(0))-3]
    
    Sharpe_Ratio = [x/y for x, y in zip(Annualized_Mean,Annualized_Standard_Deviation)]
    table1 = pd.DataFrame({
        'Annualized Mean': Annualized_Mean,
        't-stat of Annualized Mean': t_stat,
        'Annualized Standard Deviation': Annualized_Standard_Deviation,
        'Sharpe Ratio':Sharpe_Ratio,
        'Skewness':skewness,
        'Excess Kurtosis':Excess_Kurtosis
    }) 
    table1.index = ["CRSP stocks", "CRSP Bonds", "Value-weighted portfolio", "60/40 portfolio", "Unlevered RP", "Levered RP"]
    #table1['Sharpe Ratio'] = table1['Annualized Mean']/table1['Annualized Standard Deviation']
    table1['Annualized Mean'] <- table1['Annualized Mean']*100
    
    return(table1)


# In[14]:


Final_Output = comparative_stats(Port_Rets)
Final_Output


# In[ ]:





# In[ ]:




