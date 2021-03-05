# -*- coding: utf-8 -*-
"""
Created on Tue Feb 16 15:36:39 2021

@author: dakulkarni
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.tsa.stattools import adfuller
from statsmodels.tsa.holtwinters import SimpleExpSmoothing # SES
from statsmodels.tsa.holtwinters import Holt # Holts Exponential Smoothing
from statsmodels.tsa.holtwinters import ExponentialSmoothing 
import streamlit as st
import pickle


model=pickle.load(open('model.pkl','rb'))

result = pd.read_excel("C:/DS/result.xlsx")
delhi_ap = pd.read_excel("C:/DS/delhi_ap_sorted.xlsx")

def predict_f(mon, date1, hr1,result1, delhi):
    mon1 = mon
    date2 = date1
    result2 = result1
    delhi1=delhi
    if (mon1>=1) & (mon1<=4):
        if(date2>=1) & (date2<=31):
            if(hr1>=0)&(hr1<=23):
                delhi1_mon=delhi1[delhi1["month"]==mon1]
                delhi1_mon_hr=delhi1_mon[delhi1_mon["hour"]==hr1]
                delhi1_mon_hr_day=delhi1_mon_hr[delhi1_mon_hr["day"]==date2]
                if(delhi1_mon_hr_day).empty:
                    st.warning("There is no recorded PM2.5 value at this timing") 
                    if (mon1==4)&(date2==20)&(hr1>0):
                        result2=result1[result1["id"]==hr1]
                        output = int(result2["new"].values)
                        return output
                    
                    
                if not (delhi1_mon_hr_day.empty):
                    if (mon1==4)&(date2<=20):
                        output = int(delhi1_mon_hr_day["PM25"].values)
                        return output
                    if (mon1<4)&(date2<=31):
                        output = int(delhi1_mon_hr_day["PM25"].values)
                        return output
                if (mon1==4)&(date2==20)&(hr1>0):
                    result2=result1[result1["id"]==hr1]
                    output = int(result2["new"].values)
                    return output
                
                if(delhi1_mon_hr_day).empty:
                    st.warning("There is no recorded PM2.5 value at this timing") 
                    return
     
                  
                        
       
#def main():
import warnings
warnings.filterwarnings("ignore")
st.title("Forecasting Delhi Pollution (PM2.5)")
st.subheader("Enter the Month")
month = int(st.number_input("(1-4)",format="%0f"))
st.subheader("Enter the Day")
day1 =st.number_input("(1-31)",format="%0f")
st.subheader("Enter the Hour")
hr= int(st.number_input("(0-23)",format="%0f"))
warnings.filterwarnings("ignore")
    
m = st.button("Forecast") 
if m:
    if (month==4)&(day1>=21):
        st.warning("Forcasted values available only for the next 24 hours") 
    if (month>=1) & (month<=4):
        if(day1>=1) & (day1<=31):
            if(hr>=0)&(hr<=23):
                if(month==4)&(day1<20):
                    output = predict_f(month,day1,hr,result, delhi_ap)
                    st.success("Recorded PM 2.5 value is:{}".format(output))
                if(month==4)&(day1==20)&(hr==0):
                    output = predict_f(month,day1,hr,result, delhi_ap)
                    st.success("Recorded PM 2.5 value is:{}".format(output))
                if(month<4)&(day1<=31):
                    output = predict_f(month,day1,hr,result, delhi_ap)
                    st.success("Recorded PM 2.5 value is:{}".format(output))
                if(month==4)&(day1==20)&(hr>0):
                    output = predict_f(month,day1,hr,result, delhi_ap)
                    st.success("Forecasted PM 2.5 value is:{}".format(output))
                 
  
                
    if (month>4)|(month<1):
        st.warning("Forcasted values available only From Jan 01 to April 21")
    if (day1<1)|(month>31):
        st.warning("Enter the correct day values(1-31)")
    if (hr<0)|(hr>23):
        st.warning("Enter the correct hour values(0-23)")
    
        
        
        
    



