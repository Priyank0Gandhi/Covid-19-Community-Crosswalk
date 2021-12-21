# -*- coding: utf-8 -*-
"""
Created on Wed Dec  1 21:09:00 1721

@author: PRIYANK GANDHI
"""

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import plotly as py
import plotly.express as px
import plotly.graph_objs as go
import seaborn as snr
from scipy.stats import pearsonr

d1 = pd.read_csv('C:/Users/PRIYANK GANDHI/Desktop/AIT-580/Final Project/project.csv', na_values=["Manual Verification Required"], low_memory=False)
d1.head
d1.dtypes
d1.isnull().sum()
d1.replace('Manual Verification Required',999)
d1[['Total Score','HHA Score']].describe()

snr.set(font_scale=3)
#"Exploratory data Analysis:"

#To be combined with the Maps and infere what states had most rural and non rural aeas:
fig, ax = plt.subplots(figsize=(17,10))
snr.countplot(data=d1,y="Hardest Hit Area (HHA)",hue="Rural")
ax.set(title='Types of Hardest Hit Areas in Rural and Non Rural regions')








d2 = d1.loc[d1['Tribal Community (1 if yes)'] == 'Non-Tribal']
print(d2['Tribal Community (1 if yes)'])

d3 = d1.loc[(d1['Tribal Community (1 if yes)'] == 'Fully Tribal') | (d1['Tribal Community (1 if yes)']=='Partial Tribal')]
print(d3['Tribal Community (1 if yes)'])


#For Non-Tribal Regions:
fig, ax = plt.subplots(figsize=(17,10))
snr.countplot(data=d2,y="Hardest Hit Area (HHA)",hue="Rural")
ax.set(title='Hardest Hit Areas for Non Tribal Communities')

fig, ax = plt.subplots(figsize=(17,10))
snr.countplot(data=d2,y="Rural")
ax.set(title='Non Tribal People Living in Rural or Non Rural Areas')


#For Tribal Regions:
    
fig, ax = plt.subplots(figsize=(17,10))
snr.countplot(data=d3,y="Hardest Hit Area (HHA)",hue="Rural")
ax.set(title='Hardest Hit Areas For Tribal Communities living in Rural Areas')

fig, ax = plt.subplots(figsize=(30,20))
snr.countplot(data=d3,y="Rural",hue="Tribal Community (1 if yes)",)
ax.set(title=' Tribal Communities in Rural Area')



fig, ax = plt.subplots(figsize=(30,20))
snr.countplot(data=d3,y="Hardest Hit Area (HHA)",hue="Tribal Community (1 if yes)")
ax.set(title='Hardest Hit Areas in Tribal Communities')
#Regions where partially tribal communities live Have most sustained hotspots.
#Moderate burden regions are where the Fully tribal communities live.
#Surprisingly, emerging hotspot regions are most where the partially tribal people reside.
    
    






