# -*- coding: utf-8 -*-
"""
Created on Thu Feb 18 17:59:53 2016

@author: stuka
"""
import json
import urllib2
import pandas as pd 
from pandas.io.json import json_normalize
import os


os.listdir('.')
os.chdir('/home/stuka/itam2/graph4ds/Ecobici')


url = 'https://pubsbapi.smartbike.com/oauth/v2/token?client_id=541_39k7kp0ezfswokcswsc0oc8s4sccswk4g0kc84csss008gw440&client_secret=46lb6to6tp8ggcg88wo4g80cgwwoc4o40kc4c0s44s4wcwws4o&grant_type=client_credentials'
data = json.load(urllib2.urlopen(url))

nuevo = nuevo.drop(['addressNumber','altitude','location'],axis=1)
nuevo.to_csv('estaciones.csv',encoding='utf-8')

url3 = 'https://pubsbapi.smartbike.com/api/v1/stations/status.json?access_token='+data["access_token"]
llenadoestaciones = json.load(urllib2.urlopen(url3))  
llenadoestaciones.keys()
llenadoestaciones["stationsStatus"][0].keys()
result = json_normalize(llenadoestaciones,'stationsStatus')
nuevo = pd.concat([result,json_normalize(list(result['availability']))],axis=1)
nuevo = nuevo.drop(['availability','status'],axis=1)
nuevo['maxcapacity'] = nuevo['bikes']+nuevo['slots']
nuevo = nuevo.drop(['bikes','slots'],axis=1)