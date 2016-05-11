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
os.chdir('/home/stuka/itam2/arqui/DPA_Ecobici/data/ecobici/')

def getAccessToken(client_id='541_39k7kp0ezfswokcswsc0oc8s4sccswk4g0kc84csss008gw440', client_secret='46lb6to6tp8ggcg88wo4g80cgwwoc4o40kc4c0s44s4wcwws4o'):
    url = 'https://pubsbapi.smartbike.com/oauth/v2/token?client_id='+client_id+'&client_secret='+client_secret+'&grant_type=client_credentials'
    data = json.load(urllib2.urlopen(url))
    return data


try:
    url3 = 'https://pubsbapi.smartbike.com/api/v1/stations/status.json?access_token='+data["access_token"]
    llenadoestaciones = json.load(urllib2.urlopen(url3))
except:
    data = getAccessToken()
    url3 = 'https://pubsbapi.smartbike.com/api/v1/stations/status.json?access_token='+data["access_token"]
    llenadoestaciones = json.load(urllib2.urlopen(url3))

       
  
llenadoestaciones.keys()
llenadoestaciones["stationsStatus"][0].keys()
result = json_normalize(llenadoestaciones,'stationsStatus')
nuevo = pd.concat([result,json_normalize(list(result['availability']))],axis=1)
nuevo = nuevo.drop(['availability','status'],axis=1)
nuevo['maxcapacity'] = nuevo['bikes']+nuevo['slots']
nuevo['bike_prop'] = nuevo['bikes']/nuevo['maxcapacity']
nuevo['slots_prop'] = nuevo['slots']/nuevo['maxcapacity']
bins = [0.00, 0.25, 0.5, 0.75, 1.1]
group_names = ['1', '2', '3', '4']
nuevo['categoria'] = pd.cut(nuevo['bike_prop'], bins, right=False, labels=group_names)
nuevo['canasta_categoria'] = pd.cut(nuevo['bike_prop'], bins, right=False, retbins = True)[0]
nuevo = pd.concat([nuevo,pd.get_dummies(nuevo['categoria'])],axis=1)
nuevo.to_csv('current_capacity.csv',encoding='utf-8',index=False)


