#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon May 25 14:48:32 2020

@author: chengren
"""

import geopandas as gpd
#import pandas as pd
import numpy as np
#from shapely.geometry import Point, Polygon
#import numpy as np
#from itertools import compress

df = gpd.read_file("data/DNI_NASA_low.shp")
centroid = gpd.read_file("CA_census_tract_centroids2010_WGS84/CA_census_tract_centroids2010_WGS84.shp")
sun = df[['ID','annual','numyears','geometry']]

#sun['new'] = sun['geometry']

ca_boundary = Polygon([(-125,42), ( -113, 42),(-113,28),( -125,28)])
ca_poy=sun[sun.geometry.within(ca_boundary)]
ca_poy.reset_index(inplace=True)
centroid['sun']=0.0#has to be a float

for i in range(len(centroid)):
    for j in range(len(ca_poy)):
        if centroid.geometry[i].within(ca_poy.geometry[j]):
            centroid['sun'][i] = ca_poy['annual'][j]
            
#check whether there are any 0s in centroid['sun']
centroid['sun'].value_counts()
        
    

