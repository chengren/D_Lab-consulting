#!pip install shapely
from shapely.geometry import Point, Polygon
import numpy as np
from itertools import compress
# Create Point objects
indx=[1,2]
p1 = Point(24.952242, 60.1696017)
p2 = Point(24.976567, 60.1612500)
ls_p=[p1,p2]
# Create a Polygon
coord1 = Polygon([(24.950899, 60.169158), (24.953492, 60.169158), (24.953510, 60.170104), (24.950958, 60.169990)])
coord2 = Polygon([(24.950899, 59.169158), (24.953492, 59.169158), (59.953510, 59.170104), (24.950958, 59.169990)])
ls_py=[coord1,coord2]

res_tot=[]
for py in ls_py:
	for p in ls_p:
		res = py.contains(p)
		res_tot.append(res)

print(res_tot)
res_tf = np.asarray(res_tot).reshape(len(ls_p),int(len(res_tot)/len(ls_p)))
print(res_tf)

print(list(compress(indx, res_tf[1])))
