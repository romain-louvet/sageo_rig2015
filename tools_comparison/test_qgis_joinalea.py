# ---------------------------------------------------------------------------
# test_qgis_joinalea.py
# Created on: 2015-06-11 
# Author : Romain Louvet, PhD student
# Contact : romain.louvet@alumni.univ-avignon.fr
# Description: 
# This script must be executed with the python console within Qgis (copy/paste). 
# It was tested with Qgis 2.8.1 
#
# Join random points created by test_qgis_alea.py to polygons created by test_qgis_aggre.py
# and sum random points values by polygon
# 
# WARNING : bug with runalg "qgis:joinattributesbylocation" cf http://hub.qgis.org/issues/12922
# there is no result
# ---------------------------------------------------------------------------

## import qgis modules
import processing
from PyQt4.QtCore import *
from PyQt4.QtGui import *

# sys time
import time
import os

# START : execution time
start = time.time()

## working directory (depend on which computer this script is executed)
wd = "C:\\Users\\Romain Louvet\\Desktop\\test_comparatif_outils"
path = wd+"\\results\\"
groupfield = ["CODE_CO","CODE_CA","EPCI","CODE_A","CODE_D","CODE_R"]
# number of random points shapes
nalea = 2


for i in range(nalea):
# origin (output alea)
	join = path+"alea"+str(i)+".shp"
	for f in groupfield:
		if i == 0:
# display target
			map = iface.addVectorLayer(path+f+"JOIN.shp", f, "ogr")
# target (output aggreg)
		target= path+f+"JOIN.shp"
# output
		output = path+f+"ALEAJOIN"+str(i)+".shp"
# process
		processing.runalg("qgis:joinattributesbylocation",target,join,"['contains']",1,"sum",1,output)
		iface.addVectorLayer(output, f+"ALEAJOIN"+str(i), "ogr")


				
# STOP : execution time
print(str(time.time()-start)+" sec")



