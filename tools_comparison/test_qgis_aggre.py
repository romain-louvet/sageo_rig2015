# ---------------------------------------------------------------------------
# test_qgis_aggre.py
# Created on: 2015-06-11 
# Author : Romain Louvet, PhD student
# Contact : romain.louvet@alumni.univ-avignon.fr
# Description: 
# This script must be executed with the python console within Qgis (copy/paste). 
# It was tested with Qgis 2.8.1 
#
# Dissolve shape according to a list of aggregation fields and
# sum values of a list of fields for each aggregation.
# ---------------------------------------------------------------------------

## import qgis modules
import processing
from PyQt4.QtCore import *
from PyQt4.QtGui import *

# sys time
import time
# import math functions
import numpy
import os

# START : execution time
time0 = time.time()

# Local variables:
## working directory (depend on which computer this script is executed)
wd = "C:\\Users\\Romain Louvet\\Desktop\\test_comparatif_outils"
## shape at the most disaggregated level, with aggregation fields
base = wd+"\\SHP\\commune_base.shp"
base_Dissolve = wd+"\\results\\"

groupfield = ["CODE_CO","CODE_CA","EPCI","CODE_A","CODE_D","CODE_R"]
calcfield = ["KM2","count","ROAD_K","POP_M","CHOM1"]

# display layer
layer = iface.addVectorLayer(base, "base", "ogr")

layerfields = []
for field in layer.pendingFields():
    layerfields.append(field.name())


	
layercalc = []
for calc in calcfield:
    layercalc.append(layerfields.index(calc))

	
	
# Process
for field in groupfield:    
	processing.runalg("gdalogr:dissolvepolygons",base,"geometry",field,False,False,False,False,False,field,"",base_Dissolve+"intermed\\DELETE"+field+".shp")
# cursor
	cursor = layer.getFeatures()
#get aggregation column index
	groupindex = layerfields.index(field)
# row id list
	oidlist = []
# list values in each row
	sumlist = []
	for feature in cursor:
		attrs = feature.attributes()
		oid = attrs[groupindex]
		if oid in oidlist:
			sums = sumlist[oidlist.index(oid)]
			for i in range(len(calcfield)):
				sums[i] = sums[i] + attrs[layercalc[i]]
		else:
			oidlist.append(oid)
			sums = []
			for i in range(len(calcfield)):
				sums.append(attrs[layercalc[i]]) 
			sumlist.append(sums)
# write results, csv format
	fichier = open(base_Dissolve+"intermed\\"+field+".csv","w")
	fichier.write(field)
	for calc in calcfield:
		fichier.write(";"+calc)
	fichier.write("\n")
	for i in range(len(oidlist)):
		fichier.write(oidlist[i])
		sums = sumlist[i]
		for s in sums:
			fichier.write(";"+str(s))
		fichier.write("\n")
	fichier.close()
# save layer with csv join
	processing.runalg("qgis:joinattributestable",base_Dissolve+"\\intermed\\DELETE"+field+".shp",base_Dissolve+"\\intermed\\"+field+".csv",field,field,base_Dissolve+field+"JOIN.shp")
# display layers
	iface.addVectorLayer(base_Dissolve+field+"JOIN.shp", field, "ogr")

	
	
# STOP : execution time
print(str(time.time()-time0)+" sec")



