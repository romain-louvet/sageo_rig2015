# ---------------------------------------------------------------------------
# test_arcgis_aggre.py
# Created on: 2015-06-11 
# Author : Romain Louvet, PhD student
# Contact : romain.louvet@alumni.univ-avignon.fr
# Description: 
# This script can be executed independently with python or with the python console in ArcMAP (open console, right click, load).
# It needs python 2.7 and ArcGIS 10 (it was tested with Python 2.7.5 and ArcGIS 10.2.1).
#
# Dissolve shape according to a list of aggregation fields and
# sum values of a list of fields for each aggregation.
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy
# sys time
import time

# START : execution time
time0 = time.time()

# Local variables:
## working directory (depend on which computer this script is executed)
wd = "C:\\Users\\Romain Louvet\\Desktop\\test_comparatif_outils"
## shape at the most disaggregated level, with aggregation fields
base = wd+"\\SHP\\commune_base.shp"
## output directory for aggregated level
base_Dissolve = wd+"\\results\\"

# parameters
## aggregation fields
groupfield = ["CODE_CO","CODE_CA","EPCI","CODE_A","CODE_D","CODE_R"]
## variables fields (to sum)
calcfield = "KM2 SUM;count SUM;ROAD_K SUM;POP_M SUM;CHOM1 SUM"

# Process:
for f in groupfield:
	arcpy.Dissolve_management(base, base_Dissolve+f+".shp", f, calcfield, "MULTI_PART", "DISSOLVE_LINES")
		
# STOP : execution time
print(str(time.time()-time0)+" sec")