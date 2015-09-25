The data used for these test can be downloaded here : https://www.dropbox.com/s/un3ryv5q1i2pog7/test_comparatif_outils.7z?dl=0

**Computer used for the tests informations :** 8 go RAM, 32 bits, CPU i5, 3.40 GHZ

**Tests parameters:**

- **Aggregate :** 6 aggregation levels, each with 5 fields to aggregate

- **Random points :** approximatively 150 000 random points created, two times (two shape files), with the fire surfaces values from Prométhée database[^1]

- **Spatial join :** outputs from random points script are spatially joined to outputs from aggregate scripts. The randomized fire surfaces values are summed

##Execution time (sec) examples

|              | Aggregate|Random points|Spatial join  
|--------------|----------|-------------|-------------
|ArcGIS 10.2.1 | 32       | 34          | 150
|Qgis 2.8.1    | 19       | 515         | 79
|R 3.1.2       | 37       | 40          | 98

##Lines of code count

|              | Aggregate|Random points|Spatial join     
|--------------|----------|-------------|-------------
|ArcGIS 10.2.1 | 11       | 36          | 26
|Qgis 2.8.1    | 53       | 40          | 20
|R 3.1.2       | 48       | 27          | 44

A python script was used in order to count lines of code. Cf. https://github.com/romain-louvet/line_count

##Main functions used

|              | Aggregate                                        |Random points                                             |Spatial join     
|--------------|--------------------------------------------------|----------------------------------------------------------|-------------
|ArcGIS 10.2.1 | arcpy.Dissolve_management()                      | arcpy.CreateRandomPoints_management()                    | arcpy.SpatialJoin_analysis() 
|Qgis 2.8.1    | processing.runalg("dalogr:dissolvepolygons",...) | processing.runalg(''qgis:randompointsinlayerbounds'',...)| processing.runalg(''qgis:joinattributesbylocation'',...)[^3].
|R 3.1.2       | aggregate()                                      | spsample()                                               | over()

There is a bug with ''qgis:joinattributesbylocation''. This tool isn't working properly. cf. http://hub.qgis.org/issues/12922