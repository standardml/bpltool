This is a mirror of the current schemas on the isb, with absolute paths changed to relative paths. 

All paths are relative and schemas are assumed to lie in the same folder.

import orders:

pipLax.xsd imports Invoice.xsd
pielax.xsd imports Invoice.xsd
pcmlax.xsd imports Invoice.xsd
pcplax.xsd imports Invoice.xsd

pcmStrict.xsd imports pieStrict.xsd
pcpStrict.xsd imports pipStrict.xsd
pipStrict.xml imports Pipredefines.xsd and pipCOMredefines.xsd
pieStrict.xml imports PIEredefines.xsd and pieCOMredefines.xsd
PIEredefines.xsd imports pieCOMredefines.xsd
Pipredefines.xsd imports pipCOMredefines.xsd
pieCOMredefines.xsd imports CoreComponentTypesDk.xml and CoreComponentParametersDk.xml
pipCOMredefines.xsd imports CoreComponentTypesDk.xml and CoreComponentParametersDk.xml

Invoice.xml imports 0p70dk_Reusable.xml and nonCardinalTypes.xsd
nonCardinalTypes.xsd imports reusabletypes.xsd and 0p70dk_Reusable.xml

0p70dk_Reusable.xml imports CoreComponentTypesDk.xml and CoreComponentParametersDk.xml