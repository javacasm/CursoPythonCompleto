#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  posiciones.py
#  

import csv 

fichero = open("otro_archivo.txt", "w") #Crea y sobreescribe un archivo.txt

fichero.write("1234567890 que cosas pondría")

fichero.seek(17)

fichero.write("XD")



fichero.close()
