#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  datos_web1.py
#  


import csv
fichero = open('datos.csv')  #abre el archivo datos.csv
 
datos = csv.reader(fichero, delimiter=',')

for fila in datos:
	for celda in fila:
		print celda
 
