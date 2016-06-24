#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  ficheros_curso.py
#   


fichero = open("archivo.txt", "rw") #Hay que tener creado en la misma ruta archivo.txt

resultado = fichero.read()

print resultado

fichero.close()
