#!/usr/python
# -*- coding: utf-8 -*-

import MySQLdb

# Establecemos la conexión
Conexion = MySQLdb.connect(host='localhost', user='oso',passwd='panda', db='Tienda')

# Creamos el cursor
micursor = Conexion.cursor()

dato = raw_input("Dato: ")


# Ejecutamos un insert directamente
micursor.execute("INSERT INTO articulos (articulo) VALUES ('%s')" % dato



