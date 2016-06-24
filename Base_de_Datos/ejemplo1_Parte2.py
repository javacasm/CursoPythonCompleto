#!/usr/python
# -*- coding: utf-8 -*-

import MySQLdb

# Establecemos la conexión
Conexion = MySQLdb.connect(host='localhost', user='oso',passwd='panda', db='Tienda')

# Creamos el cursor
micursor = Conexion.cursor()

# Ahora vamos a hacer un SELECT
query= "SELECT * FROM articulos;"

micursor.execute(query)

# Obtenemos el resultado con fetchone
registro= micursor.fetchone()

# Imprimimos el registro resultante
print registro

#Vemos el numero de registros con la propiedad rowcount de la clase cursor, de este modo:

numero_de_registros= micursor.rowcount 

print "Se ha ingresado " + str(numero_de_registros) + " registros" 

