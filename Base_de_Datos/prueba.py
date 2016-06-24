#!/usr/python
# -*- coding: utf-8 -*-

import MySQLdb

# Establecemos la conexión
Conexion = MySQLdb.connect(host='localhost', user='oso',passwd='panda', db='Tienda')

# Creamos el cursor
micursor = Conexion.cursor()

articulo = raw_input("Articulo: ")
micursor.execute("INSERT INTO articulos (articulo) VALUES ('%s')" % articulo)




"""
articulo = raw_input("Articulo: ") 
descripcion = raw_input("Descripcion: ")
precio = raw_input("Precio: ")
stock = raw_input("Stock: ")
micursor.execute("INSERT INTO articulos (articulo,descripcion,precio,stock) VALUES ('%s,%s,%s,%s')" % articulo, %descripcion, %precio, %stock)
"""




# Hacemos un commit, por si las moscas
Conexion.commit()

# Ahora vamos a hacer un SELECT
query= "SELECT * FROM articulos WHERE precio=10;"

micursor.execute(query)

# Obtenemos el resultado con fetchone
registro= micursor.fetchone()

# Imprimimos el registro resultante
print registro

#Vemos el numero de registros con la propiedad rowcount de la clase cursor, de este modo:

numero_de_registros= micursor.rowcount 

print "Se ha ingresado " + str(numero_de_registros) + " registros" 



