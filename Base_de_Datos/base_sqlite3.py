#!/usr/bin/python
# -*- coding: utf-8 -*
import sqlite3

con = sqlite3.connect('base_sqlite3_MEMORIA')

conexion = sqlite3.connect(':memory:') 

cursor = con.cursor()

print "La base de datos se abrio"
