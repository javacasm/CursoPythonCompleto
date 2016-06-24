#!/usr/bin/python
# -*- coding: utf-8 -*-

class Animal:

	def __init__(self, nombre, patas):
		self.nombre = nombre
		self.patas = patas

	def saluda(self):
		print "El animal llamado" + self.nombre + "saluda"

class Perro(Animal):

	def ladra(self):
		print "Guau"

	def saluda(self):
		print "El perro da la patita"
	
	
mi_mascota = Perro("Rufo",4)
mi_mascota.saluda()

mi_mascota.ladra()
