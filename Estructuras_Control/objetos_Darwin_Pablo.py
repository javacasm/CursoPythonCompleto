#!/usr/bin/python
# -*- coding: utf-8 -*-

class Producto:

	def __init__(self,producto,precio,unidades):
		self.producto = producto
		self.precio = precio
		self.unidades = unidades

	def vender(self,vendidas):
			if vendidas <= self.unidades:
				self.unidades = self.unidades - vendidas
				self.precio += 10
			else:
				print "No hay bastantes"

otro = Producto("corbata",135,27)

print otro.unidades

otro.vender(4)

print otro.unidades

#print milista[1].precio   #siempre poner el punto y nombre de la variable
