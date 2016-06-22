#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  mi_primer_ejemplo.py
#  
#  Copyright 2016 Manu <makova65@gmail.com>
#  
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#  
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#  MA 02110-1301, USA.
#  
#  

import sys
from Tkinter import *

def hacer_click():
	try:
		_valor = int(entrada_texto.get())
		_valor = _valor * 5
		etiqueta.config(text=_valor)
	except ValueError:
			etiqueta.config(text="Introduce un número!")
			

app = Tk() 
app.title("Otra app gráfica en Python") 

#Ventana principal
vp = Frame(app)
vp.grid(column=0, row=0, padx=(50,50), pady=(10,10))
vp.columnconfigure(0, weight=1)
vp.rowconfigure(0, weight=1)

etiqueta = Label(vp, text="Valor")
etiqueta.grid(column=2, row=2, sticky=(W,E))

boton = Button(vp, text="OK!", command=hacer_click)
boton.grid(column=1, row=1)

valor = ""
entrada_texto = Entry(vp, width=10, textvariable=valor)
entrada_texto.grid(column=2, row=1)

app.mainloop()

