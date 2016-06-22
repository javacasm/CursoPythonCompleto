#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  otra_app34.py
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


from Tkinter import *           # Importamos el módulo Tkinter
root = Tk()                    # Creamos una ventana de fondo
                                # Creamos dos listas
li     = ["Pablo", "Renato", "José Aloso", "JJ", "Nuria"]
movie  = ["El padrino", "Naruto", "La gran estafa", "Los juegos del hambre"]
listb  = Listbox(root)          # Creamos dos Listbox
listb2 = Listbox(root)
for item in li:                 # Insertamos los nombres en el primer Listbox
    listb.insert(0,item)
 
for item in movie:              # Insertamos las peliculas en el segundo Listbox
    listb2.insert(0,item)
 
listb.pack()                    # Hacemos el pack() de los dos Listbox
listb2.pack()
root.mainloop()                 # corremos el loop)                 
