#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  ptra_app56.py
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


from Tkinter import *
 
 
def Call(): # Definimos la funcion
        lab= Label(root, text = 'Usted presiono\nel boton')
        lab.pack()
        boton['bg'] = 'blue' # Al presionar queda azul
        boton['fg'] = 'white' # Si pasamos el Mouse queda blanco
 
root = Tk() # Ventana de fondo
root.geometry('100x110+350+70') # Geometría de la ventana
boton = Button(root, text = 'Presionar', command = Call) 
boton.pack()
 
root.mainloop()
