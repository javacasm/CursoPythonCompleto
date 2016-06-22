#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  otra_app78.py
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


from Tkinter import *           # Importamos el modulo Tkinter
 
 
def DrawList(): # Creamos una lista con algunos nombres
        plist = ['Pablo','Renato','José Alonso']
 
        for item in plist: # Insertamos los items en un Listbox
                listbox.insert(END,item);
                 
         
root = Tk()                     # Creamos una ventana de fondo
 
listbox = Listbox(root)
boton = Button(root,text = "Presionar",command = DrawList)
 
boton.pack()
listbox.pack()                  # Hacemos los pack() del boton y el Listbox
root.mainloop()                 # Entramos en el loop   
