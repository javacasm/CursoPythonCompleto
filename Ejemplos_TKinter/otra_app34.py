#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  otra_app34.py
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
