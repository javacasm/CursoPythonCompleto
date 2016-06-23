#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  ventanas_TKinter_curso6.py
#  
#  


from Tkinter import *            # Importamos la libreria Tkinter
root = Tk()                      # Crear un objeto de la ventana de fondo
                                 # Una forma sencilla de crear 2 listas
                                 
lista     = ['Pablo','Renato','Lidia','José Alonso','Antonio','Manu']
movie  = ['El Padrino','Juego de Tronos','Pulp Fiction']

listb  = Listbox(root)           # Crear 2 widgets en cuadro de lista
listb2 = Listbox(root)

for item in lista:               # Inserte cada elemento dentro de lista en listb
    listb.insert(1,item)
                                  # (1)Podemos cambiar item por append
for append in movie:              # Haga lo mismo para el segundo cuadro de lista
    listb2.insert(1,append)
                                  # (2)como es la segunada lista
listb.pack()                      # Empaquetar cada cuadro de lista en la ventana principal
listb2.pack()
root.mainloop()                   # Invocar el bucle principal de la gestión de eventos
