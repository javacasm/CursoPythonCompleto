#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  ptra_app56.py
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
