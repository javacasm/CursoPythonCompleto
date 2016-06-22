#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  otra_app78.py
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
