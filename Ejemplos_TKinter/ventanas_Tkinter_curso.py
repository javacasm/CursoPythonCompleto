#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  ventanas_Tkinter_curso.py
#  


from Tkinter import *
root = Tk(className ="Mi primera GUI")
svalue = StringVar()                           # definimos el widget como string
w = Entry(root,textvariable=svalue)            # añadimos textarea widget
w.pack()
def act():
    print "por pantalla"
    print '%s' % svalue.get()
foo = Button(root,text="Clikear", command=act) # clase de objeto foo
foo.pack()
root.mainloop()
