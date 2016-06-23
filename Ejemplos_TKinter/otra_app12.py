#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  otra_app12.py
#  


from Tkinter import *                                      
root = Tk()                                                
                                                           
li = 'Diego Matias Martin Carla Lorena Roberto'.split()
listb = Listbox(root)                                      
for item in li:                                             
    listb.insert(text,item)
 
listb.pack()                                               
root.mainloop()                                            
