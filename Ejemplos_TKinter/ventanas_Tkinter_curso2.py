#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  ventanas_Tkinter_curso2.py
#  


from Tkinter import Tk, Label, Button

class MyFirstGUI:
    def __init__(self, master):
        self.master = master
        master.title("Una simple GUI")

        self.label = Label(master, text="Esta es nuestra primera GUI!")
        self.label.pack()

        self.greet_button = Button(master, text="Saluda", command=self.greet)
        self.greet_button.pack()

        self.close_button = Button(master, text="Cierra", command=master.quit)
        self.close_button.pack()

    def greet(self):
        print("¡Hola curso Darwin Eventur Python!")

root = Tk()
my_gui = MyFirstGUI(root)
root.mainloop()
