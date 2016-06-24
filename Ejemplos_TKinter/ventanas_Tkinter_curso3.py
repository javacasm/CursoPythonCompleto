#!/usr/bin/python
# -*- coding: utf-8 -*-
#
#  ventanas_Tkinter_curso3.py
#  


from Tkinter import Tk, Label, Button, StringVar

class MyFirstGUI:
    LABEL_TEXT = [
         "Esta es la segunda interfaz gráfica de usuario.",
         "Hemos hecho que sea más interesante ...",
         "... Al hacer esta etiqueta interactiva.",
         "Adelante, haz clic en él de nuevo.",
    ]
    def __init__(self, master):
        self.master = master
        master.title("Una simple GUI")

        self.label_index = 0
        self.label_text = StringVar()
        self.label_text.set(self.LABEL_TEXT[self.label_index])
        self.label = Label(master, textvariable=self.label_text)
        self.label.bind("<Button-1>", self.cycle_label_text)
        self.label.pack()

        self.greet_button = Button(master, text="Saluda", command=self.greet)
        self.greet_button.pack()

        self.close_button = Button(master, text="Cierra", command=master.quit)
        self.close_button.pack()

    def greet(self):
        print("¡Saludos pythoneros!")

    def cycle_label_text(self, event):
        self.label_index += 1
        self.label_index %= len(self.LABEL_TEXT) # encierra alrededor
        self.label_text.set(self.LABEL_TEXT[self.label_index])

root = Tk()
my_gui = MyFirstGUI(root)
root.mainloop()
