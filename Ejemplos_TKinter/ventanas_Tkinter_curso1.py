#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  ventanas_Tkinter_curso1.py
#  



from Tkinter import *
class Application(Frame):
    def __init__(self, master):
        Frame.__init__(self, master)
        self.label = Label(master,
                    text='Mira siempre el lado bello de la vida')
        self.button = Button(master, text='Click', command=self.foo)
        self.label.pack()
        self.button.pack()
    def foo(self):
        print "Hola, comando!"
 
if __name__ == '__main__':
    root = Tk()
    root.title('Hola, mundo y ole')
    app = Application(root)
    root.mainloop()
