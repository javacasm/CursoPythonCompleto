#!/usr/bin/python
# -*- coding: utf-8 -*-

"""
navegador.py programa que abre una URL en el navegador por defecto
"""

"""usamos el módulo webbrowser"""
import webbrowser

url = "https://youtu.be/P-7moc6I9Jg?t=27s"

webbrowser.open(url, new=2)
