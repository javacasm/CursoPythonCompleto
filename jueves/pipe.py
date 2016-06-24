#!/usr/bin/python
# -*- coding: utf-8 -*-


import os

no_soy_un_fichero = os.popen("ls")

resultado = no_soy_un_fichero.read()

print resultado
