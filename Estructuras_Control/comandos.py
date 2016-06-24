#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  comandos.py
#  
#  

import os

no_soy_un_fichero = os.popen("df")       #Se pueden meter otros comandos

resultado = no_soy_un_fichero.read()     #como sudo, df, etc...

print resultado
