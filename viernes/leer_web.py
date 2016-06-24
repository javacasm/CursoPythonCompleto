#!/usr/bin/python
# -*- coding: utf-8 -*-

import urllib2

una_web = urllib2.urlopen("http://www.psicobyte.com")

cabeceras = una_web.info()

contenido = una_web.read()

