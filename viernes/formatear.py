#!/usr/bin/python
# -*- coding: utf-8 -*-

nombre = "Pablo"

cadena_formateada = "Hola %s" % nombre

print cadena_formateada











#!/usr/bin/python
# -*- coding: utf-8 -*-

nombre = "Superman"
otro_nombre = "Clark Kent"

cadena_formateada = "¡%s es %s!" % nombre, otro_nobre

print cadena_formateada








#!/usr/bin/python
# -*- coding: utf-8 -*-

numero = 100

cadena_formateada = "y %d gaviotas dónde irán" % numero

print cadena_formateada










#!/usr/bin/python
# -*- coding: utf-8 -*-

numero = 1000

print "%d en hexadecimal (minúsculas) es %x" % (numero, numero)

print "%d en hexadecimal (mayúsculas) es %X" % (numero, numero)

print "%d en exponencial (minúsculas) es %e" % (numero, numero)

print "%d en exponencial (mayúsculas) es %E" % (numero, numero)

















#!/usr/bin/python
# -*- coding: utf-8 -*-

meses = ["Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"]

for i in meses:
    print "* %10s * %-10s *" % (i,i)


