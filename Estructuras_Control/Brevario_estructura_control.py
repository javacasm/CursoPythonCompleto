#!/usr/bin/python
# -*-coding: utf-8 -*-

####################################################
## Estructura de control
####################################################

# Hagamos sólo una variable
una_variable = 5

# Aquí está una declaración de un 'if'. ¡La indentación es importante en Python!
# imprime "una_variable es menor que 10"
if una_variable > 10:
    print "una_variable es completamente mas grande que 10."
elif una_variable < 10:    # Este condición 'elif' es opcional.
    print "una_variable es mas chica que 10."
else:           # Esto también es opcional.
    print "una_variable es de hecho 10."


"""
For itera sobre listas
imprime:
    perro es un mamifero
    gato es un mamifero
    raton es un mamifero
"""
for animal in ["perro", "gato", "raton"]:
    # Puedes usar % para interpolar strings formateados
    print "%s es un mamifero" % animal

"""
`range(número)` retorna una lista de números
desde cero hasta el número dado
imprime:
    0
    1
    2
    3
"""
for i in range(4):
    print i

"""
While itera hasta que una condición no se cumple.
imprime:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print x
    x += 1  # versión corta de x = x + 1

# Maneja excepciones con un bloque try/except

# Funciona desde Python 2.6 en adelante:
try:
    # Usa raise para levantar un error
    raise IndexError("Este es un error de indice")
except IndexError as e:
    pass    # Pass no hace nada. Usualmente harias alguna recuperacion aqui.
