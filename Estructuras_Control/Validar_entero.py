#!/usr/bin/python
# -*- coding: utf-8 -*-


def lee_entero():
    """ Solicita un valor entero y lo devuelve.
        Mientras el valor ingresado no sea entero, vuelve a solicitarlo. """
    while True:
        valor = raw_input("Ingrese un número entero: ")
        try:
            valor = int(valor)
            return valor
        except ValueError:
            print "ATENCIÓN: Debe ingresar un número entero."

val=lee_entero()
print val
