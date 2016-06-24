#!/usr/bin/python
# -*- coding: utf-8 -*-


def genera_lista(num):
	i = 1
	while i <= num:
		yield i
		i += 1

for i in genera_lista(5):
		print i

mi_nueva_lista = list(genera_lista(10))
