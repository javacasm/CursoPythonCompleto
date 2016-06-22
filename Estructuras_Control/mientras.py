#!/usr/bin/python
# -*- coding: utf-8 -*-

#sencillo
a = 0

while a < 10:
    a = a + 1
    print a
    
    
#Break
a = 0

while a < 10:
    a = a + 1
    if a == 3:
        break
    print a
    
#Continue
a = 0

while a < 10:
    a = a + 1
    if a == 3:
        continue
    print a
    
#else
a = 0

while a < 10:
    a = a + 1
    print a
else:
  print " Adios"
