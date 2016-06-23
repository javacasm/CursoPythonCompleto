

fichero = open("archivo.txt", "r")

resultado = fichero.read()

print resultado

fichero.close()









fichero = open("archivo.txt", "r")

resultado1 = fichero.readline()
resultado2 = fichero.readline()

print resultado1
print resultado2

fichero.close()







fichero = open("archivo.txt", "r")

resultado = fichero.readlines()

print resultado[0]
print resultado[1]

fichero.close()





fichero = open("archivo.txt", "r")

for linea in fichero:
    print linea

fichero.close()
