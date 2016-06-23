



fichero = open("otro_archivo.txt", "w")

fichero.write("hola mundo")

fichero.close()






lista = ["una cosa", "otra cosa", "y otra cosa", "y otra"]

fichero = open("otro_archivo.txt", "a")

fichero.writelines(lista)

fichero.close()
