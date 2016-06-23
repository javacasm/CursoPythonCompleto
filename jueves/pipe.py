import os

no_soy_un_fichero = os.popen("ls")

resultado = no_soy_un_fichero.read()

print resultado
