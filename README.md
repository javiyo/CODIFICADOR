# CODIFICADOR
Un codificador/decodificador de mensajes. 
Instalación ubuntu:

$sudo apt install git

###Si no esta instalado ya

$sudo apt install gfortran

###Si no estan instalados ya

$git clone https://github.com/javiyo/CODIFICADOR/

$cd CODIFICADOR

$gfortran -c criptografia.f90

$gfortran cifrador.f90 criptografia.f90 -o cifrador

$gfortran descifrador.f90 criptografia.f90 -o descifrador

Para correr
$./cifrador
$./descifrador

Nota: El $ es solo para indicar el comando, no ponerlo en el terminal
Creado por el grupo B de informática.
