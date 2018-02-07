program decipher
use criptografia
integer :: tecnic, k,b,i,long
character(len=10) :: archivoplano, archivociph !Archivoplano(nombre del archivo que queremos descifrar);Archivociph(nombre del archivo donde aparece el archiplano descifrado)
character :: letra
	print *, '¿Que tecnica de descifrado quieres emplear?' !Introduccion de tecnica
	print *, 'Para aditivo introduzca 1'
	print *, 'Para multiplicativo introduzca 2'
	print *, 'Para afin introduzca 3'
	read *, tecnic

	
	i=0
	do while(i==0) !Bucle que sirve para que en caso de introduccion de datos erronea nos vuelva a pedir claves
		if (tecnic==3) then
			print *, 'Introduce las claves de cifrado'
			read *, k,b
		else
			print *, 'Introduce la clave de cifrado'
			read *, k
		end if
		k=abs(k)
		b=abs(b)
		if ((mod(k,2)==0.OR.mod(k,13)==0).AND.(tecnic==3.OR.tecnic==2)) then !De igual manera que el cifrado,los múltiplos de 2 y 13 no pueden ser claves para descifrar,indicamos aqui el error al usuario
			print *, 'Error, con esa clave es imposible encontrar inversa'
		else
			i=1
		end if
	end do

	
	print *, '¿Como se llama el archivo con el mensaje que quieres descifrar?(sin extension)' !Introduccion de nombres de archivo
	read *, archivociph
	print *, '¿Como quieres llamar al archivo con el mensaje descifrado?(sin extension)'
	read *, archivoplano
                            !Apertura de los archivos cifrados y del archivo donde se va a descrifrar, la extension es añadida automaticamente
			open(unit=11, file=trim(archivociph)//".cfr",status="old")
			open(unit=12, file=trim(archivoplano)//".dcf") 

	long=larguitud(11) !En este momento fijamos la longitudo de nuestro mensaje gracias a la funcion larguitud

	if (tecnic==1) then !Proceso de lectura del archivo cifrado y posterior escritura del archivo descifrado letra a letra,advance es usado para leer en una linea
		do i=1,long
		read(11, "(A1)",advance='no')letra
		write(12, "(A1)",advance='no')numletra(aditivo(letranum(letra),rkaditivo(k))) !Va llamando a las distintas funciones con los argumentos correctos
		end do
	else if (tecnic==2) then
		do i=1,long
		read(11, "(A1)",advance='no')letra
		write(12, "(A1)",advance='no')numletra(multiplicativo(letranum(letra),rkmultiplicativo(k)))
		end do
	else if (tecnic==3) then
		call rkafin(k,b)
		do i=1,long
		read(11, "(A1)",advance='no')letra
		write(12, "(A1)",advance='no')numletra(afin(letranum(letra),k,b))
		end do
	else
		print *, "Error, seleccion de tecnica incorrecta" !Error que se da cuando se introduce un numero distinto de 1,2,3,ya que solo esos numeros tienen asignados las técnicas de cifrado
	end if
	close(11)
	close(12)

end program decipher
