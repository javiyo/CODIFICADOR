program cifrador
use criptografia
integer :: tecnic, k,b,i,long
character(len=10) :: archivoplano, archivociph !Archivoplano(nombre del archivo que queremos cifrar);Archivociph(nombre del archivo donde aparece el archiplano cifrado)
character :: letra
	print *, '¿Que tecnica de cifrado quieres emplear?' !Seleccion de tecnica
	print *, 'Para aditivo introduzca 1'
	print *, 'Para multiplicativo introduzca 2'
	print *, 'Para afin introduzca 3'
	read *, tecnic

	
	i=0
	do while(i==0)
		if (tecnic==3) then
			print *, 'Introduce las claves de cifrado'
			read *, k,b 
		else            !Para aditivo y multiplicativo solo es necesario una clave,k,para afin,dos,k y b"
			print *, 'Introduce la clave de cifrado'
			read *, k
		end if
		k=abs(k)
		b=abs(b)
		if ((mod(k,2)==0.OR.mod(k,13)==0).AND.(tecnic==3.OR.tecnic==2)) then !Los múltiplos de 2 y 13 como clave para el cifrado multiplicativo o afin no son válidas,de esta manera informamos al usuario
			print *, 'Error, con esa clave es imposible encontrar inversa'
		else
			i=1
		end if
	end do
	
	print *, '¿Como se llama el archivo con el mensaje a cifrar?(sin extension)'
	read *, archivoplano
	print *, '¿Como quieres llamar al archivo con el mensaje cifrado?(sin extension)'
	read *, archivociph
                            !Apertura de los archivos para lectura y posterior escritura, , la extension es añadida automaticamente
			open(unit=11, file=trim(archivoplano)//".txt",status="old")
			open(unit=12, file=trim(archivociph)//".cfr") 

	long=larguitud(11)
                        !Proceso de lectura y escritura de los archivos,se leen letra a letra y se escriben letra a letra,advance es usado para leer en la misma linea y no ir saltando a la siguiente
	if (tecnic==1) then
		do i=1,long
		read(11, "(A1)",advance='no')letra
		write(12, "(A1)",advance='no')numletra(aditivo(letranum(letra),k)) !Va llamando a las distintas funciones con los argumentos correctos
		end do
	else if (tecnic==2) then
		do i=1,long
		read(11, "(A1)",advance='no')letra
		write(12, "(A1)",advance='no')numletra(multiplicativo(letranum(letra),k))
		end do
	else if (tecnic==3) then
		do i=1,long
		read(11, "(A1)",advance='no')letra
		write(12, "(A1)",advance='no')numletra(afin(letranum(letra),k,b))
		end do
	else
		print *, "Error, seleccion de tecnica incorrecta" !Error debido a la introduccion de un número distinto de 1,2,3,ya que solo esos numeros tienen asignados las tecnicas de cifrados
	end if
end program cifrador
