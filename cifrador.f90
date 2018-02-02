program cifrador
use criptografia
integer :: tecnic, k,b,i,long
character(len=10) :: archivoplano, archivociph
character :: letra
	print *, '¿Que tecnica de cifrado quieres emplear?'
	print *, 'Para aditivo introduzca 1'
	print *, 'Para multiplicativo introduzca 2'
	print *, 'Para afin introduzca 3'
	read *, tecnic

	
	i=0
	do while(i==0)
		if (tecnic==3) then
			print *, 'Introduce las claves de cifrado'
			read *, k,b
		else
			print *, 'Introduce la clave de cifrado'
			read *, k
		end if
		k=abs(k)
		b=abs(b)
		if ((mod(k,2)==0.OR.mod(k,13)==0).AND.(tecnic==3.OR.tecnic==2)) then
			print *, 'Error, con esa clave es imposible encontrar inversa'
		else
			i=1
		end if
	end do
	
	print *, '¿Como se llama el archivo con el mensaje a cifrar?(sin extension)'
	read *, archivoplano
	print *, '¿Como quieres llamar al archivo con el mensaje cifrado?(sin extension)'
	read *, archivociph
	
			open(unit=11, file=trim(archivoplano)//".txt",status="old")
			open(unit=12, file=trim(archivociph)//".cfr")
	
	long=larguitud(11)

	if (tecnic==1) then
		do i=1,long
		read(11, "(A1)",advance='no')letra
		write(12, "(A1)",advance='no')numletra(aditivo(letranum(letra),k))
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
	end if
end program cifrador
