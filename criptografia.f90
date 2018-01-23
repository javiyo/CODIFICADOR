module criptografia

contains

	function letranum(a) !Funcion que traduce de las letras al codigo
	character, intent(in) :: a
	integer :: var,letranum
	letranum=iachar(a) - 96 !Funciona gracias a la funcion iachar() que nos devuelve el valor ASCII de cada letra
				!El -96 es unicamente para que sea acorde a los numeros que nos proporciona fernando en la tabla
	end function 
	
	function numletra(n)
	integer, intent(in) :: n
	character :: numletra     !Funcion inversa de la anterior, achar() nos devuelve una letra al meter un entero
					!Tenemos que deshacer el -96 de antes
	numletra=achar(n+96)
	end function


	function aditivo(a,k)
	integer, intent(in) :: a, k	
	integer :: ad, aditivo  !El aditivo suma la clave a nuestro numero
	ad = a + k
	do while (ad>26) 
	ad= ad-26
	end do
	aditivo = ad
	end function	
	
	function multiplicativo(a,k) !Simple funcion que nos devuelve el valor de la multiplicacion de nuestra clave por la letra
	integer, intent(in) :: a,k
	integer :: mul, multiplicativo
	mul = k*a
	if (mod(k,2)==0.OR.mod(k,13)==0) then
		print *, 'Error, con esa clave es imposible encontrar inversa'
	end if
	multiplicativo=mod(mul,26)	!Como el valor puede ser mayor de 26 deberemos de calcular el resto al seguir con los numeros
	end function multiplicativo
		
	function afin(a,k,b)
	integer, intent(in) :: a,k,b
	integer :: afin, af                   !Afin (realiza el multiplicativo y le suma el aditivo)
	if (mod(k,2)==0.OR.mod(k,13)==0) then
		print *, 'Error, con esa clave es imposible encontrar inversa'
	end if
	af = k*a+b
	afin=mod(af,26)
	end function afin
	







end module criptografia
