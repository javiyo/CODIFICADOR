module criptografia

contains

	function letranum(a) !Funcion que traduce de las letras al codigo
	character, intent(in) :: a
	integer :: var,letranum
	letranum=iachar(a) - 96 !Funciona gracias a la funcion iachar() que nos devuelve el valor ASCII de cada letra
	end function		!El -96 es unicamente para que sea acorde a los numeros que nos proporciona fernando en la tabla 
	
	function numletra(n)
	integer, intent(in) :: n
	character :: numletra   		  !Funcion inversa de la anterior, achar() nos devuelve una letra al meter un entero
	numletra=achar(n+96)			  !Tenemos que deshacer el -96 de antes
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
	multiplicativo=mod(mul,26)	!Como el valor puede ser mayor de 26 deberemos de calcular el resto al seguir con los numeros
	end function multiplicativo
		
	function afin(a,k,b)
	integer, intent(in) :: a,k,b
	integer :: afin                 !Afin (realiza el multiplicativo y le suma el aditivo)	
	afin=mod(k*a+b,26)
	if (afin==0) afin=26 !Cuando k*a+b sea igual a 26 el resto es 0, por lo que esta funcion nunca nos devuelve el numero 26
	end function afin    !para asignarlo a la z, tendremos que cambiar el 0 por la z manualmente

	function rkaditivo(k)
	integer,intent(in) :: k
	integer :: rkaditivo, ad
	ad=k
	do while (ad>26)
	ad=ad-26
	end do
	rkaditivo=26-ad
	end function rkaditivo
	
	function rkmultiplicativo(k)
	integer, intent(in) :: k
	integer :: rkmultiplicativo, i=1
	do while (mod(26*i+1,k)/=0)
	i=i+1
	end do
	rkmultiplicativo=(26*i+1)/k
	end function rkmultiplicativo
	
	subroutine rkafin(k,b)
	integer :: k, b
	k=rkmultiplicativo(k)
	b=rkaditivo(b)*k
	do while (b>26) 
	b= b-26
	end do
	end subroutine
	

end module criptografia
