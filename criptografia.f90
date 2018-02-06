module criptografia

contains

	function letranum(a) !Funcion que asigna a cada letra un número para su posterior cifrado
	character, intent(in) :: a
	integer :: letranum, i
	character,dimension(26) :: v
	v=(/"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"/)
	do i=1,26
		if (a==v(i)) letranum=i
	end do
	
	end function		

	function numletra(n) !Función que tras el cifrado,asigna el número obtenido con la técnica de cifrado a la nueva letra.
	integer, intent(in) :: n
	character :: numletra   		  
	character,dimension(26) :: v
	v=(/"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"/)
	numletra=v(n)
	end function


	function aditivo(a,k) !Función de cifrado aditivo
	integer, intent(in) :: a, k	
	integer :: ad, aditivo  !El aditivo suma la clave a nuestro numero
	ad = a + k
	do while (ad>26) !Si tras la adicion,obtenemos un número mayor que 26,restamos 26,ya que las letras tienen numero asginados en el intervalo [1,26]
	ad= ad-26
	end do
	aditivo = ad
	end function	
	
	function multiplicativo(a,k) !Simple funcion que nos devuelve el valor de la multiplicacion de nuestra clave por la letra
	integer, intent(in) :: a,k
	integer :: mul, multiplicativo
	mul = k*a
	multiplicativo=mod(mul,26)	!Como el valor puede ser mayor de 26 deberemos calcular el resto,que sera el valor utilizado en la asignacion de la nueva letra
	if (multiplicativo==0) multiplicativo=26
	end function multiplicativo
		
	function afin(a,k,b)
	integer, intent(in) :: a,k,b
	integer :: afin                 !Afin (realiza el multiplicativo y le suma el aditivo)	
	afin=mod((k*a)+b,26)
	if (afin==0) afin=26 !Cuando k*a+b sea igual a 26 el resto es 0, por lo que esta funcion nunca nos devuelve el numero 26
	end function afin    !para asignarlo a la z, tendremos que cambiar el 0 por la z manualmente(único caso que esto sucede)

	function rkaditivo(k) !Funcion que calcula la clave inversa en el cifrado aditivo
	integer,intent(in) :: k
	integer :: rkaditivo, ad
	ad=k
	do while (ad>26) !Restamos 26,ya que nuestra tabla con el abecedario solo tiene 26 caracteres
	ad=ad-26
	end do
	rkaditivo=26-ad
	end function rkaditivo
	
	function rkmultiplicativo(k) !Función que calcula la clave inversa en el cifrado multiplicativo
	integer, intent(in) :: k
	integer :: rkmultiplicativo, i=1
	do while (mod(26*i+1,k)/=0)!Este bucle hace satisfacer la condicion necesaria de que k*rk=1mod26 (siendo rk la clave inversa)
	i=i+1
	end do
	rkmultiplicativo=(26*i+1)/k
	end function rkmultiplicativo
	
	subroutine rkafin(k,b) !Función que calcula las claves inversas en el cifrado afín, combinacion de las funciones anteriores
	integer :: k, b
	k=rkmultiplicativo(k) !Aplica el algoritmo para encontrar la clave inversa
	b=rkaditivo(b)*k 
	do while (b>26) 
	b= b-26
	end do
	end subroutine

	function larguitud(a)!Función que cuenta el numero de carácteres a cifrar
	integer, intent(in) :: a
	character(len=32767) :: texto
	read(a,*)texto
	rewind(a)
	larguitud=len_trim(texto)!Cuenta los caracteres del texto sin espacios
	if (larguitud>80) then
		print*,"ERROR; El texto a cifrar debe tener menos de 80 caracteres"
		stop
	end if
	end function larguitud

end module criptografia
