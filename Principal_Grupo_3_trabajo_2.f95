program juego_de_la_vida

   use mod_juego_vida

   implicit none   

  ! Declara de variables

   type(cell), allocatable :: celula(:,:)
   integer                 :: n, tiempo_total, i, comprobacion
   integer                 :: info
   real                    :: p
   integer, allocatable    :: t_inf(:,:)     !almacena el tiempo que lleva esa celula infectada sin aun haberse recuperado

   open(unit=11, file='Estadisticas_Celulas.dat',iostat=info, action='readwrite',status='unknown')    !abrimos el archivo donde guardamos los datos de las celulas
         if (info>0) STOP 'Error al abrir el archivo'
   
p = 0.25 ! Probabilidad de estar con vida

call PantallaDeInicio

write(*,*) "Introduzca la dimension del dominio:"
read(*,*) n
write(*,*) !Para dejar espacio

allocate(celula(n,n), stat=info)
  if (info>0) STOP 'Error al alocar'
allocate(t_inf(n,n),stat=info)
  if (info>0) STOP 'Error al alocar'
  t_inf=0 !todos los tiempos iniciales de infeccion valen 0

write(*,fmt='(A,2X,I5)') "El numero total de celulas es:", n*n
write(*,*)  !Deja espacio

write(*,*) "Introduzca el tiempo que desea que dure la simulacion:"
read(*,*) tiempo_total
write(*,*)  !Deja espacio

!LEYENDA DE ELEMENTOS DEL DOMINIO
write(*,fmt='(A,/,A,/,A,/,A,/)') 'LEYENDA:', 'Espacio en blanco: Celula sana', 'Espacio con #: Celula infectada'& 
                                 ,'Espacio con X: Celula muerta'

  write(*,*) '========== INICIANDO SIMULACION =========='

  ! Inicializa el estado de las celulas con cierta probabilidad de vida.
   call InicializarDominio(n, p, celula)
   

  ! Muestra dominio por la terminal
   write(*,*) "El tablero con la situacion inicial de las celulas es el siguiente:"
   call MostrarDominio(celula)

   write(*,*) 'Para comenzar a ver su evolucion, presione cualquiere tecla'
   read(*,*)                                                                  !cualquier input que reciba el programa activará la simulacion

  !Evolucion de las celulas en base a las reglas

   do i=1,tiempo_total

    write(*,fmt='(/,A,1X,I3,/)') 'Unidad temporal:', i


    call Estadisitica_Celulas(celula,t_inf)

    call Evolucion(celula, comprobacion)

    call porcentaje_grafica(celula,n,i)

  
  !Para durante 1seg la terminal, para poder visualizar correctamente la simulacion.
    call espera(1.0)

    if (comprobacion==0) exit

   enddo

   call system ('gnuplot.exe Graficas_gnuplot_Grupo_3_trabajo_2.gnu') !Llama a gnuplot para mostar todos los graficos de los datos recopilados durante la simulacion

  !Desaloja el dominio y otros datos usados
   deallocate(celula)
   deallocate(t_inf)
  
  !Cierra archivos
   close(10)
   close(11)
   

end program