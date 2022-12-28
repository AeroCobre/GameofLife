module mod_juego_vida

   implicit none

   type cell
      integer :: estado          ! 0:vivo, 1:infectado
      integer :: ninfectado      !numero de veces que se ha infectado
      integer :: nsano           !numero de veces que se ha curado
      integer :: tmin            !unidades de tiempo minimo infectado
      integer :: tmax            !unidades de tiempo maximo infectado
      logical :: comprobar_Cambio   !comprueba si ha sufrido un cambio de estado recientemente para ver cuantas veces se infectó y recuperó
   end type cell   

   contains

   subroutine PantallaDeInicio
      write(*,*) '================ EL JUEGO DE LA VIDA ================'
      write(*,*) '      Creado por David Jimenez y Juan Martinez       '
      write(*,*) '_____________________________________________________'
   
   end subroutine

   subroutine InicializarDominio(tamano, probabilidad, dominio_inicial)

      integer, intent(in) :: tamano
      real, intent (in) :: probabilidad

      real :: numero_aleatorio
      integer :: i,j

      type(cell) :: dominio_inicial(tamano,tamano)
      dominio_inicial%tmax=0

      do i = 1, tamano

         do j = 1, tamano

            call random_number(numero_aleatorio)
            if (numero_aleatorio<probabilidad) then

               dominio_inicial(i,j)%estado = 0  !SANO
               dominio_inicial(i,j)%ninfectado = 0
               dominio_inicial(i,j)%tmin= 0     !0 porque ha empezado sin infectarse por tanto no tiene tiempo minimo
               dominio_inicial(i,j)%nsano = 0
               dominio_inicial(i,j)%comprobar_Cambio = .true.


            else 

               dominio_inicial(i,j)%estado = 1     !INFECTADO
               dominio_inicial(i,j)%ninfectado = 0
               dominio_inicial(i,j)%tmin = 1       !su tiempo minimo va a ser uno al empezar infectada
               dominio_inicial(i,j)%nsano = 0
               dominio_inicial(i,j)%comprobar_Cambio = .false.


            endif

         enddo

      enddo

   end subroutine     
       
      
   subroutine MostrarDominio(mostrar_dominio_inicial)

      type(cell) :: mostrar_dominio_inicial(:,:)
      character(len=1) :: dominio_inicial_char(size(mostrar_dominio_inicial,2),size(mostrar_dominio_inicial,2))

      integer :: i,j

      call cambio_dominio_a_character(mostrar_dominio_inicial,dominio_inicial_char)

      do i = 1, size(mostrar_dominio_inicial,2)

               write(*,fmt='(*(A,2X))') (dominio_inicial_char(i,j), j = 1, size(mostrar_dominio_inicial,2))

      enddo

   end subroutine 

   subroutine cambio_dominio_a_character(dominio,dominio_char)
         type(cell), intent(in) :: dominio(:,:)
         character(len=1), intent(out) :: dominio_Char(:,:)

         integer :: i, j

      !CAMBIA LA MATRIZ DE NUMEROS A CHARACTER
      do i=1,size(dominio,2)
         do j=1,size(dominio,2)
            if (dominio(i,j)%estado==0) then
               dominio_char(i,j)=' '

            elseif (dominio(i,j)%estado==1) then
               dominio_char(i,j)='#'

            else
               dominio_char(i,j)='X'
            
            endif
         enddo
      enddo
   end subroutine
   
   subroutine Evolucion(dominio, comprobacion_fin_virus)

      type(cell), intent(inout) :: dominio(:,:)

      integer, intent(out) :: comprobacion_fin_virus

      integer :: i, j, A, B, numero_izq, numero_der
      integer :: fila_arriba(3), fila_abajo(3), columna_izq(3), columna_der(3)
      character(len=1) :: dominio_char(size(dominio,1),size(dominio,2))

      do i = 1, size(dominio,2)

         do j = 1, size(dominio,1)

            if (i/=1 .and. j/=1 .and. i/=size(dominio,2).and. j/=size(dominio,1)) then

               fila_arriba = (/dominio(i-1,j-1)%estado,dominio(i-1,j)%estado,dominio(i-1,j+1)%estado/)
               fila_abajo = (/dominio(i+1,j-1)%estado,dominio(i+1,j)%estado,dominio(i+1,j+1)%estado/)
               numero_izq = dominio(i,j-1)%estado
               numero_der = dominio(i,j+1)%estado

                A = sum(fila_arriba,fila_arriba==1) + sum(fila_abajo,fila_abajo==1) + numero_izq + numero_der

                if (dominio(i,j)%estado==0) then

                    if (A==3) then

                       dominio(i,j)%estado = 1

                    else 

                       dominio(i,j)%estado = 0

                    endif

                else if (dominio(i,j)%estado==1) then

                  if (A==3 .or. A==2) then
  
                     dominio(i,j)%estado = 1

                  else 

                     dominio(i,j)%estado = 0

                  endif

                endif
                
            else if (i==1 .and. j/=1 .and. j/=size(dominio,1)) then

      fila_arriba = (/dominio(size(dominio,2),j-1)%estado,dominio(size(dominio,2),j)%estado,dominio(size(dominio,2),j+1)%estado/)
               fila_abajo = (/dominio(i+1,j-1)%estado,dominio(i+1,j)%estado,dominio(i+1,j+1)%estado/)
               numero_izq = dominio(i,j-1)%estado
               numero_der = dominio(i,j+1)%estado

                A = sum(fila_arriba,fila_arriba==1) + sum(fila_abajo,fila_abajo==1) + numero_izq + numero_der

                  if (dominio(i,j)%estado==0) then
  
                     if (A==3) then

                        dominio(i,j)%estado = 1
 
                     else 
 
                        dominio(i,j)%estado = 0
 
                     endif

                  else if (dominio(i,j)%estado==1) then
                     
                     if (A==3 .or. A==2) then
  
                        dominio(i,j)%estado = 1
 
                     else 
 
                        dominio(i,j)%estado = 0
 
                     endif

                  endif
  
            else if (j==1 .and. i/=1 .and. i/=size(dominio,2)) then

               fila_arriba = (/dominio(i-1,j)%estado,dominio(i-1,j+1)%estado,0/)
               fila_abajo = (/dominio(i+1,j)%estado,dominio(i+1,j+1)%estado,0/)
      columna_izq =  (/dominio(i-1,size(dominio,1))%estado,dominio(i,size(dominio,1))%estado,dominio(i+1,size(dominio,1))%estado/)
               numero_der = dominio(i,j+1)%estado

                  A = sum(fila_arriba,fila_arriba==1) + sum(fila_abajo,fila_abajo==1) + sum(columna_izq,columna_izq==1) + numero_der

                  if (dominio(i,j)%estado==0) then
  
                     if (A==3) then

                        dominio(i,j)%estado = 1
 
                     else 
 
                        dominio(i,j)%estado = 0
 
                     endif

                  else if (dominio(i,j)%estado==1) then
                     
                     if (A==3 .or. A==2) then
  
                        dominio(i,j)%estado = 1
 
                     else 
 
                        dominio(i,j)%estado = 0
 
                     endif

                  endif

            else if (i==size(dominio,2) .and. j/=1 .and. j/=size(dominio,1)) then

               fila_arriba = (/dominio(i-1,j-1)%estado,dominio(i-1,j)%estado,dominio(i-1,j+1)%estado/)
               fila_abajo =  (/dominio(1,j-1)%estado,dominio(1,j)%estado,dominio(1,j+1)%estado/)
               numero_izq =  dominio(i,j-1)%estado
               numero_der =  dominio(i,j+1)%estado

                  A = sum(fila_arriba,fila_arriba==1) + sum(fila_abajo,fila_abajo==1) + numero_izq + numero_der
  
                  if (dominio(i,j)%estado==0) then
  
                     if (A==3) then

                        dominio(i,j)%estado = 1
 
                     else 
 
                        dominio(i,j)%estado = 0
 
                     endif

                  else if (dominio(i,j)%estado==1) then
                     
                     if (A==3 .or. A==2) then
  
                        dominio(i,j)%estado = 1
 
                     else 
 
                        dominio(i,j)%estado = 0
 
                     endif

                  endif

            else if (j==size(dominio,1) .and. i/=1 .and. i/=size(dominio,2)) then

               fila_arriba = (/dominio(i-1,j-1)%estado,dominio(i-1,j)%estado,0/)
               fila_abajo = (/dominio(i+1,j-1)%estado,dominio(i+1,j)%estado,0/)
               columna_der = (/dominio(i-1,1)%estado,dominio(i,1)%estado,dominio(i+1,1)%estado/)
               numero_izq =  dominio(i,j-1)%estado

                  A = sum(fila_arriba,fila_arriba==1) + sum(fila_abajo,fila_abajo==1) + sum(columna_der,columna_der==1) + numero_der
  
                  if (dominio(i,j)%estado==0) then
  
                     if (A==3) then

                        dominio(i,j)%estado = 1
 
                     else 
 
                        dominio(i,j)%estado = 0
 
                     endif

                  else if (dominio(i,j)%estado==1) then
                     
                     if (A==3 .or. A==2) then
  
                        dominio(i,j)%estado = 1
 
                     else 
 
                        dominio(i,j)%estado = 0
 
                     endif

                  endif


            else if (i==1 .and. j==1) then

               A = dominio(1,2)%estado+dominio(2,2)%estado+dominio(2,1)%estado
  
                  if (dominio(i,j)%estado==0) then
  
                     if (A==3) then

                        dominio(i,j)%estado = 1
 
                     else 
 
                        dominio(i,j)%estado = 0
 
                     endif

                  else if (dominio(i,j)%estado==1) then
                     
                     if (A==3 .or. A==2) then
  
                        dominio(i,j)%estado = 1
 
                     else 
 
                        dominio(i,j)%estado = 0
 
                     endif

                  endif

            else if (i==1 .and. j==size(dominio,1)) then

               A = dominio(1,size(dominio,1)-1)%estado+dominio(2,size(dominio,1)-1)%estado+dominio(2,size(dominio,1))%estado
  
                 if (dominio(i,j)%estado==0) then

                    if (A==3) then

                       dominio(i,j)%estado = 1

                    else 

                       dominio(i,j)%estado = 0

                    endif

                 else if (dominio(i,j)%estado==1) then
                  
                    if (A==3 .or. A==2) then

                       dominio(i,j)%estado = 1

                    else 

                       dominio(i,j)%estado = 0

                    endif

                 endif

            else if (i==size(dominio,2) .and. j==1) then

               A = dominio(size(dominio,2)-1,1)%estado+dominio(size(dominio,1)-1,2)%estado+dominio(size(dominio,1),2)%estado
  
                 if (dominio(i,j)%estado==0) then

                    if (A==3) then

                       dominio(i,j)%estado = 1

                    else 

                       dominio(i,j)%estado = 0

                    endif

                 else if (dominio(i,j)%estado==1) then
                  
                    if (A==3 .or. A==2) then

                       dominio(i,j)%estado = 1

                    else 

                       dominio(i,j)%estado = 0

                    endif

                 endif

            else if (i==size(dominio,2) .and. j==size(dominio,1)) then

               A = dominio(size(dominio,2),size(dominio,2)-1)%estado+dominio(size(dominio,2)-1,size(dominio,2)-1)%estado
               B = dominio(size(dominio,1)-1,size(dominio,1))%estado
  
               if (dominio(i,j)%estado==0) then

                  if (A+B==3) then

                     dominio(i,j)%estado = 1

                  else 

                     dominio(i,j)%estado = 0

                  endif

               else if (dominio(i,j)%estado==1) then
                
                  if (A+B==3 .or. A+B==2) then

                     dominio(i,j)%estado = 1

                  else 

                     dominio(i,j)%estado = 0

                  endif

               endif

            endif

         enddo

      enddo

      comprobacion_fin_virus = 0

      call cambio_dominio_a_character(dominio,dominio_char)

      !Escirbe la matriz
      do i = 1, size(dominio,2)

         write(*,fmt='(*(A,2X))') (dominio_char(i,j), j = 1, size(dominio,2))

      enddo 

      do i = 1, size(dominio,2)

         do j = 1, size(dominio,1)

           if (dominio(i,j)%estado==1) then

              comprobacion_fin_virus = 10

           endif

         enddo

      enddo 

      if (comprobacion_fin_virus==0) then

         write(*,*) "El virus ha dejado de extenderse"
   
      endif

   end subroutine 

   subroutine espera(x)
     ! Esta subrutina detiene la ejecucion del programa hasta que pasen x segundos
     ! NO ES NECESARIO MODIFICAR ESTA SUBRUTINA

      implicit none
     ! Variable global
      real, intent(in) :: x
     ! Variable local
      real  :: t,t1

      call cpu_time(t)
      do
         call cpu_time(t1)    
         if (t1-t > x) exit
      enddo

   end subroutine espera

   subroutine porcentaje_grafica(dominio,dimension,tiempo)
      implicit none

      type(cell), intent(in) :: dominio(:,:)
      integer, intent(in) :: dimension, tiempo

      real :: n_infectada                          !numero de celulas sanas e infectadas
      real :: cell_total                           !numero de celulas totales
      real :: porcentaje_inf                       !porcentajes de infectadas
      integer :: info

         cell_total=real(dimension*dimension)
         n_infectada = COUNT(dominio%estado==1)     !cuenta el numero de celulas infectadas
         
         !CALCULO DE PORCENTAJE
         porcentaje_inf = (n_infectada/cell_total)*100. 

         open(unit=10, file='Porcentajes_Infectados.dat', status='unknown',action='write', iostat=info)  !crea un archivo donde guarda los porcentajes para representarlos con gnu plot y luego lo destruye al usarlo
            if(info>0) STOP 'Error al crear archivo'

            write(10,*) Tiempo, Porcentaje_inf                        !escribe los datos en el fichero
 
      end subroutine

   subroutine Estadisitica_Celulas(Dominio, t_inf)
      implicit none

      type(cell), intent(inout) :: dominio(:,:)
      integer, intent(inout)       :: t_inf(:,:)

      integer :: i,j                                                                   !recorre filas y columnas en los loop
      character(len=40) :: format                                                      !formato para escribir las estadisiticas
      

      !Incializar variables
      format='(I3,A,1X,I3,A,1X,I3,A,1X,I3)'

      write(11,*) 'ninfectado',',','nsano',',','tmin',',','tmax'                       !cabeceras del archivo separada por comas

      do i=1,size(dominio,1)                                                           !recorremos las filas y columnas
         do j=1,size(dominio,1)

            if (dominio(i,j)%estado==1) then                                           !si esta infectado

               dominio(i,j)%tmin = 1                                                   !el tiempo minimo es al menos 1 al haberse infectado
               t_inf(i,j)=t_inf(i,j)+1                                                 !el valor de el tiempo para esa celula infectada va incrementando hasta que deja de recupera y entonces se resetea a 0 por la otra condicion

               dominio(i,j)%tmax=max(t_inf(i,j), dominio(i,j)%tmax)                    !escoge el valor maximo entre el tiempo infectado en ese momento y el maximo guardado anterior
               
               if(dominio(i,j)%comprobar_Cambio .eqv. .true.) then                     !comrpueba si se ha cambiado recientemente a infectado y lo suma como una infeccion

                  dominio(i,j)%ninfectado=dominio(i,j)%ninfectado+1                    !al infectarse, le suma una unidad
                  dominio(i,j)%comprobar_Cambio = .false.                              !pone el valor paa cuando entre en el loop de recuperado que vea que es reciente esa recuperacion

               endif

            else if(dominio(i,j)%estado==0) then

               t_inf(i,j)=0                                                            !al no estar infectada, el tiempo estando infectado permanece o se resetea a 0
               
               if(dominio(i,j)%comprobar_Cambio .eqv. .false.) then                    !comprueba si se ha curado recientemente y lo suma al contador

                  dominio(i,j)%nsano=dominio(i,j)%nsano+1                              !le suma uno al contador de recuperado
                  dominio(i,j)%comprobar_Cambio = .true.

               endif
            endif

            

            write(11,fmt=format) dominio(i,j)%ninfectado, ',', dominio(i,j)%nsano, ',', dominio(i,j)%tmin, ',',dominio(i,j)%tmax    !escribe los datos de esta unidad temporal en el txt

         enddo
      enddo
      
      rewind(11)     !vuelva al principio y rescribe los datos con los datos nuevos que salgan de la siguiente unidad temporal

   end subroutine


end module