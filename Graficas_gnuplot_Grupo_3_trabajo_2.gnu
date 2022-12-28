###### GRAFICA DE LINEAS DEL NUMERO DE INFECTADOS POR UNIDAD DE TIEMPO ########
set style histogram 

set lmargin 6
set rmargin 6

set title 'Evolucion del numero de infectados y sanos'
set ylabel 'Porcentaje de Infectados'
set xlabel 'Tiempo transcurrido'
set yrange [0:100]

plot 'Porcentajes_Infectados.dat' with linespoints title 'Porcentaje de infectados'
pause mouse

reset #Resetea los ajustes puesto para esta grafica


######### GRAFICAS PARA ESTADISTICAS DE LAS CELULAS(tmin,tmax, ninf, nsan) ##############
set style histogram                       #Estilo de un histograma
set key off                               #Quita la llave de informacion
set border 3
set xrange [0:]                           #Pone como minimo 0 y el maximo esta indeterminado en el rango de ese eje
set format x "%.0f"                       #Quita los decimales en el eje X
set xtics 1                               #Hace que los labels del eje X aumenten en incrementos de 1

#PROPIEDADES DE LAS BARRAS DEL DIAGRAMA
set boxwidth 1                                                          #El rectangulo tiene una anchura de 1 unidades en el eje X
set style fill solid border lc rgb 'black'                              #Llena el rectangulo de un color y le pone borde negro para ditinguir rectangulos

#BIN O DONDE AGRUPAMOS LOS VALORES CON UN VALOR SIMILAR
bin_width = 1                                                           #Redondea los numeros al numero entero mas cercano, los mete en esa agrupacion llamada bin
bin_number(x) = floor(x/bin_width)                                      #Junta los valores que tengan el mismo valor en ese bin/agrupacion de numeros

centrar(x) = bin_width * ( bin_number(x) + 0.5 )                        #Centra las barras en su caja ya que si no para el centro de la caja se centra en el valor de la agrupacion den numeros

############ GRAFICA COLUMNA 1 ##################

#TITULOS PARA LOS EJES y GRAFICA
set title 'Frecuencia de numero de veces que una celula se infecta' 
set ylabel 'Numero de celulas'
set xlabel 'Numero de veces infectado'


plot 'Estadisticas_Celulas.dat' using (centrar($1)):(1) smooth frequency with boxes #Para la columna 1, hace un diagrama de cajas segun la frecuencia de esos valores
pause mouse                                                                          #Crea una pausa que acaba cuando el usuario le da con el raton a la grafica   

############## GRAFICA COLUMNA 2 ################

#TITULOS PARA LOS EJES y GRAFICA
set title 'Frecuencia de numero de veces que una celula se cura' 
set ylabel 'Numero de celulas'
set xlabel 'Numero de veces curada'

plot 'Estadisticas_Celulas.dat' using (centrar($2)):(2) smooth frequency with boxes #Para la columna 2, hace un diagrama de cajas segun la frecuencia de esos valores
pause mouse                                                                         

############ GRAFICA COLUMNA 3 ##################

#TITULOS PARA LOS EJES y GRAFICA
set title 'Frecuencia del tiempo minimo que una celula esta infectada' 
set ylabel 'Numero de celulas'
set xlabel 'Tiempo minimo infectado'                      

plot 'Estadisticas_Celulas.dat' using (centrar($3)):(3) smooth frequency with boxes #Para la columna 1, hace un diagrama de cajas segun la frecuencia de esos valores
pause mouse

############ GRAFICA COLUMNA 4 ##################                          

#TITULOS PARA LOS EJES y GRAFICA
set title 'Frecuencia del tiempo maximo que una celula esta infectada' 
set ylabel 'Numero de celulas'
set xlabel 'Tiempo maximo infectado'

plot 'Estadisticas_Celulas.dat' using (centrar($4)):(4) smooth frequency with boxes #Para la columna 4, hace un diagrama de cajas segun la frecuencia de esos valores
pause mouse                                                                          


