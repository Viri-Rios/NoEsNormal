Instrucciones para el código del capítulo ‘Vender caro’
================

1.  Descargar la carpeta “1\_VenderCaro” sin cambiar la estructura de
    las carpetas. Cambiar solamente el directorio raíz.

2.  Descargar las [bases de datos de la
    ENIGH](https://www.inegi.org.mx/programas/enigh/nc/2020/) en y
    colocarlas en la carpeta “Bases”.
    
      - Para el las ENIGH 2016 a 2020, descargar las bases de
        principales características de los hogares, gastos del hogar y
        gastos de los integrantes del hogar. Guardar las bases en el
        siguiente formato (XXXX representa el año).
        
          - Principales características: concentradohogarXXXX.csv
          - Gastos del hogar: gastoshogarXXXX.csv
          - Gastos de los integrantes del hogar: gastospersonaXXXX.csv
    
      - Para las ENIGH 2012 y 2014 descargar las bases de la versión
        “Nueva construcción” y repetir lo indicado para las bases de
        2016 a 2020.
    
      - Para las ENIGH 2008 y 2010 descargar las de la versión “Nueva
        construcción” y guardarlas en la carpeta Bases en el siguiente
        formato (XXXX representa el año).
        
          - Principales características: concentradohogarXXXX.csv
          - Gastos del hogar: gastoshogarXXXX.csv
          - Gastos ddiarios en alimentos, bebidas y transporte:
            gastospersonaXXXX.csv
    
      - Para las ENIGH 2002 a 2006 las de la versión “Tradicional” y
        guardarlas en la carpeta Bases en el siguiente formato (XXXX
        representa el año).
    
      - Principales características: concentradohogarXXXX.csv
    
      - Gastos del hogar: gastoshogarXXXX.csv
    
      - Las bases de estos años no están disponibles en formato CSV, por
        lo que hay que convertirlas. El siguiente código es un ejemplo
        de cómo puede hacerse a partir de las bases en formato DBF:

<!-- end list -->

``` r
install.packages("foreign")
library(foreign)
x <- read.dbf("concentradohogar2002.dbf")
write.csv(x,"concentradohogar2002.csv")
```

3.  Correr el código “01.Competir\_VenderCaro\_VF”

4.  Los resultados aparecerán en la carpeta “output”
