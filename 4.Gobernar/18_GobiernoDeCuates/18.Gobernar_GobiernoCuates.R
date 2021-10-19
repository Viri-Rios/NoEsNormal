require(readr)
require(tidyverse)
require(stringi)
require(Hmisc)
require(tidymodels)
require(dotwhisker)
require(estimatr)
require(openxlsx)
require(broom.mixed)

# Set-up ----

# Aquí se eligen los folders donde están las bases
dir_input <- "4.Gobernar/18_Gobierno de Cuates/Bases/"
# Aquí se eligen los folders donde se van a guardar los resultados
dir_output <- "4.Gobernar/18_Gobierno de Cuates/output/"
# Aquí se eligen el nombre del excel de exportación
nombre_para_guardar_excel <- "compranet.xlsx"
# Cuales años se incluyen en el estudio
añosincluidos <- seq(2020, 2010)
# Elegir si se importan los archivos de nuevo o se trabaja desde memoria (por lo menos correr la primera vez)
extraer_datos_denuevo <- F

if(!dir.exists(dir_output)){
  dir.create(dir_output)
}

# Datos externos ----

tipos_cambio <- tribble(
  ~moneda, ~tipo_de_cambio, ~año,
  "USD", 12.9225971711772 , 2010,
  "USD", 12.2618984413516 , 2011,
  "USD", 13.7144093922885 , 2012,
  "USD", 12.75969016381   , 2013,
  "USD", 13.0905070174946 , 2014,
  "USD", 14.9469157572888 , 2015,
  "USD", 17.3529093076173 , 2016,
  "USD", 21.3798981758172 , 2017,
  "USD", 19.2427164726957 , 2018,
  "USD", 19.490221589754  , 2019,
  "USD", 18.8672902164877 , 2020,
  "CAD", 12.4519231742753 , 2010,
  "CAD", 12.2790966665403 , 2011,
  "CAD", 13.5317369821361 , 2012,
  "CAD", 12.9461094388743 , 2013,
  "CAD", 12.2811773818745 , 2014,
  "CAD", 12.6808453196946 , 2015,
  "CAD", 12.4224365225557 , 2016,
  "CAD", 16.0835794896185 , 2017,
  "CAD", 15.3757157143956 , 2018,
  "CAD", 14.5340922951819 , 2019,
  "CAD", 14.5267215161173 , 2020,
  "EUR", 18.5943160303334 , 2010,
  "EUR", 16.4566915696462 , 2011,
  "EUR", 17.757422188437  , 2012,
  "EUR", 16.6029119620818 , 2013,
  "EUR", 17.8057135288947 , 2014,
  "EUR", 17.8092438361213 , 2015,
  "EUR", 18.9112098337367 , 2016,
  "EUR", 22.3141937868339 , 2017,
  "EUR", 23.2163463123537 , 2018,
  "EUR", 22.2247069187504 , 2019,
  "EUR", 21.0313590484654 , 2020,
  "JPY", 0.138877988032682, 2010,
  "JPY", 0.149353217793621, 2011,
  "JPY", 0.178899235609612, 2012,
  "JPY", 0.145575533759186, 2013,
  "JPY", 0.125040636049494, 2014,
  "JPY", 0.12425740983178 , 2015,
  "JPY", 0.145200515283948, 2016,
  "JPY", 0.181293172851851, 2017,
  "JPY", 0.170742804391547, 2018,
  "JPY", 0.181085315443518, 2019,
  "JPY", 0.172903989137828, 2020,
  "GBP", 20.9642701657403 , 2010,
  "GBP", 19.1383821912094 , 2011,
  "GBP", 21.3889944676718 , 2012,
  "GBP", 20.4372418274965 , 2013,
  "GBP", 21.4395797128787 , 2014,
  "GBP", 22.7999768593412 , 2015,
  "GBP", 25.6303198853854 , 2016,
  "GBP", 26.2619866889377 , 2017,
  "GBP", 26.0825283263548 , 2018,
  "GBP", 24.6863183573897 , 2019,
  "GBP", 24.6350305042518 , 2020,
  "MXN", 1                , 2010,
  "MXN", 1                , 2011,
  "MXN", 1                , 2012,
  "MXN", 1                , 2013,
  "MXN", 1                , 2014,
  "MXN", 1                , 2015,
  "MXN", 1                , 2016,
  "MXN", 1                , 2017,
  "MXN", 1                , 2018,
  "MXN", 1                , 2019,
  "MXN", 1                , 2020,
)

inpc <- tribble(
  ~inpc,             ~año, 
  68.1579057934935 , 2010,
  70.7356631469182 , 2011,
  73.5981751106184 , 2012,
  75.9934822211993 , 2013,
  79.4001255366521 , 2014,
  81.8342487529005 , 2015,
  83.9726637575507 , 2016,
  87.9347303728616 , 2017,
  92.8114457899236 , 2018,
  96.8632277095644 , 2019,
  100              , 2020,
)

CLAVEEST <-
  c("AGS","BC","BCS","CAMP","CDMX","CHIH","CHIS","COAH","COL","DGO","GRO",
    "GTO","HGO","JAL","MEX","MICH","MOR","NAY","NL","OAX","PUE","Q_ROO","QRO",
    "SIN","SLP","SON","TAB","TAMPS","TLAX","VER","YUC","ZAC"
  )

años_edos_gob <- openxlsx::read.xlsx(paste0(dir_output, "gobiernos.xlsx")) %>% 
  #la mayoría de los primeros periodos no hubo cambio de partido, excepto en estos
  bind_rows(tribble(~año,~estado,~partido,~periodo_absoluto,~gobernador,~clave_gob,
                    2005, "NAY", "PAN", 0,"Quitar","1Quitar4",
                    2009, "QRO", "PAN", 0,"Quitar","1Quitar4",
                    2009, "SLP", "PAN", 0,"Quitar","1Quitar4",
                    2009, "SON", "PRI", 0,"Quitar","1Quitar4",))


cambios_gobierno_partido <- años_edos_gob %>% 
  group_by(estado) %>% 
  arrange(estado, periodo_absoluto) %>% 
  mutate(
    cambio_gob = ifelse(periodo_de_gobernador==1,1,0),
    cambio_gob = ifelse(gobernador=="Quitar",NA,cambio_gob),) %>% 
  arrange(periodo_absoluto) %>% 
  distinct(estado, clave_gob, gobernador, partido, periodo_absoluto, .keep_all = T) %>% 
  group_by(estado) %>% 
  mutate(cambio_partido = ifelse(partido == lag(partido), 0, 1),
         #cambio_partido = ifelse(gobernador == lag(gobernador), 0, cambio_partido),
         cambio_partido = ifelse(cambio_gob == 0, 0, cambio_partido),
         cambio_partido = ifelse(is.na(cambio_partido), 0, cambio_partido),
  ) %>% 
  select(-año#, -min_per
         ) %>% 
  arrange(estado, periodo_absoluto) 

# Importar archivos ----

if(extraer_datos_denuevo){
  todos_archivos <- tibble()
  for (i in añosincluidos) {
    print(paste0("doing for: " , i))
    temp <- NULL
    try(temp <- read.csv(paste0(dir_input, "Contratos", i, ".csv"), nrows = 10), silent = T)
    if(!is.null(temp)){
      temp<-temp %>% 
        rename_all(tolower)%>% 
        rename_all(~stri_trans_general(., "latin-ascii")) %>% 
        rename_all(~stri_replace_all(., fixed = ".", "_")) %>% 
        rename_all(~stri_replace_all(., fixed = "de_la_", "")) %>% 
        rename_all(~stri_replace_all(., fixed = "_del_", "_")) %>% 
        rename_all(~stri_replace_all(., fixed = "_de_", "_")) %>% 
        rename_all(~stri_replace_all(., fixed = "_o_", "_"))  %>% 
        rename_all(~stri_replace_all(., fixed = "en_el_", ""))%>% 
        rename_all(~stri_replace_all(., fixed = "_uc", "uc"))%>% 
        rename_all(~stri_replace_all(., fixed = "_institucion", "")) %>% 
        rename_all(~stri_replace_all(., fixed = "orden_", ""))
    }
    tox <- F
    try(tox <- any(stri_detect(unique(temp$tipo_procedimiento), regex = "(á|é|í|ó|ú)", case_insensitive=T)), silent = T)
    if(!tox){
      encod <- "latin1"
    } else {
      encod <- "utf8"
    }
    temp <- read.csv(paste0(dir_input, "Contratos", i, ".csv"), encoding = encod #, nrows = 100
    ) %>% 
      rename_all(tolower)%>% 
      rename_all(~stri_trans_general(., "latin-ascii")) %>% 
      rename_all(~stri_replace_all(., fixed = ".", "_")) %>% 
      rename_all(~stri_replace_all(., fixed = "de_la_", "")) %>% 
      rename_all(~stri_replace_all(., fixed = "_del_", "_")) %>% 
      rename_all(~stri_replace_all(., fixed = "_de_", "_")) %>% 
      rename_all(~stri_replace_all(., fixed = "_o_", "_"))  %>% 
      rename_all(~stri_replace_all(., fixed = "en_el_", ""))%>% 
      rename_all(~stri_replace_all(., fixed = "_uc", "uc"))%>% 
      rename_all(~stri_replace_all(., fixed = "_institucion", "")) %>% 
      rename_all(~stri_replace_all(., fixed = "orden_", "")) %>% 
      mutate(across(everything() , ~as.character(.)))
    
    if(i %in% 2018:2020){
      temp <- temp %>% 
        rename(
          dependencia = institucion,
          tipo_contratacion = tipo_contratacion,
          tipo_procedimiento = tipo_procedimiento,
          moneda = moneda_contrato,
          estratificacion_mpc = estratificacion_empresa,
          c_externo = credito_externo,
          organismo = organismo_financiero,
        )
    }
    temp <- temp %>% 
      mutate(año = i) 
    todos_archivos <- bind_rows(todos_archivos,temp)
    
  }
  saveRDS(todos_archivos, paste0(dir_output, "todos_archivos_compranet.rds"))
} else {
  todos_archivos <- readRDS(paste0(dir_output, "todos_archivos_compranet.rds"))
}


# Producir montos deflactados y en moneda nacional ----

compranet <- todos_archivos %>% 
  # Nos quedamos con las columnas de interés
  select(año, gobierno, siglas, dependencia,
         tipo_contratacion, proveedor_contratista,
         tipo_procedimiento, importe_contrato, 
         moneda, estratificacion_mpc) %>% 
  # Transformamos el importe a número
  mutate(across(importe_contrato, ~as.numeric(.))) %>% 
  # Eliminamos proveedores vacíos
  mutate(proveedor_contratista = trimws(proveedor_contratista, which = "both")) %>% 
  filter(proveedor_contratista != "") %>% 
  # Unimos con base de datos de tipos de cambio
  left_join(tipos_cambio) %>% 
  # Unimos con base de datos de indice de precios
  left_join(inpc) %>% 
  # Todos los textos vacíos reemplazamos por NA
  mutate(across(everything(), ~ifelse(.=="", NA_character_, .)))  %>% 
  # Transformamos el monto a precios corrientes de 2020
  mutate(monto = ((importe_contrato*tipo_de_cambio)*100)/inpc,
         # Estandarizamos los tipos de procedimientos 
         tipo_procedimiento = case_when(
           stri_startswith_fixed(tipo_procedimiento, pattern = "adj",case_insensitive = T) ~ "adjudicacion directa",
           stri_startswith_fixed(tipo_procedimiento, pattern = "lic",case_insensitive = T) ~ "licitacion publica",
           stri_startswith_fixed(tipo_procedimiento, pattern = "inv",case_insensitive = T) ~ "invitación a cuando menos 3 personas",
           stri_startswith_fixed(tipo_procedimiento, pattern = "otr",case_insensitive = T) ~ "otras contrataciones",
           stri_startswith_fixed(tipo_procedimiento, pattern = "pro",case_insensitive = T) ~ "proyecto de convocatoria",
           stri_startswith_fixed(tipo_procedimiento, pattern = "ser",case_insensitive = T) ~ "servicios",
           T ~ tipo_procedimiento
         )
  )  %>% 
  # Cambiamos a mayúsculas y con guiones los tipos de procedimientos y los tipos de contrataciones
  mutate(across(c(tipo_procedimiento,tipo_contratacion), toupper))%>% 
  mutate(across(c(tipo_procedimiento,tipo_contratacion), ~stri_replace_all(., regex = "[:space:]+", "_")))

#write.csv(compranet, paste0(dir_output, "out_1.csv"))

# Compranet2 Encontrar 50 empresas más importantes, sumar por periodo de gob y encontrar periodos continuos ----

compra_solo_edos <- compranet %>% 
  # Nos quedamos solo con contratos estatales
  filter(gobierno == "GE") %>% 
  # Estandarizamos las siglas de los estados
  mutate(across(siglas, ~stri_replace_all(., fixed = " ", "_"))) %>% 
  # Nos quedamos solo con contratos tipo ARRENDAMIENTOS
  filter(tipo_contratacion != "ARRENDAMIENTOS") %>% 
  # Unimos con la base de gobernadores y partidos para identificar periodos de gobernantes
  left_join(años_edos_gob %>% rename(siglas=estado))

#write.csv(compra_solo_edos, paste0(dir_output, "out_2.csv"))

proveedores_limpios <- compra_solo_edos %>% 
  mutate(
    # Limpieza de proveedores ----
    # Volver a quitar dobles espacios, y posibles espacios al final o inicio
    proveedor_contratista = tolower(proveedor_contratista),
    proveedor_contratista = stri_trans_general(proveedor_contratista, "latin-ascii"),
    proveedor_contratista = trimws(proveedor_contratista, which="both"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = "[:punct:]+", ""),
    #quitar dobles espacios
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = "[:blank:]+", " "),
    
    # Revisar errores de dedo encontrados
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "integral j m", "integral jm"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "humanosac", "humanos ac"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "contrucciones", "construcciones"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "consteucciones", "construcciones"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "iztel", "itzel"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "factible pive", "factibles pive"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "monte sinahi", "monte sinai"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "consorcio para el dialogo parlamentario y la equidad oaxaca", "consorcio para el dialogo parlamentario y la equidad"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "consejo para la evaluacion de la educacion del tipo medio superior", "consejo para la evaluacion de la educacion tipo medio superior"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "enseanza", "ensenanza"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "ensenansa", "ensenanza"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "catplus", "cat plus"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "ecohoru", "eco horu"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "computadoras ambientes virtuale y algo mas", "computadoras ambientes virtuales y algo mas"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "centro de investigacion y tratamiento de las adicciones cita", "centro de investigacion y tratamiento de adicciones cita"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "comites interinstitucionales para la evaluacion de la educacion superior", "comites interinstitucionales para la evaluacion de la educacion"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "equipos", "equipo"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "carlderon", "calderon"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "carrazco melendez merced liliana", "carrazco melendez mercedes liliana"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "camara nacional de comercio servicios y turismo de xalapa veracruz", "camara nacional de comercio servicios y turismo de xalapa"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "bullet internacional", "bullet international"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "bruce medica internacional", "bruce medical internacional"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "aparatos electromecanicos von haucke", "aparatos electromecanico von haucke"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "biosistems", "biosystems"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "consutoria", "consultoria"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "laboratorios", "laboratorio"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "interaccionesgrupo", "interacciones grupo"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "artes graficas y edificaciones en morelia", "artes graficas y edificaciones de morelia"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "ya lem chem", "yalem chem"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "alta tecnologia en laboratorios", "alta tecnologia en laboratorio"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "pilitika", "politika"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "alianzas comerciales casas", "alianzas comerciales casa"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "f hinos", "f hino"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "technologies de mexico", "technologies mexico"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "ag disenos", "ag diseno"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "afianzadora aserta  grupo financiero aserta", "afianzadora aserta"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "acmax de maxico", "acmax de mexico"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = " and ", " y "),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "accesorios para laboratorio", "accesorios para laboratorios"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "absurestel", "absuresten"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "absuresten", "absuresten diagnostik"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "atr e instrumentos", "atr instrumentos"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = "[:blank:]+", " "),
    # fin errores de dedo
    
    #limpieza de abreviaciones
    # Quitar siglas tipo empresa
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = " ste", " sureste"),
    proveedor_contratista = stri_replace_all(proveedor_contratista, fixed = "sociedad cooperativa de produccion de responsabilidad limitada de capital variable", " sc de p de rl de cv"),
    
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s a b de c v$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " sab de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s de rl mi de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s de rl$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s de r l$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s a p i de c v$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s de pr de rl$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " sas de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s de rlde cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " sa de rl de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " sc de rl$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " sc de rl de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s de rl mi$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " sab de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s a de c v$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " sa de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " sa de v$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " sa decv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s de rl de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s de rl$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s de r l de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s de r l de c v$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s a p i de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s rl de ip de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " spr de rl$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " sapi de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s de rlmi$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s c de p de rl de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " sc de p de rl de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = "ac$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " a c$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " sa$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = "sa de cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " sade cv$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " sc$", ""),
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = " s c$", ""),
    # fin abr
    
    # Quitar espacios (provar diferencias)
    # proveedor_contratista = stri_replace_all(proveedor_contratista, regex = "[:blank:]+", ""),
    
    proveedor_contratista = stri_replace_all(proveedor_contratista, regex = "[:blank:]+", " "),
    proveedor_contratista = trimws(proveedor_contratista, which="both"),
    proveedor_contratista = ifelse(proveedor_contratista == "", NA, proveedor_contratista),
    # fin limpieza ----
  ) %>% 
  # Eliminamos los proveedores vacíos o NA (De nuevo)
  filter(!is.na(proveedor_contratista) ) 


suma_proveedor_edo <- proveedores_limpios %>% 
  # Controlando por estado, periodo y proveedor sumamos los montos ya actualizados y en moneda nacional
  group_by(siglas, periodo_absoluto, clave_gob, gobernador, periodo_de_gobernador, proveedor_contratista) %>% 
  summarise(monto = sum(monto)) %>% 
  # Controlamos por estado y periodo
  group_by(siglas, periodo_absoluto, clave_gob, gobernador, periodo_de_gobernador) %>% 
  # Es necesario ordenar por monto para el ranking
  arrange(desc(monto)) %>% 
  # Asignamos el orden dado el monto por proveedor
  mutate(ranking = sort.list(monto, decreasing = T),
         # Si está en los primeros 50 guardamos como 1 la nueva columna
         top50 = ifelse(ranking<=50, 1, 0), 
         # Revisamos para cada grupo el número de proveedores
         num_proovedores = max(ranking, na.rm = T))

write.csv(suma_proveedor_edo, paste0(dir_output, "provedores_unicos.csv"))

# Tomamos los provedores rankeados
proveedores_continuos <- suma_proveedor_edo %>% 
  # Eliminamos periodos indeseados
  filter(gobernador != "Quitar") %>% 
  #Solo nos quedamos con las empresas top 50
  filter(top50 == 1 ) %>% 
  # Controlamos por estado y proveedor
  group_by(siglas, proveedor_contratista ) %>%
  # Ordenamos para el lag
  arrange(siglas, proveedor_contratista,periodo_absoluto) %>%
  # Aquí revisamos si la empresa fue top50 en periodos pasados
  mutate(periodo_continuo = ifelse(periodo_absoluto == (lag(periodo_absoluto)+1), 1, 0),
         # si es NA es que no fue top50 en el periodo pasado
         periodo_continuo = ifelse(is.na(periodo_continuo), 0, periodo_continuo),
         # Mantenemos solo periodos donde hubo al menos 50 proveedores
         periodo_continuo = ifelse(num_proovedores>=50, periodo_continuo , NA) 
  )

write.csv(proveedores_continuos, paste0(dir_output, "provedores_unicos_top50_periodo.csv"))

# Sumamos el número de empresas con periodos continuo por periodo -----

compranet3 <- proveedores_continuos%>%
  group_by(siglas,gobernador, periodo_de_gobernador ,periodo_absoluto) %>% 
  summarise(empresas_con_periodo_continuo = sum(periodo_continuo)) %>% 
  ungroup() %>% 
  rename(estado = siglas)  %>% 
  left_join(cambios_gobierno_partido )%>% 
  # Revisar los quitar porque no son consistentes en todo el proceso Ej. Aguascalientes.
  mutate(empresas_con_periodo_continuo = empresas_con_periodo_continuo/50) %>% 
  arrange(periodo_absoluto) %>% 
  group_by(estado) %>% 
  mutate(
    min_per = min(periodo_absoluto, na.rm = T)-1,
    periodo_absoluto = periodo_absoluto - min_per
  ) %>% 
  arrange(estado, periodo_absoluto) %>% 
  select(-clave_gob, -min_per)%>% 
  filter(periodo_absoluto != 1)

openxlsx::write.xlsx(compranet3, 
                     paste0(dir_output, nombre_para_guardar_excel), 
                     overwrite = T)

compranet3 %>% 
  group_by(cambio_gob, estado) %>% 
  summarise(empresas_con_periodo_continuo = mean(empresas_con_periodo_continuo, na.rm=T)) %>% 
  mutate(cambio_gob = ifelse(cambio_gob ==0 , "Sin cambio Gobernador", "Con cambio Gobernador")) %>% 
  pivot_wider(names_from = cambio_gob, values_from = empresas_con_periodo_continuo) %>% 
  openxlsx::write.xlsx(paste0(dir_output, "Promedios_por_Cambio_gob_estatales.xlsx"),
                       overwrite = T)


# Regresión ----

data_regresion <- compranet3  %>% 
  mutate(entidad = factor(estado))

#*  Modelo Linear
#*  lm_mod <- 
#*   linear_reg() %>% 
#*   set_engine("lm")
#* 
#* # Modelo Bayesiano
#*  prior_dist <- rstanarm::student_t(df = 1)
#*  set.seed(123)
#*  lm_mod <-   
#*  linear_reg() %>% 
#*  set_engine("stan", 
#*  prior_intercept = prior_dist, 
#*  prior = prior_dist, chains = 4, iter = 4000) 
#*  
#*  # esto es parejo para cualquier otro modelo
#*  lm_fit <- #lm_mod %>% 
#*  fit(empresas_con_periodo_continuo ~  cambio_gob + entidad, data = data_regresion)

lm_fit <- # es posible cambiar la formula de la regresión 
  lm_robust(empresas_con_periodo_continuo ~  cambio_gob + entidad, data = data_regresion, se_type = "HC1")

general_resultados <- tidy(lm_fit, conf.int = T) %>% 
  mutate(term = stri_replace_all(term, fixed= "entidad", "")) 

general_resultados %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "darkgrey"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))

openxlsx::write.xlsx(general_resultados, 
                     paste0(dir_output, "resultados_reg.xlsx"),
                     overwrite = T)

final_compranet <- data_regresion %>% 
  group_by(estado, cambio_gob) %>% 
  summarise(empresas_con_periodo_continuo = mean(empresas_con_periodo_continuo, na.rm=T)) %>% 
  mutate(cambio_gob = ifelse(cambio_gob==0, "Sin cambio de gobierno", "Con cambio de gobierno")) %>% 
  pivot_wider(names_from = cambio_gob, values_from = empresas_con_periodo_continuo) 
  
openxlsx::write.xlsx(final_compranet, 
                     paste0(dir_output, "compranet_reshape.xlsx"), overwrite = T)


# Control por tipo de procedimiento ----

# aquí encontramos el número de empresas más bajo por tipo de procedimiento (decil 1)
min_procs <- proveedores_limpios %>% 
  filter(!tipo_procedimiento %in% c("OTRAS_CONTRATACIONES","PROYECTO_DE_CONVOCATORIA")) %>% 
  filter(!is.na(tipo_procedimiento)) %>% 
  distinct(tipo_procedimiento, siglas,periodo_absoluto, 
           proveedor_contratista) %>% 
  count(tipo_procedimiento, siglas,periodo_absoluto) %>% 
  group_by(tipo_procedimiento) %>% 
  summarise(min_proc = floor(quantile(n, probs = .1)))
  

proveedores_tipo_proc <- proveedores_limpios %>% 
  filter(!tipo_procedimiento %in% c("OTRAS_CONTRATACIONES","PROYECTO_DE_CONVOCATORIA")) %>% 
  filter(!is.na(tipo_procedimiento)) %>% 
  # Controlando por estado, periodo, procedimiento y proveedor sumamos los montos ya actualizados y en moneda nacional
  group_by(siglas, periodo_absoluto, clave_gob, gobernador, 
           periodo_de_gobernador, tipo_procedimiento, proveedor_contratista) %>% 
  summarise(monto = sum(monto)) %>% 
  # Controlamos por estado y periodo
  group_by(siglas, periodo_absoluto, clave_gob, gobernador, 
           periodo_de_gobernador, tipo_procedimiento) %>% 
  # Es necesario ordenar por monto para el ranking
  arrange(desc(monto)) %>% 
  # Recuperamos el mínimo número de empresas
  left_join(min_procs) %>% 
  # Asignamos el orden dado el monto por proveedor
  mutate(ranking = sort.list(monto, decreasing = T),
         # Si está en los primeros 50 guardamos como 1 la nueva columna
         topN = ifelse(ranking<=min_proc, 1, 0), 
         # Revisamos para cada grupo el número de proveedores
         num_proovedores = max(ranking, na.rm = T))

toprint <- proveedores_tipo_proc %>%
  ungroup() %>% 
  group_split(tipo_procedimiento)
names(toprint) <- stri_sub(sort(unique(proveedores_tipo_proc$tipo_procedimiento)),
                           1, 31)
openxlsx::write.xlsx(toprint, 
                     paste0(dir_output, "provedores_tipo_proc.xlsx"), 
                     overwrite = T)

proveedores_continuos_tipo_proc <- proveedores_tipo_proc %>% 
  group_by(tipo_procedimiento) %>% 
  ungroup() %>% 
  # Eliminamos periodos indeseados
  filter(gobernador != "Quitar") %>% 
  #Solo nos quedamos con las empresas top 50
  filter(topN == 1) %>% 
  # Controlamos por estado y proveedor
  group_by(siglas, proveedor_contratista, tipo_procedimiento ) %>%
  # Ordenamos para el lag
  arrange(siglas, proveedor_contratista,periodo_absoluto) %>%
  # Aquí revisamos si la empresa fue top50 en periodos pasados
  mutate(periodo_continuo = ifelse(periodo_absoluto == (lag(periodo_absoluto)+1), 1, 0),
         # si es NA es que no fue top50 en el periodo pasado
         periodo_continuo = ifelse(is.na(periodo_continuo), 0, periodo_continuo),
         # Mantenemos solo periodos donde hubo al menos 50 proveedores
         periodo_continuo = ifelse(num_proovedores>=min_proc, periodo_continuo , NA) 
  )

toprint <- proveedores_continuos_tipo_proc %>%
  ungroup() %>% 
  group_split(tipo_procedimiento)
names(toprint) <- stri_sub(sort(unique(proveedores_continuos_tipo_proc$tipo_procedimiento)),
                           1, 31)
openxlsx::write.xlsx(toprint, 
          paste0(dir_output, "provedores_unicos_top50_periodo_tipo_proc.xlsx"), 
          overwrite = T)


compranet3_tipo_proc <- proveedores_continuos_tipo_proc%>%
  group_by(siglas,gobernador, periodo_de_gobernador ,periodo_absoluto, tipo_procedimiento) %>% 
  summarise(empresas_con_periodo_continuo = sum(periodo_continuo)) %>% 
  ungroup() %>% 
  rename(estado = siglas)  %>% 
  left_join(cambios_gobierno_partido )%>% 
  left_join(min_procs) %>% 
  # Revisar los quitar porque no son consistentes en todo el proceso Ej. Aguascalientes.
  mutate(empresas_con_periodo_continuo = empresas_con_periodo_continuo/min_proc) %>% 
  arrange(periodo_absoluto) %>% 
  group_by(estado, tipo_procedimiento) %>% 
  mutate(
    min_per = min(periodo_absoluto, na.rm = T)-1,
    periodo_absoluto = periodo_absoluto - min_per
  ) %>% 
  arrange(estado, tipo_procedimiento,periodo_absoluto) %>% 
  select(-clave_gob)%>% 
  filter(periodo_absoluto != 1)

toprint <- compranet3_tipo_proc%>%
  ungroup() %>% 
  group_split(tipo_procedimiento)
names(toprint) <-stri_sub(sort(unique(compranet3_tipo_proc$tipo_procedimiento)),
                          1, 31)

openxlsx::write.xlsx(toprint,
                     paste0(dir_output, "compranet_tipo_proc.xlsx"), 
                     overwrite = T)

# Regresiones Tipo de procedimiento ----

data_regresion_tipo_proc <- compranet3_tipo_proc  %>% 
  mutate(entidad = factor(estado))

resultados_reg <- tibble()
for(proc in unique(data_regresion_tipo_proc$tipo_procedimiento)){
  lm_fit <- lm_robust(empresas_con_periodo_continuo ~  cambio_gob + entidad,
        data = data_regresion_tipo_proc %>% 
          filter(tipo_procedimiento == proc), se_type = "HC1")
  
  resultados_reg <- tidy(lm_fit, conf.int = T) %>% 
    mutate(term = stri_replace_all(term, fixed= "entidad", ""),
           model = stri_trans_totitle(stri_replace_all(proc, fixed = "_", " "))) %>% 
    bind_rows(resultados_reg)
    
}
tt <- resultados_reg %>%
  bind_rows(general_resultados %>%
              mutate(model="General"))
toprint <-tt  %>% 
  ungroup() %>% 
  group_split(model)
names(toprint) <-stri_sub(sort(unique(tt$model)),
                          1, 31)
openxlsx::write.xlsx(toprint, paste0(dir_output, "resultados_reg_tipo_proc.xlsx"), 
                     overwrite = T)


final_compranet_tipo_proc <- data_regresion_tipo_proc %>% 
  group_by(estado, tipo_procedimiento,cambio_gob) %>% 
  summarise(empresas_con_periodo_continuo = mean(empresas_con_periodo_continuo, na.rm=T)) %>% 
  mutate(cambio_gob = ifelse(cambio_gob==0, "Sin cambio de gobierno", "Con cambio de gobierno")) %>% 
  pivot_wider(names_from = cambio_gob, values_from = empresas_con_periodo_continuo) 


toprint <- final_compranet_tipo_proc%>%
  ungroup() %>% 
  group_split(tipo_procedimiento)
names(toprint) <-stri_sub(sort(unique(final_compranet_tipo_proc$tipo_procedimiento)),
                          1, 31)
openxlsx::write.xlsx(toprint, paste0(dir_output, "compranet_reshape_tipo_proc.xlsx"), 
                     overwrite = T)


# Gráficas ----
temp <- compranet3 %>% 
  left_join(tribble(~partido,  ~Color,
                    "PAN",  "#0079bc",
                    "PRI",  "#015734",
                    "PVEM",  "#87c344",
                    "PRD",  "#ffde00",
                    "PT",  "#ee3d44",
                    "MC",  "#fd8204",
                    "PANAL",  "#00a5ae",
                    "MORENA",  "#b85756",
                    "PES",  "#9400D3",
                    "Independiente", "darkgrey")) %>% 
  filter(!is.na(cambio_partido)) %>% 
  mutate(cambio_partido2=ifelse(cambio_partido==0,"Mismo Partido","Cambio Partido"),
         cambio_partido2=as.factor(cambio_partido2),
         cambio_partido=ifelse(cambio_partido==1,1,2),
         
         cambio_gob2 = ifelse(cambio_gob==0,"Sin cambio de gobierno", "Con cambio de gobierno"),
         cambio_gob2=as.factor(cambio_gob2),
         cambio_gob=ifelse(cambio_gob==1,1,2)
         )
# Graf 1 Cambio Partido / Partidos
ggplot(temp, aes(y=empresas_con_periodo_continuo, 
             x=cambio_partido2,
             group = partido,
             #color = ifelse(cambio_gob==1, "Con cambio de gobierno", "Sin cambio de gobierno"),
             color = partido))+
  labs(title = "Porcentaje de empresas del top 50 que mantuvieron su ranking en dos periodos",
       subtitle = "",
       colour = "",
       y = "Porcentaje",
       x = "")+
  geom_jitter(width = .1, height = 0, size = 3)+
  geom_smooth(aes(x=cambio_partido),method = "lm" , show.legend = F, se = F#, color = "green"
              )+
  scale_color_manual(breaks = distinct(temp, partido, Color)$partido,
                      values = distinct(temp, partido, Color)$Color )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 30, face = "bold",vjust = .5),
    plot.subtitle = element_text(size = 25),
    plot.caption = element_text(size = 20),
    strip.text = element_text(size = 15),
    panel.spacing.x = unit(3, "lines"),
    #text = element_text(family = "Arial Narrow"),
    axis.text.x = element_text(size = 15, vjust = 0.5),
    axis.title.y = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15),
    strip.text.x =element_text(size = 18, face = "bold"),
    legend.position = "bottom"
  )+
  hrbrthemes::scale_y_percent(limits = c(0,0.5))

ggsave(paste0(dir_output, "grafico_top50_tipo_gob_partidos.png"), width = 21, height = 11, dpi = 100)

# Graf 2 Cambio Partido / Cambio Gobierno

ggplot(temp, aes(y=empresas_con_periodo_continuo, 
           x= cambio_gob2 ,
           color = cambio_partido2,
           )
         )+
  labs(title = "Porcentaje de empresas del top 50 que mantuvieron su ranking en dos periodos",
       subtitle = "",
       colour = "",
       y = "Porcentaje",
       x = "")+
  geom_jitter(width = .1, height = 0, size = 3)+
  geom_smooth(aes(x=cambio_gob),method = "lm" , show.legend = F, se = F, color = "black"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 30, face = "bold",vjust = .5),
    plot.subtitle = element_text(size = 25),
    plot.caption = element_text(size = 20),
    strip.text = element_text(size = 15),
    panel.spacing.x = unit(3, "lines"),
    #text = element_text(family = "Arial Narrow"),
    axis.text.x = element_text(size = 15, vjust = 0.5),
    axis.title.y = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15),
    strip.text.x =element_text(size = 18, face = "bold"),
    legend.position = "bottom"
  )+
  hrbrthemes::scale_y_percent(limits = c(0,0.5))

ggsave(paste0(dir_output, "grafico_top50_cambio_gob.png"), width = 21, height = 11, dpi = 100)

# Regresion Tipo de Procedimiento


resultados_reg %>%
  bind_rows(general_resultados %>%
              mutate(model="General")) %>% 
  dwplot(dot_args = list(size = 2),
         whisker_args = list(color = "darkgrey"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))+
  ggtitle("Regresión por tipo de procedimiento sobre porcentaje de empresas topN")+
  facet_grid(~str_wrap(model, 20))+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 30, face = "bold",vjust = .5),
    plot.subtitle = element_text(size = 25),
    plot.caption = element_text(size = 20),
    strip.text = element_text(size = 15),
    panel.spacing.x = unit(3, "lines"),
    #text = element_text(family = "Arial Narrow"),
    axis.text.x = element_text(size = 15, vjust = 0.5),
    axis.title.y = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15),
    strip.text.x =element_text(size = 18, face = "bold"),
    legend.position = "bottom"
  )
ggsave(paste0(dir_output, "grafico_regresion_tipo_proc.png"), width = 21, height = 11, dpi = 100)