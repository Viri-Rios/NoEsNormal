# 01 Competir Vender Caro 2002-2018

# Bibliotecas ----
require(haven)
require(tidyverse)
require(stringi)
require(Hmisc)
require(srvyr)
require(ggthemes)
require(hrbrthemes)

# Set-up ----
# Cuales años se incluyen en el estudio
añosincluidos <- seq(2002,2020, 2)
# AquÃ­ se eligen los folders donde estÃ¡n las bases
dir_input <- "1.Competir/1_VenderCaro/Bases/"
# AquÃ­ se eligen los folders donde se van a guardar los resultados
dir_output <- "1.Competir/1_VenderCaro/output/"
# AquÃ­ se eligen el nombre del archivo de exportaciÃ³n final
nombre_para_guardar_csv <- "Resultados_2002_2020.csv"
# Elegir si se importan los archivos de nuevo o se trabaja desde memoria (por lo menos correr la primera vez)
extraer_datos_denuevo <- F

# Datos Externos ----

## Se generan listas con las claves de gastos para cada categorÃ­a usada en el estudio
lista_1 <- list("CarneDeRes"= "A023 A031 A032 A034",
                "PolloHuevo"= "A053 A054 A055 A056 A089",
                "Tortillas"= "A004",
                "BebidasNoAlc"= "A212 A214 A217",
                "Verdura"= "A104 A120 A98 A108 A125 A116 A121 A111 A107 A113 A122 A109 A112 A123 A117 A110 A133",
                "TranspTer"= "M001"
                ) %>% 
  enframe() %>%
  mutate(value = stri_split(value, fixed =" ")) %>% 
  unnest(value)%>% 
  rename(tipo = name,clave = value )

lista_2 <- list("CarneDeRes"= "A025 A034 A035 A037",
                "PolloHuevo"= "A057 A058 A059 A093",
                "Tortillas"= "A004",
                "BebidasNoAlc"= "A215 A218 A220",
                "Verdura"= "A108 A124 A102 A112 A129 A120 A125 A111 A130 A117 A126 A113 A116 A127 A121 A114 A137",
                "TranspTer"= "M001")%>% 
  enframe() %>%
  mutate(value = stri_split(value, fixed =" ")) %>% 
  unnest(value) %>% 
  rename(tipo = name,clave = value )

listas_categorias <- bind_rows(lista_1 %>% mutate(año = 2002),
                               lista_1 %>% mutate(año = 2004),
                               lista_2 %>% mutate(año = 2006),
                               lista_2 %>% mutate(año = 2008),
                               lista_2 %>% mutate(año = 2010),
                               lista_2 %>% mutate(año = 2012),
                               lista_2 %>% mutate(año = 2014),
                               lista_2 %>% mutate(año = 2016),
                               lista_2 %>% mutate(año = 2018),
                               lista_2 %>% mutate(año = 2020))
rm(lista_1, lista_2)

#Generamos los Ã­ndices de precios

datos_indices_precio <- tribble(
  ~año, ~tipo       ,   ~inpc            , ~inpp            ,  ~sobreprecio,
  2002, "CarneDeRes",   47.71            , 49.23            , 0.0813,
  2002, "TranspTer",    50.27            , 58.29            , 0.1454,
  2002, "Tortillas",    40.26            , 40.23            , 0.2619,
  2002, "BebidasNoAlc", 63.00            , 60.08            , 0.0485,
  2002, "Verdura",      61.65            , 43.93            , 0.3047,
  2002, "PolloHuevo",   (36.63+47.72)/2  , (35.01+47.87)/2  , 0.1402,
  2004, "CarneDeRes",   56.02            , 57.81            , 0.0813,
  2004, "TranspTer",    56.26            , 65.26            , 0.1454,
  2004, "Tortillas",    48.49            , 48.45            , 0.2619,
  2004, "BebidasNoAlc", 65.38            , 68.35            , 0.0485,
  2004, "Verdura",      65.56            , 45.63            , 0.3047,
  2004, "PolloHuevo",   (44.57+49.90)/2  , (43.18+50.06)/2  , 0.1402,
  2006, "CarneDeRes",   63.81            , 65.83            , 0.0813,
  2006, "TranspTer",    61.60            , 71.45            , 0.1454,
  2006, "Tortillas",    54.66            , 54.61            , 0.2619,
  2006, "BebidasNoAlc", 66.88            , 73.65            , 0.0485, 
  2006, "Verdura",      74.37            , 54.12            , 0.3047,
  2006, "PolloHuevo",   (41.44+55.62)/2  , (36.92+55.78)/2  , 0.1402,           
  2008, "CarneDeRes",   65.46            , 67.51            , 0.0813,
  2008, "TranspTer",    66.21            , 76.81            , 0.1454,
  2008, "Tortillas",    65.28            , 65.22            , 0.2619,
  2008, "BebidasNoAlc", 72.04            , 78.60            , 0.0485, 
  2008, "Verdura",      71.81            , 52.25            , 0.3047,
  2008, "PolloHuevo",   (58.84+65.26)/2  , (65.43+58.27)/2  , 0.1402,
  2010, "CarneDeRes",   74.58            , 76.61            , 0.0813,
  2010, "TranspTer",    72.91            , 84.57            , 0.1454,
  2010, "Tortillas",    75.58            , 75.51            , 0.2619,
  2010, "BebidasNoAlc", 81.06            , 84.98            , 0.0485, 
  2010, "Verdura",      86.72            , 71.75            , 0.3047,
  2010, "PolloHuevo",   (77.62+72.63)/2  , (67.40+77.81)/2  , 0.1402, 
  2012, "CarneDeRes",   87.26            , 90               , 0.0813, 
  2012, "TranspTer",    86.07            , 99.84            , 0.1454,    
  2012, "Tortillas",    95.25            , 95.16            , 0.2619,    
  2012, "BebidasNoAlc", 86.01            , 92.25            , 0.0485,    
  2012, "Verdura",      86.94            , 77.78            , 0.3047,     
  2012, "PolloHuevo",   (88.22+80.36)/2  , (88.43+80.12)/2  , 0.1402, 
  2014, "CarneDeRes",   100              , 100              , 0.0813,
  2014, "TranspTer",    100              , 100              , 0.1454,
  2014, "Tortillas",    100              , 100              , 0.2619,
  2014, "BebidasNoAlc", 100              , 100              , 0.0485,
  2014, "Verdura",      100              , 100              , 0.3047,
  2014, "PolloHuevo",   100              , 100              , 0.1402,
  2016, "CarneDeRes",   132.7            , 126.34           , 0.0813,
  2016, "TranspTer",    109.02           , 109.02           , 0.1454,
  2016, "Tortillas",    100.88           , 100.93           , 0.2619,
  2016, "BebidasNoAlc", 107.39           , 106.54           , 0.0485,
  2016, "Verdura",      125.80           , 132.30           , 0.3047,
  2016, "PolloHuevo",   (99.36+83.02)/2  , (65.53+113.15)/2 , 0.1402,
  2018, "CarneDeRes",   140.28           , 140.05           , 0.0813,
  2018, "TranspTer",    120.35           , 120.35           , 0.1454,
  2018, "Tortillas",    113.57           , 113.39           , 0.2619,
  2018, "BebidasNoAlc", 118.52           , 123.49           , 0.0485,
  2018, "Verdura",      130.79           , 120.87           , 0.3047,
  2018, "PolloHuevo",   (107.67+106.51)/2, (110.34+117.66)/2, 0.1402,
  
  2020, "CarneDeRes"          , 145.86           , 153.48             , 0.0813,
  2020, "TranspTer"           , 132.17           , 132.17             , 0.1454,
  2020, "Tortillas"           , 122.47           , 122.49             , 0.2619,
  2020, "BebidasNoAlc"        , 132.26           , 138.00             , 0.0485,
  2020, "Verdura"             , 152.92           , 156.34             , 0.3047,
  2020, "PolloHuevo"          , (114.32+112.33)/2, (96.90+148.56)/2   , 0.1402,
  
)  

# Leer Todo ----

if(extraer_datos_denuevo){
  todos_concentrados <- tibble()
  todos_hogares <- tibble()
  todos_persona <- tibble()
  
  archivos <- list.files(dir_input, pattern = ".csv")
  for (archivo in archivos) {
    añoi <- as.numeric(stri_extract_last(archivo, regex = "[:digit:]+"))
    tipo <- stri_extract_first(archivo, regex = "[:alpha:]+")
    sepp <- "\t"
    if(añoi == 2020){
      sepp <- ","
    }
    if(añoi %in% añosincluidos){
      print(archivo)
      if (tipo == "gastoshogar") {
        
        temp_hogares <- read.csv(paste0(dir_input,archivo), header = T, sep = sepp) 
        
        if("gasto_tri" %in% names(temp_hogares)){
          temp_hogares <- temp_hogares %>% 
            rename(gas_tri = gasto_tri)
        }
        temp_hogares <- temp_hogares %>% 
          mutate(gas_tri = ifelse(is.na(gas_tri), 0, gas_tri),
                 año = añoi) %>% 
          select(starts_with("folio"), clave, gasto = gas_tri, año) %>% 
          unite("folio", starts_with("folio"), sep = " ")
        
        todos_hogares <- bind_rows(todos_hogares,temp_hogares)
        
      } else if (tipo == "concentradohogar") {
        
        ## Se carga el concentrado de hogares, y se genera un identificador ?nico de hogar
        ## igual al que se gener? con las bases de gastos
        temp_concentrado <- read.delim(paste0(dir_input, archivo), header = T, sep = sepp) %>% 
          mutate(año = añoi) %>% 
          unite("folio", starts_with("folio"), sep = " ")
        
        if("hog" %in% names(temp_concentrado)){
          temp_concentrado <- temp_concentrado %>% 
            rename(factor = hog)
        }
        if("factor_hog" %in% names(temp_concentrado)){
          temp_concentrado <- temp_concentrado %>% 
            rename(factor = factor_hog)
        }
        if("ing_cor" %in% names(temp_concentrado)){
          temp_concentrado <- temp_concentrado %>% 
            rename(ingcor = ing_cor)
        }
        temp_concentrado <- temp_concentrado %>% 
          select(año, folio, ingcor, factor)
        
        todos_concentrados <- bind_rows(todos_concentrados,temp_concentrado)
      } else if (tipo == "gastopersona") {
        
        temp_persona <- read.csv(paste0(dir_input,archivo), header = T, sep = sepp) 
        
        if("gasto_tri" %in% names(temp_persona)){
          temp_persona <- temp_persona %>% 
            rename(gas_tri = gasto_tri)
        }
        temp_persona <- temp_persona %>% 
          mutate(gas_tri = ifelse(is.na(gas_tri), 0, gas_tri),
                 año = añoi) %>% 
          select(starts_with("folio"), clave, gasto = gas_tri, año) %>% 
          unite("folio", starts_with("folio"), sep = " ")
        todos_persona <- bind_rows(todos_persona,temp_persona)
      }
    }
  }
  rm(temp_hogares,temp_concentrado,temp_persona,  añoi, archivo, archivos, tipo)
  saveRDS(todos_concentrados, paste0(dir_output, "todos_concentrados.rds"))
  saveRDS(todos_hogares, paste0(dir_output, "todos_hogares.rds"))
  saveRDS(todos_persona, paste0(dir_output, "todos_persona.rds"))
} else {
  todos_persona <- readRDS(paste0(dir_output, "todos_persona.rds"))
  todos_hogares <- readRDS(paste0(dir_output, "todos_hogares.rds"))
  todos_concentrados <- readRDS(paste0(dir_output, "todos_concentrados.rds"))
}


## Se genera una variable que identifique al hogar segÃºn el decil de ingreso
## en el que se encuentra. Se calculan los deciles usando el hog de expansiÃ³n
datos_deciles <- todos_concentrados %>%
  group_by(año) %>% 
  group_modify(~{
    wtd.quantile(.x$ingcor, weights=.x$factor,probs = seq(.1, .9, .1)) %>% 
      tibble::enframe()
  }) %>%
  pivot_wider(names_from = name, values_from = value)

# Por Hogar ----

## Se colapsan las variables de interÃ©s con base en el identificador del hogar
## de manera que quede Ãºnicamente una observaciÃ³n por hogar
datos_gasto_inelastico_hogares <- todos_hogares %>% 
  left_join(listas_categorias) %>% 
  # Eliminamos gastos de categorÃ­as que no identificamos
  filter(!is.na(tipo)) %>% 
  select(folio, tipo, gasto, año) %>% 
  # Sumamos los gastos por categorÃ­a (diferentes claves) y por folio (diferentes hogares)
  group_by(año, folio, tipo) %>% 
  summarise(gasto = sum(gasto, na.rm = T))%>% 
  ungroup() %>% 
  # Transformamos la base para que cada columna sea un tipo de gasto
  pivot_wider(names_from = tipo, values_from = gasto) %>% 
  rename_at(vars(-c(folio, año)), ~paste0("gasto_",.)) %>% 
  # Sumamos el gasto por folio (Hogar) para encontrar el gasto por hogar
  rowwise() %>% 
  mutate(gasto_inelastico_hogar = sum(c_across(starts_with("gasto_")), na.rm = T) ) 

datos_gasto_inelastico_hogares<- todos_hogares %>% 
  # Obtenemos el gasto total por hogar y lo pegamos como nueva columna
  group_by(año, folio) %>% 
  summarise(gasto_hogar= sum(gasto, na.rm = T))%>% 
  ungroup() %>% 
  left_join(datos_gasto_inelastico_hogares)

# TODO: pregunta el ingreso es trimestral?, el gasto es trimestral?
## Se junta el concentrado de hogares con las bases de gastos con las estimaciones
## adicionales
datos_gasto_inelastico_hogares <- datos_gasto_inelastico_hogares  %>% 
  left_join(todos_concentrados) %>% 
  #filter(!is.na(gasto_inelastico_hogar)) %>% 
  mutate(across(starts_with("gasto_"), ~ifelse(is.na(.),0,.))) %>% 
  ## Se genera una variable que agregue el gasto por hogar obtenido a partir de
  ## la base de hogares y la base de personas.
  ## Se genera una variable que agregue el gasto por hogar obtenido a partir de
  ## la base de hogares y la base de personas, sÃ³lo en las categorÃ­as de gasto
  ## del anÃ¡lisis
  filter(ingcor>0) %>% 
  mutate(proporcion = gasto_inelastico_hogar/ingcor) %>% 
  left_join(datos_deciles) %>% 
  group_by(año) %>% 
  mutate(decil = factor(findInterval(ingcor, c(-Inf,`10%`,`20%`,`30%`,`40%`,`50%`,`60%`,`70%`,`80%`,`90%`, Inf)),
                        labels=paste0("D",seq(1,10))
                        )
         ) %>% 
  select(-ends_with("%"))

# TEST: %>% mutate(is_ing_bigger=ingcor>gasto_hogar)
# TEST: prop.table(table(datos_gasto_inelastico$is_ing_bigger))

if(!dir.exists(dir_output)){
  dir.create(dir_output)
}
## Se guarda la base de datos final con deciles de ingreso

write_csv(datos_gasto_inelastico_hogares, paste0(dir_output,"datos_gasto_inelastico_hogares.csv"))

# Por Persona ----

## Se colapsan las variables de interÃ©s con base en el identificador del hogar
## de manera que quede Ãºnicamente una observaciÃ³n por hogar
datos_gasto_inelastico_personas <- todos_persona %>% 
  left_join(listas_categorias) %>% 
  # Eliminamos gastos de categorÃ­as que no identificamos
  filter(!is.na(tipo)) %>% 
  select(folio, tipo, gasto, año) %>% 
  # Sumamos los gastos por categorÃ­a (diferentes claves) y por folio (diferentes hogares)
  group_by(año, folio, tipo) %>% 
  summarise(gasto = sum(gasto, na.rm = T))%>% 
  ungroup() %>% 
  # Transformamos la base para que cada columna sea un tipo de gasto
  pivot_wider(names_from = tipo, values_from = gasto) %>% 
  rename_at(vars(-c(folio, año)), ~paste0("gasto_",.)) %>% 
  # Sumamos el gasto por folio (Hogar) para encontrar el gasto por hogar
  rowwise() %>% 
  mutate(gasto_inelastico_personas  = sum(c_across(starts_with("gasto_")), na.rm = T) ) 

datos_gasto_inelastico_personas <- todos_persona %>% 
  # Obtenemos el gasto total por hogar y lo pegamos como nueva columna
  group_by(año, folio) %>% 
  summarise(gasto_personas = sum(gasto, na.rm = T))%>% 
  ungroup() %>% 
  left_join(datos_gasto_inelastico_personas)


## Se guarda la base de datos final con deciles de ingreso
write_csv(datos_gasto_inelastico_personas, paste0(dir_output,"datos_gasto_inelastico_personas.csv"))


# union ----

## Se pegan las bases de gasto por hogar y por persona y se colapsan con base en
## la clave del hogar para obtener el gasto total del hogar
datos_gasto_inelastico <- datos_gasto_inelastico_personas %>% 
  bind_rows(datos_gasto_inelastico_hogares) %>% 
  group_by(año, folio) %>% 
  summarise(across(starts_with("gasto_"), ~sum(., na.rm=T))) %>% 
  mutate(
         ## Se genera una variable que agregue el gasto por hogar obtenido a partir de
         ## la base de hogares y la base de personas, s?lo en las categor?as de gasto
         ## del an?lisis
         gasto_inelastico = gasto_inelastico_personas+gasto_inelastico_hogar,
         ## Se genera una variable que agregue el gasto por hogar obtenido a partir de
         ## la base de hogares y la base de personas
         gasto = gasto_hogar+gasto_personas) %>% 
  ## Se junta el concentrado de hogares con las bases de gastos con las estimaciones
  ## adicionales
  left_join(todos_concentrados) %>% 
  filter(ingcor>0) %>% 
  mutate(proporcion = gasto_inelastico/ingcor) %>% 
  ## Se genera una variable que identifique al hogar segÃºn el decil de ingreso
  ## en el que se encuentra.
  left_join(datos_deciles) %>% 
  group_by(año) %>% 
  mutate(decil = factor(findInterval(ingcor, c(-Inf,`10%`,`20%`,`30%`,`40%`,`50%`,`60%`,`70%`,`80%`,`90%`, Inf)),
                        labels=paste0("D",seq(1,10))
  )
  ) %>% 
  select(-ends_with("%"))

write_csv(datos_gasto_inelastico, paste0(dir_output,"datos_gasto_inelastico.csv"))

ggplot(datos_gasto_inelastico %>% 
         filter(proporcion<1), 
       aes(x=decil, y=proporcion)) +
  geom_violin(aes(fill = decil, color = decil))+
  #geom_point(aes(color = decil))+
  geom_smooth(aes(x=as.numeric(stri_extract_last(decil, regex="[:digit:]+"))), color = "red")+
  hrbrthemes::scale_y_percent()+
  facet_wrap(~año)+
  theme_minimal()+
  labs(
    title= str_wrap("DistribuciÃ³n de Porcentaje de Gasto InelÃ¡stico sobre Ingreso por Decil", 70),
    subtitle = "La lÃ­nea roja es una regresiÃ³n GAM con ajuste suavizado del promedio",
    caption = "Libro Viridiana RÃ­os", 
    x = "Deciles",
    y = "Porcentaje de Gasto",
    color = "",
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold",vjust = .5),
    plot.subtitle = element_text(size = 25),
    plot.caption = element_text(size = 20),
    strip.text = element_text(size = 15),
    panel.spacing.x = unit(3, "lines"),
    #text = element_text(family = "Arial Narrow"),
    axis.text.x = element_text(size = 15, vjust = 0.5, angle = 90),
    axis.title.y = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15),
    strip.text.x =element_text(size = 18, face = "bold"),
    legend.position = "none"
  )
ggsave(paste0(dir_output,"proporcion_gasto_violin.png"), width = 21, height = 11, dpi = 100)


# Final ----

datos_perdida_bienestar <-   datos_gasto_inelastico %>% 
    rename_at(vars(gasto_inelastico,gasto_hogar,gasto_inelastico_hogar,
                    gasto_inelastico_personas, gasto_personas), 
                  ~stri_replace_first(., fixed = "gasto", "gast")
           ) %>% 
  rename(gasto_hog_y_per = gasto) %>% 
    pivot_longer(cols = starts_with("gasto_"), names_prefix = "gasto_", 
                 names_to = "tipo", values_to = "gasto") %>% 
  left_join(datos_indices_precio) %>% 
  mutate(sobreprecio = sobreprecio * (1+((inpc-inpp)/100)),
         sobreprecio = ifelse(gasto ==0, NA, sobreprecio),
         perdida_bienestar = gasto * sobreprecio) %>% 
  select(-inpc, -inpp) %>% 
  pivot_wider(names_from = tipo, values_from = c(sobreprecio, perdida_bienestar, gasto)) %>% 
  rowwise() %>% 
  #* Se genera una variable que agrega la p?rdida de bienestar total considerando
  #* todos los bienes
  mutate(perdida_bienestar_decil = sum(c_across(starts_with("perdida_")), na.rm = T),
         perdida_bienestar_proporcion =perdida_bienestar_decil/ingcor ) %>% 
  rename_at(vars(gast_inelastico,gast_hogar,gast_inelastico_hogar,
                 gast_inelastico_personas, gast_personas), 
            ~stri_replace_first(., fixed = "gast", "gasto")) %>% 
  rename( gasto=gasto_hog_y_per)

# with(datos_perdida_bienestar%>% 
#        filter(perdida_bienestar_proporcion<1),
#      plotly::plot_ly(x=decil, 
#                      y=perdida_bienestar_proporcion,
#                      z = año,
#                      type="scatter3d",
#                      mode="markers",
#                      color=decil))


#* Tardado
datos_perdida_bienestar_summary <- datos_perdida_bienestar %>% 
  #* Unimos la base de nuevo pero con un "decil" Ãºnico para encontrar el 
  #* promedio al momento de agrupar por deciles
  bind_rows(datos_perdida_bienestar %>% 
              mutate(decil="D11")) %>% 
  as_survey_design(weights = factor) %>%
  group_by(año, decil, .drop = T) %>%
  summarise_at(vars(starts_with(c("perdida_", "sobreprecio_", "gasto_"))), ~survey_mean(., na.rm = T)) 


Resultados_2002_2018 <- datos_perdida_bienestar_summary %>% 
  rename(PB = perdida_bienestar_proporcion) %>% 
  select(-starts_with("perdida_bienestar")) %>% 
  # quita error estÃ¡ndar de estimaciÃ³n
  select(-ends_with("_se")) %>% 
  mutate(decil=ifelse(decil=="D11", "Total", decil))

write_csv(Resultados_2002_2018, paste0(dir_output,nombre_para_guardar_csv))


temp <- datos_perdida_bienestar_summary%>% 
  mutate(order = as.numeric(stri_extract_first(decil, regex="[:digit:]+")),
         decil=ifelse(decil=="D11", "Promedio", decil),
         perdida_bienestar_proporcion_ymax = perdida_bienestar_proporcion+1.96*perdida_bienestar_proporcion_se,
         perdida_bienestar_proporcion_ymin = perdida_bienestar_proporcion-1.96*perdida_bienestar_proporcion_se)

ggplot(temp,
       aes(x=reorder(decil, order), 
           label = paste0(round(perdida_bienestar_proporcion*100,1),"%"),
           y=perdida_bienestar_proporcion,
           # Intervalos de confianza al 95%
           ymin= perdida_bienestar_proporcion_ymax,
           ymax= perdida_bienestar_proporcion_ymin,
           fill = decil)) +
  geom_col()+
  # Intervalos de confianza al 95%
  geom_errorbar(colour = "grey", alpha = .8)+
  geom_text()+
  scale_fill_discrete(palette=ggthemes::stata_pal(scheme = "s2color"))+
  scale_y_percent(limits = c(0,max(temp$perdida_bienestar_proporcion_ymax)+.01))+
  facet_wrap(~año)+
  #ggthemes::theme_stata()+
  theme_minimal()+
  labs(
    title= str_wrap("Porcentaje de Gasto InelÃ¡stico con Sobreprecio sobre Ingreso", 80),
    subtitle = "",
    caption = "Libro Caption", 
    x = "Decil",
    y = "Porcentaje",
    color = "",
    fill = ""
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold",vjust = .5),
    plot.subtitle = element_text(size = 25),
    plot.caption = element_text(size = 20),
    strip.text = element_text(size = 15),
    panel.spacing.x = unit(3, "lines"),
    #text = element_text(family = "Arial Narrow"),
    axis.text.x = element_text(size = 15, vjust = 0.5, angle = 90),
    axis.title.y = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15),
    strip.text.x =element_text(size = 18, face = "bold"),
    legend.position = "none"
  )
ggsave(paste0(dir_output,"perdida_bienestar_proporcion.png"), width = 21, height = 11, dpi = 100)
