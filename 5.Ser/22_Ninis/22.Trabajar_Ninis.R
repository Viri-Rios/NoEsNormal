require(haven)
require(tidyverse)
require(stringi)
require(Hmisc)
require(srvyr)
require(ggthemes)
require(hrbrthemes)

# Set-up ----

# Aquí se eligen los folders donde están las bases
dir_input <- "5.Ser/22_Ninis/Bases/"
# Aquí se eligen los folders donde se van a guardar los resultados
dir_output <- "5.Ser/22_Ninis/output/"
# Aquí se eligen el nombre del excel de exportación
nombre_para_guardar_excel <- "ninis.xlsx"
# Cuales años se incluyen en el estudio
añosincluidos <- c(2005,2010,2015,2020,2021)
# Elegir si se importan los archivos de nuevo o se trabaja desde memoria (por lo menos correr la primera vez)
extraer_datos_denuevo <- F

if(!dir.exists(dir_output)){
  dir.create(dir_output)
}

# Importar archivos ----
if(extraer_datos_denuevo){
  todos_archivos <- tibble()
  for (i in añosincluidos) {
    añoi = substr(i, nchar(i)-2+1, nchar(i))
    print(paste("doing: ", i))
    encoding1  = "latin1"
    
    temp_nini <- read_dta(paste0(dir_input,"COE1T1",añoi, ".dta"), encoding = encoding1) %>% 
      mutate(año = i,
             eda = as.numeric(eda)) %>% 
      left_join(read_dta(paste0(dir_input,"COE2T1",añoi, ".dta"), encoding = encoding1) %>% 
                  mutate(año = i,
                         eda = as.numeric(eda))) %>% 
      left_join(read_dta(paste0(dir_input,"SDEMT1",añoi, ".dta"), encoding = encoding1) %>% 
                  mutate(año = i,
                         eda = as.numeric(eda))) %>% 
      filter(eda>=15) %>% 
      mutate(across(starts_with(c("p1", "p2", 
                                  "p3", "p4",
                                  "p5", "p6",
                                  "p7", "p8",
                                  "p9", "par", 
                                  "sex", "nac_",
                                  "l_nac", "cs_", 
                                  "n_hij", "e_con")), ~as.numeric(.))) %>% 
      select(starts_with(c("r_def",     "cd_a",      "ent",       "con",      
                        "upm",       "d_sem",     "n_pro_viv", "v_sel",    
                        "n_hog",     "h_mud",     "n_ent",     "per",      
                        "n_ren",     "eda",       "ur",        "tipo",     
                        "mes_cal",   "ca",        "fac_tri",   "fac_men",  
                        "año",       "n_inf",     "fac",       "p2e",      
                        "p5c_thrs",  "p3h",       "p3g1_1",    "p3d",      
                        "cs_p17",    "p2g2",      "p2f",       "sex",      
                        "p11_",      "cs_p13_1" )))
    
    
    if("fac_tri" %in% names(temp_nini)){
      temp_nini <- temp_nini %>% 
        rename(fac = fac_tri)
    }
    todos_archivos <- todos_archivos%>% 
      bind_rows(temp_nini)
    rm(temp_nini)
  }
  rm(i, encoding1, añoi)
  saveRDS(todos_archivos,paste0(dir_output, "todos_archivos_ninis.rds"))
} else {
  todos_archivos <- readRDS(paste0(dir_output, "todos_archivos_ninis.rds"))
}

años0 <- c(2005, 2010)
años1 <- c(2015,2020,2021)

# Procesar ----

ninis_1 <- todos_archivos %>% 
  filter(eda>=15) %>% 
  mutate(p2e = ifelse(p2e==12, NA, p2e),
         p5c_thrs = ifelse(is.na(p5c_thrs), 0, p5c_thrs),
         sitrabajo = ifelse(p5c_thrs > 0, 1, 0),
         empleado = ifelse(p3h == 1, 1, 0),
         indep2 =  ifelse(p3d == 1 & is.na(p3g1_1), 1, 0),
         
         indep = ifelse(p3d %in% c(2, 9), 1, 0),
         indep = ifelse(indep2==1, indep2, indep),
         
         pob_ocup = ifelse(indep == 1 | empleado == 1, 1, 0),
         nini = case_when(
           sitrabajo == 0 & cs_p17 == 2 ~ 1,
           is.na(p5c_thrs) | cs_p17 == 9 ~ 9,
           eda >= 29 ~ NA_real_,
           T ~ 0
           ),
         razon = case_when(
           p2e == 9 ~ NA_character_, # "No sabe"
           p2e == 6 ~ "Otra condición",
           p2e == 1 ~ "una persona temporalmente ausente de su actividad u oficio?",
           p2e == 2 ~ "pensionado o jubilado de su empleo?",
           p2e == 3 ~ NA_character_, #"estudiante?"
           p2e == 4 ~ "una persona que se dedica a los quehaceres de su hogar?",
           p2e == 5 ~ "una persona con alguna limitación física o mental que le impide trabajar por el resto de su vida?",
           T ~ NA_character_
         ),
         razon2 = case_when(
           p2g2 == 1 ~ "Está esperando la respuesta a una solic",
           p2g2 == 2 ~ "No hay trabajo en su especialidad, ofic",
           p2g2 == 3 ~ "No cuenta con la escolaridad, los papel",
           p2g2 == 4 ~ "Piensa que por su edad o por su aspecto",
           p2g2 == 5 ~ "En su localidad no hay trabajo o sólo s",
           p2g2 == 6 ~ "La inseguridad pública o el exceso de t",
           p2g2 == 7 ~ "Espera recuperarse de una enfermedad o",
           p2g2 == 8 ~ "Está embarazada",
           p2g2 == 9 ~ "No tiene quién le cuide a sus hijos peq",
           p2g2 == 10 ~ "No lo(a) deja un familiar",
           p2g2 == 11 ~ "Otras razones de mercado",
           p2g2 == 12 ~ "Otras razones personales",
           T ~ NA_character_
         ),
         razon = ifelse(!is.na(razon2), razon2, razon),
         
         necesidad = case_when(
           p2f == 1 ~ "Sí tiene necesidad de trabajar",
           p2f == 2 ~ "Sólo tiene deseos de trabajar",
           p2f == 3 ~ "necesidad ni deseos de trabaja",
           p2f == 9 ~ NA_character_, # "No sabe",
           T ~ NA_character_
         ),
         cuenta = 1
         ) 

#  Salida 1 ----
NinisRazon1t <- ninis_1 %>% 
  filter(nini == 1) %>% 
  #as_survey_design(weights = fac) %>%
  group_by(año,razon, necesidad, sex, .drop = T) %>%
  summarise(cuenta = sum(cuenta*fac,na.rm=T)) %>% 
  select(-ends_with("se")) %>% 
  filter(razon!= "estudiante?") %>% 
  mutate(razon = ifelse(is.na(razon), "No contesta", razon),
         necesidad = ifelse(is.na(necesidad), "No contesta", necesidad),
         quiere_trab = case_when(
                    necesidad == "Sí tiene necesidad de trabajar" ~"Sí quiere trabajar",
                    necesidad == "Sólo tiene deseos de trabajar"  ~"Sí quiere trabajar",
                    T ~ "No quiere trabajar"
         )
         ) %>% 
  arrange(año, sex, razon, necesidad, quiere_trab) %>% 
  pivot_wider(names_from = año, values_from = cuenta, names_prefix = "pob_")
  
openxlsx::write.xlsx(NinisRazon1t, paste0(dir_output, "NinisRazon.xlsx"), overwrite = T)

# Indicador Hor_dom ----
ninis_2 <- ninis_1 %>% 
  mutate(
    trabajar  = p5c_thrs,
    cursos    = ifelse(!is.na(p11_1), p11_h1, 0),
    cuidar    = ifelse(!is.na(p11_2), p11_h2, 0),
    construir = case_when(
      año %in% años0 & !is.na(p11_3) ~ p11_h3,
      año %in% años0 & is.na(p11_3)  ~ 0,
      año %in% años1 & !is.na(p11_5) ~ p11_h5,
      año %in% años1 & is.na(p11_5)  ~ 0,
      T ~ 0
    ),
    reparar   = case_when(
      año %in% años0 & !is.na(p11_4) ~ p11_h4,
      año %in% años0 & is.na(p11_4)  ~ 0,
      año %in% años1 & !is.na(p11_6) ~ p11_h6,
      año %in% años1 & is.na(p11_6)  ~ 0,
      T ~ 0
    ),
    quehacer  = case_when(
      año %in% años0 & !is.na(p11_5) ~ p11_h5,
      año %in% años0 & is.na(p11_5)  ~ 0,
      año %in% años1 & !is.na(p11_7) ~ p11_h7,
      año %in% años1 & is.na(p11_7)  ~ 0,
      T ~ 0
    ),
    servicio  = case_when(
      año %in% años0 & !is.na(p11_6) ~ p11_h6,
      año %in% años0 & is.na(p11_6)  ~ 0,
      año %in% años1 & !is.na(p11_8) ~ p11_h8,
      año %in% años1 & is.na(p11_8)  ~ 0,
      T ~ 0
    ),
    hor_dom = cuidar + construir + reparar + quehacer,
  )

# Mas de 40 ----
masde40 <- ninis_2 %>% 
  mutate(masde40 = ifelse(hor_dom>=40,1,0)) %>% 
  filter(nini == 1) %>% 
  #as_survey_rep(combined_weights = fac) %>%
  group_by(año,masde40, .drop = T) %>%
  summarise(cuenta = sum(cuenta*fac,na.rm=T))

openxlsx::write.xlsx(masde40, paste0(dir_output, "masde40.xlsx"), overwrite = T)


# Factores Nini ----
ninis_3 <- ninis_2 %>% 
  mutate(
    nini = case_when(
      eda  >= 29 ~ NA_real_,
      
      nini == 1 & razon == "Está embarazada" ~ 2,
      nini == 1 & razon == "una persona que se dedica a los quehaceres de su hogar?" ~ 2,
      nini == 1 & razon == "No tiene quién le cuide a sus hijos peq" ~ 2,
      
      nini == 1 & hor_dom >= 40 ~ 2,
      
      T ~ nini
    ),
    nini = factor(nini,levels = c(0,1,2,9), labels = c("No nini","Nini","Nini que trabaja","No se sabe")),
    escolaridad = case_when(
      cs_p13_1 <= 3               ~"Secundaria o menos" ,
      cs_p13_1 > 3 & cs_p13_1 <= 6~"Hasta prepa",
      cs_p13_1 > 6 & cs_p13_1 <10 ~"Licenciatura o más" ,
      T ~ NA_character_
    )
  ) 

ninis_3 %>% 
  ungroup() %>% 
  group_by(año,nini, .drop = T) %>%
  summarise(cuenta = sum(cuenta*fac,na.rm=T)) %>% 
  filter(!is.na(nini)) %>% 
  group_by(año) %>% 
  mutate(prop = cuenta/sum(cuenta),
         año = as.factor(año)) %>% 
  filter(prop > 0.001) %>% 
  filter(!is.na(nini))%>% 
  #group_by(año) %>% 
  #mutate(prop = cuenta/sum(cuenta),
  #       año = as.factor(año)) %>% 
  filter(nini != "No nini") %>% 
  mutate(nini= as.character(nini),
         nini = ifelse(nini == "Nini", "Ninis que trabajan", nini)) %>% 
  ggplot(aes(x = año, y = prop, fill  = nini, label = paste0(round(prop*100,0),"%")))+
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5))+
  scale_fill_grey()+
  theme_minimal() +
  scale_y_percent(limits = c(0,.3))+
  labs(
    title= "Porcentaje de los jóvenes que son:",
    subtitle = "",
    caption = "Libro Caption", 
    x = "",
    y = "",
    fill = ""
  ) +
  theme(axis.title.y = element_text(hjust = .5, #family = "Comic Sans MS",
                                    size=20), 
        legend.position="top",
        plot.title = element_text(size=20),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20,hjust = .5)
  )


NinisSexo1t_esc <- ninis_3 %>% 
  as_survey_design(weights = fac) %>%
  group_by(año,sex, nini, escolaridad, .drop = T) %>%
  summarise(cuenta   = sum(cuenta,na.rm=T),
            eda      = survey_mean(eda, na.rm = T), 
            trabajar = survey_mean(trabajar, na.rm = T), 
            cursos   = survey_mean(cursos, na.rm = T), 
            cuidar   = survey_mean(cuidar, na.rm = T), 
            construir= survey_mean(construir, na.rm = T), 
            reparar  = survey_mean(reparar, na.rm = T), 
            quehacer = survey_mean(quehacer, na.rm = T), 
            servicio = survey_mean(servicio, na.rm = T), 
            ) %>% 
  select(-ends_with("se")) %>% 
  filter(!is.na(nini)) %>% 
  #filter(!is.na(escolaridad)) %>%
  mutate(hor_dom = cuidar + construir +reparar + quehacer) %>% 
  arrange(año, sex, nini, cuenta, cursos, cuidar, construir, reparar,
          quehacer, servicio, hor_dom)

openxlsx::write.xlsx(NinisSexo1t_esc, paste0(dir_output, "NinisSexo_esc.xlsx"), overwrite = T)

NinisSexo1t <- ninis_3 %>% 
  as_survey_design(weights = fac) %>%
  group_by(año,sex, nini, .drop = T) %>%
  summarise(cuenta   = sum(cuenta,na.rm=T),
            trabajar = survey_mean(trabajar, na.rm = T), 
            cursos   = survey_mean(cursos, na.rm = T), 
            cuidar   = survey_mean(cuidar, na.rm = T), 
            construir= survey_mean(construir, na.rm = T), 
            reparar  = survey_mean(reparar, na.rm = T), 
            quehacer = survey_mean(quehacer, na.rm = T), 
            servicio = survey_mean(servicio, na.rm = T), 
  ) %>% 
  select(-ends_with("se")) %>% 
  filter(!is.na(nini)) %>% 
  mutate(hor_dom = cuidar + construir +reparar +quehacer) %>% 
  arrange(año, sex, nini, cuenta, cursos, cuidar, construir, reparar,
          quehacer, servicio, hor_dom) 
  
openxlsx::write.xlsx(NinisSexo1t, paste0(dir_output, "NinisSexo.xlsx"), overwrite = T)

grafica_ninis <- NinisSexo1t %>% 
  select(sex, nini, cuenta, año) %>% 
  group_by(año, nini) %>% 
  summarise(cuenta = sum(cuenta, na.rm = T)) %>% 
  pivot_wider(names_from = año, values_from = cuenta)

openxlsx::write.xlsx(NinisSexo1t, paste0(dir_output, "grafica_ninis.xlsx"), overwrite = T)

pest2 <- NinisSexo1t %>% 
  filter(año == 2021) %>% 
  ungroup() %>% 
  mutate(sex = case_when(
    sex == 1 ~ "Hombre",
    sex == 2 ~ "Mujer",
    T ~ NA_character_
  )) %>% 
  select(sex, nini, cuenta, hor_dom) %>% 
  pivot_wider(names_from = sex, values_from = c(cuenta, hor_dom))
  
openxlsx::write.xlsx(pest2, paste0(dir_output, "pest2.xlsx"), overwrite = T)

temp <- NinisSexo1t_esc %>% 
  filter(nini != 0) %>% 
  filter(nini != 9) %>% 
  filter(año != 2021) %>% 
  filter(!is.na(escolaridad)) 

sex_eda <- temp %>% 
  ungroup() %>% 
  select(sex, nini, cuenta, escolaridad, eda) %>% 
  as_survey_design(weights = cuenta) %>%
  group_by(escolaridad, sex ,.drop = T) %>%
  summarise(eda   = mean(eda,na.rm=T))
openxlsx::write.xlsx(sex_eda, paste0(dir_output, "sex_eda.xlsx"), overwrite = T)


pest3 <- temp %>% 
  ungroup() %>% 
  select(sex, nini, cuenta, escolaridad, eda) %>% 
  group_by(escolaridad, sex ,.drop = T) %>%
  summarise(cuenta   = sum(cuenta, na.rm=T)) %>% 
  left_join(sex_eda)

openxlsx::write.xlsx(pest3, paste0(dir_output, "pest3.xlsx"), overwrite = T)
