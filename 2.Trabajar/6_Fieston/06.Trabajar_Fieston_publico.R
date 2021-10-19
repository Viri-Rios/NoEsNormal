# 2 Trabajar 6 Fieston
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
añosincluidos <- 2019
# Aquí se eligen los folders donde están las bases
dir_input <- "2.Trabajar/6_Fieston/Bases/"
# Aquí se eligen los folders donde se van a guardar los resultados
dir_output <- "2.Trabajar/6_Fieston/outputpublico/"
dir_output_2<- "2.Trabajar/6_Fieston/bases_peq/"
# TODO : Aquí se eligen el nombre del archivo de exportación final
nombre_para_guardar_csv <- ""
# Elegir si se importan los archivos de nuevo o se trabaja desde memoria (por lo menos correr la primera vez)
extraer_datos_denuevo <- F


if(!dir.exists(dir_output)){
  dir.create(dir_output)
}

# Leer Todo ----

if(extraer_datos_denuevo){
  archivos <- list.files(dir_input, pattern = ".dta") %>% 
    as_tibble() %>% 
    rename(file = value ) %>% 
    mutate(año = stri_extract_last(file, regex = "[:digit:]+"),
           año = as.numeric(substr(año, nchar(año)-2+1, nchar(año)))+2000,
           tipo = tolower(substr(file, 0, 5))
    ) %>% 
    arrange ( año)
  
  for (i in 1:nrow(archivos)) {
    
    if(archivos$año[i] %in% añosincluidos){
      print(archivos$file[i])
      encoding1  = "latin1" #utf8
      if (archivos$tipo[i] == "coe1t") {
        if(i == 40) {
          encoding1 = NULL
        }
        temp_coe1t <- read_dta(paste0(dir_input,archivos$file[i]), encoding = encoding1) %>% 
          mutate(año = archivos$año[i])
        
        if("fac_tri" %in% names(temp_coe1t)){
          temp_coe1t <- temp_coe1t %>% 
            rename(fac = fac_tri)
        }
        
        saveRDS(temp_coe1t, paste0(dir_output_2, archivos$tipo[i],"_",archivos$año[i],".rds"))
        
      } else if (archivos$tipo[i] == "coe2t") {
        if(i == 41) {
          encoding1 = NULL
        }
        temp_coe2t <- read_dta(paste0(dir_input,archivos$file[i]), encoding = encoding1)%>% 
          mutate(año = archivos$año[i])
        if("fac_tri" %in% names(temp_coe2t)){
          temp_coe2t <- temp_coe2t %>% 
            rename(fac = fac_tri)
        }
        
        saveRDS(temp_coe2t, paste0(dir_output_2, archivos$tipo[i],"_",archivos$año[i],".rds"))
      } else if (archivos$tipo[i] == "sdemt") {
        
        temp_sdemt <- read_dta(paste0(dir_input,archivos$file[i]), encoding = encoding1)%>% 
          mutate(año = archivos$año[i])
        
        saveRDS(temp_sdemt, paste0(dir_output_2, archivos$tipo[i],"_",archivos$año[i],".rds"))
        
      }
    }
  }
  rm(temp_sdemt,temp_coe2t,temp_coe1t, archivos)
} 

todos_años <- tibble()
todos_años_priv <- tibble()
for(i in añosincluidos){
  print(paste("año", i))
  fieston <- readRDS(paste0(dir_output_2, "coe1t","_",i,".rds")) %>% 
    left_join(readRDS(paste0(dir_output_2, "coe2t","_",i,".rds"))) %>% 
    left_join(readRDS(paste0(dir_output_2, "sdemt","_",i,".rds"))) %>% 
    filter(eda>=15) %>% 
    mutate(sector = case_when(
      p4a >= 1100 & p4a <= 1199     ~11,
      p4a >= 2120 & p4a <= 2129     ~212,
      p4a >= 2300 & p4a <= 2399     ~23,
      p4a >= 3100 & p4a <= 3399     ~31,
      p4a >= 4300 & p4a <= 4399     ~43,
      p4a >= 4600 & p4a <= 4699     ~43,
      p4a >= 4800 & p4a <= 4999     ~48,
      p4a >= 5100 & p4a <= 5199     ~51,
      p4a >= 5200 & p4a <= 5299     ~52,
      p4a >= 5400 & p4a <= 5499     ~54,
      p4a >= 5600 & p4a <= 5699     ~56,
      p4a >= 6100 & p4a <= 6199     ~61,
      p4a >= 6200 & p4a <= 6299     ~62,
      p4a >= 7100 & p4a <= 7199     ~71,
      p4a >= 7200 & p4a <= 7299     ~72,
      p4a >= 8100 & p4a <= 8199     ~81,
      
      T ~ NA_real_
    ),
    priv = case_when(
      sector == 212 ~ 0,
      sector == 56 ~ 0,
      sector == 61 ~ 0,
      sector == 62 ~ 0,
      
      !is.na(sector) ~ 1,
      T ~ 0
    ),
    empleado = ifelse(p3h == 1, 1, 0),
    indep2 =  ifelse(p3d == 1 & is.na(p3g1_1), 1, 0),
    indep = ifelse(p3d %in% c(2, 9), 1, 0),
    indep = ifelse(indep2==1, indep2, indep),
    pob_ocup = ifelse(indep == 1 | empleado == 1, 1, 0),
    informal = case_when(
      tue2 == 5 ~ 1,
      pos_ocu == 3 & rama == 6 ~ 1,
      pos_ocu == 4 ~ 1,
      (seg_soc == 2 | seg_soc ==3) & tue2 == 6 ~ 1,
      pos_ocu == 1 & remune2c == 1 & (tue2 < 5 | tue2 > 7) & (seg_soc == 2  | seg_soc == 3) ~ 1,
      pos_ocu == 1 & remune2c == 2 & (tue2 < 5 | tue2 > 7) & (seg_soc == 2  | seg_soc == 3) ~ 1,
      pos_ocu == 5 & (seg_soc == 2 | seg_soc == 3) ~ 1,
      T ~ 0),
    IngEmp =  ifelse(empleado == 1, p6b2*.25/p5d_thrs, NA),
    IngInd =  ifelse(indep == 1   , p6b2*.25/p5d_thrs, NA),
    HrsEmp =  ifelse(empleado == 1, p5d_thrs, NA),
    HrsInd =  ifelse(indep == 1   , p5d_thrs, NA),
    ) 
  
  fieston_sector<-fieston %>% 
    filter(!is.na(sector)) %>% 
    as_survey_design(weights = fac) %>%
    group_by(año, sector, .drop = T) %>%
    summarise(pob_ocup = sum(pob_ocup*fac,na.rm=T), #unweighted
              empleado = sum(empleado*fac,na.rm=T),
              indep    = sum(indep*fac,na.rm=T),
              indep2   = sum(indep2*fac,na.rm=T),
              IngEmp = survey_mean(IngEmp,na.rm=T), 
              IngInd = survey_mean(IngInd,na.rm=T), 
              HrsEmp = survey_mean(HrsEmp,na.rm=T), 
              HrsInd = survey_mean(HrsInd,na.rm=T), 
              informal = survey_mean(informal,na.rm=T)) %>% 
    select(-ends_with("se"))
  
  fieston_priv<-fieston %>% 
    filter(priv == 1) %>% 
    as_survey_design(weights = fac) %>%
    group_by(año,priv, .drop = T) %>%
    summarise(pob_ocup = sum(pob_ocup*fac,na.rm=T), #unweighted
              empleado = sum(empleado*fac,na.rm=T),
              indep    = sum(indep*fac,na.rm=T),
              indep2   = sum(indep2*fac,na.rm=T),
              IngEmp = survey_mean(IngEmp,na.rm=T), 
              IngInd = survey_mean(IngInd,na.rm=T), 
              HrsEmp = survey_mean(HrsEmp,na.rm=T), 
              HrsInd = survey_mean(HrsInd,na.rm=T), 
              informal = survey_mean(informal,na.rm=T)) %>% 
    select(-ends_with("se"))
  
  todos_años_priv <- bind_rows(todos_años_priv, fieston_priv)
  
  todos_años <- bind_rows(todos_años, fieston_sector)
  
}
beepr::beep()

# Unir y procesar ----
archivos <- list.files(paste0(dir_input, "otros/"), pattern = ".dta")
scn <- tibble()
for(archivo in archivos){
  lasttt <- stri_replace_all(archivo, fixed = ".dta", "")
  temp_scn <- read_dta(paste0(dir_input,"otros/", archivo), encoding = "utf8") %>% 
    mutate(sector = as.numeric(stri_extract_last(lasttt, regex = "[:digit:]+")))%>% 
    rename(año = anio)
  scn <- bind_rows(scn, temp_scn)
}

todos_años_scn <- todos_años %>% 
  left_join(scn ) %>% 
  mutate(
    PS = s/va,
    a_ = IngInd/IngEmp,
    b_ = HrsInd/HrsEmp,
    a_ = ifelse(is.na(a_), 0, a_),
    b_ = ifelse(is.na(b_), 0, b_),
    PLI1 = PS*(1+(1*indep/empleado)),
    PLI2 = PS*(1+(a_*indep/empleado)),
    PLI2b = PS*(1+(a_*b_*indep/empleado)),
  )


saveRDS(todos_años_scn, paste0(dir_output, "lab_share_sec.rds"))  

openxlsx::write.xlsx(todos_años_scn %>% 
                       select(-PLI1, -PLI2) %>% 
                       filter(!is.na(PS)), paste0(dir_output, "lab_share_sec.xlsx"), overwrite = T)

# Archivo PRIV ----
todos_años_priv_scn <- todos_años_priv %>% 
  left_join(read_dta(paste0(dir_input, "/otros/SCN_priv.dta")) %>% 
              rename(año=anio)) %>% 
  mutate(
    PS = s/va,
    a_ = IngInd/IngEmp,
    b_ = HrsInd/HrsEmp,
    a_ = ifelse(is.na(a_), 0, a_),
    b_ = ifelse(is.na(b_), 0, b_),
    PLI1 = PS*(1+(1*indep/empleado)),
    PLI2 = PS*(1+(a_*indep/empleado)),
    PLI2b = PS*(1+(a_*b_*indep/empleado)),
  )


openxlsx::write.xlsx(todos_años_priv_scn %>% 
                       select(-PLI1, -PLI2, -a_,-b_) %>% 
                       filter(!is.na(PS)), paste0(dir_output, "lab_share_priv.xlsx"), overwrite = T)

# Archivo Todos ----
lab_share_todos <- todos_años_priv_scn %>% 
  rename(sector= priv) %>% 
  bind_rows(todos_años_scn) %>% 
  mutate(
    sector = factor(sector, 
                    levels=c(1,11,21,23,31,43,48,51,
                             52,54,56,61,62,71,72,
                             81),
                    labels = c("Total privado","Primario","Minería",
                               "Construcción","Manufactura","Comercio",
                               "Transporte","Medios masivos",
                               "Servicios financieros",
                               "Servicios profesionales","Apoyo a los negocios",
                               "Educación","Salud","Cultura",
                               "Alojamiento temporal","Otros")
    )
  ) %>% 
  arrange(año, sector)

openxlsx::write.xlsx(lab_share_todos %>% 
                       select(-PLI1, -PLI2, -a_,-b_), paste0(dir_output, "lab_share_todos.xlsx"), overwrite = T)
