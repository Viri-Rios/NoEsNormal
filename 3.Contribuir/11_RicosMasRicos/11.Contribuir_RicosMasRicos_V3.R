# 3_12 Ricos Más Ricos

require(tidyverse)
require(openxlsx)
require(multidplyr)
require(stringi)
require(beepr)
require(zip)

require(tidymodels)
require(dotwhisker)
require(estimatr)
# Set-up ----

`%+%` <- paste0
# Aquí se eligen los folders donde están las bases
dir_input <- "3.Contribuir/11_RicosMasRicos/Bases/"
# Aquí se eligen los folders donde se van a guardar los resultados
dir_output <- "3.Contribuir/11_RicosMasRicos/output/"
# Elegir si se importan los archivos de nuevo o se trabaja desde memoria (por lo menos correr la primera vez)
descargar_datos_denuevo <- F

if(!dir.exists(dir_output)){
  dir.create(dir_output)
}
# Descargar ----
if (descargar_datos_denuevo) {
  for (i in 2013:2015) {
    url <- "http://omawww.sat.gob.mx/cifras_sat/Documents/Anuales_ISR_PF_"%+%i%+%".zip"
    if(!file.exists(dir_input %+%"downloadF/ruido"%+% i%+%".zip")){
      print(url)
      download.file(url, destfile = dir_input%+%"downloadF/ruido" %+% i%+%".zip",method = "curl")
      zip::unzip(dir_input%+%"downloadF/ruido" %+% i%+%".zip", exdir = dir_input, )
    }
  }
  for (i in 2013:2015) {
    url <- "https://wu1aqauatsta003.blob.core.windows.net/lineamientosarticulo19lif/agp/DIM/F30a1/"%+%i%+%"/F30a1_"%+%i%+%".zip"
    if(file.exists(dir_input %+%"downloadF/F30a1_"%+% i%+%".zip")){
      print(url)
      #download.file(url, destfile = dir_input%+%"downloadF/F30a1_" %+% i%+%".zip",method = "curl")
      zip::unzip(dir_input%+%"downloadF/F30a1_" %+% i%+%".zip", exdir = dir_input)
    }
  }
  for (añoi in 2013:2015) {
    cluster <- new_cluster(16)
    cluster_library(cluster, "dplyr")
    RUIDO_FINAL_PF <- read_delim(paste0(dir_input, "RUIDO_FINAL_PF_",añoi,".tab"),
                                 delim = "\t", escape_double = FALSE, 
                                 trim_ws = TRUE) %>% 
      select(id = RFC_ANON,
             ingresos = I_DEC_TIAONCT1_AA,
             causados = I_DEC_IRCEMEA1_AA)
    
    dim <- read_delim(paste0("3.Contribuir/11_RicosMasRicos/Bases/F30a1_",añoi,".txt"), 
                      delim = "|", escape_double = FALSE, trim_ws = TRUE) %>% 
      select(id = Identificador,
             ingresos=i_c1_300101_AA,
             causados=i_c1_301601_AA)
    
    tt <- RUIDO_FINAL_PF %>% 
      bind_rows(dim) %>% 
      group_by(id) %>% 
      partition(cluster)
    
    tt <-tt  %>% 
      summarise(across(c(ingresos, causados), ~sum(.,na.rm = T)))%>% 
      rename_at(vars(c(ingresos, causados)), ~paste0(.,"_", añoi))%>% 
      collect() 
    
    saveRDS(tt, paste0(dir_output, "merge", añoi,".rds"))
    rm(tt, dim, RUIDO_FINAL_PF, cluster)
    gc()
  }
  beep()
}


# Procesamiento ----

all <- readRDS(paste0(dir_output, "merge", 2015,".rds")) %>% 
  full_join(readRDS(paste0(dir_output, "merge", 2014,".rds"))) %>% 
  full_join(readRDS(paste0(dir_output, "merge", 2013,".rds")))

beep()

impuestos <- all %>%
  pivot_longer(cols = starts_with(c("ingresos", "causado")),
               names_sep = "_", names_to = c("tipo", "año"))%>% 
  group_by(tipo, id) %>% 
  summarise(value = mean(value, na.rm=T)) %>%
  pivot_wider(names_from = tipo, values_from = value, names_prefix = "mean_") %>% 
  filter(mean_ingresos>0)%>% 
  filter(!is.na(mean_ingresos))

todos_por_Q <- impuestos %>% 
  arrange(mean_ingresos) %>% 
  rowid_to_column(var = "sorted") %>% 
  mutate(#sorted = sort.list(mean_ingresos, decreasing = F),
         decil = 1)
  
max = max(todos_por_Q$sorted)
for (maxQ in c(10,100,1000,10000,100000)){
    print("doing for maxQ: " %+% maxQ)
    against = maxQ/10
    todos_por_Q <- todos_por_Q %>% 
      mutate(decil = ifelse(decil == against, 
                            floor(sorted/(max/maxQ))+1,
                            decil
                            ),
             decil = ifelse(decil > maxQ, decil-1, decil)
             )
}

todos_por_Q2 <- todos_por_Q %>%
  mutate(
    cuantil = case_when(
      decil < 10     ~ decil,
      decil < 100    ~ 10,
      decil < 1000   ~ 11,
      decil < 10000  ~ 12,
      decil < 100000 ~ 13,
      T ~ 14
    )
  )

saveRDS(todos_por_Q2, paste0(dir_output, "total_append13_15.rds"))

intermedia <- todos_por_Q2 %>% 
  mutate(tasaefectiva = mean_causados*100/mean_ingresos,
         tasaefectiva = ifelse(tasaefectiva>100, 100,tasaefectiva),
         tasalegal = case_when(
           mean_ingresos >= 5952.85 & mean_ingresos <= 50524.92 ~6.50,     
           mean_ingresos >= 50524.93 & mean_ingresos <= 88793.04 ~10.88, 
           mean_ingresos >= 88793.05 & mean_ingresos <= 103218.00 ~16, 
           mean_ingresos >= 103218.01 & mean_ingresos <= 123580.20 ~17.92, 
           mean_ingresos >= 123580.21 & mean_ingresos <= 249243.48 ~21.36, 
           mean_ingresos >= 249243.49 & mean_ingresos <= 392841.96 ~23.52, 
           mean_ingresos >= 392841.97 & mean_ingresos <= 750000.00 ~30, 
           mean_ingresos >= 750000.01 & mean_ingresos <= 1000000.00 ~32, 
           mean_ingresos >= 1000000.01 & mean_ingresos <= 3000000.00 ~34, 
           mean_ingresos >= 3000000.01 ~ 35, 
           T ~ 1.92
         ),
         impuesto_potencial = mean_ingresos*tasalegal/100,
         evasion = impuesto_potencial - mean_causados,
         tasacero = ifelse(tasaefectiva == 0, 1, 0),
         tasauno = ifelse(tasaefectiva >= 100, 1, 0),
  ) 

total_append13_15_final_ctl <- intermedia %>% 
  group_by(cuantil) %>% 
  summarize(
    cuenta = n(),
    t_ef_media = mean(tasaefectiva, na.rm=T),
    t_ef_stdv = sd(tasaefectiva, na.rm=T),
    t_ef_maximo = max(tasaefectiva, na.rm=T),
    t_ef_minimo = min(tasaefectiva, na.rm=T),
    q1 = quantile(tasaefectiva, probs = .01,na.rm=T),
    q2 = quantile(tasaefectiva, probs = .25,na.rm=T),
    q3 = quantile(tasaefectiva, probs = .5 ,na.rm=T),
    q4 = quantile(tasaefectiva, probs = .75,na.rm=T),
    ingresos = mean(mean_ingresos, na.rm = T),
    causados = mean(mean_causados, na.rm = T),
    tasalegal = mean(tasalegal, na.rm = T),
    impuesto_potencial = mean(impuesto_potencial, na.rm = T),
    evasion = mean(evasion, na.rm = T),
    tasacero = mean(tasacero, na.rm = T),
    tasauno = mean(tasauno, na.rm = T),
  )

write.xlsx(total_append13_15_final_ctl,
           paste0(dir_output, "total_append13_15_final_ctl.xlsx"), 
           overwrite =T)

# Regresión ----
data_regresion <- intermedia %>% 
  mutate(tasaefectiva = mean_causados*100/mean_ingresos,
         millonarios = ifelse(decil > 999, 1, 0))
  
lm_fit <- lm_robust(tasaefectiva ~ 
                      mean_ingresos + millonarios, 
                    data = data_regresion,
                    se_type = "HC1")

summary(lm_fit)

general_resultados <- tidy(lm_fit, conf.int = T)  

general_resultados %>% 
  dwplot(dot_args = list(size = 2#, color = "black"
                         ),
         #whisker_args = list(color = "darkgrey"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +
  facet_wrap(~term, scales = "free_x")+
  scale_x_continuous(labels = comma)

openxlsx::write.xlsx(general_resultados, paste0(dir_output, "resultados_reg.xlsx"), overwrite = T)
