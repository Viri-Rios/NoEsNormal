# 3_15 Edad
require(haven)
require(tidyverse)
require(openxlsx)



# Set-up ----

# Aquí se eligen los folders donde están las bases
dir_input <- "3.Contribuir/15_Edad/Bases/"
# Aquí se eligen los folders donde se van a guardar los resultados
dir_output <- "3.Contribuir/15_Edad/output/"

if(!dir.exists(dir_output)){
  dir.create(dir_output)
}

predial <- read.xlsx(paste0(dir_input, "ValorAgregadoMunicipios2019.xlsx")) %>% 
  rename(va = VA, clave_mun = municip) %>% 
  mutate(va = va*1000, 
         clave_ent = substr(clave_mun, 1,2)) %>% 
  left_join(read_dta(paste0(dir_input,"predial2.dta"), encoding = NULL) ) %>% 
  #filter(!is.na(predial2018)) %>% 
  mutate(predial_potencial = va*0.011,
         predial_perdido = (predial_potencial - predial2018)/1000000) %>% 
  left_join(read_dta(paste0(dir_input,"muni_tot2020.dta"), encoding = NULL) ) %>% 
  arrange(clave_mun, va, predial2018)


openxlsx::write.xlsx(predial, paste0(dir_output, "grafica.xlsx"), overwrite = T)


predial %>% 
  mutate(pob_predial0 = case_when(
    predial2018 == 0  ~ 1,
    is.na(predial2018) ~ NA_real_,
    T ~ 0
  )) %>% 
  group_by(pob_predial0) %>% 
  summarise(pob = sum(pob, na.rm = T)) %>% 
  openxlsx::write.xlsx(paste0(dir_output, "PoblacionPredia0.xlsx"), overwrite = T)

predial %>% 
  arrange(predial_perdido) %>% 
  mutate(orden = sort.list(predial_perdido, decreasing = T)) %>% 
  filter(orden<100) %>% 
  mutate(cuenta = 1) %>% 
  group_by(estado) %>% 
  summarise(cuenta = sum(cuenta, na.rm = T)) %>% 
  arrange(desc(cuenta)) %>% 
  openxlsx::write.xlsx(paste0(dir_output, "ConcentracionEnEstados.xlsx"), overwrite = T)

predial %>%
  mutate(pob_predial_bien = case_when(
    predial_perdido < 0 ~ 1,
    is.na(predial_perdido) ~ NA_real_,
    T ~ 0
  )) %>% 
  group_by(pob_predial_bien) %>% 
  summarise(pob = sum(pob, na.rm = T)) %>% 
  openxlsx::write.xlsx(paste0(dir_output, "PoblacionPrediaBien.xlsx"), overwrite = T)

predial %>%
  select(clave_mun, estado, mun, pob, va, predial2018, predial_perdido) %>% 
  ungroup() %>% 
  arrange(desc(predial2018)) %>% 
  mutate(cuenta = 1,
         pred_tot = sum(predial2018, na.rm = T),
         pred_pct = predial2018/pred_tot,
         pct_acum = cumsum(pred_pct)
         ) %>% 
  filter(pct_acum < 0.51) %>% 
  arrange(estado, desc(predial2018)) %>% 
  group_by(estado) %>% 
  mutate(mun_x_estado = sum(cuenta, na.rm = T)) %>% 
  select(-pred_pct,-pred_tot, -cuenta) %>% 
  arrange(desc(predial2018)) %>% 
  openxlsx::write.xlsx(paste0(dir_output, "Municipios50pct.xlsx"), overwrite = T)
