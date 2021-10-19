require(tidyverse)
require(hrbrthemes)
require(scales)
# Set-up ----

# Aquí se eligen los folders donde están las bases
dir_input <- "1.Competir/2_Exitos/Bases/"
# Aquí se eligen los folders donde se van a guardar los resultados
dir_output <- "1.Competir/2_Exitos/output/"
# Aquí se eligen el nombre del excel de exportación
nombre_para_guardar_csv <- "mejores_trabajadores_procesada.csv"

if(!dir.exists(dir_output)){
  dir.create(dir_output)
}

exitos_raw <- read.csv(paste0(dir_input, "exitos.csv"))

exitos_procesada <- exitos_raw %>% pivot_longer(cols = starts_with("año"),
                            names_to = "año", names_prefix = "año_",
                            values_to = "value") %>% 
  mutate(año=as.numeric(año),
         value = ifelse(is.na(value), 0,value)) %>% 
  arrange(empresa, año) %>% 
  group_by(empresa) %>% 
  mutate(id = data.table::rleid(value)) %>% 
  group_by(empresa,id) %>% 
  mutate(años_corrientes  = cumsum(value)) %>% 
  mutate(años_corrientes = ifelse(años_corrientes==0,NA, años_corrientes)) %>% 
  #ungroup() %>% 
  #select(-id) %>% 
  #pivot_wider(id_cols = empresa, names_from = año, values_from = años_corrientes)
  group_by(año) %>% 
  summarise(promedio_años_corrientes = sum(años_corrientes, na.rm=T)/20)


ggplot(exitos_procesada, 
       aes(x = año,
           y = promedio_años_corrientes)) + 
  geom_col()+
  labs(x="", y = "",
       title = str_wrap("Años consecutivos en los que las empresas del Top-20 se han mantenido en él",50))+
  xlim(1985,NA)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))
ggsave(paste0(dir_output, "top20_exitos.png"), width = 21, height = 11, dpi = 100)
