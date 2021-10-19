require(tidyverse)

# Set-up ----

# Aquí se eligen los folders donde están las bases
dir_input <- "2.Trabajar/7_MejoresTrabajadores/Bases/"
# Aquí se eligen los folders donde se van a guardar los resultados
dir_output <- "2.Trabajar/7_MejoresTrabajadores/output/"
# Aquí se eligen el nombre del excel de exportación
nombre_para_guardar_csv <- "mejores_trabajadores_procesada.csv"

if(!dir.exists(dir_output)){
  dir.create(dir_output)
}

mejores_raw <- read.csv(paste0(dir_input, "mejores_trabajadores.csv")) 
mejores_trabajadores_procesada <- mejores_raw %>% 
  mutate(cambio_productividad = 
           indice_productividad_laboral_horas_trabajadas_2019 -
           indice_productividad_laboral_horas_trabajadas_2005,
         cambio_costo_unitario_mano_obra = 
           indice_costo_unitario_mano_obra_horas_trabajadas_2019 -
           indice_costo_unitario_mano_obra_horas_trabajadas_2005
  )

write.csv(mejores_trabajadores_procesada,
          file = paste0(dir_output, nombre_para_guardar_csv),
          row.names = F)

ggplot(mejores_trabajadores_procesada,
       aes(x = cambio_productividad, y = cambio_costo_unitario_mano_obra,
           size  = personalremunerado, label = sector,
           )
)+geom_point(color = "grey", alpha = .5)+scale_size_continuous()+
  
  xlim(-100,100)+
  ylim(-100,100)
ggsave(paste0(dir_output, "mejores_trabajadores.png"), width = 21, height = 11, dpi = 100)
