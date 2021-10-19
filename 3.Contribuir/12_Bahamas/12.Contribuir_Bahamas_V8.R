require(tidyverse)
require(hrbrthemes)
require(scales)
# Set-up ----

# Aquí se eligen los folders donde están las bases
dir_input <- "3.Contribuir/12_Bahamas/Bases/"
# Aquí se eligen los folders donde se van a guardar los resultados
dir_output <- "3.Contribuir/12_Bahamas/output/"

if(!dir.exists(dir_output)){
  dir.create(dir_output)
}

bahamas_raw <- read.csv(paste0(dir_input, "bahamas.csv")) 

ggplot(bahamas_raw,
       aes(x = str_wrap(Estrato, 15), 
           y = Total.a.deber,
           fill = Estrato,
           label = paste( "$",format(round(Total.a.deber,0), big.mark = ",", )),
       )
)+geom_col(show.legend = F)+ 
  geom_label(show.legend = F)+
  labs(x="", y = "Pesos", title = "Total a deber")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::label_dollar())
ggsave(paste0(dir_output, "bahamas_simple.png"), width = 21, height = 11, dpi = 100)



subtt = "La línea punteada es lo que se debería recaudar si el impuesto fuera proporcional al ingreso"
subtt = str_wrap(subtt, 70)
bahamas_raw %>% 
  pivot_longer(cols = -c(Estrato, Impuestos.totales,Total.a.deber,Distribución.ingreso.de.hogares),
               names_to = "tipo", values_to = "value") %>% #count(tipo)
  mutate(tipo=case_when(
    tipo == "Distribución.del.pago.de.Seguridad.social" ~  "Seguridad Social" ,
    tipo == "Distribución.del.pago.del.IEPS" ~          "IEPS",   
    tipo == "Distribución.del.pago.del.IVA" ~     "IVA",        
    T ~ NA_character_
#    tipo == "Distribución.ingreso.de.hogares" ~ "Lo que se debería recaudar",
  )) %>% 
  filter(!is.na(tipo)) %>% 
ggplot(aes(x = str_wrap(Estrato, 15), 
           y = value,
           fill = tipo,
           label = paste(round(value*100,0), "%"),
       )
)+geom_col(position = position_dodge(width = 1))+
  geom_label(position = position_dodge(width = 1),show.legend = F)+
  geom_segment(aes(x = as.numeric(stri_extract_first(Estrato, regex = "[:digit:]+"))-.5, y = Distribución.ingreso.de.hogares, yend = Distribución.ingreso.de.hogares,
                   xend = as.numeric(stri_extract_first(Estrato, regex = "[:digit:]+"))+.5), color = "red", linetype = 2)+
  labs(x="", y = "Pesos",fill="", title = "Total a deber", subtitle = subtt)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_percent(limits = c(0,.6))+
  scale_x_discrete(expand = c(0, 0.5))
ggsave(paste0(dir_output, "bahamas_impuestos.png"), width = 21, height = 11, dpi = 100)
