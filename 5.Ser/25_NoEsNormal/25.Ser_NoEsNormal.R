# 05 21 clase media
require(haven)
require(tidyverse)
require(stringi)
require(Hmisc)
require(srvyr)
require(ggthemes)
require(hrbrthemes)
library(extrafont)
loadfonts(device = "win")

# Set-up ----

# Aquí se eligen los folders donde están las bases
dir_input <- "5.Ser/25_NoEsNormal/Bases/"
# Aquí se eligen los folders donde se van a guardar los resultados
dir_output <- "5.Ser/25_NoEsNormal/output/"
# Aquí se eligen el nombre del archivo de exportación final
nombre_para_guardar_xlsx <- "CuartilesGrafica.xlsx"
# Elegir si se importan los archivos de nuevo o se trabaja desde memoria (por lo menos correr la primera vez)
extraer_datos_denuevo <- F

if(!dir.exists(dir_output)){
  dir.create(dir_output)
}

coneval_enigh <- read_dta(paste0(dir_input, "ENIGH_CONEVAL_EC_2018.dta")) %>% 
  group_by(folioviv, foliohog) %>% 
  distinct(folioviv, foliohog, ict_evalua, E_mmip, factor, ubica_geo) %>% 
  mutate(ict_evalua = ict_evalua*1.23296187054653) %>% 
  left_join(tribble(
    ~E_mmip,~estratos,
    1      , "1. Pobreza muy alta" ,
    2      , "2. Pobreza alta" ,
    3      , "3. Pobreza moderada" ,
    4      , "4. Vulnerables" ,
    5      , "5. Clase media" ,
    6      , "6. Clase alta" 
  ))

coneval_enigh_collapse <- coneval_enigh %>% 
  ungroup() %>% 
  as_survey_design(weights = factor) %>% 
  group_by(estratos) %>% 
  summarise(cuantile = survey_quantile(x = ict_evalua, quantiles = c(0,.25,.5,.75),vartype = NULL, na.rm = T)) %>% 
  rename_at(vars(starts_with("cuantile")), ~stri_replace(., fixed= "cuantile_", ""))

openxlsx::write.xlsx(coneval_enigh_collapse, paste0(dir_output,nombre_para_guardar_xlsx), overwrite =T)


ttt <- coneval_enigh %>%
  ungroup() %>% 
  #uncount(factor) %>% 
  #head(1000) %>% 
  filter(E_mmip!= 6) 

ttt$random <- sample(1:100000, nrow(ttt), replace=T)
ttt$random <- ttt$random<(2000/(nrow(ttt)/100000))

prop.table(table(ttt$random))

ggplot(ttt , aes(x=estratos, 
             y=ict_evalua,
             fill="darkgrey",
             color = "darkgrey",
             weight = factor
             )) +
  geom_boxplot(outlier.shape = NA, alpha= .9) +
  geom_jitter(aes(y = ifelse(random, ict_evalua, NA)), alpha = .5)+
  theme_minimal() +
    scale_fill_grey()+
    scale_color_grey()+
    scale_y_continuous(limits = c(0,100000), labels = comma)+
  labs(
    title= "",
    subtitle = "",
    caption = "Libro Caption", 
    x = "Estrato",
    y = "Ingreso corriente total mensual per capita"
  ) +
  theme(axis.title.y = element_text(hjust = .5,size=20), 
        legend.position="none",
        plot.title = element_text(size=20),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20,hjust = .5)
  )

ggsave(paste0(dir_output,"box_plot.png"), width = 21, height = 11, dpi = 100)

