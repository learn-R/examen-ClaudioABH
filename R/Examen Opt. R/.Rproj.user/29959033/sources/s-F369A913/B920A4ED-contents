##Examen Opt. Introducción a R
##Prof. Valentina Andrade
##Ayudante Dafne Jaime
####Claudio Bustos Hodges
##05/12/2022

# DIRECTORIO DE TRABAJO ---------------------------------------------------
setwd("C:/Users/claudio/Desktop/CLASES OPT. R/02-clase/R/examen-ClaudioABH/Input")

# PAQUETES ----------------------------------------------------------------
pacman::p_load(sjmisc,
               sjPlot,
               tidyverse,
               haven,
               dplyr,
               ggplot2,
               srvyr,
               tidyr,
               survey,
               plotrix)

# BASE DE DATOS -----------------------------------------------------------
data_original_ENUSC <- read_sav("base-usuario-18-enusc-2021-sav.sav")

# SELECCIÓN DE VARIABLES --------------------------------------------------
data_a_usar <- select(data_original_ENUSC, P4_1_1, P1_1_1, rph_edad, rph_sexo, enc_region, rph_ID)

# RENOMBRE DE VARIABLES ---------------------------------------------------
names(data_a_usar) = c("victimizacion", "percepcion_delincuencia", "edad", "sexo", "region", "ID")

# EXPLORACIÓN DE DATOS ----------------------------------------------------
descr(data_a_usar$victimizacion)
descr(data_a_usar$percepcion_delincuencia)
descr(data_a_usar$edad)
descr(data_a_usar$sexo)
descr(data_a_usar$region)
descr(data_a_usar$region)

# FILTRO EDAD -----------------------------------------------------
data_a_usar$edad <- na_if(data_a_usar$edad, 0)
data_a_usar$edad <- na_if(data_a_usar$edad, 1)
data_a_usar$edad <- na_if(data_a_usar$edad, 10)
table(data_a_usar$edad)
data_a_usar <- mutate(data_a_usar, edad = case_when(edad>=2 & edad==3 & edad<=4~"Entre 20 y 39 años",
                                                    edad>=5 & edad==6 & edad<=7~"Entre 40 y 69 años"))
table(data_a_usar$edad)

# FILTRO Y RECODIFICACIÓN REGIÓN ---------------------------------------------------
data_a_usar$region <- na_if(data_a_usar$region, 1)
data_a_usar$region <- na_if(data_a_usar$region, 2)
data_a_usar$region <- na_if(data_a_usar$region, 3)
data_a_usar$region <- na_if(data_a_usar$region, 4)
data_a_usar$region <- na_if(data_a_usar$region, 5)
data_a_usar$region <- na_if(data_a_usar$region, 6)
data_a_usar$region <- na_if(data_a_usar$region, 13)
data_a_usar$region <- na_if(data_a_usar$region, 15)
table(data_a_usar$region)
data_a_usar <- mutate(data_a_usar, region = case_when(region==7~"VII",
                                                      region==8~"VIII",
                                                      region==9~"IX",
                                                      region==10~"X",
                                                      region==11~"XI",
                                                      region==12~"XII",
                                                      region==14~"XIV",
                                                      region==16~"XVI"))
table(data_a_usar$region)

# FILTRO VICTIMIZACIÓN ------------------------------------------------------------
data_a_usar$victimizacion <- na_if(data_a_usar$victimizacion, 88)
data_a_usar$victimizacion <- na_if(data_a_usar$victimizacion, 99)
table(data_a_usar$victimizacion)

# FILTRO PERCEPCIÓN DELINCUENCIA ------------------------------------------
data_a_usar$percepcion_delincuencia <- na_if(data_a_usar$percepcion_delincuencia, 88)
data_a_usar$percepcion_delincuencia <- na_if(data_a_usar$percepcion_delincuencia, 99)
table(data_a_usar$percepcion_delincuencia)

# OBJETO ENCUESTA ---------------------------------------------------------
ENUSC <- data_a_usar %>% 
  as_survey_design(id=ID,
                   strata = )

# PERCEPCIÓN DELINCUENCIA -------------------------------------------------

# ANÁLISIS PERCEPCIÓN DELINCUENCIA-EDAD -----------------------------------
ENUSC %>% 
  group_by(percepcion_delincuencia, edad) %>% 
  summarise(prop = survey_prop(vartype = "ci", level = .99, na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

##Tabla de contingencia delincuencia-edad
sjt.xtab(data_a_usar$edad,data_a_usar$percepcion_delincuencia,
         show.col.prc = TRUE,
         show.summary = FALSE,
         encoding= "UTF-8",
         tittle= "tabla de contingencia delincuencia-edad")

##Gráfico delincuencia-edad
plot_xtab(data_a_usar$edad, data_a_usar$percepcion_delincuencia,
            type = "bar", title = "Gráfico delincuencia-edad")

# ANÁLISIS PERCEPCIÓN DELINCUENCIA-SEXO -----------------------------------
ENUSC %>% 
  group_by(percepcion_delincuencia, sexo) %>% 
  summarise(prop = survey_prop(vartype = "ci", level = .99, na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup() 

##Tabla de contingencia percepción delincuencia-sexo
sjt.xtab(data_a_usar$sexo, data_a_usar$percepcion_delincuencia,
         show.col.prc = TRUE,
         show.summary = FALSE,
         encoding= "UTF-8",
         tittle= "tabla de contingencia delincuencia-sexo")

##Gráficos percepción delincuencia-sexo
plot_xtab(data_a_usar$sexo, data_a_usar$percepcion_delincuencia, 
          type = "bar", title = "Gráfico delincuencia-sexo")

# ANÁLISIS PERCEPCIÓN DELINCUENCIA-REGIÓN --------------------------------------------
ENUSC %>% 
  group_by(percepcion_delincuencia, region) %>% 
  summarise(prop = survey_prop(vartype = "ci", level = .99, na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

##Tabla de contingencia percepción delincuencia-región
sjt.xtab(data_a_usar$region, data_a_usar$percepcion_delincuencia,
         show.col.prc = TRUE,
         show.summary = FALSE,
         encoding= "UTF-8",
         tittle= "tabla de contingencia delincuencia-región")

##Gráfico percepción delincuencia-región
plot_xtab(data_a_usar$region, data_a_usar$percepcion_delincuencia,
            type = "bar", title = "Gráfico delincuencia-región")

# VICTMIZACIÓN ------------------------------------------------------------
 
# ANÁLISIS SEXO-VICTIMIZACIÓN ---------------------------------------------
ENUSC %>% 
  group_by(victimizacion, sexo) %>% 
  summarise(prop = survey_prop(vartype = "ci", level = .99, na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

##Tabla de contingencia sexo-victimización
sjt.xtab(data_a_usar$sexo,data_a_usar$victimizacion,
         show.col.prc = TRUE,
         show.summary = FALSE,
         encoding= "UTF-8",
         tittle= "tabla de contingencia victimización-sexo")

##Gráfico sexo-victimización
plot_xtab(data_a_usar$sexo, data_a_usar$victimizacion, 
          type = "bar", title = "Gráfico victimización-sexo")

# ANÁLISIS EDAD-VICTIMIZACIÓN --------------------------------------------
ENUSC %>% 
  group_by(victimizacion, edad) %>% 
  summarise(prop = survey_prop(vartype = "ci", level = .99, na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

##Tabla de contingencia edad-victimización
sjt.xtab(data_a_usar$edad,data_a_usar$victimizacion,
         show.col.prc = TRUE,
         show.summary = FALSE,
         encoding= "UTF-8",
         tittle= "tabla de contingencia victimización-edad")

##Gráfico edad-victimización
plot_xtab(data_a_usar$edad, data_a_usar$victimizacion, 
          type = "bar", title = "Gráfico victimización-edad")

# ANÁLISIS REGIÓN-VICTIMIZACIÓN -------------------------------------------
ENUSC %>% 
  group_by(victimizacion, region) %>% 
  summarise(prop = survey_prop(vartype = "ci", level = .99, na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

##Tabla de contingencia región-victimización
sjt.xtab(data_a_usar$region,data_a_usar$victimizacion,
         show.col.prc = TRUE,
         show.summary = FALSE,
         encoding= "UTF-8",
         tittle= "tabla de contingencia victimización-región")

##Gráfico región-victimización
plot_xtab(data_a_usar$region, data_a_usar$victimizacion, 
          type = "bar", title = "Gráfico victimización-región")

# DATOS PROCESADOS --------------------------------------------------------
save(ENUSC, data_a_usar, file = "C:/Users/claudio/Desktop/CLASES OPT. R/02-clase/R/examen-ClaudioABH/Output/data/datos_proc.rds")
save(ENUSC, data_a_usar, file = "C:/Users/claudio/Desktop/CLASES OPT. R/02-clase/R/examen-ClaudioABH/Output/data/datos_proc.RData")


