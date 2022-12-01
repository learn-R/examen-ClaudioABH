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
data_a_usar <- select(data_original_ENUSC, P2_1_1, P1_1_1, rph_edad, rph_sexo, enc_region, rph_ID)

# RENOMBRE DE VARIABLES ---------------------------------------------------
names(data_a_usar) = c("fuente_informacion", "percepcion_delincuencia", "edad", "sexo", "region", "ID")

# EXPLORACIÓN DE DATOS ----------------------------------------------------
descr(data_a_usar$fuente_informacion)
descr(data_a_usar$percepcion_delincuencia)
descr(data_a_usar$edad)
descr(data_a_usar$sexo)
descr(data_a_usar$region)
descr(data_a_usar$region)

# RECODIFICACIÓN Y FILTRO EDAD -----------------------------------------------------
data_a_usar$edad <- na_if(data_a_usar$edad, 0)
data_a_usar$edad <- na_if(data_a_usar$edad, 1)
data_a_usar$edad <- na_if(data_a_usar$edad, 10)
table(data_a_usar$edad)

# FILTRO REGIÓN ---------------------------------------------------
data_a_usar$region <- na_if(data_a_usar$region, 1)
data_a_usar$region <- na_if(data_a_usar$region, 2)
data_a_usar$region <- na_if(data_a_usar$region, 3)
data_a_usar$region <- na_if(data_a_usar$region, 4)
data_a_usar$region <- na_if(data_a_usar$region, 5)
data_a_usar$region <- na_if(data_a_usar$region, 6)
data_a_usar$region <- na_if(data_a_usar$region, 13)
data_a_usar$region <- na_if(data_a_usar$region, 15)
table(data_a_usar$region)

# FILTRO FUENTE DE INFORMACIÓN ---------------------------------------
data_a_usar$fuente_informacion <- na_if(data_a_usar$fuente_informacion, 1)
data_a_usar$fuente_informacion <- na_if(data_a_usar$fuente_informacion, 2)
data_a_usar$fuente_informacion <- na_if(data_a_usar$fuente_informacion, 3)
data_a_usar$fuente_informacion <- na_if(data_a_usar$fuente_informacion, 77)
table(data_a_usar$fuente_informacion)
