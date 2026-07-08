#####################################################################################################################
# Trabajamos con la EPH individuos - 4° trimestre de 2025
#####################################################################################################################

# Remover objetos del ambiente
rm(list = ls())

# Librerías
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(ggthemes)
library(gt)


###############################################
### BASE  y  PREGUNTA DE INVESTIGACION   ###
###############################################

#setwd("...")
setwd ("...")

base <- read_excel("usu_individual_T425.xlsx")

# Forzamos a que sean números
base$AGLOMERADO <- as.numeric(base$AGLOMERADO)
base$CH06 <- as.numeric(base$CH06)
base$PP04A <- as.numeric(base$PP04A)
base$PP07A <- as.numeric(base$PP07A)
base$P21 <- as.numeric(base$P21)

#Filtramos una base solo para los datos de Tucuman , para mayores de 18 años.
base_tucuman <- base[which(base$AGLOMERADO == 29 & base$CH06 >= 18 & base$CH06 <= 70), ]

# Crear sector_tipo: 1 estatal, 0 privado,
base_tucuman$sector_tipo <- NA 
base_tucuman$sector_tipo[base_tucuman$PP04A == 1] <- 1  # Estatal
base_tucuman$sector_tipo[base_tucuman$PP04A == 2] <- 0  # Privado


  


# Gente que tenga ingresos mayores a 0
base_analisis <- base_tucuman %>%
  filter(P21 > 0) # Nos quedamos solo con los que declararon ingresos

#Checkeo
nrow(base_tucuman) 
nrow(base_analisis) 
table(base_analisis$sector_tipo) 

# Crear anios_antiguedad: 
# Creamos la variable numérica basada en la imagen
base_analisis <- base_analisis %>%
  mutate(AANT = case_when(
    PP07A == 1 ~ 0.1,  # Menos de 1 mes (representación en años)
    PP07A == 2 ~ 0.2,  # 1 a 3 meses
    PP07A == 3 ~ 0.4,  # 3 a 6 meses
    PP07A == 4 ~ 0.7,  # 6 a 12 meses
    PP07A == 5 ~ 3.0,  # 1 a 5 años (punto medio)
    PP07A == 6 ~ 10.0, # Más de 5 años (valor proxy de carrera)
    TRUE ~ NA_real_    # El código 9 y otros quedan como vacíos
  ))


###############################################
###  Análisis exploratorio y descriptivo ###
###############################################

# 1.
install.packages("gt")
install.packages("webshot2") 
library(tidyverse)
library(gt)
library(webshot2)
library(tidyverse)

# 1. Calculamos los datos
tabla_datos <- base_analisis %>%
  select(Ingreso = P21, Antigüedad = AANT, Edad = CH06) %>%
  summarise(across(everything(), list(
    Media = ~mean(.x, na.rm = TRUE),
    Mediana = ~median(.x, na.rm = TRUE),
    Desvio = ~sd(.x, na.rm = TRUE),
    Min = ~min(.x, na.rm = TRUE),
    Max = ~max(.x, na.rm = TRUE),
    N = ~sum(!is.na(.x))
  ))) %>%
  pivot_longer(everything(), names_to = c("Variable", "Estadistico"), names_sep = "_") %>%
  pivot_wider(names_from = Estadistico, values_from = value)

# 2. Redondeamos los números para que no tengan mil decimales
tabla_datos <- tabla_datos %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

# 3. VER LA TABLA EN LA CONSOLA (Para copiar los números)
print(tabla_datos)

# 4. EXPORTAR A EXCEL (CSV)
# Esto va a crear un archivo en tu carpeta de trabajo
write.csv2(tabla_datos, "Tabla_2_Descriptivos.csv", row.names = FALSE)

#-----------Graaficos..


# Grafico 1: Distribución del Ingreso (Variable Dependiente)
ggplot(base_analisis, aes(x = P21)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(title = "Distribución del Ingreso de la Ocupación Principal",
       subtitle = "Aglomerado Tucumán - 4to Trimestre 2025",
       x = "Ingreso ($)", y = "Densidad",
       caption = "Fuente: Elaboración propia en base a EPH-INDEC") +
  theme_minimal()

#Estudiamos OUTLIERS
# Los 10 ingresos más ALTOS
top_10_ingresos <- base_analisis %>%
  select(P21, sector_tipo, CH06, AANT) %>% # Seleccionamos columnas clave para ver el contexto
  arrange(desc(P21)) %>%                  # Ordenamos de mayor a menor
  head(10)                                # Tomamos los primeros 10

# Los 10 ingresos más BAJOS
bottom_10_ingresos <- base_analisis %>%
  select(P21, sector_tipo, CH06, AANT) %>% 
  arrange(P21) %>%                        # Ordenamos de menor a mayor
  head(10)

# Ver en consola
print(top_10_ingresos)
print(bottom_10_ingresos)


# 1. Preparamos los datos: convertimos los números a etiquetas de texto
# Esto es para que en el eje X diga "1 a 5 años" y no "3"
base_grafico <- base_analisis %>%
  filter(!is.na(AANT)) %>% 
  mutate(rango_antiguedad = case_when(
    AANT == 0.1 ~ "Menos de 1 mes",
    AANT == 0.2 ~ "1 a 3 meses",
    AANT == 0.4 ~ "3 a 6 meses",
    AANT == 0.7 ~ "6 a 12 meses",
    AANT == 3.0 ~ "1 a 5 años",
    AANT == 10.0 ~ "Más de 5 años"
  )) %>%
  # Forzamos el orden correcto de las barras (de menor a mayor tiempo)
  mutate(rango_antiguedad = factor(rango_antiguedad, 
                                   levels = c("Menos de 1 mes", "1 a 3 meses", "3 a 6 meses", 
                                              "6 a 12 meses", "1 a 5 años", "Más de 5 años")))

# 2. Creamos el gráfico de barras
ggplot(base_grafico, aes(x = rango_antiguedad)) +
  geom_bar(fill = "darkgreen", alpha = 0.7) +
  # Agregamos etiquetas con la cantidad de personas sobre cada barra
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size=3.5) +
  labs(title = "Gráfico 2: Distribución de la Antigüedad Laboral",
       subtitle = "Trabajadores ocupados - Aglomerado Tucumán (4to Trim 2025)",
       x = "Rango de Antigüedad", 
       y = "Cantidad de Trabajadores (N)",
       caption = "Fuente: Elaboración propia en base a EPH-INDEC") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Inclinamos las letras para que se lean bie

# Grafico 3: Distribución de la Edad
ggplot(base_analisis, aes(x = CH06)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(title = "Distribución de la Edad de los Trabajadores",
       subtitle = "Aglomerado Tucumán - 4to Trimestre 2025",
       x = "Edad (años)", y = "Densidad",
       caption = "Fuente: Elaboración propia en base a EPH-INDEC") +
  theme_minimal()


##2.3
# 1. Tabla de Medias y Desvíos por Grupo (Público vs Privado)
tabla_comparativa <- base_analisis %>%
  group_by(sector_tipo) %>%
  summarise(
    Media_Ingreso = round(mean(P21, na.rm = TRUE),0),
    Mediana_Ingreso = round(median(P21, na.rm = TRUE),0),
    Desvio_Std = round(sd(P21, na.rm = TRUE),0),
    Casos = n()
  ) %>%
  mutate(Sector = ifelse(sector_tipo == 1, "Público", "Privado"))

print(tabla_comparativa)
write.csv2(tabla_comparativa, "Tabla_2_3_Sectores_Original.csv", row.names = FALSE)

# 2. Boxplot de Ingreso por Sector
# Usamos coord_cartesian para "hacer zoom" y que el outlier de 15 millones no aplaste el gráfico
ggplot(base_analisis, aes(x = factor(sector_tipo), y = P21, fill = factor(sector_tipo))) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 1) +
  scale_x_discrete(labels = c("0" = "Privado", "1" = "Público")) +
  scale_fill_manual(values = c("skyblue", "orange"), labels = c("Privado", "Público")) +
  # El zoom es clave para ver las cajas:
  coord_cartesian(ylim = c(0, 2500000)) + 
  labs(title = "Gráfico 3: Distribución del Ingreso según Sector",
       subtitle = "Comparación Sector Público vs. Privado en Tucumán",
       x = "Sector de pertenencia",
       y = "Ingreso de la ocupación principal ($)",
       fill = "Sector",
       caption = "Fuente: Elaboración propia en base a EPH-INDEC. Nota: Eje Y limitado a $2.5M para visualización.") +
  theme_minimal()


############################
###### Adicionales ########
#########################

# --- 2. FILTRAR EL OUTLIER DE 15 MILLONES ---
# Creamos una base nueva sin ese valor para comparar
base_sin_outlier <- base_analisis %>% 
  filter(P21 < 15000000)


# --- 3. NUEVA TABLA DE MEDIAS (SIN EL OUTLIER) ---
tabla_comparativa_limpia <- base_sin_outlier %>%
  group_by(sector_tipo) %>%
  summarise(
    Media_Ingreso = round(mean(P21, na.rm = TRUE),0),
    Mediana_Ingreso = round(median(P21, na.rm = TRUE),0),
    Desvio_Std = round(sd(P21, na.rm = TRUE),0),
    Casos = n()
  ) %>%
  mutate(Sector = ifelse(sector_tipo == 1, "Público", "Privado"))

# Imprimir para ver la diferencia en la consola
print("--- TABLA CON OUTLIER ---")
print(tabla_comparativa)
print("--- TABLA SIN OUTLIER (Limpia) ---")
print(tabla_comparativa_limpia)


# --- 4. EXPORTAR TABLA LIMPIA A EXCEL (CSV) ---
write.csv2(tabla_comparativa_limpia, "Tabla_2_3_Sectores_Limpia.csv", row.names = FALSE)




################################
# TEST ESTADISTICOS 
################################

### TEST DE UNA COLA
# --- PASO 3: Estadísticos muestrales ---
n <- sum(!is.na(base_sin_outlier$P21))
media_muestral <- mean(base_sin_outlier$P21, na.rm = TRUE)
desvio_muestral <- sd(base_sin_outlier$P21, na.rm = TRUE)
mu_0 <- 700000 # Valor de referencia
error_estandar <- desvio_muestral / sqrt(n)

# --- PASO 4: Estadístico t observado ---
t_obs <- (media_muestral - mu_0) / error_estandar

# --- PASO 5: Valor crítico (al 5% de significatividad, una cola derecha) ---
alfa <- 0.05
t_critico <- qt(1 - alfa, df = n - 1)

# --- PASO 6: p-valor ---
p_valor <- pt(t_obs, df = n - 1, lower.tail = FALSE)

# IMPRIMIR RESULTADOS PARA EL INFORME 3.1
cat("RESULTADOS 3.1 (UNA COLA):\n",
    "n:", n, "\n",
    "Media:", media_muestral, "\n",
    "Desvío:", desvio_muestral, "\n",
    "Error Estándar:", error_estandar, "\n",
    "t observado:", t_obs, "\n",
    "t crítico:", t_critico, "\n",
    "p-valor:", p_valor)

# --- PASO 9: Verificación con t.test ---
t.test(base_sin_outlier$P21, mu = mu_0, alternative = "greater")


#### test de dos colas
# --- PASO 3: Estadísticos muestrales ---
n_edad <- sum(!is.na(base_sin_outlier$CH06))
media_edad <- mean(base_sin_outlier$CH06, na.rm = TRUE)
desvio_edad <- sd(base_sin_outlier$CH06, na.rm = TRUE)
mu_0_edad <- 40
error_estandar_edad <- desvio_edad / sqrt(n_edad)

# --- PASO 4: Estadístico t observado ---
t_obs_edad <- (media_edad - mu_0_edad) / error_estandar_edad

# --- PASO 5: Valor crítico (al 5%, dos colas) ---
t_critico_bilateral <- qt(1 - alfa/2, df = n_edad - 1)

# --- PASO 6: p-valor (bilateral) ---
p_valor_edad <- 2 * pt(abs(t_obs_edad), df = n_edad - 1, lower.tail = FALSE)

# IMPRIMIR RESULTADOS PARA EL INFORME 3.2
cat("RESULTADOS 3.2 (DOS COLAS):\n",
    "n:", n_edad, "\n",
    "Media:", media_edad, "\n",
    "t observado:", t_obs_edad, "\n",
    "t crítico (+/-):", t_critico_bilateral, "\n",
    "p-valor:", p_valor_edad)

# --- PASO 9: Verificación ---
t.test(base_sin_outlier$CH06, mu = mu_0_edad, alternative = "two.sided")

## TEST de diferencia de medias
# Enunciamos: H0: mu_pub - mu_priv = 0  vs  H1: mu_pub - mu_priv != 0
# Usamos bilateral para ser más exigentes.

test_diferencia <- t.test(P21 ~ sector_tipo, data = base_sin_outlier, alternative = "two.sided")

# Mostrar resultado completo
print(test_diferencia)

##############################
# MATRIZ DE CORRELACION 
###############################

#PEQUEÑOS AJUSTES A LA VARIABLE EDUCACION.

library(tidyverse)

base_sin_outlier <- base_sin_outlier %>%
  mutate(educ_nivel = case_when(
    NIVEL_ED == 7 ~ 0,  # Sin instrucción -> el nivel más bajo
    NIVEL_ED == 1 ~ 1,  # Primario incompleto
    NIVEL_ED == 2 ~ 2,  # Primario completo
    NIVEL_ED == 3 ~ 3,  # Secundario incompleto
    NIVEL_ED == 4 ~ 4,  # Secundario completo
    NIVEL_ED == 5 ~ 5,  # Superior incompleto
    NIVEL_ED == 6 ~ 6,  # Superior completo
    TRUE ~ NA_real_     # El 9 (Ns/Nr) y cualquier otro valor pasan a ser NA
  ))

# Verificamos que haya quedado bien
table(base_sin_outlier$educ_nivel, base_sin_outlier$NIVEL_ED)

##### matriz

install.packages("ggcorrplot")
library(ggcorrplot)
library(tidyverse)

# Usamos la variable nueva educ_nivel
base_corr <- base_sin_outlier %>%
  mutate(
    Ingreso = P21,
    Edad = CH06,
    Antiguedad = AANT,
    Sector_Pub = sector_tipo,
    Educacion = educ_nivel,       # <--- Aquí usamos la recodificada
    Mujer = ifelse(CH04 == 2, 1, 0) 
  ) %>%
  select(Ingreso, Edad, Antiguedad, Sector_Pub, Educacion, Mujer) %>%
  drop_na()

# Correlación
corr_matrix <- cor(base_corr)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE)

library(ggcorrplot)
library(tidyverse)

# 1. Graficamos con todos los requisitos
grafico_corr <- ggcorrplot(corr_matrix, 
                           hc.order = TRUE, 
                           type = "lower", 
                           lab = TRUE,
                           lab_size = 4,
                           colors = c("#6D9EC1", "white", "#E46726"), # Azul (negativo) a Naranja (positivo)
                           title = "Gráfico 4: Matriz de Correlaciones Lineales",
                           legend.title = "Coef. Corr",
                           ggtheme = theme_minimal()) +
  labs(subtitle = "Variables socio-laborales en el aglomerado Tucumán (4to Trim 2025)",
       caption = "Fuente: Elaboración propia en base a microdatos de la EPH-INDEC") +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(hjust = 0, size = 9))

# Mostrar el gráfico
print(grafico_corr)

# Guardar la imagen
ggsave("grafico_4_correlaciones.png", grafico_corr, width = 8, height = 6)


###########3
# REGRESIONES
###############3

install.packages("stargazer")
install.packages("car")

library(stargazer)
library(car) # Para el VIF

library(tidyverse)

# Creamos las variables definitivas en la base
base_sin_outlier <- base_sin_outlier %>%
  mutate(
    # 1. Género (1 = Mujer, 0 = Varón)
    Mujer = ifelse(CH04 == 2, 1, 0),
    
    # 2. Educación (Escala 0 a 6)
    educ_nivel = case_when(
      NIVEL_ED == 7 ~ 0,
      NIVEL_ED == 1 ~ 1,
      NIVEL_ED == 2 ~ 2,
      NIVEL_ED == 3 ~ 3,
      NIVEL_ED == 4 ~ 4,
      NIVEL_ED == 5 ~ 5,
      NIVEL_ED == 6 ~ 6,
      TRUE ~ NA_real_
    ),
    
    # 3. Sector Público (1 = Público, 0 = Privado)
    Sector_Pub = ifelse(sector_tipo == 1, 1, 0)
  )

# Modelo 1: Regresión Simple (basado en la correlación más fuerte: Antigüedad)
m1 <- lm(P21 ~ AANT, data = base_sin_outlier)

# Modelo 2: Incorporación de Variable Dummy (Sector Público)
m2 <- lm(P21 ~ AANT + sector_tipo, data = base_sin_outlier)

# Modelo 3: Regresión Múltiple Completa (Agregamos Educación, Género y Edad)
m3 <- lm(P21 ~ AANT + sector_tipo + educ_nivel + Mujer + CH06, data = base_sin_outlier)

# Modelo 4: Transformación Logarítmica (Recomendado por el sesgo del ingreso)
# Usamos log(P21) para que el modelo sea más robusto
m4 <- lm(log(P21) ~ AANT + sector_tipo + educ_nivel + Mujer + CH06, data = base_sin_outlier)

# --- TABLA COMPARATIVA FINAL (5.5) ---
stargazer(m1, m2, m3, m4, 
          type = "text", # Cambiá a "html" para exportar a Word/PDF
          title = "Tabla 3: Modelos de Regresión Lineal para el Ingreso",
          covariate.labels = c("Antigüedad", "Sector Público", "Nivel Educativo", "Mujer", "Edad"),
          dep.var.labels = c("Ingreso ($)", "Log(Ingreso)"),
          out = "modelos_regresion.txt")

# --- DIAGNÓSTICO DE MULTICOLINEALIDAD (VIF) para el Modelo 3 ---
vif(m3)

# 1. Aseguramos que los modelos estén bien calculados (usando tus nuevas variables)
m1 <- lm(P21 ~ AANT, data = base_sin_outlier)
m2 <- lm(P21 ~ AANT + Sector_Pub, data = base_sin_outlier)
m3 <- lm(P21 ~ AANT + Sector_Pub + educ_nivel + Mujer + CH06, data = base_sin_outlier)
m4 <- lm(log(P21) ~ AANT + Sector_Pub + educ_nivel + Mujer + CH06, data = base_sin_outlier)

# 2. Exportar a HTML (Este archivo se crea en tu carpeta de trabajo)
stargazer(m1, m2, m3, m4, 
          type = "html", 
          title = "Tabla 3: Modelos de Regresión Lineal para el Ingreso",
          covariate.labels = c("Antigüedad", "Sector Público", "Nivel Educativo", "Mujer", "Edad"),
          dep.var.labels = c("Ingreso ($)", "Log(Ingreso)"),
          digits = 2,
          out = "Tabla_Regresiones.html")
