#----------------------------
#TALLER 3
#Integrantes:Camilo Ospina, David Pascagaza y Emily Rodriguez
#----------------------------


# Limpiar memoria
rm(list = ls())

# Cargamos las librerias necesarias para manipulacion de datos
library(dplyr)
library(tidyr)
library(scales)
library(readxl)
library(haven)
library(writexl)


#Paths datasets utilizados

tenderos_raw <- read_dta("C:\\Users\\Emily\\OneDrive\\Documentos\\Haciendo Economía\\Taller 3\\TenderosFU03_Publica.dta")
TerriData_Dim2_Sub3 <- read_excel("C:\\Users\\Emily\\OneDrive\\Documentos\\Haciendo Economía\\Taller 3\\TerriData_Dim2.xlsx",
                                  sheet = "Hoja02")  # <- FIX: los totales de poblacion (H+M) estan en Hoja02

#Elimina las etiquetas y deja los valores subyacentes como numéricos
tenderos_raw <- tenderos_raw %>%
  mutate(uso_internet = zap_labels(uso_internet))

# ---------------------
# TAREA 1: Promedio de uso de internet por municipio
# ---------------------

internet_por_ciudad <- tenderos_raw %>%
  group_by(Munic_Dept, Municipio) %>%  # Agrupa los datos por departamento y municipio
  summarize(internet = mean(uso_internet, na.rm = TRUE)) %>%  # Calcula el promedio de uso de internet (en porcentaje)
  ungroup() %>%  # Desagrupa los datos
  rename(divipola = Munic_Dept)  # Renombra la columna de departamento a "divipola"

# ---------------------
# TAREA 2: Transformar datos de actividades y calcular uso de internet por tipo de actividad
# ---------------------

#Limpiamos y organizamos datos de tal forma que se puedan agrupar por actividad

ren_frame <- tenderos_raw %>%
  # Renombramos las columnas de actividades con nombres descriptivos
  rename(Tienda.1 = actG1, ComidaPreparada.2 = actG2, PeluqueriayBelleza.3 = actG3, Ropa.4 = actG4,
         Otras.5 = actG5, PapeleriayComunicaciones.6 = actG6, VidaNocturna.7 = actG7, ProductosInventario.8 = actG8,
         Salud.9 = actG9, Servicios.10 = actG10, FerreteriayAfines.11 = actG11)

col_frame <- ren_frame %>%
  # Convertimos las columnas de actividades en dos columnas: "category" (nombre original) y "total" (valor 0/1)
  pivot_longer(cols = Tienda.1:ferreteria.11, names_to = "category", values_to = "total")

col_frame <- col_frame %>%
  # Separamos la columna "category" en dos: el nombre de la actividad y su numero
  separate(category, c("Actividad", "actG"))

#Frame con uso de internet por actividad economica

internet_por_actividad <- col_frame %>%
  group_by(actG, Actividad) %>%  # Agrupamos por numero de actividad y nombre
  summarize(internet = mean(uso_internet, na.rm = TRUE))  # Calculamos el promedio de uso de internet por actividad

# ---------------------
# TAREA 3: Promedio de uso de internet por municipio y actividad (solo donde la actividad esta presente)
# ---------------------

internet_por_ciudad_actividad <- col_frame %>%
  filter(total == 1) %>%  # Se filtran solo las filas donde la actividad está presente (valor 1)
  group_by(Munic_Dept, Municipio, actG, Actividad) %>%  # Se agrupa por municipio y tipo de actividad
  summarise(internet = round(mean(uso_internet, na.rm = TRUE), 0)) %>%  # Se calcula el promedio de uso de internet y se redondea a entero
  rename(divipola = Munic_Dept) %>%  #Se renombra  la columna del departamento
  arrange(Municipio, actG)  #Se ordena por municipio y tipo de actividad

# ---------------------
# TAREA 4: Procesar datos demograficos para obtener poblacion total por municipio
# ---------------------

#Limpieza de base datos

TerriData_Dim2_Sub3 <- TerriData_Dim2_Sub3 %>%
  mutate(
    `Dato Numérico Limpio` = `Dato Numérico` %>%
      gsub("\\.", "", .) %>%
      gsub(",00$", "", .) %>%
      gsub(",", ".", .) %>%
      as.numeric()
  )

TerriData_Dim2_Sub3 <- TerriData_Dim2_Sub3 %>%
  mutate(divipola=as.numeric(`Código Entidad`))

#Frame con poblacion por codigo DANE
poblacion <- TerriData_Dim2_Sub3 %>%
  filter(
    Año == 2024,
    Indicador %in% c("Población total de hombres", "Población total de mujeres"),
    `Unidad de Medida` != "Porcentaje (el valor está multiplicado por 100)"
  ) %>%
  group_by(divipola) %>%
  summarise(poblacion = sum(`Dato Numérico Limpio`, na.rm = TRUE))

# Utilizamos merge() para unir las dos bases de datos principales

base_final_internet_poblacion <- merge(poblacion, internet_por_ciudad_actividad,
                                       by.x = "divipola", by.y = "divipola")

# --------------------
#Exportamos dataframes a excel para graficar con powerbi utilizando libreria writexl
# --------------------


# Guardar como Excel cada dataframe

#write_xlsx(internet_por_ciudad_actividad, "internet_por_ciudad_actividad.xlsx")
#write_xlsx(base_final_internet_poblacion, "base_final_internet_poblacion.xlsx")

# ---------------------
# FIX previo: internet_por_ciudad_actividad estaba perdiendo el %
# (mean() sin *100 + round(,0) -> todo se colapsaba a 0 o 1)
# ---------------------

internet_por_ciudad_actividad <- col_frame %>%
  filter(total == 1) %>%
  group_by(Munic_Dept, Municipio, actG, Actividad) %>%
  summarise(internet = round(mean(uso_internet, na.rm = TRUE) * 100, 0)) %>%  # <- FIX: *100
  rename(divipola = Munic_Dept) %>%
  arrange(Municipio, actG)

# Volver a correr el merge con la version corregida
base_final_internet_poblacion <- merge(poblacion, internet_por_ciudad_actividad,
                                       by.x = "divipola", by.y = "divipola")

# =============================================================
# TAREA 6: Reshape a formato LARGO y EXTENSO
# =============================================================

# --- BASE LARGA ---
base_larga <- base_final_internet_poblacion %>%
  mutate(pob_millones = round(poblacion / 1e6, 1)) %>%
  select(divipola, municipio = Municipio, actG, Actividad, internet, pob_millones) %>%
  arrange(divipola, actG)

print(base_larga)

# --- BASE EXTENSA (wide) ---
base_extensa <- base_larga %>%
  select(divipola, municipio, pob_millones, actG, internet) %>%
  pivot_wider(
    names_from  = actG,
    values_from = internet,
    names_prefix = "internet_"
  ) %>%
  arrange(divipola)

print(base_extensa)

# Exportar ambas versiones
write_xlsx(base_larga, "base_larga.xlsx")
write_xlsx(base_extensa, "base_extensa.xlsx")
