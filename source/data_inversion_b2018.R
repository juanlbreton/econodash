# librería
library(tidyverse)


# url de origen de datos periódico
url <- 
  "https://www.inegi.org.mx/contenidos/programas/ifb/2018/tabulados/des/ifb_indice.xlsx"


# descarga de archivo original y periódico
download.file(
  url = url,
  "archivo.xlsx"
)


# apertura de archivo de datos
inve_00 <- 
  readxl::read_xlsx(
    "archivo.xlsx",
    skip = 4,
    trim_ws = TRUE
  )


# columnas que quedan
totcol <- 
  ncol(inve_00) - 3



# limpieza de datos
inve_01 <- 
  inve_00[c(2,3,6,7,10), ] |> 
  janitor::clean_names() |> 
  mutate(
    denominacion = coalesce(denominacion, x2)
  ) |> 
  select(-c(x2:x4)) |> 
  mutate(
    denominacion = case_when(
      denominacion == "Nacional" ~ "Maq y Eq Nacional",
      denominacion == "Importado" ~ "Maq y Eq Importado",
      denominacion == "TOTAL" ~ "Total",
      denominacion == "Construcción" ~ "Construcción",
      denominacion == "Maquinaria y equipo" ~ "Maquinaria y Equipo",
      .default = "Sin clasificación"
    )
  ) |> 
  pivot_longer(cols = 2:all_of(totcol),
               names_to = "col",
               values_to = "val") |> 
  mutate(val = as.numeric(val)) |> 
  nest(.by = denominacion)



# función para fecha
prep_data <- function(df){
  df |> 
    mutate(fecha_inicio = seq.Date(from = as.Date("1993-01-01"),
                                   along.with = val,
                                   by = "1 month"),
           fecha_final = fecha_inicio + months(1) - days(1)
    )
}


# implementación de fecha
inve_02 <- 
  inve_01 |> 
  mutate(
    nuevo = map(data, prep_data)
  ) |> 
  select(-data) |> 
  unnest(nuevo) |> 
  select(-col) |> 
  relocate(val, .after = everything())


# borrado de archivo
file.remove("archivo.xlsx")


# limpieza de environment
rm(list = setdiff(ls(), "inve_02"))

















