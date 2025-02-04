
# librerías
library(tidyverse)


url <- 
  "https://www.inegi.org.mx/contenidos/programas/inpc/2018a/datosabiertos/inpc_indicador_mensual_csv.zip"


download.file(url, "inpc.zip")


# descomprimir
unzip("inpc.zip", 
      exdir = "inpc")



# leer archivos
inflac_00 <- 
  read_csv("inpc/conjunto_de_datos/conjunto_de_datos_inpc_mensual.csv")


# limpieza de datos
inflac_01 <- 
  inflac_00 |> 
  janitor::clean_names() |> 
  select(-c(1:2, 6:7)) |> 
  mutate(fecha = dmy(fecha), 
         concepto = 
           case_when(
             str_detect(concepto, "(INPC)") ~ "INPC",
             str_detect(concepto, "Mercancías") ~ "SUBYACENTE: MERCANCÍAS",
             str_detect(concepto, "Servicios") ~ "SUBYACENTE: SERVICIOS",
             str_detect(concepto, "Subyacente") ~ "SUBYACENTE",
             str_detect(concepto, "Agropecuarios") ~ "NO SUBYACENTE: AGROPECUARIOS",
             str_detect(concepto, "Energéticos") ~ "NO SUBYACENTE: ENERGÉTICOS",
             str_detect(concepto, "No subyacente") ~ "NO SUBYACENTE",
             .default = "No identificado"
           ),
         concepto = str_to_title(concepto))


file.remove("inpc.zip")

unlink("inpc", recursive = TRUE)

rm(list = setdiff(ls(), "inflac_01"))
