library(tidyverse)
dados_demanda <- read_csv("https://raw.githubusercontent.com/beatrizmilz/etec/refs/heads/main/demanda_etec_2025_1.csv")

dados_geocoding <- read_csv("https://raw.githubusercontent.com/beatrizmilz/etec/refs/heads/main/geocoding-unidades/dados-geocoding.csv")

dados <- left_join(dados_demanda, dados_geocoding, by = "unidade")

textos_remover <- c(
  "MTec",
  "Ensino Médio com Habilitação Profissional de",
  "20% On-line",
  "Mtec-N",
  "MTec",
  "Especialização",
  "Ensino Médio com Itinerário Formativo de",
  "Técnico Em "
)


dados_curso_resumido <- dados |>
  mutate(
    curso_resumido =
      stringr::str_remove_all(curso, "- .*") |>
      stringr::str_remove_all(paste0(textos_remover, collapse = "|")) |>
      stringr::str_remove_all("Técnico em ") |>
      stringr::str_trim(),
    unidade_pad = stringr::str_trunc(unidade, width = 40, side = "right")
  ) 


library(geobr)
library(sf)
municipios_sp <- geobr::read_municipality("SP")

# crs_municipios <- st_crs(municipios_sp)

pontos_sf <- st_as_sf(dados_curso_resumido, coords = c("lon", "lat"), crs = 4674, remove = FALSE)

# pontos_sf |> 
#   ggplot() +
#   geom_sf()

pontos_com_municipio <- st_join(pontos_sf, municipios_sp) |> 
  sf::st_drop_geometry()


readr::write_csv(
  pontos_com_municipio,
  "shinyapps/dados-etec.csv")

