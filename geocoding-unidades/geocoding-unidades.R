library(tidyverse)
library(ggmap)

register_google(key = Sys.getenv("GOOGLEMAPS_API_KEY"))


dados <- read_csv("demanda_etec_2025_1.csv")

cod_unidades <- unique(dados$unidade)

# Função para salvar os dados de geocoding
salvar_geocode <- function(cod_unidade) {
  nome_arquivo <- paste0("geocoding-unidades/dados/", cod_unidade, ".csv")
  texto_buscar <- paste0("Escola técnica estadual ", cod_unidade) |>
    stringr::str_remove_all(pattern = "-ee-.*|-emef-.*|-faculdade-.*|-fatec-.*|-emeb-.*|-polo-.*") |>
    stringr::str_replace_all(pattern = "-", replacement = " ") |>
    stringr::str_trim()

  if (!file.exists(nome_arquivo)) {
    usethis::ui_info("Buscando por: {texto_buscar}")
    result_geocode <- geocode(texto_buscar)
    result <- result_geocode |>
      mutate(unidade = cod_unidade)

    write_csv(result, file = nome_arquivo)
    return(result)
  }

  Sys.sleep(10)

}

# purrr::map(cod_unidades, salvar_geocode, .progress = TRUE)


# Ler os dados já geocodificados
dados_geocoding <- list.files("geocoding-unidades/dados/", full.names = TRUE) |>
  purrr::map_dfr(read_csv)

unidade_falta_dados <- dados_geocoding |>
  dplyr::filter(is.na(lat)) |>
  dplyr::pull(unidade)

arquivos_unidade_falta_dados <- paste0("geocoding-unidades/dados/", unidade_falta_dados, ".csv")

rstudioapi::navigateToFile(arquivos_unidade_falta_dados[4])

# fs::file_delete(paste0("geocoding-unidades/dados/", unidade_falta_dados, ".csv"))
# 
# purrr::map(unidade_falta_dados, salvar_geocode, .progress = TRUE)
