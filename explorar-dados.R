library(tidyverse)

dados <-  readr::read_csv("demanda_etec_2025_1.csv")
# explorar os dados: Ensino Médio

em_dia <- dados |>
  dplyr::filter(stringr::str_starts(curso, "Ensino Médio")) |>
  dplyr::filter(periodo != "Noite") |>
  dplyr::arrange(desc(demanda)) |>
  dplyr::mutate(
    curso_resumido = stringr::str_extract(curso, ".* -") |>
      stringr::str_remove_all("-|MTec -") |>
      stringr::str_remove("Ensino Médio com Habilitação Profissional de Técnico em") |>
      stringr::str_trim()
  )

em_dia |>
  dplyr::distinct(unidade)

em_dia |>
  dplyr::distinct(curso_resumido) |>
  dplyr::arrange(curso_resumido) |> View()


etecs_mais_proximas <- c(
  "etec-lauro-gomes",
  "etec-juscelino-kubitschek-de-oliveira",
  "etec-julio-de-mesquita",
  "etec-dr-celso-giglio",
  "etec-professor-andre-bogasia",
  "etec-professor-basilides-de-godoy"
)

em_dia |>
  dplyr::filter(unidade %in% etecs_mais_proximas) |> View()
