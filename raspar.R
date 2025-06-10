raspar_demanda_etec <- function(unidade, semestre, ano) {
  url <- paste0(
    "https://vestibulinho.etec.sp.gov.br/demanda-resultado/?semestre=",
    ano,
    "-",
    semestre,
    "o-semestre&unidade=",
    unidade
  )

  resultado <- httr::GET(url)

  if (httr::status_code(resultado) != 200) {
    stop("Erro ao acessar a URL: ", url)
  }

  conteudo <- httr::content(resultado)
  tabela <- rvest::html_table(conteudo) |>
    purrr::pluck(1)

  tabela_prep <- tabela |>
    dplyr::mutate(
      ano = ano,
      semestre = semestre,
      unidade = unidade
    ) |>
    janitor::clean_names()


  readr::write_csv(tabela_prep,
    file = paste0("dados/", unidade, "_", semestre, "_", ano, ".csv")
  )

  tabela_prep
}

# experimentando a função ---
raspar_demanda_etec(
  unidade = "etec-juscelino-kubitschek-de-oliveira",
  semestre = "1",
  ano = "2025"
)

# para iterar ---

semestres <- c("1")

anos <- c(2025)


unidades <- httr::GET(
  "https://vestibulinho.etec.sp.gov.br/demanda-unidades/?semestre=2025-1o-semestre"
) |>
  httr::content() |>
  rvest::html_nodes("div.accordion-item") |>
  rvest::html_nodes("button.accordion-toggle") |>
  rvest::html_attr("onclick") |>
  stringr::str_extract("(?<=unidade=)[^']+") |>
  unique() |>
  sort()


# iterar ----

purrr::map(unidades,
  ~ raspar_demanda_etec(
    unidade = .x,
    semestre = "1",
    ano = 2025
  ),
  .progress = TRUE
)


# ler em uma tabela

arquivos <- fs::dir_ls("dados/")

dados <- readr::read_csv(arquivos)


dados |>
  readr::write_csv("demanda_etec_2025_1.csv")
