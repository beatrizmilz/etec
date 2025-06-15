library(shiny)
library(tidyverse)
library(shinyWidgets)
library(reactable)
library(bslib)
library(bsicons)
library(leaflet)
library(plotly)
dados <- read_csv("dados-etec.csv")

# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = "Demanda dos cursos da ETEC no primeiro semestre de 2025",
  sidebar = sidebar(
    title = "Filtros",
    width = "30%",
    pickerInput(
      inputId = "periodo",
      label = "Período:",
      choices = c("Manhã", "Manhã e Tarde", "Tarde", "Noite", "On-line"),
      selected = unique(dados$periodo),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    ),
    prettySwitch(
      inputId = "apenas_ensino_medio",
      label = "Apenas Ensino Médio",
      fill = FALSE,
      status = "primary"
    ),
    pickerInput(
      inputId = "municipio",
      label = "Município:",
      choices = sort(unique(dados$name_muni)),
      selected = unique(dados$name_muni),
      options = pickerOptions(
        liveSearch = TRUE,
        actionsBox = TRUE,
        width = "100%"
      ),
      multiple = TRUE
    ),
    pickerInput(
      inputId = "unidade",
      label = "Unidade:",
      choices = sort(unique(dados$unidade_pad)),
      selected = unique(dados$unidade_pad),
      
      options = pickerOptions(
        liveSearch = TRUE,
        actionsBox = TRUE,
        width = "100%"
      ),
      multiple = TRUE
    ),
    
    
    pickerInput(
      inputId = "curso",
      label = "Curso:",
      choices = sort(unique(dados$curso_resumido)),
      selected = unique(dados$curso_resumido),
      options = pickerOptions(
        liveSearch = TRUE,
        actionsBox = TRUE,
        width = "100%"
      ),
      multiple = TRUE
    ),
    downloadButton("download_dados", "Baixar tabela")
    
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Unidades",
      value = textOutput("value_box_unidades"),
      showcase = bsicons::bs_icon("building")
    ),
    value_box(
      title = "Total de cursos",
      value = textOutput("value_box_cursos"),
      showcase = bsicons::bs_icon("mortarboard")
    ),
    value_box(
      title = "Total de vagas",
      value = textOutput("value_box_vagas"),
      showcase = bsicons::bs_icon("person-workspace")
    )
  ),
  navset_card_tab(
    height = 450,
    full_screen = TRUE,
    nav_panel("Tabela", card(reactableOutput("tabela"))),
    nav_panel("Mapa", card(leaflet::leafletOutput("mapa"))),
    nav_panel("Gráfico", card(plotly::plotlyOutput("grafico"))),
    nav_panel(
      shiny::icon("circle-info"),
      markdown(
        "Olá! Aqui é a [Beatriz Milz](https://github.com/beatrizmilz), e este é um aplicativo Shiny para explorar a demanda dos cursos da ETEC para o <b>primeiro semestre de 2025</b>. <br> É possível filtrar por período, unidade e curso, além de visualizar as informações em uma tabela e em um mapa interativo. <br> Os dados foram extraídos do [site da ETEC](https://vestibulinho.etec.sp.gov.br/demanda/). <br> Algumas coordenadas das unidades podem estar incorretas, pois foram obtidas através de geolocalização usando a API do Google. <br>Caso encontre algum erro, você pode me avisar pelo [GitHub](https://github.com/beatrizmilz/etec/issues/new) ou [email: milz.bea@gmail.com](mailto:milz.bea@gmail.com). <br> Obrigada! "
      )
    )
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  dados_filtrados <- reactive({
    dados_filtrados <- dados |>
      filter(
        periodo %in% input$periodo,
        curso_resumido %in% input$curso,
        unidade_pad %in% input$unidade,
        name_muni %in% input$municipio
      ) |>
      arrange(desc(demanda)) |>
      select(-ano, -semestre)
    
    if (input$apenas_ensino_medio) {
      dados_filtrados <- dados_filtrados |>
        filter(str_starts(curso, "Ensino Médio"))
    }
    
    return(dados_filtrados)
    
  })
  
  output$value_box_vagas <- renderText({
    total_vagas <- sum(dados_filtrados()$vagas, na.rm = TRUE) |>
      format(big.mark = ".", decimal.mark = ",")
    
    total_vagas
  })
  
  output$value_box_unidades <- renderText({
    total_unidades <- length(unique(dados_filtrados()$unidade))
    total_unidades
  })
  
  
  output$value_box_cursos <- renderText({
    total_unidades <- length(unique(dados_filtrados()$curso_resumido))
    total_unidades
  })
  
  output$tabela <- renderReactable({
    dados_filtrados() |>
      select(name_muni,
             unidade,
             curso,
             periodo,
             inscritos,
             vagas,
             demanda) |>
      reactable(
        searchable = TRUE,
        columns = list(
          name_muni = colDef(name = "Município"),
          unidade = colDef(name = "Unidade"),
          curso = colDef(name = "Curso"),
          periodo = colDef(name = "Período"),
          inscritos = colDef(name = "Inscritos"),
          vagas = colDef(name = "Vagas"),
          demanda = colDef(name = "Demanda")
        )
      )
  })
  
  output$grafico <- plotly::renderPlotly({
    dados_prep <-    dados_filtrados() |>
      group_by(curso_resumido, name_muni) |>
      summarise(
        inscritos = sum(inscritos, na.rm = TRUE),
        vagas = sum(vagas, na.rm = TRUE),
        candidatos_por_vaga = inscritos / vagas,
        .groups = "drop"
      ) |>
      arrange(desc(candidatos_por_vaga)) |>
      mutate(curso_resumido = fct_reorder(curso_resumido, candidatos_por_vaga))
    
    
    
    grafico_ggplot <- dados_prep |>
      slice_max(order_by = candidatos_por_vaga, n = 10) |>
      ggplot(aes(y = curso_resumido, x = candidatos_por_vaga, fill = candidatos_por_vaga)) +
      geom_col(position = "dodge") +
      labs(title = "Cursos com a maior demanda", x = "Pessoas inscritas / Vagas", y = "Cursos") +
      theme_minimal()
    
    plotly::ggplotly(grafico_ggplot)
    
  })
  
  output$mapa <- leaflet::renderLeaflet({
    dados_prep_unidade <- dados_filtrados() |>
      group_by(unidade, lat, lon, name_muni) |>
      summarise(
        inscritos = sum(inscritos, na.rm = TRUE),
        vagas = sum(vagas, na.rm = TRUE),
        cand_vaga_geral = inscritos / vagas,
        quant_cursos = n_distinct(curso),
        cursos = paste(unique(curso_resumido), collapse = ", "),
        .groups = "drop"
      ) |>
      arrange(desc(cand_vaga_geral))
    
    dados_prep_unidade |>
      leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::addCircleMarkers(
        lng = ~ lon,
        lat = ~ lat,
        clusterOptions = leaflet::markerClusterOptions(),
        popup = ~ paste0(
          "<strong>Unidade:</strong> ",
          unidade,
          "<br>",
          "<strong>Município:</strong> ",
          name_muni,
          "<br>",
          "<strong>Cursos:</strong> ",
          cursos ,
          "<br>",
          "<strong>Inscritos:</strong> ",
          inscritos,
          "<br>",
          "<strong>Vagas:</strong> ",
          vagas,
          "<br>"
        ),
      )
  })
  
    # Download dos dados filtrados
  output$download_dados <- downloadHandler(
    filename = function() {
      paste0("dados_etec_filtrados_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(dados_filtrados(), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
