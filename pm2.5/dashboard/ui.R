library(shiny)
library(shinydashboard)
library(ggplot2)
library(tmap)
library(leaflet)
library(bslib)

shinyUI(
  fluidPage(
    title = "Desempenho dos Dados de PM2.5 no Paraná",
    theme = bs_theme(bootswatch = "flatly"),
    
    # Título principal do painel
    titlePanel("Desempenho dos Dados de PM2.5 no Paraná"),
    
    # Layout com painel de navegação lateral
    navlistPanel(
      id = "main_nav",
      widths = c(3, 9), # Define a largura do painel lateral (3/12) e do conteúdo principal (9/12)
      "Análises", # Título para a seção de navegação
      
      # Aba 1: Médias Mensais (Gráfico de Barras)
      tabPanel("Médias Mensais",
               h3("Médias Mensais de PM2.5 (μg/m³) — CAMS vs. DON"),
               plotOutput("barras", height = "600px")
      ),
      
      # Aba 2: Dispersão (Comparação Municipal)
      tabPanel("Dispersão e Bland-Altman (Municípios)",
               h3("Análise de Concordância por Município"),
               fluidRow(
                 column(6, plotOutput("dispersao_municipios", height = "500px")),
                 column(6, plotOutput("bland_altman_municipios", height = "500px"))
               )
      ),
      
      # Aba 3: Dispersão (Comparação Mensal)
      tabPanel("Dispersão e Bland-Altman (Mensal)",
               h3("Análise de Concordância Mensal"),
               fluidRow(
                 column(6, plotOutput("dispersao_mensal", height = "500px")),
                 column(6, plotOutput("bland_altman_mensal", height = "500px"))
               )
      ),
      
      # Aba 4: Séries Temporais e Boxplots
      tabPanel("Séries Temporais e Boxplots",
               h3("Comparativo Mensal — Séries e Distribuições"),
               plotOutput("series_temporais", height = "400px"),
               hr(),
               plotOutput("boxplots", height = "400px")
      ),
      
      # Aba 5: Mapas Estáticos
      tabPanel("Mapas de Distribuição (Estático)",
               h3("Distribuição Espacial do PM2.5 (μg/m³) no Paraná (2023)"),
               fluidRow(
                 column(6, plotOutput("mapa_cams_static", height = "500px")),
                 column(6, plotOutput("mapa_don_static", height = "500px"))
               )
      ),
      
      # Aba 6: Mapas Interativos
      tabPanel("Mapas de Distribuição (Interativo)",
               h3("Mapas Interativos de PM2.5 (μg/m³)"),
               fluidRow(
                 column(6, 
                        h4("CAMS"), 
                        tmapOutput("mapa_cams_interactive", height = "600px")
                 ),
                 column(6, 
                        h4("DON"), 
                        tmapOutput("mapa_don_interactive", height = "600px")
                 )
               )
      )
    )
  )
)