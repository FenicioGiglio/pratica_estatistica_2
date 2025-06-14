library(shiny)
library(tidyverse)
library(lubridate)
library(sf)
library(geobr)
library(readxl)
library(tmap)

cams <- readRDS("../bases/PM2.5_diario_2023.rds")

pr_cams <- cams |>
  mutate(data = ymd(Date),
         mes = month(data)) |>
  mutate(estado = case_when(str_starts(Cod, "41") ~ "Paraná")) |>
  filter(!is.na(estado)) |>
  mutate(Cod = as.character(Cod)) |>
  select(Cod, data, mes, PM2.5)

donvonkelar <- read_excel("../bases/dados_completos_consolidado_donkelar.xlsx")

pr_don <- donvonkelar |>
  filter(SIGLA_UF == 41) |>
  mutate(CD_MUN = as.character(CD_MUN))

shapepr <- read_sf("../bases/PR_Municipios_2024/PR_Municipios_2024.shp")

# Preparação de dados para os gráficos
mes_niveis <- c("Jan", "Fev", "Mar", "Abril", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

# Dados para gráfico de barras e séries temporais
pr_don_mensal <- pr_don |>
  mutate(mes = factor(case_when(
    Mes == 1 ~ "Jan", Mes == 2 ~ "Fev", Mes == 3 ~ "Mar", Mes == 4 ~ "Abril",
    Mes == 5 ~ "Mai", Mes == 6 ~ "Jun", Mes == 7 ~ "Jul", Mes == 8 ~ "Ago",
    Mes == 9 ~ "Set", Mes == 10 ~ "Out", Mes == 11 ~ "Nov", Mes == 12 ~ "Dez"
  ), levels = mes_niveis)) |>
  group_by(mes) |>
  summarise(DON = mean(Media_PM25))

pr_cams_mensal <- pr_cams |>
  mutate(mes = factor(case_when(
    mes == 1 ~ "Jan", mes == 2 ~ "Fev", mes == 3 ~ "Mar", mes == 4 ~ "Abril",
    mes == 5 ~ "Mai", mes == 6 ~ "Jun", mes == 7 ~ "Jul", mes == 8 ~ "Ago",
    mes == 9 ~ "Set", mes == 10 ~ "Out", mes == 11 ~ "Nov", mes == 12 ~ "Dez"
  ), levels = mes_niveis)) |>
  group_by(mes) |>
  summarise(CAMS = mean(PM2.5))

pr_mensal_longo <- pr_don_mensal |>
  left_join(pr_cams_mensal, by = "mes") |>
  pivot_longer(cols = c(DON, CAMS), names_to = "pm", values_to = "valor")

pr_mensal <- pr_don_mensal |>
  left_join(pr_cams_mensal, by = "mes") |>
  mutate(diferenca = DON - CAMS, media = (DON + CAMS) / 2)

# Dados para gráficos de dispersão
pr_cams_munmen <- pr_cams |>
  group_by(Cod, mes) |>
  summarise(pm2.5_cams = mean(PM2.5))

pr_total <- pr_don |>
  left_join(pr_cams_munmen, by = c("CD_MUN" = "Cod", "Mes" = "mes")) |>
  mutate(diferenca = Media_PM25 - pm2.5_cams,
         media = (Media_PM25 + pm2.5_cams) / 2)

# Dados para boxplots
pr_longo <- pr_total |>
  pivot_longer(cols = c(Media_PM25, pm2.5_cams), names_to = "pm", values_to = "valor") |>
  mutate(
    mes = factor(case_when(
      Mes == 1 ~ "Jan", Mes == 2 ~ "Fev", Mes == 3 ~ "Mar", Mes == 4 ~ "Abril",
      Mes == 5 ~ "Mai", Mes == 6 ~ "Jun", Mes == 7 ~ "Jul", Mes == 8 ~ "Ago",
      Mes == 9 ~ "Set", Mes == 10 ~ "Out", Mes == 11 ~ "Nov", Mes == 12 ~ "Dez"
    ), levels = mes_niveis),
    PM = if_else(pm == "Media_PM25", "DON", "CAMS")
  )

# Dados para mapas
cams_cod <- pr_cams |> group_by(Cod) |> summarise(pm2.5_cod_cams = mean(PM2.5))
don_cod <- pr_don |> group_by(CD_MUN) |> summarise(pm2.5_cod_don = mean(Media_PM25))
cams_shape <- left_join(shapepr, cams_cod, by = c("CD_MUN" = "Cod"))
don_shape <- left_join(shapepr, don_cod, by = "CD_MUN")

# Definição do servidor
shinyServer(function(input, output) {
  
  # Renderiza o gráfico de barras
  output$barras <- renderPlot({
    ggplot(pr_mensal_longo, aes(x = mes, y = valor, fill = pm)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      labs(x = "Mês", y = "PM2.5 μg/m³ Médio", fill = "",
           caption = "Fonte de dados: CAMS e Vondonkelar") +
      scale_fill_manual(values = c("CAMS" = "#007bff", "DON" = "#dc3545")) +
      theme_minimal(base_size = 16)
  })
  
  # Renderiza o gráfico de dispersão por municípios
  output$dispersao_municipios <- renderPlot({
    ggplot(pr_total, aes(x = pm2.5_cams, y = Media_PM25)) +
      geom_point(alpha = 0.6) +
      labs(x = "PM2.5μg/m³ CAMS", y = "PM2.5μg/m³ DON", title = "Dispersão entre os PM2.5 (Municípios)",
           caption = "Fonte de dados: CAMS e Vondonkelar") +
      theme_minimal(base_size = 16)
  })
  
  # Renderiza o gráfico Bland-Altman por municípios
  output$bland_altman_municipios <- renderPlot({
    media_diff <- mean(pr_total$diferenca, na.rm = TRUE)
    sd_diff <- sd(pr_total$diferenca, na.rm = TRUE)
    limite_superior <- media_diff + 1.96 * sd_diff
    limite_inferior <- media_diff - 1.96 * sd_diff
    
    ggplot(pr_total, aes(x = media, y = diferenca)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = media_diff, linetype = "dotted", color = "blue", size = 1) +
      geom_hline(yintercept = limite_superior, linetype = "dashed", color = "red", size = 1) +
      geom_hline(yintercept = limite_inferior, linetype = "dashed", color = "red", size = 1) +
      labs(y = "Diferenças (DON - CAMS)", x = "Médias", title = "Bland-Altman (Municípios)",
           caption = "Fonte de dados: CAMS e Vondonkelar") +
      theme_minimal(base_size = 16)
  })
  
  # Renderiza o gráfico de dispersão mensal
  output$dispersao_mensal <- renderPlot({
    ggplot(pr_mensal, aes(x = CAMS, y = DON)) +
      geom_point(size = 3) +
      labs(x = "PM2.5μg/m³ CAMS", y = "PM2.5μg/m³ DON", title = "Dispersão entre os PM2.5 (Mensal)",
           caption = "Fonte de dados: CAMS e Vondonkelar") +
      theme_minimal(base_size = 16)
  })
  
  # Renderiza o gráfico Bland-Altman mensal
  output$bland_altman_mensal <- renderPlot({
    media_diff2 <- mean(pr_mensal$diferenca, na.rm = TRUE)
    sd_diff2 <- sd(pr_mensal$diferenca, na.rm = TRUE)
    limite_superior2 <- media_diff2 + 1.96 * sd_diff2
    limite_inferior2 <- media_diff2 - 1.96 * sd_diff2
    
    ggplot(pr_mensal, aes(x = media, y = diferenca)) +
      geom_point(size = 3) +
      geom_hline(yintercept = media_diff2, linetype = "dotted", color = "blue", size = 1) +
      geom_hline(yintercept = limite_superior2, linetype = "dashed", color = "red", size = 1) +
      geom_hline(yintercept = limite_inferior2, linetype = "dashed", color = "red", size = 1) +
      labs(y = "Diferenças (DON - CAMS)", x = "Médias", title = "Bland-Altman (Mensal)",
           caption = "Fonte de dados: CAMS e Vondonkelar") +
      theme_minimal(base_size = 16)
  })
  
  # Renderiza o gráfico de séries temporais
  output$series_temporais <- renderPlot({
    ggplot(pr_mensal_longo, aes(x = mes, y = valor, color = pm, group = pm)) +
      geom_line(size = 1.2) +
      labs(title = "Séries Temporais das Médias Mensais", x = "Mês", y = "PM2.5μg/m³ Médio", color = "Satélites",
           caption = "Fonte de dados: CAMS e Vondonkelar") +
      scale_color_manual(values = c("CAMS" = "#007bff", "DON" = "#dc3545")) +
      theme_minimal(base_size = 16)
  })
  
  # Renderiza os boxplots
  output$boxplots <- renderPlot({
    ggplot(pr_longo, aes(x = mes, y = valor, fill = PM)) +
      geom_boxplot() +
      labs(title = "Boxplots das Médias Mensais por Município", x = "Mês", y = "PM2.5μg/m³ Médio", fill = "Satélites",
           caption = "Fonte de dados: CAMS e Vondonkelar") +
      scale_fill_manual(values = c("CAMS" = "#007bff", "DON" = "#dc3545")) +
      theme_minimal(base_size = 16)
  })
  
  # Renderiza os mapas estáticos
  output$mapa_cams_static <- renderPlot({
    ggplot(cams_shape) +
      geom_sf(aes(fill = pm2.5_cod_cams)) +
      scale_fill_gradient(name = "CAMS\nPM2.5μg/m³", low = "#9bccf2", high = "#022138", limits = c(7, 24)) +
      labs(caption = "Fonte de dados: CAMS") +
      theme_minimal(base_size = 16)
  })
  
  output$mapa_don_static <- renderPlot({
    ggplot(don_shape) +
      geom_sf(aes(fill = pm2.5_cod_don)) +
      scale_fill_gradient(name = "DON\nPM2.5μg/m³", low = "#9bccf2", high = "#022138", limits = c(7, 24)) +
      labs(caption = "Fonte de dados: Vondonkelar") +
      theme_minimal(base_size = 16)
  })
  
  # Renderiza os mapas interativos
  output$mapa_cams_interactive <- renderTmap({
    tm_shape(cams_shape) +
      tm_polygons(col = "pm2.5_cod_cams", palette = "Blues", title = "PM2.5μg/m³ - CAMS",
                  id = "NM_MUN", alpha = 0.7, breaks = c(7, 12, 17, 22, 25),
                  popup.vars = c("PM2.5μg/m³" = "pm2.5_cod_cams"))
  })
  
  output$mapa_don_interactive <- renderTmap({
    tm_shape(don_shape) +
      tm_polygons(col = "pm2.5_cod_don", palette = "Blues", title = "PM2.5μg/m³ - DON",
                  id = "NM_MUN", alpha = 0.7, breaks = c(7, 12, 17, 22, 25),
                  popup.vars = c("PM2.5μg/m³" = "pm2.5_cod_don"))
  })
  
})