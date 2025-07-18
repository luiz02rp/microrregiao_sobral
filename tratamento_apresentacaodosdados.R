# carregando pacotes ------------------------------------------------------
library(tidyverse)
library(janitor)
library(sf)
library(scales)
library(geobr)
library(readr)
library(plotly)
# chamando banco de dados -------------------------------------------------
# população do ceará
ce_municipios <- read_municipality(code_muni = "CE", year = 2024)

# população da micro sobral
pop_micro_sobral <- read_csv2("data/micro_sobral.csv")

# mortalidade da micro sobral
mort_micro_sobral <- read_csv("data/mortalidade_micro_sobral.csv",
                              locale = locale(encoding = "Latin1")) 


# tratamento dos dados ----------------------------------------------------
# população dos municípios da micro sobral
pop_micro_sobral <- pop_micro_sobral |> 
  clean_names() |> 
  as_tibble() |> 
  mutate(
    municipio = str_to_title(municipio), # garante que a primeira letra seja maiúscula
    populacao = as.numeric(populacao) # garante que os valores sejam numéricos
  )
# juntando bancos de dados
ce_municipios <- ce_municipios |> 
  mutate(
    name_muni = str_to_title(name_muni)) |> 
  inner_join(pop_micro_sobral, by = c("name_muni" = "municipio"))

# tratando banco da mortalidade
mort_micro_sobral <- mort_micro_sobral |> 
  clean_names() |> 
  rename(municipio = munici_pio)

# análise descritiva ------------------------------------------------------
pop_micro_sobral |> 
  summarise(
    qde_municipios = n(),
    total_populacao = sum(populacao),
    media = mean(populacao),
    mediana = median(populacao), 
    desvio_padrao = sd(populacao),
    valor_minimo = min(populacao),
    valor_maximo = max(populacao)
  )

# análise gráfica ---------------------------------------------------------
# A) Mapa de Calor Hierárquico
ce_municipios |> 
  ggplot()+
  geom_sf(aes(fill = populacao), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "inferno", labels = comma) +
  labs(
    title = "Distribuição populacional da Microrregião de Sobral",
    fill = "População"
  ) +
  theme_void()

# B) Gráfico de coluna
# população x nome do município
gra_col <- pop_micro_sobral |> 
  mutate(populacao_formatada = format(populacao, big.mark = ".", decimal.mark = ",")) |> 
  ggplot(
    aes(x = populacao,
        y = reorder(municipio, populacao),
        text = paste("Município:", municipio, "<br>População:", populacao_formatada)),
    color = "white"
  )+
  geom_col(fill = "grey")+
  labs(
    title = "Distribuição populacional por município",
    x = "População",
    y = "Municípios"
  )+
  # remover espaçamento entre o nome dos municípios e as colunas
  scale_x_continuous(expand = c(0,0),
                     breaks = c(6131, 43358, 197663),
                     limits = c(0, max(pop_micro_sobral$populacao)*1.02))+
  # remover fundo quadriculado
  theme(
    panel.background = element_blank(),
    axis.line.y = element_line(),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.margin = margin(l = 100, r = 20),
    axis.line.x = element_line()
  )
# tranformar em gráfico interativo
gra_col_int <- ggplotly(gra_col, tooltip = "text") |> 
  layout(margin = list(l = 120, r = 50, t = 50, b = 50),
         yaxis = list(
           title = list(standoff = 10)
         ))
            
  
  


