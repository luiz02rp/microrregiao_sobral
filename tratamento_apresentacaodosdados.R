# carregando pacotes ------------------------------------------------------
library(tidyverse)
library(janitor)


# chamando banco de dados -------------------------------------------------
pop_micro_sobral <- read_csv2("data/micro_sobral.csv")


# tratamento dos dados ----------------------------------------------------
pop_micro_sobral <- pop_micro_sobral |> 
  clean_names() |> 
  as_tibble() |> 
  mutate(
    municipio = str_to_title(municipio), # garante que a primeira letra seja maiúscula
    populacao = as.numeric(populacao) # garante que os valores sejam numéricos
  )

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




