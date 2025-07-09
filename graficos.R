library(ggplot2)
library(tidyverse)
library(psych)
library(dplyr)
library(janitor)

df <- read.csv("planilha_psis_municipio - níveis_atencao_psi.csv")
df <- clean_names(df)

ggplot(data.frame(x = df$total_de_psicologos), aes(x = x)) +
  geom_density(fill = "grey", alpha = 0.7) +
  labs(title = "Gráfico de Densidade - Total de Psicólogos",
       x = "Valores",
       y = "Densidade")

describe(df$total_de_psicologos)

df$pop_psi <- gsub(",", ".", df$pop_psi)      # troca vírgula por ponto decimal
df$pop_psi <- gsub("[^0-9.-]", "", df$pop_psi) # remove símbolos e letras
df$pop_psi <- trimws(df$pop_psi)              # remove espaços
df$pop_psi <- as.numeric(df$pop_psi)

describe(df$pop_psi)

ggplot(data.frame(x = df$pop_psi), aes(x = x)) +
  geom_density(fill = "grey", alpha = 0.7) +
  labs(title = "Gráfico de Densidade - População por Psicólogo",
       x = "Valores",
       y = "Densidade")
