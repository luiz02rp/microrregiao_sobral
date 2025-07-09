library(ggplot2)
library(tidyverse)
library(psych)
library(dplyr)
library(janitor)
library(ggpubr)
library(corrplot)
library(stringr)
library(stringi)

df <- df %>%
  mutate(qual_nivel_predomina = qual_nivel_predomina %>%
           stri_trans_general("Latin-ASCII") %>%  # remove acentos
           str_to_lower() %>%
           str_trim()
  )


df <- read.csv("planilha_psis_municipio - níveis_atencao_psi.csv",
               fileEncoding = "UTF-8-BOM", 
stringsAsFactors = FALSE)
df2 <- read.csv("data/pop_idade_sexo - Página1.csv")
df2 <- janitor::clean_names(df2)

df <- janitor::clean_names(df)

colunas_para_manter_como_texto <- c("municipios", "qual_nivel_predomina")

colunas_caracter <- names(df)[sapply(df, is.character)]

colunas_para_converter <- setdiff(colunas_caracter, colunas_para_manter_como_texto)

print(colunas_para_converter)

colunas_texto <- c(df$municipios, df$qual_nivel_predomina)

limpa_coluna <- function(col) {
  col <- gsub("\\.", "", col)       # remove ponto de milhar
  col <- gsub(",", ".", col)        # troca vírgula decimal por ponto
  col <- gsub("[^0-9.-]", "", col)  # remove qualquer outro símbolo
  col <- trimws(col)
  return(as.numeric(col))
}

df[colunas_para_converter] <- lapply(df[colunas_para_converter], limpa_coluna)

df <- left_join(df, df2, by = c("municipios" = "municipio"))

dados <- df %>%
  mutate(qual_nivel_predomina = qual_nivel_predomina %>%
           str_to_lower() %>%         # deixar tudo minúsculo
           str_trim() %>%             # remover espaços antes/depois
           str_replace_all("í", "i")  # padronizar acento, se necessário
  )


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

# Carregar pacotes necessários


# Dados de exemplo (substitua pelo seu dataframe real)
df <- df |>
  data.frame(
  municip = c("Sobral", "Ipu", "Frecheirinha", "Mucambo", "..."), # Complete com seus dados
  atencao_primaria = c(18, 3, 3, 0, ...),                           # Valores da sua planilha
  atencao_secundaria = c(37, 5, 4, 0, ...),                         # Valores da sua planilha
  populacao = c(215286, 42968, 16362, 14009, ...)                   # Valores da sua planilha
)

# 1. Correlação: Atenção Primária × População
cor_primaria <- cor.test(
  x = df$atencao_primaria, 
  y = df$populacao, 
  method = "spearman", 
  exact = FALSE  # Desativa cálculos exatos para amostras > 50
)

# 2. Correlação: Atenção Secundária × População
cor_secundaria <- cor.test(
  x = df$atencao_secundaria, 
  y = df$populacao, 
  method = "spearman",
  exact = FALSE
)

# Resultados das correlações
cat("--- CORRELAÇÕES ---\n")
cat("Atenção Primária × População:\n")
cat("Rho =", cor_primaria$estimate, "| p-value =", cor_primaria$p.value, "\n\n")
cat("Atenção Secundária × População:\n")
cat("Rho =", cor_secundaria$estimate, "| p-value =", cor_secundaria$p.value, "\n")

# 3. Visualização com gráficos de dispersão
plot_primaria <- ggplot(df, aes(x = populacao, y = atencao_primaria)) +
  geom_point(color = "#4E84C4") +
  geom_smooth(method = "lm", se = FALSE, color = "#D95F02") +
  labs(
    title = "Atenção Primária × População",
    x = "População",
    y = "Número de Psicólogos (Primária)"
  ) +
  theme_minimal()

plot_secundaria <- ggplot(df, aes(x = populacao, y = atencao_secundaria)) +
  geom_point(color = "#1B9E77") +
  geom_smooth(method = "lm", se = FALSE, color = "#7570B3") +
  labs(
    title = "Atenção Secundária × População",
    x = "População",
    y = "Número de Psicólogos (Secundária)"
  ) +
  theme_minimal()

# Combinar gráficos
ggarrange(plot_primaria, plot_secundaria, ncol = 2)

# 4. Matriz de correlação (opcional)
cor_matrix <- df |> 
  select(atencao_primaria, atencao_secundaria, populacao) |> 
  cor(method = "spearman")

corrplot(cor_matrix, method = "number", type = "upper")



# Carregar pacotes necessários

shapiro.test(df$atencao_primaria)
shapiro.test(df$atencao_secundaria)
shapiro.test(df$populacao)

# 1. Correlação: Atenção Primária × População
cor_primaria <- corr.test(df$populacao, df$atencao_primaria, method = "spearman")

cor_primaria <- tibble(
  Estatística = c("Coeficiente r", "Valor-p", "Limite Inferior IC 95%", "Limite Superior IC 95%", "t-valor"),
  Valor = c(
    cor_primaria$r,
    cor_primaria$p,
    cor_primaria$ci$lower,
    cor_primaria$ci$upper,
    cor_primaria$t
  )
)

cor_primaria

# 2. Correlação: Atenção Secundária × População
cor_secundaria <- corr.test(df$populacao, df$atencao_secundaria, method = "spearman")


cor_secundaria <- tibble(
  Estatística = c("Coeficiente r", "Valor-p", "Limite Inferior IC 95%", "Limite Superior IC 95%", "t-valor"),
  Valor = c(
    cor_secundaria$r,
    cor_secundaria$p,
    cor_secundaria$ci$lower,
    cor_secundaria$ci$upper,
    cor_secundaria$t
  )
)

cor_secundaria

# 4. Matriz de correlação (opcional)
cor_matrix <- df |> 
  select(atencao_primaria, atencao_secundaria, populacao) |> 
  cor(method = "spearman")

corrplot(cor_matrix, method = "number", type = "upper")

shapiro.test(df$atencao_primaria)
shapiro.test(df$atencao_secundaria)
wilcox.test(df$atencao_primaria, df$atencao_secundaria)

df <- df |>
  mutate(
    atencao_primaria = ifelse(atencao_primaria == 0, NA, atencao_primaria),
    atencao_secundaria = ifelse(atencao_secundaria == 0, NA, atencao_secundaria))

df <- df |> 
  mutate(
    pop_pri = populacao/atencao_primaria,
    pop_sec = populacao/atencao_secundaria
)

describe(df$pop_pri)
describe(df$pop_sec)
