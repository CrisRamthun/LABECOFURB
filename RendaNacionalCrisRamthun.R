# Carregar pacotes necessários
library(tidyverse)
library(WDI)
library(ggplot2)
library(dplyr)
library(plotly)

# Configuração para remover notação científica
options(scipen = 999)

# Importação de dados globais
rendanacional <- WDI(country = 'all', indicator = 'NY.ADJ.NNTY.KD.ZG')

# Verificação: Garantir que rendanacional não está vazio
if (nrow(rendanacional) == 0) {
  stop("Erro: Os dados de renda nacional não foram carregados corretamente.")
}

# Definição de países de interesse
paises <- c('BR', 'US')
rendanacionalBRUS <- WDI(country = paises, indicator = 'NY.ADJ.NNTY.KD.ZG')

# Importação de dados de corte transversal (Ano de 2021)
rendanacional2021 <- WDI(country = 'all', indicator = 'NY.ADJ.NNTY.KD.ZG', start = 2021, end = 2021)

# Importação de série temporal apenas para o Brasil
rendanacionalBR <- WDI(country = 'BR', indicator = 'NY.ADJ.NNTY.KD.ZG')

### Gráfico de Dados em Painel ###
rendanacional <- rendanacional %>%
  mutate(destaque = ifelse(country == "Brazil", "Brasil", "Outros"))

ggplot(rendanacional, aes(x = year, y = NY.ADJ.NNTY.KD.ZG)) +
  geom_point(color = "blue", alpha = 0.5, size = 1.2) +
  geom_point(data = filter(rendanacional, country == "Brazil"),
             aes(x = year, y = NY.ADJ.NNTY.KD.ZG),
             color = "red", size = 1.2, shape = 17) +
  labs(title = "Crescimento Ajustado da Renda Nacional",
       x = "Ano",
       y = "Variação (%)") +
  theme_minimal()

### Gráfico de Corte Transversal ###
rendanacional2021 <- rendanacional2021 %>%
  mutate(color = ifelse(country == "Brazil", "red", "black"))

ggplot(rendanacional2021, aes(y = NY.ADJ.NNTY.KD.ZG, x = year, color = color)) +
  geom_point(size = 4, alpha = 0.8) +
  labs(title = "Corte Transversal da Renda Nacional",
       x = "Ano",
       y = "PIB") +
  scale_color_identity() +
  theme_minimal()

### Gráfico de Série Temporal ###
ggplot(rendanacionalBR, aes(y = NY.ADJ.NNTY.KD.ZG, x = year)) +
  geom_line() +
  labs(title = "Evolução da Renda Nacional do Brasil",
       x = "Ano",
       y = "Variação (%)") +
  theme_minimal()
