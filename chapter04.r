# Capítulo 4 - Workflow: basics
# Criar objeto

objeto <- 12 %% 7
objeto

esse_e_um_nome_longo_para_objeto <- 2.5

## 4.3 Calling functions
## 

(y <- seq(1, 10, length.out = 5))
# colocando entre parenteses ele já executa o objeto

# 4.4 Exercícios
## 1. Não funciona pois há um typo em variable
## 2.  
library(tidyverse)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl == 8)
filter(diamonds, carat > 3)

## 3. Alt Shift K abre todas as shortcuts