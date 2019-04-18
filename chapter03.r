## Capítulo 3 - Data visualisation ===========================

## 3.1.1 - Pré requisitos - carregar o tidyverse
## Se não estiver instalado, rodar install.packages("tidyverse") e aguardar

library(tidyverse)

## 3.2 - Carregar o dataframe mpg, presente no ggplot2
mpg

## Agora vou analisar as variáveis presentes

glimpse(mpg)
 # 11 variáveis! 

## Plotar um gráfico de pontos comparando displ (tamanho do motor em litros)
## e hwy, eficiência do combustível, em milhas por galão (!)

ggplot(data = mpg) +
  geom_point(aes(displ, hwy)) # OK!
# Dá pra perceber que há uma correlação negativa entre o tamanho do motor
# e a eficiência do veículo, ou seja, quanto maior a capacidade, maior a eficiência
# e por sua vez, menor o consumo.

## Milhas por galão é uma unidade horrível. Convertendo para km/l:
## Basta dividir por aproximadamente 2,352
## Jogando mpg para um objeto (novo_mpg) para criar uma nova variável pra hwy (n_hwy)

novo_mpg <- mpg %>% 
  mutate(n_hwy = hwy/2.352) # Lembrar de usar . e não ,

# Checando:
glimpse(novo_mpg) # Tudo certo! 
 
## 3.2.4 - Exercícios
# 1. Nada.

# 2. Pelo glimpse, vemos 234 observações, e 11 variáveis no original. 12, no meu novo banco.

# 3. drv indica a tração. f = dianteira, r = traseira, 4wd = 4x4

# 4. Obtemos a quantidade de cilindros de acordo com a eficiência. 
# Carros com menos cilindros estão mais distribuídos no meio, apesar de uma conseguirem
# alcançar uma maior eficiência de km/l. Carros com mais cilindros possuem menos eficiência.

ggplot(novo_mpg) +
  geom_point(aes(n_hwy, cyl))

# 5. O gráfico não ajuda em nada pois as duas variáveis (drv e class) são categóricas

ggplot(novo_mpg) +
  geom_point(aes(class, drv))

## 3.3 - Aesthetic Mapping
## Adicionando uma terceira variável para classificar cada ponto no geom_point

ggplot(novo_mpg)+
  geom_point(aes(displ, n_hwy, color = class))
# O problema dos carros com baixa eficiência e tamanho de motor grande é que eles,
# dentro das classes do banco, são "2seater", ou seja, esportivos. Explicado.
# Os autores avisam que color, nesse caso, é melhor do que size (para categorização).
# A justificativa é de que size é uma classificação ORDENADA, enquanto class, não
# Isso gera um desencaixe na estrutura do gráfico. Não faz nem sentido. 
# Shape (forma) e Alpha (transparência) já são úteis nesse caso, como Color.

# Para mudar a cor, basta digitar color FORA de AES
ggplot(novo_mpg)+
  geom_point(aes(displ, n_hwy), color = "red")

## 3.3.1 - Exercícios 
# 1. Os pontos não estão azuis pois a cor deve ser indicada fora de aes()

# 2. A partir de glimpse(mpg), temos que manufacturer, model, trans, drv, fl e class
#são categóricas, enquanto displ, year, cyl, cty, hwy são contínuas.

# 3. Shape não pode ser setada para uma variável contínua. 
ggplot(novo_mpg)+
  geom_point(aes(displ, n_hwy, color = year))

# 4. Temos uma linha reta com slope de 45’ já que cada ponto se refere a ele mesmo
# no outro eixo.
ggplot(novo_mpg)+
  geom_point(aes(n_hwy, n_hwy))

# 5. Stroke se refere ao tamanho do ponto 
ggplot(novo_mpg)+
  geom_point(aes(displ, n_hwy), stroke = 0.3)

# 6. Ele pode transformar a categorização de cor de forma booleana
ggplot(novo_mpg)+
  geom_point(aes(displ, n_hwy, color = cyl < 5))


## 3.5 - Facets

ggplot(novo_mpg)+
  geom_point(aes(displ, n_hwy)) +
  facet_wrap(~ class, nrow = 3)
# O facet_wrap divide em vários gráficos, no número de valores existentes 
# em uma categórica (no caso, class). Como são 7 classes, cada classe tem um gráfico,
# logo, são 7 gráficos. 

ggplot(novo_mpg)+
  geom_point(aes(displ, n_hwy)) +
  facet_grid(drv ~ cyl)
# O facet_grid divide cruzando outras variáveis, no caso, drv e cyl

## 3.5.1 - Exercícios
# 1. Ele vai dividir em tantos gráficos quanto forem os valores da contínua
ggplot(novo_mpg)+
  geom_point(aes(displ, n_hwy)) +
  facet_wrap(~ cty)

# 2. A inexistência de carros com certo n’ de cilindros em certas trações
# 3. O . remove aquela dimensão do facet (colunas ou linhas)
# 4. Aprimora a visualização pelo isolamento, ao mesmo tempo que, se houverem 
# muitos gráficos, fica mais difícil identificar cada um.
# 5. nrow define número de linhas e ncol o de colunas. facet_grid não suporta
# tais funções pois o número é definido pelas variáveis escolhidas.
# 6. Para ter menos casos de colunas e mais de linhas, facilitando a visualização.

## 3.6 - Geometric Objects
## Copiando o gráfico misto utilizado na seção:

ggplot(novo_mpg, aes(displ, n_hwy))+
  geom_point(aes(color = drv))+
  geom_smooth(aes(linetype=drv, color = drv)) # OK

# Usando filter dentro do geom

ggplot(novo_mpg, aes(displ, n_hwy))+
  geom_point(aes(color = class))+
  geom_smooth(data=filter(novo_mpg, class == "compact"), se=FALSE)
