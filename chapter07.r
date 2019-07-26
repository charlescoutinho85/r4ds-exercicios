# Exploratory Data Analysis ============================================================
# 
# Capítulo bem básico em que ele trata sobre EDA, exploratory data analysis, ou seja,
# estar em contato, conhecer, dissecar o banco.

library(tidyverse)
library(janitor)
library(ggthemes)
library(patchwork)
library(magick)

# Exercícios

# 7.3.4
# ============================================================
# 1. Selecionar x,y,z. O que elas dizem? Qual é comprimento, largura e profundidade?
# Vamos plotar...
ggplot(diamonds)+
  geom_density(aes(x = x), color = "dodgerblue3")+
  ggplot(diamonds)+
  geom_density(aes(x = y), color = "chartreuse4")+
  ggplot(diamonds)+
  geom_density(aes(x = z), color = "orange")+
  plot_layout(nrow = 3)
  
# De olho, podemos ver que y e z são bem similares, quase iguais. 
# x é que tem uma distribuição diferente. Todas são multimodais (não são normais). 
# Será que tem outliers? Vamos sumarizar.

diamonds %>% 
  select(x,y,z) %>% 
  summary()

# Observa-se que x possui uma média próxima da moda, o que faria com que a gente
# acreditasse, caso não olhássemos nos gráficos, que era uma distribuição normal.
# Um problema observado é que existem valores ZERADOS. Como é possível uma medida 
# espacial ser 0? y e z tem problemas: em y, enquanto o terceiro quartil é 6, 
# o máximo é 58.900.  Em x, enqunato o terceiro quartil é 4.040, o máximo é 31. 
# Valores que soam anômalos, pelo menos em uma distribuição sobre diamantes. 
# Vamos corrigir tudo isso.

diamonds %>% 
  filter(x != 0) %>% 
  filter(y != 0) %>% 
  filter(z != 0) -> diamonds1 

# Removi os 0 e criei um novo objeto. Agora vamos 
# sumarizar tudo de novo.

diamonds1 %>% 
  summary() 

# Agora não tem zero, mas tem aqueles valores que parecem ser outliers. Vamos verificar.
# 
# Se eu fizer o boxplot, que me mostra os outliers, eu estaria vendo de cada variável.

ggplot(diamonds1)+
  geom_boxplot(aes(y = x))

# Vamos olhar a distribuição dos outliers ENTRE as variáveis.

ggplot(diamonds1)+
  geom_jitter(aes(x = x, y = y))+
  ggplot(diamonds1)+
  geom_jitter(aes(x = x, y = z))+
  ggplot(diamonds1)+
  geom_jitter(aes(x = y, y = z))+
  plot_layout(nrow = 3)

# Tem uns outliers chatinhos...

# Como remover os outliers? O R não tem nenhuma função que eu conheça para tal.
# Então vamos ter que usar os conhecimentos estatísticos  e matemáticos pra isso. 
# Se é outlier tudo que supera em 1.5x o terceiro quartil e está mmais 1.5x do primeiro...

diamonds1 %>% 
  filter(between(x, 1.5 * quantile(diamonds1$x, probs = 0.25), 
                 1.5 * quantile(diamonds1$x, probs = 0.75)),
         between(y, 1.5 * quantile(diamonds1$y, probs = 0.25), 
                 1.5 * quantile(diamonds1$y, probs = 0.75)),
         between(z, 1.5 * quantile(diamonds1$z, probs = 0.25), 
                 1.5 * quantile(diamonds1$z, probs = 0.75))) %>% 
  summary()

# Agora sim, removi alguns (não todos, pois a distribuição mudou). Vamos olhar. 
# E sim, eu sei que remover os outliers é desaconselhado. O Hadley ensina que é melhor
# imputar os valores como NA. Mas isso é só na próxima seção. Pro que ele pede aqui,
# é possível.

diamonds1 %>% 
  filter(between(x, 1.5 * quantile(diamonds1$x, probs = 0.25), 
                 1.5 * quantile(diamonds1$x, probs = 0.75)),
         between(y, 1.5 * quantile(diamonds1$y, probs = 0.25), 
                 1.5 * quantile(diamonds1$y, probs = 0.75)),
         between(z, 1.5 * quantile(diamonds1$z, probs = 0.25), 
                 1.5 * quantile(diamonds1$z, probs = 0.75))) -> diamonds_clean
  
ggplot(diamonds_clean)+
  geom_jitter(aes(x = x, y = y), color = "chartreuse4")+
  ggplot(diamonds_clean)+
  geom_jitter(aes(x = x, y = z), color = "dodgerblue3")+
  ggplot(diamonds_clean)+
  geom_jitter(aes(x = y, y = z), color = "firebrick2")+
  plot_layout(nrow = 3)

# Me parece bem melhor agora. 

# Aí ele pergunta o que é comprimento, o que é largura e o que é profundidade. 
# A profundidade de um diamante é dada pela seguinte imagem (ctrl+enter pra rodar):

image_read_svg('https://upload.wikimedia.org/wikipedia/commons/3/3a/Diamond_facets.svg')

# Se observarmos nas curvas de densidade de cada variável...
ggplot(diamonds_clean)+
  geom_density(aes(x = x), fill = "chartreuse4", alpha = 0.8)+
  geom_density(aes(x = y), fill = "dodgerblue3", alpha = 0.7)+
  geom_density(aes(x = z), fill = "firebrick2", alpha = 0.7)

# temos que Z é uma curva distinta de x e y (que estão sobrepostos). 
# Logo, Z provavelmente é a profundidade (pavillion depth), enquanto x e y são 
# comprimento ou largura. 
# Na documentação (em ?diamonds) a gente descobre que é isso mesmo: z é profundidade,
# x é comprimento e y é largura. 

# ============================================================
# 2. Explora a distribuição de `price`. Há algo errado?

diamonds %>% 
  ggplot()+
  geom_histogram(aes(x = price), bins = 1000)

# Há um buraco em algum lugar ali. Vamos cortar pra enxergar melhor. 

diamonds %>% 
  filter(price, between(price, 1250, 1600)) %>% 
  ggplot()+
  geom_histogram(aes(x = price), bins = 100) # Ali a falha. Faltam dados entre ~1450 e ~1550


# 3. Quantos diamantes tem 0.99 carat? Quantos tem 1 carat? Qual a causa disso?
# carat = quilate. 

diamonds %>%
  filter(carat, between(carat, 0.99, 1)) %>% # ele só quer nesse intervalo
  ggplot()+
  geom_histogram(aes(x = carat))

# Dá pra ver que tem bem poucos com 0.99 e muitos com 1. Quantos?

diamonds %>% 
  filter(carat, between(carat, 0.99, 1)) %>% 
  count(carat)

# 23 com 0.99 e 1558 com 1. Pq a diferença?               Não sei. 
# =================================================================

# 4. Comparar coord_cartesian com xlim e ylim quando damos zoom num histograma.

diamonds %>% 
  ggplot()+
  geom_histogram(aes(x = carat))+
  coord_cartesian(xlim = c(0.9, 1))

diamonds %>% 
  ggplot()+
  geom_histogram(aes(x = carat))+
  coord_cartesian(ylim = c(10000, 15000))

diamonds %>% 
  ggplot()+
  geom_histogram(aes(x = carat))+
  xlim(0.9, 1)  

diamonds %>% 
  ggplot()+
  geom_histogram(aes(x = carat))+
  ylim(10000, 15000)

# Pelo coord_cartesian, o stat é calculado antes. Pelo xlim fora do coord_cartesian, o stat
# é calculado depois... 

# Exercícios 7.4.1 - Missing values

# 1. O que acontece com valores NA no histograma? E num geom_bar? Pq tem diferença?

# Primeiro vamos ver se tem algum NA no banco, em price...
diamonds %>% 
  filter(is.na(price))

# 0. Vamos criar uns, então, e fazer um histograma.

diamonds %>% 
  mutate(price = ifelse(price < 3933, NA, price)) %>%  # 3933 é a média de price.
  ggplot()+
  geom_histogram(aes(x = price)) 

# Resposta: ele acusa um erro:
# Warning message:
# Removed 34283 rows containing non-finite values (stat_bin).

# Agora o de barras. O de barras pede a contagem de uma variável categória. Vamos usar CUT.

diamonds %>% 
  mutate(cut = ifelse(cut != "Premium", cut, NA)) %>% 
  ggplot()+
  geom_bar(aes(x = cut))

# Ele trata o NA como uma categoria, mas como não tem valores, nada é mostrado. 

# 2. o que faz o na.rm = TRUE em mean() e sum()? 
# Ele remove da sumarização (antes dela) os valores que são NA.
# ==============================================================
# 
# 7.5.1.1
#
#
#
# ==============================================================
# 7.5.2.1
#
#
# 
# ==============================================================
# 7.5.3.1
# 
# 
