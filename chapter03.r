## Capítulo 3 - Data visualisation ===========================

## 3.1.1 - Pré requisitos - carregar o tidyverse
## Se não estiver instalado, rodar install.packages("tidyverse") e aguardar

pacman::p_load("tidyverse", "cowplot", "brazilmaps")

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


## 3.7 - Statistical Transformations

glimpse(diamonds)
ggplot(diamonds) +
  geom_bar(aes(x = cut))

# Cut não é uma variável de diamonds. Ainda assim, aparece no gráfico. O que
# acontece é que alguns gráficos passam por transformações, como 
# os de barra,histograma e frequência, que mostram os bins dos dados. 
# Outros, como o smooth, criam um modelo e dão predições. Outros, como os boxplots,
# sumarizam os dados. Daí a função "stat".

ggplot(diamonds)+
  stat_count(aes(x=cut)) # Geoms e stats são intercambeáveis, dando no mesmo.

demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

# Mudando pra proporção ao invés de contagem:
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

# Mudando pra sumarização
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# Qualquer dúvida, é só digitar ?stat_TÉCNICA

# 1. stat_summary usa o pointrange. A dificuldade está no fato de que geom_pointrange
# não calcula automaticamente os pontos mínimo e máximo. Logo, é preciso explicitar
# com o stat_summary:
ggplot(data = diamonds) +
  geom_pointrange(mapping = aes(x = cut, y = depth),
                  stat = "summary",
                  fun.ymin = min,
                  fun.ymax = max,
                  fun.y = median)

# 2. geom_bar faz com que a altura seja proporcional ao número de casos (usa 
# stat_count) enquanto geom_col representa valores na altura (usa stat_identity)
# 3. Sem tempo de fazer todos os pares, mas é o básico: pointrange - summary,
# identity - col, count - bar, bin - histograma, smooth - smooth etc
# 4.  Y (valor predito), ymin e ymax (intervalo de confiança) e erro padrão
# 5. Se não for explicitado group = 1, o geom_bar vai agrupar pelo x (no caso,
# cut) fazendo com que todos tenham a mesma proporção. Com =1, ou qualquer coisa, 
# as proporções de cada nível são relativas a todos os níveis de cut. 
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

# 3.8 Position Adjustments
# Para gerar um gráfico empilhado:
p1 <- ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")
p2 <- ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
p3 <- ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")
p4 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
plot_grid(p1, p2,p3,p4, nrow = 2, ncol = 2) # Exemplo utilizando o pacote cowplot pra 
# mais de um gráfico na mesma visualização
# O position Jitter pode ser também usado em geom_jitter

## 3.8.1 - Exercícios
# 1. O problema é que há um overplotting. É possível melhorá-lo com jitter. 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cty, y = hwy), position = "jitter")

# 2. width e height, em % (0.0 a 1.0)
# 3. geom_count conta o número de observações em cada cruzamento e plota nos pontos
ggplot(data = mpg) + 
  geom_count(mapping = aes(x = displ, y = hwy))

# 4. position_dodge2
ggplot(data = mpg, aes(x = drv, y = hwy, colour = class)) +
  geom_boxplot(show.legend = FALSE)

## 3.9 - Coordinate Systems
ggplot(data = mpg, mapping = aes(x = class, y = hwy, color = class)) + 
  geom_boxplot() +
  coord_flip()
# Para virar os gráficos. 

world <- map_data("world")

ggplot(world, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()+
  theme_bw()
# Ajusta o mapa

ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)+
  coord_polar()+theme_bw()
# Faz u mgráfico coxcomb

## 3.9.1 Exercícios
# 1. Sem theta = y, o gráfico fica irregular. theta = y gera uma espécie de gráfico alvo
ggplot(mpg, aes(x = class, fill = drv))+
  geom_bar()+
  coord_polar(theta = "y")

# 2. Adiciona títulos nos eixos, no gráfico, e legenda
# 3. coord_map() faz uma projeção mercator (passando de 3d para 2d), já o quickmap leva a 
# uma projeção aproximada, mas mais rápida
# 4. O gráfico mostra uma correlação positiva entre cty e hwy. coord_fixed é importante 
# para travar as coordenadas e elas não serem ajustadas pelos dados. geom_abline cria uma 
# linha de referência.

## 3.10 : The layered grammar of graphics
## NÃO RODAR
ggplot(data = <DATA>) + 
  <GEOM_FUNCTION>(
  mapping = aes(<MAPPINGS>),
  stat = <STAT>, 
  position = <POSITION>) +
  <COORDINATE_FUNCTION> +
  <FACET_FUNCTION>





