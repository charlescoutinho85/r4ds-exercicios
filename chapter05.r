# Data Transformation

# Carregar tidyverse e nycflights13
pacman::p_load("tidyverse", "nycflights13")

flights <- nycflights13::flights
glimpse(flights)

niver <- flights %>% 
  filter(month == 5, day == 15) # Filtra todos os vôos de 15 de Maio

# Comparações
sqrt(2) ^ 2 == 2 # Falso! Mas se fizermos aproximação...
near(sqrt(2) ^ 2, 2) # Verdadeiro! 

# Operators
# | é "ou", & é "e", ! é "não" e xor é "um ou outro, mas não um e outro"

# %in% 
mai_jun <- filter(flights, month == 5 | month == 6)
mai_jun <- filter(flights, month %in% c(5,6))

# Vôos que não estejam atrasados mais de 2h, tanto na partida quanto na chegada

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

## 5.2.4 Exercícios 
# 1.1
flights %>% 
  filter(arr_delay >= 120)
# 1.2
flights %>% 
  filter(dest %in% c("IAH", "HOU"))
# 1.3
flights %>% 
  filter(carrier %in% c("UA", "AA", "DL"))
# 1.4           
flights %>% 
  filter(month %in% 7:9)
# 1.5
flights %>% 
  filter(arr_delay > 120, dep_delay <= 0)
# 1.6           
flights %>% 
  filter(dep_delay >= 60, dep_delay - arr_delay > 30)
# 1.7
flights %>% 
  filter(dep_time <= 600 | dep_time == 2400)
# OU usando módulo %%, já que meia noite não é 0, mas 2400. Se dividirmos 600 
# (6 da manhã, por exemplo) por 2400 e pegarmos o módulo, teremos 600 ainda. Se
# tirarmos 2400 de 2400, teremos 0. Logo, usando módulo fica mais fácil pois:
# dividindo a hora de partida por 2400, se o módulo for menor que 600, significa
# que o vôo saiu antes das seis da manhã, inclusive. 
flights %>% 
  filter(dep_time %% 2400 <= 600)

# 2. between() é a mesma coisa que x >= left & x <= right,
# ou seja, se um valor cai no intervalo, sendo usado da seguinte forma:
# between(x, left, right)
# Poderíamos ter reescrito a 1.4 da seguinte forma:
flights %>% 
  filter(between(month, 7, 9))

# 3. 
filter(flights, is.na(dep_time))

# 4.1 NA ^ 0 é 1 pois x^0 = 1
# 4.2 NA | TRUE é TRUE pois x | TRUE é x ou verdadeiro, e isso sempre é verdadeiro.
# já NA & FALSE é falso pois x & FALSE é sempre falso pra todos os valores
# 4.3 NA * 0 = NA pois qualquer valor que seja multiplicado pelo infinito (ou 
# menos infinito) é indefinido. 

## 5.3 Arrange()
flights %>% 
  arrange(desc(dep_delay)) # Organiza por ordem decrescente o atraso na partidas

## 5.3.1 Exercícios
## 1. 
flights %>% 
  arrange(desc(is.na(dep_time), dep_time))

## 2.1
flights %>% 
  arrange(desc(dep_delay))
## 2.2
flights %>% 
  subset(., dep_delay < 0) %>% 
  arrange(dep_delay) %>% 
  View()
## 3.
flights %>% 
 arrange(air_time) %>% 
 View() # Se "fastest" (mais rápido) se referir a tempo no ar

flights %>% 
  arrange((distance *1.609) / (air_time * 60)) %>% 
  View() # Se "fastest" se referir a velocidade, convertendo em milha pra km e minuto pra hora
## 4. 
flights %>% 
  arrange(air_time) %>% 
  View() # Viajou menos, se for tempo
flights %>% 
  arrange(distance) %>% 
  View() # Viajou menos, se for distância
flights %>% 
  arrange(desc(air_time)) %>% 
  View() # Viajou mais, se for tempo
flights %>% 
  arrange(desc(distance)) %>% 
  View() # Viajou mais, se for distância

## 5.4.1 Exercícios
# 1. 
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, 4, 6, 7, 9)
select(flights, 4, 6:7, 9)
select(flights, dep_time, dep_delay:arr_time, arr_delay)
select(flights, starts_with("dep_"), starts_with("arr_"))
# 2. O select() ignora a repetição
# 3.  one_of() pega um vetor de nomes de columas (salvo em objeto anteriormente) e seleciona
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))
# 4. contains() ignora a capitalização da letra, podendo ser maiúscula ou minúscula.
# Para mudar isso, é só adicionar ignore.case = FALSE
select(flights, contains("TIME", ignore.case = FALSE))
 

## 5.5 Mutate
# O mutate() permite criar novas variáveis em função das existentes

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)
mutate(flights_sml,
       gain = dep_delay - arr_delay, # Ganho, diminuindo o atraso da chegada do atraso da 
       # partida
       speed = (distance*1.609) / air_time * 60) %>%  # Velocidade, dividindo distância pelo tempo em ar
       View()
# O transmute() deixa só as variáveis criadas
transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)


## 5.5.2 Exercícios

## 1. Pegando o inteiro da divisão por 100, temos a hora. Multiplicando por 60, temos a 
## quantidade de minutos. Somando o módulo (o resto) da divisão por 100, temos os minutos
## isolados + a hora em minutos. Dividindo por 1440, e pegando o módulo, pegamos ou o mesmo
## número de minutos, em todas as horas, ou 0, no caso de meia-noite (1440 minutos passados
## em 1 dia)

flights %>% 
  mutate(dep_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         sched_dep_mins = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440)

## 2. 
flights$air_time
flights$arr_time - flights$dep_time

# É esperado que airtime = arrtime - deptime

flights_compare <- flights %>% 
  mutate(dep_time = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         arr_time = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
         air_time_dif = air_time - arr_time + dep_time)
nrow(filter(flights_compare, air_time_dif != 0)) # Ou seja, diferente de zero
# 327150 casos
nrow(flights_compare) # Ver total de casos
# 336776 casos, ou seja, existem vôos que a diferença não é zero. Alguns voos podem ser 
# posteriores a meia-noite, por isso gera o problema. Ou então os vôos cruzaram algum
# fuso diferente, o que também geraria o erro. Basta plotar. 

flights_compare %>% 
  ggplot(aes(air_time_dif)) +
  geom_histogram(bins = 100, binwidth = 1)
# Se fosse o caso das hipóteses citadas, tudo estaria concentrado em 60, pois mudaria somente
# a hora (não tendo um fuso que seja de meia hora, i.e.)  
# Um dia resolvo isso. 

## 3. É esperado que dep_time menos sched_dep_time seja igual a dep_delay
flights_partida <- flights %>% 
  mutate(dep_time_min = (dep_time %% 100 + dep_time%/% 100*60) %% 1440,
         sched_dep_time_min = (sched_dep_time %% 100 + sched_dep_time %/% 100*60) %% 1440,
         dep_delay_diff = dep_delay - dep_time_min + sched_dep_time_min)

filter(flights_partida, dep_delay_diff != 0)        
# Não é igual a zero! Muitos são diferentes de zero. Não é problema de fuso, já
# que se refere somente a partidas. Vamos plotar...
ggplot(filter(flights_partida, dep_delay_diff>0),
       aes(y= sched_dep_time_min, x = dep_delay_diff))+
  geom_point() # o Data usado já é um filter, tidyverse é maravilhoso...
# Ou seja, todos os vôos estão em 1440, ou seja, foram feitos meia noite. Por 
# conta disso não são zero.

## 4. 
(flights_atrasados <- flights %>% 
  mutate(dep_delay_max = min_rank(desc(dep_delay))) %>%  # Cria a variável em ordem decrescente
    filter(!dep_delay_max > 10) %>% # Pega todas as maiores que 10 e exclui
    arrange(dep_delay_max) %>% # Define a nova variável como a ordem
    select(dep_delay_max, year:day, dep_delay, everything())) # Joga a nova pra frente da tabela

## 5. Ele printa os números de 2 a 10, e insere ao final 12 e 11
## avisando que o comprimento do objeto maior não é múltiplo do comprimento do
## objeto menor.
1:3 + 1:10

## 6. 
pi # O R tem um base::pi com o valor de pi
sin(pi)
cos(pi)
tan(pi)
asin(-0.5)
acos(-0.5)
atan(-0.5)



### 5.6 Grouped Summaries
# O summarize funde, a partir de uma função (média, mediana, soma, etc), os dados da tabela

summarise(flights, delay = mean(dep_delay, na.rm=TRUE)) # A média dos atrasos, tirando os NA

# Usando o group_by para agregar (só para fins estatísticos, não diretamente) por ano, mês e dia
flights %>% 
  group_by(year, month, day) %>% 
  summarise(delay = mean(dep_delay, na.rm=TRUE))

# Introdução ao pipe (finalmente!)
# O Hadley cria um código enorme sem o pip,e no livro, e depois insere o pipe. Antes de ver
# o resultado dele, vou tentar reescrever o código grande usando o pipe. 

flights %>% 
  group_by(dest) %>% # Agrupa por destino
  summarise(count = n(), # Contagem de observações
            dist = mean(distance, na.rm=TRUE), # Média da Distância 
            delay = mean(arr_delay, na.rm=TRUE)) %>% # Média do atraso da chegada
  filter(delay, count > 20, dest != "HNL") %>% # Filtra por observações acima de 20 com destino diferente de HNL (Honolulu)
  ggplot(aes(dist, delay))+
  geom_point(aes(size = count), alpha = 1/3)+
  geom_smooth(se= FALSE, method = "loess")
# CHECK! o pipe faz com que x %>% f(y) seja f(x, y) e x %>% f(y) %>% g(z) seja g(f(x,y),z)

# É importante remover os NA (com na.rm= TRUE) para que os resultados não sejam NA.

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% group_by(year, month, day) %>% summarise(mean = mean(dep_delay))


## 5.6.7 Exercícios
## 1. Mais importante é o atraso na chegada, pois pode atrasar os próximos vôos (entendendo
## que a maioria não faz vôos diretos). Um atraso na partida pode ser compensado no ar, 
## enquanto um atraso na chegada, não. 

## 2. 
not_cancelled %>% count(dest) 

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(n = n())

not_cancelled %>% count(tailnum, wt = distance)

not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(n = sum(distance))

## 3. O mais importante é arr_delay, oa traso na chegada (vide questão 1)
flights %>% 
  filter(is.na(dep_delay), is.na(arr_delay)) %>% 
  select(dep_time, dep_delay, arr_time, sched_arr_time, arr_delay)

## 4. Quando mais vôos, maior o número de cancelados. 
cancelados <- flights %>% 
  mutate(cancelado = (is.na(arr_delay) | is.na(dep_delay))) %>% 
  group_by(year,month,day) %>% 
  summarise(ncancelado = sum(cancelado),
            nvoos = n())
  
ggplot(filter(cancelados, ncancelado < 300), aes(nvoos, ncancelado)) +
  geom_point()+
  geom_smooth(method="lm")

# a proporção é vista em:

(cancelados_atrasados <- flights %>% 
  mutate(cancelados = (is.na(arr_delay) | is.na(dep_delay))) %>% 
  group_by(year, month, day) %>% 
  summarise(propocancel = mean(cancelados),
            mediadepdelay = mean(dep_delay, na.rm=TRUE),
            mediaarrdelay = mean(arr_delay, na.rm=TRUE)))
ggplot(cancelados_atrasados)+
  geom_point(aes(x = mediadepdelay,y = propocancel), color = "red")+
  geom_point(aes(x = mediaarrdelay, y = propocancel), color = "darkblue")
## Ambos crescem juntos, praticamente na mesma proporção 


## 5. 
flights %>% group_by(carrier, dest) %>% summarise(n())

flights %>% group_by(carrier) %>% summarise(arr_delay = mean(arr_delay, na.rm=TRUE)) %>% 
  arrange(desc(arr_delay))
# F9 - Frontier Airlines é a mais atrasada. Não entendi muito bem a separação entre aeroportos
# ruins e companhias ruins. Entendo que posso montar uma tabela pra cada, mas fazer a conexão
# entre ambas, não sei. 

## 6. sort coloca em ordem crescente de n. é Booleano (true or false)

##. 5.7 Grouped mutates and filters

flights %>% 
  group_by(year, month, day) %>% 
  filter(rank(desc(arr_delay))<10) # Os que mais atrasam a chegada

flights %>% 
  group_by(dest) %>% 
  filter(n() >= 365) %>%  # Destinos mais populares (com pelo menos 1 voo por dia)
  select(dest, everything())

flights %>% 
  group_by(dest) %>% 
  filter(n() >= 365) %>%  # Destinos mais populares (com pelo menos 1 voo por dia)
  filter(arr_delay>0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% # Proporção (arr_Delay sobre a soma de arr_delay)
  select(prop_delay, year:day, arr_delay)

## 5.7.1 Exercises

# 1. O group_by, mutate e filter funcionam com mean (média), sum(soma), median(mediana), 
# std(desvio padrão, lead (valor anterior), lag (pr'óximo valor), min_rank(ordem crescente), 
# row_number (número da observação). As matemáticas ( +, -, %%, %/%), lógicas (< == > etc),
# logarítmicas (log, log2, log10), 

# 2. 
flights %>% 
  group_by(tailnum) %>% 
  select(tailnum, everything()) %>% 
  filter(!is.na(tailnum)) %>% 
  mutate(ontimerec = !is.na(arr_time) & (arr_delay <= 0)) %>% 
  group_by(tailnum) %>% 
  summarise(ontimerec = mean(ontimerec), n = n()) %>% 
  filter(n>=23) %>% # com quantile(count(flights, tailnum)$n) dá pra descobrir os quantis. 23 é 25%
    filter(min_rank(ontimerec)==1)

# 3. 
flights %>% 
  group_by(hour) %>% 
  select(hour, everything()) %>% 
  summarise(arr_delay = mean(arr_delay, na.rm=TRUE)) %>% 
  arrange(arr_delay)

#4. 
flights %>% 
  filter(arr_delay >0) %>%
  group_by(dest, origin, carrier, flight) %>% 
  mutate(arr_delay = sum(arr_delay)) %>% 
  group_by(dest) %>% 
  mutate(propdelay = arr_delay / sum(arr_delay)) %>% 
  arrange(dest, desc(propdelay)) %>% 
  select(carrier, flight,origin,dest, propdelay)

#5. 
(lagdelay <- flights %>% 
  arrange(origin, month, day, dep_time) %>% 
  group_by(origin) %>% 
  mutate(lagdepdelay = lag(dep_delay)) %>% # pega o próximo valor de dep_delay
  filter(!is.na(dep_delay), !is.na(lagdepdelay)) %>% 
  group_by(lagdepdelay) %>% 
  summarise(mediadepdelay = mean(dep_delay)) %>% 
  ggplot(aes(lagdepdelay, mediadepdelay))+
  geom_point()+
  geom_smooth(method=loess)+
  scale_x_continuous(breaks=seq(0,1500, by=60))) # Marca por hora 

#6. Primeiro, estandardizar ((x-media(x))/desviopadrao(x))
stdflights <- flights %>% 
  filter(!is.na(air_time)) %>% # Remove os NA
  group_by(dest, origin) %>% # Agrupa por destino e origem
  mutate(mediaairtime = mean(air_time),
         sdairtime = sd(air_time),
        n = n()) %>% # Número de observações
  ungroup() %>% 
  mutate(stdairtime = (air_time - mediaairtime)/(sdairtime+1)) # Adiciona 1 pois existe 0

ggplot(stdflights, aes(stdairtime))+
  geom_density() # Cauda maior pra direita 

stdflights %>% 
  arrange(stdairtime) %>% 
  select(flight, origin, dest, month, day, air_time, mediaairtime, stdairtime) %>% 
  head(5) 
  
# 7. 
atleast2carriers <- flights %>% 
  select(dest, carrier) %>% 
  group_by(dest,carrier) %>% 
  filter(row_number()==1) %>% 
  group_by(dest) %>% 
  mutate(n_carrier = n_distinct(carrier)) %>% 
  filter(n_carrier >= 2)

carrierdest <- atleast2carriers %>% 
  group_by(carrier) %>% 
  summarise(n_dest = n()) %>% 
  arrange(desc(n_dest))
head(carrierdest)
# EV tem 51 destinos, é a ExpressJet 

# 8. 
flights %>% 
  arrange(tailnum, year, month, day) %>% 
  group_by(tailnum) %>% 
  mutate(delay1h = dep_delay>60,
         delayant = cumsum(delay1h)) %>% 
  filter(delayant < 1) %>% 
  count(sort=TRUE)
  