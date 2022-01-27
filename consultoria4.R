library(tidyverse)
library(readxl)

base = read_csv2("suicidios.csv")
municipio = read_excel("Municipios_2010.xlsx")
 
#Tratando a base de dados
base = base |> 
  mutate(suicidio_paf = factor(suicidio_paf, labels = c("Nao","Sim")),
         id_legal = factor(id_legal, labels = c("Nao","Sim")),
         trab_armado = factor(trab_armado, labels = c("Nao","Sim")),
         sexo = factor(sexo, labels = c("Masculino","Feminino")),
         raca = factor(raca, labels = c("Branca","Preta","Amarela",
                                        "Parda","Indigena","Ignorado")),
         estado_civil = factor(estado_civil, 
                             labels = c("Solteiro","Casado","Viuvo",
                                        "Separado","Divorciado","Ignorado")),
         escolaridade = factor(escolaridade, 
                               labels = c("Nenhuma","1 a 3 anos","4 a 7 anos",
                                          "8 a 11 anos","5 a 12 anos","Ignorado")))
         

#Visualizando a base
base

#Carregando pacote
library(rstanarm)

#Ajustando o modelo
ajuste = stan_glm(formula = suicidio_paf ~ .,
                  data = base,
                  family = binomial(link = "logit"),
                  prior_intercept = normal(0,10),
                  refresh = 0,
                  chain = 2,
                  iter = 10000,
                  warmup = 2000,
                  thin = 4)

#Visualizando o ajuste
ajuste$stanfit

#Plotando os intervalos de credibilidade
plot(ajuste, prob = 0.9)

#Calculando intervalo de credibilidade
posterior_interval(object = ajuste, 
                   regex_pars = "id_legal", 
                   prob = 0.8)

#Visualizando convergencia
plot(ajuste, 
     plotfun = "combo", 
     regex_pars = "id_legal")

#Entendo as prioris utilizadas
prior_summary(ajuste)
