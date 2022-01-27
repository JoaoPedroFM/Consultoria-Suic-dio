library(tidyverse)
library(readxl)
library(epiDisplay)

base = read_csv2("suicidios.csv")
municipio = read_excel("Municipios_2010.xlsx")
sul = base |> 
  mutate(UF= str_sub(Microrregiao, 1, 2)) |> 
  filter(UF== 41|UF== 42|UF== 43)
sul = mutate_if(sul, is.character, as.factor)



sul$Microrregiao = as.factor(sul$Microrregiao)


#Tratando a sul de dados
sul = na.omit(sul) |>  # 2 observacoes com valores faltantes
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
                                          "8 a 11 anos","12 anos ou mais","Ignorado")))
         

################################################################################
# ANALISE EXPLORATORIA

# Tabela de frequencias
tab1(sul$suicidio_paf,graph = F)
tab1(sul$id_legal,graph = F)
tab1(sul$trab_armado,graph = F)
tab1(sul$sexo,graph = F)
tab1(sul$raca,graph = F)
tab1(sul$estado_civil,graph = F)
tab1(sul$escolaridade,graph = F)


# IDADE
#Histograma da idade dos candidatos (população geral)
sul |> 
  ggplot(mapping = aes(x = idade)) +
  geom_histogram(fill= "darkred", col="white")+
  # scale_x_continuous(breaks = seq(15,90,5))+
  scale_y_continuous(label = scales::label_number(big.mark = ".",
                                                  decimal.mark = ",")) +
  labs(y= "Frequência", x="Idade") + theme_classic() + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, 
                                                            size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))


################################################################################
# MODELAGEM

#Carregando pacote
library(rstanarm)

#Ajustando o modelo
ajuste = stan_glm(formula = suicidio_paf ~ Microrregiao + idade + id_legal
                  + trab_armado + sexo + raca + estado_civil + escolaridade,
                  data = sul,
                  family = binomial(link = "logit"),
                  prior_intercept = normal(0,10),
                  refresh = 0,
                  chain = 2,
                  iter = 1000,
                  warmup = 200,
                  thin = 4)

#Visualizando o ajuste
ajuste$stanfit

save(ajuste, file="ajuste.rdata")
load("ajuste.rdata")

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
