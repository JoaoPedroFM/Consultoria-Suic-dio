{library(tidyverse)
library(dplyr)
library(readxl)
library(epiDisplay)
library(CARBayesdata)
library(sp)
library(dplyr)
library(sf)
library(ggplot2)
library(spdep)
library(spatialreg)
library(rgdal)
library(leaflet)}

base = read_csv2("suicidios.csv")
municipio = read_excel("Municipios_2010.xlsx")
sul = base |> 
  mutate(UF= str_sub(Microrregiao, 1, 2)) |> 
  filter(UF== 41|UF== 42|UF== 43)

sul$Microrregiao = as.character(sul$Microrregiao)

# fazendo o mapa da proporcao
prop = sul |> 
  dplyr::select(c(1,2)) |> 
  group_by(Microrregiao) |> 
  summarise(media = mean(suicidio_paf))

library(sf)
PR = st_read("PR_Microrregioes_2020.shp")
SC = st_read("SC_Microrregioes_2020.shp")
RS = st_read("RS_Microrregioes_2020.shp")

SUL_SF = rbind(PR, SC, RS)

# juntando as informacoes numa base unica
prop <- left_join(x=SUL_SF, y=prop, by=c("CD_MICRO"="Microrregiao"))
head(prop)

colours <- colorNumeric(palette = "YlOrRd", domain = prop$media)
map1 <- leaflet(data=prop) |>
  addTiles() |>
  addPolygons(fillColor = ~colours(media), color="", weight=1,
              fillOpacity = 0.8) |>
  addLegend(pal = colours, values = prop$media, opacity = 1,
            title="média") |>
  addScaleBar(position="bottomleft")
map1
# mais escuros, maiores os precos
# localmente ha areas que formam clusters em relacao aos precos

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

# Tabela de frequencias para as variaveis qualitativas
tab1(sul$suicidio_paf,graph = F)
tab1(sul$id_legal,graph = F)
tab1(sul$trab_armado,graph = F)
tab1(sul$sexo,graph = F)
tab1(sul$raca,graph = F)
tab1(sul$estado_civil,graph = F)
tab1(sul$escolaridade,graph = F)


# # IDADE
# #Histograma da idade dos candidatos (populacao geral)
# sul |> 
#   ggplot(mapping = aes(x = idade)) +
#   geom_histogram(fill= "darkred", col="white")+
#   # scale_x_continuous(breaks = seq(15,90,5))+
#   scale_y_continuous(label = scales::label_number(big.mark = ".",
#                                                   decimal.mark = ",")) +
#   labs(y= "Frequencia", x="Idade") + theme_classic() + 
#   theme(legend.position = "none", plot.title = element_text(hjust = 0.5, 
#                                                             size=14, face="bold"), 
#         text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))

summary(sul$idade)
sd(sul$idade)

################################################################################
# suicidio_paf x raca
arma_raca <- sul |> 
  group_by(raca,suicidio_paf) |> 
  summarize(n = n())  |>
  mutate(pct = n/sum(n),
         rotulo = scales::percent(pct))

# adicionando os percentuais no gráfico
arma_raca |>
  ggplot(mapping = aes(x = raca,
                       y = pct,
                       fill = factor(suicidio_paf))) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(label = rotulo), 
            size = 3, 
            position = position_stack(vjust = 0.5))  +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Raça",
       y = "Proporção",
       fill = "Uso de arma de fogo no suicídio",
       title="Perfil das pessoas que cometeram suicídio por arma de fogo por raça",
       subtitle="Região sul") + theme_classic() +
  theme(legend.position = "top")  +
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"),
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

################################################################################
# suicidio_paf x sexo
arma_sexo <- sul |> 
  group_by(sexo,suicidio_paf) |> 
  summarize(n = n()) |> 
  mutate(pct = n/sum(n),
         rotulo = scales::percent(pct))

# adicionando os percentuais no gráfico
arma_sexo |>
  ggplot(mapping = aes(x = sexo,
                       y = pct,
                       fill = factor(suicidio_paf))) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(label = rotulo), 
            size = 3, 
            position = position_stack(vjust = 0.5))  +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Sexo",
       y = "Proporção",
       fill = "Uso de arma de fogo no suicídio",
       title="Perfil das pessoas que cometeram suicídio por arma de fogo por sexo ",
       subtitle="Região sul") + theme_classic() +
  theme(legend.position = "top")  +
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"),
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

################################################################################
# suicidio_paf x estado civil
arma_civil <- sul |> 
  group_by(estado_civil,suicidio_paf) |> 
  summarize(n = n()) |> 
  mutate(pct = n/sum(n),
         rotulo = scales::percent(pct))

# adicionando os percentuais no gráfico
arma_civil |>
  ggplot(mapping = aes(x = estado_civil,
                       y = pct,
                       fill = factor(suicidio_paf))) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(label = rotulo), 
            size = 3, 
            position = position_stack(vjust = 0.5))  +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Estado Civil",
       y = "Proporção",
       fill = "Uso de arma de fogo no suicídio",
       title="Perfil das pessoas que cometeram suicídio por arma de fogo por estado civil",
       subtitle="Região sul") + theme_classic() +
  theme(legend.position = "top")  +
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"),
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))


################################################################################
# suicidio_paf x escolaridade
arma_esc <- sul |> 
  group_by(escolaridade,suicidio_paf) |> 
  summarize(n = n()) |> 
  mutate(pct = n/sum(n),
         rotulo = scales::percent(pct))

# adicionando os percentuais no gráfico
arma_esc |>
  ggplot(mapping = aes(x = escolaridade,
                       y = pct,
                       fill = factor(suicidio_paf))) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(label = rotulo), 
            size = 3, 
            position = position_stack(vjust = 0.5))  +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Escolaridade",
       y = "Proporção",
       fill = "Uso de arma de fogo no suicídio",
       title="Perfil das pessoas que cometeram suicídio por arma de fogo por escolaridade",
       subtitle="Região sul") + theme_classic() +
  theme(legend.position = "top")  +
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"),
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))




################################################################################
# MODELAGEM

#Carregando pacote
library(rstanarm)
library(spBayes)
library(CARBayes)

# juntando as informacoes numa base unica
municipio$Microrregiao = as.character(municipio$Microrregiao)
municipio = municipio |> 
  filter(UF== 41|UF== 42|UF== 43) |> 
  distinct(Microrregiao, Nome_Micro) 
sul_mun <- left_join(x=sul, y=municipio, by="Microrregiao")
head(sul_mun)

# ajuste= stan_glm(formula = suicidio_paf ~ idade + id_legal + trab_armado
#                  + sexo + raca + estado_civil + escolaridade + Nome_Micro, 
#                  family="binomial"(link="logit"), data = sul_mun,
#                  prior_intercept = normal(0,10),
#                  refresh = 0,
#                  chain = 2,
#                  iter = 10000,
#                  warmup = 2000,
#                  thin = 4)

# save(ajuste, file="ajuste.Rdata")
load("ajuste.Rdata")

#Visualizando o ajuste
ajuste$stanfit

data_ajuste = as.data.frame(ajuste[["stan_summary"]])
data_ajuste = data_ajuste |> 
  dplyr::select(mean, `2.5%`, `97.5%`) |> 
  rename(IC1=`2.5%`, IC2=`97.5%`) |> 
  mutate(mean= round(mean,2), IC1= round(IC1, 2), IC2= round(IC2, 2)) |> 
  filter(IC1<0, IC2>0)

data_ajuste$nome = rownames(data_ajuste)

# data_ajuste = data_ajuste |> 
#   dplyr::select(nome, everything()) 

# openxlsx::write.xlsx(data_ajuste_reorg, file="data_ajuste_reorg.xlsx")


#Fazendo a leitura do arquivo Base saude.xlsx
data_ajuste_reorg = data_ajuste |> 
  gather(key = "IC",
         value = "valor",
         2:3)

data_ajuste_reorg = data_ajuste_reorg |> 
  dplyr::select(nome, IC, valor, mean) |> 
  mutate(exp_mean = exp(mean))

data_ajuste_reorg = data_ajuste_reorg |> 
  mutate(exp_mean = round(exp_mean,2))

# comparando expectativa de vida nos dois anos
data_ajuste_reorg |> 
  ggplot(mapping = aes(x= valor, 
                       y = nome)) +
  geom_point(aes(color = factor(IC))) +
  geom_vline(xintercept=0, col="green") +
  geom_line(aes(group = nome)) +
  labs (x = "",
        y = "Variável",
        color = "Limite do \nIntervalo",
        title = "Intervalos de 95% de credibilidade",
        subtitle = "IC1 = 2.5% e IC2 = 97.5%") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(linetype = "dashed"))+
  scale_y_discrete(guide = guide_axis(n.dodge = 2)) 

#Visualizando convergência
plot(ajuste, 
     plotfun = "combo", 
     regex_pars = "raca")

##################################################################################
# # Ajuste 2- sem a microrregiao Cerro Azul
# 
# sul_mun2 = sul_mun |> 
#   filter(Nome_Micro != "Cerro Azul")
# 
# # ajuste2= stan_glm(formula = suicidio_paf ~ idade + id_legal + trab_armado
# #                  + sexo + raca + estado_civil + escolaridade + Nome_Micro,
# #                  family="binomial"(link="logit"), data = sul_mun2,
# #                  prior_intercept = normal(0,10),
# #                  refresh = 0,
# #                  chain = 2,
# #                  iter = 10000,
# #                  warmup = 2000,
# #                  thin = 4)
# 
# # save(ajuste2, file="ajuste2.Rdata")
# load("ajuste2.Rdata")
# 
# #Visualizando o ajuste2
# ajuste2$stanfit

