library(tidyverse)
library(dplyr)
library(readxl)
library(epiDisplay)
{library(CARBayesdata)
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
sul = mutate_if(sul, is.numeric, as.factor)
sul$idade = as.numeric(sul$idade)

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
            title="media") |>
  addScaleBar(position="bottomleft")
map1
# mais escuros, maiores os precos
# localmente ha areas que formam clusters em relacao aos precos

# #Tratando a sul de dados
# sul = na.omit(sul) |>  # 2 observacoes com valores faltantes
#   mutate(suicidio_paf = factor(suicidio_paf, labels = c("Nao","Sim")),
#          id_legal = factor(id_legal, labels = c("Nao","Sim")),
#          trab_armado = factor(trab_armado, labels = c("Nao","Sim")),
#          sexo = factor(sexo, labels = c("Masculino","Feminino")),
#          raca = factor(raca, labels = c("Branca","Preta","Amarela",
#                                         "Parda","Indigena","Ignorado")),
#          estado_civil = factor(estado_civil,
#                                labels = c("Solteiro","Casado","Viuvo",
#                                           "Separado","Divorciado","Ignorado")),
#          escolaridade = factor(escolaridade,
#                                labels = c("Nenhuma","1 a 3 anos","4 a 7 anos",
#                                           "8 a 11 anos","12 anos ou mais","Ignorado")))

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
#Histograma da idade dos candidatos (populacao geral)
sul |> 
  ggplot(mapping = aes(x = idade)) +
  geom_histogram(fill= "darkred", col="white")+
  # scale_x_continuous(breaks = seq(15,90,5))+
  scale_y_continuous(label = scales::label_number(big.mark = ".",
                                                  decimal.mark = ",")) +
  labs(y= "Frequencia", x="Idade") + theme_classic() + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, 
                                                            size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))

summary(sul$idade)
sd(sul$idade)

################################################################################
# MODELAGEM

#Carregando pacote
library(rstanarm)
library(spBayes)
library(CARBayes)

# juntando as informacoes numa base unica
SUL_SF <- left_join(x=SUL_SF, y=sul, by=c("CD_MICRO"="Microrregiao"))
head(SUL_SF)

ajuste_esp = S.glm(formula = suicidio_paf ~ idade + id_legal + trab_armado
                            + sexo + raca + estado_civil + escolaridade, 
                            family="binomial", data = SUL_SF, burnin=10,
                            trials= rep(50,21946), n.sample= 1000)


conf = data.frame(ajuste_esp$summary.results)
nome = c("(Intercept)","idade","id_legal","trab_armado","sexo","raca",
         "estado_civil","escolaridade")
nome = as.data.frame(nome)
conf = cbind(conf, nome)


#Fazendo a leitura do arquivo Base saude.xlsx
conf = conf |> 
  gather(key = "IC",
         value = "valor",
         2:3)

conf

# comparando expectativa de vida nos dois anos
conf |> 
  filter(nome!="(Intercept)") |> 
  ggplot(mapping = aes(x= valor, 
                       y = reorder(nome, valor, max))) +
  geom_point(aes(color = IC)) +
  geom_line(aes(group = nome)) +
  geom_vline(xintercept=0, col="red") +
  labs (x = "Expectativa de vida (anos)",
        y = "UF",
        color = "Ano",
        title = "Expectativa de vida por UF",
        subtitle = "Ano de 2010") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(linetype = "dashed"))

