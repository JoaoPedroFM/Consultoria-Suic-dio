library(tidyverse)
library(dplyr)
library(readxl)
library(epiDisplay)
library(CARBayes)
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
# 
# sul_med <- sul |> 
#   dplyr::select(-UF) |> 
#   group_by(Microrregiao) |> 
#   summarise(suicidio_paf=mean(suicidio_paf), idade=mean(idade), id_legal=mean(id_legal),
#             trab_armado=mean(trab_armado), sexo=median(sexo), raca = median(raca),
#             estado_civil=median(estado_civil), escolaridade=median(escolaridade))


sul$Microrregiao <- as.character(sul$Microrregiao)

sul = na.omit(sul) |>  # 2 observacoes com valores faltantes
  mutate(id_legal = factor(id_legal, labels = c("Nao","Sim")),
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



library(sf)
PR = st_read("PR_Microrregioes_2020.shp")
SC = st_read("SC_Microrregioes_2020.shp")
RS = st_read("RS_Microrregioes_2020.shp")

SUL_SF = rbind(PR, SC, RS)

SUL_SF <- left_join(x=SUL_SF, y=sul, by=c("CD_MICRO"="Microrregiao"))


 
# polig <- as(SUL_SF, "Spatial")
# 
# polig <- merge(x=polig, y=sul, by="CD_MICRO", all.x=FALSE)
# 
# sul_sp <- SpatialPolygonsDataFrame(Sr=polig@polygons, data=SUL_SF, match.ID = TRUE)
# 
# nc_sp <- sf:::as_Spatial(SUL_SF$geometry)
# 
# try_sp <- as(SUL_SF, "Spatial")
# 
# save(try_sp, file="base.RData")
# 
# load("base.RData")








library(rstanarm)

ajuste= stan_glm(formula = suicidio_paf ~ idade + id_legal + trab_armado
                   + sexo + raca + estado_civil + escolaridade, 
                   family="binomial"(link="logit"), data = SUL_SF,
                 prior_intercept = normal(0,10),
                 refresh = 0,
                 chain = 2,
                 iter = 1000,
                 warmup = 20,
                 thin = 4)

ajuste$stanfit


ajuste_esp = S.glm(formula = suicidio_paf ~ idade + id_legal + trab_armado
                   + sexo + raca + estado_civil + escolaridade, 
                   family="binomial", data = SUL_SF, burnin=10,
                   trials= rep(50,21946), n.sample= 1000)


print(ajuste_esp)

# cOM S.CARdissimilarity
ajuste_dis = S.CARdissimilarity(formula = suicidio_paf ~ idade + id_legal + trab_armado
                   + sexo + raca + estado_civil + escolaridade, 
                   family="binomial", data = SUL_SF, burnin=10, Z=Z,
                   trials= rep(50,21946), n.sample= 1000)








SUL_SF$r_car <- ajuste_esp[["residuals"]][["response"]]
SUL_SF$r_ind <- ajuste$residuals

# residuos CAR vs IND
ggplot(SUL_SF,aes(x = r_car , y = r_ind)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col=2, lwd=1.1) +
  labs(x='resíduos CAR',y='resíduos IND', title= "CAR vs IND - resíduos") +
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5))

