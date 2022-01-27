library(tidyverse)
library(dplyr)
library(readxl)
library(epiDisplay)

base = read_csv2("suicidios.csv")
municipio = read_excel("Municipios_2010.xlsx")
parana = base |> 
  mutate(UF= str_sub(Microrregiao, 1, 2)) |> 
  filter(UF== 41)
parana$Microrregiao = as.character(parana$Microrregiao)

library(sf)
PR = st_read("PR_Microrregioes_2020.shp")
# juntando as informacoes numa base unica
parana <- left_join(x=PR, y=parana, by=c("CD_MICRO"="Microrregiao"))
head(parana)

# fazendo o mapa da proporcao
prop = parana |> 
  dplyr::select(c(1:4,13)) |> 
  group_by(NM_MICRO) |> 
  summarise(media = mean(suicidio_paf))

colours <- colorNumeric(palette = "YlOrRd", domain = prop$media)
map1 <- leaflet(data=prop) |>
  addTiles() |>
  addPolygons(fillColor = ~colours(media), color="", weight=1,
              fillOpacity = 0.8) |>
  addLegend(pal = colours, values = prop$media, opacity = 1,
            title="media") |>
  addScaleBar(position="bottomleft")
map1
# mais escuros, maiores os preços
# localmente há áreas que formam clusters em relação aos preços