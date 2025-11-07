# pacotes
library(geodata)
library(terra)

# 1) Baixar limites estaduais do Brasil (GADM nível 1)
#    (ajuste 'path' para onde você quer guardar os arquivos baixados)
br_estados <- geodata::gadm(country = "Brazil", level = 1, path = "data_gadm")

# 2) Lista de UFs que compõem a Amazônia Legal
ufs_amz_legal <- c(
  "Acre", "Amazonas", "Amapá", "Maranhão",
  "Mato Grosso", "Pará", "Rondônia", "Roraima", "Tocantins"
)

# 3) Filtrar esses estados (a coluna costuma ser NAME_1 no GADM)
amz_legal_estados <- br_estados[br_estados$NAME_1 %in% ufs_amz_legal, ]

# 4) Dissolver fronteiras internas para virar um único polígono
amz_legal <- terra::aggregate(amz_legal_estados)

# (opcional) garantir geometria válida
amz_legal <- terra::makeValid(amz_legal)

# (opcional) reprojetar para SIRGAS 2000 (EPSG:4674) ou outra CRS de interesse
# amz_legal <- terra::project(amz_legal, "EPSG:4674")

# 5) Salvar como Shapefile (ou melhor ainda, como GeoPackage)
terra::writeVector(amz_legal, "amazonia_legal.shp", overwrite = TRUE)
# alternativa recomendada (formato moderno, sem limitações do .shp):
# terra::writeVector(amz_legal, "amazonia_legal.gpkg", overwrite = TRUE)

plot(amz_legal)
