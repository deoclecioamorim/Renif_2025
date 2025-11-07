# =========================================================================
# Arquivo gerado automaticamente de: 02_extr_dados_raster.qmd
# Data: 05/11/2025 15:57:29
# Renif
# Autor: Prof. Dr. Deoclecio Jardim Amorim
# Versão: 1.0
# =========================================================================

#' 
## -----------------------------------------------------------------------------
#| include: false
library(knitr)
opts_knit$set(global.par = TRUE)
par(mar = c(5, 5, 1, 1))

#' 
#' ## Introdução
#' 
#' Este documento apresenta o processo para extrair variáveis climáticas a partir 
#' de arquivos raster. Usamos os pacotes `sf` e `terra` para manipulação espacial
#' e `writexl` para exportação.
#' 
#' ---
#' 
#' ## 1. Configuração inicial
#' 
## ----config-------------------------------------------------------------------

# Limpar ambiente
rm(list = ls())      
gc(reset = TRUE)     
graphics.off()       

# Pacotes necessários
if(!require(readxl))install.packages("readxl", dep = TRUE, quiet = TRUE)
if(!require(tidyverse))install.packages("tidyverse", dep = TRUE, quiet = TRUE)
if(!require(terra))install.packages("terra", dep = TRUE, quiet = TRUE)
if(!require(geodata))install.packages("geodata", dep = TRUE, quiet = TRUE)
if(!require(geobr))install.packages("geobr", dep = TRUE, quiet = TRUE)
if(!require(sf))install.packages("sf", dep = TRUE, quiet = TRUE)
if(!require(sp))install.packages("sp", dep = TRUE, quiet = TRUE)
if(!require(here))install.packages("here", dep = TRUE, quiet = TRUE)



#' 
#' ---
#' 
#' ## 2. Importação de dados isotópicos
#' 
## ----importacao---------------------------------------------------------------
# Caminho do arquivo de dados
madeira_path <- here("dados","madeiras.csv")

# Caminho do arquivo de dados
madeira_amz <- read.csv(madeira_path, header = TRUE)
colnames(madeira_amz)

# data clean
madeira_amz <- madeira_amz %>% dplyr::select("latitude", "longitude","Site", "Family","d13C_wood")




#' 
#' ---
#' 
#' ## 3. Conversão para objeto espacial `sf`
#' 
#' Nessa etapa, é importante garantir que os dados estejam na mesma projeção dos 
#' arquivos raster, a fim de minimizar os erros durante o processo de extração.
#' 
#' 
## -----------------------------------------------------------------------------
# Converter para objeto sf com CRS SIRGAS 2000 (EPSG:4674)
madeira_amz_proj <- sf::st_as_sf(madeira_amz , coords = c("longitude", "latitude"), crs = 4674)

# Visualizar pontos
plot(madeira_amz_proj[1], pch = 20, col = "black", main = NA, axes = TRUE, graticule = TRUE)


#' 
#' ---
#' 
#' ## 4. Carregamento e empilhamento de rasters
#' 
#' Lembre-se de que agora vamos utilizar os arquivos raster gerados na etapa anterior, 
#' os quais foram armazenados na pasta **raster**. 
#' 
#' O processo a seguir irá criar um único raster multicamada utilizando a 
#' função `rast` do pacote `terra`.
#' 
## ----stack--------------------------------------------------------------------
# Listar arquivos raster na pasta
raster_files <- list.files(path = here("raster"), pattern = "\\.tif$", full.names = TRUE)

# Carregar como objetos raster
raster_list <- lapply(raster_files, terra::rast)

# Empilhar como SpatRaster
r_stack <- terra::rast(raster_list)

# Nomear camadas com base nos arquivos
names(r_stack) <- tools::file_path_sans_ext(basename(raster_files))


#' 
#' ---
#' 
#' ## 5. Projeção e extração dos valores climáticos
#' 
#' Com o raster multicamada obtido, podemos realizar a extração dos valores climáticos de acordo com as coordenadas de interesse. 
#' 
#' A função `extract`, do pacote `terra`, oferece dois métodos para extrair valores em pontos: **“simples”** e **“bilinear”**. 
#' 
#' - No modo **simples**, é retornado o valor da célula raster em que o ponto se encontra.
#' - No modo **bilinear**, o valor retornado é uma interpolação bilinear calculada a partir das quatro células raster mais próximas do ponto.
#' 
#' 
#' 
## -----------------------------------------------------------------------------
# Reprojetar raster se necessário
if (!sf::st_crs(madeira_amz_proj) == sf::st_crs(r_stack)) {
  r_stack <- terra::project(r_stack, sf::st_crs(madeira_amz_proj)$wkt)
}

# Extrair valores
extracted_values <- terra::extract(r_stack, madeira_amz_proj, method = "bilinear")

# Combinar com dados originais
madeira_amz_proj <- cbind(madeira_amz_proj, extracted_values[, -1])


#' 
#' ---
#' 
#' ## 6. Exportação do conjunto final
#' 
#' A etapa final consiste em exportar a planilha de dados e dar início ao processo de modelagem, com a implementação dos modelos estatísticos.
#' 
#' 
## ----exportacao---------------------------------------------------------------
# Extrair coordenadas
coordinates <- sf::st_coordinates(madeira_amz_proj)

# Converter para data.frame e reorganizar
madeira_amz_proj_df <- madeira_amz_proj %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(x = coordinates[, 1], y = coordinates[, 2]) %>%
  dplyr::relocate(x, y)

# Criar pasta (caso não exista)
if (!dir.exists(here("dados"))) dir.create(here("dados"))


# Exportar para Excel
writexl::write_xlsx(madeira_amz_proj_df, here("dados", "madeira_amz_var_clim.xlsx"))

# Visualizar primeiras linhas
head(madeira_amz_proj_df)

#' 
#' ---
#' 
#' ## Considerações finais
#' 
#' O procedimento descrito neste capítulo é útil para preparar conjuntos de dados georreferenciados com atributos climáticos, os quais podem ser utilizados em análises espaciais ou como variáveis preditoras em modelos de machine learning.
