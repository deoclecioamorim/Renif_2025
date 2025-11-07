# =========================================================================
# Arquivo gerado automaticamente de: 03_integracao_ML.qmd
# Data: 05/11/2025 15:57:29
# Renif
# Autor: Prof. Dr. Deoclecio Jardim Amorim
# Versão: 1.0
# =========================================================================

#' 
## -----------------------------------------------------------------------------
#| include: false

# Ajuste global para gráficos
library(knitr)
opts_knit$set(global.par = TRUE)
par(mar = c(5, 5, 1, 1))


#' 
#' # Introdução 
#' 
#' Neste módulo, aprenderemos a integrar dados geoespaciais com modelos de **Machine Learning** para gerar mapas preditivos (isoscapes).
#' 
#' O objetivo aqui é mostrar como **aprendizado de máquina, como Random Forest**, pode ser usado para mais eficientes para prever padrões espaciais de isótopos.
#' 
#' Durante o exercício, você irá:
#' 
#' - Preparar dados de amostras combinados com covariáveis ambientais;
#' - Ajustar modelos de machine learning, por exemplo, o modelo **Random Forest**;
#' - Avaliar a importância das variáveis no modelo;
#' - Gerar um **mapa preditivo espacial** do δ13C.
#' 
#' 
#' 
#' ---
#' 
#' 
#' # 1. Configuração inicial
#' 
## ----config-------------------------------------------------------------------
# Limpar área de trabalho
rm(list = ls())      
gc(reset = TRUE)     
graphics.off()       

#Pacotes necessários
if(!require(readxl))install.packages("readxl", dep = TRUE, quiet = TRUE)
if(!require(tidyverse))install.packages("tidyverse", dep = TRUE, quiet = TRUE)
if(!require(terra))install.packages("terra", dep = TRUE, quiet = TRUE)
if(!require(geodata))install.packages("geodata", dep = TRUE, quiet = TRUE)
if(!require(geobr))install.packages("geobr", dep = TRUE, quiet = TRUE)
if(!require(sf))install.packages("sf", dep = TRUE, quiet = TRUE)
if(!require(sp))install.packages("sp", dep = TRUE, quiet = TRUE)
if(!require(here))install.packages("here", dep = TRUE, quiet = TRUE)
if(!require(caret))install.packages("caret", dep = TRUE, quiet = TRUE)
if(!require(randomForest))install.packages("randomForest", dep = TRUE, quiet = TRUE)
if(!require(ranger))install.packages("ranger", dep = TRUE, quiet = TRUE)
if(!require(VSURF))install.packages("VSURF", dep = TRUE, quiet = TRUE)



#' 
#' # 2. Reprodutibilidade 
#' 
#' A reprodutibilidade é um princípio fundamental da ciência, pois garante que os resultados obtidos em uma pesquisa possam ser verificados e replicados por outros pesquisadores ou em futuras análises. Assegurar que os procedimentos sejam replicáveis aumenta a transparência, a credibilidade dos dados e fortalece a robustez das conclusões.
#' 
#' 
## ----seed---------------------------------------------------------------------
set_reproducibility <- function(seed = 1350) {
  set.seed(seed)
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
}

# Fixando a reprodutibilidade
set_reproducibility()

#' 
#' # 3. Carregando o conjunto de dados 
#' 
## ----importacao_dados---------------------------------------------------------
amz_var_clim <- readxl::read_excel(here::here("dados", "madeira_amz_var_clim.xlsx"), sheet = 1)
head(amz_var_clim) #leitura das primeiras 6 linhas
str(amz_var_clim) #Estrutura dos dados


#' 
#' 
#' # 4. Preparação dos dados
#' 
#' Vamos definir a variável resposta e as covariáveis que serão utilizadas na modelagem.
#' 
## ----resp_covar---------------------------------------------------------------

#Tratando NAs
d13.amz <- amz_var_clim[!is.na(amz_var_clim$d13C_wood), ]


# Transformar tibble em SpatVector (definindo x e y como coordenadas)
sites <- vect(as.data.frame(d13.amz), geom = c("x", "y"), crs = "EPSG:4674")

#Retirando coordenadas
d13.amz <- d13.amz %>% dplyr::select(-x, -y, -Site, -Family)

#Definindo a variável resposta e covariáveis 
resposta <- d13.amz$d13C_wood

#Preditoras
preditoras <- d13.amz%>% dplyr::select(-d13C_wood)

#Checando valores ausentes
if (anyNA(preditoras)) {
  cat("Warning: Missing values found in predictors. Please handle them before proceeding.\n")
  print(which(is.na(predictors), arr.ind = TRUE))
} else {
  cat("No missing values found in predictors.\n")
}



#' 
#' # 2 Ajuste dos modelos
#' 
#' ## Modelo Random Forest
#' 
#' Trabalharemos com a Random Forest, que faz parte de uma família 
#' de métodos chamada Árvores de Classificação e Regressão (CART). 
#' 
## ----rf1----------------------------------------------------------------------
#Nome da variável preditora
pred.names<- names(preditoras)

#Definindo a formula do modelo de regressão
formula_rf1 <- as.formula(paste("d13C_wood ~", paste(pred.names, collapse = " + ")))
formula_rf1

#Modelo RF com o pacotes ramdomForest
#Fixando a reprodutibilidade
set_reproducibility()

rf.mod1  <- randomForest(
  formula_rf1,
  data = d13.amz,
  ntree = 2000,
  importance = TRUE,
  keep.forest = TRUE
)

rf.mod1


#' 
#' Os resultados do modelo RF com todas as variáveis sugere que mais de 51% da
#' variância é explicada. O valor de MSE ficou em torno de 1.24.
#' 
#' Vamos dar uma olhada rápida no ajuste do modelo:
#' 
#' 
## ----plotRF-------------------------------------------------------------------

# Criar um data frame com Observado e Predito
dados_plot <- data.frame(
  Observado = d13.amz$d13C_wood,
  Predito = rf.mod1$predicted
)

# Plotar com ggplot2
ggplot(dados_plot, aes(x = Observado, y = Predito)) +
  geom_point(shape = 21, fill = "white", size = 3, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  labs(
    x = expression("Mensurado "*delta^{13}*"C"["Madeira"]),
    y = expression("Predito "*delta^{13}*"C"["Madeira"])
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12))


#' 
#' 
#' ## Seleção inicial de variáveis
#' 
#' Até agora, ignoramos amplamente a seleção de variáveis. Durante o desenvolvimento 
#' do modelo RF faz sentido permitir que o modelo explore o máximo possível de 
#' características do conjunto de dados, para que possa descobrir as relações preditivas 
#' mais úteis. Porém ao mesmo tempo, o uso de conjuntos de características excessivos 
#' ou muito redundantes pode tornar o ajuste do modelo mais desafiador. Assim, é ideal 
#' utilizar alguma abordagem de pré-seleção de variáveis. Diante disso, optamos por
#' utilizar o algoritmo VSURF.
#' 
#' 
## ----vsurf--------------------------------------------------------------------

# Convertendo as variáveis preditoras para o formato matricial (requirido pelo VSURF)
preditoras_matrix <- as.matrix(preditoras)

# Rodando VSURF para seleção de variáveis
# Fixando a reprodutibilidade
set_reproducibility()

vsurf_result <- VSURF::VSURF(preditoras_matrix, resposta, 
                             ntree = 500,
                             nfor.thres = 20,
                             nfor.interp = 100,
                             nfor.pred = 10,
                             nsd = 1,
                             parallel = FALSE,
                             verbose=FALSE)


#Extração das variáveis selecionadas VSURF 
threshold_vars <- names(preditoras)[vsurf_result$varselect.thres]
interp_vars    <- names(preditoras)[vsurf_result$varselect.interp]
pred_vars      <- names(preditoras)[vsurf_result$varselect.pred]

#Print das variáveis
cat("\nVariaveis selecionadas (Threshold):\n")
print(threshold_vars)

cat("nVariaveis selecionadas (Interpretação):\n")
print(interp_vars)

cat("nVariaveis selecionadas (Predição):\n")
print(pred_vars)



#' 
#' ---
#' 
#' O algoritmo VSURF selecionou as variáveis `vapr_mean` e `elev_mean`. 
#' 
#' 
## ----rfVSURF------------------------------------------------------------------

#Construindo a formula
formula_rf2 <- as.formula(paste("d13C_wood ~", paste(pred_vars, collapse = " + ")))
formula_rf2

#Fixando a reprodutibidade
set_reproducibility()

rf.mod2 <- randomForest(
  formula_rf2,
  data = d13.amz,
  ntree = 2000,
  importance = TRUE,
  keep.forest = TRUE
)

rf.mod2

#Plot error OOB  vs número de árvores
plot(rf.mod2, main = "OOB Error vs Number of Trees")


#' 
#' ## Ajuste de Hiperparâmetros
#' 
#' No modelo RF o principal parâmetro é o `mtry` determina quantas características diferentes são
#' consideradas na seleção da regra de decisão em cada nó.
#' 
#' 
## ----tune---------------------------------------------------------------------

rf.tune <- caret::train(formula_rf2,
                        data = d13.amz,
                        ntree = 500)
rf.tune$results
rf.tune$bestTune


#' 
#' 
#' Os melhores resultados são obtidos usando o menor valor de `mtry` (2). Esse é 
#' o valor que teria sido escolhido por padrão pela função `randomForest()`, 
#' portanto, aceitar o valor padrão para esse hiperparâmetro é adequado neste caso.
#' 
#' 
#' ## Teste
#' 
#'  Um teste simples de divisão de dados é fácil de realizar.
#' 
#' 
## ----splitData----------------------------------------------------------------

# Fixando a reprodutibilidade
set_reproducibility()
partition <- caret::createDataPartition(d13.amz$d13C_wood, p = 0.8, list = FALSE)
treino <- d13.amz[partition, ]
teste <- d13.amz[-partition, ]

# AJustando o modelo com dados de treino
rf.train <-  randomForest(formula_rf2, data = treino, ntree = 500)
rf.train

# Data frame Observado x Predito
dados_plot2 <- data.frame(
  Observado = teste$d13C_wood,
  Predito = predict(rf.train, teste)
)

# Calcular MSE
MSE_value <- mean((dados_plot2$Predito - dados_plot2$Observado)^2)
MSE_value

# Calcular R2
R2_value<- (cor(dados_plot2$Observado, dados_plot2$Predito))^2
R2_value

# ggplot com anotação
ggplot(dados_plot2, aes(x = Observado, y = Predito)) +
  geom_point(shape = 21, fill = "white", size = 3, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1.2) +
  labs(
    x = expression("Mensurado "*delta^{13}*"C"["Madeira"]),
    y = expression("Predito "*delta^{13}*"C"["Madeira"])
  ) +
  annotate("text",
           x = min(dados_plot$Observado) + 0.95 * diff(range(dados_plot$Observado)),
           y = min(dados_plot$Predito) + 0.05 * diff(range(dados_plot$Predito)),
           label = paste0("MSE = ", round(MSE_value, 2)),
           hjust = 1, vjust = 0, size = 5) +
  annotate("text",
           x = min(dados_plot$Observado) + 0.95 * diff(range(dados_plot$Observado)),
           y = min(dados_plot$Predito) + 0.2 * diff(range(dados_plot$Predito)),
           label = paste0("R2 = ", round(R2_value, 2)),
           hjust = 1, vjust = 0, size = 5) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )



#' 
#' O código abaixo é uma ligeira modificação do que usamos acima e usa o
#' objeto `fitControl` para direcionar a função `train()` para realizar o que 
#' chamamos de validação cruzada. Para isso, optamos por utilizar o método 
#' Leave-Group-Out CV (também conhecido como Monte Carlo CV).
#' 
#' 
## ----CV-----------------------------------------------------------------------

# Leave-Group-Out CV (também conhecido como Monte Carlo CV).
fitControl <- caret::trainControl(method = "LGOCV", number = 10, p = 0.8)

# Fixando a reprodutibidade
set_reproducibility()
rf.mod3 <-caret::train(formula_rf2,
                  data= d13.amz, method="rf",
                  trControl=fitControl)


rf.mod3$resample
rf.mod3




#' 
#' 
#' Obtemos um conjunto de métricas ligeiramente diferente aqui, mas elas são muito semelhantes às obtidas em nosso teste de dados com divisão única. A otimização e o teste de modelos de ML são um tema vasto. Para obter mais informações e exemplos de diferentes abordagens que podem ser aplicadas na modelagem CART, há um excelente texto online que acompanha o [pacote caret](https://topepo.github.io/caret/index.html).
#' 
#' 
#' 
#' ## Compreendendo o modelo
#' 
#' Entender os modelos de ML (também chamados de *ML interpretável*) também é um tema vasto e em constante desenvolvimento, mas existem algumas ferramentas simples que podem nos ajudar a começar a entender como nosso modelo está se comportando. 
#' 
#' Veja o gráfico de importância das variáveis, que mostra basicamente o que seu nome sugere.
#' 
#' 
#' 
## ----VIP----------------------------------------------------------------------
varImpPlot(rf.mod3$finalModel,main='Variable Importance Plot: Base Model')

imp<-varImp(rf.mod3$finalModel)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  

imp


#' 
#' Observa-se que a variável ``vapr_mean`` tem o maior impacto para δ13C da madeira.
#' 
#' 
#' # Isoscape
#' 
#' Nessa etapa, vamos recuperar os arquivos raster gerados na primeira etapa e criar um único raster multicamada, que será utilizado para realizar a predição espacial com a função `predict` do pacote `terra`.
#' 
#' ---
#' 
## ----predEspacial-------------------------------------------------------------
# Listar todos os arquivos raster na pasta 'raster/' 
raster_files <- list.files(path = here("raster"), pattern = '\\.tif$', full.names = TRUE)  

# Carregar todos os rasters em uma lista 
# terra ->> para carregar cada arquivo raster
raster_list <- lapply(raster_files, terra::rast)  
raster_list

# Empilhar os rasters em um único objeto SpatRaster
# terra ->> para empilhar todos os rasters em um único objeto
r_stack <- terra::rast(raster_list)  

# Verificar os nomes dos rasters 
names(r_stack) <- basename(raster_files) %>% tools::file_path_sans_ext()  
print(names(r_stack))  
class(r_stack)


#' ## Predição 
#' 
#' Finalmente, utilizaremos a nossa melhor predição do modelo RF para construir um mapa espacial
#' do `d13C_wood`.  
#' 
## ----isoscape-----------------------------------------------------------------
#Predição
isoscape <- terra::predict(r_stack, rf.mod3, na.rm = TRUE)


#' 
#' 
#' ## Calculando os desvio padrão da isoscape
#' 
#' Para o cáculo do desvio padrão utilizaremos a seguinte expressão:
#' 
#' $$ SD \approx \frac{Q_{0.84} - Q_{0.16}}{2} $$
#' Assim, obtemos uma estimativa consistente de SD espacial (incerteza) a partir do intervalo central de 68% das predições do modelo QRF.
#' 
## -----------------------------------------------------------------------------

# Usando o pacote "ranger".
# Set reproducibility
set_reproducibility()
mod.qrf <- ranger::ranger(formula_rf2, data=d13.amz, num.trees = 500, quantreg = TRUE)
mod.qrf

# Quantis ~ ±1 DP (68%)
isoscape$ci.16 <- terra::predict(r_stack, mod.qrf, type = "quantiles", quantiles = 0.16,
                        na.rm = TRUE)
isoscape$ci.84 <- terra::predict(r_stack, mod.qrf, type = "quantiles", quantiles = 0.84,
                        na.rm = TRUE)

# SD aproximado a partir de Q0.84 - Q0.16
isoscape$sd <- (isoscape$ci.84 - isoscape$ci.16) / 2


#' 
#' Exportando os mapas gerados.
#' 
## -----------------------------------------------------------------------------
# Verificar e criar diretório para salvar os rasters, se não existir
if (!dir.exists(here("isoscape"))) dir.create(here("isoscape"))

#Exportando raster
isoscape.d13c <- c(isoscape$lyr1, isoscape$sd)

terra::writeRaster(isoscape.d13c, filename = here("isoscape", "isoscape_dc13.tif"), overwrite = TRUE)


#' 
#' ---
#' 
#' Vejamos um plot simples da Isoscape gerada pela nossa predição.
#' 
## ----plotIsoscape-------------------------------------------------------------
# Visualizar a isoscape
terra::plot(isoscape[[1]], main = "δ13C")
# Adicionar os pontos amostrais
points(sites, pch = 21, bg = "yellow", cex = 0.8)


#' 
## ----plotIsoscapeSD-----------------------------------------------------------
# Visualizar a isoscape
terra::plot(isoscape$sd, main = "SD")
# Adicionar os pontos amostrais
points(sites, pch = 21, bg = "yellow", cex = 0.8)


#' 
#' 
#' 
