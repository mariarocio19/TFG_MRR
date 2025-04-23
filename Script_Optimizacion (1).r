
library(tidyquant)   # Para descargar datos de Yahoo Finance y manipularlos de forma eficiente
library(dplyr)       # Para manipulación de datos
library(ggplot2) 
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI)
library(ROI.plugin.symphony)
library(ROI.plugin.glpk)
library(DEoptim)
library(foreach)
library(tidyr)
library(ROI.plugin.quadprog)
library(tidyverse)
# Para graficar

acciones <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", "NVDA", "META", "BABA", "NFLX", "JPM",
              "V", "MA", "PYPL", "DIS", "KO", "PEP", "XOM", "CVX", "PFE", "MRNA",
              "NIO", "TCEHY", "BIDU", "BP", "RIO", "SAN", "HSBC", "CSCO", "INTC", "ORCL",
              "ABNB", "UBER", "SHOP", "SQ", "TSM", "ZM", "SNAP", "DDOG", "DOCU", "PLTR",
              "VALE", "BHP", "BCE", "UL", "PG", "T", "TM", "HMC", "NSANY")


datos_acciones <- tq_get(
  x    = acciones,
  get  = "stock.prices",
  from = "2014-12-31",    # Fecha de inicio
  to   = "2024-12-21"     # Fecha de fin (futura, ojo con la disponibilidad real de datos)
)

# Si deseas verificar rápidamente la estructura de los datos:
head(datos_acciones)

--------------------------------------------------------------------------------
# Convertimos a formato ancho (tabla de precios ajustados)
datos_wide <- datos_acciones %>%
select(symbol, date, adjusted) %>%
pivot_wider(names_from = symbol, values_from = adjusted) %>%
arrange(date)

# Eliminamos filas con NA
datos_wide <- na.omit(datos_wide)

dim(datos_wide)

# Convertimos a un xts (objeto de series temporales), útil para PerformanceAnalytics
library(xts)

precios_xts <- xts(datos_wide[,-1], order.by = datos_wide$date)

# Calculamos rentabilidades mensuales (podrías usar weeklyReturn, dailyReturn, etc.)
precios_mensuales <- to.monthly(precios_xts, indexAt = "lastof", OHLC = FALSE)
rentabilidades <- Return.calculate(precios_mensuales, method = "log")
rentabilidades <- na.omit(rentabilidades)

dim(rentabilidades)
colnames(rentabilidades)

# Contar los NA por columna
nas_por_col <- colSums(is.na(datos_wide))

# Escoge un umbral para excluir columnas con demasiados NA
umbral <- 10  # Ajusta según tu criterio
columnas_validas <- names(nas_por_col)[nas_por_col < umbral]

# Filtra solo las columnas que aprueban ese criterio
datos_wide_filtrado <- datos_wide %>%
  select(date, all_of(columnas_validas))

# Luego ya harías na.omit(), etc.
datos_wide_filtrado <- na.omit(datos_wide_filtrado)


--------------------------------------------------------------------------------
  
# Extraemos la lista de activos
activos <- colnames(rentabilidades)

# Creamos la especificación de la cartera
port_spec <- portfolio.spec(assets = activos)

# Añadimos restricciones: fully invested (suma pesos = 1) y long only (pesos >= 0)
port_spec <- add.constraint(port_spec, type = "full_investment")
port_spec <- add.constraint(port_spec, type = "long_only")

# Añadimos objetivos para la media (retorno) y la StdDev (riesgo)
# Si prefieres var o VaR, adaptalo aquí.
port_spec <- add.objective(port_spec, type = "return", name = "mean")
port_spec <- add.objective(port_spec, type = "risk", name = "StdDev")

--------------------------------------------------------------------------------
opt_random <- optimize.portfolio(
  R               = rentabilidades,
  portfolio       = port_spec,
  optimize_method = "random",
  search_size     = 5000,  # Número de carteras aleatorias a muestrear
  trace           = TRUE
)

opt_random

--------------------------------------------------------------------------------
opt_de <- optimize.portfolio(
  R               = rentabilidades,
  portfolio       = port_spec,
  optimize_method = "DEoptim",
  search_size     = 1500,   # Ajusta según tu capacidad de cómputo
  trace           = TRUE
)

opt_de
--------------------------------------------------------------------------------
opt_pso <- optimize.portfolio(
  R               = rentabilidades,
  portfolio       = port_spec,
  optimize_method = "pso",
  search_size     = 1000,  # Iteraciones del enjambre
  trace           = TRUE
)

opt_pso
--------------------------------------------------------------------------------
# Extraemos estadísticas y pesos de cada optimización (puede que uses extractObjectiveMeasures() 
# según tu versión de PortfolioAnalytics)
  
 

# Extraer pesos óptimos
weights_random <- extractWeights(opt_random)
weights_de     <- extractWeights(opt_de)
weights_pso    <- extractWeights(opt_pso)

# Rentabilidad media anualizada para cada método
mean_random <- mean(Return.portfolio(rentabilidades, weights = weights_random))
mean_de     <- mean(Return.portfolio(rentabilidades, weights = weights_de))
mean_pso    <- mean(Return.portfolio(rentabilidades, weights = weights_pso))

# Desviación estándar anualizada para cada método
sd_random <- StdDev(Return.portfolio(rentabilidades, weights = weights_random))
sd_de     <- StdDev(Return.portfolio(rentabilidades, weights = weights_de))
sd_pso    <- StdDev(Return.portfolio(rentabilidades, weights = weights_pso))

# Tabla comparativa (sin errores)
comparison_table <- data.frame(
  Method = c("Random", "DEoptim", "PSO"),
  Mean   = c(mean_random, mean_de, mean_pso),
  StdDev = c(sd_random, sd_de, sd_pso)
)

print(comparison_table)


#Imprimimos los pesos óptimos
print(extractWeights(opt_random))
print(extractWeights(opt_de))
print(extractWeights(opt_pso))

--------------------------------------------------------------------------------
#Gráficos: 
# Extraer los pesos óptimos de cada optimización
pesos_random <- extractWeights(opt_random)
pesos_deoptim <- extractWeights(opt_de)
pesos_pso <- extractWeights(opt_pso)

# Convertir los pesos a un DataFrame para visualización
df_pesos <- data.frame(
  Activo = names(pesos_random),  # Extraer nombres de los activos
  Random = as.numeric(pesos_random), 
  DEoptim = as.numeric(pesos_deoptim), 
  PSO = as.numeric(pesos_pso)
)

df_pesos_largo <- df_pesos %>%
  pivot_longer(cols = c(Random, DEoptim, PSO), names_to = "Método", values_to = "Peso")

# Seleccionar los 15 activos con mayor peso en al menos una optimización
top_15_activos <- df_pesos_largo %>%
  group_by(Activo) %>%
  summarise(Max_Peso = max(Peso)) %>%
  top_n(15, Max_Peso) %>%
  pull(Activo)

df_top_pesos <- df_pesos_largo %>% filter(Activo %in% top_15_activos)

# Gráfico de comparación de pesos
ggplot(df_top_pesos, aes(x = Activo, y = Peso, fill = Método)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de Pesos Óptimos en Diferentes Métodos de Optimización",
       x = "Activo",
       y = "Peso Asignado",
       fill = "Método de Optimización") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotamos etiquetas para mejor visibilidad

# Filtrar solo los pesos de PSO
df_pso <- df_pesos %>%
  select(Activo, PSO) %>%
  filter(PSO > 0.01)  # Excluir pesos menores a 1% para mejor visibilidad

# Gráfico de torta
ggplot(df_pso, aes(x = "", y = PSO, fill = Activo)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribución de Pesos en la Optimización PSO") +
  theme_void()
--------------------------------------------------------------------------------
#Gráficos Económicos: 
# Obtener rentabilidad y riesgo de cada activo (media y desviación estándar de las rentabilidades históricas)
rentabilidades_medias <- colMeans(rentabilidades, na.rm = TRUE)
volatilidades <- apply(rentabilidades, 2, sd, na.rm = TRUE)

# Calcular rentabilidad esperada y riesgo total de cada optimización
retorno_random <- sum(df_pesos$Random * rentabilidades_medias)
riesgo_random <- sqrt(t(df_pesos$Random) %*% cov(rentabilidades) %*% df_pesos$Random)

retorno_deoptim <- sum(df_pesos$DEoptim * rentabilidades_medias)
riesgo_deoptim <- sqrt(t(df_pesos$DEoptim) %*% cov(rentabilidades) %*% df_pesos$DEoptim)

retorno_pso <- sum(df_pesos$PSO * rentabilidades_medias)
riesgo_pso <- sqrt(t(df_pesos$PSO) %*% cov(rentabilidades) %*% df_pesos$PSO)

# Ratio de Sharpe (suponiendo una tasa libre de riesgo de 0.02 mensual)
tasa_libre_riesgo <- 0.02
sharpe_random <- (retorno_random - tasa_libre_riesgo) / riesgo_random
sharpe_deoptim <- (retorno_deoptim - tasa_libre_riesgo) / riesgo_deoptim
sharpe_pso <- (retorno_pso - tasa_libre_riesgo) / riesgo_pso

# Crear DataFrame con los resultados
df_resultados <- data.frame(
  Método = c("Random", "DEoptim", "PSO"),
  Rentabilidad = c(retorno_random, retorno_deoptim, retorno_pso),
  Riesgo = c(riesgo_random, riesgo_deoptim, riesgo_pso),
  Sharpe = c(sharpe_random, sharpe_deoptim, sharpe_pso)
)

print(df_resultados)  # Ver tabla con los resultados


ggplot(df_resultados, aes(x = Riesgo, y = Rentabilidad, color = Método, label = Método)) +
  geom_point(size = 5) +
  geom_text(vjust = -0.5) +
  labs(title = "Relación Rentabilidad-Riesgo en Diferentes Métodos de Optimización",
       x = "Volatilidad (Desviación Estándar del Portafolio)",
       y = "Rentabilidad Esperada",
       color = "Método") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(df_resultados, aes(x = Método, y = Sharpe, fill = Método)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Comparación del Ratio de Sharpe en Diferentes Métodos",
       x = "Método de Optimización",
       y = "Ratio de Sharpe") +
  theme_minimal()


# Generar carteras simuladas aleatorias
simulaciones <- 5000
pesos_sim <- replicate(simulaciones, runif(ncol(rentabilidades)))
pesos_sim <- apply(pesos_sim, 2, function(x) x / sum(x))

rentabilidad_sim <- apply(pesos_sim, 2, function(w) sum(w * rentabilidades_medias))
riesgo_sim <- apply(pesos_sim, 2, function(w) sqrt(t(w) %*% cov(rentabilidades) %*% w))

# Verificar la estructura de df_frontera
print(colnames(df_frontera))

# Si df_frontera no tiene una columna "Método", la añadimos como NA para evitar conflictos
if (!"Método" %in% colnames(df_frontera)) {
  df_frontera$Método <- NA
}

# Agregar las carteras optimizadas asegurando la misma estructura de columnas
df_frontera <- rbind(df_frontera,
                     data.frame(Riesgo = c(riesgo_random, riesgo_deoptim, riesgo_pso),
                                Rentabilidad = c(retorno_random, retorno_deoptim, retorno_pso),
                                Método = factor(c("Random", "DEoptim", "PSO")))) # Convertir a factor para evitar errores


# Agregar las carteras optimizadas
df_frontera <- rbind(df_frontera,
                     data.frame(Riesgo = c(riesgo_random, riesgo_deoptim, riesgo_pso),
                                Rentabilidad = c(retorno_random, retorno_deoptim, retorno_pso),
                                Método = c("Random", "DEoptim", "PSO")))

# Graficamos la Frontera Eficiente
ggplot(df_frontera, aes(x = Riesgo, y = Rentabilidad)) +
  geom_point(alpha = 0.3, color = "gray") +  # Carteras simuladas
  geom_point(data = df_resultados, aes(color = Método), size = 4) +
  geom_text(data = df_resultados, aes(label = Método), vjust = -0.5) +
  labs(title = "Frontera Eficiente y Métodos de Optimización",
       x = "Riesgo (Volatilidad)",
       y = "Rentabilidad Esperada") +
  theme_minimal()


# Instala ggplot2 si no lo tienes
# install.packages("ggplot2")

library(ggplot2)

# Crear el dataframe
datos <- data.frame(
  Metodo = c("Random", "DEoptim", "PSO"),
  Rentabilidad = c(0.00622, 0.00739, 0.00391),
  Riesgo = c(0.0368, 0.0378, 0.0454),
  Sharpe = c(0.4296, 0.5253, 0.1730)
)

# Gráfico de barras del ratio de Sharpe
ggplot(datos, aes(x = Metodo, y = Sharpe, fill = Metodo)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(Sharpe, 2)), vjust = -0.5) +
  labs(
    title = "Comparación del Ratio de Sharpe por Algoritmo",
    y = "Ratio de Sharpe",
    x = "Método"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
--------------------------------------------------------------------------------
#MACHINE LEARNING: # Pesos óptimos de DEoptim
  # --- Extraer pesos (solución robusta) ---
  weights_de <- extractWeights(opt_de)

# En caso de que lo anterior falle, utiliza esta alternativa:
if(is.null(weights_de)){
  weights_de <- opt_de$weights
}

# Verifica que obtienes pesos válidos
print(weights_de)

# --- Retornos cartera DEoptim ---
retornos_cartera_de <- Return.portfolio(rentabilidades, weights = weights_de)

# Verifica los retornos
head(retornos_cartera_de)

# ---- Preparación datos para ML (bien estructurada) ----
# Creamos dataset para ML
datos_ml <- data.frame(
  retorno = as.numeric(retornos_cartera_de),
  coredata(rentabilidades) # predictores: retornos individuales activos
)

# Elimina cualquier NA
datos_ml <- na.omit(datos_ml)

# Confirmar estructura
str(datos_ml)

# Definición variables predictoras y objetivo
X <- datos_ml[,-1]  # predictores
y <- datos_ml$retorno  # objetivo


# GLMNET (Elastic Net)
set.seed(123)
modelo_glmnet <- train(X, y, method = "glmnet", trControl = train_control)

# ---- Comparación de modelos ----
resultados <- resamples(list(
  Regresion_Lineal = modelo_lm,
  Random_Forest    = modelo_rf,
  SVM              = modelo_svm,
  GLMNET           = modelo_glmnet
))







# Tabla comparativa (RMSE, MAE)
summary(resultados)

# Gráficos para visualizar claramente los resultados
dotplot(resultados, metric = "RMSE")
dotplot(resultados, metric = "MAE")
r2_glmnet <- max(modelo_glmnet$results$Rsquared)

# Mejor modelo según RMSE
metricas_rmse <- resultados$values[,grep("RMSE", colnames(resultados$values))]
mejor_modelo <- names(which.min(colMeans(metricas_rmse)))




# Predicciones con GLMNET
predicciones_glmnet <- predict(modelo_glmnet, newdata = X)

df_comparacion <- data.frame(
  Real = y,
  Predicho = predicciones_glmnet
)

ggplot(df_comparacion, aes(x = Real, y = Predicho)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  labs(
    title = "Comparación entre Retornos DEoptim y Predicción GLMNET",
    x = "Retorno real (DEoptim)",
    y = "Retorno predicho (GLMNET)"
  ) +
  theme_minimal()

# Asegúrate de tener los datos de GLMNET y DEoptim ya generados:
# - retornos_de: retornos reales de la cartera DEoptim
# - predicciones_glmnet: retornos estimados por GLMNET

# Convertir a objeto xts para conservar estructura temporal
library(xts)

# Aseguramos que tengan las mismas fechas (por si hubo NA)
fechas <- index(rentabilidades)
fechas_validas <- fechas[1:nrow(datos_ml)]

# Crear series xts
retornos_real_xts <- xts(retornos_cartera_de[1:length(fechas_validas)], order.by = fechas_validas)
retornos_glmnet_xts <- xts(predicciones_glmnet, order.by = fechas_validas)

# Combinar ambas series
comparacion_rentabilidad <- merge(
  DEoptim = retornos_real_xts,
  GLMNET = retornos_glmnet_xts
)

# Gráfico comparativo
chart.TimeSeries(
  comparacion_rentabilidad,
  legend.loc = "bottomleft",
  colorset = c("blue", "darkred"),
  main = "Comparativa de Rentabilidad: DEoptim vs. GLMNET",
  ylab = "Retorno diario",
  auto.grid = TRUE
)

# --- Pesos DEoptim (ya extraídos previamente) ---
weights_de <- extractWeights(opt_de)
if (is.null(weights_de)) {
  weights_de <- opt_de$weights
}

# --- Pesos GLMNET: coeficientes del mejor modelo ---
# Extraemos el modelo final ajustado
modelo_final_glmnet <- modelo_glmnet$finalModel

# Extraer el lambda óptimo
best_lambda <- modelo_glmnet$bestTune$lambda

# Coeficientes del modelo para ese lambda
coef_glmnet <- coef(modelo_final_glmnet, s = best_lambda)

# Convertir a vector simple, quitar intercepto (el primero)
pesos_glmnet <- as.numeric(coef_glmnet)[-1]
nombres_glmnet <- rownames(coef_glmnet)[-1]

# Creamos data frame comparativo
df_pesos_comparados <- data.frame(
  Activo = nombres_glmnet,
  Peso_DEoptim = as.numeric(weights_de[nombres_glmnet]),
  Peso_GLMNET = pesos_glmnet
)

# Reemplazar NAs (si GLMNET puso 0s en algunos activos no seleccionados)
df_pesos_comparados[is.na(df_pesos_comparados)] <- 0

# Convertir a formato largo para ggplot2
library(reshape2)
df_largo <- melt(df_pesos_comparados, id.vars = "Activo",
                 variable.name = "Metodo", value.name = "Peso")

# Gráfico comparativo
library(ggplot2)

ggplot(df_largo, aes(x = reorder(Activo, -Peso), y = Peso, fill = Metodo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparación de Pesos: DEoptim vs. GLMNET",
    x = "Activo",
    y = "Peso / Coeficiente"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

