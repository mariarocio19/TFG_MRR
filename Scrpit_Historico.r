
library(tidyquant)   # Para descargar datos de Yahoo Finance y manipularlos de forma eficiente
library(dplyr)       # Para manipulación de datos
library(ggplot2)     # Para graficar

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

#Pequeño ejemplo ilustrativo: 
ejemplo_df <- datos_acciones %>%
  filter(symbol %in% c("AAPL", "TSLA")) %>%
  arrange(symbol, date) %>%
  head(6)  # Mostramos solo las primeras 6 filas

# Visualizamos el data frame reducido
ejemplo_df

# Si deseas verificar rápidamente la estructura de los datos:
head(datos_acciones)

ggplot(datos_acciones, aes(x = date, y = adjusted, color = symbol)) +
  geom_line() +
  labs(title = "Evolución del precio ajustado de las acciones",
       x = "Fecha",
       y = "Precio Ajustado",
       color = "Acción") +
  theme_minimal()

#Por separado: 
ggplot(datos_acciones, aes(x = date, y = adjusted, color = symbol)) +
  geom_line() +
  facet_wrap(~ symbol, scales = "free_y") +
  labs(title = "Evolución del precio ajustado de las acciones (faceteado)",
       x = "Fecha",
       y = "Precio Ajustado") +
  theme_minimal() +
  theme(legend.position = "none")  # Para ocultar la leyenda si deseas

#Gráfico normalizado:
datos_normalizados <- datos_acciones %>%
  group_by(symbol) %>%
  mutate(price_index = adjusted / first(adjusted))

ggplot(datos_normalizados, aes(x = date, y = price_index, color = symbol)) +
  geom_line() +
  labs(title = "Evolución normalizada de las acciones (base = 1 en la primera fecha)",
       x = "Fecha",
       y = "Índice de Precio (Normalizado)") +
  theme_minimal()


# Calculamos la rentabilidad entre la primera y la última observación de cada activo
rentabilidades <- datos_acciones %>%
  group_by(symbol) %>%
  # "rend" será (precio último / precio primero) - 1
  summarize(rend_final = (last(adjusted) / first(adjusted)) - 1) %>%
  arrange(desc(rend_final))

# Extraemos los 3 tickers con mayor crecimiento
top_3 <- rentabilidades %>%
  head(3) %>%
  pull(symbol)

# Imprimimos los 3 mejores
top_3

# Filtramos solo para las 3 acciones con mejor evolución
datos_top_3 <- datos_acciones %>%
  filter(symbol %in% top_3)

ggplot(datos_top_3, aes(x = date, y = adjusted, color = symbol)) +
  geom_line() +
  labs(title = "Evolución del Precio Ajustado - Top 3 Acciones",
       x = "Fecha",
       y = "Precio Ajustado") +
  theme_minimal()

# Creamos la columna con el precio normalizado
datos_top_3_norm <- datos_top_3 %>%
  group_by(symbol) %>%
  mutate(price_index = adjusted / first(adjusted))

# Graficamos la evolución normalizada
ggplot(datos_top_3_norm, aes(x = date, y = price_index, color = symbol)) +
  geom_line() +
  labs(title = "Evolución Normalizada (Base = 1 en la primera fecha)",
       x = "Fecha",
       y = "Índice de Precio Normalizado") +
  theme_minimal()


# Cálculo de rentabilidad total y geométrica anualizada
rentabilidades <- datos_acciones %>%
  group_by(symbol) %>%
  summarize(
    rend_final = (last(adjusted) / first(adjusted)) - 1,
    period_years = as.numeric(difftime(max(date), min(date), units = "days")) / 365,
    geom_return = (last(adjusted) / first(adjusted))^(1 / period_years) - 1
  ) %>%
  arrange(desc(rend_final))

# Extraemos los 3 tickers con mayor rentabilidad final
top_3 <- rentabilidades %>%
  head(3) %>%
  pull(symbol)

print(rentabilidades %>% filter(symbol %in% top_3))

# Filtramos solo para las 3 acciones con mejor evolución
datos_top_3 <- datos_acciones %>%
  filter(symbol %in% top_3)

# Gráfico evolución de precio ajustado
ggplot(datos_top_3, aes(x = date, y = adjusted, color = symbol)) +
  geom_line() +
  labs(title = "Evolución del Precio Ajustado - Top 3 Acciones",
       x = "Fecha",
       y = "Precio Ajustado") +
  theme_minimal()

# Creamos la columna con el precio normalizado
datos_top_3_norm <- datos_top_3 %>%
  group_by(symbol) %>%
  mutate(price_index = adjusted / first(adjusted))

# Gráfico evolución normalizada
ggplot(datos_top_3_norm, aes(x = date, y = price_index, color = symbol)) +
  geom_line() +
  labs(title = "Evolución Normalizada (Base = 1 en la primera fecha)",
       x = "Fecha",
       y = "Índice de Precio Normalizado") +
  theme_minimal()