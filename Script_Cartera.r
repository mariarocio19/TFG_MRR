library(quantmod)
library(tidyquant)
library(dplyr)
library(BatchGetSymbols)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(DEoptim)
library(ROI)
library(ROI.plugin.symphony)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)


#Creamos nuestra cartera de acciones gracias a los datos extraídos de yahoo finance:

acciones <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA", "NVDA", "META", "BABA", "NFLX", "JPM",
             "V", "MA", "PYPL", "DIS", "KO", "PEP", "XOM", "CVX", "PFE", "MRNA",
             "NIO", "TCEHY", "BIDU", "BP", "RIO", "SAN", "HSBC", "CSCO", "INTC", "ORCL",
             "ABNB", "UBER", "SHOP", "SQ", "TSM", "ZM", "SNAP", "DDOG", "DOCU", "PLTR",
             "VALE", "BHP", "BCE", "UL", "PG", "T", "TM", "HMC", "NSANY")

#Descargamos los históricos:

# Obtener datos de las 50 acciones: coger datos de diez años en dato diario. 
#31/12/2014 - 31/12/2024, 252 días de cotizaciones. 
stock_data <- tq_get(acciones, from = "2024-01-01", to = Sys.Date(), get = "stock.prices")

# Verificar datos
head(stock_data)

--------------------------------------------------------------------------------
#obtener la capitalización de mercado y sector de cada empresa
# Obtener datos fundamentales de las empresas
fundamental_data <- BatchGetSymbols(tickers = acciones, first.date = Sys.Date() - 365,
                                    last.date = Sys.Date(), do.cache = FALSE)$df.control

# Ver datos fundamentales
head(fundamental_data)

--------------------------------------------------------------------------------

# Crear un mapeo de países a regiones
  regiones <- c(
    "América",       # AAPL
    "América",       # MSFT
    "América",       # GOOGL
    "América",       # AMZN
    "América",       # TSLA
    "América",       # NVDA
    "América",       # META
    "Asia-Pacífico", # BABA
    "América",       # NFLX
    "América",       # JPM
    "América",       # V
    "América",       # MA
    "América",       # PYPL
    "América",       # DIS
    "América",       # KO
    "América",       # PEP
    "América",       # XOM
    "América",       # CVX
    "América",       # PFE
    "América",       # MRNA
    "Asia-Pacífico", # NIO
    "Asia-Pacífico", # TCEHY
    "Asia-Pacífico", # BIDU
    "Europa",        # BP
    "Asia-Pacífico", # RIO (Australia)
    "Europa",        # SAN (España)
    "Europa",        # HSBC (Reino Unido)
    "América",       # CSCO
    "América",       # INTC
    "América",       # ORCL
    "América",       # ABNB
    "América",       # UBER
    "América",       # SHOP (Canadá)
    "América",       # SQ
    "Asia-Pacífico", # TSM (Taiwán)
    "América",       # ZM
    "América",       # SNAP
    "América",       # DDOG
    "América",       # DOCU
    "América",       # PLTR
    "América",       # VALE (Brasil)
    "Asia-Pacífico", # BHP (Australia)
    "América",       # BCE (Canadá)
    "Europa",        # RDS.A (Países Bajos / UK)
    "Europa",        # UL (Unilever)
    "América",       # PG
    "América",       # T
    "Asia-Pacífico", # TM (Toyota, Japón)
    "Asia-Pacífico", # HMC (Honda, Japón)
    "Asia-Pacífico"  # NSANY (Nissan, Japón)
  )


# Crear dataframe con la región asignada
df_regiones <- data.frame(Accion = acciones, Region = regiones)

# Verificar clasificación por región
head(df_regiones)


-------------------------------------------------------------------------------
  sectores <- c(
    "Tecnología y Comunicación", # AAPL
    "Tecnología y Comunicación", # MSFT
    "Tecnología y Comunicación", # GOOGL
    "Consumo y Entretenimiento", # AMZN
    "Consumo y Entretenimiento", # TSLA
    "Tecnología y Comunicación", # NVDA
    "Tecnología y Comunicación", # META
    "Consumo y Entretenimiento", # BABA
    "Consumo y Entretenimiento", # NFLX
    "Finanzas y Servicios",       # JPM
    "Finanzas y Servicios",       # V
    "Finanzas y Servicios",       # MA
    "Finanzas y Servicios",       # PYPL
    "Consumo y Entretenimiento", # DIS
    "Consumo y Entretenimiento", # KO
    "Consumo y Entretenimiento", # PEP
    "Industria y Energía",       # XOM
    "Industria y Energía",       # CVX
    "Industria y Energía",       # PFE
    "Industria y Energía",       # MRNA
    "Industria y Energía",       # NIO
    "Tecnología y Comunicación", # TCEHY
    "Tecnología y Comunicación", # BIDU
    "Industria y Energía",       # BP
    "Industria y Energía",       # RIO
    "Finanzas y Servicios",       # SAN
    "Finanzas y Servicios",       # HSBC
    "Tecnología y Comunicación", # CSCO
    "Tecnología y Comunicación", # INTC
    "Tecnología y Comunicación", # ORCL
    "Tecnología y Comunicación", # ABNB
    "Consumo y Entretenimiento", # UBER
    "Tecnología y Comunicación", # SHOP
    "Finanzas y Servicios",       # SQ
    "Tecnología y Comunicación", # TSM (Nueva Acción)
    "Tecnología y Comunicación", # ZM
    "Tecnología y Comunicación", # SNAP
    "Tecnología y Comunicación", # DDOG
    "Tecnología y Comunicación", # DOCU
    "Tecnología y Comunicación", # PLTR
    "Industria y Energía",       # VALE
    "Industria y Energía",       # BHP
    "Tecnología y Comunicación", # BCE
    "Industria y Energía",       # RDS.A
    "Consumo y Entretenimiento", # UL
    "Consumo y Entretenimiento", # PG
    "Tecnología y Comunicación", # T
    "Industria y Energía",       # TM
    "Industria y Energía",       # HMC
    "Industria y Energía"        # NSANY
  )


# Crear dataframe con sectores
df_sectores <- data.frame(Accion = acciones, Sector = sectores)

# Verificar clasificación por sector
head(df_sectores)

-------------------------------------------------------------------------------
#Clasificamos por capitalización búrsatil: 
# Asignar capitalización bursátil manualmente
cap_bursatil <- c("Large", "Large", "Large", "Large", "Large", "Large", "Large", "Mid", "Large", "Large",
                    "Large", "Large", "Mid", "Large", "Large", "Large", "Large", "Large", "Large", "Mid",
                    "Mid", "Mid", "Mid", "Mid", "Small", "Mid", "Mid", "Mid", "Mid", "Mid",
                    "Mid", "Small", "Mid", "Small", "Large", "Small", "Small", "Small", "Small", "Small",
                    "Small", "Mid", "Small", "Mid", "Mid", "Large", "Mid", "Mid", "Mid", "Small")

# Crear dataframe con capitalización bursátil
df_cap <- data.frame(Accion = acciones, Capitalizacion = cap_bursatil)

# Verificar clasificación por capitalización bursátil
head(df_cap)

-------------------------------------------------------------------------------
#Creamos nuestra cartera clasificada 
  
df_sectores <- data.frame(Accion = acciones, Sector = sectores) %>%
distinct(Accion, .keep_all = TRUE)  # Eliminar duplicados

df_cap <- data.frame(Accion = acciones, Capitalizacion = cap_bursatil) %>%
distinct(Accion, .keep_all = TRUE)  # Eliminar duplicados

df_final <- df_paises %>%
inner_join(df_sectores, by = "Accion", relationship = "many-to-many") %>%
inner_join(df_cap, by = "Accion", relationship = "many-to-many")
df_final <- df_regiones %>%
  inner_join(df_sectores, by = "Accion") %>%
  inner_join(df_cap, by = "Accion")

# Ver las primeras filas del dataframe final
head(df_final)

-------------------------------------------------------------------------------
#Visualizamos nuestra cartera:

ggplot(df_final, aes(x = Region, fill = Region)) +
geom_bar() +
labs(title = "Distribución de la cartera por Región", x = "Región", y = "Número de acciones")

ggplot(df_final, aes(x = Sector, fill = Sector)) +
geom_bar() +
coord_flip() +
labs(title = "Distribución de la cartera por sector", x = "Sector", y = "Número de acciones")

ggplot(df_final, aes(x = Capitalizacion, fill = Capitalizacion)) +
  geom_bar() +
  labs(title = "Distribución de la cartera por capitalización bursátil", x = "Categoría", y = "Número de empresas")

--------------------------------------------------------------------------------

  
#Comenzamos el análisis de optimización: 
  
# Obtener datos históricos correctamente
getSymbols(acciones, from = "2014-12-31", to = "2024-12-31", src = "yahoo", auto.assign = TRUE)
datos <- na.omit(merge(Cl(get(acciones[1])), Cl(get(acciones[2])), all = FALSE))
for (i in 3:length(acciones)) {
  datos <- merge(datos, Cl(get(acciones[i])), all = FALSE)
}
retornos <- Return.calculate(datos)

# Dividir datos en train (2014-2021) y test (2022-2024)
train <- retornos["2014-12-31/2021-12-31"]
test <- retornos["2022-01-01/2024-12-31"]

# Especificar cartera
port <- portfolio.spec(assets = colnames(train))
port <- add.constraint(port, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
port <- add.constraint(port, type = "long_only")

# Algoritmos de optimización
port_naive <- add.constraint(port, type = "box", min = 0.01, max = 0.05)
port_markowitz <- add.objective(port, type = "risk", name = "var", enabled = TRUE)
port_cvar <- add.objective(port, type = "risk", name = "CVaR", enabled = TRUE)


# Optimizar con PortfolioAnalytics
opt_naive <- optimize.portfolio(train, port_naive, optimize_method = "DEoptim")
opt_markowitz <- optimize.portfolio(train, port_markowitz, optimize_method = "ROI", trace = TRUE, solver = "quadprog")
opt_cvar <- optimize.portfolio(train, port_cvar, optimize_method = "ROI", trace = TRUE, solver = "glpk")



# Resultados
extractWeights(opt_naive)
extractWeights(opt_markowitz)
extractWeights(opt_cvar)
summary(opt_naive)
summary(opt_markowitz)
summary(opt_cvar)
