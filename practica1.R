install.packages("readr")
install.packages("dplyr")
install.packages("zoo")
install.packages("corrplot")
install.packages("tidyr")

library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)
library(zoo)

# 1 analisis comparativo entre empresas
apple <- read_csv("archive/Data/Stocks/aapl.us.txt")
microsoft <- read_csv("archive/Data/Stocks/msft.us.txt")
tesla <- read_csv("archive/Data/Stocks/tsla.us.txt")
xom <- read_csv("archive/Data/Stocks/xom.us.txt")
jnj <- read_csv("archive/Data/Stocks/jnj.us.txt")

print(apple)

aapl <- apple %>% select(Date,Close) 
msft <- microsoft %>% select(Date,Close) 
tsla <- tesla %>% select(Date,Close) 

stocks <- aapl %>%
  rename(AAPL = Close) %>%
  inner_join(msft %>% rename(MSFT = Close), by = "Date") %>%
  inner_join(tsla %>% rename(TSLA = Close), by = "Date")


cor_matrix <- cor(stocks[,-1], use = "complete.obs")
print(cor_matrix)

corrplot(cor_matrix,method = "color",type = "upper",tl.col = "black",
         tl.srt = 45, addCoef.col = "black")



# rendimiento relativo 
aapl <- apple %>% select(Date, Close) %>% rename(AAPL = Close)
msft <- microsoft %>% select(Date, Close) %>% rename(MSFT = Close)
tsl <- tesla %>% select(Date, Close) %>% rename(TSLA = Close)
xom <- xom %>% select(Date, Close) %>% rename(XOM = Close)
jnj <- jnj %>% select(Date, Close) %>% rename(JNJ = Close)

# Unir en un solo dataset por fecha
stocks <- aapl %>%
  inner_join(msft, by = "Date") %>%
  inner_join(tsl, by = "Date") %>%
  inner_join(xom, by = "Date") %>%
  inner_join(jnj, by = "Date")
print(stocks)
# Rendimiento logaritmicos
returns <- stocks %>% 
  mutate(across(-Date, ~ log(.x/lag(.x)))) %>%
  na.omit()


# Transformar los datos a formato largo para ggplot
returns_long <- returns %>%
  pivot_longer(cols = -Date, names_to = "Stock", values_to = "Return")

# Gráfico de líneas de retornos diarios
ggplot(returns_long, aes(x = Date, y = Return, color = Stock)) +
  geom_line(alpha = 0.7) +
  labs(title = "Retornos logarítmicos diarios de las acciones",
       x = "Fecha",
       y = "Retorno logarítmico") +
  theme_minimal() +
  theme(legend.title = element_blank())


cor_matrix <- cor(returns %>% select(-Date))
print(cor_matrix)
corrplot(cor_matrix,method = "color",type = "upper",tl.col = "black",
           tl.srt = 45, addCoef.col = "black")

# anomalias
anomalies <- returns %>%
  pivot_longer(cols = -Date, names_to = "Stock", values_to = "Return") %>%
  group_by(Stock) %>%
  mutate(mean_ret = mean(Return),
         sd_ret = sd(Return),
         z_score = (Return - mean_ret)/sd_ret,
         anomaly = abs(z_score) > 3) %>%
  filter(anomaly)

print(anomalies)
# grafico de anomalias
ggplot(returns_long, aes(x = Date, y = Return, color = Stock)) +
  geom_line(alpha = 0.6) +
  geom_point(data = anomalies, aes(x = Date, y = Return), color = "red", size = 2) +
  labs(title = "Retornos diarios con anomalías destacadas",
       x = "Fecha", y = "Retorno logarítmico") +
  theme_minimal()


# Fila con el retorno máximo
aapl %>% filter(Return == max(Return, na.rm = TRUE))

# # Fila con el retorno mínimo
aapl %>% filter(Return == min(Return, na.rm = TRUE))



print(apple)
# agregar variables
aapl <- select(apple,Date,Close,Volume) %>% 
  arrange(Date) %>%
  mutate(
    Return = log(Close/lag(Close)),
    CumReturn = cumprod(1 + coalesce(Return,0)) - 1,
    Vol30 = rollapply(Return,30,sd,fill = NA, align = "right"),
    MA20 = rollapply(Close, 20, mean, fill = NA, align = "right"),
    Anomaly = abs((Return - mean(Return, na.rm=TRUE)) / sd(Return, na.rm = TRUE)) > 3
  )

aapl$DailyVol <- abs(aapl$Return)

aapl$Volatility_Level <- cut(aapl$DailyVol,
                             breaks = c(-Inf, 0.01, 0.03, Inf),
                             labels = c("Baja", "Media", "Alta"),
                             right = TRUE)

# 3. Asegurarse de que R la interprete como un factor ordenado
aapl$Volatility_Level <- factor(aapl$Volatility_Level, ordered = TRUE, levels = c("Baja", "Media", "Alta"))

# Ver la estructura del dataframe con la nueva variable
str(aapl$Volatility_Level)
# print(aapl)
print(aapl)
cat("Análisis de la variable 'Anomaly':\n")
table(aapl$Anomaly)
prop.table(table(aapl$Anomaly)) * 100
cat("\n\n")

# Análisis de la nueva variable ordinal 'Volatility_Level'
cat("Análisis de la variable 'Volatility_Level':\n")
table(aapl$Volatility_Level)
prop.table(table(aapl$Volatility_Level)) * 100

nrow(aapl)

library(openxlsx)
View(aapl)
write.xlsx(aapl, "aapl_datos.xlsx")
