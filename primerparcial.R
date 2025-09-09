library(dplyr)
data <- read.csv("gdpindustrias.csv")


construction <- data %>%
  filter(Industry == "Construction") %>%
  select(Quarter, GDP = GDP..millions.of.current.dollars.)

information <- data %>%
  filter(Industry == "Information") %>%
  select(Quarter, GDP = GDP..millions.of.current.dollars.)

comparison <- merge(construction, information, by = "Quarter", suffixes = c("_Construction", "_Information"))
print(comparison)

comparison <- comparison %>%
  mutate(
    diferencia = GDP_Construction - GDP_Information,
    proporcion = GDP_Construction / GDP_Information
  )

construction_selected <- comparison$GDP_Construction
information_selected <- comparison$GDP_Information

t_test_selected <- t.test(construction_selected, information_selected, var.equal = FALSE)

print(t_test_selected)

construction_all <- construction$GDP
information_all <- information$GDP

# prueba de medias global
print(var.test(construction_all, information_all))

print(t.test(construction_all, information_all, var.equal = FALSE))




library(ggplot2)

data$Date.Quarter.Ended <- as.Date(data$Date.Quarter.Ended, format = "%m/%d/%Y")
data$Year <- as.numeric(substr(data$Date.Quarter.Ended, 1, 4))
print(data$Year)
top_industries <- data %>%
  group_by(Industry) %>%
  summarise(mean_GDP = mean(GDP..millions.of.current.dollars., na.rm = TRUE)) %>%
  arrange(desc(mean_GDP)) %>%
  slice(1:5) %>%
  pull(Industry)

data_top <- data %>%
  filter(Industry %in% top_industries)

ggplot(data_top, aes(x = factor(Year), y = GDP..millions.of.current.dollars., fill = Industry)) +
  geom_boxplot() +
  labs(
    title = "Evolución del PIB de las 5 industrias con mayor PIB",
    x = "Año",
    y = "PIB (millones de USD)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



# 2.b
information_until_2016 <- data %>%
  filter(Industry == "Information" & Year <= 2016) %>%

print(information_until_2016)



# 2.c
split_da <- information_until_2016 %>%
  mutate(
    Periodo = case_when(
      Year >= 2005 & Year <= 2010 ~ "2005-2010",
      Year >= 2011 & Year <= 2016 ~ "2011-2016",
      TRUE ~ NA_character_
    )
  )

# Ver resultado
print(split_da)




# 2.d 
p1 <- construction_until_2016 %>%
  filter(Periodo == "2005-2010") %>%
  pull(GDP..millions.of.current.dollars.)

p2 <- construction_until_2016 %>%
  filter(Periodo == "2011-2016") %>%
  pull(GDP..millions.of.current.dollars.)

# Prueba t de una cola (media primer periodo > media segundo periodo)
t_test_result <- t.test(p1, p2, alternative = "greater", var.equal = FALSE)
print(t_test_result)

#2.e
library(dplyr)

# Filtrar Construction hasta 2016
construction_until_2016 <- data %>%
  filter(Industry == "Construction" & Year <= 2016)

# Crear columna de periodo
construction_until_2016 <- construction_until_2016 %>%
  mutate(
    Periodo = case_when(
      Year >= 2005 & Year <= 2010 ~ "2005-2010",
      Year >= 2011 & Year <= 2016 ~ "2011-2016",
      TRUE ~ NA_character_
    )
  )

# Extraer vectores por periodo
p1 <- construction_until_2016 %>%
  filter(Periodo == "2005-2010") %>%
  pull(GDP..millions.of.current.dollars.)

p2 <- construction_until_2016 %>%
  filter(Periodo == "2011-2016") %>%
  pull(GDP..millions.of.current.dollars.)

# Prueba t de una cola (media primer periodo > media segundo periodo)
t_test_result <- t.test(p1, p2, alternative = "greater", var.equal = FALSE)

print(t_test_result)






# 3.a
library(dplyr)

subset_data <- data %>%
  filter(Industry %in% c("Construction", "Information") & Year <= 2016)

cor_data <- merge(construction, information, by = "Quarter")

correlation <- cor(cor_data$GDP_Construction, cor_data$GDP_Information, use = "complete.obs")
cat("Correlación  entre Construction e Information:", correlation, "\n")


# 3.b
library(dplyr)

construction_data <- data %>%
  filter(Industry == "Construction" & Year <= 2016)


# Regresión lineal: PIB ~ Año
reg_model <- lm(GDP..millions.of.current.dollars. ~ Year, data = construction_data)

summary(reg_model)

# Predicciones y gráfico
library(ggplot2)

ggplot(construction_data, aes(x = Year, y = GDP..millions.of.current.dollars.)) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(
    title = "Regresión lineal del PIB - Industria Construction",
    x = "Año",
    y = "PIB (millones USD)"
  ) +
  theme_minimal()

