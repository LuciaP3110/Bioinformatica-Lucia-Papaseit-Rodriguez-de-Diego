# LUCÍA PAPASEIT RODRÍGUEZ DE DIEGO_TRABAJO2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)
library(ggplot2)
datos <- read.csv("datos_biomed.csv")

# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)
head(datos)
summary(datos)
dim(datos)
str(datos)

num_variables <- ncol(datos); num_variables
niveles_trat <- sort(unique(datos$Tratamiento))
num_tratamientos <- length(niveles_trat); num_tratamientos

# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)
ggplot(datos, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) +
  geom_boxplot() +
  labs(title = "Glucosa por Tratamiento")

ggplot(datos, aes(x = Tratamiento, y = Presion, fill = Tratamiento)) +
  geom_boxplot() +
  labs(title = "Presión por Tratamiento")

ggplot(datos, aes(x = Tratamiento, y = Colesterol, fill = Tratamiento)) +
  geom_boxplot() +
  labs(title = "Colesterol por Tratamiento")

# 4. Realiza un violin plot (investiga qué es). (1 pt)
ggplot(datos, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot de Glucosa por Tratamiento",
       x = "Tratamiento", y = "Glucosa")


# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
plot(datos$Glucosa, datos$Presion,
     xlab = "Glucosa", ylab = "Presión",
     main = "Glucosa vs Presión por Tratamiento", type = "n")

colores <- c("black", "blue", "red")
pchs    <- c(19, 17, 15)

for (i in seq_along(niveles_trat)) {
  sub <- datos[datos$Tratamiento == niveles_trat[i], ]
  points(sub$Glucosa, sub$Presion, col = colores[i], pch = pchs[i])
}

legend("bottomright", legend = niveles_trat, col = colores, pch = pchs,
       title = "Tratamiento", bty = "n")

# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
ggplot(datos, aes(x = Presion, y = Colesterol, color = Tratamiento)) +
  geom_point() +
  facet_grid(. ~ Tratamiento) +
  labs(title = "Colesterol vs Presión por Tratamiento")

# 7. Realiza un histogramas para cada variable. (0.5 pts)
hist(datos$Glucosa,    main = "Histograma de Glucosa",    xlab = "Glucosa")
hist(datos$Presion,    main = "Histograma de Presión",    xlab = "Presión")
hist(datos$Colesterol, main = "Histograma de Colesterol", xlab = "Colesterol")


# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)
datos$Tratamiento <- factor(datos$Tratamiento, levels = niveles_trat)
str(datos$Tratamiento)

# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
aggregate(Glucosa ~ Tratamiento, data = datos, FUN = mean)
aggregate(Glucosa ~ Tratamiento, data = datos, FUN = sd)


# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)
placebo  <- datos[datos$Tratamiento == "Placebo", ]
farmacoA <- datos[datos$Tratamiento == "FarmacoA", ]
farmacoB <- datos[datos$Tratamiento == "FarmacoB", ]


# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)
shapiro.test(placebo$Glucosa)
shapiro.test(farmacoA$Glucosa)
shapiro.test(farmacoB$Glucosa)

if (shapiro.test(placebo$Glucosa)$p.value > 0.05 &&
    shapiro.test(farmacoA$Glucosa)$p.value > 0.05 &&
    shapiro.test(farmacoB$Glucosa)$p.value > 0.05) {
  
  summary(aov(Glucosa ~ Tratamiento, data = datos))
  
} else {
  
  kruskal.test(Glucosa ~ Tratamiento, data = datos)
}


# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)
summary(aov(Glucosa ~ Tratamiento, data = datos))



