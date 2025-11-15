# CandelaGarcíaMarina_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)

#Para empezar, comprobamos la ruta de nuestro archivo datos_biomed.csv para que se muestre en el directorio donde trabaja R.
getwd()

# Cargamos la librería readr, que proporciona funciones eficientes para leer archivos CSV y trabajar con datos de forma moderna en R.
# Mediante datos_biomed <- read_csv("datos_biomed.csv"): se importa el archivo CSV y se guarda en el objeto datos.
# La función read_csv(): detecta automáticamente los tipos de datos y carga la tabla.
library(readr)
datos_biomed <- read_csv("datos_biomed.csv")


# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)

# Exploramos las siguientes funciones: 
head(datos_biomed)# Muestra las primeras 6 filas del dataframe, para ver cómo se ven las variables y los valores iniciales.
summary(datos_biomed)# Resumen estadístico de cada variable.
dim(datos_biomed)# Muestra filas y columnas, filas = tratamientos, columnas = variables.
str(datos_biomed)# Muestra la estructura interna, tipo de columnas y ejemplo de datos. 

# Extraemos el número de variables (columnas)
num_variables <- ncol(datos_biomed) # ncol: calcula la cantidad de columnas del objeto datos_biomed.
# Se introducen dentro de la variable num_variables, mediante "<-".
cat("Número de variables:", num_variables, "\n") #Imprime un mensaje en pantalla uniendo texto y el número calculado.

# El archivo tiene 5 variables medidas.

# Extraemos el número de tratamientos (filas)
num_tratamientos <- nrow(datos_biomed)# nrow:cuenta cuántas filas tiene el data frame.
# Se introducen dentro de la variable num_tratamientos, mediante "<-".
cat("Número de tratamientos:", num_tratamientos, "\n") # Muestra ese resultado en pantalla.

# El archivo tiene 100 tratamientos.

# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)

# Cargamos la librería ggplot2 para gráficas avanzadas.
install.packages("ggplot2")
library(ggplot2)

# Convertimos datos a formato "largo" para ggplot2 con tidyr.
# Dado que nuestro dataset original tiene este formato ancho, pero ggplot necesita un formato largo para trabajar con varias variables.
# tidyr: es una librería que permite reorganizar datos, necesario para que ggplot2 pueda manejar varias variables en una sola gráfica.
install.packages("tidyr")
library(tidyr)

# Seleccionamos las columnas numéricas (Glucosa, Presion, Colesterol) y los tratamientos.
# Vamos a generar una versión transformada llamada datos_largos del dataframe inicial, ya que en boxplot trabajamos con varias variables.
datos_largos <- datos_biomed %>% 
  pivot_longer(cols = c(Glucosa, Presion, Colesterol),
               names_to = "Variable",
               values_to = "Valor")
# pivot_longer(): es la función que pertenece a la librería tidyr, que convierte un dataset de formato ancho a formato largo.
# cols = c(Glucosa, Presion, Colesterol): indica qué columnas queremos convertir al formato largo.
# names_to = "Variable": crea una columna nueva llamada Variable.
# values_to = "Valor": crea una columna llamada Valor con los valores numéricos correspondientes.

# Creamos los boxplot: Valor vs Tto, separados por Variables.
ggplot(datos_largos, aes(x = Tratamiento, y = Valor, fill = Tratamiento)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y") +  
  theme_minimal() +                           
  labs(title = "Boxplots por tratamiento para cada variable",
       x = "Tratamiento",
       y = "Valor")

# ggplot(): inicia la construcción del gráfico.
# datos_largos: es el dataframe en formato largo que hemos creado para usar en ggplot.
# aes(): mapeo estético, define qué se coloca en cada eje:
# x = Tratamiento: Cada tratamiento será un grupo diferente.
# y = Valor: Los valores numéricos (glucosa, presión, colesterol).
# fill = Tratamiento: Cada tratamiento se colorea distinto.
# geom_boxplot(): Añade los boxplots a la figura.
# facet_wrap(): crea un panel separado para cada variable.
# scales = "free_y": permite que cada variable use su propia escala del eje Y.
# theme_minimal(): aplica un diseño limpio, sin líneas de fondo excesivas.
# labs: añade los textos al título del gráfico y a las etiquetas de los ejes.

# Interpretación del boxplot:
# El Fármaco A se asocia a colesterol más alto.
# El Fármaco B a una mayor presión arterial.
# La glucosa apenas muestra diferencias claras entre tratamientos.
# El placebo no parece producir cambios significativos
# Se confirma que las variaciones observadas en Fármaco A y B se deben al efecto de los tratamientos y no a factores externos.

# 4. Realiza un violin plot (investiga qué es). (1 pt)

# Un violin plot es un gráfico que muestra la distribución de una variable numérica para distintos grupos.
# Combina un boxplot con una curva de densidad.Permite ver, no solo la mediana y los cuartiles, sino también la forma completa de la distribución.
ggplot(datos_largos, aes(x = Tratamiento, y = Valor, fill = Tratamiento)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.15, color = "black", fill = "white") +
  facet_wrap(~Variable, scales = "free_y") + 
  labs(title = "Violin plots por tratamiento para cada variable",
       x = "Tratamiento",
       y = "Valor") +
  theme_minimal()

# En el violin plot también trabajamos con varias variables, por lo que utilizamos datos_largos.
# geom_violin(): crea el gráfico de violín, muestra la forma completa de la distribución.
# trim = FALSE. Trim es un argumento que controla el recorte de la curva violin. En este caso, al ser =FALSE, indica que no se recorten las colas del violín para mostrar toda la distribución.
# geom_boxplot(): añade un boxplot encima del violín para poder ver la mediana, los cuartiles y los valores extremos.
# - Utiliza diferentes parámetros como: 
#   width = 0.15, establece las medidas para que el boxplot sea más estrecho y no tape al violin.
#   color = "black", hace que los bordes del boxplot sean negros.
#   fill = "white", hace que el interior del boxplot sea blanco para que contraste sobre el violin.

# Interpretación del violinplot: 
# En general, ninguno de los fármacos parece reducir glucosa, presión arterial o colesterol.
# El placebo se sitúa como el grupo con valores más bajos y distribuciones más estables.


# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
plot(datos_biomed$Glucosa, datos_biomed$Presion,
     xlab = "Glucosa",
     ylab = "Presión",
     main = "Dispersión: Glucosa vs Presión",
     pch = 19,                
     col = c("FarmacoA" = "green", "FarmacoB" = "orange", "Placebo" = "purple")) 

legend("bottomright",         
       legend = "Individuos", 
       pch = 19,              
       col = c("FarmacoA" = "green", "FarmacoB" = "orange", "Placebo" = "purple"))

# plot(): crea el gráfico de dispersión. Coloca la Glucosa en el eje X (xlab), la Presión en el eje Y (ylab) y define el título usando "main".
# $: es el operador para acceder a una columna. Permite acceder tanto a la columna "Glucosa" como a la columna "Presión" dentro del mismo dataframe (datos_biomed). Este operador se usa en funciones que necesitan vectores, como plot(). En este caso se crearán dos vectores, uno con todos los valores de la columna "Glucosa" y otro con los de la columna "Presión".
# En este caso podemos usar datos_biomed para un mejor funcionamiento, ya que sólo comparamos dos variables.
# pch = 19: define el tipo de punto (círculo sólido).
# col =c(): definimos qué color usar para cada categoría.
# legend("bottomright", ...):añade la leyenda en la esquina inferior derecha, usando el mismo símbolo y color que los puntos del gráfico.
# legend= "Individuos": en un gráfico de dispersión cada punto representa a un individuo del estudio.

# Interpretación del gráfico de dispersión: 
# Muestra relación entre glucosa y presión arterial diferenciada por tto.
# Permite comparar la variabilidad entre Fármaco A, B y placebo
# La glucosa y la presión arterial no muestran una correlación aparente. 
# Los individuos de los distintos tratamientos se distribuyen de forma similar, lo que sugiere que los fármacos no alteran la relación entre ambas variables.

# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)

# Un facet grid divide el gráfico en varios paneles según una variable categórica, permite comparar la misma relación entre grupos. En este caso facilita observar cómo cambia la relación entre Colesterol y presión para cada tratamiento.
# A diferencia del boxplot, donde se resumen medianas y cuartiles, o del violinplot, que muestra una distribución completa; el facet grid también permite una comparación visual pero se centra en la relación entre variables.
ggplot(datos_biomed, aes(x = Presion, y = Colesterol)) +
  geom_point(aes(color = Tratamiento), size = 2) +
  facet_grid(~ Tratamiento) +
  labs(title = "Colesterol vs Presión por tratamiento",
       x = "Presión",
       y = "Colesterol") +
  theme_minimal()   

# Aquí también comparamos dos variables, usamos datos_biomed.
# geom_point(): dibuja los puntos del gráfico de dispersión, utiliza parámetros que permiten seleccionar tanto el color como el tamaño.
# A través de "color=Tratamiento", R da un color diferente a cada tratamiento.
# facet_grid(): crea un subgráfico (panel) para cada tratamiento.

# Interpretacuón facetgrid:
# No se observa una relación clara entre colesterol y presión arterial en ninguno de los tratamientos. 
# Sin embargo, sí hay diferencias en los niveles de colesterol: Fármaco A muestra valores más altos, Fármaco B intermedios y el placebo los más bajos. 
# La presión arterial permanece similar entre grupos, con pequeñas diferencias.

# 7. Realiza un histogramas para cada variable. (0.5 pts)

ggplot(datos_largos, aes(x = Valor, fill = Variable)) +
  geom_histogram(bins = 30, alpha = 0.8, color = "white") +
  facet_wrap(~ Variable, scales = "free") +
  labs(
    title = "Distribución de valores - Histogramas por variable",
    x = "Valor medido",
    y = "Frecuencia"
  ) +
  theme_minimal()

# geom_histogram(): añade los histogramas.
# bins = 30: divide los datos en 30 grupos o barras.
# alpha = 0.8: da transparencia ligera para ver mejor la forma.
# color = "white": bordes blancos en las barras para que sean más legibles.
# facet_wrap(): divide el gráfico en un panel independiente por cada variable.
# scales = "free" permite que cada panel tenga sus propias escalas en X e Y, porque cada variable usa rangos distintos.

# Interpretación histograma:
# Muestran que Glucosa tiene la distribución más simétrica y concentrada.
# La Presión y Colesterol presentan mayor dispersión pero formas aproximadamente normales. 
# No se observan valores atípicos extremos, por lo que los datos parecen adecuados para análisis paramétricos

# 8. Crea un factor a partir del tratamiento. Investiga factor(). (1 pt)
datos_biomed <- read_csv("datos_biomed.csv")
datos_biomed$Tratamiento <- factor(datos_biomed$Tratamiento)
levels(datos_biomed$Tratamiento)

# factor(): convierte una variable en un tipo de dato categórico con niveles. En este caso, se transforma la columna "Tratamiento" en un factor para que R la reconozca como una variable con categorías (“FarmacoA”, “FarmacoB”, “Placebo”).Este cambio permite agrupar, clasificar y generar gráficos por cada nivel del tratamiento.
# Para ello, se accede a la columna Tratamiento del dataframe mediante "datos_biomed$Tratamiento".
# A través de el operador de asignación "<-", toma ese factor y lo guarda en la columna "Tratamiento", de modo que la variable queda convertida en un factor.
# levels(): muestra los niveles de un factor. En este caso, devuelve las categorías presentes en la columna "Tratamiento", que ahora es un factor.


# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
media_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos_biomed, FUN = mean)
sd_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos_biomed, FUN = sd)

# aggregate(): permite aplicar una función a una variable, agrupando por una categoría.
# En este caso, Glucosa es la variable numérica y Tratamiento es la variable que define los grupos.
# Glucosa ~ Tratamiento: calcula la media de Glucosa para cada categoría de Tratamiento.
# FUN = mean: calcula la media de glucosa por tratamiento.
# FUN = sd: calcula la desviación estándar de glucosa por tratamiento.
# Tanto la media como la desviación se guardan en sus variables correspondientes mediante el operador de asignación "<-".

#Interpretación:
# Podemos observar que las medias de las glucosas son similares entre los tres grupos.
# Sin embargo, la desviación estandar muestran una variabilidad moderada.
# Los valores de Placebo son más dispersos, lo que indica mayor variabilidad en los pacientes sin tratamiento.

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)
placebo   <- datos_biomed[datos_biomed$Tratamiento == "Placebo", ]
farmacoA  <- datos_biomed[datos_biomed$Tratamiento == "FarmacoA", ]
farmacoB  <- datos_biomed[datos_biomed$Tratamiento == "FarmacoB", ]

# datos_biomed$Tratamiento == "variable": compara cada fila de la columna "Tratamiento" con el texto indicado ("Placebo", "FarmacoA" o "FarmacoB")
# Mediante el operador "==",se genera automáticamente un vector lógico con TRUE en las filas que cumplen la condición y FALSE en las que no.
# datos_biomed[ ... , ]: utiliza ese vector lógico para seleccionar únicamente las filas donde el valor es TRUE.
# A través de el operador de asignación "<-",las filas seleccionadas se guardan en una nueva variable (placebo, farmacoA o farmacoB) según el tratamiento elegido.

# Para interpretarlos verificamos primeras filas de cada grupo:
head(placebo)
head(farmacoA)
head(farmacoB)

# El placebo, muestran variabilidad normal, sin valores extremos y niveles moderados y estables.
# El Fármaco A, presenta algunos valores altos de colesterol, glucosa y colesterol má variables y con mayor dispersión que el placebo.
# El Fármaco B, tiene valores intermedios.

# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)

shapiro.test(placebo$Glucosa)
shapiro.test(farmacoA$Glucosa)
shapiro.test(farmacoB$Glucosa)

# shapiro.test(): aplica el test de Shapiro–Wilk para evaluar si los datos siguen una distribución normal. 
# Se aplica por separado a cada tratamiento utilizando las columnas de "Glucosa" extraídas mediante "$".
# Para elegir la prueba adecuada para comparar medias, nos fijamos en los datos de p value:
# - Si hay normalidad se realiza ANOVA.
# - Si no hay normalidad se realiza Kruskal–Wallis.

# En el caso de la Glucosa, todos los grupos presentan p > 0.05, los tres grupos siguen una distribución normal. 
# Como son normales, para comparar las medias se utilizará ANOVA.
anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos_biomed)
summary(anova_glucosa)

# Evaluamos la normalidad del "Colesterol" con el mismo código que para la "Glucosa".
shapiro.test(placebo$Colesterol)
shapiro.test(farmacoA$Colesterol)
shapiro.test(farmacoB$Colesterol)

# En el caso del colesterol, las distribuciones de los grupos también siguen una distribución normal. 
# Por lo que la prueba adecuada de evaluación también sería ANOVA.
anova_colesterol <- aov(Colesterol ~ Tratamiento, data = datos_biomed)
summary(anova_colesterol)

# Evaluamos la normalidad del "Presión" con el mismo código que para la "Glucosa".
shapiro.test(placebo$Presion)
shapiro.test(farmacoA$Presion)
shapiro.test(farmacoB$Presion)

# Las distribuciones de la Presión son normales
# Se recurre a ANOVA.
anova_presion <- aov(Presion ~ Tratamiento, data = datos_biomed)
summary(anova_presion)

# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)
anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos_biomed)
summary(anova_glucosa)

# ANOVA muestra las diferencias significativas en la media de glucosa entre tratamientos.
# aov(): ejecuta ANOVA de un factor. 
# Analiza la variable Glucosa en función del Tratamiento ("Glucosa ~ Tratamiento"), dentro de los datos del dataframe (data = datos).Se guardan mediante "<-", en la variable "anova_glucosa".
# summary(anova_glucosa): muestra los resultados completos del ANOVA:
#- Df: grados libertad (2 para Tto, 97 para Residuals).
#- Sum Sq y Mean Sq: suma de cuadrados y media de cuadrados para cada fuente de variación.
#- F value = 2.358: relación variabilidad entre ttos respecto a variabilidad residual.

# En cuanto a los resultados, las diferencias significativas se observan a través de Pr(>F): p value
# Podemos observar que p = 0.1, es decir:
# - No hay diferencias significativas entre los tratamientos en los niveles de glucosa (es mayor que 0.05.)


