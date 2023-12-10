# Tarea 1. Representación de aproximaciones de distribuciones

## Parte 1
Representar en un gráfico para cada valor de probabilidad p=0,5, 0,1 y 0,05, la evolución de la aproximación de las distribuciones binomiales con n=3,10,25,50 a su correspondiente distribución normal (Nota: se debe hacer un gráfico para 0,5 con todas las n, otro para 0,1 con todas las n y otro para 0,05 con todas las n).

### Resolución
Debido a que debemos iterar para cada valor de probabilidad con su respectivo valor de n, crearemos una función que reciba como parámetro la probabilidad que se vaya a iterar sobre todos los valores de n definidos en el enunciado. Y realizaremos la sistematización del siguiente algoritmo:

1. Creamos la función que recibe como parámetro la probabilidad p.
2. establecemos los valores de estilo de nuestro gráfico, en este caso, los colores y el tipo de línea.
3. Definimos los valores de n que se van a iterar (3, 10, 25, 50) en un vector.
4. Creamos un marco vacío para el gráfico.
5. Iteramos sobre los distintos valores de n.
	1. Establecemos el rango de valores de x según el vector creado en el paso 3.
	2. Calculamos la distribución binomial.
	3. Calculamos la distribución normal aproximada.
	4. Agregamos las líneas al gráfico, utilizamos la variable *"type"* y *"pch"* para que se muestre el gráfico con puntos y líneas y un estilo diferente de punto por cada valor de "n". Por último cerramos el ciclo.
6. Agregamos una leyenda al gráfico para mejorar la visualización.
7. Ejecutamos la función para cada valor de *"p"*.

### Código
```R
# Función para crear el gráfico para un valor de p dado
plot_dbinomial_dnormal <- function(p) {
  # Establecer los colores para cada valor de n y el tipo de línea
  colores <- c("blue", "green", "red", "purple") # Colores
  tipo_linea <- c(1, 2, 3, 4) # Estilos de línea

  # Establecer el rango de valores de n
  valores_n <- c(3, 10, 25, 50)

  # Crear un marco vacío para el gráfico
  plot(0, 0, xlim = c(0, max(valores_n)), ylim = c(0, 1), xlab = "x", ylab = "Probabilidad",
       main = paste("Distribuciones Binomial y Normal para p =", p), type = "n")

  # Iterar sobre los distintos valores de n
  for (i in 1:length(valores_n)) {
    n <- valores_n[i]

    # Rango de valores para x
    valores_x <- 0:n
    
    # Calcular la distribución binomial
    valores_binom <- dbinom(valores_x, n, p)
    
    # Calcular la distribución normal aproximada
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    x_norm <- seq(min(valores_x), max(valores_x), 0.1)
    valores_norm <- dnorm(x_norm, mean = mu, sd = sigma)

    # Agregar las líneas al gráfico
    lines(valores_x, valores_binom, col = colores[i], type = "o", pch = i)
    lines(x_norm, valores_norm, col = colores[i], lty = tipo_linea[i])
  }

  # Añadir una leyenda
  legend("topright", legend = paste("n =", valores_n), col = colores, lty = tipo_linea, pch = 1:length(valores_n), cex = 0.8)
}
```
Creamos los gráficos para cada valor de p.
Línea: distribución binomial
Puntos: distribución normal aproximada
```R
# Crear gráficos para p = 0.5
plot_dbinomial_dnormal(0.5)

# Crear gráficos para p = 0.1
plot_dbinomial_dnormal(0.1)

# Crear gráficos para p = 0.05
plot_dbinomial_dnormal(0.05)
```
### Gráficos
#### Gráfico 1
![[Pasted image 20231210161950.png]]

#### Gráfico 2
![[Pasted image 20231210162001.png]]

#### Gráfico 3
![[Pasted image 20231210162015.png]]

## Parte 2
Representar en un gráfico para cada valor de probabilidad p=0.5, 0.1 y 0.05, la evolución de la aproximación de las distribuciones binomiales con n=3,25,50 a su correspondiente distribución de Poisson (Nota: se debe hacer un gráfico para 0.5 con todas las n, otro para 0.1 con todas las n y otro para 0.05 con todas las n).

## Resolución
Debido a que debemos iterar para cada valor de probabilidad con su respectivo valor de n, crearemos una función que reciba como parámetro la probabilidad que se vaya a iterar sobre todos los valores de n desfinidos en el enunciado.Y realizaremos la sistematización del siguiente algoritmo:

1. Creamos la función que recibe como parámetro la probabilidad p.
2. establecemos los valores de estilo de nuestro gráfico, en este caso, los colores y el tipo de línea.
3. Definimos los valores de n que se van a iterar (3, 25, 50) en un vector.
4. Creamos un marco vacío para el gráfico y defino el inio del eje y en cero (0) para mejorar visiblidad.
5. Iteramos sobre los distintos valores de n.
	1. Establecemos el rango de valores de x según el vector creado en el paso 3.
	2. Calculamos la distribución binomial.
	3. Calculamos la distribución de Poisson.
	4. Agregamos las líneas al gráfico, utilizamos la variable *"type"* y *"pch"* para que se muestre el gráfico con puntos y líneas y un estilo diferente de punto por cada valor de *"n"*. Por último cerramos el ciclo.
6. Agregamos una leyenda al gráfico para mejorar la visualización.
7. Ejectuamos la función para cada valor de *"p"*.

### Código
```R
# Función para crear el gráfico para un valor de p dado
plot_approximation_poisson <- function(p) {
  # Establecer los colores para cada valor de n
  colors <- c("blue", "green", "red")

  # Establecer el rango de valores de n
  n_values <- c(3, 25, 50)

  # Crear un marco vacío para el gráfico y defino el inio del eje y en cero (0) para mejorar visiblidad
  plot(0, 0, xlim = c(0, max(n_values)), ylim = c(0, 1), xlab = "x", ylab = "Probabilidad",
       main = paste("Distribuciones Binomial y Poisson para p =", p), type = "n")

  # Iterar sobre los distintos valores de n
  for (i in 1:length(n_values)) {
    n <- n_values[i]

    # Rango de valores para x
    x_values <- 0:n
    
    # Calcular la distribución binomial
    binom_values <- dbinom(x_values, n, p)
    
    # Calcular la distribución de Poisson aproximada
    lambda <- n * p
    poisson_values <- dpois(x_values, lambda)

    # Agregar las líneas al gráfico
    lines(x_values, binom_values, col = colors[i], type = "o", pch = i)
    lines(x_values, poisson_values, col = colors[i], lty = i, lwd = 2)
  }

  # Añadir una leyenda
  legend("topright", legend = paste("n =", n_values), col = colors, lty = 1:length(n_values), pch = 1:length(n_values), cex = 0.8)
}
```
Creamos los gráficos para cada valor de p.
Línea: distribución binomial.
Puntos: distribución Poisson.
```R
# Crear gráficos para p = 0.5
plot_approximation_poisson(0.5)

# Crear gráficos para p = 0.1
plot_approximation_poisson(0.1)

# Crear gráficos para p = 0.05
plot_approximation_poisson(0.05)
```

### Gráficos
#### Gráfico 1 (p=0.5)
![[Pasted image 20231210162653.png]]

#### Gráfico 2 (p=0.1)
![[Pasted image 20231210162657.png]]

#### Gráfico 3 (p=0.05)
![[Pasted image 20231210162701.png]]

## Parte 3
Analiza los 6 gráficos obtenidos y valora que aproximaciones son mejores indicando cuales han sido los factores determinantes.

### Respuesta
Según lo observado en los gráficos, se puede concluir que la aproximación de la distribución binomial a la distribución normal es mejor cuando el valor de p es igual a 0.5 y con el mayor número de ensayos, ya que en este caso la distribución binomial es simétrica y más precisa y la distribución normal también lo es. En los otros dos casos, la distribución binomial no es simétrica y la distribución normal tampoco lo es, por lo que la aproximación no es tan buena. Lo mismo podemos decir de la aproximación de la distribución binomial a la distribución de Poisson, ya que la distribución de Poisson es simétrica y la distribución binomial es simétrica cuando p = 0.5 mejorando su valor de aproximación a medida que incrementa el número de ensayos.
Es decir, hemos tomado como factores determinantes la simetría de las distribuciones dadas por el porcentaje de probabilidad y el número de ensayos.