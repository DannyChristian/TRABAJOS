# 1. Instalar y cargar paquetes necesarios
pkgs <- c("bench", "dqrng", "randtoolbox", "sodium", "dplyr", "ggplot2")
new  <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new)      # instala solo lo que falte

library(bench)        # High precision timing, memoria y GC :contentReference[oaicite:2]{index=2}
library(dqrng)        # PCG, Xoroshiro128++, Threefry en C++
library(randtoolbox)  # WELL y MT19937 vía runif()
library(sodium)       # random(n) con ChaCha20
library(dplyr)        # Manipulación de datos
library(ggplot2)      # Gráficos con Grammar of Graphics

# 2. Definir generadores de números aleatorios
gens <- list(
  LGC          = function(n) { RNGkind("Super-Duper"); runif(n) },
  MT19937      = function(n) { RNGkind("Mersenne-Twister"); runif(n) },
  WELL         = function(n) {
    set.generator("WELL", order = 512, version = "a", seed = 123)
    runif(n)
  },
  PCG64        = function(n) {
    dqRNGkind("pcg64"); dqset.seed(123L)
    dqrunif(n)
  },
  Xoroshiro128 = function(n) {
    dqRNGkind("Xoroshiro128++"); dqset.seed(123L)
    dqrunif(n)
  },
  Threefry     = function(n) {
    dqRNGkind("Threefry"); dqset.seed(123L)
    dqrunif(n)
  },
  ChaCha20     = function(n) {
    # random() devuelve n bytes; escalamos a [0,1]
    as.numeric(random(n)) / 255
  }
)

# 3. Configurar parámetros del benchmark
sizes <- c(1e2, 1e4, 1e6, 1e8)   # de 10^2 a 10^8 muestras
reps  <- 5                       # repeticiones para estabilizar
results <- data.frame()          # donde acumularemos los datos

# 4. Ejecutar benchmarking y recolectar métricas
for (gen in names(gens)) {
  fun <- gens[[gen]]
  for (n in sizes) {
    # ejecutar bench::mark una sola vez con varias iteraciones
    bm <- bench::mark(
      expr       = { vec <- fun(n) },
      iterations = reps,
      check      = FALSE,
      memory     = TRUE,
      filter_gc  = TRUE
    )

    results <- rbind(results, data.frame(
      generator   = gen,
      n           = n,
      min_time    = as.numeric(bm$min),
      median_time = as.numeric(bm$median),
      max_time    = max(as.numeric(bm$time[[1]])),  # <-- cálculo manual
      mem_mb      = as.numeric(bm$mem_alloc) / 1024^2,
      gc_per_sec  = as.numeric(bm$`gc/sec`),
      n_itr       = as.integer(bm$n_itr),
      throughput  = n / as.numeric(bm$median),
      stringsAsFactors = FALSE
    ))

  }
}

# ordenar resultados
results <- results %>% arrange(generator, n)

# 5. Exportar tabla de resultados
write.csv(results, "benchmark_results_full.csv", row.names = FALSE)


# 6. Graficar todas las métricas comparadas

#Tiempo (min, median, max) en un solo gráfico
ggplot(results, aes(x = n)) +
  geom_line(aes(y = min_time,    color = generator, linetype = "min")) +
  geom_line(aes(y = median_time, color = generator, linetype = "median")) +
  geom_line(aes(y = max_time,    color = generator, linetype = "max")) +
  scale_x_log10() + scale_y_log10() +
  labs(
    x        = "Tamaño de muestra (log10)",
    y        = "Tiempo (s, log10)",
    color    = "Generador",
    linetype = "Estadística",
    title    = "Comparativa de tiempo (min/median/max)"
  ) +
  theme_minimal()

#Memoria asignada
ggplot(results, aes(x = n, y = mem_mb, color = generator)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  labs(
    x     = "Tamaño de muestra (log10)",
    y     = "Memoria (MB)",
    title = "Consumo de memoria"
  ) +
  theme_minimal()

#Throughput
ggplot(results, aes(x = n, y = throughput, color = generator)) +
  geom_line() + geom_point() +
  scale_x_log10() + scale_y_log10() +
  labs(
    x     = "Tamaño de muestra (log10)",
    y     = "Throughput (números/s, log10)",
    title = "Throughput de generación"
  ) +
  theme_minimal()

#Presión del Garbage Collector (GC/sec)
ggplot(results, aes(x = n, y = gc_per_sec, color = generator)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  labs(
    x     = "Tamaño de muestra (log10)",
    y     = "GC por segundo",
    title = "Frecuencia de recolección de basura"
  ) +
  theme_minimal()

#Iteraciones efectivas (n_itr)
ggplot(results, aes(x = n, y = n_itr, color = generator)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  labs(
    x     = "Tamaño de muestra (log10)",
    y     = "Iteraciones efectivas",
    title = "Iteraciones tras filtrar GC"
  ) +
  theme_minimal()

