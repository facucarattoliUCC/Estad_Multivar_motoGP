# ==========================================================================================
# Paso R1: paquetes y ruta

# install.packages(c("readr", "dplyr", "tibble"))   # solo la primera vez

library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(corrplot)  # Para heatmap de correlación

getwd()   # debe terminar en ".../proyecto"

# ==========================================================================================
# Paso R2: importar los CSV
clasif      <- read_csv("venv/data/clean/clasificaciones.csv", guess_max = 100000)
eventos     <- read_csv("venv/data/clean/eventos.csv")
sesiones    <- read_csv("venv/data/clean/sesiones.csv")
standings   <- read_csv("venv/data/clean/standings.csv")

# ==========================================================================================
# Paso R3: primera mirada

dim(clasif)       # deberían ser 33106 filas y 23 columnas
glimpse(clasif)

glimpse(eventos)

glimpse(sesiones)

# chequeo significado variable status en clasif
clasif %>%
  count(status, sort = TRUE)

# ==========================================================================================
# ETAPA 0c :: CALIDAD DE DATOS
# ==========================================================================================
datasets <- list(
  clasif    = clasif,
  eventos   = eventos,
  sesiones  = sesiones,
  standings = standings
)

dir.create("outputs", showWarnings = FALSE)
dir.create("figuras", showWarnings = FALSE)

# *********************************************************************************************

resumen_na <- function(df, nombre) {
  tibble(
    dataset     = nombre,
    variable    = names(df),
    tipo        = sapply(df, function(x) class(x)[1]),
    n_filas     = nrow(df),
    n_na        = sapply(df, function(x) sum(is.na(x))),
    n_na_falsos = sapply(df, function(x) {
      if (is.character(x)) sum(trimws(x) %in% c("", "NA", "N/A", "n/a", "-", "null", "NULL"), na.rm = TRUE)
      else 0L
    })
  ) %>%
    mutate(pct_na = round(100 * (n_na + n_na_falsos) / n_filas, 2)) %>%
    arrange(desc(pct_na))
}

na_resumen <- bind_rows(lapply(names(datasets), function(n) resumen_na(datasets[[n]], n)))
na_resumen %>% filter(pct_na > 0) %>% print(n = Inf)

# --- ¿El faltante es estructural? Cruzar NA con status y con tipo de sesión ---
clasif %>%
  group_by(status) %>%
  summarise(n = n(),
            na_mejor_vuelta = sum(is.na(mejor_vuelta_s)),
            na_gap_primero  = sum(is.na(gap_primero_s)),
            na_top_speed    = sum(is.na(top_speed)),
            na_posicion     = sum(is.na(posicion)),
            .groups = "drop") %>%
  arrange(desc(n))

clasif %>%
  group_by(type) %>%
  summarise(n = n(),
            pct_na_mejor_vuelta = round(100 * mean(is.na(mejor_vuelta_s)), 1),
            pct_na_top_speed    = round(100 * mean(is.na(top_speed)), 1),
            pct_na_posicion     = round(100 * mean(is.na(posicion)), 1))

# Consistencia texto vs. numérico (¿hay texto sin su versión en segundos?)
clasif %>% filter(!is.na(mejor_vuelta) & is.na(mejor_vuelta_s)) %>% count(status)
clasif %>% filter(!is.na(gap_primero)  & is.na(gap_primero_s))  %>% count(status)

# Sesiones: faltantes de clima por año (suele ser que no se registraba antes)
sesiones %>%
  group_by(year) %>%
  summarise(n = n(),
            across(c(pista, aire, humedad, suelo, clima), ~ round(100 * mean(is.na(.x)), 1)))

# Gráfico
na_resumen %>%
  filter(pct_na > 0) %>%
  ggplot(aes(x = reorder(variable, pct_na), y = pct_na)) +
  geom_col() + coord_flip() +
  facet_wrap(~dataset, scales = "free_y") +
  labs(title = "Porcentaje de valores faltantes por variable", x = NULL, y = "% faltante")
ggsave("figuras/faltantes.png", width = 10, height = 7)

# ******************************************************************************************

# 6a. Duplicados exactos
sapply(datasets, function(df) sum(duplicated(df)))

# 6b. Duplicados por clave natural
claves <- list(
  clasif    = c("session_id", "rider_id"),
  eventos   = c("event_id"),            # si event_id se repite entre años: c("year", "event_id")
  sesiones  = c("session_id"),
  standings = c("year", "rider_id")
)

dup_por_clave <- function(df, clave) {
  df %>%
    group_by(across(all_of(clave))) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    arrange(across(all_of(clave)))
}

dups <- lapply(names(claves), function(n) dup_por_clave(datasets[[n]], claves[[n]]))
names(dups) <- names(claves)
sapply(dups, nrow)

# Si hay, inspeccionar (¿son idénticos o difieren en alguna columna?)
# View(dups$clasif)

# 6c. Consistencia de identificadores
# ¿Un rider_id con varios nombres o un mismo nombre con varios rider_id?
clasif %>% distinct(rider_id, piloto) %>% count(rider_id) %>% filter(n > 1)
clasif %>% distinct(rider_id, piloto) %>% count(piloto)   %>% filter(n > 1)

# ¿Claves foráneas rotas? (sesiones sin evento, clasif sin sesión)
anti_join(sesiones, eventos,  by = c("year", "event_id")) %>% nrow()
anti_join(clasif,   sesiones, by = "session_id")          %>% nrow()

# ******************************************************************************************
# 7a. Rangos de todas las numéricas
rangos <- bind_rows(lapply(names(datasets), function(n) {
  datasets[[n]] %>%
    select(where(is.numeric)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
    group_by(variable) %>%
    summarise(dataset = n,
              min = min(valor, na.rm = TRUE),
              mediana = median(valor, na.rm = TRUE),
              max = max(valor, na.rm = TRUE),
              n_negativos = sum(valor < 0, na.rm = TRUE),
              n_ceros = sum(valor == 0, na.rm = TRUE),
              .groups = "drop")
}))
rangos %>% print(n = Inf)

# 7b. Reglas de dominio explícitas (cada una devuelve las filas que la violan)
reglas <- list(
  # --- clasif ---
  posicion_menor_1         = clasif %>% filter(posicion < 1),
  mejor_vuelta_no_positiva = clasif %>% filter(mejor_vuelta_s <= 0),
  mejor_vuelta_muy_corta   = clasif %>% filter(mejor_vuelta_s < 30),      # ninguna vuelta de MotoGP baja de ~30 s
  top_speed_no_positiva    = clasif %>% filter(top_speed <= 0),
  top_speed_irreal         = clasif %>% filter(top_speed > 380),          # récord ~366 km/h
  vueltas_negativas        = clasif %>% filter(vueltas < 0),
  vuelta_mejor_gt_vueltas  = clasif %>% filter(vuelta_mejor > vueltas),   # la mejor vuelta no puede ser posterior a la última
  gap_primero_negativo     = clasif %>% filter(gap_primero_s < 0),
  gap_previo_negativo      = clasif %>% filter(gap_previo_s < 0),
  gap_previo_gt_primero    = clasif %>% filter(gap_previo_s > gap_primero_s),
  # --- sesiones ---
  humedad_fuera_0_100      = sesiones %>% filter(humedad < 0 | humedad > 100),
  aire_irreal              = sesiones %>% filter(aire < -10 | aire > 50),
  pista_irreal             = sesiones %>% filter(pista < -10 | pista > 70),
  suelo_irreal             = sesiones %>% filter(suelo < -10 | suelo > 70),
  pista_menor_que_aire_ext = sesiones %>% filter(pista < aire - 15),      # sospechoso, revisar
  # --- eventos ---
  fecha_fin_antes_inicio   = eventos %>% filter(date_end < date_start),
  fecha_futura_rara        = eventos %>% filter(date_start > as.Date("2026-12-31")),
  # --- standings ---
  puntos_negativos         = standings %>% filter(puntos < 0),
  pos_campeonato_menor_1   = standings %>% filter(pos_campeonato < 1)
)

imposibles <- tibble(regla = names(reglas), n_filas = sapply(reglas, nrow)) %>%
  arrange(desc(n_filas))
print(imposibles, n = Inf)

# Ver las filas de una regla que dio > 0
# View(reglas$vuelta_mejor_gt_vueltas)

# 7c. Consistencia entre tablas
# Año de la sesión vs. año del evento; año de fecha vs. columna year
sesiones %>% filter(!is.na(date), as.integer(format(as.Date(date), "%Y")) != year) %>% nrow()
eventos  %>% filter(as.integer(format(as.Date(date_start), "%Y")) != year) %>% nrow()

# 7d. Categóricas: niveles raros, typos, mayúsculas
clasif    %>% count(status, sort = TRUE)
clasif    %>% count(type, sort = TRUE)
clasif    %>% count(pais, sort = TRUE) %>% print(n = Inf)
clasif    %>% count(fabricante, sort = TRUE)
sesiones  %>% count(clima, sort = TRUE)
eventos   %>% count(status, sort = TRUE)
eventos   %>% count(test)

# *********************************************************************************************
# 8a. Resumen global (sirve para variables sin grupo natural)
resumen_outliers <- function(df, nombre, excluir = character(0), k = 1.5) {
  df %>%
    select(where(is.numeric), -any_of(excluir)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
    filter(!is.na(valor)) %>%
    group_by(variable) %>%
    summarise(dataset = nombre,
              q1 = quantile(valor, .25), q3 = quantile(valor, .75),
              iqr = q3 - q1,
              lim_inf = q1 - k * iqr, lim_sup = q3 + k * iqr,
              n_out = sum(valor < lim_inf | valor > lim_sup),
              pct_out = round(100 * n_out / n(), 2),
              .groups = "drop")
}

ids <- c("year", "number", "rider_id", "rider_legacy_id", "session_id", "event_id", "season_id")

out_resumen <- bind_rows(
  resumen_outliers(clasif,    "clasif",    ids),
  resumen_outliers(sesiones,  "sesiones",  ids),
  resumen_outliers(standings, "standings", ids),
  resumen_outliers(eventos,   "eventos",   ids)
)
out_resumen %>% arrange(desc(pct_out)) %>% print(n = Inf)

# 8b. Boxplots globales por dataset
boxplot_dataset <- function(df, nombre, excluir = ids) {
  df %>%
    select(where(is.numeric), -any_of(excluir)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
    filter(!is.na(valor)) %>%
    ggplot(aes(x = "", y = valor)) +
    geom_boxplot(outlier.colour = "red", outlier.alpha = 0.4) +
    facet_wrap(~variable, scales = "free_y") +
    labs(title = paste("Boxplots -", nombre), x = NULL, y = NULL)
}

p1 <- boxplot_dataset(clasif, "clasificaciones");  p1; ggsave("figuras/box_clasif.png",    p1, width = 10, height = 7)
p2 <- boxplot_dataset(sesiones, "sesiones");       p2; ggsave("figuras/box_sesiones.png",  p2, width = 10, height = 7)
p3 <- boxplot_dataset(standings, "standings");     p3; ggsave("figuras/box_standings.png", p3, width = 10, height = 7)

# 8c. Boxplots por circuito (donde tiene sentido comparar)
# Solo carreras para tiempos; ajustar el valor de type según cómo esté codificado
clasif %>% count(type)    # ver los valores reales

p4 <- clasif %>%
  filter(!is.na(mejor_vuelta_s)) %>%
  ggplot(aes(x = reorder(circuito, mejor_vuelta_s, median), y = mejor_vuelta_s)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.4) +
  coord_flip() +
  labs(title = "Mejor vuelta (s) por circuito", x = NULL, y = "segundos")
p4; ggsave("figuras/box_mejor_vuelta_circuito.png", p4, width = 9, height = 10)

p5 <- clasif %>%
  filter(!is.na(top_speed)) %>%
  ggplot(aes(x = reorder(circuito, top_speed, median), y = top_speed)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.4) +
  coord_flip() +
  labs(title = "Velocidad máxima por circuito", x = NULL, y = "km/h")
p5; ggsave("figuras/box_topspeed_circuito.png", p5, width = 9, height = 10)

p6 <- sesiones %>%
  ggplot(aes(x = factor(year), y = pista)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.4) +
  labs(title = "Temperatura de pista por año", x = "año", y = "°C")
p6; ggsave("figuras/box_pista_year.png", p6, width = 9, height = 6)

# 8d. Marcar (no borrar) atípicos DENTRO de cada grupo
marcar_outlier_grupo <- function(df, var, grupos, k = 1.5) {
  df %>%
    group_by(across(all_of(grupos))) %>%
    mutate(
      .q1 = quantile(.data[[var]], .25, na.rm = TRUE),
      .q3 = quantile(.data[[var]], .75, na.rm = TRUE),
      !!paste0("out_", var) := .data[[var]] < .q1 - k * (.q3 - .q1) |
        .data[[var]] > .q3 + k * (.q3 - .q1)
    ) %>%
    select(-.q1, -.q3) %>%
    ungroup()
}

clasif <- clasif %>%
  marcar_outlier_grupo("mejor_vuelta_s", c("circuito", "type")) %>%
  marcar_outlier_grupo("top_speed",      c("circuito", "type"))

sesiones <- sesiones %>%
  marcar_outlier_grupo("pista", "year") %>%
  marcar_outlier_grupo("aire",  "year")

# Cuántos quedaron marcados
clasif   %>% summarise(across(starts_with("out_"), ~ sum(.x, na.rm = TRUE)))
sesiones %>% summarise(across(starts_with("out_"), ~ sum(.x, na.rm = TRUE)))

# Mirar los más extremos antes de decidir
clasif %>% filter(out_mejor_vuelta_s) %>%
  select(year, circuito, type, piloto, status, mejor_vuelta_s) %>%
  arrange(desc(mejor_vuelta_s)) %>% head(20)

# *******************************************************************************************

registro <- tribble(
  ~dataset,   ~problema,   ~variable,                 ~n_afectados, ~criterio,                                        ~decision,                               ~justificacion,
  "clasif",   "faltantes", "mejor_vuelta_s / top_speed", NA,        "NA concentrados en status != clasificado/finalizó", "Conservar NA, no imputar",              "Faltante estructural: el piloto no completó vueltas o no hay dato",
  "clasif",   "faltantes", "gap_primero_s / gap_previo_s", NA,      "El líder no tiene gap",                          "Conservar NA (o 0 para el líder si se necesita)", "Un gap al primero no existe para el primero",
  "sesiones", "faltantes", "pista, aire, humedad, suelo", NA,       "% NA por año (R5)",                              "Conservar NA, excluir del análisis de clima esos años", "No se puede reconstruir la medición; imputar inventaría datos",
  "clasif",   "duplicados","session_id + rider_id",   NA,           "duplicated() por clave natural",                 "Eliminar si son idénticos; revisar si difieren", "Misma sesión y piloto no puede aparecer dos veces",
  "todas",    "duplicados","fila completa",           NA,           "duplicated() sobre todas las columnas",          "Eliminar duplicados exactos",           "Registros repetidos sin información nueva",
  "clasif",   "imposibles","vuelta_mejor > vueltas",  NA,           "vuelta_mejor > vueltas",                         "Convertir a NA",                        "La mejor vuelta no puede ser posterior a la última vuelta",
  "clasif",   "imposibles","gap_previo_s > gap_primero_s", NA,      "gap_previo_s > gap_primero_s",                   "Revisar y convertir a NA si es error",  "El gap al anterior no puede superar al gap al primero",
  "sesiones", "imposibles","humedad",                 NA,           "humedad < 0 o > 100",                            "Convertir a NA",                        "Es un porcentaje",
  "eventos",  "imposibles","date_end < date_start",   NA,           "date_end < date_start",                          "Corregir/NA según el caso",             "Un evento no termina antes de empezar",
  "standings","imposibles","puntos",                  NA,           "puntos < 0",                                     "Convertir a NA",                        "Los puntos no pueden ser negativos",
  "clasif",   "atipicos",  "mejor_vuelta_s",          NA,           "Tukey 1.5*IQR dentro de circuito x type",        "Marcar con flag, conservar",            "Extremos plausibles (lluvia, bandera roja, vuelta de salida)",
  "clasif",   "atipicos",  "top_speed",               NA,           "Tukey 1.5*IQR dentro de circuito x type",        "Marcar con flag, conservar si < 380",   "Velocidades altas son reales (rebufo); solo se elimina lo imposible",
  "sesiones", "atipicos",  "pista, aire",             NA,           "Tukey 1.5*IQR dentro de year",                   "Marcar con flag, conservar",            "Condiciones climáticas extremas son legítimas"
)

registro
write_csv(registro, "outputs/registro_decisiones_calidad.csv")

# *******************************************************************************************
# Función auxiliar: cuenta cuántas filas cambian en cada regla (para llenar n_afectados)
clasif_limpio <- clasif %>%
  distinct() %>%                                               # 1) duplicados exactos
  distinct(session_id, rider_id, .keep_all = TRUE) %>%         # 2) duplicados por clave (queda la 1ª; revisar antes)
  mutate(
    vuelta_mejor   = if_else(vuelta_mejor > vueltas, NA_real_, as.numeric(vuelta_mejor)),  # 3) imposibles -> NA
    mejor_vuelta_s = if_else(mejor_vuelta_s <= 0 | mejor_vuelta_s < 30, NA_real_, mejor_vuelta_s),
    top_speed      = if_else(top_speed <= 0 | top_speed > 380, NA_real_, top_speed),
    gap_primero_s  = if_else(gap_primero_s < 0, NA_real_, gap_primero_s),
    gap_previo_s   = if_else(gap_previo_s < 0, NA_real_, gap_previo_s)
  ) %>%
  filter(is.na(posicion) | posicion >= 1)

sesiones_limpio <- sesiones %>%
  distinct() %>%
  distinct(session_id, .keep_all = TRUE) %>%
  mutate(
    humedad = if_else(humedad < 0 | humedad > 100, NA_real_, humedad),
    aire    = if_else(aire  < -10 | aire  > 50, NA_real_, aire),
    pista   = if_else(pista < -10 | pista > 70, NA_real_, pista),
    suelo   = if_else(suelo < -10 | suelo > 70, NA_real_, suelo)
  )

eventos_limpio <- eventos %>%
  distinct() %>%
  distinct(event_id, .keep_all = TRUE)

standings_limpio <- standings %>%
  distinct() %>%
  distinct(year, rider_id, .keep_all = TRUE) %>%
  mutate(puntos = if_else(puntos < 0, NA_real_, puntos))

# Verificación antes / después
tibble(
  tabla  = c("clasif", "sesiones", "eventos", "standings"),
  antes  = c(nrow(clasif), nrow(sesiones), nrow(eventos), nrow(standings)),
  despues = c(nrow(clasif_limpio), nrow(sesiones_limpio), nrow(eventos_limpio), nrow(standings_limpio))
) %>% mutate(eliminadas = antes - despues)

# Guardar
dir.create("venv/data/final", showWarnings = FALSE)
write_csv(clasif_limpio,    "venv/data/final/clasificaciones_limpio.csv")
write_csv(sesiones_limpio,  "venv/data/final/sesiones_limpio.csv")
write_csv(eventos_limpio,   "venv/data/final/eventos_limpio.csv")
write_csv(standings_limpio, "venv/data/final/standings_limpio.csv")

# **************************************************************************************

# Pendiente de faltantes: ¿dónde están los NA de "number"?
clasif %>% count(type, number) %>% print(n = Inf)

# Diagnóstico: ¿cómo vienen las columnas de clima?
sesiones %>% select(pista, aire, humedad, suelo) %>% glimpse()
sesiones %>% select(pista, aire, humedad, suelo) %>% head(10)

# ¿Cuántos valores NO se pueden convertir a número, por columna?
sesiones %>%
  summarise(across(c(pista, aire, humedad, suelo),
                   ~ sum(is.na(suppressWarnings(as.numeric(.x))) & !is.na(.x))))

# *************************************************************************************
sesiones <- sesiones %>%
  mutate(
    aire_c      = parse_number(aire),
    humedad_pct = parse_number(humedad),
    suelo_c     = parse_number(suelo)
  )

# Verificación 1: la conversión no debe generar NA nuevos (esperado: 0 en las tres)
sesiones %>% summarise(across(c(aire_c, humedad_pct, suelo_c), ~ sum(is.na(.x))))

# Verificación 2: rangos de las nuevas columnas
sesiones %>% select(aire_c, humedad_pct, suelo_c) %>% summary()

# Verificación 3: valores de pista y si los 8 NA de pista y clima son las mismas sesiones
sesiones %>% count(pista)
sesiones %>% filter(is.na(pista)) %>% count(is.na(clima))

# ****************************************************************************************
sesiones <- sesiones %>%
  mutate(
    aire_c      = parse_number(aire),
    humedad_pct = parse_number(humedad),
    suelo_c     = parse_number(suelo)
  )

sesiones %>%
  filter(is.na(aire_c) | is.na(humedad_pct) | is.name(suelo_c)) %>%
  select(year, event_id, type, pista, clima, aire, humedad, suelo) %>%
  print(n = Inf)
# ****************************************************************************************

# Duplicados exactos
sapply(list(clasif = clasif, eventos = eventos, sesiones = sesiones, standings = standings),
       function(df) sum(duplicated(df)))

# Duplicados por clave (misma sesión y piloto; mismo piloto y año; etc.)
clasif    %>% count(session_id, rider_id) %>% filter(n > 1) %>% nrow()
sesiones  %>% count(session_id)           %>% filter(n > 1) %>% nrow()
eventos   %>% count(event_id)             %>% filter(n > 1) %>% nrow()
standings %>% count(year, rider_id)       %>% filter(n > 1) %>% nrow()
# ****************************************************************************************

reglas <- list(
  posicion_menor_1        = clasif   %>% filter(posicion < 1),
  mejor_vuelta_no_pos     = clasif   %>% filter(mejor_vuelta_s <= 0),
  top_speed_no_pos        = clasif   %>% filter(top_speed <= 0),
  vueltas_negativas       = clasif   %>% filter(vueltas < 0),
  vuelta_mejor_gt_vueltas = clasif   %>% filter(vuelta_mejor > vueltas),
  gap_primero_negativo    = clasif   %>% filter(gap_primero_s < 0),
  gap_previo_negativo     = clasif   %>% filter(gap_previo_s < 0),
  gap_previo_gt_primero   = clasif   %>% filter(gap_previo_s > gap_primero_s),
  humedad_fuera_0_100     = sesiones %>% filter(humedad_pct < 0 | humedad_pct > 100),
  puntos_negativos        = standings %>% filter(puntos < 0)
)

imposibles <- tibble(regla = names(reglas), n_filas = sapply(reglas, nrow))
print(imposibles, n = Inf)

# Los 214 top_speed = 0: ¿en qué tipo de sesión y con qué status caen?
clasif %>% filter(top_speed == 0) %>% count(type, status)

clasif %>% filter(top_speed == 0) %>%
  summarise(n = n(),
            con_mejor_vuelta = sum(!is.na(mejor_vuelta_s)),
            sin_mejor_vuelta = sum(is.na(mejor_vuelta_s)),
            vueltas_media    = mean(vueltas, na.rm = TRUE))
# ****************************************************************************************

library(tidyr); library(ggplot2)
dir.create("outputs", showWarnings = FALSE)
dir.create("figuras", showWarnings = FALSE)
dir.create("venv/data/final", showWarnings = FALSE)

# 1) Conversión de sesiones (texto -> número; "º" y "%" sin número quedan NA)
sesiones <- sesiones %>%
  mutate(aire_c      = parse_number(aire),
         humedad_pct = parse_number(humedad),
         suelo_c     = parse_number(suelo))

# 2) Corregir imposibles: top_speed = 0 -> NA (no se elimina ninguna fila)
clasif_limpio <- clasif %>%
  select(-any_of(c("out_mejor_vuelta_s", "out_top_speed"))) %>%   # borra banderas provisorias
  mutate(top_speed = if_else(top_speed <= 0, NA_real_, top_speed))

# 3) Recalcular atípicos (Tukey 1.5*IQR, dentro de circuito x tipo de sesión)
marcar_outlier_grupo <- function(df, var, grupos, k = 1.5) {
  df %>%
    group_by(across(all_of(grupos))) %>%
    mutate(.q1 = quantile(.data[[var]], .25, na.rm = TRUE),
           .q3 = quantile(.data[[var]], .75, na.rm = TRUE),
           !!paste0("out_", var) := .data[[var]] < .q1 - k * (.q3 - .q1) |
             .data[[var]] > .q3 + k * (.q3 - .q1)) %>%
    select(-.q1, -.q3) %>%
    ungroup()
}

clasif_limpio <- clasif_limpio %>%
  marcar_outlier_grupo("mejor_vuelta_s", c("circuito", "type")) %>%
  marcar_outlier_grupo("top_speed",      c("circuito", "type"))

# Números finales de atípicos (para la pregunta 1 y el registro)
n_out_mv <- sum(clasif_limpio$out_mejor_vuelta_s, na.rm = TRUE)
n_out_ts <- sum(clasif_limpio$out_top_speed, na.rm = TRUE)
n_out_mv; n_out_ts
clasif_limpio %>% count(out_mejor_vuelta_s)
clasif_limpio %>% count(out_top_speed)

# 4) Boxplots de las variables de H1, ya corregidas (figura para el informe)
p_final <- clasif_limpio %>%
  select(mejor_vuelta_s, gap_primero_s, gap_previo_s, posicion, top_speed, vueltas) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  ggplot(aes(x = "", y = valor)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.4) +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Boxplots de las variables de rendimiento (datos corregidos)", x = NULL, y = NULL)
p_final
ggsave("figuras/boxplots_variables_rendimiento.png", p_final, width = 10, height = 7)

# 5) Tablas finales (eventos y standings no cambian)
sesiones_limpio  <- sesiones
eventos_limpio   <- eventos
standings_limpio <- standings

# 6) Registro de decisiones
registro <- tribble(
  ~dataset,   ~problema,    ~variable,                              ~n_afectados, ~criterio,                                          ~decision,                                  ~justificacion,
  "clasif",   "faltantes",  "mejor_vuelta_s, vuelta_mejor",         6255,         "NA por status y tipo de sesión",                   "Conservar NA, no imputar",                 "Estructural: RAC y SPR no traen el dato; resto son pilotos sin vuelta",
  "clasif",   "faltantes",  "top_speed, gap_previo_s",              6041,         "100 % NA en RAC y SPR (4701 + 1340)",              "Conservar NA, no imputar",                 "La fuente no registra el dato en carreras y sprints",
  "clasif",   "faltantes",  "posicion",                             1362,         "NA en status distinto de INSTND",                  "Conservar NA",                             "Piloto sin clasificar: la posición no existe",
  "clasif",   "faltantes",  "number",                               12028,        "NA en WUP, RAC, SPR, PR, TTS y 193 FP",            "Conservar NA",                             "Estructural: sesiones únicas no se numeran",
  "sesiones", "faltantes",  "aire, humedad, suelo (valor 'º' / '%')", 20,         "Texto con símbolo y sin número",                   "Convertir a NA (aire_c, humedad_pct, suelo_c)", "Faltante disfrazado; no se puede reconstruir",
  "sesiones", "faltantes",  "pista, clima",                         8,            "NA reales",                                        "Conservar NA",                             "Casos aislados sin patrón por año",
  "eventos / standings", "faltantes", "country / equipo",           2,            "NA reales (1 y 1)",                                "Conservar NA",                             "Casos aislados",
  "todas",    "duplicados", "fila completa y claves naturales",     0,            "duplicated() y conteo por clave",                  "No se elimina ninguna fila",               "No hay duplicados",
  "clasif",   "imposibles", "top_speed <= 0",                       214,          "Velocidad 0 con vueltas = 0 y sin mejor_vuelta_s", "Convertir a NA, conservar la fila",        "El 0 es ausencia de dato, no medición",
  "todas",    "imposibles", "otras 9 reglas de dominio",            0,            "posición, tiempos, gaps, vueltas, humedad, puntos", "Sin acción",                              "Todos los valores dentro de rangos posibles",
  "clasif",   "atipicos",   "mejor_vuelta_s",                       n_out_mv,     "Tukey [Q1-1.5*IQR, Q3+1.5*IQR] por circuito x tipo", "Marcar con out_mejor_vuelta_s y conservar", "Extremos plausibles (lluvia, bandera roja)",
  "clasif",   "atipicos",   "top_speed",                            n_out_ts,     "Tukey [Q1-1.5*IQR, Q3+1.5*IQR] por circuito x tipo", "Marcar con out_top_speed y conservar",    "Valores altos o bajos plausibles; los imposibles ya se trataron"
)
registro
write_csv(registro, "outputs/registro_decisiones_calidad.csv")

# 7) Guardar bases finales
write_csv(clasif_limpio,    "venv/data/final/clasificaciones_limpio.csv")
write_csv(sesiones_limpio,  "venv/data/final/sesiones_limpio.csv")
write_csv(eventos_limpio,   "venv/data/final/eventos_limpio.csv")
write_csv(standings_limpio, "venv/data/final/standings_limpio.csv")

# 8) Filas antes / después (para la pregunta 2)
tibble(tabla = c("clasif", "sesiones", "eventos", "standings"),
       antes = c(nrow(clasif), nrow(sesiones), nrow(eventos), nrow(standings)),
       despues = c(nrow(clasif_limpio), nrow(sesiones_limpio), nrow(eventos_limpio), nrow(standings_limpio)))

# ========================================================================================
# ETAPA 0d :: ANALISIS DESCRIPTIVO
# ========================================================================================

str(datasets)   # revisar tipos de variables

# ****************************************************************************************

library(ggplot2)

# ---- 1. Limpieza mínima: sesiones tiene numéricas guardadas como texto ----
# ("23º", "71%", "28º") -> se convierten a número
datasets$sesiones$aire    <- readr::parse_number(datasets$sesiones$aire)
datasets$sesiones$humedad <- readr::parse_number(datasets$sesiones$humedad)
datasets$sesiones$suelo   <- readr::parse_number(datasets$sesiones$suelo)

# ---- 2. Función que hace todo el análisis descriptivo de una tabla ----
descriptivo <- function(df, nombre, ids = character(0),
                        excluir_num = character(0), como_cat = character(0)) {
  
  cat("\n\n##########", toupper(nombre), "##########\n")
  df <- as.data.frame(df)
  
  # variables numéricas que en realidad son categorías (ej. year)
  for (v in como_cat) df[[v]] <- as.factor(df[[v]])
  
  # separar por tipo, sacando identificadores y variables redundantes
  numericas   <- df[sapply(df, is.numeric)]
  numericas   <- numericas[setdiff(names(numericas), excluir_num)]
  categoricas <- df[sapply(df, function(x) is.character(x) || is.factor(x) || is.logical(x))]
  categoricas <- categoricas[setdiff(names(categoricas), ids)]
  
  # ---- Resumen numérico ----
  if (ncol(numericas) > 0) {
    cat("\n--- Resumen numérico ---\n")
    resumen <- data.frame(
      n       = sapply(numericas, function(x) sum(!is.na(x))),
      NAs     = sapply(numericas, function(x) sum(is.na(x))),
      media   = sapply(numericas, mean, na.rm = TRUE),
      desvio  = sapply(numericas, sd, na.rm = TRUE),
      minimo  = sapply(numericas, min, na.rm = TRUE),
      Q1      = sapply(numericas, quantile, probs = 0.25, na.rm = TRUE),
      mediana = sapply(numericas, median, na.rm = TRUE),
      Q3      = sapply(numericas, quantile, probs = 0.75, na.rm = TRUE),
      maximo  = sapply(numericas, max, na.rm = TRUE)
    )
    print(round(resumen, 2))
  }
  
  # ---- Matriz de correlaciones + mapa de calor (necesita 2+ numéricas) ----
  if (ncol(numericas) >= 2) {
    matriz_cor <- cor(numericas, use = "pairwise.complete.obs")
    cat("\n--- Matriz de correlaciones ---\n")
    print(round(matriz_cor, 2))
    
    cor_larga <- as.data.frame(as.table(matriz_cor))
    names(cor_larga) <- c("Var1", "Var2", "Correlacion")
    
    p <- ggplot(cor_larga, aes(Var1, Var2, fill = Correlacion)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Correlacion, 2)), size = 3) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                           midpoint = 0, limits = c(-1, 1)) +
      labs(title = paste("Correlaciones -", nombre), x = NULL, y = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
  } else {
    cat("\n(No hay 2 o más variables numéricas: no se calcula correlación)\n")
  }
  
  # ---- Tablas de frecuencias ----
  for (var in names(categoricas)) {
    cat("\n--- Frecuencias:", var, "---\n")
    frec_abs <- sort(table(categoricas[[var]], useNA = "ifany"), decreasing = TRUE)
    tabla <- data.frame(
      Categoria         = names(frec_abs),
      Frec_absoluta     = as.vector(frec_abs),
      Frec_relativa_pct = round(as.vector(prop.table(frec_abs)) * 100, 2)
    )
    print(tabla, row.names = FALSE)
  }
}

# ---- 3. Aplicarla a las 4 tablas ----
descriptivo(datasets$clasif, "clasif",
            ids         = c("event_id", "session_id", "rider_id"),
            excluir_num = c("rider_legacy_id", "number", "gap_primero", "gap_previo"),
            como_cat    = "year")

descriptivo(datasets$eventos, "eventos",
            ids      = c("season_id", "event_id"),
            como_cat = "year")

descriptivo(datasets$sesiones, "sesiones",
            ids         = c("event_id", "session_id"),
            excluir_num = "number",
            como_cat    = "year")

descriptivo(datasets$standings, "standings",
            ids         = "rider_id",
            excluir_num = "rider_legacy_id",
            como_cat    = "year")

# ================================================================================
# ETAPA 1: ACP o PCA (Principal Component Analysis)
# ================================================================================

# INICIO Etapa 1a 

library(tidyverse)

# --- Lectura (rutas tomadas de list.files, sin escribir nada a mano) ---
ruta <- list.files(getwd(), pattern = "limpio", recursive = TRUE, full.names = TRUE)
rd   <- function(p) read_csv(ruta[grepl(p, ruta)], col_types = cols(.default = "c"))

# --Asigno variable rd (tiene rutas con los archivos csv) a las variables standing, sesiones, clasificacion -- 
standing      <- rd("standings")
sesiones      <- rd("sesiones")
clasificacion <- rd("clasif")

# --- Chequeo: las claves de las tablas que se unen deben ser únicas ---
sesiones %>% count(year, event_id, session_id) %>% dplyr::filter(n > 1) %>% nrow() # debe dar 0
standing %>% count(year, rider_id)             %>% dplyr::filter(n > 1) %>% nrow() # debe dar 0

# --- Unión ---
datos <- clasificacion %>%
  left_join(select(sesiones, year, event_id, session_id, aire_c, humedad_pct, suelo_c),
            by = c("year", "event_id", "session_id")) %>%
  left_join(select(standing, year, rider_id, puntos),
            by = c("year", "rider_id"))

nrow(clasificacion); nrow(datos)   # deben ser iguales (33106)

# --- Parseo y armado de X_num ---
num  <- function(x) as.numeric(gsub(",", ".", gsub("[^0-9,.\\-]", "", as.character(x))))
vars <- c("puntos", "suelo_c", "humedad_pct", "aire_c",
          "vuelta_mejor", "vueltas", "top_speed",
          "mejor_vuelta_s", "gap_primero_s", "gap_previo_s")

datos_num <- datos %>% mutate(across(all_of(vars), num))
head(datos_num[, vars])                  # verificar que son numéricas y razonables
colSums(is.na(datos_num[, vars]))        # NA por variable, antes de eliminar filas

ok    <- complete.cases(datos_num[, vars])
X_num <- datos_num[ok, vars]
nrow(X_num) # --cantidad de filas en X_num debe ser menor que nrow(clasificacion); nrow(datos): (33106)--

# --- 1) Varianza y desvío de cada variable (captura a pegar) ---
varianzas <- data.frame(variable = vars,
                        varianza = sapply(X_num, var),
                        desvio   = sapply(X_num, sd))
print(varianzas, digits = 4, row.names = FALSE)

barplot(varianzas$varianza, names.arg = vars, log = "y", las = 2,
        main = "Varianza por variable (escala log)")

# --- 2) ACP sin y con estandarizar ---
pca_sin <- prcomp(X_num, scale. = FALSE)
pca_con <- prcomp(X_num, scale. = TRUE)

round(summary(pca_sin)$importance[2, 1:2] * 100, 1)   # % varianza PC1 y PC2, sin estandarizar
round(summary(pca_con)$importance[2, 1:2] * 100, 1)   # % varianza PC1 y PC2, estandarizado
round(cbind(sin_estandarizar = pca_sin$rotation[, 1],
            estandarizado    = pca_con$rotation[, 1]), 3)

# FIN Etapa 1a 
# ************************************************************************************
# INICIO Etapa 1b

eig <- pca_con$sdev^2

tab_eig <- data.frame(
  PC        = paste0("PC", seq_along(eig)),
  autovalor = eig,
  var_pct   = 100 * eig / sum(eig),
  acum_pct  = cumsum(100 * eig / sum(eig)),
  kaiser    = eig > 1
)
# -- Tabla: PC , autovalores, porcentaje varianza explicado por cada PC, porcentaje acumulado y kaiser --
print(tab_eig, digits = 3, row.names = FALSE)   # captura a pegar

sum(eig)   # debe dar 10

plot(eig, type = "b", pch = 19, xaxt = "n",
     xlab = "Componente", ylab = "Autovalor", main = "Gráfico del codo")
axis(1, at = seq_along(eig))
abline(h = 1, lty = 2, col = "red")   # línea de Kaiser

# FIN Etapa 1b
# ************************************************************************************
# INICIO Etapa 1c

k   <- 4   # <- reemplazar por el número de componentes que retuvieron en b)
eig <- pca_con$sdev^2

carga <- sweep(pca_con$rotation[, 1:k], 2, pca_con$sdev[1:k], "*")  # cor variable-componente
cos2  <- carga^2                                                    # calidad de representación
contr <- 100 * sweep(cos2, 2, eig[1:k], "/")                        # contribuciones (%)

colnames(carga) <- colnames(cos2) <- colnames(contr) <- paste0("Dim", 1:k)

round(carga, 3)                                  # captura 1a: correlaciones
round(cbind(cos2, total = rowSums(cos2)), 3)     # captura 1b: calidad de representación
round(contr[, 1:2], 1)                           # captura 2: contribuciones a Dim1 y Dim2

colSums(contr)                                   # control: cada columna debe sumar 100

# FIN Etapa 1c
# ************************************************************************************
# INICIO Etapa 1d

library(factoextra)   # si no la tienen: install.packages("factoextra")
install.packages("factoextra")

# Variable de contexto alineada con las filas de X_num
ctx <- factor(datos_num$type[ok])
table(ctx)

# Círculo de correlaciones
fviz_pca_var(pca_con, axes = c(1, 2), repel = TRUE,
             col.var = "contrib")          # color según contribución al plano

# Biplot: individuos coloreados por ctx + flechas
fviz_pca_biplot(pca_con, axes = c(1, 2),
                geom.ind = "point", col.ind = ctx, pointsize = 1, alpha.ind = 0.4,
                addEllipses = TRUE, ellipse.level = 0.9,
                col.var = "black", repel = TRUE, legend.title = "Tipo de sesión")

# 3 observaciones más extremas en el plano 1-2
sc   <- pca_con$x[, 1:2]
dist <- sqrt(rowSums(sc^2))
top3 <- order(dist, decreasing = TRUE)[1:3]

ext <- cbind(fila_X = top3,
             dist   = round(dist[top3], 2),
             round(sc[top3, ], 2),
             datos_num[ok, ][top3, c("year", "circuito", "type", "piloto", vars)])

# --captura para la tabla de extremos--
print(as.data.frame(ext), row.names = FALSE)

# Calidad de representación en el plano (para la pregunta sobre la variable mal representada)
round(sort(rowSums(cos2[, 1:2])), 3)

# FIN Etapa 1d
# ************************************************************************************
# ====================================================================================
# ETAPA 2: Analisis Fctorial Exploratorio 
# ====================================================================================

# -- descargar tabla X_num como archivo csv --  
write.csv(X_num, file = "X_num_var_numericas.csv", row.names = FALSE)

# ************************************************************************************
# INICIO Etapa 2a  
install.packages("psych")
library(psych)
X <- read.csv("X_num_var_numericas.csv")

# Muestra, faltantes, duplicados
dim(X); colSums(is.na(X)); sum(duplicated(X))

# Normalidad y atípicos (antes de tratar)
describe(X)[, c("skew","kurtosis")]
colSums(abs(scale(X)) > 3)

# Matriz original
R0 <- cor(X)
det(R0); cortest.bartlett(R0, n = nrow(X)); KMO(R0)

# Tratamiento: winsorizar p99.5 + log(1+x) en los gaps
for (v in c("gap_primero_s","gap_previo_s")) {
  tope <- quantile(X[[v]], 0.995)
  X[[v]] <- log1p(pmin(X[[v]], tope))
}

# Eliminar top_speed (MSA < 0.5)
X2 <- X[, setdiff(names(X), "top_speed")]

# Matriz final (capturas)
R <- cor(X2)
det(R)
cortest.bartlett(R, n = nrow(X2))
KMO(R)
describe(X2)[, c("skew","kurtosis")]

# -- chequeo variables muy correlacionadas -- 
R <- cor(X)
det(R)               # compare con 0.00001
diag(solve(R))       # VIF de cada variable
which(abs(R) > 0.8 & R < 1, arr.ind = TRUE)   # pares muy correlacionados

# FIN Etapa 2a  
# ************************************************************************************
# INICIO Etapa 2b

library(psych)
X0 <- read.csv("X_num_var_numericas.csv")

# INTERRUPTOR: variables que se excluyen del AFE.
# 8 variables:  c("puntos", "top_speed")
# 10 variables: c()
vars_excluir <- c("puntos", "top_speed")
X <- X0[, setdiff(names(X0), vars_excluir)]

# ---- Kaiser ----
ev <- eigen(cor(X))$values
round(ev, 3); sum(ev > 1)
round(cumsum(ev) / length(ev) * 100, 1)           # % acumulado

# ---- Gráfico de sedimentación ----
scree(X, factors = FALSE, pc = TRUE)
abline(h = 1, lty = 2, col = "red")

# ---- Análisis paralelo ----
fa.parallel(X, fm = "pa", fa = "both", n.iter = 500)

# ---- Interpretabilidad: soluciones vecinas ----
for (k in 2:4) {
  f <- fa(X, nfactors = k, fm = "pa", rotate = "varimax")
  cat("\n===== k =", k, "=====\n")
  print(f$loadings, cutoff = 0.30)
  print(round(f$communality, 2))                   # > 1 indica caso Heywood
  cat("RMSR:", round(f$rms, 4), "\n")
}

# FIN Etapa 2b
# ******************************************************************************
# INICIO Etapa 2c / 2d

library(GPArotation)    # necesaria para oblimin
X0 <- read.csv("X_num_var_numericas.csv")
vars_excluir <- c("puntos", "top_speed")      # suplementarias
X <- X0[, setdiff(names(X0), vars_excluir)]

# Extracción por ejes principales, 4 factores, con cada rotación
f_none <- fa(X, nfactors = 4, fm = "pa", rotate = "none")
f_vari <- fa(X, nfactors = 4, fm = "pa", rotate = "varimax")
f_quar <- fa(X, nfactors = 4, fm = "pa", rotate = "quartimax")
f_prom <- fa(X, nfactors = 4, fm = "pa", rotate = "promax")
f_obli <- fa(X, nfactors = 4, fm = "pa", rotate = "oblimin")

# Patrón de cada rotación (se ocultan cargas < 0.30)
for (nm in c("f_none","f_vari","f_quar","f_prom","f_obli")) {
  cat("\n=====", nm, "=====\n")
  f <- get(nm)
  print(f$loadings, cutoff = 0.30)
  cat("Complejidad media:", round(mean(f$complexity), 2), "\n")
}

# Correlación entre factores en las oblicuas
round(f_prom$Phi, 2)
round(f_obli$Phi, 2)

# Comunalidades, unicidades y varianza (rotación elegida: varimax)
round(f_vari$communality, 2)       # h2; > 1 = Heywood
round(f_vari$uniquenesses, 2)
f_vari$Vaccounted                  # SC, % y % acumulado
f_vari$rms                         # RMSR

# FIN Etapa 2c / 2d
# ******************************************************************************
# INICIO Etapa 2e

# -- carga de variables sobre factores --
print(f$loadings, cutoff = 0.30) 

X0 <- read.csv("X_num_var_numericas.csv")

# Variables de cada factor (según la matriz de patrón final)
factores <- list(
  PA1 = c("suelo_c", "aire_c", "humedad_pct"),   # humedad_pct carga negativa
  PA2 = c("vueltas", "vuelta_mejor"),
  PA3 = c("gap_primero_s", "gap_previo_s")
  # PA4 = "mejor_vuelta_s"  -> una sola variable, el alfa no se puede calcular
)

# ---- 1. Alfa de cada factor con detalle ----
for (nm in names(factores)) {
  cat("\n=====", nm, ":", paste(factores[[nm]], collapse = ", "), "=====\n")
  a <- psych::alpha(X0[, factores[[nm]]], check.keys = TRUE)  # invierte ítems con carga negativa
  print(a)
}

# ---- 2. Tabla resumen (para la captura) ----
resumen <- do.call(rbind, lapply(names(factores), function(nm) {
  a <- psych::alpha(X0[, factores[[nm]]], check.keys = TRUE, warnings = FALSE)
  data.frame(
    Factor      = nm,
    N_variables = length(factores[[nm]]),
    Alfa_bruto  = round(a$total$raw_alpha, 3),
    Alfa_estand = round(a$total$std.alpha, 3),
    r_media     = round(a$total$average_r, 3)
  )
}))
print(resumen)

# ---- 3. Alfa si se elimina cada variable (solo factores con 3 o más variables) ----
a1 <- psych::alpha(X0[, factores$PA1], check.keys = TRUE)
round(a1$alpha.drop[, c("raw_alpha", "std.alpha")], 3)

# ---- 4. Verificación manual de PA1 invirtiendo humedad_pct a mano ----
X_pa1 <- X0[, c("suelo_c", "aire_c", "humedad_pct")]
X_pa1$humedad_pct <- -X_pa1$humedad_pct
psych::alpha(X_pa1)$total[, c("raw_alpha", "std.alpha", "average_r")]

# FIN Etapa 2e
# ******************************************************************************

# ================================================================================
# ETAPA 3: ANALISIS DE CONGLOMERADOS
# ================================================================================

#INCIO Etapa 3a, 3b

# Reemplaza 'mi_tabla' por el nombre de tu objeto en R 
# y 'resultado.csv' por el nombre con el que quieres guardarlo
write.csv(clasif_limpio, file = "clasif_limpio.csv", row.names = FALSE)
write.csv(eventos_limpio, file = "eventos_limpio.csv", row.names = FALSE)
write.csv(sesiones_limpio, file = "sesiones_limpio.csv", row.names = FALSE)
write.csv(standings_limpio, file = "standings_limpio.csv", row.names = FALSE)

# 3a. PASO 1. Lectura de datos
clasif <- read_csv("clasif_limpio.csv", show_col_types = FALSE)
dim(clasif)    # esperado: 33106 filas, 25 columnas

# 3a. PASO 2. Seleccion de sesiones validas
# RAC y SPR tienen NA estructurales en mejor_vuelta_s, gap_previo_s, etc.
# Nos quedamos solo con sesiones con datos completos de las variables.

vars_cluster <- c("vueltas", "gap_primero_s", "gap_previo_s")
base_ses <- clasif %>%
  dplyr::filter(if_all(all_of(c(vars_cluster, "mejor_vuelta_s")), ~ !is.na(.x)))

nrow(base_ses)           # esperado: 26851
table(base_ses$type)     # esperado: FP 15636, Q 5038, WUP 4609, PR 1199, P 349, TTS 20

# 3a. PASO 3. Chequeo de redundancia: mejor_vuelta_s vs gap_primero_s
# gap_primero_s es la diferencia entre la mejor vuelta del piloto y la
# mejor vuelta de la sesion. Lo verificamos numericamente.

base_ses <- base_ses %>%
  group_by(session_id) %>%
  mutate(gap_recalculado = mejor_vuelta_s - min(mejor_vuelta_s)) %>%
  ungroup()

max(abs(base_ses$gap_primero_s - base_ses$gap_recalculado))   # esperado: ~0
cor(base_ses$gap_primero_s, base_ses$gap_recalculado)         # esperado: 1


# 3a. PASO 4. Agregacion piloto-temporada (MEDIA de las sesiones)

base_pt <- base_ses %>%
  group_by(year, rider_id, piloto) %>%
  summarise(
    n_sesiones = n(),
    across(all_of(vars_cluster), mean),
    .groups = "drop"
  ) %>%
  mutate(etiqueta = make.unique(paste(piloto, year, sep = " ")))

nrow(base_pt)                 # esperado: 337
summary(base_pt$n_sesiones)   # esperado: min 2, mediana 100, max 120
summary(base_pt[, vars_cluster])

# 3a. PASO 5. Correlaciones entre las variables que entran

round(cor(base_pt[, vars_cluster]), 2) 
# esperado: vueltas~gap_primero -0.05 | vueltas~gap_previo 0.14 gap_primero~gap_previo 0.77

# 3a. PASO 6. Estandarizacion (z-scores)
X <- as.data.frame(base_pt[, vars_cluster])
rownames(X) <- base_pt$etiqueta

Z <- scale(X)

round(colMeans(Z), 10)    # esperado: 0 en las 3 variables
apply(Z, 2, sd)           # esperado: 1 en las 3 variables

# Diagnostico de valores extremos (Ward es sensible a ellos)
round(apply(abs(Z), 2, max), 2)   # esperado: 5.76 | 5.69 | 10.32
colSums(abs(Z) > 3)               # cantidad de observaciones con |z| > 3

# 3a. PASO 7. Matriz de distancias euclidianas
d_euc <- dist(Z, method = "euclidean")

# 3a. PASO 8. Clustering jerarquico, metodo Ward.D2
hc <- hclust(d_euc, method = "ward.D2")

tail(round(hc$height, 2), 8)
# esperado: 7.21 7.40 9.00 9.33 10.65 16.30 17.51 26.52

# 3a PASO 9. Dendrograma y corte en k candidatos

plot(hc, labels = FALSE, hang = -1,
     main = "Dendrograma - Ward.D2 sobre datos estandarizados (piloto-temporada)",
     xlab = "Observaciones (piloto-temporada)", sub = "")
rect.hclust(hc, k = 4, border = c("red", "blue", "darkgreen", "orange"))

# Altura de la fusion que reduce de k a k-1 clusters
k_cand <- 2:10
alturas <- rev(hc$height)[k_cand - 1]
plot(k_cand, alturas, type = "b", pch = 19,
     xlab = "k (numero de clusters)",
     ylab = "Altura de fusion (k -> k-1)",
     main = "Alturas de fusion del dendrograma")

# Tamaño de los clusters para cada k candidato
for (k in 2:6) {
  cat("\n--- k =", k, "---\n")
  print(table(cutree(hc, k = k)))
}
# esperado (tamaños, el orden de los numeros puede variar):
# k=2: 5 / 332             k=3: 5 / 120 / 212
# k=4: 5 / 120 / 177 / 35  k=5: 5 / 120 / 177 / 15 / 20
# k=6: 2 / 3 / 120 / 177 / 15 / 20

# Quienes forman el cluster mas chico en k = 2
cl2 <- cutree(hc, k = 2)
chico <- as.integer(names(which.min(table(cl2))))
base_pt %>%
  dplyr::filter(cl2 == chico) %>%
  select(etiqueta, n_sesiones, all_of(vars_cluster))

# 3a. PASO 10. Guardar objetos para los incisos siguientes
write_csv(base_pt, "base_piloto_temporada.csv")
saveRDS(list(base_pt = base_pt, Z = Z, hc = hc, vars = vars_cluster),
        "etapa3_objetos.rds")


#FIN Etapa 3a, 3b
# ******************************************************************************

# INICIO Etapa 3c

# 3c. PASO 0. Paquetes, carpeta y PARAMETROS (lo unico que hay que editar)

install.packages("cluster")   
library(dplyr)
library(readr)
library(cluster)

# --ruta carpeta raiz del proyecto--
setwd("/Users/facu/Desktop/UCC.nosync/2026/3. SecondSemester/Estadistica Multivariada/4.Proyecto Ingredor.nosync")        

k_elegido    <- 4                    # -- k elegido en b) --
excluir      <- c()                  # -- EDITAR si en b) sacaron observacion -- 

min_sesiones <- 0                    # -- EDITAR si en b) fijaste un minimo de sesiones --
vars_cluster <- c("vueltas", "gap_primero_s", "gap_previo_s")


# 3c. PASO 1. Reconstruir la base y el jerarquico EXACTAMENTE como en b)
# Si se excluyeron observaciones, hay que RE-ESTANDARIZAR: la media y el
# desvio cambian al sacar filas, y con ellos las distancias.

base_pt <- read_csv("base_piloto_temporada.csv", show_col_types = FALSE) %>%
  dplyr::filter(n_sesiones >= min_sesiones, !(etiqueta %in% excluir))

nrow(base_pt)

X <- as.data.frame(base_pt[, vars_cluster])
rownames(X) <- base_pt$etiqueta
Z <- scale(X)

d_euc <- dist(Z, method = "euclidean")
hc    <- hclust(d_euc, method = "ward.D2")
hier  <- cutree(hc, k = k_elegido)       # particion jerarquica con el k elegido

table(hier)


# 3c. PASO 2. K-means con el k elegido
# nstart = 50: k-means arranca de centros al azar y puede quedar en un
# optimo local; probamos 50 arranques y se queda con el de menor
# suma de cuadrados intra-cluster. set.seed() hace el resultado reproducible.

set.seed(123)
km <- kmeans(Z, centers = k_elegido, nstart = 50, iter.max = 100)

km$size                                  # tamaño de cada cluster
round(km$betweenss / km$totss * 100, 1)  # % de variabilidad explicada por la particion
round(km$centers, 2)                     # centros, en unidades estandarizadas


# 3c. PASO 3. Tabla cruzada jerarquico vs k-means
# Los numeros de cluster son etiquetas arbitrarias: "1" del jerarquico
# no tiene por que ser "1" de k-means. Lo que importa es como se reparten.

tabla_cruzada <- table(Jerarquico = hier, Kmeans = km$cluster)
addmargins(tabla_cruzada)

# Medidas de parecido entre las dos particiones
# (a) Pureza: proporcion de observaciones que caen en el cluster k-means
#     dominante de su cluster jerarquico
pureza <- sum(apply(tabla_cruzada, 1, max)) / sum(tabla_cruzada)

# (b) Indice de Rand ajustado (ARI): 1 = particiones identicas,
#     ~0 = parecido esperable por azar. No depende de como se numeren.
ari <- function(a, b) {
  tab  <- table(a, b)
  n    <- sum(tab)
  s_ij <- sum(choose(tab, 2))
  s_a  <- sum(choose(rowSums(tab), 2))
  s_b  <- sum(choose(colSums(tab), 2))
  esp  <- s_a * s_b / choose(n, 2)
  (s_ij - esp) / (0.5 * (s_a + s_b) - esp)
}

cat("\nPureza (jerarquico -> k-means):", round(pureza, 3), "\n")
cat("Indice de Rand ajustado (ARI):  ", round(ari(hier, km$cluster), 3), "\n")


# 3.c PASO 4. Silhouette del k-means
# s(i) = (b - a) / max(a, b), con a = distancia media de i a los demas de
# su cluster y b = distancia media al cluster vecino mas cercano.
# Va de -1 a 1: cerca de 1 = bien ubicada; ~0 = en la frontera;
# negativa = esta mas cerca de otro cluster que del propio.
# Se calcula sobre la MISMA matriz de distancias (d_euc).

sil_km <- silhouette(km$cluster, d_euc)

sil_km_df <- data.frame(
  etiqueta  = base_pt$etiqueta,
  cluster   = sil_km[, "cluster"],
  vecino    = sil_km[, "neighbor"],
  sil_width = sil_km[, "sil_width"]
)

# Silhouette promedio global
cat("\nSilhouette promedio (k-means):", round(mean(sil_km_df$sil_width), 3), "\n")

# Silhouette promedio por cluster y conteo de negativas
resumen_sil_km <- sil_km_df %>%
  group_by(cluster) %>%
  summarise(
    n           = n(),
    sil_prom    = round(mean(sil_width), 3),
    n_negativas = sum(sil_width < 0),
    pct_neg     = round(100 * mean(sil_width < 0), 1),
    .groups = "drop"
  )
print(resumen_sil_km)

cat("\nTotal de observaciones con silhouette negativa:",
    sum(sil_km_df$sil_width < 0),
    "de", nrow(sil_km_df),
    "(", round(100 * mean(sil_km_df$sil_width < 0), 1), "%)\n")

# Cuales son las observaciones con silhouette negativa
negativas_km <- sil_km_df %>%
  dplyr::filter(sil_width < 0) %>%
  arrange(sil_width)
print(dplyr::as_tibble(negativas_km), n = Inf)


# 3c. PASO 5. Grafico de silhouette (k-means)
plot(sil_km,
     main = paste0("Silhouette - K-means (k = ", k_elegido, ")"),
     col = 2:(k_elegido + 1), border = NA)


# 3c. PASO 6. Silhouette del jerarquico (para comparar las dos particiones)

sil_hc <- silhouette(hier, d_euc)

cat("\nSilhouette promedio (jerarquico):", round(mean(sil_hc[, "sil_width"]), 3), "\n")

data.frame(cluster = sil_hc[, "cluster"], sil_width = sil_hc[, "sil_width"]) %>%
  group_by(cluster) %>%
  summarise(n = n(),
            sil_prom = round(mean(sil_width), 3),
            n_negativas = sum(sil_width < 0),
            .groups = "drop") %>%
  print()

plot(sil_hc,
     main = paste0("Silhouette - Jerarquico Ward.D2 (k = ", k_elegido, ")"),
     col = 2:(k_elegido + 1), border = NA)


# 3c. PASO 7. Guardar asignaciones para los incisos d) y e)

base_pt$cl_hier <- hier
base_pt$cl_km   <- km$cluster
base_pt$sil_km  <- sil_km_df$sil_width

write_csv(base_pt, "base_pt_clusters.csv")
saveRDS(list(base_pt = base_pt, Z = Z, hc = hc, km = km, d_euc = d_euc,
             k = k_elegido, vars = vars_cluster),
        "etapa3_objetos_c.rds")

# FIN Etapa 3c
# ******************************************************************************

# INICIO Etapa 3d

# 3d. PASO 0. Paquetes y carpeta de trabajo
library(dplyr)
library(readr)

setwd("/Users/facu/Desktop/UCC.nosync/2026/3. SecondSemester/Estadistica Multivariada/4.Proyecto Ingredor.nosync")

# 3d. PASO 1. Datos: las 8 variables que sobrevivieron al AFE
# (se descartan top_speed y puntos por MSA bajo, igual que en el AFE)

X_num <- read_csv("X_num_var_numericas.csv", show_col_types = FALSE)

vars_afe <- c("suelo_c", "humedad_pct", "aire_c",
              "vuelta_mejor", "vueltas",
              "mejor_vuelta_s", "gap_primero_s", "gap_previo_s")

Xv <- as.data.frame(X_num[, vars_afe])
dim(Xv)    # esperado: 26467 filas, 8 columnas


# 3d. PASO 2. Matriz de correlaciones entre variables
R <- cor(Xv, method = "pearson")
round(R, 2)
# esperado (valores clave): suelo_c~aire_c 0.84 | vuelta_mejor~vueltas 0.84 |
# gap_primero_s~gap_previo_s 0.61 | suelo_c~humedad_pct -0.52 |
# aire_c~humedad_pct -0.43 | mejor_vuelta_s con el resto: |r| <= 0.25

# ---------------------------------------------------------------------
# 3d. PASO 3. Distancia entre variables: d = 1 - |r|
# Dos variables muy correlacionadas (positiva O negativamente) aportan
# la misma informacion, por eso se usa el valor absoluto: d = 0 si
# |r| = 1 (redundantes) y d = 1 si r = 0 (independientes).

D_var <- as.dist(1 - abs(R))


# 3d. PASO 4. Clustering jerarquico de variables (enlace promedio)
hc_var <- hclust(D_var, method = "average")

round(hc_var$height, 3)
# esperado: 0.158 0.162 0.388 0.523 0.783 0.885 0.928

# Correlacion cofenetica: cuanto mas cerca de 1, mejor resume el
# dendrograma las distancias originales
round(cor(D_var, cophenetic(hc_var)), 3)


# 3d. PASO 5. Dendrograma de variables
plot(hc_var, hang = -1,
     main = "Dendrograma de variables (distancia = 1 - |r|, enlace promedio)",
     xlab = "", sub = "", ylab = "Distancia (1 - |r|)")
rect.hclust(hc_var, k = 4, border = c("red", "blue", "darkgreen", "orange"))
abline(h = 0.6, lty = 2, col = "grey40")   # linea de corte de referencia


# 3d. PASO 6. Tabla 3d: bloques de variables redundantes (k = 4)
bloque <- cutree(hc_var, k = 4)

r_intra <- sapply(sort(unique(bloque)), function(b) {
  v <- names(bloque)[bloque == b]
  if (length(v) < 2) return(NA_real_)
  m <- abs(R[v, v])
  mean(m[upper.tri(m)])
})

tabla_3d <- data.frame(Variable = names(bloque), Bloque = as.integer(bloque)) %>%
  group_by(Bloque) %>%
  summarise(Variables = paste(Variable, collapse = ", "),
            n_variables = n(),
            .groups = "drop") %>%
  mutate(r_abs_prom_intra = round(r_intra, 2))

print(as.data.frame(tabla_3d))
# esperado:
#   {suelo_c, humedad_pct, aire_c}        r abs prom ~ 0.60
#   {vuelta_mejor, vueltas}               r ~ 0.84
#   {mejor_vuelta_s}                      (sola, NA)
#   {gap_primero_s, gap_previo_s}         r ~ 0.61

write_csv(tabla_3d, "tabla_3d_bloques_variables.csv")


# 3d. PASO 7. Robustez: ¿los bloques cambian con otra correlacion u otro enlace?
for (tipo_cor in c("pearson", "spearman")) {
  Rc <- cor(Xv, method = tipo_cor)
  Dc <- as.dist(1 - abs(Rc))
  for (met in c("average", "complete", "ward.D2")) {
    cl <- cutree(hclust(Dc, method = met), k = 4)
    bloques <- sapply(sort(unique(cl)),
                      function(b) paste(names(cl)[cl == b], collapse = "+"))
    cat("\n", tipo_cor, "-", met, ":\n   ",
        paste(sort(bloques), collapse = "  |  "), "\n")
  }
}

# FIN Etapa 3d
# ******************************************************************************

# INICIO Etapa 3e

# 3e. PASO 0. Paquetes, carpeta y PARAMETROS
library(dplyr)
library(readr)

setwd("/Users/facu/Desktop/UCC.nosync/2026/3. SecondSemester/Estadistica Multivariada/4.Proyecto Ingredor.nosync")

particion    <- "cl_km"            # >>> EDITAR: "cl_km" o "cl_hier" (la que elijas perfilar)
var_contexto <- "fabricante_agr"   # >>> EDITAR: "fabricante_agr" o "year"
umbral_z     <- 0.5                # |media z| >= 0.5 se considera rasgo distintivo
vars_cluster <- c("vueltas", "gap_primero_s", "gap_previo_s")

# ---------------------------------------------------------------------
# 3e. PASO 1. Leer la base con las asignaciones de cluster (generada en c)
# ---------------------------------------------------------------------
base_pt <- read_csv("base_pt_clusters.csv", show_col_types = FALSE)
names(base_pt)
base_pt$cluster <- factor(base_pt[[particion]])
table(base_pt$cluster)

# ---------------------------------------------------------------------
# 3e. PASO 2. Recuperar el fabricante de cada piloto-temporada
# La unidad de analisis es piloto-temporada, asi que el fabricante se
# resume con la MODA (el mas frecuente) entre sus sesiones validas.
# ---------------------------------------------------------------------
clasif <- read_csv("clasif_limpio.csv", show_col_types = FALSE)

moda <- function(x) names(sort(table(x), decreasing = TRUE))[1]

fab <- clasif %>%
  dplyr::filter(if_all(all_of(c(vars_cluster, "mejor_vuelta_s")), ~ !is.na(.x))) %>%
  group_by(year, rider_id) %>%
  summarise(n_fab    = n_distinct(fabricante),
            fab_moda = moda(fabricante),
            .groups = "drop")

base_pt <- base_pt %>% left_join(fab, by = c("year", "rider_id"))

sum(is.na(base_pt$fab_moda))   # esperado: 0
table(base_pt$n_fab)           # esperado (base completa): 332 con 1 fabricante, 5 con 2
table(base_pt$fab_moda)

# Categorias con pocas observaciones: YAHAMA es un error de tipeo de Yamaha;
# "Yamaha Forward" y "ART" tienen muy pocos casos y se agrupan en "Otros"
# (con frecuencias esperadas < 5 el test chi-cuadrado pierde validez).
# >>> Se puede modificar este criterio si se prefiere otro agrupamiento <<<
base_pt <- base_pt %>%
  mutate(fabricante_agr = case_when(
    fab_moda == "YAHAMA"                      ~ "Yamaha",
    fab_moda %in% c("Yamaha Forward", "ART")  ~ "Otros",
    TRUE                                      ~ fab_moda))

table(base_pt$fabricante_agr)
# esperado (base completa): Ducati 99, Honda 71, Yamaha 57, KTM 45,
#                           Aprilia 36, Suzuki 23, Otros 6

# ---------------------------------------------------------------------
# 3e. PASO 3. Perfil numerico: n, %, medias en unidades originales
# ---------------------------------------------------------------------
perfil_medias <- base_pt %>%
  group_by(cluster) %>%
  summarise(n = n(),
            pct = round(100 * n() / nrow(base_pt), 1),
            across(all_of(vars_cluster), ~ round(mean(.x), 2)),
            n_sesiones_mediana = median(n_sesiones),
            .groups = "drop")
print(as.data.frame(perfil_medias))

# Medias globales, como referencia
round(colMeans(base_pt[, vars_cluster]), 2)

# ---------------------------------------------------------------------
# 3e. PASO 4. Perfil estandarizado: cuanto se aleja cada cluster de la media
# global, medido en desvios estandar (z). Es lo que define los
# "rasgos distintivos": comparable entre variables de distinta escala.
# ATENCION: en gap_primero_s y gap_previo_s, un valor ALTO significa
# MAS segundos de diferencia, es decir PEOR posicion relativa.
# ---------------------------------------------------------------------
Zp <- scale(base_pt[, vars_cluster])

perfil_z <- as.data.frame(Zp) %>%
  mutate(cluster = base_pt$cluster) %>%
  group_by(cluster) %>%
  summarise(across(everything(), ~ round(mean(.x), 2)), .groups = "drop")
print(as.data.frame(perfil_z))

describir <- function(z) {
  alto <- names(z)[z >=  umbral_z]
  bajo <- names(z)[z <= -umbral_z]
  paste0("ALTO: ", paste(alto, collapse = ", "),
         " | BAJO: ", paste(bajo, collapse = ", "))
}
perfil_z$rasgos_auto <- apply(perfil_z[, vars_cluster], 1, describir)
print(perfil_z[, c("cluster", "rasgos_auto")])

# ---------------------------------------------------------------------
# 3e. PASO 5. Datos de contexto por cluster: fabricantes y años dominantes
# ---------------------------------------------------------------------
top_fab <- base_pt %>%
  count(cluster, fabricante_agr) %>%
  group_by(cluster) %>%
  mutate(pct = round(100 * n / sum(n))) %>%
  slice_max(n, n = 2, with_ties = FALSE) %>%
  summarise(fabricantes_top = paste0(fabricante_agr, " (", pct, "%)", collapse = ", "),
            .groups = "drop")

anios <- base_pt %>%
  group_by(cluster) %>%
  summarise(anio_mediano = median(year), anio_min = min(year), anio_max = max(year),
            .groups = "drop")

print(as.data.frame(top_fab))
print(as.data.frame(anios))

# ---------------------------------------------------------------------
# 3e. PASO 6. Observaciones mas tipicas de cada cluster
# (las 3 mas cercanas al centro del cluster en el espacio estandarizado)
# Sirven para ponerle nombre y formular hipotesis con casos concretos.
# ---------------------------------------------------------------------
Zdf <- as.data.frame(Zp)
Zdf$cluster <- base_pt$cluster
centros <- aggregate(. ~ cluster, data = Zdf, FUN = mean)

base_pt$dist_centro <- sapply(seq_len(nrow(Zdf)), function(i) {
  c0 <- as.numeric(centros[centros$cluster == Zdf$cluster[i], vars_cluster])
  sqrt(sum((as.numeric(Zdf[i, vars_cluster]) - c0)^2))
})

tipicos <- base_pt %>%
  group_by(cluster) %>%
  slice_min(dist_centro, n = 3, with_ties = FALSE) %>%
  summarise(casos_tipicos = paste(etiqueta, collapse = "; "), .groups = "drop")
print(as.data.frame(tipicos))

# ---------------------------------------------------------------------
# 3e. PASO 7. TABLA 3e-1: perfil de cada cluster (para completar a mano
# las columnas "nombre" y "hipotesis")
# ---------------------------------------------------------------------
tabla_3e1 <- perfil_medias %>%
  left_join(perfil_z[, c("cluster", "rasgos_auto")], by = "cluster") %>%
  left_join(top_fab, by = "cluster") %>%
  left_join(anios, by = "cluster") %>%
  left_join(tipicos, by = "cluster") %>%
  mutate(nombre = "", hipotesis = "")
print(as.data.frame(tabla_3e1))
write_csv(tabla_3e1, "tabla_3e1_perfiles.csv")

# ---------------------------------------------------------------------
# 3e. PASO 8. Cruce de los clusters con la variable de contexto
# ---------------------------------------------------------------------
contexto <- factor(base_pt[[var_contexto]])
tab <- table(Cluster = base_pt$cluster, Contexto = contexto)

addmargins(tab)                              # frecuencias absolutas
round(100 * prop.table(tab, 1), 1)           # % por FILA: como se reparte cada cluster
#   entre los valores del contexto
round(100 * prop.table(tab, 2), 1)           # % por COLUMNA: de que clusters esta
#   compuesto cada valor del contexto

# Test chi-cuadrado de independencia. Con celdas de pocas observaciones
# se usa p-valor por simulacion (Monte Carlo), que es valido igual.
set.seed(123)
chi_sim  <- chisq.test(tab, simulate.p.value = TRUE, B = 10000)
chi_asin <- suppressWarnings(chisq.test(tab))
chi_sim

# Frecuencias esperadas bajo independencia: % de celdas con valor < 5
round(100 * mean(chi_asin$expected < 5), 1)

# Residuos estandarizados: |valor| > 2 indica una celda donde el cluster
# aparece MAS (positivo) o MENOS (negativo) de lo esperado por azar
round(chi_asin$stdres, 2)

# V de Cramer: intensidad de la asociacion (0 = ninguna, 1 = total)
V <- sqrt(as.numeric(chi_asin$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
cat("V de Cramer:", round(V, 3), "\n")

# ---------------------------------------------------------------------
# 3e. PASO 9. Graficos del cruce
# ---------------------------------------------------------------------
# (a) Barras apiladas: composicion de cada cluster segun el contexto
colores <- hcl.colors(ncol(tab), "Dark 3")
op <- par(mar = c(5, 4, 4, 9), xpd = TRUE)
barplot(t(100 * prop.table(tab, 1)), col = colores,
        xlab = "Cluster", ylab = "% dentro del cluster",
        main = paste("Clusters segun", var_contexto))
legend("topright", inset = c(-0.28, 0), legend = colnames(tab),
       fill = colores, bty = "n", title = var_contexto)
par(op)

# (b) Mosaico con residuos: color = desvio respecto de la independencia
mosaicplot(tab, shade = TRUE, las = 2,
           main = paste("Mosaico: cluster vs", var_contexto))

# ---------------------------------------------------------------------
# PASO 10. Guardar
# ---------------------------------------------------------------------
write_csv(base_pt, "base_pt_perfilado.csv")


#GRAFICO MEJORADA PARA ETAPA 3e
library(ggplot2)

# Gráfico de barras 100% apiladas por clúster según fabricante
ggplot(base_pt, aes(x = factor(cl_km), fill = fabricante_agr)) +
  geom_bar(position = "fill", color = "black", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Composición de cada Clúster según Fabricante",
    subtitle = "Distribución porcentual por fabricante dentro del clúster",
    x = "Clúster (K-means)",
    y = "Porcentaje dentro del Clúster (%)",
    fill = "Fabricante"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    panel.grid.major.x = element_blank()
  )

# Guardar la imagen en alta resolución
ggsave("figura_3e_clusters_fabricante.png", width = 9, height = 5.5, dpi = 300)

# FIN Etapa 3e
# ******************************************************************************

# ==============================================================================
# ETAPA 4 : ANALISIS DE CORRESPONDENCIA MULTIPLE (ACM)
# ==============================================================================

# INICIO Etapa 4a
# Objetivo del script:
#   1) Reconstruir la base a nivel piloto-sesion, alineada fila a fila con X_num
#   2) Explorar las categorias "crudas" de las candidatas
#   3) Recodificar / agrupar categorias raras
#   4) Armar X_cat final y la tabla (variable, categoria, n, %) para el informe
# ==============================================================================

# ---- 4a.0. Paquetes ------------------------------------------------------
install.packages("dplyr")   # solo si no lo tenes instalado
library(dplyr)


# ---- 4a.1. Carga de datos ------------------------------------------------
# Ajusta la ruta a la carpeta donde estan tus CSV
# setwd("C:/ruta/a/tu/carpeta")

clasif    <- read.csv("clasif_limpio.csv",          stringsAsFactors = FALSE)
sesiones  <- read.csv("sesiones_limpio.csv",        stringsAsFactors = FALSE)
standings <- read.csv("standings_limpio.csv",       stringsAsFactors = FALSE)
X_num     <- read.csv("X_num_var_numericas.csv",    stringsAsFactors = FALSE)

# ---- 4a.2. Reconstruccion de la base piloto-sesion -----------------------
# Clasificacion es la tabla "central" (una fila por piloto en cada sesion).
# Se le pegan: clima/pista (desde Sesiones, por session_id) y puntos
# (desde Standings, por temporada + piloto).
base <- clasif %>%
  left_join(sesiones  %>% select(session_id, pista, clima,
                                 aire_c, humedad_pct, suelo_c),
            by = "session_id") %>%
  left_join(standings %>% select(year, rider_id, puntos),
            by = c("year", "rider_id"))

# Mismo criterio que en X_num: casos completos en las 10 variables numericas
vars_num <- names(X_num)
base_completa <- base %>%
  dplyr::filter(if_all(all_of(vars_num), ~ !is.na(.x)))

# ---- 4a.2.1 Control de alineacion con X_num (MUY importante) --------------
# Si esto pasa, cada fila de base_completa es la misma fila de X_num.
stopifnot(nrow(base_completa) == nrow(X_num))
for (v in vars_num) {
  stopifnot(isTRUE(all.equal(as.numeric(base_completa[[v]]),
                             as.numeric(X_num[[v]]),
                             check.attributes = FALSE)))
}
cat("OK: base_completa tiene", nrow(base_completa),
    "filas y esta alineada con X_num\n")

# ---- 4a.3. Exploracion de categorias CRUDAS --------------------------------
crudas <- c("fabricante", "type", "pista", "clima", "year", "circuito")
for (v in crudas) {
  cat("\n---", v, "---\n")
  print(cbind(n = table(base_completa[[v]], useNA = "ifany"),
              pct = round(100 * prop.table(table(base_completa[[v]],
                                                 useNA = "ifany")), 2)))
}

# ---- 4a. Recodificacion / agrupacion ------------------------------------
X_cat <- base_completa %>%
  transmute(
    # Fabricante: mismo fabricante con distinta etiqueta -> se unifica
    fabricante = factor(case_when(
      fabricante == "Yamaha Forward" ~ "Yamaha",   # Forward Racing usaba Yamaha
      fabricante == "ART"            ~ "Aprilia",  # Aprilia Racing Technology
      TRUE                           ~ fabricante)),
    
    # Anio: 11 categorias bien balanceadas (6.7% - 10.7%), se mantiene
    anio = factor(year),
    
    # Tipo de sesion: se agrupan las 3 categorias minoritarias
    tipo_sesion = factor(case_when(
      type %in% c("P", "PR", "TTS") ~ "Otras",
      TRUE                          ~ type),
      levels = c("FP", "Q", "WUP", "Otras")),
    
    # Estado de pista: seca vs. con agua (mojada o mixta)
    estado_pista = factor(case_when(
      pista == "Dry" ~ "Seca",
      TRUE           ~ "Mojada_o_mixta"),
      levels = c("Seca", "Mojada_o_mixta")),
    
    # Clima: se unen todas las categorias de precipitacion
    clima = factor(case_when(
      clima == "Clear"         ~ "Despejado",
      clima == "Partly-Cloudy" ~ "Parc_nublado",
      clima == "Cloudy"        ~ "Nublado",
      clima %in% c("Light-Rain", "Raining",
                   "Heavy-Rain", "Snowing") ~ "Precipitacion"),
      levels = c("Despejado", "Parc_nublado", "Nublado", "Precipitacion"))
  )

# Variable de PERFIL DE RENDIMIENTO (cluster de la etapa 3)
# Se agrega cuando me pases como esta guardado. Ejemplos:
#   (a) si el clustering se hizo sobre estas mismas 26.467 filas y en el mismo orden:
#       X_cat$perfil <- factor(cluster_final, labels = paste0("Perfil", sort(unique(cluster_final))))
#   (b) si se hizo por piloto-temporada: hay que unir por (year, rider_id)
# if (exists("cluster_final")) { X_cat$perfil <- factor(paste0("Perfil", cluster_final)) }

# Variable ILUSTRATIVA propuesta (no activa): circuito
X_cat_sup <- data.frame(circuito = factor(base_completa$circuito))

# ---- 4a.5. Controles ------------------------------------------------------
stopifnot(!anyNA(X_cat))
str(X_cat)
summary(X_cat)

# ---- 4a.6. Tabla del inciso a): variable | categoria | n | % ---------------
tabla_X_cat <- bind_rows(lapply(names(X_cat), function(v) {
  t <- table(X_cat[[v]])
  data.frame(variable  = v,
             categoria = names(t),
             n         = as.integer(t),
             pct       = round(100 * as.integer(t) / sum(t), 1))
}))
tabla_X_cat$alerta <- ifelse(tabla_X_cat$pct < 5, "< 5%", "")

print(tabla_X_cat, row.names = FALSE)

# Resumen: cantidad de categorias por variable (influye en la inercia total)
print(tabla_X_cat %>% count(variable, name = "n_categorias"))

# ---- 4a.7. Guardado -------------------------------------------------------
write.csv(tabla_X_cat, "tabla_4a_X_cat_piloto_x_sesion.csv", row.names = FALSE)
saveRDS(X_cat,     "X_cat.rds")
saveRDS(X_cat_sup, "X_cat_sup.rds")

# FIN Etapa 4a
# *********************************************************************************
# INICIO Etapa 4b

# Objetivo del script:
#   1) Definir 2 pares de variables de X_cat
#   2) Para cada par: tabla de contingencia, test chi-cuadrado, V de Cramer
#      y residuos estandarizados (que categorias se atraen / se repelen)
#   3) Matriz de V de Cramer entre TODAS las variables (vision global)


# ---- 4b.0. Datos ----------------------------------------------------------
# Usa el X_cat creado en la etapa 4a (si ya lo tenes en memoria no hace falta cargarlo)
if (!exists("X_cat")) X_cat <- readRDS("X_cat.rds")
str(X_cat)
dim(X_cat)   # esperado: 26467 filas x 5 variables

# ---- 4b.1. Funcion que analiza un par de variables ------------------------
analisis_par <- function(data, v1, v2) {
  
  tab  <- table(data[[v1]], data[[v2]], dnn = c(v1, v2))
  test <- suppressWarnings(chisq.test(tab))          # chi-cuadrado de independencia
  n    <- sum(tab)
  k    <- min(nrow(tab), ncol(tab)) - 1
  V    <- sqrt(as.numeric(test$statistic) / (n * k)) # V de Cramer (0 a 1)
  
  cat("\n==========================================================\n")
  cat("PAR:", v1, "x", v2, "\n")
  cat("==========================================================\n")
  
  cat("\n--- Tabla de contingencia (frecuencias) ---\n")
  print(addmargins(tab))
  
  cat("\n--- Porcentajes por fila (como se reparte", v2, "dentro de cada", v1, ") ---\n")
  print(round(100 * prop.table(tab, 1), 1))
  
  cat("\n--- Test chi-cuadrado de independencia ---\n")
  cat("Chi2 =", round(as.numeric(test$statistic), 1),
      "| gl =", test$parameter,
      "| p-valor =", format.pval(test$p.value, digits = 3, eps = 1e-16), "\n")
  cat("V de Cramer =", round(V, 3), "\n")
  cat("Celdas con frecuencia esperada < 5:",
      sum(test$expected < 5), "de", length(tab), "\n")
  
  cat("\n--- Residuos estandarizados (|valor| > 3: celda muy sobre/sub-representada) ---\n")
  print(round(test$stdres, 1))
  
  invisible(list(tabla = tab, test = test, V = V))
}

# ---- 4b.2. PAR 1: estado_pista x clima -------------------------------------
# Ambas describen las condiciones de la sesion (H3)
par1 <- analisis_par(X_cat, "estado_pista", "clima")

# ---- 4b.3. PAR 2: fabricante x anio ----------------------------------------
# Relaciona fabricantes con temporada (H3)
par2 <- analisis_par(X_cat, "fabricante", "anio")

# ---- 4b.4. Matriz de V de Cramer entre todas las variables ------------------
# Con 26.467 filas (no independientes entre si) casi cualquier p-valor da
# "significativo"; el V de Cramer mide la MAGNITUD de la asociacion.
cramer_v <- function(x, y) {
  tab  <- table(x, y)
  chi  <- suppressWarnings(chisq.test(tab))$statistic
  sqrt(as.numeric(chi) / (sum(tab) * (min(dim(tab)) - 1)))
}
vars  <- names(X_cat)
V_mat <- matrix(NA_real_, length(vars), length(vars),
                dimnames = list(vars, vars))
for (i in seq_along(vars)) {
  for (j in seq_along(vars)) {
    V_mat[i, j] <- if (i == j) 1 else cramer_v(X_cat[[vars[i]]], X_cat[[vars[j]]])
  }
}
cat("\n--- Matriz de V de Cramer ---\n")
print(round(V_mat, 3))

# ---- 4b.5. Guardado ----------------------------------------------------------
write.csv(as.data.frame.matrix(addmargins(par1$tabla)), "tabla_4b_par1_contingencia.csv")
write.csv(as.data.frame.matrix(addmargins(par2$tabla)), "tabla_4b_par2_contingencia.csv")
write.csv(round(V_mat, 3), "tabla_4b_V_Cramer.csv")

# FIN Etapa 4b
# *********************************************************************************
# INICIO Etapa 4c

# Objetivo del script:
#   1) Ajustar el ACM sobre X_cat (variables activas)
#   2) Tabla de autovalores y porcentajes de inercia (brutos y ajustados)
#   3) Graficos de sedimentacion (scree plot)
#   4) Criterios para decidir cuantas dimensiones retener
# ==============================================================================

# ---- 4c.0. Paquetes ------------------------------------------------------
install.packages(c("FactoMineR", "factoextra"))   # solo si no los tenes instalados
library(FactoMineR)
library(factoextra)   # carga tambien ggplot2

# ---- 4c.1. Datos -----------------------------------------------------------
if (!exists("X_cat")) X_cat <- readRDS("X_cat.rds")
stopifnot(all(sapply(X_cat, is.factor)))   # el ACM necesita factores
str(X_cat)

p <- ncol(X_cat)                          # cantidad de variables activas
K <- sum(sapply(X_cat, nlevels))          # cantidad total de categorias
n_dim_max <- K - p                        # nro. maximo de dimensiones del ACM
cat("p =", p, "| K =", K, "| dimensiones maximas =", n_dim_max, "\n")
cat("Inercia total =", K / p - 1, "\n")
cat("Autovalor medio (umbral = 1/p) =", round(1 / p, 4), "\n")

# ---- 4c.2. Ajuste del ACM ---------------------------------------------------
# ncp = n_dim_max: se calculan todas las dimensiones; luego decidimos cuantas retener
res.mca <- MCA(X_cat, ncp = n_dim_max, graph = FALSE)

# ---- 4c.3. Autovalores y porcentajes de inercia -------------------------------
eig <- as.data.frame(res.mca$eig)
colnames(eig) <- c("autovalor", "pct_inercia", "pct_acumulado")

lam    <- eig$autovalor
umbral <- 1 / p

# Correccion de Benzecri con el denominador de Greenacre (inercia ajustada):
#   solo las dimensiones con autovalor > 1/p tienen inercia "ajustada"
lam_adj <- ifelse(lam > umbral, (p / (p - 1))^2 * (lam - umbral)^2, 0)
den_adj <- (p / (p - 1)) * (sum(lam^2) - (K - p) / p^2)

tabla_eig <- data.frame(
  Dimension          = rownames(eig),
  autovalor          = round(lam, 4),
  pct_inercia        = round(eig$pct_inercia, 2),
  pct_acumulado      = round(eig$pct_acumulado, 2),
  mayor_que_media    = lam > umbral,
  pct_inercia_ajust  = round(100 * lam_adj / den_adj, 2),
  pct_ajust_acum     = round(cumsum(100 * lam_adj / den_adj), 2)
)
print(tabla_eig, row.names = FALSE)

# Cuantas dimensiones superan el autovalor medio
cat("\nDimensiones con autovalor > 1/p:", sum(lam > umbral), "\n")

# ---- 4c.4. Graficos de sedimentacion --------------------------------------------
# (i) Scree plot con % de inercia. La linea roja es el % que corresponde al autovalor medio
umbral_pct <- 100 * umbral / (K / p - 1)     # = 100 / (K - p)
g_scree_pct <- fviz_screeplot(res.mca, ncp = n_dim_max, addlabels = TRUE,
                              barfill = "steelblue", barcolor = "steelblue") +
  geom_hline(yintercept = umbral_pct, linetype = "dashed", color = "red") +
  labs(title = "Grafico de sedimentacion del ACM",
       x = "Dimensiones", y = "% de inercia explicada")
print(g_scree_pct)

# (ii) Scree plot con autovalores. La linea roja es el autovalor medio (1/p)
g_scree_eig <- fviz_screeplot(res.mca, choice = "eigenvalue", ncp = n_dim_max,
                              addlabels = TRUE, barfill = "steelblue",
                              barcolor = "steelblue") +
  geom_hline(yintercept = umbral, linetype = "dashed", color = "red") +
  labs(title = "Grafico de sedimentacion del ACM (autovalores)",
       x = "Dimensiones", y = "Autovalor")
print(g_scree_eig)

# ---- 4c.5. Guardado ------------------------------------------------------------------
write.csv(tabla_eig, "tabla_4c_autovalores.csv", row.names = FALSE)
ggsave("grafico_4c_sedimentacion_pct.png", g_scree_pct, width = 9, height = 5, dpi = 300)
ggsave("grafico_4c_sedimentacion_autovalores.png", g_scree_eig, width = 9, height = 5, dpi = 300)
saveRDS(res.mca, "res_mca.rds")

# FIN Etapa 4c
# *********************************************************************************
# INICIO Etapa 4d

# Objetivo del script:
#   1) Mapa de categorias (Dim 1 x Dim 2)
#   2) Contribuciones de las categorias a cada dimension
#   3) cos2 (calidad de representacion) de las categorias
#   4) Tabla resumen para responder:
#        - que categorias contribuyen mas a cada dimension
#        - que categorias estan mal representadas (cos2 bajo)

# ---- 4d.0. Paquetes y datos ------------------------------------------------
library(FactoMineR)
library(factoextra)
library(dplyr)

if (!exists("X_cat"))   X_cat   <- readRDS("X_cat.rds")
if (!exists("res.mca")) res.mca <- readRDS("res_mca.rds")   # ajuste de la etapa 4c

# >>> PARAMETRO: cantidad de dimensiones retenidas en el inciso c) <<<
ndim <- 2

# Umbrales de lectura (se pueden ajustar con fundamento)
K <- sum(sapply(X_cat, nlevels))
umbral_contrib <- 100 / K     # contribucion media esperada de una categoria (%)
umbral_cos2    <- 0.20        # por debajo: categoria pobremente representada
cat("Contribucion media esperada =", round(umbral_contrib, 2), "%\n")

# ---- 4d.1. Tabla base: una fila por categoria ---------------------------------
cats      <- rownames(res.mca$var$coord)
var_grupo <- factor(rep(names(X_cat), sapply(X_cat, nlevels)), levels = names(X_cat))
stopifnot(length(cats) == length(var_grupo))

coord   <- res.mca$var$coord[,   1:ndim, drop = FALSE]
contrib <- res.mca$var$contrib[, 1:ndim, drop = FALSE]
cos2    <- res.mca$var$cos2[,    1:ndim, drop = FALSE]
colnames(coord)   <- paste0("coord_Dim",   1:ndim)
colnames(contrib) <- paste0("contrib_Dim", 1:ndim)
colnames(cos2)    <- paste0("cos2_Dim",    1:ndim)

tab_cat <- data.frame(
  variable  = var_grupo,
  categoria = cats,
  n         = unlist(lapply(X_cat, function(x) as.integer(table(x)))),
  round(coord, 3),
  round(contrib, 2),
  round(cos2, 3),
  cos2_total = round(rowSums(cos2), 3),   # calidad en el subespacio retenido
  row.names = NULL
)
print(tab_cat, row.names = FALSE)

# ---- 4d.2. MAPA DE CATEGORIAS (Dim 1 x Dim 2) -----------------------------------
# (i) Coloreado por variable
g_mapa <- fviz_mca_var(res.mca, axes = c(1, 2), repel = TRUE,
                       col.var = var_grupo, legend.title = "Variable",
                       ggtheme = theme_minimal()) +
  labs(title = "ACM - Mapa de categorias (Dim 1 y Dim 2)")
print(g_mapa)

# (ii) Coloreado por cos2 (calidad de representacion en el plano 1-2)
g_mapa_cos2 <- fviz_mca_var(res.mca, axes = c(1, 2), repel = TRUE,
                            col.var = "cos2",
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                            ggtheme = theme_minimal()) +
  labs(title = "ACM - Mapa de categorias segun cos2 (Dim 1 y Dim 2)")
print(g_mapa_cos2)

# ---- 4d.3. CONTRIBUCIONES ---------------------------------------------------------
# Graficos: barras ordenadas; la linea roja es la contribucion media (100/K)
g_contrib <- list()
for (d in 1:ndim) {
  g_contrib[[d]] <- fviz_contrib(res.mca, choice = "var", axes = d, top = K,
                                 fill = "steelblue", color = "steelblue") +
    labs(title = paste("Contribucion de las categorias a la Dim", d))
  print(g_contrib[[d]])
}

# Tabla: categorias que superan la contribucion media, por dimension
for (d in 1:ndim) {
  col <- paste0("contrib_Dim", d)
  top <- tab_cat[tab_cat[[col]] > umbral_contrib, ]
  top <- top[order(-top[[col]]),
             c("variable", "categoria", "n", paste0("coord_Dim", d), col)]
  cat("\n--- Categorias con contribucion > media (", round(umbral_contrib, 2),
      "%) en la Dim", d, "---\n", sep = "")
  print(top, row.names = FALSE)
}

# ---- 4d.4. COS2 ---------------------------------------------------------------------
# Grafico: cos2 acumulado en las dimensiones retenidas
g_cos2 <- fviz_cos2(res.mca, choice = "var", axes = 1:ndim, top = K,
                    fill = "steelblue", color = "steelblue") +
  labs(title = paste("cos2 de las categorias (Dim 1 a", ndim, ")"))
print(g_cos2)

# Categorias mal representadas
mal_rep <- tab_cat[tab_cat$cos2_total < umbral_cos2,
                   c("variable", "categoria", "n", "cos2_total")]
mal_rep <- mal_rep[order(mal_rep$cos2_total), ]
cat("\n--- Categorias con cos2 total <", umbral_cos2,
    "(conviene no interpretar) ---\n")
print(mal_rep, row.names = FALSE)

# ---- 4d.5. Guardado -----------------------------------------------------------------------
write.csv(tab_cat, "tabla_4d_categorias.csv", row.names = FALSE)
ggsave("grafico_4d_mapa_categorias.png",      g_mapa,      width = 9, height = 7, dpi = 300)
ggsave("grafico_4d_mapa_categorias_cos2.png", g_mapa_cos2, width = 9, height = 7, dpi = 300)
for (d in 1:ndim) {
  ggsave(paste0("grafico_4d_contrib_dim", d, ".png"), g_contrib[[d]],
         width = 9, height = 5, dpi = 300)
}
ggsave("grafico_4d_cos2.png", g_cos2, width = 9, height = 5, dpi = 300)

# FIN Etapa 4d
# *********************************************************************************