# IMPLEMENTACIÓN DE QUERIES PARA REDES BAYESIANAS GAUSSIANAS
# ===========================================================

# Este script contiene ejemplos de implementación de las queries propuestas
# Debe ejecutarse después de ajustar cualquiera de los 3 DAGs

# FUNCIONES AUXILIARES
# ===================

# Función para calcular percentiles con manejo de NA
safe_quantile <- function(x, probs, na.rm = TRUE) {
  quantile(x, probs = probs, na.rm = na.rm)
}

# Función para ejecutar queries con manejo de errores
execute_query <- function(fitted_dag, event_expr, evidence_expr, n_samples = 10000) {
  tryCatch({
    cpquery(fitted_dag, 
            event = event_expr, 
            evidence = evidence_expr, 
            method = "lw", 
            n = n_samples)
  }, error = function(e) {
    cat("Error en query:", e$message, "\n")
    return(NA)
  })
}

# QUERIES CATEGORIA 1: SÍNDROME METABÓLICO Y CONTAMINANTES
# ========================================================

query_sindrome_contaminantes <- function(fitted_dag, data_clean) {
  cat("\n=== QUERIES SÍNDROME METABÓLICO Y CONTAMINANTES ===\n")
  
  # Percentiles de contaminantes
  pm25_p90 <- safe_quantile(data_clean$PM25, 0.9)
  no2_p90 <- safe_quantile(data_clean$NO2, 0.9)
  so2_p90 <- safe_quantile(data_clean$SO2, 0.9)
  
  # Query 1.1: PM2.5 y NO2 altos -> Síndrome metabólico
  q1 <- execute_query(fitted_dag,
    event = (SindromeMetabolico > 0.5),
    evidence = (PM25 > pm25_p90) & (NO2 > no2_p90)
  )
  cat("Q1.1 - P(Síndrome Metabólico | PM2.5 alto, NO2 alto):", round(q1, 3), "\n")
  
  # Query 1.2: Múltiples contaminantes -> Síndrome metabólico
  q2 <- execute_query(fitted_dag,
    event = (SindromeMetabolico > 0.5),
    evidence = (PM25 > pm25_p90) & (NO2 > no2_p90) & (SO2 > so2_p90)
  )
  cat("Q1.2 - P(Síndrome Metabólico | PM2.5, NO2, SO2 altos):", round(q2, 3), "\n")
}

# QUERIES CATEGORIA 2: DIFERENCIAS DEMOGRÁFICAS
# ==============================================

query_demograficos <- function(fitted_dag, data_clean) {
  cat("\n=== QUERIES DIFERENCIAS DEMOGRÁFICAS ===\n")
  
  glucosa_p75 <- safe_quantile(data_clean$Glucosa, 0.75)
  hdl_p25 <- safe_quantile(data_clean$ColesterolHDL, 0.25)
  
  # Query 2.1: Edad y glucosa
  q1_mayor <- execute_query(fitted_dag,
    event = (Glucosa > glucosa_p75),
    evidence = (Edad > 50)
  )
  q1_menor <- execute_query(fitted_dag,
    event = (Glucosa > glucosa_p75),
    evidence = (Edad <= 30)
  )
  cat("Q2.1a - P(Glucosa alta | Edad > 50):", round(q1_mayor, 3), "\n")
  cat("Q2.1b - P(Glucosa alta | Edad <= 30):", round(q1_menor, 3), "\n")
  
  # Query 2.2: Sexo y HDL
  q2_hombre <- execute_query(fitted_dag,
    event = (ColesterolHDL < 40),
    evidence = (Sexo == 1)
  )
  q2_mujer <- execute_query(fitted_dag,
    event = (ColesterolHDL < 50),
    evidence = (Sexo == 2)
  )
  cat("Q2.2a - P(HDL bajo | Hombre):", round(q2_hombre, 3), "\n")
  cat("Q2.2b - P(HDL bajo | Mujer):", round(q2_mujer, 3), "\n")
}

# QUERIES CATEGORIA 3: INFLAMACIÓN Y CASCADAS METABÓLICAS
# =======================================================

query_inflamacion <- function(fitted_dag, data_clean) {
  cat("\n=== QUERIES INFLAMACIÓN Y CASCADAS METABÓLICAS ===\n")
  
  pcr_p90 <- safe_quantile(data_clean$PCR, 0.9)
  trig_p75 <- safe_quantile(data_clean$Trigliceridos, 0.75)
  pm25_p90 <- safe_quantile(data_clean$PM25, 0.9)
  
  # Query 3.1: PCR alta -> Triglicéridos altos
  q1 <- execute_query(fitted_dag,
    event = (Trigliceridos > trig_p75),
    evidence = (PCR > pcr_p90)
  )
  cat("Q3.1 - P(Triglicéridos altos | PCR alta):", round(q1, 3), "\n")
  
  # Query 3.2: Cadena causal contaminante-inflamación-resistencia insulina
  if ("ResistenciaInsulina" %in% names(data_clean)) {
    ri_p75 <- safe_quantile(data_clean$ResistenciaInsulina, 0.75)
    q2 <- execute_query(fitted_dag,
      event = (ResistenciaInsulina > ri_p75),
      evidence = (PM25 > pm25_p90) & (PCR > pcr_p90)
    )
    cat("Q3.2 - P(Resistencia Insulina alta | PM2.5 alto, PCR alta):", round(q2, 3), "\n")
  }
}

# QUERIES CATEGORIA 4: FUNCIÓN RENAL
# ==================================

query_funcion_renal <- function(fitted_dag, data_clean) {
  cat("\n=== QUERIES FUNCIÓN RENAL ===\n")
  
  so2_p90 <- safe_quantile(data_clean$SO2, 0.9)
  creat_p75 <- safe_quantile(data_clean$Creatinina, 0.75)
  
  # Query 4.1: SO2 -> Creatinina
  q1 <- execute_query(fitted_dag,
    event = (Creatinina > creat_p75),
    evidence = (SO2 > so2_p90)
  )
  cat("Q4.1 - P(Creatinina alta | SO2 alto):", round(q1, 3), "\n")
  
  # Query 4.2: Síndrome cardiorrenal
  q2 <- execute_query(fitted_dag,
    event = (Creatinina > creat_p75) & (SindromeMetabolico > 0.5),
    evidence = (SO2 > so2_p90) & (PM25 > safe_quantile(data_clean$PM25, 0.9))
  )
  cat("Q4.2 - P(Creatinina alta & Síndrome Metabólico | Contaminantes altos):", round(q2, 3), "\n")
}

# QUERIES CATEGORIA 5: MICRONUTRIENTES
# ====================================

query_micronutrientes <- function(fitted_dag, data_clean) {
  cat("\n=== QUERIES MICRONUTRIENTES ===\n")
  
  vitd_p25 <- safe_quantile(data_clean$VitaminaD, 0.25)
  glucosa_p75 <- safe_quantile(data_clean$Glucosa, 0.75)
  
  # Query 5.1: Vitamina D baja -> Glucosa alta
  q1 <- execute_query(fitted_dag,
    event = (Glucosa > glucosa_p75),
    evidence = (VitaminaD < vitd_p25)
  )
  cat("Q5.1 - P(Glucosa alta | Vitamina D baja):", round(q1, 3), "\n")
  
  # Query 5.2: Albúmina -> Vitamina D
  if ("Albumina" %in% names(data_clean)) {
    albu_p25 <- safe_quantile(data_clean$Albumina, 0.25)
    q2 <- execute_query(fitted_dag,
      event = (VitaminaD < vitd_p25),
      evidence = (Albumina < albu_p25)
    )
    cat("Q5.2 - P(Vitamina D baja | Albúmina baja):", round(q2, 3), "\n")
  }
}

# QUERIES MULTIVARIADAS COMPLEJAS
# ===============================

query_multivariadas <- function(fitted_dag, data_clean) {
  cat("\n=== QUERIES MULTIVARIADAS COMPLEJAS ===\n")
  
  # Query 6.1: Perfil de riesgo integral en mujeres >45
  q1 <- execute_query(fitted_dag,
    event = (Glucosa > 100) & (Trigliceridos > 150) & (ColesterolHDL < 50),
    evidence = (Sexo == 2) & (Edad > 45) & (PM25 > safe_quantile(data_clean$PM25, 0.9))
  )
  cat("Q6.1 - P(Perfil de riesgo metabólico | Mujer >45 años, PM2.5 alto):", round(q1, 3), "\n")
  
  # Query 6.2: Síndrome inflamatorio sistémico
  pcr_p90 <- safe_quantile(data_clean$PCR, 0.9)
  ferr_p90 <- safe_quantile(data_clean$Ferritina, 0.9)
  hemo_p25 <- safe_quantile(data_clean$Hemoglobina, 0.25)
  
  q2 <- execute_query(fitted_dag,
    event = (PCR > pcr_p90) & (Ferritina > ferr_p90) & (Hemoglobina < hemo_p25),
    evidence = (PM25 > safe_quantile(data_clean$PM25, 0.9)) & (NO2 > safe_quantile(data_clean$NO2, 0.9))
  )
  cat("Q6.2 - P(Síndrome inflamatorio | Contaminantes altos):", round(q2, 3), "\n")
}

# FUNCIÓN PRINCIPAL PARA EJECUTAR TODAS LAS QUERIES
# =================================================

ejecutar_todas_queries <- function(fitted_dag, data_clean) {
  cat("EJECUCIÓN DE QUERIES PARA RED BAYESIANA GAUSSIANA")
  cat("\n==================================================\n")
  
  query_sindrome_contaminantes(fitted_dag, data_clean)
  query_demograficos(fitted_dag, data_clean)
  query_inflamacion(fitted_dag, data_clean)
  query_funcion_renal(fitted_dag, data_clean)
  query_micronutrientes(fitted_dag, data_clean)
  query_multivariadas(fitted_dag, data_clean)
  
  cat("\n=== RESUMEN COMPLETADO ===\n")
}

# EJEMPLO DE USO:
# ===============
# # Después de ajustar dag3_fitted y tener data_clean:
# ejecutar_todas_queries(dag3_fitted, data_clean)

# QUERIES DE INTERVENCIÓN (SIMULACIÓN DE POLÍTICAS)
# =================================================

query_intervencion <- function(fitted_dag, data_clean) {
  cat("\n=== QUERIES DE INTERVENCIÓN (SIMULACIÓN) ===\n")
  
  # Simular reducción de PM2.5 del percentil 90 al 50
  pm25_p90 <- safe_quantile(data_clean$PM25, 0.9)
  pm25_p50 <- safe_quantile(data_clean$PM25, 0.5)
  
  # Probabilidad actual con PM2.5 alto
  prob_actual <- execute_query(fitted_dag,
    event = (SindromeMetabolico > 0.5),
    evidence = (PM25 > pm25_p90)
  )
  
  # Probabilidad con PM2.5 reducido
  prob_reducido <- execute_query(fitted_dag,
    event = (SindromeMetabolico > 0.5),
    evidence = (PM25 > pm25_p50) & (PM25 <= pm25_p90)
  )
  
  reduccion_riesgo <- prob_actual - prob_reducido
  
  cat("Intervención - Reducción PM2.5:\n")
  cat("  Riesgo actual (PM2.5 alto):", round(prob_actual, 3), "\n")
  cat("  Riesgo con reducción:", round(prob_reducido, 3), "\n")
  cat("  Reducción absoluta de riesgo:", round(reduccion_riesgo, 3), "\n")
}