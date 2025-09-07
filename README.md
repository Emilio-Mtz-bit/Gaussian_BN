# Red Bayesiana Gaussiana: Contaminación Atmosférica y Biomarcadores

## Objetivo
Modelar las relaciones causales entre contaminantes del aire y biomarcadores de salud usando datos de ENSANUT2020 mediante redes bayesianas gaussianas.

## Estructura del Proyecto

### `/data/`
- `Final_Data_Dag's.csv`: Dataset principal con contaminantes atmosféricos y biomarcadores de salud

### `/scripts/queries/`
- `queries_redes_bayesianas.txt`: Propuestas de consultas probabilísticas para evaluación del modelo

## Contaminantes Analizados
- **Partículas**: PM2.5, PM10
- **Gases**: NO2, SO2, CO, NH3

## Biomarcadores de Salud  
- **Inflamación**: PCR
- **Metabolismo**: Glucosa, Insulina, HbA1c, Triglicéridos
- **Perfil lipídico**: Colesterol HDL/LDL/Total
- **Función renal**: Creatinina, Ácido úrico
- **Función hepática**: Albúmina
- **Hematología**: Hemoglobina, Ferritina
- **Vitaminas**: Vitamina D, Vitamina B12

## Metodología


## Uso
Los modelos permiten evaluar probabilidades de alteraciones en biomarcadores bajo diferentes escenarios de exposición a contaminantes atmosféricos en población mexicana.