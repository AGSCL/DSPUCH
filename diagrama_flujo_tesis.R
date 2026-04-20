library(DiagrammeR)

# 1. Definición de Etiquetas en Español (con los datos de la imagen)
lab_orig <- "Dataset C1 Original \n(n = 259,395; Pacientes = 121,464)"

lab_orig_clean <- paste(
  "Pre-procesamiento y Controles de Calidad",
  "&#8226; Eliminar registros duplicados exactos",
  "&#8226; Resolver episodios superpuestos (mantener el más largo / fusionar)",
  "&#8226; Corregir fechas de ingreso/egreso inconsistentes",
  "&#8226; Corregir edades/fechas de nacimiento inverosímiles",
  "&#8226; Eliminar días de tratamiento negativos/inverosímiles",
  "", 
  sep = "\\l"
)

lab_post_orig <- "Dataset C1 \n(n = 173,728; Pacientes = 121,299)"

lab_post_orig_clean <- paste(
  "&#8226; Colapsar tratamientos consecutivos vinculados", 
  "(brecha <=45 días, derivación=sí)", 
  sep = "\\l"
)

lab_proc <- "C1 Limpio y Colapsado\n(n = 162,897; Pacientes = 121,299)"

lab_flag <- "Ordenado por Fecha de Admisión\n+ Marcador de Primer Tratamiento"

lab_discard_first <- paste(
  "&#8226; Marcador de elegibilidad (edad 18–64 e ingreso en 2010–2020 inclusive)",
  "&#8226; Exclusión de >=3er tratamiento elegible por paciente",
  "n= 55,195; Pacientes= 32,667 apartados",
  "", sep = "\\l"
)

lab_after <- "Tras la Regla del Primer Tratamiento\nn= 107,702; Pacientes= 88,632"

lab_discard_single <- paste( 
  "1er tratamiento elegible por paciente (con tiempo al evento)",
  "Pacientes = 128", sep="\\l"
)

lab_final <- "Dataset C1 Final\nn = 107,574; Pacientes = 88,504"

# 2. Generación del Diagrama
gr_esp <- grViz(
  paste0(
    'digraph flowchart {
      graph [layout = dot, rankdir = TB]

      node [fontname = "Helvetica", shape = rectangle, fontsize = 33, style = filled, fillcolor = white, ranksep=0.2, nodesep=0.2]

      # Bloques principales y notas
      pre_original   [label = "', lab_orig, '", fillcolor = lightgray, shape = box]
      pre_clean      [label = "', lab_orig_clean, '", shape = note, fillcolor = white]

      pre2_original  [label = "', lab_post_orig, '", fillcolor = white, shape = box]
      pre2_clean     [label = "', lab_post_orig_clean, '", shape = note, fillcolor = "#FFF9C4"] # LemonChiffon

      original       [label = "', lab_proc, '", fillcolor = lightgray, shape = box]
      marked         [label = "', lab_flag, '", shape = box]
      after_rule     [label = "', lab_after, '", shape = box]
      final_dataset  [label = "', lab_final, '", fillcolor = lightgray, shape = box]

      discard_first   [label = "', lab_discard_first, '", shape = note, fillcolor = "#FFE4E1"] # MistyRose
      discard_single  [label = "', lab_discard_single, '", shape = note, fillcolor = "#FFE4E1"] # MistyRose

      # Puntos invisibles para las uniones en T
      v00 [shape = point, width = 0, style = invis]
      v0  [shape = point, width = 0, style = invis]
      v1  [shape = point, width = 0, style = invis]
      v2  [shape = point, width = 0, style = invis]
      v3  [shape = point, width = 0, style = invis]

      # Flujo Principal
      pre_original -> v00 [arrowhead = none]
      v00 -> pre2_original
      
      pre2_original -> v0 [arrowhead = none]
      v0 -> original      

      original -> v1 [arrowhead = none]
      v1 -> marked
      marked -> v2 [arrowhead = none]
      v2 -> after_rule
      after_rule -> v3 [arrowhead = none]
      v3 -> final_dataset

      # Conexiones laterales a las notas de exclusión
      v00 -> pre_clean     [color = black]
      v0 -> pre2_clean     [color = black]
      v2 -> discard_first  [color = black]
      v3 -> discard_single [color = black]

      # Alineación de rangos
      { rank = same; pre_clean; v00 }
      { rank = same; pre2_clean; v0 }
      { rank = same; discard_first; v2 }
      { rank = same; discard_single; v3 }
    }'
  ),
  width = 600, height = 900
)

# Mostrar el resultado
gr_esp


unlink(paste0(gsub("/cons","",getwd()),"/cons/_figs/_flowchart_pred_esp_files"), recursive = TRUE)
htmlwidgets::saveWidget(gr_esp, paste0(gsub("/cons","",getwd()),"/cons/_figs/_flowchart_pred_esp.html"))
webshot::webshot(paste0(gsub("/cons","",getwd()),"/cons/_figs/_flowchart_pred_esp.html"), 
                 paste0(gsub("/cons","",getwd()),"/cons/_figs/_flowchart_pred_esp.png"),
                 vwidth = 300, vheight = 300*1.5,  zoom=10, expand=100)  # Prueba con diferentes coordenadas top, left, width, and height