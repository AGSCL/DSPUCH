#http://saludpublica.uchile.cl/extension/presentacion/material-grafico-institucional-escuela-de-salud-publica

```{r, load_refs, include=F, eval=T, cache=FALSE}
library(RefManageR)
BibOptions(check.entries = FALSE,
           bib.style = "numeric",
           cite.style = "numeric",
           style = "markdown",
           super = TRUE,
           hyperlink = FALSE,
           dashed = FALSE)
warning(paste0(path2,"/libreria_generica.txt"))

myBib <- ReadBib(paste0(path2,"/libreria_generica.txt"), check = FALSE)

```


class: title-slide, middle


background-image: url(_style/Logo_nDP_monotono_vertical_en.png)
background-position: bottom right
background-size: 20%

<br>
  
  ## .text_80[Assessing the impact of Substance abuse treatment (SUT) on the justice system contact prevention in Chile]
  
  <br>
  
  .line_space_11[
    
    
    <br>
      
      .text_70[[Code available in: `r fontawesome::fa(name = "github")`](https://github.com/AGSCL/DSPUCH)]
    
    .text_110[Propuesta - Concurso Fondo de Investigación Intramural nDP]
    
  ]

.bg-text[
  <hr />
    
    `r withr::with_locale(new = c('LC_TIME' = 'C'), code =format(Sys.time(),'%B %d, %Y'))`
  
  
  .text_100[Mariel Mateo Piñones & Andrés González Santa Cruz]
  
  .text_70[mdmateo@uc.cl]
  .text_70[gonzalez.santacruz.andres@gmail.com] [`r fontawesome::fa(name = "github")`](https://github.com/AGSCL) [`r fontawesome::fa(name = "orcid", fill="green")`](https://orcid.org/0000-0002-5166-9121)
]

<br>
  
  ```{r echo=FALSE, out.width = '15%'}
knitr::include_graphics('./_style/blank_space.png')
knitr::include_graphics('./_style/blank_space.png')
knitr::include_graphics('./_style/blank_space.png')
knitr::include_graphics('./_style/blank_space.png')
knitr::include_graphics('./_style/blank_space.png')
```

???
  *#_#_#_#_#_#_#_#_#_#_
  **NOTA**
  *#_#_#_#_#_#_#_#_#_#_
  
  - Mi nombre es andrés gonzález. Soy asistente de investigación del proyecto que lidera Alvaro Castillo
- En esta instancia haré un resumen de una idea de investigación que dirige mi compañera Mariel Mateo, que en esta instancia no se encuentra disponible debido a diferencias de horario. Se encuentra en Australia.

---
  layout: true
class: animated, fadeIn
---
  ## Antecedentes
  
  - Los trastornos por uso de sustancias (TUS) requieren un abordaje **multidimensional** $`r Cite(myBib, c("Stephen2012","RN266" ))`$.

- Reducir el uso se asocia a una reducción de la actividad criminal $`r Cite(myBib, "prendergast2006")`$.

- **“puerta giratoria”** $`r Cite(myBib, c("White2012","Valenzuela2012","Sullivan2019"))`$.

- **Evidencia:** Países desarrollados, muestras pequeñas, modalidades específicas de tratamiento, determinados perfiles de usuarios de drogas, etc. $`r Cite(myBib, c("Krebs2008","Burdon2007", "Teesson2015" ))`$
  
  - Completar el tratamiento: Uno de los criterios más utilizados para evaluar efectividad $`r Cite(myBib, c("Eastwood2018","Brorson2013"))`$.


.center[
  .darkred[
    **¿Mismo contexto que en nuestro país /región?**
  ]
]

---
  ## Antecedentes (2)
  
  - Perfiles de consumo distintos  $`r Cite(myBib, c("Pacurucu2019","Reyes2013","Silva2009", "Castro2021"))`$ 
  
  - Chile, caso de interés, sistemas de monitoreo de tratamientos más antiguos en la región $`r Cite(myBib, c("Marin2018"))`$ 
  
  - Cambios en patrones de contactos con el sistema de justicia y tratamiento TUS $`r Cite(myBib, c("Castillo2015"))`$
  
  - Los tratamientos no sobran: Brecha de acceso: 1:10 usuarios tratados por TUS (13° Estudio Nacional de uso de Sustancias en Población General 2018) $`r Cite(myBib, c("nuevo1"))`$.

- Chile, desafíos en materia de **evaluación efectividad de tratamientos** $`r Cite(myBib, c("nuevo2"))`$. (DIPRES-ISUC, 2020)

- Completar tratamiento ~ readmisión tratamientos TUS (Chile) $`r Cite(myBib, c("Olivari2021"))`$.


.center[
  .darkgreen[
    **¿qué ocurre con los contactos con el sistema de justicia?**
  ]
]

???
  *#_#_#_#_#_#_#_#_#_#_
  **NOTA**
  *#_#_#_#_#_#_#_#_#_#_
  
  
  ---
  class: center, middle

## Objetivos

Examinar el impacto de tratamientos por uso de sustancias en la prevención del contacto con el sistema judicial en Chile, en el corto (3 a 6 meses), medio (1 año) y largo plazo (3 años)

???
  *#_#_#_#_#_#_#_#_#_#_
  **NOTA**
  *#_#_#_#_#_#_#_#_#_#_
  - Nuestra hipótesis es que los pacientes que completan tendrán una probabilidad más baja de contactos con el sistema judicial en comparación con los que no completan, aunque este efecto disminuya a medida que el tiempo pasa.


---
  class: center, middle
# Gracias!

<br>
  
  <div class="centered"> Contacto: mdmateo@uc.cl 
& 
  gonzalez.santacruz.andres@gmail.com </div>
  
  <br>
  
  <br>
  
  <br>
  
  ```{r, echo=FALSE,  fig.align="center", out.width=300, error=T}

knitr::include_graphics(paste0(sub("2019 \\(github\\)/SUD_CL","2022 \\(github\\)",path2),"/_style/Logo_nDP_color_hz_en.png"))

```
<br>
  
  ---
  ## Referencias
  
  ```{r refs1, echo=FALSE, results="asis"}
PrintBibliography(myBib, start= 1, end= 7)
#https://github.com/ropensci/RefManageR/blob/master/R/rmdCite.R
```

---
  ## Referencias (2)
  
  ```{r refs2, echo=FALSE, results="asis"}
PrintBibliography(myBib, start= 8, end= 15)
#https://github.com/ropensci/RefManageR/blob/master/R/rmdCite.R
```

---
  ## Referencias (3)
  
  ```{r refs3, echo=FALSE, results="asis"}
PrintBibliography(myBib, start= 16, end= 24)
#https://github.com/ropensci/RefManageR/blob/master/R/rmdCite.R
```

```{r exp, eval=F,echo=F, results="asis"}
library(pagedown)

pagedown::chrome_print(gsub(".Rmd",".html",rstudioapi::getSourceEditorContext()$path),output="pres_2022_3.pdf")
xaringanBuilder::build_pdf(gsub(".Rmd",".Rmd",rstudioapi::getSourceEditorContext()$path),complex_slides = TRUE,output_file="pres_2022_4.pdf")
xaringanBuilder::build_pdf(gsub(".Rmd",".Rmd",rstudioapi::getSourceEditorContext()$path),complex_slides = TRUE,output_file="pres_2022_5.pdf")
```

