# Load data & packages ----------------------------------------------------------------

rm(list = ls()) 
unlink("*_cache", recursive=T)
#fuentes: 
#https://rpubs.com/georgy_makarov/897844
load("1_2023.RData")

local({r <- getOption("repos")
r["CRAN"] <- "http://cran.r-project.org" 
options(repos=r)
})
copiar_nombres <- function(x,row.names=FALSE,col.names=TRUE,dec=",",...) {
  if(class(try(dplyr::ungroup(x)))[1]=="tbl_df"){
    if(options()$OutDec=="."){
      options(OutDec = dec)
      write.table(format(data.frame(x)),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ".")
      return(x)
    } else {
      options(OutDec = ",")
      write.table(format(data.frame(x)),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ",")
      return(x)    
    }
  } else {
    if(options()$OutDec=="."){
      options(OutDec = dec)
      write.table(format(x),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ".")
      return(x)
    } else {
      options(OutDec = ",")
      write.table(format(x),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ",")
      return(x)       
    }
  }
}  
pacman::p_unlock(lib.loc = .libPaths()) #para no tener problemas reinstalando paquetes

if(!require(pacman)){install.packages("pacman")}
if(!require(devtools)){install.packages("devtools", type = "win.binary", dependencies=T)}

pacman::p_load(powerSurvEpi, APCtools, ggpattern, withr, boot, matrixStats, knitr, tidyr, stringi,stringr, ggplot2, Hmisc, kableExtra, plotly, janitor, rbokeh, zoo, broom, sqldf, devtools, codebook, data.table, panelr, RColorBrewer, lsmeans, finalfit, ggiraph, sf, treemapify, dplyr, tidyverse, epiR, survminer, ggfortify, survMisc, foreign, reshape2, stargazer, tableone, MatchIt, cobalt, eha, igraph, Amelia, DiagrammeR, DiagrammeRsvg, rsvg, mstate, htmltools, webshot, flexsurv, muhaz, Metrics, rpivotTable, caret, polycor, ClusterR, flextable, ggstatsplot, ggside, daff, explore, sjPlot, compareGroups, job, missForest, showtext, ggpattern, distill, showtext, googleVis, tidylog, magick, dlookr, easystats, tidylog, sqldf,  adjustedCurves, ggpmisc, rms, install=T)

#Error in if (options$noisey == TRUE) message(paste("\n", options$engine, : argument is of length zero


if(!require(survcomp)){BiocManager::install("survcomp")}



# Modify databases ----------------------------------------------------------------


# tesis ags ---------------------------------------------------------------
invisible("With referrals")
bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>% 
  #dplyr::filter(motivo_de_egreso!="Derivación") %>% 
  dplyr::summarize(p0025_dias= quantile(dias_en_tratamiento, .0025,na.rm=T),
                   length_p0025= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .0025,na.rm=T)]),
                   p005_dias= quantile(dias_en_tratamiento, .005,na.rm=T),
                   length_p005= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .005,na.rm=T)]),
                   p01_dias= quantile(dias_en_tratamiento, .01,na.rm=T),
                   length_p01= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .01,na.rm=T)]),
                   p025_dias= quantile(dias_en_tratamiento, .025,na.rm=T),
                   length_p025= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .025,na.rm=T)])
  )
invisible("Without referrals")
bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>% 
  dplyr::filter(motivo_de_egreso!="Derivación") %>% 
  dplyr::summarize(p0025_dias= quantile(dias_en_tratamiento, .0025,na.rm=T),
                   length_p0025= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .0025,na.rm=T)]),
    p005_dias= quantile(dias_en_tratamiento, .005,na.rm=T),
                   length_p005= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .005,na.rm=T)]),
                   p01_dias= quantile(dias_en_tratamiento, .01,na.rm=T),
                   length_p01= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .01,na.rm=T)]),
                   p025_dias= quantile(dias_en_tratamiento, .025,na.rm=T),
                   length_p025= length(dias_en_tratamiento[dias_en_tratamiento<= quantile(dias_en_tratamiento, .025,na.rm=T)])
                   )
paste0("Total of treatment episodes w/o referrals: ",
CONS_C1_2010_19 %>%
  bind_rows(CONS_C1_2019_22) %>%
  filter(motivo_de_egreso != "Derivación", dias_en_tratamiento >= 0) %>%
  nrow() %>% as.numeric() %>% format(big.mark=",")
)
paste0("Total of treatment episodes w referrals: ",
       CONS_C1_2010_19 %>%
         bind_rows(CONS_C1_2019_22) %>%
         filter(motivo_de_egreso == "Derivación", dias_en_tratamiento >= 0) %>%
         nrow() %>% as.numeric() %>% format(big.mark=",")
)

CONS_C1_2010_19 %>%
  bind_rows(CONS_C1_2019_22) %>%
  filter(motivo_de_egreso != "Derivación", dias_en_tratamiento >= 0) %>%
  {
    ggplot(data = .) +
      geom_histogram(aes(x = dias_en_tratamiento), bins = 80) +
      theme_sjplot() +
      labs(x = "Days in treatment", y = "Count") +
      geom_vline(xintercept = quantile(.$dias_en_tratamiento, .0025, na.rm = TRUE), color="red")+
      geom_vline(xintercept = quantile(.$dias_en_tratamiento, .01, na.rm = TRUE), color="purple")+
      geom_vline(xintercept = quantile(.$dias_en_tratamiento, .025, na.rm = TRUE), color="blue")+
      xlim(0,2000)+
      labs(caption=paste0("Note. Red, Violet and Blue lines depicts percentiles 0.25 ",
                         # format(nrow(quantile(.$dias_en_tratamiento, .0025, na.rm = TRUE)),big.mark=","),
                          ", 1", 
                         #format(nrow(quantile(.$dias_en_tratamiento, .01, na.rm = TRUE)),big.mark=","),
                          " & 2.5, respectively"))
                         #format(nrow(quantile(.$dias_en_tratamiento, .025, na.rm = TRUE)),big.mark=",")))
    }


invisible( "Sólo los primeros episodios de tratamiento por cada sujeto, para ver si hay superposición con los casos puros que yo quiero seleccionar")
bind_rows(CONS_C1_2010_19,CONS_C1_2019_22) %>%  #dplyr::filter(is.na(motivo_de_egreso)) %>% 
  dplyr::mutate(date_adm= readr::parse_date(fecha_ingreso_a_tratamiento,c("%d/%m/%Y"))) %>% 
  dplyr::arrange(HASH_KEY, date_adm) %>% 
  dplyr::group_by(HASH_KEY) %>% 
  dplyr::slice(1) %>% # nrow()  #106,534
  dplyr::ungroup() %>% 
  dplyr::filter(motivo_de_egreso!="Derivación", dias_en_tratamiento>=0) %>% 
  ggplot()+
  geom_histogram(aes(x=dias_en_tratamiento), bins=80)+ #149 casos en 0 // 2,076 casos en 21 días o menos
  sjPlot::theme_sjplot()+
  labs(x="Days in treatment", y= "Count")


# Sample size -------------------------------------------------------------

# http://powerandsamplesize.com/Calculators/Test-Time-To-Event-Data/Cox-PH-Equivalence
# https://ph-ivshiny.iowa.uiowa.edu/rpterson/MSDshiny/
require(powerSurvEpi)


#ssizeEpiCont.default Sample Size Calculation for Cox Proportional Hazards Regression with Nonbinary Covariates for Epidemiological Studies
#Sample size calculation for Cox proportional hazards regression with nonbinary covariates for Epidemiological Studies.

# power numeric. postulated power.
# theta numeric. postulated hazard ratio.
# sigma2 numeric. variance of the covariate of interest.
# psi numeric. proportion of subjects died of the disease of interest.
# rho2 numeric. square of the multiple correlation coefficient between the covariate of
# interest and other covariates.
# alpha numeric. type I error rate.


CONS_C1_2010_19 %>%
  bind_rows(CONS_C1_2019_22) %>% 
  summarise(mean=mean(dias_en_tratamiento, na.rm=T), (sd(dias_en_tratamiento, na.rm=T)^2))

ssizeEpiCont.default(power= 0.8,
                     theta= 0.99,
                     sigma2= sd(cpdata2$time_in_trt)^2,
                     psi= 0.00743,
                     rho2= 0.8, #(0.7)
                     alpha = 0.1)

#stpower cox -.01005034, power(.8) sd(176.5311) r2(0.9) failprob(0.00743)
#stpower cox -.01005034, power(.95) sd(176.5311) r2(0.8) failprob(0.00743)
ssizeEpiCont.default(power= 0.90,
                     theta= .99,
                     sigma2= sd(cpdata2$time_in_trt)^2,
                     psi= 0.00743,
                     rho2= 0.8, #(0.7)
                     alpha = 0.01)



#An item's SMC value, its squared multiple correlation, indicates the proportion of the item's 
#variance which may be linked to, or predicted from, the other items in the subtest. 
#As mentioned in the previous topic, the SMC is sometimes used as an estimate of the amount of 
#variance any single item has in common with the other items.

#SMC was 0.67. We may interpret this as meaning that 67% of Q2's 
#variance can be explained by the other items in the subtest. 

# Hsieh and Lavori (2000) assumed one-sided test,
# while this implementation assumed two-sided test.
# Hence alpha=0.1 here (two-sided test) will correspon

#PLONSKY, L. and GHANBAR, H. (2018), Multiple Regression in L2 Research: A Methodological Synthesis and Guide to 
#Interpreting R2 Values  . The Modern Language Journal, 102: 713-731. https://doi.org/10.1111/modl.12509

#Researchers can detect multicollinearity and singularity by an array of statistics such as 
#squared multiple correlation (SMC), variance inflation rate (VIF), tolerance value, and 
#condition index, all provided by most statistical packages.