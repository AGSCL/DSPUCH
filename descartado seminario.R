
---
  ## Ajuste confusión
  
  
  .panelset.sideways[
    
    .panel[.panel-name[Estandarización]
           
           .pull_left[
             .code_10[
               ```{r pre-p1-cont_conf, echo=T, cache= T, dev.args = list(bg = 'transparent'), paged.print=TRUE, message=F, error=T, warning=F, eval=F}
               library(tidyverse)
               library(gganimate)
               library(ggthemes)
               options(expressions= 100000)
               
               
               df <- data.frame(L = as.integer((1:200>100))) %>% #variable binaria. Primeros 100= 0; Segundos=1
                 mutate(A = log(.2)*L + rbinom(200,1,0.32)) %>% #Variable explicativa, distribución normal, base .5+ doble de L
                 mutate(Y = -.5*A + 4*L + 1 + rnorm(200),time="1") %>%
                 group_by(L) %>%
                 mutate(mean_A=mean(A),mean_Y=mean(Y)) %>%
                 ungroup()
               
               #Calculamos las correlaciones
               before_cor <- paste("1. Inicio, datos crudos: r total= ",round(cor(df$A,df$Y),3),sep='')
               afterlab <-  paste('6. Lo resultante es la correlación entre X e Y controlando por W: ', round(cor(df$A-df$mean_A,df$Y-df$mean_Y),3),sep='')
               
               #Add step 2 in which X is demeaned, and 3 in which both X and Y are, and 4 which just changes label
               dffull <- rbind(
                 #Step 1: Raw data only
                 df %>% mutate(mean_A=NA,mean_Y=NA,time=before_cor),
                 #Step 2: Add x-lines
                 df %>% mutate(mean_Y=NA,time='2. Identificar diferencias explicadas por L'),
                 #Step 3: X de-meaned 
                 df %>% mutate(A = A - mean_A,mean_A=0,mean_Y=NA,time="3. Eliminar diferencias en A explicadas por L"),
                 #Step 4: Remove X lines, add Y
                 df %>% mutate(A = A - mean_A,mean_A=NA,time="4. Averiguar diferencias en Y son explicadas por L"),
                 #Step 5: Y de-meaned
                 df %>% mutate(A = A - mean_A,Y = Y - mean_Y,mean_A=NA,mean_Y=0,time="5. Eliminar diferencias en Y explicadas por L"),
                 #Step 6: Raw demeaned data only
                 df %>% mutate(A = A - mean_A,Y = Y - mean_Y,mean_A=NA,mean_Y=NA,time=afterlab))
               
               p <- ggplot(dffull,aes(y=Y,x=A,color=as.factor(L)))+geom_point()+
                 geom_vline(aes(xintercept=mean_A,color=as.factor(L)))+
                 geom_hline(aes(yintercept=mean_Y,color=as.factor(L)))+
                 guides(color=guide_legend(title="W"))+
                 scale_color_colorblind()+
                 labs(caption="Fuente: Nick Huntington-Klein (https://github.com/NickCH-K/causalgraphs)")+
                 labs(title = 'La relación entre X e Y Controlando por variable binaria L\n{next_state}')+
                 transition_states(time,transition_length=c(12,32,12,32,12,12),state_length=c(160,100,75,100,75,160),wrap=FALSE)+
                 ease_aes('sine-in-out')+
                 exit_fade()+enter_fade()
               animate(p,nframes=100)
               ```
             ]
           ]
           
           .pull_right[    
             ```{r p1-cont_conf, eval=T,  dev.args = list(bg = 'transparent'), echo=T, warning=FALSE, include=T, paged.print=TRUE, fig.align="center", out.width="100%", out.height="100%", error=T, dpi=750, fig.showtext=T}
             knitr::include_graphics('./_figs/control_regresion.gif')
             ```
           ]    
    ]
    
    .panel[.panel-name[Regresión]
           
           .details-code[
             
             ```{r p1-cont_conf2, eval=T,  dev.args = list(bg = 'transparent'), echo=T, warning=FALSE, include=T, paged.print=TRUE, fig.align="center", out.width="100%", out.height="100%", error=T, dpi=750, fig.showtext=T}
             
             
             afterlab <-  paste('6. Lo resultante es la correlación entre X e Y controlando por W: ', round(cor(df$A-df$mean_A,df$Y-df$mean_Y),3),sep='')
             #0.579
             
             #Step 1: Raw data only
             dffull_ipw <-df %>% mutate(mean_A=NA,mean_Y=NA,time=before_cor,ipw=1)
             modelo_lineal<-lm(Y~A,data = df)
             #Add predicted values
             dffull_ipw <-df %>% dplyr::mutate(lm=predict(modelo_lineal,newdata = df, type="response"))
             
             #Step 2: Probability of treatment#<<
             modelo_exposicion <- lm(A ~ L, data = df)#<<
             #Add predicted values
             dffull_ipw <- rbind(dffull_ipw,
                                 dffull_ipw %>% mutate(time='2. Identificar diferencias explicadas por L'))
             dplyr::left_join()
             df %>% dplyr::mutate(ipw=1/predict(modelo_exposicion,newdata = df, type="response"))
             dffull_ipw1<-
               df %>% dplyr::mutate(ipw=1/predict(modelo_exposicion,newdata = df, type="response")) 
             
             %>% 
               dplyr::mutate(eso=cov.wt(cbind(dffull_ipw1$A,dffull_ipw1$Y), wt = dffull_ipw1$ipw, cor = TRUE))
             
             cov.wt(cbind(dffull_ipw1$A,dffull_ipw1$Y), wt = rep(1,200), cor = TRUE)$cor[1,2]
             
             cov.wt(cbind(dffull_ipw1$A,dffull_ipw1$Y), wt = dffull_ipw1$ipw, cor = TRUE)$cor[1,2]
             
             lm(Y~ A, data=dffull_ipw1, weight=ipw)  
             
             sqrt(summary(lm(Y~ A, data=df))$r.squared)
             
             cor(df$A,df$Y)
             
             0.254438
             
             0.2449898
             
             cor(df$A-df$mean_A,df$Y-df$mean_Y)^2
             
             ggplot(aes(x=gdpPercap, y=lifeExp, color=continent, size=pop)) +
               
               ggplot(dffull,aes(y=Y,x=A,color=as.factor(L),size=1))+geom_point()+  
               guides(color=guide_legend(title="W"))+
               scale_color_colorblind()
             
             ipwplot(weights = temp$ipw.weights, timevar = startstop$fuptime,
                     + binwidth = 100, main = "Stabilized weights", xlab = "Days since HIV
+ seroconversion", ylab = "Logarithm of weights", xaxt = "n")
             
             
             dffull <- rbind(
               #Step 1: Raw data only
               df %>% mutate(mean_X=NA,mean_Y=NA,time=before_cor),
               #Step 2: Add x-lines
               df %>% mutate(mean_Y=NA,time='2. Figure out what differences in X are explained by Z'),
               #Step 3: X de-meaned 
               df %>% mutate(X = mean_X,mean_Y=NA,time="3. Remove everything in X not explained by Z"),
               #Step 4: Remove X lines, add Y
               df %>% mutate(X = mean_X,mean_X=NA,time="4. Figure out what differences in Y are explained by Z"),
               #Step 5: Y de-meaned
               df %>% mutate(X = mean_X,Y = mean_Y,mean_X=NA,time="5. Remove everything in Y not explained by Z"),
               #Step 6: Raw demeaned data only
               df %>% mutate(X =  mean_X,Y =mean_Y,mean_X=NA,mean_Y=NA,YL=mean_Y,XL=mean_X,time=afterlab))
             
             ```
           ]
    ]
  ]


.panel[.panel-name[Regresión]
       
       .details-code[
         
         ```{r p1-cont_conf3, eval=T,  dev.args = list(bg = 'transparent'), echo=T, warning=FALSE, include=T, paged.print=TRUE, fig.align="center", fig.width = 7, fig.height = 5, out.width="70%", out.height="70%", error=T, dpi=750, fig.showtext=T}
         
         ```
       ]
]
]

<br>