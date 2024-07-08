#https://cran.r-project.org/web/packages/powerSurvEpi/powerSurvEpi.pdf
rm(list = ls());gc()
data<-
  rio::import("___datos_minsal/2023-08-11 DatosDefuncionesEncrip.csv")


invisible("Para homologar nombres")
iconv_xlsx<-rio::import("iconv.xlsx")

replacements <- setNames(as.character(iconv_xlsx[, 2]), iconv_xlsx[, 1])


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("Bajamos las bases de octubre 2023: definitiva")
path<-paste0(getwd()) #C:\Users\CISS Fondecyt\Mi unidad\Alvacast\SISTRAT 2023\data\20230927_original_data\senda\encriptados_c1
dir_c14<-paste0(gsub(" \\(github\\)","",gsub("22","23",path)),"/data/20231018_original_data/")

#matches a string that contains _enc.
SISTRAT23_c14<-list.files(path=toString(dir_c14), pattern="_enc")
#discard other agreements
SISTRAT23_c14 <- SISTRAT23_c14[!startsWith(SISTRAT23_c14, "c")]

#Import datasets from Sept 27, 2022
for (i in 1:length(SISTRAT23_c14)) {
  x<-SISTRAT23_c14[i]
  xn<- paste0(stringr::str_sub(x, 1, 4),ifelse(is.na(str_extract(x,"(?<=_dup)\\d+")),"",str_extract(x, "(?<=_dup)\\d+")))
  readr::read_delim(paste0(dir_c14, x),
                    na = c("", "NA","null"),
                    locale = locale(encoding = "windows-1252"),
                    guess_max = min(1e5, Inf), 
                    trim_ws=T,
                    skip=0)  %>%
    #rename_with(., ~ gsub("'", "", iconv(.x, from = "UTF-8", to='ASCII//TRANSLIT'))) %>% 
    rename_with(~ stringr::str_replace_all(.x, c("\\u009c"="u",
                                                 "\\u0097"="o",
                                                 "\\u0087"="a",
                                                 "\\u0092"="i",
                                                 "\\u0096"="n")))   %>%
    janitor::clean_names() %>% 
    as.data.frame() %>% 
    dplyr::rename("HASH_KEY"="hashkey") %>% 
    dplyr::select(HASH_KEY, everything()) %>% 
    janitor::clean_names() %>% 
    assign(paste0("SISTRAT234_c1_",xn),.,envir = .GlobalEnv)
}

#MERGE DATABASES
CONS_C1_2010_22_sub<- ls()[grepl("SISTRAT234_c1_",ls())]

CONS_C1_2010_234=data.table::rbindlist(mget(CONS_C1_2010_22_sub), idcol="TABLE", fill=T) %>% 
  dplyr::mutate(TABLE = str_extract(TABLE, "(?<=c1_)\\d+")) %>% #distinct(TABLE)
  dplyr::select(TABLE, hash_key, everything()) 

#unique(CONS_C1_2010_234$TABLE)
# 20100 20110 20120 20130 20141 20142 20151 20152 20161 20162 20171 20172 20181 20182 20190 20191 20192 20200 20211 20212 20221 20222 
# 9772  10872 11521 15687 16383  1198 16383  3630 16383  3576 16383  4095 16383  3527  9401 16383  1085 14907 16383  1123 16383  1603 
#table(CONS_C1_2010_233$TABLE)
# 2010  2011  2012  2013  2014        2015        2016        2017        2018        20191 20192       2020  2021        2022 
# 9772 10872 11521 15687 16383        16383       16383       16383       16383       16383  9401       14907 16383       16383 
#table(CONS_C1_2010_222$TABLE) #fallida ago 2023
# 2010  2011  2012  2013  2014        2015        2016        2017        2018        20191 20192       2020  2021        2022 .csv    
# 9772  10872 11521 15687 17581       20013       19959       20478       19910       17468  9401       14907 17506       17986 21744  
#table(CONS_C1_2010_221$TABLE)
# 2010  2011  2012  2013  2014        2015        2016        2017        2018        20191 20192       2020  2021        2022 
# 9763  10837 11470 15669 17580       20013       19959       20478       19910       17467  9401       14906 17506       17986 

#rm(list= ls(pattern="SISTRAT234_c1_"))

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
library(powerSurvEpi)
library(tidyverse)
library(Hmisc)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

CONS_C1_2010_234_df<-
CONS_C1_2010_234%>% 
  dplyr::mutate(fecha_ingreso_a_tratamiento= str_replace_all(fecha_ingresoa_tratamiento,"/","-"),
                fecha_egreso_de_tratamiento= str_replace_all(fecha_egresode_tratamiento,"/","-")) %>% 
  dplyr::mutate(fecha_egreso_de_tratamiento= ifelse(fecha_egreso_de_tratamiento=="13082013","13-08-2013",fecha_egreso_de_tratamiento)) %>% 
  dplyr::mutate(fecha_ingreso_a_tratamiento= readr::parse_date(fecha_ingreso_a_tratamiento, format="%d-%m-%Y")) %>% 
  dplyr::mutate(fecha_egreso_de_tratamiento= readr::parse_date(fecha_egreso_de_tratamiento, format="%d-%m-%Y")) %>% 
  dplyr::arrange(hash_key,fecha_ingreso_a_tratamiento) %>% 
  #dplyr::select(hash_key, fecha_ingreso_a_tratamiento, edad) %>% 
  dplyr::group_by(hash_key) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(edad>=18 & edad<=64) 


CONS_C1_2010_234_df_m<-
CONS_C1_2010_234_df %>% 
  dplyr::left_join(data[,c("HASHKEY","DIA_DEF","MES_DEF","ANO_DEF")], by=c("hash_key"="HASHKEY"))


CONS_C1_2010_234_df_m %>% 
  dplyr::mutate(date_def=readr::parse_date(glue::glue("{DIA_DEF}-{MES_DEF}-{ANO_DEF}"), format="%d-%m-%Y")) %>%
  dplyr::filter(!is.na(date_def)) %>% #nrow() #4229
 # dplyr::filter(date_def>=fecha_egreso_de_tratamiento)%>% nrow() #3606
  dplyr::filter(date_def<=fecha_egreso_de_tratamiento, fecha_ingreso_a_tratamiento>=date_def) %>% nrow() #314

CONS_C1_2010_234_df_m %>%
  dplyr::mutate(date_def = readr::parse_date(glue::glue("{DIA_DEF}-{MES_DEF}-{ANO_DEF}"), format = "%d-%m-%Y")) %>%
  dplyr::filter(!is.na(date_def)) %>%
  dplyr::filter(date_def>=fecha_egreso_de_tratamiento) %>% 
  dplyr::filter(as.numeric(date_def)-as.numeric(fecha_egreso_de_tratamiento) <= 5 * 365.25) %>% nrow()
CONS_C1_2010_234_df_m2<-
CONS_C1_2010_234_df_m %>%
  dplyr::mutate(date_def = readr::parse_date(glue::glue("{DIA_DEF}-{MES_DEF}-{ANO_DEF}"), format = "%d-%m-%Y")) %>%
  dplyr::mutate(event= ifelse(!is.na(date_def),1,0)) %>%
  dplyr::filter(date_def>=fecha_egreso_de_tratamiento) %>% 
  dplyr::mutate(diff=as.numeric(date_def)-as.numeric(fecha_egreso_de_tratamiento))

km_fit <- survfit(Surv(diff, event) ~ 1, data=CONS_C1_2010_234_df_m2)
summary(km_fit, times = c(1,30,60,90*(1:50)))
# 2,701 mueren dentro de 5 años.

# Call: survfit(formula = Surv(diff, event) ~ 1, data = CONS_C1_2010_234_df_m2)

# time n.risk n.event survival  std.err lower 95% CI upper 95% CI
# 1   3571      44 0.987798 0.001828     0.984221      0.99139
# 30   3494      71 0.968109 0.002926     0.962391      0.97386
# 60   3436      57 0.952302 0.003549     0.945371      0.95928
# 90   3379      56 0.936772 0.004053     0.928862      0.94475
# 180   3232     148 0.895729 0.005089     0.885810      0.90576
# 270   3100     131 0.859401 0.005789     0.848130      0.87082
# 360   2947     155 0.816417 0.006447     0.803878      0.82915
# 450   2777     169 0.769551 0.007013     0.755928      0.78342
# 540   2629     146 0.729063 0.007401     0.714700      0.74371
# 630   2481     150 0.687465 0.007719     0.672502      0.70276
# 720   2310     170 0.640322 0.007992     0.624848      0.65618
# 810   2176     133 0.603439 0.008146     0.587682      0.61962
# 900   2052     126 0.568497 0.008248     0.552559      0.58489
# 990   1911     142 0.529118 0.008312     0.513075      0.54566
# 1080   1769     142 0.489739 0.008325     0.473692      0.50633
# 1170   1637     130 0.453688 0.008291     0.437727      0.47023
# 1260   1537      99 0.426234 0.008235     0.410395      0.44268
# 1350   1434     105 0.397116 0.008148     0.381463      0.41341
# 1440   1324     111 0.366334 0.008023     0.350941      0.38240
# 1530   1216     106 0.336938 0.007871     0.321859      0.35272
# 1620   1110     106 0.307543 0.007685     0.292844      0.32298
# 1710   1023      86 0.283694 0.007507     0.269356      0.29880
# 1800    929      94 0.257626 0.007283     0.243741      0.27230
# 1890    858      71 0.237937 0.007091     0.224437      0.25225
# 1980    782      76 0.216861 0.006863     0.203819      0.23074
# 2070    713      70 0.197449 0.006629     0.184874      0.21088
# 2160    628      85 0.173877 0.006311     0.161936      0.18670
# 2250    569      58 0.157793 0.006071     0.146332      0.17015
# 2340    514      55 0.142540 0.005822     0.131574      0.15442
# 2430    452      64 0.124792 0.005503     0.114458      0.13606
# 2520    405      48 0.111481 0.005241     0.101668      0.12224
# 2610    345      57 0.095674 0.004898     0.086539      0.10577
# 2700    303      42 0.084027 0.004620     0.075443      0.09359
# 2790    261      42 0.072379 0.004315     0.064398      0.08135
# 2880    222      40 0.061287 0.003994     0.053938      0.06964
# 2970    190      31 0.052690 0.003720     0.045880      0.06051
# 3060    171      19 0.047421 0.003539     0.040968      0.05489
# 3150    143      29 0.039379 0.003239     0.033516      0.04627
# 3240    107      35 0.029673 0.002826     0.024621      0.03576
# 3330     80      27 0.022185 0.002453     0.017863      0.02755
# 3420     58      22 0.016084 0.002095     0.012461      0.02076
# 3510     35      23 0.009706 0.001633     0.006980      0.01350
# 3600     23      12 0.006378 0.001326     0.004244      0.00959
# 3690     13      10 0.003605 0.000998     0.002095      0.00620
# 3780      8       5 0.002219 0.000783     0.001110      0.00443
# 3870      3       5 0.000832 0.000480     0.000268      0.00258
# 3960      2       1 0.000555 0.000392     0.000139      0.00222

# 3603/4226
# 36030.8525793
# 314/4226
# [1] 0.07430194
# hay 7.4% de sujetos que mueren mientras están en tratamiento
# 314/3603
# [1] 0.0871496 # de los que tienen 


#2023-12-07
CONS_C1_2010_234_df_m %>% 
       dplyr::mutate(date_def=readr::parse_date(glue::glue("{DIA_DEF}-{MES_DEF}-{ANO_DEF}"), format="%d-%m-%Y"))  %>% nrow()
#[1] 103075

#en mi tesis tengo 103,074 (1 menos)
#y también tengo 4,226 eventos, 3 menos que estos (4229)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


samp_size=expand.grid(v=seq(.01, 1, by=.05), i=seq(1.01, 2, by=.05), samp_size=rep(0,))

for (v in 1:length(samp_size$samp_size)){
    samp_size[v,"samp_size"]<- powerSurvEpi::numDEpi.default(.8, theta=samp_size[v,"i"], p=samp_size[v,"v"],.5,.05)
}

samp_size %>% 
  dplyr::filter(samp_size<5000) %>% 
ggplot(aes(x=interaction(v,i), y=samp_size, color=i)) +
  geom_point(size=.8) +
  theme_minimal() +
  labs(title="Scatter plot of A vs. B", x="A", y="B")+
  theme(axis.text.y = element_text(size = 2))+
  coord_flip()
ggsave("tamano_muestra.png",height=10, width=10, dpi=500)


invisible("Mortality 29:65")
sample_size_i_have <-c()
ages_i_have <-c(29:65)
ther_dis<-c()  
n_treatments <-data.frame()
n_first_treatment <-data.frame()
total <-c()

for (v in 29:65){
        sample_size_i_have[(v-28)]<-
  CONS_C1_2010_234%>% 
    dplyr::mutate(fecha_ingreso_a_tratamiento= str_replace_all(fecha_ingresoa_tratamiento,"/","-"),
                  fecha_egreso_de_tratamiento= str_replace_all(fecha_egresode_tratamiento,"/","-")) %>% 
    dplyr::mutate(fecha_egreso_de_tratamiento= ifelse(fecha_egreso_de_tratamiento=="13082013","13-08-2013",fecha_egreso_de_tratamiento)) %>% 
    dplyr::mutate(fecha_ingreso_a_tratamiento= readr::parse_date(fecha_ingreso_a_tratamiento, format="%d-%m-%Y")) %>% 
    dplyr::mutate(fecha_egreso_de_tratamiento= readr::parse_date(fecha_egreso_de_tratamiento, format="%d-%m-%Y")) %>% 
    dplyr::arrange(hash_key,fecha_ingreso_a_tratamiento) %>% 
    #dplyr::select(hash_key, fecha_ingreso_a_tratamiento, edad) %>% 
    dplyr::group_by(hash_key) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(edad>=15 & edad<=v) %>% 
    dplyr::inner_join(janitor::clean_names(data), by=c("hash_key"="hashkey")) %>% 
    dplyr::rename("codigo_identificacion"="codigo_identificaci_af_a3n") %>% 
    dplyr::select("hash_key","codigo_identificacion", "senda", "edad", "nacionalidad", 
                  "sexo.x", "fecha_ingresoa_tratamiento", "fecha_egresode_tratamiento", 
                  "motivodeegreso_alta_administra", "dia_nac", "mes_nac", "ano1_nac", "ano2_nac",
                  "sexo.y", "edad_tipo", "dia_def", "mes_def", "ano_def") %>% 
    #filtré el malo
    dplyr::filter(hash_key!="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464") %>% 
    distinct(hash_key) %>% nrow()
    
        ther_dis[(v-28)]<-  
    CONS_C1_2010_234%>% 
          dplyr::mutate(fecha_ingreso_a_tratamiento= str_replace_all(fecha_ingresoa_tratamiento,"/","-"),
                        fecha_egreso_de_tratamiento= str_replace_all(fecha_egresode_tratamiento,"/","-")) %>% 
          dplyr::mutate(fecha_egreso_de_tratamiento= ifelse(fecha_egreso_de_tratamiento=="13082013","13-08-2013",fecha_egreso_de_tratamiento)) %>% 
          dplyr::mutate(fecha_ingreso_a_tratamiento= readr::parse_date(fecha_ingreso_a_tratamiento, format="%d-%m-%Y")) %>% 
          dplyr::mutate(fecha_egreso_de_tratamiento= readr::parse_date(fecha_egreso_de_tratamiento, format="%d-%m-%Y")) %>% 
          dplyr::arrange(hash_key,fecha_ingreso_a_tratamiento) %>% 
          #dplyr::select(hash_key, fecha_ingreso_a_tratamiento, edad) %>% 
          dplyr::group_by(hash_key) %>% 
          dplyr::slice(1) %>% 
          dplyr::ungroup() %>% 
          dplyr::filter(edad>=15 & edad<=v) %>% janitor::tabyl(motivode_egreso) %>% dplyr::filter(grepl("Alta Terap", motivode_egreso)) %>% data.frame()   
        
        #make the filtered database
          CONS_C1_2010_234%>% 
          dplyr::mutate(fecha_ingreso_a_tratamiento= str_replace_all(fecha_ingresoa_tratamiento,"/","-"),
                        fecha_egreso_de_tratamiento= str_replace_all(fecha_egresode_tratamiento,"/","-")) %>% 
          dplyr::mutate(fecha_egreso_de_tratamiento= ifelse(fecha_egreso_de_tratamiento=="13082013","13-08-2013",fecha_egreso_de_tratamiento)) %>% 
          dplyr::mutate(fecha_ingreso_a_tratamiento= readr::parse_date(fecha_ingreso_a_tratamiento, format="%d-%m-%Y")) %>% 
          dplyr::mutate(fecha_egreso_de_tratamiento= readr::parse_date(fecha_egreso_de_tratamiento, format="%d-%m-%Y")) %>% 
          dplyr::arrange(hash_key,fecha_ingreso_a_tratamiento) %>% 
          #dplyr::select(hash_key, fecha_ingreso_a_tratamiento, edad) %>% 
          dplyr::group_by(hash_key) %>% 
          dplyr::slice(1) %>% 
          dplyr::ungroup() %>% 
          dplyr::filter(edad>=18 & edad<=v) %>% 
          distinct(hash_key) %>% 
          assign(paste0("hash_18_",v),., envir =.GlobalEnv)

          #count treatments
          n_treatments<-
            rbind.data.frame(n_treatments, 
        CONS_C1_2010_234%>% 
          distinct(hash_key, tipode_plan, diasen_tratamiento, sexo, edad, fecha_ingresoa_tratamiento,
                   fecha_egresode_tratamiento, fecha_egresode_tratamiento, motivode_egreso, 
                   escolaridad_af_aoltimoa_af_a_ocursado, sustancia_principal, frecuenciade_consumo_sustancia, i_dcentro,
                   edad_inicio_sustancia_principal, diagn_af_a3stico_trs_consumo_sustanc) %>% 
          dplyr::filter(hash_key %in% get(paste0("hash_18_",v))$hash_key) %>% 
          dplyr::group_by(hash_key) %>% 
          dplyr::summarise(n=n()) %>% 
          dplyr::summarise(total=sum(n),n_per=n(),min=min(n), p025= quantile(n, .025), p25= quantile(n, .25), mean= mean(n), sd=sd(n), p50= quantile(n, .5), p75= quantile(n, .75), p975= quantile(n, .975), max=max(n))
            )
        
          n_first_treatment<-
          rbind.data.frame(n_first_treatment, 
            CONS_C1_2010_234%>% 
            dplyr::mutate(fecha_ingreso_a_tratamiento= str_replace_all(fecha_ingresoa_tratamiento,"/","-"),
                          fecha_egreso_de_tratamiento= str_replace_all(fecha_egresode_tratamiento,"/","-")) %>% 
            dplyr::mutate(fecha_egreso_de_tratamiento= ifelse(fecha_egreso_de_tratamiento=="13082013","13-08-2013",fecha_egreso_de_tratamiento)) %>% 
            dplyr::mutate(fecha_ingreso_a_tratamiento= readr::parse_date(fecha_ingreso_a_tratamiento, format="%d-%m-%Y")) %>% 
            dplyr::mutate(fecha_egreso_de_tratamiento= readr::parse_date(fecha_egreso_de_tratamiento, format="%d-%m-%Y")) %>% 
            dplyr::arrange(hash_key,fecha_ingreso_a_tratamiento) %>% 
            #dplyr::select(hash_key, fecha_ingreso_a_tratamiento, edad) %>% 
            dplyr::group_by(hash_key) %>% 
            dplyr::slice(1) %>% 
            dplyr::ungroup() %>% 
            dplyr::filter(edad>=15 & edad<=v) %>% 
      dplyr::mutate(n=diasen_tratamiento) %>% 
      dplyr::summarise(tramo=paste0("18-",v),min=min(n, na.rm=T), p025= quantile(n, .025, na.rm=T), p25= quantile(n, .25, na.rm=T), mean= mean(n, na.rm=T), sd=sd(n, na.rm=T), 
                       p50= quantile(n, .5, na.rm=T), p75= quantile(n, .75, na.rm=T), p975= quantile(n, .975, na.rm=T), max=max(n, na.rm=T))
          )
            
          
    total[(v-28)]<-  
    CONS_C1_2010_234%>% 
      dplyr::mutate(fecha_ingreso_a_tratamiento= str_replace_all(fecha_ingresoa_tratamiento,"/","-"),
                    fecha_egreso_de_tratamiento= str_replace_all(fecha_egresode_tratamiento,"/","-")) %>% 
      dplyr::mutate(fecha_egreso_de_tratamiento= ifelse(fecha_egreso_de_tratamiento=="13082013","13-08-2013",fecha_egreso_de_tratamiento)) %>% 
      dplyr::mutate(fecha_ingreso_a_tratamiento= readr::parse_date(fecha_ingreso_a_tratamiento, format="%d-%m-%Y")) %>% 
      dplyr::mutate(fecha_egreso_de_tratamiento= readr::parse_date(fecha_egreso_de_tratamiento, format="%d-%m-%Y")) %>% 
      dplyr::arrange(hash_key,fecha_ingreso_a_tratamiento) %>% 
      #dplyr::select(hash_key, fecha_ingreso_a_tratamiento, edad) %>% 
      dplyr::group_by(hash_key) %>% 
      dplyr::slice(1) %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(edad>=15 & edad<=v) %>% 
      distinct(hash_key) %>% nrow()
}

data_samp_size<- #n_first_treatment n_treatments
cbind.data.frame(ages_i_have,total, sample_size_i_have, n_first_treatment, rename_all(n_treatments[,c("mean", "p75", "p975", "max")],~paste0(., "_n_trts"))) %>% 
  dplyr::mutate(perc=scales::percent(sample_size_i_have/total, accuracy=.1)) %>% 
  dplyr::rename("Age intervals"="ages_i_have","Deaths"="sample_size_i_have","Total Patients"="total","Percentage"="perc") %>%
  dplyr::select(tramo, everything()) %>% dput()
  
alta_ter_dias_tr_18_65<-
CONS_C1_2010_234%>% 
  dplyr::mutate(fecha_ingreso_a_tratamiento= str_replace_all(fecha_ingresoa_tratamiento,"/","-"),
                fecha_egreso_de_tratamiento= str_replace_all(fecha_egresode_tratamiento,"/","-")) %>% 
  dplyr::mutate(fecha_egreso_de_tratamiento= ifelse(fecha_egreso_de_tratamiento=="13082013","13-08-2013",fecha_egreso_de_tratamiento)) %>% 
  dplyr::mutate(fecha_ingreso_a_tratamiento= readr::parse_date(fecha_ingreso_a_tratamiento, format="%d-%m-%Y")) %>% 
  dplyr::mutate(fecha_egreso_de_tratamiento= readr::parse_date(fecha_egreso_de_tratamiento, format="%d-%m-%Y")) %>% 
  dplyr::arrange(hash_key,fecha_ingreso_a_tratamiento) %>% 
  #dplyr::select(hash_key, fecha_ingreso_a_tratamiento, edad) %>% 
  dplyr::group_by(hash_key) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(edad>=15 & edad<=v) %>% 
  dplyr::mutate(diasen_tratamiento=cut2(diasen_tratamiento,g=20)) %>% #number of quantile groups
  dplyr::mutate(motivode_egreso=grepl("Alta Ter", motivode_egreso)) %>% 
  dplyr::group_by(diasen_tratamiento, motivode_egreso) %>% 
  dplyr::summarise(n=n())

alta_ter_dias_tr_18_29<-
  CONS_C1_2010_234%>% 
  dplyr::mutate(fecha_ingreso_a_tratamiento= str_replace_all(fecha_ingresoa_tratamiento,"/","-"),
                fecha_egreso_de_tratamiento= str_replace_all(fecha_egresode_tratamiento,"/","-")) %>% 
  dplyr::mutate(fecha_egreso_de_tratamiento= ifelse(fecha_egreso_de_tratamiento=="13082013","13-08-2013",fecha_egreso_de_tratamiento)) %>% 
  dplyr::mutate(fecha_ingreso_a_tratamiento= readr::parse_date(fecha_ingreso_a_tratamiento, format="%d-%m-%Y")) %>% 
  dplyr::mutate(fecha_egreso_de_tratamiento= readr::parse_date(fecha_egreso_de_tratamiento, format="%d-%m-%Y")) %>% 
  dplyr::arrange(hash_key,fecha_ingreso_a_tratamiento) %>% 
  #dplyr::select(hash_key, fecha_ingreso_a_tratamiento, edad) %>% 
  dplyr::group_by(hash_key) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(edad>=15 & edad<=29) %>% 
  dplyr::mutate(diasen_tratamiento=cut2(diasen_tratamiento,g=20)) %>% #number of quantile groups
  dplyr::mutate(motivode_egreso=grepl("Alta Ter", motivode_egreso)) %>% 
  dplyr::group_by(diasen_tratamiento, motivode_egreso) %>% 
  dplyr::summarise(n=n())

hash_18_64

#ggplot(alta_ter_dias_tr_18_29, aes(diasen_tratamiento,n, fill=motivode_egreso))+
ggplot(alta_ter_dias_tr_18_65, aes(x = diasen_tratamiento, y = n, fill = motivode_egreso, group = motivode_egreso)) +
  geom_area(position = "stack", alpha = 0.6) +
  labs(
    title = "Treatment Duration and Number of Patients (18-65)",
    x = "Days in Treatment",
    y = "Number of Patients",
    fill = "Therapeutic discharge (True/False)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"), name = "Therapeutic discharge")


#ggplot(alta_ter_dias_tr_18_29, aes(diasen_tratamiento,n, fill=motivode_egreso))+
  ggplot(alta_ter_dias_tr_18_29, aes(x = diasen_tratamiento, y = n, fill = motivode_egreso, group = motivode_egreso)) +
  geom_area(position = "stack", alpha = 0.6) +
  labs(
    title = "Treatment Duration and Number of Patients (18-29)",
    x = "Days in Treatment",
    y = "Number of Patients",
    fill = "Therapeutic discharge (True/False)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"), name = "Therapeutic discharge")


structure(list(`Age intervals` = 29:65, `Total Patients` = c(18853L, 
  22268L, 25950L, 29522L, 33186L, 36832L, 40583L, 44398L, 48509L, 
  52392L, 55994L, 59190L, 62304L, 65017L, 67857L, 70725L, 73449L, 
  76141L, 78802L, 81204L, 83396L, 85521L, 87495L, 89313L, 91017L, 
  92713L, 94224L, 95679L, 96968L, 98104L, 99200L, 100194L, 101075L, 
  101821L, 102499L, 103074L, 103513L), Deaths = c(381L, 446L, 526L, 
  595L, 695L, 781L, 870L, 978L, 1079L, 1174L, 1271L, 1397L, 1500L, 
  1592L, 1701L, 1824L, 1968L, 2105L, 2215L, 2361L, 2493L, 2642L, 
  2745L, 2857L, 2994L, 3157L, 3310L, 3435L, 3562L, 3673L, 3792L, 
  3890L, 3989L, 4081L, 4157L, 4226L, 4285L), Percentage = c("2.0%", 
  "2.0%", "2.0%", "2.0%", "2.1%", "2.1%", "2.1%", "2.2%", "2.2%", 
  "2.2%", "2.3%", "2.4%", "2.4%", "2.4%", "2.5%", "2.6%", "2.7%", 
  "2.8%", "2.8%", "2.9%", "3.0%", "3.1%", "3.1%", "3.2%", "3.3%", 
  "3.4%", "3.5%", "3.6%", "3.7%", "3.7%", "3.8%", "3.9%", "3.9%", 
  "4.0%", "4.1%", "4.1%", "4.1%")), class = "data.frame", row.names = c(NA, 
  -37L))

#alta_ter_dias_tr_18_29
structure(list(diasen_tratamiento = structure(c(1L, 1L, 2L, 2L, 
  3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 8L, 8L, 9L, 9L, 10L, 
  10L, 11L, 11L, 12L, 12L, 13L, 13L, 14L, 14L, 15L, 15L, 16L, 16L, 
  17L, 17L, 18L, 18L, 19L, 19L, 20L, 20L, NA), .Label = c("[-345,  27)", 
  "[  27,  43)", "[  43,  57)", "[  57,  68)", "[  68,  80)", "[  80,  91)", 
  "[  91, 102)", "[ 102, 116)", "[ 116, 129)", "[ 129, 144)", "[ 144, 159)", 
  "[ 159, 179)", "[ 179, 199)", "[ 199, 224)", "[ 224, 252)", "[ 252, 288)", 
  "[ 288, 334)", "[ 334, 393)", "[ 393, 492)", "[ 492,3324]"), class = "factor"), 
  motivode_egreso = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, 
  FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, 
  TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, 
  FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, 
  TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), n = c(950L, 
  9L, 924L, 13L, 1006L, 15L, 908L, 13L, 903L, 20L, 894L, 30L, 
  871L, 42L, 916L, 56L, 881L, 72L, 838L, 67L, 837L, 105L, 835L, 
  143L, 738L, 179L, 718L, 232L, 640L, 295L, 640L, 322L, 549L, 
  374L, 492L, 439L, 433L, 511L, 480L, 460L, 3L)), class = c("grouped_df", 
  "tbl_df", "tbl", "data.frame"), row.names = c(NA, -41L), groups = structure(list(
  diasen_tratamiento = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 
  7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 
  19L, 20L, NA), .Label = c("[-345,  27)", "[  27,  43)", "[  43,  57)", 
  "[  57,  68)", "[  68,  80)", "[  80,  91)", "[  91, 102)", 
  "[ 102, 116)", "[ 116, 129)", "[ 129, 144)", "[ 144, 159)", 
  "[ 159, 179)", "[ 179, 199)", "[ 199, 224)", "[ 224, 252)", 
  "[ 252, 288)", "[ 288, 334)", "[ 334, 393)", "[ 393, 492)", 
  "[ 492,3324]"), class = "factor"), .rows = structure(list(
  1:2, 3:4, 5:6, 7:8, 9:10, 11:12, 13:14, 15:16, 17:18, 
  19:20, 21:22, 23:24, 25:26, 27:28, 29:30, 31:32, 33:34, 
  35:36, 37:38, 39:40, 41L), ptype = integer(0), class = c("vctrs_list_of", 
  "vctrs_vctr", "list"))), class = c("tbl_df", "tbl", "data.frame"
  ), row.names = c(NA, -21L), .drop = TRUE))

## Estadisticas 18-64 -------------------------------------------------------

#2023-11-08
alta_ter_dias_tr_18_64<-
  CONS_C1_2010_234%>% 
  dplyr::mutate(fecha_ingreso_a_tratamiento= str_replace_all(fecha_ingresoa_tratamiento,"/","-"),
                fecha_egreso_de_tratamiento= str_replace_all(fecha_egresode_tratamiento,"/","-")) %>% 
  dplyr::mutate(fecha_egreso_de_tratamiento= ifelse(fecha_egreso_de_tratamiento=="13082013","13-08-2013",fecha_egreso_de_tratamiento)) %>% 
  dplyr::mutate(fecha_ingreso_a_tratamiento= readr::parse_date(fecha_ingreso_a_tratamiento, format="%d-%m-%Y")) %>% 
  dplyr::mutate(fecha_egreso_de_tratamiento= readr::parse_date(fecha_egreso_de_tratamiento, format="%d-%m-%Y")) %>% 
  dplyr::arrange(hash_key,fecha_ingreso_a_tratamiento) %>% 
  #dplyr::select(hash_key, fecha_ingreso_a_tratamiento, edad) %>% 
  dplyr::group_by(hash_key) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(edad>=18 & edad<=64) %>% 
  dplyr::mutate(diasen_tratamiento=cut2(diasen_tratamiento,g=20)) %>% #number of quantile groups
  dplyr::mutate(motivode_egreso=grepl("Alta Ter", motivode_egreso)) %>% 
  dplyr::group_by(diasen_tratamiento, motivode_egreso) %>% 
  dplyr::summarise(n=n())


alta_ter_dias_tr_18_64 %>% 
  dplyr::filter(!is.na(diasen_tratamiento)) %>% 
  dplyr::group_by(diasen_tratamiento) %>% 
  dplyr::mutate(perc=n/sum(n)) %>% 
  #dplyr::mutate(diasen_tratamiento=ifelse(diasen_tratamiento=="[-345,  34)","[0,  34)",diasen_tratamiento))
  dplyr::mutate(diasen_tratamiento = plyr::revalue(diasen_tratamiento, c("[-345,  34)" = "[0,  34)"))) %>% 
ggplot(aes(x = diasen_tratamiento, y = perc, fill = motivode_egreso, group = motivode_egreso)) +
  geom_area(position = "stack", alpha = 0.6) +
  labs(
   # title = "Treatment Duration and Number of Patients (18-64)",
    x = "Days in Treatment",
    y = "Proportion of Patients",
    fill = "Therapeutic discharge (True/False)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  scale_fill_manual(values = c("TRUE" = "gray20", "FALSE" = "gray70"), name = "Therapeutic discharge")

ggsave(paste0(getwd(),"/_figs/diasentto_completa_base.png"),width = 7, height = 5, dpi = 600)

age_int_df<-
cbind.data.frame(tramo = c("18-29", "18-30", "18-31", "18-32", 
"18-33", "18-34", "18-35", "18-36", "18-37", "18-38", "18-39", 
"18-40", "18-41", "18-42", "18-43", "18-44", "18-45", "18-46", 
"18-47", "18-48", "18-49", "18-50", "18-51", "18-52", "18-53", 
"18-54", "18-55", "18-56", "18-57", "18-58", "18-59", "18-60", 
"18-61", "18-62", "18-63", "18-64", "18-65"), `Age intervals` = 29:65, 
`Total Patients` = c(18853L, 22268L, 25950L, 29522L, 33186L, 
36832L, 40583L, 44398L, 48509L, 52392L, 55994L, 59190L, 62304L, 
65017L, 67857L, 70725L, 73449L, 76141L, 78802L, 81204L, 83396L, 
85521L, 87495L, 89313L, 91017L, 92713L, 94224L, 95679L, 96968L, 
98104L, 99200L, 100194L, 101075L, 101821L, 102499L, 103074L, 
103513L), Deaths = c(381L, 446L, 526L, 595L, 695L, 781L, 
870L, 978L, 1079L, 1174L, 1271L, 1397L, 1500L, 1592L, 1701L, 
1824L, 1968L, 2105L, 2215L, 2361L, 2493L, 2642L, 2745L, 2857L, 
2994L, 3157L, 3310L, 3435L, 3562L, 3673L, 3792L, 3890L, 3989L, 
4081L, 4157L, 4226L, 4285L), min = c(-345, -345, -345, -345, 
-345, -345, -345, -345, -345, -345, -345, -345, -345, -345, 
-345, -345, -345, -345, -345, -345, -345, -345, -345, -345, 
-345, -345, -345, -345, -345, -345, -345, -345, -345, -345, 
-345, -345, -345), p025 = c(14.225, 15, 15, 15, 16, 16, 16, 
16, 16, 16, 17, 17, 17, 17, 17.8500000000001, 18, 18, 18, 
18, 18, 19, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 20, 
20, 20, 20, 20), p25 = c(79, 79, 80, 81, 81, 82, 82, 83, 
83, 84, 84, 85, 85, 85, 86, 86, 87, 87, 88, 88, 89, 89, 89, 
90, 90, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92), 
mean = c(189.937665782493, 191.415873158462, 193.214384828862, 
194.921296923702, 195.623933685004, 196.76412275937, 197.745452304659, 
198.979315938894, 200.052399315365, 201.574671605315, 203.147379327593, 
204.677110795647, 206.109771369854, 207.459758442957, 208.613650770251, 
210.264872280841, 211.414603252431, 212.49613727139, 213.985540730961, 
215.073261472128, 216.501565367591, 217.570434314723, 218.60735845605, 
219.71539616048, 220.762886597938, 221.825964049114, 223.111940377735, 
224.029096269655, 224.898078133221, 225.611197781245, 226.398838334947, 
227.28628621633, 227.989836010411, 228.570338058888, 229.248172546723, 
229.824310947205, 230.271407725239), sd = c(173.45142787871, 
176.257221450586, 179.536376460692, 181.221543735, 181.963807445358, 
184.567253270521, 185.07296300083, 184.638913869004, 185.814498789387, 
187.242150669369, 190.644626938893, 192.918346753975, 193.791664253145, 
195.332820077457, 196.045250307291, 199.083926362439, 199.379280570056, 
200.081659725025, 201.131810541333, 201.634505756753, 203.296333990504, 
204.284930358146, 204.786148093574, 205.757821134602, 206.300044002323, 
207.632575945591, 209.064416548844, 210.027312998452, 210.720406851207, 
211.269428765599, 211.939485346767, 213.041956591291, 213.783146506332, 
214.209781932986, 215.026354600604, 215.464529347802, 216.146293081989
), p50 = c(143, 145, 146, 147, 147, 148, 148, 149, 150, 151, 
152, 153, 154, 155, 156, 157, 158, 160, 161, 162, 162, 163, 
164, 165, 167, 167, 168, 168, 169, 170, 170, 171, 171, 172, 
172, 173, 174), p75 = c(251, 252, 254, 256, 258, 259, 261, 
263, 264, 266, 268, 270, 272, 273, 274, 276, 278, 280, 282, 
285, 287, 288, 290, 292, 294, 295, 296, 298, 299, 301, 301, 
302, 302, 303, 304, 305, 305), p975 = c(608.774999999998, 
615, 619, 624, 624, 626, 629, 634, 638, 645, 650, 656, 661, 
665, 667, 672, 676.474999999991, 681, 686, 692, 697, 700, 
703, 707, 711, 714, 719, 721, 723, 725, 728, 731, 733, 736, 
738, 741, 743), max = c(3324, 3324, 3324, 3324, 3324, 3324, 
3324, 3324, 3324, 3324, 3324, 3888, 3888, 3906, 3906, 3906, 
3906, 3906, 4056, 4056, 4056, 4056, 4056, 4056, 4056, 4056, 
4056, 4056, 4056, 4056, 4056, 4056, 4056, 4056, 4056, 4056, 
4056), mean_n_trts = c(1.46743051135158, 1.48169937575785, 
1.49485529307488, 1.50299786592595, 1.51140575561248, 1.51953517417393, 
1.52353260066039, 1.52899970718742, 1.53253071658283, 1.53654253593174, 
1.53890664904542, 1.53908665461488, 1.53962088502961, 1.53926725729051, 
1.53914171186041, 1.53783722640122, 1.53729168935846, 1.53639348568427, 
1.53585614395756, 1.53466004950556, 1.53290964686132, 1.53136108512629, 
1.53036779664891, 1.52917860981727, 1.52733585303683, 1.52567089481405, 
1.52401218386169, 1.52291017788833, 1.52130106118576, 1.51968849066797, 
1.51845280698394, 1.51668280219177, 1.5155826424204, 1.51419171086231, 
1.51308318211087, 1.51202545768533, 1.51111948373135), p75_n_trts = c(2, 
   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), p975_n_trts = c(4, 
4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), max_n_trts = c(12L, 
12L, 12L, 12L, 12L, 12L, 15L, 98L, 98L, 98L, 98L, 98L, 98L, 
98L, 98L, 98L, 98L, 98L, 98L, 98L, 98L, 98L, 98L, 98L, 98L, 
98L, 98L, 98L, 98L, 98L, 98L, 98L, 98L, 98L, 98L, 98L, 98L
), Percentage = c("2.0%", "2.0%", "2.0%", "2.0%", "2.1%", 
"2.1%", "2.1%", "2.2%", "2.2%", "2.2%", "2.3%", "2.4%", "2.4%", 
"2.4%", "2.5%", "2.6%", "2.7%", "2.8%", "2.8%", "2.9%", "3.0%", 
"3.1%", "3.1%", "3.2%", "3.3%", "3.4%", "3.5%", "3.6%", "3.7%", 
"3.7%", "3.8%", "3.9%", "3.9%", "4.0%", "4.1%", "4.1%", "4.1%"
))
invisible("Statistics")
slice(age_int_df,(length(age_int_df$`Age intervals`)-1):length(age_int_df$`Age intervals`))


# Concept, length of stay -------------------------------------------------------


par(mfrow=c(1,3))
time_in_treatment <- seq(0, 10, length.out = 100)
u_shaped_probability <- (-(time_in_treatment - 5)^2 + 25) / 25
time_in_treatment <- seq(0, 3, length.out = 100)
plot(time_in_treatment, u_shaped_probability, type = 'l', col = 'black',
     main = 'Inverse U-shaped Probability',
     xlab = '', ylab = 'Probability')
# Linear Probability
time_in_treatment <- seq(0, 3, length.out = 100)
linear_probability <- 1 - (0.1 * time_in_treatment)
plot(time_in_treatment, sort(linear_probability), type = 'l', col = 'black',
     main = 'Linear Probability',
     xlab = 'Time in Treatment', ylab = '')
# Logarithmic Probability
time_in_treatment <- seq(0.1, 3, length.out = 100) # starting from 0.1 to avoid log(0)
logarithmic_values <- log(time_in_treatment)
logarithmic_probability <- (logarithmic_values - min(logarithmic_values)) / (max(logarithmic_values) - min(logarithmic_values))
plot(time_in_treatment, logarithmic_probability, type = 'l', col = 'black',
     main = 'Logarithmic Probability',
     xlab = '', ylab = '')


filename0 <- paste0(getwd(),"/_figs/fig_tesis_prob_tiempo_tto.png")

png(filename0, width = 6, height = 3.89, units = 'in', res = 600)

par(mfrow=c(1,3))
time_in_treatment <- seq(0, 10, length.out = 100)
u_shaped_probability <- (-(time_in_treatment - 5)^2 + 25) / 25
time_in_treatment <- seq(0, 3, length.out = 100)
plot(time_in_treatment, u_shaped_probability, type = 'l', col = 'black',
     main = 'Probabilidad U-inversa',
     xlab = '', ylab = 'Probabilidad')
# Linear Probability
time_in_treatment <- seq(0, 3, length.out = 100)
linear_probability <- 1 - (0.1 * time_in_treatment)
plot(time_in_treatment, sort(linear_probability), type = 'l', col = 'black',
     main = 'Probabildiad lineal',
     xlab = 'Tiempo en tratamiento', ylab = '')
# Logarithmic Probability
time_in_treatment <- seq(0.1, 3, length.out = 100) # starting from 0.1 to avoid log(0)
logarithmic_values <- log(time_in_treatment)
logarithmic_probability <- (logarithmic_values - min(logarithmic_values)) / (max(logarithmic_values) - min(logarithmic_values))
plot(time_in_treatment, logarithmic_probability, type = 'l', col = 'black',
     main = 'Probabilidad logarítmica',
     xlab = '', ylab = '')
dev.off()

# Hospitalizaciones -------------------------------------------------------

  
X2023_11_07_DatosEgresosHosp_encrip <- 
  read.delim("C:/Users/CISS Fondecyt/Mi unidad/Alvacast/SISTRAT 2023/data/20231107_egres_hosp/2023-11-07  DatosEgresosHosp_encrip.csv", sep="~")

dtX2023_11_07_DatosEgresosHosp_encrip<-data.table::data.table(janitor::clean_names(X2023_11_07_DatosEgresosHosp_encrip))

unique_runs <- dtX2023_11_07_DatosEgresosHosp_encrip[, .SD[1], by = run]


joined_dt <- CONS_C1_2010_234[unique_runs, on = .(hash_key = run), by = .EACHI]
joined_dt2 <- CONS_C1_2010_234[dtX2023_11_07_DatosEgresosHosp_encrip, on = .(hash_key = run), allow.cartesian = TRUE]

invisible("Rut unicos")
length(unique(CONS_C1_2010_234$hash_key))
#106307
invisible("Rut en hospitalizaciones")
joined_dt %>% dplyr::filter(!is.na(tipo_centro)) %>% distinct(hash_key) %>% nrow()
#67328
#63.33% tiene registros

table(joined_dt$glosa_diag1) %>% 
  data.frame() %>% 
  arrange(desc(Freq))

## recuento palabras -------------------------------------------------------
if(!require(tm)){install.packages("tm")} 
if(!require(wordcloud2)){install.packages("wordcloud2")}
if(!require(wordcloud)){install.packages("wordcloud")}

corpus <- Corpus(VectorSource(joined_dt$glosa_diag1)) # formato de texto
d  <- tm_map(corpus, tolower)
d  <- tm_map(d, stripWhitespace)
d <- tm_map(d, removePunctuation)
d <- tm_map(d, removeNumbers)
d <- tm_map(d, removeWords, stopwords("spanish"))
d <- tm_map(d, removeWords, "menos")
#d <- tm_map(d, removeWords, c("usted", "pues", "tal", "tan", "así", "dijo", "cómo", "sino", "entonces", "aunque", "don", "doña","hacia","mayor","algún","cada","tambien","ello","mostrara","sólo", "ser","tener","manera","actualmente","mejor","casos","trabajo","parte","contar",""))
tdm <- TermDocumentMatrix(d)
m <- as.matrix(tdm) #lo vuelve una matriz
v <- sort(rowSums(m),decreasing=TRUE) #lo ordena y suma
df <- data.frame(word = names(v),freq=v) # lo nombra y le da formato de data.frame
#findFreqTerms(tdm)
#
#https://rpubs.com/brandonkopp/creating-word-clouds-in-r
wordcloud::wordcloud(words = df$word, freq = df$freq, 
                     max.words=200, random.order=FALSE, rot.per=0.35, 
                     colors=brewer.pal(8, "Greys"))

objs<-list(df= df, d=d, joined_dt=joined_dt)
invisible("muy grande, no lo hace")


save.image(objs, "word_count.RData")
#save(file="word_count.RData", list= c("df", "d", "joined_dt"))

## Figura pacientes hipotéticos -------------------------------------------------------


library(ggplot2)
set.seed(2125)
# Create data
data <- data.frame(
  y=abs(rpois(1:250,15)),
  y2=abs(rpois(1:250,15))
) %>% 
  dplyr::filter(y2>y, y>=8, y2<=24) %>%
  dplyr::mutate(Paciente=row_number()) %>% 
  #filtrar tratamientos más largos que 3 años
  dplyr::mutate(diff_treat=y2-y) %>% 
  dplyr::filter(diff_treat<=3)
for (i in 1:nrow(data)){
  data$y3[i]<-base::sample(x=seq(from=data$y2[i]+1,to=22),1)
  data$y3[i]<-ifelse(data$y3[i]<=data$y2[i],22,data$y3[i])
  data$y3[i]<-ifelse(data$y3[i]>=25,24,data$y3[i])
  data$y4[i]<-ifelse(!is.na(data$y3[i]),base::sample(x=seq(from=data$y3[i]+1,to=24),1),24)
  data$y4[i]<-ifelse(data$y4[i]<=data$y3[i],24,data$y4[i])
  data$y4[i]<-ifelse(data$y4[i]>=25,24,data$y4[i])
}
set.seed(2125)
pac_aleatorio1<-sample(1:max(data$Paciente),40)
pac_aleatorio2<-setdiff(sample(1:max(data$Paciente),40), pac_aleatorio1)
# Horizontal version, antes era 1985
end_plot<-22

#2023-11-07
data_mod<-
data %>%
  dplyr::mutate(y42 = y3 + 3, resta = y4 - y3) %>% 
  dplyr::mutate(y4=ifelse(resta>3,y42,y4)) 
  #dplyr::mutate(y4= dplyr::case_when(resta > 3 ~ y42, TRUE~ y4))

fig_trans<-ggplot(data_mod) 
###>=10
#datos de tratamiento en periodo de seguimiento, posterior al 2010
fig_trans<-try(fig_trans+geom_segment(data=dplyr::filter(data_mod,y>=10,y2<=end_plot), aes(x=Paciente, xend=Paciente, y=y, yend=y2), color="gray15", alpha=.6, size=.8))
#datos de tratamiento en periodo de seguimiento, exceden el seguiemiento. linea entera hasta el 2020
fig_trans<-try(fig_trans+ geom_segment(data=dplyr::filter(data_mod,y>=10,y2>end_plot), aes(x=Paciente, xend=Paciente, y=y, yend=end_plot), color="gray15", alpha=.6, size=.8))
#datos de tratamiento en periodo de seguimiento, exceden el seguiemiento. linea entrecortada despues del 2020
fig_trans<-try(fig_trans+geom_segment(data=dplyr::filter(data_mod,y>=10,y2>end_plot), aes(x=Paciente, xend=Paciente, y=end_plot, yend=y2), color="gray15", alpha=.6, linetype="dotted", size=.8))
###<10
#datos de tratamiento en periodo de seguimiento, exceden el seguiemiento. linea entrecortada despues del 2020
fig_trans<-try(fig_trans+geom_segment(data=dplyr::filter(data_mod,y<10), aes(x=Paciente, xend=Paciente, y=y, yend=10), color="gray15", alpha=.6, linetype="dotted", size=.8))
#datos de tratamiento en periodo de seguimiento, menos de 2010 pero sólo y1
fig_trans<-try(fig_trans+geom_segment(data=dplyr::filter(data_mod,y<10,y2>=10,y2<=end_plot), aes(x=Paciente, xend=Paciente, y=10, yend=y2), color="gray15", alpha=.6,  size=.8))
#datos de tratamiento en periodo de seguimiento, exceden el seguiemiento. linea entera hasta el 2020
# fig_trans<-try(fig_trans+geom_segment(data=dplyr::filter(data,y<10,y2>end_plot), aes(x=Paciente, xend=Paciente, y=10, yend=end_plot), color="#21177A", alpha=.6, size=.8))
# CONTACTO: VICTIMARIO: debo saber si y3 está dentro o no del seguimiento
###>=10
#datos de justicia en periodo de seguimiento, linea solida
fig_trans<-try(fig_trans+geom_segment(data=dplyr::filter(data_mod,y3<=end_plot, y4<=end_plot, Paciente%in% pac_aleatorio1), aes(x=Paciente, xend=Paciente, y=y3, yend=y4), color="gray70", alpha=.6, size=.8))
#datos de justicia en periodo de seguimiento, exceden el seguiemiento pero parten en él. linea solida hasta el 2020
fig_trans<-try(fig_trans+geom_segment(data=dplyr::filter(data_mod,y3<=end_plot, y4>end_plot, Paciente%in% pac_aleatorio1), aes(x=Paciente, xend=Paciente, y=y3, yend=end_plot), color="gray70", alpha=.6, size=.8))
#si estoy dentro de los datos en y, peroen end plot no, tengo q hacer el interlineado afuera
fig_trans<-try(fig_trans+geom_segment(data=dplyr::filter(data_mod,y3<=end_plot, y4>end_plot, Paciente%in% pac_aleatorio1), aes(x=Paciente, xend=Paciente, y=end_plot, yend=y4), color="gray70", alpha=.6,linetype="dotted",  size=.8))
#datos de tratamiento en periodo de seguimiento, exceden el seguiemiento. linea entrecortada despues del 2020
fig_trans<-try(fig_trans+geom_segment(data=dplyr::filter(data_mod,y3>end_plot, y4>end_plot, Paciente%in% pac_aleatorio1), aes(x=Paciente, xend=Paciente, y=y3, yend=y4), color="gray70", alpha=.6, linetype="dotted", size=.8))
fig_trans_final<-
  #Puntos
  fig_trans+
  #Tratamientos completados
  geom_point(data=dplyr::filter(data_mod,Paciente%in% sample(c(pac_aleatorio1,pac_aleatorio2),15)), aes(x=Paciente, y=y2), color="gray15", size=3, alpha=.6) +
  #Victimario
  geom_point(data=dplyr::filter(data_mod,Paciente%in% pac_aleatorio1), aes(x=Paciente, y=y3), color="gray15", size=3, alpha=.6, shape=15)+
  #víctima (no necesita tiempo de seguimiento)
  geom_point(data=dplyr::filter(data_mod,Paciente%in% pac_aleatorio2), aes(x=Paciente, y=y3), color="gray15", size=3, alpha=.6, shape=17)+
  theme_light() +
  coord_flip() +
  #fechas donde yo tomo gente
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=10, ymax=10.5,
           alpha = .2, fill="gray60")+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=21.5, ymax=22,
           alpha = .2, fill="gray60")+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )+
  scale_x_continuous(breaks=seq(1,max(data$Paciente),by=10))+
  scale_y_continuous(breaks=seq(min(data$y),max(data$y4),by=2), labels=seq(min(data$y),max(data$y4),by=2)+2000)+
  labs(y="Time of follow-up (in years)", x="Individuals (ID)")+
  #labs(y="Follow-up (in quarters)",x="Individual(ID)", caption="Note. Dot= Complete treatment;\nSquare= Contact w/ justice system (imputed);\nTriangle= Contact w/ justice system (victim);\nBlue line= Time in treatment;\nOrange line= Time in conctact w/justice system;\nShared area=Follow-up window")+
  theme(plot.caption = element_text(hjust = 0, face= "italic"))
ggsave(paste0(getwd(),"/_figs/situacion_hipotética2.png"),fig_trans_final,width = 7, height = 5, dpi = 600)
fig_trans_final

fig_trans_final_esp<-
  #Puntos
  fig_trans+
  #Tratamientos completados
  geom_point(data=dplyr::filter(data_mod,Paciente%in% sample(c(pac_aleatorio1,pac_aleatorio2),15)), aes(x=Paciente, y=y2), color="gray15", size=3, alpha=.6) +
  #Victimario
  geom_point(data=dplyr::filter(data_mod,Paciente%in% pac_aleatorio1), aes(x=Paciente, y=y3), color="gray15", size=3, alpha=.6, shape=15)+
  #víctima (no necesita tiempo de seguimiento)
  geom_point(data=dplyr::filter(data_mod,Paciente%in% pac_aleatorio2), aes(x=Paciente, y=y3), color="gray15", size=3, alpha=.6, shape=17)+
  theme_light() +
  coord_flip() +
  #fechas donde yo tomo gente
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=10, ymax=10.5,
           alpha = .2, fill="gray60")+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=21.5, ymax=22,
           alpha = .2, fill="gray60")+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )+
  scale_x_continuous(breaks=seq(1,max(data$Paciente),by=10))+
  scale_y_continuous(breaks=seq(min(data$y),max(data$y4),by=2), labels=seq(min(data$y),max(data$y4),by=2)+2000)+
  labs(y="Tiempo de seguimiento (en años)", x="Individuos (ID)")+
  #labs(y="Follow-up (in quarters)",x="Individual(ID)", caption="Note. Dot= Complete treatment;\nSquare= Contact w/ justice system (imputed);\nTriangle= Contact w/ justice system (victim);\nBlue line= Time in treatment;\nOrange line= Time in conctact w/justice system;\nShared area=Follow-up window")+
  theme(plot.caption = element_text(hjust = 0, face= "italic"))
ggsave(paste0(getwd(),"/_figs/situacion_hipotética2_esp.png"),fig_trans_final_esp,width = 7, height = 5, dpi = 600)
 
# La figura muestra distintas trayectorias de la cohorte. Incluye personas con y sin contactos con el sistema judicial. La entrada de paciente a la cohorte retrospectiva empieza a la fecha en que es admitido por primera vez en un tratamiento por uso de sustancias enn el período de 2010-2019 y figura en las listas anuales del SENDA al 2010 al 2019 (independientemente si tienen tratamientos previos).
# 
# Punto= Completa tratamiento;Cuadrado= Contacto con el sistema judicial como imputado;
#Triángulo= Contacto con el sistema judicial como víctima;Area sombreada: Ventana de seguimiento;
#Línea Naranja=Tiempo de contacto con el sistema judicial;Azul=Tiempo en tratamiento
# 
# Censura administrativa: entrega información SENDA (2019-11-13),
# 
# Otra censura ocurre después que se produce un evento de resultado competidor
# Cuando un paciente deja la cohorte sin experimentar resultados (ej, si se va fuera del país).
# Eventuales competidores: tiempo en tratamiento (especialmente residenciales) o una sentencia aunque debemos ver si es posible conocer este tiempo de censura


#file:///E:/Mi%20unidad/Alvacast/SISTRAT%202022%20(github)/pres_2022jun_5.html?panelset1=regional2#p6


## Figura modelo multiestado  -------------------------------------------------------

library(etm)

filename <- paste0(getwd(),"/_figs/transmat_tesis.png")

png(filename, width = 7, height = 5, units = 'in', res = 600)


#Definimos la matriz de transicion
states_trans<-c("Treatment\ncompletion", "Readmission", "Death")

trans_matrix <- matrix(c(NA, 1, 2,
                         NA, NA, 3,
                         NA, NA, NA),
                       nrow=3, ncol=3,byrow=TRUE,dimnames=list(from=states_trans,to=states_trans))

#generar una figura
Epi::boxes.Lexis(trans_matrix, wmult=1.3, hmult=1.3, cex=.8,
                 boxpos = list(y = c(25,75,25),
                               x = 20+(1:3)*(20)-10),
                 txt.arr=c(expression("1) " *lambda['12']),
                           expression("2) " *lambda['13']),
                           expression("3) " *lambda['23'])
                 ))
#title(sub = paste0("No recurring states;Absorbing state: Offending ")) ## internal titles

#Definimos un vector de los estados y transiciones
states_trans_lab<-paste0(1:3,") ",states_trans)
attr(states_trans_lab,"names")<-1:3

dev.off()


library(etm)

filename <- paste0(getwd(),"/_figs/transmat_tesis_esp.png")

png(filename, width = 7, height = 5, units = 'in', res = 600)


#Definimos la matriz de transicion
states_trans<-c("Completa\ntratamiento", "Readmisión", "Fallecimiento")

trans_matrix <- matrix(c(NA, 1, 2,
                         NA, NA, 3,
                         NA, NA, NA),
                       nrow=3, ncol=3,byrow=TRUE,dimnames=list(from=states_trans,to=states_trans))

#generar una figura
Epi::boxes.Lexis(trans_matrix, wmult=1.3, hmult=1.3, cex=.8,
                 boxpos = list(y = c(25,75,25),
                               x = 20+(1:3)*(20)-10),
                 txt.arr=c(expression("1) " *lambda['12']),
                           expression("2) " *lambda['13']),
                           expression("3) " *lambda['23'])
                 ))
#title(sub = paste0("No recurring states;Absorbing state: Offending ")) ## internal titles

#Definimos un vector de los estados y transiciones
states_trans_lab<-paste0(1:3,") ",states_trans)
attr(states_trans_lab,"names")<-1:3

dev.off()