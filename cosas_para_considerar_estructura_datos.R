exposure = t,
family = "binomial",
link = "logit",
numerator = ~ factor(policonsumo) + edad_ini,
denominator = ~ v + factor(policonsumo) + edad_ini,
mod1b<- geem(dt~ t+ time+ factor(policonsumo)+ edad_ini, 
             
             id=id ,weights=ipw, data = data.frame(datos), family=poisson,

v1 <- (0.4*t1 + 0.78*dt1 + rnorm(1000, 0, sqrt(0.99))) + 5
#definir el confusor tiempo-dependiente v2 como una función de t2 y dt2
v2 <- (0.4*t2 + 0.78*dt2 + rnorm(1e3, 0, sqrt(0.55))) + 5
#definir el confusor tiempo-dependiente v3 como una función de t3 y dt3
v3 <- (0.4*t3 + 0.78*dt3 + rnorm(1e3, 0, sqrt(0.33))) + 5

             
where A is the exposure for subject i at time tij (time points range starting at k=0 to k=j).
The numerator contains the probability of the observed exposure at each time point (aik) conditioned on the observed exposure 
history of the previous time point (aik) and the observed non-time varying covariates (vi). 
The denominator contains the probability of the observed exposure at each time point conditioned 
on the observed exposure history of the previous time point (aik−1), the observed time-varying
covariates history at the current time point (cik), and the non-time varying covariates (vi).

numerador
A~ tk-1 + dt+ L1 + L2 --> si dt es en el presente (está en la función) pero también está en el denominador

#dt es en el presente (está en la función) pero también está en el denominador de la asignación de tratamiento

denominador
A~ 
  #define tme-varying confounder v1 as a function of t1 and d1
  
  #define tme-varying confounder v1 as a function of t1 and d1