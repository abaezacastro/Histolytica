################################################################################

#Initial parameter values
fixed_params<-c(b=0.0183,d=0.0058,N_0=1.8E6, delta=26,s=0.9,cyst_pp=0.4,Rain_max=max_rain_IZ)#3000000/1.8E 
params <- c(sigPRO=0.01874, #noise
            rho=0.004,#reporting rate
            sigOBS=0.17, #par noise
            beta_D=7.7,# probability of infection associated to the concentration of cysts in households
            beta_E=10.4,# probability of infection associated to the concentration of cysts in the environtment
            gamma=1.9, #the effect of rainfall
            sigma=0.3,
            alpha=1.0,
            v_r=15.3,
            v0=0.44,
            S_0=0.80,
            E_0=0.05,
            I_0=0.1,
            C_E0=0.05, #concentration of pathogens in water. measure as number of cysts in a volume of water 
            C_D0=0.05,
            g=2.69,
            w=1)




################################################################################
#particle filter
pf <- pfilter(histolytica_pomp,Np=1000,params=c(params,fixed_params),verbose=T)
logLik(pf)
plot(pf)


#load object from previous search
miff2_Iztapalapa<-readRDS("histolitica_miff_output_Iztapalapa")
################################################################################
#particle filter
pf <- pfilter(histolytica_pomp_IZ,Np=1000,params=coef(miff2_Iztapalapa),verbose=T)
logLik(pf)
plot(pf)


################################################################################


miff_test<-mif2(histolytica_pomp_IZ,
                Np=1000,
                Nmif=1,
                cooling.type="geometric",
                cooling.fraction.50=0.1,
                transform=TRUE,
                start=coef(pf),
                rw.sd=rdd,
                pred.mean=TRUE,
                filter.mean=TRUE,max.fail=500)

miff2_Iztapalapa<-continue(miff_test)

ll<-numeric(80)


for (i in 1:80){
  miff2_Iztapalapa<-mif2(miff2_Iztapalapa)
  ll[i]<-logLik(miff2_Iztapalapa)
  print(logLik(miff2_Iztapalapa))
  
}
plot.ts(ll)
logLik(miff2_Iztapalapa)
barplot((coef(miff2_Iztapalapa)/c(params,fixed_params))-1)
############################################################################################
#Benckmark 
nb_lik <- function(theta) {
  -sum(dnbinom(as.vector(obs(histolytica_pomp_IZ)),size=exp(theta[1]),prob=exp(theta[2]),log=TRUE))
} 
nb_mle <- optim(c(0,-5),nb_lik)
-nb_mle$value
############################################################################################

#saveRDS(object = miff2_Iztapalapa,file = "histolitica_miff_output_Iztapalapa")

