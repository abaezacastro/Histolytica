################################################################################
library(foreach)
#Initial parameter values
fixed_params<-c(d=0.001282,N=1.8E6, cyst_pp=0.9,p3_b=0,vo=2.35,p2_b=1)#3000000/1.8E 
params <- c(sigPRO=0.01874, #noise
            rho=0.003,
            sigOBS=0.17,
            beta_0=0.01,
            p1_b=2.058164e-06,
            del=5,
            s=0.8,
            p1_v=0.04,
            S_0=0.15,
            E_0=0.05,
            I_0=0.5,
            C_E0=257*E7, #concentration of pathogens in water. measure as number of cysts in a volume of water 
            C_D0=257*E5,
            g=0.7,
            w=0.7
)

################################################################################
#particle filter
pf <- pfilter(histolytica_pomp_IZ,Np=1000,params=c(params,fixed_params),verbose=T)
logLik(pf)
plot(pf)


################################################################################
rdd<-rw.sd(sigPRO=0.0001,
           rho=0.00001,
           sigOBS=0.0001,
           beta_D=0.0001,
           beta_E=0.0001,
           gamma=0.1,
           
           del=0.0001,
           s=0.0001,
           p1_v=0.00001,
           w=0.00001,
           g=0.00001,
           S_0=ivp(0.0001),
           E_0=ivp(0.0001),
           I_0=ivp(0.0001),
           C_0=ivp(0.0001)
)


miff_test<-mif2(histolytica_pomp_IZ,
                Np=1000,
                Nmif=50,
                cooling.type="geometric",
                cooling.fraction.50=0.6,
                transform=TRUE,
                start=coef(pf),
                rw.sd=rdd,
                pred.mean=TRUE,
                filter.mean=TRUE,max.fail=500)

miff2_Iztapalapa<-continue(miff_test)

ll<-numeric(30)
for (i in 1:30){
  miff2_Iztapalapa<-mif2(miff2_Iztapalapa)
  ll[i]<-logLik(miff2_Iztapalapa)
  print(logLik(miff2_Iztapalapa))
  
}
plot.ts(ll)
logLik(miff2_Iztapalapa)
coef(miff2_Iztapalapa)

############################################################################################
#Benckmark 
nb_lik <- function(theta) {
  -sum(dnbinom(as.vector(obs(histolytica_pomp_IZ)),size=exp(theta[1]),prob=exp(theta[2]),log=TRUE))
} 
nb_mle <- optim(c(0,-5),nb_lik)
-nb_mle$value
############################################################################################

#saveRDS(object = miff2_Iztapalapa,file = "histolitica_miff_output_Iztapalapa")
#
#miff2_Iztapalapa<-readRDS("histolitica_miff_output_Iztapalapa")
pf <- pfilter(histolytica_pomp_IZ,Np=10000,params=coef(miff2_Iztapalapa),verbose=T)
logLik(pf)

pf <- pfilter(histolytica_pomp_IZ,Np=10000,params=coef(pf),verbose=T)
logLik(pf)


histolytica_pomp_IZ %>% 
  simulate(params=coef(miff2_Iztapalapa),nsim=400,as.data.frame=TRUE,include.data=TRUE) %>%
  subset(select=c(time,sim,Amebiasis)) %>%
  mutate(data=sim=="data") %>%
  ddply(~time+data,summarize,
        p=c(0.1,0.5,0.9),q=quantile(Amebiasis,prob=p,names=FALSE,na.rm=T)) %>%
  mutate(p=mapvalues(p,from=c(0.1,0.5,0.9),to=c("lo","med","hi")),
         data=mapvalues(data,from=c(TRUE,FALSE),to=c("data","simulation"))) %>%
  dcast(time+data~p,value.var='q') %>%
  ggplot(aes(x=time,y=med,color=data,fill=data,ymin=lo,ymax=hi))+
  geom_ribbon(alpha=I(0.2))+scale_fill_manual(values=c("green","black"), name="fill")->ggplot_msims

