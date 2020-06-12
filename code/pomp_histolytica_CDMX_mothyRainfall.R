

#####################################################
###########Files to create the pomp object###########
#####################################################
#load Pomp package
################################################
#create snippets of  c++ code in R
################################################
pomp_estimation_monthy<-function(pomp_obj=NA,covar_pomp,Nsim=50,parameters=NA){
  J<-readRDS("./data/disease_data_mexicoCity")
  Population_CDMX2015<-read.csv(file = "./data/population_delegations.csv",header = F)
  j=1#Amebiasis cases
  i=covar_pomp[[2]]#municipality
  tss<-as.vector(J[i,j,c(4,5,8,1,9,7,6,2,12,11,10,3),1])
  for(y in 2:7){
    tss<-c(tss,as.vector(J[i,j,c(4,5,8,1,9,7,6,2,12,11,10,3),y]))
  }
  df <- data.frame(date=seq(from = 2005,
                                 to = 2012-1/12,
                                 length.out = length(tss)),
                   Amebiasis = tss)
  
  
  dat_pomp=data.frame(time=as.numeric(df$date),Amebiasis=df$Amebiasis)
#  dat_pomp=rbind(data.frame(time=2004.917,Amebiasis=NA),dat_pomp)
print(names(J[,1,1,1])[i])
print(as.character(Population_CDMX2015$V2[which(Population_CDMX2015$V1==i)]))
covar_pomp2<-covar_pomp[[1]]
#Skeleton of the model
histolytica.sklt<- Csnippet("
//dilution effect of rainfall on cyst concentration
double v_E=v0 + v_r * (RR/Rain_max);

//force of infection

double foi=((beta_D * C_D) + beta_E *(1 +  gamma * (RR/Rain_max)) * C_E);

//Population size
int N=S+E+I+R;

//calculate transitions

double rtfoi=  foi * S ;
double lost_immunity = w * R ;
double pop_growth_rate = b*N - d*S;
double EI_rate = delta * E;
double E_dead_rate = d * E;
double I_dead_rate = d * I;
double recovery_rate = g * I;
double R_dead_rate = d * R;
double cyst_prt_E = cyst_pp * s * (1-sigma) * (I/N);
double cyst_prt_D = cyst_pp * s * sigma * (I/N);
double cyst_decay_E = v_E * C_E;
double cyst_decay_D = v0 * C_D;
                            
double rr_rt  =  rho * (1-s) *EI_rate; 

// Calculate equation step
DS = pop_growth_rate - rtfoi + lost_immunity;
DE = rtfoi - EI_rate - E_dead_rate;
DI = EI_rate - recovery_rate - I_dead_rate;
DR = recovery_rate - lost_immunity - R_dead_rate;
DC_D = cyst_prt_D - cyst_decay_D;
DC_E = cyst_prt_E - cyst_decay_E;
Dcases = rr_rt;
                            ")

##random_process
histolytica_rdpr<- Csnippet("
  // gamma white noise
  double dW = rgammawn(sigma_process,dt);
  //dilution effect of rainfall on cyst concentration
  double v_E=v0 + v_r * (RR/Rain_max);
  //force of infection
  double foi=((beta_D * C_D) + beta_E *(1 +  gamma * (RR/Rain_max)) * C_E) *(dW/dt);
 //Population size
  int N=S+E+I+R;
//calcualte transitions
  
  double rtfoi= foi * S * dt;
  double lost_immunity = w * R * dt;
  double pop_growth_rate = (b * N - d * S) * dt;
  double EI_rate = delta * E * dt;
  double E_dead_rate = d * E * dt;
  double I_dead_rate = d * I * dt;
  double recovery_rate = g * I * dt;
  double R_dead_rate = d * R * dt;
  double cyst_prt_E = cyst_pp * s * (1- sigma) * (I/N) * dt;
  double cyst_prt_D = cyst_pp * s * sigma * (I/N) * dt;
  double cyst_decay_E = v_E * C_E * dt;
  double cyst_decay_D = v0 * C_D * dt;
  double rr_rt  =  rho * (1-s) * EI_rate;
                            
  // Calculate equation step
     S += pop_growth_rate - rtfoi + lost_immunity;
     E += rtfoi - EI_rate - E_dead_rate;
     I += EI_rate - recovery_rate - I_dead_rate;
     R += recovery_rate - lost_immunity  - R_dead_rate;
     C_E += cyst_prt_E - cyst_decay_E;
     C_D += cyst_prt_D - cyst_decay_D;
     cases += rr_rt; 
     W += (dW - dt) / sigma_process;")

#snippets that specify the measurement model (rmeasure and dmeasure)
############ rmeas #################

rmeas <- Csnippet("
  double size = 1.0/sigma_observation/sigma_observation;
                  Amebiasis = rnbinom_mu(size,cases);
                  ")

############ dmeas #################
dmeas <- Csnippet("
                  double size = 1.0/sigma_observation/sigma_observation;
                  
                  if (cases >= 0.0) {
                  lik = dnbinom_mu(Amebiasis,size,cases+0.1,1);
                  } else {
                  lik = 1e-18;
                  }                   
                  if (!give_log) lik = exp(lik);
                  ")


###########################################################################
init <- Csnippet("

  S = nearbyint(N_0*S_0);
  E = nearbyint(N_0*E_0);
  I = nearbyint(N_0*I_0);
  R = nearbyint(N_0*(1-S_0-E_0-I_0));
  C_D = C_D0;
  C_E = C_E0;
  cases=0;
  W=0;  
                ")
################################################################################
toEst <- Csnippet("
              T_sigma_process = log(sigma_process);
              T_sigma_observation = log(sigma_observation) ;            
              T_rho = log(rho);
              T_beta_D = log(beta_D);
              T_beta_E = log(beta_E);
              T_delta = log(delta);
              T_gamma =log(gamma);
              T_alpha=log(alpha);
              T_cyst_pp=log(cyst_pp);
              T_v0 = log(v0);
              T_v_r = log(v_r);
              T_sigma=logit(sigma);
              T_w=log(w);
              T_g=log(g);
              T_S_0=logit(S_0);
              T_E_0=logit(E_0);
              T_I_0=logit(I_0);
              T_C_D0=log(C_D0);
              T_C_E0=log(C_E0);
                  ")


################################################################################
fromEst<-Csnippet("
  sigma_process=exp(T_sigma_process);
  sigma_observation=exp(T_sigma_observation);            
  rho=exp(T_rho);
  beta_D=exp(T_beta_D);
  beta_E=exp(T_beta_E);
  delta=exp(T_delta);
  gamma=exp(T_gamma);
  alpha=exp(T_alpha);
  cyst_pp=exp(T_cyst_pp);
  v0=exp(T_v0);
  v_r=exp(T_v_r);
  sigma=expit(T_sigma);
  w=exp(T_w);
  g=exp(T_g);
  S_0=expit(T_S_0);
  E_0=expit(T_E_0);
  I_0=expit(T_I_0);
  C_E0=exp(T_C_E0);
  C_D0=exp(T_C_D0);
")

################################################################################
rp_names <-c("sigma_process","rho","sigma_observation","beta_D","beta_E","sigma","delta","gamma","alpha","v_r","v0","w","g")

ivp_names <-c("S_0","E_0","I_0","C_D0","C_E0")

fp_names=c("b","d","N_0","s","cyst_pp","Rain_max")

################################################################################
histolytica_pomp <- pomp(
              data=dat_pomp,
              times="time",
              t0=with(dat_pomp,2*time[1]-time[2]),
              rprocess = euler(step.fun = histolytica_rdpr,delta.t = 1/365),  #  
              skeleton=vectorfield(histolytica.sklt),
              rinit=init,
              statenames=c("S","E","I", "R","C_D","C_E","cases","W"),
              paramnames=c(rp_names,ivp_names,fp_names),
              accumvars = c("W","cases"),
              rmeasure=rmeas,
              dmeasure=dmeas,
              covar=covariate_table(time=covar_pomp2$tt,RR=covar_pomp2$RR,times ="time"),
              partrans =parameter_trans(toEst = toEst, fromEst = fromEst)  
   )
b0=0.0183
d0=0.0058
lamda=1+(b0-d0)
#print(Population_CDMX2015$V3[which(Population_CDMX2015$V1==i)])
P0=Population_CDMX2015$V3[which(Population_CDMX2015$V1==i)]/(lamda^10)
max_rain=max(covar_pomp2$RR,na.rm=T)

if(is.na(pomp_obj)){
fixed_params<-c(b=b0,d=d0,N_0=P0,s=0.91,
                cyst_pp=0.4,
                delta=26,
                Rain_max=max_rain)#3000000/1.8E 

#new_par_BJ<- c(sigma_process=0.0187, #noise
            # rho=0.04,#reporting rate
            # sigma_observation=0.169, #par noise
            # beta_D=5.1,# probability of infection associated to the concentration of cysts in households
            # beta_E=12,# probability of infection associated to the concentration of cysts in the environtment
            # gamma=45,
            # alpha=1,
            # v_r=117,#91,
            # v0=0.67,
            # g=2.6,
            # w=0.098,
            # sigma=0.28,
            # S_0=0.3,
            # E_0=0.0047,
            # I_0=0.1,
            # C_E0=0.042, #concentration of pathogens in water. measure as number of cysts in a volume of water 
            # C_D0=0.13)
            # 



#Initial_paramters=c(new_par_BJ,fixed_params)
miff2_IZb<-readRDS(file = "histolitica_miff_output_Iztapalapa")
Initial_paramters=coef(miff2_IZb)

}
else{
  Initial_paramters=coef(pomp_obj)
}
Initial_paramters[["N_0"]]<-P0
##############################################################################################
#pf <- pfilter(histolytica_pomp,Np=10000,params=Initial_paramters,verbose=T)

#########################################################################################
rdd_a<-rw.sd(sigma_process=0.0001,
             rho=0.0001,
             sigma_observation=0.0001,
             beta_D=0.001,
             beta_E=0.01,
             gamma=0.01,
             alpha=0,
             sigma=ivp(0.001),
             v_r=0.001,
             v0=0.001,
             w=0.001,
             g=0.001,
             S_0=ivp(0.01),
             E_0=ivp(0.01),
             I_0=ivp(0.01),
             C_D0=0.001,
             C_E0=0.001
)


rdd_b<-rw.sd(sigma_process=0.0001,
           rho=0.0001,
           sigma_observation=0.0001,
           beta_D=0.0001,
           beta_E=0.001,
           gamma=0.001,
           alpha=0,
           sigma=ivp(0.00001),
           v_r=0.0001,
           v0=0.0001,
           w=0.00001,
           g=0.00001,
           S_0=ivp(0.001),
           E_0=ivp(0.000001),
           I_0=ivp(0.001),
           C_D0=0.0001,
           C_E0=0.0001
)

rdd_c<-rw.sd(sigma_process=0.001,
             rho=0.00001,
             sigma_observation=0.001,
             beta_D=0.00001,
             beta_E=0.00001,
             gamma=0.0001,
             alpha=0,
             sigma=ivp(0.00001),
             v_r=0.0001,
             v0=0.0001,
             w=0.0001,
             g=0.0001,
             S_0=ivp(0.001),
             E_0=ivp(0.0001),
             I_0=ivp(0.0001),
             C_D0=0.00001,
             C_E0=0.00001
)



rdd_f<-rw.sd(sigma_process=0.001,
             rho=0.0001,
             sigma_observation=0.001,
             beta_D=0,
             beta_E=0.00001,
             gamma=0.001,
             alpha=0,
             sigma=ivp(0),
             v_r=0.0001,
             v0=0,
             w=0,
             g=0,
             S_0=ivp(0.001),
             E_0=ivp(0.001),
             I_0=ivp(0.001),
             C_D0=0.001,
             C_E0=0.001
)


Initial_paramters[["alpha"]]<-1


miff2<-mif2(histolytica_pomp,
                Np=5000,
                Nmif=1,
                cooling.type="geometric",
                cooling.fraction.50=0.5,
                params=Initial_paramters,#Initial_paramters,
                rw.sd=rdd_f,
                max.fail=500)

theta=unlist(coef(miff2))

theta.t <- partrans(miff2,theta,"toEst")
theta.nt <- partrans(miff2,theta.t,"fromEst")
estpars <- setdiff(names(theta),c("delta","sigma_process","sigma_observation",
                                  "alpha","w","g",
                                  "beta_D","sigma",
                                  "Rain_max","N_0",
                                  "b","d","cyst_pp",
                                  "v0","rho",
                                  "v_r","beta_E","gamma","s"))

theta.t.hi<- theta.t.lo<- theta.nt
theta.t.lo[estpars] <- theta.nt[estpars]-0.1*theta.nt[estpars]
theta.t.hi[estpars]<- theta.nt[estpars]+0.1*theta.nt[estpars]

runifDesign(
  lower= partrans(miff2,theta.t.lo,"toEst"),
  upper=partrans(miff2,theta.t.hi,"toEst"),
  nseq=20
) %>% as.data.frame() -> pr_1
pr_1 <- as.data.frame(pr_1)
pr_1 <- as.data.frame(t(partrans(miff2,t(pr_1),"fromEst")))



foreach (p=iterators::iter(pr_1,"row"),
         .combine=rbind,
         .errorhandling="remove",
         .inorder=FALSE,
         .packages=c("pomp","magrittr","reshape2","plyr"),
         .export="parus"
) %dopar%{
  tic <- Sys.time()
  miff2 %>% 
    
    mif2(params=unlist(p),
         Nmif=Nsim,
         Np=1000,
         cooling.fraction.50=0.5,
         cooling.type="geometric",
         rw.sd=rdd_f) %>%
    mif2() -> mf
  
  print(logLik(mf))
  pf <- replicate(10,pfilter(mf,Np=1000))  ## independent particle filters
  ll <- sapply(pf,logLik)
  ll <- logmeanexp(ll,se=TRUE)
  nfail <- sapply(pf,getElement,"nfail")  ## number of filtering failures
  
  toc <- Sys.time()
  etime <- toc-tic
  units(etime) <- "hours"
  
  data.frame(as.list(coef(mf)),
             loglik = ll[1],
             loglik.se = ll[2],
             nfail.min = min(nfail),
             nfail.max = max(nfail))
} -> random_designed_res
pairs(~loglik+beta_E+gamma+v_r,data=random_designed_res)

###########################################################################
#filter
random_designed_res %>%
  filter(rank(-loglik)<=10) %>%
  ungroup() %>%
  filter(nfail.max<3) -> pd_3





foreach (p=iterators::iter(pd_3,"row"),
         .combine=rbind,
         .errorhandling="remove",
         .inorder=FALSE,
         .packages=c("pomp","magrittr","reshape2","plyr"),
         .export="parus"
) %dopar%{
  tic <- Sys.time()
  miff2 %>% 
    
    mif2(params=unlist(p),Nmif=10,Np=1000,
         cooling.fraction.50=0.1,
         cooling.type="geometric",
         rw.sd=rdd_f) %>%
    mif2() -> mf
  
  pf <- replicate(10,pfilter(mf,Np=1000))  ## independent particle filters
  ll <- sapply(pf,logLik)
  ll <- logmeanexp(ll,se=TRUE)
  nfail <- sapply(pf,getElement,"nfail")  ## number of filtering failures
  
  toc <- Sys.time()
  etime <- toc-tic
  units(etime) <- "hours"
  
  data.frame(as.list(coef(mf)),
             loglik = ll[1],
             loglik.se = ll[2],
             nfail.min = min(nfail),
             nfail.max = max(nfail))
}  -> output

output %>%
  filter(rank(-loglik)<=1)->rr

miff2 %>% 
  
  mif2(params=unlist(rr)[1:24],Nmif=50,Np=1000,
       cooling.fraction.50=0.1,
       cooling.type="geometric",
       rw.sd=rdd_f) %>%
  mif2() -> mf

pairs(~loglik+beta_E+gamma+v_r,data=output)

#ll=numeric(Nsim)
#miff2=readRDS(file = "best_estimated_object_IZ")
#for (h in round(Nsim/2,0):Nsim){

#    miff2<-mif2(miff2,
#              Np=3000,
#              Nmif=1,
#              cooling.type="geometric",
#              cooling.fraction.50=0.5,
#              rw.sd=rdd_c,
#              max.fail=500)
  
#  ll[h]<-logLik(miff2)
#  print(c(h,logLik(miff2)))
rr<-list(mf,output)
 return(rr) 
}


#plot.ts(ll)

