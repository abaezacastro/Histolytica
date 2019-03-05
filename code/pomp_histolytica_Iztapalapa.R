

#####################################################
###########Files to create the pomp object###########
#####################################################
#load Pomp package
require(pomp)
################################################
#create snippets of  c++ code in R
################################################

#Skeleton of the model
histolytica.sklt<- Csnippet("
//dilution effect of rainfall on cyst concentration
double v_E=v0 + v_r * RR;
//force of infection
double foi=(beta_D * C_D) + beta_E *(1 +  gamma * (RR/Rain_max))* C_E;
//calcualte transitions

double rtfoi=  foi * S ;
double lostImrt = w * R ;
double pgrt = d*(N - S);
double infrt = del * E;
double expdert = d * E;
double infdert=d * I;
double recrt= g*I;
double recdr=d*R;
double cyst_prt_E = cyst_pp * s * (1 - sigma) * I * dt;
double cyst_prt_D = cyst_pp * s * (sigma) * I * dt;
double cyst_decay_E= v_E * C_E * dt;
double cyst_decay_D= v0 * C_D * dt;
                            
double rr_rt  =  rho * del * E; 

// Calculate equation step
DS =pgrt - rtfoi + lostImrt;
DE =rtfoi- expdert - infrt;
DI =infrt-infdert-recrt;
DR =recrt-recdr-lostImrt;
DC_D =cyst_prt_D-cyst_decay_D;
DC_E =cyst_prt_E-cyst_decay_E;
Dcases =rr_rt;
                            ")

##random_process
histolytica_rdpr<- Csnippet("
  // gamma white noise
  double dW = rgammawn(sigPRO,dt);
  //dilution effect of rainfall on cyst concentration
  double v_E=v0 + v_r * RR;
  //force of infection
 double foi=((beta_D * C_D) + beta_E *(1 +  gamma * (RR/Rain_max)) * C_E) *(dW/dt);

  //calcualte transitions
  
  double rtfoi= foi * S *dt;
  double lostImrt = w * R * dt;
  double pgrt = d * (N - S) * dt;
  double infrt = del * E * dt;
  double expdert = d * E * dt;
  double infdert= d * I * dt;
  double recrt= g * I * dt;
  double recdr= d * R * dt;
  double cyst_prt_E = cyst_pp * s * (1 - sigma) * I * dt;
  double cyst_prt_D = cyst_pp * s * (sigma) * I * dt;
  double cyst_decay_E= v_E * C_E * dt;
  double cyst_decay_D= v0 * C_D * dt;
  double rr_rt  =  rho * del * E*dt;
                            
  // Calculate equation step
     S += pgrt - rtfoi + lostImrt;
     E += rtfoi- expdert - infrt;
     I += infrt-infdert-recrt;
     R += recrt-recdr-lostImrt;
     C_E += cyst_prt_E-cyst_decay_E;
     C_D += cyst_prt_D-cyst_decay_D;
     cases += rr_rt; 
     W += (dW-dt)/sigPRO;")

#snippets that specify the measurement model (rmeasure and dmeasure)
############ rmeas #################

rmeas <- Csnippet("
  double size = 1.0/sigOBS/sigOBS;
                  Amebiasis = rnbinom_mu(size,cases);
                  ")

############ dmeas #################
dmeas <- Csnippet("
                  double size = 1.0/sigOBS/sigOBS;
                  
                  if (cases >= 0.0) {
                  lik = dnbinom_mu(Amebiasis,size,cases+0.1,1);
                  } else {
                  lik = 1e-18;
                  }                   
                  if (!give_log) lik = exp(lik);
                  ")


###########################################################################
init <- Csnippet("

  S = nearbyint(N*S_0);
  E = nearbyint(N*E_0);
  I = nearbyint(N*I_0);
  R = nearbyint(N*(1-S_0-E_0-I_0));
  C_D = C_D0;
  C_E = C_E0;
  cases=0;
  W=0;  
                  ")

toEst <- Csnippet("
              TsigPRO=logit(sigPRO);
              TsigOBS=logit(sigOBS) ;            
              Trho=logit(rho);
              Tbeta_D=log(beta_D);
              Tbeta_E =log(beta_E);
              Tgamma=log(gamma);
              Tv0  =log(v0);
              Tv_r  =log(v_r);
              Ts=logit(s);
              Tsigma=logit(sigma);
              Tw=log(w);
              Tg=log(g);
              TS_0=logit(S_0);
              TE_0=logit(E_0);
              TI_0=logit(I_0);
              TC_D0=log(C_D0);
              TC_E0=log(C_E0);
                  ")

#Tp2_b  =log(p2_b);
#Tp3_b  =log(p3_b);
#Tdel =log(del);
#Tcyst_pp=log(cyst_pp);
#Tvo=log(vo);


fromEst<-Csnippet("
  TsigPRO=expit(sigPRO);
  TsigOBS=expit(sigOBS);            
  Trho=expit(rho);
  Tbeta_D=exp(beta_D);
  Tbeta_E=exp(beta_E);
  Tgamma=exp(gamma);
  Tv0  =exp(v0);
  Tv_r  =exp(v_r);
  Ts=expit(s);
  Tsigma=expit(sigma);
  Tw=exp(w);
  Tg=exp(g);
  TS_0=expit(S_0);
  TE_0=expit(E_0);
  TI_0=expit(I_0);
  TC_E0=exp(C_E0);
  TC_D0=exp(C_D0);
")
rp_names <-c("sigPRO","rho","sigOBS","beta_E","beta_D","gamma","v0","v_r","s","sigma","w","g","del")
ivp_names <-c("S_0","E_0","I_0","C_D0","C_E0")
fp_names=c("d","N","cyst_pp","Rain_max")

################################################################################
histolytica_pomp_IZ <- pomp(data=dat_IZ,
     times="time",
     t0=with(dat_IZ,2*time[1]-time[2]),
     rprocess = euler.sim(step.fun = histolytica_rdpr,delta.t = 1/365),  #  
    skeleton=vectorfield(histolytica.sklt),
     initializer=init,
     statenames=c("S","E","I", "R","C_D","C_E","cases","W"),
     paramnames=c(rp_names,ivp_names,fp_names),
     zeronames = c("W","cases"),
     rmeasure=rmeas,
     dmeasure=dmeas,
    covar=covartable_IZ,
    tcovar="time",
    toEstimationScale=toEst, 
    fromEstimationScale=fromEst  
   )



