#profiling
require(tidyverse)
miff2=readRDS(file = "best_estimated_object_IZ")
coef(miff2)[17]<-2854437519
theta=unlist(coef(miff2))
estpars <- setdiff(names(theta),c("sigma_observation",
                                  "Rain_max",
                                  "sigma",
                                  "d",
                                  "N_0",
                                  "s",
                                  "b",
                                  "g",
                                  "w",
                                  "alpha",
                                  "cyst_pp",
                                  "C_D0",
                                  "C_E0",
                                  "delta",
                                  "rho"))

theta.t <- partrans(miff2,theta,"toEst")
theta.nt <- partrans(miff2,theta.t,"fromEst")


theta.t.hi <- theta.t.lo <- theta.t
theta.t.lo[estpars] <- theta.t[estpars]-log(1)
theta.t.hi[estpars] <- theta.t[estpars]+log(1)

profileDesign(
  beta_E=seq(from=log(0.000001),to=log(0.2),length=30),
  lower=theta.t.lo,
  upper=theta.t.hi,
  nprof=40
) -> pd_2
pd_2 <- as.data.frame(pd_2[,-6])
pd_2 <- as.data.frame(t(partrans(miff2,t(pd_2),"fromEst")))
pairs(~beta_E+beta_E+v_r+sigma+g+w+E_0,data=pd_2)
head(pd_2)
##################################################################################

library(plyr)
foreach (p=iterators::iter(pd_2,"row"),
         .combine=rbind,
         .errorhandling="remove",
         .inorder=FALSE,
         .packages=c("pomp","magrittr","reshape2","plyr"),
         .export="parus"
) %dopar%{
  tic <- Sys.time()
  miff2 %>% 
    
    mif2(params=unlist(p),Nmif=50,Np=1000,
         cooling.fraction.50=0.5,
         cooling.type="geometric",
         rw.sd=rdd_e) %>%
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
} %>%   arrange(beta_E,-loglik) -> beta_E_prof_d

saveRDS(object = beta_E_prof_d,file = "first_beta_E_profile_exploration_zoom")
pairs(~loglik+beta_E+
        sigma_process+
        sigma_observation+
        beta_D+
        delta+
        v_r+
        C_E0+
        C_D0+
      cyst_pp,
      data=beta_E_prof_d,subset=loglik>max(loglik)-100)

##################################################################
#filter top 20 estimations
beta_E_prof_d %>%
  mutate(beta_E=exp(signif(log(beta_E),5))) %>%
  group_by(beta_E) %>%
  filter(rank(-loglik)<=20) %>%
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
    
    mif2(params=unlist(p),Nmif=50,Np=1000,
         cooling.fraction.50=0.1,
         cooling.type="geometric",
         rw.sd=rdd_e) %>%
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
} %>%   arrange(beta_E,-loglik) -> beta_E_prof_e



 beta_E_prof_e %>%
  subset(nfail.max<3) %>%
  mutate(beta_E=exp(signif(log(beta_E),5))) %>%
  group_by(beta_E) %>%
  filter(rank(-loglik)<=2) %>%
  ungroup() -> beta_E_prof_f


 beta_E_profile=beta_E_prof_f %>%
  ggplot(aes(x=beta_E,y=loglik))+
  geom_jitter()+
  geom_smooth(method="loess",span = 0.3)+
  scale_y_continuous("Log likelihood")+
  theme_bw()

#+
#  scale_x_log10()
png(filename = "profile_beta_E.png",width = 14,height = 12,units = "cm",res=300)
beta_E_profile
dev.off()

saveRDS(object = beta_E_prof_f,file = "final_beta_E_profile_exploration_zoom")

pairs(~loglik+beta_E+
        sigma_process+
        sigma_observation+
        beta_D+
        delta+
        v_r+
        C_E0+
        C_D0+
        cyst_pp,
      data=beta_E_prof_d)

plot(beta_E_prof_d$C_E0,beta_E_prof_d$loglik)

beta_E_prof_d$C_E0[which.max(beta_E_prof_d$loglik)]
