require(pomp)
library(foreach)
library(gridExtra)
library(magrittr)
require(ggplot2)
library(reshape2)
require(dplyr)

require(plyr)

source("code/data_hystolytica.R")
source("code/simulate_histolytica.R")
source("code/pomp_histolytica_CDMX_mothyRainfall.R")
source("code/pomp_histolytica_CDMX.R")


miff2_IZ<-pomp_estimation_monthy(covar_pomp = caovariable_IZ,Nsim = 2)
miff2_IZ_dr<-pomp_estimation_daily(covar_pomp = caovariable_IZ,Nsim = 50)
saveRDS(object = miff2_IZ,file = "histolitica_miff_output_Iztapalapa")
plot_Unobserved_state(miff2_IZ[[1]],save_image = F,name_fig = "Iztapalapa")

miff2_TLP<-pomp_estimation_monthy(covar_pomp = caovariable_TLP,Nsim = 2)
saveRDS(object = miff2_TLP,file = "histolitica_miff_output_Tlalpan")
plot_Unobserved_state(miff2_TLP[[1]],save_image = TRUE,name_fig = "Tlalplan")

miff2_BJ<-pomp_estimation_monthy(covar_pomp = caovariable_BJ,Nsim = 50)
miff2_BJ_dr<-pomp_estimation_daily(covar_pomp = caovariable_BJ,Nsim = 50)
plot_Unobserved_state(miff2_BJ_dr[[1]],save_image = F,name_fig = "Benito_Juarez")
saveRDS(object = miff2_BJ,file = "histolitica_miff_output_BenitoJuarez")

miff2_AO<-pomp_estimation_monthy(covar_pomp = caovariable_AO,Nsim = 50)
saveRDS(object = miff2_AO,file = "histolitica_miff_output_AlvaroObregon")

miff2_MA<-pomp_estimation_monthy(covar_pomp = caovariable_MA,Nsim = 50)
plot_Unobserved_state(miff2_MA[[1]],save_image = F,name_fig = "MilpaAlta")
saveRDS(object = miff2_IZ,file = "histolitica_miff_output_Iztapalapa")


