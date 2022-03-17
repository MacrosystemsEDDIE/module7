NP_model <- function(PHYTO, DIN, TEMP, PAR, maxUptake){

  #STATES
  curr_PHYTO = PHYTO
  curr_DIN = DIN
  
  #PARAMETERS
  maxUptake = maxUptake #day-1; this is the only parameter we are tuning
  kspar=120 #uEinst m-2 s-1
  ksdin=0.5 #mmol m-3
  maxGrazing=1.0 # day-1
  ksphyto=1 #mmol N m-3
  pFaeces=0.3 #unitless
  mortalityRate=0.4 #(mmmolN m-3)-1 day-1
  excretionRate=0.1 #day-1
  mineralizationRate=0.1 #day-1
  Chl_Nratio = 1 #mg chl (mmolN)-1
  Q10 = 2  #unitless
  refTEMP = 20

  #DRIVERS
  PAR <- PAR
  TEMP <- TEMP 

  #FLUX EQUATIONS HERE
  Temp_effect = Q10^((TEMP-refTEMP)/10)
  N_Uptake <- maxUptake*PHYTO*(PAR/(PAR+kspar))*(DIN/(DIN+ksdin))*Temp_effect

  Mortality <- mortalityRate*PHYTO^2
  Mineralization <- mineralizationRate *
    Temp_effect

  #Convert from plankton biomass to Chlorophyll to compare to data
  Chlorophyll <- PHYTO^Chl_Nratio

  PHYTO = curr_PHYTO + N_Uptake - Mortality
  DIN = curr_DIN + Mortality - N_Uptake #+ NLOAD + Excretion

  return(list(PHYTO = PHYTO,
              DIN = DIN,
              maxUptake = maxUptake))   # the ordinary output variables
}
