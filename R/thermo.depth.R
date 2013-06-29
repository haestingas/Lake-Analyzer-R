#
# FindThermoDepth finds the thermocline depth from a temperature profile.
#
#
# Author: Luke Winslow <lawinslow@gmail.com>
# Adapted from FindThermoDepth.m in https://github.com/jread-usgs/Lake-Analyzer/
#
thermo.depth <- function(wtr, depths, Smin = 0.1, seasonal=TRUE){
  #argh, data structures in R!
  #lets just do this the hard way to start.
  
  if(any(is.na(wtr))){
    return(NaN)
  }
  
  #We need water density, not temperature to do this
  rhoVar = water.density(wtr)
  
  dRhoPerc = 0.15; #in percentage max for unique thermocline step, used later
  numDepths = length(depths);
  drho_dz = get.drho_dz(rhoVar, depths)

  #look for two distinct maximum slopes, lower one assumed to be seasonal
  # actually right here we are just grabbing maximum slope
  thermoInd = which.max(drho_dz)
  mDrhoZ = drho_dz[thermoInd]
  thermoD = depths[thermoInd]
  
  if(thermoInd > 1 && thermoInd < numDepths-1){  #if within range
		Sdn = -(depths[thermoInd+1] - depths[thermoInd])/
  		(drho_dz[thermoInd+1] - drho_dz[thermoInd])
  		
		Sup = (depths[thermoInd]-depths[thermoInd-1])/
 			(drho_dz[thermoInd]-drho_dz[thermoInd-1])
  		
  	upD  = depths[thermoInd];
  	dnD  = depths[thermoInd+1];
  	if( !is.infinite(Sup) & !is.infinite(Sdn) ){
  		thermoD = dnD*(Sdn/(Sdn+Sup))+upD*(Sup/(Sdn+Sup));
  	}
  }
  
  dRhoCut = max( c(dRhoPerc*mDrhoZ, Smin) )
  locs = findPeaks(drho_dz, dRhoCut)
  pks = drho_dz[locs]
  
	if(length(pks) == 0){
		SthermoD = thermoD
		SthermoInd = thermoInd
	}else{
		mDrhoZ = pks[length(pks)]
		SthermoInd = locs[length(pks)]

		if(SthermoInd > thermoInd + 1){
			SthermoD = mean(depths[SthermoInd:(SthermoInd+1)])
			
			if(SthermoInd > 1 && SthermoInd < numDepths - 1){
				Sdn = -(depths[SthermoInd+1] - depths[SthermoInd])/
					(drho_dz[SthermoInd+1] - drho_dz[SthermoInd])
					
				Sup = (depths[SthermoInd] - depths[SthermoInd-1])/
					(drho_dz[SthermoInd] - drho_dz[SthermoInd-1])
					
				upD  = depths[SthermoInd]
				dnD  = depths[SthermoInd+1]
				
				if( !is.infinite(Sup) & !is.infinite(Sdn) ){
					SthermoD = dnD*(Sdn/(Sdn+Sup))+upD*(Sup/(Sdn+Sup))
				}
			}
		}else{
			SthermoD = thermoD
			SthermoInd = thermoInd
		}
	}
  
  if(SthermoD < thermoD){
	  SthermoD = thermoD
	  SthermoInd = thermoInd
  }
  if(seasonal){
    return(SthermoD)
  }else{
    return(thermoD)
  }
  
  #list( thermoD, thermoInd, drho_dz, SthermoD, SthermoInd )
}

# Finds the local peaks in a vector. Checks the optionally supplied threshold 
#  for minimum height.
findPeaks <- function(x, thresh=0){
	pks <- which(diff(sign(diff(x, na.pad=FALSE)),na.pad=FALSE) < 0) + 2
	return(pks[x[pks-1] - x[pks] >= thresh])
}


