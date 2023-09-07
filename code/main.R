library(tidyverse)
library(writexl)
library(readxl)
library(ggpattern)
library(colorspace)
"%!in%" <- Negate("%in%")

# Functions
#===============================================================================
getjob <- function(jobcode) {
  #university
  if (jobcode > 10000 & jobcode %% 10 > 0){
    return(list("univ", jobcode, ""))
  }
  #PLA
  if (jobcode > 3000 & jobcode < 4000){
    if (jobcode < 3010){
      return(list("pla", "cmc"))
    }
    if (jobcode < 3020){
      return(list("pla", "Chief of Staff", ""))
    }
    if (jobcode < 3030){
      return(list("pla", "Political", ""))
    }
    if (jobcode < 3040){
      return(list("pla", "Logistics", ""))
    }
    if (jobcode < 3050){
      return(list("pla", "Armament", ""))
    }
    if (jobcode < 3060){
      return(list("pla", "Training", ""))
    }
    if (jobcode < 3070){
      return(list("pla", "Mobilization", ""))
    }
    if (jobcode < 3080){
      return(list("pla", "Law and Politics", ""))
    }
    if (jobcode < 3090){
      return(list("pla", "Political Department", ""))
    }
    if (jobcode < 3110){
      return(list("pla", "Navy", ""))
    }
    if (jobcode < 3120){
      return(list("pla", "AF", ""))
    }
    if (jobcode < 3130){
      return(list("pla", "Missile", ""))
    } 
    if (jobcode < 3140){
      return(list("pla", "Support", ""))
    }
    if (jobcode < 3150){
      return(list("pla", "Armed Police", ""))
    }
    if (jobcode < 3160){
      return(list("pla", "Militia", ""))
    }
    if (jobcode < 3170){
      return(list("pla", "Specialized", ""))
    }
    if (jobcode < 3174){
      return(list("pla", "Army", ""))
    }
    if (jobcode < 3175){
      return(list("pla", "Beijing", ""))
    }
    if (jobcode < 3176){
      return(list("pla", "Xinjiang", ""))
    }
    if (jobcode < 3177){
      return(list("pla", "Tibet", ""))
    }
    if (jobcode <= 3179){
      return(list("pla", "Army", ""))
    }
    if (jobcode < 3190){
      return(list("pla", "Safeguard", ""))
    }
    if (jobcode <= 3509){
      return(list("pla", "Central", ""))
    }
    if (jobcode <= 3519){
      return(list("pla", "Northern", ""))
    }
    if (jobcode <= 3529){
      return(list("pla", "Western", ""))
    }
    if (jobcode <= 3539){
      return(list("pla", "Jinan", ""))
    }
    if (jobcode <= 3549){
      return(list("pla", "Eastern", ""))
    }
    if (jobcode <= 3559){
      return(list("pla", "Southern", ""))
    }
    if (jobcode <= 3569){
      return(list("pla", "Chengdu", ""))
    }
    if (jobcode < 3580){
      return(list("pla", "Army", ""))
    }
    if (jobcode < 3590){
      return(list("pla", "Eastern", ""))
    }
    if (jobcode < 3600){
      return(list("pla", "Southern", ""))
    }
    if (jobcode < 3610){
      return(list("pla", "Southern", ""))
    }
    return(list("pla", "error", ""))
  }
  #NPC
  if (jobcode > 4000 & jobcode < 4011){
    return(list("NPC", "NPC", ""))
  }
  #CPPCC
  if (jobcode > 4500 & jobcode < 4511){
    return(list("CPPCC", "CPPCC", ""))
  }
  #State Council
  if (jobcode > 1000 & jobcode < 1580){
    if (jobcode < 1100){
      return(list("sc", "SC", ""))
    } 
    if (jobcode < 1110){
      return(list("sc", "MFA", ""))
    }
    if (jobcode < 1120){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOD", ""))
      } else {return(list("SOE", "MOD", ""))}
    }
    if (jobcode < 1130){
      if (jobcode %% 10 < 9) {
        return(list("sc", "NDRC", ""))
      } else {return(list("SOE", "NDRC", ""))}
    } 
    if (jobcode < 1140){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOE", ""))
      } else {return(list("SOE", "MOE", ""))}
    } 
    if (jobcode < 1150){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MST", ""))
      } else {return(list("SOE", "MST", ""))}
    } 
    if (jobcode < 1160){
      if (jobcode %% 10 < 9) {
        return(list("sc", "DSTC", ""))
      } else {return(list("SOE", "DSTC", ""))}
    }
    if (jobcode < 1170){
      if (jobcode %% 10 < 9) {
        return(list("sc", "SNC", ""))
      } else {return(list("SOE", "SNC", ""))}
    }
    if (jobcode < 1180){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MPS", ""))
      } else {return(list("SOE", "MPS", ""))}
    }
    if (jobcode < 1190){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MSS", ""))
      } else {return(list("SOE", "MSS", ""))}
    }
    if (jobcode < 1200){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MSu", ""))
      } else {return(list("SOE", "MSu", ""))}
    }
    if (jobcode < 1210){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MCA", ""))
      } else {return(list("SOE", "MCA", ""))}
    }
    if (jobcode < 1220){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOJ", ""))
      } else {return(list("SOE", "MOJ", ""))}
    }
    if (jobcode < 1230){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOF", ""))
      } else {return(list("SOE", "MOF", ""))}
    }
    if (jobcode < 1240){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOP", ""))
      } else {return(list("SOE", "MOP", ""))}
    }
    if (jobcode < 1250){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MLSS", ""))
      } else {return(list("SOE", "MLSS", ""))}
    }
    if (jobcode < 1260){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MNR", ""))
      } else {return(list("SOE", "MNR", ""))}
    }
    if (jobcode < 1270){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MCON", ""))
      } else {return(list("SOE", "MCON", ""))}
    }
    if (jobcode < 1280){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOR", ""))
      } else {return(list("SOE", "MOR", ""))}
    }
    if (jobcode < 1290){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOT", ""))
      } else {return(list("SOE", "MOT", ""))}
    }
    if (jobcode < 1300){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MIIT", ""))
      } else {return(list("SOE", "MIIT", ""))}
    }
    if (jobcode < 1310){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MWR", ""))
      } else {return(list("SOE", "MWR", ""))}
    }
    if (jobcode < 1320){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOA", ""))
      } else {return(list("SOE", "MOA", ""))}
    }
    if (jobcode < 1330){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MCOM", ""))
      } else {return(list("SOE", "MCOM", ""))}
    }
    if (jobcode < 1340){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MCUL", ""))
      } else {return(list("SOE", "MCUL", ""))}
    }
    if (jobcode < 1350){
      if (jobcode %% 10 < 9) {
        return(list("sc", "NHC", ""))
      } else {return(list("SOE", "NHC", ""))}
    }
    if (jobcode < 1360){
      if (jobcode %% 10 < 9) {
        return(list("sc", "NHC", ""))
      } else {return(list("SOE", "NHC", ""))}
    }
    if (jobcode < 1370){
      if (round(jobcode %% 1, 2) == 0) {
        return(list("sc", "Finance", ""))
      } else if (round(jobcode %% 1, 2) == 0.01) {
        return(list("SOE", "ICBC", ""))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("SOE", "ABC", ""))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("SOE", "BOC", ""))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("SOE", "CCB", ""))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("SOE", "BOComm", ""))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("SOE", "EXIM", ""))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("SOE", "ADBC", ""))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("SOE", "CDB", ""))
      } else if (round(jobcode %% 1, 2) == 0.09) {
        return(list("SOE", "CITIC", ""))
      } else if (round(jobcode %% 1, 2) == 0.10) {
        return(list("SOE", "CMB", ""))
      } else if (round(jobcode %% 1, 2) == 0.11) {
        return(list("SOE", "PAB", ""))
      } else if (round(jobcode %% 1, 2) == 0.12) {
        return(list("SOE", "Industrial", ""))
      } else if (round(jobcode %% 1, 2) == 0.13) {
        return(list("SOE", "GFB", ""))
      } else if (round(jobcode %% 1, 2) == 0.14) {
        return(list("SOE", "EBB", ""))
      } else if (round(jobcode %% 1, 2) == 0.15) {
        return(list("SOE", "PFB", ""))
      } else if (round(jobcode %% 1, 2) == 0.16) {
        return(list("SOE", "HXB", ""))
      } else if (round(jobcode %% 1, 2) == 0.17) {
        return(list("SOE", "MSB", ""))
      } else if (round(jobcode %% 1, 2) == 0.18) {
        return(list("SOE", "ZJB", ""))
      } else if (round(jobcode %% 1, 2) == 0.19) {
        return(list("SOE", "HFB", ""))
      } else if (round(jobcode %% 1, 2) == 0.20) {
        return(list("SOE", "BHB", ""))
      } else if (round(jobcode %% 1, 2) == 0.21) {
        return(list("SOE", "Other Bank", ""))
      } else if (round(jobcode %% 1, 2) == 0.22) {
        return(list("SOE", "CIC", ""))
      } else if (round(jobcode %% 1, 2) == 0.23) {
        return(list("sc", "Finance", ""))
      } else if (round(jobcode %% 1, 2) == 0.24) {
        return(list("sc", "Finance", ""))
      } else if (round(jobcode %% 1, 2) == 0.25) {
        return(list("SOE", "Union", ""))
      }
    }
    if (jobcode < 1380){
      if (jobcode %% 10 < 9) {
        return(list("sc", "NAO", ""))
      } else {return(list("SOE", "NAO", ""))}
    }
    if (jobcode < 1390){
      if (jobcode %% 10 < 9) {
        return(list("sc", "SSRC", ""))
      } else {return(list("SOE", "SSRC", ""))}
    }
    if (jobcode < 1400){
      if (jobcode %% 10 < 9) {
        return(list("sc", "ImExMC", ""))
      } else {return(list("SOE", "ImExMC", ""))}
    }
    if (jobcode < 1410){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MEP", ""))
      } else {return(list("SOE", "MEP", ""))}
    }
    if (jobcode > 1560 & jobcode < 1570){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MEM", ""))
      } else {return(list("SOE", "MEM", ""))}
    }
    if (jobcode > 1570 & jobcode < 1580){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MVA", ""))
      } else {return(list("SOE", "MVA", ""))}
    }
    if (jobcode < 1420){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MaM", ""))
      } else {return(list("SOE", "MaM", ""))}
    }
    if (jobcode < 1430){
      if (jobcode %% 10 < 9) {
        return(list("sc", "ETC", ""))
      } else {return(list("SOE", "ETC", ""))}
    }
    if (jobcode < 1440){
      if (jobcode %% 10 < 9) {
        return(list("sc", "CoalM", ""))
      } else {return(list("SOE", "CoalM", ""))}
    }
    if (jobcode < 1450){
      if (jobcode %% 10 < 9) {
        return(list("sc", "PCM", ""))
      } else {return(list("SOE", "PCM", ""))}
    }
    if (jobcode < 1460){
      if (jobcode %% 10 < 9) {
        return(list("sc", "LIM", ""))
      } else {return(list("SOE", "LIM", ""))}
    }
    if (jobcode < 1470){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MIM", ""))
      } else {return(list("SOE", "MIM", ""))}
    }
    if (jobcode < 1480){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MEIM", ""))
      } else {return(list("SOE", "MEIM", ""))}
    }
    if (jobcode < 1496){
      return(list("sc", "Finance", ""))
    }
    if (jobcode < 1500){
      return(list("SOE", "SASAC", ""))
    }
    if (jobcode < 1510){
      if (round(jobcode %% 1, 2) == 0.01) {
        return(list("sc", "Customs", ""))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("sc", "Taxation", ""))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("sc", "SAIC", ""))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("sc", "AQSIQ", ""))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("sc", "NRTA", ""))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("sc", "Sport", ""))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("sc", "MEM", ""))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("sc", "Market", ""))
      } else if (round(jobcode %% 1, 2) == 0.09) {
        return(list("sc", "Statistics", ""))
      } else if (round(jobcode %% 1, 2) == 0.1) {
        return(list("sc", "MNR", ""))
      } else if (round(jobcode %% 1, 2) == 0.11) {
        return(list("sc", "IP", ""))
      } else if (round(jobcode %% 1, 2) == 0.12) {
        return(list("sc", "MCUL", ""))
      } else if (round(jobcode %% 1, 2) == 0.13) {
        return(list("sc", "Religion", ""))
      } else if (round(jobcode %% 1, 2) == 0.14) {
        return(list("sc", "Counsellor", ""))
      } else if (round(jobcode %% 1, 2) == 0.15) {
        return(list("sc", "Office Admin", ""))
      } else if (round(jobcode %% 1, 2) == 0.16) {
        return(list("sc", "IP", ""))
      } else if (round(jobcode %% 1, 2) == 0.17) {
        return(list("sc", "MSu", ""))
      } else if (round(jobcode %% 1, 2) == 0.18) {
        return(list("sc", "MMI", ""))
      } else if (round(jobcode %% 1, 2) == 0.19) {
        return(list("sc", "MMI", ""))
      } else if (round(jobcode %% 1, 2) == 0.20) {
        return(list("sc", "NDRC", ""))
      } else if (round(jobcode %% 1, 2) == 0.21) {
        return(list("sc", "Market", ""))
      } else if (round(jobcode %% 1, 2) == 0.22) {
        return(list("sc", "MEP", ""))
      } else if (round(jobcode %% 1, 2) == 0.23) {
        return(list("sc", "MNR", ""))
      } else if (round(jobcode %% 1, 2) == 0.24) {
        return(list("sc", "Market", ""))
      } else if (round(jobcode %% 1, 2) == 0.25) {
        return(list("sc", "Construction", ""))
      } else if (round(jobcode %% 1, 2) == 0.26) {
        return(list("sc", "Foreign Press", ""))
      } else if (round(jobcode %% 1, 2) == 0.27) {
        return(list("sc", "InternationalCommunications", ""))
      } else if (round(jobcode %% 1, 2) == 0.28) {
        return(list("sc", "Market", ""))
      } else if (round(jobcode %% 1, 2) == 0.29) {
        return(list("sc", "MOA", ""))
      } else if (round(jobcode %% 1, 2) == 0.30) {
        return(list("sc", "CIDCA", ""))
      }
    }
    if (jobcode < 1520){
      if (round(jobcode %% 1, 2) == 0.01) {
        return(list("sc", "Overseas", ""))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("sc", "Hong Kong Affairs", ""))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("sc", "Legislative Affairs", ""))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("sc", "Research Office", ""))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("sc", "Taiwan Affairs", ""))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("sc", "610", ""))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("sc", "News", ""))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("sc", "Cyberspace", ""))
      }
    }
    if (jobcode < 1530){
      if (round(jobcode %% 1, 2) == 0.01) {
        return(list("sc", "Xinhua", ""))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("sc", "Engineering Academy", ""))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("sc", "Development Research", ""))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("sc", "Governance Academy", ""))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("sc", "MEM", ""))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("sc", "Meterology", ""))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("sc", "Finance", ""))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("sc", "Finance", ""))
      } else if (round(jobcode %% 1, 2) == 0.09) {
        return(list("sc", "Finance", ""))
      } else if (round(jobcode %% 1, 2) == 0.10) {
        return(list("sc", "SERC", ""))
      } else if (round(jobcode %% 1, 2) == 0.11) {
        return(list("sc", "MOF", ""))
      } else if (round(jobcode %% 1, 2) == 0.12) {
        return(list("sc", "NSFC", ""))
      } else if (round(jobcode %% 1, 2) == 0.13) {
        return(list("sc", "Cooperatives", ""))
      } else if (round(jobcode %% 1, 2) == 0.15) {
        return(list("sc", "MNR", ""))
      } else if (round(jobcode %% 1, 2) == 0.16) {
        return(list("sc", "CMG", ""))
      } else if (round(jobcode %% 1, 2) == 0.17) {
        return(list("sc", "MWR", ""))
      } else if (round(jobcode %% 1, 2) == 0.18) {
        return(list("SOE", "CIC", ""))
      }
    }
    if (jobcode < 1540){
      if (round(jobcode %% 1, 2) == 0.01) {
        return(list("sc", "Complaints", ""))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("sc", "NDRC", ""))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("sc", "NDRC", ""))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("sc", "MIIT", ""))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("sc", "MLSS", ""))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("sc", "MLSS", ""))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("sc", "MNR", ""))
      } else if (round(jobcode %% 1, 2) == 0.09) {
        return(list("sc", "MNR", ""))
      } else if (round(jobcode %% 1, 2) == 0.10) {
        return(list("sc", "MOT", ""))
      } else if (round(jobcode %% 1, 2) == 0.11) {
        return(list("sc", "MOT", ""))
      } else if (round(jobcode %% 1, 2) == 0.12) {
        return(list("sc", "MOT", ""))
      } else if (round(jobcode %% 1, 2) == 0.13) {
        return(list("sc", "MCUL", ""))
      } else if (round(jobcode %% 1, 2) == 0.14) {
        return(list("sc", "NHC", ""))
      } else if (round(jobcode %% 1, 2) == 0.15) {
        return(list("sc", "Finance", ""))
      } else if (round(jobcode %% 1, 2) == 0.16) {
        return(list("sc", "MEM", ""))
      } else if (round(jobcode %% 1, 2) == 0.17) {
        return(list("party", "General Office", ""))
      } else if (round(jobcode %% 1, 2) == 0.18) {
        return(list("party", "General Office", ""))
      } else if (round(jobcode %% 1, 2) == 0.19) {
        return(list("party", "General Office", ""))
      } else if (round(jobcode %% 1, 2) == 0.20) {
        return(list("sc", "DSTC", ""))
      } else if (round(jobcode %% 1, 2) == 0.21) {
        return(list("sc", "DSTC", ""))
      } else if (round(jobcode %% 1, 2) == 0.22) {
        return(list("sc", "MOE", ""))
      } else if (round(jobcode %% 1, 2) == 0.23) {
        return(list("sc", "MEP", ""))
      } else if (round(jobcode %% 1, 2) == 0.24) {
        return(list("sc", "NDRC", ""))
      } else if (round(jobcode %% 1, 2) == 0.25) {
        return(list("sc", "Market", ""))
      } else if (round(jobcode %% 1, 2) == 0.26) {
        return(list("sc", "Market", ""))
      } else if (round(jobcode %% 1, 2) == 0.27) {
        return(list("SOE", "Gold", ""))
      } else if (round(jobcode %% 1, 2) == 0.28) {
        return(list("sc", "NDRC", ""))
      } else if (round(jobcode %% 1, 2) == 0.29) {
        return(list("sc", "MOF", ""))
      } else if (round(jobcode %% 1, 2) == 0.30) {
        return(list("sc", "MNR", ""))
      } else if (round(jobcode %% 1, 2) == 0.31) {
        return(list("sc", "Customs", ""))
      } else if (round(jobcode %% 1, 2) == 0.32) {
        return(list("sc", "Non-Ferrous Metal", ""))
      } else if (round(jobcode %% 1, 2) == 0.33) {
        return(list("sc", "Market", ""))
      }
    }
    if (jobcode < 1550) {
      return(list("sc", "LSG", ""))
    }
    if (jobcode > 1550 & jobcode < 1570){
      if (round(jobcode %% 1, 2) == 0.01) {
        return(list("sc", "HK Liason", ""))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("sc", "Macau Liason", ""))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("sc", "Large Firms", ""))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("sc", "MFA", ""))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("sc", "HK Security", ""))
      }
    }
    return(list("sc", "error", ""))
  }
  #Mass Organization
  if (jobcode > 6400 & jobcode < 6410){
    if (round(jobcode %% 1, 2) == 0.01) {
      return(list("mo", "ACFTU", ""))
    } else if (round(jobcode %% 1, 2) == 0.02) {
      return(list("mo", "ACWF", ""))
    } else if (round(jobcode %% 1, 2) == 0.03) {
      return(list("mo", "ACFIC", ""))
    } else if (round(jobcode %% 1, 2) == 0.04) {
      return(list("mo", "CCPIT", ""))
    } else if (round(jobcode %% 1, 2) == 0.05) {
      return(list("mo", "CAST", ""))
    } else if (round(jobcode %% 1, 2) == 0.06) {
      return(list("mo", "CFLAC", ""))
    } else if (round(jobcode %% 1, 2) == 0.07) {
      return(list("mo", "CWA", ""))
    } else if (round(jobcode %% 1, 2) == 0.08) {
      return(list("mo", "CLS", ""))
    } else if (round(jobcode %% 1, 2) == 0.09) {
      return(list("mo", "ACJA", ""))
    } else if (round(jobcode %% 1, 2) == 0.10) {
      return(list("mo", "ACFROC", ""))
    } else if (round(jobcode %% 1, 2) == 0.11) {
      return(list("mo", "ACFTC", ""))
    } else if (round(jobcode %% 1, 2) == 0.12) {
      return(list("mo", "WRSA", ""))
    } else if (round(jobcode %% 1, 2) == 0.13) {
      return(list("mo", "CPAFFC", ""))
    } else if (round(jobcode %% 1, 2) == 0.14) {
      return(list("mo", "CPIFA", ""))
    } else if (round(jobcode %% 1, 2) == 0.15) {
      return(list("mo", "CDPF", ""))
    } else if (round(jobcode %% 1, 2) == 0.16) {
      return(list("mo", "RCSC", ""))
    } else if (round(jobcode %% 1, 2) == 0.17) {
      return(list("mo", "CSCLF", ""))
    } else if (round(jobcode %% 1, 2) == 0.18) {
      return(list("mo", "HuangPu", ""))
    } else if (round(jobcode %% 1, 2) == 0.19) {
      return(list("mo", "CSIPW", ""))
    } else if (round(jobcode %% 1, 2) == 0.20) {
      return(list("mo", "NAVEC", ""))
    } else if (round(jobcode %% 1, 2) == 0.21) {
      return(list("mo", "PPA", ""))
    } else if (round(jobcode %% 1, 2) == 0.22) {
      return(list("mo", "ACYF", ""))
    } else if (round(jobcode %% 1, 2) == 0.23) {
      return(list("mo", "CBA", ""))
    } else if (round(jobcode %% 1, 2) == 0.24) {
      return(list("mo", "RCHR", ""))
    } else if (round(jobcode %% 1, 2) == 0.25) {
      return(list("mo", "Socialist", ""))
    } else if (round(jobcode %% 1, 2) == 0.26) {
      return(list("mo", "CSA", ""))
    } else if (round(jobcode %% 1, 2) == 0.27) {
      return(list("mo", "CBA", ""))
    } else if (round(jobcode %% 1, 2) == 0.28) {
      return(list("mo", "ACSF", ""))
    } else if (round(jobcode %% 1, 2) == 0.29) {
      return(list("mo", "ACSportF", ""))
    } else if (round(jobcode %% 1, 2) == 0.30) {
      return(list("mo", "CSNP", ""))
    } else if (round(jobcode %% 1, 2) == 0.31) {
      return(list("mo", "CCA", ""))
    } else if (round(jobcode %% 1, 2) == 0.32) {
      return(list("mo", "ARATS", ""))
    } else if (round(jobcode %% 1, 2) == 0.33) {
      return(list("mo", "CNLIC", ""))
    }
  }
  #Party
  if (jobcode > 6000 & jobcode < 7000){
    if (jobcode < 6005){
      return(list("CCP", "", ""))
    }
    if (jobcode < 6010){
      return(list("party", "Secretariat", ""))
    }
    if (jobcode < 6020){
      return(list("party", "COD", ""))
    }
    if (jobcode < 6030){
      return(list("party", "CPD", ""))
    }
    if (jobcode < 6040){
      return(list("party", "UFD", ""))
    }
    if (jobcode < 6050){
      return(list("party", "CID", ""))
    }
    if (jobcode < 6060){
      return(list("party", "General Office", ""))
    }
    if (jobcode < 6080){
      return(list("party", "Newspapers", ""))
    }
    if (jobcode < 6090){
      return(list("party", "CPHRC", ""))
    }
    if (jobcode < 6100){
      return(list("party", "CPDRC", ""))
    }
    if (jobcode < 6110){
      return(list("party", "CTB", ""))
    }
    if (jobcode < 6120){
      return(list("party", "CSOWC", ""))
    }
    if (jobcode < 6130){
      return(list("party", "CDOWC", ""))
    }
    if (jobcode < 6150){
      return(list("party", "CLPLG", ""))
    }
    if (jobcode < 6160){
      return(list("party", "CSSC", ""))
    }
    if (jobcode < 6170){
      return(list("party", "CPRC", ""))
    }
    if (jobcode < 6180){
      return(list("party", "CTWO", ""))
    }
    if (jobcode < 6190){
      return(list("party", "CEPO", ""))
    }
    if (jobcode < 6200){
      return(list("party", "CEAC", ""))
    }
    if (jobcode < 6210){
      return(list("party", "CSC", ""))
    }
    if (jobcode < 6220){
      return(list("party", "CGB", ""))
    }
    if (jobcode < 6230){
      return(list("party", "CRC", ""))
    }
    if (jobcode > 6300 & jobcode < 6310){
      return(list("party", "LSG", ""))
    }
    if (jobcode > 6500 & jobcode < 6510){
      return(list("party", "CDIC", ""))
    }
    if (jobcode > 6600 & jobcode < 6610){
      return(list("party", "CYL", ""))
    }
    if (jobcode > 6700 & jobcode < 6710){
      return(list("party", "CAC", ""))
    }
    if (jobcode > 6710 & jobcode < 6720){
      return(list("party", "CISIC", ""))
    }
    if (jobcode > 6900 & jobcode < 7000){
      if (round(jobcode %% 1, 2) == 0.1) {
        return(list("sc", "Law and Politics", ""))
      } else if (round(jobcode %% 1, 2) == 0.2) {
        return(list("sc", "Industrial", ""))
      } else if (round(jobcode %% 1, 2) == 0.3) {
        return(list("sc", "Agriculture", ""))
      } else if (round(jobcode %% 1, 2) == 0.4) {
        return(list("sc", "Propaganda", ""))
      }
    }
    return(list("party", "error", ""))
  }
  #Province
  if (jobcode > 2000 & jobcode < 3000){
    if (jobcode < 2011) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Beijing", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Beijing", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Beijing", "SSD"))
      } else {
        return(list("pro", "Beijing", ""))
      }
    }
    if (jobcode < 2021) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Tianjin", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Tianjin", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Tianjin", "SSD"))
      } else {
        return(list("pro", "Tianjin", ""))
      }
    }
    if (jobcode < 2031) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Hebei", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Hebei", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Hebei", "SSD"))
      } else {
        return(list("pro", "Hebei", ""))
      }
    }
    if (jobcode < 2041) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Shanxi", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Shanxi", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Shanxi", "SSD"))
      } else {
        return(list("pro", "Shanxi", ""))
      }
    }
    if (jobcode < 2051) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Inner Mongolia", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Inner Mongolia", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Inner Mongolia", "SSD"))
      } else {
        return(list("pro", "Inner Mongolia", ""))
      }    
    }
    if (jobcode < 2061) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Liaoning", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Liaoning", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Liaoning", "SSD"))
      } else {
        return(list("pro", "Liaoning", ""))
      }
    }
    if (jobcode < 2071) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Jilin", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Jilin", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Jilin", "SSD"))
      } else {
        return(list("pro", "Jilin", ""))
      }
    }
    if (jobcode < 2081) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Heilongjiang", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Heilongjiang", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Heilongjiang", "SSD"))
      } else {
        return(list("pro", "Heilongjiang", ""))
      }
    }
    if (jobcode < 2091) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Shanghai", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Shanghai", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Shanghai", "SSD"))
      } else {
        return(list("pro", "Shanghai", ""))
      }
    }
    if (jobcode < 2101) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Jiangsu", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Jiangsu", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Jiangsu", "SSD"))
      } else {
        return(list("pro", "Jiangsu", ""))
      }
    }
    if (jobcode < 2111) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Zhejiang", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Zhejiang", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Zhejiang", "SSD"))
      } else {
        return(list("pro", "Zhejiang", ""))
      }
    }
    if (jobcode < 2121) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Anhui", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Anhui", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Anhui", "SSD"))
      } else {
        return(list("pro", "Anhui", ""))
      }
    }
    if (jobcode < 2131) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Fujian", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Fujian", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Fujian", "SSD"))
      } else {
        return(list("pro", "Fujian", ""))
      }
    }
    if (jobcode < 2141) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Jiangxi", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Jiangxi", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Jiangxi", "SSD"))
      } else {
        return(list("pro", "Jiangxi", ""))
      }
    }
    if (jobcode < 2151) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Shandong", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Shandong", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Shandong", "SSD"))
      } else {
        return(list("pro", "Shandong", ""))
      }
    }
    if (jobcode < 2161) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Henan", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Henan", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Henan", "SSD"))
      } else {
        return(list("pro", "Henan", ""))
      }
    }
    if (jobcode < 2171) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Hubei", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Hubei", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Hubei", "SSD"))
      } else {
        return(list("pro", "Hubei", ""))
      }
    }
    if (jobcode < 2181) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Hunan", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Hunan", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Hunan", "SSD"))
      } else {
        return(list("pro", "Hunan", ""))
      }
    }
    if (jobcode < 2191) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Guangdong", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Guangdong", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Guangdong", "SSD"))
      } else {
        return(list("pro", "Guangdong", ""))
      }
    }
    if (jobcode < 2201) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Guangxi", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Guangxi", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Guangxi", "SSD"))
      } else {
        return(list("pro", "Guangxi", ""))
      }
    }
    if (jobcode < 2211) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Hainan", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Hainan", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Hainan", "SSD"))
      } else {
        return(list("pro", "Hainan", ""))
      }
    }
    if (jobcode < 2221) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Chongqing", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Chongqing", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Chongqing", "SSD"))
      } else {
        return(list("pro", "Chongqing", ""))
      }
    }
    if (jobcode < 2231) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Sichuan", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Sichuan", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Sichuan", "SSD"))
      } else {
        return(list("pro", "Sichuan", ""))
      }
    }
    if (jobcode < 2241) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Guizhou", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Guizhou", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Guizhou", "SSD"))
      } else {
        return(list("pro", "Guizhou", ""))
      }
    }
    if (jobcode < 2251) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Yunnan", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Yunnan", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Yunnan", "SSD"))
      } else {
        return(list("pro", "Yunnan", ""))
      }
    }
    if (jobcode < 2261) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Tibet", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Tibet", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Tibet", "SSD"))
      } else {
        return(list("pro", "Tibet", ""))
      }
    }
    if (jobcode < 2271) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Shaanxi", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Shaanxi", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Shaanxi", "SSD"))
      } else {
        return(list("pro", "Shaanxi", ""))
      }
    }
    if (jobcode < 2281) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Gansu", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Gansu", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Gansu", "SSD"))
      } else {
        return(list("pro", "Gansu", ""))
      }
    }
    if (jobcode < 2291) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Qinghai", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Qinghai", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Qinghai", "SSD"))
      } else {
        return(list("pro", "Qinghai", ""))
      }
    }
    if (jobcode < 2301) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Ningxia", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Ningxia", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Ningxia", "SSD"))
      } else {
        return(list("pro", "Ningxia", ""))
      }
    }
    if (jobcode < 2311) {
      if (round(jobcode %% 10, 1) == 5.6) {
        return(list("pro", "Xinjiang", "PLPC"))
      } else if (round(jobcode %% 10, 1) == 6.1) {
        return(list("pro", "Xinjiang", "PSD"))
      } else if (round(jobcode %% 10, 1) == 8.6) {
        return(list("pro", "Xinjiang", "SSD"))
      } else {
        return(list("pro", "Xinjiang", ""))
      }
    }
    return(list("pro", "error", ""))
  }
  #NPC
  if (jobcode > 5000 & jobcode < 5010){
    return(list("NPC", "SPC", ""))
  }
  if (jobcode > 5010 & jobcode < 5020){
    return(list("NPC", "SPP", ""))
  }
  return(list("unknown", "error", ""))
}
career <- function(dataframe, eyear) {
  n <- nrow(dataframe)
  careerlist <- list(member = list(),
                     career = list())
  joblist <- c("pro", "sc", "SOE", "party", "pla", "univ", "NPC", "CPPCC", "mo")
  dataframe$lastpro <- dataframe$lastsc <- dataframe$lastSOE <- dataframe$lastparty <- 0
  dataframe$lastpla <- dataframe$lastuniv <- dataframe$lastNPC <- dataframe$lastCPPCC <- 0
  dataframe$lastmo <- 0
  dataframe$npro <- dataframe$nsc <- dataframe$nSOE <- dataframe$nparty <- 0
  dataframe$npla <- dataframe$nuniv <- dataframe$nNPC <- dataframe$nCPPCC <- 0
  dataframe$nmo <- dataframe$MPS <- dataframe$PSD <- dataframe$MSS <- 0
  dataframe$SSD <- dataframe$CLPLG <- dataframe$PLPC <- 0
  dataframe$policing <- dataframe$widepolicing <- 0
  for (entry in 1:n){
    province <- state <- SOE <- party <- c()
    pla <- univ <- NPC <- CPPCC <- MO <- c()
    careerpath <- c()
    for (job in 1:36) {
      jobid <- paste0("job", job)
      jobs <- paste0(jobid, "s")
      jobe <- paste0(jobid, "e")
      if (is.na(dataframe[entry, jobe]) | is.na(dataframe[entry,jobid])) {
        break
      }
      #print(paste(entry, job))
      if (dataframe[entry, jobs] > eyear) {
        next
      }
      joblst <- getjob(as.numeric(dataframe[entry, jobid]))
      jobdesc <- toString(joblst[1])
      jobloc <- toString(joblst[2])
      jobspec <- toString(joblst[3])
      #print(as.numeric(dataframe[entry, jobid]))
      if (jobloc == "error") {
        print(as.numeric(dataframe[entry, jobid]))
        next
      }
      if (dataframe[entry, jobe] >= eyear & jobdesc %in% joblist){
        dataframe[entry, paste0("last", jobdesc)] = 1
      }
      careerpath <- c(careerpath, paste(jobdesc, jobloc, jobspec))
      if (jobloc == "MPS") {
        dataframe$MPS[entry] <- 1
      }
      if (jobloc == "MSS") {
        dataframe$MSS[entry] <- 1
      }
      if (jobloc == "CLPLG") {
        dataframe$CLPLG[entry] <- 1
      }
      if (jobspec == "PSD") {
        dataframe$PSD[entry] <- 1
      }
      if (jobspec == "SSD") {
        dataframe$SSD[entry] <- 1
      }
      if (jobspec == "PLPC") {
        dataframe$PLPC[entry] <- 1
      }
      dataframe$policing <- as.numeric((dataframe$MPS + dataframe$MSS + dataframe$CLPLG) > 0)
      dataframe$widepolicing <- as.numeric((dataframe$policing + dataframe$PSD + dataframe$SSD + dataframe$PLPC) > 0)
      if (jobdesc == "pro"){
        if (length(province) >= 2) {
          if ((jobloc == province[length(province) - 1]) & 
              (jobloc != province[length(province)])) {
            province[length(province)] <- jobloc
          } else {province <- c(province, jobloc)}
        } else {province <- c(province, jobloc)}
      }
      if (jobdesc == "sc" & jobloc %!in% state){
        state <- c(state, jobloc)
        dataframe$nsc[entry] <- dataframe$nsc[entry] + 1
      }
      if (jobdesc == "SOE" & jobloc %!in% SOE){
        SOE <- c(SOE, jobloc)
        dataframe$nSOE[entry] <- dataframe$nSOE[entry] + 1
      }
      if (jobdesc == "party" & jobloc %!in% party){
        party <- c(party, jobloc)
        dataframe$nparty[entry] <- dataframe$nparty[entry] + 1
      }
      if (jobdesc == "pla" & jobloc %!in% pla){
        pla <- c(pla, jobloc)
        dataframe$npla[entry] <- dataframe$npla[entry] + 1
      }
      if (jobdesc == "univ" & jobloc %!in% univ) {
        univ <- c(univ, jobloc)
        dataframe$nuniv[entry] <- dataframe$nuniv[entry] + 1
      }
      if (jobdesc == "NPC" & jobloc %!in% NPC) {
        NPC <- c(NPC, jobloc)
        dataframe$nNPC[entry] <- dataframe$nNPC[entry] + 1
      }
      if (jobdesc == "CPPCC" & jobloc %!in% CPPCC){
        CPPCC <- c(CPPCC, jobloc)
        dataframe$nCPPCC[entry] <- dataframe$nCPPCC[entry] + 1
      }
      if (jobdesc == "mo" & jobloc %!in% MO) {
        dataframe$nmo[entry] <- dataframe$nmo[entry] + 1
      }
    }
    dataframe$npro[entry] <- length(unique(province))
    careerlist$member[[entry]] <- dataframe$cname[entry]
    careerlist$career[[entry]] <- careerpath
    #print(paste("Province", province))
    #print(paste("State Council", state))
    #print(paste("PLA", pla))
    #print(paste("SOE", SOE))
    #print(paste("Party", party))
    #print(paste("University", univ))
    #print(paste("CPPCC", CPPCC))
  }
  return(list(dataframe, careerlist))
}
#===============================================================================

# Reading Data
#===============================================================================
biographical_data18th_20thPC <- read_excel("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/dataset/biographical_data18th_20thPC.xlsx")
birthplace <- read_excel("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/dataset/province_uni_codebook.xlsx", sheet = "birthplace")
unis <- read_excel("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/dataset/province_uni_codebook.xlsx", sheet = "schools")
network <- read_excel("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/dataset/Network_18th_20thPC_clean.xlsx")
#===============================================================================

# Data Processing
#===============================================================================
#====utilities====
top_unis <- unis$code[1:10]
province <- birthplace |> 
  dplyr::select(id1, ename1) |> 
  distinct()
master <- biographical_data18th_20thPC[,-c(28:44, 48:60)]
master$top_ba <- as.numeric(master$bauniv %in% top_unis)
master$top_uni <- as.numeric(master$bauniv %in% top_unis | master$mauniv %in% top_unis | master$phduniv %in% top_unis)

# Divide up the dataset into different party congresses
fcc20 <- master |> 
  filter(cc20 > 2000 & cc20 < 3000)
acc20 <- master |> 
  filter(cc20 > 3000)
fcc19 <- master |> 
  filter(cc19 > 2000 & cc19 < 3000)
acc19 <- master |> 
  filter(cc19 > 3000)
fcc18 <- master |> 
  filter(cc18 > 2000 & cc18 < 3000)
acc18 <- master |> 
  filter(cc18 > 3000)
politburo20 <- master |> 
  filter(cc20 < 2000)
politburo19 <- master |> 
  filter(cc19 < 2000)
politburo18 <- master |> 
  filter(cc18 < 2000)

# Correction
fcc20[c(10, 58, 62), c("job30s", "job30e", "job31", "job31s", "job31e", "job32")] <- NA
fcc18[9, c("job4", "job4s", "job4e")] <- list(2043.00, 2002.0, 2016.0)
acc18[6, c("job2", "job2s", "job2e")] <- list(1123.00, 2014.0, 2017)
acc18[83, c("job2", "job2s", "job2e")] <- list(2225.100, 2007.0, 2013)
acc18[90, c("job4", "job4s", "job4e")] <- list(2271.100, 2016.0, 2016)
fcc19[7, c("job2", "job2s", "job2e")] <- list(1123.00, 2014.0, 2017)
fcc19[20, c("job4", "job4s", "job4e")] <- list(2043.00, 2002.0, 2016.0)
fcc20[34, c("job4", "job4s", "job4e")] <- list(2043.00, 2002.0, 2016.0)
acc18[83, c("job3", "job3s", "job3e")] <- list(2225.100, 2007, 2013)
politburo20[17, c("job2", "job2s", "job2e")] <- list(1123.00, 2014.0, 2017)


#====Career Track and Width====
career_var <- c("cname", "ename", "byear", "pyear", "bpro", "gender", "ethnic", 
                "edu", "top_ba",
                "bauniv", "bamajor", "mauniv", "mamajor", "phduniv", "phdmajor",
                "cc18", "cc19", "cc20", "lastparty", "lastSOE", "lastsc",
                "lastpro", "lastCPPCC", "lastNPC", "lastuniv", "lastpla",
                "lastmo", "nparty", "nSOE", "nsc", "npro", "nCPPCC",
                "nNPC", "nuniv", "npla", "nmo", "MPS", "MSS", "CLPLG",
                "PSD", "SSD", "PLPC", "policing", "widepolicing")
career_return <- career(fcc20, 2022)
fcc20_career <- career_return[[1]][, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
fcc20_career$narrow <- as.numeric(fcc20_career$width < 2)
fcc20_career$narrow_alt <- as.numeric(fcc20_career$width <= 1.3)
fcc20_path <- career_return[[2]]

career_return <- career(acc20, 2022)
acc20_career <- career_return[[1]][, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
acc20_career$narrow <- as.numeric(acc20_career$width < 2)
acc20_career$narrow_alt <- as.numeric(acc20_career$width <= 1.3)
acc20_path <- career_return[[2]]

career_return <- career(fcc19, 2017)
fcc19_career <- career_return[[1]][, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
fcc19_career$narrow <- as.numeric(fcc19_career$width < 2)
fcc19_career$narrow_alt <- as.numeric(fcc19_career$width <= 1.3)
fcc19_path <- career_return[[2]]

career_return <- career(acc19, 2017)
acc19_career <- career_return[[1]][, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
acc19_career$narrow <- as.numeric(acc19_career$width < 2)
acc19_career$narrow_alt <- as.numeric(acc19_career$width <= 1.3)
acc19_path <- career_return[[2]]

career_return <- career(fcc18, 2012)
fcc18_career <- career_return[[1]][, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
fcc18_career$narrow <- as.numeric(fcc18_career$width < 2)
fcc18_career$narrow_alt <- as.numeric(fcc18_career$width <= 1.3)
fcc18_path <- career_return[[2]]

career_return <- career(acc18, 2012)
acc18_career <- career_return[[1]][, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
acc18_career$narrow <- as.numeric(acc18_career$width < 2)
acc18_career$narrow_alt <- as.numeric(acc18_career$width <= 1.3)
acc18_path <- career_return[[2]]

career_return <- career(politburo18, 2012)
politburo18_career <- career_return[[1]][, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
politburo18_path <- career_return[[2]]

career_return <- career(politburo19, 2017)
politburo19_career <- career_return[[1]][, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
politburo19_path <- career_return[[2]]

career_return <- career(politburo20, 2022)
politburo20_career <- career_return[[1]][, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
politburo20_path <- career_return[[2]]

network[network > 0 & network <= 5] = 1
network[network > 5 & network < 1000] = 2
names(network)[1] = "ename"
#===============================================================================

# Analysis DataFrame
#===============================================================================
# Create analysis dataframe
stats <- data.frame(matrix(nrow = 6, ncol = 1)) 
colnames(stats) <- "session"
stats$session <- c("18", "18", "19", "19", "20", "20")
stats$member <- c("Full", "Alternate", "Full", "Alternate", "Full", "Alternate")

# Generate additional columns
fcc20$sage <- 2022-fcc20$byear
acc20$sage <- 2022-acc20$byear
fcc19$sage <- 2017-fcc19$byear
acc19$sage <- 2017-acc19$byear
fcc18$sage <- 2012-fcc18$byear
acc18$sage <- 2012-acc18$byear

# Populate analysis dataframe
stats$size <- c(
  fcc18 |> nrow(),
  acc18 |> nrow(),
  fcc19 |> nrow(),
  acc19 |> nrow(),
  fcc20 |> nrow(),
  acc20 |> nrow()
)

stats$meansage <- c(
  fcc18 |> dplyr::select(sage) |> colMeans(na.rm = T),
  acc18 |> dplyr::select(sage) |> colMeans(na.rm = T),
  fcc19 |> dplyr::select(sage) |> colMeans(na.rm = T),
  acc19 |> dplyr::select(sage) |> colMeans(na.rm = T),
  fcc20 |> dplyr::select(sage) |> colMeans(na.rm = T),
  acc20 |> dplyr::select(sage) |> colMeans(na.rm = T)
)

stats$pfemale <- c(
  fcc18 |> filter(gender == 1) |> nrow(),
  acc18 |> filter(gender == 1) |> nrow(),
  fcc19 |> filter(gender == 1) |> nrow(),
  acc19 |> filter(gender == 1) |> nrow(),
  fcc20 |> filter(gender == 1) |> nrow(),
  acc20 |> filter(gender == 1) |> nrow()
) * 100 / stats$size

stats$pminority <- c(
  fcc18 |> filter(ethnic > 0) |> nrow(),
  acc18 |> filter(ethnic > 0) |> nrow(),
  fcc19 |> filter(ethnic > 0) |> nrow(),
  acc19 |> filter(ethnic > 0) |> nrow(),
  fcc20 |> filter(ethnic > 0) |> nrow(),
  acc20 |> filter(ethnic > 0) |> nrow()
) * 100 / stats$size

stats$top_ba <- c(
  fcc18 |> filter(top_ba == 1) |> nrow(),
  acc18 |> filter(top_ba == 1) |> nrow(),
  fcc19 |> filter(top_ba == 1) |> nrow(),
  acc19 |> filter(top_ba == 1) |> nrow(),
  fcc20 |> filter(top_ba == 1) |> nrow(),
  acc20 |> filter(top_ba == 1) |> nrow()
) * 100 / stats$size

stats$top_uni <- c(
  fcc18 |> filter(top_uni == 1) |> nrow(),
  acc18 |> filter(top_uni == 1) |> nrow(),
  fcc19 |> filter(top_uni == 1) |> nrow(),
  acc19 |> filter(top_uni == 1) |> nrow(),
  fcc20 |> filter(top_uni == 1) |> nrow(),
  acc20 |> filter(top_uni == 1) |> nrow()
) * 100 / stats$size

stats$pla <- c(
  fcc18_career |> filter(lastpla == 1) |> nrow(),
  acc18_career |> filter(lastpla == 1) |> nrow(),
  fcc19_career |> filter(lastpla == 1) |> nrow(),
  acc19_career |> filter(lastpla == 1) |> nrow(),
  fcc20_career |> filter(lastpla == 1) |> nrow(),
  acc20_career |> filter(lastpla == 1) |> nrow()
) * 100 / stats$size

stats$sc <- c(
  fcc18_career |> filter(lastsc == 1) |> nrow(),
  acc18_career |> filter(lastsc == 1) |> nrow(),
  fcc19_career |> filter(lastsc == 1) |> nrow(),
  acc19_career |> filter(lastsc == 1) |> nrow(),
  fcc20_career |> filter(lastsc == 1) |> nrow(),
  acc20_career |> filter(lastsc == 1) |> nrow()
) * 100 / stats$size

stats$SOE <- c(
  fcc18_career |> filter(lastSOE == 1) |> nrow(),
  acc18_career |> filter(lastSOE == 1) |> nrow(),
  fcc19_career |> filter(lastSOE == 1) |> nrow(),
  acc19_career |> filter(lastSOE == 1) |> nrow(),
  fcc20_career |> filter(lastSOE == 1) |> nrow(),
  acc20_career |> filter(lastSOE == 1) |> nrow()
) * 100 / stats$size

stats$party <- c(
  fcc18_career |> filter(lastparty == 1) |> nrow(),
  acc18_career |> filter(lastparty == 1) |> nrow(),
  fcc19_career |> filter(lastparty == 1) |> nrow(),
  acc19_career |> filter(lastparty == 1) |> nrow(),
  fcc20_career |> filter(lastparty == 1) |> nrow(),
  acc20_career |> filter(lastparty == 1) |> nrow()
) * 100 / stats$size

stats$province <- c(
  fcc18_career |> filter(lastpro == 1) |> nrow(),
  acc18_career |> filter(lastpro == 1) |> nrow(),
  fcc19_career |> filter(lastpro == 1) |> nrow(),
  acc19_career |> filter(lastpro == 1) |> nrow(),
  fcc20_career |> filter(lastpro == 1) |> nrow(),
  acc20_career |> filter(lastpro == 1) |> nrow()
) * 100 / stats$size

stats$CPPCC <- c(
  fcc18_career |> filter(lastCPPCC == 1) |> nrow(),
  acc18_career |> filter(lastCPPCC == 1) |> nrow(),
  fcc19_career |> filter(lastCPPCC == 1) |> nrow(),
  acc19_career |> filter(lastCPPCC == 1) |> nrow(),
  fcc20_career |> filter(lastCPPCC == 1) |> nrow(),
  acc20_career |> filter(lastCPPCC == 1) |> nrow()
) * 100 / stats$size

stats$NPC <- c(
  fcc18_career |> filter(lastNPC == 1) |> nrow(),
  acc18_career |> filter(lastNPC == 1) |> nrow(),
  fcc19_career |> filter(lastNPC == 1) |> nrow(),
  acc19_career |> filter(lastNPC == 1) |> nrow(),
  fcc20_career |> filter(lastNPC == 1) |> nrow(),
  acc20_career |> filter(lastNPC == 1) |> nrow()
) * 100 / stats$size

stats$univ <- c(
  fcc18_career |> filter(lastuniv == 1) |> nrow(),
  acc18_career |> filter(lastuniv == 1) |> nrow(),
  fcc19_career |> filter(lastuniv == 1) |> nrow(),
  acc19_career |> filter(lastuniv == 1) |> nrow(),
  fcc20_career |> filter(lastuniv == 1) |> nrow(),
  acc20_career |> filter(lastuniv == 1) |> nrow()
) * 100 / stats$size

stats$massorg <- c(
  fcc18_career |> filter(lastmo == 1) |> nrow(),
  acc18_career |> filter(lastmo == 1) |> nrow(),
  fcc19_career |> filter(lastmo == 1) |> nrow(),
  acc19_career |> filter(lastmo == 1) |> nrow(),
  fcc20_career |> filter(lastmo == 1) |> nrow(),
  acc20_career |> filter(lastmo == 1) |> nrow()
) * 100 / stats$size

bprovince <- data.frame(matrix(nrow = 6, ncol = nrow(province)))
for (i in 1:nrow(province)) {
  bprovince[, i] <- c(
    fcc18 |> filter(bpro == i) |> nrow(),
    acc18 |> filter(bpro == i) |> nrow(),
    fcc19 |> filter(bpro == i) |> nrow(),
    acc19 |> filter(bpro == i) |> nrow(),
    fcc20 |> filter(bpro == i) |> nrow(),
    acc20 |> filter(bpro == i) |> nrow()
  )
}
colnames(bprovince) <- province$ename1
stats <- stats |> cbind(bprovince)

stats$width <- c(
  fcc18_career|> dplyr::select(width) |> colMeans(na.rm = T),
  acc18_career|> dplyr::select(width) |> colMeans(na.rm = T),
  fcc19_career|> dplyr::select(width) |> colMeans(na.rm = T),
  acc19_career|> dplyr::select(width) |> colMeans(na.rm = T),
  fcc20_career|> dplyr::select(width) |> colMeans(na.rm = T),
  acc20_career|> dplyr::select(width) |> colMeans(na.rm = T)
)

stats$narrow <- c(
  fcc18_career |> filter(narrow == 1) |> nrow(),
  acc18_career |> filter(narrow == 1) |> nrow(),
  fcc19_career |> filter(narrow == 1) |> nrow(),
  acc19_career |> filter(narrow == 1) |> nrow(),
  fcc20_career |> filter(narrow == 1) |> nrow(),
  acc20_career |> filter(narrow == 1) |> nrow()
) * 100 / stats$size

stats$narrow_alt <- c(
  fcc18_career |> filter(narrow_alt == 1) |> nrow(),
  acc18_career |> filter(narrow_alt == 1) |> nrow(),
  fcc19_career |> filter(narrow_alt == 1) |> nrow(),
  acc19_career |> filter(narrow_alt == 1) |> nrow(),
  fcc20_career |> filter(narrow_alt == 1) |> nrow(),
  acc20_career |> filter(narrow_alt == 1) |> nrow()
) * 100 / stats$size

#========Networks========
removelst <- c()
fcc18_network <- fcc18_career |> 
  dplyr::select(ename) |> 
  left_join(network[, c("ename", "Xi Jinping", "Li Keqiang", "Wang Qishan", "Li Xi", "Li Qiang", "Cai Qi")])
fcc18_network[is.na(fcc18_network)] = 0
if (nrow(distinct(fcc18_network)) == nrow(fcc18)) {
  fcc18_network <- distinct(fcc18_network)
} else {
  for (i in 1:(nrow(fcc18_network)-1)) {
    for (j in (i+1):nrow(fcc18_network)) {
      #print(paste(i,j))
      if (fcc18_network$ename[j] == fcc18_network$ename[i]){
        print(fcc18_network$ename[j])
        fcc18_network[i, 2:ncol(fcc18_network)] = lapply(fcc18_network[c(i,j), 2:ncol(fcc18_network)], max)
        removelst <- c(removelst, j)
      } else {break}
    }
  }
}
if (length(removelst) > 0){
  fcc18_network <- fcc18_network[-removelst, ]
}
if (nrow(fcc18_network) != nrow(fcc18_career)) {
  print("Require Manual Review")
}

removelst <- c()
acc18_network <- acc18_career |> 
  dplyr::select(ename) |> 
  left_join(network[, c("ename", "Xi Jinping", "Li Keqiang", "Wang Qishan", "Li Xi", "Li Qiang", "Cai Qi")])
acc18_network[is.na(acc18_network)] = 0
if (nrow(distinct(acc18_network)) == nrow(acc18)) {
  acc18_network <- distinct(acc18_network)
} else {
  for (i in 1:(nrow(acc18_network)-1)) {
    for (j in (i+1):nrow(acc18_network)) {
      if (acc18_network$ename[j] == acc18_network$ename[i]){
        print(acc18_network$ename[j])
        acc18_network[i, 2:ncol(acc18_network)] = lapply(acc18_network[c(i,j), 2:ncol(acc18_network)], max)
        removelst <- c(removelst, j)
      } else {break}
    }
  }
}
if (length(removelst) > 0) {
  acc18_network <- acc18_network[-removelst, ]
}
if (nrow(acc18_network) != nrow(acc18_career)) {
  print("Require Manual Review")
}

removelst <- c()
fcc19_network <- fcc19_career |> 
  dplyr::select(ename) |> 
  left_join(network[, c("ename", "Xi Jinping", "Li Keqiang", "Wang Qishan", "Li Xi", "Li Qiang", "Cai Qi")])
fcc19_network[is.na(fcc19_network)] = 0
if (nrow(distinct(fcc19_network)) == nrow(fcc19)) {
  fcc19_network <- distinct(fcc19_network)
} else {
  for (i in 1:(nrow(fcc19_network)-1)) {
    for (j in (i+1):nrow(fcc19_network)) {
      #print(paste(i,j))
      if (fcc19_network$ename[j] == fcc19_network$ename[i]){
        print(fcc19_network$ename[j])
        fcc19_network[i, 2:ncol(fcc19_network)] = lapply(fcc19_network[c(i,j), 2:ncol(fcc19_network)], max)
        removelst <- c(removelst, j)
      } else {break}
    }
  }
}
if (length(removelst) > 0) {
  fcc19_network <- fcc19_network[-removelst, ]
}
if (nrow(fcc19_network) != nrow(fcc19_career)) {
  print("Require Manual Review")
}

removelst <- c()
acc19_network <- acc19_career |> 
  dplyr::select(ename) |> 
  left_join(network[, c("ename", "Xi Jinping", "Li Keqiang", "Wang Qishan", "Li Xi", "Li Qiang", "Cai Qi")])
acc19_network[is.na(acc19_network)] = 0
if (nrow(distinct(acc19_network)) == nrow(acc19)) {
  acc19_network <- distinct(acc19_network)
} else {
  for (i in 1:(nrow(acc19_network)-1)) {
    for (j in (i+1):nrow(acc19_network)) {
      #print(paste(i,j))
      if (acc19_network$ename[j] == acc19_network$ename[i]){
        print(acc19_network$ename[j])
        acc19_network[i, 2:ncol(acc19_network)] = lapply(acc19_network[c(i,j), 2:ncol(acc19_network)], max)
        removelst <- c(removelst, j)
      } else {break}
    }
  }
}
if (length(removelst) > 0){
  acc19_network <- acc19_network[-removelst, ]
}
if (nrow(acc19_network) != nrow(acc19_career)) {
  print("Require Manual Review")
}

removelst <- c()
fcc20_network <- fcc20_career |> 
  dplyr::select(ename) |> 
  left_join(network[, c("ename", "Xi Jinping", "Li Keqiang", "Wang Qishan", "Li Xi", "Li Qiang", "Cai Qi")])
fcc20_network[is.na(fcc20_network)] = 0
if (nrow(distinct(fcc20_network)) == nrow(fcc20)) {
  fcc20_network <- distinct(fcc20_network)
} else {
  for (i in 1:(nrow(fcc20_network)-1)) {
    for (j in (i+1):nrow(fcc20_network)) {
      #print(paste(i,j))
      if (fcc20_network$ename[j] == fcc20_network$ename[i]){
        print(fcc20_network$ename[j])
        fcc20_network[i, 2:ncol(fcc20_network)] = lapply(fcc20_network[c(i,j), 2:ncol(fcc20_network)], max)
        removelst <- c(removelst, j)
      } else {break}
    }
  }
}
if (length(removelst) > 0) {
  fcc20_network <- fcc20_network[-removelst, ]
}
fcc20_network <- fcc20_network[1:5, ] |> 
  rbind(fcc20_network[5:nrow(fcc20_network), ])
if (nrow(fcc20_network) != nrow(fcc20_career)) {
  print("Require Manual Review")
}

removelst <- c()
acc20_network <- acc20_career |> 
  dplyr::select(ename) |> 
  left_join(network[, c("ename", "Xi Jinping", "Li Keqiang", "Wang Qishan", "Li Xi", "Li Qiang", "Cai Qi")])
acc20_network[is.na(acc20_network)] = 0
if (nrow(distinct(acc20_network)) == nrow(acc20)) {
  acc20_network <- distinct(acc20_network)
} else {
  for (i in 1:(nrow(acc20_network)-1)) {
    for (j in (i+1):nrow(acc20_network)) {
      #print(paste(i,j))
      if (acc20_network$ename[j] == acc20_network$ename[i]){
        print(acc20_network$ename[j])
        acc20_network[i, 2:ncol(acc20_network)] = lapply(acc20_network[c(i,j), 2:ncol(acc20_network)], max)
        removelst <- c(removelst, j)
      } else {break}
    }
  }
}
if (length(removelst) > 0) {
  acc20_network <- acc20_network[-removelst, ]
}
if (nrow(acc20_network) != nrow(acc20_career)) {
  print("Require Manual Review")
}

#====Xi Jinping====
stats$XJP_0 <- c(
  fcc18_network |> filter(`Xi Jinping` == 0) |> nrow(),
  acc18_network |> filter(`Xi Jinping` == 0) |> nrow(),
  fcc19_network |> filter(`Xi Jinping` == 0) |> nrow(),
  acc19_network |> filter(`Xi Jinping` == 0) |> nrow(),
  fcc20_network |> filter(`Xi Jinping` == 0) |> nrow(),
  acc20_network |> filter(`Xi Jinping` == 0) |> nrow()
) * 100 / stats$size

stats$XJP_1 <- c(
  fcc18_network |> filter(`Xi Jinping` == 1) |> nrow(),
  acc18_network |> filter(`Xi Jinping` == 1) |> nrow(),
  fcc19_network |> filter(`Xi Jinping` == 1) |> nrow(),
  acc19_network |> filter(`Xi Jinping` == 1) |> nrow(),
  fcc20_network |> filter(`Xi Jinping` == 1) |> nrow(),
  acc20_network |> filter(`Xi Jinping` == 1) |> nrow()
) * 100 / stats$size

stats$XJP_2 <- c(
  fcc18_network |> filter(`Xi Jinping` == 2) |> nrow(),
  acc18_network |> filter(`Xi Jinping` == 2) |> nrow(),
  fcc19_network |> filter(`Xi Jinping` == 2) |> nrow(),
  acc19_network |> filter(`Xi Jinping` == 2) |> nrow(),
  fcc20_network |> filter(`Xi Jinping` == 2) |> nrow(),
  acc20_network |> filter(`Xi Jinping` == 2) |> nrow()
) * 100 / stats$size

stats$XJP <- 100 - stats$XJP_0

stats$XJP_avg <- c(
  fcc18_network |> dplyr::select(`Xi Jinping`) |> colMeans(na.rm = T),
  acc18_network |> dplyr::select(`Xi Jinping`) |> colMeans(na.rm = T),
  fcc19_network |> dplyr::select(`Xi Jinping`) |> colMeans(na.rm = T),
  acc19_network |> dplyr::select(`Xi Jinping`) |> colMeans(na.rm = T),
  fcc20_network |> dplyr::select(`Xi Jinping`) |> colMeans(na.rm = T),
  acc20_network |> dplyr::select(`Xi Jinping`) |> colMeans(na.rm = T)
)

#====Li Keqiang====
stats$LKQ_0 <- c(
  fcc18_network |> filter(`Li Keqiang` == 0) |> nrow(),
  acc18_network |> filter(`Li Keqiang` == 0) |> nrow(),
  fcc19_network |> filter(`Li Keqiang` == 0) |> nrow(),
  acc19_network |> filter(`Li Keqiang` == 0) |> nrow(),
  fcc20_network |> filter(`Li Keqiang` == 0) |> nrow(),
  acc20_network |> filter(`Li Keqiang` == 0) |> nrow()
) * 100 / stats$size

stats$LKQ_1 <- c(
  fcc18_network |> filter(`Li Keqiang` == 1) |> nrow(),
  acc18_network |> filter(`Li Keqiang` == 1) |> nrow(),
  fcc19_network |> filter(`Li Keqiang` == 1) |> nrow(),
  acc19_network |> filter(`Li Keqiang` == 1) |> nrow(),
  fcc20_network |> filter(`Li Keqiang` == 1) |> nrow(),
  acc20_network |> filter(`Li Keqiang` == 1) |> nrow()
) * 100 / stats$size

stats$LKQ_2 <- c(
  fcc18_network |> filter(`Li Keqiang` == 2) |> nrow(),
  acc18_network |> filter(`Li Keqiang` == 2) |> nrow(),
  fcc19_network |> filter(`Li Keqiang` == 2) |> nrow(),
  acc19_network |> filter(`Li Keqiang` == 2) |> nrow(),
  fcc20_network |> filter(`Li Keqiang` == 2) |> nrow(),
  acc20_network |> filter(`Li Keqiang` == 2) |> nrow()
) * 100 / stats$size

stats$LKQ <- 100 - stats$LKQ_0

stats$LKQ_avg <- c(
  fcc18_network |> dplyr::select(`Li Keqiang`) |> colMeans(na.rm = T),
  acc18_network |> dplyr::select(`Li Keqiang`) |> colMeans(na.rm = T),
  fcc19_network |> dplyr::select(`Li Keqiang`) |> colMeans(na.rm = T),
  acc19_network |> dplyr::select(`Li Keqiang`) |> colMeans(na.rm = T),
  fcc20_network |> dplyr::select(`Li Keqiang`) |> colMeans(na.rm = T),
  acc20_network |> dplyr::select(`Li Keqiang`) |> colMeans(na.rm = T)
)

#====Wang Qishan====
stats$WQS_0 <- c(
  fcc18_network |> filter(`Wang Qishan` == 0) |> nrow(),
  acc18_network |> filter(`Wang Qishan` == 0) |> nrow(),
  fcc19_network |> filter(`Wang Qishan` == 0) |> nrow(),
  acc19_network |> filter(`Wang Qishan` == 0) |> nrow(),
  fcc20_network |> filter(`Wang Qishan` == 0) |> nrow(),
  acc20_network |> filter(`Wang Qishan` == 0) |> nrow()
) * 100 / stats$size

stats$WQS_1 <- c(
  fcc18_network |> filter(`Wang Qishan` == 1) |> nrow(),
  acc18_network |> filter(`Wang Qishan` == 1) |> nrow(),
  fcc19_network |> filter(`Wang Qishan` == 1) |> nrow(),
  acc19_network |> filter(`Wang Qishan` == 1) |> nrow(),
  fcc20_network |> filter(`Wang Qishan` == 1) |> nrow(),
  acc20_network |> filter(`Wang Qishan` == 1) |> nrow()
) * 100 / stats$size

stats$WQS_2 <- c(
  fcc18_network |> filter(`Wang Qishan` == 2) |> nrow(),
  acc18_network |> filter(`Wang Qishan` == 2) |> nrow(),
  fcc19_network |> filter(`Wang Qishan` == 2) |> nrow(),
  acc19_network |> filter(`Wang Qishan` == 2) |> nrow(),
  fcc20_network |> filter(`Wang Qishan` == 2) |> nrow(),
  acc20_network |> filter(`Wang Qishan` == 2) |> nrow()
) * 100 / stats$size

stats$WQS <- 100 - stats$WQS_0

stats$WQS_avg <- c(
  fcc18_network |> dplyr::select(`Wang Qishan`) |> colMeans(na.rm = T),
  acc18_network |> dplyr::select(`Wang Qishan`) |> colMeans(na.rm = T),
  fcc19_network |> dplyr::select(`Wang Qishan`) |> colMeans(na.rm = T),
  acc19_network |> dplyr::select(`Wang Qishan`) |> colMeans(na.rm = T),
  fcc20_network |> dplyr::select(`Wang Qishan`) |> colMeans(na.rm = T),
  acc20_network |> dplyr::select(`Wang Qishan`) |> colMeans(na.rm = T)
)

#====Li Qiang====
stats$LQ_0 <- c(
  fcc18_network |> filter(`Li Qiang` == 0) |> nrow(),
  acc18_network |> filter(`Li Qiang` == 0) |> nrow(),
  fcc19_network |> filter(`Li Qiang` == 0) |> nrow(),
  acc19_network |> filter(`Li Qiang` == 0) |> nrow(),
  fcc20_network |> filter(`Li Qiang` == 0) |> nrow(),
  acc20_network |> filter(`Li Qiang` == 0) |> nrow()
) * 100 / stats$size

stats$LQ_1 <- c(
  fcc18_network |> filter(`Li Qiang` == 1) |> nrow(),
  acc18_network |> filter(`Li Qiang` == 1) |> nrow(),
  fcc19_network |> filter(`Li Qiang` == 1) |> nrow(),
  acc19_network |> filter(`Li Qiang` == 1) |> nrow(),
  fcc20_network |> filter(`Li Qiang` == 1) |> nrow(),
  acc20_network |> filter(`Li Qiang` == 1) |> nrow()
) * 100 / stats$size

stats$LQ_2 <- c(
  fcc18_network |> filter(`Li Qiang` == 2) |> nrow(),
  acc18_network |> filter(`Li Qiang` == 2) |> nrow(),
  fcc19_network |> filter(`Li Qiang` == 2) |> nrow(),
  acc19_network |> filter(`Li Qiang` == 2) |> nrow(),
  fcc20_network |> filter(`Li Qiang` == 2) |> nrow(),
  acc20_network |> filter(`Li Qiang` == 2) |> nrow()
) * 100 / stats$size

stats$LQ <- 100 - stats$LQ_0

stats$LQ_avg <- c(
  fcc18_network |> dplyr::select(`Li Qiang`) |> colMeans(na.rm = T),
  acc18_network |> dplyr::select(`Li Qiang`) |> colMeans(na.rm = T),
  fcc19_network |> dplyr::select(`Li Qiang`) |> colMeans(na.rm = T),
  acc19_network |> dplyr::select(`Li Qiang`) |> colMeans(na.rm = T),
  fcc20_network |> dplyr::select(`Li Qiang`) |> colMeans(na.rm = T),
  acc20_network |> dplyr::select(`Li Qiang`) |> colMeans(na.rm = T)
)

#====Li Xi====
stats$LX_0 <- c(
  fcc18_network |> filter(`Li Xi` == 0) |> nrow(),
  acc18_network |> filter(`Li Xi` == 0) |> nrow(),
  fcc19_network |> filter(`Li Xi` == 0) |> nrow(),
  acc19_network |> filter(`Li Xi` == 0) |> nrow(),
  fcc20_network |> filter(`Li Xi` == 0) |> nrow(),
  acc20_network |> filter(`Li Xi` == 0) |> nrow()
) * 100 / stats$size

stats$LX_1 <- c(
  fcc18_network |> filter(`Li Xi` == 1) |> nrow(),
  acc18_network |> filter(`Li Xi` == 1) |> nrow(),
  fcc19_network |> filter(`Li Xi` == 1) |> nrow(),
  acc19_network |> filter(`Li Xi` == 1) |> nrow(),
  fcc20_network |> filter(`Li Xi` == 1) |> nrow(),
  acc20_network |> filter(`Li Xi` == 1) |> nrow()
) * 100 / stats$size

stats$LX_2 <- c(
  fcc18_network |> filter(`Li Xi` == 2) |> nrow(),
  acc18_network |> filter(`Li Xi` == 2) |> nrow(),
  fcc19_network |> filter(`Li Xi` == 2) |> nrow(),
  acc19_network |> filter(`Li Xi` == 2) |> nrow(),
  fcc20_network |> filter(`Li Xi` == 2) |> nrow(),
  acc20_network |> filter(`Li Xi` == 2) |> nrow()
) * 100 / stats$size

stats$LX <- 100 - stats$LX_0

stats$LX_avg <- c(
  fcc18_network |> dplyr::select(`Li Xi`) |> colMeans(na.rm = T),
  acc18_network |> dplyr::select(`Li Xi`) |> colMeans(na.rm = T),
  fcc19_network |> dplyr::select(`Li Xi`) |> colMeans(na.rm = T),
  acc19_network |> dplyr::select(`Li Xi`) |> colMeans(na.rm = T),
  fcc20_network |> dplyr::select(`Li Xi`) |> colMeans(na.rm = T),
  acc20_network |> dplyr::select(`Li Xi`) |> colMeans(na.rm = T)
)

#====Cai Qi====
stats$CQ_0 <- c(
  fcc18_network |> filter(`Cai Qi` == 0) |> nrow(),
  acc18_network |> filter(`Cai Qi` == 0) |> nrow(),
  fcc19_network |> filter(`Cai Qi` == 0) |> nrow(),
  acc19_network |> filter(`Cai Qi` == 0) |> nrow(),
  fcc20_network |> filter(`Cai Qi` == 0) |> nrow(),
  acc20_network |> filter(`Cai Qi` == 0) |> nrow()
) * 100 / stats$size

stats$CQ_1 <- c(
  fcc18_network |> filter(`Cai Qi` == 1) |> nrow(),
  acc18_network |> filter(`Cai Qi` == 1) |> nrow(),
  fcc19_network |> filter(`Cai Qi` == 1) |> nrow(),
  acc19_network |> filter(`Cai Qi` == 1) |> nrow(),
  fcc20_network |> filter(`Cai Qi` == 1) |> nrow(),
  acc20_network |> filter(`Cai Qi` == 1) |> nrow()
) * 100 / stats$size

stats$CQ_2 <- c(
  fcc18_network |> filter(`Cai Qi` == 2) |> nrow(),
  acc18_network |> filter(`Cai Qi` == 2) |> nrow(),
  fcc19_network |> filter(`Cai Qi` == 2) |> nrow(),
  acc19_network |> filter(`Cai Qi` == 2) |> nrow(),
  fcc20_network |> filter(`Cai Qi` == 2) |> nrow(),
  acc20_network |> filter(`Cai Qi` == 2) |> nrow()
) * 100 / stats$size

stats$CQ <- 100 - stats$CQ_0

stats$CQ_avg <- c(
  fcc18_network |> dplyr::select(`Cai Qi`) |> colMeans(na.rm = T),
  acc18_network |> dplyr::select(`Cai Qi`) |> colMeans(na.rm = T),
  fcc19_network |> dplyr::select(`Cai Qi`) |> colMeans(na.rm = T),
  acc19_network |> dplyr::select(`Cai Qi`) |> colMeans(na.rm = T),
  fcc20_network |> dplyr::select(`Cai Qi`) |> colMeans(na.rm = T),
  acc20_network |> dplyr::select(`Cai Qi`) |> colMeans(na.rm = T)
)


#========Policing and Stability========
security <- data.frame(matrix(nrow = 9, ncol = 1)) 
colnames(security) <- "session"
security$session <- c("18", "18", "18", "19", "19", "19", "20", "20", "20")
security$member <- c("Politburo", "Full", "Alternate", 
                  "Politburo", "Full", "Alternate",
                  "Politburo", "Full", "Alternate")
security$size <- c(
  politburo18 |> nrow(),
  fcc18 |> nrow(),
  acc18 |> nrow(),
  politburo19 |> nrow(),
  fcc19 |> nrow(),
  acc19 |> nrow(),
  politburo20 |> nrow(),
  fcc20 |> nrow(),
  acc20 |> nrow()
)

security$MPS <- c(
  politburo18_career |> filter(MPS == 1) |> nrow(),
  fcc18_career |> filter(MPS == 1) |> nrow(),
  acc18_career |> filter(MPS == 1) |> nrow(),
  politburo19_career |> filter(MPS == 1) |> nrow(),
  fcc19_career |> filter(MPS == 1) |> nrow(),
  acc19_career |> filter(MPS == 1) |> nrow(),
  politburo20_career |> filter(MPS == 1) |> nrow(),
  fcc20_career |> filter(MPS == 1) |> nrow(),
  acc20_career |> filter(MPS == 1) |> nrow()
)

security$MSS <- c(
  politburo18_career |> filter(MSS == 1) |> nrow(),
  fcc18_career |> filter(MSS == 1) |> nrow(),
  acc18_career |> filter(MSS == 1) |> nrow(),
  politburo19_career |> filter(MSS == 1) |> nrow(),
  fcc19_career |> filter(MSS == 1) |> nrow(),
  acc19_career |> filter(MSS == 1) |> nrow(),
  politburo20_career |> filter(MSS == 1) |> nrow(),
  fcc20_career |> filter(MSS == 1) |> nrow(),
  acc20_career |> filter(MSS == 1) |> nrow()
)

security$CLPLG <- c(
  politburo18_career |> filter(CLPLG == 1) |> nrow(),
  fcc18_career |> filter(CLPLG == 1) |> nrow(),
  acc18_career |> filter(CLPLG == 1) |> nrow(),
  politburo19_career |> filter(CLPLG == 1) |> nrow(),
  fcc19_career |> filter(CLPLG == 1) |> nrow(),
  acc19_career |> filter(CLPLG == 1) |> nrow(),
  politburo20_career |> filter(CLPLG == 1) |> nrow(),
  fcc20_career |> filter(CLPLG == 1) |> nrow(),
  acc20_career |> filter(CLPLG == 1) |> nrow()
)

security$policing <- c(
  politburo18_career |> filter(policing == 1) |> nrow(),
  fcc18_career |> filter(policing == 1) |> nrow(),
  acc18_career |> filter(policing == 1) |> nrow(),
  politburo19_career |> filter(policing == 1) |> nrow(),
  fcc19_career |> filter(policing == 1) |> nrow(),
  acc19_career |> filter(policing == 1) |> nrow(),
  politburo20_career |> filter(policing == 1) |> nrow(),
  fcc20_career |> filter(policing == 1) |> nrow(),
  acc20_career |> filter(policing == 1) |> nrow()
)

security$PSD <- c(
  politburo18_career |> filter(PSD == 1 & policing == 0) |> nrow(),
  fcc18_career |> filter(PSD == 1 & policing == 0) |> nrow(),
  acc18_career |> filter(PSD == 1 & policing == 0) |> nrow(),
  politburo19_career |> filter(PSD == 1 & policing == 0) |> nrow(),
  fcc19_career |> filter(PSD == 1 & policing == 0) |> nrow(),
  acc19_career |> filter(PSD == 1 & policing == 0) |> nrow(),
  politburo20_career |> filter(PSD == 1 & policing == 0) |> nrow(),
  fcc20_career |> filter(PSD == 1 & policing == 0) |> nrow(),
  acc20_career |> filter(PSD == 1 & policing == 0) |> nrow()
)

security$SSD <- c(
  politburo18_career |> filter(SSD == 1 & policing == 0) |> nrow(),
  fcc18_career |> filter(SSD == 1 & policing == 0) |> nrow(),
  acc18_career |> filter(SSD == 1 & policing == 0) |> nrow(),
  politburo19_career |> filter(SSD == 1 & policing == 0) |> nrow(),
  fcc19_career |> filter(SSD == 1 & policing == 0) |> nrow(),
  acc19_career |> filter(SSD == 1 & policing == 0) |> nrow(),
  politburo20_career |> filter(SSD == 1 & policing == 0) |> nrow(),
  fcc20_career |> filter(SSD == 1 & policing == 0) |> nrow(),
  acc20_career |> filter(SSD == 1 & policing == 0) |> nrow()
)

security$PLPC <- c(
  politburo18_career |> filter(PLPC == 1 & policing == 0) |> nrow(),
  fcc18_career |> filter(PLPC == 1 & policing == 0) |> nrow(),
  acc18_career |> filter(PLPC == 1 & policing == 0) |> nrow(),
  politburo19_career |> filter(PLPC == 1 & policing == 0) |> nrow(),
  fcc19_career |> filter(PLPC == 1 & policing == 0) |> nrow(),
  acc19_career |> filter(PLPC == 1 & policing == 0) |> nrow(),
  politburo20_career |> filter(PLPC == 1 & policing == 0) |> nrow(),
  fcc20_career |> filter(PLPC == 1 & policing == 0) |> nrow(),
  acc20_career |> filter(PLPC == 1 & policing == 0) |> nrow()
)

security$widepolicing <- c(
  politburo18_career |> filter(widepolicing == 1) |> nrow(),
  fcc18_career |> filter(widepolicing == 1) |> nrow(),
  acc18_career |> filter(widepolicing == 1) |> nrow(),
  politburo19_career |> filter(widepolicing == 1) |> nrow(),
  fcc19_career |> filter(widepolicing == 1) |> nrow(),
  acc19_career |> filter(widepolicing == 1) |> nrow(),
  politburo20_career |> filter(widepolicing == 1) |> nrow(),
  fcc20_career |> filter(widepolicing == 1) |> nrow(),
  acc20_career |> filter(widepolicing == 1) |> nrow()
)
#========Heatmap========
positions = c("PLA", "State Council", "SOE", "Province", "Party", "University", 
              "NPC", "CPPCC", "Mass Organization")
#====Female====
femalechange <- data.frame(matrix(nrow = 4, ncol = 1))
colnames(femalechange) <- "session" 
femalechange$session <- c("19", "19", "20", "20")
femalechange$member <- c("Full", "Alternate", "Full", "Alternate")
femalechange$size <- c(
  fcc19 |> nrow(),
  acc19 |> nrow(),
  fcc20 |> nrow(),
  acc20 |> nrow()
)
femalechange$pla <- c(
  fcc19_career |> filter(lastpla == 1 & gender == 1) |> nrow(),
  acc19_career |> filter(lastpla == 1 & gender == 1) |> nrow(),
  fcc20_career |> filter(lastpla == 1 & gender == 1) |> nrow(),
  acc20_career |> filter(lastpla == 1 & gender == 1) |> nrow()
) * 100 / femalechange$size
femalechange$sc <- c(
  fcc19_career |> filter(lastsc == 1 & gender == 1) |> nrow(),
  acc19_career |> filter(lastsc == 1 & gender == 1) |> nrow(),
  fcc20_career |> filter(lastsc == 1 & gender == 1) |> nrow(),
  acc20_career |> filter(lastsc == 1 & gender == 1) |> nrow()
) * 100 / femalechange$size
femalechange$SOE <- c(
  fcc19_career |> filter(lastSOE == 1 & gender == 1) |> nrow(),
  acc19_career |> filter(lastSOE == 1 & gender == 1) |> nrow(),
  fcc20_career |> filter(lastSOE == 1 & gender == 1) |> nrow(),
  acc20_career |> filter(lastSOE == 1 & gender == 1) |> nrow()
) * 100 / femalechange$size
femalechange$pro <- c(
  fcc19_career |> filter(lastpro == 1 & gender == 1) |> nrow(),
  acc19_career |> filter(lastpro == 1 & gender == 1) |> nrow(),
  fcc20_career |> filter(lastpro == 1 & gender == 1) |> nrow(),
  acc20_career |> filter(lastpro == 1 & gender == 1) |> nrow()
) * 100 / femalechange$size
femalechange$party <- c(
  fcc19_career |> filter(lastparty == 1 & gender == 1) |> nrow(),
  acc19_career |> filter(lastparty == 1 & gender == 1) |> nrow(),
  fcc20_career |> filter(lastparty == 1 & gender == 1) |> nrow(),
  acc20_career |> filter(lastparty == 1 & gender == 1) |> nrow()
) * 100 / femalechange$size
femalechange$univ <- c(
  fcc19_career |> filter(lastuniv == 1 & gender == 1) |> nrow(),
  acc19_career |> filter(lastuniv == 1 & gender == 1) |> nrow(),
  fcc20_career |> filter(lastuniv == 1 & gender == 1) |> nrow(),
  acc20_career |> filter(lastuniv == 1 & gender == 1) |> nrow()
) * 100 / femalechange$size
femalechange$NPC <- c(
  fcc19_career |> filter(lastNPC == 1 & gender == 1) |> nrow(),
  acc19_career |> filter(lastNPC == 1 & gender == 1) |> nrow(),
  fcc20_career |> filter(lastNPC == 1 & gender == 1) |> nrow(),
  acc20_career |> filter(lastNPC == 1 & gender == 1) |> nrow()
) * 100 / femalechange$size
femalechange$CPPCC <- c(
  fcc19_career |> filter(lastCPPCC == 1 & gender == 1) |> nrow(),
  acc19_career |> filter(lastCPPCC == 1 & gender == 1) |> nrow(),
  fcc20_career |> filter(lastCPPCC == 1 & gender == 1) |> nrow(),
  acc20_career |> filter(lastCPPCC == 1 & gender == 1) |> nrow()
) * 100 / femalechange$size
femalechange$mo <- c(
  fcc19_career |> filter(lastmo == 1 & gender == 1) |> nrow(),
  acc19_career |> filter(lastmo == 1 & gender == 1) |> nrow(),
  fcc20_career |> filter(lastmo == 1 & gender == 1) |> nrow(),
  acc20_career |> filter(lastmo == 1 & gender == 1) |> nrow()
) * 100 / femalechange$size
femalechange <- t(femalechange[,-1:-3]) |> as.data.frame()
femalechange$position <- positions
femalechange$full <- femalechange$V3 - femalechange$V1
femalechange$alt <- femalechange$V4 - femalechange$V2
femalechange <- femalechange[,-1:-4]

#====Minorities====
minoritychange <- data.frame(matrix(nrow = 4, ncol = 1))
colnames(minoritychange) <- "session" 
minoritychange$session <- c("19", "19", "20", "20")
minoritychange$member <- c("Full", "Alternate", "Full", "Alternate")
minoritychange$size <- c(
  fcc19 |> nrow(),
  acc19 |> nrow(),
  fcc20 |> nrow(),
  acc20 |> nrow()
)
minoritychange$pla <- c(
  fcc19_career |> filter(lastpla == 1 & ethnic > 0) |> nrow(),
  acc19_career |> filter(lastpla == 1 & ethnic > 0) |> nrow(),
  fcc20_career |> filter(lastpla == 1 & ethnic > 0) |> nrow(),
  acc20_career |> filter(lastpla == 1 & ethnic > 0) |> nrow()
) * 100 / minoritychange$size
minoritychange$sc <- c(
  fcc19_career |> filter(lastsc == 1 & ethnic > 0) |> nrow(),
  acc19_career |> filter(lastsc == 1 & ethnic > 0) |> nrow(),
  fcc20_career |> filter(lastsc == 1 & ethnic > 0) |> nrow(),
  acc20_career |> filter(lastsc == 1 & ethnic > 0) |> nrow()
) * 100 / minoritychange$size
minoritychange$SOE <- c(
  fcc19_career |> filter(lastSOE == 1 & ethnic > 0) |> nrow(),
  acc19_career |> filter(lastSOE == 1 & ethnic > 0) |> nrow(),
  fcc20_career |> filter(lastSOE == 1 & ethnic > 0) |> nrow(),
  acc20_career |> filter(lastSOE == 1 & ethnic > 0) |> nrow()
) * 100 / minoritychange$size
minoritychange$pro <- c(
  fcc19_career |> filter(lastpro == 1 & ethnic > 0) |> nrow(),
  acc19_career |> filter(lastpro == 1 & ethnic > 0) |> nrow(),
  fcc20_career |> filter(lastpro == 1 & ethnic > 0) |> nrow(),
  acc20_career |> filter(lastpro == 1 & ethnic > 0) |> nrow()
) * 100 / minoritychange$size
minoritychange$party <- c(
  fcc19_career |> filter(lastparty == 1 & ethnic > 0) |> nrow(),
  acc19_career |> filter(lastparty == 1 & ethnic > 0) |> nrow(),
  fcc20_career |> filter(lastparty == 1 & ethnic > 0) |> nrow(),
  acc20_career |> filter(lastparty == 1 & ethnic > 0) |> nrow()
) * 100 / minoritychange$size
minoritychange$univ <- c(
  fcc19_career |> filter(lastuniv == 1 & ethnic > 0) |> nrow(),
  acc19_career |> filter(lastuniv == 1 & ethnic > 0) |> nrow(),
  fcc20_career |> filter(lastuniv == 1 & ethnic > 0) |> nrow(),
  acc20_career |> filter(lastuniv == 1 & ethnic > 0) |> nrow()
) * 100 / minoritychange$size
minoritychange$NPC <- c(
  fcc19_career |> filter(lastNPC == 1 & ethnic > 0) |> nrow(),
  acc19_career |> filter(lastNPC == 1 & ethnic > 0) |> nrow(),
  fcc20_career |> filter(lastNPC == 1 & ethnic > 0) |> nrow(),
  acc20_career |> filter(lastNPC == 1 & ethnic > 0) |> nrow()
) * 100 / minoritychange$size
minoritychange$CPPCC <- c(
  fcc19_career |> filter(lastCPPCC == 1 & ethnic > 0) |> nrow(),
  acc19_career |> filter(lastCPPCC == 1 & ethnic > 0) |> nrow(),
  fcc20_career |> filter(lastCPPCC == 1 & ethnic > 0) |> nrow(),
  acc20_career |> filter(lastCPPCC == 1 & ethnic > 0) |> nrow()
) * 100 / minoritychange$size
minoritychange$mo <- c(
  fcc19_career |> filter(lastmo == 1 & ethnic > 0) |> nrow(),
  acc19_career |> filter(lastmo == 1 & ethnic > 0) |> nrow(),
  fcc20_career |> filter(lastmo == 1 & ethnic > 0) |> nrow(),
  acc20_career |> filter(lastmo == 1 & ethnic > 0) |> nrow()
) * 100 / minoritychange$size
minoritychange <- t(minoritychange[,-1:-3]) |> as.data.frame()
minoritychange$position <- positions
minoritychange$full <- minoritychange$V3 - minoritychange$V1
minoritychange$alt <- minoritychange$V4 - minoritychange$V2
minoritychange <- minoritychange[,-1:-4]

#====Top BA====
topunivchange <- data.frame(matrix(nrow = 4, ncol = 1))
colnames(topunivchange) <- "session" 
topunivchange$session <- c("19", "19", "20", "20")
topunivchange$member <- c("Full", "Alternate", "Full", "Alternate")
topunivchange$size <- c(
  fcc19 |> nrow(),
  acc19 |> nrow(),
  fcc20 |> nrow(),
  acc20 |> nrow()
)
topunivchange$pla <- c(
  fcc19_career |> filter(lastpla == 1 & top_ba == 1) |> nrow(),
  acc19_career |> filter(lastpla == 1 & top_ba == 1) |> nrow(),
  fcc20_career |> filter(lastpla == 1 & top_ba == 1) |> nrow(),
  acc20_career |> filter(lastpla == 1 & top_ba == 1) |> nrow()
) * 100 / topunivchange$size
topunivchange$sc <- c(
  fcc19_career |> filter(lastsc == 1 & top_ba == 1) |> nrow(),
  acc19_career |> filter(lastsc == 1 & top_ba == 1) |> nrow(),
  fcc20_career |> filter(lastsc == 1 & top_ba == 1) |> nrow(),
  acc20_career |> filter(lastsc == 1 & top_ba == 1) |> nrow()
) * 100 / topunivchange$size
topunivchange$SOE <- c(
  fcc19_career |> filter(lastSOE == 1 & top_ba == 1) |> nrow(),
  acc19_career |> filter(lastSOE == 1 & top_ba == 1) |> nrow(),
  fcc20_career |> filter(lastSOE == 1 & top_ba == 1) |> nrow(),
  acc20_career |> filter(lastSOE == 1 & top_ba == 1) |> nrow()
) * 100 / topunivchange$size
topunivchange$pro <- c(
  fcc19_career |> filter(lastpro == 1 & top_ba == 1) |> nrow(),
  acc19_career |> filter(lastpro == 1 & top_ba == 1) |> nrow(),
  fcc20_career |> filter(lastpro == 1 & top_ba == 1) |> nrow(),
  acc20_career |> filter(lastpro == 1 & top_ba == 1) |> nrow()
) * 100 / topunivchange$size
topunivchange$party <- c(
  fcc19_career |> filter(lastparty == 1 & top_ba == 1) |> nrow(),
  acc19_career |> filter(lastparty == 1 & top_ba == 1) |> nrow(),
  fcc20_career |> filter(lastparty == 1 & top_ba == 1) |> nrow(),
  acc20_career |> filter(lastparty == 1 & top_ba == 1) |> nrow()
) * 100 / topunivchange$size
topunivchange$univ <- c(
  fcc19_career |> filter(lastuniv == 1 & top_ba == 1) |> nrow(),
  acc19_career |> filter(lastuniv == 1 & top_ba == 1) |> nrow(),
  fcc20_career |> filter(lastuniv == 1 & top_ba == 1) |> nrow(),
  acc20_career |> filter(lastuniv == 1 & top_ba == 1) |> nrow()
) * 100 / topunivchange$size
topunivchange$NPC <- c(
  fcc19_career |> filter(lastNPC == 1 & top_ba == 1) |> nrow(),
  acc19_career |> filter(lastNPC == 1 & top_ba == 1) |> nrow(),
  fcc20_career |> filter(lastNPC == 1 & top_ba == 1) |> nrow(),
  acc20_career |> filter(lastNPC == 1 & top_ba == 1) |> nrow()
) * 100 / topunivchange$size
topunivchange$CPPCC <- c(
  fcc19_career |> filter(lastCPPCC == 1 & top_ba == 1) |> nrow(),
  acc19_career |> filter(lastCPPCC == 1 & top_ba == 1) |> nrow(),
  fcc20_career |> filter(lastCPPCC == 1 & top_ba == 1) |> nrow(),
  acc20_career |> filter(lastCPPCC == 1 & top_ba == 1) |> nrow()
) * 100 / topunivchange$size
topunivchange$mo <- c(
  fcc19_career |> filter(lastmo == 1 & top_ba == 1) |> nrow(),
  acc19_career |> filter(lastmo == 1 & top_ba == 1) |> nrow(),
  fcc20_career |> filter(lastmo == 1 & top_ba == 1) |> nrow(),
  acc20_career |> filter(lastmo == 1 & top_ba == 1) |> nrow()
) * 100 / topunivchange$size
topunivchange <- t(topunivchange[,-1:-3]) |> as.data.frame()
topunivchange$position <- positions
topunivchange$full <- topunivchange$V3 - topunivchange$V1
topunivchange$alt <- topunivchange$V4 - topunivchange$V2
topunivchange <- topunivchange[,-1:-4]

#====Narrow====
narrowchange <- data.frame(matrix(nrow = 4, ncol = 1))
colnames(narrowchange) <- "session" 
narrowchange$session <- c("19", "19", "20", "20")
narrowchange$member <- c("Full", "Alternate", "Full", "Alternate")
narrowchange$size <- c(
  fcc19 |> nrow(),
  acc19 |> nrow(),
  fcc20 |> nrow(),
  acc20 |> nrow()
)
narrowchange$pla <- c(
  fcc19_career |> filter(lastpla == 1 & narrow == 1) |> nrow(),
  acc19_career |> filter(lastpla == 1 & narrow == 1) |> nrow(),
  fcc20_career |> filter(lastpla == 1 & narrow == 1) |> nrow(),
  acc20_career |> filter(lastpla == 1 & narrow == 1) |> nrow()
) * 100 / narrowchange$size
narrowchange$sc <- c(
  fcc19_career |> filter(lastsc == 1 & narrow == 1) |> nrow(),
  acc19_career |> filter(lastsc == 1 & narrow == 1) |> nrow(),
  fcc20_career |> filter(lastsc == 1 & narrow == 1) |> nrow(),
  acc20_career |> filter(lastsc == 1 & narrow == 1) |> nrow()
) * 100 / narrowchange$size
narrowchange$SOE <- c(
  fcc19_career |> filter(lastSOE == 1 & narrow == 1) |> nrow(),
  acc19_career |> filter(lastSOE == 1 & narrow == 1) |> nrow(),
  fcc20_career |> filter(lastSOE == 1 & narrow == 1) |> nrow(),
  acc20_career |> filter(lastSOE == 1 & narrow == 1) |> nrow()
) * 100 / narrowchange$size
narrowchange$pro <- c(
  fcc19_career |> filter(lastpro == 1 & narrow == 1) |> nrow(),
  acc19_career |> filter(lastpro == 1 & narrow == 1) |> nrow(),
  fcc20_career |> filter(lastpro == 1 & narrow == 1) |> nrow(),
  acc20_career |> filter(lastpro == 1 & narrow == 1) |> nrow()
) * 100 / narrowchange$size
narrowchange$party <- c(
  fcc19_career |> filter(lastparty == 1 & narrow == 1) |> nrow(),
  acc19_career |> filter(lastparty == 1 & narrow == 1) |> nrow(),
  fcc20_career |> filter(lastparty == 1 & narrow == 1) |> nrow(),
  acc20_career |> filter(lastparty == 1 & narrow == 1) |> nrow()
) * 100 / narrowchange$size
narrowchange$univ <- c(
  fcc19_career |> filter(lastuniv == 1 & narrow == 1) |> nrow(),
  acc19_career |> filter(lastuniv == 1 & narrow == 1) |> nrow(),
  fcc20_career |> filter(lastuniv == 1 & narrow == 1) |> nrow(),
  acc20_career |> filter(lastuniv == 1 & narrow == 1) |> nrow()
) * 100 / narrowchange$size
narrowchange$NPC <- c(
  fcc19_career |> filter(lastNPC == 1 & narrow == 1) |> nrow(),
  acc19_career |> filter(lastNPC == 1 & narrow == 1) |> nrow(),
  fcc20_career |> filter(lastNPC == 1 & narrow == 1) |> nrow(),
  acc20_career |> filter(lastNPC == 1 & narrow == 1) |> nrow()
) * 100 / narrowchange$size
narrowchange$CPPCC <- c(
  fcc19_career |> filter(lastCPPCC == 1 & narrow == 1) |> nrow(),
  acc19_career |> filter(lastCPPCC == 1 & narrow == 1) |> nrow(),
  fcc20_career |> filter(lastCPPCC == 1 & narrow == 1) |> nrow(),
  acc20_career |> filter(lastCPPCC == 1 & narrow == 1) |> nrow()
) * 100 / narrowchange$size
narrowchange$mo <- c(
  fcc19_career |> filter(lastmo == 1 & narrow == 1) |> nrow(),
  acc19_career |> filter(lastmo == 1 & narrow == 1) |> nrow(),
  fcc20_career |> filter(lastmo == 1 & narrow == 1) |> nrow(),
  acc20_career |> filter(lastmo == 1 & narrow == 1) |> nrow()
) * 100 / narrowchange$size
narrowchange <- t(narrowchange[,-1:-3]) |> as.data.frame()
narrowchange$position <- positions
narrowchange$full <- narrowchange$V3 - narrowchange$V1
narrowchange$alt <- narrowchange$V4 - narrowchange$V2
narrowchange <- narrowchange[,-1:-4]
#====
changefull <- cbind(positions, femalechange$full, minoritychange$full, 
                    topunivchange$full, narrowchange$full) |> as.data.frame()
changealt <- cbind(positions, femalechange$alt, minoritychange$alt,
                   topunivchange$alt, narrowchange$alt) |> as.data.frame()
names(changefull) <- names(changealt) <- c("Positions", "Female", "Minority",
                                           "Top University", "Narrow")
changefull <- changefull |> 
  pivot_longer(!Positions, names_to = "Type", values_to = "Percentage")
changealt <- changealt |> 
  pivot_longer(!Positions, names_to = "Type", values_to = "Percentage")
changefull$Percentage <- as.numeric(changefull$Percentage)
changealt$Percentage <- as.numeric(changealt$Percentage)
changefull$Positions <- factor(changefull$Positions,
                               levels = c("Province", "State Council", "Party",
                                          'PLA', 'SOE', 'NPC', 'CPPCC', 'University',
                                          'Mass Organization'))
changealt$Positions <- factor(changealt$Positions,
                               levels = c("Province", "State Council", "Party",
                                          'PLA', 'SOE', 'NPC', 'CPPCC', 'University',
                                          'Mass Organization'))
#===============================================================================

# Analyzing Career Width
#===============================================================================
#====FCC 20====
narrow_list <- which(fcc20_career$narrow == 1)
fcc20_narrow <- data.frame(cname = fcc20$cname[narrow_list],
                           onepro = NA, onesc = NA, oneparty = NA, onePLA = NA,
                           SOE = NA, finance = NA, tech = NA,
                           scientist = NA, univ = NA, mo = NA,
                           NPC = NA, CPPCC = NA)
for (i in 1:length(narrow_list)){
  joblist <- toString(fcc20_path$career[[narrow_list[i]]])
  if (str_count(joblist, "pro") >= 3) {
    fcc20_narrow$onepro[i] = 1
  } else if (str_count(joblist, "NPC") > 2){
    fcc20_narrow$NPC[i] = 1
  } else if (str_count(joblist, "CPPCC") > 2) {
    fcc20_narrow$CPPCC[i] = 1
  } else if (grepl("pla", joblist)) {
    fcc20_narrow$onePLA[i] = 1
  } else if (grepl("party", joblist)) {
    fcc20_narrow$oneparty[i] = 1
  } else if (grepl("DSTC", joblist)) {
    fcc20_narrow$tech[i] = 1
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    fcc20_narrow$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    fcc20_narrow$scientist[i] = 1
  } else if (grepl("sc", joblist)) {
    fcc20_narrow$onesc[i] = 1
  } else if (grepl("SOE", joblist)) {
    fcc20_narrow$SOE[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(fcc20$phdmajor[narrow_list[i]])) {
      if (fcc20$phdmajor[narrow_list[i]] == 4) {
      fcc20_narrow$scientist[i] = 1
      } else {fcc20_narrow$univ[i] = 1}
    } else if ((!is.na(fcc20$mamajor[narrow_list[i]]) & fcc20$mamajor[narrow_list[i]] == 4)){
      fcc20_narrow$scientist[i] = 1
    } else {fcc20_narrow$univ[i] = 1}
  } else if (grepl("mo ", joblist)) {
    fcc20_narrow$mo[i] = 1
  } else if (grepl("pro", joblist)){
    fcc20_narrow$onepro[i] = 1
  }
}

#====ACC 20====
narrow_list <- which(acc20_career$narrow == 1)
acc20_narrow <- data.frame(cname = acc20$cname[narrow_list],
                           onepro = NA, onesc = NA, oneparty = NA, onePLA = NA,
                           SOE = NA, finance = NA, tech = NA,
                           scientist = NA, univ = NA, mo = NA,
                           NPC = NA, CPPCC = NA)
for (i in 1:length(narrow_list)){
  joblist <- toString(acc20_path$career[[narrow_list[i]]])
  if (str_count(joblist, "pro") >= 3) {
    acc20_narrow$onepro[i] = 1
  } else if (str_count(joblist, "NPC") > 2){
    acc20_narrow$NPC[i] = 1
  } else if (str_count(joblist, "CPPCC") > 2) {
    acc20_narrow$CPPCC[i] = 1
  } else if (grepl("pla", joblist)) {
    acc20_narrow$onePLA[i] = 1
  } else if (grepl("party", joblist)) {
    acc20_narrow$oneparty[i] = 1
  } else if (grepl("DSTC", joblist)) {
    acc20_narrow$tech[i] = 1
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    acc20_narrow$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    acc20_narrow$scientist[i] = 1
  } else if (grepl("sc", joblist)) {
    acc20_narrow$onesc[i] = 1
  } else if (grepl("SOE", joblist)) {
    acc20_narrow$SOE[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(acc20$phdmajor[narrow_list[i]])) {
      if (acc20$phdmajor[narrow_list[i]] == 4) {
        acc20_narrow$scientist[i] = 1
      } else {acc20_narrow$univ[i] = 1}
    } else if ((!is.na(acc20$mamajor[narrow_list[i]]) & acc20$mamajor[narrow_list[i]] == 4)){
      acc20_narrow$scientist[i] = 1
    } else {acc20_narrow$univ[i] = 1}
  } else if (grepl("mo ", joblist)) {
    acc20_narrow$mo[i] = 1
  } else if (grepl("pro", joblist)){
    acc20_narrow$onepro[i] = 1
  }
}

#====FCC 19====
narrow_list <- which(fcc19_career$narrow == 1)
fcc19_narrow <- data.frame(cname = fcc19$cname[narrow_list],
                           onepro = NA, onesc = NA, oneparty = NA, onePLA = NA,
                           SOE = NA, finance = NA, tech = NA,
                           scientist = NA, univ = NA, mo = NA,
                           NPC = NA, CPPCC = NA)
for (i in 1:length(narrow_list)){
  joblist <- toString(fcc19_path$career[[narrow_list[i]]])
  if (str_count(joblist, "pro") >= 3) {
    fcc19_narrow$onepro[i] = 1
  } else if (str_count(joblist, "NPC") > 2){
    fcc19_narrow$NPC[i] = 1
  } else if (str_count(joblist, "CPPCC") > 2) {
    fcc19_narrow$CPPCC[i] = 1
  } else if (grepl("pla", joblist)) {
    fcc19_narrow$onePLA[i] = 1
  } else if (grepl("party", joblist)) {
    fcc19_narrow$oneparty[i] = 1
  } else if (grepl("DSTC", joblist)) {
    fcc19_narrow$tech[i] = 1
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    fcc19_narrow$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    fcc19_narrow$scientist[i] = 1
  } else if (grepl("sc", joblist)) {
    fcc19_narrow$onesc[i] = 1
  } else if (grepl("SOE", joblist)) {
    fcc19_narrow$SOE[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(fcc19$phdmajor[narrow_list[i]])) {
      if (fcc19$phdmajor[narrow_list[i]] == 4) {
        fcc19_narrow$scientist[i] = 1
      } else {fcc19_narrow$univ[i] = 1}
    } else if ((!is.na(fcc19$mamajor[narrow_list[i]]) & fcc19$mamajor[narrow_list[i]] == 4)){
      fcc19_narrow$scientist[i] = 1
    } else {fcc19_narrow$univ[i] = 1}
  } else if (grepl("mo ", joblist)) {
    fcc19_narrow$mo[i] = 1
  } else if (grepl("pro", joblist)){
    fcc19_narrow$onepro[i] = 1
  }
}

#====ACC 19====
narrow_list <- which(acc19_career$narrow == 1)
acc19_narrow <- data.frame(cname = acc19$cname[narrow_list],
                           onepro = NA, onesc = NA, oneparty = NA, onePLA = NA,
                           SOE = NA, finance = NA, tech = NA,
                           scientist = NA, univ = NA, mo = NA,
                           NPC = NA, CPPCC = NA)
for (i in 1:length(narrow_list)){
  joblist <- toString(acc19_path$career[[narrow_list[i]]])
  if (str_count(joblist, "pro") >= 3) {
    acc19_narrow$onepro[i] = 1
  } else if (str_count(joblist, "NPC") > 2){
    acc19_narrow$NPC[i] = 1
  } else if (str_count(joblist, "CPPCC") > 2) {
    acc19_narrow$CPPCC[i] = 1
  } else if (grepl("pla", joblist)) {
    acc19_narrow$onePLA[i] = 1
  } else if (grepl("party", joblist)) {
    acc19_narrow$oneparty[i] = 1
  } else if (grepl("DSTC", joblist)) {
    acc19_narrow$tech[i] = 1
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    acc19_narrow$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    acc19_narrow$scientist[i] = 1
  } else if (grepl("sc", joblist)) {
    acc19_narrow$onesc[i] = 1
  } else if (grepl("SOE", joblist)) {
    acc19_narrow$SOE[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(acc19$phdmajor[narrow_list[i]])) {
      if (acc19$phdmajor[narrow_list[i]] == 4) {
        acc19_narrow$scientist[i] = 1
      } else {acc19_narrow$univ[i] = 1}
    } else if ((!is.na(acc19$mamajor[narrow_list[i]]) & acc19$mamajor[narrow_list[i]] == 4)){
      acc19_narrow$scientist[i] = 1
    } else {acc19_narrow$univ[i] = 1}
  } else if (grepl("mo ", joblist)) {
    acc19_narrow$mo[i] = 1
  } else if (grepl("pro", joblist)){
    acc19_narrow$onepro[i] = 1
  }
}
#====FCC 18====
narrow_list <- which(fcc18_career$narrow == 1)
fcc18_narrow <- data.frame(cname = fcc18$cname[narrow_list],
                           onepro = NA, onesc = NA, oneparty = NA, onePLA = NA,
                           SOE = NA, finance = NA, tech = NA,
                           scientist = NA, univ = NA, mo = NA,
                           NPC = NA, CPPCC = NA)
for (i in 1:length(narrow_list)){
  joblist <- toString(fcc18_path$career[[narrow_list[i]]])
  if (str_count(joblist, "pro") >= 3) {
    fcc18_narrow$onepro[i] = 1
  } else if (str_count(joblist, "NPC") > 2){
    fcc18_narrow$NPC[i] = 1
  } else if (str_count(joblist, "CPPCC") > 2) {
    fcc18_narrow$CPPCC[i] = 1
  } else if (grepl("pla", joblist)) {
    fcc18_narrow$onePLA[i] = 1
  } else if (grepl("party", joblist)) {
    fcc18_narrow$oneparty[i] = 1
  } else if (grepl("DSTC", joblist)) {
    fcc18_narrow$tech[i] = 1
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    fcc18_narrow$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    fcc18_narrow$scientist[i] = 1
  } else if (grepl("sc", joblist)) {
    fcc18_narrow$onesc[i] = 1
  } else if (grepl("SOE", joblist)) {
    fcc18_narrow$SOE[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(fcc18$phdmajor[narrow_list[i]])) {
      if (fcc18$phdmajor[narrow_list[i]] == 4) {
        fcc18_narrow$scientist[i] = 1
      } else {fcc18_narrow$univ[i] = 1}
    } else if ((!is.na(fcc18$mamajor[narrow_list[i]]) & fcc18$mamajor[narrow_list[i]] == 4)){
      fcc18_narrow$scientist[i] = 1
    } else {fcc18_narrow$univ[i] = 1}
  } else if (grepl("mo ", joblist)) {
    fcc18_narrow$mo[i] = 1
  }  else if (grepl("pro", joblist)){
    fcc18_narrow$onepro[i] = 1
  }
}

#====ACC 18====
narrow_list <- which(acc18_career$narrow == 1)
acc18_narrow <- data.frame(cname = acc18$cname[narrow_list],
                           onepro = NA, onesc = NA, oneparty = NA, onePLA = NA,
                           SOE = NA, finance = NA, tech = NA,
                           scientist = NA, univ = NA, mo = NA,
                           NPC = NA, CPPCC = NA)
for (i in 1:length(narrow_list)){
  joblist <- toString(acc18_path$career[[narrow_list[i]]])
  if (str_count(joblist, "pro") >= 3) {
    acc18_narrow$onepro[i] = 1
  } else if (str_count(joblist, "NPC") > 2){
    acc18_narrow$NPC[i] = 1
  } else if (str_count(joblist, "CPPCC") > 2) {
    acc18_narrow$CPPCC[i] = 1
  } else if (grepl("pla", joblist)) {
    acc18_narrow$onePLA[i] = 1
  } else if (grepl("party", joblist)) {
    acc18_narrow$oneparty[i] = 1
  } else if (grepl("DSTC", joblist)) {
    acc18_narrow$tech[i] = 1
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    acc18_narrow$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    acc18_narrow$scientist[i] = 1
  } else if (grepl("sc", joblist)) {
    acc18_narrow$onesc[i] = 1
  } else if (grepl("SOE", joblist)) {
    acc18_narrow$SOE[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(acc18$phdmajor[narrow_list[i]])) {
      if (acc18$phdmajor[narrow_list[i]] == 4) {
        acc18_narrow$scientist[i] = 1
      } else {acc18_narrow$univ[i] = 1}
    } else if ((!is.na(acc18$mamajor[narrow_list[i]]) & acc18$mamajor[narrow_list[i]] == 4)){
      acc18_narrow$scientist[i] = 1
    } else {acc18_narrow$univ[i] = 1}
  } else if (grepl("mo ", joblist)) {
    acc18_narrow$mo[i] = 1
  } else if (grepl("pro", joblist)){
    acc18_narrow$onepro[i] = 1
  }
}
#==============
fcc18_narrow$check <- rowSums(fcc18_narrow[-1], na.rm = T)
fcc19_narrow$check <- rowSums(fcc19_narrow[-1], na.rm = T)
fcc20_narrow$check <- rowSums(fcc20_narrow[-1], na.rm = T)
acc18_narrow$check <- rowSums(acc18_narrow[-1], na.rm = T)
acc19_narrow$check <- rowSums(acc19_narrow[-1], na.rm = T)
acc20_narrow$check <- rowSums(acc20_narrow[-1], na.rm = T)
#===============================================================================

# Analyzing Expertise
#===============================================================================
#====FCC20====
fcc20_expert <- data.frame(cname = fcc20$cname,
                           finance = NA, tech = NA, scientist = NA)
for (i in 1:nrow(fcc20_expert)){
  joblist <- toString(fcc20_path$career[[i]])
  if (grepl("DSTC", joblist)) {
    fcc20_expert$tech[i] = 1
  } else if (grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    fcc20_expert$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    fcc20_expert$scientist[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(fcc20$phdmajor[i])) {
      if (fcc20$phdmajor[i] == 4) {
        fcc20_expert$scientist[i] = 1
      }
    } else if ((!is.na(fcc20$mamajor[i]) & fcc20$mamajor[i] == 4)){
      fcc20_expert$scientist[i] = 1
    } 
  }
}

#====ACC20====
acc20_expert <- data.frame(cname = acc20$cname,
                           finance = NA, tech = NA, scientist = NA)
for (i in 1:nrow(acc20_expert)){
  joblist <- toString(acc20_path$career[[i]])
  if (grepl("DSTC", joblist)) {
    acc20_expert$tech[i] = 1
  } else if (grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    acc20_expert$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    acc20_expert$scientist[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(acc20$phdmajor[i])) {
      if (acc20$phdmajor[i] == 4) {
        acc20_expert$scientist[i] = 1
      }
    } else if ((!is.na(acc20$mamajor[i]) & acc20$mamajor[i] == 4)){
      acc20_expert$scientist[i] = 1
    } 
  }
}

#====FCC19====
fcc19_expert <- data.frame(cname = fcc19$cname,
                           finance = NA, tech = NA, scientist = NA)
for (i in 1:nrow(fcc19_expert)){
  joblist <- toString(fcc19_path$career[[i]])
  if (grepl("DSTC", joblist)) {
    fcc19_expert$tech[i] = 1
  } else if (grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    fcc19_expert$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    fcc19_expert$scientist[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(fcc19$phdmajor[i])) {
      if (fcc19$phdmajor[i] == 4) {
        fcc19_expert$scientist[i] = 1
      }
    } else if ((!is.na(fcc19$mamajor[i]) & fcc19$mamajor[i] == 4)){
      fcc19_expert$scientist[i] = 1
    } 
  }
}
#====ACC19====
acc19_expert <- data.frame(cname = acc19$cname,
                           finance = NA, tech = NA, scientist = NA)
for (i in 1:nrow(acc19_expert)){
  joblist <- toString(acc19_path$career[[i]])
  if (grepl("DSTC", joblist)) {
    acc19_expert$tech[i] = 1
  } else if (grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    acc19_expert$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    acc19_expert$scientist[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(acc19$phdmajor[i])) {
      if (acc19$phdmajor[i] == 4) {
        acc19_expert$scientist[i] = 1
      }
    } else if ((!is.na(acc19$mamajor[i]) & acc19$mamajor[i] == 4)){
      acc19_expert$scientist[i] = 1
    } 
  }
}

#====FCC18====
fcc18_expert <- data.frame(cname = fcc18$cname,
                           finance = NA, tech = NA, scientist = NA)
for (i in 1:nrow(fcc18_expert)){
  joblist <- toString(fcc18_path$career[[i]])
  if (grepl("DSTC", joblist)) {
    fcc18_expert$tech[i] = 1
  } else if (grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    fcc18_expert$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    fcc18_expert$scientist[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(fcc18$phdmajor[i])) {
      if (fcc18$phdmajor[i] == 4) {
        fcc18_expert$scientist[i] = 1
      }
    } else if ((!is.na(fcc18$mamajor[i]) & fcc18$mamajor[i] == 4)){
      fcc18_expert$scientist[i] = 1
    } 
  }
}

#====ACC18====
acc18_expert <- data.frame(cname = acc18$cname,
                           finance = NA, tech = NA, scientist = NA)
for (i in 1:nrow(acc18_expert)){
  joblist <- toString(acc18_path$career[[i]])
  if (grepl("DSTC", joblist)) {
    acc18_expert$tech[i] = 1
  } else if (grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    acc18_expert$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    acc18_expert$scientist[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(acc18$phdmajor[i])) {
      if (acc18$phdmajor[i] == 4) {
        acc18_expert$scientist[i] = 1
      }
    } else if ((!is.na(acc18$mamajor[i]) & acc18$mamajor[i] == 4)){
      acc18_expert$scientist[i] = 1
    } 
  }
}

#====politburo18====
politburo18_expert <- data.frame(cname = politburo18$cname,
                           finance = NA, tech = NA, scientist = NA)
for (i in 1:nrow(politburo18_expert)){
  joblist <- toString(politburo18_path$career[[i]])
  if (grepl("DSTC", joblist)) {
    politburo18_expert$tech[i] = 1
  } else if (grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    politburo18_expert$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    politburo18_expert$scientist[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(politburo18$phdmajor[i])) {
      if (politburo18$phdmajor[i] == 4) {
        politburo18_expert$scientist[i] = 1
      }
    } else if ((!is.na(politburo18$mamajor[i]) & politburo18$mamajor[i] == 4)){
      politburo18_expert$scientist[i] = 1
    } 
  }
}

#====politburo19====
politburo19_expert <- data.frame(cname = politburo19$cname,
                           finance = NA, tech = NA, scientist = NA)
for (i in 1:nrow(politburo19_expert)){
  joblist <- toString(politburo19_path$career[[i]])
  if (grepl("DSTC", joblist)) {
    politburo19_expert$tech[i] = 1
  } else if (grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    politburo19_expert$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    politburo19_expert$scientist[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(politburo19$phdmajor[i])) {
      if (politburo19$phdmajor[i] == 4) {
        politburo19_expert$scientist[i] = 1
      }
    } else if ((!is.na(politburo19$mamajor[i]) & politburo19$mamajor[i] == 4)){
      politburo19_expert$scientist[i] = 1
    } 
  }
}

#====politburo20====
politburo20_expert <- data.frame(cname = politburo20$cname,
                           finance = NA, tech = NA, scientist = NA)
for (i in 1:nrow(politburo20_expert)){
  joblist <- toString(politburo20_path$career[[i]])
  if (grepl("DSTC", joblist)) {
    politburo20_expert$tech[i] = 1
  } else if (grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("Finance", joblist) |
             grepl("Union", joblist)) {
    politburo20_expert$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    politburo20_expert$scientist[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(politburo20$phdmajor[i])) {
      if (politburo20$phdmajor[i] == 4) {
        politburo20_expert$scientist[i] = 1
      }
    } else if ((!is.na(politburo20$mamajor[i]) & politburo20$mamajor[i] == 4)){
      politburo20_expert$scientist[i] = 1
    } 
  }
}
#===============================================================================

#Visualization
#===============================================================================

fcc_stats <- stats[c(1, 3, 5),]
acc_stats <- stats[c(2, 4, 6),]

#====Narrow Careers and Career Width====
ggplot() +
  geom_line(aes(x = session, y = width, group = 1, linetype = "Full"), data = fcc_stats) + 
  geom_point(aes(x = session, y = width, linetype = "Full"), data = fcc_stats) +
  geom_line(aes(x = session, y = width, group = 1, linetype = "Alternate"), data = acc_stats) +
  geom_point(aes(x = session, y = width, linetype = "Alternate"), data = acc_stats) +
  scale_linetype_manual(values = c("Full" = 1, "Alternate" = 2)) +
  ylim(0, 5) +
  labs(x = "Party Congress Session", y = "Average Career Width", linetype = "Member", 
       title = "Career Width of the CCP Central Committee (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/Career_Width_CPPCC.png")

ggplot() +
  geom_line(aes(x = session, y = narrow, group = 1, linetype = "Full"), data = fcc_stats) + 
  geom_point(aes(x = session, y = narrow, linetype = "Full"), data = fcc_stats) +
  geom_line(aes(x = session, y = narrow, group = 1, linetype = "Alternate"), data = acc_stats) +
  geom_point(aes(x = session, y = narrow, linetype = "Alternate"), data = acc_stats) +
  scale_linetype_manual(values = c("Full" = 1, "Alternate" = 2)) +
  labs(x = "Party Congress Session", y = "Percentage of Members with Narrow Career Paths", linetype = "Member", 
       title = "Narrow Career Paths among CCP Central Committee Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_Narrow_Career_(1).png")

ggplot() +
  geom_line(aes(x = session, y = narrow_alt, group = 1, linetype = "Full"), data = fcc_stats) + 
  geom_point(aes(x = session, y = narrow_alt, linetype = "Full"), data = fcc_stats) +
  geom_line(aes(x = session, y = narrow_alt, group = 1, linetype = "Alternate"), data = acc_stats) +
  geom_point(aes(x = session, y = narrow_alt, linetype = "Alternate"), data = acc_stats) +
  scale_linetype_manual(values = c("Full" = 1, "Alternate" = 2)) +
  labs(x = "Party Congress Session", y = "Percentage of Members with Narrow Career Paths", linetype = "Member", 
       title = "Narrow Career Paths among CCP Central Committee Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_Narrow_Career_(2).png")

#====CCPCC Positions====
fcc_stats_t <- fcc_stats[, c("pla", "sc", "province", "SOE", "party")] |> 
  t() |> data.frame()
names(fcc_stats_t) = c("18", "19", "20")
fcc_stats_t$type = c("PLA", "State Council", "Provincial", "SOE", "Party")
fcc_stats_t <- fcc_stats_t |> 
  gather(session, percentage, "18":"20")

acc_stats_t <- acc_stats[, c("pla", "sc", "province", "SOE", "party")] |> 
  t() |> data.frame()
names(acc_stats_t) = c("18", "19", "20")
acc_stats_t$type = c("PLA", "State Council", "Provincial", "SOE", "Party")
acc_stats_t <- acc_stats_t |> 
  gather(session, percentage, "18":"20")

ggplot(fcc_stats_t)+
  geom_bar(aes(x = type, y = percentage, fill = session), stat = "identity", position = "dodge") +
  labs(x = "Position", y = "Percentage of Members", fill = "Session",
       title = "Position Held by CCP Central Committee Full Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_Position_Full.png")

ggplot(acc_stats_t)+
  geom_bar(aes(x = type, y = percentage, fill = session), stat = "identity", position = "dodge") +
  labs(x = "Position", y = "Percentage of Members", fill = "Session",
       title = "Position Held by CCP Central Committee Alternate Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_Position_Alternate.png")

#====Categorizing Narrow Career Paths====
acc_narrow <- colSums(acc18_narrow[, 2:(ncol(acc18_narrow) - 1)], na.rm = T) |> 
  data.frame() |> 
  cbind(colSums(acc19_narrow[, 2:(ncol(acc19_narrow) - 1)], na.rm = T)) |> 
  cbind(colSums(acc20_narrow[, 2:(ncol(acc20_narrow) - 1)], na.rm = T))
names(acc_narrow) <- c("18", "19", "20")
acc_narrow$Position <- c("One Province", "One Ministry", "One Party Organ",
                         "One PLA Region", "Mostly Other SOE", "Finance Expert",
                         "Technology Expert", "Scientist", "Other Academic",
                         "Mostly Mass Organization", "NPC", "CPPCC")
acc_narrow <- acc_narrow |> 
  filter(Position %!in% c("One Party Organ", "NPC", "CPPCC", "Mostly Mass Organization")) |> 
  gather(session, count, "18":"20")

fcc_narrow <- colSums(fcc18_narrow[, 2:(ncol(fcc18_narrow) - 1)], na.rm = T) |> 
  data.frame() |> 
  cbind(colSums(fcc19_narrow[, 2:(ncol(fcc19_narrow) - 1)], na.rm = T)) |> 
  cbind(colSums(fcc20_narrow[, 2:(ncol(fcc20_narrow) - 1)], na.rm = T))
names(fcc_narrow) <- c("18", "19", "20")
fcc_narrow$Position <- c("One Province", "One Ministry", "One Party Organ",
                         "One PLA Region", "Mostly Other SOE", "Finance Expert",
                         "Technology Expert", "Scientist", "Other Academic",
                         "Mostly Mass Organization", "NPC", "CPPCC")
fcc_narrow <- fcc_narrow |> 
  filter(Position %!in% c("One Party Organ", "NPC", "CPPCC", "Mostly Mass Organization")) |> 
  gather(session, count, "18":"20")

ggplot(fcc_narrow) +
  geom_bar(aes(x = Position, y = count, fill = session, col = session), stat = "identity", position = "dodge") +
  labs(x = "Narrow Path", y = "Count", fill = "Session", col = "Session",
       title = "Narrow Career Paths of CCP Central Committee Full Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/Narrow_Paths_CCPCC_Full.png")

ggplot(acc_narrow) +
  geom_bar(aes(x = Position, y = count, fill = session, col = session), stat = "identity", position = "dodge") +
  labs(x = "Narrow Path", y = "Count", fill = "Session", col = "Session",
       title = "Narrow Career Paths of CCP Central Committee Alternate Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/Narrow_Paths_CCPCC_Alternate.png")

fcc_narrow$member <- "Full"
acc_narrow$member <- "Alternate"
narrowmember <- rbind(fcc_narrow, acc_narrow)
narrowmember$member <- factor(narrowmember$member,
                              level = c("Full", "Alternate"))

ggplot(narrowmember) +
  geom_bar_pattern(aes(x = Position, y = count, pattern = member, fill = session, col = session),
                   stat = "identity", position = "dodge",
                   pattern_color = NA,
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.25,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 1) +
  scale_pattern_manual(values = c(Full = "none", Alternate = "stripe")) +
  labs(x = "Narrow Career Path", y = "Count", fill = "Session",
       col = "Session", pattern = "Members",
       title = "Narrow Career Paths of CCP Central Committee Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom") +
  guides(pattern = guide_legend(override.aes = list(fill = "white", col = "black")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#====XJP Networks====
fcc_XJP <- stats[c(1, 3, 5), c("session", "XJP")]
acc_XJP <- stats[c(2, 4, 6), c("session", "XJP")]

ggplot() +
  geom_line(aes(x = session, y = XJP, group = 1, lty = "Full Member"), data = fcc_XJP) +
  geom_line(aes(x = session, y = XJP, group = 1, lty = "Alternate Member"), data = acc_XJP) +
  scale_linetype_manual(values = c("Full Member" = 1,
                                   "Alternate Member" = 2)) +
  labs(x = "Session", y = "Percentage of Members",
       title = "Share of CCPCC Members with Ties with Xi Jinping (2012-2022)",
       lty = "Member") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_XJP_Ties_Total.png")

fcc_XJPyear <- fcc_stats[, c("XJP_1", "XJP_2")] |> 
  t() |> data.frame()
names(fcc_XJPyear) <- c("18", "19", "20")
fcc_XJPyear$exp <- factor(c("Less than 5 Years", "More than 5 Years"),
                        levels = c("Less than 5 Years", "More than 5 Years"))
fcc_XJPyear <- fcc_XJPyear |>
  gather(session, percentage, "18":"20")
fcc_XJPyear$member <- "Full"

acc_XJPyear <- acc_stats[, c("XJP_1", "XJP_2")] |> 
  t() |> data.frame()
names(acc_XJPyear) <- c("18", "19", "20")
acc_XJPyear$exp <- factor(c("Less than 5 Years", "More than 5 Years"),
                        levels = c("Less than 5 Years", "More than 5 Years"))
acc_XJPyear <- acc_XJPyear |> 
  gather(session, percentage, "18":"20")
acc_XJPyear$member <- "Alternate"
XJPyear <- rbind(fcc_XJPyear, acc_XJPyear)
XJPyear$member <- factor(XJPyear$member,
                         levels = c("Full", "Alternate"))

ggplot(XJPyear)+
  geom_bar_pattern(aes(x = exp, y = percentage, pattern = member, fill = session),
                   stat = "identity",
                   position = "dodge",
                   pattern_color = NA,
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.25,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 1) +
  scale_pattern_manual(values = c(Full = "none", Alternate = "stripe")) +
  labs(x = "Position", y = "Percentage of Members", fill = "Session",
       pattern = "Members",
       title = "CCPCC Members Working Experience with Xi Jinping (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom") +
  guides(pattern = guide_legend(override.aes = list(fill = "white", col = "black")),
         fill = guide_legend(override.aes = list(pattern = "none")))
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_XJP_Exp.png")

#====LKQ Networks====
fcc_LKQ <- stats[c(1, 3, 5), c("session", "LKQ")]
acc_LKQ <- stats[c(2, 4, 6), c("session", "LKQ")]

ggplot() +
  geom_line(aes(x = session, y = LKQ, group = 1, lty = "Full Member"), data = fcc_LKQ) +
  geom_line(aes(x = session, y = LKQ, group = 1, lty = "Alternate Member"), data = acc_LKQ) +
  scale_linetype_manual(values = c("Full Member" = 1,
                                   "Alternate Member" = 2)) +
  labs(x = "Session", y = "Percentage of Members",
       title = "Share of CCPCC Members with Ties with Li Keqiang (2012-2022)",
       lty = "Member") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_LKQ_Ties_Total.png")

fcc_LKQyear <- fcc_stats[, c("LKQ_1", "LKQ_2")] |> 
  t() |> data.frame()
names(fcc_LKQyear) <- c("18", "19", "20")
fcc_LKQyear$exp <- factor(c("Less than 5 Years", "More than 5 Years"),
                          levels = c("Less than 5 Years", "More than 5 Years"))
fcc_LKQyear <- fcc_LKQyear |>
  gather(session, percentage, "18":"20")
fcc_LKQyear$member <- "Full"

acc_LKQyear <- acc_stats[, c("LKQ_1", "LKQ_2")] |> 
  t() |> data.frame()
names(acc_LKQyear) <- c("18", "19", "20")
acc_LKQyear$exp <- factor(c("Less than 5 Years", "More than 5 Years"),
                          levels = c("Less than 5 Years", "More than 5 Years"))
acc_LKQyear <- acc_LKQyear |> 
  gather(session, percentage, "18":"20")
acc_LKQyear$member <- "Alternate"
LKQyear <- rbind(fcc_LKQyear, acc_LKQyear)
LKQyear$member <- factor(LKQyear$member,
                         levels = c("Full", "Alternate"))

ggplot(LKQyear)+
  geom_bar_pattern(aes(x = exp, y = percentage, pattern = member, fill = session),
                   stat = "identity",
                   position = "dodge",
                   pattern_color = NA,
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.25,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 1) +
  scale_pattern_manual(values = c(Full = "none", Alternate = "stripe")) +
  labs(x = "Position", y = "Percentage of Members", fill = "Session",
       pattern = "Members",
       title = "CCPCC Members Working Experience with Li Keqiang (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom") +
  guides(pattern = guide_legend(override.aes = list(fill = "white", col = "black")),
         fill = guide_legend(override.aes = list(pattern = "none")))
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_LKQ_Exp.png")

#====WQS Networks====
fcc_WQS <- stats[c(1, 3, 5), c("session", "WQS")]
acc_WQS <- stats[c(2, 4, 6), c("session", "WQS")]

ggplot() +
  geom_line(aes(x = session, y = WQS, group = 1, lty = "Full Member"), data = fcc_WQS) +
  geom_line(aes(x = session, y = WQS, group = 1, lty = "Alternate Member"), data = acc_WQS) +
  scale_linetype_manual(values = c("Full Member" = 1,
                                   "Alternate Member" = 2)) +
  labs(x = "Session", y = "Percentage of Members",
       title = "Share of CCPCC Members with Ties with Wang Qishan (2012-2022)",
       lty = "Member") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_WQS_Ties_Total.png")

fcc_WQSyear <- fcc_stats[, c("WQS_1", "WQS_2")] |> 
  t() |> data.frame()
names(fcc_WQSyear) <- c("18", "19", "20")
fcc_WQSyear$exp <- factor(c("Less than 5 Years", "More than 5 Years"),
                          levels = c("Less than 5 Years", "More than 5 Years"))
fcc_WQSyear <- fcc_WQSyear |>
  gather(session, percentage, "18":"20")
fcc_WQSyear$member <- "Full"

acc_WQSyear <- acc_stats[, c("WQS_1", "WQS_2")] |> 
  t() |> data.frame()
names(acc_WQSyear) <- c("18", "19", "20")
acc_WQSyear$exp <- factor(c("Less than 5 Years", "More than 5 Years"),
                          levels = c("Less than 5 Years", "More than 5 Years"))
acc_WQSyear <- acc_WQSyear |> 
  gather(session, percentage, "18":"20")
acc_WQSyear$member <- "Alternate"
WQSyear <- rbind(fcc_WQSyear, acc_WQSyear)
WQSyear$member <- factor(WQSyear$member,
                         levels = c("Full", "Alternate"))

ggplot(WQSyear)+
  geom_bar_pattern(aes(x = exp, y = percentage, pattern = member, fill = session),
                   stat = "identity",
                   position = "dodge",
                   pattern_color = NA,
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.25,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 1) +
  scale_pattern_manual(values = c(Full = "none", Alternate = "stripe")) +
  labs(x = "Position", y = "Percentage of Members", fill = "Session",
       pattern = "Members",
       title = "CCPCC Members Working Experience with Wang Qishan (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom") +
  guides(pattern = guide_legend(override.aes = list(fill = "white", col = "black")),
         fill = guide_legend(override.aes = list(pattern = "none")))
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_WQS_Exp.png")

#====LQ Networks====
fcc_LQ <- stats[c(1, 3, 5), c("session", "LQ")]
acc_LQ <- stats[c(2, 4, 6), c("session", "LQ")]

ggplot() +
  geom_line(aes(x = session, y = LQ, group = 1, lty = "Full Member"), data = fcc_LQ) +
  geom_line(aes(x = session, y = LQ, group = 1, lty = "Alternate Member"), data = acc_LQ) +
  scale_linetype_manual(values = c("Full Member" = 1,
                                   "Alternate Member" = 2)) +
  labs(x = "Session", y = "Percentage of Members",
       title = "Share of CCPCC Members with Ties with Li Qiang (2012-2022)",
       lty = "Member") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_LQ_Ties_Total.png")

fcc_LQyear <- fcc_stats[, c("LQ_1", "LQ_2")] |> 
  t() |> data.frame()
names(fcc_LQyear) <- c("18", "19", "20")
fcc_LQyear$exp <- factor(c("Less than 5 Years", "More than 5 Years"),
                          levels = c("Less than 5 Years", "More than 5 Years"))
fcc_LQyear <- fcc_LQyear |>
  gather(session, percentage, "18":"20")
fcc_LQyear$member <- "Full"

acc_LQyear <- acc_stats[, c("LQ_1", "LQ_2")] |> 
  t() |> data.frame()
names(acc_LQyear) <- c("18", "19", "20")
acc_LQyear$exp <- factor(c("Less than 5 Years", "More than 5 Years"),
                          levels = c("Less than 5 Years", "More than 5 Years"))
acc_LQyear <- acc_LQyear |> 
  gather(session, percentage, "18":"20")
acc_LQyear$member <- "Alternate"
LQyear <- rbind(fcc_LQyear, acc_LQyear)
LQyear$member <- factor(LQyear$member,
                         levels = c("Full", "Alternate"))

ggplot(LQyear)+
  geom_bar_pattern(aes(x = exp, y = percentage, pattern = member, fill = session),
                   stat = "identity",
                   position = "dodge",
                   pattern_color = NA,
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.25,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 1) +
  scale_pattern_manual(values = c(Full = "none", Alternate = "stripe")) +
  labs(x = "Position", y = "Percentage of Members", fill = "Session",
       pattern = "Members",
       title = "CCPCC Members Working Experience with Li Qiang (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom") +
  guides(pattern = guide_legend(override.aes = list(fill = "white", col = "black")),
         fill = guide_legend(override.aes = list(pattern = "none")))
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_LQ_Exp.png")

#====LX Networks====
fcc_LX <- stats[c(1, 3, 5), c("session", "LX")]
acc_LX <- stats[c(2, 4, 6), c("session", "LX")]

ggplot() +
  geom_line(aes(x = session, y = LX, group = 1, lty = "Full Member"), data = fcc_LX) +
  geom_line(aes(x = session, y = LX, group = 1, lty = "Alternate Member"), data = acc_LX) +
  scale_linetype_manual(values = c("Full Member" = 1,
                                   "Alternate Member" = 2)) +
  labs(x = "Session", y = "Percentage of Members",
       title = "Share of CCPCC Members with Ties with Li Xi (2012-2022)",
       lty = "Member") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_LX_Ties_Total.png")

fcc_LXyear <- fcc_stats[, c("LX_1", "LX_2")] |> 
  t() |> data.frame()
names(fcc_LXyear) <- c("18", "19", "20")
fcc_LXyear$exp <- factor(c("Less than 5 Years", "More than 5 Years"),
                         levels = c("Less than 5 Years", "More than 5 Years"))
fcc_LXyear <- fcc_LXyear |>
  gather(session, percentage, "18":"20")
fcc_LXyear$member <- "Full"

acc_LXyear <- acc_stats[, c("LX_1", "LX_2")] |> 
  t() |> data.frame()
names(acc_LXyear) <- c("18", "19", "20")
acc_LXyear$exp <- factor(c("Less than 5 Years", "More than 5 Years"),
                         levels = c("Less than 5 Years", "More than 5 Years"))
acc_LXyear <- acc_LXyear |> 
  gather(session, percentage, "18":"20")
acc_LXyear$member <- "Alternate"
LXyear <- rbind(fcc_LXyear, acc_LXyear)
LXyear$member <- factor(LXyear$member,
                        levels = c("Full", "Alternate"))

ggplot(LXyear)+
  geom_bar_pattern(aes(x = exp, y = percentage, pattern = member, fill = session),
                   stat = "identity",
                   position = "dodge",
                   pattern_color = NA,
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.25,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 1) +
  scale_pattern_manual(values = c(Full = "none", Alternate = "stripe")) +
  labs(x = "Position", y = "Percentage of Members", fill = "Session",
       pattern = "Members",
       title = "CCPCC Members Working Experience with Li Xi (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom") +
  guides(pattern = guide_legend(override.aes = list(fill = "white", col = "black")),
         fill = guide_legend(override.aes = list(pattern = "none")))
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_LX_Exp.png")

#====CQ Networks====
fcc_CQ <- stats[c(1, 3, 5), c("session", "CQ")]
acc_CQ <- stats[c(2, 4, 6), c("session", "CQ")]

ggplot() +
  geom_line(aes(x = session, y = CQ, group = 1, lty = "Full Member"), data = fcc_CQ) +
  geom_line(aes(x = session, y = CQ, group = 1, lty = "Alternate Member"), data = acc_CQ) +
  scale_linetype_manual(values = c("Full Member" = 1,
                                   "Alternate Member" = 2)) +
  labs(x = "Session", y = "Percentage of Members",
       title = "Share of CCPCC Members with Ties with Cai Qi (2012-2022)",
       lty = "Member") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_CQ_Ties_Total.png")

fcc_CQyear <- fcc_stats[, c("CQ_1", "CQ_2")] |> 
  t() |> data.frame()
names(fcc_CQyear) <- c("18", "19", "20")
fcc_CQyear$exp <- factor(c("Less than 5 Years", "More than 5 Years"),
                         levels = c("Less than 5 Years", "More than 5 Years"))
fcc_CQyear <- fcc_CQyear |>
  gather(session, percentage, "18":"20")
fcc_CQyear$member <- "Full"

acc_CQyear <- acc_stats[, c("CQ_1", "CQ_2")] |> 
  t() |> data.frame()
names(acc_CQyear) <- c("18", "19", "20")
acc_CQyear$exp <- factor(c("Less than 5 Years", "More than 5 Years"),
                         levels = c("Less than 5 Years", "More than 5 Years"))
acc_CQyear <- acc_CQyear |> 
  gather(session, percentage, "18":"20")
acc_CQyear$member <- "Alternate"
CQyear <- rbind(fcc_CQyear, acc_CQyear)
CQyear$member <- factor(CQyear$member,
                        levels = c("Full", "Alternate"))

ggplot(CQyear)+
  geom_bar_pattern(aes(x = exp, y = percentage, pattern = member, fill = session),
                   stat = "identity",
                   position = "dodge",
                   pattern_color = NA,
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.25,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 1) +
  scale_pattern_manual(values = c(Full = "none", Alternate = "stripe")) +
  labs(x = "Position", y = "Percentage of Members", fill = "Session",
       pattern = "Members",
       title = "CCPCC Members Working Experience with Cai Qi (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom") +
  guides(pattern = guide_legend(override.aes = list(fill = "white", col = "black")),
         fill = guide_legend(override.aes = list(pattern = "none")))
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_CQ_Exp.png")

#====Expertise====
fcc_expert <- colSums(fcc18_expert[, 2:ncol(fcc18_expert)], na.rm = T) |> 
  data.frame() |> 
  cbind(colSums(fcc19_expert[, 2:ncol(fcc19_expert)], na.rm = T)) |> 
  cbind(colSums(fcc20_expert[, 2:ncol(fcc20_expert)], na.rm = T))
names(fcc_expert) <- c("18", "19", "20")
fcc_expert$Position <- c("Finance", "Technology", "Natural Science")
fcc_expert <- fcc_expert |> 
  gather(session, count, "18":"20")

acc_expert <- colSums(acc18_expert[, 2:ncol(acc18_expert)], na.rm = T) |> 
  data.frame() |> 
  cbind(colSums(acc19_expert[, 2:ncol(acc19_expert)], na.rm = T)) |> 
  cbind(colSums(acc20_expert[, 2:ncol(acc20_expert)], na.rm = T))
names(acc_expert) <- c("18", "19", "20")
acc_expert$Position <- c("Finance", "Technology", "Natural Science")
acc_expert <- acc_expert |> 
  gather(session, count, "18":"20")

ggplot(fcc_expert) +
  geom_bar(aes(x = Position, y = count, fill = session), stat = "identity", position = "dodge") +
  labs(x = "Expertise", y = "Count", fill = "Session",
       title = "Career Expertise of CCPCC Full Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_Full_Member_Expertise.png")

ggplot(acc_expert) +
  geom_bar(aes(x = Position, y = count, fill = session), stat = "identity", position = "dodge") +
  labs(x = "Expertise", y = "Count", fill = "Session",
       title = "Career Expertise of CCPCC Alternate Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_Alternate_Member_Expertise.png")

politburo_expert <- colSums(politburo18_expert[, 2:ncol(politburo18_expert)], na.rm = T) |> 
  data.frame() |> 
  cbind(colSums(politburo19_expert[, 2:ncol(politburo19_expert)], na.rm = T)) |> 
  cbind(colSums(politburo20_expert[, 2:ncol(politburo20_expert)], na.rm = T))
names(politburo_expert) <- c("18", "19", "20")
politburo_expert$Position <- c("Finance", "Technology", "Natural Science")
politburo_expert <- politburo_expert |> 
  gather(session, count, "18":"20")

ggplot(politburo_expert) +
  geom_bar(aes(x = Position, y = count, fill = session), stat = "identity", position = "dodge") +
  labs(x = "Expertise", y = "Count", fill = "Session",
       title = "Career Expertise of Politburo Members (2012-2022)") +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/Politburo_Expertise.png")

total_expert <- politburo_expert |> 
  cbind(fcc_expert$count, acc_expert$count)
names(total_expert) <- c("Expertise", "Session", "Politburo", "FCC", "ACC")
total_expert <- total_expert |> 
  mutate(Total = politburo + FCC + ACC)

ggplot(total_expert) +
  geom_bar(aes(x = Expertise, y = Total, fill = Session), stat = "identity", position = "dodge") +
  labs(x = "Expertise", y = "Count", fill = "Session",
       title = "Career Expertise of All Central Committee Members (2012-2022)") +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/Total_Expertise.png")
#====Policing and Security====
securitycoc <- security |> 
  dplyr::select(session, size, policing, widepolicing) |> 
  group_by(session) |> 
  summarize(size = sum(size), 
            policing = sum(policing), 
            widepolicing = sum(widepolicing))
securitycoc$policing <- securitycoc$policing * 100 / securitycoc$size
securitycoc$widepolicing <- securitycoc$widepolicing * 100 / securitycoc$size
securitymember <- securitycoc[rep(seq_len(nrow(securitycoc)), each = 3), 1:2]
securitymember$member <- security$member
securitymember$policing <- security$policing * 100 / securitymember$size
securitymember$widepolicing <- security$widepolicing * 100 / securitymember$size
securityorg <- security |> 
  dplyr::select(session, MPS, MSS, CLPLG, PSD, SSD, PLPC) |> 
  group_by(session) |> 
  summarize("Public Security" = sum(MPS) + sum(PSD), 
            "State Security" = sum(MSS) + sum(SSD), 
            "Political and Legal Affairs" = sum(CLPLG) + sum(PLPC)) |> 
  gather("session")
securityorg$size <- rep(securitycoc$size, 3)
names(securityorg) <- c("org", "widepolicing", "size")
securityorg$session <- rep(c("18", "19", "20"), 3)
ggplot() +
  geom_bar(aes(x = session, y = policing, fill = member), data = securitymember,
           stat = "identity") +
  geom_point(aes(x = session, y = policing), data = securitycoc) +
  geom_line(aes(x = session, y = policing), group = 1, data = securitycoc) +
  labs(x = "Party Congress Session", y = "Percentage of Member with Policing Experience", fill = "Member", 
       title = "Policing Experience of the CCP Central Committee (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"))
ggplot() +
  geom_bar(aes(x = session, y = widepolicing, fill = member), data = securitymember,
           stat = "identity", alpha = 0.8) +
  geom_point(aes(x = session, y = widepolicing), data = securitycoc) +
  geom_line(aes(x = session, y = widepolicing), group = 1, data = securitycoc) +
  labs(x = "Party Congress Session", y = "Percentage of Member with Policing Experience", fill = "Member", 
       title = "Policing Experience of the CCP Central Committee (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggplot() +
  geom_bar(aes(x = session, y = widepolicing/2.5, fill = org), data = securityorg,
           stat = "identity", alpha = 0.8, position = 'dodge') +
  geom_point(aes(x = session, y = widepolicing), data = securitycoc) +
  geom_line(aes(x = session, y = widepolicing), group = 1, data = securitycoc) +
  labs(x = "Party Congress Session", y = "Percentage of Member with Policing Experience", fill = "Member", 
       title = "Policing Experience of the CCP Central Committee (2012-2022)") +
  theme_minimal()+
  scale_y_continuous("Percentage", sec.axis = sec_axis(~ . * 2.5, name = "Count")) +
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")


#====Heatmap====
ggplot(changefull, aes(y = Positions, x = Type, fill = Percentage)) +
  geom_tile() +
  geom_text(aes(label = paste0(round(Percentage, 2),"%")), color = "black", size = 3) +
  scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-3.5, 3.5), type = "div") +
  coord_fixed() +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"))

ggplot(changealt, aes(y = Positions, x = Type, fill = Percentage)) +
  geom_tile() +
  geom_text(aes(label = paste0(round(Percentage, 2),"%")), color = "black", size = 3) +
  scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-6, 6)) +
  coord_fixed() +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"))
#===============================================================================

#Export DataFrame
#===============================================================================
write_xlsx(stats, "/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/dataset/summary_stats.xlsx")


#Extra
#===============================================================================
ggplot(fcc_XJPyear)+
  geom_bar(aes(x = exp, y = percentage, fill = session), stat = "identity", position = "dodge") +
  labs(x = "Position", y = "Percentage of Members", fill = "Session",
       title = "Experience with Xi Jinping of CCPCC Full Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_Full_XJP_Exp.png")

ggplot(acc_XJPyear)+
  geom_bar(aes(x = exp, y = percentage, fill = session), stat = "identity", position = "dodge") +
  labs(x = "Position", y = "Percentage of Members", fill = "Session",
       title = "Experience with Xi Jinping of CCPCC Alternate Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_Alternate_XJP_Exp.png")