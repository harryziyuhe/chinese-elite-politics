library(tidyverse)
library(writexl)
library(readxl)
"%!in%" <- Negate("%in%")

# Functions
#===============================================================================
getjob <- function(jobcode) {
  #university
  if (jobcode > 10000){
    return(list("univ", jobcode))
  }
  #PLA
  if (jobcode > 3000 & jobcode < 4000){
    if (jobcode < 3010){
      return(list("pla", "cmc"))
    }
    if (jobcode < 3020){
      return(list("pla", "Chief of Staff"))
    }
    if (jobcode < 3030){
      return(list("pla", "Political"))
    }
    if (jobcode < 3040){
      return(list("pla", "Logistics"))
    }
    if (jobcode < 3050){
      return(list("pla", "Armament"))
    }
    if (jobcode < 3060){
      return(list("pla", "Training"))
    }
    if (jobcode < 3070){
      return(list("pla", "Mobilization"))
    }
    if (jobcode < 3080){
      return(list("pla", "Law and Politics"))
    }
    if (jobcode < 3090){
      return(list("pla", "Political Department"))
    }
    if (jobcode < 3110){
      if (round(jobcode %% 1, 2) == 0) {
        return(list("pla", "Navy"))
      } else if (round(jobcode %% 1, 2) == 0.01) {
        return(list("pla", "Northern"))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("pla", "Eastern"))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("pla", "Southern"))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("pla", "Chief of Staff"))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("pla", "Political"))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("pla", "Logistics"))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("pla", "Armament"))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("pla", "Discipline"))
      } else if (round(jobcode %% 1, 2) == 0.09) {
        return(list("pla", "Marine"))
      } else if (round(jobcode %% 1, 2) == 0.1) {
        return(list("pla", "Navy Experiment"))
      } else if (round(jobcode %% 1, 2) == 0.11) {
        return(list("pla", "Djibouti"))
      } else if (round(jobcode %% 1, 2) == 0.12) {
        return(list("pla", "Navy Academy"))
      } else if (round(jobcode %% 1, 2) == 0.15) {
        return(list("pla", "Navy"))
      }
    }
    if (jobcode < 3120){
      if (round(jobcode %% 1, 2) == 0) {
        return(list("pla", "AF"))
      } else if (round(jobcode %% 1, 2) == 0.01) {
        return(list("pla", "Central"))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("pla", "Northern"))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("pla", "Western"))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("pla", "Jinan"))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("pla", "Eastern"))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("pla", "Southern"))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("pla", "Chengdu"))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("pla", "Chief of Staff"))
      } else if (round(jobcode %% 1, 2) == 0.09) {
        return(list("pla", "Political"))
      } else if (round(jobcode %% 1, 2) == 0.1) {
        return(list("pla", "Logistics"))
      } else if (round(jobcode %% 1, 2) == 0.11) {
        return(list("pla", "Armament"))
      } else if (round(jobcode %% 1, 2) == 0.12) {
        return(list("pla", "Discipline"))
      } else if (round(jobcode %% 1, 2) == 0.13) {
        return(list("pla", "Airbourne"))
      } else if (round(jobcode %% 1, 2) == 0.14) {
        return(list("pla", "AF Experiment"))
      } else if (round(jobcode %% 1, 2) == 0.15) {
        return(list("pla", "AF Flight Experiment"))
      } else if (round(jobcode %% 1, 2) == 0.16) {
        return(list("pla", "AF Academy"))
      } else if (round(jobcode %% 1, 2) == 0.2) {
        return(list("pla", "AF"))
      }
    }
    if (jobcode < 3130){
      if (round(jobcode %% 1, 2) == 0) {
        return(list("pla", "Missile"))
      } else if (round(jobcode %% 1, 2) == 0.01) {
        return(list("pla", "Missile 51"))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("pla", "Missile 52"))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("pla", "Missile 55"))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("pla", "Missile 56"))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("pla", "Missile 53"))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("pla", "Missile 54"))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("pla", "Missile 52"))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("pla", "Missile 21"))
      } else if (round(jobcode %% 1, 2) == 0.09) {
        return(list("pla", "Missile 20"))
      } else if (round(jobcode %% 1, 2) == 0.1) {
        return(list("pla", "Chief of Staff"))
      } else if (round(jobcode %% 1, 2) == 0.11) {
        return(list("pla", "Political"))
      } else if (round(jobcode %% 1, 2) == 0.12) {
        return(list("pla", "Logistics"))
      } else if (round(jobcode %% 1, 2) == 0.13) {
        return(list("pla", "Armament"))
      } else if (round(jobcode %% 1, 2) == 0.14) {
        return(list("pla", "Discipline"))
      } else if (round(jobcode %% 1, 2) == 0.15) {
        return(list("pla", "Training Base"))
      } else if (round(jobcode %% 1, 2) == 0.16) {
        return(list("pla", "308"))
      } else if (round(jobcode %% 1, 2) == 0.17) {
        return(list("pla", "Missile Research"))
      } else if (round(jobcode %% 1, 2) == 0.18) {
        return(list("pla", "Saudi"))
      } else if (round(jobcode %% 1, 2) == 0.19) {
        return(list("pla", "Missile Academy"))
      }
    } 
    if (jobcode < 3140){
      if (round(jobcode %% 1, 2) == 0) {
        return(list("pla", "Support"))
      } else if (round(jobcode %% 1, 2) == 0.01) {
        return(list("pla", "Chief of Staff"))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("pla", "Political"))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("pla", "Discipline"))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("pla", "Network"))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("pla", "Support Space"))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("pla", "Support Hospital"))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("pla", "311"))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("pla", "Electronic"))
      } else if (round(jobcode %% 1, 2) == 0.09) {
        return(list("pla", "Support Univ"))
      }
    }
    if (jobcode < 3150){
      return(list("pla", "Armed Police"))
    }
    if (jobcode < 3160){
      return(list("pla", "Militia"))
    }
    if (jobcode < 3170){
      return(list("pla", "Specialized"))
    }
    if (jobcode < 3174){
      return(list("pla", "Army"))
    }
    if (jobcode < 3175){
      return(list("pla", "Beijing"))
    }
    if (jobcode < 3176){
      return(list("pla", "Xinjiang"))
    }
    if (jobcode < 3177){
      return(list("pla", "Tibet"))
    }
    if (jobcode < 3179){
      if (round(jobcode %% 1, 2) == 0.01) {
        return(list("pla", "Chief of Staff"))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("pla", "Political"))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("pla", "Logistics"))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("pla", "Armament"))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("pla", "Discipline"))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("pla", "Army 31"))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("pla", "Army 32"))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("pla", "Army Reserach"))
      } else if (round(jobcode %% 1, 2) == 0.09) {
        return(list("pla", "Army 81"))
      } else if (round(jobcode %% 1, 2) == 0.1) {
        return(list("pla", "Army Academy"))
      } else if (round(jobcode %% 1, 2) == 0.11) {
        return(list("pla", "Army Engineering"))
      } else if (round(jobcode %% 1, 2) == 0.12) {
        return(list("pla", "Infrantry Academy"))
      } else if (round(jobcode %% 1, 2) == 0.13) {
        return(list("pla", "Armor Academy"))
      } else if (round(jobcode %% 1, 2) == 0.14) {
        return(list("pla", "Other Army Academy"))
      }
    }
    if (floor(jobcode) == 3179){
      return(list("pla", "Army"))
    }
    if (jobcode < 3190){
      if (round(jobcode %% 1, 2) == 0) {
        return(list("pla", "Safeguard"))
      } else if (round(jobcode %% 1, 2) == 0.01) {
        return(list("pla", "Chief of Staff"))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("pla", "Political"))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("pla", "Discipline"))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("pla", "Safeguard Wuhan"))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("pla", "Safeguard Wuxi"))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("pla", "Safeguard Guilin"))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("pla", "Safeguard Xining"))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("pla", "Safeguard Shenyang"))
      } else if (round(jobcode %% 1, 2) == 0.09) {
        return(list("pla", "Safeguard Zhengzhou"))
      } else if (round(jobcode %% 1, 2) == 0.15) {
        return(list("pla", "PLA Hospital"))
      } else if (round(jobcode %% 1, 2) == 0.25) {
        return(list("pla", "Medical Research"))
      }
    }
    if (jobcode < 3506){
      return(list("pla", "Central"))
    }
    if (jobcode < 3507){
      if (round(jobcode %% 1, 2) < 0.2) {
        return(list("pla", "Beijing"))
      } else if (round(jobcode %% 1, 2) < 0.3) {
        return(list("pla", "Tianjin"))
      } else if (round(jobcode %% 1, 2) < 0.4) {
        return(list("pla", "Hebei"))
      } else if (round(jobcode %% 1, 2) < 0.5) {
        return(list("pla", "Shanxi"))
      } else if (round(jobcode %% 1, 2) < 0.6) {
        return(list("pla", "Inner Mongolia"))
      } else if (round(jobcode %% 1, 2) < 0.7) {
        return(list("pla", "Henan"))
      } else if (round(jobcode %% 1, 2) < 0.8) {
        return(list("pla", "Hubei"))
      } else if (round(jobcode %% 1, 2) < 1) {
        return(list("pla", "Central"))
      }
    }
    if (jobcode < 3508){
      if (round(jobcode %% 1, 2) < 0.2) {
        return(list("pla", "27 Group"))
      } else if (round(jobcode %% 1, 2) < 0.3) {
        return(list("pla", "38 Group"))
      } else if (round(jobcode %% 1, 2) < 0.4) {
        return(list("pla", "65 Group"))
      } else if (round(jobcode %% 1, 2) < 0.5) {
        return(list("pla", "83 Group"))
      }
    }
    if (floor(jobcode) == 3509){
      return(list("pla", "Central"))
    }
    if (jobcode < 3516){
      return(list("pla", "Northern"))
    }
    if (jobcode < 3517){
      if (round(jobcode %% 1, 2) < 0.2) {
        return(list("pla", "Liaoning"))
      } else if (round(jobcode %% 1, 2) < 0.3) {
        return(list("pla", "Jilin"))
      } else if (round(jobcode %% 1, 2) < 0.4) {
        return(list("pla", "Heilongjiang"))
      } else if (round(jobcode %% 1, 2) < 0.5) {
        return(list("pla", "Inner Mongolia"))
      } else if (round(jobcode %% 1, 2) < 0.6) {
        return(list("pla", "Shandong"))
      } else if (round(jobcode %% 1, 2) < 1) {
        return(list("pla", "Northern"))
      }
    }
    if (jobcode < 3518){
      if (round(jobcode %% 1, 2) < 0.2) {
        return(list("pla", "16 Group"))
      } else if (round(jobcode %% 1, 2) < 0.3) {
        return(list("pla", "39 Group"))
      } else if (round(jobcode %% 1, 2) < 0.4) {
        return(list("pla", "40 Group"))
      } else if (round(jobcode %% 1, 2) < 0.5) {
        return(list("pla", "80 Group"))
      }
    }
    if (floor(jobcode) == 3519){
      return(list("pla", "Northern"))
    }
    if (jobcode < 3526){
      return(list("pla", "Western"))
    }
    if (jobcode < 3527){
      if (round(jobcode %% 1, 2) < 0.2) {
        return(list("pla", "Shaanxi"))
      } else if (round(jobcode %% 1, 2) < 0.3) {
        return(list("pla", "Gansu"))
      } else if (round(jobcode %% 1, 2) < 0.4) {
        return(list("pla", "Ningxia"))
      } else if (round(jobcode %% 1, 2) < 0.5) {
        return(list("pla", "Qinghai"))
      } else if (round(jobcode %% 1, 2) < 0.6) {
        return(list("pla", "Xinjiang"))
      } else if (round(jobcode %% 1, 2) < 0.7) {
        return(list("pla", "Sichuan"))
      } else if (round(jobcode %% 1, 2) < 0.8) {
        return(list("pla", "Chongqing"))
      } else if (round(jobcode %% 1, 2) < 1) {
        return(list("pla", "Western"))
      }
    }
    if (jobcode < 3528){
      if (round(jobcode %% 1, 2) < 0.2) {
        return(list("pla", "21 Group"))
      } else if (round(jobcode %% 1, 2) < 0.3) {
        return(list("pla", "47 Group"))
      } else if (round(jobcode %% 1, 2) < 0.4) {
        return(list("pla", "77 Group"))
      }
    }
    if (floor(jobcode) == 3529){
      return(list("pla", "Western"))
    }
    if (jobcode < 3536){
      return(list("pla", "Jinan"))
    }
    if (jobcode < 3537){
      if (round(jobcode %% 1, 2) < 0.2) {
        return(list("pla", "Shandong"))
      } else if (round(jobcode %% 1, 2) < 0.3) {
        return(list("pla", "Henan"))
      }
    }
    if (jobcode < 3538){
      if (round(jobcode %% 1, 2) < 0.2) {
        return(list("pla", "20 Group"))
      } else if (round(jobcode %% 1, 2) < 0.3) {
        return(list("pla", "26 Group"))
      } else if (round(jobcode %% 1, 2) < 0.4) {
        return(list("pla", "54 Group"))
      }
    }
    if (floor(jobcode) == 3539){
      return(list("pla", "Jinan"))
    }
    if (jobcode < 3546){
      return(list("pla", "Eastern"))
    }
    if (jobcode < 3547){
      if (round(jobcode %% 1, 2) < 0.2) {
        return(list("pla", "Shanghai"))
      } else if (round(jobcode %% 1, 2) < 0.3) {
        return(list("pla", "Jiangsu"))
      } else if (round(jobcode %% 1, 2) < 0.4) {
        return(list("pla", "Zhejiang"))
      } else if (round(jobcode %% 1, 2) < 0.5) {
        return(list("pla", "Anhui"))
      } else if (round(jobcode %% 1, 2) < 0.6) {
        return(list("pla", "Fujian"))
      } else if (round(jobcode %% 1, 2) < 0.7) {
        return(list("pla", "Jiangxi"))
      } else if (round(jobcode %% 1, 2) < 1) {
        return(list("pla", "Eastern"))
      }
    }
    if (jobcode < 3548){
      if (round(jobcode %% 1, 2) < 0.2) {
        return(list("pla", "1 Group"))
      } else if (round(jobcode %% 1, 2) < 0.3) {
        return(list("pla", "12 Group"))
      } else if (round(jobcode %% 1, 2) < 0.4) {
        return(list("pla", "31 Group"))
      }
    }
    if (floor(jobcode) == 3549){
      return(list("pla", "Eastern"))
    }
    if (jobcode < 3556){
      return(list("pla", "Southern"))
    }
    if (jobcode < 3557){
      if (round(jobcode %% 1, 2) < 0.2) {
        return(list("pla", "HK"))
      } else if (round(jobcode %% 1, 2) < 0.3) {
        return(list("pla", "Hubei"))
      } else if (round(jobcode %% 1, 2) < 0.4) {
        return(list("pla", "Hunan"))
      } else if (round(jobcode %% 1, 2) < 0.5) {
        return(list("pla", "Guangdong"))
      } else if (round(jobcode %% 1, 2) < 0.6) {
        return(list("pla", "Guangxi"))
      } else if (round(jobcode %% 1, 2) < 0.7) {
        return(list("pla", "Hainan"))
      } else if (round(jobcode %% 1, 2) < 0.8) {
        return(list("pla", "Yunnan"))
      } else if (round(jobcode %% 1, 2) < 0.9) {
        return(list("pla", "Guizhou"))
      } else if (round(jobcode %% 1, 2) < 1) {
        return(list("pla", "Southern"))
      }
    }
    if (jobcode < 3558){
      if (round(jobcode %% 1, 2) < 0.2) {
        return(list("pla", "41 Group"))
      } else if (round(jobcode %% 1, 2) < 0.3) {
        return(list("pla", "42 Group"))
      } else if (round(jobcode %% 1, 2) < 0.5) {
        return(list("pla", "Southern"))
      }
    }
    if (floor(jobcode) == 3559){
      return(list("pla", "Southern"))
    }
    if (jobcode < 3566){
      return(list("pla", "Chengdu"))
    }
    if (jobcode < 3567){
      if (round(jobcode %% 1, 2) < 0.2) {
        return(list("pla", "Chongqing"))
      } else if (round(jobcode %% 1, 2) < 0.3) {
        return(list("pla", "Sichuan"))
      } else if (round(jobcode %% 1, 2) < 0.4) {
        return(list("pla", "Yunnan"))
      } else if (round(jobcode %% 1, 2) < 0.5) {
        return(list("pla", "Guizhou"))
      } else if (round(jobcode %% 1, 2) < 0.6) {
        return(list("pla", "Tibet"))
      }
    }
    if (jobcode < 3568){
      if (round(jobcode %% 1, 2) < 0.2) {
        return(list("pla", "13 Group"))
      } else if (round(jobcode %% 1, 2) < 0.3) {
        return(list("pla", "14 Group"))
      }
    }
    if (floor(jobcode) == 3569){
      return(list("pla", "Chengdu"))
    }
    if (jobcode < 3580){
      return(list("pla", "Unknown"))
    }
    if (jobcode < 3590){
      return(list("pla", "Fuzhou"))
    }
    if (jobcode < 3600){
      return(list("pla", "Wuhan"))
    }
    if (jobcode < 3610){
      return(list("pla", "Fuzhou"))
    }
    return(list("pla", "error"))
  }
  #NPC
  if (jobcode > 4000 & jobcode < 4011){
    return(list("NPC", "NPC"))
  }
  #CPPCC
  if (jobcode > 4500 & jobcode < 4511){
    return(list("CPPCC", "CPPCC"))
  }
  #State Council
  if (jobcode > 1000 & jobcode < 1580){
    if (jobcode < 1100){
      return(list("sc", "SC"))
    } 
    if (jobcode < 1110){
      return(list("sc", "MFA"))
    }
    if (jobcode < 1120){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOD"))
      } else {return(list("SOE", "MOD"))}
    }
    if (jobcode < 1130){
      if (jobcode %% 10 < 9) {
        return(list("sc", "NDRC"))
      } else {return(list("SOE", "NDRC"))}
    } 
    if (jobcode < 1140){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOE"))
      } else {return(list("SOE", "MOE"))}
    } 
    if (jobcode < 1150){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MST"))
      } else {return(list("SOE", "MST"))}
    } 
    if (jobcode < 1160){
      if (jobcode %% 10 < 9) {
        return(list("sc", "DSTC"))
      } else {return(list("SOE", "DSTC"))}
    }
    if (jobcode < 1170){
      if (jobcode %% 10 < 9) {
        return(list("sc", "SNC"))
      } else {return(list("SOE", "SNC"))}
    }
    if (jobcode < 1180){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MPS"))
      } else {return(list("SOE", "MPS"))}
    }
    if (jobcode < 1190){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MSS"))
      } else {return(list("SOE", "MSS"))}
    }
    if (jobcode < 1200){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MSu"))
      } else {return(list("SOE", "MSu"))}
    }
    if (jobcode < 1210){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MCA"))
      } else {return(list("SOE", "MCA"))}
    }
    if (jobcode < 1220){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOJ"))
      } else {return(list("SOE", "MOJ"))}
    }
    if (jobcode < 1230){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOF"))
      } else {return(list("SOE", "MOF"))}
    }
    if (jobcode < 1240){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOP"))
      } else {return(list("SOE", "MOP"))}
    }
    if (jobcode < 1250){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MLSS"))
      } else {return(list("SOE", "MLSS"))}
    }
    if (jobcode < 1260){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MNR"))
      } else {return(list("SOE", "MNR"))}
    }
    if (jobcode < 1270){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MCON"))
      } else {return(list("SOE", "MCON"))}
    }
    if (jobcode < 1280){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOR"))
      } else {return(list("SOE", "MOR"))}
    }
    if (jobcode < 1290){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOT"))
      } else {return(list("SOE", "MOT"))}
    }
    if (jobcode < 1300){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MIIT"))
      } else {return(list("SOE", "MIIT"))}
    }
    if (jobcode < 1310){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MWR"))
      } else {return(list("SOE", "MWR"))}
    }
    if (jobcode < 1320){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MOA"))
      } else {return(list("SOE", "MOA"))}
    }
    if (jobcode < 1330){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MCOM"))
      } else {return(list("SOE", "MCOM"))}
    }
    if (jobcode < 1340){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MCUL"))
      } else {return(list("SOE", "MCUL"))}
    }
    if (jobcode < 1350){
      if (jobcode %% 10 < 9) {
        return(list("sc", "NHC"))
      } else {return(list("SOE", "NHC"))}
    }
    if (jobcode < 1360){
      if (jobcode %% 10 < 9) {
        return(list("sc", "NHC"))
      } else {return(list("SOE", "NHC"))}
    }
    if (jobcode < 1370){
      if (round(jobcode %% 1, 2) == 0) {
        return(list("sc", "PBOC"))
      } else if (round(jobcode %% 1, 2) == 0.01) {
        return(list("SOE", "ICBC"))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("SOE", "ABC"))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("SOE", "BOC"))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("SOE", "CCB"))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("SOE", "BOComm"))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("SOE", "EXIM"))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("SOE", "ADBC"))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("SOE", "CDB"))
      } else if (round(jobcode %% 1, 2) == 0.09) {
        return(list("SOE", "CITIC"))
      } else if (round(jobcode %% 1, 2) == 0.10) {
        return(list("SOE", "CMB"))
      } else if (round(jobcode %% 1, 2) == 0.11) {
        return(list("SOE", "PAB"))
      } else if (round(jobcode %% 1, 2) == 0.12) {
        return(list("SOE", "Industrial"))
      } else if (round(jobcode %% 1, 2) == 0.13) {
        return(list("SOE", "GFB"))
      } else if (round(jobcode %% 1, 2) == 0.14) {
        return(list("SOE", "EBB"))
      } else if (round(jobcode %% 1, 2) == 0.15) {
        return(list("SOE", "PFB"))
      } else if (round(jobcode %% 1, 2) == 0.16) {
        return(list("SOE", "HXB"))
      } else if (round(jobcode %% 1, 2) == 0.17) {
        return(list("SOE", "MSB"))
      } else if (round(jobcode %% 1, 2) == 0.18) {
        return(list("SOE", "ZJB"))
      } else if (round(jobcode %% 1, 2) == 0.19) {
        return(list("SOE", "HFB"))
      } else if (round(jobcode %% 1, 2) == 0.20) {
        return(list("SOE", "BHB"))
      } else if (round(jobcode %% 1, 2) == 0.21) {
        return(list("SOE", "Other Bank"))
      } else if (round(jobcode %% 1, 2) == 0.22) {
        return(list("SOE", "CIC"))
      } else if (round(jobcode %% 1, 2) == 0.23) {
        return(list("sc", "WBIMF"))
      } else if (round(jobcode %% 1, 2) == 0.24) {
        return(list("sc", "SHGOLD"))
      } else if (round(jobcode %% 1, 2) == 0.25) {
        return(list("SOE", "Union"))
      }
    }
    if (jobcode < 1380){
      if (jobcode %% 10 < 9) {
        return(list("sc", "NAO"))
      } else {return(list("SOE", "NAO"))}
    }
    if (jobcode < 1390){
      if (jobcode %% 10 < 9) {
        return(list("sc", "SSRC"))
      } else {return(list("SOE", "SSRC"))}
    }
    if (jobcode < 1400){
      if (jobcode %% 10 < 9) {
        return(list("sc", "ImExMC"))
      } else {return(list("SOE", "ImExMC"))}
    }
    if (jobcode < 1410){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MEP"))
      } else {return(list("SOE", "MEP"))}
    }
    if (jobcode > 1560 & jobcode < 1570){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MEM"))
      } else {return(list("SOE", "MEM"))}
    }
    if (jobcode > 1570 & jobcode < 1580){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MVA"))
      } else {return(list("SOE", "MVA"))}
    }
    if (jobcode < 1420){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MaM"))
      } else {return(list("SOE", "MaM"))}
    }
    if (jobcode < 1430){
      if (jobcode %% 10 < 9) {
        return(list("sc", "ETC"))
      } else {return(list("SOE", "ETC"))}
    }
    if (jobcode < 1440){
      if (jobcode %% 10 < 9) {
        return(list("sc", "CoalM"))
      } else {return(list("SOE", "CoalM"))}
    }
    if (jobcode < 1450){
      if (jobcode %% 10 < 9) {
        return(list("sc", "PCM"))
      } else {return(list("SOE", "PCM"))}
    }
    if (jobcode < 1460){
      if (jobcode %% 10 < 9) {
        return(list("sc", "LIM"))
      } else {return(list("SOE", "LIM"))}
    }
    if (jobcode < 1470){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MIM"))
      } else {return(list("SOE", "MIM"))}
    }
    if (jobcode < 1480){
      if (jobcode %% 10 < 9) {
        return(list("sc", "MEIM"))
      } else {return(list("SOE", "MEIM"))}
    }
    if (jobcode < 1496){
      return(list("sc", "SASAC"))
    }
    if (jobcode < 1500){
      return(list("SOE", "SASAC"))
    }
    if (jobcode < 1510){
      if (round(jobcode %% 1, 2) == 0.01) {
        return(list("sc", "Customs"))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("sc", "Taxation"))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("sc", "SAIC"))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("sc", "AQSIQ"))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("sc", "NRTA"))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("sc", "Sport"))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("sc", "MEM"))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("sc", "Market"))
      } else if (round(jobcode %% 1, 2) == 0.09) {
        return(list("sc", "Statistics"))
      } else if (round(jobcode %% 1, 2) == 0.1) {
        return(list("sc", "MNR"))
      } else if (round(jobcode %% 1, 2) == 0.11) {
        return(list("sc", "IP"))
      } else if (round(jobcode %% 1, 2) == 0.12) {
        return(list("sc", "MCUL"))
      } else if (round(jobcode %% 1, 2) == 0.13) {
        return(list("sc", "Religion"))
      } else if (round(jobcode %% 1, 2) == 0.14) {
        return(list("sc", "Counsellor"))
      } else if (round(jobcode %% 1, 2) == 0.15) {
        return(list("sc", "Office Admin"))
      } else if (round(jobcode %% 1, 2) == 0.16) {
        return(list("sc", "IP"))
      } else if (round(jobcode %% 1, 2) == 0.17) {
        return(list("sc", "MSu"))
      } else if (round(jobcode %% 1, 2) == 0.18) {
        return(list("sc", "MMI"))
      } else if (round(jobcode %% 1, 2) == 0.19) {
        return(list("sc", "MMI"))
      } else if (round(jobcode %% 1, 2) == 0.20) {
        return(list("sc", "NDRC"))
      } else if (round(jobcode %% 1, 2) == 0.21) {
        return(list("sc", "Market"))
      } else if (round(jobcode %% 1, 2) == 0.22) {
        return(list("sc", "MEP"))
      } else if (round(jobcode %% 1, 2) == 0.23) {
        return(list("sc", "MNR"))
      } else if (round(jobcode %% 1, 2) == 0.24) {
        return(list("sc", "Market"))
      } else if (round(jobcode %% 1, 2) == 0.25) {
        return(list("sc", "Construction"))
      } else if (round(jobcode %% 1, 2) == 0.26) {
        return(list("sc", "Foreign Press"))
      } else if (round(jobcode %% 1, 2) == 0.27) {
        return(list("sc", "InternationalCommunications"))
      } else if (round(jobcode %% 1, 2) == 0.28) {
        return(list("sc", "Market"))
      } else if (round(jobcode %% 1, 2) == 0.29) {
        return(list("sc", "MOA"))
      } else if (round(jobcode %% 1, 2) == 0.30) {
        return(list("sc", "CIDCA"))
      }
    }
    if (jobcode < 1520){
      if (round(jobcode %% 1, 2) == 0.01) {
        return(list("sc", "Overseas"))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("sc", "Hong Kong Affairs"))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("sc", "Legislative Affairs"))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("sc", "Research Office"))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("sc", "Taiwan Affairs"))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("sc", "610"))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("sc", "News"))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("sc", "Cyberspace"))
      }
    }
    if (jobcode < 1530){
      if (round(jobcode %% 1, 2) == 0.01) {
        return(list("sc", "Xinhua"))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("sc", "Engineering Academy"))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("sc", "Development Research"))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("sc", "Governance Academy"))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("sc", "MEM"))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("sc", "Meterology"))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("sc", "NAFR"))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("sc", "CSRC"))
      } else if (round(jobcode %% 1, 2) == 0.09) {
        return(list("sc", "NAFR"))
      } else if (round(jobcode %% 1, 2) == 0.10) {
        return(list("sc", "SERC"))
      } else if (round(jobcode %% 1, 2) == 0.11) {
        return(list("sc", "MOF"))
      } else if (round(jobcode %% 1, 2) == 0.12) {
        return(list("sc", "NSFC"))
      } else if (round(jobcode %% 1, 2) == 0.13) {
        return(list("sc", "Cooperatives"))
      } else if (round(jobcode %% 1, 2) == 0.15) {
        return(list("sc", "MNR"))
      } else if (round(jobcode %% 1, 2) == 0.16) {
        return(list("sc", "CMG"))
      } else if (round(jobcode %% 1, 2) == 0.17) {
        return(list("sc", "MWR"))
      } else if (round(jobcode %% 1, 2) == 0.18) {
        return(list("SOE", "CIC"))
      }
    }
    if (jobcode < 1540){
      if (round(jobcode %% 1, 2) == 0.01) {
        return(list("sc", "Complaints"))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("sc", "NDRC"))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("sc", "NDRC"))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("sc", "MIIT"))
      } else if (round(jobcode %% 1, 2) == 0.06) {
        return(list("sc", "MLSS"))
      } else if (round(jobcode %% 1, 2) == 0.07) {
        return(list("sc", "MLSS"))
      } else if (round(jobcode %% 1, 2) == 0.08) {
        return(list("sc", "MNR"))
      } else if (round(jobcode %% 1, 2) == 0.09) {
        return(list("sc", "MNR"))
      } else if (round(jobcode %% 1, 2) == 0.10) {
        return(list("sc", "MOT"))
      } else if (round(jobcode %% 1, 2) == 0.11) {
        return(list("sc", "MOT"))
      } else if (round(jobcode %% 1, 2) == 0.12) {
        return(list("sc", "MOT"))
      } else if (round(jobcode %% 1, 2) == 0.13) {
        return(list("sc", "MCUL"))
      } else if (round(jobcode %% 1, 2) == 0.14) {
        return(list("sc", "NHC"))
      } else if (round(jobcode %% 1, 2) == 0.15) {
        return(list("sc", "PBOC"))
      } else if (round(jobcode %% 1, 2) == 0.16) {
        return(list("sc", "MEM"))
      } else if (round(jobcode %% 1, 2) == 0.17) {
        return(list("party", "General Office"))
      } else if (round(jobcode %% 1, 2) == 0.18) {
        return(list("party", "General Office"))
      } else if (round(jobcode %% 1, 2) == 0.19) {
        return(list("party", "General Office"))
      } else if (round(jobcode %% 1, 2) == 0.20) {
        return(list("sc", "DSTC"))
      } else if (round(jobcode %% 1, 2) == 0.21) {
        return(list("sc", "DSTC"))
      } else if (round(jobcode %% 1, 2) == 0.22) {
        return(list("sc", "MOE"))
      } else if (round(jobcode %% 1, 2) == 0.23) {
        return(list("sc", "MEP"))
      } else if (round(jobcode %% 1, 2) == 0.24) {
        return(list("sc", "NDRC"))
      } else if (round(jobcode %% 1, 2) == 0.25) {
        return(list("sc", "Market"))
      } else if (round(jobcode %% 1, 2) == 0.26) {
        return(list("sc", "Market"))
      } else if (round(jobcode %% 1, 2) == 0.27) {
        return(list("SOE", "Gold"))
      } else if (round(jobcode %% 1, 2) == 0.28) {
        return(list("sc", "NDRC"))
      } else if (round(jobcode %% 1, 2) == 0.29) {
        return(list("sc", "MOF"))
      } else if (round(jobcode %% 1, 2) == 0.30) {
        return(list("sc", "MNR"))
      } else if (round(jobcode %% 1, 2) == 0.31) {
        return(list("sc", "Customs"))
      } else if (round(jobcode %% 1, 2) == 0.32) {
        return(list("sc", "Non-Ferrous Metal"))
      } else if (round(jobcode %% 1, 2) == 0.33) {
        return(list("sc", "Market"))
      }
    }
    if (jobcode < 1550) {
      return(list("sc", "LSG"))
    }
    if (jobcode > 1550 & jobcode < 1570){
      if (round(jobcode %% 1, 2) == 0.01) {
        return(list("sc", "HK Liason"))
      } else if (round(jobcode %% 1, 2) == 0.02) {
        return(list("sc", "Macau Liason"))
      } else if (round(jobcode %% 1, 2) == 0.03) {
        return(list("sc", "Large Firms"))
      } else if (round(jobcode %% 1, 2) == 0.04) {
        return(list("sc", "MFA"))
      } else if (round(jobcode %% 1, 2) == 0.05) {
        return(list("sc", "HK Security"))
      }
    }
    return(list("sc", "error"))
  }
  #Mass Organization
  if (jobcode > 6400 & jobcode < 6410){
    if (round(jobcode %% 1, 2) == 0.01) {
      return(list("mo", "ACFTU"))
    } else if (round(jobcode %% 1, 2) == 0.02) {
      return(list("mo", "ACWF"))
    } else if (round(jobcode %% 1, 2) == 0.03) {
      return(list("mo", "ACFIC"))
    } else if (round(jobcode %% 1, 2) == 0.04) {
      return(list("mo", "CCPIT"))
    } else if (round(jobcode %% 1, 2) == 0.05) {
      return(list("mo", "CAST"))
    } else if (round(jobcode %% 1, 2) == 0.06) {
      return(list("mo", "CFLAC"))
    } else if (round(jobcode %% 1, 2) == 0.07) {
      return(list("mo", "CWA"))
    } else if (round(jobcode %% 1, 2) == 0.08) {
      return(list("mo", "CLS"))
    } else if (round(jobcode %% 1, 2) == 0.09) {
      return(list("mo", "ACJA"))
    } else if (round(jobcode %% 1, 2) == 0.10) {
      return(list("mo", "ACFROC"))
    } else if (round(jobcode %% 1, 2) == 0.11) {
      return(list("mo", "ACFTC"))
    } else if (round(jobcode %% 1, 2) == 0.12) {
      return(list("mo", "WRSA"))
    } else if (round(jobcode %% 1, 2) == 0.13) {
      return(list("mo", "CPAFFC"))
    } else if (round(jobcode %% 1, 2) == 0.14) {
      return(list("mo", "CPIFA"))
    } else if (round(jobcode %% 1, 2) == 0.15) {
      return(list("mo", "CDPF"))
    } else if (round(jobcode %% 1, 2) == 0.16) {
      return(list("mo", "RCSC"))
    } else if (round(jobcode %% 1, 2) == 0.17) {
      return(list("mo", "CSCLF"))
    } else if (round(jobcode %% 1, 2) == 0.18) {
      return(list("mo", "HuangPu"))
    } else if (round(jobcode %% 1, 2) == 0.19) {
      return(list("mo", "CSIPW"))
    } else if (round(jobcode %% 1, 2) == 0.20) {
      return(list("mo", "NAVEC"))
    } else if (round(jobcode %% 1, 2) == 0.21) {
      return(list("mo", "PPA"))
    } else if (round(jobcode %% 1, 2) == 0.22) {
      return(list("mo", "ACYF"))
    } else if (round(jobcode %% 1, 2) == 0.23) {
      return(list("mo", "CBA"))
    } else if (round(jobcode %% 1, 2) == 0.24) {
      return(list("mo", "RCHR"))
    } else if (round(jobcode %% 1, 2) == 0.25) {
      return(list("mo", "Socialist"))
    } else if (round(jobcode %% 1, 2) == 0.26) {
      return(list("mo", "CSA"))
    } else if (round(jobcode %% 1, 2) == 0.27) {
      return(list("mo", "CBA"))
    } else if (round(jobcode %% 1, 2) == 0.28) {
      return(list("mo", "ACSF"))
    } else if (round(jobcode %% 1, 2) == 0.29) {
      return(list("mo", "ACSportF"))
    } else if (round(jobcode %% 1, 2) == 0.30) {
      return(list("mo", "CSNP"))
    } else if (round(jobcode %% 1, 2) == 0.31) {
      return(list("mo", "CCA"))
    } else if (round(jobcode %% 1, 2) == 0.32) {
      return(list("mo", "ARATS"))
    } else if (round(jobcode %% 1, 2) == 0.33) {
      return(list("mo", "CNLIC"))
    }
  }
  #Party
  if (jobcode > 6000 & jobcode < 7000){
    if (jobcode < 6005){
      return(list("CCP", ""))
    }
    if (jobcode < 6010){
      return(list("party", "Secretariat"))
    }
    if (jobcode < 6020){
      return(list("party", "COD"))
    }
    if (jobcode < 6030){
      return(list("party", "CPD"))
    }
    if (jobcode < 6040){
      return(list("party", "UFD"))
    }
    if (jobcode < 6050){
      return(list("party", "CID"))
    }
    if (jobcode < 6060){
      return(list("party", "General Office"))
    }
    if (jobcode < 6080){
      return(list("party", "Newspapers"))
    }
    if (jobcode < 6090){
      return(list("party", "CPHRC"))
    }
    if (jobcode < 6100){
      return(list("party", "CPDRC"))
    }
    if (jobcode < 6110){
      return(list("party", "CTB"))
    }
    if (jobcode < 6120){
      return(list("party", "CSOWC"))
    }
    if (jobcode < 6130){
      return(list("party", "CDOWC"))
    }
    if (jobcode < 6150){
      return(list("party", "CLPLG"))
    }
    if (jobcode < 6160){
      return(list("party", "CSSC"))
    }
    if (jobcode < 6170){
      return(list("party", "CPRC"))
    }
    if (jobcode < 6180){
      return(list("party", "CTWO"))
    }
    if (jobcode < 6190){
      return(list("party", "CEPO"))
    }
    if (jobcode < 6200){
      return(list("party", "CEAC"))
    }
    if (jobcode < 6210){
      return(list("party", "CSC"))
    }
    if (jobcode < 6220){
      return(list("party", "CGB"))
    }
    if (jobcode < 6230){
      return(list("party", "CRC"))
    }
    if (jobcode > 6300 & jobcode < 6310){
      return(list("party", "LSG"))
    }
    if (jobcode > 6500 & jobcode < 6510){
      return(list("party", "CDIC"))
    }
    if (jobcode > 6600 & jobcode < 6610){
      return(list("party", "CYL"))
    }
    if (jobcode > 6700 & jobcode < 6710){
      return(list("party", "CAC"))
    }
    if (jobcode > 6710 & jobcode < 6720){
      return(list("party", "CISIC"))
    }
    if (jobcode > 6900 & jobcode < 7000){
      if (round(jobcode %% 1, 2) == 0.1) {
        return(list("sc", "Law and Politics"))
      } else if (round(jobcode %% 1, 2) == 0.2) {
        return(list("sc", "Industrial"))
      } else if (round(jobcode %% 1, 2) == 0.3) {
        return(list("sc", "Agriculture"))
      } else if (round(jobcode %% 1, 2) == 0.4) {
        return(list("sc", "Propaganda"))
      }
    }
    return(list("party", "error"))
  }
  #Province
  if (jobcode > 2000 & jobcode < 3000){
    if (jobcode < 2011) {
      return(list("pro", "Beijing"))
    }
    if (jobcode < 2021) {
      return(list("pro", "Tianjin"))
    }
    if (jobcode < 2031) {
      return(list("pro", "Hebei"))
    }
    if (jobcode < 2041) {
      return(list("pro", "Shanxi"))
    }
    if (jobcode < 2051) {
      return(list("pro", "Inner Mongolia"))
    }
    if (jobcode < 2061) {
      return(list("pro", "Liaoning"))
    }
    if (jobcode < 2071) {
      return(list("pro", "Jilin"))
    }
    if (jobcode < 2081) {
      return(list("pro", "Heilongjiang"))
    }
    if (jobcode < 2091) {
      return(list("pro", "Shanghai"))
    }
    if (jobcode < 2101) {
      return(list("pro", "Jiangsu"))
    }
    if (jobcode < 2111) {
      return(list("pro", "Zhejiang"))
    }
    if (jobcode < 2121) {
      return(list("pro", "Anhui"))
    }
    if (jobcode < 2131) {
      return(list("pro", "Fujian"))
    }
    if (jobcode < 2141) {
      return(list("pro", "Jiangxi"))
    }
    if (jobcode < 2151) {
      return(list("pro", "Shandong"))
    }
    if (jobcode < 2161) {
      return(list("pro", "Henan"))
    }
    if (jobcode < 2171) {
      return(list("pro", "Hubei"))
    }
    if (jobcode < 2181) {
      return(list("pro", "Hunan"))
    }
    if (jobcode < 2191) {
      return(list("pro", "Guangdong"))
    }
    if (jobcode < 2201) {
      return(list("pro", "Guangxi"))
    }
    if (jobcode < 2211) {
      return(list("pro", "Hainan"))
    }
    if (jobcode < 2221) {
      return(list("pro", "Chongqing"))
    }
    if (jobcode < 2231) {
      return(list("pro", "Sichuan"))
    }
    if (jobcode < 2241) {
      return(list("pro", "Guizhou"))
    }
    if (jobcode < 2251) {
      return(list("pro", "Yunnan"))
    }
    if (jobcode < 2261) {
      return(list("pro", "Tibet"))
    }
    if (jobcode < 2271) {
      return(list("pro", "Shaanxi"))
    }
    if (jobcode < 2281) {
      return(list("pro", "Gansu"))
    }
    if (jobcode < 2291) {
      return(list("pro", "Qinghai"))
    }
    if (jobcode < 2301) {
      return(list("pro", "Ningxia"))
    }
    if (jobcode < 2311) {
      return(list("pro", "Xinjiang"))
    }
    return(list("pro", "error"))
  }
  #NPC
  if (jobcode > 5000 & jobcode < 5010){
    return(list("NPC", "SPC"))
  }
  if (jobcode > 5010 & jobcode < 5020){
    return(list("NPC", "SPP"))
  }
  return("unknown", "error")
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
  dataframe$nmo <- 0
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
      jobdesc <- joblst[1]
      jobloc <- joblst[2]
      #print(as.numeric(dataframe[entry, jobid]))
      if (jobloc == "error") {
        print(as.numeric(dataframe[entry, jobid]))
        next
      }
      if (dataframe[entry, jobe] >= eyear & jobdesc %in% joblist){
        dataframe[entry, paste0("last", jobdesc)] = 1
      }
      careerpath <- c(careerpath, paste(jobdesc, jobloc))
      if (jobdesc == "pro" & jobloc %!in% province){
        province <- c(province, jobloc)
        dataframe$npro[entry] <- dataframe$npro[entry] + 1
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

# Data Processing
#===============================================================================
biographical_data18th_20thPC <- read_excel("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/dataset/biographical_data18th_20thPC.xlsx")
birthplace <- read_excel("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/dataset/province_uni_codebook.xlsx", sheet = "birthplace")
unis <- read_excel("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/dataset/province_uni_codebook.xlsx", sheet = "schools")
network <- read_excel("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/dataset/Network_18th_20thPC_clean.xlsx")

# utilities 
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
psc20 <- master |> 
  filter(cc20 < 2000)
psc19 <- master |> 
  filter(cc19 < 2000)
psc18 <- master |> 
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
psc20[17, c("job2", "job2s", "job2e")] <- list(1123.00, 2014.0, 2017)


#Career Track and Width
career_var <- c("cname", "ename", "byear", "pyear", "bpro", "edu", 
                "bauniv", "bamajor", "mauniv", "mamajor", "phduniv", "phdmajor",
                "cc18", "cc19", "cc20", "lastparty", "lastSOE", "lastsc",
                "lastpro", "lastCPPCC", "lastNPC", "lastuniv", "lastpla",
                "lastmo", "nparty", "nSOE", "nsc", "npro", "nCPPCC",
                "nNPC", "nuniv", "npla", "nmo")
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

career_return <- career(psc18, 2012)
psc18_career <- career_return[[1]][, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
psc18_path <- career_return[[2]]

career_return <- career(psc19, 2017)
psc19_career <- career_return[[1]][, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
psc19_path <- career_return[[2]]

career_return <- career(psc20, 2022)
psc20_career <- career_return[[1]][, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
psc20_path <- career_return[[2]]

network[network > 1 & network <= 3] = 2
network[network > 3 & network <= 10] = 3
network[network > 10 & network < 1000] = 4
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
  fcc18 |> filter(ethnic == 1) |> nrow(),
  acc18 |> filter(ethnic == 1) |> nrow(),
  fcc19 |> filter(ethnic == 1) |> nrow(),
  acc19 |> filter(ethnic == 1) |> nrow(),
  fcc20 |> filter(ethnic == 1) |> nrow(),
  acc20 |> filter(ethnic == 1) |> nrow()
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

removelst <- c()
offset <- 0
fcc18_network <- fcc18_career |> 
  dplyr::select(ename) |> 
  left_join(network[, c("ename", "Xi Jinping")])
fcc18_network[is.na(fcc18_network)] = 0
if (nrow(distinct(data.frame(fcc18$ename))) == nrow(fcc18)) {
  fcc18_network <- distinct(fcc18_network)
} else {
  for (i in 1:nrow(fcc18)) {
    for (j in (i+offset+1):nrow(fcc18_network)) {
      if (j > nrow(fcc18_network)) {break}
      if (fcc18_network$ename[j] == fcc18$ename[i]){
        offset <- offset + 1
        if (fcc18_network$`Xi Jinping`[j] == fcc18_network$`Xi Jinping`[i]){
          removelst <- c(removelst, j)
        }
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
offset <- 0
acc18_network <- acc18_career |> 
  dplyr::select(ename) |> 
  left_join(network[, c("ename", "Xi Jinping")])
acc18_network[is.na(acc18_network)] = 0
if (nrow(distinct(data.frame(acc18$ename))) == nrow(acc18)) {
  acc18_network <- distinct(acc18_network)
} else {
  for (i in 1:nrow(acc18)) {
    for (j in (i+offset+1):nrow(acc18_network)) {
      if (j > nrow(acc18_network)) {break}
      if (acc18_network$ename[j] == acc18$ename[i]){
        offset <- offset + 1
        if (acc18_network$`Xi Jinping`[j] == acc18_network$`Xi Jinping`[i]){
          removelst <- c(removelst, j)
        }
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
offset <- 0
fcc19_network <- fcc19_career |> 
  dplyr::select(ename) |> 
  left_join(network[, c("ename", "Xi Jinping")])
fcc19_network[is.na(fcc19_network)] = 0
if (nrow(distinct(data.frame(fcc19$ename))) == nrow(fcc19)) {
  fcc19_network <- distinct(fcc19_network)
} else {
  for (i in 1:nrow(fcc19)) {
    for (j in (i+offset+1):nrow(fcc19_network)) {
      if (j > nrow(fcc19_network)) {break}
      if (fcc19_network$ename[j] == fcc19$ename[i]){
        offset <- offset + 1
        if (fcc19_network$`Xi Jinping`[j] == fcc19_network$`Xi Jinping`[i]){
          removelst <- c(removelst, j)
        }
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
offset <- 0
acc19_network <- acc19_career |> 
  dplyr::select(ename) |> 
  left_join(network[, c("ename", "Xi Jinping")])
acc19_network[is.na(acc19_network)] = 0
if (nrow(distinct(data.frame(acc19$ename))) == nrow(acc19)) {
  acc19_network <- distinct(acc19_network)
} else {
  for (i in 1:nrow(acc19)) {
    for (j in (i+offset+1):nrow(acc19_network)) {
      if (j > nrow(acc19_network)) {break}
      if (acc19_network$ename[j] == acc19$ename[i]){
        offset <- offset + 1
        if (acc19_network$`Xi Jinping`[j] == acc19_network$`Xi Jinping`[i]){
          removelst <- c(removelst, j)
        }
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
offset <- 0
fcc20_network <- fcc20_career |> 
  dplyr::select(ename) |> 
  left_join(network[, c("ename", "Xi Jinping")])
fcc20_network[is.na(fcc20_network)] = 0
if (nrow(distinct(data.frame(fcc20$ename))) == nrow(fcc20)) {
  fcc20_network <- distinct(fcc20_network)
} else {
  for (i in 1:nrow(fcc20)) {
    for (j in (i+offset+1):nrow(fcc20_network)) {
      if (j > nrow(fcc20_network)) {break}
      if (fcc20_network$ename[j] == fcc20$ename[i]){
        offset <- offset + 1
        if (fcc20_network$`Xi Jinping`[j] == fcc20_network$`Xi Jinping`[i]){
          removelst <- c(removelst, j)
        }
      } else {break}
    }
  }
}
if (length(removelst) > 0) {
  fcc20_network <- fcc20_network[-removelst, ]
}
fcc20_network$ename[5] = "Wang Kai"
if (nrow(fcc20_network) != nrow(fcc20_career)) {
  print("Require Manual Review")
}

removelst <- c()
offset <- 0
acc20_network <- acc20_career |> 
  dplyr::select(ename) |> 
  left_join(network[, c("ename", "Xi Jinping")])
acc20_network[is.na(acc20_network)] = 0
if (nrow(distinct(data.frame(acc20$ename))) == nrow(acc20)) {
  acc20_network <- distinct(acc20_network)
} else {
  for (i in 1:nrow(acc20)) {
    for (j in (i+offset+1):nrow(acc20_network)) {
      if (j > nrow(acc20_network)) {break}
      if (acc20_network$ename[j] == acc20$ename[i]){
        offset <- offset + 1
        if (acc20_network$`Xi Jinping`[j] == acc20_network$`Xi Jinping`[i]){
          removelst <- c(removelst, j)
        }
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

stats$Xi_0 <- c(
  fcc18_network |> filter(`Xi Jinping` == 0) |> nrow(),
  acc18_network |> filter(`Xi Jinping` == 0) |> nrow(),
  fcc19_network |> filter(`Xi Jinping` == 0) |> nrow(),
  acc19_network |> filter(`Xi Jinping` == 0) |> nrow(),
  fcc20_network |> filter(`Xi Jinping` == 0) |> nrow(),
  acc20_network |> filter(`Xi Jinping` == 0) |> nrow()
) * 100 / stats$size

stats$Xi_1 <- c(
  fcc18_network |> filter(`Xi Jinping` == 1) |> nrow(),
  acc18_network |> filter(`Xi Jinping` == 1) |> nrow(),
  fcc19_network |> filter(`Xi Jinping` == 1) |> nrow(),
  acc19_network |> filter(`Xi Jinping` == 1) |> nrow(),
  fcc20_network |> filter(`Xi Jinping` == 1) |> nrow(),
  acc20_network |> filter(`Xi Jinping` == 1) |> nrow()
) * 100 / stats$size

stats$Xi_2 <- c(
  fcc18_network |> filter(`Xi Jinping` == 2) |> nrow(),
  acc18_network |> filter(`Xi Jinping` == 2) |> nrow(),
  fcc19_network |> filter(`Xi Jinping` == 2) |> nrow(),
  acc19_network |> filter(`Xi Jinping` == 2) |> nrow(),
  fcc20_network |> filter(`Xi Jinping` == 2) |> nrow(),
  acc20_network |> filter(`Xi Jinping` == 2) |> nrow()
) * 100 / stats$size

stats$Xi_3 <- c(
  fcc18_network |> filter(`Xi Jinping` == 3) |> nrow(),
  acc18_network |> filter(`Xi Jinping` == 3) |> nrow(),
  fcc19_network |> filter(`Xi Jinping` == 3) |> nrow(),
  acc19_network |> filter(`Xi Jinping` == 3) |> nrow(),
  fcc20_network |> filter(`Xi Jinping` == 3) |> nrow(),
  acc20_network |> filter(`Xi Jinping` == 3) |> nrow()
) * 100 / stats$size

stats$Xi_4 <- c(
  fcc18_network |> filter(`Xi Jinping` == 4) |> nrow(),
  acc18_network |> filter(`Xi Jinping` == 4) |> nrow(),
  fcc19_network |> filter(`Xi Jinping` == 4) |> nrow(),
  acc19_network |> filter(`Xi Jinping` == 4) |> nrow(),
  fcc20_network |> filter(`Xi Jinping` == 4) |> nrow(),
  acc20_network |> filter(`Xi Jinping` == 4) |> nrow()
) * 100 / stats$size

stats$Xi <- 100 - stats$Xi_0

stats$Xi_avg <- c(
  fcc18_network |> dplyr::select(`Xi Jinping`) |> colMeans(na.rm = T),
  acc18_network |> dplyr::select(`Xi Jinping`) |> colMeans(na.rm = T),
  fcc19_network |> dplyr::select(`Xi Jinping`) |> colMeans(na.rm = T),
  acc19_network |> dplyr::select(`Xi Jinping`) |> colMeans(na.rm = T),
  fcc20_network |> dplyr::select(`Xi Jinping`) |> colMeans(na.rm = T),
  acc20_network |> dplyr::select(`Xi Jinping`) |> colMeans(na.rm = T)
)

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
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
             grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("sc SASAC", joblist) |
             grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
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
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
             grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("sc SASAC", joblist) |
             grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
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
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
             grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("sc SASAC", joblist) |
             grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
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
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
             grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("sc SASAC", joblist) |
             grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
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
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
             grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("sc SASAC", joblist) |
             grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
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
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
             grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("sc SASAC", joblist) |
             grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
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
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
             grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
             grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
             grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
             grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
             grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
             grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
             grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
             grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("sc SASAC", joblist) |
             grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
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
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
        grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
        grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
        grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
        grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
        grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
        grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
        grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
        grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("sc SASAC", joblist) |
        grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
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
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
      grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
      grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
      grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
      grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
      grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
      grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
      grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
      grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("SASAC", joblist) |
      grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
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
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
      grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
      grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
      grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
      grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
      grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
      grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
      grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
      grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("SASAC", joblist) |
      grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
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
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
      grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
      grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
      grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
      grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
      grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
      grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
      grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
      grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("SASAC", joblist) |
      grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
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
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
      grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
      grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
      grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
      grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
      grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
      grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
      grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
      grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("SASAC", joblist) |
      grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
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

#====PSC18====
psc18_expert <- data.frame(cname = psc18$cname,
                           finance = NA, tech = NA, scientist = NA)
for (i in 1:nrow(psc18_expert)){
  joblist <- toString(psc18_path$career[[i]])
  if (grepl("DSTC", joblist)) {
    psc18_expert$tech[i] = 1
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
      grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
      grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
      grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
      grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
      grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
      grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
      grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
      grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("SASAC", joblist) |
      grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
    psc18_expert$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    psc18_expert$scientist[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(psc18$phdmajor[i])) {
      if (psc18$phdmajor[i] == 4) {
        psc18_expert$scientist[i] = 1
      }
    } else if ((!is.na(psc18$mamajor[i]) & psc18$mamajor[i] == 4)){
      psc18_expert$scientist[i] = 1
    } 
  }
}

#====PSC19====
psc19_expert <- data.frame(cname = psc19$cname,
                           finance = NA, tech = NA, scientist = NA)
for (i in 1:nrow(psc19_expert)){
  joblist <- toString(psc19_path$career[[i]])
  if (grepl("DSTC", joblist)) {
    psc19_expert$tech[i] = 1
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
      grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
      grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
      grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
      grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
      grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
      grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
      grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
      grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("SASAC", joblist) |
      grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
    psc19_expert$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    psc19_expert$scientist[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(psc19$phdmajor[i])) {
      if (psc19$phdmajor[i] == 4) {
        psc19_expert$scientist[i] = 1
      }
    } else if ((!is.na(psc19$mamajor[i]) & psc19$mamajor[i] == 4)){
      psc19_expert$scientist[i] = 1
    } 
  }
}

#====PSC20====
psc20_expert <- data.frame(cname = psc20$cname,
                           finance = NA, tech = NA, scientist = NA)
for (i in 1:nrow(psc20_expert)){
  joblist <- toString(psc20_path$career[[i]])
  if (grepl("DSTC", joblist)) {
    psc20_expert$tech[i] = 1
  } else if (grepl("PBOC", joblist) | grepl("ICBC", joblist) | grepl("ABC", joblist) |
      grepl("BOC", joblist) | grepl("CCB", joblist) | grepl("BOComm", joblist) |
      grepl("EXIM", joblist) | grepl("ADBC", joblist) | grepl("CDB", joblist) |
      grepl("CITIC", joblist) | grepl("CMB", joblist) | grepl("PAB", joblist) |
      grepl("Industrial", joblist) | grepl("GFB", joblist) | grepl("EBB", joblist) |
      grepl("PFB", joblist) | grepl("HXB", joblist) | grepl("MSB", joblist) |
      grepl("ZJB", joblist) | grepl("HFB", joblist) | grepl("BHB", joblist) |
      grepl("Other Bank", joblist) | grepl("CIC", joblist) | grepl("WBIMF", joblist) |
      grepl("SHGOLD", joblist) | grepl("Union", joblist) | grepl("SASAC", joblist) |
      grepl("NAFR", joblist) | grepl("CSRC", joblist)) {
    psc20_expert$finance[i] = 1
  } else if (grepl("univ", joblist) & (grepl("Engineering Academy", joblist) | grepl("NSFC", joblist))){
    psc20_expert$scientist[i] = 1
  } else if (grepl("univ", joblist)) {
    if (!is.na(psc20$phdmajor[i])) {
      if (psc20$phdmajor[i] == 4) {
        psc20_expert$scientist[i] = 1
      }
    } else if ((!is.na(psc20$mamajor[i]) & psc20$mamajor[i] == 4)){
      psc20_expert$scientist[i] = 1
    } 
  }
}
#===============================================================================

#Visualization

#===============================================================================

fcc_stats <- stats[c(1, 3, 5),]
acc_stats <- stats[c(2, 4, 6),]

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
  geom_bar(aes(x = Position, y = count, fill = session), stat = "identity", position = "dodge") +
  labs(x = "Narrow Path", y = "Count", fill = "Session",
       title = "Narrow Career Paths of CCP Central Committee Alternate Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/Narrow_Paths_CCPCC_Full.png")


ggplot(acc_narrow) +
  geom_bar(aes(x = Position, y = count, fill = session), stat = "identity", position = "dodge") +
  labs(x = "Narrow Path", y = "Count", fill = "Session",
       title = "Narrow Career Paths of CCP Central Committee Alternate Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/Narrow_Paths_CCPCC_Alternate.png")

fcc_xi <- stats[c(1, 3, 5), c("session", "Xi")]
acc_xi <- stats[c(2, 4, 6), c("session", "Xi")]

ggplot() +
  geom_line(aes(x = session, y = Xi, group = 1, lty = "Full Member"), data = fcc_xi) +
  geom_line(aes(x = session, y = Xi, group = 1, lty = "Alternate Member"), data = acc_xi) +
  scale_linetype_manual(values = c("Full Member" = 1,
                                   "Alternate Member" = 2)) +
  labs(x = "Session", y = "Percentage of Members",
       title = "Share of CCPCC Members with Ties with Xi (2012-2022)",
       lty = "Member") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_Xi_Ties_Total.png")

fcc_xiyear <- fcc_stats[, c("Xi_1", "Xi_2", "Xi_3", "Xi_4")] |> 
  t() |> data.frame()
names(fcc_xiyear) = c("18", "19", "20")
fcc_xiyear$exp = factor(c("Less than 1 Year", "1-3 Years", "3-10 Years", "More than 10 Years"),
                        levels = c("Less than 1 Year", "1-3 Years", "3-10 Years", "More than 10 Years"))
fcc_xiyear <- fcc_xiyear |>
  gather(session, percentage, "18":"20")

acc_xiyear <- acc_stats[, c("Xi_1", "Xi_2", "Xi_3", "Xi_4")] |> 
  t() |> data.frame()
names(acc_xiyear) = c("18", "19", "20")
acc_xiyear$exp = factor(c("Less than 1 Year", "1-3 Years", "3-10 Years", "More than 10 Years"),
                        levels = c("Less than 1 Year", "1-3 Years", "3-10 Years", "More than 10 Years"))
acc_xiyear <- acc_xiyear |> 
  gather(session, percentage, "18":"20")

ggplot(fcc_xiyear)+
  geom_bar(aes(x = exp, y = percentage, fill = session), stat = "identity", position = "dodge") +
  labs(x = "Position", y = "Percentage of Members", fill = "Session",
       title = "Experience with Xi of CCPCC Full Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_Full_Xi_Exp.png")

ggplot(acc_xiyear)+
  geom_bar(aes(x = exp, y = percentage, fill = session), stat = "identity", position = "dodge") +
  labs(x = "Position", y = "Percentage of Members", fill = "Session",
       title = "Experience with Xi of CCPCC Alternate Members (2012-2022)") +
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/CCPCC_Alternate_Xi_Exp.png")

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

psc_expert <- colSums(psc18_expert[, 2:ncol(psc18_expert)], na.rm = T) |> 
  data.frame() |> 
  cbind(colSums(psc19_expert[, 2:ncol(psc19_expert)], na.rm = T)) |> 
  cbind(colSums(psc20_expert[, 2:ncol(psc20_expert)], na.rm = T))
names(psc_expert) <- c("18", "19", "20")
psc_expert$Position <- c("Finance", "Technology", "Natural Science")
psc_expert <- psc_expert |> 
  gather(session, count, "18":"20")

ggplot(psc_expert) +
  geom_bar(aes(x = Position, y = count, fill = session), stat = "identity", position = "dodge") +
  labs(x = "Expertise", y = "Count", fill = "Session",
       title = "Career Expertise of Politburo Members (2012-2022)") +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        legend.position = "bottom")
ggsave("/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/figures/Politburo_Expertise.png")

total_expert <- psc_expert |> 
  cbind(fcc_expert$count, acc_expert$count)
names(total_expert) <- c("Expertise", "Session", "PSC", "FCC", "ACC")
total_expert <- total_expert |> 
  mutate(Total = PSC + FCC + ACC)

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


write_xlsx(stats, "/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/dataset/summary_stats.xlsx")
