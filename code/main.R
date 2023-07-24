library(tidyverse)
library(writexl)
"%!in%" <- Negate("%in%")

#==============================================================================#
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
        return(list("sc", "MST"))
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
        return(list("sc", "MIIT"))
      } else if (round(jobcode %% 1, 2) == 0.21) {
        return(list("sc", "MIIT"))
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
  joblist <- c("pro", "sc", "SOE", "party", "pla", "univ", "NPC", "CPPCC", "mo")
  dataframe$lastpro <- dataframe$lastsc <- dataframe$lastSOE <- dataframe$lastparty <- 0
  dataframe$lastpla <- dataframe$lastuniv <- dataframe$lastNPC <- dataframe$lastCPPCC <- 0
  dataframe$lastmo <- 0
  dataframe$npro <- dataframe$nsc <- dataframe$nSOE <- dataframe$nparty <- 0
  dataframe$npla <- dataframe$nuniv <- dataframe$nNPC <- dataframe$nCPPCC <- 0
  dataframe$nmo <- 0
  for (entry in 1:n){
    province <- c()
    state <- c()
    SOE <- c()
    party <- c()
    pla <- c()
    univ <- c()
    NPC <- c()
    CPPCC <- c()
    MO <- c()
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
    #print(paste("Province", province))
    #print(paste("State Council", state))
    #print(paste("PLA", pla))
    #print(paste("SOE", SOE))
    #print(paste("Party", party))
    #print(paste("University", univ))
  }
  return(dataframe)
}
#==============================================================================#

biographical_data18th_20thPC <- read_excel("Documents/GitHub/chinese-elite-politics/dataset/biographical_data18th-20th.xlsx")
birthplace <- read_excel("Documents/GitHub/chinese-elite-politics/dataset/province_uni_codebook.xlsx", sheet = "birthplace")
unis <- read_excel("Documents/GitHub/chinese-elite-politics/dataset/province_uni_codebook.xlsx", sheet = "schools")

# utilities 
top_unis <- unis$code[1:10]
province <- birthplace |> 
  dplyr::select(id1, ename1) |> 
  distinct()
master <- biographical_data18th_20thPC[,-c(28:44, 48:60)]
master$top_ba <- as.numeric(master$bauniv %in% top_unis)
master$top_uni <- as.numeric(master$bauniv %in% top_unis | master$mauniv %in% top_unis | master$phduniv %in% top_unis)

#Career Track and Width
career_var <- c("cname", "ename", "byear", "pyear", "bpro", "edu", 
                "bauniv", "bamajor", "mauniv", "mamajor", "phduniv", "phdmajor",
                "cc18", "cc19", "cc20", "lastparty", "lastSOE", "lastsc",
                "lastpro", "lastCPPCC", "lastNPC", "lastuniv", "lastpla",
                "lastmo", "nparty", "nSOE", "nsc", "npro", "nCPPCC",
                "nNPC", "nuniv", "npla", "nmo")
fcc20_career <- career(fcc20, 2022)[, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
fcc20_career$narrow <- as.numeric(fcc20_career$width < 2)
fcc20_career$narrow_alt <- as.numeric(fcc20_career$width <= 1.3)

acc20_career <- career(acc20, 2022)[, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
acc20_career$narrow <- as.numeric(acc20_career$width < 2)
acc20_career$narrow_alt <- as.numeric(acc20_career$width <= 1.3)

fcc19_career <- career(fcc19, 2017)[, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
fcc19_career$narrow <- as.numeric(fcc19_career$width < 2)
fcc19_career$narrow_alt <- as.numeric(fcc19_career$width <= 1.3)

acc19_career <- career(acc19, 2017)[, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
acc19_career$narrow <- as.numeric(acc19_career$width < 2)
acc19_career$narrow_alt <- as.numeric(acc19_career$width <= 1.3)

fcc18_career <- career(fcc18, 2012)[, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
fcc18_career$narrow <- as.numeric(fcc18_career$width < 2)
fcc18_career$narrow_alt <- as.numeric(fcc18_career$width <= 1.3)

acc18_career <- career(acc18, 2012)[, career_var] |> 
  mutate(width = nparty + nsc + npro + nCPPCC + nNPC + npla + 
           0.3 * (as.numeric(nSOE > 0) + as.numeric(nuniv > 0) + as.numeric(nmo > 0)))
acc18_career$narrow <- as.numeric(acc18_career$width < 2)
acc18_career$narrow_alt <- as.numeric(acc18_career$width <= 1.3)


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

# Correction
fcc20[c(10, 58, 62), c("job30s", "job30e", "job31", "job31s", "job31e", "job32")] <- NA

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
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        legend.position = "bottom")
  
ggplot() +
  geom_line(aes(x = session, y = narrow, group = 1, linetype = "Full"), data = fcc_stats) + 
  geom_point(aes(x = session, y = narrow, linetype = "Full"), data = fcc_stats) +
  geom_line(aes(x = session, y = narrow, group = 1, linetype = "Alternate"), data = acc_stats) +
  geom_point(aes(x = session, y = narrow, linetype = "Alternate"), data = acc_stats) +
  scale_linetype_manual(values = c("Full" = 1, "Alternate" = 2)) +
  labs(x = "Party Congress Session", y = "Percentage of Members with Narrow Career Paths", linetype = "Member", 
       title = "Narrow Career Paths among CCP Central Committee Members (2012-2022)") +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        legend.position = "bottom")

ggplot() +
  geom_line(aes(x = session, y = narrow_alt, group = 1, linetype = "Full"), data = fcc_stats) + 
  geom_point(aes(x = session, y = narrow_alt, linetype = "Full"), data = fcc_stats) +
  geom_line(aes(x = session, y = narrow_alt, group = 1, linetype = "Alternate"), data = acc_stats) +
  geom_point(aes(x = session, y = narrow_alt, linetype = "Alternate"), data = acc_stats) +
  scale_linetype_manual(values = c("Full" = 1, "Alternate" = 2)) +
  labs(x = "Party Congress Session", y = "Percentage of Members with Narrow Career Paths", linetype = "Member", 
       title = "Narrow Career Paths among CCP Central Committee Members (2012-2022)") +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        legend.position = "bottom")

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
  labs(x = "Position", "Percentage of Members", fill = "Session",
       title = "Position Held by CCP Central Committee Members (2012-2022)") +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        legend.position = "bottom")

ggplot(acc_stats_t)+
  geom_bar(aes(x = type, y = percentage, fill = session), stat = "identity", position = "dodge") +
  labs(x = "Position", "Percentage of Members", fill = "Session",
       title = "Position Held by CCP Central Committee Members (2012-2022)") +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        legend.position = "bottom")

write_xlsx(stats, "/Users/ziyuhe/Documents/GitHub/chinese-elite-politics/dataset/summary_stats.xlsx")
