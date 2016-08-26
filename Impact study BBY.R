##############################################################################
#### This program is to identify if compliance variables impact the sales ####
#### in Best Buy.                                                         ####
##############################################################################

# Set file path
getwd()
setwd("C:/Users/zlin/Desktop/Merch Impact/Compliance Data/AW")

# Load package
require(dplyr)

Merch_Impact_Test_BBY = function(input) {
  if (!file.exists(input)) {
    stop()
  } else {
    file = input
  }
 
  # Load data file
  Merch_Impact_raw = read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  # Merch_Impact_raw = read.csv("Merch Impact for Chromecast.csv", header = TRUE, stringsAsFactors = FALSE)
  Merch_Impact_raw$Fixt_delta = as.numeric(Merch_Impact_raw$Fixt_delta)
  
  # Add one more column for Fixture type index (Extended Endcap = 1, Legacy Endcap = 0)
  Merch_Impact = mutate(Merch_Impact_raw, Fixture_ind = ifelse(Merch_Impact_raw$Fixture.Type == "Legacy Endcap", 0, 1))
  Merch_Impact$Fixture_ind = as.factor(Merch_Impact$Fixture_ind)
  
  # Convert compliance variable into categorical variable for Chi-square test.
  col_num = c(match("Loc_delta", names(Merch_Impact)),match("Demo_delta", names(Merch_Impact)),match("Msg_delta", names(Merch_Impact)),
              match("OSA_delta", names(Merch_Impact)))
  for (i in col_num) {
    Merch_Impact[,i] = ifelse(Merch_Impact[,i] <= 0, "no", "yes")
  }
  

  if (file == "Merch Impact for Chromecast.csv") {
    temp = Merch_Impact[Merch_Impact$Ave_sale < 100,]
    Merch_Impact = mutate(Merch_Impact, salelvl = ifelse(Merch_Impact$Ave_sale <= quantile(temp$Ave_sale,1/3), "Low",
                          ifelse(Merch_Impact$Ave_sale > quantile(temp$Ave_sale,1/3) &
                          Merch_Impact$Ave_sale <= quantile(temp$Ave_sale,2/3), "Med", 
                          ifelse(Merch_Impact$Ave_sale > quantile(temp$Ave_sale,2/3) &
                          Merch_Impact$Ave_sale <= quantile(temp$Ave_sale,1), "High", "Outlier"))))
    
    # Chi-Square test and T-Test
    sha_result = shapiro.test(Merch_Impact$Ave_sale)
    
    # OSA for Chromecast
    CC_TAB_OSA = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$OSA_delta[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (CC_TAB_OSA <= 5)) {
      CC_ind_OSA = fisher.test(CC_TAB_OSA)
    } else {
      CC_ind_OSA = chisq.test(CC_TAB_OSA)  
    }
    
    if (sha_result$p.value < 0.05) {
      CC_test_OSA = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$OSA_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$OSA_delta == "no"], conf.int = TRUE)
    } else {
      CC_test_OSA = t.test(Merch_Impact$Ave_sale[Merch_Impact$OSA_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$OSA_delta == "no"])
    }
    
    # Demo for Chromecast
    CC_TAB_DEMO = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$Demo_delta[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (CC_TAB_DEMO <= 5)) {
      CC_ind_DEMO = fisher.test(CC_TAB_DEMO)
    } else {
      CC_ind_DEMO = chisq.test(CC_TAB_DEMO)  
    }

    if (sha_result$p.value < 0.05) {
      CC_test_DEMO = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$Demo_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Demo_delta == "no"], conf.int = TRUE)
    } else {
      CC_test_DEMO = t.test(Merch_Impact$Ave_sale[Merch_Impact$Demo_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Demo_delta == "no"])
    }
    
    # Msg for Chromecast
    CC_TAB_MSG = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$Msg_delta[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (CC_TAB_MSG <= 5)) {
      CC_ind_MSG = fisher.test(CC_TAB_MSG)
    } else {
      CC_ind_MSG = chisq.test(CC_TAB_MSG)  
    }
    
    if (sha_result$p.value < 0.05) {
      CC_test_MSG = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "no"], conf.int = TRUE)
    } else {
      CC_test_MSG = t.test(Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "no"])
    }
    
    # Loc for Chromecast
    CC_TAB_LOC = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$Loc_delta[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (CC_TAB_LOC <= 5)) {
      CC_ind_LOC = fisher.test(CC_TAB_LOC)
    } else {
      CC_ind_LOC = chisq.test(CC_TAB_LOC)  
    }
    
    if (sha_result$p.value < 0.05) {
      CC_test_LOC = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "no"], conf.int = TRUE)
    } else {
      CC_test_LOC = t.test(Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "no"])
    }
    
    # Fixture Type for Chromecast
    CC_TAB_FixT = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$Fixture.Type[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (CC_TAB_FixT <= 5)) {
      CC_ind_FixT = fisher.test(CC_TAB_FixT)
    } else {
      CC_ind_FixT = chisq.test(CC_TAB_FixT)  
    }
    
    if (sha_result$p.value < 0.05) {
      CC_test_FixT = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$Fixture.Type == "Extended Endcap"],
                                 Merch_Impact$Ave_sale[Merch_Impact$Fixture.Type == "Legacy Endcap"], conf.int = TRUE)
    } else {
      CC_test_FixT = t.test(Merch_Impact$Ave_sale[Merch_Impact$Fixture.Type == "Extended Endcap"],
                            Merch_Impact$Ave_sale[Merch_Impact$Fixture.Type == "Legacy Endcap"])
    }
    
    result = list(Chromecast_OSA_table = CC_TAB_OSA,Chromecast_OSA_Chisq_result = CC_ind_OSA,Chromecast_OSA_result = CC_test_OSA,
                  Chromecast_MSG_table = CC_TAB_MSG,Chromecast_MSG_Chisq_result = CC_ind_MSG,Chromecast_MSG_result = CC_test_MSG,
                  Chromecast_DEMO_table = CC_TAB_DEMO,Chromecast_DEMO_Chisq_result = CC_ind_DEMO,Chromecast_DEMO_result = CC_test_DEMO,
                  Chromecast_LOC_table = CC_TAB_LOC,Chromecast_LOC_Chisq_result = CC_ind_LOC,Chromecast_LOC_result = CC_test_LOC,
                  Chromecast_FixT_table = CC_TAB_FixT,Chromecast_FixT_Chisq_result = CC_ind_FixT,Chromecast_FixT_result = CC_test_FixT)
    
  } else if (file == "Merch Impact for Chromebook.csv") {
    temp = Merch_Impact[Merch_Impact$Ave_sale < 12.5,]
    Merch_Impact = mutate(Merch_Impact, salelvl = ifelse(Merch_Impact$Ave_sale <= quantile(temp$Ave_sale,1/3), "Low",
                          ifelse(Merch_Impact$Ave_sale > quantile(temp$Ave_sale,1/3) &
                          Merch_Impact$Ave_sale <= quantile(temp$Ave_sale,2/3),"Med", 
                          ifelse(Merch_Impact$Ave_sale > quantile(temp$Ave_sale,2/3) &
                          Merch_Impact$Ave_sale <= quantile(temp$Ave_sale,1), "High", "Outlier"))))
    
    sha_result = shapiro.test(Merch_Impact$Ave_sale)
    # Demo for Chromebook
    CB_TAB_DEMO = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$Demo_delta[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (CB_TAB_DEMO <= 5)) {
      CB_ind_DEMO = fisher.test(CB_TAB_DEMO)
    } else {
      CB_ind_DEMO = chisq.test(CB_TAB_DEMO)  
    }
    
    if (sha_result$p.value < 0.05) {
      CB_test_DEMO = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$Demo_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Demo_delta == "no"], conf.int = TRUE)
    } else {
      CB_test_DEMO = t.test(Merch_Impact$Ave_sale[Merch_Impact$Demo_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Demo_delta == "no"])
    }
    
    # Msg for Chromebook
    CB_TAB_MSG = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$Msg_delta[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (CB_TAB_MSG <= 5)) {
      CB_ind_MSG = fisher.test(CB_TAB_MSG)
    } else {
      CB_ind_MSG = chisq.test(CB_TAB_MSG)  
    }
    
    if (sha_result$p.value < 0.05) {
      CB_test_MSG = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "no"], conf.int = TRUE)
    } else {
      CB_test_MSG = t.test(Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "no"])
    }
    
    # Loc for Chromebook
    CB_TAB_LOC = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$Loc_delta[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (CB_TAB_LOC <= 5)) {
      CB_ind_LOC = fisher.test(CB_TAB_LOC)
    } else {
      CB_ind_LOC = chisq.test(CB_TAB_LOC)  
    }
    
    if (sha_result$p.value < 0.05) {
      CB_test_LOC = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "no"], conf.int = TRUE)
    } else {
      CB_test_LOC = t.test(Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "no"])
    }
    
    # Fixture Type for Chromebook
    CB_TAB_FixT = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$Fixture.Type[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (CB_TAB_FixT <= 5)) {
      CB_ind_FixT = fisher.test(CB_TAB_FixT)
    } else {
      CB_ind_FixT = chisq.test(CB_TAB_FixT)  
    }
    
    if (sha_result$p.value < 0.05) {
      CB_test_FixT = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$Fixture.Type == "Extended Endcap"],
                                 Merch_Impact$Ave_sale[Merch_Impact$Fixture.Type == "Legacy Endcap"], conf.int = TRUE)
    } else {
      CB_test_FixT = t.test(Merch_Impact$Ave_sale[Merch_Impact$Fixture.Type == "Extended Endcap"],
                            Merch_Impact$Ave_sale[Merch_Impact$Fixture.Type == "Legacy Endcap"])
    }
    
    result = list(Chromebook_MSG_table = CB_TAB_MSG,Chromebook_MSG_Chisq_result = CB_ind_MSG,Chromebook_MSG_result = CB_test_MSG,
                  Chromebook_DEMO_table = CB_TAB_DEMO,Chromebook_DEMO_Chisq_result = CB_ind_DEMO,Chromebook_DEMO_result = CB_test_DEMO,
                  Chromebook_LOC_table = CB_TAB_LOC,Chromebook_LOC_Chisq_result = CB_ind_LOC,Chromebook_LOC_result = CB_test_LOC,
                  Chromebook_FixT_table = CB_TAB_FixT,Chromebook_FixT_Chisq_result = CB_ind_FixT,Chromebook_FixT_result = CB_test_FixT)
  } else if (file == "Merch Impact for Chromecast Audio.csv") {
    temp = Merch_Impact
    Merch_Impact = mutate(Merch_Impact, salelvl = ifelse(temp$Ave_sale <= 1, "Low",
                          ifelse(temp$Ave_sale > 1 &
                          temp$Ave_sale <= 2,"Med", ifelse(temp$Ave_sale > 2 & temp$Ave_sale < 7, "High", "Outlier"))))
    
    sha_result = shapiro.test(Merch_Impact$Ave_sale)
    # OSA for Chromecast Audio
    CCA_TAB_OSA = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$OSA_delta[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (CCA_TAB_OSA <= 5)) {
      CCA_ind_OSA = fisher.test(CCA_TAB_OSA)
    } else {
      CCA_ind_OSA = chisq.test(CCA_TAB_OSA)  
    }
    
    if (sha_result$p.value < 0.05) {
      CCA_test_OSA = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$OSA_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$OSA_delta == "no"], conf.int = TRUE)
    } else {
      CCA_test_OSA = t.test(Merch_Impact$Ave_sale[Merch_Impact$OSA_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$OSA_delta == "no"])
    }
    
    # Msg for Chromecast Audio
    CCA_TAB_MSG = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$Msg_delta[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (CCA_TAB_MSG <= 5)) {
      CCA_ind_MSG = fisher.test(CCA_TAB_MSG)
    } else {
      CCA_ind_MSG = chisq.test(CCA_TAB_MSG)  
    }
    
    if (sha_result$p.value < 0.05) {
      CCA_test_MSG = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "no"], conf.int = TRUE)
    } else {
      CCA_test_MSG = t.test(Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "no"])
    }
    
    # Loc for Chromecast Audio
    CCA_TAB_LOC = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$Loc_delta[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (CCA_TAB_LOC <= 5)) {
      CCA_ind_LOC = fisher.test(CCA_TAB_LOC)
    } else {
      CCA_ind_LOC = chisq.test(CCA_TAB_LOC)  
    }
    
    if (sha_result$p.value < 0.05) {
      CCA_test_LOC = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "no"], conf.int = TRUE)
    } else {
      CCA_test_LOC = t.test(Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "no"])
    }
    
    # Fixture Type for Chromecast Audio
    CCA_TAB_FixT = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$Fixture.Type[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (CCA_TAB_FixT <= 5)) {
      CCA_ind_FixT = fisher.test(CCA_TAB_FixT)
    } else {
      CCA_ind_FixT = chisq.test(CCA_TAB_FixT)  
    }
    
    if (sha_result$p.value < 0.05) {
      CCA_test_FixT = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$Fixture.Type == "Extended Endcap"],
                                 Merch_Impact$Ave_sale[Merch_Impact$Fixture.Type == "Legacy Endcap"], conf.int = TRUE)
    } else {
      CCA_test_FixT = t.test(Merch_Impact$Ave_sale[Merch_Impact$Fixture.Type == "Extended Endcap"],
                            Merch_Impact$Ave_sale[Merch_Impact$Fixture.Type == "Legacy Endcap"])
    }
    
    result = list(ChromecastA_OSA_table = CCA_TAB_OSA,ChromecastA_OSA_Chisq_result = CCA_ind_OSA,ChromecastA_OSA_result = CCA_test_OSA,
                  ChromecastA_MSG_table = CCA_TAB_MSG,ChromecastA_MSG_Chisq_result = CCA_ind_MSG,ChromecastA_MSG_result = CCA_test_MSG,
                  ChromecastA_LOC_table = CCA_TAB_LOC,ChromecastA_LOC_Chisq_result = CCA_ind_LOC,ChromecastA_LOC_result = CCA_test_LOC,
                  ChromecastA_FixT_table = CCA_TAB_FixT,ChromecastA_FixT_Chisq_result = CCA_ind_FixT,ChromecastA_FixT_result = CCA_test_FixT)
  } else {
    temp = Merch_Impact[Merch_Impact$Ave_sale < 4,]
    Merch_Impact = mutate(Merch_Impact, salelvl = ifelse(Merch_Impact$Ave_sale <= quantile(temp$Ave_sale,1/3), "Low",
                                                         ifelse(Merch_Impact$Ave_sale > quantile(temp$Ave_sale,1/3) &
                                                                  Merch_Impact$Ave_sale <= quantile(temp$Ave_sale,2/3), "Med", 
                                                                ifelse(Merch_Impact$Ave_sale > quantile(temp$Ave_sale,2/3) &
                                                                         Merch_Impact$Ave_sale <= quantile(temp$Ave_sale,1), "High", "Outlier"))))
    
    # Chi-Square test and T-Test
    sha_result = shapiro.test(Merch_Impact$Ave_sale)
    
    # OSA for AW
    AW_TAB_OSA = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$OSA_delta[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (AW_TAB_OSA <= 5)) {
      AW_ind_OSA = fisher.test(AW_TAB_OSA)
    } else {
      AW_ind_OSA = chisq.test(AW_TAB_OSA)  
    }
    
    if (sha_result$p.value < 0.05) {
      AW_test_OSA = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$OSA_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$OSA_delta == "no"], conf.int = TRUE)
    } else {
      AW_test_OSA = t.test(Merch_Impact$Ave_sale[Merch_Impact$OSA_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$OSA_delta == "no"])
    }
    
    # Demo for AW
    AW_TAB_DEMO = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$Demo_delta[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (AW_TAB_DEMO <= 5)) {
      AW_ind_DEMO = fisher.test(AW_TAB_DEMO)
    } else {
      AW_ind_DEMO = chisq.test(AW_TAB_DEMO)  
    }
    
    if (sha_result$p.value < 0.05) {
      AW_test_DEMO = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$Demo_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Demo_delta == "no"], conf.int = TRUE)
    } else {
      AW_test_DEMO = t.test(Merch_Impact$Ave_sale[Merch_Impact$Demo_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Demo_delta == "no"])
    }
    
    # Msg for AW
    AW_TAB_MSG = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$Msg_delta[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (AW_TAB_MSG <= 5)) {
      AW_ind_MSG = fisher.test(AW_TAB_MSG)
    } else {
      AW_ind_MSG = chisq.test(AW_TAB_MSG)  
    }
    
    if (sha_result$p.value < 0.05) {
      AW_test_MSG = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "no"], conf.int = TRUE)
    } else {
      AW_test_MSG = t.test(Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Msg_delta == "no"])
    }
    
    # Loc for AW
    AW_TAB_LOC = table(Merch_Impact$salelvl[Merch_Impact$salelvl != "Outlier"],Merch_Impact$Loc_delta[Merch_Impact$salelvl != "Outlier"])
    if ("TRUE" %in% (AW_TAB_LOC <= 5)) {
      AW_ind_LOC = fisher.test(AW_TAB_LOC)
    } else {
      AW_ind_LOC = chisq.test(AW_TAB_LOC)  
    }
    
    if (sha_result$p.value < 0.05) {
      AW_test_LOC = wilcox.test(Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "no"], conf.int = TRUE)
    } else {
      AW_test_LOC = t.test(Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "yes"],Merch_Impact$Ave_sale[Merch_Impact$Loc_delta == "no"])
    }
    
    result = list(AW_OSA_table = AW_TAB_OSA,AW_OSA_Chisq_result = AW_ind_OSA,AW_OSA_result = AW_test_OSA,
                  AW_MSG_table = AW_TAB_MSG,AW_MSG_Chisq_result = AW_ind_MSG,AW_MSG_result = AW_test_MSG,
                  AW_DEMO_table = AW_TAB_DEMO,AW_DEMO_Chisq_result = AW_ind_DEMO,AW_DEMO_result = AW_test_DEMO,
                  AW_LOC_table = AW_TAB_LOC,AW_LOC_Chisq_result = AW_ind_LOC,AW_LOC_result = AW_test_LOC)
  }
  return(result)
}

Merch_Impact_Test_BBY("Merch Impact for AW.csv")

