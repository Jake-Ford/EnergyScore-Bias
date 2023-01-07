# write a function that returns the non-default (or default) rate for a specified group

profit_max_thresh = function(score, class_name, ndr_rate)
  
   
{
  if (score == "FICO") 
    {

    fico_range <- seq(450,850,1)
    
    ndr_df <- data.frame(FICO = fico_range)
    
    
      if (class_name == "Race")
              {
              
              for (i in 1:length(ndr_df$FICO))
              
                {
                
                fico_i = ndr_df$FICO[i]
                
                ndr_df$Total[i]<- prop.table(table(race_data$DPD90_KEYCD[(race_data$FICOCLV8_SCORE>=fico_i)]))[1]
                ndr_df$White[i] <- prop.table(table(race_data$DPD90_KEYCD[(race_data$FICOCLV8_SCORE>=fico_i) & (race_data$Race=="White")]))[1]
                ndr_df$Black[i] <- prop.table(table(race_data$DPD90_KEYCD[(race_data$FICOCLV8_SCORE>=fico_i) & (race_data$Race=="Black")]))[1]
                ndr_df$Hispanic[i] <- prop.table(table(race_data$DPD90_KEYCD[(race_data$FICOCLV8_SCORE>=fico_i) & (race_data$Race=="Hispanic")]))[1]
                ndr_df$Asian[i] <- prop.table(table(race_data$DPD90_KEYCD[(race_data$FICOCLV8_SCORE>=fico_i) & (race_data$Race=="Asian")]))[1]
                
              }
            
              total_val = ndr_df$FICO[which.min(abs(ndr_df$Total-ndr_rate))]
              white_val = ndr_df$FICO[which.min(abs(ndr_df$White-ndr_rate))]
              black_val = ndr_df$FICO[which.min(abs(ndr_df$Black-ndr_rate))]
              hispanic_val = ndr_df$FICO[which.min(abs(ndr_df$Hispanic-ndr_rate))]
              asian_val = ndr_df$FICO[which.min(abs(ndr_df$Asian-ndr_rate))]
              
              print(paste0("Total Value: ", total_val) ) 
              print(paste0("White Value: ", white_val) ) 
              print(paste0("Black Value: ", black_val) ) 
              print(paste0("Hispanic Value: ", hispanic_val) ) 
              print(paste0("Asian Value: ", asian_val) ) 
              
              df <- data.frame(Variables = c("Total", "White", "Black", "Hispanic", "Asian"),
                               Thresholds = c(total_val, white_val, black_val, hispanic_val, asian_val))
              return (df)
            }
    
      if (class_name == "Income")
      {
        
        for (i in 1:length(ndr_df$FICO))
          
        {
          
          fico_i = ndr_df$FICO[i]
          
          ndr_df$Total[i]<- prop.table(table(joined$DPD90_KEYCD[(joined$FICOCLV8_SCORE>=fico_i)]))[1]
          ndr_df$Low[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$FICOCLV8_SCORE>=fico_i) & (joined$Income=="Low")]))[1]
          ndr_df$Medium[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$FICOCLV8_SCORE>=fico_i) & (joined$Income=="Medium")]))[1]
          ndr_df$High[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$FICOCLV8_SCORE>=fico_i) & (joined$Income=="High")]))[1]

        }
        
        total_val = ndr_df$FICO[which.min(abs(ndr_df$Total-ndr_rate))]
        low_val = ndr_df$FICO[which.min(abs(ndr_df$Low-ndr_rate))]
        med_val = ndr_df$FICO[which.min(abs(ndr_df$Medium-ndr_rate))]
        hi_val = ndr_df$FICO[which.min(abs(ndr_df$High-ndr_rate))]

        print(paste0("Total Value: ", total_val) ) 
        print(paste0("Low Income Value: ", low_val) ) 
        print(paste0("Medium Income Value: ", med_val) ) 
        print(paste0("High Income Value: ", hi_val) ) 

        df <- data.frame(Variables = c("Total", "Low", "Medium", "High"),
                         Thresholds = c(total_val, low_val, med_val, hi_val))
        return (df)
      }
    
    if (class_name == "Education")
    {
      
      for (i in 1:length(ndr_df$FICO))
        
      {
        
        fico_i = ndr_df$FICO[i]
        
        ndr_df$VT[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$FICOCLV8_SCORE>=fico_i) & (joined$Education=="Vocational/Technical")]))[1]
        ndr_df$HS[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$FICOCLV8_SCORE>=fico_i) & (joined$Education=="High School")]))[1]
        ndr_df$College[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$FICOCLV8_SCORE>=fico_i) & (joined$Education=="College")]))[1]
        ndr_df$Graduate[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$FICOCLV8_SCORE>=fico_i) & (joined$Education=="Graduate School")]))[1]
        
      }
      
      vt_val = ndr_df$FICO[which.min(abs(ndr_df$VT-ndr_rate))]
      hs_val = ndr_df$FICO[which.min(abs(ndr_df$HS-ndr_rate))]
      college_val = ndr_df$FICO[which.min(abs(ndr_df$College-ndr_rate))]
      graduate_val = ndr_df$FICO[which.min(abs(ndr_df$Graduate-ndr_rate))]
      
      print(paste0("VT Value: ", vt_val) ) 
      print(paste0("High School Value: ", hs_val) ) 
      print(paste0("College Value: ", college_val) ) 
      print(paste0("Graduate Value: ", graduate_val) ) 
      
      df <- data.frame(Variables = c("Vocational/Technical", "High School", "College", "Graduate School"),
                       Thresholds = c(vt_val, hs_val, college_val, graduate_val))
      return (df)
    }
    
    if (class_name == "HomeOwnership")
    {
      
      for (i in 1:length(ndr_df$FICO))
        
      {
        
        fico_i = ndr_df$FICO[i]
        
        ndr_df$Total[i]<- prop.table(table(joined$DPD90_KEYCD[(joined$FICOCLV8_SCORE>=fico_i)]))[1]
        ndr_df$Rent[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$FICOCLV8_SCORE>=fico_i) & (joined$HomeOwnership=="Rent")]))[1]
        ndr_df$Own[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$FICOCLV8_SCORE>=fico_i) & (joined$HomeOwnership=="Own")]))[1]

      }
      
      total_val = ndr_df$FICO[which.min(abs(ndr_df$Total-ndr_rate))]
      rent_val = ndr_df$FICO[which.min(abs(ndr_df$Rent-ndr_rate))]
      own_val = ndr_df$FICO[which.min(abs(ndr_df$Own-ndr_rate))]

      print(paste0("Total Value: ", total_val) ) 
      print(paste0("Rent Value: ", rent_val) ) 
      print(paste0("Own Value: ", own_val) ) 

      df <- data.frame(Variables = c("Total", "Rent", "Own"),
                       Thresholds = c(total_val, rent_val, own_val))
      return (df)
    }
    
    }
    
    
    if (score == "EnergyScore") {
      
      es_range <- seq(0,100,1)

      ndr_df <- data.frame(EnergyScore = es_range)
      
      if (class_name == "Race")
        {
        
        for (i in 1:length(ndr_df$EnergyScore))
          
        {
          
          EnergyScore_i = ndr_df$EnergyScore[i]
          
          ndr_df$Total[i] <- prop.table(table(race_data$DPD90_KEYCD[(race_data$EnergyScore>=EnergyScore_i)]))[1]
          ndr_df$White[i] <- prop.table(table(race_data$DPD90_KEYCD[(race_data$EnergyScore>=EnergyScore_i) & (race_data$Race=="White")]))[1]
          ndr_df$Black[i] <- prop.table(table(race_data$DPD90_KEYCD[(race_data$EnergyScore>=EnergyScore_i) & (race_data$Race=="Black")]))[1]
          ndr_df$Hispanic[i] <- prop.table(table(race_data$DPD90_KEYCD[(race_data$EnergyScore>=EnergyScore_i) & (race_data$Race=="Hispanic")]))[1]
          ndr_df$Asian[i] <- prop.table(table(race_data$DPD90_KEYCD[(race_data$EnergyScore>=EnergyScore_i) & (race_data$Race=="Asian")]))[1]
          
        }
        total_val = ndr_df$EnergyScore[which.min(abs(ndr_df$Total-ndr_rate))]
        white_val = ndr_df$EnergyScore[which.min(abs(ndr_df$White-ndr_rate))]
        black_val = ndr_df$EnergyScore[which.min(abs(ndr_df$Black-ndr_rate))]
        hispanic_val = ndr_df$EnergyScore[which.min(abs(ndr_df$Hispanic-ndr_rate))]
        asian_val = ndr_df$EnergyScore[which.min(abs(ndr_df$Asian-ndr_rate))]
        
        print(paste0("Total Value: ", total_val) ) 
        print(paste0("White Value: ", white_val) ) 
        print(paste0("Black Value: ", black_val) ) 
        print(paste0("Hispanic Value: ", hispanic_val) ) 
        print(paste0("Asian Value: ", asian_val) )  
        
        df <- data.frame(Variables = c("Total", "White", "Black", "Hispanic", "Asian"),
                         Thresholds = c(total_val, white_val, black_val, hispanic_val, asian_val))
        return (df)
        
        
      }
      
      if (class_name == "Income")
      {
        
        for (i in 1:length(ndr_df$EnergyScore))
          
        {
          
          EnergyScore_i = ndr_df$EnergyScore[i]
          
          ndr_df$Total[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$EnergyScore>=EnergyScore_i)]))[1]
          ndr_df$Low[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$EnergyScore>=EnergyScore_i) & (joined$Income=="Low")]))[1]
          ndr_df$Medium[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$EnergyScore>=EnergyScore_i) & (joined$Income=="Medium")]))[1]
          ndr_df$High[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$EnergyScore>=EnergyScore_i) & (joined$Income=="High")]))[1]

        }
        total_val = ndr_df$EnergyScore[which.min(abs(ndr_df$Total-ndr_rate))]
        low_val = ndr_df$EnergyScore[which.min(abs(ndr_df$Low-ndr_rate))]
        med_val = ndr_df$EnergyScore[which.min(abs(ndr_df$Medium-ndr_rate))]
        hi_val = ndr_df$EnergyScore[which.min(abs(ndr_df$High-ndr_rate))]

        print(paste0("Total Value: ", total_val) ) 
        print(paste0("Low Income Value: ", low_val) ) 
        print(paste0("Medium Income Value: ", med_val) ) 
        print(paste0("High Income Value: ", hi_val) ) 

        df <- data.frame(Variables = c("Total", "Low", "Medium", "High"),
                         Thresholds = c(total_val, low_val, med_val, hi_val))
        return (df)
        
        
      }
      
      if (class_name == "Education")
      {
        
        for (i in 1:length(ndr_df$EnergyScore))
          
        {
          
          EnergyScore_i = ndr_df$EnergyScore[i]
          
          ndr_df$VT[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$EnergyScore>=EnergyScore_i) & (joined$Education=="Vocational/Technical")]))[1]
          ndr_df$HS[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$EnergyScore>=EnergyScore_i) & (joined$Education=="High School")]))[1]
          ndr_df$College[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$EnergyScore>=EnergyScore_i) & (joined$Education=="College")]))[1]
          ndr_df$Graduate[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$EnergyScore>=EnergyScore_i) & (joined$Education=="Graduate School")]))[1]
          
        }
        vt_val = ndr_df$EnergyScore[which.min(abs(ndr_df$VT-ndr_rate))]
        hs_val = ndr_df$EnergyScore[which.min(abs(ndr_df$HS-ndr_rate))]
        college_val = ndr_df$EnergyScore[which.min(abs(ndr_df$College-ndr_rate))]
        grad_val = ndr_df$EnergyScore[which.min(abs(ndr_df$Graduate-ndr_rate))]
        
        print(paste0("VT Value: ", vt_val) ) 
        print(paste0("High School Value: ", hs_val) ) 
        print(paste0("College Value: ", college_val) ) 
        print(paste0("Graduate Value: ", grad_val) ) 
        
        df <- data.frame(Variables = c("Vocational/Technical", "High School", "College", "Graduate School"),
                         Thresholds = c(vt_val, hs_val, college_val, grad_val))
        return (df)
        
        
      }
      
      if (class_name == "HomeOwnership")
      {
        
        for (i in 1:length(ndr_df$EnergyScore))
          
        {
          
          EnergyScore_i = ndr_df$EnergyScore[i]
          
          ndr_df$Total[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$EnergyScore>=EnergyScore_i)]))[1]
          ndr_df$Rent[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$EnergyScore>=EnergyScore_i) & (joined$HomeOwnership=="Rent")]))[1]
          ndr_df$Own[i] <- prop.table(table(joined$DPD90_KEYCD[(joined$EnergyScore>=EnergyScore_i) & (joined$HomeOwnership=="Own")]))[1]

        }
        total_val = ndr_df$EnergyScore[which.min(abs(ndr_df$Total-ndr_rate))]
        rent_val = ndr_df$EnergyScore[which.min(abs(ndr_df$Rent-ndr_rate))]
        own_val = ndr_df$EnergyScore[which.min(abs(ndr_df$Own-ndr_rate))]

        print(paste0("Total Value: ", total_val) ) 
        print(paste0("Rent Value: ", rent_val) ) 
        print(paste0("Own Value: ", own_val) ) 

        df <- data.frame(Variables = c("Total", "Rent", "Own"),
                         Thresholds = c(total_val, rent_val, own_val))
        return (df)
        
        
      }
      }
  
  
    
}



# Demographic parity

demo_parity_thresh <- function(class, class_name,percentage)
  {
    
    
    # idea of the function is to return for all races the FICO score for percentage P and race R
    fico = quantile(joined$FICOCLV8_SCORE[joined[,class]==class_name], percentage, na.rm=T)
    es = quantile(joined$EnergyScore[joined[,class]==class_name], percentage, na.rm=T)
    
    
    # print(paste0("FICO Cutoff: ", fico) ) 
    # print(paste0("EnergyScore Cutoff: ", es) ) 
  
    df <- data.frame(Thresholds = c(fico, es))
    return (df)

  }





# Equal Opportunity parity 


equ_op_thresh = function(score, class_name, eq_op_rate)
{


{
  if (score == "FICO") 
    {
    
    
    
        fico_range <- seq(450,850,1)
        
        tp_df <- data.frame(FICO = fico_range)
        
        
        if (class_name == "Race")
        {
          just_positives <- joined %>%
            filter(DPD90_KEYCD==0) %>%
            filter(!is.na(Race))
          
          for (i in 1:length(tp_df$FICO))
            
          {
            
            fico_i = tp_df$FICO[i]
            
            tp_df$Total[i] <- sum(just_positives$FICOCLV8_SCORE > tp_df$FICO[i])/nrow(just_positives) *100
            tp_df$Asian[i] <- sum(just_positives$FICOCLV8_SCORE[just_positives$Race=="Asian"] > tp_df$FICO[i])/ sum(just_positives$Race=="Asian")*100
            tp_df$Black[i] <- sum(just_positives$FICOCLV8_SCORE[just_positives$Race=="Black"] > tp_df$FICO[i])/ sum(just_positives$Race=="Black")*100
            tp_df$White[i] <- sum(just_positives$FICOCLV8_SCORE[just_positives$Race=="White"] > tp_df$FICO[i])/ sum(just_positives$Race=="White")*100
            tp_df$Hispanic[i] <- sum(just_positives$FICOCLV8_SCORE[just_positives$Race=="Hispanic"] > tp_df$FICO[i])/ sum(just_positives$Race=="Hispanic")*100
    
          }
          
          total_val = tp_df$FICO[which.min(abs(tp_df$Total-eq_op_rate))]
          white_val = tp_df$FICO[which.min(abs(tp_df$White-eq_op_rate))]
          black_val = tp_df$FICO[which.min(abs(tp_df$Black-eq_op_rate))]
          hispanic_val = tp_df$FICO[which.min(abs(tp_df$Hispanic-eq_op_rate))]
          asian_val = tp_df$FICO[which.min(abs(tp_df$Asian-eq_op_rate))]
          
          print(paste0("Total Value: ", total_val) ) 
          print(paste0("White Value: ", white_val) ) 
          print(paste0("Black Value: ", black_val) ) 
          print(paste0("Hispanic Value: ", hispanic_val) ) 
          print(paste0("Asian Value: ", asian_val) ) 
          
          df <- data.frame(Variables = c("Total", "White", "Black", "Hispanic", "Asian"),
                           Thresholds = c(total_val, white_val, black_val, hispanic_val, asian_val))
          return (df)
        }
    
      if (class_name == "Income")
      {
        just_positives <- joined %>%
          filter(DPD90_KEYCD==0) %>%
          filter(!is.na(Income))
        
        for (i in 1:length(tp_df$FICO))
          
        {
          
          fico_i = tp_df$FICO[i]
          
          tp_df$Total[i] <- sum(just_positives$FICOCLV8_SCORE > tp_df$FICO[i])/nrow(just_positives) *100
          tp_df$Low[i] <- sum(just_positives$FICOCLV8_SCORE[just_positives$Income=="Low"] > tp_df$FICO[i])/ sum(just_positives$Income=="Low")*100
          tp_df$Medium[i] <- sum(just_positives$FICOCLV8_SCORE[just_positives$Income=="Medium"] > tp_df$FICO[i])/ sum(just_positives$Income=="Medium")*100
          tp_df$High[i] <- sum(just_positives$FICOCLV8_SCORE[just_positives$Income=="High"] > tp_df$FICO[i])/ sum(just_positives$Income=="High")*100
  
        }
        
        total_val = tp_df$FICO[which.min(abs(tp_df$Total-eq_op_rate))]
        low_val = tp_df$FICO[which.min(abs(tp_df$Low-eq_op_rate))]
        med_val = tp_df$FICO[which.min(abs(tp_df$Medium-eq_op_rate))]
        hi_val = tp_df$FICO[which.min(abs(tp_df$High-eq_op_rate))]
  
        print(paste0("Total Value: ", total_val) ) 
        print(paste0("Low Value: ", low_val) ) 
        print(paste0("Medium Value: ", med_val) ) 
        print(paste0("High Value: ", hi_val) ) 
  
        df <- data.frame(Variables = c("Total", "Low", "Medium", "High"),
                         Thresholds = c(total_val, low_val, med_val, hi_val))
        return (df)
      }
        
        
        
        if (class_name == "Education"){
          
          just_positives <- joined %>%
            filter(!is.na(Education))
          
          
          for (i in 1:length(tp_df$FICO))
            
          {
            
            EnergyScore_i = tp_df$FICO[i]
            
            tp_df$Total[i] <- sum((just_positives$FICOCLV8_SCORE > tp_df$FICO[i]) & just_positives$DPD90_KEYCD==0 )/sum(just_positives$DPD90_KEYCD==0)* 100
            
            tp_df$VT[i] <- sum((just_positives$FICOCLV8_SCORE > tp_df$FICO[i]) & (just_positives$DPD90_KEYCD==0) &
                                 (just_positives$Education=="Vocational/Technical"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$Education=="Vocational/Technical")* 100
            
            tp_df$HS[i] <- sum((just_positives$FICOCLV8_SCORE > tp_df$FICO[i]) & (just_positives$DPD90_KEYCD==0) &
                                 (just_positives$Education=="High School"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$Education=="High School")* 100
            
            tp_df$College[i] <- sum((just_positives$FICOCLV8_SCORE > tp_df$FICO[i]) & (just_positives$DPD90_KEYCD==0) &
                                      (just_positives$Education=="College"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$Education=="College")* 100
            
            tp_df$Grad[i] <- sum((just_positives$FICOCLV8_SCORE > tp_df$FICO[i]) & (just_positives$DPD90_KEYCD==0) &
                                   (just_positives$Education=="Graduate School"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$Education=="Graduate School")* 100        
            
            
          }
          total_val = tp_df$FICO[which.min(abs(tp_df$Total-eq_op_rate))]
          vt_val = tp_df$FICO[which.min(abs(tp_df$VT-eq_op_rate))]
          hs_val = tp_df$FICO[which.min(abs(tp_df$HS-eq_op_rate))]
          college_val = tp_df$FICO[which.min(abs(tp_df$College-eq_op_rate))]
          grad_val = tp_df$FICO[which.min(abs(tp_df$Grad-eq_op_rate))]
          
          print(paste0("Total Value: ", total_val) ) 
          print(paste0("Vocational/Technical Value: ", vt_val) ) 
          print(paste0("High School Value: ", hs_val) ) 
          print(paste0("College Value: ", college_val) )  
          print(paste0("Graduate Value: ", grad_val) )  
          
          
          df <- data.frame(Variables = c("Total", "Vocational/Technical", "High School", "College", "Graduate School"),
                           Thresholds = c(total_val, vt_val, hs_val, college_val, grad_val))
          return (df)
          
          
        } 
        
        
        if (class_name == "HomeOwnership"){
          
          
          just_positives <- joined %>%
            filter(!is.na(HomeOwnership))
          
          for (i in 1:length(tp_df$FICO))
            
          {
            
            EnergyScore_i = tp_df$FICO[i]
            
            tp_df$Total[i] <- sum((just_positives$FICOCLV8_SCORE > tp_df$FICO[i]) & just_positives$DPD90_KEYCD==0 )/sum(just_positives$DPD90_KEYCD==0)* 100
            
            tp_df$Rent[i] <- sum((just_positives$FICOCLV8_SCORE > tp_df$FICO[i]) & (just_positives$DPD90_KEYCD==0) &
                                   (just_positives$HomeOwnership=="Rent"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$HomeOwnership=="Rent")* 100
            
            tp_df$Own[i] <- sum((just_positives$FICOCLV8_SCORE > tp_df$FICO[i]) & (just_positives$DPD90_KEYCD==0) &
                                  (just_positives$HomeOwnership=="Own"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$HomeOwnership=="Own")* 100
            
            
            
            
            
          }
          total_val = tp_df$FICO[which.min(abs(tp_df$Total-eq_op_rate))]
          rent_val = tp_df$FICO[which.min(abs(tp_df$Rent-eq_op_rate))]
          own_val = tp_df$FICO[which.min(abs(tp_df$Own-eq_op_rate))]
          
          print(paste0("Total Value: ", total_val) ) 
          print(paste0("Rent Value: ", rent_val) ) 
          print(paste0("Own Value: ", own_val) ) 
          
          df <- data.frame(Variables = c("Total", "Rent", "Own" ),
                           Thresholds = c(total_val, rent_val, own_val))
          return (df)
          
          
        }
    
    
    
    }
  
  
  if (score == "EnergyScore") {
    
    es_range <- seq(0,100,1)
    
    tp_df <- data.frame(EnergyScore = es_range)
    
    if (class_name == "Race"){
      
      for (i in 1:length(tp_df$EnergyScore))
        
      {
        
        EnergyScore_i = tp_df$EnergyScore[i]
        
        tp_df$Total[i] <- sum((race_data$EnergyScore > tp_df$EnergyScore[i]) & race_data$DPD90_KEYCD==0 )/sum(race_data$DPD90_KEYCD==0)* 100
        
        tp_df$White[i] <- sum((race_data$EnergyScore > tp_df$EnergyScore[i]) & (race_data$DPD90_KEYCD==0) &
                                (race_data$Race=="White"))/sum(race_data$DPD90_KEYCD==0 & race_data$Race=="White")* 100
        
        tp_df$White[i] <- sum((race_data$EnergyScore > tp_df$EnergyScore[i]) & (race_data$DPD90_KEYCD==0) &
                                (race_data$Race=="White"))/sum(race_data$DPD90_KEYCD==0 & race_data$Race=="White")* 100
        
        tp_df$Black[i] <- sum((race_data$EnergyScore > tp_df$EnergyScore[i]) & (race_data$DPD90_KEYCD==0) &
                                (race_data$Race=="Black"))/sum(race_data$DPD90_KEYCD==0 & race_data$Race=="Black")* 100
        
        tp_df$Asian[i] <- sum((race_data$EnergyScore > tp_df$EnergyScore[i]) & (race_data$DPD90_KEYCD==0) &
                                (race_data$Race=="Asian"))/sum(race_data$DPD90_KEYCD==0 & race_data$Race=="Asian")* 100
        
        tp_df$Hispanic[i] <- sum((race_data$EnergyScore > tp_df$EnergyScore[i]) & (race_data$DPD90_KEYCD==0) &
                                   (race_data$Race=="Hispanic"))/sum(race_data$DPD90_KEYCD==0 & race_data$Race=="Hispanic")* 100
      }
      total_val = tp_df$EnergyScore[which.min(abs(tp_df$Total-eq_op_rate))]
      white_val = tp_df$EnergyScore[which.min(abs(tp_df$White-eq_op_rate))]
      black_val = tp_df$EnergyScore[which.min(abs(tp_df$Black-eq_op_rate))]
      hispanic_val = tp_df$EnergyScore[which.min(abs(tp_df$Hispanic-eq_op_rate))]
      asian_val = tp_df$EnergyScore[which.min(abs(tp_df$Asian-eq_op_rate))]
      
      print(paste0("Total Value: ", total_val) ) 
      print(paste0("White Value: ", white_val) ) 
      print(paste0("Black Value: ", black_val) ) 
      print(paste0("Hispanic Value: ", hispanic_val) ) 
      print(paste0("Asian Value: ", asian_val) )  
      
      df <- data.frame(Variables = c("Total", "White", "Black", "Hispanic", "Asian"),
                       Thresholds = c(total_val, white_val, black_val, hispanic_val, asian_val))
      return (df)
      
      
    } 
    
    if (class_name == "Income"){
      
      just_positives <- joined %>%
        filter(!is.na(Income))
      
      for (i in 1:length(tp_df$EnergyScore))
        
      {
        
        EnergyScore_i = tp_df$EnergyScore[i]
        
        tp_df$Total[i] <- sum((just_positives$EnergyScore > tp_df$EnergyScore[i]) & just_positives$DPD90_KEYCD==0 )/sum(just_positives$DPD90_KEYCD==0)* 100
        
        tp_df$Low[i] <- sum((just_positives$EnergyScore > tp_df$EnergyScore[i]) & (just_positives$DPD90_KEYCD==0) &
                                (just_positives$Income=="Low"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$Income=="Low")* 100
        
        tp_df$Medium[i] <- sum((just_positives$EnergyScore > tp_df$EnergyScore[i]) & (just_positives$DPD90_KEYCD==0) &
                                 (just_positives$Income=="Medium"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$Income=="Medium")* 100
        
        tp_df$High[i] <- sum((just_positives$EnergyScore > tp_df$EnergyScore[i]) & (just_positives$DPD90_KEYCD==0) &
                                (just_positives$Income=="High"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$Income=="High")* 100

        

      }
      total_val = tp_df$EnergyScore[which.min(abs(tp_df$Total-eq_op_rate))]
      low_val = tp_df$EnergyScore[which.min(abs(tp_df$Low-eq_op_rate))]
      medium_val = tp_df$EnergyScore[which.min(abs(tp_df$High-eq_op_rate))]
      high_val = tp_df$EnergyScore[which.min(abs(tp_df$Medium-eq_op_rate))]
      
      print(paste0("Total Value: ", total_val) ) 
      print(paste0("Low Value: ", low_val) ) 
      print(paste0("Medium Value: ", medium_val) ) 
      print(paste0("High Value: ", high_val) )  
      
      df <- data.frame(Variables = c("Total", "Low", "Medium", "High"),
                       Thresholds = c(total_val, low_val, medium_val, high_val))
      return (df)
      
      
    } 
    
    if (class_name == "Education"){
      
      just_positives <- joined %>%
        filter(!is.na(Education))
      
      
      for (i in 1:length(tp_df$EnergyScore))
        
      {
        
        EnergyScore_i = tp_df$EnergyScore[i]
        
        tp_df$Total[i] <- sum((just_positives$EnergyScore > tp_df$EnergyScore[i]) & just_positives$DPD90_KEYCD==0 )/sum(just_positives$DPD90_KEYCD==0)* 100
        
        tp_df$VT[i] <- sum((just_positives$EnergyScore > tp_df$EnergyScore[i]) & (just_positives$DPD90_KEYCD==0) &
                              (just_positives$Education=="Vocational/Technical"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$Education=="Vocational/Technical")* 100
        
        tp_df$HS[i] <- sum((just_positives$EnergyScore > tp_df$EnergyScore[i]) & (just_positives$DPD90_KEYCD==0) &
                                 (just_positives$Education=="High School"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$Education=="High School")* 100
        
        tp_df$College[i] <- sum((just_positives$EnergyScore > tp_df$EnergyScore[i]) & (just_positives$DPD90_KEYCD==0) &
                               (just_positives$Education=="College"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$Education=="College")* 100

        tp_df$Grad[i] <- sum((just_positives$EnergyScore > tp_df$EnergyScore[i]) & (just_positives$DPD90_KEYCD==0) &
                                  (just_positives$Education=="Graduate School"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$Education=="Graduate School")* 100        
        
        
      }
      total_val = tp_df$EnergyScore[which.min(abs(tp_df$Total-eq_op_rate))]
      vt_val = tp_df$EnergyScore[which.min(abs(tp_df$VT-eq_op_rate))]
      hs_val = tp_df$EnergyScore[which.min(abs(tp_df$HS-eq_op_rate))]
      college_val = tp_df$EnergyScore[which.min(abs(tp_df$College-eq_op_rate))]
      grad_val = tp_df$EnergyScore[which.min(abs(tp_df$Grad-eq_op_rate))]
      
      print(paste0("Total Value: ", total_val) ) 
      print(paste0("Vocational/Technical Value: ", vt_val) ) 
      print(paste0("High School Value: ", hs_val) ) 
      print(paste0("College Value: ", college_val) )  
      print(paste0("Graduate Value: ", grad_val) )  
      
      
      df <- data.frame(Variables = c("Total", "Vocational/Technical", "High School", "College", "Graduate School"),
                       Thresholds = c(total_val, vt_val, hs_val, college_val, grad_val))
      return (df)
      
      
    } 
    
    
    if (class_name == "HomeOwnership"){
      
      
      just_positives <- joined %>%
        filter(!is.na(HomeOwnership))
      
      for (i in 1:length(tp_df$EnergyScore))
        
      {
        
        EnergyScore_i = tp_df$EnergyScore[i]
        
        tp_df$Total[i] <- sum((just_positives$EnergyScore > tp_df$EnergyScore[i]) & just_positives$DPD90_KEYCD==0 )/sum(just_positives$DPD90_KEYCD==0)* 100
        
        tp_df$Rent[i] <- sum((just_positives$EnergyScore > tp_df$EnergyScore[i]) & (just_positives$DPD90_KEYCD==0) &
                              (just_positives$HomeOwnership=="Rent"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$HomeOwnership=="Rent")* 100
        
        tp_df$Own[i] <- sum((just_positives$EnergyScore > tp_df$EnergyScore[i]) & (just_positives$DPD90_KEYCD==0) &
                                 (just_positives$HomeOwnership=="Own"))/sum(just_positives$DPD90_KEYCD==0 & just_positives$HomeOwnership=="Own")* 100
        

        
        
        
      }
      total_val = tp_df$EnergyScore[which.min(abs(tp_df$Total-eq_op_rate))]
      rent_val = tp_df$EnergyScore[which.min(abs(tp_df$Rent-eq_op_rate))]
      own_val = tp_df$EnergyScore[which.min(abs(tp_df$Own-eq_op_rate))]

      print(paste0("Total Value: ", total_val) ) 
      print(paste0("Rent Value: ", rent_val) ) 
      print(paste0("Own Value: ", own_val) ) 

      df <- data.frame(Variables = c("Total", "Rent", "Own" ),
                       Thresholds = c(total_val, rent_val, own_val))
      return (df)
      
      
    }
    
    
    
    }
  
  
  
  }
}



get_p_fico <- function(RaceName, P)
{
  
  temp_df <- joined %>%
    filter(Race == RaceName)
  
  x <- temp_df$FICOCLV8_SCORE[temp_df$Race==RaceName]
  
  fun_ecdf <- (x)
  
  fun_ecdf(P)
}




get_p_es <- function(RaceName, P)
{
  
  temp_df <- joined %>%
    filter(Race == RaceName)
  
  x <- temp_df$EnergyScore
  
  fun_ecdf <- ecdf(x)
  
  fun_ecdf(P)
}



































# this should replace the above eventually, likely a double for loop
test_func = function(score, class_name)
  
{
  if (score=="FICO")
  {
    
    fico_range <- seq(450,850,1)
    
    ndr_df <- data.frame(FICO = fico_range)
    
    # idea is to print out the length of whatever class we put in there (race, edu, inc, hs)
    
    # Race
    
    if (class_name == "Race")
    {
      
      for (i in 1:length(ndr_df$FICO))
      {
        x = length(unique(na.omit(joined$Race)))
        fico_i = ndr_df$FICO[i]
        
        
        
      }
      
      
      
      
      i = length(unique(na.omit(joined$Race)))
      
      for (m in 1:i)
        
      {
        print(unique(na.omit(joined$Race))[m])
        
        
      }
    }
    
    
    
    # Income
    
    if (class_name == "Income")
    {
      i = length(unique(na.omit(joined$Income)))
      
      for (m in 1:i)
        
      {
        print(unique(na.omit(joined$Income))[m])
        
        
      }  }
    
    # Homeowner
    if (class_name == "HomeOwnership")
    {
      i = length(unique(na.omit(joined$HomeOwnership)))
      
      for (m in 1:i)
        
      {
        print(unique(na.omit(joined$HomeOwnership))[m])
        
        
      }  }
    
    
    
    # Education
    if (class_name == "Education")
    {
      i = length(unique(na.omit(joined$Education)))
      
      for (m in 1:i)
        
      {
        print(unique(na.omit(joined$Education))[m])
        
        
      }  }
    
    
  }
}
























