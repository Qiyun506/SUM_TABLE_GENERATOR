
COL_NAMES<-c("Postbaseline","Treatment","Baseline","N","N1","n","Precent(%)")
NORM<- c("Normal","Abnormal NCS","Abnormal CS")
STATUS = length(NORM)*length(NORM)
BASEDATE = "Day 1"
#study = unit
#===============================================================================
# This function will generate the function that 
# @param df: the dataframe that we want to make adjustion to
#
# RETURN: the template that we have 

SHIFT_FRAME<-function(df){
  trtp_num<-length(unique(df$trtp))
  row_num<-trtp_num * STATUS
  output<-data.frame(matrix(ncol = length(COL_NAMES), nrow = row_num))
  colnames(output) <- COL_NAMES
  
  POST_VEC<-rep(NORM,each = trtp_num * length(NORM))
  TRTP_VEC<-rep(unique((df$trtp)),STATUS)
  BASE_VEC<-rep(NORM, times = trtp_num * length(NORM))
  output$Postbaseline<-POST_VEC
  output$Treatment<-TRTP_VEC
  output$Baseline<-BASE_VEC
  output[is.na(output)]<-0
  length<-nrow(output)
  #output$TestNames<-rep("",nrow(output))
  output1<-data.frame()
  #functions for adding the shift table for each subjects
  #empty_shell<-output
  #for(i in unique(df$Test)){
  #  temp<-empty_shell
  #  temp$TestNames<-rep(i,nrow(empty_shell))
  #  output1<-rbind(output1,temp)
  #}
  return(output)
}

#===============================================================================
# This function will 
# @param df: the dataframe that we want to make adjustion to
#
# RETURN: the template that we have 

add_to_df<-function(df){
  df$INTP_BASE<-rep(0, nrow(df))
  df$INTP_POST<-rep(0, nrow(df))
  for(test in unique(df$Test)){
    print(test)
    BLS<-baseline_status(df,BASEDATE,test)
    WLS<-get_post_status(df,test)
    for(i in 1:nrow(df)){
      df$INTP_BASE[i]<-BLS[[df$trtp[i]]][[df$Subject[i]]]
      df$INTP_POST[i]<-WLS[[df$Subject[i]]]
    }
  }
  return(df)
}


#TEST BASED
#assisting function for add_baseline_to_df function, used for finding the
#baseline value for each test
baseline_status<-function(df,BASEDATE,study){
  BS_list<-list()
  part_df<-df[df$FolderName == BASEDATE & df$Test == study,]
  treatments<-unique(df$trtp)
  for(t in treatments){
    BS_list[[t]]<-list()
    t_p_df<-part_df[part_df$trtp == t,]
    BS_list[[t]]<- setNames(as.list(t_p_df$INTP_EGORRES),t_p_df$Subject)
  }
  return(BS_list)
}

#TEST BASED
#assisting function for add_to_df function, used for finding the
#post Baseline value for each test
get_post_status<-function(df,study){
  worst_list<-list()
  part_df<-df[df$Test == study,]
  for(i in unique(part_df$Subject)){
    subj_df<-part_df[part_df$Subject == i,]
    normal_vec<-unique(subj_df$INTP_EGORRES)
    normal_vec <- normal_vec[order(factor(normal_vec, levels = c("Abnormal CS", "Abnormal NCS", "Normal")))]
    worst_list[[i]] = normal_vec[1]
  }
  return(worst_list)
}




fulfill_shift_table<-function(df,shell){
  single_df<-df[!duplicated(df$Subject), ]
  for(i in 1:nrow(shell)){
    
    filter_info<-shell[i,]
    post = filter_info[["Postbaseline"]][1]
    treat = filter_info[["Treatment"]][1]
    base = filter_info[["Baseline"]][1]
    
    summ_df <- single_df[single_df$trtp == treat & single_df$INTP_BASE == base & 
                         single_df$INTP_POST == post, ]
    N_df <- single_df[single_df$trtp == treat,]
    shell$n[i] = nrow(summ_df)
    #print(shell$n[i])
    shell$N[i] = nrow(N_df)
    #print(shell$N[i])
    shell$N1[i] = nrow(N_df)
    #print(shell$N1[i])
  }
  
  precent <- shell$n/shell$N
  shell$`Precent(%)`<-precent
  return(shell)
}




