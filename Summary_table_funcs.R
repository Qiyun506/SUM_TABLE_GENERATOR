library(glue)
library(devtools)
install_github("emwozniak/Table1")
library(Table1)
library(dplyr)
library(tidyr)


# Static Variable
TEMPLATE_NAMES<-c("Unit", "Visit","Type", "Treatment","N",    "n",    "Mean", "SD",   "Median"  , "Q1",   "Q3",   "Min", "Max")


#===============================================================================
#==this function will convert the certain column from "chr" type to "num" type,
#==param:
#==df: the dataframe you want to modify
#==col_vec: a vector of column names that you want to change type
#==RETRUN:the modified df that you want
chr_to_num <- function(df, col_vec) {
  for(col_name in col_vec){
    if (col_name %in% names(df) && is.character(df[[col_name]])) {
      df[[col_name]] <- as.numeric(df[[col_name]])
    }
  }
  return(df)
}


#===============================================================================
#baseLine_value_wide<-function(df,base_day,studyName_vec){
#  base_df<-df[df$FolderName == base_day,]
#  base_values<-list()
#  for(i in studyName_vec){
#    base_values[[i]] = base_df[[i]]
#  }
#  return(base_values)
#}
#===============================================================================



#===============================================================================
#== find the baseline value for each study
#==param:
#==df: the dataframe you want to modify
#==base_day: a string in VISIT that regard as the baseline
#==studyName_vec: the vector that contains each study
#==RETRUN: a list that stores the list of study, and the each list of study 
#          contains a list with subject----> baseline data for subject
baseLine_value_long<-function(df,base_day,studyName_vec){
  base_df<-df[df$FolderName == base_day,]
  base_values<-list()
  #print(1)
  for(i in studyName_vec){
    base_df_part<-base_df[base_df$Test == i,]
    SUBJ<-base_df_part$Subject
    named_list <- setNames(as.list(base_df_part[["score"]]),SUBJ)
    #paired_list <- mapply(list, SUBJ, base_df[[i]], SIMPLIFY = FALSE)
    base_values[[i]] = named_list
    #print(2)
  }
  return(base_values)
}



#==============================================================================
#get the change from baseline value for different studies
#@params df: input dataframe
#@params studyName: the study you want to find for change value
#@params b_value: the list that stores all base value
#
#return: a dataframe that append the change from baseline
#with a new column ended with "_change"
get_change_wide<-function(df,studyName,b_value){
  base_vec<-vector()
  change_vec<-vector()
  sub_df<-subset(df,select = c("Subject",studyName))
  #print(nrow(sub_df))
  study_baseline<-b_value[[studyName]]
  freq_vec<-vector()
  for(subj in 1:length(unique(sub_df$Subject))){
    his_days<-nrow(sub_df[sub_df == unique(sub_df$Subject)[subj],])
    freq_vec<-c(freq_vec,his_days)
  }
  base_vec<-rep(study_baseline,times = freq_vec )
  #print(length(base_vec))
  change_vec<-base_vec - sub_df[[studyName]]
  #print(freq_vec)
  #print((change_vec))
  df[[glue("{studyName}_change")]]<-change_vec
  print(glue("col {studyName} done for change"))
  return(df)
}

#==============================================================================
#get the change from baseline value for different studies for long form
#@params df: input dataframe
#@params studyName: the study you want to find for change value
#@params b_value: the list that stores all base value
#
#return: a dataframe that append the change from baseline
#with a new column ended with "_change"
get_change_long<-function(df,b_value){
  base_vec<-vector()
  change_vec<-vector()
  #print(1)
  for(i in 1: nrow(df)){
    base_vec<-c(base_vec,b_value[[df$Test[i]]][[df$Subject[i]]])
    #print(i)
  }
  #print()
  if(class(base_vec) != "numeric"){
    base_vec<-as.numeric(base_vec)
  }
  
  if(class(df$score) != "numeric"){
    df$score<-as.numeric(df$score)
  }
  change_vec<-df$score - base_vec
  #print(1)
  df[[glue("Test_change")]]<-change_vec
  df[["baseline"]]<-base_vec
  #print(1)
  print(glue("col done for change"))
  return(df)
}


#===============================================================================
# process the raw data, remove empty, select the safety groups and tidy the 
# format
# @params study_path: the path of raw data 
# @params common_path: the data frame that contains safety population
# @params remove: the value that we need to remove because of missing value
# @params before_baseLine: if the baseline is not the initial study, then we set
#         this as true
# @params need_to_remove: if so, we need to add the string here tell the function 
#         to remove it from the df
# @params duplicated_title: if we have two title, in the raw df, we need to 
#         remove the row
#
# return: the processed df that ready for next step.
#
pre_process<-function(study_path,common_path,remove = "",before_baseLine = FALSE,
                      need_to_remove = "",duplicated_title = FALSE){
  df<-read.csv(study_path)
  comm<-read.csv(common_path)
  if(duplicated_title){
    df<-df[2:nrow(df),]
  }
  df<-left_join(df,comm, by = "Subject")
  df<-df[df$trtp != remove,]
  if(before_baseLine){
    df<-df[df$FolderName != need_to_remove,]
  }
  return(df)
}

#===============================================================================
# get the summary table with ERIC LIU's format, 
# @params:  shell(df): the template that we will fulfill the blanks into it  
# @params:  df: the dataframe that we use to modify
# @params:  Baseline(string): the baseline string
# @params:  studyNames: the study we want to use to see the 
# @params:  COMM(df): the safety dataframe
# 
# RETURN: the summary table that fit the format 
get_summary_table_long<-function(shell,df,Baseline,studyNames,COMM){
  
  num<-nrow(shell)
  shell$Unit<-rep(studyNames,times = num)
  
  global_df<-df
  df<-global_df[global_df$Test == studyNames,]
  for(i in 1: num){
    filter_info<-shell[i,]
    #first Layer filter for df
    if(filter_info$Visit == "Baseline"){
      sum_df<-df[df$FolderName == Baseline & df$trtp == filter_info$Treatment,]
    }
    else{
      sum_df<-df[df$FolderName == filter_info$Visit & df$trtp == filter_info$Treatment,]
    }
    #second Layer filter for df
    if(filter_info$Type == "Actual"){
      data_vec<-sum_df[["score"]]
    }
    else if(filter_info$Type == "Change from Baseline"){
      no_baseline_df<-sum_df[sum_df$FolderName != Baseline, ]
      data_vec<-no_baseline_df[["Test_change"]]
    }
    #print(data_vec)
    
    COMM<-COMM[COMM$trtp != "",]
    n_len<-nrow(COMM[COMM$trtp == filter_info$Treatment,])
    
    shell$N[i]<-n_len
    shell$n[i]<-length(data_vec)
    shell$Mean[i]<-mean(data_vec)
    shell$SD[i]<-sd(data_vec)
    shell$Median[i]<-median(data_vec)
    shell$Q1[i]<-quantile(data_vec, 0.25,type = 3)
    shell$Q3[i]<-quantile(data_vec, 0.75,type = 3)
    shell$Min[i]<-min(data_vec)
    shell$Max[i]<-max(data_vec)
  }
  return(shell)
#=======================================================loop for one study END===============================================
#=======================================================here ↓↓↓↓↓↓↓ we want to output the data==============================
    #write.csv(shell, glue("{getwd()}/{filter_info$Unit}_summ.csv"), row.names=FALSE)
}


#DF operation method
#===============================================================================
# This function will remove the column with input name
# @param df: the dataframe that we want to make adjustion to
# @param colname: the column that we want to make move
#
# RETURN: the dataframe that have already removed the df
rm_col<-function(df,colname){
  return(df[,names(df)!= colname])
}


#===============================================================================
# This function will generate the function that 
# @param df: the dataframe that we want to make adjustion to
# @param baseline(string): the visit that we regarded as the baseline.
# @param col_names: the column that we want to add to the raw template
#
# RETURN: the template that we have 
DRAW_FRAME<-function(df,baseline,col_names = TEMPLATE_NAMES){
  row_num<-(length(unique(df$FolderName))-1) * length(unique(df$trtp)) * 2 + length(unique(df$trtp))
  output<-data.frame(matrix(ncol = length(col_names), nrow = row_num))
  colnames(output) <- col_names
  
  #add for Type Column in template
  typeVec<-c(rep("Actual",times = length(unique(df$trtp))),
             rep("Change from baseline",times = length(unique(df$trtp))))
  typeVec<-rep(typeVec,times = length(unique(df$FolderName))-1)
  Baseline_manually<-rep("Actual",times = length(unique(df$trtp)))
  output$Type<-c(Baseline_manually,typeVec)
  
  #add for Treatment in template
  treatments<-unique(df$trtp)
  output$Treatment<-rep(treatments,times = row_num/length(unique(df$trtp)))
  
  #add for visits in template
  VIS<-sort(rep(unique(df$FolderName), each = 2*length(unique(df$trtp))))
  VIS <- VIS[order(VIS != baseline)][6:length(VIS)]
  output$Visit<-VIS
  
  return(output)
}

#===============================================================================
# This function will generate the function that 
# @param df: the data frame that we want to export
# @param name(string): name of the output csv file.
#
# RETURN: none 
export_data<-function(df,name){
  write.csv(df, glue("{getwd()}/{name}.csv"), row.names = FALSE)
}


#===============================================================================
# This function will convert the wide frame to long frame 
# @param df: the data frame that we want to convert
# @param keyCols(vector): names of the vector we want to make stack with.
#
# RETURN: none 
wide_to_long<-function(df,keyCols){
  M_long<- df %>%
    pivot_longer(
      cols = keyCols,
      names_to = "Test",
      values_to = "score"
    )
  return(M_long)
}











































