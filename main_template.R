#import the functions that we gonna use
source("C:/Users/Avansight/VXX_401/code/Summary_table_funcs.R")
COMM_PATH = "path for the safety population"
DATA_PATH = "path for the raw dataset that "
REMOVE = "the thing that used as missing data"
VISIT_REMOVE = "the visit before baseline and we need to remove this"
BASELINE_NOT_FIRST_DATE = TRUE #TRUE if the baseline is not the initial date
HAVE_SUBTITLES = "Boolean Value" #TRUE if we have the subtitle in the raw df
BASELINE # = _____
NAME_SUMM = "Summary table name"
NAME_DERIVED = "Derived dataset name"

#===============================================================================

#import the safety population for the summary table
COMM<-read.csv("What we want to Read")

#import raw data and generate the merged data with the function
M_data<-pre_process(study_path = DATA_PATH,
                    common_path = COMM_PATH,
                    remove = REMOVE,
                    before_baseLine = BASELINE_NOT_FIRST_DATE,
                    need_to_remove = VISIT_REMOVE,
                    duplicated_title = HAVE_ANNOTATION_SUBTITLES
)

#... if we have more raw data, we can use this method again and again

#select the studies we want to use for making summary table
keyCols<-names(M_data)[num1:num2]
M_data<-chr_to_num(M_data,keyCols)
#convert the long table to wide table
M_long<-wide_to_long(M_data,keyCols)
#find the baseline value if we need 
b_long<-baseLine_value_long(M_long,BASELINE,keyCols)
#get change from baseline and get the whole data frame
M_long<-get_change_long(M_long,b_long)
#generate the dataframe as the output df
output_df<-data.frame()
#get the template
output1<-DRAW_FRAME(M_long,BASELINE)

for(unit in unique(M_long$Test)){
  OUT<-get_summary_table_long(output1,M_long,BASELINE,unit,COMM)
  print(glue("done for {unit} summary making"))
  output_df<-rbind(output_df,OUT)
}

#export the summary table  
export_data(output_df,NAME_SUMM)
#export the derived data
export_data(M_long,NAME_DERIVED)