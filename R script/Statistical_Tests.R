library(ScottKnottESD)
library(dplyr)

files = c('activemq-5.0.0', 'activemq-5.1.0',
          'activemq-5.2.0', 'activemq-5.3.0',
          'groovy-1_5_7', 'groovy-1_6_BETA_1',
          'hbase-0.94.0', 'hbase-0.95.0',
          'hive-0.10.0', 'hive-0.9.0',
          'jruby-1.1', 'jruby-1.4.0',
          'jruby-1.5.0', 'lucene-2.3.0',
          'lucene-2.9.0', 'lucene-3.0.0',
          'wicket-1.3.0-beta2',
          'wicket-1.3.0-incubating-beta-1')
DATA_PATH = 'C:/Users/Motaz/Desktop/work/Model_Selection_Issue/analyse_results/code'
ML_ALGO_LIST = c('KNN','DT', 'RF')
SCORERS = c('MCC', 'LogLoss', 'Gmean', 'ACC', 'BAL_ACC', 'AUC', 'F1', 'F1_W')

compare_algorithm <- function(orgs,data,metric,algos, scorers, greater_is_better = TRUE){
  results <- data.frame(Organization = character(),model = character(),scorer = character(),group = double(),mean = double(),median = double())
  for (org_name in orgs) {
    cat('working on org',org_name,'\n')
    org_data = subset(data,file == org_name)
    
    cat('working on metric',metric,'\n')
    
    
    for (algo in algos) {
      cat('Working on model',algo,'\n')
      algo_data <-subset(org_data,model==algo)
      print(algo_data)
      algos_list = NULL
      for (scorer in scorers){
        x <- unlist(subset(algo_data,scorer==scorer)[metric],use.names = FALSE)
        if (greater_is_better == FALSE){
          x <- x*-1
        }
        algos_list[[scorer]] <- x  
      }
      metric_df =  as.data.frame(algos_list)
      print(metric_df)
      sk <- sk_esd(metric_df)
      for (scorer in scorers) {
        x <- unlist(subset(algo_data,scorer==scorer)[metric],use.names = FALSE)
        new_entry <- list(file= org_name,group=sk$groups[[scorer]],model=algo, scorer=scorer,mean= mean(x),median = median(x))
        results <- rbind(results,new_entry)
      }
      plot(sk)
    }
      
    }
    
  return(results)
}
#main 
ALL_ORGS_DATA <- read.csv(paste(DATA_PATH,'/','Final_results_prepared.csv',sep=''))

MCC_metric <- compare_algorithm(orgs=files,data=ALL_ORGS_DATA,metric="MCC" ,algos=ML_ALGO_LIST, scorers=SCORERS, greater_is_better = TRUE)
AUC_metric <- compare_algorithm(orgs=files,data=ALL_ORGS_DATA,metric="AUC",algos=ML_ALGO_LIST, scorers=SCORERS, greater_is_better = TRUE)
f1_metric <- compare_algorithm(orgs=files,data=ALL_ORGS_DATA,metric="f1",algos=ML_ALGO_LIST, scorers=SCORERS,greater_is_better = TRUE)
G_metric <- compare_algorithm(orgs=files,data=ALL_ORGS_DATA,metric="G",algos=ML_ALGO_LIST, scorers=SCORERS,greater_is_better = TRUE)

write.csv(MCC_metric,paste(DATA_PATH,'/','MCC_results.csv',sep=''), row.names = FALSE)
write.csv(AUC_metric,paste(DATA_PATH,'/','AUC_results.csv',sep=''), row.names = FALSE)
write.csv(f1_metric,paste(DATA_PATH,'/','F1_results.csv',sep=''), row.names = FALSE)
write.csv(G_metric,paste(DATA_PATH,'/','G_results.csv',sep=''), row.names = FALSE)