library(survival)
library(rms)
library(dplyr)
library(survminer)
library(ggplot2)
library(ggfortify)
library(forecast)
library(ranger)

variables <- c('inducingflag','Smelly','SurvivalTime','LOC','prev_fixing')

type_smells <- c('ExcessiveInterlangCommunication', 'Toomuchclustring','ToomuchScattering', 'UnusedMethodDeclaration','UnusedMethodImplementation', 'UnusedParameter','AssumingSafeReturnValue', 'ExcessiveObjects', 'NotHandlingExceptions','NotCachingObjects','NotSecuringLibraries','HardCodingLibraries','NotUsingRelativePath', 'MemoryManagementMismatch','LocalReferencesAbuse')

covariates <- c('Smelly','LOC','prev_fixing','ExcessiveInterlangCommunication', 'Toomuchclustring','ToomuchScattering', 'UnusedMethodDeclaration','UnusedMethodImplementation', 'UnusedParameter','AssumingSafeReturnValue', 'ExcessiveObjects', 'NotHandlingExceptions','NotCachingObjects','NotSecuringLibraries','HardCodingLibraries','NotUsingRelativePath', 'MemoryManagementMismatch','LocalReferencesAbuse')

systems <- c('conscrypt', 'frostwire','javacpp','jna','OpenDDS','pljava','realm-java','rocksdb')

sys_name <- "rocksdb"
print(paste('Survival analysis results for each smell type or variable for ', sys_name, sep = ''))
print('================================================================')
path_name <- paste('C:\\Users\\C5265284\\PycharmProjects\\SurvivalAnalysis\\data\\survival\\cleaned\\',sys_name,'_merged2_cleaned_time.csv',sep = '')
count_path_name <- paste('C:\\Users\\C5265284\\PycharmProjects\\SurvivalAnalysis\\data\\survival\\cleaned\\',sys_name,'_merged2_cleaned_flag_count.csv',sep = '')
print(count_path_name)
mydata <- read.csv(path_name)
attach(mydata)

flag_count <- read.csv(count_path_name)


for(cov in covariates){
  print('')
  print(paste("Result for: ",cov,sep=''))
  print('=========================================================================')
  if (is.element(cov ,type_smells) == TRUE) {
    svar <- paste("Smelly_",cov,sep='')
    scount <- get(svar, flag_count)
  }
  else {
    svar <- "Smelly"
    scount <- 1  # just to satisfy the cindition for execution
  } 
  
  if(scount > 0){
  coxsurvival <- coxph(Surv(SurvivalTime, inducingflag) ~ get(svar), data = mydata)
  testdata <- cox.zph(coxsurvival)
  
  
  print(summary(coxsurvival))
  
  #print(testdata)
  print ('')
  print (paste('Covariate :', cov , sep = '' ))
  print(paste('exp(coef) :',coef(summary(coxsurvival))[2], sep = ''))
  print(paste('p-value (Cox hazard model) :',coef(summary(coxsurvival))[5], sep = ''))
  print(paste('p-value (Porportional hazards assumption) :',testdata[[1]][5], sep = ''))
  }
  else{
    print ('')
    print (paste('Covariate :', cov ,' count: ', scount, sep = '' ))
    print('exp(coef) : NA')
    print('p-value (Cox hazard model) : NA')
    print('p-value (Porportional hazards assumption) : NA')
  }
}
detach(mydata)
