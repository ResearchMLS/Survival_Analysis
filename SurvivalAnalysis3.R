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

systems <- c('conscrypt', 'frostwire','javacpp','jna','OpenDDS','pljava','realm-java','rocksdb')
#sys_name <- "conscrypt"
for(sys_name in systems){
  print('')
  print(paste('Survival analysis results for all smell for ', sys_name, sep = ''))
  print('================================================================')
  path_name <- paste('C:\\Users\\C5265284\\PycharmProjects\\SurvivalAnalysis\\data\\survival\\cleaned\\',sys_name,'_merged2_cleaned_time.csv',sep = '')
  print(path_name)
  mydata <- read.csv(path_name)
  attach(mydata)
  
  coxsurvival <- coxph(Surv(SurvivalTime, inducingflag) ~ Smelly, data = mydata)
  testdata <- cox.zph(coxsurvival)
  
  
  print(summary(coxsurvival))
  
  print(testdata)
  
  print (paste('Covariate :', 'Smelly', sep = '' ))
  print(paste('exp(coef) :',coef(summary(coxsurvival))[2], sep = ''))
  print(paste('p-value (Cox hazard model) :',coef(summary(coxsurvival))[5], sep = ''))
  print(paste('p-value (Porportional hazards assumption) :',testdata[[1]][5], sep = ''))
  detach(mydata)
  }

