setwd("C:\\Users\\andre\\Desktop\\Progetto R Data factory")

#install.packages("dplyr")
#install.packages("zoo")
#install.packages("data.table")
#install.packages("gbm")
#install.packages("ggplot2")


library("ggplot2")
library("dplyr")   #we will use the pipe %>% from this package
library("zoo")
library("data.table")
library("gbm")

getwd()

#DATA ACQUISITION
#creating data frames in R importing CSV files

telemetry <- read.csv("PdM_telemetry.csv")
str(telemetry)   #876.100 rows
View(telemetry)

errors <- read.csv("PdM_errors.csv")
str(errors)  #3919 rows
View(errors)
maint <- read.csv("PdM_maint.csv")
str(maint)   #3286 rows
View(maint)

machines <- read.csv("PdM_machines.csv")
str(machines)    #100 rows
View(machines)

failures <- read.csv("PdM_failures.csv")
str(failures)    #761 << 3286 rows contained in maint
View(failures)

#in order to find shared and not shared elements
shared<-failures [failures$datetime %in% maint$datetime,]
View(shared)
'%!in%' <- Negate('%in%')
not_shared<-failures[failures$datetime %!in% maint$datetime,]
View(not_shared)


#DATA VISUALIZATION: visualizing data set
#Telemetry
ggplot(data=telemetry%>% filter(machineID==1), aes(x=datetime, y=volt, group=1)) + geom_line(color="red")
#Errors
ggplot(data=errors, aes(x=errorID))+geom_bar(fill="orange",stat="count")+
  labs(title="Number of errors by Type", x="error types")

ggplot(data=errors%>% filter(machineID==1), aes(x=errorID))+
  geom_bar(fill="orange", stat="count")+
  labs(title="Number of errors by Type for Machine 1", x="error types")

#Maintenance
ggplot(data=maint, aes(x=comp))+
  geom_bar(fill="red", stat="count")+
  labs(title="Number of components replaced by type", x="component types")
ggplot(data=maint %>% filter(machineID==1), aes(x=comp))+
  geom_bar(fill="red", stat="count")+
  labs(title="Number of components replaced by type for Machine 1",
       x="component types")
ggplot(data=maint, aes(x=machineID))+
  geom_bar(aes(fill=comp),stat="count")+
  labs(title="Number of components replaced by type for each Machine",
       x="machineID")

#Machines
ggplot(data=machines, aes(x=age))+
  geom_bar(fill="red", stat="count")+
  labs(title="Number of Machines of a certain age", x="age")
#Failures
ggplot(data=failures, aes(x=failure))+
  geom_bar(fill="orange", stat="count")+
  labs(title="Number of Failures of a certain type", x="failure type")
ggplot(data=failures, aes(x=machineID))+
  geom_bar(aes(fill=failure),stat="count")+
  labs(title="Number of Failures of a certain type for each Machine",
       x="machineID")



str(telemetry)
str(failures)
str(maint)
str(errors)
str(machines)


#TELEMETRY

telemetry$datetime <- as.POSIXct(telemetry$datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")

#ERRORS
str(errors)
errors$datetime <- as.POSIXct(errors$datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")
errors$errorID <- as.factor(errors$errorID)

#MAINT
str(maint)
maint$datetime <- as.POSIXct(maint$datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")
maint$comp <- as.factor(maint$comp)

#FAILURE
str(failures)
failures$datetime <- as.POSIXct(failures$datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")
failures$failure <- as.factor(failures$failure)


#MACHINES
str(machines)
machines$model <- as.factor(machines$model)


#CLEANING MISSING VALUES
#TELEMETRY
sum(is.na(telemetry))
sum(is.na(failures))
sum(is.na(errors))
sum(is.na(machines))
sum(is.na(maint))

#DUPLICATED ROWS
sum(duplicated(telemetry))
sum(duplicated(failures))
sum(duplicated(errors))
sum(duplicated(maint))
sum(duplicated(machines))


#IDENTIFYING OUTLIERS
boxplot(telemetry$volt, ylab="volt")
boxplot.stats(telemetry$volt)$out
range(telemetry$volt)


#FEATURE ENGENEERING

telemetrymean <- telemetry %>%
  arrange(machineID,datetime) %>%
  group_by(machineID) %>%
  mutate(voltmean= rollapply(volt, width=3, FUN=mean, align="right", fill=NA, by=3),
         rotatemean= rollapply(rotate, width=3, FUN=mean, align='right', fill=NA, by=3),
         pressuremean= rollapply(pressure, width=3, FUN=mean, align='right', fill=NA, by=3),
         vibrationmean= rollapply(vibration, width=3, FUN=mean, align='right', fill=NA, by=3)) %>%
  select(machineID,datetime,voltmean,rotatemean,pressuremean,vibrationmean) %>%
  filter(!is.na(voltmean)) %>%
  ungroup()

telemetrysd <- telemetry %>%
  arrange(machineID,datetime) %>%
  group_by(machineID) %>%
  mutate(voltsd= rollapply(volt, width=3, FUN=mean, align="right", fill=NA, by=3),
         rotatesd= rollapply(rotate, width=3, FUN=mean, align='right', fill=NA, by=3),
         pressuresd= rollapply(pressure, width=3, FUN=mean, align='right', fill=NA, by=3),
         vibrationsd= rollapply(vibration, width=3, FUN=mean, align='right', fill=NA, by=3)) %>%
  select(machineID,datetime,voltsd,rotatesd,pressuresd,vibrationsd) %>%
  filter(!is.na(voltsd)) %>%
  ungroup()


telemetrymean_24h <- telemetry %>%
  arrange(machineID,datetime) %>%
  group_by(machineID) %>%
  mutate(voltmean_24h= rollapply(volt, width=24, FUN=mean, align="right", fill=NA, by=3),
         rotatemean_24h= rollapply(rotate, width=24, FUN=mean, align='right', fill=NA, by=3),
         pressuremean_24h= rollapply(pressure, width=24, FUN=mean, align='right', fill=NA, by=3),
         vibrationmean_24h=rollapply(vibration, width=24, FUN=mean, align='right', fill=NA, by=3)) %>%
  select(machineID,datetime,voltmean_24h,rotatemean_24h,pressuremean_24h,vibrationmean_24h) %>%
  filter(!is.na(voltmean_24h)) %>%
  ungroup()



telemetrysd_24h <- telemetry %>%
  arrange(machineID,datetime) %>%
  group_by(machineID) %>%
  mutate(voltsd_24h= rollapply(volt, width=24, FUN=mean, align="right", fill=NA, by=3),
         rotatesd_24h= rollapply(rotate, width=24, FUN=mean, align='right', fill=NA, by=3),
         pressuresd_24h= rollapply(pressure, width=24, FUN=mean, align='right', fill=NA, by=3),
         vibrationsd_24h= rollapply(vibration, width=24, FUN=mean, align='right', fill=NA, by=3)) %>%
  select(machineID,datetime,voltsd_24h,rotatesd_24h,pressuresd_24h,vibrationsd_24h) %>%
  filter(!is.na(voltsd_24h)) %>%
  ungroup()

telemetryfeat <- data.frame(telemetrymean,telemetrysd[,-c(1:2)])
telemetryfeat_24h <- data.frame(telemetrymean_24h, telemetrysd_24h[,-c(1:2)])

telemetryfeat_final <- telemetryfeat %>%
  left_join(telemetryfeat_24h, by=c("datetime", "machineID"))%>%
  filter(!is.na(voltmean_24h))



#ERRORS
str(errors)
View(errors)

errorcount <- errors %>%
  mutate(error1 = as.integer(errorID == 'error1'),
         error2= as.integer(errorID=='error2'),
         error3= as.integer(errorID=='error3'),
         error4= as.integer(errorID=='error4'),
         error5 = as.integer(errorID=='error5'))

errorcount_final <- errorcount %>%
  group_by(machineID,datetime) %>%
  summarise(error1sum= sum(error1),
            error2sum=sum(error2),
            error3sum=sum(error3),
            error4sum=sum(error4),
            error5sum=sum(error5)) %>%
  ungroup()

errorfeat <- telemetry %>%
  select(datetime,machineID) %>%
  left_join(errorcount_final, by=c('datetime', 'machineID'))

errorfeat[is.na(errorfeat)] <- 0
View(errorfeat)


errorfeat_final <- errorfeat %>%
  arrange(machineID,datetime) %>%
  group_by(machineID) %>%
  mutate(error1count= rollapply(error1sum, width=24, FUN=sum, align='right', fill=NA, by=3),
         error2count= rollapply(error2sum, width=24, FUN=sum, align='right', fill=NA, by=3),
         error3count= rollapply(error3sum, width=24, FUN=sum, align='right', fill=NA, by=3),
         error4count= rollapply(error4sum, width=24, FUN=sum, align='right', fill=NA, by=3),
         error5count= rollapply(error5sum, width=24, FUN=sum, align='right', fill=NA, by=3),) %>%
  select(machineID,datetime,error1count,error2count,error3count,error4count,error5count) %>%
  filter(!is.na(error1count)) %>%
  ungroup()
View(errorfeat_final)

nrow(errorfeat_final)


#MAINTENANCE
comprep <- maint %>%
  mutate(comp1= as.integer(comp== 'comp1'),
         comp2= as.integer(comp=='comp2'),
         comp3= as.integer(comp=='comp3'),
         comp4=as.integer(comp=='comp4')) %>%
  select(-comp)

comprep <- as.data.table(comprep)
setkey(comprep,machineID,datetime)

comp1rep <- comprep[comp1==1, .(datetime,machineID, lastrepcomp1=datetime)]
comp2rep <- comprep[comp2==1, .(datetime,machineID,lastrepcomp2=datetime)]
comp3rep <- comprep[comp3==1, .(datetime,machineID,lastrepcomp3=datetime)]
comp4rep <- comprep[comp4==1, .(datetime,machineID,lastrepcomp4=datetime)]


compdate <- as.data.table(telemetryfeat_final[,c(1:2)])
setkey(compdate,machineID,datetime)

comp1feat <- comp1rep[compdate[,.(machineID,datetime)], roll=TRUE]
comp2feat <-comp2rep[compdate[,.(machineID,datetime)], roll=TRUE]
comp3feat <- comp3rep[compdate[,.(machineID,datetime)],roll=TRUE]
comp4feat <- comp4rep[compdate[,.(machineID,datetime)],roll=TRUE]

comp1feat$sincelastcomp1 <- as.numeric(difftime(comp1feat$datetime,comp1feat$lastrepcomp1, units="days"))
comp2feat$sincelastcomp2 <- as.numeric(difftime(comp2feat$datetime, comp2feat$lastrepcomp2, units = 'days'))
comp3feat$sincelastcomp3 <- as.numeric(difftime(comp3feat$datetime, comp3feat$lastrepcomp3, units = 'days'))
comp4feat$sincelastcomp4 <- as.numeric(difftime(comp4feat$datetime,comp4feat$lastrepcomp4, units='days'))


compfeat_final <- data.frame(compdate, comp1feat[,.(sincelastcomp1)], comp2feat[,.(sincelastcomp2)], comp3feat[,.(sincelastcomp3)], comp4feat[,.(sincelastcomp4)])

#machines nothing to do


finalfeat <- data.frame(telemetryfeat_final,errorfeat_final[,-c(1:2)])
finalfeat <- finalfeat %>%
  left_join(compfeat_final, by=c('datetime', 'machineID')) %>%
  left_join(machines, by=c('machineID'))

View(finalfeat)




#DATA LABELING

labeled <- left_join(finalfeat, failures, by=c('machineID')) %>%
  mutate(datediff = difftime(datetime.y, datetime.x, units = 'hours')) %>%
  filter(datediff<=24, datediff>=0)

View(labeled)

labeledfeatures <- left_join(finalfeat,
                             labeled %>% select(datetime.x, machineID,failure),
                             by= c("datetime"= "datetime.x", "machineID")) %>%
  arrange(machineID, datetime)


levels(labeledfeatures$failure) <- c(levels(labeledfeatures$failure), 'none')
labeledfeatures$failure[is.na(labeledfeatures$failure)] <- 'none'

View(labeledfeatures)


#TRAINDATA AND TESTDATA

traindata1 <- labeledfeatures[labeledfeatures$datetime < "2015-07-31 01:00:00",]
testdata1 <- labeledfeatures[labeledfeatures$datetime > "2015-08-01 01:00:00",]

traindata2 <- labeledfeatures[labeledfeatures$datetime < "2015-08-31 01:00:00",]
testdata2 <- labeledfeatures[labeledfeatures$datetime > "2015-09-01 01:00:00",]

traindata3 <- labeledfeatures[labeledfeatures$datetime < "2015-09-30 01:00:00",]
testdata3 <- labeledfeatures[labeledfeatures$datetime > "2015-10-01 01:00:00",]


library(gbm)

#model <- gbm(formula, data, distribution,n.trees,interaction.depth,shrinkage)

set.seed(1234)
trainformula <- as.formula(paste('failure', paste(names(labeledfeatures)[c(3:29)], collapse='+'), sep='~'))
trainformula


gbm_model1 <- gbm(formula = trainformula, data = traindata1, distribution = 'multinomial', n.trees = 50, interaction.depth = 5, shrinkage = 0.1)
gbm_model2 <- gbm(formula = trainformula, data = traindata2, distribution = 'multinomial', n.trees = 50, interaction.depth = 5, shrinkage = 0.1)
gbm_model3 <- gbm(formula = trainformula, data = traindata3, distribution = 'multinomial', n.trees = 50, interaction.depth = 5, shrinkage = 0.1)


#predict <- predict(gbm_model,testing_data, n.trees, type='response')
pred_gbm1 <- predict(gbm_model1, testdata1, n.trees=50, type='response')
pred_gbm2 <- predict(gbm_model2, testdata2, n.trees=50, type='response')
pred_gbm3 <- predict(gbm_model3, testdata3, n.trees=50, type='response')


pred_gbm3

# label distribution after features are labeled - the class imbalance problem 
ggplot(labeledfeatures, aes(x=failure)) +
  geom_bar(fill="red") + 
  labs(title = "label distribution", x = "labels")



# define evaluate function 
Evaluate<-function(actual=NULL, predicted=NULL, cm=NULL){
  if(is.null(cm)) {
    actual = actual[!is.na(actual)] 
    predicted = predicted[!is.na(predicted)] 
    f = factor(union(unique(actual), unique(predicted))) 
    actual = factor(actual, levels = levels(f))
    predicted = factor(predicted, levels = levels(f)) 
    cm = as.matrix(table(Actual=actual, Predicted=predicted))
  }
  
  n = sum(cm) # number of instances 
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class 
  p = rowsums / n # distribution of instances over the classes
  q = colsums / n # distribution of instances over the predicted classes
  
  #accuracy 
  accuracy = sum(diag) / n
  #per class 
  recall = diag / rowsums 
  precision = diag / colsums 
  f1 = 2 * precision * recall / (precision + recall)
  #macro 
  macroPrecision = mean(precision) 
  macroRecall = mean(recall)
  macroF1 = mean(f1)
  
  
  #1-vs-all matrix
  oneVsAll = lapply(1 : nc,
                    function(i){
                      v = c(cm[i,i],
                            rowsums[i] - cm[i,i], colsums[i] - cm[i,i],
                            n-rowsums[i] - colsums[i] + cm[i,i]);
                      
                      return(matrix(v, nrow = 2, byrow = T))})
  s = matrix(0, nrow=2, ncol=2) 
  for(i in 1:nc){s=s+oneVsAll[[i]]}
  #avg accuracy 
  avgAccuracy = sum(diag(s))/sum(s)
  #micro 
  microPrf = (diag(s) / apply(s,1, sum))[1];
  #majority class 
  mcIndex = which(rowsums==max(rowsums))[1] # majority-class index 
  mcAccuracy = as.numeric(p[mcIndex])
  mcRecall = 0*p; 
  mcRecall[mcIndex] = 1 
  mcPrecision = 0*p; 
  mcPrecision[mcIndex] = p[mcIndex] 
  mcF1 = 0*p; 
  mcF1[mcIndex] = 2 * mcPrecision[mcIndex] / (mcPrecision[mcIndex] +1)
  
  #random accuracy 
  expAccuracy = sum(p*q) 
  #kappa
  kappa = (accuracy - expAccuracy) / (1 - expAccuracy)
  #random guess 
  rgAccuracy = 1 / nc 
  rgPrecision = p
  rgRecall = 0*p + 1 / nc 
  rgF1 = 2 * p / (nc * p + 1)
  #rnd weighted 
  rwgAccurcy = sum(p^2) 
  rwgPrecision = p
  rwgRecall = p 
  rwgF1 = p
  classNames = names(diag) 
  if(is.null(classNames)) classNames = paste("C",(1:nc),sep="")
  
  return(list(
    ConfusionMatrix = cm, Metrics = data.frame(
      Class = classNames, 
      Accuracy = accuracy, 
      Precision = precision, 
      Recall = recall,
      F1 = f1, 
      MacroAvgPrecision = macroPrecision, 
      MacroAvgRecall = macroRecall,
      MacroAvgF1 = macroF1, 
      AvgAccuracy = avgAccuracy, 
      MicroAvgPrecision = microPrf, 
      MicroAvgRecall = microPrf, 
      MicroAvgF1 = microPrf,
      MajorityClassAccuracy = mcAccuracy, 
      MajorityClassPrecision = mcPrecision, 
      MajorityClassRecall = mcRecall,
      MajorityClassF1 = mcF1, Kappa = kappa,
      RandomGuessAccuracy = rgAccuracy, 
      RandomGuessPrecision = rgPrecision, 
      RandomGuessRecall = rgRecall,
      RandomGuessF1 = rgF1,
      RandomWeightedGuessAccurcy = rwgAccurcy, 
      RandomWeightedGuessPrecision = rwgPrecision, 
      RandomWeightedGuessRecall= rwgRecall,
      RandomWeightedGuessWeightedF1 = rwgF1)))
}



# evaluation metrics for first split 
pred_gbm1 <- as.data.frame(predict(gbm_model1, testdata1,
n.trees = 50,type = "response"))
names(pred_gbm1) <- gsub(".50", "", names(pred_gbm1)) 
pred_gbm1$failure <- as.factor(colnames(pred_gbm1)[max.col(pred_gbm1)])
eval1 <- Evaluate(actual=testdata1$failure,predicted=pred_gbm1$failure) 
eval1$ConfusionMatrix
t(eval1$Metrics)





# report the recall rates for the models 
rownames <- c("comp1","comp2","comp3","comp4","none")
rownames 
data.frame(cbind(failure = rownames,
                 gbm_model1_Recall = eval1$Metrics$Recall, 
                 gbm_model2_Recall = eval2$Metrics$Recall, 
                 gbm_model3_Recall = eval3$Metrics$Recall))


(sum(labeledfeatures$error1count==1))



