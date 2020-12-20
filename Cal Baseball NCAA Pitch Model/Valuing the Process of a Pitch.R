#########################
###Libraries and stuff###
#########################

library(data.table)
library(lubridate)
library(ggplot2)
library(MASS)
library(xgboost)
library(Matrix)
library(RCurl)
library(e1071)


set.seed(69)

memory.limit(size=17592186044415)

###############
###Data pull###
###############

###Cal Trackman login info is specified

url = NA ###redacted
userpwd = NA ###redacted

###data.table to store Trackman data in is initialized 

full=data.table()

###Trackman FTP has folders full of .csvs where the folders are labeled by date
###Years, months, and days are looped through to access and scrape .csvs from these folders, utilizing RCurl package
###Loop runs from 1/1/2019 through the current date

for (i in 2019:year(Sys.Date())){
  for(j in 1:12){
    for(k in 1:31){
      tryCatch({
        url_specific=paste0(url,i,'/',j,'/',k,'/CSV/') ###loop determined url is specified
        filenames = getURL(url_specific, ftp.use.epsv = FALSE, dirlistonly = TRUE) ###info is pulled from url
        filenames = strsplit(filenames, "\r\n") ###filenames are seperated
        filenames = unlist(filenames) ###filenames are unlisted
        files=do.call('rbind',lapply(paste0(url_specific,filenames),function(x){fread(x)})) ###.csvs in specified folder are read-in and combined into one data.table
        full=rbind(full,files)},error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) ###.csvs in specified folder are combined with full data.table, loop is not terminated if no files/folder is found during an iteration 
    }}}



###full data.table is reformatted in case of irregularities 
full = data.table(full) 

##################
###Data cleanup###
##################

###Duplicates are removed from the dataset
###It appears as though there are rows with different PitchUIDs that are in fact duplicates

full = full[!(duplicated(PitchUID)) & 
              !(duplicated(full[,c('PitcherThrows','BatterSide','Balls','Strikes',
                                   'VertRelAngle','HorzRelAngle','SpinRate','SpinAxis',
                                   'InducedVertBreak','HorzBreak','PlateLocHeight','PlateLocSide',
                                   'RelHeight','RelSide','Extension',
                                   'EffectiveVelo','ExitSpeed','Angle')]))]

###Date formatting is cleaned
full[,Date:=mdy(Date)][
  ###'Out' related events are grouped together in the HitResult column for modeling purposes
  ,HitResult:=ifelse(PlayResult %in% c('Error','FieldersChoice','Out','Sacrifice'),'Out',PlayResult)][
    ###wOBA weights are assigned to each hit result. Currently hard coded based off of Ian Joffe's (Cal student) research
    ,wOBA:=ifelse(HitResult=='Out'|KorBB=='Strikeout',0,
                  ifelse(HitResult=='Single',.89,
                         ifelse(HitResult=='Double',1.24,
                                ifelse(HitResult=='Triple',1.58,
                                       ifelse(HitResult=='HomeRun',1.77,
                                              ifelse(KorBB=='Walk',.7,
                                                     ifelse(PitchCall=='HitByPitch',.73,NA)))))))][
                                                       ###'Strike' and 'Out; related events are grouped togher in the PitchResult column for modeling purposes
                                                       ,PitchResult:=ifelse(PitchCall == 'InPlay',HitResult,
                                                                            ifelse(PitchCall %in% c('StrikeCalled','StrikeSwinging'),'Strike',PitchCall))][
                                                                              ###Single 'Count' column is created for modeling purposes
                                                                              ,Count:=paste0(Balls,'-',Strikes)][
                                                                                ###Single Pitcher vs. Batter handedness 'Matchup' column is created for modeling purposes
                                                                                ,Matchup:=paste0(PitcherThrows,'-',BatterSide)][
                                                                                  ###Horizontal locations are fixed to allow models to identify inside vs. outside pitches as similar regardless of batter handedness 
                                                                                  ,PlateLocSideFixed:=ifelse(BatterSide=='Left',-PlateLocSide,PlateLocSide)][
                                                                                    ,HorzBreakFixed:=ifelse(BatterSide=='Left',-HorzBreak,HorzBreak)][
                                                                                      ,RelSideFixed:=ifelse(BatterSide=='Left',-RelSide,RelSide)][
                                                                                        ,HorzRelAngleFixed:=ifelse(BatterSide=='Left',-HorzRelAngle,HorzRelAngle)]



###'contact' data.table is created from balls in play with exit velo and angle information for xwOBA model fit

contact = full[HitResult!='Undefined' & !(is.na(ExitSpeed)) & !(is.na(Angle)) & KorBB=='Undefined']

###xwWOBA model is fit

xwOBAfit = svm(wOBA~ExitSpeed*Angle,data=contact)

###xwOBA predictions are made

full[,xwOBA:=predict(xwOBAfit,full[,c('ExitSpeed','Angle')],na.action=na.exclude)][
  ###xwOBA values are assigned for balls not put in play
  ,xwOBA:=ifelse(KorBB=='Walk',.7,
                 ifelse(PitchResult=='HitByPitch',.73,
                        ifelse(KorBB=='Strikeout',0,xwOBA)))][
                          ###Through count xwOBA values are calculated for each pitch of an at-bat
                          ,xwOBA_through:=last(xwOBA),by=c('GameUID','PAofInning','Inning','Top/Bottom')]

###Count wOBA, xwOBA, and through count xwOBA averages are calculated

count_woba = full[,.(wOBACount=mean(wOBA,na.rm=TRUE),
                     xwOBACount=mean(xwOBA,na.rm=TRUE),
                     xwOBACount_through=mean(xwOBA_through,na.rm=TRUE)),by='Count']

###Count resulting from a pitch is captured in the 'NextCount' column

full[,NextCount:=ifelse(PitchResult %in% c('BallCalled','BallIntentional'),paste0(Balls+1,'-',Strikes),
                        ifelse(PitchResult %in% c('FoulBall','Strike'),paste0(Balls,'-',Strikes+1),NA))]

###wOBA, xwOBA, and through count xwOBA averages are included in the 'full' data.table for both current and next count

full = merge(full,count_woba,by='Count',all.x=TRUE)

full = merge(full,count_woba,by.x='NextCount',by.y='Count',all.x=TRUE,suffixes=c('','.next'))

###'PitchwOBA', 'PitchxwOBA', and 'PitchxwOBA_through' columns are created
###For end of at bat this is Result (in terms of wOBA, xwOBA, or xwOBA_through) minus count average (in terms of wOBA, xwOBA, or xwOBA_through)
###For balls and strikes that do not end an at bat this is count average of resulting count (in terms of wOBA, xwOBA, or xwOBA_through) minus count average of count (in terms of wOBA, xwOBA, or xwOBA_through)

full[,PitchwOBA:=ifelse(PitchResult=='FoulBall' & Strikes==2,0,
                        ifelse(PitchResult %in% c('BallCalled','BallIntentional','Strike','FoulBall') & !(KorBB %in% c('Strikeout','Walk')),wOBACount.next-wOBACount,wOBA-wOBACount))][
                          ,PitchxwOBA:=ifelse(PitchResult=='FoulBall' & Strikes==2,0,
                                              ifelse(PitchResult %in% c('BallCalled','BallIntentional','Strike','FoulBall') & !(KorBB %in% c('Strikeout','Walk')),xwOBACount.next-xwOBACount,xwOBA-xwOBACount))][
                                                ,PitchxwOBA_through:=ifelse(PitchResult=='FoulBall' & Strikes==2,0,
                                                                            ifelse(PitchResult %in% c('BallCalled','BallIntentional','Strike','FoulBall') & !(KorBB %in% c('Strikeout','Walk')),xwOBACount_through.next-xwOBACount_through,xwOBA-xwOBACount_through))]



###'pitches' data.table is created, throwing out uninformative pitches for pitch valuing purposes

pitches = full[!(PitchResult %in% c('BallIntentional','CatchersInterference','Undefined'))] 

###Pitches without a 'PitchxwOBA_through' value are removed as this is the dependent variable for our pitch value model

pitches_clean = pitches[!(is.na(PitchxwOBA_through))]

###NA values are set to a number that will never be seen in the actual data (xgboost does not allow NAs)

pitches_clean[is.na(pitches_clean)] = -1000000

###The dependent variable ('PitchxwOBA_through') is specified for our pitch value model

pitch_lab = pitches_clean[,PitchxwOBA_through] 

###The predictors are specified for our pitch value model

pitch_mat = sparse.model.matrix(~.-1,pitches_clean[,c('PitcherThrows','BatterSide','Matchup','Balls','Strikes','Count',
                                                      'VertRelAngle','HorzRelAngleFixed','SpinRate','SpinAxis',
                                                      'InducedVertBreak','HorzBreakFixed','PlateLocHeight','PlateLocSideFixed',
                                                      'RelHeight','RelSideFixed','Extension',
                                                      'EffectiveVelo')])

###########################
###Hyperparameter tuning###
###########################

###Grid of hyperparameters to test is created 
###160 combinations of eta and max_depth values are to be tested

param_grid = expand.grid(eta = seq(.01,.2,by=.01), 
                         max_depth = seq(3,10,by=1))

###data.table to store tuning results in is initialized

tuning = data.table()

###Each row of the hyperparameter grid is looped through
###For each row we use xgboost's cross validation function to measure the performance (RMSE) of the given combination of hyperparameters

for (i in 1:nrow(param_grid))
{
  param = param_grid[i,] ###Row of the hyperparameter grid to test is specified 
  
  ###Five-fold cross validation is performed
  ###We look to minimize out-of-sample RMSE through 100000 rounds
  ###The process is stopped if improvement is not seen for 1000 rounds 
  
  cv = xgb.cv(data = pitch_mat,label = pitch_lab,objective = 'reg:squarederror',metrics = 'rmse',
              eta = param$eta, max_depth = param$max_depth, 
              nrounds = 100000, nfold = 5,early_stopping_rounds = 1000,
              verbose=TRUE)
  
  tune = NULL ####Variable to store this iteration's tuning results in is initialized 
  
  tune$eta = rep(param$eta,max(cv$evaluation_log$iter)) ###eta value tested this iteration is stored 
  tune$max_depth = rep(param$max_depth,max(cv$evaluation_log$iter)) ###max_depth value tested this iteration is stored
  tune$nrounds = cv$evaluation_log$iter ###Round numbers are stored
  tune$rmse = cv$evaluation_log$test_rmse_mean ###Out-of-sample RMSEs for each round are stored
  
  tuning = rbind(tuning,tune) ###This iteration's tuning results are stored in the comprehensive data.table
  
}

###############################
###Model fit and diagnostics###
###############################

###Our model is fit according to the hyperparameters that performed the best in tuning

pitchfitxg = xgboost(data = pitch_mat,label = pitch_lab,objective = 'reg:squarederror',metrics = 'rmse',
                     eta = .01, max_depth = 9,
                     nrounds = 906,
                     verbose=TRUE)



###Feature importances of model are saved

pitchfitxg_importance = xgb.importance(model=pitchfitxg,feature_names=colnames(pitch_mat))

###Top 15 feature importances are visualized 

xgb.ggplot.importance(pitchfitxg_importance,top_n=15)



###Two data.tables are created, each consisting of 1 artificial pitch
###The characteristics of these pitches are set to the average of a given pitch type (pitch type can be altered)
###fake_pitches2 is designed to allow us to change the value of 1 pitch characteristic whilst holding the other variables constant to fake_pitches1
###This allows us to estimate the effect of a change in this variable, similar to how one would interpret the coefficient in a linear regression
###Note that this is designed as a sanity check rather than a true estimate of that variable's effect
###Interactions are not captured via this process...and complex interactions between characteristics are the driving force behind our choice of model

###Defaults are set to fastball, ignoring handedness and count
###Changes to fake_pitches2 characteristics can be made within the data.table declaration

pitch_type = 'Fastball'

pitcher_handedness_1 = NA
batter_handedness_1 = NA 
balls_1 = NA
strikes_1 = NA

pitcher_handedness_2 = NA
batter_handedness_2 = NA 
balls_2 = NA
strikes_2 = NA

fake_pitches = data.table(PitcherThrows=factor(pitcher_handedness_1,levels=levels(factor(full[,PitcherThrows]))),
                          BatterSide=factor(batter_handedness_1,levels=levels(factor(full[,BatterSide]))),
                          Matchup=factor(ifelse(is.na(pitcher_handedness_1)|is.na(batter_handedness_1),NA,
                                                paste0(pitcher_handedness_1,'-',batter_handedness_1)),levels=levels(factor(full[,Matchup]))),
                          Balls=balls_1,
                          Strikes=strikes_1,
                          Count=factor(ifelse(is.na(balls_1)|is.na(strikes_1),NA,
                                              paste0(balls_1,'-',strikes_1)),levels=levels(factor(full[,Count]))),
                          VertRelAngle=mean(full[AutoPitchType==pitch_type,VertRelAngle],na.rm=TRUE),
                          VertRelAngle=mean(full[AutoPitchType==pitch_type,VertRelAngle],na.rm=TRUE),
                          HorzRelAngleFixed=mean(full[AutoPitchType==pitch_type,HorzRelAngleFixed],na.rm=TRUE),
                          SpinRate=mean(full[AutoPitchType==pitch_type,SpinRate],na.rm=TRUE),
                          SpinAxis=mean(full[AutoPitchType==pitch_type,SpinAxis],na.rm=TRUE),
                          InducedVertBreak=mean(full[AutoPitchType==pitch_type,InducedVertBreak],na.rm=TRUE),
                          HorzBreakFixed=mean(full[AutoPitchType==pitch_type,HorzBreakFixed],na.rm=TRUE),
                          PlateLocHeight=mean(full[AutoPitchType==pitch_type,PlateLocHeight],na.rm=TRUE),
                          PlateLocSideFixed=mean(full[AutoPitchType==pitch_type,PlateLocSideFixed],na.rm=TRUE),
                          RelHeight=mean(full[AutoPitchType==pitch_type,RelHeight],na.rm=TRUE),
                          RelSideFixed=mean(full[AutoPitchType==pitch_type,RelSideFixed],na.rm=TRUE),
                          Extension=mean(full[AutoPitchType==pitch_type,Extension],na.rm=TRUE),
                          EffectiveVelo=mean(full[AutoPitchType==pitch_type,EffectiveVelo],na.rm=TRUE))

fake_pitches2 = data.table(PitcherThrows=factor(pitcher_handedness_2,levels=levels(factor(full[,PitcherThrows]))),
                           BatterSide=factor(batter_handedness_2,levels=levels(factor(full[,BatterSide]))),
                           Matchup=factor(ifelse(is.na(pitcher_handedness_2)|is.na(batter_handedness_2),NA,
                                                 paste0(pitcher_handedness_2,'-',batter_handedness_2)),levels=levels(factor(full[,Matchup]))),
                           Balls=balls_2,
                           Strikes=strikes_2,
                           Count=factor(ifelse(is.na(balls_2)|is.na(strikes_2),NA,
                                               paste0(balls_2,'-',strikes_2)),levels=levels(factor(full[,Count]))),
                           VertRelAngle=mean(full[AutoPitchType==pitch_type,VertRelAngle],na.rm=TRUE),
                           HorzRelAngleFixed=mean(full[AutoPitchType==pitch_type,HorzRelAngleFixed],na.rm=TRUE),
                           SpinRate=mean(full[AutoPitchType==pitch_type,SpinRate],na.rm=TRUE),
                           SpinAxis=mean(full[AutoPitchType==pitch_type,SpinAxis],na.rm=TRUE),
                           InducedVertBreak=mean(full[AutoPitchType==pitch_type,InducedVertBreak],na.rm=TRUE),
                           HorzBreakFixed=mean(full[AutoPitchType==pitch_type,HorzBreakFixed],na.rm=TRUE),
                           PlateLocHeight=mean(full[AutoPitchType==pitch_type,PlateLocHeight],na.rm=TRUE),
                           PlateLocSideFixed=mean(full[AutoPitchType==pitch_type,PlateLocSideFixed],na.rm=TRUE),
                           RelHeight=mean(full[AutoPitchType==pitch_type,RelHeight],na.rm=TRUE),
                           RelSideFixed=mean(full[AutoPitchType==pitch_type,RelSideFixed],na.rm=TRUE),
                           Extension=mean(full[AutoPitchType==pitch_type,Extension],na.rm=TRUE),
                           EffectiveVelo=mean(full[AutoPitchType==pitch_type,EffectiveVelo],na.rm=TRUE))

###NA values are set to a number that will never be seen in the actual data (xgboost does not allow NAs)

fake_pitches[is.na(fake_pitches)] = -1000000
fake_pitches2[is.na(fake_pitches2)] = -1000000

options(na.action='na.pass') ###Needed for factor level NAs

###The predictors are specified for our pitch value model

fake_pitches_mat = sparse.model.matrix(~.-1,fake_pitches[,c('PitcherThrows','BatterSide','Matchup','Balls','Strikes','Count',
                                                            'VertRelAngle','HorzRelAngleFixed','SpinRate','SpinAxis',
                                                            'InducedVertBreak','HorzBreakFixed','PlateLocHeight','PlateLocSideFixed',
                                                            'RelHeight','RelSideFixed','Extension',
                                                            'EffectiveVelo')])

fake_pitches_mat2 = sparse.model.matrix(~.-1,fake_pitches2[,c('PitcherThrows','BatterSide','Matchup','Balls','Strikes','Count',
                                                              'VertRelAngle','HorzRelAngleFixed','SpinRate','SpinAxis',
                                                              'InducedVertBreak','HorzBreakFixed','PlateLocHeight','PlateLocSideFixed',
                                                              'RelHeight','RelSideFixed','Extension',
                                                              'EffectiveVelo')])

###Pitch value predictions are made and compared to estimate the effect of the altered variable 

fake_pitches[,PitchValue:=predict(pitchfitxg,fake_pitches_mat)]
fake_pitches2[,PitchValue:=predict(pitchfitxg,fake_pitches_mat2)]

fake_pitches2[,PitchValue] - fake_pitches[,PitchValue]



###A host of simple linear models are created to compare our model with

###Location model
null1 = lm(PitchxwOBA_through~PlateLocHeight*PlateLocSideFixed,data=pitches_clean)

###Spin rate model
null2 = lm(PitchxwOBA_through~SpinRate + I(SpinRate^2),data=pitches_clean)

###Velocity and break model
null3 = lm(PitchxwOBA_through~EffectiveVelo+HorzBreakFixed+InducedVertBreak+EffectiveVelo*HorzBreakFixed,data=pitches_clean)



###Predictions are made on the training data using each model (including our final model)

pitches_clean[,PitchValue:=predict(pitchfitxg,pitch_mat)]

pitches_clean[,PitchValueNull1:=predict(null1,pitches_clean)]
pitches_clean[,PitchValueNull2:=predict(null2,pitches_clean)]
pitches_clean[,PitchValueNull3:=predict(null3,pitches_clean)]

###In-sample RMSE is calculated for each model and compared to the standard deviation of the dependent variable

sqrt(mean((pitches_clean[,PitchValue]-pitches_clean[,PitchxwOBA_through])^2))

sqrt(mean((pitches_clean[,PitchValueNull1]-pitches_clean[,PitchxwOBA_through])^2))
sqrt(mean((pitches_clean[,PitchValueNull2]-pitches_clean[,PitchxwOBA_through])^2))
sqrt(mean((pitches_clean[,PitchValueNull3]-pitches_clean[,PitchxwOBA_through])^2))

sd(pitches_clean[,PitchxwOBA_through])

######################
###Cross validation###
######################

###data.table to store cross validation results in is initialized

out_of_sample = data.table()

###1000 rounds of cross validation are performed 
###Each round randomly samples 75% of the data for model training, leaving the other 25% to test upon
###Our model and the three simple linear null values are fit on the training data each round
###Predictions from each model are made on the test data each round

for (i in 1:1000)
{
  set.seed(i)   ###A new seed is set each round as to alter the random samples that are created 

  x = sample(length(pitch_lab),.75*length(pitch_lab)) ###75% of our data's indices are sampled at random 

  ###Training set is created based on the sampled indices (multiple forms for differences between lm and xgboost)
  
  pitches_clean_train = pitches_clean[x,]
  pitch_mat_train = pitch_mat[x,]
  pitch_lab_train = pitch_lab[x]
  
  ###Testing set is created based on the sampled indices (multiple forms for differences between lm and xgboost)
  
  pitches_clean_test = pitches_clean[-x,]
  pitch_mat_test = pitch_mat[-x,]
  pitch_lab_test = pitch_lab[-x]
  
  ###xgboost model is fit on the training data
  
  pitchfitxg_temp = xgboost(data = pitch_mat_train,label = pitch_lab_train,objective = 'reg:squarederror',metrics = 'rmse',
                            eta = .01, max_depth = 9, 
                            nrounds = 906, 
                            verbose=TRUE)
  
  ###null models are fit on the training data
  
  null1 = lm(PitchxwOBA_through~PlateLocHeight*PlateLocSideFixed,data=pitches_clean_train)
  null2 = lm(PitchxwOBA_through~SpinRate + I(SpinRate^2),data=pitches_clean_train)
  null3 = lm(PitchxwOBA_through~EffectiveVelo+HorzBreakFixed+InducedVertBreak+EffectiveVelo*HorzBreakFixed,data=pitches_clean_train)
  
  ###Predictions from each model are made on the test data
  
  pitch_values_model = predict(pitchfitxg_temp,pitch_mat_test,reshape=TRUE)
  pitch_values_null1 = predict(null1,pitches_clean_test)
  pitch_values_null2 = predict(null2,pitches_clean_test)
  pitch_values_null3 = predict(null3,pitches_clean_test)
    

  
  pitch_values_tuning_temp = data.table(cbind(i,pitch_values_model,pitch_values_null1,pitch_values_null2,pitch_values_null3,pitch_lab_test))   ###The results of this round are combined into one data.table
  
  out_of_sample = rbind(out_of_sample,pitch_values_tuning_temp) ###This iteration's results are stored in the comprehensive data.table

}

###The out-of-sample RMSE for each of our models is calculated for each round, as well as the standard deviation of the dependent variable in the test set

rmse = out_of_sample[,.(RMSE_model=sqrt(mean((pitch_values_model-pitch_lab_test)^2)),
                        RMSE_null1=sqrt(mean((pitch_values_null1-pitch_lab_test)^2)),
                        RMSE_null2=sqrt(mean((pitch_values_null2-pitch_lab_test)^2)),
                        RMSE_null3=sqrt(mean((pitch_values_null3-pitch_lab_test)^2)),
                        SD=sd(pitch_lab_test)),by='i']

summary(rmse)

###Our model's RMSEs from each round are visualized as to assess the variation in our out-of-sample performance

truehist(rmse[,RMSE_model],prob=FALSE,xlab='RMSE')