
pa<-read.csv("extensions_prearb.csv")
pa.2<-read.csv("extensions_prearb.csv")
head(pa)
pa$Age <-as.numeric(pa$Age)
pa$Minors.OPSx2 <-pa$Minors.OPSx^2
library(MASS)
pa.hitters.2<-pa.2[which(pa.2$Pitcher.Hitter=="H"),]
pa.hitters
pa.hitters$Age
pa.pitchers<-pa[-which(pa$Pitcher.Hitter=="H"),]
pa.pitchers


lm_1h<-lm(AdjAnnVal~Minors.OPSx+Minors.AV, data=pa.hitters)
summary(lm_1h)

lm_1hMajors<-lm(AdjAnnVal~MajorsOPSx+MajorsAV, data=pa.hitters)
summary(lm_1hMajors)
# a 1-point increase in OPS is associated with an increase in AAV by $23,404,
# holding batting average constant.

#a 1-point increase in OPS is associated with an increase in AAV by $20,000.
#Thus, a 50-point increase in OPS is associated with an increase in AAV by $1M

lm_1p<-lm(AdjAnnVal~Minors.ERA+Minors.K.BB, data=pa.pitchers)
summary(lm_1p)
#according to this regression, a 1-point increase in ERA is associated with a 
# $1.51M increase in salary AAV
# There is a negative term on K/BB rate, but it isn't significant.
lm_2p <- lm(AdjAnnVal~Majors.ERA+Majors.K.BB, data=pa.pitchers)
summary(lm_2p)
####################################
lm1a<-lm(AdjAnnVal~Minors.OPSx+Minors.AV+Age+International, data=pa.hitters)
summary(lm1a)

#with inflation adjustments, we actually have more of a relationship.
#a 1-point increase in minors OPS is associated with a $27,997 increase in
#average annual value 


lm2<-lm(AAV~Minors.ERA + Minors.K.BB, data=pa.pitchers)
lm_2.5<-lm(AdjAnnVal~Minors.OPSx+Age+International, data=pa.hitters)
summary(lm_2.5)
pa.hitters$logAAV<-log(pa.hitters$AAV)
pa.pitchers$logAAV<-log(pa.pitchers$AAV)
pa.hitters$logAAV




#Full regression for Hitters

lm_h_adj<-lm(AdjAnnVal~Age+MajorsOPSx+MajorsAV+Minors.OPSx+Minors.AV+International
              +Top3Rounds+Service.Time, data=pa.hitters)

summary(lm_h_full)
summary(lm_h_adj)

#Squared term on age (student suggestion?)
lm_h_age2<-lm(AdjAnnVal~Age^2+MajorsOPSx+MajorsAV+Minors.OPSx+Minors.AV+International
             +Top3Rounds+Service.Time, data=pa.hitters)
summary(lm_h_age2)

lm_p_full<-lm(AdjAnnVal~Age+Majors.ERA+Majors.K.BB+Minors.ERA+Minors.K.BB+International
              +Top3Rounds+Service.Time, data=pa.pitchers)
summary(lm_p_full)

#The above does not follow the 1 in 10 rule...

#here's a regression with just three variables
lm_p_abbrev<-lm(AdjAnnVal~Majors.ERA+Majors.K.BB+Minors.ERA+Minors.K.BB+International
              +Top3Rounds+Service.Time, data=pa.pitchers)


cor(pa.hitters$MajorsAV,pa.hitters$Minors.AV)  = .4829
cor(pa.hitters$MajorsOPSx,pa.hitters$Minors.OPSx) = .5104

lm_h_full_yrs<-lm(Years~Age+MajorsOPSx+MajorsAV+Minors.OPSx+Minors.AV+International
                  +Top3Round+Service.Time, data=pa.hitters)

summary(lm_h_full_yrs)


#####3###### LOGLINEAR FUNCTION
#HITTER
lm_2log<-lm(logAAV~Minors.OPSx+Age+International, data=pa.hitters)
summary(lm_2log)
#PITCHER



mean(pa.pitchers$Years, na.rm=T)
median(pa.pitchers$Years, na.rm=T) 
#Thus, an increase of a player's ERA by 1 point (e.g. from 3.00 to 4.00)
# represents an average of 4.1*1.51 = $6.19M in total earnings across the deal.

#Is team's trust in a player based on his minor-league stats?
lm_yh<-lm(Years~Minors.OPSx, data=pa.hitters)
summary(lm_yh)
range(pa.hitters$Years) # 2 to 8
range(pa.pitchers$Years, na.rm=T) # 2 to 6


##############################
#What proportion of pre-arb pitcher contracts take away FA eligibility?
#What proportion of hitter?
lm_yh<-lm(FA_BLEED~AAV, data=pa.hitters)
summary(lm_yh)

mean(pa.hitters$FA_BLEED) # 76.53%
mean(pa.pitchers$FA_BLEED) # 57.50%

option_diff_P <-mean((pa.pitchers$Years+pa.pitchers$Options), na.rm=T) - 
                mean(pa.pitchers$Years, na.rm=T)
option_diff_H <-mean((pa.hitters$Years+pa.hitters$Options), na.rm=T)-
                mean(pa.hitters$Years, na.rm = T)

option_diff_P # 1.725
option_diff_H # 1.375


#Considerations of Service Time on how many years. Does it remain predictive
#When performance is taken into account?
lm_10<-lm(Years~Age+Service.Time, data=pa)
summary(lm_10)

lm_ager<-lm(Years~Age, data=pa)

#Getting distribution for Case set
library(ggplot2)

#Plot 1: Age
ggplot(pa.hitters.2, aes(Age_Round))+
  geom_histogram(aes(y=..density..),binwidth=1,color="blue", fill="white")+
  geom_density()+
  ggtitle("Case Group: Age Distribution")
  #stat_bin(binwidth=1, geom="text", aes(label=..density..), vjust=-1.5)

#-------------------------------------------------------------------------  
mean(pa.hitters$Age) #25.4027
mode(pa.hitters$Age)
sd(pa.hitters$Age) #2.15319


ctl<-read.csv("sportsecon_control_master.csv")
nrow(ctl)
case_vec<-c("Ozzie Albies","Ronald Acuna Jr.",
            "Alex Bregman",
            "Jose Martinez","Jorge Polanco","Whit Merrifield",
            "Ketel Marte","Paul DeJong","Stephen Piscotty",
            "Rougned Odor","Jose Ramirez","Tim Anderson",
            "Yangervis Solarte","Odubel Herrera","Gregory Polanco",
            "Kolten Wong","Juan Lagares","Brian Dozier","Adam Eaton",
            "Christian Yelich","Jedd Gyorko",
            "Jason Kipnis","Yan Gomes","Mike Trout","Starling Marte",
            "Matt Carpenter","Andrelton Simmons","Jose Altuve",
            "Anthony Rizzo","Paul Goldschmidt","Allen Craig",
          "Carlos Santana","Jonathan Lucroy",
            "Alcides Escobar","Andrew McCutchen","Cameron Maybin",
            "Salvador Perez","Elvis Andrus","Jose Tabata",
            "Ryan Hanigan","Alexei Ramirez","Carlos Gonzalez",
            "Ben Zobrist","Adam Lind","Mark Reynolds",
            "Denard Span","Justin Upton","Nick Markakis",
            "Dustin Pedroia","Hanley Ramirez","Ryan Braun",
            "Evan Longoria","Chris Young","Aaron Hill","Ian Kinsler",
            "Curtis Granderson","Troy Tulowitzki")
ctl_nodups<-ctl[-which(ctl$Name==case_vec),]
nrow(ctl_nodups)

ifelse(case_vec <= ctl$Name,1,0)

#0-1 sd
25.40+1*(2.153) #27.553
25.40-1*(2.153) #23.247
#Let's make it 23,24,25,26,27
#sample .68*64 = 44

ctl_1sd <- ctl[which(ctl$Age>=23 & ctl$Age<=27),]

ctl_64_g1

#1-2 sd
25.40+2*(2.153) #29.706
25.40-2*(2.153) #21.094
#Let's make it  21,22 and 28,29
#sample 64*.27= 17

ctl_2sd <- ctl[which(ctl$Age==21|ctl$Age==22|ctl$Age==28|ctl$Age==29),]

#2-3 sd
25.40+3*(2.153) #31.86
25.40-3*(2.153) #18.941
#Let's make it 18,19,20 and 30,31
#sample 64*.05 = 3

ctl_3sd<- ctl[which(ctl$Age==19|ctl$Age==20|ctl$Age==30|ctl$Age==31|ctl$Age==32),]


ctl_64_g1<-ctl_1sd[sample(nrow(ctl_1sd), 44), ] 
ctl_64_g2 <-ctl_2sd[sample(nrow(ctl_2sd), 17), ] 
ctl_64_g3 <-ctl_3sd[sample(nrow(ctl_3sd), 3), ] 

ctl_64_f<-rbind(ctl_64_g1, ctl_64_g2,ctl_64_g3)

#Create
ctl_64_f$MajorsOPSx<-1000*ctl_64_f$OPS

write.csv(ctl_64_f,"/Users/anthonyargenziano/Desktop/SportsEcon Final Paper\\ctl_64.csv",row.names=F)


#Logistic Regression
#install.packages("aod")
library(aod)
library(ggplot2)
cc<-read.csv("sportsecon_casecontrol.csv")
head(cc)
cc$Case<-factor(cc$Case)

mylogit <- glm(Case ~ MajorsOPSx + MajorsAV,
               data = cc, family = "binomial")

summary(mylogit) #MajorsOPSx is p-value of .0464!

plot(cc$MajorsOPSx,cc$Case)

zeros_ones<-c(rep(0,64),rep(1,64))

final_mtx<-cbind(predict(mylogit,type="response"),zeros_ones)

colnames(final_mtx)<-c("Predicted","Actual")

final_df<-as.data.frame(final_mtx)

final_df[,3]<-ifelse(final_mtx[,1]>=.5,1,0)

final_df[which(final_df[,2]==final_df[,3]),]

sum(ifelse(final_df[,2]==final_df[,3],1,0))/nrow(final_df)
#With 68.75% accuracy, our model predicts whether or not
#the player has had a pre-arbitration extension
#or not.

final_mtx
#Plot 2: Season OPSx
ggplot(pa.hitters, aes(MajorsOPSx))+
  geom_histogram(aes(y=..density..),binwidth=20,
                 color="green", fill="white")+
  geom_density()+
  ggtitle("Case Group: OPSx1000 Distribution")

mean(pa.hitters$MajorsOPSx) #801.0781
sd(pa.hitters$MajorsOPSx) #90.385


#-------------------------------------------------------
#Not assuming a normal distribution
ctl<-read.csv("sportsecon_control_master.csv")
ctl21<-ctl[which(ctl$Age==21),]
ctl21_samp<-ctl21[sample(nrow(ctl21), 1), ] 

ctl22<-ctl[which(ctl$Age==22),]
ctl22_samp<-ctl22[sample(nrow(ctl22), 3), ] 
ctl22_xtra<-ctl22[sample(nrow(ctl22),1),]


ctl23<-ctl[which(ctl$Age==23),]
ctl23_samp<-ctl23[sample(nrow(ctl23), 10), ] 
ctl23_xtra1<-ctl23[sample(nrow(ctl23),1),]
ctl23_xtra1

ctl24<-ctl[which(ctl$Age==24),]
ctl24_samp<-ctl24[sample(nrow(ctl24), 6), ] 
ctl24_xtra<-ctl24[sample(nrow(ctl23),1),]

ctl25<-ctl[which(ctl$Age==25),]
ctl25_samp<-ctl25[sample(nrow(ctl25), 16), ] 
ctl25_xtra3<-ctl25[sample(nrow(ctl25),3),]
ctl25_xtra3

ctl26<-ctl[which(ctl$Age==26),]
ctl26_samp<-ctl26[sample(nrow(ctl26), 13), ] 
ctl26_xtra2<-ctl26[sample(nrow(ctl26), 2), ] 

ctl27<-ctl[which(ctl$Age==27),]
ctl27_samp<-ctl27[sample(nrow(ctl27), 5), ] 
ctl27_xtra2<-ctl27[sample(nrow(ctl27),2),]

ctl28<-ctl[which(ctl$Age==28),]
ctl28_samp<-ctl28[sample(nrow(ctl28), 3), ] 

ctl29<-ctl[which(ctl$Age==29),]
ctl29_samp<-ctl29[sample(nrow(ctl29), 3), ] 

ctl30<-ctl[which(ctl$Age==30),]
ctl30_samp<-ctl30[sample(nrow(ctl30), 2), ] 

ctl31<-ctl[which(ctl$Age==31),]
ctl31_samp<-ctl31[sample(nrow(ctl31), 2), ] 

no_normalcurve<-rbind(ctl21_samp,ctl22_samp,ctl23_samp,
      ctl24_samp,ctl25_samp,ctl26_samp,ctl27_samp,
      ctl28_samp,ctl29_samp,ctl30_samp,ctl31_samp)

write.csv(no_normalcurve,"updatedcc.csv",row.names=F)


weightedages_cc<-read.csv("weightedages_csv.csv")
head(weightedages_cc)



#TRAIN AND TEST GROUPS (80/20 SPLIT)
#function that splits DFs

control_df<-as.data.frame(weightedages_cc[1:64,])
case_df<-as.data.frame(weightedages_cc[65:128,])

set.seed(102)
split_df<-function(df,p) {
  split_index<-sample(1:nrow(df), replace = F, size = nrow(df)*p)
  return(split_index)
}

#Generate the split within the control data
train_index_cl<-split_df(df=control_df,p=.8)
train_control<-control_df[train_index_cl,]
test_control<- control_df[-train_index_cl,]

#Generate the split within the case data
train_index_ca<-split_df(df=case_df,p=.8)
train_case<-case_df[train_index_ca,]
test_case<-case_df[-train_index_ca,]

#_______
# Now, merge the train and test data into one
train_official<-rbind(train_control,train_case)
test_official<-rbind(test_control,test_case)

#Perform Logistic Regression on the Training Set
logit_nonorm <- glm(Case ~ MajorsOPSx + MajorsAV+International+Top3Round,
               data = train_official, family = "binomial")
summary(logit_nonorm)

#And Now Predict Based on Your Test Set

logmodpredict<-predict(logit_nonorm, newdata=test_official, type= "response")
predicted_v_actual<-cbind(logmodpredict,test_official$Case)
colnames(predicted_v_actual)<-c("Predicted","Actual")


#Visualizations
#Visualizes the Fit of the Logistic Regression and its ability to classify our data.
library(pROC)
predicted<-predict(logit_nonorm, test_official, type= "response")
rocplot<-plot(roc(test_official$Case, as.numeric(predicted)), print.auc=T,
              col="red", xlim=c(0,1),main="Probability of Pre-Arb Extension")

mean(test_official$Case) #.828, which is a really excellent fit.




