#providing recommendations to a coach based on trackman data from pitcher

#Read in data

pitcher<-read.csv("player_trackman.csv")
#two dataframes: one with batted ball data, one without
#batted_balls will house just the batted balls in the pitcher dataset
batted_balls<-complete.cases(pitcher)
batted_balls<- pitcher[complete.cases(pitcher),]
batted_balls<-na.omit(pitcher)
#we then reassign the name pitcher to the full original data, minus batted ball data
##equivalent querying techniques in SQL would have also worked.

pitcher <- read.csv("player_trackman.csv")[1:26]
##NOTE: In excel, I converted all NULL to NA (converting from SQL to R-friendly terms)

# Issues with our pitcher
# Create any information or visuals you would give the coach
 ## ---------

# 1. Show that the pitcher does, in fact, throw hard compared to average MLB arm
## extracted from baseballsavant.com
statcast<-read.csv("pitch_arsenals.csv")

means<-NA
for (i in 3:ncol(statcast)){
  means[i]<-round(mean(statcast[,i],na.rm=T,),2)
}
statcastdf<-as.data.frame(rbind(colnames(statcast),means))[2,4:9]
colnames(statcastdf)<-c("Fastball","Sinker","Cutter",
                        "Slider","Changeup","Curveball")
statcastdf
# Now compare to our velocity per pitch type for "Pitcher X"
tapply(pitcher$rel_speed,INDEX=pitcher$tagged_pitch_type, FUN = mean, na.rm= T)

#Figure 1 generated in Excel

#Perform the same thing for average MLB spin rate of each pitch
spinrates<-read.csv("spinrates.csv") #from baseball savant leaderboard

means2<-NA
for (i in 3:ncol(spinrates)){
  means2[i]<-round(mean(spinrates[,i],na.rm=T,),2)
}
spinratesdf<-as.data.frame(rbind(colnames(spinrates),means2))[2,4:9]
colnames(spinratesdf)<-c("Fastball","Sinker","Cutter",
                        "Slider","Changeup","Curveball")
#Now compare these means to our "Pitcher X"
tapply(pitcher$spin_rate,INDEX=pitcher$tagged_pitch_type, FUN = mean, na.rm= T)

#curveball spin rate is uncharacteristically low, especially given its thrown harder
quantile(spinrates$cu_avg_spin,c(.23), na.rm=T) #it sits in 23rd percentile for spin.

#Figure 2
boxplot(spinrates$cu_avg_spin, 
        main="2020: MLB Avg Spin Rate, Curveballs", 
        xlab = "Mean RPM")
points(tapply(pitcher$spin_rate,INDEX=pitcher$tagged_pitch_type, FUN = mean, na.rm= T)[[2]],
       col=10,pch=15)
text(tapply(pitcher$spin_rate,INDEX=pitcher$tagged_pitch_type, FUN = mean, na.rm= T)[[2]]-50,
     "Pitcher X")
text(y=fivenum(spinrates$cu_avg_spin), labels = fivenum(spinrates$cu_avg_spin),x=1.30)


# One hypothesis is that our pitcher is not using the right pitch mix,
  # i.e. he might be using his more effective pitches too infrequently, or vice versa.

#we can find swinging strike rate by pitch type
## pitcher[which(pitcher$tagged_pitch_type=="pitchname"),]
df_by_pitch<-list(0)
swstr_pct<-NA
whiff_rate<-NA
#Develop dfs for each pitch type and then parse frequency tables for cond'l pitch result
for (i in levels(pitcher$tagged_pitch_type)){
  df_by_pitch[[i]]<-pitcher[which(pitcher$tagged_pitch_type==i),]
    #create a list of dataframes segmented by pitch type
  swstr_pct[i]<-round(ftable(df_by_pitch[[i]]$pitch_call)[[6]]/
                        nrow(df_by_pitch[[i]]), 3)
    #for each pitch type dataframe, calculate swinging strike rate above
  whiff_rate[i]<-round(ftable(df_by_pitch[[i]]$pitch_call)[[6]]/
    (ftable(df_by_pitch[[i]]$pitch_call)[[2]]+
       ftable(df_by_pitch[[i]]$pitch_call)[[4]]+
        ftable(df_by_pitch[[i]]$pitch_call)[[6]]),3)
    #and now calculate whiff rate (whiffs divided by total swings)
}
levels(pitcher$tagged_pitch_type)
matrix_A<-cbind(swstr_pct,whiff_rate)[c(-1,-5,-7),]

install.packages("reactable")
library(reactable)
reactable(matrix_A)

#below code is borrowed from https://glin.github.io/reactable/articles/examples.html#theming
options(reactable.theme = reactableTheme(
  color = "hsl(233, 9%, 87%)",
  backgroundColor = "hsl(233, 9%, 19%)",
  borderColor = "hsl(233, 9%, 22%)",
  stripedColor = "hsl(233, 12%, 22%)",
  highlightColor = "hsl(233, 12%, 24%)",
  inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
))

##dataviz code above from  https://glin.github.io/reactable/articles/examples.html#theming

#Finally, we will find out what properties of his curveball make hitters swing at it
##Logistic regression where swing variable gets 1 if yes, 0 if take
levels(pitcher$pitch_call)
head(pitcher)
pitcher$swing<-rep(0,nrow(pitcher))
#Creating the swing variable
pitcher$swing<-ifelse(pitcher$pitch_call == "FoulBall"|
                        pitcher$pitch_call == "StrikeSwinging"|
                          pitcher$pitch_call == "InPlay",
                            1,
                              0)
head(pitcher)
#The curveball, despite its spin deficiency, is generating a ton of whiffs and swstr%
#Therefore, we should examine how it is deployed, which will tell us how he can 
#improve it to generate more swings than it does right now (since that rate is pretty low.)
curves_df<-pitcher[which(pitcher$tagged_pitch_type=="Curveball"),]

nrow(curves_df)/nrow(pitcher) #pitcher throws curve 13.6% of the time.
#we mention this in the beginning of the write-up

#Make test and train indices for cross-validation
set.seed(90)
train80 <-sample(nrow(curves_df), .8*nrow(curves_df))
test20 <- c(1:nrow(curves_df))[-train80]

#logistic regression - right now, focus on the fixed effects model (no interaction terms)
curveswing_logit<-glm(swing~rel_speed+extension+spin_rate+vert_break+strike_prob+horz_break
                      +induced_vert_break+,
                      data=curves_df,
                      family=binomial,
                      subset=train80)
summary(curveswing_logit)


#AIC is a criterion that minimizes deviance while penalizing use of extraneous predictors
library(MASS)
stepAIC(curveswing_logit)

#the variables that survive the AIC-minimized logit make it into our random forest model
#this classifies events as "swings" or "takes", however we can develop
#more in-depth visuals using the pdp package

#Random Forest model using caret()
library(caret)
model_rf2 <- train(swing ~ spin_rate+extension+rel_speed+vert_break+induced_vert_break,
                  data = curves_df,
                  method = "rf",
                  preProcess = c("scale", "center"),
                  verbose=T)

library(pdp) #allows non-linear effects of predictors to be visualized
figure4<-partial(object = model_rf2, pred.var="spin_rate", plot=T, 
        rug=T, which.class = 2, palette = "inferno", 
        parallel = T,plot.engine = "ggplot2", chull = T)

partial(object = model_rf2, pred.var="induced_vert_break", plot=T, 
        rug=T, which.class = 2, palette = "inferno", 
        parallel = T,plot.engine = "ggplot2", chull = T)
