# Term project R template 
# Prof.Pedram Jahangiry 


library(tidyverse)
library(stargazer)
library(car)
library(effects)
library(corrgram)
library(wooldridge)
library(lmtest)

###############################################################################
# Data : We are going to use the dataset beauty!

help(beauty)

summary(beauty)

# If you have Na's, you have to clean the dataset first by using the following code. 

df <- beauty
df <- df[complete.cases(df),]

# if you want to exclude some variables, you can use select function from dplyr package. 

df <- select(df, -c(lwage,looks,union,south, expersq))

# Find the most influential variables by eyeballing the correlogram
corrgram(df, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt,main="Correlations between variables")

# let's look at the structure of our data set:
str(df)



###############################################################################
### basic model

MRM <- lm(wage ~ belavg+abvavg+exper+goodhlth+black+female+married+bigcity+smllcity+service+educ,df)
# or alternatively do lm(wage~ . , df)

stargazer(MRM, type = "text")


hist(resid(MRM), freq = FALSE)
lines(density(resid(MRM), adjust = 2), lty = "dotted", col="red", lwd=3) 

# testing for heteroskedasticity
# if homoskedastic, continue your journey with simple OLS. If heteroskedastic, try to transform the data first. 
# If not possible, remember you have to use robust SE or WLS for your final report. 

bptest(MRM) # bad news. we reject the null: Homoskedastic. 


# Let's try log transformation and see if that works. 
MRM_log <- lm(log(wage) ~ belavg+abvavg+exper+goodhlth+black+female+married+bigcity+smllcity+service+educ,beauty)
stargazer(MRM,MRM_log, type = "text")

# we are going to use the log model because of higher adjusted R square.
hist(resid(MRM_log), freq = FALSE)
lines(density(resid(MRM_log), adjust = 2), lty = "dotted", col="red", lwd=3) 

bptest(MRM_log) # still bad news! we reject the null: Homoskedastic
# Good news! You can still use OLS but you need to report robust standard errors. 



## Robust standard errors
coeftest(MRM_log)
coeftest(MRM_log, vcov= hccm(MRM_log, type="hc0"))





# check for multicollinearity
vif(MRM_log) # another good news


###############################################################################
### quadratic model
MRM_log_quad <- lm(log(wage) ~ belavg+abvavg+exper+I(exper^2)+goodhlth+black+female+married+bigcity+service+smllcity+educ,df)
stargazer(MRM_log,MRM_log_quad, type = "text")

plot(effect("exper", MRM_log_quad)) # it seems that we should drop exper^2. why? counter-intuitive. 



###############################################################################
### Model with interactions

MRM_log_inter <- lm(log(wage) ~ belavg+abvavg+exper+goodhlth+black+female+married+female:married + bigcity+smllcity+educ + service,df)
stargazer(MRM_log, MRM_log_inter, type = "text") # good news
vif(MRM_log_inter) # still doing fine, no sign of multicollinearity

#let's try another interaction

MRM_log_inter2 <- lm(log(wage) ~ belavg+abvavg+exper+goodhlth+black+female+married+female:married + female:exper+ bigcity+smllcity+educ+service,df)
stargazer(MRM_log_inter, MRM_log_inter2, type = "text") # female:exper not significant. So we will stick to MRM_log_inter






###############################################################################
# Hypothesis testing 

# testing if gender matters? 

# using our Heteroskedastic log model
linearHypothesis(MRM_log_inter, c("female=0", "female:married=0"), vcov=hccm(MRM_log_inter,type="hc0")) # Ok, we reject the null. So it seems that gender matters. 

##############################################################################






# now let's work on a LPM model: Linear probability Model

summary(df)

# constructing the dummy dependent variable: EAA (earning above average)
df<- mutate(df, EEA=ifelse((wage>mean(wage)),1,0))

LPM <- lm(EEA ~ belavg+abvavg+exper+goodhlth+black+female+married+female:married + bigcity+smllcity+educ ,df)
stargazer(LPM, type = "text")

# we know that this model violates homoskedasticity assumption. why?
bptest(LPM)

# So we need to correct for standard errors or use WLS (because the functional form of heteroskedasticity is known)

#WLS steps
y_hat <- predict(LPM)
summary(y_hat) # this suggests that LPM is not the best approach. 
h     <- y_hat * (1-y_hat)
range(h) # So we need to force h>0

h<- ifelse(h<0,0.01,h)
summary(h)

w<- 1/h
LPM_wls <- lm(EEA ~ belavg+abvavg+exper+goodhlth+black+female+married+female:married + bigcity+smllcity+educ , weights = w ,df)
stargazer(LPM, LPM_wls, type = "text")

y_hat_wls <- predict(LPM_wls)
summary(y_hat_wls)

Confusion_Matrix <- table(df[, "EEA"], predict(LPM_wls) >= 0.5)
prop.table(Confusion_Matrix,1)
(Percent_correctly_predicted_overall <- (sum(y_hat_wls >= 0.5 & df$EEA==1) + sum(y_hat_wls <= 0.5 & df$EEA==0)) / length(df$EEA)) 



# Done! 








# some additional codes FYI
# Redefining dummy variables

df <- mutate(happiness, Ishappy=ifelse((happy=="very happy"|happy=="pretty happy"),1,0) , income_gt25k=ifelse((income=="$25000 or more"),1,0), Isreligious=ifelse((attend=="never"),0,1))
df <- df[,c("Ishappy","income_gt25k", "Isreligious", "prestige","divorce","educ","babies","preteen","teens","mothfath16","black","female","blackfemale","unem10")]
corrgram(df, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt,main="Correlations between variables")






### FGLS (optional)
MRM <- lm(price~ bdrms+lotsize+sqrft, hprice1 )
qqnorm(resid(MRM), pch = 1, frame = FALSE)
qqline(resid(MRM), col = "red", lwd = 2)

MRM_log <- lm(lprice~ bdrms+lotsize+sqrft, hprice1 )
qqnorm(resid(MRM_log), pch = 1, frame = FALSE)
qqline(resid(MRM_log), col = "red", lwd = 2)

stargazer(MRM,MRM_log, type="text")
bptest(MRM)
bptest(MRM_log)


# FGLS steps
uhat <- resid(MRM)
logu2 <- log(uhat^2)

reg_step2 <- lm(logu2~ bdrms+lotsize+sqrft,hprice1)
ghat <- predict(reg_step2)
hhat <- exp(ghat)
w<- 1/hhat


MRM_FGLS <- lm(price~ bdrms+lotsize+sqrft, weights = w, hprice1 )
stargazer(MRM, MRM_FGLS, MRM_log,type="text")


coeftest(MRM) 
coeftest(MRM,vcov= hccm(MRM,type = "hc0"))



