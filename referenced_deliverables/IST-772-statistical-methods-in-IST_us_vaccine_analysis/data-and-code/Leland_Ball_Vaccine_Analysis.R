# Leland Ball
# 2020-09-14
# IST-722 Statistical Methods in IST
# US Vaccine Analysis

# package installer, thanks Matthew(username) from S.O.
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}
using("changepoint","MCMCpack","BEST","BaylorEdPsych", "BayesFactor", "TSA", "psych", "Hmisc", "corrplot", "car")


# library("changepoint")
# library("MCMCpack")
# library("BEST")
# library("BaylorEdPsych") # for pseudo-r^2 
# library("BayesFactor")
# library("TSA") # for time-series analysis
# library("psych") # for "describe" function (use psych::describe)
# library("Hmisc") # correlations and such
# library("corrplot") # correlations and such
# library("car") # vif function for detection of multicollinearity

# usVaccines.Rdata - Time series data from the World Health Organization reporting vaccination rates in the U.S. for five common vaccines
# DTP1 = First dose of Diphtheria/Pertussis/Tetanus vaccine; 
# HepB_BD = Hepatitis B, Birth Dose; 
# Pol3 = Polio third dose; 
# Hib3 - Influenza third dose; 
# MCV1 = Measles first dose

# allSchoolsReportStatus.RData - A list of California kindergartens and whether they reported vaccination data to the state in 2013
#'data.frame':	7381 obs. of  3 variables:
#  $ name    : Name of the school
#$ pubpriv : "PUBLIC" or "PRIVATE"
#$ reported: "Y" or "N"


# districtsX.RData - A sample of California public school districts from the 2013 data collection, along with specific numbers and 
# percentages for each district: 
# 'data.frame':	700 obs. of  13 variables:
# DistrictName    : Name of the district
# WithoutDTP      : Percentage of students without the DTP vaccine
# WithoutPolio    : Percentage of students without the Polio vaccine
# WithoutMMR      : Percentage of students without the MMR vaccine
# WithoutHepB     : Percentage of students without the Hepatitis B vaccine
# PctUpToDate     : Percentage of all enrolled students with completely up-to-date vaccines
# DistrictComplete: Boolean indicating whether or not the district's reporting was complete
# PctBeliefExempt : Percentage of all enrolled students with belief exceptions
# PctChildPoverty : Percentage of children in the district living below the poverty line
# PctFreeMeal     : Percentage of children in the district eligible for free student meals
# PctFamilyPoverty: num  Percentage of families in the district living below the poverty line
# Enrolled        : Total number of enrolled students in the district
# TotalSchools    : Total number of different schools in the district

############ Loading Data ##########
#setwd("C:/Users/Leland/Desktop/school/IST-772-Statistical-Methods-in-IST/final")
usv <- get(load("usVaccines.RData"))
allSchools <- get(load("allSchoolsReportStatus.RData"))
districts <- get(load("districts3.RData"))


############ Supporting Functions ###########
# HDI plot

hdi_plot <- function(mcmcOut, col_name) {
  par(mfcol=c(1,1))
  hist(mcmcOut[,col_name], 
       main=paste("95% HDI for", col_name),
       xlab=paste("Priors for", col_name))
  abline(v=quantile(mcmcOut[,col_name], c(0.025)), col="black")
  abline(v=quantile(mcmcOut[,col_name], c(0.975)), col="black")
  print(quantile(mcmcOut[,col_name], c(0.025)))
  print(quantile(mcmcOut[,col_name], c(0.975)))
  print(paste("Point estimate for", col_name, "=", mean(mcmcOut[,col_name])))
}

# convert log-odds to odds
realizeTheOdds <- function(mcmcLogitOut, coefficientName){
  logOdds <- as.matrix(mcmcLogitOut[,coefficientName])
  odds <- apply(logOdds, 1, exp)
  hist(odds, main=paste("Histogram of odds for ", coefficientName, sep=""))
  abline(v=1, col="blue")
  abline(v=quantile(odds, c(0.025)), col="red")
  abline(v=quantile(odds, c(0.975)), col="red")
}

# returns just the real odds x:1 that there is a correlation
bfCorTest <- function (name_a, name_b){
  x <- districts[, name_a]
  y <- districts[, name_b]
  zx <- scale(x) # Standardize X
  zy <- scale(y) # Standardize Y
  zData <- data.frame(x=zx,rhoNot0=zy) # Put in a data frame
  bfOut <- generalTestBF(x ~ rhoNot0, data=zData) # linear coefficient
  mcmcOut <- posterior(bfOut,iterations=10000) # posterior samples
  #print(summary(mcmcOut[,"rhoNot0"])) # Get the HDI for rho
  return(paste(name_a, " & ", name_b, " = ", 
               extractBF(bfOut, logbf=FALSE, onlybf=TRUE), 
               " point est = ", mean( mcmcOut[, "rhoNot0"] ), 
               sep="") )
  #return(bfOut) # Return Bayes factor object
  #return(mcmcOut)
}

plotWithNormCurve <- function(data, main=""){
  hist_plot <- hist(data, breaks=10, density=5, main=main) 
  fit_x <- seq(min(data), max(data), length=40) 
  fit_y <- dnorm(fit_x, sd=sd(data), mean=mean(data)) 
  fit_y <- fit_y * diff(hist_plot$mids[1:2]) * length(data) 
  lines(fit_x, fit_y, col="black", lwd=3)
}

# takes dataframe with "date" column and plots the given column with smoothed trend line
plot_trend <- function(data, col_name){
  ggp <- ggplot(data=data, aes_string(x="date", y=col_name)) + 
    geom_line(color="#00ADBB", size=1)
  ggp + stat_smooth(color="#FC3E07", fill="#FC3E07", method="loess", se=FALSE)
}

############ DESCRIPTIVE STATISTICS #############
### How have U.S. vaccination rates varied over time?
plot(usv, main="US Vaccination Rates by Year by Vaccine")
boxplot(usv, main="Vaccination Rates by Vaccine")

### Are vaccination rates increasing or decreasing?
usv_df <- data.frame(usv)
usv_df$date <- as.Date(time(usv))
plot_trend(usv_df, "DTP1")
plot_trend(usv_df, "HepB_BD")
plot_trend(usv_df, "Pol3")
plot_trend(usv_df, "Hib3")
plot_trend(usv_df, "MCV1")

# dependent samples t-test (one-sample test on differences)
usv_start <- window(usv, start=1980, end=1980)[1,]
usv_end <- window(usv, start=2017, end=2017)[1,]
usv_rates_df <- data.frame(vaccine=c('DTP1', 'HepB_BD', 'Pol3', 'Hib3', 'MCV1'), start=usv_start, end=usv_end, diff=usv_end-usv_start)
diff_t <- t.test(usv_rates_df$diff)
diff_t
# One Sample t-test
# 
# data:  usv_rates_df$diff
# t = 1.7701, df = 4, p-value = 0.1514
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -8.641889 39.041889
# sample estimates:
#   mean of x 
# 15.2

best_out <- BESTmcmc(usv_rates_df$diff)
summary(best_out)
# mean median   mode HDI%   HDIlo HDIup compVal %>compVal
# mu      14.439  13.79 12.831   95 -14.166 46.32       0        89
# sigma   28.356  23.18 18.894   95   4.902 62.76                  
# nu      30.402  21.41  5.544   95   1.001 88.78                  
# log10nu  1.280   1.33  1.465   95   0.333  2.09                  
# effSz    0.646   0.63  0.634   95  -0.410  1.71       0        89
hdi(best_out)
# mu        nu     sigma
# lower -14.16643  1.000551  4.902383
# upper  46.31538 88.777331 62.756263
# attr(,"credMass")
# [1] 0.95





### Which vaccination has the highest rate at the conclusion of the time series?
usv[nrow(usv), ]
# that would be DTP1
# DTP1 HepB_BD    Pol3    Hib3    MCV1 
# 98      64      94      93      92 


### Which vaccination has the lowest rate at the conclusion of the time series?
usv[nrow(usv), ]
# that would be HepB_BD
# DTP1 HepB_BD    Pol3    Hib3    MCV1 
# 98      64      94      93      92 



### Which vaccine has the greatest volatility? 
psych::describe(usv)
#vars          n mean   sd     median trimmed  mad min max range skew  kurtosis se
#DTP1       1 38 94.05  5.87     97   94.69 2.97  81  99    18 -1.01    -0.64 0.95
#HepB_BD    2 38 34.21 22.54     19   32.78 4.45  11  74    63  0.61    -1.40 3.66
#Pol3       3 38 87.16 15.35     93   90.31 3.71  24  97    73 -2.52     6.16 2.49
#Hib3       4 38 89.21  7.14     91   90.28 2.97  52  94    42 -3.70    16.55 1.16
#MCV1       5 38 91.24  4.19     92   91.44 1.48  82  98    16 -0.40    -0.10 0.68

psych::describe(diff(usv))
#vars          n  mean  sd     median  trimmed mad min max range  skew kurtosis   se
#DTP1       1 37  0.41  2.44      0    0.29 1.48  -8  11    19  1.12    10.41 0.40
#HepB_BD    2 37  1.30  4.16      1    1.10 1.48  -8  18    26  1.35     5.81 0.68
#Pol3       3 37 -0.03 18.76      0    0.52 1.48 -73  73   146 -0.21     9.92 3.08
#Hib3       4 37  0.22  8.35      0    0.19 1.48 -34  35    69  0.09    12.80 1.37
#MCV1       5 37  0.16  4.76      0    0.19 1.48 -15  16    31  0.07     4.76 0.78



### 2.	What proportion of public schools reported vaccination data? 
pct_reported_pu <- nrow(allSchools[allSchools$reported=="Y" & allSchools$pubpriv=="PUBLIC",]) / nrow(allSchools)
# 75.7
pie(c(pct_reported_pu, 1-pct_reported_pu), 
    labels=c(paste(round(pct_reported_pu*100, 1), "% reported", sep=""), 
             paste(round((1-pct_reported_pu)*100, 1), "% not reported", sep="")),
    col=hcl.colors(2, palette = "viridis"),
    main="Number of Public Schools Reporting Vaccination Data")


### What proportion of private schools reported vaccination data? 
pct_reported_pr <- nrow(allSchools[allSchools$reported=="Y" & allSchools$pubpriv=="PRIVATE",]) / nrow(allSchools)
# 18.9
pie(c(pct_reported_pr, 1-pct_reported_pr), 
    labels=c(paste(round(pct_reported_pr*100, 1), "% reported", sep=""), 
             paste(round((1-pct_reported_pr)*100, 1), "% not reported", sep="")),
    col=hcl.colors(2, palette = "viridis"),
    main="Number of Private Schools Reporting Vaccination Data")

### Was there any credible difference in overall reporting proportions between public and private schools?
allSchools$pub_num <- as.numeric(allSchools$pubpriv=="PUBLIC")
allSchools$reported_num <- as.numeric(allSchools$reported=="Y")
# make a factor version of the variables for MCMClogit and others
allSchools$reported <- factor(allSchools$reported)
allSchools$pubpriv <- factor(allSchools$pubpriv)

# frequentist first
# H0: both groups were sampled from the same population
# Ha: The groups come from different populations
pubpriv_prop <- aov(reported_num ~ pubpriv, allSchools)
summary(pubpriv_prop)
# Df Sum Sq Mean Sq F value Pr(>F)    
# pubpriv        1   20.7  20.655   426.1 <2e-16 ***
# Residuals   7379  357.7   0.048  
# Large value for F, and significant p=<2e-16 < a=0.05 cause us to reject
# the Null Hypothesis. These groups come from different populations, implying
# that public and private schools have credible differences between them when it 
# comes to overall reporting proportions

# Bayesian next
pubpriv_prop_bf <- anovaBF(reported_num ~ pubpriv, allSchools)
anova_posteriors <- posterior(pubpriv_prop_bf, iterations=10000)
summary(pubpriv_prop_bf)
# Bayes factor analysis
# --------------
#   [1] pubpriv : 1.770577e+88 ±0%
hdi_plot(anova_posteriors, "mu")
# 95% HDI: 0.90 to 0.92 
# shows a very-likely significance in the Bayes Factor Analysis
# 95% HDI does not overlap 0, and coefficient point-estimate is 0.91 corroborating the Frequentist
# findings that there is a statistically significant difference between these two groups
# 2.5% 
# 0.9048302 
# 97.5% 
# 0.9167262 
# [1] "Point estimate for mu = 0.910742248746231"



### 3.	What are 2013 vaccination rates for individual vaccines (i.e., DOT, Polio, MMR, and HepB) 
# in California public schools? 
100 - colMeans(districts[c('WithoutDTP', 'WithoutHepB', 'WithoutPolio', 'WithoutMMR')])
# DTP          HepB      Polio        MMR           
# 89.70429     92.19429  90.12429     89.76714      


### How do these rates for individual vaccines in California districts compare with overall US vaccination 
# rates (make an informal comparison to the final observations in the time series)? 
window(usv, start=2013, end=2013)
# this is across all the USA:
#       DTP1 HepB_BD Pol3 Hib3 MCV1
#2013   98      74   93   93   92
window(usv, start=2017, end=2017)
#       DTP1 HepB_BD Pol3 Hib3 MCV1
#2017   98      64   94   93   92
# informally, the 2013 mean California Public School vaccination rates were under the US average rates for the same year
# for DTP1/DTP, Polio/Pol3, MMR/MCV1
# and over average for HepB/HepB_BD
# This remains true for the US average vaccination rates in 2017
# Informally, CA has a much higher rate of HepB/HepB_BD vaccination than the US average



### 4.	Among districts, how are the vaccination rates for individual vaccines related? 
### In other words, if students are missing one vaccine are they missing all of the others?
districts_just_vac <- districts[c('WithoutDTP', 'WithoutHepB', 'WithoutPolio', 'WithoutMMR')]
districts_cor <- cor(districts_just_vac)
districsts_cor_p <- rcorr(as.matrix(districts_just_vac))$P
pairs(districts_just_vac, upper.panel=NULL)
corrplot(districts_cor, 
         type="lower", 
         method="pie", 
         sig.level=0.05,
         tl.col="blue",
         insig="blank",
         title="")
districts_cor
# WithoutDTP WithoutHepB WithoutPolio WithoutMMR
# WithoutDTP    1.0000000   0.8905016    0.9817876  0.9784040
# WithoutHepB   0.8905016   1.0000000    0.9057329  0.8968082
# WithoutPolio  0.9817876   0.9057329    1.0000000  0.9718178
# WithoutMMR    0.9784040   0.8968082    0.9718178  1.0000000
# ^highly correlated at or above a significance level of a=0.05
# if a student is missing one vaccine, they appear to miss all of the others


#combinations <- expand.grid(colnames(districts_just_vac), colnames(districts_just_vac))
bfCorTest("WithoutHepB", "WithoutDTP")
bfCorTest("WithoutPolio",   "WithoutDTP")
bfCorTest("WithoutMMR",   "WithoutDTP")
bfCorTest("WithoutPolio",  "WithoutHepB")
bfCorTest("WithoutMMR",  "WithoutHepB")
bfCorTest("WithoutMMR", "WithoutPolio")
# b <- bfCorTest(districts_just_vac$WithoutDTP, districts_just_vac$WithoutHepB)




############ PREDICTIVE STATISTICS #############




# (For all of these analyses, use PctChildPoverty, PctFreeMeal, PctFamilyPoverty, Enrolled,  
# and TotalSchools as predictors. Transform variables as necessary to improve prediction and/or 
# interpretability. In general, if there is a Bayesian version of an analysis available, you are 
# expected to run that analysis in addition to the frequentist version of the analysis.)

### explore how well each variable fits a visual normal curve
# sqrt, log, 1/x
plotWithNormCurve(districts$PctChildPoverty, "PctChildPoverty") # fairly normal. chi-sq-esque, skewed right
  plotWithNormCurve(sqrt(districts$PctChildPoverty), "sqrt(PctChildPoverty)") # nice'n'normal
plotWithNormCurve(districts$PctFreeMeal, "PctFreeMeal") # blob. vaguelly normal
  #plotWithNormCurve(scale(districts$PctFreeMeal), "sqrt(PctFreeMeal)")
plotWithNormCurve(districts$PctFamilyPoverty, "PctFamilyPoverty") # chi-sq, decent skew right
  plotWithNormCurve(sqrt(districts$PctFamilyPoverty), "sqrt(PctFamilyPoverty)") # nice'n'normal
plotWithNormCurve(districts$Enrolled, "Enrolled") # highly skewed right
  plotWithNormCurve(log(districts$Enrolled), "log(Enrolled)") # vaguelly normal
plotWithNormCurve(districts$TotalSchools, "TotalSchools") # highly skewed right. probably not fixable
  plotWithNormCurve(1/districts$TotalSchools, "1/TotalSchools")
# apply transforms to new copy of data
districts_t <- districts
districts_t$PctChildPoverty <- scale(sqrt(districts_t$PctChildPoverty))
# no transform for PctFreeMeal
#districts_t$PctFreeMeal <- scale(districts_t$PctFreeMeal) #TODO: <----------REMOVE ME
districts_t$PctFamilyPoverty <- scale(sqrt(districts_t$PctFamilyPoverty))
districts_t$Enrolled <- scale(log(districts_t$Enrolled))
districts_t$TotalSchools <- scale(1/districts_t$TotalSchools)

####TODO: REMOVE ME #######
#plotWithNormCurve(districts_t$PctChildPoverty, "PctChildPoverty") # fairly normal. chi-sq-esque, skewed right
#plotWithNormCurve(districts_t$PctFreeMeal, "PctFreeMeal") # blob. vaguelly normal
#plotWithNormCurve(districts_t$PctFamilyPoverty, "PctFamilyPoverty") # chi-sq, decent skew right
#plotWithNormCurve(districts_t$Enrolled, "Enrolled") # highly skewed right
#plotWithNormCurve(districts_t$TotalSchools, "TotalSchools") # highly skewed right. probably not fixable
####TODO: REMOVE ME #######


# some of these percents kinda go "together" like belief exemption and completion percent... 
# is anything else correlated with other things????
r <- districts_t[,c("PctChildPoverty", "PctFreeMeal", "PctFamilyPoverty", "Enrolled", "TotalSchools")]
r$PctChildPoverty <- as.numeric(r$PctChildPoverty)
r$PctFreeMeal <- as.numeric(districts_t$PctFreeMeal)
r$PctFamilyPoverty <- as.numeric(r$PctFamilyPoverty)
r$Enrolled <- as.numeric(r$Enrolled)
r$TotalSchools <- as.numeric(r$TotalSchools)
rcorr(as.matrix(r))
pairs(r, upper.panel=NULL)
corrplot(cor(r), 
         type="lower", 
         method="pie", 
         sig.level=0.05,
         tl.col="blue",
         insig="blank",
         title="")

# YES: PctChildPoverty and PctFreeMeal and PctFamilyPoverty
# ^ both according to the p-values and by strength of association
# YES: also Enrolled and TotalSchools (inversely)



### 5.	What variables predict whether or not a district's reporting was complete?
districts_t$DistrictComplete <- as.numeric(districts_t$DistrictComplete) # convert DistrictComplete to numeric
DistrictComplete_glmout <- glm(DistrictComplete ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools,
                               districts_t,
                               family=binomial(link="logit"))
summary(DistrictComplete_glmout)
# Call:
#   glm(formula = DistrictComplete ~ PctChildPoverty + PctFreeMeal + 
#         PctFamilyPoverty + Enrolled + TotalSchools, family = binomial(link = "logit"), 
#       data = districts_t)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.4573   0.0209   0.1804   0.4231   0.9741  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      -7.40192    2.56867  -2.882  0.00396 ** 
# PctChildPoverty   0.29962    0.42373   0.707  0.47950    
# PctFreeMeal      -0.02015    0.01380  -1.460  0.14435    
# PctFamilyPoverty -0.48150    0.42582  -1.131  0.25816    
# Enrolled          1.38359    0.32176   4.300 1.71e-05 ***
# TotalSchools     11.90296    2.70934   4.393 1.12e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 328.66  on 699  degrees of freedom
# Residual deviance: 255.22  on 694  degrees of freedom
# AIC: 267.22
# 
# Number of Fisher Scoring iterations: 9

# H0: term has no effect on predicting DistrictComplete
# Ha: term has an effect on predicting DistrictComplete (they are not independent)
anova(DistrictComplete_glmout,  test="Chisq")
# p<a=0.05 for Enrolled, TotalSchools, and on occasion: PctFreeMeal. Reject H0 for these terms, 
# this implies that Enrolled, TotalSchools, and on occasion: PctFreeMeal have significant effects on DistrictComplete.
PseudoR2(DistrictComplete_glmout)
# McFadden     Adj.McFadden        Cox.Snell       Nagelkerke McKelvey.Zavoina           Effron            Count        Adj.Count 
# 0.22346959       0.18087248       0.09960563       0.26583136       0.74756273       0.15958986       0.94142857       0.06818182 
# AIC    Corrected.AIC 
# 267.21512650     267.33633862
vif(DistrictComplete_glmout)
# values of over 5 or 10 indicate problematic multicollinearity 
# PctChildPoverty      PctFreeMeal PctFamilyPoverty         Enrolled     TotalSchools 
# 8.979757         3.188573         7.267617         4.189732         4.023604 

# One predictor logistic with Bayesian estimation
bayesLogitOut <- MCMClogit(DistrictComplete ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools, districts_t)
summary(bayesLogitOut)
summary(exp(bayesLogitOut))
# real odds...
# Iterations = 1001:11000
# Thinning interval = 1 
# Number of chains = 1 
# Sample size per chain = 10000 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#   Mean        SD  Naive SE Time-series SE
# (Intercept)      5.526e-03 2.144e-02 2.144e-04      6.504e-04
# PctChildPoverty  1.451e+00 6.169e-01 6.169e-03      2.582e-02
# PctFreeMeal      9.791e-01 1.449e-02 1.449e-04      6.866e-04
# PctFamilyPoverty 6.836e-01 2.723e-01 2.723e-03      1.101e-02
# Enrolled         4.538e+00 1.559e+00 1.559e-02      7.128e-02
# TotalSchools     2.266e+07 2.684e+08 2.684e+06      5.529e+06
# 
# 2. Quantiles for each variable:
#   2.5%       25%       50%       75%     97.5%
# (Intercept)      1.593e-06 6.415e-05 4.332e-04 1.869e-03 5.745e-02
# PctChildPoverty  6.095e-01 1.022e+00 1.348e+00 1.712e+00 3.055e+00
# PctFreeMeal      9.512e-01 9.691e-01 9.787e-01 9.890e-01 1.007e+00
# PctFamilyPoverty 2.753e-01 4.937e-01 6.376e-01 8.437e-01 1.295e+00
# Enrolled         2.273e+00 3.448e+00 4.251e+00 5.351e+00 8.382e+00
# TotalSchools     2.121e+03 4.748e+04 2.949e+05 2.020e+06 1.593e+08
plot(bayesLogitOut)
realizeTheOdds(bayesLogitOut, "PctChildPoverty")
realizeTheOdds(bayesLogitOut, "PctFreeMeal")
realizeTheOdds(bayesLogitOut, "PctFamilyPoverty")
realizeTheOdds(bayesLogitOut, "Enrolled")
realizeTheOdds(bayesLogitOut, "TotalSchools")

# NOTE: upon removing PctChildPoverty and PctFamilyPoverty, the model found additional significance to PctFreeMeal



### 6.	What variables predict the percentage of all enrolled students with completely up-to-date vaccines?
districts_t$PctUpToDate_binary <- as.numeric(districts_t$PctUpToDate == 100) # convert PctUpToDate to binary numeric
PctUpToDate_binary_glmout <- glm(PctUpToDate_binary ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools, districts_t,
                               family=binomial(link="logit"))
summary(PctUpToDate_binary_glmout)
# Call:
#   glm(formula = PctUpToDate_binary ~ PctChildPoverty + PctFreeMeal + 
#         PctFamilyPoverty + Enrolled + TotalSchools, family = binomial(link = "logit"), 
#       data = districts_t)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -1.27203  -0.33780  -0.16098  -0.08619   2.85379  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      -2.18439    1.58707  -1.376 0.168708    
# PctChildPoverty  -0.08116    0.24516  -0.331 0.740621    
# PctFreeMeal       0.02279    0.01036   2.201 0.027751 *  
# PctFamilyPoverty  0.35203    0.24800   1.420 0.155751    
# Enrolled         -0.75956    0.22411  -3.389 0.000701 ***
# TotalSchools      0.82962    0.93839   0.884 0.376649    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 301.01  on 699  degrees of freedom
# Residual deviance: 230.69  on 694  degrees of freedom
# AIC: 242.69
# 
# Number of Fisher Scoring iterations: 7

# H0: term has no effect on predicting PctUpToDate
# Ha: term has an effect on predicting PctUpToDate (they are not independent)
# significance of PctFreeMeal and Enrolled (reject H0 for these two)
anova(PctUpToDate_binary_glmout,  test="Chisq")
# p<a=0.05 for Enrolled, PctChildPoverty. Reject H0 for these terms, 
PseudoR2(PctUpToDate_binary_glmout)
# McFadden     Adj.McFadden        Cox.Snell       Nagelkerke McKelvey.Zavoina           Effron            Count        Adj.Count 
# 0.23362698       0.18711722       0.09558200       0.27348044       0.46138775       0.11889505       0.94285714      -0.02564103 
# AIC    Corrected.AIC 
# 242.68754129     242.80875342 

vif(PctUpToDate_binary_glmout) # greater than 5 or 10 indicates confounding multicollinearity
# PctChildPoverty      PctFreeMeal PctFamilyPoverty         Enrolled     TotalSchools 
# 3.121810         1.796195         3.236680         2.059132         1.978777 

# One predictor logistic with Bayesian estimation
bayesLogitOut <- MCMClogit(PctUpToDate_binary ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools, districts_t)
summary(bayesLogitOut)
summary(exp(bayesLogitOut))
# Iterations = 1001:11000
# Thinning interval = 1 
# Number of chains = 1 
# Sample size per chain = 10000 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#   Mean      SD  Naive SE Time-series SE
# (Intercept)      0.3190 0.74673 0.0074673      0.0249711
# PctChildPoverty  0.9335 0.23526 0.0023526      0.0097560
# PctFreeMeal      1.0234 0.01087 0.0001087      0.0004827
# PctFamilyPoverty 1.5204 0.39574 0.0039574      0.0176869
# Enrolled         0.4727 0.10543 0.0010543      0.0046507
# TotalSchools     4.0092 4.61222 0.0461222      0.2094477
# 
# 2. Quantiles for each variable:
#   
#   2.5%     25%     50%    75%   97.5%
# (Intercept)      0.004776 0.03205 0.09982 0.2960  1.9219
# PctChildPoverty  0.550254 0.76950 0.90044 1.0741  1.4403
# PctFreeMeal      1.002205 1.01609 1.02312 1.0310  1.0443
# PctFamilyPoverty 0.903079 1.23592 1.45474 1.7620  2.4486
# Enrolled         0.296600 0.39682 0.46249 0.5413  0.7048
# TotalSchools     0.366498 1.32550 2.59330 4.7736 16.9361

plot(bayesLogitOut)
realizeTheOdds(bayesLogitOut, "PctChildPoverty")
realizeTheOdds(bayesLogitOut, "PctFreeMeal")
realizeTheOdds(bayesLogitOut, "PctFamilyPoverty")
realizeTheOdds(bayesLogitOut, "Enrolled")
realizeTheOdds(bayesLogitOut, "TotalSchools")



### 7.	What variables predict the percentage of all enrolled students with belief exceptions?
lmRes <- lm(PctBeliefExempt ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools, districts_t)
summary(lmRes)
# Call:
#   lm(formula = PctBeliefExempt ~ PctChildPoverty + PctFreeMeal + 
#        PctFamilyPoverty + Enrolled + TotalSchools, data = districts_t)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -15.902  -4.016  -1.167   1.408  46.306 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      24.61945    2.89605   8.501  < 2e-16 ***
# PctChildPoverty   0.96209    0.49825   1.931  0.05390 .  
# PctFreeMeal      -0.10125    0.02009  -5.039 5.98e-07 ***
# PctFamilyPoverty -0.93656    0.50250  -1.864  0.06277 .  
# Enrolled         -2.44756    0.38550  -6.349 3.92e-10 ***
# TotalSchools     -4.92372    1.51744  -3.245  0.00123 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 7.813 on 694 degrees of freedom
# Multiple R-squared:  0.1836,	Adjusted R-squared:  0.1777 
# F-statistic: 31.21 on 5 and 694 DF,  p-value: < 2.2e-16
vif(lmRes)
# PctChildPoverty      PctFreeMeal PctFamilyPoverty         Enrolled     TotalSchools 
# 4.673514         2.828160         4.062062         4.330683         4.274759 

lmBFRes <- lmBF(PctBeliefExempt ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools, districts_t)
summary(lmBFRes)
# Bayes factor analysis
# --------------
#   [1] PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools : 2.52162e+25 ±0%
# 
# Against denominator:
#   Intercept only 
# ---
#   Bayes factor type: BFlinearModel, JZS

lmBFResPosteriors <- lmBF(PctBeliefExempt ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools, districts_t,
                          posterior=TRUE, iterations=10000)
# summary(lmBFResPosteriors)
# Iterations = 1:10000
# Thinning interval = 1 
# Number of chains = 1 
# Sample size per chain = 10000 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#   Mean      SD  Naive SE Time-series SE
# mu                5.69300 0.29367 0.0029367      0.0028351
# PctChildPoverty   0.93643 0.49113 0.0049113      0.0049113
# PctFreeMeal      -0.09899 0.01955 0.0001955      0.0001955
# PctFamilyPoverty -0.90910 0.49828 0.0049828      0.0049828
# Enrolled         -2.38938 0.38277 0.0038277      0.0038277
# TotalSchools     -4.80744 1.50445 0.0150445      0.0150445
# sig2             61.13057 3.29719 0.0329719      0.0329719
# g                 0.08577 0.07965 0.0007965      0.0008174
# 
# 2. Quantiles for each variable:
#   
#   2.5%      25%      50%      75%    97.5%
# mu                5.12438  5.49251  5.69235  5.88847  6.27869
# PctChildPoverty  -0.01969  0.60325  0.93652  1.26843  1.91067
# PctFreeMeal      -0.13777 -0.11195 -0.09915 -0.08574 -0.06059
# PctFamilyPoverty -1.87469 -1.24717 -0.91030 -0.57253  0.07466
# Enrolled         -3.13909 -2.64554 -2.39009 -2.13118 -1.63417
# TotalSchools     -7.78450 -5.83429 -4.80495 -3.80047 -1.84111
# sig2             55.02549 58.88508 60.99085 63.23347 67.96047
# g                 0.02311  0.04389  0.06433  0.09920  0.28793

realizeTheOdds(lmBFResPosteriors, "PctChildPoverty")
realizeTheOdds(lmBFResPosteriors, "PctFreeMeal")
realizeTheOdds(lmBFResPosteriors, "PctFamilyPoverty")
realizeTheOdds(lmBFResPosteriors, "Enrolled")
realizeTheOdds(lmBFResPosteriors, "TotalSchools")



### 8.	What's the big picture, based on all of the foregoing analyses? 
### The staff member in the state legislator's office is interested to know how to allocate financial assistance to school 
### districts to improve both their vaccination rates and their reporting compliance. What have you learned from the data 
### and analyses that might inform this question?
#table(districts$)

# try adding pctBeliefExempt and PctUpToDate
### 6.	What variables predict the percentage of all enrolled students with completely up-to-date vaccines?
### but this time, incorporating both pctBeliefExempt and PctUpToDate
districts_t$PctUpToDate_binary <- as.numeric(districts_t$PctUpToDate + districts_t$PctBeliefExempt >= 100)# convert PctUpToDate to binary numeric
PctUpToDate_binary_glmout <- glm(PctUpToDate_binary ~ PctFreeMeal + PctChildPoverty + PctFamilyPoverty + Enrolled + TotalSchools, districts_t,
                                 family=binomial(link="logit"))
summary(PctUpToDate_binary_glmout)

anova(PctUpToDate_binary_glmout,  test="Chisq")
# p<a=0.05 for Enrolled, PctChildPoverty. Reject H0 for these terms, 
PseudoR2(PctUpToDate_binary_glmout)

vif(PctUpToDate_binary_glmout) # greater than 5 or 10 indicates confounding multicollinearity

# One predictor logistic with Bayesian estimation
bayesLogitOut <- MCMClogit(PctUpToDate_binary ~ PctChildPoverty + PctFreeMeal + PctFamilyPoverty + Enrolled + TotalSchools, districts_t)
summary(bayesLogitOut)
summary(exp(bayesLogitOut))

plot(bayesLogitOut)
realizeTheOdds(bayesLogitOut, "PctChildPoverty")
realizeTheOdds(bayesLogitOut, "PctFreeMeal")
realizeTheOdds(bayesLogitOut, "PctFamilyPoverty")
realizeTheOdds(bayesLogitOut, "Enrolled")
realizeTheOdds(bayesLogitOut, "TotalSchools")









