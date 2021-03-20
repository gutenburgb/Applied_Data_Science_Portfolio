# IST-687 Applied Data Science
# Data analysis for a major hotel chain.

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
using("gdata","tidyverse","ISOweek","dplyr", "sqldf", "readxl", "maps", "reshape2", "e1071")


#Function To remove axis formats from maps
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

printVecInfo <- function(v){
  vMean <- mean(v)
  vMedian <- median(v)
  vMin <- min(v)
  vMax <- max(v)
  vStdev <- sd(v)
  vQuant5 <- quantile(v, 0.05)
  vQuant95 <- quantile(v, 0.95)
  vSkew <- skewness(v)
  cat("mean: ", vMean, "\n")
  cat("median: ", vMedian, "\n")
  cat("min: ", vMin, "max: ", vMax, "\n")
  cat("sd: ", vStdev, "\n")
  cat("quantile(0.05-0.95): ", vQuant5, "--", vQuant95, "\n")
  cat("skewness: ", vSkew, "\n")
  naCount <- length(v[is.na(v)])
  if(naCount > 0){
    cat("na pct: ", round(naCount/length(v), digits=5), "\n")
  } else {
    cat("na pct: NONE\n")
  }
  
}

# loads and munges the data alright. You will need to adjust dataPath

dataPath = "data/ProjectSurveyData.xlsx"

loadData <- function(path){
  types <- c("text", "text", "text",
            "date", "date", "numeric","numeric",
            "numeric", "numeric", "text", "numeric",
            "date", "numeric", "text", "date",
            "text", "text", "text", "text",
            "numeric", "numeric", "text", "text",
            "text", "text", "numeric", "numeric",
            "numeric", "numeric", "numeric", "numeric",
            "numeric", "numeric", "numeric", "numeric",
            "numeric", "text", "text", "text",
            "numeric", "text", "text", "text",
            "numeric", "numeric", "text", "text",
            "text", "text", "text", "text",
            "text", "text", "text", "text",
            "text",  "text",  "text")
  

  # Read data from excel file
  data <- read_excel(path, sheet=NULL, range=NULL, col_names=TRUE, 
                     col_types=types, na=c("NA", "", ""), trim_ws=TRUE)
  
  # munge GP_Tier data
  data$GP_Tier<-as.character(data$GP_Tier)
  data$GP_Tier <- tolower(data$GP_Tier)
  data$GP_Tier[is.na(data$GP_Tier)] <- "none"
  data$GP_Tier[data$GP_Tier=="diam"] <- "diamond"
  data$GP_Tier[data$GP_Tier=="plat"] <- "platinum"
  data$GP_Tier[data$GP_Tier=="card"] <- "other"
  data$GP_Tier[data$GP_Tier=="courtesy"] <- "other"
  data$GP_Tier[data$GP_Tier=="ldia"] <- "diamond"
  data$GP_Tier[data$GP_Tier=="lifetime diamond"] <- "diamond"
  
  # NPS SCORE = % of promoters - percentage of detractors
  # NPS_by_hotel 
  # tapply(mpg, cyl, mean)
  
  # replace NA's with mean
  #air$Ozone[is.na(air$ozone)] <- mean(air$Ozone, na.rm=TRUE)
  
  
  # convert to date
  data$CHECK_OUT_DATE_C <- as.Date(data$CHECK_OUT_DATE_C)
  data$CHECK_OUT_DATE_MONTH <- strftime(data$CHECK_OUT_DATE_C, "%m") # months 01, 02, 03, 04, 05
  data$CHECK_OUT_DATE_DAY <- strftime(data$CHECK_OUT_DATE_C, "%d")
  #factor(data$CHECK_OUT_DATE_DAY)
  # data$Property_ID_PL has 195 unique hotels
  anyNA(data$Property_ID_PL)
  factor(data$Property_ID_PL)
  str(data$Property_ID_PL)
  
  # make the hotel name more accessible
  names(data)[names(data) == "Hotel.Name.Long_PL"] <- "NAME"
  
  
  # make month-day column, for easy graphing
  start <- as.Date("2014-01-01 UTC")
  data$DAYS_TOTAL <- as.numeric( as.Date(data$CHECK_IN_DATE_C) - start)
  
  ### add week to data ###

  date<-NULL
  weeks<-NULL
  # create a dataset mapping each day of the year to a week, and merge with data
  date <- seq(as.Date("2014/1/1 UTC"), as.Date("2014/12/31 UTC"), by="day") 
  weeks <- data.frame(DAYS_TOTAL=date)
  weeks <- mutate(weeks, WEEK=ISOweek(date))
  weeks$WEEK <- as.numeric(substr(ISOweek(date), nchar(as.character(ISOweek(date)))-1, nchar(as.character(ISOweek(date))) + 2))
  weeks$DAYS_TOTAL <- as.numeric( as.Date(weeks$DAYS_TOTAL) - start) 
  data <- merge(data,weeks, by=c('DAYS_TOTAL'))
  
  
  ### Drop non-US data ###
  
  #sqldf("SELECT Country_PL, count(Country_PL) FROM data GROUP BY Country_PL")
  #1          Canada               250
  #2           China               208
  #3           Egypt                72
  #4         Germany                98
  #5       Hong Kong               215
  #6           India               226
  #7       Indonesia                23
  #8           Japan                98
  #9          Mexico                75
  #10          Qatar                67
  #11   Saudi Arabia                28
  #12    South Korea               265
  #13    Switzerland                58
  #14     Tajikistan                12
  #15         Turkey                30
  #16        Ukraine                32
  #17 United Kingdom               118
  #18  United States             17466
  data <- data[data$Country_PL == "United States",]
  
  data$Overall_Sat_H [is.na(data$Overall_Sat_H)] <- mean(data$Overall_Sat_H, na.rm = TRUE)
  data$Guest_Room_H [is.na(data$Guest_Room_H)] <- mean(data$Guest_Room_H, na.rm = TRUE)
  data$Tranquility_H [is.na(data$Tranquility_H)] <- mean(data$Tranquility_H, na.rm = TRUE)
  data$Condition_Hotel_H [is.na(data$Condition_Hotel_H)] <- mean(data$Condition_Hotel_H, na.rm = TRUE)
  data$Customer_SVC_H [is.na(data$Customer_SVC_H)] <- mean(data$Customer_SVC_H, na.rm = TRUE)
  data$Staff_Cared_H [is.na(data$Staff_Cared_H)] <- mean(data$Staff_Cared_H, na.rm = TRUE)
  data$Internet_Sat_H [is.na(data$Internet_Sat_H)] <- mean(data$Internet_Sat_H, na.rm = TRUE)
  data$Check_In_H [is.na(data$Check_In_H)] <- mean(data$Check_In_H, na.rm = TRUE)
  data$F.B_FREQ_H [is.na(data$F.B_FREQ_H)] <- mean(data$F.B_FREQ_H, na.rm = TRUE)
  data$F.B_Overall_Experience_H [is.na(data$F.B_Overall_Experience_H)] <- mean(data$F.B_Overall_Experience_H, na.rm = TRUE)
  data$LENGTH_OF_STAY_C [is.na(data$LENGTH_OF_STAY_C)] <- mean(data$LENGTH_OF_STAY_C, na.rm = TRUE)
  data$NUMBER_OF_ROOMS_C [is.na(data$NUMBER_OF_ROOMS_C)] <- mean(data$NUMBER_OF_ROOMS_C, na.rm = TRUE)
  data$ADULT_NUM_C [is.na(data$ADULT_NUM_C)] <- mean(data$ADULT_NUM_C, na.rm = TRUE)
  data$PACE_R [is.na(data$PACE_R)] <- mean(data$PACE_R, na.rm = TRUE)
  
  
  # can we get state names from lat/lon using the map package? (YES, in two (or three) lines of R)
  # add states to *most* every row (some will be NA)
  data$STATE_PL <- map.where("state", data$Property.Longitude_PL, data$Property.Latitude_PL)
  data$STATE_PL <- str_replace(data$STATE_PL, ":main", "")
  data$STATE_PL <- str_replace(data$STATE_PL, ":south", "")
  data$STATE_PL <- str_replace(data$STATE_PL, ":north", "")
  data$STATE_PL <- str_replace(data$STATE_PL, ":east", "")
  data$STATE_PL <- str_replace(data$STATE_PL, ":west", "")
  # this shows 69 columns that are NAs: data[is.na(data$STATE_PL),]
  # Property_ID_PL 18002 is texas. put this in manually
  data$STATE_PL[data$Property_ID_PL=="18002"] <- "texas"
                                                               
  return(data)
}

#warnings() # view any warnings resulting from the read_excel command
data <- loadData(dataPath)

# Percent NA (before data-munging)
# Column Name			% na
# CHILDREN_NUM_C	0.9127287768
# Club.Type_PL	0.7661048496
# Internet_Sat_H	0.5497363251
# F.B_FREQ_H	0.4983972702
# F.B_Overall_Experience_H	0.4983972702
# Tranquility_H	0.3713163065
# Check_In_H	0.3676972392
# Staff_Cared_H	0.3674904353
# GP_Tier	0.3078792266
# STATE_R	0.1727846138
# GUEST_COUNTRY_R	0.1015406887
# Age_Range_H	0.0199565712
# Customer_SVC_H	0.0142694654
# Gender_H	0.0110123048
# Condition_Hotel_H	0.0109606039
# Guest_Room_H	0.0097197808
# Guest_Country_H	0.0063592183
# LENGTH_OF_STAY_C	0.0050149933
# NUMBER_OF_ROOMS_C	0.0026367490
# Overall_Sat_H	0.0010857202
# ROOM_TYPE_CODE_C	0.0006204115
# ADULT_NUM_C	0.0003619067
# PACE_R	0.0001034019





## Question: Does membership in gold/platinum/whatever clubs increase rating of hotels?	
## Analysis Techniques: multiple-regression analysis
## Visualizations: bar charts, pie charts	
## Variables Used: GP_Tier, F.B_Overall_Experience_H, Overall_Sat_H

# Let's attempt a correlation between GP_Tier and Overall_Sat_H (leaving out F.B_Overall_Experience_H)
# make a new data frame with just these columns. Add an "id" column for dcast() reshaping later
tierAndSatis <- data.frame(c(1:length(data$GP_Tier)),
                           data$GP_Tier, 
                           data$Overall_Sat_H,
                           stringsAsFactors=TRUE)
# rename them nicely
colnames(tierAndSatis) <- c("id", "GP_Tier","Overall_Sat_H")
str(tierAndSatis)

# use dcast to change the GP_Tier column into one new column per type of GP_Tier (diamond, gold, etc)
preCorDF <- dcast(tierAndSatis, id ~ GP_Tier, fill=0, value.var="Overall_Sat_H" )

# remove "none" andn "other" columns. "other" will be counted as "none". There's only 41 such rows (length(cordf[cordf$other>0,]$id))
preCorDF <- subset(preCorDF, select = -c(id, none, other))
str(preCorDF)
head(preCorDF)
# Change each membership category into a 0 or 1, for the regression analysis step
cordf <- data.frame( preCorDF[,1:3] <- sapply(preCorDF[,1:3], function(v){
  return(as.integer(v>0))
  }) )
# we lost a column doing that. Add Overall_Sat_H back in...
cordf$Overall_Sat_H <- tierAndSatis$Overall_Sat_H # I checked, and these rows still line up fine
str(cordf)
head(cordf)
# use diamond, gold, and platinum columns to predict Overall_Sat_H using the cordf data frame
results <- lm(formula=Overall_Sat_H ~ diamond+gold+platinum, data=cordf)
# print the results
summary(results)




# NOW what if we made one column which was just "any membership in a tier"? and regression'd that?
# add this column
cordf$any_tier = 0
# for any tier that was, set this column equal to 1
cordf[cordf$diamond==1 | cordf$gold==1 | cordf$platinum==1,]$any_tier = 1
# remove all but these two columns
cordfsm <- subset(cordf, select = c(any_tier, Overall_Sat_H ))
head(cordfsm)
# regression analysis
anyTierResults <- lm(formula=Overall_Sat_H ~ any_tier, data=cordfsm)
# print the results
summary(anyTierResults)


# let's go on to see if membership in any tier is correlated with F.B_Overall_Experience_H somehow?
#length(data$F.B_Overall_Experience_H)
#length(cordfsm$any_tier) # their lengths are still equal

cordfsm$FB_Overall <- data$F.B_Overall_Experience_H
# FB_Overall has almost 50% nulls
FB_Median <- median(cordfsm$FB_Overall, na.rm=TRUE) # 9
# fill with median value
cordfsm$FB_Overall[is.na(cordfsm$FB_Overall)] <- FB_Median
# run regression
anyTierFBResults <- lm(formula=FB_Overall ~ any_tier, data=cordfsm)
summary(anyTierFBResults)



## convert each factor to its own column, with a 1 or 0 if present
#pivot_wider(tierAndSatis, names_from=tierAndSatis$GP_Tier, values_fill=0)
## remove the "none" column, keep the rows. This is necessary for the correlation







## Question: Which hotels are up-and-coming? which are declining?
## Analysis Techniques: descriptive statistics
## Visualizations: MAPS, scatterplots, trendlines, Top-10 Table	
## Variables Used: CHECK_OUT_DATE_C, F.B_Overall_Experience_H, Property_ID_PL






#week_calendar <- summarise(weekdate=min(weeks))

# Graph it by week (days past jan 1, 2014)
#hotelsByWeek <- sqldf("SELECT
#      Property_ID_PL AS PROPERTY_ID,
#      week,
#      NAME,
#      COUNT(NPS_TYPE) AS 'RECORD_CT',
#        (SUM(NPS_Type='Promoter')*1.0)/(COUNT(NPS_Type)*1.0) -
#        (SUM(NPS_Type='Detractor')*1.0)/(COUNT(NPS_Type)*1.0) AS 'NPS_SCORE'
#      FROM data 
#      GROUP BY Property_ID_PL, week
#      HAVING RECORD_CT > 25
#      ORDER BY Property_ID_PL, week desc")
## narrow it down to only those having at least 4 weeks of data
#hotelsByWeek <- sqldf("SELECT * 
#      FROM hotelsByWeek 
#      WHERE PROPERTY_ID IN (
#        SELECT PROPERTY_ID
#        FROM hotelsByWeek 
#        GROUP BY PROPERTY_ID
#        HAVING count(week) > 3
#      )")
#ggplot(hotelsByWeek, aes(group=PROPERTY_ID, x=WEEK, y=NPS_SCORE, color=RECORD_CT)) + geom_line()
#plot(hotelsByWeek$WEEK, hotelsByWeek$NPS_SCORE, col="blue", main="NPS_SCORE by WEEK", xlab="WEEK", ylab="NPS_SCORE")
## pch = 16, cex = 1.3, 


#data$ENTRY_TIME_R
#convertToDateTime(data$ENTRY_TIME_R, origin="1900-01-01")




######################################################################
# QUESTION: What hotels have improved over the course
# of this month's collection of data?
######################################################################

# chop data in half by time, and see if data improved or not?  RUN T-TESTS DIRECTLY
nps_by_day <- sqldf("SELECT 
    Property_ID_PL AS PROPERTY_ID,
    CHECK_OUT_DATE_MONTH,
    CHECK_OUT_DATE_DAY AS DAY,
    NAME,
    COUNT(NPS_TYPE) AS 'RECORD_CT',
      (SUM(NPS_Type='Promoter')*1.0)/(COUNT(NPS_Type)*1.0) -
      (SUM(NPS_Type='Detractor')*1.0)/(COUNT(NPS_Type)*1.0) AS 'NPS_SCORE'
    FROM data 
    GROUP BY Property_ID_PL, CHECK_OUT_DATE_DAY
    ORDER BY Property_ID_PL, CHECK_OUT_DATE_MONTH asc, CHECK_OUT_DATE_DAY asc")

# Assumes that things are ORDERED before submitting (See ORDER BY above)
ab_nps_improved <- aggregate(nps_by_day$NPS_SCORE, list(nps_by_day$PROPERTY_ID), function(v){
  # split this in half
  a <- v[1:(length(v)-(length(v)/2))]
  b <- v[((length(v)/2)+1):(length(v)+(1*(length(v)%%2) ))]
  # ensure a and b have same length. add mean of the shorter one, to the shorter list
  # this should only be an off-by-one thing
  if(length(a) > length(b)){ b<-c(b, mean(b)) }
  if(length(b) > length(a)){ a<-c(a, mean(a)) }
  # is 'a' less than 'b'? AKA: was there improvement?
  # if is less than 0.01, then we reject H0, and we can say that there IS a difference in means
  improvement <- "not enough info"
  if(length(a) > 1){
    p <- t.test(a, b, alternative=c("less"), paired=TRUE, var.equal=TRUE, conf.level = 0.95)
    # if alpha > p-value, then we have improvement! ('yes'), else, 'no'
    improvement <- ifelse( (1-attr(p$conf.int, 'conf.level')) > p$p.value, 'yes', 'no')
  }
  return(improvement)
})

# 170 in total
cat("not improved count:", length(ab_nps_improved$x[ab_nps_improved$x == "no"]) )
cat("improved count:", length(ab_nps_improved$x[ab_nps_improved$x == "yes"]) )
mostImproved <- data.frame(Property_ID_PL=ab_nps_improved$Group.1, improved=ab_nps_improved$x)

# Assumes that things are ORDERED before submitting (See ORDER BY above)
ab_nps_declined <- aggregate(nps_by_day$NPS_SCORE, list(nps_by_day$PROPERTY_ID), function(v){
  # split this in half
  a <- v[1:(length(v)-(length(v)/2))]
  b <- v[((length(v)/2)+1):(length(v)+(1*(length(v)%%2) ))]
  # ensure a and b have same length. add mean of the shorter one, to the shorter list
  # this should only be an off-by-one thing
  if(length(a) > length(b)){ b<-c(b, mean(b)) }
  if(length(b) > length(a)){ a<-c(a, mean(a)) }
  # is 'a' greater than 'b'? AKA: was there a decline in performance?
  # if is less than 0.01, then we reject H0, and we can say that there IS a difference in means
  improvement <- "not enough info"
  if(length(a) > 1){
    p <- t.test(a, b, alternative=c("greater"), paired=TRUE, var.equal=TRUE, conf.level = 0.95)
    # if alpha > p-value, then we have decline! ('yes'), else, 'no'
    improvement <- ifelse( (1-attr(p$conf.int, 'conf.level')) > p$p.value, 'yes', 'no')
  }
  return(improvement)
})

# 170 in total
cat("not declined count:", length(ab_nps_declined$x[ab_nps_declined$x == "no"]) )
cat("declined count:", length(ab_nps_declined$x[ab_nps_declined$x == "yes"]) )
mostDeclined <- data.frame(Property_ID_PL=ab_nps_declined$Group.1, declined=ab_nps_declined$x)

# combine "declined" and "improved" data into new columns, for plotting
data <- merge(mostDeclined, data, by=c('Property_ID_PL') )
data <- merge(mostImproved, data, by=c('Property_ID_PL') )

plotImprovement <- data.frame(data$Property_ID_PL, 
                              data$Hotel.Name.Short_PL,
                              data$improved,
                              data$Property.Latitude_PL, 
                              data$Property.Longitude_PL)

plotDeclines <- data.frame(data$Property_ID_PL, 
                              data$Hotel.Name.Short_PL,
                              data$declined,
                              data$Property.Latitude_PL, 
                              data$Property.Longitude_PL)

us <- map_data("state")
improvMap <- ggplot() + geom_polygon(data=us, aes(x=long, y=lat, group=group), color="white") + coord_fixed(1.3)
improvMap <- improvMap + geom_point(data=plotImprovement, 
                              aes(x=plotImprovement$data.Property.Longitude_PL, 
                                  y=plotImprovement$data.Property.Latitude_PL, 
                                  shape=plotImprovement$data.improved,
                                  color=plotImprovement$data.improved,
                                  size=plotImprovement$data.improved))
improvMap <- improvMap + ggtitle("Hotels With NPS Score Improvements this Month") + theme(plot.title=element_text(hjust=0.5))
improvMap <- improvMap + ditch_the_axes
improvMap$labels$colour="  "
improvMap$labels$shape=" improved?"
improvMap$labels$size="  "
improvMap

declinMap <- ggplot() + geom_polygon(data=us, aes(x=long, y=lat, group=group), color="white") + coord_fixed(1.3)
declinMap <- declinMap + geom_point(data=plotDeclines, 
                                    aes(x=plotDeclines$data.Property.Longitude_PL, 
                                        y=plotDeclines$data.Property.Latitude_PL, 
                                        shape=plotDeclines$data.declined,
                                        color=plotDeclines$data.declined,
                                        size=plotDeclines$data.declined))
declinMap <- declinMap + ggtitle("Hotels With NPS Score Declinations this Month") + theme(plot.title=element_text(hjust=0.5))
declinMap <- declinMap + ditch_the_axes
declinMap$labels$colour="  "
declinMap$labels$shape=" declined?"
declinMap$labels$size="  "
declinMap

################ LETS PLOT BOTH?? ###############
data$imp_and_dec <- "unchanged"
data$imp_and_dec[data$improved=="yes"] <- "improved"
data$imp_and_dec[data$declined=="yes"] <- "declined"
plotBoth <- data.frame(data$Property_ID_PL, 
                           data$Hotel.Name.Short_PL,
                           data$imp_and_dec,
                           data$Property.Latitude_PL, 
                           data$Property.Longitude_PL)
# remove unchanged
plotBoth <- plotBoth[plotBoth$data.imp_and_dec!="unchanged",]
changeMap <- ggplot() + geom_polygon(data=us, aes(x=long, y=lat, group=group), color="white") + coord_fixed(1.3)
changeMap <- changeMap + geom_point(data=plotBoth, 
                                    aes(x=plotBoth$data.Property.Longitude_PL, 
                                        y=plotBoth$data.Property.Latitude_PL, 
                                        shape=plotBoth$data.imp_and_dec,
                                        color=plotBoth$data.imp_and_dec))
changeMap <- changeMap + ggtitle("Hotels With NPS Score Changes this Month") + theme(plot.title=element_text(hjust=0.5))
changeMap <- changeMap + ditch_the_axes
changeMap$labels$colour="  "
changeMap$labels$shape=" change"
changeMap

################# ALSO DO A CHART ################

unique(plotBoth)
t <- data.frame(change=data$imp_and_dec, 
                Hotel.Name.Short_PL=data$Hotel.Name.Short_PL,
                Location_PL=data$Location_PL,
                Type_PL=data$Type_PL,
                STATE_PL=data$STATE_PL)
t <- unique(t)
t[t$change=="improved",]
t[t$change=="declined",]






























####################################################################################################################################
# QUESTION: Do OWNED/FRANCHISED (Type_PL) and LOCATION (Location_PL) affect whether or not a hotel IMPROVED/DECLINED (imp_and_dec) ? or NPS_SCORE?
#####################################################################################################################################
# add NPS_Score_Category: "high" / "low" factors for each hotel
score_by_hotel <- sqldf("SELECT 
      Property_ID_PL,
        (SUM(NPS_Type='Promoter')*1.0)/(COUNT(NPS_Type)*1.0) -
        (SUM(NPS_Type='Detractor')*1.0)/(COUNT(NPS_Type)*1.0) AS 'NPS_Score_Category'
      FROM data 
      GROUP BY Property_ID_PL
      ")
# find high/lows
mean_nps <- mean(score_by_hotel$NPS_Score_Category)
score_by_hotel$NPS_Score_Category <- ifelse(score_by_hotel$NPS_Score_Category > mean_nps, "high", "low")
# and merge that into our data array
data <- merge(data, score_by_hotel, by=c('Property_ID_PL'))

# coerce some data to be in factor form
data$imp_and_dec <- as.factor(data$imp_and_dec)
data$Type_PL <- as.factor(data$Type_PL)
data$Location_PL <- as.factor(data$Location_PL)
data$NPS_Score_Category <- as.factor(data$NPS_Score_Category)
data$Brand_PL <- as.factor(data$Brand_PL)
# data$Class_PL <- as.factor(data$Class_PL) # Class_PL


randIndex <- sample(1:dim(data)[1]) # randomize data
cutpoint2_3 <- floor(2*dim(data)[1]/3) # In order to split data, create a 2/3 cutpoint and round the number
# create train data set
nps_loc.train <- data[randIndex[1:cutpoint2_3],]
cat("Training data dimensions:", dim(nps_loc.train))
# create test data, which contains the left 1/3 of the overall data
nps_loc.test <- data[randIndex[(cutpoint2_3+1):dim(data)[1]],]
cat("Test data dimensions:", dim(nps_loc.test))   # check test data set
## perform NB analysis
nps_loc_model <- naiveBayes(NPS_Score_Category ~ Location_PL+Type_PL+Brand_PL, data=nps_loc.train) # tried: NPS_Score_Category, and imp_and_dec
nps_loc_model
nps_loc_predictions <- predict(nps_loc_model,nps_loc.test)
nps_loc_comparisons <- data.frame(nps_loc.test$NPS_Score_Category, nps_loc_predictions) # tried: NPS_Score_Category, and imp_and_dec
colnames(nps_loc_comparisons) <- c("actual","predicted")
nps_loc_perc_nb <- length(which(nps_loc_comparisons$actual==nps_loc_comparisons$predicted))/dim(nps_loc_comparisons)[1]
cat("percent predicted correctly with NB:", nps_loc_perc_nb)
## confusion matrix
nps_loc_confusion_matrix <- table(test=nps_loc_comparisons$actual,pred=nps_loc_comparisons$predicted)
print(nps_loc_confusion_matrix)
# inspect. Note how everything was simply put in the "unchanged" box... It's hard to tell
table(nps_loc_predictions)


# +3-4% when we add in Brand_PL (70% total), no improvement when adding in Class_PL
sqldf("SELECT Class_PL, COUNT(Class_PL) FROM data GROUP BY Class_PL")

allCombos <- expand.grid( unique(factor(data$Location_PL)), unique(factor(data$Type_PL)), unique(factor(data$Brand_PL)) )
names(allCombos) <- c("Location_PL", "Type_PL", "Brand_PL")

allCombos$prediction <- predict(nps_loc_model,allCombos)
print(allCombos)
sqldf("SELECT Location_PL, COUNT(prediction) FROM allCombos WHERE prediction=\"low\" GROUP BY Location_PL")
sqldf("SELECT Location_PL, COUNT(prediction) FROM allCombos WHERE prediction=\"high\" GROUP BY Location_PL")

# add columns back in, for plotting
nps_loc_comparisons$Location_PL <- nps_loc.test$Location_PL
nps_loc_comparisons$Type_PL <- nps_loc.test$Type_PL
nps_loc_comparisons$Brand_PL <- nps_loc.test$Brand_PL
nps_loc_comparisons$correct <- ifelse(nps_loc_comparisons$actual==nps_loc_comparisons$predicted,"correct","wrong")

plot.nb <- ggplot(nps_loc_comparisons,aes(x=Type_PL,y=Brand_PL)) + 
  geom_point(aes(size=2,color=Location_PL,shape=correct)) + 
  ggtitle("NB - high/low NPS")
plot.nb













######### re-do models with the following columns: #########
#Likelihood_Recommend_H
#NPS_Type
#(these two are identical)
#by 
#any_tier

##############################################################################################
## Question: Does membership in gold/platinum/whatever clubs increase rating of hotels?	
##############################################################################################
## Analysis Techniques: multiple-regression analysis
## Visualizations: bar charts, pie charts	
## Variables Used: GP_Tier, Likelihood_Recommend_H
#head(data['GP_Tier'])
#head(data['Likelihood_Recommend_H'])

# Let's attempt a correlation between GP_Tier and Likelihood_Recommend_H
# make a new data frame with just these columns. Add an "id" column for dcast() reshaping later
tierAndSatis <- data.frame(c(1:length(data$GP_Tier)),
                           data$GP_Tier, 
                           data$Likelihood_Recommend_H,
                           stringsAsFactors=TRUE)
# rename them nicely
colnames(tierAndSatis) <- c("id", "GP_Tier","Likelihood_Recommend_H")
str(tierAndSatis)

# use dcast to change the GP_Tier column into one new column per type of GP_Tier (diamond, gold, etc)
preCorDF <- dcast(tierAndSatis, id ~ GP_Tier, fill=0, value.var="Likelihood_Recommend_H" )

# remove "none" and "other" columns. "other" will be counted as "none". There's only ~20 such rows (length(cordf[cordf$other>0,]$id))
preCorDF <- subset(preCorDF, select = -c(id, none, other))
str(preCorDF)
head(preCorDF)
# Change each membership category into a 0 or 1, for the regression analysis step
cordf <- data.frame( preCorDF[,1:3] <- sapply(preCorDF[,1:3], function(v){
  return(as.integer(v>0))
}) )
# we lost a column doing that. Add Likelihood_Recommend_H back in...
cordf$Likelihood_Recommend_H <- tierAndSatis$Likelihood_Recommend_H # I checked, and these rows still line up fine
str(cordf)
head(cordf)
# use diamond, gold, and platinum columns to predict Likelihood_Recommend_H using the cordf data frame
results <- lm(formula=Likelihood_Recommend_H ~ diamond+gold+platinum, data=cordf)
# print the results
summary(results)


########### calculate NPS_SCORE for each GP_Tier ############
score_by_tier <- sqldf("SELECT 
      GP_Tier,
      COUNT(NPS_TYPE) AS 'RECORD_CT',
        (SUM(NPS_Type='Promoter')*1.0)/(COUNT(NPS_Type)*1.0) -
        (SUM(NPS_Type='Detractor')*1.0)/(COUNT(NPS_Type)*1.0) AS 'NPS_SCORE'
      FROM data 
      GROUP BY GP_Tier
      ")
score_by_tier


### Perhaps a Bayesian analysis? Naive Bayes here goes ###
# Customers that give you a 6 or below are Detractors, a score of 7 or 8 are called Passives, and a 9 or 10 are Promoters.#
head(data$NPS_Type)
cordf$NPS_Type <- "Detractor"
cordf$NPS_Type[cordf$Likelihood_Recommend_H>=7] <- "Passive"
cordf$NPS_Type[cordf$Likelihood_Recommend_H>=9] <- "Promoter"
cordf$detractor <- ifelse(cordf$NPS_Type == "Detractor", 1, 0)
cordf$passive <- ifelse(cordf$NPS_Type == "Passive", 1, 0)
cordf$promoter <- ifelse(cordf$NPS_Type == "Promoter", 1, 0)
cordf$no_tier <- ifelse(cordf$diamond==0 & cordf$gold==0 & cordf$platinum==0, 1, 0)
cordf$NPS_Type <- as.factor(cordf$NPS_Type) # convert NPS_Type into factors, for NB algorithm
head(cordf)
str(cordf)

randIndex <- sample(1:dim(cordf)[1]) # randomize data
# chop into training and test data sets
# In order to split data, create a 2/3 cutpoint and round the number
cutpoint2_3 <- floor(2*dim(cordf)[1]/3)

# create train data set
cordf.train <- cordf[randIndex[1:cutpoint2_3],]
cat("Training data dimensions:", dim(cordf.train))

# create test data, which contains the left 1/3 of the overall data
cordf.test <- cordf[randIndex[(cutpoint2_3+1):dim(cordf)[1]],]
cat("Test data dimensions:", dim(cordf.test))   # check test data set

## perform NB analysis
nbModel <- naiveBayes(NPS_Type ~ no_tier+diamond+gold+platinum,data=cordf.train)
nbModel
predictions <- predict(nbModel,cordf.test)
comparisons <- data.frame(cordf.test$NPS_Type, predictions)
colnames(comparisons) <- c("actual","predicted")

perc_nb <- length(which(comparisons$actual==comparisons$predicted))/dim(comparisons)[1]
cat("percent predicted correctly with NB:", perc_nb)
## confusion matrix
confusion_matrix <- table(test=comparisons$actual,pred=comparisons$predicted)
print(confusion_matrix)
## However... all of the predictions are "Promoter" so this is useless
table(predictions)



########################################################################################
# Question: What surveyed factors are most influential to getting a higher NPS score?
########################################################################################
# mean(each_survey_column_H) BY HOTEL
agg_survey <- sqldf("SELECT 
      Property_ID_PL, 
      COUNT(Property_ID_PL) AS 'RECORD_CT',
        (SUM(NPS_Type='Promoter')*1.0)/(COUNT(NPS_Type)*1.0) -
        (SUM(NPS_Type='Detractor')*1.0)/(COUNT(NPS_Type)*1.0) AS 'NPS_SCORE',
      AVG(Overall_Sat_H) AS 'Overall_Sat_H',
      AVG(Guest_Room_H) AS 'Guest_Room_H',
      AVG(Tranquility_H) AS 'Tranquility_H',
      AVG(Condition_Hotel_H) AS 'Condition_Hotel_H',
      AVG(Customer_SVC_H) AS 'Customer_SVC_H',
      AVG(Staff_Cared_H) AS 'Staff_Cared_H',
      AVG(Internet_Sat_H) AS 'Internet_Sat_H',
      AVG(Check_In_H) AS 'Check_In_H',
      AVG(\"F.B_Overall_Experience_H\") AS 'F.B_Overall_Experience_H'
      FROM data
      GROUP BY Property_ID_PL")

str(agg_survey)
# Compute correlation matrix for all survey questions. 
# Drop non-numeric columns first
remove_cols <- c('Property_ID_PL', 'RECORD_CT')
just_numeric <- agg_survey[ , !(names(agg_survey) %in% remove_cols)]
# now run correlation
cor_res <- cor(just_numeric)
round(cor_res, 2)


# Now let's perform a regression between the highest correlated columns: NPS_SCORE and Overall_Sat_H
results <- lm(formula=NPS_SCORE ~ Overall_Sat_H, data=agg_survey)
summary(results)

# and NPS_SCORE and Customer_SVC_H
results <- lm(formula=NPS_SCORE ~ Customer_SVC_H, data=agg_survey)
summary(results)

# ALL except Overall_Sat_H 
results <- lm(formula=NPS_SCORE ~ Guest_Room_H+Tranquility_H+Condition_Hotel_H+Customer_SVC_H+Staff_Cared_H+Internet_Sat_H+Check_In_H+F.B_Overall_Experience_H, data=agg_survey)
summary(results)
#Guest_Room_H              0.12198    0.02335   5.224 5.36e-07 ***
#Tranquility_H             0.01557    0.02200   0.708  0.48011    
#Condition_Hotel_H         0.05673    0.01809   3.136  0.00203 ** 
#Customer_SVC_H            0.19418    0.02865   6.778 2.18e-10 ***
#Staff_Cared_H            -0.06335    0.03673  -1.725  0.08645 .  
#Internet_Sat_H            0.05834    0.01983   2.942  0.00374 ** 
#Check_In_H                0.03339    0.03814   0.875  0.38262    
#F.B_Overall_Experience_H  0.05069    0.02663   1.903  0.05880 .  
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 0.05023 on 161 degrees of freedom
#Multiple R-squared:  0.828,	Adjusted R-squared:  0.8195  <------------
#F-statistic:  96.9 on 8 and 161 DF,  p-value: < 2.2e-16

# Just the higher performing ones... Guest_Room_H+Condition_Hotel_H+Customer_SVC_H+Internet_Sat_H
results <- lm(formula=NPS_SCORE ~ Guest_Room_H+Condition_Hotel_H+Customer_SVC_H+Internet_Sat_H, data=agg_survey)
summary(results)
#(Intercept)       -3.29561    0.18289 -18.020  < 2e-16 ***
#  Guest_Room_H       0.13314    0.02288   5.820 2.99e-08 ***
#  Condition_Hotel_H  0.06434    0.01725   3.731 0.000262 ***
#  Customer_SVC_H     0.17862    0.02013   8.875 1.12e-15 ***
#  Internet_Sat_H     0.05858    0.01965   2.982 0.003303 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 0.05084 on 165 degrees of freedom
#Multiple R-squared:  0.8195,	Adjusted R-squared:  0.8151   <---------
#F-statistic: 187.2 on 4 and 165 DF,  p-value: < 2.2e-16

# Let's plot some residuals
plot(results)

# Now just the highest 3 performing coefficients: Guest_Room_H+Condition_Hotel_H+Customer_SVC_H
results <- lm(formula=NPS_SCORE ~ Guest_Room_H+Condition_Hotel_H+Customer_SVC_H, data=agg_survey)
summary(results)
#(Intercept)       -2.94547    0.14350 -20.526     < 2e-16 ***
#  Guest_Room_H       0.12638    0.02330   5.424 0.000000203 ***
#  Condition_Hotel_H  0.06944    0.01756   3.954    0.000114 ***
#  Customer_SVC_H     0.19825    0.01947  10.185     < 2e-16 ***
#Residual standard error: 0.05203 on 166 degrees of freedom
#Multiple R-squared:  0.8097,	Adjusted R-squared:  0.8063 
#F-statistic: 235.5 on 3 and 166 DF,  p-value: < 2.2e-16


# Just the internet satisfaction now
results <- lm(formula=NPS_SCORE ~ Internet_Sat_H, data=agg_survey)
summary(results)
# Adjusted R-squared:  0.1364 

# Just the Guest Room 
results <- lm(formula=NPS_SCORE ~ Guest_Room_H, data=agg_survey)
summary(results)
# Adjusted R-squared:  0.6722

# Just customer service
results <- lm(formula=NPS_SCORE ~ Customer_SVC_H, data=agg_survey)
summary(results)
# Adjusted R-squared:  0.5896 

# Just the Hotel Condition
results <- lm(formula=NPS_SCORE ~ Condition_Hotel_H, data=agg_survey)
summary(results)
# Adjusted R-squared:  0.566 


### Are there any moderating effects? ###
results <- lm(formula=NPS_SCORE ~ Condition_Hotel_H*Guest_Room_H, data=agg_survey)
summary(results)
# Adjusted R-squared:  0.6861 
# no coefficients had significant impact

results <- lm(formula=NPS_SCORE ~ Guest_Room_H*Customer_SVC_H, data=agg_survey)
summary(results)
# Adjusted R-squared:  0.7881 
# no coefficients had significant impact
### Looks like, no moderating effects worth mentioning ###



# do business/pleasure column have an effect on the NPS_Score?
# LELAND: which type of hotel does best? airport? data$Location_PL  ... factor(data$Location_PL)
# LELAND: which hotels are going DOWN in NPS_Score
# avg revenue by nps type? - Laura ran... 
# 


