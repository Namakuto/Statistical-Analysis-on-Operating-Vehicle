#=================================== Description ==================================
#
# This is a piece of R code which conducts an exploratory analysis of various .csv
# files from vehicle sensor data (e.g., car movement speed, internal car fluid
# temperature).
# 
# I was supplied with 100 .csv files and was tasked with proposing an appropriate
# analysis plan/machine learning or statistical (regression) model for analysing the
# data.
# 
# I chose to build a function to quicken the import of .csv files and later, decided
# to concatenate all 100 files into one (low MB size, thus plausible), due to a large
# percentage of missing data (80-90% missing) in each of the individual .csv files.
#
#=================================================================================== 

library(jsonlite)
library(dlookr); library(tibble); library(skimr)


setwd("~/Jupyter Projects/R/----OperationBlock")


# ---------------------------- Load files/ import data -----------------------------

# Import data dictionary
js<-fromJSON(txt="./DataScienceTestFiles/column_dict.json", flatten = TRUE)
class(js)

# See all files in DataScience folder
files_csv<-list.files(path = "./DataScienceTestFiles", pattern = c("^O","\\.csv$"))
length(grep("^O", files_csv))

# Function to import [csv.name]#.csv of our choosing. 
load_data<-function(num){
  x<-read.csv(paste("./DataScienceTestFiles/",files_csv[num+1],sep = ""), header = TRUE)
}
df<-load_data(0)

head(df)
newX<-as.POSIXct(df$X, origin="1970-01-01")
df$X<-newX
head(df)

# View internal structure of object, browse missing data amount
str(df)
print(diagnose(df), n=Inf)
skim(df)
print(object.size(df), standard="auto", units="Mb")


Main_df<-load_data(0)
for (i in files_csv[2:length(files_csv)]){
  x<-read.csv(paste("./DataScienceTestFiles/",i,sep = ""), header = TRUE)
  Main_df<-rbind(Main_df,x)
}
print(object.size(Main_df), standard="auto", units="Mb")
newX<-as.POSIXct(Main_df$X, origin="1970-01-01")
Main_df$X<-newX

str(Main_df)
print(diagnose(Main_df), n=Inf)

par(mfrow=c(2,1),mai = c(0.6, 0.5, 0.3, 0.1))
#hist(Main_df$X, breaks="years", freq = TRUE)
hist(Main_df$X, breaks="months", freq = TRUE, col="grey",
     main = "Distribution of Dates in the Concatenated Dataset")
hist(Main_df$X, breaks="weeks", freq = TRUE, col="grey",
     main = "Distribution of Dates in the Concatenated Dataset")

range(Main_df$X) # Data is from April 19th, 2018 - Feb 13th, 2019... doesn't appear to initially match the
# histogram when breaks = "years"

# ------------------------- Begin actual analysis of data ----------------------------

library(stats); library(nortest)

ad.test(Main_df$X) # Data not normal

#library(finalfit)

diag_df<-diagnose(Main_df)
low_missing<-diag_df$variables[diag_df$missing_percent<=90] # Chose 90% cut-off based on cut-off for satisfactory
# bias, based on simulation study by Madley-Dowd et al (2019). Multiple imputation (MI) still performed well; thus
# assuming that other strong impuation methods (such as maximum likelihood imputation, using EM algorithm) would
# do well.

Reduced_df<-Main_df[low_missing]
diagnose(Reduced_df) # Looks much better; also looking at "unique" count to determine if most variables likely 
# continuous/categorical ones; looks like most continuous, nothing binary

# ------------------------------------ Notes: ----------------------------------
# - No binary classification
# - Reviewed description of "Column 25" in JSON data dictionary + Welcome document--this is categorical
# - Not enough categories to make log linear model. Could only do ML with EM if doing linear model.

plot(Reduced_df)

library(corrplot)
# Create correlation matrix + correlation significance test. Chose to use pairwise complete observations then later
# compared to using only rows 100% complete with observations (i.e., compelely devoid of even 1 missing value
# in the entire row) to see if any difference/bias. Both returned identical results.
cor.df<-cor(Reduced_df[2:15], use="pairwise.complete.obs")
sig.df<-cor.mtest(Reduced_df[2:15]) # test showed most correlations significant, but data also has lots of missing
# data; also massive sample size [which tends to exaggerate significance/result in significant results during
# statistical tests]). 

# Make a corrplot--originally had it show p-values but font too small to see.
corrplot(cor.df, method="color")

# Some variables of interest-- X1:X6, X16:X21
# At this point, need to start thinking of any sort of model to deal with missing data (e.g., random forest,
# multiple imputation--which uses statistical models to predict the values, ML using EM, etc).
# Did not do missing data analysis to determing what imputation best. Technically, this should have been done
# first.

# Vehicle speed (X16) could be interesting to work with. X16 associated with everything BUT X7, X8, X25

Reduced_df<-Reduced_df[2:ncol(Reduced_df)] # will remove date var now as not planning on using and just 
# making it harder to analyse things right now.

set.seed(3)
smaller<-Reduced_df[sample(nrow(Reduced_df),nrow(Reduced_df)*.1),  ] # Took 20% sample to make plotting easier

par(mfrow=c(3,2), mai = c(0.3, 0.3, 0.1, 0.1))
plot(smaller$X16~., data=smaller, ylab = "Vehicle speed (km/h)") # Looks like we have some pretty linear 
# associations--again, we haven't imputed anything, or removed any variables beyond those with extrenously 
# large amounts of missing data. So what we see here shouldn't be too different from the true/original trends
# in the data.

# Deciding to use random forest (with regression) or linear model. Although random forest could have a hard time 
# predicting values not seen in its training set---> use linear regression?

# Is linear regression even okay/plausible to begin with?
Reduced_df.lm<-lm(Reduced_df$X16~., Reduced_df)
summary(Reduced_df.lm) 
# Linear model actually fits really well already and has low p-value (significant model),
# even before fixing anything, and despite deleting all rows with at least one missing value (LM uses listwise
# deletion)...


par(mfrow=c(1,2))
hist(Reduced_df$X16, breaks = 40, col="dodgerblue1", 
     main = "Histogram of Vehicle speed (km/h)") 
hist(Reduced_df$X16[Reduced_df$X16<20], breaks=10, col="dodgerblue1",
     main = "Histogram of Vehicle speed (km/h)") # High frequency of an extreme value, but also likely 
# a "natural" value and not necessarily an error/outlier--reasonable that vehicle would be at 0km/h at times.

par(mfrow=c(2,2), mai = c(0.5, 0.4, 0.3, 0.1))
plot(Reduced_df.lm, which=1:4) # Checking if the outlier values dratistically (negatively impact) our model

# --------------------------------- Next steps? --------------------------------------

# Possibly try transforming the outcome variable, dichotomizing it (risky), some 
# sort of polynomial model, or a support vector machine model.

# Check for multicollinearity if using a traditional statistical model.

# Use stepwise variable selection to remove/add variables.

# Conduct likelihood ratio (LR) test between every removal/addition of a variable 
# if using a traditional statistical model (nested)




