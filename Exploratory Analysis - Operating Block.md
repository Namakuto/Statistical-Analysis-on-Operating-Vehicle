---
title: "Exploratory Data Analysis and Preliminary Statistical Modeling on Vehicle Sensor Data"
author: "Nyasha M"
date: "26/04/2021"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

This is an R Markdown file which conducts an exploratory analysis of various .csv files from vehicle sensor data (e.g., car movement speed, internal car fluid temperature).

I was supplied with 100 .csv files and was tasked with proposing an appropriate analysis plan/machine learning or statistical (regression) model for analysing the data. Each .csv file represented one operating block for the vehicle.

I chose to build a function to quicken the import of .csv files and later, decided to concatenate all 100 files into one (low MB size, thus plausible), due to a large percentage of missing data (80-90% missing) in each of the individual .csv files.

```{r library_load, include=FALSE, message=FALSE, warning=FALSE}
library(jsonlite)
library(dlookr); library(tibble); library(skimr); library(corrplot)

setwd("~/Jupyter Projects/R/Preteckt_OperationBlock")
```
<center> **Loading Files/ Importing Data** </center>

Load a data dictionary I obtained for figuring out what each of the columns stand for, in each .csv file. Confirm that there are 100 .csv files to work with in the directory where I stored the files.
```{r load_json, echo=TRUE}
js<-fromJSON(txt="./DataScienceTestFiles/column_dict.json", flatten = TRUE)

files_csv<-list.files(path = "./DataScienceTestFiles", pattern = c("^O","\\.csv$"))
length(grep("^O", files_csv))
```

Create a function to try reading in a .csv file. Upon previewing the data, I noticed that we were dealing with a UNIX format for the time column of the dataset (`X`). I converted this column into a 'YYYY-MM-DD' format and previewed the converted sample .csv file. 
```{r function_test, echo=TRUE}
load_data<-function(num){
  x<-read.csv(paste("./DataScienceTestFiles/",files_csv[num+1],sep = ""), header = TRUE)
}
df<-load_data(0)

newX<-as.POSIXct(df$X, origin="1970-01-01")
df$X<-newX
head(df)
```

Now I am getting a closer look at the data. 
```{r preview_data_structure, echo=TRUE}
print(diagnose(df))
print(object.size(df), standard="auto", units="Mb")
```

I noticed that my one sample contained a lot of missing data. The file size of the .csv was also fairly small. I looked at a few more .csv using my above loading function and noticed that many of the other .csv files in the folder were also like this.

I decided to do a loop to concatenate all the .csv files together, and improve the sample size of my overall data.
```{r loop_load_data, echo=TRUE}
Main_df<-load_data(0)
for (i in files_csv[2:length(files_csv)]){
  x<-read.csv(paste("./DataScienceTestFiles/",i,sep = ""), header = TRUE)
  Main_df<-rbind(Main_df,x)
}
print(object.size(Main_df), standard="auto", units="Mb")
newX<-as.POSIXct(Main_df$X, origin="1970-01-01")
Main_df$X<-newX
```

Now obtaining a preview of my concatenated dataset to see if all the variables were parameterized and appeared to have been loaded in correctly.
```{r structure_main_df, echo=TRUE}
print(diagnose(Main_df), n=Inf)
```

What is the time/date range of the concatenated dataset? 
```{r time_range, echo=TRUE}
par(mfrow=c(2,1),mai = c(0.6, 0.5, 0.3, 0.1))

hist(Main_df$X, breaks="months", freq = TRUE, col="grey",
     main = "Distribution of Dates in the Concatenated Dataset")
hist(Main_df$X, breaks="weeks", freq = TRUE, col="grey",
     main = "Distribution of Dates in the Concatenated Dataset")

range(Main_df$X)
```

<center> **Begin Actual Analysis of Data** </center>

How much missing data is there after combining all 100 .csv operating blocks into one?
```{r diagnose_missing_main_df, echo=TRUE}
diag_df<-diagnose(Main_df)
diag_df
```

There's still a lot of missing data. Let's choose to remove variables with >90% missing data, as suggested by the results of the simulation study by Madley-Dowd et al (2019). Multiple imputation (MI) still performed well when 90% or less of the data was missing. 
```{r percent_missing_main_df, echo=TRUE}
low_missing<-diag_df$variables[diag_df$missing_percent<=90]

Reduced_df<-Main_df[low_missing]
diagnose(Reduced_df)

```

Much better.

Note, the data seems to be all continuous. However, there's too much data to make a scatterplot matrix across everything right now. Let's make a correlation matrix for some quick visualizations. 

We have to use "pairwise complete observations" or something similar for the corrplot package or it won't work.
```{r corrplot, echo=TRUE}
cor.df<-cor(Reduced_df[2:15], use="pairwise.complete.obs")
sig.df<-cor.mtest(Reduced_df[2:15])

# Make a corrplot--originally had it show p-values but font too small to see.
corrplot(cor.df, method="color")
```

There's relatively high correlations among the variables.

At this point, we need to start examining some sort of objective or test what sort of model we might like to use. Vehicle speed (variable X16) seems like it might be an interesting outcome to work with. X16 is associated with everything BUT X7, X8, X25.

Removing the date variable first to make it easier to generate a model.
```{r remove_date, echo=TRUE}
Reduced_df<-Reduced_df[2:ncol(Reduced_df)]
```

Making another scatterplot matrix, this time on a reduced sample of the data with vehicle speed as an outcome to see what type of model might be appropriate to use if vehicle speed was set as our outcome.
```{r reduced_scatterplot_vehiclespd, echo=TRUE, out.extra='trim={0 0 0 0.2cm},clip'}
set.seed(3)
smaller<-Reduced_df[sample(nrow(Reduced_df),nrow(Reduced_df)*.1),  ]

par(mfrow=c(3,2), mai = c(0.3, 0.3, 0.1, 0.1))
plot(smaller$X16~., data=smaller, ylab = "Vehicle speed (km/h)")
```

Multiple linear regression might not be the best, but let's try try it for now and see what happens (or what the diagnostic plot post-modeling might suggest. This is just a test model).
```{r lm, echo=TRUE}
Reduced_df.lm<-lm(Reduced_df$X16~., Reduced_df)
summary(Reduced_df.lm) 
```

Output here has a lot of exponents in many of the numbers. Let's look at the distribution of our outcome variable to start (should have done this at the beginning).
```{r browse_outcome, echo=TRUE}
par(mfrow=c(1,2))

hist(Reduced_df$X16, breaks = 40, col="dodgerblue1", 
     main = "Histogram of Vehicle speed (km/h)") 
hist(Reduced_df$X16[Reduced_df$X16<20], breaks=10, col="dodgerblue1",
     main = "Histogram of Vehicle speed (km/h)")
```

We have an extremely high frequency of 0 km/h. but this is also a pretty "natural"/normal speed for a vehicle and thus the high frequently of this value is not necessarily an error/outlier--reasonable that a vehicle would be at 0 km/h often.

Perhaps we might need to try some sort of dichotomous model? But let's look at the model diagnostics first to get more information.
```{r model_diag, echo=TRUE}
par(mfrow=c(2,2), mai = c(0.5, 0.4, 0.3, 0.1))
plot(Reduced_df.lm, which=1:4)
```

The residuals vs fitted and Normal Q-Q plots look okay. Scale-location suggests that multiple linear regression might not be best, or that we might want to try transforming our outcome variable. Cook's distance suggests some outliers to look into.

<center> **Next steps?** </center>

* Possibly try transforming the outcome variable, dichotomizing it (risky), some sort of polynomial model, or a support vector machine model.

* Check for multicollinearity if using a traditional statistical model.

* Use stepwise variable selection to remove/add variables.

* Conduct likelihood ratio (LR) test between every removal/addition of a variable if using a traditional statistical model (nested)


