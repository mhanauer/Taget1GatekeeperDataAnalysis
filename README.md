---
title: "TLC Data Analysis"
output: ''
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library the packages that you need
```{r}
library(foreign)
library(nnet)
library(ggplot2)
library(prettyR)
library(nlme)
library(prettyR)
library(descr)
library(Amelia)
library(mitools)
library(BaylorEdPsych)
library(openxlsx)
library(lavaan)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(DescTools)
library(MissMech)
library(robustlmm)
library(jtools)
library(lmtest)
library(lmerTest)
```


Ok so I need to transpose the data.  Use row.names = NULL forces numbering, so that will be used when we transpose it

Then grab the variables that we want.  We want the id, treatment, section 1, sections 1 through 4 and then demographics

Then we are renaming every variable.  
```{r}
#setwd("P:/Evaluation/TN Lives Count_Writing/3_Target1_SUICClinicalTrainingComparison/3_Data & Analyses")
#datPre = read.csv("Pre.csv", header = FALSE, row.names = NULL)

datPre = t(datPre)
write.csv(datPre, "datPre.csv", row.names = FALSE)
datPre = read.csv("datPre.csv", header = TRUE)
head(datPre)

datPre = datPre[,c(1, 3, 7:18, 21:35, 38:45, 49:72, 78:80, 83, 85)]
datPre = data.frame(datPre)
head(datPre)
colnames(datPre) = c("ID", "Treatment", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB","Age", "Gender", "Eth", "Race", "Edu")
head(datPre)
### Get rif of first row once you figure out which variables to keep
datPre = datPre[-1,]
datPre = data.frame(datPre)
head(datPre)

```
Do the same as the pre where we transpose the data find the variables that correspdond to ID, treatment, and all four sections.  No demographic on post, so we need to merge the data on ID.  Variables are lined up with the variable above for naming
```{r}
#setwd("P:/Evaluation/TN Lives Count_Writing/3_Target1_SUICClinicalTrainingComparison/3_Data & Analyses")
#datPost = read.csv("Post.csv", header = FALSE, row.names = NULL)

datPost = t(datPost)
write.csv(datPost, "datPost.csv", row.names = FALSE)
datPost = read.csv("datPost.csv", header = TRUE)
head(datPost)

datPost = datPost[,c(3, 5, 15:26, 29:43, 46:53, 57:80)]

datPost = data.frame(datPost)

colnames(datPost) = c("ID", "Treatment", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB")
head(datPost)

#### Need to add these variables, because they were not include in the post survey and need to stack them ,"Age", "Gender", "Eth", "Race", "Edu"
### Need to merge the data then put into long form.
datPost$ID = as.factor(datPost$ID)
datPost = datPost[-1,]
datPost$Treatment = NULL
datPost = data.frame(datPost)

```
Now try to get the three-month follow up data

Grab all of the data that you need


```{r}
head(dat3month)
dat3month = dat3month[c(7, 11:22, 23:69)]
dim(datPost)
head(datPost)

# Now rename everything 
colnames(dat3month) = c("ID", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB")

head(dat3month)
typeof(dat3month$ID)
order(dat3month$ID)
```

Here merge all the data sets
Probably should only keep those who have data at the baseline, in case there are errors later on and baseline data was never collected?
```{r}
dim(datPre)
dim(datPost)


write.csv(datPre, "datPre.csv", row.names = FALSE)
datPre = read.csv("datPre.csv", header = TRUE)

write.csv(datPost, "datPost.csv", row.names = FALSE)
datPost = read.csv("datPost.csv", header = TRUE)
datPrePost = merge(datPre, datPost, by = "ID",  all = TRUE, sort = TRUE)


datPrePost3month = merge(datPrePost, dat3month, by = "ID", all = TRUE, sort = TRUE)

head(datPrePost3month)


### Now make long format
### These variables are not included: 							

datPrePost3month = reshape(datPrePost3month, varying  = list(c("Sec1Qa.x", "Sec1Qa.y", "Sec1Qa"), c("Sec1Qb.x", "Sec1Qb.y", "Sec1Qb"), c("Sec1Qc.x", "Sec1Qc.y", "Sec1Qc"), c("Sec1Qd.x", "Sec1Qd.y", "Sec1Qd"), c("Sec1Qe.x", "Sec1Qe.y", "Sec1Qe"), c("Sec1Qf.x", "Sec1Qf.y", "Sec1Qf"), c("Sec1Qg.x", "Sec1Qg.y", "Sec1Qg"), c("Sec1Qh.x", "Sec1Qh.y", "Sec1Qh"), c("Sec1Qi.x", "Sec1Qi.y", "Sec1Qi"), c("Sec1Qj.x", "Sec1Qj.y", "Sec1Qj"), c("Sec1Qk.x", "Sec1Qk.y", "Sec1Qk"), c("Sec1Ql.x", "Sec1Ql.y", "Sec1Ql"), c("Sec2Qa.x", "Sec2Qa.y", "Sec2Qa"), c("Sec2Qb.x", "Sec2Qb.y", "Sec2Qb"), c("Sec2Qc.x", "Sec2Qc.y", "Sec2Qc"), c("Sec2Qd.x", "Sec2Qd.y", "Sec2Qd"), c("Sec2Qe.x", "Sec2Qe.y", "Sec2Qe"), c("Sec2Qf.x", "Sec2Qf.y", "Sec2Qf"), c("Sec2Qg.x", "Sec2Qg.y", "Sec2Qg"), c("Sec2Qh.x", "Sec2Qh.y", "Sec2Qh"), c("Sec2Qi.x", "Sec2Qi.y", "Sec2Qi"), c("Sec2Qj.x", "Sec2Qj.y", "Sec2Qj"), c("Sec2Qk.x", "Sec2Qk.y", "Sec2Qk"), c("Sec2Ql.x", "Sec2Ql.y", "Sec2Ql"), c("Sec2Qm.x", "Sec2Qm.y", "Sec2Qm"), c("Sec2Qn.x", "Sec2Qn.y", "Sec2Qn"), c("Sec2Qo.x", "Sec2Qo.y", "Sec2Qo"), c("Sec3Qa.x", "Sec3Qa.y","Sec3Qa"), c("Sec3Qb.x", "Sec3Qb.y", "Sec3Qb"), c("Sec3Qc.x", "Sec3Qc.y", "Sec3Qc"), c("Sec3Qd.x", "Sec3Qd.y", "Sec3Qd"), c("Sec3Qe.x", "Sec3Qe.y", "Sec3Qe"), c("Sec3Qf.x", "Sec3Qf.y", "Sec3Qf"), c("Sec3Qg.x", "Sec3Qg.y", "Sec3Qg"), c("Sec3Qh.x", "Sec3Qh.y", "Sec3Qh"), c("Sec4QaA.x", "Sec4QaA.y", "Sec4QaA"), c("Sec4QaB.x", "Sec4QaB.y", "Sec4QaB"), c("Sec4QbA.x", "Sec4QbA.y", "Sec4QbA"), c("Sec4QbB.x", "Sec4QbB.y", "Sec4QbB"), c("Sec4QcA.x", "Sec4QcA.y", "Sec4QcA"), c("Sec4QcB.x", "Sec4QcB.y", "Sec4QcB"), c("Sec4QdA.x", "Sec4QdA.y", "Sec4QdA"), c("Sec4QdB.x", "Sec4QdB.y", "Sec4QdB"), c("Sec4QeA.x", "Sec4QeA.y", "Sec4QeA"), c("Sec4QeB.x", "Sec4QeB.y", "Sec4QeB"), c("Sec4QfA.x", "Sec4QfA.y", "Sec4QfA"), c("Sec4QfB.x", "Sec4QfB.y", "Sec4QfB"), c("Sec4QgA.x", "Sec4QgA.y", "Sec4QgA"), c("Sec4QgB.x", "Sec4QgB.y", "Sec4QgB"), c("Sec4QhA.x", "Sec4QhA.y", "Sec4QhA"), c("Sec4QhB.x", "Sec4QhB.y", "Sec4QhB"), c("Sec4QiA.x", "Sec4QiA.y", "Sec4QiA"), c("Sec4QiB.x", "Sec4QiB.y", "Sec4QiB"), c("Sec4QjA.x", "Sec4QjA.y", "Sec4QjA"), c("Sec4QjB.x", "Sec4QjB.y", "Sec4QjB"), c("Sec4QkA.x", "Sec4QkA.y", "Sec4QkA"), c("Sec4QkB.x", "Sec4QkB.y", "Sec4QkB"), c("Sec4QlA.x", "Sec4QlA.y", "Sec4QlA"), c("Sec4QlB.x", "Sec4QlB.y", "Sec4QlB")), direction = "long", times =c(0,1,2))

head(datPrePost3month)

```



Look at descirptives, then do basic psychometrics

Werid changes 1's to 2's and 0's to 1's, but it is happening and just add another ifelse statement to fix it.

```{r, include=FALSE}
write.csv(datPrePost3month, "datPrePost3month.csv", row.names = FALSE)
datPrePost3month = read.csv("datPrePost3month.csv", header = TRUE)




describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = as.factor(datPrePost3month$Sec1Qf.x)
describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = ifelse(datPrePost3month$Sec1Qf.x== 5, NA, datPrePost3month$Sec1Qf.x)
describe.factor(datPrePost3month$Sec1Qf.x)
datPrePost3month$Sec1Qf.x = ifelse(datPrePost3month$Sec1Qf.x == 2,1,0)
describe.factor(datPrePost3month$Sec1Qf.x)

describe.factor(datPrePost3month$Sec1Qi.x)
datPrePost3month$Sec1Qi.x = ifelse(datPrePost3month$Sec1Qi.x == 5, NA, datPrePost3month$Sec1Qi.x)
datPrePost3month$Sec1Qi.x = ifelse(datPrePost3month$Sec1Qi.x == 9, NA, datPrePost3month$Sec1Qi.x)
describe.factor(datPrePost3month$Sec1Qi.x)



describe.factor(datPrePost3month$Sec1Qg.x)
datPrePost3month$Sec1Qg.x = ifelse(datPrePost3month$Sec1Qg.x == 3, NA, datPrePost3month$Sec1Qg.x)
describe.factor(datPrePost3month$Sec1Qg.x)



describe.factor(datPrePost3month$Sec1Qh.x)
datPrePost3month$Sec1Qh.x = ifelse(datPrePost3month$Sec1Qh.x == 4, NA, datPrePost3month$Sec1Qh.x)
describe.factor(datPrePost3month$Sec1Qh.x)

describe.factor(datPrePost3month$Sec1Qk.x) 
datPrePost3month$Sec1Qk.x =ifelse(datPrePost3month$Sec1Qk.x == 5, NA, datPrePost3month$Sec1Qk.x)
describe.factor(datPrePost3month$Sec1Qk.x)


describe.factor(datPrePost3month$Sec2Qf.x)
datPrePost3month$Sec2Qf.x = ifelse(datPrePost3month$Sec2Qf.x == 0, NA, datPrePost3month$Sec2Qf.x)
describe.factor(datPrePost3month$Sec2Qf.x)



describe.factor(datPrePost3month$Sec1Ql.x)
datPrePost3month$Sec1Ql.x = ifelse(datPrePost3month$Sec1Ql.x > 1, NA, datPrePost3month$Sec1Ql.x)
describe.factor(datPrePost3month$Sec1Ql.x)


describe.factor(datPrePost3month$Sec2Qo.x)
datPrePost3month$Sec2Qo.x = ifelse(datPrePost3month$Sec2Qo.x == 0, NA, datPrePost3month$Sec2Qo.x)
describe.factor(datPrePost3month$Sec2Qo.x)


# 5 = -3; 4 = -2, 3 = -1, 6=0, 7=1,  8 = 2, 1 = NA, NA = NA, 9 = 3
describe.factor(datPrePost3month$Sec4QfA.x)
datPrePost3month$Sec4QfA.x = ifelse(datPrePost3month$Sec4QfA.x == " ", NA, ifelse(datPrePost3month$Sec4QfA.x == "-", NA, datPrePost3month$Sec4QfA.x))
describe.factor(datPrePost3month$Sec4QfA.x)

datPrePost3month$Sec4QfA.x = ifelse(datPrePost3month$Sec4QfA.x == 5, -3, ifelse(datPrePost3month$Sec4QfA.x  == 4, -2, ifelse(datPrePost3month$Sec4QfA.x == 3, -1, ifelse(datPrePost3month$Sec4QfA.x == 6, 0, ifelse(datPrePost3month$Sec4QfA.x == 7, 1, ifelse(datPrePost3month$Sec4QfA.x == 8, 2, ifelse(datPrePost3month$Sec4QfA.x == 1, NA, ifelse(datPrePost3month$Sec4QfA.x == 9, 3, datPrePost3month$Sec4QfA.x ))))))))
describe.factor(datPrePost3month$Sec4QfA.x)

describe.factor(datPrePost3month$Sec4QfB.x)
datPrePost3month$Sec4QfB.x = ifelse(datPrePost3month$Sec4QfB.x == -23, NA, datPrePost3month$Sec4QfB.x)
describe.factor(datPrePost3month$Sec4QfB.x)

describe.factor(datPrePost3month$Sec4QgB.x)
datPrePost3month$Sec4QgB.x = ifelse(datPrePost3month$Sec4QgB.x == -11, NA, datPrePost3month$Sec4QgB.x)
describe.factor(datPrePost3month$Sec4QgB.x)


describe.factor(datPrePost3month$Sec4QhA.x)
datPrePost3month$Sec4QhA.x = ifelse(datPrePost3month$Sec4QhA.x == -4, NA, datPrePost3month$Sec4QhA.x)
describe.factor(datPrePost3month$Sec4QhA.x)

describe.factor(datPrePost3month$Sec4QeB.x)
datPrePost3month$Sec4QeB.x = ifelse(datPrePost3month$Sec4QeB.x == -32, NA, ifelse(datPrePost3month$Sec4QeB.x == -4, NA, datPrePost3month$Sec4QeB.x))
describe.factor(datPrePost3month$Sec4QeB.x)

describe.factor(datPrePost3month$Sec2Qh.x)
datPrePost3month$Sec2Qh.x = ifelse(datPrePost3month$Sec2Qh.x == 56, NA, datPrePost3month$Sec2Qh.x)
describe.factor(datPrePost3month$Sec2Qh.x)

summary(datPrePost3month)
```
Need the deviation from each measure with the expert score.  

Need to do this for every pair.  Rename each var with
```{r}
summary(datPrePost3month)

head(datPrePost3month)

datPrePost3month$Sec4QaA.x =  datPrePost3month$Sec4QaA.x--2.71
datPrePost3month$Sec4QaB.x =  datPrePost3month$Sec4QaB.x- 1.86 

datPrePost3month$Sec4QbA.x =  datPrePost3month$Sec4QbA.x--2.71
datPrePost3month$Sec4QbB.x =  datPrePost3month$Sec4QbB.x- 1.86 

datPrePost3month$Sec4QcA.x =  datPrePost3month$Sec4QcA.x--2.14
datPrePost3month$Sec4QcB.x =  datPrePost3month$Sec4QcB.x-2.14 

datPrePost3month$Sec4QdA.x =  datPrePost3month$Sec4QdA.x-1.29 
datPrePost3month$Sec4QdB.x =  datPrePost3month$Sec4QdB.x--2.71

datPrePost3month$Sec4QeA.x =  datPrePost3month$Sec4QeA.x-2.43 
datPrePost3month$Sec4QeB.x =  datPrePost3month$Sec4QeB.x--2.71 

datPrePost3month$Sec4QfA.x =  datPrePost3month$Sec4QfA.x--2 
datPrePost3month$Sec4QfB.x =  datPrePost3month$Sec4QfB.x-2.57


datPrePost3month$Sec4QgA.x =  datPrePost3month$Sec4QgA.x-2  
datPrePost3month$Sec4QgB.x =  datPrePost3month$Sec4QgB.x--1.29 

datPrePost3month$Sec4QhA.x =  datPrePost3month$Sec4QhA.x--2.29 
datPrePost3month$Sec4QhB =   datPrePost3month$Sec4QhB.x-2.14

datPrePost3month$Sec4QiA.x =  datPrePost3month$Sec4QiA.x--1.29 
datPrePost3month$Sec4QiB.x =  datPrePost3month$Sec4QiB.x-1.29  

datPrePost3month$Sec4QjA.x =  datPrePost3month$Sec4QjA.x-2.29 
datPrePost3month$Sec4QjB.x =  datPrePost3month$Sec4QjB.x--2.43  

datPrePost3month$Sec4QkA.x =  datPrePost3month$Sec4QkA.x--2.42  
datPrePost3month$Sec4QkB.x =  datPrePost3month$Sec4QkB.x-2.43 

datPrePost3month$Sec4QlA.x =  datPrePost3month$Sec4QlA.x-2.00 
datPrePost3month$Sec4QlB.x =  datPrePost3month$Sec4QlB.x-3.00 

```
Now try to get the psychometrics.  Just try reliabilty for the first one
Just get the baseline values, try invar later on with time.

Subsetting items for each measure by putting them together so we can measure reliability.  

Then creating a new data set where we are only getting the baseline, so just measuring baseline psychometrics

Focus on omega

Then for measure three I am reverse scoring, but will change this with more information
```{r}
## Reliability for section one
head(datPrePost3month)


datPrePost3monthSec1 = datPrePost3month[,c(8,9:20)]
head(datPrePost3monthSec1)
datPrePost3monthSec1Base = subset(datPrePost3monthSec1, time == 0)
describe.factor(datPrePost3monthSec1Base$time)
datPrePost3monthSec1Base$time = NULL

datPrePost3monthSec1Base = data.frame(datPrePost3monthSec1Base)
write.csv(datPrePost3monthSec1Base, "datPrePost3monthSec1Base.csv", row.names = FALSE)
datPrePost3monthSec1Base = read.csv("datPrePost3monthSec1Base.csv", header = TRUE)

CronbachAlpha(datPrePost3monthSec1Base, na.rm = TRUE)
omegaSec1Base = omega(datPrePost3monthSec1Base)
summary(omegaSec1Base)

## Reliability for section two
head(datPrePost3month)
datPrePost3monthSec2 = datPrePost3month[,c(8, 21:35)]
head(datPrePost3monthSec2)


datPrePost3monthSec2Base = subset(datPrePost3monthSec2, time == 0)
describe.factor(datPrePost3monthSec2Base$time)
datPrePost3monthSec2Base$time = NULL

datPrePost3monthSec2Base = data.frame(datPrePost3monthSec2Base)
write.csv(datPrePost3monthSec2Base, "datPrePost3monthSec2Base.csv", row.names = FALSE)
datPrePost3monthSec2Base = read.csv("datPrePost3monthSec2Base.csv", header = TRUE)

CronbachAlpha(datPrePost3monthSec2Base, na.rm = TRUE)
summary(datPrePost3monthSec2Base)

omegaSec2Base =  omega(datPrePost3monthSec2Base)
summary(omegaSec2Base)

#Reliabiltiy for section three
head(datPrePost3month)
datPrePost3monthSec3 = datPrePost3month[,c(8, 36:43)]
### Need to get reverse scoring for A,C,E,G
head(datPrePost3monthSec3)
summary(datPrePost3monthSec3)

datPrePost3monthSec3$Sec3Qa.x = ifelse(datPrePost3monthSec3$Sec3Qa.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qa.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qa.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qa.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qa.x == 5,1,datPrePost3monthSec3$Sec3Qa.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qa.x)


datPrePost3monthSec3$Sec3Qc.x = ifelse(datPrePost3monthSec3$Sec3Qc.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qc.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qc.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qc.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qc.x == 5,1,datPrePost3monthSec3$Sec3Qc.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qc.x)

datPrePost3monthSec3$Sec3Qd.x = ifelse(datPrePost3monthSec3$Sec3Qd.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qd.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qd.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qd.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qd.x == 5,1,datPrePost3monthSec3$Sec3Qd.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qd.x)


datPrePost3monthSec3$Sec3Qg.x = ifelse(datPrePost3monthSec3$Sec3Qg.x == 1, 5, ifelse(datPrePost3monthSec3$Sec3Qg.x == 2,4, ifelse(datPrePost3monthSec3$Sec3Qg.x == 3,3, ifelse(datPrePost3monthSec3$Sec3Qg.x == 4,2, ifelse(datPrePost3monthSec3$Sec3Qg.x == 5,1,datPrePost3monthSec3$Sec3Qg.x)))))
describe.factor(datPrePost3monthSec3$Sec3Qg.x)


datPrePost3monthSec3Base = subset(datPrePost3monthSec3, time == 0)
describe.factor(datPrePost3monthSec3Base$time)
datPrePost3monthSec3Base$time = NULL

datPrePost3monthSec3Base = data.frame(datPrePost3monthSec3Base)
write.csv(datPrePost3monthSec3Base, "datPrePost3monthSec3Base.csv", row.names = FALSE)
datPrePost3monthSec3Base = read.csv("datPrePost3monthSec3Base.csv", header = TRUE)

CronbachAlpha(datPrePost3monthSec3Base, na.rm = TRUE)

omegaSec3Base =  omega(datPrePost3monthSec3Base)
summary(omegaSec3Base)


##Reliability for section four
head(datPrePost3month)
datPrePost3monthSec4 = datPrePost3month[,c(8, 44:67)]
head(datPrePost3monthSec4)

datPrePost3monthSec4Base = subset(datPrePost3monthSec4, time == 0)
describe.factor(datPrePost3monthSec4Base$time)
datPrePost3monthSec4Base$time = NULL

datPrePost3monthSec4Base = data.frame(datPrePost3monthSec4Base)
write.csv(datPrePost3monthSec4Base, "datPrePost3monthSec4Base.csv", row.names = FALSE)
datPrePost3monthSec4Base = read.csv("datPrePost3monthSec4Base.csv", header = TRUE)

CronbachAlpha(datPrePost3monthSec4Base, na.rm = TRUE)
summary(datPrePost3monthSec4Base)

omegaSec4Base =  omega(datPrePost3monthSec4Base)
summary(omegaSec4Base)

```
Ok now try EFA for all four
```{r}
parallel1 = fa.parallel(datPrePost3monthSec1Base, fa= "fa")
parallel1$fa.values

parallel2 = fa.parallel(datPrePost3monthSec2Base, fa= "fa")
parallel2$fa.values

parallel3 = fa.parallel(datPrePost3monthSec3Base, fa= "fa")
parallel3$fa.values

parallel4 = fa.parallel(datPrePost3monthSec4Base, fa= "fa")
parallel4$fa.values

```
Try CFA for three assessments
```{r}
head(datPrePost3monthSec1Base)
model1 = 'SA =~ Sec1Qa.x + Sec1Qb.x + Sec1Qc.x + Sec1Qd.x + Sec1Qe.x + Sec1Qf.x + Sec1Qe.x + Sec1Qf.x + Sec1Qg.x + Sec1Qh.x + Sec1Qi.x + Sec1Qk.x + Sec1Ql.x'

fit1 = cfa(model1, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = datPrePost3monthSec1Base)
summary(fit1, fit.measures = TRUE)


head(datPrePost3monthSec2Base)
model2 = 'SA =~ Sec2Qa.x + Sec2Qb.x + Sec2Qc.x + Sec2Qd.x + Sec2Qe.x + Sec2Qf.x + Sec2Qg.x + Sec2Qh.x + Sec2Qi.x + Sec2Qj.x + Sec2Qk.x + Sec2Ql.x + Sec2Qm.x + Sec2Qn.x + Sec2Qo.x'

fit2 = cfa(model2, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = datPrePost3monthSec2Base)
summary(fit2, fit.measures = TRUE)


head(datPrePost3monthSec3Base)
model3 = 'SA =~ Sec3Qa.x + Sec3Qb.x + Sec3Qc.x + Sec3Qd.x + Sec3Qe.x + Sec3Qf.x + Sec3Qg.x + Sec3Qh.x'

fit3 = cfa(model3, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = datPrePost3monthSec3Base)
summary(fit3, fit.measures = TRUE)


head(datPrePost3monthSec4Base)
model4 = 'SA =~ Sec4QaA.x + Sec4QaB.x + Sec4QbA.x + Sec4QbB.x + Sec4QcA.x +Sec4QcB.x + Sec4QdA.x + Sec4QdB.x + Sec4QeA.x + Sec4QeB.x + Sec4QfA.x + Sec4QfB.x + Sec4QgA.x + Sec4QgB.x + Sec4QhA.x + Sec4QhB.x + Sec4QiA.x + Sec4QiB.x + Sec4QjA.x + Sec4QjB.x + Sec4QkA.x + Sec4QkB.x + Sec4QlA.x + Sec4QlB.x'


fit4 = cfa(model4, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = datPrePost3monthSec4Base)
summary(fit4, fit.measures = TRUE)

```




Now we are getting total scores for the three measures use apply and create a smaller data set.
Ok so we are summing up even with missing.  So if missing then just skips that value.

Then put that data set back together call it analysis at the end.


Only 5% if the data is missing, but missing at random test is significant

For this analysis, change gender 1,0, Race to white versus non-white (5 versus everything else), edu to bachelors and below versus masters and above (5 and below 1)
Ok moving on to analysis
Data cleaning for analysis

Need to make sure time is out before the row sums, because we do not want to count them.

If you sum up a row a of all NAs then that should be NA.  So any value that is zero is NA, because it is not poss
```{r}
datPrePost3monthSec1$time = NULL
datPrePost3monthSec2$time = NULL
datPrePost3monthSec3$time = NULL
datPrePost3monthSec4$time = NULL

write.csv(datPrePost3monthSec1, "datPrePost3monthSec1.csv", row.names = FALSE)
datPrePost3monthSec1 = read.csv("datPrePost3monthSec1.csv", header = TRUE)

write.csv(datPrePost3monthSec2, "datPrePost3monthSec2.csv", row.names = FALSE)
datPrePost3monthSec2 = read.csv("datPrePost3monthSec2.csv", header = TRUE)

write.csv(datPrePost3monthSec3, "datPrePost3monthSec3.csv", row.names = FALSE)
datPrePost3monthSec3 = read.csv("datPrePost3monthSec3.csv", header = TRUE)

write.csv(datPrePost3monthSec4, "datPrePost3monthSec4.csv", row.names = FALSE)
datPrePost3monthSec4 = read.csv("datPrePost3monthSec4.csv", header = TRUE)

write.csv(datPrePost3monthSec1, "datPrePost3monthSec1.csv", row.names = FALSE)


sum(is.na(datPrePost3monthSec1))
Sec1Total = rowSums(datPrePost3monthSec1)
Sec2Total = rowSums(datPrePost3monthSec2)
Sec3Total = rowSums(datPrePost3monthSec3)
Sec4Total = rowSums(datPrePost3monthSec4)


datPrePost3monthAnalysis = data.frame(ID = datPrePost3month$ID, Treatment = datPrePost3month$Treatment, Age =  datPrePost3month$Age, Gender = datPrePost3month$Gender, Race = datPrePost3month$Race, Edu = datPrePost3month$Edu, Time = datPrePost3month$time,Sec1Total =  Sec1Total, Sec2Total = Sec2Total, Sec3Total = Sec3Total, Sec4Total = Sec4Total)

datPrePost3monthAnalysis$Gender = ifelse(datPrePost3monthAnalysis$Gender == 1,1,0)
datPrePost3monthAnalysis$Race = ifelse(datPrePost3monthAnalysis$Race == 5, 0, 1)
datPrePost3monthAnalysis$Edu = ifelse(datPrePost3monthAnalysis$Edu < 6, 1, 0)

#datPrePost3monthAnalysisComplete = subset(datPrePost3monthAnalysis, Treatment == 1 | Treatment == 2)

# Getting the data ready
write.csv(datPrePost3monthAnalysis, "datPrePost3monthAnalysis.csv", row.names = FALSE)
datPrePost3monthAnalysis = read.csv("datPrePost3monthAnalysis.csv", header = TRUE)

datPrePost3monthAnalysis$Treatment
# Maybe make sure time and treatment are treated as factors
datPrePost3monthAnalysis$Treatment = as.factor(datPrePost3monthAnalysis$Treatment)
datPrePost3monthAnalysis$Time = as.factor(datPrePost3monthAnalysis$Time)
datPrePost3monthAnalysis$Gender = as.factor(datPrePost3monthAnalysis$Gender)
datPrePost3monthAnalysis$Race = as.factor(datPrePost3monthAnalysis$Race)
datPrePost3monthAnalysis$Edu = as.factor(datPrePost3monthAnalysis$Edu)
```
Assess missing values for prePost3month and prePost
```{r}
sum(is.na(datPrePost3monthAnalysis))
datPrePost3monthAnalysisComplete = na.omit(datPrePost3monthAnalysis)
dim(datPrePost3monthAnalysis)[1]
dim(datPrePost3monthAnalysisComplete)[1]

1-(dim(datPrePost3monthAnalysisComplete)[1] / dim(datPrePost3monthAnalysis)[1])

# Missing values for prePost
write.csv(datPrePost3monthAnalysis, "datPrePost3monthAnalysis.csv", row.names = FALSE)
datPrePost3monthAnalysis = read.csv("datPrePost3monthAnalysis.csv", header = TRUE)
datPrePost3monthAnalysis$Time = as.factor(datPrePost3monthAnalysis$Time)
datPrePostAnalysis = subset(datPrePost3monthAnalysis, Time  == 0 | Time == 1)
write.csv(datPrePostAnalysis, "datPrePostAnalysis.csv", row.names = FALSE)
datPrePostAnalysis = read.csv("datPrePostAnalysis.csv", header = TRUE)

## Ok no need to impute for pre and post
datPrePostAnalysisComplete = na.omit(datPrePostAnalysis)
1-(dim(datPrePostAnalysisComplete)[1]/dim(datPrePostAnalysis)[1])
TestMCARNormality(datPrePostAnalysis)

## Missing values for pre, post, and 3month
datPrePost3monthAnalysisComplete = na.omit(datPrePost3monthAnalysis)
1-(dim(datPrePost3monthAnalysisComplete)[1]/dim(datPrePost3monthAnalysis)[1])
write.csv(datPrePost3monthAnalysis, "datPrePost3monthAnalysis.csv", row.names = FALSE)
datPrePost3monthAnalysis = read.csv("datPrePost3monthAnalysis.csv", header = TRUE)

TestMCARNormality(datPrePost3monthAnalysis)
```
Running model one with complete 
```{r}


modelOutcome1 = lmer(Sec1Total ~ factor(Treatment)*Time  +  Edu + Gender + Age + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome1)

library(emmeans)
m.emm = emmeans(modelOutcome1, "Treatment")
m.emm
contrast()

## R^2
r.squaredLR(modelOutcome1)

## Plotting the interaction effect
cat_plot(modelOutcome1, pred = "Time", modx = "Treatment", cluster = "ID")

modelOutcome1Robust = rlmer(Sec1Total ~ Treatment*Time + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome1Robust)
# Get p-values
coefs = data.frame(coef(summary(modelOutcome1Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

### Getting residuals for level one
residModel1Level1 = HLMresid(modelOutcome1, level = 1, standardize = TRUE)

head(residModel1Level1)
ggplot_qqnorm(x = residModel1Level1, line = "rlm")

### Getting residuals for level two
residModel1Level2 = HLMresid(modelOutcome1, level = "ID", standardize = TRUE)
head(residModel1Level2)
ggplot_qqnorm(x = residModel1Level2$`(Intercept)`, line = "rlm")

```
Model one moderators
```{r}
## Try model two with different moderators
modelOutcome1Age = lmer(Sec1Total ~ Treatment*Time*Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome1Age)

modelOutcome1Edu = lmer(Sec1Total ~ Treatment*Time*Edu + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome1Edu)

modelOutcome1Gender = lmer(Sec1Total ~ Treatment*Time*Gender + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome1Gender)

modelOutcome1Race = lmer(Sec1Total ~ Treatment*Time*Race + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome1Race)
```

Model two 
```{r}
## Final model goes here
modelOutcome2 = lmer(Sec2Total ~ Treatment*Time + Edu + Gender + Age + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome2)

anova(modelOutcome2, L = c("NumericPackage3:time" = 1, "NumericPackage2:time" = -1))


resid2 = HLMresid(modelOutcome2, level = 1, standardize  = TRUE)
head(resid2)

# R^2
r.squaredLR(modelOutcome2)

## Plotting the interaction effect
cat_plot(modelOutcome2, pred = "Time", modx = "Treatment", cluster = "ID")

###  Robust model here same results
modelOutcome2Robust = rlmer(Sec2Total ~ Treatment*Time + Edu + Gender + Age + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
# Get p-values
coefs = data.frame(coef(summary(modelOutcome2Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs


### Getting residuals for level one
residModel2Level1 = HLMresid(modelOutcome2, level = 1, standardize = TRUE)

head(residModel2Level1)
ggplot_qqnorm(x = residModel2Level1, line = "rlm")
hist(residModel2Level1)
summary(residModel2Level1)

### Getting residuals for level two
residModel2Level2 = HLMresid(modelOutcome2, level = "ID", standardize = TRUE)
head(residModel2Level2)
ggplot_qqnorm(x = residModel2Level2$`(Intercept)`, line = "rlm")
hist(residModel2Level2)
summary(residModel2Level2)

## 

```
Model two moderators
```{r}
## Try model two with different moderators
modelOutcome2Age = lmer(Sec2Total ~ Treatment*Time*Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome2Age)

modelOutcome2Edu = lmer(Sec2Total ~ Treatment*Time*Edu + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome2Edu)

modelOutcome2Gender = lmer(Sec2Total ~ Treatment*Time*Gender + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome2Gender)

modelOutcome2Race = lmer(Sec2Total ~ Treatment*Time*Race + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome2Race)
```


Model three 
```{r}
## Final model goes here
modelOutcome3 = lmer(Sec3Total ~ Treatment*Time + Edu + Gender + Age + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome3)

## Plotting the interaction effect
cat_plot(modelOutcome3, pred = "Time", modx = "Treatment", cluster = "ID")

## R^2
r.squaredLR(modelOutcome3)


## Robust version
modelOutcome3Robust = rlmer(Sec3Total ~ Treatment*Time + Edu + Gender + Age + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome3Robust)
coefs = data.frame(coef(summary(modelOutcome3Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

### Getting residuals for level one
residModel3Level1 = HLMresid(modelOutcome3, level = 1, standardize = TRUE)

head(residModel3Level1)
ggplot_qqnorm(x = residModel3Level1, line = "rlm")
hist(residModel3Level1)
summary(residModel3Level1)

### Getting residuals for level two
residModel3Level2 = HLMresid(modelOutcome3, level = "ID", standardize = TRUE)
head(residModel3Level2)
ggplot_qqnorm(x = residModel3Level2$`(Intercept)`, line = "rlm")
hist(residModel3Level2)
summary(residModel3Level2)
```
Model three moderators
```{r}
## Try model two with different moderators
modelOutcome3Age = lmer(Sec3Total ~ Treatment*Time*Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome3Age)

modelOutcome3Edu = lmer(Sec3Total ~ Treatment*Time*Edu + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome3Edu)


modelOutcome3Gender = lmer(Sec3Total ~ Treatment*Time*Gender + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome3Gender)

# Cross over interaction .09 maybe worth talking about
modelOutcome3Race = lmer(Sec3Total ~ Treatment*Time*Race + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome3Race)
```



Model 4 
```{r}
## Final model goes here
modelOutcome4 = lmer(Sec4Total ~ Treatment*Time + Edu + Gender + Age + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome4)


## Plotting the interaction effect
cat_plot(modelOutcome4, pred = "Time", modx = "Treatment", cluster = "ID")

##R^2
r.squaredLR(modelOutcome4)


##Robust version
modelOutcome4Robust = rlmer(Sec4Total ~ Treatment*Time + Edu + Gender + Age + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome4Robust)
coefs = data.frame(coef(summary(modelOutcome4Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

### Getting residuals for level one
residModel4Level1 = HLMresid(modelOutcome4, level = 1, standardize = TRUE)

head(residModel4Level1)
ggplot_qqnorm(x = residModel4Level1, line = "rlm")
hist(residModel4Level1)
summary(residModel4Level1)

### Getting residuals for level two
residModel4Level2 = HLMresid(modelOutcome4, level = "ID", standardize = TRUE)
head(residModel4Level2)
ggplot_qqnorm(x = residModel4Level2$`(Intercept)`, line = "rlm")
hist(residModel4Level2)
summary(residModel4Level2)

leverage(modelOutcome4, level = 1)
```
Model four moderators
```{r}
## Try model two with different moderators
modelOutcome4Age = lmer(Sec4Total ~ Treatment*Time*Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome4Age)

modelOutcome4Edu = lmer(Sec4Total ~ Treatment*Time*Edu + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome4Edu)

# Significant
modelOutcome4Gender = lmer(Sec4Total ~ Treatment*Time*Gender + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome4Gender)

cat_plot(modelOutcome4Gender, pred = "Time", modx = "Treatment", mod2 = "Gender", cluster = "ID")


modelOutcome4Race = lmer(Sec4Total ~ Treatment*Time*Race + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePost3monthAnalysisComplete)
summary(modelOutcome4Race)
```
####################################################
Running models starting with one with complete for Pre and Post
####################################################
```{r}
datPrePostAnalysisComplete$Treatment = as.factor(datPrePostAnalysisComplete$Treatment)
modelOutcome1 = lmer(Sec1Total ~ Treatment*Time  +  Edu + Gender + Age + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome1)

## R^2
r.squaredLR(modelOutcome1)

## Plotting the interaction effect
cat_plot(modelOutcome1, pred = "Time", modx = "Treatment", cluster = "ID")

modelOutcome1Robust = rlmer(Sec1Total ~ Treatment*Time + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome1Robust)
# Get p-values
coefs = data.frame(coef(summary(modelOutcome1Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

### Getting residuals for level one
residModel1Level1 = HLMresid(modelOutcome1, level = 1, standardize = TRUE)

head(residModel1Level1)
ggplot_qqnorm(x = residModel1Level1, line = "rlm")

### Getting residuals for level two
residModel1Level2 = HLMresid(modelOutcome1, level = "ID", standardize = TRUE)
head(residModel1Level2)
ggplot_qqnorm(x = residModel1Level2$`(Intercept)`, line = "rlm")

```
Model one moderators
```{r}
## Try model two with different moderators
modelOutcome1Age = lmer(Sec1Total ~ Treatment*Time*Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome1Age)

modelOutcome1Edu = lmer(Sec1Total ~ Treatment*Time*Edu + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome1Edu)

modelOutcome1Gender = lmer(Sec1Total ~ Treatment*Time*Gender + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome1Gender)

modelOutcome1Race = lmer(Sec1Total ~ Treatment*Time*Race + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome1Race)
```

Model two 
```{r}
## Final model goes here
modelOutcome2 = lmer(Sec2Total ~ Treatment*Time + Edu + Gender + Age + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome2)
datPrePostAnalysisComplete$Treatment
resid2 = HLMresid(modelOutcome2, level = 1, standardize  = TRUE)
head(resid2)

# R^2
r.squaredLR(modelOutcome2)

## Plotting the interaction effect
cat_plot(modelOutcome2, pred = "Time", modx = "Treatment", cluster = "ID")

###  Robust model here same results
modelOutcome2Robust = rlmer(Sec2Total ~ Treatment*Time + Edu + Gender + Age + Race + (1 | ID),  data = datPrePostAnalysisComplete)
# Get p-values
coefs = data.frame(coef(summary(modelOutcome2Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs


### Getting residuals for level one
residModel2Level1 = HLMresid(modelOutcome2, level = 1, standardize = TRUE)

head(residModel2Level1)
ggplot_qqnorm(x = residModel2Level1, line = "rlm")
hist(residModel2Level1)
summary(residModel2Level1)

### Getting residuals for level two
residModel2Level2 = HLMresid(modelOutcome2, level = "ID", standardize = TRUE)
head(residModel2Level2)
ggplot_qqnorm(x = residModel2Level2$`(Intercept)`, line = "rlm")
hist(residModel2Level2)
summary(residModel2Level2)
```
Model two moderators
```{r}
## Try model two with different moderators
modelOutcome2Age = lmer(Sec2Total ~ Treatment*Time*Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome2Age)

modelOutcome2Edu = lmer(Sec2Total ~ Treatment*Time*Edu + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome2Edu)

modelOutcome2Gender = lmer(Sec2Total ~ Treatment*Time*Gender + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome2Gender)

modelOutcome2Race = lmer(Sec2Total ~ Treatment*Time*Race + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome2Race)
```


Model three 
```{r}
## Final model goes here
modelOutcome3 = lmer(Sec3Total ~ Treatment*Time + Edu + Gender + Age + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome3)

## Plotting the interaction effect
cat_plot(modelOutcome3, pred = "Time", modx = "Treatment", cluster = "ID")

## R^2
r.squaredLR(modelOutcome3)


## Robust version
modelOutcome3Robust = rlmer(Sec3Total ~ Treatment*Time + Edu + Gender + Age + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome3Robust)
coefs = data.frame(coef(summary(modelOutcome3Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

### Getting residuals for level one
residModel3Level1 = HLMresid(modelOutcome3, level = 1, standardize = TRUE)

head(residModel3Level1)
ggplot_qqnorm(x = residModel3Level1, line = "rlm")
hist(residModel3Level1)
summary(residModel3Level1)

### Getting residuals for level two
residModel3Level2 = HLMresid(modelOutcome3, level = "ID", standardize = TRUE)
head(residModel3Level2)
ggplot_qqnorm(x = residModel3Level2$`(Intercept)`, line = "rlm")
hist(residModel3Level2)
summary(residModel3Level2)
```
Model three moderators
```{r}
## Try model two with different moderators
modelOutcome3Age = lmer(Sec3Total ~ Treatment*Time*Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome3Age)

modelOutcome3Edu = lmer(Sec3Total ~ Treatment*Time*Edu + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome3Edu)


modelOutcome3Gender = lmer(Sec3Total ~ Treatment*Time*Gender + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome3Gender)

# Cross over interaction .09 maybe worth talking about
modelOutcome3Race = lmer(Sec3Total ~ Treatment*Time*Race + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome3Race)
```



Model 4 
```{r}
## Final model goes here
modelOutcome4 = lmer(Sec4Total ~ Treatment*Time + Edu + Gender + Age + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome4)


## Plotting the interaction effect
cat_plot(modelOutcome4, pred = "Time", modx = "Treatment", cluster = "ID")

##R^2
r.squaredLR(modelOutcome4)


##Robust version
modelOutcome4Robust = rlmer(Sec4Total ~ Treatment*Time + Edu + Gender + Age + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome4Robust)
coefs = data.frame(coef(summary(modelOutcome4Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

### Getting residuals for level one
residModel4Level1 = HLMresid(modelOutcome4, level = 1, standardize = TRUE)

head(residModel4Level1)
ggplot_qqnorm(x = residModel4Level1, line = "rlm")
hist(residModel4Level1)
summary(residModel4Level1)

### Getting residuals for level two
residModel4Level2 = HLMresid(modelOutcome4, level = "ID", standardize = TRUE)
head(residModel4Level2)
ggplot_qqnorm(x = residModel4Level2$`(Intercept)`, line = "rlm")
hist(residModel4Level2)
summary(residModel4Level2)

leverage(modelOutcome4, level = 1)
```
Model four moderators
```{r}
## Try model two with different moderators
modelOutcome4Age = lmer(Sec4Total ~ Treatment*Time*Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome4Age)

modelOutcome4Edu = lmer(Sec4Total ~ Treatment*Time*Edu + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome4Edu)

# Significant
modelOutcome4Gender = lmer(Sec4Total ~ Treatment*Time*Gender + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome4Gender)

cat_plot(modelOutcome4Gender, pred = "Time", modx = "Treatment", mod2 = "Gender", cluster = "ID")


modelOutcome4Race = lmer(Sec4Total ~ Treatment*Time*Race + Age + Edu + Gender  + Race + (1 | ID),  data = datPrePostAnalysisComplete)
summary(modelOutcome4Race)
```


Extra code
```{r}
### Try random effects model cannot run too many covariates
modelOutcome2Robust = rlmer(Sec2Total ~ Treatment*Time + Edu + Gender + Age + Race + (Time | ID),  data = datPrePostAnalysisComplete)
# Get p-values
coefs = data.frame(coef(summary(modelOutcome2Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

### Try random effects model try no covariates nope won't run either just random intercepts
modelOutcome2Robust = rlmer(Sec2Total ~ Treatment*Time + (Time | ID),  data = datPrePostAnalysisComplete)
# Get p-values
coefs = data.frame(coef(summary(modelOutcome2Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
```



Extra code
```{r}
### Try random effects model cannot run too many covariates
modelOutcome2Robust = rlmer(Sec2Total ~ Treatment*Time + Edu + Gender + Age + Race + (Time | ID),  data = datPrePost3monthAnalysisComplete)
# Get p-values
coefs = data.frame(coef(summary(modelOutcome2Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

### Try random effects model try no covariates nope won't run either just random intercepts
modelOutcome2Robust = rlmer(Sec2Total ~ Treatment*Time + (Time | ID),  data = datPrePost3monthAnalysisComplete)
# Get p-values
coefs = data.frame(coef(summary(modelOutcome2Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
```




