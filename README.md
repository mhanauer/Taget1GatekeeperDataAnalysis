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
datPost = data.frame(datPost)



datPrePost = merge(datPre, datPost, by = "ID",  all = TRUE)

### Now make long format
### These variables are not included: 							

datPrePost = reshape(datPrePost, varying  = list(c("Treatment.x", "Treatment.y"), c("Sec1Qa.x", "Sec1Qa.y"), c("Sec1Qb.x", "Sec1Qb.y"), c("Sec1Qc.x", "Sec1Qc.y"), c("Sec1Qd.x", "Sec1Qd.y"), c("Sec1Qe.x", "Sec1Qe.y"), c("Sec1Qf.x", "Sec1Qf.y"), c("Sec1Qg.x", "Sec1Qg.y"), c("Sec1Qh.x", "Sec1Qh.y"), c("Sec1Qi.x", "Sec1Qi.y"), c("Sec1Qk.x", "Sec1Qk.y"), c("Sec1Ql.x", "Sec1Ql.y"), c("Sec2Qa.x", "Sec2Qa.y"), c("Sec2Qb.x", "Sec2Qb.y"), c("Sec2Qc.x", "Sec2Qc.y"), c("Sec2Qd.x", "Sec2Qd.y"), c("Sec2Qe.x", "Sec2Qe.y"), c("Sec2Qf.x", "Sec2Qf.y"), c("Sec2Qg.x", "Sec2Qg.y"), c("Sec2Qh.x", "Sec2Qh.y"), c("Sec2Qi.x", "Sec2Qi.y"), c("Sec2Qj.x", "Sec2Qj.y"), c("Sec2Qk.x", "Sec2Qk.y"), c("Sec2Ql.x", "Sec2Ql.y"), c("Sec2Qm.x", "Sec2Qm.y"), c("Sec2Qn.x", "Sec2Qn.y"), c("Sec2Qo.x", "Sec2Qo.y"), c("Sec3Qa.x", "Sec3Qa.y"), c("Sec3Qb.x", "Sec3Qb.y"), c("Sec3Qc.x", "Sec3Qc.y"), c("Sec3Qd.x", "Sec3Qd.y"), c("Sec3Qe.x", "Sec3Qe.y"), c("Sec3Qf.x", "Sec3Qf.y"), c("Sec3Qg.x", "Sec3Qg.y"), c("Sec3Qh.x", "Sec3Qh.y"), c("Sec4QaA.x", "Sec4QaA.y"), c("Sec4QfA.x", "Sec4QfA.y"), c("Sec4QfB.x", "Sec4QfB.y"), c("Sec4QgA.x", "Sec4QgA.y"), c("Sec4QgB.x", "Sec4QgB.y"), c("Sec4QhA.x", "Sec4QhA.y"), c("Sec4QhB.x", "Sec4QhB.y"), c("Sec4QiA.x", "Sec4QiA.y"), c("Sec4QiB.x", "Sec4QiB.y"), c("Sec4QjA.x", "Sec4QjA.y"), c("Sec4QjB.x", "Sec4QjB.y"), c("Sec4QkA.x", "Sec4QkA.y"), c("Sec4QkB.x", "Sec4QkB.y"), c("Sec4QlA.x", "Sec4QlA.y"), c("Sec4QlB.x", "Sec4QlB.y"), c("Sec1Qj.x", "Sec1Qj.y"), c("Sec4QaB.x", "Sec4QaB.y"), c("Sec4QbA.x", "Sec4QbA.y"), c("Sec4QbB.x", "Sec4QbB.y"), c("Sec4QcA.x", "Sec4QcA.y"), c("Sec4QcB.x", "Sec4QcB.y"), c("Sec4QdA.x", "Sec4QdA.y"), c("Sec4QdB.x", "Sec4QdB.y"), c("Sec4QeA.x", "Sec4QeA.y"), c("Sec4QeB.x", "Sec4QeB.y")), direction = "long", times =c(0,1))

```
Look at descirptives, then do basic psychometrics

Werid changes 1's to 2's and 0's to 1's, but it is happening and just add another ifelse statement to fix it.

```{r}
write.csv(datPrePost, "datPrePost.csv", row.names = FALSE)
datPrePost = read.csv("datPrePost.csv", header = TRUE)



describe.factor(datPrePost$Sec1Qf.x)
datPrePost$Sec1Qf.x = as.factor(datPrePost$Sec1Qf.x)
describe.factor(datPrePost$Sec1Qf.x)
datPrePost$Sec1Qf.x = ifelse(datPrePost$Sec1Qf.x== 5, NA, datPrePost$Sec1Qf.x)
describe.factor(datPrePost$Sec1Qf.x)



describe.factor(datPrePost$Sec1Qi.x)
datPrePost$Sec1Qi.x = ifelse(datPrePost$Sec1Qi.x == 5, NA, datPrePost$Sec1Qi.x)
datPrePost$Sec1Qi.x = ifelse(datPrePost$Sec1Qi.x == 9, NA, datPrePost$Sec1Qi.x)
describe.factor(datPrePost$Sec1Qi.x)



describe.factor(datPrePost$Sec1Qg.x)
datPrePost$Sec1Qg.x = ifelse(datPrePost$Sec1Qg.x == 3, NA, datPrePost$Sec1Qg.x)
describe.factor(datPrePost$Sec1Qg.x)



describe.factor(datPrePost$Sec1Qh.x)
datPrePost$Sec1Qh.x = ifelse(datPrePost$Sec1Qh.x == 4, NA, datPrePost$Sec1Qh.x)
describe.factor(datPrePost$Sec1Qh.x)

describe.factor(datPrePost$Sec1Qk.x) 
datPrePost$Sec1Qk.x =ifelse(datPrePost$Sec1Qk.x == 5, NA, datPrePost$Sec1Qk.x)
describe.factor(datPrePost$Sec1Qk.x)
summary(datPrePost)

describe.factor(datPrePost$Sec2Qf.x)
datPrePost$Sec2Qf.x = ifelse(datPrePost$Sec2Qf.x == 0, NA, datPrePost$Sec2Qf.x)
describe.factor(datPrePost$Sec2Qf.x)



describe.factor(datPrePost$Sec1Ql.x)
datPrePost$Sec1Ql.x = ifelse(datPrePost$Sec1Ql.x > 1, NA, datPrePost$Sec1Ql.x)
describe.factor(datPrePost$Sec1Ql.x)


describe.factor(datPrePost$Sec2Qo.x)
datPrePost$Sec2Qo.x = ifelse(datPrePost$Sec2Qo.x == 0, NA, datPrePost$Sec2Qo.x)
describe.factor(datPrePost$Sec2Qo.x)


# 5 = -3; 4 = -2, 3 = -1, 6=0, 7=1,  8 = 2, 1 = NA, NA = NA, 9 = 3
describe.factor(datPrePost$Sec4QfA.x)
datPrePost$Sec4QfA.x = ifelse(datPrePost$Sec4QfA.x == " ", NA, ifelse(datPrePost$Sec4QfA.x == "-", NA, datPrePost$Sec4QfA.x))
describe.factor(datPrePost$Sec4QfA.x)

datPrePost$Sec4QfA.x = ifelse(datPrePost$Sec4QfA.x == 5, -3, ifelse(datPrePost$Sec4QfA.x  == 4, -2, ifelse(datPrePost$Sec4QfA.x == 3, -1, ifelse(datPrePost$Sec4QfA.x == 6, 0, ifelse(datPrePost$Sec4QfA.x == 7, 1, ifelse(datPrePost$Sec4QfA.x == 8, 2, ifelse(datPrePost$Sec4QfA.x == 1, NA, ifelse(datPrePost$Sec4QfA.x == 9, 3, datPrePost$Sec4QfA.x ))))))))
describe.factor(datPrePost$Sec4QfA.x)

describe.factor(datPrePost$Sec4QfB.x)
datPrePost$Sec4QfB.x = ifelse(datPrePost$Sec4QfB.x == -23, NA, datPrePost$Sec4QfB.x)
describe.factor(datPrePost$Sec4QfB.x)

describe.factor(datPrePost$Sec4QgB.x)
datPrePost$Sec4QgB.x = ifelse(datPrePost$Sec4QgB.x == -11, NA, datPrePost$Sec4QgB.x)
describe.factor(datPrePost$Sec4QgB.x)


describe.factor(datPrePost$Sec4QhA.x)
datPrePost$Sec4QhA.x = ifelse(datPrePost$Sec4QhA.x == -4, NA, datPrePost$Sec4QhA.x)
describe.factor(datPrePost$Sec4QhA.x)

describe.factor(datPrePost$Sec4QeB.x)
datPrePost$Sec4QeB.x = ifelse(datPrePost$Sec4QeB.x == -32, NA, ifelse(datPrePost$Sec4QeB.x == -4, NA, datPrePost$Sec4QeB.x))
describe.factor(datPrePost$Sec4QeB.x)

describe.factor(datPrePost$Sec2Qh.x)
datPrePost$Sec2Qh.x = ifelse(datPrePost$Sec2Qh.x == 56, NA, datPrePost$Sec2Qh.x)
describe.factor(datPrePost$Sec2Qh.x)

```
Now we are going to try to put together measure four.
Compare A and B version of each question.  See which one has the higher score for the correct answer.  If the correct answer has the higher score then the new variable will be 1 and if not a zero.  

Need to do this for every pair.  Rename each var with
```{r}
head(datPrePost)

datPrePost$Sec4Qa = ifelse(datPrePost$Sec4QaA.x < datPrePost$Sec4QaB.x, 1, 0)
describe.factor(datPrePost$Sec4Qa)

datPrePost$Sec4Qb = ifelse(datPrePost$Sec4QbA.x < datPrePost$Sec4QbB.x, 1, 0)
describe.factor(datPrePost$Sec4Qb)

datPrePost$Sec4Qc = ifelse(datPrePost$Sec4QcA.x < datPrePost$Sec4QcB.x, 1, 0)
describe.factor(datPrePost$Sec4Qc)

testC = data.frame(Time = datPrePost$time,Sec4QcA.x = datPrePost$Sec4QcA.x, Sec4QcB.x = datPrePost$Sec4QcB.x)
apply(testC, 2, mean, na.rm = TRUE)
compmeans(testC$Sec4QcB.x, testC$Time)


datPrePost$Sec4Qd = ifelse(datPrePost$Sec4QdA.x > datPrePost$Sec4QdB.x, 1, 0)
describe.factor(datPrePost$Sec4Qd)

# This item is moving in the wrong direction over time
testD = data.frame(Time = datPrePost$time,Sec4QdA.x = datPrePost$Sec4QdA.x, Sec4QdB.x = datPrePost$Sec4QdB.x)
apply(testD, 2, mean, na.rm = TRUE)
compmeans(testD$Sec4QdA.x, testD$Time)

datPrePost$Sec4Qe = ifelse(datPrePost$Sec4QeA.x > datPrePost$Sec4QeB.x, 1, 0)
describe.factor(datPrePost$Sec4Qe)

datPrePost$Sec4Qf = ifelse(datPrePost$Sec4QfA.x < datPrePost$Sec4QfB.x, 1, 0)
describe.factor(datPrePost$Sec4Qf)

datPrePost$Sec4Qg = ifelse(datPrePost$Sec4QgA.x > datPrePost$Sec4QgB.x, 1, 0)
describe.factor(datPrePost$Sec4Qg)

datPrePost$Sec4Qh = ifelse(datPrePost$Sec4QhA.x < datPrePost$Sec4QhB.x, 1, 0)
describe.factor(datPrePost$Sec4Qh)

datPrePost$Sec4Qi = ifelse(datPrePost$Sec4QiA.x < datPrePost$Sec4QiB.x, 1, 0)
describe.factor(datPrePost$Sec4Qi)

datPrePost$Sec4Qj = ifelse(datPrePost$Sec4QjA.x > datPrePost$Sec4QjB.x, 1, 0)
describe.factor(datPrePost$Sec4Qj)

datPrePost$Sec4Qk = ifelse(datPrePost$Sec4QkA.x < datPrePost$Sec4QkB.x, 1, 0)
describe.factor(datPrePost$Sec4Qk)

# This item is not moving at all
testK = data.frame(Time = datPrePost$time,Sec4QkA.x = datPrePost$Sec4QkA.x, Sec4QkB.x = datPrePost$Sec4QkB.x)
apply(testK, 2, mean, na.rm = TRUE)
compmeans(testK$Sec4QkB.x, testK$Time)


datPrePost$Sec4Ql = ifelse(datPrePost$Sec4QlA.x > datPrePost$Sec4QlB.x, 1, 0)
describe.factor(datPrePost$Sec4Ql)

# This item is not moving over time
testL = data.frame(Time = datPrePost$time,Sec4QlA.x = datPrePost$Sec4QlA.x, Sec4QlB.x = datPrePost$Sec4QlB.x)
apply(testL, 2, mean, na.rm = TRUE)
compmeans(testL$Sec4QlA.x, testL$Time)

```



Now try to get the psychometrics.  Just try reliabilty for the first one
Just get the baseline values, try invar later on with time.

Reliability is bad for the baseline for Section 1 measures
Measure two has great reliabiltiy, not bad for measure three, worry about measure 4 later

```{r}
## Reliability for section one
datPrePostSec1 = datPrePost[,c(7,9:21)]
head(datPrePostSec1)

datPrePostSec1Base = subset(datPrePostSec1, time == 0)
describe.factor(datPrePostSec1Base$time)
datPrePostSec1Base$time = NULL

datPrePostSec1Base = data.frame(datPrePostSec1Base)
write.csv(datPrePostSec1Base, "datPrePostSec1Base.csv", row.names = FALSE)
datPrePostSec1Base = read.csv("datPrePostSec1Base.csv", header = TRUE)

CronbachAlpha(datPrePostSec1Base, na.rm = TRUE)
omegaSec1Base = omega(datPrePostSec1Base)
summary(omegaSec1Base)

## Reliability for section two
head(datPrePost)
datPrePostSec2 = datPrePost[,c(7, 20:34)]
head(datPrePostSec2)


datPrePostSec2Base = subset(datPrePostSec2, time == 0)
describe.factor(datPrePostSec2Base$time)
datPrePostSec2Base$time = NULL

datPrePostSec2Base = data.frame(datPrePostSec2Base)
write.csv(datPrePostSec2Base, "datPrePostSec2Base.csv", row.names = FALSE)
datPrePostSec2Base = read.csv("datPrePostSec2Base.csv", header = TRUE)

CronbachAlpha(datPrePostSec2Base, na.rm = TRUE)
summary(datPrePostSec2Base)

omegaSec2Base =  omega(datPrePostSec2Base)
summary(omegaSec2Base)

#Reliabiltiy for section three
head(datPrePost)
datPrePostSec3 = datPrePost[,c(7, 35:42)]
head(datPrePostSec3)
summary(datPrePostSec3)

datPrePostSec3Base = subset(datPrePostSec3, time == 0)
describe.factor(datPrePostSec3Base$time)
datPrePostSec3Base$time = NULL

datPrePostSec3Base = data.frame(datPrePostSec3Base)
write.csv(datPrePostSec3Base, "datPrePostSec3Base.csv", row.names = FALSE)
datPrePostSec3Base = read.csv("datPrePostSec3Base.csv", header = TRUE)

CronbachAlpha(datPrePostSec3Base, na.rm = TRUE)

omegaSec3Base =  omega(datPrePostSec3Base)
summary(omegaSec3Base)


##Reliability for section four
head(datPrePost)
datPrePostSec4 = datPrePost[,c(7, 69:80)]
head(datPrePostSec4)

datPrePostSec4Base = subset(datPrePostSec4, time == 0)
describe.factor(datPrePostSec4Base$time)
datPrePostSec4Base$time = NULL

datPrePostSec4Base = data.frame(datPrePostSec4Base)
write.csv(datPrePostSec4Base, "datPrePostSec4Base.csv", row.names = FALSE)
datPrePostSec4Base = read.csv("datPrePostSec4Base.csv", header = TRUE)

CronbachAlpha(datPrePostSec4Base, na.rm = TRUE)
summary(datPrePostSec4Base)

omegaSec4Base =  omega(datPrePostSec4Base)
summary(omegaSec4Base)

### Reliability for short version of section dropping c,d,k,l
head(datPrePost)
datPrePostSec4Short = datPrePost[,c(7, 69:70, 73:78)]
head(datPrePostSec4Short)

datPrePostSec4BaseShort = subset(datPrePostSec4Short, time == 0)
describe.factor(datPrePostSec4BaseShort$time)
datPrePostSec4BaseShort$time = NULL

datPrePostSec4BaseShort = data.frame(datPrePostSec4BaseShort)
write.csv(datPrePostSec4BaseShort, "datPrePostSec4BaseShort.csv", row.names = FALSE)
datPrePostSec4Base = read.csv("datPrePostSec4BaseShort.csv", header = TRUE)

CronbachAlpha(datPrePostSec4BaseShort, na.rm = TRUE)
summary(datPrePostSec4BaseShort)

omegaSec4Base =  omega(datPrePostSec4BaseShort)
summary(omegaSec4Base)

```
Ok now try EFA for all three 
First one terriable
```{r}
parallel1 = fa.parallel(datPrePostSec1Base, fa= "fa")
parallel1$fa.values

parallel2 = fa.parallel(datPrePostSec2Base, fa= "fa")
parallel2$fa.values

parallel3 = fa.parallel(datPrePostSec3Base, fa= "fa")
parallel3$fa.values

parallel4 = fa.parallel(datPrePostSec4Base, fa= "fa")
parallel4$fa.values

parallel4Short = fa.parallel(datPrePostSec4BaseShort, fa= "fa")
parallel4Short$fa.values
```
Try CFA for three assessments
```{r}
head(datPrePostSec1Base)
model1 = 'SA =~ Sec1Qa.x + Sec1Qb.x + Sec1Qc.x + Sec1Qd.x + Sec1Qe.x + Sec1Qf.x + Sec1Qe.x + Sec1Qf.x + Sec1Qg.x + Sec1Qh.x + Sec1Qi.x + Sec1Qk.x + Sec1Ql.x'

fit1 = cfa(model1, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = datPrePostSec1Base)
summary(fit1, fit.measures = TRUE)


head(datPrePostSec2Base)
model2 = 'SA =~ Sec2Qa.x + Sec2Qb.x + Sec2Qc.x + Sec2Qd.x + Sec2Qe.x + Sec2Qf.x + Sec2Qg.x + Sec2Qh.x + Sec2Qi.x + Sec2Qj.x + Sec2Qk.x + Sec2Ql.x + Sec2Qm.x + Sec2Qn.x + Sec2Qo.x'

fit2 = cfa(model2, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = datPrePostSec2Base)
summary(fit2, fit.measures = TRUE)


head(datPrePostSec3Base)
model3 = 'SA =~ Sec3Qa.x + Sec3Qb.x + Sec3Qc.x + Sec3Qd.x + Sec3Qe.x + Sec3Qf.x + Sec3Qg.x + Sec3Qh.x'

fit3 = cfa(model3, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = datPrePostSec3Base)
summary(fit3, fit.measures = TRUE)

head(datPrePostSec4Base)
model4 = 'SA =~ Sec4Qa + Sec4Qb + Sec4Qc + Sec4Qd + Sec4Qe + Sec4Qf + Sec4Qg + Sec4Qh + Sec4Qi + Sec4Qj + Sec4Qk + Sec4Ql'
fit4 = cfa(model4, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = datPrePostSec4Base)
summary(fit4, fit.measures = TRUE)

head(datPrePostSec4BaseShort)
model4Short = 'SA =~ Sec4Qa + Sec4Qb  + Sec4Qe + Sec4Qf + Sec4Qg + Sec4Qh + Sec4Qi + Sec4Qj '
fit4Short = cfa(model4Short, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = datPrePostSec4BaseShort)
summary(fit4Short, fit.measures = TRUE)

```

Now we are getting total scores for the three measures use apply and create a smaller data set.
Ok so we are summing up even with missing.  So if missing then just skips that value.

Then put that data set back together call it analysis at the end.

Remember Race is messed up so look at Race earlier

Only 5% if the data is missing, but missing at random test is significant

For this analysis, change gender 1,0, Race to white versus non-white (5 versus everything else), edu to bachelors and below versus masters and above (5 and below 1)

Just deleting values for now run imputation later

Remember need to grab just the baseline

```{r}
Sec1BaseTotal = rowSums(datPrePostSec1Base, na.rm = TRUE)
Sec2BaseTotal = rowSums(datPrePostSec2Base, na.rm = TRUE)
Sec3BaseTotal = rowSums(datPrePostSec3Base, na.rm = TRUE)
Sec4BaseTotal = rowSums(datPrePostSec4Base, na.rm = TRUE)
Sec4BaseTotalShort = rowSums(datPrePostSec4BaseShort, na.rm = TRUE)
```

Ok moving on to analysis
Data cleaning for analysis
```{r}
Sec1Total = rowSums(datPrePostSec1, na.rm = TRUE)
Sec2Total = rowSums(datPrePostSec2, na.rm = TRUE)
Sec3Total = rowSums(datPrePostSec3, na.rm = TRUE)
Sec4BaseTotal = rowSums(datPrePostSec4, na.rm = TRUE)
Sec4BaseTotalShort = rowSums(datPrePostSec4Short, na.rm = TRUE)

datPrePostAnalysis = data.frame(ID = datPrePost$ID, Treatment = datPrePost$Treatment.x, Age =  datPrePost$Age, Gender = datPrePost$Gender, Race = datPrePost$Race, Edu = datPrePost$Edu, Time = datPrePost$time,Sec1Total =  Sec1BaseTotal, Sec2Total = Sec2Total, Sec3Total = Sec3Total)

datPrePostAnalysis$Gender = ifelse(datPrePostAnalysis$Gender == 1,1,0)
datPrePostAnalysis$Race = ifelse(datPrePostAnalysis$Race == 5, 0, 1)
datPrePostAnalysis$Edu = ifelse(datPrePostAnalysis$Edu < 6, 1, 0)

#datPrePostAnalysisComplete = subset(datPrePostAnalysis, Treatment == 1 | Treatment == 2)

# Getting the data ready
datPrePostAnalysisTest = na.omit(datPrePostAnalysis)
write.csv(datPrePostAnalysisTest, "datPrePostAnalysisTest.csv", row.names = FALSE)
datPrePostAnalysisTest = read.csv("datPrePostAnalysisTest.csv", header = TRUE)


# Maybe make sure time and treatment are treated as factors
datPrePostAnalysisTest$Treatment = as.factor(datPrePostAnalysisTest$Treatment)
datPrePostAnalysisTest$Time = as.factor(datPrePostAnalysisTest$Time)
```


Try adding robust as well need to read into this

Problem not converging.  I wonder if we need to get rid of one of the treatments, because there is not enough data to estimate the difference?

Ok so basically some people are starting lower, but the end result is the same
```{r}


# Not working double check (may be a bad measure)
modelOutcome1 = lmer(Sec1Total ~ Treatment*Time + (1 | ID),  data = datPrePostAnalysisTest)
summary(modelOutcome1)

## Working!!! Yay!!!!

modelOutcome2Robust = rlmer(Sec2Total ~ Treatment*Time + (1 | ID),  data = datPrePostAnalysisTest)
# Get p-values
coefs = data.frame(coef(summary(modelOutcome2Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

### Try adding additional covariates 

modelOutcome2Robust = rlmer(Sec2Total ~ Treatment*Time + Edu + Gender + Age + Race + (1 | ID),  data = datPrePostAnalysisTest)
# Get p-values
coefs = data.frame(coef(summary(modelOutcome2Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

### Try random effects model cannot run too many covariates
modelOutcome2Robust = rlmer(Sec2Total ~ Treatment*Time + Edu + Gender + Age + Race + (Time | ID),  data = datPrePostAnalysisTest)
# Get p-values
coefs = data.frame(coef(summary(modelOutcome2Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

### Try random effects model try no covariates nope won't run either just random intercepts
modelOutcome2Robust = rlmer(Sec2Total ~ Treatment*Time + (Time | ID),  data = datPrePostAnalysisTest)
# Get p-values
coefs = data.frame(coef(summary(modelOutcome2Robust)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

## Cannot run robust with contrasts so just regular


modelOutcome2 = lmer(Sec2Total ~ Treatment*Time + + Edu + Gender + Age + Race + + (1 | ID),  data = datPrePostAnalysisTest)
summary(modelOutcome2)
library(jtools)
cat_plot(modelOutcome2, pred = "Time", modx = "Treatment", cluster = "ID")

## Working!!! Yay!!!!
modelOutcome3 = rlmer(Sec3Total ~ Treatment*Time + (1 | ID),  data = datPrePostAnalysisTest)
summary(modelOutcome3)

```
