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
```
Load data and figure out how to transpose it
```{r}
#setwd("P:/Evaluation/TN Lives Count_Writing/3_Target1_SUICClinicalTrainingComparison/3_Data & Analyses")
#datPre = read.csv("Pre.csv", header = FALSE, row.names = NULL)

datPre = t(datPre)
write.csv(datPre, "datPre.csv", row.names = FALSE)
datPre = read.csv("datPre.csv", header = TRUE)
head(datPre)

datPre = datPre[,c(4, 6, 10:21, 24:38, 41:48, 52:75, 81:83, 86, 88)]
datPre = data.frame(datPre)
head(datPre)
colnames(datPre) = c("ID", "Treatment", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB","Age", "Gender", "Eth", "Race", "Edu")
head(datPre)
### Get rif of first row once you figure out which variables to keep
datPre = datPre[-1,]
datPre = data.frame(datPre)
head(datPre)

## add a time variable so we can stack later
datPre$time = rep(0,dim(datPre)[1])
datPre$ID = as.factor(datPre$ID)
head(datPre)
```
Load up the post data and should be able to run the same with post and 1 for time.  

Section 1 of pre is allinged with Section 2 of post

All section are plus one for the post

Where is Section II in pre for post in the data?
```{r}
#setwd("P:/Evaluation/TN Lives Count_Writing/3_Target1_SUICClinicalTrainingComparison/3_Data & Analyses")
#datPost = read.csv("Post.csv", header = FALSE, row.names = NULL)

datPost = t(datPost)
write.csv(datPost, "datPost.csv", row.names = FALSE)
datPost = read.csv("datPost.csv", header = TRUE)
head(datPost)

datPost = datPost[,c(3, 5, 15:26, 29:43, 46:53, 57:80)]
dim(datPre)
datPost = data.frame(datPost)
head(datPost)
dim(datPost)
colnames(datPost) = c("ID", "Treatment", "Sec1Qa", "Sec1Qb", "Sec1Qc", "Sec1Qd", "Sec1Qe", "Sec1Qf", "Sec1Qg", "Sec1Qh", "Sec1Qi", "Sec1Qj", "Sec1Qk", "Sec1Ql", "Sec2Qa", "Sec2Qb", "Sec2Qc", "Sec2Qd", "Sec2Qe", "Sec2Qf", "Sec2Qg",  "Sec2Qh", "Sec2Qi", "Sec2Qj", "Sec2Qk", "Sec2Ql", "Sec2Qm", "Sec2Qn", "Sec2Qo", "Sec3Qa", "Sec3Qb", "Sec3Qc", "Sec3Qd", "Sec3Qe", "Sec3Qf", "Sec3Qg", "Sec3Qh", "Sec4QaA", "Sec4QaB", "Sec4QbA", "Sec4QbB", "Sec4QcA", "Sec4QcB", "Sec4QdA", "Sec4QdB", "Sec4QeA", "Sec4QeB", "Sec4QfA","Sec4QfB", "Sec4QgA", "Sec4QgB", "Sec4QhA", "Sec4QhB", "Sec4QiA", "Sec4QiB", "Sec4QjA", "Sec4QjB", "Sec4QkA", "Sec4QkB", "Sec4QlA", "Sec4QlB")
head(datPost)
#### Need to add these variables, because they were not include in the post survey and need to stack them ,"Age", "Gender", "Eth", "Race", "Edu"
### Need to merge the data then put into long form.
datPost$ID = as.factor(datPost$ID)
datPost = datPost[-1,]
datPost = data.frame(datPost)
head(datPost)

## add a time variable so we can stack later
datPost$time = rep(1,dim(datPost)[1])
head(datPost)


datPrePost = merge(datPre, datPost, by = "ID",  all = TRUE)
head(datPrePost)
### Now make long format
### These variables are not included: 							

datPrePost = reshape(datPrePost, varying  = list(c("Treatment.x", "Treatment.y"), c("Sec1Qa.x", "Sec1Qa.y"), c("Sec1Qb.x", "Sec1Qb.y"), c("Sec1Qc.x", "Sec1Qc.y"), c("Sec1Qd.x", "Sec1Qd.y"), c("Sec1Qe.x", "Sec1Qe.y"), c("Sec1Qf.x", "Sec1Qf.y"), c("Sec1Qg.x", "Sec1Qg.y"), c("Sec1Qh.x", "Sec1Qh.y"), c("Sec1Qi.x", "Sec1Qi.y"), c("Sec1Qk.x", "Sec1Qk.y"), c("Sec1Ql.x", "Sec1Ql.y"), c("Sec2Qa.x", "Sec2Qa.y"), c("Sec2Qb.x", "Sec2Qb.y"), c("Sec2Qc.x", "Sec2Qc.y"), c("Sec2Qd.x", "Sec2Qd.y"), c("Sec2Qe.x", "Sec2Qe.y"), c("Sec2Qf.x", "Sec2Qf.y"), c("Sec2Qg.x", "Sec2Qg.y"), c("Sec2Qh.x", "Sec2Qh.y"), c("Sec2Qi.x", "Sec2Qi.y"), c("Sec2Qj.x", "Sec2Qj.y"), c("Sec2Qk.x", "Sec2Qk.y"), c("Sec2Ql.x", "Sec2Ql.y"), c("Sec2Qm.x", "Sec2Qm.y"), c("Sec2Qn.x", "Sec2Qn.y"), c("Sec2Qo.x", "Sec2Qo.y"), c("Sec3Qa.x", "Sec3Qa.y"), c("Sec3Qb.x", "Sec3Qb.y"), c("Sec3Qc.x", "Sec3Qc.y"), c("Sec3Qd.x", "Sec3Qd.y"), c("Sec3Qe.x", "Sec3Qe.y"), c("Sec3Qf.x", "Sec3Qf.y"), c("Sec3Qg.x", "Sec3Qg.y"), c("Sec3Qh.x", "Sec3Qh.y"), c("Sec4QaA.x", "Sec4QaA.y"), c("Sec4QfA.x", "Sec4QfA.y"), c("Sec4QfB.x", "Sec4QfB.y"), c("Sec4QgA.x", "Sec4QgA.y"), c("Sec4QgB.x", "Sec4QgB.y"), c("Sec4QhA.x", "Sec4QhA.y"), c("Sec4QhB.x", "Sec4QhB.y"), c("Sec4QiA.x", "Sec4QiA.y"), c("Sec4QiB.x", "Sec4QiB.y"), c("Sec4QjA.x", "Sec4QjA.y"), c("Sec4QjB.x", "Sec4QjB.y"), c("Sec4QkA.x", "Sec4QkA.y"), c("Sec4QkB.x", "Sec4QkB.y"), c("Sec4QlA.x", "Sec4QlA.y"), c("Sec4QlB.x", "Sec4QlB.y"), c("Sec1Qj.x", "Sec1Qj.y"), c("Sec4QaB.x", "Sec4QaB.y"), c("Sec4QbA.x", "Sec4QbA.y"), c("Sec4QbB.x", "Sec4QbB.y"), c("Sec4QcA.x", "Sec4QcA.y"), c("Sec4QcB.x", "Sec4QcB.y"), c("Sec4QdA.x", "Sec4QdA.y"), c("Sec4QdB.x", "Sec4QdB.y"), c("Sec4QeA.x", "Sec4QeA.y"), c("Sec4QeB.x", "Sec4QeB.y")), direction = "long", times =c(0,1))

sum(is.na(datPrePost))
head(datPrePost)
write.csv(datPrePost, "datPrePost.csv", row.names = FALSE)

```
Look at descirptives, then do basic psychometrics

Werid changes 1's to 2's and 0's to 1's, but it is happening and just add another ifelse statement to fix it.

Errors below

datPrePost$Sec1Qf.x = one result of 5 should either be one or zero
datPrePost$Sec1Qi.x = 5 and 9 

Race translation
12 == 5
10 == 3
1 == " " change to NA
13 == 6
NA == NA
2 == 1
7 == 2
8 == 7 (multi)
11 == 7
3 == 7 
5 == 7
6 == 7
9 == 7

```{r}
write.csv(datPrePost, "datPrePost.csv", row.names = FALSE)
datPrePost = read.csv("datPrePost.csv", header = TRUE)
summary(datPrePost)

describe.factor(datPrePost$Race)
# Need to create a multiracial category, which will be 7 just use a bunch of ifelse statements.  This makes things funky see code above

datPrePost$Race = ifelse(datPrePost$Race == '1, 3', 7, datPrePost$Race)
describe.factor(datPrePost$Race)
#Getting all the multiracial categories the same
datPrePost$Race = ifelse(datPrePost$Race == 8, 8, ifelse(datPrePost$Race == 11, 8, ifelse(datPrePost$Race == 3, 8, ifelse(datPrePost$Race == 5, 8, ifelse(datPrePost$Race == 6, 8, ifelse(datPrePost$Race == 9,8, datPrePost$Race))))))
describe.factor(datPrePost$Race)
# Now change original values back
datPrePost$Race = ifelse(datPrePost$Race == 12,5, ifelse(datPrePost$Race == 10,3,ifelse(datPrePost$Race == 1, NA, ifelse(datPrePost$Race == 13, 6, ifelse(datPrePost$Race == 2, 1, ifelse(datPrePost$Race == 7,2, datPrePost$Race))))))



describe.factor(datPrePost$Sec1Qf.x)
datPrePost$Sec1Qf.x = as.factor(datPrePost$Sec1Qf.x)
describe.factor(datPrePost$Sec1Qf.x)
datPrePost$Sec1Qf.x = ifelse(datPrePost$Sec1Qf.x== 5, NA, datPrePost$Sec1Qf.x)
datPrePost$Sec1Qf.x = ifelse(datPrePost$Sec1Qf.x == 2, 1, 0)
describe.factor(datPrePost$Sec1Qf.x)



describe.factor(datPrePost$Sec1Qi.x)
datPrePost$Sec1Qi.x = ifelse(datPrePost$Sec1Qi.x == 5, NA, datPrePost$Sec1Qi.x)
datPrePost$Sec1Qi.x = ifelse(datPrePost$Sec1Qi.x == 9, NA, datPrePost$Sec1Qi.x)
describe.factor(datPrePost$Sec1Qi.x)

summary(datPrePost)

describe.factor(datPrePost$Sec1Qg.x)
datPrePost$Sec1Qg.x = ifelse(datPrePost$Sec1Qg.x == 3, NA, datPrePost$Sec1Qg.x)
describe.factor(datPrePost$Sec1Qg.x)

summary(datPrePost)

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

summary(datPrePost)

describe.factor(datPrePost$Sec1Ql.x)
datPrePost$Sec1Ql.x = ifelse(datPrePost$Sec1Ql.x > 1, NA, datPrePost$Sec1Ql.x)
describe.factor(datPrePost$Sec1Ql.x)

summary(datPrePost)
describe.factor(datPrePost$Sec2Qo.x)
datPrePost$Sec2Qo.x = ifelse(datPrePost$Sec2Qo.x == 0, NA, datPrePost$Sec2Qo.x)
describe.factor(datPrePost$Sec2Qo.x)


# 5 = -3; 4 = -2, 3 = -1, 6=0, 7=1,  8 = 2, 1 = NA, NA = NA, 9 = 3
describe.factor(datPrePost$Sec4QfA.x)
datPrePost$Sec4QfA.x = ifelse(datPrePost$Sec4QfA.x == " ", NA, ifelse(datPrePost$Sec4QfA.x == "-", NA, datPrePost$Sec4QfA.x))

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


```



