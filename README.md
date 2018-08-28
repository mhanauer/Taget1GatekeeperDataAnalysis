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
datPrePost = reshape(datPrePost, varying  = list(c("Treatment.x", "Treatment.y"), c("Sec1Qa.x", "Sec1Qa.y"), c("Sec1Qb.x", "Sec1Qb.y"), c("Sec1Qc.x", "Sec1Qc.y"), c("Sec1Qd.x", "Sec1Qd.y"), c("Sec1Qe.x", "Sec1Qe.y"), c("Sec1Qf.x", "Sec1Qf.y"), c("Sec1Qg.x", "Sec1Qg.y"), c("Sec1Qh.x", "Sec1Qh.y"), c("Sec1Qi.x", "Sec1Qi.y"), c("Sec1Qk.x", "Sec1Qk.y"), c("Sec1Ql.x", "Sec1Ql.y"), c("Sec2Qa.x", "Sec2Qa.y"), c("Sec2Qb.x", "Sec2Qb.y"), c("Sec2Qc.x", "Sec2Qc.y"), c("Sec2Qd.x", "Sec2Qd.y"), c("Sec2Qe.x", "Sec2Qe.y"), c("Sec2Qf.x", "Sec2Qf.y"), c("Sec2Qg.x", "Sec2Qg.y"), c("Sec2Qh.x", "Sec2Qh.y"), c("Sec2Qi.x", "Sec2Qi.y"), c("Sec2Qj.x", "Sec2Qj.y"), c("Sec2Qk.x", "Sec2Qk.y"), c("Sec2Ql.x", "Sec2Ql.y"), c("Sec2Qm.x", "Sec2Qm.y"), c("Sec2Qn.x", "Sec2Qn.y"), c("Sec2Qo.x", "Sec2Qo.y"), c("Sec3Qa.x", "Sec3Qa.y"), c("Sec3Qb.x", "Sec3Qb.y"), c("Sec3Qc.x", "Sec3Qc.y"), c("Sec3Qd.x", "Sec3Qd.y"), c("Sec3Qe.x", "Sec3Qe.y"), c("Sec3Qf.x", "Sec3Qf.y"), c("Sec3Qg.x", "Sec3Qg.y"), c("Sec3Qh.x", "Sec3Qh.y"), c("Sec4QaA.x", "Sec4QaA.y"), c("Sec4QfA.x", "Sec4QfA.y"), c("Sec4QfB.x", "Sec4QfB.y"), c("Sec4QgA.x", "Sec4QgA.y"), c("Sec4QgB.x", "Sec4QgB.y"), c("Sec4QhA.x", "Sec4QhA.y"), c("Sec4QhB.x", "Sec4QhB.y"), c("Sec4QiA.x", "Sec4QiA.y"), c("Sec4QiB.x", "Sec4QiB.y"), c("Sec4QjA.x", "Sec4QjA.y"), c("Sec4QjB.x", "Sec4QjB.y"), c("Sec4QkA.x", "Sec4QkA.y"), c("Sec4QkB.x", "Sec4QkB.y"), c("Sec4QlA.x", "Sec4QlA.y"), c("Sec4QlB.x", "Sec4QlB.y")), direction = "long", times =c(0,1))

sum(is.na(datPrePost))

```
Look at descirptives, then do basic psychometrics

Werid changes 1's to 2's and 0's to 1's, but it is happening and just add another ifelse statement to fix it.

Errors below

datPrePost$Sec1Qf.x = one result of 5 should either be one or zero
datPrePost$Sec1Qi.x = 5 and 9 
```{r}
write.csv(datPrePost, "datPrePost.csv", row.names = FALSE)
datPrePost = read.csv("datPrePost.csv", header = TRUE)
summary(datPrePost)

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
describe.factor(datPrePost$Race)
# Need to create a multiracial category, which will be 7 just use a bunch of ifelse statements
datPrePost$Race = as.factor(datPrePost$Race)
describe.factor(datPrePost$Race)
head(datPrePost$Race)
write.csv(datPrePost, "datPrePost.csv", row.names  = FALSE)
datPrePost = read.csv("datPrePost.csv", header = TRUE)

datPrePost$RaceTest = ifelse(datPrePost$Race == '1 1', 7, datPrePost$Race)

describe.factor(datPrePost$RaceTest)
describe.factor(datPrePost$Race)
```



