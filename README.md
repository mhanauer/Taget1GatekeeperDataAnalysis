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
#datPre = datPre[-1,]
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

### Get rif of first row once you figure out which variables to keep


```



