#######################################################################################
# Analysis 2 for Manuscript: A 2x2 factorial randomized controlled trial of rhetorical training and s...
#######################################################################################
#setting environment -------------------------------------------------------------------
#remove all objects and then check

# Adelia is testing analysis2
# Elias testing
# Elias testing2

rm(list = ls())
ls()
#dettach all packages
detach()
# set workdir
setwd("C:\\GitHub\\UEM_2x2\\")

# data1=reading the first database (work professor x student)
# Header for data1 ####################################
# GRO = Group, ENC=Encounter with mentor and researcher
#       1=Control, 2=Template, 3=Swarm, 4=Template+Swarm
# EXP=Explanation from mentor to researcher, QOW=Quality of writing
# SOR=satisfaction of Student, CWR=Communication from mentor with Student
# reading the second database (introdcution corrections)

# Install packages
install.packages("nortest") # to use Anderson-Darling test
install.packages("RCurl") # to read remote spreadsheet in GDocs
install.packages("moments")
install.packages("irr")

# Load packages
library(nortest)
library(RCurl)
library(moments)            # load the moments package
library(irr)

# Reading remoda data in GDocs ------------------------------------------------------------------------
# Questions
options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
uem.data <- getURL("https://docs.google.com/spreadsheet/pub?hl=pt&hl=pt&key=0ArSWDBjbC6hHdFRMeVIwUENiNkZZcDlYSVVXM1lPOGc&single=true&gid=0&output=csv")
data2<-read.csv(textConnection(uem.data), header=T)
attach(data2)
detach(data2)

# Data for Kappa
uem.data2 <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArSWDBjbC6hHdDNxWkNGTHBUZnVScE13clhieEVRcmc&single=true&gid=1&output=csv")
data3<-read.csv(textConnection(uem.data2), header=T)
attach(data3)
detach(data3)

# Normality Test
attach(data2)
detach(data2)
summary(data2)
dim(data2)

kurtosis(Q1) - 3       # apply the kurtosis function
kurtosis(Q2) - 3
kurtosis(Q3) - 3
kurtosis(Q4) - 3
kurtosis(Q5) - 3
kurtosis(Q6) - 3
kurtosis(Q7) - 3
kurtosis(Q8) - 3
kurtosis(Q9) - 3
kurtosis(Q10) - 3
kurtosis(Q11) - 3
kurtosis(Q12) - 3
kurtosis(Q13) - 3

skewness(Q1)
skewness(Q2)
skewness(Q3)
skewness(Q4)
skewness(Q5)
skewness(Q6)
skewness(Q7)
skewness(Q8)
skewness(Q9)
skewness(Q10)
skewness(Q11)
skewness(Q12)
skewness(Q13)

ad.test(Q1)
ad.test(Q2)
ad.test(Q3)
ad.test(Q4)
ad.test(Q5)
ad.test(Q6)
ad.test(Q7)
ad.test(Q8)
ad.test(Q9)
ad.test(Q10)
ad.test(Q11)
ad.test(Q12)
ad.test(Q13)

detach(data2)

# Kappa Test
attach(data3)
q1<-data.frame(Q1,Q1a)
q2<-data.frame(Q2,Q2a)
q3<-data.frame(Q3,Q3a)
q4<-data.frame(Q4,q4a)
q5<-data.frame(Q5,q5a)
q6<-data.frame(Q6,q6a)
q7<-data.frame(Q7,q7a)
q8<-data.frame(Q8,q8a)
q9<-data.frame(Q9,q9a)
q10<-data.frame(Q10,q10a)
q11<-data.frame(Q11,q11a)
q12<-data.frame(Q12,q12a)
q13<-data.frame(Q13,q13a)

kappa2(q1[,1:2], "squared") # predefined set of squared weights
kappa2(q2[,1:2], "squared") # predefined set of squared weights
kappa2(q3[,1:2], "squared") # predefined set of squared weights
kappa2(q4[,1:2], "squared") # predefined set of squared weights
kappa2(q5[,1:2], "squared") # predefined set of squared weights
kappa2(q6[,1:2], "squared") # predefined set of squared weights
kappa2(q7[,1:2], "squared") # predefined set of squared weights
kappa2(q8[,1:2], "squared") # predefined set of squared weights
kappa2(q9[,1:2], "squared") # predefined set of squared weights
kappa2(q10[,1:2], "squared") # predefined set of squared weights
kappa2(q11[,1:2], "squared") # predefined set of squared weights
kappa2(q12[,1:2], "squared") # predefined set of squared weights
kappa2(q13[,1:2], "squared") # predefined set of squared weights

detach(data3)

# Kruskal-Wallis
attach(data2)
kruskal.test(Q1 ~ Gro, data= data2)
kruskal.test(Q2 ~ Gro, data= data2)
kruskal.test(Q3 ~ Gro, data= data2)
kruskal.test(Q4 ~ Gro, data= data2)
kruskal.test(Q5 ~ Gro, data= data2)
kruskal.test(Q6 ~ Gro, data= data2)
kruskal.test(Q7 ~ Gro, data= data2)
kruskal.test(Q8 ~ Gro, data= data2)
kruskal.test(Q9 ~ Gro, data= data2)
kruskal.test(Q10 ~ Gro, data= data2)
kruskal.test(Q11 ~ Gro, data= data2)
kruskal.test(Q12 ~ Gro, data= data2)
kruskal.test(Q13 ~ Gro, data= data2)