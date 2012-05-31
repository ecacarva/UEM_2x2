#######################################################################################
# Analysis 1 for Manuscript: A 2x2 factorial randomized controlled trial of rhetorical training and s...
#######################################################################################
#setting environment -------------------------------------------------------------------
#remove all objects and then check
#remove all objects and then check

rm(list = ls())
ls()
# Adelia
#dettach all packages
detach()

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

# Load packages
library(nortest)
library(RCurl)

# Reading remoda data in GDocs ------------------------------------------------------------------------
options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
uem.data <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArSWDBjbC6hHdDM5eGFubjJtbGV3Ukd0cEpaMDRHcFE&single=true&gid=0&output=csv")
data1<-read.csv(textConnection(uem.data), header=T)
attach(data1)
# Verify normality
ad.test(enc)
ad.test(exp)
ad.test(qow)
ad.test(sor)
ad.test(cwr)

# Data nor normal - verify variance - use Kruskal Wallis Test
kruskal.test(enc ~ gro) 
kruskal.test(exp ~ gro) 
kruskal.test(qow ~ gro) 
kruskal.test(sor ~ gro) 
kruskal.test(cwr ~ gro) 

# One-Way Anova
aov(enc~gro)
aov(exp~gro)
aov(qow~gro)
aov(sor~gro)
aov(cwr~gro)

#building a boxplot to analyse the groups and actions
boxplot(enc~gro)
boxplot(exp~gro)
boxplot(qow~gro)
boxplot(sor~gro) 
boxplot(cwr~gro)
