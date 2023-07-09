install.packages("geepack") #install package

library(geepack) #load library
data(respiratory) #load data package

# regular GLM
m1 <- glm(outcome ~ center + treat + age + baseline, data=respiratory,                
          family=binomial())     

# GEE with independence correlation structure
gee_independence <- geeglm(outcome ~ center + treat + age + baseline, data=respiratory, id=id, 
                  family=binomial(), corstr="independence")

# GEE with exchangeable correlation structure
gee_exchangeable <- geeglm(outcome ~ center + treat + age + baseline, data=respiratory, id=id, 
                  family=binomial(), corstr="exchangeable")

# GEE with unstructured correlation structure
gee_unstructured <- geeglm(outcome ~ center + treat + age + baseline, data=respiratory, id=id, 
                  family=binomial(), corstr="unstructured")

# GEE with Auto-Regressive Order 1 correlation structure
gee_ar1 <- geeglm(outcome ~ center + treat + age + baseline, data=respiratory, id=id, 
                  family=binomial(), corstr="ar1")  


# Creating list data structure for all relevant correlation types (different GEE models)
corrStructureList = list(gee.ind, gee.exc, gee.uns, gee.ar1)

# Loops through the list and performs "row binding {rbind}" to form a single batch
# lapply + QIC = "Quasi Information Criterion" which allows for quasi-likelihood under the independence model
do.call(rbind, lapply(corrStructureList, QIC))


lapply(corrStructureList, tidy)

# https://online.stat.psu.edu/stat504/lesson/12/12.1 (for correlation structure information)