#Problem statement :- http://www2.research.att.com/~volinsky/DataMining/Columbia2011/HW/HW2.html
#Problem 2
#=======================================================
setwd("/Users/akshaykulkarni/Documents/Data-Mining-Tasks")
pbc = read.table("data/pbc.txt",na.strings='.', header=TRUE)

complete = !is.na(pbc$chol) & !is.na(pbc$copper) & !is.na(pbc$trig) & !is.na(pbc$platelet)
chol_complete = pbc$chol[complete]
copper_complete = pbc$copper[complete]
trig_complete = pbc$trig[complete]
platelet_complete = pbc$platelet[complete]

par(mfrow=c(2,2))
hist(log(chol_complete),xlab="chol",main="Chol Frequency")
hist(log(copper_complete),xlab="copper",main="copper Frequency")
hist(log(trig_complete),xlab="trig",main="trig Frequency")
hist(sqrt(platelet_complete),xlab="platelet",main="platelet Frequency")

independent_df = data.frame(log(chol_complete), log(copper_complete), 
                            log(trig_complete), sqrt(platelet_complete))

pairs(independent_df)

#To fill missing values in the column of chol and copper. We can do following things:-
# - Build regression model of chol vd copper
# - Use mean to fill null values
# - use median to fill null values
# - Use mice library. imputed = complete(mice(input))




