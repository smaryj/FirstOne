################################################################
### Final Project/ Step 2/ Data Mashing Stage/ Due May 2nd/2013
################################################################

#########################################
# Seyedeh Maryam Javanmard
# Max Lan
# Final Project/ Part #1
#########################################

library(XML)

statenames1 = read.table("http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2012/stateNames.txt", header=TRUE, stringsAsFactors=FALSE)[,1]

statenames = statenames1[-2]

##########################
### For One State
##########################

countyResults = function(xmlcounty){
  
  candidates = c(xmlValue(xmlcounty[[1]][[3]]), xmlSApply(xmlcounty, function(x){xmlValue(x[[1]])})[-1])
  
  votes = c(xmlValue(xmlcounty[[1]][[9]]), xmlSApply(xmlcounty, function(x){xmlValue(x[[7]])})[-1])
  
  party = c(xmlValue(xmlcounty[[1]][[5]]), xmlSApply(xmlcounty, function(x){xmlValue(x[[3]])})[-1])
  
  votes = as.numeric(gsub(",", "", votes))
  
  data.frame(candidates=candidates, party=party, votes=votes)
  
}


xmlState = function(state){
  
  xmlfile = paste("http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2012/", state, ".xml", sep="")
  
  doc1 = xmlParse(xmlfile)
  
  root1 = xmlRoot(doc1)
  
  # County Names:
  
  countyNames = xmlSApply(root1, function(x){xmlValue(x[[1]][[1]][[1]])})[-1]
  
  names(countyNames) = NULL
  
  gsub(" $", "", tolower(countyNames))
  
  #State Names:
  
  stateName = rep(state, times=length(countyNames))
  
  # Popular Votes:
  
  popvote = xmlApply(root1, countyResults)[-1]
  
  romneyvotes = sapply(popvote, function(x){x[x$candidate=='M. Romney', 'votes']})
  
  obamavotes = sapply(popvote, function(x){x[x$candidate=='B. Obama (i)', 'votes']})
  
  data.frame(State=stateName, County=countyNames, Romney=romneyvotes, Obama=obamavotes)
  
}

### The Final Data Frame for Part One:

endResult=lapply(statenames, xmlState)

endResult = do.call("rbind", endResult)

endResult$State = gsub("-", " ", endResult$State)

Part1 = endResult

######################################
# Bhavna Challa
# Final Project/ Part #2
######################################

B01003 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/B01003.csv", header = TRUE)
DP02 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP02.csv", header = TRUE)
DP03 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP03.csv", header = TRUE)
attach(B01003)
attach(DP03)

### Variables from B01003
length(unique(GEO.display.label))
### should be 3217 observations for each variable


unique(B01003$GEO.display.label)[1432]
whitealone = B01003[B01003$POPGROUP.display.label == "White alone", c("GEO.display.label", "HD01_VD01")]
rownames(whitealone) = c(1:3214)
match(unique(B01003$GEO.display.label), whitealone$GEO.display.label)
B01003[B01003$GEO.display.label == "Shannon County, South Dakota", ]
B01003[B01003$GEO.display.label == "Buffalo County, South Dakota", ]
B01003[B01003$GEO.display.label == "Jefferson County, Mississippi", ]
### counties 1432, 2369 and 2417 don't have White alone data. Add NA's for those values
row1 = matrix(c("Shannon County, South Dakota", "NA"), ncol = 2)
colnames(row1) = (c("GEO.display.label", "HD01_VD01"))
row2 = matrix(c("Buffalo County, South Dakota", "NA"), ncol = 2)
colnames(row2) = (c("GEO.display.label", "HD01_VD01"))
row3 = matrix(c("Jefferson County, Mississippi", "NA"), ncol = 2)
colnames(row3) = (c("GEO.display.label", "HD01_VD01"))

a = rbind(whitealone[1:1432,], row3)
rownames(a) = c(1:1433)
b = rbind(whitealone[1433:2368,], row2)
rownames(b) = c(1434:2370)
c = rbind(whitealone[2370:2417,], row1)
rownames(c) = c(2371:2419)
d = whitealone[2417: 3214,]
rownames(d) = c(2420:3217)
whiteonly = rbind(a, b, c, d)
colnames(whiteonly) = c("County Names", "White Alone")


totalpop = B01003[B01003$POPGROUP.display.label == "Total population", ("HD01_VD01")]
totalpop = as.data.frame(totalpop)
colnames(totalpop) = "Total Population"


### variables from DP03
employment = DP03[, c("HC01_VC04", "HC01_VC05", "HC01_VC06", "HC01_VC08", "HC01_VC10", "HC03_VC13", "HC03_VC31", "HC03_VC34")]
names(employment) = c("16 and Over", "Estimate In Labor Force", "Estimate in Civilian Labor Force", "Estimate Unemployed Civilian Labor Force", "Estimate Not in Labor Force", "Percent Unemployed", "Percent Using Public Transporation", "Percent Work from Home")
occupations = DP03[, c("HC03_VC41", "HC03_VC42", "HC03_VC50", "HC03_VC51", "HC03_VC52", "HC03_VC54", "HC03_VC62")]
names(occupations) = c("Percent Management, business, science, and arts Occupations", "Percent Service Industry Occupations", "Percent Agriculture, forestry, fishing and hunting, and mining Industry", "Percent Construction Industry", "Percent Manufacturing Industry", "Percent Retail Industry", "Percent Public Administration")
income = DP03[, c("HC03_VC75", "HC03_VC80", "HC03_VC84", "HC01_VC85", "HC03_VC90")]
names(income) = c("Percent Household income less 10,000", "Percent Household income btwn 50,000 and 74,999", "Percent Household income greater 200,000", "Estimate Median Household Income", "Percent with Social Security")


### variables from DP02
missing = match(unique(B01003$GEO.display.label), DP02$GEO.display.label) ### last 78 counties are missing
a= as.data.frame(c(DP02$HC01_VC03, rep(NA, 78)))
b= as.data.frame(c(DP02$HC03_VC07, rep(NA, 78)))
c = as.data.frame(c(DP02$HC03_VC12, rep(NA, 78)))
d = as.data.frame(c(DP02$HC03_VC10, rep(NA, 78)))
e = as.data.frame(c(DP02$HC01_VC20, rep(NA, 78)))
f = as.data.frame(c(DP02$HC03_VC40, rep(NA, 78)))
g = as.data.frame(c(DP02$HC03_VC87, rep(NA, 78)))
h = as.data.frame(c(DP02$HC03_VC90, rep(NA, 78)))
i = as.data.frame(c(DP02$HC03_VC130, rep(NA, 78)))
household = data.frame(a, b, c, d, e, f, g, h, i)
names(household) = c("Total Households", "Percent Married Couple Families", "Percent Single Mother Families", "Percent Single Father Families", "Average Household Size", "Percent Divorced", "Percent over 25 with High School Diploma", "Percent over 25 with Bachelor's", "Percent Born in US")

Part2 = data.frame(whiteonly, totalpop,employment, occupations, income, household)

#############################
# Kiho Park
# Final Project/ Part #3
#############################

library(XML)

gml=xmlParse("http://www.stat.berkeley.edu/users/nolan/data/Project2012/counties.gml")

root=xmlRoot(gml)

State=sapply(getNodeSet(root,"//state/gml:name"),function(x) xmlValue(x,trim=T)) 
###extract the names of states; length=51

county=sapply(getNodeSet(root,"//state/county/gml:name"),function(x) xmlValue(x,trim=T))
###extract the names of counties; length=3140

xcoord=as.numeric(sapply(getNodeSet(root,"//gml:X"),function(x) xmlValue(x,trim=T)))
###extract the longitude of each county

ycoord=as.numeric(sapply(getNodeSet(root,"//gml:Y"),function(x) xmlValue(x,trim=T)))
###extract the longitude of each county
a=getNodeSet(root,"//state")

b=rep(0,length(State))
for(i in 1:length(State)) {
  b[i]=length(xmlSApply(a[[i]],is.null))
}
countyperstate=b-1 ### Each a[[i]] contains the name of the state along with all the counties of that state.
### I subtracted 1 to take into account for the name of state (which we don' want), so that we are left with number of counties per state.
state=rep(State,countyperstate) 

m=data.frame(state,county,xcoord,ycoord)

Part3 = m

#############################
# Timothy Lee
# Final Project/ Part #4
#############################

countyVotes2004 = read.table("http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2004.txt",header=TRUE)

countyVotes2004[,1] = as.character(countyVotes2004[,1])

location = strsplit(countyVotes2004[,1],",")

countyVotes2004[,4] = countyVotes2004[,3]
countyVotes2004[,3] = countyVotes2004[,2]
names(countyVotes2004) = c("State","County","BushVotes","KerryVotes")

for (i in 1:length(location)) {
  countyVotes2004[i,1] = location[[i]][1]
  countyVotes2004[i,2] = location[[i]][2]
}

uppercase = function (x) {
  u = unlist(strsplit(x," "))
  substr(u,1,1) = toupper(substr(u,1,1))
  return(paste(u,collapse = " "))
}

countyVotes2004[,1] = sapply(countyVotes2004[,1],uppercase)
countyVotes2004[,2] = sapply(countyVotes2004[,2],uppercase)
countyVotes2004[,3] = as.numeric(countyVotes2004[,3])
countyVotes2004[,4] = as.numeric(countyVotes2004[,4])

Part4 = countyVotes2004

########################
# Data Consolidation
########################

Part13Cleaner=function(x){
  x=tolower(x)
  x=gsub("saint","saints",x)
  x=gsub(" st. ", " saints ", x)
  x=gsub(" ste. "," saints ",x)
  x=gsub("city", "", x)
  x=gsub("borough","",x)
  x=gsub(" parish", "", x)
  x=gsub(" county", "",x)
  x=gsub("-"," ",x)
  x=gsub("dewitt", "de witt",x)
  x=gsub("mc kean","mckean",x)
  x=gsub("jodaviess","jo daviess",x)
  x=gsub("lasalle","la salle",x)
  x=gsub("de soto","desoto",x)
  x=gsub("jeff davis","jefferson davis",x)
  x=gsub("de baca","debaca",x)
  x=gsub("le flore","leflore",x)
  x=gsub("la vaca","lavaca",x)
  x=gsub("miami dade","dade",x)
  x=gsub(" $", "", x)
  x=gsub(" $","",x)
  return(x)
}

Part2Cleaner=function(x){
  x=gsub("(s|S)te?\\. ", "saints ", x)
  x=gsub("city$", "", x)
  x=gsub("(p|P)arish$", "", x)
  x=gsub("City$", "", x)
  x=gsub("County$", "",x)
  x=gsub("-"," ",x)
  x=gsub("new york$","manhattan",x)
  x=gsub("(city and borough$)|(municipality$)|(parish$)","",x)
  x=gsub("borough$","",x)
  x=gsub("miami dade$","dade",x)
  x=gsub("lasalle$","la salle",x)
  x=gsub("mc kean$","mckean",x)
  x=gsub("du page$","dupage",x)
  x=gsub("la porte$","laporte",x)
  x=gsub("dewitt", "de witt",x)
  x=gsub("de soto","desoto",x)
  x=gsub("jeff davis","jefferson davis",x)
  x=gsub("de baca","debaca",x)
  x=gsub("le flore","leflore",x)
  x=gsub("la vaca","lavaca",x)  
  x=gsub(" $","",x)
  x=tolower(x)
  return(x)
}

Part4Cleaner=function(x){
  x=gsub("(s|S)t. ", "saints ", x)
  x=gsub(" st "," saints ",x)
  x=gsub("city$", "", x)
  x=gsub("(p|P)arish$", "", x)
  x=gsub("City$", "", x)
  x=gsub("County$", "",x)
  x=gsub("-"," ",x)
  x=gsub("new york$","manhattan",x)
  x=gsub("(borough$)|(city and borough$)|(municipality$)|(parish$)","",x)
  x=gsub("de kalb","dekalb",x)
  x=gsub("de soto$","desoto",x)
  x=gsub("du page$","dupage",x)
  x=gsub("dewitt", "de witt",x)
  x=gsub("obrien$","o'brien",x)
  x=gsub("prince georges$",paste("prince george","'","s",sep=""),x)
  x=gsub("queen annes$",paste("queen anne","'","s",sep=""),x)
  x=gsub("saints marys$",paste("saints mary","'","s",sep=""),x)
  x=gsub("jeff davis","jefferson davis",x)
  x=gsub("de baca","debaca",x)
  x=gsub("le flore","leflore",x)
  x=gsub("la moure$","lamoure",x)
  x=gsub("la porte","laporte",x)
  x=gsub("dona ana$","doña ana",x)
  x=gsub(" $","",x)
  x=tolower(x)
  return(x)
}

#
Part1[,1]= as.character(Part1[,1])
Part1[,2] = as.character(Part1[,2])
Part1[,1] = sapply(Part1[,1],tolower)
Part1[,2] = sapply(Part1[,2],tolower)
Part1[,1] = paste(Part1[,1], ", ", Part1[,2],sep = "")
Part1 = Part1[,-2]
names(Part1)[1] = "location"
Part1$location=Part13Cleaner(Part1$location)


#
Part2[,1] = as.character(Part2[,1])
loc2 = tolower(gsub(" County","",Part2[,1]))
loc2 = strsplit(loc2,", ")
loc2 = sapply(loc2,function(x) {paste(x[2], ", ",x[1],sep="")})
Part2[,1] = loc2
names(Part2)[1] = "location"
Part2$location=Part2Cleaner(Part2$location)

#
Part3[,1] = as.character(Part3[,1])
Part3[,2] = as.character(Part3[,2])
Part3[,1] = (sapply(Part3[,1],tolower))
Part3[,2] = (sapply(Part3[,2],function(x) {tolower(gsub(" County","",x))}))
Part3[,1] = paste(Part3[,1],", ",Part3[,2],sep="")
Part3 = Part3[-2]
names(Part3)[1] = "location"
Part3[Part3$location=="new york, new york",1]="new york, manhattan"
Part3$location=Part13Cleaner(Part3$location)

#
Part4[,1] = sapply(Part4[,1],tolower)
Part4[,2] = sapply(Part4[,2],tolower)
Part4[,1] = paste(Part4[,1],", ",Part4[,2],sep="")
Part4 = Part4[-2]
names(Part4)[1] = "location"
Part4$location=gsub("district of columbia, washington","district of columbia, district of columbia",Part4$location)
Part4$location=Part4Cleaner(Part4$location)

#############################################
### Combining Data Frames
#############################################

f14 = merge(Part1,Part4,all=TRUE)
f23 = merge(Part2,Part3,all = TRUE)
f1234 = merge(f14,f23,all=TRUE)
locF = strsplit(f1234[,1],", ")
locS = sapply(locF,function(x) {uppercase(paste(x[1]))})
locC = sapply(locF,function(x) {uppercase(paste(x[2]))})
finalDF = data.frame(locS,locC,f1234[-1])
finalDF[,1]=as.character(finalDF[,1])
finalDF[,2]=as.character(finalDF[,2])
finalDF[,38]=as.numeric(finalDF[,38])
finalDF[,39]=as.numeric(finalDF[,39])
names(finalDF)[1] = "State"
names(finalDF)[2] = "County"
names(finalDF)[38]="Longitude (X)"
names(finalDF)[39]="Latitude (Y)"

#############################################
## Testing the data
#############################################

# bad = which(rowSums(is.na(finalDF)) > 0)
# bad = setdiff(bad,grep(pattern='(Alaska|Puerto Rico|Virginia)',x=finalDF$State))
# finalDF[bad,"State"]

#############################################
### Final Project/ Step 3/ Part A
### Timothy Lee/ Max Lan
#############################################
library(rpart)
winner2004=as.numeric(finalDF[,"BushVotes"]>finalDF[,"KerryVotes"])

winner2012=as.numeric(finalDF[,"Romney"]>finalDF[,"Obama"])

data3a=data.frame(outcome04=winner2004,outcome12=winner2012,
                  finalDF$Longitude, finalDF$Latitude,
                  GOP04=finalDF$BushVotes,DEM04=finalDF$KerryVotes,
                  Estimate.Median.Household.Income=as.numeric(finalDF$Estimate.Median.Household.Income))
data3a=na.omit(data3a)

trainingSet=sample(2996,1498)
trainingA=data3a[trainingSet,]
trainingB=data3a[-trainingSet,]

test3=rpart(outcome04 ~ GOP04 + DEM04 + Estimate.Median.Household.Income,
            data=data3a,method="class", control = rpart.control(cp = 0.005))

test3c=rpart(outcome04 ~ GOP04 + DEM04 + Estimate.Median.Household.Income,
             data=trainingA,method="class", control = rpart.control(cp = 0.005))

plot(test3,ylim=c(0.55,1.1),uniform = TRUE,main="Prediction Tree for County Winners in 2012")
text(test3,cex=.75,use.n=TRUE)
legend(23,y=1.1,c("1=GOP win","0=DEM win"))

t3=predict(test3,data3a,type="class")
model3=t3==data3a$outcome12
sum(model3,na.rm=T)/length(model3)

t3c=predict(test3c,trainingB,type="class")
model3c=t3c==trainingB$outcome04
sum(model3c,na.rm=T)/length(model3c)

#######################################################
### Map of the tree prediction for 2012
library(maps)

map(database="state")

points(data3a[,3]/10^6, data3a[,4]/10^6, pch=16, cex=0.5, col=c("blue", "red")[t3])

mtext("Predictions for 2012 Using GOP and Democrat Votes and Median Household Income (cp=0.005)", 
      side=3, line=1, font=2)

mtext("The prediction results for each State and County are represented by\n
      Red points (Republicans) and Blue points (Democrats).", side=1, line=2, cex=0.8, font=2)

#######################################################
### map of actual results for 2012

#map(database="state")

#points(data3a[,3]/10^6, data3a[,4]/10^6, pch=16, cex=0.5, col=c("blue", "red")[data3a$outcome12+1])

#mtext("The Actual Results in 2012", side=3, line=1, col="red", font=2)


########################################################
### Final Project/ Step 3/ Part B
### Seyedeh Maryam Javanmard/ Kiho Park/ Bhavna Challa
########################################################

### Knn Package:

library(class)

### Choosing Longitude and Latitude and Some Variables From Our Data Frame:

mydata = data.frame(outcome04=as.numeric(finalDF$BushVotes > finalDF$KerryVotes),
                    finalDF$Longitude, finalDF$Latitude, 
                    finalDF$Percent.Unemployed, finalDF$Percent.Divorced,
                    finalDF$Percent.Born.in.US,
                    finalDF$Percent.over.25.with.High.School.Diploma,
                    finalDF$Average.Household.Size)

### Removing NA's:

mydata = na.omit(mydata)

### Nearest Neighbor:

cl = mydata[,1]

#########################
### Plot #1:
#########################

library(maps)

### The Predicted Data Using Knn Function (K=4):

map(database="state")

predicted = knn(train=mydata, test=mydata, cl, k=4, prob=TRUE)

errors = abs(as.numeric(as.character(predicted))-cl) + 1

#######################################################

### To check our model:

#points(mydata[,2]/10^6, mydata[,3]/10^6, pch=16, cex=0.5, col=c("green", "red")[errors])

#######################################################

points(mydata[,2]/10^6, mydata[,3]/10^6, pch=16, cex=0.5, col=c("blue", "red")[predicted])

mtext("Predictions for 2012 Based on 2004 Election Data\n(K=4)", side=3, line=1, font=2)

mtext("The prediction results for each State and County are represented by\n
      Red points (Republicans) and Blue points (Democrats).", side=1, line=2, cex=0.8, font=2)

###############################
### Plot #2:
###############################

library(maps)

### The Predicted Data Using Knn Function (K=50):

map(database="state")

predicted = knn(train=mydata, test=mydata, cl, k=50, prob=TRUE)

errors = abs(as.numeric(as.character(predicted))-cl) + 1

#######################################################

### To check our model:

#points(mydata[,2]/10^6, mydata[,3]/10^6, pch=16, cex=0.5, col=c("green", "red")[errors])

#######################################################

points(mydata[,2]/10^6, mydata[,3]/10^6, pch=16, cex=0.5, col=c("blue", "red")[predicted])

mtext("Predictions for 2012 Based on 2004 Election Data\n(K=50)", side=3, line=1, font=2)

mtext("The prediction results for each State and County are represented by\n
      Red points (Republicans) and Blue points (Democrats).", side=1, line=2, cex=0.8, font=2)

####################################
### The Actual Results in 2012:
####################################

# actual12 = as.numeric(finalDF$Romney > finalDF$Obama) + 1
# 
# map(database="state")
# 
# points(mydata[,2]/10^6, mydata[,3]/10^6, pch=16, cex=.5, col=c("blue", "red")[actual12])
# 
# mtext("The Actual Results in 2012", side=3, line=1, col="red", font=2)








