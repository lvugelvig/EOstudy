# Clean the memory
rm(list=ls())
par(mfrow=c(1,1))

# Load lubridate to handle dates
library(lubridate)

## NOTE: THIS IS EXTREMELY COARSE, NEED TO DISTINGUISH BETWEEN TYPES OF EVENTS
## AND I AM JUST PLAYING WITH THE DATA.

## Download the data
# install.packages("RCurl")
library(RCurl)
URL <- "https://docs.google.com/spreadsheets/d/1i67UACuGm4gKLlunEzIHdxCBSZJP2bhZWB3lSPkEocg/export?gid=0&format=csv"
tmp <- getURL(URL)
odata <- read.csv(textConnection(tmp), stringsAsFactors = FALSE)

#odata <- read.csv("AllData_20161206.csv", stringsAsFactors = FALSE)
dim(odata)
cat("We screened ", nrow(odata), " evoldir ads, ")

# Select only the ones that we include in the study (removing duplicates etc.)
adata <- odata[odata$Include == TRUE,]
dim(adata)
cat("and kept ", nrow(adata), "of them. \n")

# Add year-month column
adata <- cbind(adata, Ad_YM = paste0(adata$Ad_year, "-", formatC(month(adata$Ad_FullDate), width = 2, format = "d", flag = "0")))

# Add column indicating whether a reply has been received
adata <- cbind(adata, Org_reply = !(adata$Timestamp==""))

# Adding columns corresponding to ratios
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# NOTE: need to use the corrected values if there has been a correction
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
adata <- cbind(adata, 
               rInv = adata$Invited_W / adata$Invited_nb, 
               rIns = adata$Instructor_W / adata$Instructor_nb, 
               rOrg = as.numeric(adata$Org_W) / as.numeric(adata$Org_nb))

# Look at proportion of invited women as function of proportion women organizers
plot(adata$rOrg, adata$rInv, xlim = c(0, 1), ylim = c(0, 1), xlab = "Prop. W among organisers", ylab = "Prop. W among invited speakers", las = 1)
aOI  <- lm(rInv~rOrg, data=adata)
abline(aOI)
summary(aOI)
## NOTE: need to check at some point the number of organisers, 
## because sometimes "1" does not reflect the actual number.

# Histogram of proportion of women
hist(adata$rInv, xlim=c(0,1), breaks=seq(-0.025,1.025,by=0.05), main = "AllData, Prop invited women", xlab = "Prop. invited women", axes = FALSE, col='grey')
axis(1, at=seq(0, 1, by=0.1))
axis(2, las=1)

cat("========================================================================\n  QUESTION: WHAT DO WE DO WITH EVENTS WITH ONLY ONE PERSON INVITED?\n========================================================================\n")

# Those who replied
aa <- adata[!(adata$Timestamp=="" | is.na(adata$Timestamp)), ]
dim(aa)
cat("We received ", nrow(aa), " responses, ", sum(aa[,"Question.5"]=="Yes"), "of which we can use (respondent ticked Yes for data sharing).")

# Check consistency when merging data
all(aa$Ad_ID == aa$Event.name)

# Histogram of proportion of women
hist(aa$rInv, xlim=c(0,1), breaks=seq(-0.025,1.025,by=0.05), main = "Only replies, Prop invited women", xlab = "Prop. invited women", col="red", freq=TRUE)
hist(aa[aa$Question.2=="Yes",]$rInv, xlim=c(0,1), breaks=seq(-0.025,1.025,by=0.05), main = "Only replies, Prop invited women", xlab = "Prop. invited women", col="yellow", add=TRUE, freq=TRUE)

# Check lines where people corrected the result
checkno <- aa[aa$Question.0=="No",]

compcols.oneline <- function(a, b){
  if(is.na(a) & is.na(b)) return(TRUE)
  else
    if( (is.na(a) & !is.na(b)) | (!is.na(a) & is.na(b)) ) return(FALSE)
    else
      if(a==b) return(TRUE) else return(FALSE)
}
compcols <- function(A, B){
  n <- length(A)
  out <- rep(0, n)
  for (i in 1:n){
    out[i] <- compcols.oneline(A[i], B[i])
  }
  return(out)
}

tmp <- aa[, c(1, 32, 12, 33, 13, 34, 14, 35, 15, 36, 16, 37, 17, 38, 18, 39, 19, 40)]
diffcols <- !( compcols(tmp[,3], tmp[,4]) & compcols(tmp[,5], tmp[,6]) & compcols(tmp[,7], tmp[,8]) & compcols(tmp[,9], tmp[,10]) & compcols(tmp[,11], tmp[,12]) & compcols(tmp[,13], tmp[,14]) & compcols(tmp[,15], tmp[,16]) & compcols(tmp[,17], tmp[,18]))

tmp[diffcols & tmp$Question.0=="Yes",]

# Effect of proportion of female speakers on whether organizers replied
tmp.reply <- adata$Org_reply1==TRUE | adata$Org_reply2==TRUE #((adata$Org_reply1)|(adata$Org_reply2)) & (!is.na(adata$Org_reply1)|!is.na(adata$Org_reply2))
tmp.reply[is.na(tmp.reply)] <- FALSE
source("plotting.R")
histSMD.TF(tmp.reply, adata$rInv)

tmp.reply.inv <- tmp.reply[!is.na(adata$rInv)]
tmp.reply.ins <- tmp.reply[!is.na(adata$rIns)]


plothist <- function(whichones, what="rInv", fillcol=gray(0.2), ...){
  brks <- seq(-0.025, 1.025, by=0.05) #<- seq(-0.025,1.025,by=0.05)
  hist(adata[,what][whichones], breaks=brks, plot=TRUE, ylim=c(0,yM), border="white", col=fillcol, axes=FALSE, 
             xlab = "Proportion of female speakers", ylab="Count", xlim=range(brks), ...)
  # Add grid
  for (i in seq(1, yM)) lines(c(brks[1], brks[length(brks)]), c(i,i), col="white", lwd=0.5)
  # Add axes
  axis(1, at=seq(0,1,by=0.1), pos=0)
  axis(2, pos=brks[1])
}
par(mfrow=c(2,1))
plothist(!tmp.reply.inv, main="Did not reply")
plothist(tmp.reply.inv, main="Replied")
summary(aov(adata$rInv[!is.na(adata$rInv)] ~ tmp.reply.inv))

par(mfrow=c(2,1))
plothist(!tmp.reply.ins, what= "rIns", main="Did not reply")
plothist(tmp.reply.ins, what="rIns", main="Replied")
summary(aov(adata$rIns[!is.na(adata$rIns)] ~ tmp.reply.ins))

# Plotting with boxplot
boxplot(adata$rInv ~ tmp.reply, main="Replied?")
text(1:2, rep(0.9, 2), paste("n=", tapply(adata$rInv, tmp.reply, length), sep="") )

summary(lm(adata$rInv ~ tmp.reply))
summary(aov(adata$rInv ~ tmp.reply))
adata[is.na(adata$Org_reply1),]

adata$Org_name1[duplicated(adata$Org_name1)]

# Create columns with ratios
tbl <- cbind(aa, rInv = aa$New_Invited_W / aa$New_Invited_nb, #
             rIns = aa$New_Instructor_W / aa$New_Instructor_nb, #
             rOrg = aa$New_Org_W / aa$New_Org_nb)

par(mfrow=c(1,1))
# Plots
# Questions
par(las=1)
boxplot(rInv ~ Question.1, data = tbl, main="Q1: Aware?", ylab="Proportion female speakers")
text(1:2, rep(0.9, 2), paste("n=", tapply(tbl$Question.1, tbl$Question.1, length), sep="") )
summary(aov(rInv ~ Question.1, data = tbl))

boxplot(rInv ~ Question.2, data = tbl, main="Q2: Took gender into account?", ylab="Proportion female speakers")
text(1:2, rep(0.9, 2), paste("n=", tapply(tbl$Question.2, tbl$Question.2, length), sep="") )
summary(aov(rInv ~ Question.2, data = tbl))

boxplot(rInv ~ Question.3, data = tbl, main="Q3: Prescriptions?", ylab="Proportion female speakers")
text(1:3, rep(0.9, 3), paste("n=", tapply(tbl$Question.3, tbl$Question.3, length), sep="") )
summary(aov(rInv ~ Question.3, data = tbl))

summary(aov(rInv ~ Question.3=="Suggested", data = tbl))

#---------------------------------------------------------------------------------------------------------
# Check proportion of replies as function of time

allreplies <- aggregate(cbind(adata$Ad_YM, 1*!is.na(adata$Ad_ID), adata$Org_reply==TRUE, (adata$Org_reply1==TRUE & !is.na(adata$Org_reply1)),  (adata$Org_reply2==TRUE & !is.na(adata$Org_reply2))), by=list(adata$Ad_YM), FUN=sum)
names(allreplies) <- c("Date", "Ignore", "NbAds", "NbReplies", "NbR1", "NbR2")
allreplies <- cbind(allreplies, PropR = allreplies$NbReplies/allreplies$NbAds, PropR1 = allreplies$NbR1/allreplies$NbAds, PropR2 = allreplies$NbR2/allreplies$NbAds)
# Sanity check
all(allreplies$NbAds == allreplies$R1 + allreplies$R2)

# Plot
yxmin <- 0.5
plot(yxmin,0, type="n", ylim=c(0,1), xlim=c(yxmin,nrow(allreplies)), 
     axes=FALSE, xlab = "Date", ylab = "Proportion of replies")
par(las=1)
axis(1, at=1:nrow(allreplies), labels = c("A", "M", "J", "J", "A", "S", "O", "N", "D", "J", "F", "M"))
yline <- 2
for(i in 1:9) mtext("2016", 1, at = i, line = yline)
for(i in 10:12) mtext("2017", 1, at = i, line = yline)

axis(2, at=seq(0,1,by=0.1), pos=yxmin)

points(allreplies$PropR, pch=16)
points(allreplies$PropR1, pch=1)
plot(allreplies$Date, allreplies$PropR, type="p", pch=1)


# Global proportion of replies
sumreplies <- data.frame(matrix(colSums(cbind(adata$Ad_YM, 1*!is.na(adata$Ad_ID), adata$Org_reply==TRUE, (adata$Org_reply1==TRUE & !is.na(adata$Org_reply1)),  (adata$Org_reply2==TRUE & !is.na(adata$Org_reply2)))), nrow = 1))
names(sumreplies) <- c("Ignore", "NbAds", "NbReplies", "NbR1", "NbR2")
sumreplies$NbReplies/sumreplies$NbAds
