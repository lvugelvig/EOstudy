# Clean the memory
rm(list=ls())

## NOTE: THIS IS EXTREMELY COARSE, NEED TO DISTINGUISH BETWEEN TYPES OF EVENTS
## AND I AM JUST PLAYING WITH THE DATA.

## Load the data
odata <- read.csv("AllData_20161108.csv", stringsAsFactors = FALSE)
dim(odata)

# Select only the ones that we include in the study (removing duplicates etc.)
adata <- odata[odata$Include == TRUE,]
dim(adata)

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
tmp.reply <- ((adata$Org_reply1)|(adata$Org_reply2)) & (!is.na(adata$Org_reply1)|!is.na(adata$Org_reply2))
boxplot(adata$rInv ~ tmp.reply, main="Replied?")
text(1:2, rep(0.9, 2), paste("n=", tapply(adata$rInv, tmp.reply, length), sep="") )

summary(lm(adata$rInv ~ tmp.reply))

adata[is.na(adata$Org_reply1),]

adata$Org_name1[duplicated(adata$Org_name1)]

# Create columns with ratios
tbl <- cbind(aa, rInv = aa$New_Invited_W / aa$New_Invited_nb, #
             rIns = aa$New_Instructor_W / aa$New_Instructor_nb, #
             rOrg = aa$New_Org_W / aa$New_Org_nb)

# Plots
# Questions
par(las=1)
boxplot(rInv ~ Question.1, data = tbl, main="Q1: Aware?", ylab="Proportion female speakers")
text(1:2, rep(0.9, 2), paste("n=", tapply(tbl$Question.1, tbl$Question.1, length), sep="") )

boxplot(rInv ~ Question.2, data = tbl, main="Q2: Took gender into account?", ylab="Proportion female speakers")
text(1:2, rep(0.9, 2), paste("n=", tapply(tbl$Question.2, tbl$Question.2, length), sep="") )

boxplot(rInv ~ Question.3, data = tbl, main="Q3: Prescriptions?", ylab="Proportion female speakers")
text(1:3, rep(0.9, 3), paste("n=", tapply(tbl$Question.3, tbl$Question.3, length), sep="") )

