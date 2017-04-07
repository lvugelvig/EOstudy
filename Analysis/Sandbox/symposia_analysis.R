# Clean the memory
rm(list=ls())

########################################################################################
## Download the data
# install.packages("RCurl")
library(RCurl)
URL <- "https://docs.google.com/spreadsheets/d/1RE9EXek2Vs2JxaTrvnPnocgRPbUI29I-9fXZI0wUG6I/export?gid=0&format=csv"
tmp <- getURL(URL)
odata <- read.csv(textConnection(tmp), stringsAsFactors = FALSE)


# Select only the ones that we include in the study (removing duplicates etc.)
adata <- odata[odata$Include == TRUE,]

# Data for which we received a response
# Those who replied
aa <- adata[!(adata$Timestamp=="" | is.na(adata$Timestamp)), ]

cat("We screened", nrow(odata), "symposia, ")
cat("and kept", nrow(adata), "of them. \n")
cat("We received", nrow(aa), "responses, ", sum(aa[,"Question.5"]=="Yes"), "of which we can use (respondent ticked Yes for data sharing).")

##################################################
# Adding columns corresponding to ratios
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# NOTE: need to use the corrected values if there has been a correction
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
adata <- cbind(adata, 
               rInv = adata$Invited_W / adata$Invited_nb, 
               rIns = adata$Instructor_W / adata$Instructor_nb, 
               rOrg = as.numeric(adata$Org_W) / as.numeric(adata$Org_nb))

# Histogram of proportion of women
hist(adata$rInv, xlim=c(0,1), breaks=seq(-0.025,1.025,by=0.05), main = "Symposia, Prop invited women", xlab = "Prop. invited women", axes = FALSE, col='grey')
axis(1, at=seq(0, 1, by=0.1))
axis(2, las=1)

# Look at proportion of invited women as function of proportion women organizers
plot(adata$rOrg, adata$rInv, xlim = c(0, 1), ylim = c(0, 1), xlab = "Prop. W among organisers", ylab = "Prop. W among invited speakers", las = 1)
aOI  <- lm(rInv~rOrg, data=adata)
abline(aOI)
summary(aOI)

## Effect of proportion of women on whether the organizers replied:
library(MASS)
Q0.lda <- lda(tmp.reply ~ adata$rInv)
summary(Q0.lda)
Q0.lda


# Plotting with boxplot
boxplot(adata$rInv ~ tmp.reply, main="Replied?")
text(1:2, rep(0.9, 2), paste("n=", tapply(adata$rInv, tmp.reply, length), sep="") )

summary(lm(tmp.reply ~ adata$rInv))

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

# Did they say no because wrong?
tbl[tbl$Question.1=="No", c("Org_name1", "Question.0")]

# Contingency table for Q1
table(tbl$Question.1, tbl$Question.0)

# Q2
boxplot(rInv ~ Question.2, data = tbl, main="Q2: Took gender into account?", ylab="Proportion female speakers")
text(1:2, rep(0.9, 2), paste("n=", tapply(tbl$Question.2, tbl$Question.2, length), sep="") )
summary(aov(rInv ~ Question.2, data = tbl))

# Q3
boxplot(rInv ~ Question.3, data = tbl, main="Q3: Prescriptions?", ylab="Proportion female speakers")
text(1:3, rep(0.9, 3), paste("n=", tapply(tbl$Question.3, tbl$Question.3, length), sep="") )
summary(aov(rInv ~ Question.3, data = tbl))


