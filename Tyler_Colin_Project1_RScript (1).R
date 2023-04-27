#Library Necessary Packages
library(caret)
library(ggplot2)
library(rpart)
library(rpart.plot)


# Set Non-Valid Entry to NA
proj1$capital.gain[proj1$capital.gain == 99999] <- NA

#Find mean and SD of Capital Gain 
cgm <- mean(proj1$capital.gain, na.rm = TRUE)
cgsd <- sd(proj1$capital.gain, na.rm = TRUE)

#set seed 
set.seed(12345)

#Create an Imputation Model
imputation_model<-preProcess(proj1, method = c("knnImpute"))

#Impute Values for Missing Data
proj1cg.imp<-predict(imputation_model,proj1)

#Undo Standardaization and Create New Variable
proj1$cg.imp <- round(proj1cg.imp$capital.gain * cgsd + cgm, 5)

#Find Mean and SD of Imputed Capital Gains
mean(proj1$cg.imp)
sd(proj1$cg.imp)

#Construct a Flag Variable for Those Missing in Capital Gains
proj1$cg.missing<-ifelse(is.na(proj1$capital.gain),1,0)

#Construct a Contingency Table (Income against Missing Capital Gains)

incomecg.missingCT<-table(proj1$income, proj1$cg.missing)
incomecg.missing.with.sum<- addmargins(A=incomecg.missingCT, margin = c(1,2), FUN = sum, quiet = TRUE)
round(prop.table(incomecg.missingCT,2),2)
incomecg.missing.with.sum


#Create an Index Field 
proj1$ID<- 1:nrow(proj1)

#Find Data for ID 2001
proj1[proj1$ID == 2001,]

#Rename marital.status to marital.status.old
names(proj1)[names(proj1)=="marital.status"] <- "marital.status.old"

#Reclassify the marital.status variable
proj1$marital.status <- ifelse(proj1$marital.status.old %in% c("Married-AF-Spouse", "Married-civ-spouse", "Married-spouse-absent"), "Married", "Other")

#Create a Table for Income and Marital Status
incomemarriage_CT<- table(proj1$income, proj1$marital.status)

#Create a Prop Table for Income(Rows) and Marital Status
prop.table(incomemarriage_CT,2)

#Reclassify where CG or CL is 1 and 0 is 0
proj1$capgl <- ifelse(proj1$cg.imp != 0 | proj1$capital.loss != 0, 1, 0)

#Construct a Table for Income(rows) and capgl
incomecapgl_CT<-table(proj1$income, proj1$capgl)

#Construct a Contingency Table for Income(Rows)and capgl
prop.table(incomecapgl_CT,2)

#Find the proportion of high earners, over all records in the data set 
table(proj1$income)
3554/14797

#Find the mean and SD of capital.loss
mean(proj1$capital.loss)
sd(proj1$capital.loss)

#Find the number of outlier capital loss records (High End)
sum(proj1$capital.loss>1307.54)

#Find the sum of outlier records as they correspond to income 
sum(proj1$capital.loss>1307.54 & proj1$income == ">50K")


#Divide to find proportion
353/679

# Predict Income Based on Education
EI<- rpart(formula = income ~ education, data = proj1)

#Create a decision tree
rpart.plot(EI)

#Bin Based on Decision Tree
educ.bin<- cut(proj1$education, breaks = c(-1000, 0.24, 0.49, 1000))

#Contruct a Table
IE_CT<- table(proj1$income, educ.bin)
prop.table(IE_CT)

#Z-standardize Education
proj1$education_z<- (proj1$education-mean(proj1$education))/(sd(proj1$education))

#construct a Z_IE table
IE_CT_Z<- table(proj1$income,educ.bin)

#Bin Z-Standardized Education
educ.bin_z<- cut(proj1$education_z, breaks = c(-1000, 0.24, 0.49, 1000))

#Construct a Contingency Table for Income and educ.bin_z with column proportions
IE_CT_ZZ<-table(proj1$income, educ.bin_z)
prop.table(IE_CT_ZZ, 2)
IE_CT_ZZ

#Construct Bar Chart of Binned Education with an Overlay of Income
ggplot(proj1, aes(educ.bin_z))+geom_bar(aes(fill=income), color="black")+xlab("Binned Z-Standardized Education")+ylab("Frequency")+ggtitle("Bar Chart of Binned Education with an Overlay of Income")

#Construct a Normalized Bar Chart of Binned Education with an Overlay of Income
ggplot(proj1, aes(educ.bin_z))+geom_bar(aes(fill=income), color="black",position = "fill")+xlab("Binned Z-Standardized Education")+ylab("Frequency")+ggtitle("Normalized Bar Chart of Binned Education with an Overlay of Income")

# Construct a Table for Sex and Income
sexincome_table<- table(proj1$income,proj1$sex)

#Construct a Table for Sex and Income with Column Proportions and Column Totals
prop.table(sexincome_table,2)
t2<-addmargins(A=sexincome_table, margin = c(1,2), FUN = sum, quiet = TRUE)
t2
