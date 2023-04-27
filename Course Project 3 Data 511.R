#libarary necessary packages

#set occupation to NULL & set seed to 12345
proj3_income$occupation<-NULL
set.seed(12345)

# Set Non-Valid Entry to NA
proj3_income$capital.gain[proj3_income$capital.gain == 99999] <- NA

#find mean and standard deviation
cgm <- mean(proj3_income$capital.gain, na.rm = TRUE)
cgsd <- sd(proj3_income$capital.gain, na.rm = TRUE)




#Create an Imputation Model
imputation_model<-preProcess(proj3_income, method = c("knnImpute"))

#Impute Values for Missing Data
proj3_incomecg.imp<-predict(imputation_model,proj3_income)

#Undo Standardaization and Create New Variable
proj3_income$cg.imp <- round(proj3_incomecg.imp$capital.gain * cgsd + cgm, 5) 

#provide a summary of income
summary(proj3_income$income)




#Rename education to education.old
names(proj3_income)[names(proj3_income)=="education"] <- "education.old"
#Reclassify education into educ with two categories, low and high
proj3_income$educ <- ifelse(proj3_income$education.old %in% c("10th", "11th", "12th","1st-4th","5th-6th","7th-8th","9th","HS-grad","Preschool"), "Low", "High")

#contruct a table for income and education
educincomect<-table(proj3_income$income,proj3_income$educ)

#income educ chi sq test
chisq_incomeeduc<- chisq.test(proj3_income$educ,proj3_income$income)
chisq_incomeeduc

#rename relationship 
names(proj3_income)[names(proj3_income)=="relationship"] <- "relationship.old"
#Reclassify  into relationship with two categories, HusWife and Other
proj3_income$HusWife <- ifelse(proj3_income$relationship.old %in% c("Husband","Wife"), "HusWife", "Other")

#construct a table for income and relationship
incomerelact<-table(proj3_income$income,proj3_income$HusWife)
prop.table(incomerelact,2)

# chi squared test for income and marital status
chisq_incomerela<-chisq.test(proj3_income$HusWife,proj3_income$income)
chisq_incomerela

#Create Partition
intrain<-createDataPartition(y=proj3_income$income,p=.5,list=FALSE)
proj3_income.train<- proj3_income[intrain,]
proj3_income.test<-proj3_income[-intrain,]

#summary of income for partition
summary(proj3_income.train)
summary(proj3_income.test)

#Merge Parts and costruct a boxplot
proj3_income.train$part<-rep("train", nrow(proj3_income.train))
proj3_income.test$part<- rep("test", nrow(proj3_income.test))
proj3_income.all<-rbind(proj3_income.train, proj3_income.test)
boxplot(cg.imp~as.factor(part), data = proj3_income.all)
boxplot(capital.loss~as.factor(part),data = proj3_income.all)

#kruskal wallis test for imputed capital gains and capital loss
kruskal.test(cg.imp~as.factor(part),data = proj3_income.all)
kruskal.test(capital.loss~as.factor(part), data = proj3_income.all)

#Create a Prop Table for the Education
lowedu.train<-sum(proj3_income.train$educ %in% c("Low"))
highedu.train<-sum(proj3_income.train$educ %in% c("High"))
lowedu.test<-sum(proj3_income.test$educ %in% c("Low"))
highedu.test<-sum(proj3_income.test$educ %in% c("High"))


#Create a Matrix and a prop table for partitioned educ
lowedu.test<-sum(proj3_income.test$educ %in% c("Low"))
highedu.test<-sum(proj3_income.test$educ %in% c("High"))
lowedu.train<-sum(proj3_income.train$educ %in% c("Low"))
highedu.train<-sum(proj3_income.train$educ %in% c("High"))
ptedu<-matrix(c(7446,7308,8835,8972), nrow = 2)
colnames(ptedu)<-c("Low", "High")
row.names(ptedu)<- c("Training", "Test")
ptedu
round(prop.table(ptedu,1),4)


#Create a prop table for partitioned relationship
HusWife.train<-sum(proj3_income.train$HusWife %in% c("HusWife"))
Other.train<-sum(proj3_income.train$HusWife %in% c("Other"))
HusWife.test<-sum(proj3_income.test$HusWife %in% c("HusWife"))
Other.test<-sum(proj3_income.test$HusWife %in% c("Other"))
ptrel<- matrix(c(9012,8788,7269,7492), nrow=2)
colnames(ptrel)<-c("Other", "HusWife")
row.names(ptrel)<- c("Train", "Test")
round(prop.table(ptrel,1),4)


#summary of data.train$income
summary(proj3_income.train$income)

