#--------------------------------Work directory####

getwd()

#--------------------------------------Citations####
citation()  #Citation for R
citation("psych")  
citation("foreign")
citation("memisc")
citation("car")
citation("multicon")
citation("lavaan")
citation("AutoModel")
citation("apaTables")
citation("lavaan")
citation("semPlot")
citation("semTools")
#--------------------------------------Load data####
library(foreign)
METUX2018<-read.spss("ArtsApp2018_Metux_model.sav", use.value.labels = F, to.data.frame = TRUE) #Data frame 
View(METUX2018)
str(METUX2018)
head(METUX2018)
names(METUX2018)

#--------------------------------------Data preparations####
options(scipen = 999)
library(memisc)
library(car)

#--Recode gender for data analysis
table(METUX2018$Gender) #1=Males, 2=Females, 3=Other

METUX2018$Gender2 <- Recode(METUX2018$Gender, "1= 1; 2= 0; 3=NA" 
                          , as.numeric=T) 

table(METUX2018$Gender2)
table(METUX2018$Gender)

#Create labels for the new recoded Gender variable
METUX2018$Gender2 = factor(METUX2018$Gender2, labels = c("Females", "Males")) #Females=0, Males=1, Other/3=NA
table(METUX2018$Gender2)

#--Recode name for type of digital tool
table(METUX2018$Type_digital_tool)
METUX2018$Type_digital_tool = factor(METUX2018$Type_digital_tool, labels = c("mobile_app", "software",
                                                                             "online_resource", "other")) #Mobile app=1, Software=2, Online resource=3, Other=4
table(METUX2018$Type_digital_tool)

#--Recode name for subject
table(METUX2018$Subject)
METUX2018$Subject = factor(METUX2018$Subject, labels = c("natural_sciences", "social_sciences",
                                                       "economics", "health", "humanities", "law", "arts")) #natural_sciences=1, social_sciences=2, economics=3, health=4, 5=humanities, 6=law, 7=arts
table(METUX2018$Subject)

#--------------------------------------Creating scales and Cronbachs alpha for total sample####
library(multicon)
library(psych)

#---Interface competence
#-Recode reversed worded items
METUX2018$Interface_comp3_REV <- Recode(METUX2018$Interface_comp3_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                            , as.numeric=T) 

METUX2018$Interface_comp4_REV <- Recode(METUX2018$Interface_comp4_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                                        , as.numeric=T) 

METUX2018$Interface_comp5_REV <- Recode(METUX2018$Interface_comp5_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                                        , as.numeric=T) 

#-Creating and adding scales, Cronbachs alpha
Interface_comp_scale <- data.frame(METUX2018$Interface_comp1, METUX2018$Interface_comp2, METUX2018$Interface_comp3_REV, METUX2018$Interface_comp4_REV, METUX2018$Interface_comp5_REV)
METUX2018$Interface_competence<- Interface_comp <- composite(Interface_comp_scale, R=NULL, rel = TRUE, Zitems = F, maxScore = 5) #a=0.82 for total sample

#---Interface autonomy
#-Recode reversed worded items
METUX2018$Interface_aut3_REV <- Recode(METUX2018$Interface_aut3_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                                        , as.numeric=T) 

METUX2018$Interface_aut4_REV <- Recode(METUX2018$Interface_aut4_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                                       , as.numeric=T) 

METUX2018$Interface_aut5_REV <- Recode(METUX2018$Interface_aut5_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                                       , as.numeric=T) 

#-Creating and adding scales, Cronbachs alpha
Interface_aut_comp_scale <- data.frame(METUX2018$Interface_aut1, METUX2018$Interface_aut2, METUX2018$Interface_aut3_REV, METUX2018$Interface_aut4_REV, METUX2018$Interface_aut5_REV)
METUX2018$Interface_autonomy <- Interface_aut <- composite(Interface_aut_comp_scale, R=NULL, rel = TRUE, Zitems = F, maxScore = 5) #a=0.74 for total sample
alpha(Interface_aut_comp_scale)

#---Task competence
#-Recode reversed worded items
METUX2018$Task_comp3_REV <- Recode(METUX2018$Task_comp3_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                                        , as.numeric=T) 

METUX2018$Task_comp4_REV <- Recode(METUX2018$Task_comp4_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                                   , as.numeric=T) 

#-Creating and adding scales, Cronbachs alpha
Task_comp_scale <- data.frame(METUX2018$Task_comp1, METUX2018$Task_comp2, METUX2018$Task_comp3_REV, METUX2018$Task_comp4_REV)
METUX2018$Task_competence<- Task_comp <- composite(Task_comp_scale, R=NULL, rel = TRUE, Zitems = F, maxScore = 5) #a=0.77 for total sample

#---Task autonomy
#-Recode reversed worded items
METUX2018$Task_aut1_REV <- Recode(METUX2018$Task_aut1_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                                       , as.numeric=T) 
METUX2018$Task_aut2_REV <- Recode(METUX2018$Task_aut2_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                                  , as.numeric=T) 
METUX2018$Task_aut3_REV <- Recode(METUX2018$Task_aut3_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                                  , as.numeric=T) 
METUX2018$Task_aut4_REV <- Recode(METUX2018$Task_aut4_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                                  , as.numeric=T) 

#-Creating and adding scales, Cronbachs alpha
Task_aut_scale <- data.frame(METUX2018$Task_aut1_REV, METUX2018$Task_aut2_REV, METUX2018$Task_aut3_REV, METUX2018$Task_aut4_REV)
METUX2018$Task_autonomy<- Task_aut <- composite(Task_aut_scale, R=NULL, rel = TRUE, Zitems = F, maxScore = 5) #a=0.77 for total sample

#---Engagement
#-Recode reversed worded items
METUX2018$Enga_PU1_REV <- Recode(METUX2018$Enga_PU1_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                                  , as.numeric=T) 
METUX2018$Enga_PU2_REV <- Recode(METUX2018$Enga_PU2_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                                 , as.numeric=T) 
METUX2018$Enga_PU3_REV <- Recode(METUX2018$Enga_PU3_R, "1= 5; 2= 4; 3=3; 4=2; 5=1" 
                                 , as.numeric=T) 
#-Creating and adding scales, Cronbachs alpha

#--subscale
#FA
FA_alpha <- data.frame(METUX2018$Enga_FA1,METUX2018$Enga_FA2, METUX2018$Enga_FA3)
METUX2018$FA<- FA_scale <- composite(FA_alpha, R=NULL, rel = TRUE, Zitems = F, maxScore = 5) #a=0.88 for total sample

#PU
PU_alpha <- data.frame(METUX2018$Enga_PU1_REV, METUX2018$Enga_PU2_REV, METUX2018$Enga_PU3_REV)
METUX2018$PU<- PU_scale <- composite(PU_alpha, R=NULL, rel = TRUE, Zitems = F, maxScore = 5) #a=0.87 for total sample

#AE
AE_alpha <- data.frame(METUX2018$Enga_AE1, METUX2018$Enga_AE2, METUX2018$Enga_AE3)
METUX2018$AE<- AE_scale <- composite(AE_alpha, R=NULL, rel = TRUE, Zitems = F, maxScore = 5) #a=0.83 for total sample

#RW
RW_alpha <- data.frame(METUX2018$Enga_RW1, METUX2018$Enga_RW2, METUX2018$Enga_RW3)
METUX2018$RW<- RW_scale <- composite(RW_alpha, R=NULL, rel = TRUE, Zitems = F, maxScore = 5) #a=0.80 for total sample

#---Perceived learning
#-Recode reversed worded items
METUX2018$Perc_learn4_REV <- Recode(METUX2018$Perc_learn4_R, "1= 7; 2= 6; 3=5; 4=4; 5=3; 6=2; 7=1" 
                                  , as.numeric=T) 

#-Creating and adding scales, Cronbachs alha
Perc_learn_scale <- data.frame(METUX2018$Perc_learn1, METUX2018$Perc_learn2, METUX2018$Perc_learn3, METUX2018$Perc_learn4_REV)
METUX2018$Perceived_learning<- Perc_learn <- composite(Perc_learn_scale, R=NULL, rel = TRUE, Zitems = F, maxScore = 7) #a=0.86

#---Well-being
#-Creating and adding scales, Cronbachs alha
Flour_scale <- data.frame(METUX2018$Flourishing1, METUX2018$Flourishing2, METUX2018$Flourishing3, METUX2018$Flourishing4, METUX2018$Flourishing5,
                               METUX2018$Flourishing6, METUX2018$Flourishing7, METUX2018$Flourishing8)
METUX2018$Flourishing<- Flourishing_scale <- composite(Flour_scale, R=NULL, rel = TRUE, Zitems = F, maxScore = 7) #a=0.93

#--------------------------------------Descriptive analyses####
library(psych)
library(AutoModel)

#--Multivariate assumptions
assumption1<- run_model("Engagement", c("Interface_competence", "Interface_autonomy"), c("Task_competence", "Task_autonomy"), dataset = METUX2018, assumptions.check = TRUE)
assumption2<- run_model("Perceived_learning", c("Interface_competence", "Interface_autonomy"), c("Task_competence", "Task_autonomy"), dataset = METUX2018, assumptions.check = TRUE)
assumption3<- run_model("Flourishing", c("Interface_competence", "Interface_autonomy"), c("Task_competence", "Task_autonomy"), dataset = METUX2018, assumptions.check = TRUE)

#----Descriptives total sample
desc <- data.frame(METUX2018$Interface_competence, METUX2018$Interface_autonomy, METUX2018$Task_competence, METUX2018$Task_autonomy,
                   METUX2018$FA, METUX2018$PU, METUX2018$AE, METUX2018$RW, METUX2018$Perceived_learning, METUX2018$Flourishing)
describe(desc)
describe(METUX2018$Age)
describe(METUX2018$Study_years)

#------subsets
METUX2018$Sample
table(METUX2018$Sample)
sample_Nor<-subset(METUX2018, Sample=="1")
sample_Turk<-subset(METUX2018, Sample=="2")
str(sample_Nor)
str(sample_Turk)

#---descriptive subsets
#-Norwegian sample
desc_samplenor <- data.frame(sample_Nor$Interface_competence, sample_Nor$Interface_autonomy,sample_Nor$Task_competence, sample_Nor$Task_autonomy,
                             sample_Nor$Engagement, sample_Nor$Perceived_learning, sample_Nor$Flourishing)
describe(desc_samplenor)

describe(sample_Nor$Age)
describe(sample_Nor$Study_years)
table(sample_Nor$Gender2)

#-TurkPrime sample
desc_sampleturk <- data.frame(sample_Turk$Interface_competence, sample_Turk$Interface_autonomy, sample_Turk$Task_competence, sample_Turk$Task_autonomy,
                              sample_Turk$Engagement, sample_Turk$Perceived_learning, sample_Turk$Flourishing)
describe(desc_sampleturk)

describe(sample_Turk$Age)
describe(sample_Turk$Study_years)
table(sample_Turk$Gender2)

#--------------------------------------Preliminary analyses####

#--------Correlation for total sample 
corr_var <- data.frame(METUX2018$Interface_competence, METUX2018$Interface_autonomy, METUX2018$Task_competence, METUX2018$Task_autonomy, METUX2018$FA, METUX2018$PU, METUX2018$AE, METUX2018$RW, METUX2018$Perceived_learning, METUX2018$Flourishing)
corr.test(corr_var)


#-Create APA table
library(apaTables)
apa.cor.table(corr_var, filename = "Table2.doc", table.number = 2,
              show.conf.interval = TRUE, landscape = TRUE)

#--------------------------------------Primary analyses####
library(lavaan)
library(semPlot)
library(semTools)

#-------------CFA Total sample####
#--Specify CFA models

#-Interface competence
CFA_IntComp <- "
Int_comp =~ Interface_comp1 + Interface_comp2 + Interface_comp3_REV + Interface_comp4_REV + Interface_comp5_REV
"
fit_CFA_Int_comp <- cfa(model = CFA_IntComp, data = METUX2018) 
semPaths(fit_CFA_Int_comp, what= "std")
summary(fit_CFA_Int_comp, standardized = TRUE, estimates = TRUE, header = TRUE, ci = TRUE, fit.measures = TRUE) #Poor model fit
fitmeasures(fit_CFA_Int_comp)
modificationindices(fit_CFA_Int_comp)

#-Interface competence correlate comp1 and comp2
CFA_IntComp2 <- "
Int_comp =~ Interface_comp1 + Interface_comp2 + Interface_comp3_REV + Interface_comp4_REV + Interface_comp5_REV
Interface_comp1~~Interface_comp2
"
fit_CFA_Int_comp2 <- cfa(model = CFA_IntComp2, data = METUX2018) 
semPaths(fit_CFA_Int_comp2, what= "std")
summary(fit_CFA_Int_comp2, standardized = TRUE, estimates = TRUE, header = TRUE, ci = TRUE, fit.measures = TRUE) #Good model fit
fitmeasures(fit_CFA_Int_comp2)

#Compare model 1 and model 2
lavTestLRT(fit_CFA_Int_comp, fit_CFA_Int_comp2, type = "Chisq", model.names = NULL) 
cbind(model1=inspect(fit_CFA_Int_comp, 'fit.measures'), model2=inspect(fit_CFA_Int_comp2, 'fit.measures'))

#-Interface autonomy
CFA_IntAut <- "
Int_Aut =~ Interface_aut1 + Interface_aut2 + Interface_aut3_REV + Interface_aut4_REV + Interface_aut5_REV
"
fit_CFA_Int_aut <- cfa(model = CFA_IntAut, data = METUX2018) 
semPaths(fit_CFA_Int_aut, what= "std")
summary(fit_CFA_Int_aut, standardized = TRUE, estimates = TRUE, header = TRUE, ci = TRUE, fit.measures = TRUE) #Some model fits ok, some poor
fitmeasures(fit_CFA_Int_aut)
modificationindices(fit_CFA_Int_aut)

#-Interface autonomy deleted aut1 and aut2
CFA_IntAut2 <- "
Int_Aut =~ Interface_aut3_REV + Interface_aut4_REV + Interface_aut5_REV
"
fit_CFA_Int_aut2 <- cfa(model = CFA_IntAut2, data = METUX2018) 
semPaths(fit_CFA_Int_aut2, what= "std")
summary(fit_CFA_Int_aut2, standardized = TRUE, estimates = TRUE, header = TRUE, ci = TRUE, fit.measures = TRUE) #Good model fit
fitmeasures(fit_CFA_Int_aut2)

#Compare model 1 and model 2
lavTestLRT(fit_CFA_Int_aut, fit_CFA_Int_aut2, type = "Chisq", model.names = NULL) 
cbind(model1=inspect(fit_CFA_Int_aut, 'fit.measures'), model2=inspect(fit_CFA_Int_aut2, 'fit.measures'))

#-Task competence
CFA_TaskComp <- "
Task_comp =~ Task_comp1 + Task_comp2 + Task_comp3_REV + Task_comp4_REV
"
fit_CFA_Task_comp <- cfa(model = CFA_TaskComp, data = METUX2018) 
semPaths(fit_CFA_Task_comp, what= "std")
summary(fit_CFA_Task_comp, standardized = TRUE, estimates = TRUE, header = TRUE, ci = TRUE, fit.measures = TRUE) #Poor model fit
fitmeasures(fit_CFA_Task_comp)
modificationindices(fit_CFA_Task_comp)

#-Task competence correlate comp1 and comp2
CFA_TaskComp2 <- "
Task_comp =~ Task_comp1 + Task_comp2 + Task_comp3_REV + Task_comp4_REV
Task_comp1~~Task_comp2
"
fit_CFA_Task_comp2 <- cfa(model = CFA_TaskComp2, data = METUX2018) 
semPaths(fit_CFA_Task_comp2, what= "std")
summary(fit_CFA_Task_comp2, standardized = TRUE, estimates = TRUE, header = TRUE, ci = TRUE, fit.measures = TRUE) #Good model fit
fitmeasures(fit_CFA_Task_comp2)

#Compare model 1 and model 2
lavTestLRT(fit_CFA_Task_comp, fit_CFA_Task_comp2, type = "Chisq", model.names = NULL) 
cbind(model1=inspect(fit_CFA_Task_comp, 'fit.measures'), model2=inspect(fit_CFA_Task_comp2, 'fit.measures'))

#-Task autonomy
CFA_TaskAut <- "
Task_aut =~ Task_aut1_REV + Task_aut2_REV + Task_aut3_REV + Task_aut4_REV
"
fit_CFA_Task_aut <- cfa(model = CFA_TaskAut, data = METUX2018) 
semPaths(fit_CFA_Task_aut, what= "std")
summary(fit_CFA_Task_aut, standardized = TRUE, estimates = TRUE, header = TRUE, ci = TRUE, fit.measures = TRUE) #Acceptable model fit
modificationindices(fit_CFA_Task_aut)
fitmeasures(fit_CFA_Task_aut)

#-Perceived learning
CFA_Perc_learn <- "
Perc_learn =~ Perc_learn1 + Perc_learn2 + Perc_learn3 + Perc_learn4_REV
"
fit_CFA_Perc_learn <- cfa(model = CFA_Perc_learn, data = METUX2018) 
semPaths(fit_CFA_Perc_learn, what= "std")
summary(fit_CFA_Perc_learn, standardized = TRUE, estimates = TRUE, header = TRUE, ci = TRUE, fit.measures = TRUE) #Good model fit
fitmeasures(fit_CFA_Perc_learn)

#-Well-being
CFA_WB <- "
WB =~ Flourishing1 + Flourishing2 + Flourishing3 + Flourishing4 + Flourishing5 +
Flourishing6 + Flourishing7 + Flourishing8
"
fit_CFA_WB <- cfa(model = CFA_WB, data = METUX2018) 
semPaths(fit_CFA_WB, what= "std")
summary(fit_CFA_WB, standardized = TRUE, estimates = TRUE, header = TRUE, ci = TRUE, fit.measures = TRUE) #Acceptable model fit
fitmeasures(fit_CFA_WB)

#-Engagement

#First-order
first_order_engagement <- "
Eng_FA=~ Enga_FA1 + Enga_FA2 + Enga_FA3
Eng_PU =~Enga_PU1_REV + Enga_PU2_REV + Enga_PU3_REV
Eng_AE =~Enga_AE1 + Enga_AE2 + Enga_AE3
Eng_RW =~Enga_RW1 + Enga_RW2 + Enga_RW3
"

fit_first_order_engagement <- cfa(model = first_order_engagement, data = METUX2018)
semPaths(fit_first_order_engagement, what= "std")
summary(fit_first_order_engagement,standardize=TRUE, estimates=TRUE, header=TRUE, ci=TRUE, fit.measure=TRUE)
fitmeasures(fit_first_order_engagement)

#------------Measurement model####
measurementmod <- "
Int_comp =~ Interface_comp1 + Interface_comp2 + Interface_comp3_REV + Interface_comp4_REV + Interface_comp5_REV
Int_Aut =~ Interface_aut1 + Interface_aut2 + Interface_aut3_REV + Interface_aut4_REV + Interface_aut5_REV
Task_comp =~ Task_comp1 + Task_comp2 + Task_comp3_REV + Task_comp4_REV
Task_aut =~ Task_aut1_REV + Task_aut2_REV + Task_aut3_REV + Task_aut4_REV
Eng_FA=~ Enga_FA1 + Enga_FA2 + Enga_FA3
Eng_PU =~Enga_PU1_REV + Enga_PU2_REV + Enga_PU3_REV
Eng_AE =~Enga_AE1 + Enga_AE2 + Enga_AE3
Eng_RW =~Enga_RW1 + Enga_RW2 + Enga_RW3
Perc_learn =~ Perc_learn1 + Perc_learn2 + Perc_learn3 + Perc_learn4_REV
WB =~ Flourishing1 + Flourishing2 + Flourishing3 + Flourishing4 + Flourishing5 +
Flourishing6 + Flourishing7 + Flourishing8
"
fit_measurementmod<- cfa(model = measurementmod, data = METUX2018,missing="ml") 
summary(fit_measurementmod, standardized = TRUE, estimates = TRUE, header = TRUE, ci = TRUE, fit.measures = TRUE) #A
fitmeasures(fit_measurementmod)
modificationindices(fit_measurementmod, sort. = T)
inspect( fit_measurementmod, "rsquare" ) 
reliability(fit_measurementmod)

measurementmod1 <- "
Int_comp =~ Interface_comp1 + Interface_comp2 + Interface_comp3_REV + Interface_comp4_REV + Interface_comp5_REV
Interface_comp1~~Interface_comp2
Int_Aut =~ Interface_aut3_REV + Interface_aut4_REV + Interface_aut5_REV
Task_comp =~ Task_comp1 + Task_comp2 + Task_comp3_REV + Task_comp4_REV
Task_comp1~~Task_comp2
Task_aut =~ Task_aut1_REV + Task_aut2_REV + Task_aut3_REV + Task_aut4_REV
Perc_learn =~ Perc_learn1 + Perc_learn2 + Perc_learn3 + Perc_learn4_REV
WB =~ Flourishing1 + Flourishing2 + Flourishing3 + Flourishing4 + Flourishing5 +
Flourishing6 + Flourishing7 + Flourishing8
"
fit_measurementmod1<- cfa(model = measurementmod1, data = METUX2018,missing="ml") 
summary(fit_measurementmod1, standardized = TRUE, estimates = TRUE, header = TRUE, ci = TRUE, fit.measures = TRUE) #A
fitmeasures(fit_measurementmod1)
modificationindices(fit_measurementmod1, sort. = T)
?inspect 
inspect( fit_measurementmod1, "rsquare" ) 
reliability(fit_measurementmod1)

#------------Structural Equation Model Total sample####


#-------Model 1
sem_model1 <- "
#--Measurement models
#Interface competence
Int_comp =~ Interface_comp1 + Interface_comp2 + Interface_comp3_REV + Interface_comp4_REV + Interface_comp5_REV
Interface_comp1~~Interface_comp2

#Interface autonomy
Int_Aut =~ Interface_aut3_REV + Interface_aut4_REV + Interface_aut5_REV

#Task competence
Task_comp =~ Task_comp1 + Task_comp2 + Task_comp3_REV + Task_comp4_REV
Task_comp1~~Task_comp2

#Task autonomy
Task_aut =~ Task_aut1_REV + Task_aut2_REV + Task_aut3_REV + Task_aut4_REV

# Engagement
Eng_FA=~ Enga_FA1 + Enga_FA2 + Enga_FA3
Eng_PU =~Enga_PU1_REV + Enga_PU2_REV + Enga_PU3_REV
Eng_AE =~Enga_AE1 + Enga_AE2 + Enga_AE3 
Eng_RW =~Enga_RW1 + Enga_RW2 + Enga_RW3
Enga_RW1 ~~ Enga_RW2

# Perceived learning
Perc_learn =~ Perc_learn1 + Perc_learn2 + Perc_learn3 + Perc_learn4_REV

# Well-being
WB =~ Flourishing1 + Flourishing2 + Flourishing3 + Flourishing4 + Flourishing5 +
  Flourishing6 + Flourishing7 + Flourishing8

#--Regressions (mediators)
Task_comp ~ a*Int_comp + c*Int_Aut
Task_aut ~ b*Int_comp + d*Int_Aut

# Regressions (outcomes)
Eng_FA ~ e*Task_comp + k*Task_aut
Eng_PU ~ f*Task_comp + l*Task_aut
Eng_AE ~ g*Task_comp + m*Task_aut
Eng_RW ~ h*Task_comp + n*Task_aut
Perc_learn ~ i*Task_comp + o*Task_aut
WB ~ j*Task_comp + p*Task_aut

#Indirect effect
indirect1:= a*e
indirect2:= a*f
indirect3:= a*g
indirect4:= a*h
indirect5:= a*i
indirect6:= a*j

indirect7:= b*k
indirect8:= b*l
indirect9:= b*m
indirect10:= b*n
indirect11:= b*o
indirect12:= b*p

indirect13:= c*e
indirect14:= c*f
indirect15:= c*g
indirect16:= c*h
indirect17:= c*i
indirect18:= c*j

indirect19:= d*k
indirect20:= d*l
indirect21:= d*m
indirect22:= d*n
indirect23:= d*o
indirect24:= d*p

"

#Analyzing model and viewing visually
fitSEM_model1 <- sem(sem_model1, data = METUX2018, missing = "ML")

semPaths(fitSEM_model1, what= "std", layout = "tree3", edge.color = "black")

summary(fitSEM_model1, standardized = TRUE, estimates = TRUE, header = TRUE, ci = TRUE, fit.measures = TRUE, rsq = TRUE)
fitMeasures(fitSEM_model1)

#Checking modification indices for model improvement 
modificationindices(fitSEM_model1, sort. = TRUE)
