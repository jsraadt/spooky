##############
###WELCOME!###
##############

#######################
###R PRimer Workshop###
#######################

#######################################
###          presented by           ###
###THE OFFICE OF RESEARCH CONSULTING###
#######################################

##################################
###         hosted by          ###
###THE TOULOUSE GRADUATE SCHOOL###
##################################

#We would like to extend our thanks to:
#The Department of Educational Psychology
#Dr. Robin Henson
#The College of Education
#The Toulouse Graduate School
#Dr. Joseph Oppong
#Amber Landers
#Stacey Vesta

###################
###ABOUT THE ORC###
###################

#We are Genea and Jay
#PhD studetns
#Research Methods, Measurement, and Statistics concentration
#Department of Educational Psychology

#In the ORC, we help with all aspects of the research process:

#quantitative, qualitative, and mixed methods;
#research questions and research problems;
#theoretical frameworks and literature reviews;
#research design, sampling;
#data colelction, and data analysis;
#resutls and inferences;
#refelxivity;
#validity and trustworthiness;
#dependabiltiy and reliability; and
#legitimacy

#You can contact us:
#coe-orc@unt.edu
#940.565.4414
#Matthews Hall Rm. 313

#Schedule an appointment or walk-in
#appointmetns can be in-person, over the phone, or via video chat

#Fall 2019 Hours
#Monday: 1-7
#Tuesday: 9-5
#Wednesday: 9-5
#Thursday: 9-7
#Friday: 9-5

##################
####SPOOKY DATA###
##################
#
#    * \
#  __/__\__
#,  ((''))\V/
# \__/  \__|
#   /____\ |
#   _/  |  |
#
#This data comes from the Survey of Scottish Witchcraft (SSW) from 1563 to 1736
#By Julian Goodare, Lauren Martin, Joyce Miller and Louise Yeoman
#January 2003
browseURL('http://www.shca.ed.ac.uk/Research/witches/')

#This workshop assumes some understanding of:
#categorical data analysis and inferential statistical tests

#We are going to use two of the main files from the SSW database
#Let's start

#import a file and save it as an object
accused=read.csv(file.choose(),header=T,na.strings="")

#Look at the data
View(accused)#view the whole dataset
head(accused,10)#look at the first 10 rows of the data set
names(accused)#look at the column names of the dataset
order(names(accused_and_case))
names(accused_and_case)[order(names(accused_and_case))]
accused$Sex#look at the data in the Sex column

#you can also use matrix notation
accused[,11]
#the brackets are [row,column]

#Describe the data
install.packages("psych")#install a package
require(psych)#load the package
describe(accused)#use a function from the psych package

#what is the average age?
#...

#how many males and females?
table(accused$Sex)

#this works because accused is a data frame type object
class(accused)

#how many of each socioeconomic class?
#copy, paste, and revise the syntax function above
#...

#what proportion of the accused were midwives?
#hint: start with a table object and use the sum() function
my_occupation_table=table(accused$Occupation)

#######
#Don't peek at the solution...
#
##
###
####
#####
######
#####
####
###
##
#
##
###
####
#####
######
#####
####
###
##
#
##
###
####
#####
######
#####
####
###
##
#Or else.... mwahahahaha!
##
###
####
#####
######
#####
####
###
##
#
##
###
####
#####
######
#####
####
###
##
#
##
###
####
#####
######
#####
####
###
##
#
accused$Occupation/sum(accused$Occupation)
prop.table(accused$Occupation)
my_occupation_array=as.array(my_occupation_table)
prop.table(my_occupation_array)
#8.91%

#what proportion of the accused were female?
#copy, paste, and revise the syntax above
#...

#how many and what proportion of females were of low socieconomic class?
table(accused$Sex,accused$SocioecStatus)
prop.table(as.array(table(accused$Sex,accused$SocioecStatus)))

#recode socioeconomic class and save as an object
unique(accused$SocioecStatus)#check the unique values of the socioeconomic status column
SES_High_Class=ifelse(accused$SocioecStatus=="Middling",0,
                      ifelse(accused$SocioecStatus=="Landless",0,
                             ifelse(accused$SocioecStatus=="Lower",0,
                                    ifelse(accused$SocioecStatus=="Very Poor",0,
                                           ifelse(accused$SocioecStatus=="",NA,1)))))
#this is "dichotomizing" the data or "dummy" coding

#how many accused feamles were high class?
table(accused$Sex,SES_High_Class)

#what are the odds of being low class and female?

#save that table as an object
sex_by_class=table(accused$Sex,SES_High_Class)
sex_by_class=table(SES_High_Class,accused$Sex)

#Find the odds ratio
#OR=(a*d)/(b*c)
(sex_by_class[1,1]*sex_by_class[2,2])/(sex_by_class[2,1]*sex_by_class[1,2])
#or use a helpful package
install.packages('epitools')
require(epitools)
oddsratio(sex_by_class)
#accused individuals of low class are 2 times more likely to be female than male

#are those odds statistically significant?
#do a chi square test of independence
#Null hypothesis is independence
chisq.test(sex_by_class)
#is sex independent of class?
#...

#in other words: can the accused person's sex predict their class?
#do a logistic regression
sex_regressed_on_class=glm(SES_High_Class~accused$Sex,family='binomial')
summary(sex_regressed_on_class)

#let's get spookier

#get more data
case=read.csv(file.choose(),header=T,na.strings="")

#now, merge the datasets and save as an object
accused_and_case=merge(accused,case,'AccusedRef')

#let's get the data that we want
#spooky_data=accused_and_case[,c(1,11,24:26,32,48:75,81:82,84,86,88:94,98:107,109:125,127:128,131:135)]
spooky_data=accused_and_case[,c(1,11,50,58,120)]
View(spooky_data)
#fit a full logistic regression and individual logistic regressions

spooky_glm_full=glm(as.factor(Sex)~
                      Consulting_p+
                      Folk_healing_p+
                      Cursing,
                    data=accused_and_case,
                    family='binomial')

spooky_glm_consulting=glm(as.factor(Sex)~Consulting_p,
                          data=accused_and_case,
                          family='binomial')

spooky_glm_folk_healing=glm(as.factor(Sex)~Folk_healing_p,
                            data=accused_and_case,
                            family='binomial')

spooky_glm_cursing=glm(as.factor(Sex)~Cursing,
                       data=accused_and_case,
                       family='binomial')

#calculate the predicted fit lines with all possible data
my_n_row=nrow(accused_and_case)
my_possible_values=seq(0,1,my_n_row)

my_possible_data=data.frame(
       Consulting_p=my_possible_values,
       Folk_healing_p=my_possible_values,
       Cursing=my_possible_values)

full_glm_prediction=predict(spooky_glm_full,
                            newdata=my_possible_data)
consulting_glm_prediction=predict(spooky_glm_consulting,
                                  newdata=my_possible_data)

folk_healing_glm_prediction=predict(spooky_glm_folk_healing,
                                    newdata=my_possible_data)

cursing_glm_prediction=predict(spooky_glm_cursing,
                               newdata=my_possible_data)

#calculate fit line as a probabilities
#this is a common procedure in logistic regreesion
#probability is a logistic function of the odds

full_pr=exp(full_glm_prediction)/(1+exp(full_glm_prediction))

consulting_pr=exp(consulting_glm_prediction)/(1+exp(consulting_glm_prediction))

folk_healing_pr=exp(folk_healing_glm_prediction)/(1+exp(folk_healing_glm_prediction))

cursing_pr=exp(cursing_glm_prediction)/(1+exp(cursing_glm_prediction))

#draw a labelled empty plot with labelled axes and no ticks on the x-axis

plot(NA,xlim=c(0,1),ylim=c(0,1),
     main='Probability of Being Female Given an Accusation',
     xlab="Accused",ylab="Pr(Female)",
     xaxt='n')

#draw custom ticks on x-axis
axis(1,at=c(0,1),labels=c('No','Yes'))

#draw the predicted probability fit lines

lines(seq(0,1,length.out=length(full_pr)),full_pr,col='black')

lines(seq(0,1,length.out=length(consulting_pr)),consulting_pr,col='blue')

lines(seq(0,1,length.out=length(folk_healing_pr)),folk_healing_pr,col='green')

lines(seq(0,1,length.out=length(cursing_pr)),cursing_pr,col='red')

#add a legend to the plot

legend("topleft",legend=c('Full Model','Consulting','Healing','Cursing'),
       col=c('black','blue','green','red'), lty=1, cex=0.8)

#make the plot again, but automatically export it as a PDF
#do this by putting the code between pdf() and dev.off()

pdf()

plot(NA,xlim=c(0,1),ylim=c(0,1),
     main='Probability of Being Female Given an Accusation',
     xlab="Accused",ylab="Pr(Female)",
     xaxt='n')
axis(1,at=c(0,1),labels=c('No','Yes'))
lines(seq(0,1,length.out=length(full_pr)),full_pr,col='black')
lines(seq(0,1,length.out=length(consulting_pr)),consulting_pr,col='blue')
lines(seq(0,1,length.out=length(folk_healing_pr)),folk_healing_pr,col='green')
lines(seq(0,1,length.out=length(cursing_pr)),cursing_pr,col='red')
legend("topleft",legend=c('Full Model','Consulting','Healing','Cursing'),
       col=c('black','blue','green','red'), lty=1, cex=0.8)

dev.off()

#bonus:
my_glm_fun=function(x,y,x.name,y.name){

       my_data=data.frame(x,y)

       my_glm=glm(
              as.factor(y)~x,
              data=my_data,
              family='binomial')

       my_n_row=nrow(my_data)
       my_possible_values=seq(0,1,length.out=my_n_row)

       my_prediction=predict(
              my_glm,
              newdata=data.frame(my_possible_values))

       my_probability=exp(my_prediction)/(1+exp(my_prediction))
       
       print(my_probability)
       summary(my_glm,level=.85)

       plot(NA,xlim=c(0,1),ylim=c(0,1),
               main=paste('Probability of',y.name,'given',x.name),
              xlab=x.name,ylab=paste("Pr(",y.name,")"),
              xaxt='n')

       axis(1,at=c(0,1),labels=c('No/False','Yes/True'))

       lines(seq(0,1,length.out=length(my_probability)),my_probability,col='black')

       legend("topleft",legend=c(x.name),
       col=c('black'), lty=1, cex=0.8)
}

my_glm_fun(x=accused_and_case$Sex,y=accused_and_case$Consulting_p,"Male","Consulting")
