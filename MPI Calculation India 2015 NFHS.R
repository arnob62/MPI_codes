
#packages required data.table, dplyr, ipumsr
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
if (!require("dplyr")) stop("dplyr package is required") 
if (!require("data.table")) stop("data.table package is required") 
if (!require("expss")) stop("expss package is required") 
if (!require("ggplot2")) stop("ggplot2 package is required")

install.packages("ggplot2", dependencies = TRUE)
memory.limit(size=50000000)

#Here we will work with IPUMS data (India -2015). WE will construct Multidimensional Poverty Index

#To provide a background about Multidimensional Poverty Index( MPI):
#Monetary poverty certainly provides very useful information. Yet poor people themselves
#define their poverty much more broadly to include lack of education, health, housing,
#empowerment, employment, personal security and more. No one indicator, such as income,
#is uniquely able to capture the multiple aspects that contribute to poverty. For this reason,
#since 1997, Human Development Reports (HDRs) have measured poverty in ways different than 3
#traditional income-based measures. The Human Poverty Index (HPI) was the first such
#measure; the Multidimensional Poverty Index (MPI) succeeded it in 2010.

# This code is an extract from the full-work of MPI construction. 
# Here we will only Develop the below tow indices
#1. Years	of	Schooling(Education-1 Index) : 
# This index takes value '1' if No household	member aged	 10 years or	older has	completed	five	years	of	schooling.
#else '0'. 
# 2.Child	Mortality (Education-2 Index):
# This index takes value '1' if Any	child	has	died	in	the	household.
#else '0'.
setwd("C:/MPI_IND") #please use appropriate path
getwd()

ddi <- read_ipums_ddi("idhs_00006.xml") #Household Data IPUMS(India-2015)
data <- read_ipums_micro(ddi)

ddi_birth <- read_ipums_ddi("idhs_00010.xml") #birth data (India-2015)
data_birth <- read_ipums_micro(ddi_birth)

# NOTE: To load data, we must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).


viewdata<-data[1:100,] #To see the Household data structure 
viewdata_birth<-data_birth[1:100,]  #To see the Household data structure


#extracting ID from Household data
ID =select(data,SAMPLE,IDHSHID,HHMEMBERS, HHLINENO)
demography= select(data,IDHSHID, HHLINENO, GEO_IA1992_2015,GEO_IA2015,
                   GEOALT_IA2015,SEX,HHAGE,AADHAARCARD,HHEADSEXHH,
                   BPLCARDHH,HCHHANYVISITPLACE,INSHHANY,INSHHANYEMPLOYER,
                   INSHHANYEMPREIM,INSHHANYSOCS,INSHHANYBPL,INSHHANYCENGOVEMP,
                   INSHHANYGOVEMP,EDLEVEL,URBANHH)
demography2= select(data_birth,IDHSHID,ETHNICITYIA,RELIGION)
demography2= demography2%>% group_by(IDHSHID) %>% filter(row_number(RELIGION) == 1)

demography= merge(demography, demography2, by= "IDHSHID", all.x= T)
demography$ETHNICITYIA[is.na(demography$ETHNICITYIA)]=0
demography$RELIGION[is.na(demography$RELIGION)]=0
demography$ETHNICITYIA= as.factor(demography$ETHNICITYIA)
demography=demography %>% mutate(ETHNICITYIA=dplyr::recode(ETHNICITYIA, 
                                                           '10'=	'Scheduled caste',	
                                                           '20' =	'Scheduled tribe',
                                                       
                                                           '31'=	'Other backward caste',
                                                           '32'=	'General/Upper caste',
                                                           '97'	=  'Dont know'
                                                           ))

demography$RELIGION= as.factor(demography$RELIGION)
demography= demography %>% mutate(RELIGION= dplyr:: recode( RELIGION,
                                                            '0'=	'NO RELIGION',
                                                            '1000'=	'MUSLIM',
                                                            '2000'=	'CHRISTIAN',
                                                            '3000'=	'BUDDHIST/NEO-BUDDHIST',
                                                            '3100'=	'Buddhist',
                                                            '4000'=	'HINDU',
                                                            '5000'=	'JEWISH',
                                                            '7200'=	'Sikh',
                                                            '7300'=	'Zoroastrian',
                                                            '7400'=	'Jain',
                                                            '9000'=	'OTHER'
                                                            
  
  
))

IDunique= select(filter(ID,HHLINENO==1),SAMPLE, IDHSHID,HHMEMBERS)
HH =select(data,SAMPLE,IDHSHID,HHMEMBERS, HHLINENO, HHAGE, EDYEARS, SCHOOLNOW) #HH stands for household


##education-1

#extracting education related variables 
ED=select(data,IDHSHID,HHLINENO, LINENOHHRESP,HHMEMBERS, SEX, HHAGE ,EDLEVEL, EDLEVYR, EDYEARS, EDSUMM,SCHOOLNOW, EDLEVYRNOW, EDYEARSNOW)

ED$EDadult= ifelse(ED$HHAGE <= 10,0 , ifelse (ED$HHAGE >10 & ED$HHAGE < 97,1,99))
#marking target population - age>10 years


ED$Ad_EDyrs5=with(ED,ifelse(EDadult >= 1 & EDYEARS < 6,0, 
                            ifelse (EDadult==0,0,
                                    ifelse (EDadult >= 1 & EDYEARS >= 6 & EDYEARS < 90 ,1,
                                            99))))
# Ad_EDyrs5= 1 if individual has greater than 5 years of education.Computed for all HH-members with age >10 
# Ad_EDyrs5= 99 if individual's year of education information is missing
#else 0

#to check our result 
ED1chk=select(ED,IDHSHID, HHAGE,EDYEARS,SCHOOLNOW,EDadult,Ad_EDyrs5)
ED1chk[ED1chk$HHAGE == 97,] #HHAGE= 97 is 'Don't know age'. Finding is even if age is missing individual might have more than 5 years of education


DTEDMain= data.table(ED)
ED_= DTEDMain[,sum(Ad_EDyrs5), by = IDHSHID]  
#When Ad_EDyrs5 is either 0 or 1 for an HH, it gives us total number of adults who had more than 5 years of education.
#else deviding V1 by 99, from the remainder we get total number of adults who had more than 5 years of education.
ED_1= DTEDMain[,sum(EDadult !=0), by = IDHSHID]
ED_1$adult= ED_1$V1 #total number of adults (age>10)in a HH
ED_1= select(ED_1,IDHSHID,adult)

EDpoor1=merge(HH, ED_,by = "IDHSHID")
EDpoor1=merge(EDpoor1, ED_1,by = "IDHSHID")

EDpoor1$ED1=with(EDpoor1,ifelse(V1%%99 !=0,0, 
                                ifelse (V1%%99 ==0 & V1==0 & adult == 0 ,0,
                                        ifelse(V1%%99 ==0 & V1==0 & adult !=0, 1,
                                               ifelse (V1%%99==0 & V1!=0 & (V1/99)/adult <0.67,1
                                                       ,99)))))

EDpoor1$pchg= ((EDpoor1$adult - EDpoor1$V1%%99)/EDpoor1$adult)*100
EDpoor2=EDpoor1[EDpoor1$HHLINENO == "1",] #Compuation for 1st of 5 #result1

table1= select(EDpoor2,IDHSHID, pchg)  #result2
freq=table(table1$pchg)
print(freq)
data.table(freq)
EDpoor2[EDpoor2$pchg == "4.54545454545455",]

#marking the households as Deprived(ED1=1),Not-Deprived(ED1=0),Missing info(ED1=99)
# The condition "(V1/99)/adult <0.67" implies only if in a HH more than 2/3 adults have missing data, we mark the HH's Education-1 index as missing 

#check

EDpoor1[EDpoor1$IDHSHID == "35604 03003987",]
EDpoor1[EDpoor1$V1==297 & EDpoor1$adult > 3 ,] # to check missing value handling
EDpoor1[EDpoor1$V1==199 ,] # to check missing value handling

#plot1= table1
#plot 2 
ED$IDIndi= paste(ED$IDHSHID,ED$HHLINENO,sep="_")
EDInd=select(ED,IDIndi,SEX,HHAGE,Ad_EDyrs5) #result3
ED1_gender1 = select(ED,SEX,EDYEARS,EDadult,Ad_EDyrs5) 


Ed1_gender2= data.table(ED1_gender1)
ED1_gender3= Ed1_gender2[Ed1_gender2$EDadult == "1",]


ED1_gender4=
  ED1_gender3 %>%
  group_by(SEX, Ad_EDyrs5) %>%
  summarise(number= n())

ED1_gender5=
  ED1_gender3 %>%
  group_by(SEX) %>%
  summarise(count= n())

ED1_Gender= merge(ED1_gender4,ED1_gender5,by="SEX")
ED1_Gender$pchg= (ED1_Gender$number/ED1_Gender$count)*100 #result4(i) 



#Heath-1

#for this we will use the birth data instead of the houselhold data. 
#subsetting Child Mortality related variables
C_ALIVE=select(data_birth,IDHSHID,LINENO,KIDALIVE,KIDCURAGE,KIDLIVESWITH, AGE, BIRTHSIN5YRS,KIDBIRTHYR,KIDAGEDEATH)


#to check consistency between two IPUMS datasets Houshold and Births 
  data_birth_chk=C_ALIVE[C_ALIVE$IDHSHID == "35604 01000671",] 
  data_birth_chk_=C_ALIVE[C_ALIVE$IDHSHID == "35604 01000101",] 
  data_birth_chk2=C_ALIVE[C_ALIVE$KIDALIVE == 0,]
  data_birth_chk3=data_birth_chk2[data_birth_chk2$KIDLIVESWITH==99,] #checked with 10,20 - they gave out 0 elements indicating for dead kids 10/20 were not reported 
  data_c= select(data,LINENOHHRESP,IDHSHID,HHLINENO,SEX, HHAGE)
  data_c[data_c$IDHSHID == "35604 01000671", ] 
#end of checks 
  
Health1_1 = C_ALIVE
Health1_1$KIDAGEDEATH_= with(Health1_1,ifelse(KIDAGEDEATH<= 212,1,
                                             ifelse(KIDAGEDEATH > 212 & KIDAGEDEATH<=224,2,
                                                    ifelse(KIDAGEDEATH > 224 & KIDAGEDEATH<=236,3,
                                                           ifelse (KIDAGEDEATH > 236 & KIDAGEDEATH<=248,4,
                                                                   ifelse(KIDAGEDEATH > 248 & KIDAGEDEATH<=260,5,
                                                                          ifelse(KIDAGEDEATH >300,KIDAGEDEATH-300,
                                                                          0)))))))

  
Health1_2= Health1_1[Health1_1$KIDAGEDEATH_ !=0, ]
Health1_2$KIDBIRTHYR= as.numeric(Health1_2$KIDBIRTHYR)
Health1_2$KIDAGEDEATH_= as.numeric( Health1_2$KIDAGEDEATH_)



Health1_2$KIDDEATHYR= Health1_2$KIDBIRTHYR+ Health1_2$KIDAGEDEATH_
Health1_2$DeadYN= with(Health1_2,ifelse(KIDALIVE== 0 & KIDAGEDEATH_ <=18 & KIDDEATHYR >=2010,1,0 ))

Health1_3= Health1_2%>% group_by(IDHSHID, LINENO) %>% summarize(DeadTOT_woman= sum(DeadYN))



H_1= Health1_3%>% group_by(IDHSHID) %>% summarize(DEADTOT= sum(DeadTOT_woman)) #number of dead child in family. Here we have considered if ay dead child ever, not just in past 5 years or any such period

Hpoor1=merge(HH, H_1,by = "IDHSHID",all.x = TRUE) #HH and H_1 has unequal number of observations as not all HH has children.

index= is.na(Hpoor1)
Hpoor1[index]= 0 #if a HH doesn't have children we consider the HH is not poor wrt this index 

Hpoor1$H1= ifelse(Hpoor1$DEADTOT >=1,1,0) #result 2.1

#combining two indices 
#EDpoor1= select(filter(EDpoor1,HHLINENO==1),IDHSHID,HHMEMBERS, ED1)
#Hpoor1= select(filter(Hpoor1,HHLINENO==1),IDHSHID, H1)
Poor1=merge(EDpoor2, Hpoor1,by = "IDHSHID" , all.y= TRUE)
Poor1= apply_labels(Poor1,
                    ED1="HH has no inidividual atteding school for 5+ years",
                    H1="HH had child-death in past"
                    
)
Poor1[Poor1$IDHSHID == "35604 01000671",] # we can see both the indices

#EDucation-2
ED$SCHOOL_Child= with(ED,ifelse(HHAGE >= 6 & HHAGE <= 15 & SCHOOLNOW==0,0,
                                ifelse (HHAGE >= 6 & HHAGE <= 15 & SCHOOLNOW %in% c(10,11,12),1,
                                        ifelse (HHAGE >= 6 & HHAGE <= 15 & SCHOOLNOW %in% c(97,98,99),99,
                                                
                                                        0)))) #whether child is going to school or not 
ED$Child= with(ED,ifelse(HHAGE >= 6 & HHAGE <= 15 ,1,ifelse (HHAGE %in% c(97,98),99,
                                                             0)))
ED2= data.table(ED)

ED2_1= ED2[,sum(Child == 1), by = IDHSHID] 

ED2_2=merge(ED2_1,IDunique,by = "IDHSHID")
ED2_2$Childtot=ED2_2$V1 #total children - this information will be useful to mark HH with no child 
ED2_2= select(ED2_2,IDHSHID,Childtot)
Ed2_3= ED2[,sum(SCHOOL_Child), by = IDHSHID]

ED2_4=merge(Ed2_3, ED2_2,by = "IDHSHID")

#check
EDck0= ED2_4[ED2_4$IDHSHID=="35604 01007929",]
EDck1= ED2_4[ED2_4$V1%%99==0,] #implies no data for missing schooling information for a child. 
# here Childtot is number of children. and, V1 number of schoolgoing children. There's no missing information in this dataset. 



ED2_4$ED2=with(ED2_4,ifelse(V1 == Childtot,0, 
                            
                                    
                                    1))
ED2_4$ED2_= with(ED2_4, ifelse (Childtot==0,"No Child",ED2))


#Health2

Health2_1= select(data,IDHSHID,HHLINENO,GEO_IA2015,SEX,HHAGE,HWFBMI,HWFHEIGHT,HWCHTAPCT,HWCBMIZWHO, HWCHAZNCHS, HWCHAZWHO,HWMHEIGHT, HWFWHYNOTMEAS,HWCWEIGHT,HWMBMI,HWCWHYNOTMEAS,HWMWHYNOTMEAS,AGEMOHHLT5, HWCHEIGHT)
Health2_1$HWFBMI= with(Health2_1,ifelse(HWFBMI %in% c(9999,9998),0,HWFBMI))
Health2_1$HWMBMI= with(Health2_1,ifelse(HWMBMI %in% c(9999,9998),0,HWMBMI))
Health2_1$HWCHEIGHT= with(Health2_1,ifelse(HWCHEIGHT%in% c(9999,9998,9997,9996,9995,9994),0,HWCHEIGHT/1000))
Health2_1$HWCWEIGHT= with(Health2_1,ifelse(HWCWEIGHT%in% c(9999,9998,9997,9996,9995,9994),0,HWCWEIGHT/10))
Health2_1$HWCBMI= Health2_1$HWCWEIGHT/(Health2_1$HWCHEIGHT*Health2_1$HWCHEIGHT)
chkk4 = Health2_1[Health2_1$HHAGE== 13 & Health2_1$SEX==1,] #strange case no data for age 13 with no missing BMI
Health2_1$HWCBMI[!is.finite(Health2_1$HWCBMI)] <- 0
Health2_1$BMISEX=with(Health2_1,ifelse (HHAGE<19,3,
                                        ifelse(HHAGE>=15 & HHAGE <= 70 & SEX==2,2,
                                               ifelse(HHAGE>=15 &HHAGE<=70 & SEX==1,1,
                                                      0))))

chkk = select (Health2_1,IDHSHID, HWCHEIGHT, HWCWEIGHT, HWCBMI, HHAGE,HWCBMIZWHO,HWCHTAPCT, HWCHAZNCHS,HWCHAZWHO)
chkk2= chkk[chkk$HHAGE <= 5 & chkk$HWCBMIZWHO < 0, ]



Health2_1$BMITEST = pmax(Health2_1$HWFBMI, Health2_1$HWMBMI, Health2_1$HWCBMI)

chk = select(Health2_1, HWFBMI, HWMBMI, HWCBMIZWHO,BMITEST, HHAGE)
Health2_2= Health2_1[!(Health2_1$BMITEST==0),]

Height0to2B= read_labelled_xlsx("lhfa_boys_0_to_2_years_zscores.xlsx")
Height0to2G= read_labelled_xlsx("lhfa_girls_0_to_2_years_zscores.xlsx")
Height2to5B= read_labelled_xlsx("lhfa_boys_2_to_5_years_zscores.xlsx")
Height2to5G= read_labelled_xlsx("lhfa_girls_2_to_5_years_zscores.xlsx")
BMI0to2B= read_labelled_xlsx("bmi_boys_0_to_2_years_zscores.xlsx")
BMI0to2G= read_labelled_xlsx("bmi_girls_0_to_2_years_zscores.xlsx")
BMI2to5B= read_labelled_xlsx("bmi_boys_2_to_5_years_zscores.xlsx")
BMI2to5G= read_labelled_xlsx("bmi_girls_2_to_5_years_zscores.xlsx")
BMI5to19B= read_labelled_xlsx("bmi_boys_z_who_2007_exp.xlsx")
BMI5to19G= read_labelled_xlsx("bmi_boys_z_who_2007_exp.xlsx")
Monthrename <- function(argument1){
    argument1$AGEMOHHLT5 =  argument1$Month
    argument1$HeightStandard =  argument1$SD1neg
    argument1= select(argument1,AGEMOHHLT5,HeightStandard)
 
  return(argument1)
}
Height0to2B=Monthrename(Height0to2B)
Height0to2G=Monthrename(Height0to2G)
Height2to5B=Monthrename(Height2to5B)
Height2to5G=Monthrename(Height2to5G)
Height0to2B$SEX= 1
Height0to2G$SEX= 2
Height2to5B$SEX= 1
Height2to5G$SEX= 2

BMIrename1 <- function(argument1){
  argument1$AGEMOHHLT5 = argument1$Month
  argument1$BMIStandard = argument1$SD2neg
  
                        argument1$SEX= 1
  argument1= select(argument1,AGEMOHHLT5,BMIStandard,SEX)
  return(argument1)
  
}
BMI0to2B= BMIrename1(BMI0to2B)
BMI2to5B= BMIrename1(BMI2to5B)
BMI5to19B= BMIrename1(BMI5to19B)
BMIrename2 <- function(argument1){
  argument1$AGEMOHHLT5 = argument1$Month
  argument1$BMIStandard = argument1$SD2neg
  
  argument1$SEX= 2
  argument1= select(argument1,AGEMOHHLT5,BMIStandard,SEX)
  return(argument1)
  
}
BMI0to2G= BMIrename2(BMI0to2G)
BMI2to5G= BMIrename2(BMI2to5G)
BMI5to19G= BMIrename2(BMI5to19G)

BMI5to19B$HHAGE= with(BMI5to19B, ifelse(AGEMOHHLT5 >= 60 & AGEMOHHLT5 <= 71, 5,
                                        ifelse (AGEMOHHLT5 >= 72 & AGEMOHHLT5 <= 83 , 6,
                                                ifelse(AGEMOHHLT5 >= 84 & AGEMOHHLT5 <= 95,7,
                                                       ifelse(AGEMOHHLT5 >= 96 & AGEMOHHLT5 <= 107,8,
                                                              ifelse(AGEMOHHLT5 >= 108 & AGEMOHHLT5 <= 119,9,
                                                                     ifelse(AGEMOHHLT5 >= 120 & AGEMOHHLT5 <= 131,10,
                                                                            ifelse(AGEMOHHLT5 >= 132 & AGEMOHHLT5 <= 143,11,
                                                                                   ifelse(AGEMOHHLT5 >= 144 & AGEMOHHLT5 <= 155,12,
                                                                                          ifelse(AGEMOHHLT5 >= 156 & AGEMOHHLT5 <= 167,13,
                                                                                                 ifelse(AGEMOHHLT5 >= 168 & AGEMOHHLT5 <= 179,14,
                                                                                                        ifelse(AGEMOHHLT5 >= 180 & AGEMOHHLT5 <= 192,15,
                                                                                                               ifelse(AGEMOHHLT5 >= 192 & AGEMOHHLT5 <= 203,16,
                                                                                                                       ifelse(AGEMOHHLT5 >= 204 & AGEMOHHLT5 <= 215,17,
                                                                                                                             ifelse(AGEMOHHLT5 >= 216 & AGEMOHHLT5 <= 228,18,99)))))))))))))))


BMI5to19G$HHAGE= with(BMI5to19G, ifelse(AGEMOHHLT5 >= 60 & AGEMOHHLT5 <= 71, 5,
                                        ifelse (AGEMOHHLT5 >= 72 & AGEMOHHLT5 <= 83 , 6,
                                                ifelse(AGEMOHHLT5 >= 84 & AGEMOHHLT5 <= 95,7,
                                                       ifelse(AGEMOHHLT5 >= 96 & AGEMOHHLT5 <= 107,8,
                                                              ifelse(AGEMOHHLT5 >= 108 & AGEMOHHLT5 <= 119,9,
                                                                     ifelse(AGEMOHHLT5 >= 120 & AGEMOHHLT5 <= 131,10,
                                                                            ifelse(AGEMOHHLT5 >= 132 & AGEMOHHLT5 <= 143,11,
                                                                                   ifelse(AGEMOHHLT5 >= 144 & AGEMOHHLT5 <= 155,12,
                                                                                          ifelse(AGEMOHHLT5 >= 156 & AGEMOHHLT5 <= 167,13,
                                                                                                 ifelse(AGEMOHHLT5 >= 168 & AGEMOHHLT5 <= 179,14,
                                                                                                        ifelse(AGEMOHHLT5 >= 180 & AGEMOHHLT5 <= 192,15,
                                                                                                               ifelse(AGEMOHHLT5 >= 192 & AGEMOHHLT5 <= 203,16,
                                                                                                                      ifelse(AGEMOHHLT5 >= 204 & AGEMOHHLT5 <= 215,17,
                                                                                                                             ifelse(AGEMOHHLT5 >= 216 & AGEMOHHLT5 <= 228,18,99)))))))))))))))                      


BMI0TO5 <- bind_rows(BMI0to2B,BMI0to2G,BMI2to5B,BMI2to5G)
BMI0TO5= BMI0TO5%>% group_by(SEX, AGEMOHHLT5) %>% filter(row_number(BMIStandard) == 1)
Health2_21 = merge(Health2_2,BMI0TO5, by = c("AGEMOHHLT5", "SEX"), all.x = TRUE)
BMI5TO19 <- bind_rows(BMI5to19G,BMI5to19B)
BMI5TO19= BMI5TO19%>% group_by(SEX, HHAGE) %>% filter(row_number(BMIStandard) == 1)
BMI5TO19$BMIStandard= BMI5TO19$BMIStandard*100
Health2_22 = merge(Health2_21,BMI5TO19, by = c("HHAGE", "SEX"), all.x = TRUE)
Health2_22$BMIStandardAdult = with(Health2_22, ifelse(BMISEX <3 , 1850,0))
Health2_22$BMIStandard.x[is.na(Health2_22$BMIStandard.x)]=0
Health2_22$BMIStandard.y[is.na(Health2_22$BMIStandard.y)]=0
Health2_22$BMISTANDARD = pmax(Health2_22$BMIStandard.x, Health2_22$BMIStandard.y, Health2_22$BMIStandardAdult)
Health2_22$AGEMOHHLT5= Health2_22$AGEMOHHLT5.x
Height0TO5 <- bind_rows(Height0to2B,Height0to2G,Height2to5B,Height2to5G)
Height0TO5= Height0TO5%>% group_by(SEX, AGEMOHHLT5) %>% filter(row_number(HeightStandard) == 1)
Height0TO5$HeightStandard = Height0TO5$HeightStandard/100
Health2_23= merge(Health2_22,Height0TO5,by= c("AGEMOHHLT5","SEX"),all.x= TRUE)
Health2_23$HeightStandard[is.na(Health2_23$HeightStandard)]=0

chkii= Health2_23[Health2_23$HHAGE > 5,  ]
chkii= Health2_22[Health2_22$BMISEX == 3 & Health2_22$SEX == 2,  ]


chki= Health2_2[Health2_2$BMISEX < 3 & Health2_2$BMITEST <= 1850,]
Health2_23$BMISTANDARD= with(Health2_23, ifelse(HHAGE== 5, BMIStandard.x,BMISTANDARD))

Health2_23$H2_1=with(Health2_23,ifelse (AGEMOHHLT5 <= 59 & SEX==1 & HWCHEIGHT <= HeightStandard ,1,
                               ifelse (AGEMOHHLT5 <= 59 & SEX==2 & HWCHEIGHT <= HeightStandard ,1,
                               ifelse ( BMITEST <= BMISTANDARD ,1,
                                        0))))
                             
Health2_3= data.table(Health2_23)
Health2_4= Health2_3[,sum(H2_1), by = IDHSHID]
Health2_4$H2= with(Health2_4, ifelse(V1==0,0,1))



#wealth
wealth1=select(data,IDHSHID, HHLINENO,ELECTRCHH,TOILETSHAREYN,TOILETTYPE,PC, DRAWNCART, ROOF, WALL, TIMETOWTRHH,DRINKWTR,FLOOR,COOKFUEL,MOTORCYCLHH,FRIDGEHH,RADIOHH,BWTV,COLORTV,HHPHONEHH,MOBPHONE,CARHH, PC)

wealth1$W1_1= with(wealth1,ifelse(ELECTRCHH==1,0,1))
wealth1$W1_2= with(wealth1,ifelse(TOILETSHAREYN==1,1,
                                  ifelse (!(TOILETTYPE %in% c(1210,1250,3400)),1,
                                          0)))

wealth1$W1_3= with(wealth1,ifelse(  !(DRINKWTR %in% c(2100, 2110,2111,2112,3120,3200,4000,6000)),0,1))
wealth1$w1_3 = with(wealth1, ifelse(TIMETOWTRHH ==995,0,W1_3))
wealth1$w1_3 = with(wealth1, ifelse(TIMETOWTRHH <= 30 ,0,W1_3))
  
wealth1$W1_4= with(wealth1,ifelse(FLOOR %in% c(113,114,121,400)  ,1,0))
wealth1$W1_4= with(wealth1,ifelse(ROOF <= 300 ,1,W1_4))
wealth1$W1_4= with(wealth1,ifelse(WALL <= 300 ,1,W1_4))


wealth1$W1_5= with(wealth1,ifelse(COOKFUEL >= 500,1,0))
wealth1$possesion= wealth1$BWTV+ wealth1$COLORTV+ wealth1$RADIOHH + wealth1$MOTORCYCLHH+wealth1$FRIDGEHH + wealth1$CARHH+ wealth1$HHPHONEHH+ wealth1$MOBPHONE + wealth1$PC + wealth1$DRAWNCART
wealth1$w1_6= with(wealth1,ifelse(possesion >= 2,0,1))


# final datasets :

#Wealth1(w1-6)
# Hpoor1 
#EDpoor2
# poor1 combines the above 2
#Health2_4
#ED2_4
ED1H1HH= select(filter(Poor1,HHLINENO.y==1),IDHSHID,HHMEMBERS.x, H1,ED1)

#check = select(filter(wealth1, TIMETOWTRHH == 998 & DRINKWTR==9998), IDHSHID,TIMETOWTRHH)
wealthHH = select(filter(wealth1,HHLINENO== 1), IDHSHID,W1_1,W1_2,W1_3,W1_4,W1_5,w1_6)
H2HH= select(Health2_4, IDHSHID,H2)
ED2HH= select(ED2_4, IDHSHID,ED2,ED2_)
MPI1=merge(ED1H1HH,wealthHH,by = "IDHSHID" , all.y= TRUE)
MPI2=merge(MPI1,H2HH,by = "IDHSHID" , all.x= TRUE)
MPI3=merge(MPI2,ED2HH,by = "IDHSHID" , all.x= TRUE)
Full_analysis1= merge(MPI3,demography,by= "IDHSHID")

chki_t= MPI3[MPI3$ED2 == 99 ,]
chkchk= chki_t[chki_t$ED2_ != "No Child",]
chk23= chkchk[chkchk$ED2== 99,] #0 obs implies There is no HH with both ED1 and ED2 missing

MPI4= MPI3
MPI4$ED2= ifelse(MPI4$ED2_ == "No Child",0,MPI4$ED2)

#check
CK = MPI4[MPI4$ED1== 99, ]
CHKh= ED[ED$IDHSHID== "35604 14026025",]
#check

MPI4$ED2= ifelse (MPI4$ED1== 99 ,MPI4$ED2/3,MPI4$ED2) #if ED1 missing then ED2 = ED/3

MPI4$ED1= ifelse (MPI4$ED2== 99 ,MPI4$ED1/3,MPI4$ED1) #if ED2 missing then ED1 = ED1/3

MPI4$ED1= ifelse(MPI4$ED1 !=99 & MPI4$ED2 != 99 , MPI4$ED1/6, MPI4$ED1) # else 1/6 weight - ED1*1/6
MPI4$ED2= ifelse(MPI4$ED1 !=99 & MPI4$ED2 != 99 , MPI4$ED2/6, MPI4$ED2) # ED2*1/6
MPI4$H1=ifelse(is.na(MPI4$H2)== "TRUE" , MPI4$H1/3,MPI4$H1/6)
MPI4$H2=ifelse(is.na(MPI4$H2)== "TRUE" , MPI4$H2,MPI4$H2/6)
MPI4$W1_1= MPI4$W1_1/18
MPI4$W1_2= MPI4$W1_2/18
MPI4$W1_3= MPI4$W1_3/18
MPI4$W1_4= MPI4$W1_4/18
MPI4$W1_5= MPI4$W1_5/18
MPI4$w1_6= MPI4$w1_6/18

MPI5= MPI4

MPI5$Missing1 =  rowSums(MPI5 == 99)

#check 
ckk= MPI5[MPI5$IDHSHID== "35604 14026025",]
#check

MPI5$Missing1[is.na(MPI5$Missing1)]=0 #because Missing1 is not meant to count how many na. 
MPI5$Missing2= rowSums(is.na(MPI5))  # For na-s we have missing2 
MPI5$Missing= MPI5$Missing1+MPI5$Missing2 #Total missing parameters 
MPI5[MPI5== 99]=0 
MPI5[is.na(MPI5)]=0 #Now we need to just assign them 0. because we will add them. 

MPI5$Multiplier = 10- MPI5$Missing # No of non missing Parameters. We don't always multiply by 10. 
MPI5$total= MPI5$Multiplier*(MPI5$ED1+MPI5$ED2+ MPI5$H1+ MPI5$H2+ MPI5$W1_1+MPI5$W1_2+MPI5$W1_3+MPI5$W1_4+MPI5$W1_5+MPI5$w1_6)
MPI5$Poormember= ifelse(MPI5$total<=3 ,0,MPI5$HHMEMBERS.x) # This column is 0 for non deprived HH. 
MPI5$Poor= ifelse(MPI5$total<=3 ,0,1)


n= sum(MPI5$Poormember)
N= sum(MPI5$HHMEMBERS.x)
H= n/N
MPI5$A_numerator = (MPI5$Poormember*MPI5$total)/MPI5$Multiplier
A_numerator_sum = sum(MPI5$A_numerator)
A= A_numerator_sum/n
MPI_index_India= H*A



