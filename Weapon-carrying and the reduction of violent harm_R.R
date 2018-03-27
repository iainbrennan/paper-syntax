# This is code to replicate the analyses and figures from my paper, "Weapon-carrying and the reduction of violent harm"
# Code developed by Iain Brennan, University of Hull, i.brennan@hull.ac.uk

# The analysis consists of four data sets from the Offending, Crime and Justice
# Survey that ran in England and Wales between 2004 and 2006. The data used are available for free from UK Data 
# Service (https://www.ukdataservice.ac.uk/). The UK Data Service data set codes for survey waves 2004 to 2006 are 
# SN5374; SN5601 and SN6000, respectively.

# Apologies for the clumsiness of this code: it is my first use of R.

## Combine four years of OCJS data
OCJS2004 <- read.table("~/SN5374.tab", sep="\t", header=TRUE)
OCJS2005 <- read.table("~/SN5601.tab", sep="\t", header=TRUE)
OCJS2006 <- read.table("~/SN6000.tab", sep="\t", header=TRUE)

## Create new variable 'sweep' that corresponds to the survey wave
OCJS2004$sweep<-2004
OCJS2005$sweep<-2005
OCJS2006$sweep<-2006

#Disable scientific notation (1.05e10)
options(scipen=999)

## Cleaning and creating dependent variable
OCJS2004$b2knib[OCJS2004$b2knib==2]<-0
OCJS2004$b2knib[OCJS2004$b2knib==3]<-NA
OCJS2004$b2knib[OCJS2004$b2knib==4]<-NA

OCJS2004$b2gunb[OCJS2004$b2gunb==2]<-0
OCJS2004$b2gunb[OCJS2004$b2gunb==3]<-NA
OCJS2004$b2gunb[OCJS2004$b2gunb==4]<-NA

OCJS2004$b1wepb<-NA
OCJS2004$b1wepb[OCJS2004$b2knib==1 | OCJS2004$b2gunb==1]<-1
OCJS2004$b1wepb[OCJS2004$b2knib==0 & OCJS2004$b2gunb==0]<-0

OCJS2005$b2knib[OCJS2005$b2knib==2]<-0
OCJS2005$b2knib[OCJS2005$b2knib==-8]<-NA
OCJS2005$b2knib[OCJS2005$b2knib==-9]<-NA

OCJS2005$b2gunb[OCJS2005$b2gunb==2]<-0
OCJS2005$b2gunb[OCJS2005$b2gunb==-8]<-NA
OCJS2005$b2gunb[OCJS2005$b2gunb==-9]<-NA

OCJS2005$b1wepb<-NA
OCJS2005$b1wepb[OCJS2005$b2knib==1 | OCJS2005$b2gunb==1]<-1
OCJS2005$b1wepb[OCJS2005$b2knib==0 & OCJS2005$b2gunb==0]<-0

OCJS2006$b2knib[OCJS2006$b2knib==2]<-0
OCJS2006$b2knib[OCJS2006$b2knib==-8]<-NA
OCJS2006$b2knib[OCJS2006$b2knib==-9]<-NA

OCJS2006$b2gunb[OCJS2006$b2gunb==2]<-0
OCJS2006$b2gunb[OCJS2006$b2gunb==-8]<-NA
OCJS2006$b2gunb[OCJS2006$b2gunb==-9]<-NA

OCJS2006$b1wepb<-NA
OCJS2006$b1wepb[OCJS2006$b2knib==1 | OCJS2006$b2gunb==1]<-1
OCJS2006$b1wepb[OCJS2006$b2knib==0 & OCJS2006$b2gunb==0]<-0

weaponuse<-c(OCJS2004$b1wepb, OCJS2005$b1wepb, OCJS2006$b1wepb)

OCJS2004$b1wepb<-factor(OCJS2004$b1wepb, levels = c(0,1), labels = c("No", "Yes"))
OCJS2005$b1wepb<-factor(OCJS2005$b1wepb, levels = c(0,1), labels = c("No", "Yes"))
OCJS2006$b1wepb<-factor(OCJS2006$b1wepb, levels = c(0,1), labels = c("No", "Yes"))


## Cleaning variables (by wave)

# 2004 

OCJS2004$b1wepb[OCJS2004$b1wepb==-9]<- NA
OCJS2004$h1fren[OCJS2004$h1fren==-9]<- NA
OCJS2004$resptype[OCJS2004$resptype==8 | OCJS2004$resptype==9]<-NA

OCJS2004$h1fren[OCJS2004$h1fren==-9]<- NA
OCJS2004$g1area[OCJS2004$g1area==-8]<- NA
OCJS2004$g1arec[OCJS2004$g1arec==-8]<- NA
OCJS2004$g1ared[OCJS2004$g1ared==-8]<- NA
OCJS2004$g1aree[OCJS2004$g1aree==-8]<- NA
OCJS2004$g1safe[OCJS2004$g1safe==-8]<- NA
OCJS2004$b2knib[OCJS2004$b2knib==-9]<- NA
OCJS2004$b2knic[OCJS2004$b2knic==-9]<- NA
OCJS2004$b2gunb[OCJS2004$b2gunb==-9]<- NA
OCJS2004$b2gunc[OCJS2004$b2gunc==-9]<- NA
OCJS2004$g2trpol[OCJS2004$g2trpol==-8]<- NA

OCJS2004$h1fren[OCJS2004$h1fren==-9 | OCJS2004$h1fren==-8]<- NA
OCJS2004$h1fren[OCJS2004$h1fren==-9 | OCJS2004$h1fren==-8]<- NA
OCJS2004$b2knib[OCJS2004$b2knib==-9 | OCJS2004$b2knib==-8]<- NA
OCJS2004$b2knic[OCJS2004$b2knic==-9 | OCJS2004$b2knic==-8]<- NA
OCJS2004$b2knic[OCJS2004$b2knic==-9 | OCJS2004$b2knic==-8]<- NA
OCJS2004$b2gunb[OCJS2004$b2gunb==-9 | OCJS2004$b2gunb==-8]<- NA
OCJS2004$b2gunc[OCJS2004$b2gunc==-9 | OCJS2004$b2gunc==-8]<- NA 

OCJS2004$h1fren[OCJS2004$h1fren==6]<- NA
OCJS2004$h1fren[OCJS2004$h1fren==7]<- NA

OCJS2004$v1for[OCJS2004$v1for==-8]<-NA
OCJS2004$v1for[OCJS2004$v1for==3]<-NA
OCJS2004$v1for[OCJS2004$v1for==4]<-NA
OCJS2004$v1thre[OCJS2004$v1thre==3]<-NA
OCJS2004$v1thre[OCJS2004$v1thre==4]<-NA
OCJS2004$v1thre[OCJS2004$v1thre==-8]<-NA

OCJS2004$v1for[OCJS2004$v1for==2]<-0
OCJS2004$v1thre[OCJS2004$v1thre==2]<-0
OCJS2004$h1fren[OCJS2004$h1fren==5]<-4


# 2005 
OCJS2005$b1wepb[OCJS2005$b1wepb==-9]<- NA
OCJS2005$h1fren[OCJS2005$h1fren==-9]<- NA
OCJS2005$h1fren[OCJS2005$h1fren==-9]<- NA
OCJS2005$g1area[OCJS2005$g1area==-8]<- NA
OCJS2005$g1areb[OCJS2005$g1areb==-8]<- NA
OCJS2005$g1arec[OCJS2005$g1arec==-8]<- NA
OCJS2005$g1ared[OCJS2005$g1ared==-8]<- NA
OCJS2005$g1aree[OCJS2005$g1aree==-8]<- NA
OCJS2005$g1safe[OCJS2005$g1safe==-8]<- NA
OCJS2005$b2knib[OCJS2005$b2knib==-9]<- NA
OCJS2005$b2knic[OCJS2005$b2knic==-9]<- NA
OCJS2005$b2gunb[OCJS2005$b2gunb==-9]<- NA
OCJS2005$b2gunc[OCJS2005$b2gunc==-9]<- NA
OCJS2005$g2trpol[OCJS2005$g2trpol==-8]<- NA
OCJS2005$resptype[OCJS2005$resptype==8 | OCJS2005$resptype==9]<-NA

OCJS2005$h1fren[OCJS2005$h1fren==-9 | OCJS2005$h1fren==-8]<- NA
OCJS2005$b2knib[OCJS2005$b2knib==-9 | OCJS2005$b2knib==-8]<- NA
OCJS2005$b2knic[OCJS2005$b2knic==-9 | OCJS2005$b2knic==-8]<- NA
OCJS2005$b2knic[OCJS2005$b2knic==-9 | OCJS2005$b2knic==-8]<- NA
OCJS2005$b2gunb[OCJS2005$b2gunb==-9 | OCJS2005$b2gunb==-8]<- NA
OCJS2005$b2gunc[OCJS2005$b2gunc==-9 | OCJS2005$b2gunc==-8]<- NA

OCJS2005$h1fren[OCJS2005$h1fren==6]<- NA
OCJS2005$h1fren[OCJS2005$h1fren==7]<- NA

OCJS2005$v1for[OCJS2005$v1for==3]<-NA
OCJS2005$v1for[OCJS2005$v1for==4]<-NA
OCJS2005$v1for[OCJS2005$v1for==-8]<-NA
OCJS2005$v1thre[OCJS2005$v1thre==3]<-NA
OCJS2005$v1thre[OCJS2005$v1thre==4]<-NA
OCJS2005$v1thre[OCJS2005$v1thre==-8]<-NA

OCJS2005$v1for[OCJS2005$v1for==2]<-0
OCJS2005$v1thre[OCJS2005$v1thre==2]<-0
OCJS2005$h1fren[OCJS2005$h1fren==5]<-4

# 2006
OCJS2006$b1wepb[OCJS2006$b1wepb==-9]<- NA
OCJS2006$h1fren[OCJS2006$h1fren==-9]<- NA

OCJS2006$h1fren[OCJS2006$h1fren==-9]<- NA
OCJS2006$g1area[OCJS2006$g1area==-8]<- NA
OCJS2006$g1areb[OCJS2006$g1areb==-8]<- NA
OCJS2006$g1arec[OCJS2006$g1arec==-8]<- NA
OCJS2006$g1ared[OCJS2006$g1ared==-8]<- NA
OCJS2006$g1aree[OCJS2006$g1aree==-8]<- NA
OCJS2006$g1safe[OCJS2006$g1safe==-8]<- NA
OCJS2006$b2knib[OCJS2006$b2knib==-9]<- NA
OCJS2006$b2knic[OCJS2006$b2knic==-9]<- NA
OCJS2006$b2gunb[OCJS2006$b2gunb==-9]<- NA
OCJS2006$b2gunc[OCJS2006$b2gunc==-9]<- NA
OCJS2006$g2trpol[OCJS2006$g2trpol==-8]<- NA
OCJS2006$resptype[OCJS2006$resptype==8 | OCJS2006$resptype==9]<-NA

OCJS2006$h1fren[OCJS2006$h1fren==-9 | OCJS2006$h1fren==-8]<- NA
OCJS2006$h1fren[OCJS2006$h1fren==-9 | OCJS2006$h1fren==-8]<- NA
OCJS2006$b2knib[OCJS2006$b2knib==-9 | OCJS2006$b2knib==-8]<- NA
OCJS2006$b2knic[OCJS2006$b2knic==-9 | OCJS2006$b2knic==-8]<- NA
OCJS2006$b2knic[OCJS2006$b2knic==-9 | OCJS2006$b2knic==-8]<- NA
OCJS2006$b2gunb[OCJS2006$b2gunb==-9 | OCJS2006$b2gunb==-8]<- NA
OCJS2006$b2gunc[OCJS2006$b2gunc==-9 | OCJS2006$b2gunc==-8]<- NA
OCJS2006$h1fren[OCJS2006$h1fren==6]<- NA
OCJS2006$h1fren[OCJS2006$h1fren==7]<- NA

OCJS2006$v1for[OCJS2006$v1for==3]<-NA
OCJS2006$v1for[OCJS2006$v1for==4]<-NA
OCJS2006$v1for[OCJS2006$v1for==-8]<-NA
OCJS2006$v1thre[OCJS2006$v1thre==3]<-NA
OCJS2006$v1thre[OCJS2006$v1thre==4]<-NA
OCJS2006$v1thre[OCJS2006$v1thre==-8]<-NA

OCJS2006$v1for[OCJS2006$v1for==2]<-0
OCJS2006$v1thre[OCJS2006$v1thre==2]<-0
OCJS2006$h1fren[OCJS2006$h1fren==5]<-4

## Retaining variables
# 2004

O2004<-data.frame(OCJS2004$sweep, OCJS2004$caseref, OCJS2004$b1wepb, OCJS2004$respage, OCJS2004$respsex, OCJS2004$ethnic4,  
                  OCJS2004$YAnyDrug, OCJS2004$h1fren, OCJS2004$g1area, OCJS2004$g1areb, OCJS2004$g1arec, OCJS2004$g1ared,
                  OCJS2004$g1aree, OCJS2004$g1safe, OCJS2004$g1dist01, OCJS2004$g1dist02, OCJS2004$g1dist03, OCJS2004$g1dist04, OCJS2004$g1dist05,
                  OCJS2004$g1dist06, OCJS2004$g1dist07, OCJS2004$g1dist08, OCJS2004$g2trpol, OCJS2004$v1for, OCJS2004$v1thre, 
                  OCJS2004$resptype, OCJS2004$pfa)

# 2005
O2005<-data.frame(OCJS2005$sweep, OCJS2005$caseref, OCJS2005$b1wepb, OCJS2005$respage, OCJS2005$respsex, OCJS2005$ethnic4,
                  OCJS2005$YAnyDrug, OCJS2005$h1fren, OCJS2005$g1area, OCJS2005$g1areb, OCJS2005$g1arec, OCJS2005$g1ared,
                  OCJS2005$g1aree, OCJS2005$g1safe, OCJS2005$g1dist01, OCJS2005$g1dist02, OCJS2005$g1dist03, OCJS2005$g1dist04, OCJS2005$g1dist05,
                  OCJS2005$g1dist06, OCJS2005$g1dist07, OCJS2005$g1dist08, OCJS2005$g2trpol, OCJS2005$v1for, OCJS2005$v1thre, 
                  OCJS2005$resptype, OCJS2005$pfa)

# 2006
O2006<-data.frame(OCJS2006$sweep, OCJS2006$caseref, OCJS2006$b1wepb, OCJS2006$respage, OCJS2006$respsex, OCJS2006$ethnic4, 
                  OCJS2006$YAnyDrug, OCJS2006$h1fren, OCJS2006$g1todo, OCJS2006$g1area, OCJS2006$g1areb, OCJS2006$g1arec, OCJS2006$g1ared,
                  OCJS2006$g1aree, OCJS2006$g1safe, OCJS2006$g1dist01, OCJS2006$g1dist02, OCJS2006$g1dist03, OCJS2006$g1dist04, OCJS2006$g1dist05,
                  OCJS2006$g1dist06, OCJS2006$g1dist07, OCJS2006$g1dist08, OCJS2006$g2trpol, OCJS2006$v1for, OCJS2006$v1thre, 
                  OCJS2006$resptype, OCJS2006$pfa)


## Generate consistent variable names across waves prior to merging waves


# 2004
colnames(O2004)[colnames(O2004)=="OCJS2004.sweep"]<- "sweep"
colnames(O2004)[colnames(O2004)=="OCJS2004.caseref"]<- "caseref"
colnames(O2004)[colnames(O2004)=="OCJS2004.b1wepb"]<- "b1wepb"
colnames(O2004)[colnames(O2004)=="OCJS2004.respage"]<- "respage"
colnames(O2004)[colnames(O2004)=="OCJS2004.respsex"]<- "respsex"
colnames(O2004)[colnames(O2004)=="OCJS2004.ethnic4"]<- "ethnic4"
colnames(O2004)[colnames(O2004)=="OCJS2004.edqual"]<- "edqual"
colnames(O2004)[colnames(O2004)=="OCJS2004.marital"]<- "marital"
colnames(O2004)[colnames(O2004)=="OCJS2004.YAnyDrug"]<- "YAnyDrug"
colnames(O2004)[colnames(O2004)=="OCJS2004.h1fren"]<- "h1fren"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1todo"]<- "g1todo"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1area"]<- "g1area"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1areb"]<- "g1areb"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1arec"]<- "g1arec"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1ared"]<- "g1ared"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1aree"]<- "g1aree"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1safe"]<- "g1safe"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1retu"]<- "g1retu"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1dist01"]<- "g1dist01"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1dist02"]<- "g1dist02"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1dist03"]<- "g1dist03"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1dist04"]<- "g1dist04"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1dist05"]<- "g1dist05"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1dist06"]<- "g1dist06"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1dist07"]<- "g1dist07"
colnames(O2004)[colnames(O2004)=="OCJS2004.g1dist08"]<- "g1dist08"
colnames(O2004)[colnames(O2004)=="OCJS2004.g2trpol"]<- "g2trpol"
colnames(O2004)[colnames(O2004)=="OCJS2004.caseref"]<- "caseref"
colnames(O2004)[colnames(O2004)=="OCJS2004.v1for"]<-"v1for"
colnames(O2004)[colnames(O2004)=="OCJS2004.v1thre"]<-"v1thre"
colnames(O2004)[colnames(O2004)=="OCJS2004.resptype"]<-"resptype"

# 2005
colnames(O2005)[colnames(O2005)=="OCJS2005.sweep"]<- "sweep"
colnames(O2005)[colnames(O2005)=="OCJS2005.caseref"]<- "caseref"
colnames(O2005)[colnames(O2005)=="OCJS2005.b1wepb"]<- "b1wepb"
colnames(O2005)[colnames(O2005)=="OCJS2005.respage"]<- "respage"
colnames(O2005)[colnames(O2005)=="OCJS2005.respsex"]<- "respsex"
colnames(O2005)[colnames(O2005)=="OCJS2005.ethnic4"]<- "ethnic4"
colnames(O2005)[colnames(O2005)=="OCJS2005.YAnyDrug"]<- "YAnyDrug"
colnames(O2005)[colnames(O2005)=="OCJS2005.h1fren"]<- "h1fren"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1todo"]<- "g1todo"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1area"]<- "g1area"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1areb"]<- "g1areb"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1arec"]<- "g1arec"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1ared"]<- "g1ared"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1aree"]<- "g1aree"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1safe"]<- "g1safe"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1retu"]<- "g1retu"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1dist01"]<- "g1dist01"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1dist02"]<- "g1dist02"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1dist03"]<- "g1dist03"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1dist04"]<- "g1dist04"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1dist05"]<- "g1dist05"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1dist06"]<- "g1dist06"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1dist07"]<- "g1dist07"
colnames(O2005)[colnames(O2005)=="OCJS2005.g1dist08"]<- "g1dist08"
colnames(O2005)[colnames(O2005)=="OCJS2005.g2trpol"]<- "g2trpol"
colnames(O2005)[colnames(O2005)=="OCJS2005.caseref"]<- "caseref"
colnames(O2005)[colnames(O2005)=="OCJS2005.v1for"]<-"v1for"
colnames(O2005)[colnames(O2005)=="OCJS2005.v1thre"]<-"v1thre"
colnames(O2005)[colnames(O2005)=="OCJS2005.resptype"]<-"resptype"
colnames(O2005)[colnames(O2005)=="OCJS2005.pfa"]<-"pfa"

# 2006
colnames(O2006)[colnames(O2006)=="OCJS2006.sweep"]<- "sweep"
colnames(O2006)[colnames(O2006)=="OCJS2006.caseref"]<- "caseref"
colnames(O2006)[colnames(O2006)=="OCJS2006.b1wepb"]<- "b1wepb"
colnames(O2006)[colnames(O2006)=="OCJS2006.respage"]<- "respage"
colnames(O2006)[colnames(O2006)=="OCJS2006.respsex"]<- "respsex"
colnames(O2006)[colnames(O2006)=="OCJS2006.ethnic4"]<- "ethnic4"
colnames(O2006)[colnames(O2006)=="OCJS2006.YAnyDrug"]<- "YAnyDrug"
colnames(O2006)[colnames(O2006)=="OCJS2006.h1fren"]<- "h1fren"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1todo"]<- "g1todo"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1area"]<- "g1area"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1areb"]<- "g1areb"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1arec"]<- "g1arec"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1ared"]<- "g1ared"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1aree"]<- "g1aree"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1safe"]<- "g1safe"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1retu"]<- "g1retu"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1dist01"]<- "g1dist01"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1dist02"]<- "g1dist02"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1dist03"]<- "g1dist03"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1dist04"]<- "g1dist04"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1dist05"]<- "g1dist05"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1dist06"]<- "g1dist06"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1dist07"]<- "g1dist07"
colnames(O2006)[colnames(O2006)=="OCJS2006.g1dist08"]<- "g1dist08"
colnames(O2006)[colnames(O2006)=="OCJS2006.g2trpol"]<- "g2trpol"
colnames(O2006)[colnames(O2006)=="OCJS2006.caseref"]<- "caseref"
colnames(O2006)[colnames(O2006)=="OCJS2006.v1for"]<-"v1for"
colnames(O2006)[colnames(O2006)=="OCJS2006.v1thre"]<-"v1thre"
colnames(O2006)[colnames(O2006)=="OCJS2006.resptype"]<-"resptype"
colnames(O2006)[colnames(O2006)=="OCJS2006.pfa"]<-"pfa"

## Merge waves into single data set
OCJS_all<-rbind(O2004, O2005, O2006)

## Cleaning missed variables
OCJS_all$white<-NA
OCJS_all$white[OCJS_all$ethnic4==1]<-1
OCJS_all$white[OCJS_all$ethnic4!=1]<-0
 
OCJS_all$agesq<-OCJS_all$respage^2


OCJS_all$respsex <- factor(OCJS_all$respsex, levels = c(1,2), labels = c("Male", "Female"))
OCJS_all$h1fren<-factor(OCJS_all$h1fren, levels = c(1,2,3,4), labels = c("None", "A few", "Quite a lot", "All or nearly all of them"))
OCJS_all$g1todo<-factor(OCJS_all$g1todo, levels = c(1,2,3,4), labels = c("A lot", "Quite a lot", "Not very much", "Nothing"))
OCJS_all$g1area<-factor(OCJS_all$g1area, levels = c(1,2,3,4,5), labels = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"))
OCJS_all$g1areb<-factor(OCJS_all$g1areb, levels = c(1,2,3,4,5), labels = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disgree", "Strongly disagree"))
OCJS_all$g1arec<-factor(OCJS_all$g1arec, levels = c(1,2,3,4,5), labels = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"))
OCJS_all$g1ared<-factor(OCJS_all$g1ared, levels = c(1,2,3,4,5), labels = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disgree", "Strongly disagree"))
OCJS_all$g1aree<-factor(OCJS_all$g1aree, levels = c(1,2,3,4,5), labels = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"))
OCJS_all$g1safe<-factor(OCJS_all$g1safe, levels = c(1,2,3,4), labels = c("Very safe", "Fairly safe", "Fairly unsafe", "Very unsafe"))
OCJS_all$g2trpol<-factor(OCJS_all$g2trpol, levels = c(1,2,3,4), labels = c("A lot", "A fair amount", "Not very much", "Not at all"))
OCJS_all$violyr<-factor(OCJS_all$violyr, levels = c(0,1), labels = c("No", "Yes"))
OCJS_all$YAnyDrug<-factor(OCJS_all$YAnyDrug, levels = c(0,1), labels = c("No", "Yes"))
OCJS_all$disorder<-OCJS_all$g1dist01 + OCJS_all$g1dist02 + OCJS_all$g1dist03 + OCJS_all$g1dist04 + OCJS_all$g1dist05 + OCJS_all$g1dist06
OCJS_all$resptype<-factor(OCJS_all$resptype, levels=c(1,2), labels=c("Panel", "New"))

OCJS_all$violyr<-factor(OCJS_all$violyr)
OCJS_all$sweep<-factor(OCJS_all$sweep)
OCJS_all$YAnyDrug<-factor(OCJS_all$YAnyDrug)
OCJS_all$v1for<-factor(OCJS_all$v1for)
OCJS_all$v1thre<-factor(OCJS_all$v1thre)
OCJS_all$h1fren<-factor(OCJS_all$h1fren, ordered=TRUE, nmax=4)
OCJS_all$g1area<-factor(OCJS_all$g1area, ordered=TRUE)
OCJS_all$g1areb<-factor(OCJS_all$g1areb, ordered=TRUE)
OCJS_all$g1arec<-factor(OCJS_all$g1arec, ordered=TRUE)
OCJS_all$g1ared<-factor(OCJS_all$g1ared, ordered=TRUE)
OCJS_all$g1aree<-factor(OCJS_all$g1aree, ordered=TRUE)
OCJS_all$g1safe<-factor(OCJS_all$g1safe, ordered=TRUE)

# Limit data set to respondents 10-25 years old
OCJS_25<-subset(OCJS_all, respage<26)


# Models

# Bonferroni-adjusted p-values
p.adjust(pvals, "bonferroni")

# Create 'training' and 'testing' data sets

OCJS_4_6$sweep<-as.numeric(OCJS_4_6$sweep)
training<-(OCJS_4_6$sweep<4)
testing<-!training

training_data <- OCJS_4_6[training, ]
testing_data <- OCJS_4_6[testing,]

b1wepb_testing<-OCJS_4_6$b1wepb[testing]

library(ROCR)

# full_is

full_is<-glm(b1wepb ~ male + zage + agesq + nonwhite + disorder + violyr + YAnyDrug + g2trpol + v1for + h1fren, data=training_data, family="binomial", na.action=na.omit)
summary(full_is)
full_is_pred <- predict(full_is, testing_data, type="response")
model_pred_weapon <- rep("No", 795)
model_pred_weapon[full_is_pred > 0.1] <- "Yes"
table(model_pred_weapon, b1wepb_testing)
prob <- predict(full_is, newdata=testing_data, type="response")
pred <- prediction(prob, testing_data$b1wepb)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# train_lr_demog

train_lr_demog<-glm(b1wepb ~ male + zage + agesq + nonwhite, data=training_data, family="binomial", na.action=na.omit)
summary(train_lr_demog)
train_lr_demog_pred <- predict(train_lr_demog, testing_data, type="response")
model_pred_weapon <- rep("No", 795)
model_pred_weapon[train_lr_demog_pred > 0.1] <- "Yes"
table(model_pred_weapon, b1wepb_testing)
prob <- predict(train_lr_demog, newdata=testing_data, type="response")
pred <- prediction(prob, testing_data$b1wepb)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc


# train_lr_pers

train_lr_pers<-glm(b1wepb ~ violyr + YAnyDrug + g2trpol, data=training_data, family="binomial", na.action=na.omit)
summary(train_lr_pers)
train_lr_pers_pred <- predict(train_lr_pers, testing_data, type="response")
model_pred_weapon <- rep("No", 795)
model_pred_weapon[train_lr_pers_pred > 0.1] <- "Yes"
table(model_pred_weapon, b1wepb_testing)
prob <- predict(train_lr_pers, newdata=testing_data, type="response")
pred <- prediction(prob, testing_data$b1wepb)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc


# train_lr_victim

train_lr_victim<-glm(b1wepb ~ v1for + v1thre, data=training_data, family="binomial", na.action=na.omit)
summary(train_lr_victim)
train_lr_victim_pred <- predict(train_lr_victim, testing_data, type="response")
model_pred_weapon <- rep("No", 795)
model_pred_weapon[train_lr_victim_pred > 0.1] <- "Yes"
table(model_pred_weapon, b1wepb_testing)
prob <- predict(train_lr_victim, newdata=testing_data, type="response")
pred <- prediction(prob, testing_data$b1wepb)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc


# train_lr_intp

train_lr_intp<-glm(b1wepb ~ h1fren, data=training_data, family="binomial", na.action=na.omit)
summary(train_lr_intp)
train_lr_intp_pred <- predict(train_lr_intp, testing_data, type="response")
model_pred_weapon <- rep("No", 795)
model_pred_weapon[train_lr_intp_pred > 0.1] <- "Yes"
table(model_pred_weapon, b1wepb_testing)
prob <- predict(train_lr_intp, newdata=testing_data, type="response")
pred <- prediction(prob, testing_data$b1wepb)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc


# train_lr_area

train_lr_area<-glm(b1wepb ~ disorder + g1safe, data=training_data, family="binomial", na.action=na.omit)
summary(train_lr_area)
train_lr_area_pred <- predict(train_lr_area, testing_data, type="response")
model_pred_weapon <- rep("No", 795)
model_pred_weapon[train_lr_area_pred > 0.1] <- "Yes"
table(model_pred_weapon, b1wepb_testing)
prob <- predict(train_lr_area, newdata=testing_data, type="response")
pred <- prediction(prob, testing_data$b1wepb)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# train_lr_full

train_lr_full<-glm(b1wepb ~ male + zage + agesq + nonwhite + violyr + YAnyDrug + g2trpol + v1for + v1thre + h1fren + disorder + g1safe, data=training_data, family="binomial", na.action=na.omit)
summary(train_lr_full)
train_lr_full_pred <- predict(train_lr_full, testing_data, type="response")
model_pred_weapon <- rep("No", 795)
model_pred_weapon[train_lr_full_pred > 0.1] <- "Yes"
table(model_pred_weapon, b1wepb_testing)
prob <- predict(train_lr_full, newdata=testing_data, type="response")
pred <- prediction(prob, testing_data$b1wepb)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

exp(cbind("Odds ratio"=coef(train_lr_demog), confint.default(train_lr_demog, level=0.95)))
exp(cbind("Odds ratio"=coef(train_lr_pers), confint.default(train_lr_pers, level=0.95)))
exp(cbind("Odds ratio"=coef(train_lr_victim), confint.default(train_lr_victim, level=0.95)))
exp(cbind("Odds ratio"=coef(train_lr_intp), confint.default(train_lr_intp, level=0.95)))
exp(cbind("Odds ratio"=coef(train_lr_area), confint.default(train_lr_area, level=0.95)))
exp(cbind("Odds ratio"=coef(train_lr_sweep), confint.default(train_lr_sweep, level=0.95)))
exp(cbind("Odds ratio"=coef(train_lr_full), confint.default(train_lr_full, level=0.95)))
exp(cbind("Odds ratio"=coef(train_lr_best), confint.default(train_lr_best, level=0.95)))

library(pscl)

pR2(train_lr_demog)
pR2(train_lr_pers)
pR2(train_lr_victim)
pR2(train_lr_intp)
pR2(train_lr_area)
pR2(train_lr_full)
pR2(train_lr_best)
