rm(list=ls())
setwd("/Users/jvila/Dropbox/euit/Doctorants/RafaLlorens/SHARE/RafaSHARE/")

library(haven)
library(Epi)
library(Hmisc)
library(gdata)
library(labelled)

datpre <- as.data.frame(read_sav("../data/Easy567depurada v311gener.sav"))

head(datpre)
str(datpre)

stat.table(list(Country=País, QoL=CASP), 
           list(N=count(),'%'=percent(CASP)), 
           data=datpre,margins=T)

stat.table(index=list(Country=País), list(N=count(), 
    "mean"=mean(casp.12), "SD"= sd(casp.12)), data=datpre)

stat.table(list(Country=País, Missing=is.na(CASP)), 
           list(N=count(),'%'=percent(is.na(CASP))), 
           data=datpre,margins=T)

stat.table(list(Country=País, Missing=is.na(escaladepresion)), 
           list(N=count(),'%'=percent(is.na(escaladepresion))), 
           data=datpre,margins=T)

# with Missing
# datpre$temp <- with(datpre, ifelse(is.na(escaladepresion), "Missing", escaladepresion)) 
# stat.table(index=list(Genero,"Escala Depre"=temp),
#         contents=list(count(), '%'=percent(temp)),
#         data=datpre, margins=T)


datpre$Identificadorpersona <- toupper(datpre$Identificadorpersona)
datpre$Identificadorhogar <- toupper(datpre$Identificadorhogar)
datpre$Identificadorpareja <- toupper(datpre$Identificadorpareja)
length(unique(datpre$Identificadorpersona))

# corregeixo errors
datpre$Identificadorpersona <- with(datpre, ifelse(Identificadorpersona=="EG-280794-01" & Ola==5 & Genero== 0, "EG-280794-03", Identificadorpersona))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="CZ-024433-02" & Ola==7 & Genero== 0, "CZ-024433-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpareja=="CZ-024433-01-03" & Ola==7 & Genero== 0, "CZ-024433-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="CZ-437040-01" & Ola==7 & Genero== 0, "CZ-437040-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="CZ-437040-02" & Ola==7 & Genero== 1, "CZ-437040-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-176888-01" & Ola==6 & Genero== 0, "EG-176888-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-176888-01" & Ola==7 & Genero== 0, "EG-176888-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-176888-02" & Ola==6 & Genero== 1, "EG-176888-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-176888-02" & Ola==7 & Genero== 1, "EG-176888-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-659833-01" & Ola==6 & Genero== 0, "EG-659833-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-659833-02" & Ola==6 & Genero== 1, "EG-659833-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="SI-548294-01" & Ola==5 & Genero== 0, "SI-548294-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="SI-548294-02" & Ola==5 & Genero== 1, "SI-548294-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="DE-486996-01" & Ola==7 & Genero== 1, "DE-486996-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="DE-486996-02" & Ola==7 & Genero== 0, "DE-486996-01-02", Identificadorpareja))
datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="DE-486996-02" & Ola==7 & Genero== 0, "DE-486996-A", Identificadorhogar))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="DE-746995-01" & Ola==7 & Genero== 1, "DE-746995-01-03", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="DE-746995-03" & Ola==7 & Genero== 0, "DE-746995-01-03", Identificadorpareja))
datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="DE-746995-03" & Ola==7 & Genero== 0, "DE-746995-A", Identificadorhogar))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="DK-088955-01" & Ola==7 & Genero== 1, "DK-088955-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="DK-088955-02" & Ola==7 & Genero== 0, "DK-088955-01-02", Identificadorpareja))
datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="DK-088955-02" & Ola==7 & Genero== 0, "DK-088955-A", Identificadorhogar))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-049809-02" & Ola==6 & Genero== 1, "EG-049809-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-113241-02" & Ola==7 & Genero== 0, "EG-113241-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-429370-01" & Ola==7 & Genero== 1, "EG-429370-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-429370-02" & Ola==7 & Genero== 0, "EG-429370-01-02", Identificadorpareja))
datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="EG-429370-02" & Ola==7 & Genero== 0, "EG-429370-A", Identificadorhogar))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-503142-03" & Ola==7 & Genero== 0, "EG-503142-01-03", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-571277-02" & Ola==7 & Genero== 1, "EG-571277-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-625924-02" & Ola==7 & Genero== 1, "EG-625924-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-630694-01" & Ola==7 & Genero== 1, "EG-630694-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-630694-02" & Ola==7 & Genero== 0, "EG-630694-01-02", Identificadorpareja))
datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="EG-630694-02" & Ola==7 & Genero== 0, "EG-630694-A", Identificadorhogar))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="LU-186391-02" & Ola==7 & Genero== 0, "LU-186391-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="LU-355145-02" & Ola==7 & Genero== 0, "LU-355145-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="SE-121379-01" & Ola==7 & Genero== 1, "SE-121379-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="SE-121379-02" & Ola==7 & Genero== 0, "SE-121379-01-02", Identificadorpareja))
datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="SE-121379-02" & Ola==7 & Genero== 0, "SE-121379-A", Identificadorhogar))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="SE-285504-02" & Ola==7 & Genero== 0, "SE-285504-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="SE-322366-01" & Ola==7 & Genero== 0, "SE-322366-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="SE-322366-02" & Ola==7 & Genero== 1, "SE-322366-01-02", Identificadorpareja))
datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="SE-322366-02" & Ola==7 & Genero== 1, "SE-322366-A", Identificadorhogar))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="SE-914685-01" & Ola==7 & Genero== 0, "SE-914685-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="SE-914685-02" & Ola==7 & Genero== 1, "SE-914685-01-02", Identificadorpareja))
datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="SE-914685-02" & Ola==7 & Genero== 1, "SE-914685-A", Identificadorhogar))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="DE-188859-02" & Ola==6 & Genero== 0, "DE-188859-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="DE-188859-02" & Ola==7 & Genero== 0, "DE-188859-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="DE-712739-02" & Ola==6 & Genero== 0, "DE-712739-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="DE-712739-02" & Ola==7 & Genero== 0, "DE-712739-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="DE-963491-01" & Ola==6 & Genero== 1, "DE-963491-01-05", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="DE-963491-01" & Ola==7 & Genero== 1, "DE-963491-01-05", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="DK-735581-03" & Ola==6 & Genero== 0, "DK-735581-01-03", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="DK-735581-03" & Ola==7 & Genero== 0, "DK-735581-01-03", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-007361-03" & Ola==6 & Genero== 1, "EG-007361-01-03", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-007361-03" & Ola==7 & Genero== 1, "EG-007361-01-03", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="SE-205337-02" & Ola==6 & Genero== 0, "SE-205337-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="SE-205337-02" & Ola==7 & Genero== 0, "SE-205337-01-02", Identificadorpareja))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="SE-583765-01" & Ola==6 & Genero== 0, "SE-583765-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="SE-583765-01" & Ola==7 & Genero== 0, "SE-583765-01-02", Identificadorpareja))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="BN-173614-01" & Ola==5 & Genero== 1, "BN-173614-B", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="DE-165281-02" & Ola==5 & Genero== 1, "DE-165281-B", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="DE-188859-01" & Ola==5 & Genero== 1, "DE-188859-B", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="DE-337772-02" & Ola==7 & Genero== 1, "DE-337772-A", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="DE-712739-01" & Ola==5 & Genero== 1, "DE-712739-B", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="DE-963491-05" & Ola==5 & Genero== 0, "DE-963491-B", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="DK-007887-02" & Ola==5 & Genero== 1, "DK-007887-B", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="DK-082427-01" & Ola==7 & Genero== 0, "DK-082427-A", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="DK-735581-01" & Ola==5 & Genero== 1, "DK-735581-B", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="EG-007361-01" & Ola==5 & Genero== 0, "EG-007361-B", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="EG-492307-01" & Ola==7 & Genero== 1, "EG-492307-A", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="EG-568525-01" & Ola==7 & Genero== 1, "EG-568525-A", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="EG-927328-01" & Ola==7 & Genero== 1, "EG-927328-A", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="ES-537489-02" & Ola==7 & Genero== 1, "ES-537489-A", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="IT-001044-04" & Ola==5 & Genero== 1, "IT-001044-B", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="LU-079270-02" & Ola==7 & Genero== 1, "LU-079270-A", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="SE-065947-01" & Ola==7 & Genero== 0, "SE-065947-A", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="SE-205337-01" & Ola==5 & Genero== 1, "SE-205337-B", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="SE-583765-02" & Ola==5 & Genero== 1, "SE-583765-B", Identificadorhogar))

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="SI-537920-02" & Ola==7 & Genero== 0, "SI-537920-A", Identificadorhogar))




# ID register
datpre$ID <- with(datpre, paste(Identificadorpersona, "-", Ola, sep=""))
length(unique(datpre$ID))
Hmisc::label(datpre$ID) <- "Register ID"


xvaris <- c("Identificadorpersona", "Identificadorhogar", "Identificadorpareja", 
            "Ola", "entrevistapareja", "ID", "Genero")
head(datpre[, xvaris])


# Per crear si es "Partner" o "Participant"
##########################################
# selecciono hogar amb 6 registres + Identificadorpareja no missing
tmp06 <- with(subset(datpre, !is.na(Identificadorpareja) & Identificadorpareja!=""), table(Identificadorhogar))
tmp06 <- names(tmp06[tmp06==6])
subdat06 <- datpre[datpre$Identificadorhogar%in%tmp06, xvaris]
subdat06 <- subdat06[order(subdat06$Identificadorhogar), ]

table(nchar(subdat06$Identificadorpareja))
table(nchar(subdat06$Identificadorpersona))
head(subdat06)
subdat06$xxx <- with(subdat06, paste(substr(Identificadorpersona, 1, 9),
                                     substr(Identificadorpareja, 13,15), sep=""))
subdat06$PartiType <- with(subdat06, ifelse(Identificadorpersona==xxx, "Partner", "Participant"))

# selecciono no seleccionats anteriorment i hogar amb 3 registres
xhogar <- names(table(subdat06$Identificadorhogar))
tmp03 <- with(subset(datpre, Identificadorhogar%nin%xhogar), table(Identificadorhogar))
tmp03 <- names(tmp03[tmp03==3])
subdat03 <- datpre[datpre$Identificadorhogar%in%tmp03, xvaris]

table(subdat03$entrevistapareja, useNA = "ifany") # tenen parella pero no hi ha dades??? poso que son participants
table(nchar(subdat03$Identificadorpareja))
table(nchar(subdat03$Identificadorpersona))
head(subdat03)
subdat03$PartiType <- "Participant"

# selecciono no seleccionats anteriorment i 5 hogar
xhogar <- c(names(table(subdat06$Identificadorhogar)), names(table(subdat03$Identificadorhogar)))
tmp05 <- with(subset(datpre, Identificadorhogar%nin%xhogar), table(Identificadorhogar))
tmp05 <- names(tmp05[tmp05==5])
subdat05 <- datpre[datpre$Identificadorhogar%in%tmp05, xvaris]

table(nchar(subdat05$Identificadorpareja))
table(nchar(subdat05$Identificadorpersona))
head(subdat05)
subdat05$xxx <- with(subdat05, paste(substr(Identificadorpersona, 1, 9),
                                     substr(Identificadorpareja, 13,15), sep=""))
subdat05$PartiType <- with(subdat05, ifelse(Identificadorpersona==xxx, "Partner", "Participant"))

# selecciono no seleccionats anteriorment i 4 hogar
xhogar <- c(names(table(subdat06$Identificadorhogar)), 
            names(table(subdat03$Identificadorhogar)),
            names(table(subdat05$Identificadorhogar))
            )
tmp04 <- with(subset(datpre, Identificadorhogar%nin%xhogar), table(Identificadorhogar))
tmp04 <- names(tmp04[tmp04==4])
subdat04 <- datpre[datpre$Identificadorhogar%in%tmp04, xvaris]


# selecciono no seleccionats anteriorment i 2 hogar
xhogar <- c(names(table(subdat06$Identificadorhogar)), 
            names(table(subdat03$Identificadorhogar)),
            names(table(subdat05$Identificadorhogar)),
            names(table(subdat04$Identificadorhogar))
            )
tmp02 <- with(subset(datpre, Identificadorhogar%nin%xhogar), table(Identificadorhogar))
tmp02 <- names(tmp02[tmp02==2])
subdat02 <- datpre[datpre$Identificadorhogar%in%tmp02, xvaris]

datpre[datpre$Identificadorpersona%in%subdat02$Identificadorpersona, xvaris]

# selecciono no seleccionats anteriorment i 1 hogar
xhogar <- c(names(table(subdat06$Identificadorhogar)), 
            names(table(subdat03$Identificadorhogar)),
            names(table(subdat05$Identificadorhogar)),
            names(table(subdat04$Identificadorhogar)),
            names(table(subdat02$Identificadorhogar))
            )
tmp01 <- with(subset(datpre, Identificadorhogar%nin%xhogar), table(Identificadorhogar))
tmp01 <- names(tmp01[tmp01==1])
subdat01 <- datpre[datpre$Identificadorhogar%in%tmp01, xvaris]

subdat01$PartiType <- "Participant"


typeparti <- rbind(subdat01[, c("ID", "PartiType")], 
      # subdat02[, c("ID", "PartiType")],
      subdat03[, c("ID", "PartiType")],
      # subdat04[, c("ID", "PartiType")],
      subdat05[, c("ID", "PartiType")],
      subdat06[, c("ID", "PartiType")])
(repes <- with(datpre, table(ID)))[repes>1]
(repes <- with(typeparti, table(ID)))[repes>1]
datpre$ID[datpre$ID%nin%typeparti$ID]

datpre <- merge(datpre, typeparti, by = "ID", all.x =T)

Hmisc::label(datpre$PartiType) <- "Participant/Partner"



# visit number
datpre <- datpre[order(datpre$ID), ]
x2<-sort(datpre$Identificadorpersona)
tt<-table(x2)
ordre <- sapply(1:length(tt), function(i) 1:tt[i])
visnumber <- c(unlist(ordre))
datpre <- cbind(datpre,visnumber)
Hmisc::label(datpre$visnumber) <- "Visit number"

# Identificadorpersona, always 3??
length((repes <- with(datpre,table(Identificadorpersona)))[repes==3])

# # couple
# tmp <- subset(datpre,visnumber==1)[, c("ID", "Identificadorpersona", "Identificadorhogar", "Identificadorpareja", "Genero", "entrevistapareja")]
# tmp <- tmp[order(tmp$Identificadorpersona, tmp$Genero), ]
# tmp$coupleID <- substr(tmp$Identificadorpersona,1, 9)
# Hmisc::label(tmp$coupleID) <- "Couple/single ID"
# 
# x2<- tmp$coupleID
# tt<-table(x2)
# ordre <- sapply(1:length(tt), function(i) 1:tt[i])
# xxx <- c(unlist(ordre))
# tmp <- cbind(tmp,xxx)
# selec <- subset(tmp, xxx==2)[, "coupleID"]
# tmp$couple <- factor(with(tmp, ifelse(coupleID%in%selec, "Y", "N")))
# Hmisc::label(tmp$couple) <- "Couple data"

# # hetero?
# xxx <- subset(tmp, couple=="Y")
# xxx <- xxx[order(xxx$coupleID, xxx$Genero), ]
# xxx$xxx <- rep(c(0,1), nrow(xxx)/2)
# yyy <- xxx[(xxx$xxx!= xxx$Genero), "coupleID"]
# tmp$hetero <- with(tmp, ifelse(coupleID==yyy, "Homo", "Hetero"))
# Hmisc::label(tmp$hetero) <- "Homo/Hetero relationship"

# # merging
# datpre <- merge(datpre, tmp[, c("Identificadorpersona", "coupleID", "couple","hetero")], by="Identificadorpersona", all.x=T)
# 
# Sex
datpre$sex <- factor(with(datpre, ifelse(Genero==0, "Male", ifelse(Genero==1, "Female", NA))))
Hmisc::label(datpre$sex) <- "Sex"

# country 
tmp <- val_labels(datpre$País)
tmp <- data.frame(tmp)
tmp <- rename.vars(tmp, "tmp", "País")
tmp$country <- rownames(tmp)

datpre <- merge(datpre, tmp, by= "País", all.x=T)
datpre$country <- factor(datpre$country)
Hmisc::label(datpre$country) <- "Country"

# versionentrevista
label(datpre$versionentrevista)
datpre$versionentrevista <- as.factor(datpre$versionentrevista )
Hmisc::label(datpre$versionentrevista) <- "Vers. entrevista"

# nacidopaisentrevist
label(datpre$nacidopaisentrevist)
datpre$nacidopaisentrevist <- as.factor(with(datpre, ifelse(nacidopaisentrevist==1, "Y", 
                                                    ifelse(nacidopaisentrevist==2, "N", NA))))
Hmisc::label(datpre$nacidopaisentrevist) <- "Born country Interv."

# age
datpre$age <- floor(datpre$edad)
Hmisc::label(datpre$age) <- "Age"

# yearGroup
tmp <- val_labels(datpre$grupoedad)
tmp <- data.frame(tmp)
tmp <- rename.vars(tmp, "tmp", "grupoedad")
tmp$yearGroup <- rownames(tmp)

datpre <- merge(datpre, tmp, by= "grupoedad", all.x=T)
datpre$yearGroup <- factor(datpre$yearGroup)
Hmisc::label(datpre$yearGroup) <- "Born year Group"

# ageGroup
tmp <- val_labels(datpre$gruoedad)
tmp <- data.frame(tmp)
tmp <- rename.vars(tmp, "tmp", "gruoedad")
tmp$ageGroup <- rownames(tmp)

datpre <- merge(datpre, tmp, by= "gruoedad", all.x=T)
datpre$ageGroup <- factor(datpre$ageGroup)
Hmisc::label(datpre$ageGroup) <- "Age Group"

# estadocivil
tmp <- val_labels(datpre$estadocivil)
tmp <- data.frame(tmp)
tmp <- rename.vars(tmp, "tmp", "estadocivil")
tmp$civilStat <- rownames(tmp)

datpre <- merge(datpre, tmp, by= "estadocivil", all.x=T)
datpre$civilStat <- factor(datpre$civilStat)
Hmisc::label(datpre$civilStat) <- "Civil Status"

# edadpareja
datpre$edadpareja <- floor(datpre$edadpareja)
datpre$edadpareja <- with(datpre, ifelse(edadpareja==0, NA, edadpareja))
Hmisc::label(datpre$edadpareja) <- "Couple Age"

# vivirconyugue
label(datpre$vivirconyugue)
datpre$vivirconyugue <- as.factor(with(datpre, ifelse(vivirconyugue==1, "Y", 
                                                    ifelse(vivirconyugue==2, "N", NA))))
Hmisc::label(datpre$vivirconyugue) <- "Living with a Couple"

# "hijovivecerca",  "vivemadre", "vivepadre", "hermanosvivos", "nivelestudios", "situacionlaboral, llegarfindemes
xvaris <- c("hijovivecerca",  "vivemadre", "vivepadre",  "nivelestudios",
            "situacionlaboral", "llegarfindemes", "zonaresidencia",
            "ayudaeconomica", "saludinfantil", "vacunasinfantil",
            "enfermcronic", "limitacionAVD", "dificultadmovilidad",
            "saludautopercibida", "visitasmedico12meses", "ingresohospital12meses",
            "categoriaIMC", "Fumaadiario", "bebeadiario",
            "activfisica", "escaladepresion", "CASP")

for (i in 1:length(xvaris)){
    tmp <- val_labels(datpre[, xvaris[i]])
    tmp <- data.frame(tmp)
    tmp <- rename.vars(tmp, "tmp", xvaris[i])
    tmp$xxx <- rownames(tmp)
    
    datpre <- merge(datpre, tmp, by= xvaris[i], all.x=T)
    x <- label(datpre[,xvaris[i]])
    datpre[,xvaris[i]] <- factor(datpre$xxx)
    datpre <- remove.vars(datpre, "xxx")
    Hmisc::label(datpre[,xvaris[i]]) <- x
}

datpre$casp.12 <- as.numeric(as.character(datpre$casp.12))
Hmisc::label(datpre$casp.12) <- "CASP:índice calidad vida/bienestar"

# hermanosvivos
Hmisc::label(datpre$hermanosvivos) <- "Alive brothers"

# limitacionAVD
Hmisc::label(datpre$limitacionAVD) <- "Acti.Vida Diaria Indice W & H"

# saving
dat <- datpre
save(dat, file = "../data/dat.rda")



