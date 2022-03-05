rm(list=ls())
setwd("/Users/jvila/Dropbox/euit/Doctorants/RafaLlorens/SHARE/RafaSHARE/")

library(haven)
library(Epi)
library(Hmisc)
library(gdata)
library(labelled)

# datpre <- as.data.frame(read_sav("../data/Easy567depurada v311gener.sav"))
datpre <- as.data.frame(read_sav("../data/OLA567DEPURADAdef.sav"))

head(datpre)
str(datpre)

stat.table(list(Country=País, QoL=CASP), 
           list(N=count(),'%'=percent(CASP)), 
           data=datpre,margins=T)

x <- stat.table(index=list(Country=País), list(N=count(), 
    "mean"=mean(casp.12), "SD"= sd(casp.12)), data=datpre)

y <- stat.table(list(Country=País, Missing=is.na(CASP)), 
           list(N=count()), data=datpre)

y <- stat.table(list(Country=País, Missing=is.na(CASP)), 
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
###################
xvaris <- c("Identificadorpersona", "Identificadorhogar", "Identificadorpareja", "Genero", "Ola")
subset(datpre, substr(datpre$Identificadorpersona, 1, 9) == "SI-537920")[, xvaris]

datpre$Identificadorhogar <- with(datpre, ifelse(Identificadorpersona=="EG-049809-01" & Ola==6 & Genero== 0, "EG-049809-A", Identificadorhogar))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-049809-01" & Ola==6 & Genero== 0, "EG-049809-01-02", Identificadorpareja))

datpre$Identificadorhogar  <- with(datpre, ifelse(Identificadorpersona=="EG-113241-01" & Ola==7 & Genero== 1, "EG-113241-A", Identificadorhogar))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-113241-01" & Ola==7 & Genero== 1, "EG-113241-01-02", Identificadorpareja))

datpre$Identificadorhogar  <- with(datpre, ifelse(Identificadorpersona=="EG-503142-01" & Ola==7 & Genero== 1, "EG-503142-A", Identificadorhogar))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-503142-01" & Ola==7 & Genero== 1, "EG-503142-01-03", Identificadorpareja))

datpre$Identificadorhogar  <- with(datpre, ifelse(Identificadorpersona=="EG-571277-01" & Ola==7 & Genero== 0, "EG-571277-A", Identificadorhogar))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-571277-01" & Ola==7 & Genero== 0, "EG-571277-01-02", Identificadorpareja))

datpre$Identificadorhogar  <- with(datpre, ifelse(Identificadorpersona=="EG-625924-01" & Ola==7 & Genero== 0, "EG-625924-A", Identificadorhogar))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="EG-625924-01" & Ola==7 & Genero== 0, "EG-625924-01-02", Identificadorpareja))

datpre$Identificadorhogar  <- with(datpre, ifelse(Identificadorpersona=="LU-186391-01" & Ola==7 & Genero== 1, "LU-186391-A", Identificadorhogar))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="LU-186391-01" & Ola==7 & Genero== 1, "LU-186391-01-02", Identificadorpareja))

datpre$Identificadorhogar  <- with(datpre, ifelse(Identificadorpersona=="LU-355145-01" & Ola==7 & Genero== 1, "LU-355145-A", Identificadorhogar))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="LU-355145-01" & Ola==7 & Genero== 1, "LU-355145-01-02", Identificadorpareja))

datpre$Identificadorhogar  <- with(datpre, ifelse(Identificadorpersona=="SE-285504-01" & Ola==7 & Genero== 1, "SE-285504-A", Identificadorhogar))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="SE-285504-01" & Ola==7 & Genero== 1, "SE-285504-01-02", Identificadorpareja))


datpre$Identificadorpersona <- with(datpre, ifelse(Identificadorpersona=="EG-280794-01" & Ola==5 & Genero== 0, "EG-280794-03", Identificadorpersona))

datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpersona=="CZ-024433-02" & Ola==7 & Genero== 0, "CZ-024433-01-02", Identificadorpareja))
datpre$Identificadorpareja <- with(datpre, ifelse(Identificadorpareja=="CZ-024433-01-03" & Ola==7 & Genero== 1, "CZ-024433-01-02", Identificadorpareja))

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

# subdat01$PartiType <- "Participant"


typeparti <- rbind(#subdat01[, c("ID", "PartiType")], 
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
datpre$ageGroup <- factor(with(datpre, ifelse(age<50, "<50",
                                ifelse(age%in%c(50:64), "50-64",
                                ifelse(age%in%c(65:74), "65-74",
                                ifelse(age%in%c(75:84), "75-84",
                                ifelse(age>=85, "85+", NA)))))))
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
            "situacionlaboral", "llegafindemes", "arearesidencia",
            "ayudaeconomica", "saludinfancia", "vacunasinfantil",
            "enfermcronic", "limitacionAVD", "dificultadmovilidad",
            "saludautopercibida", "visitasmedico12meses", "ingresohospital12meses",
            "categoriaIMC", "Fumaadiario", "bebeadiario",
            "activfisica", "escaladepresion", "CASP")
# xvaris[xvaris%nin%names(datpre)]
# names(datpre)[names(datpre)%nin%xvaris]
# agrep("zonaresidencia", names(datpre), value =T)


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

# nivelestudios
table(datpre$nivelestudios)
datpre$nivelestudios <- with(datpre, factor(nivelestudios, levels = c("bajo", "medio", "Alto"))) 
Hmisc::label(datpre$nivelestudios) <- "Educational level"



datpre$CASP <- factor(as.character(datpre$CASP), 
    levels=c("baja calidad vida", "moderada calidad vida", "alta calidad vida", "muy alta calidad vida"))
Hmisc::label(datpre$CASP) <- "CASP:categories"

datpre$casp.12 <- as.numeric(as.character(datpre$casp.12))
Hmisc::label(datpre$casp.12) <- "CASP:índice calidad vida/bienestar"

# hermanosvivos
Hmisc::label(datpre$hermanosvivos) <- "Alive brothers"

# limitacionAVD
Hmisc::label(datpre$limitacionAVD) <- "Acti.Vida Diaria Indice W & H"


# Age at Recruitment 
tmp <- subset(datpre, visnumber==1)[,c("Identificadorpersona", "age")]
tmp <- rename.vars(tmp, "age", "ageRecru")
datpre <- merge(datpre, tmp, by="Identificadorpersona", all.x=T) 
Hmisc::label(datpre$ageRecru) <- "Age at recruitment"

x <- table(datpre$Identificadorhogar)
datpre$SurvPartner <- factor(with(datpre, ifelse(Identificadorhogar%in%names(x[x==6]), "Yes", 
                          ifelse(Identificadorhogar%in%names(x[x==3]), "No", NA))))
Hmisc::label(datpre$SurvPartner) <- "Partner surveyed"

datpre <- rename.vars(datpre, "nºenfermedadescronicas", "chronicNum")
datpre$chronicNum <- as.numeric(datpre$chronicNum)
Hmisc::label(datpre$chronicNum) <- "Num Chronic disease"

datpre$chronicCat <- factor(with(datpre, ifelse(chronicNum==0, "No", 
                                  ifelse(chronicNum%in%c(1,2), "1-2", "3+"))),
                            levels = c("No", "1-2", "3+"))
Hmisc::label(datpre$chronicCat) <- "Chronic disease (grouped)"


# saving
dat <- datpre

# selecting participant with age < 49
vis1 <- subset(dat, PartiType=="Participant" & visnumber ==1)
todele <- subset(vis1, age<50)$Identificadorhogar




dat$flow <- with(dat, ifelse(Identificadorhogar%in%todele, "Parti.Age <49", "ok"))
  
save(dat, file = "../data/dat.rda")


# algunes correccions
#####################
rm(list=ls())
setwd("/Users/jvila/Dropbox/euit/Doctorants/RafaLlorens/SHARE/RafaSHARE/")

library(Hmisc)
library(gdata)
library(labelled)

load("../data/dat.rda")

# numeronietos
temp <- subset(dat, visnumber==1)[, c("Identificadorpersona", "numeronietos")]
temp <- rename.vars(temp, "numeronietos", "xxx")
dat <- merge(dat, temp, by = "Identificadorpersona", all.x=T)
dat$numeronietos <-  with(dat, ifelse(is.na(numeronietos), xxx, numeronietos))

# numeronietos GR
dat$numnietosGR <- factor(with(dat, ifelse(numeronietos== 0, "No", ifelse(numeronietos <= 4, "1-4", 
                                    ifelse(numeronietos <= 8, "5-8",
                                    ifelse(numeronietos >= 9, "9+", NA))))),
                levels= c("No", "1-4", "5-8", "9+"))
Hmisc::label(dat$numnietosGR) <- "Numero nietos (agrupado)"


# saludautopercibida
dat$saludautopercibida <- factor(dat$saludautopercibida, levels = c("Pesima", "Regular", "Buena", "Muy buena", "Excelente"))

# numerohijos
dat$numhijosGR <- factor(with(dat, ifelse(numerohijos== 0, "No", ifelse(numerohijos <= 2, "1-2", 
                                           ifelse(numerohijos <=5, "3-5",
                                           ifelse(numerohijos>=6, "6+", NA))))), 
                levels= c("No", "1-2", "3-5", "6+"))
Hmisc::label(dat$numhijosGR) <- "Numero hijos (agrupado)"


# saludinfancia
dat$saludinfancia <- factor(dat$saludinfancia, levels = c("mala", "buena", "muy buena"))
dat$civilStat <- factor(dat$civilStat, levels = c("soltero/a", "casado‎/a o pareja registrada", "divorciado‎/a o separado‎/a", "viudo/a"))
Hmisc::label(dat$civilStat) <- "Civil Status"

# visitasmedico12meses
dat$visitasmedico12meses <- factor(dat$visitasmedico12meses, levels = c("de 1 a 5", "de 6 a 10",
                                            "de 11 a 20", "de 21 a 30", "de 31 a 100"))
Hmisc::label(dat$visitasmedico12meses) <- "Visitas medico 12meses"

# bebeadiario
dat$bebeadiario <- factor(dat$bebeadiario, levels = c("no bebe o menos de 1-2 veces al mes", 
                                                      "bebe entre 1 y 4 días a la semana", 
                                                      "bebe casi todos los días"))
Hmisc::label(dat$bebeadiario) <- "Bebe alcohol a diario"

# activfisica
dat$activfisica <- factor(dat$activfisica, levels = c("no activo‎/a", "activo‎/a"))
Hmisc::label(dat$activfisica) <- "Actividad fisica"

# escaladepresion
dat$escaladepresion <- factor(dat$escaladepresion, levels = c("Baja depresión",
                                                              "Moderada depresión",
                                                              "Alta depresión"))
Hmisc::label(dat$escaladepresion) <- "Escala de depresion EURO-D"

save(dat, file = "../data/dat.rda")

# 1)	Refer les anàlisi amb la base dades nova
# 2)	Eliminar els menors de 50 anys
# 3)	Reordenar les categories de CASP en el resultats
# 4)	Explorar el següent model multivariat
# 
# CASP = sexe + age + ruralitat + país + nivell estudis + salut auto percebuda + partner + estat civil + viu en parella + fills + nets + llegarfindemes + Estado de salud infantil + enfermedades crónicas + Acti.Vida Diaria Indice W & H + saludautopercibida + visitasmedico12meses + Fuma a diario + bebe alcohol a diario + Actividad física: + Escala de depresion EURO-D
# 
