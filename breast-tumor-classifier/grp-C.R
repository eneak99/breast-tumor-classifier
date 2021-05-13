cl = read.csv("ClinicalData_82patients.csv", header=T, row.name = 1)
### elimine la dernière colonne (Comments) --> 12 variables cliniques
clinical = cl[,-c(13)]

#table(clinical$reference.sample.batch.ID)
#table(clinical$p53.status)
table(Ref=clinical$reference.sample.batch.ID, p53=clinical$p53.status)
barplot( t(table(clinical$reference.sample.batch.ID, clinical$p53.status)), legend=T, xlab="reference.sample.batch.ID", ylab="Effectif")
chisq.test( table(droplevels(clinical$reference.sample.batch.ID[ clinical$reference.sample.batch.ID != "CDB"]), clinical$p53.status[ clinical$reference.sample.batch.ID != "CDB"]) )
fisher.test( table(droplevels(clinical$reference.sample.batch.ID[ clinical$reference.sample.batch.ID != "CDB"]), clinical$p53.status[ clinical$reference.sample.batch.ID != "CDB"]) )

#table(clinical$Grade)
#table(clinical$Histology)
table(clinical$Grade, clinical$Histology)
barplot( table(clinical$Grade, clinical$Histology), legend=T)
#chisq.test( table(clinical$Grade, clinical$Histology) )
#fisher.test( table(clinical$Grade, clinical$Histology) )

#clinical [ , c("Overall.survival", "Age.at.diagnosis") ]
plot( clinical$Age.at.diagnosis, clinical$Overall.survival )
cor.test( clinical$Age.at.diagnosis, clinical$Overall.survival )

#table(clinical$reference.sample.batch.ID)
#table(clinical$subtype)
table(clinical$subtype, clinical$reference.sample.batch.ID)
barplot( table(clinical$subtype, clinical$reference.sample.batch.ID), legend=T)
chisq.test( table(clinical$subtype, clinical$reference.sample.batch.ID) )
fisher.test( table(clinical$subtype, clinical$reference.sample.batch.ID) )

#table(clinical$subtype)
#table(clinical$Histology)
table(clinical$Histology, clinical$subtype)
barplot( table(clinical$Histology, clinical$subtype), legend=T)
chisq.test( table(clinical$Histology, clinical$subtype) )
