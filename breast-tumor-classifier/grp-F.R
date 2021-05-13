clinical = read.csv("ClinicalData_82patients.csv", header=T, row.name=1)
str(clinical)

###(tumor size et Age at diagnosis)
clinical [ , c("tumor.size.", "Age.at.diagnosis") ]
boxplot( clinical$Age.at.diagnosis ~ as.factor(clinical$tumor.size.), xlab="tumor.size.", ylab="Age.at.diagnosis" )
points( clinical$Age.at.diagnosis ~ as.factor(clinical$tumor.size.) )
pairwise.t.test( clinical$Age.at.diagnosis, clinical$tumor.size. )

###(status et node status)
#table(clinical$Status)
#table(clinical$node.status)
table(node.status=clinical$node.status, Status=clinical$Status)
barplot( t(table(clinical$node.status, clinical$Status)), legend=T, xlab="node.status", ylab="Effectif" )
chisq.test( table(clinical$node.status, clinical$Status) )
fisher.test( table(clinical$node.status, clinical$Status) )

###(p53 status et relapse free survival)
clinical [ , c("p53.status", "Relapse.free.survival")]
boxplot( clinical$Relapse.free.survival ~ as.factor(clinical$p53.status), xlab="p53.status", ylab="Relapse.free.survival" )
points( clinical$Relapse.free.survival ~ as.factor(clinical$p53.status) )
pairwise.t.test( clinical$Relapse.free.survival, clinical$p53.status)

###(subtype et node status)
#table(clinical$subtype)
#table(clinical$node.status)
table(node.status=clinical$node.status, subtype=clinical$subtype)
barplot( table(clinical$node.status, clinical$subtype), legend=T, xlab="subtype", ylab="Effectif")
chisq.test( table(clinical$node.status, clinical$subtype) )
fisher.test( table(clinical$node.status, clinical$subtype) )

###(reference sample batch ID et grade)
#table(clinical$reference.sample.batch.ID)
#table(clinical$Grade)
table(Grade=clinical$Grade, Ref=clinical$reference.sample.batch.ID)
barplot( table(clinical$Grade, clinical$reference.sample.batch.ID), legend=T, xlab="reference.sample.batch.ID")
chisq.test( table(clinical$Grade[ clinical$reference.sample.batch.ID !="CDB" ], droplevels(clinical$reference.sample.batch.ID[ clinical$reference.sample.batch.ID !="CDB" ])) )
fisher.test( table(clinical$Grade[ clinical$reference.sample.batch.ID !="CDB" ], droplevels(clinical$reference.sample.batch.ID[ clinical$reference.sample.batch.ID !="CDB" ])) )

###(histology et p53 status)
table(Histo=clinical$Histology, p53=clinical$p53.status)
barplot( t(table(clinical$Histology, clinical$p53.status)), legend=T, xlab="Histology", ylab="Effectif")
chisq.test( table(droplevels(clinical$Histology[ clinical$Histology!="Fibroadenoma" & clinical$Histology!="Normal breast" ]), clinical$p53.status[ clinical$Histology!="Fibroadenoma" & clinical$Histology!="Normal breast" ]) )
fisher.test( table(droplevels(clinical$Histology[ clinical$Histology!="Fibroadenoma" & clinical$Histology!="Normal breast" ]), clinical$p53.status[ clinical$Histology!="Fibroadenoma" & clinical$Histology!="Normal breast" ]) )
