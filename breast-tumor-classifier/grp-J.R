clinical = read.csv("ClinicalData_82patients.csv", header=T, row.name=1)

### Age.at.diagnosis vs reference.sample.batch.ID
clinical [ , c("Age.at.diagnosis", "reference.sample.batch.ID")]
boxplot(clinical$Age.at.diagnosis~ clinical$reference.sample.batch.ID, xlab="reference.sample.batch.ID", ylab="Age.at.diagnosis")
points(clinical$Age.at.diagnosis~ clinical$reference.sample.batch.ID)
pairwise.t.test( clinical$Age.at.diagnosis[ clinical$reference.sample.batch.ID!= "CDB" ], droplevels( clinical$reference.sample.batch.ID[ clinical$reference.sample.batch.ID!= "CDB" ] ) )

### Status vs Histology
#table(clinical$Status)
#table(clinical$Histology)
table(Status=clinical$Status, Histology=clinical$Histology)
barplot ( table(clinical$Status, clinical$Histology), legend=T, xlab="Histology" )
chisq.test(table(droplevels(clinical$Histology[ is.na(clinical$Status)==F ]), droplevels(as.factor(clinical$Status[ is.na(clinical$Status)==F ]))))
fisher.test(table(droplevels(clinical$Histology[ is.na(clinical$Status)==F ]), droplevels(as.factor(clinical$Status[ is.na(clinical$Status)==F ]))))

### ER.status vs p53.status
#table(clinical$ER.status)
#table(clinical$p53.status)
table(ERstatus=clinical$ER.status, p53=clinical$p53.status)
barplot ( table(clinical$ER.status, clinical$p53.status), legend=T, xlab="p53.status")
chisq.test (table(clinical$ER.status, clinical$p53.status))
fisher.test (table(clinical$ER.status, clinical$p53.status))

### subtype vs Overall.survival
clinical [ , c("subtype", "Overall.survival")]
boxplot( clinical$Overall.survival ~ clinical$subtype, xlab="subtype", ylab="Overall.survival")
points ( clinical$Overall.survival ~ clinical$subtype)
pairwise.t.test( clinical$Overall.survival, clinical$subtype)

### Relapse.free.survival vs reference.sample.batch.ID
clinical [ , c("Relapse.free.survival", "reference.sample.batch.ID")]
boxplot(clinical$Relapse.free.survival ~ clinical$reference.sample.batch.ID, xlab="reference.sample.batch.ID", ylab="Relapse.free.survival")
points(clinical$Relapse.free.survival ~ clinical$reference.sample.batch.ID)
pairwise.t.test( clinical$Relapse.free.survival[ clinical$reference.sample.batch.ID!= "CDB" ], droplevels( clinical$reference.sample.batch.ID[ clinical$reference.sample.batch.ID!= "CDB" ] ) )
