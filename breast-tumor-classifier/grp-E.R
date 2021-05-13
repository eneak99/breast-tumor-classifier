clinical = read.csv("ClinicalData_82patients.csv", header=T, row.name=1)

### Age.at.diagnosis vs ER.status
boxplot(clinical$Age.at.diagnosis ~ as.factor(clinical$ER.status), xlab="ER.status", ylab="Age.at.diagnosis")
points(clinical$Age.at.diagnosis ~ as.factor(clinical$ER.status))
pairwise.t.test(clinical$Age.at.diagnosis,clinical$ER.status)

### tumor.size. vs Status
table(Taille_tumeur=clinical$tumor.size.,Status=clinical$Status)
barplot(table(clinical$tumor.size.,clinical$Status),legend=T, xlab="Status")
chisq.test(table(as.factor(clinical$tumor.size.),as.factor(clinical$Status)))
fisher.test(table(as.factor(clinical$tumor.size.),as.factor(clinical$Status)))

### Histology vs node.status
table(Histo=clinical$Histology,Node_status=clinical$node.status)
barplot(table(clinical$Histology,clinical$node.status),legend=T, xlab="node.status")
barplot(t(table(clinical$Histology,clinical$node.status)),legend=T, xlab="Histology") # on visualise ici beaucoup mieux que l'échantillonage est très biaisé. Su-représentation des histologies de type Ductal.
chisq.test(table(clinical$Histology,clinical$node.status))
fisher.test(table(clinical$Histology,clinical$node.status))

### subtype vs Grade
table(subtype=clinical$subtype,Grade=clinical$Grade)
barplot(t(table(clinical$subtype,clinical$Grade)),legend=T,xlab="subtype")
chisq.test(table(clinical$subtype,clinical$Grade))
fisher.test(table(clinical$subtype,clinical$Grade))

### reference.sample.batch.ID vs Histology
table(Ref=clinical$reference.sample.batch.ID,Histo=clinical$Histology)
barplot(table(clinical$reference.sample.batch.ID,clinical$Histology),legend=T,xlab="Histology")
chisq.test(table(clinical$reference.sample.batch.ID,clinical$Histology))
fisher.test(table(clinical$reference.sample.batch.ID,clinical$Histology))

### p53.status vs subtype
table(p53=clinical$p53.status,subtype=clinical$subtype)
barplot(table(clinical$p53.status,clinical$subtype),legend=T,xlab="subtype")
chisq.test(table(clinical$p53.status,clinical$subtype))
fisher.test(table(clinical$p53.status,clinical$subtype))
