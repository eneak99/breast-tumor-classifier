cl = read.csv("ClinicalData_82patients.csv", header=T, row.name = 1)
clinical = cl[,-c(13)]

## on a une variable quantitative "Age.at.diagnosis" et une variable qualitative "Grade"
clinical [ , c("Grade", "Age.at.diagnosis")]
boxplot( clinical$Age.at.diagnosis ~ as.factor(clinical$Grade), xlab="Grade", ylab="Age.at.diagnosis")
points( clinical$Age.at.diagnosis ~ as.factor(clinical$Grade) )
pairwise.t.test( clinical$Age.at.diagnosis, clinical$Grade)
## les p-values sont supérieures à 0.05 donc non rejet de H0, les variables ne sont pas associées (indépendance des variables)

## on a deux variables qualitatives "p53.status" et "Status"
table(clinical$Status)
table(clinical$p53.status)
table(clinical$Status, clinical$p53.status)
barplot( table(clinical$Status, clinical$p53.status), legend=T, xlab="p53.status")
chisq.test( table(clinical$Status, clinical$p53.status) )
## avec le test de khi-deux, on a un message d'erreur qui nous dit que l'approximation peut être incorrect
## donc on refait un test de fisher --- ce test n'est pas valide car petits effectifs (<5)
fisher.test( table(clinical$Status, clinical$p53.status) )
## la p-value est inférieure à 0.05 donc rejet de H0, les variables ne sont pas indépentantes
## --> association statistique des variables

## on a une variable quantitative "Overall.survival" et une variable qualitative "node.status"
clinical [ , c("node.status", "Overall.survival")]
boxplot( clinical$Overall.survival ~ as.factor(clinical$node.status), xlab="node.status", ylab="Overall.survival")
points( clinical$Overall.survival ~ as.factor(clinical$node.status))
pairwise.t.test( clinical$Overall.survival, clinical$node.status)
## les p-values sont supérieures à 0.05 donc non rejet de H0, les variables ne sont pas associées (indépendance des variables)

## on a deux variables qualitatives "ER.status" et "subtype"
table(ER.status=clinical$ER.status, subtype=clinical$subtype)
barplot( table(clinical$ER.status, clinical$subtype), legend=T, xlab="subtype")
chisq.test( table(clinical$ER.status, clinical$subtype) )
## avec le test de khi-deux, on a un message d'erreur qui nous dit que l'approximation peut être incorrect
## donc on refait un test de fisher --- ce test n'est pas valide car petits effectifs (<5)
fisher.test( table(clinical$ER.status, clinical$subtype) )
## la p-value est inférieure à 0.05 donc rejet de H0, les variables ne sont pas indépendantes
## --> association statistique des variables

## on a deux variables qualitatives "tumor.size." et "reference.sample.batch.ID"
table(clinical$tumor.size., clinical$reference.sample.batch.ID)
barplot( table(clinical$tumor.size., clinical$reference.sample.batch.ID), legend=T, xlab="reference.sample.batch.ID")
chisq.test( table( droplevels(as.factor(clinical$tumor.size.[ clinical$reference.sample.batch.ID != "CDB" ])), droplevels(clinical$reference.sample.batch.ID[ clinical$reference.sample.batch.ID != "CDB" ] )) )
## avec le test de khi-deux, on a un message d'erreur qui nous dit que l'approximation peut être incorrect
## donc on refait un test de fisher --- ce test n'est pas valide car petits effectifs (<5)
fisher.test( table( droplevels(as.factor(clinical$tumor.size.[ clinical$reference.sample.batch.ID != "CDB" ])), droplevels(clinical$reference.sample.batch.ID[ clinical$reference.sample.batch.ID != "CDB" ] )) )
## nla p-value est supérieure à 0.05 donc non-rejet de H0, les variables sont indépendantes

## on a deux variables qualitatives "node.status" et "p53.status"
table(clinical$node.status, clinical$p53.status)
barplot( table(clinical$node.status, clinical$p53.status), legend=T, xlab="p53.status")
fisher.test( table(clinical$node.status, clinical$p53.status) )
## la p-value est supérieure à 0.05 donc non-rejet de H0, les variables sont indépendantes
