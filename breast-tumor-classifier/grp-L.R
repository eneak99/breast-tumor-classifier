### Groupe L
### Analyse des associations entre les paires de variables cliniques
clinical = read.csv("ClinicalData_82patients.csv", header=T, row.name=1)

### 1) p53.status et age.at.diagnosis (qualitative/quantitative)
clinical [ , c("p53.status", "Age.at.diagnosis")]
boxplot( clinical$Age.at.diagnosis ~ as.factor(clinical$p53.status), xlab="p53.status", ylab="Age.at.diagnosis")
points( clinical$Age.at.diagnosis ~ as.factor(clinical$p53.status))
pairwise.t.test( clinical$Age.at.diagnosis, clinical$p53.status)
### La probabilite de se tromper si on rejette H0 est de 0,11 (valeur p). On considère ce risque trop élevé (p>0.05), donc on accepte H0, ce qui signifie que la moyenne d'âge des patientes dont le statut p53 est égal à 0 (=wild type) ne diffère pas significativement de celle des patientes dont le statut p53 est égale à 1 (=gène muté). Il y a donc indépendance de ces deux variables (p53.status et Age.at.diagnosis).

### 2) subtype et status (qualitative/qualitative)
#table(clinical$subtype)
#table(clinical$Status)
table(Status=clinical$Status, subtype=clinical$subtype)
barplot( table(clinical$Status, clinical$subtype), legend=T, xlab="subtype" )
#chisq.test( table(clinical$Status, clinical$subtype) ) # test non valide, car effectifs trop faibles pour certaines cases de la table de contingence (table(Status=clinical$Status, subtype=clinical$subtype)). On doit donc procéder ici au test de Fisher
fisher.test( table(clinical$Status, clinical$subtype), workspace = 2e8 )
### La probabilite de se tromper si on rejette H0 est de p=0.02742. Alors on rejette H0, donc il y a association entre ces deux variables (status et subtype)

### 3) reference.sample.batch.ID et overall.survival (qualitative/quantitative)
clinical [ , c("reference.sample.batch.ID", "Overall.survival")]
boxplot( clinical$Overall.survival ~ clinical$reference.sample.batch.ID)
points( clinical$Overall.survival ~ clinical$reference.sample.batch.ID)
# pour faire les tests de comparaison de moyenne, on ne prend pas en compte la modalité CDB de la variable reference.., pour laquelle 1 seule valeur de survie est présente dans le jeu de données. Insuffisant pour le test (facteur 1/(n-1), donc valeur non définie pour n=1)
pairwise.t.test( clinical$Overall.survival[ clinical$reference.sample.batch.ID!="CDB" ], droplevels(clinical$reference.sample.batch.ID[ clinical$reference.sample.batch.ID!="CDB" ]))
### conclusion à compléter

### 4) histology et ER.status (qualitative/qualitative)
#table(clinical$ER.status)
#table(clinical$Histology)
table(ER_status=clinical$ER.status, Histo=clinical$Histology)
barplot( table(clinical$ER.status, clinical$Histology), legend=T, xlab="Histology")
#il faut ici éliminer les catégories d'Histologie pour lesquelles les patients n'ont pas de données de ER.status
chisq.test( table( droplevels(as.factor(clinical$ER.status[ clinical$Histology!="Fibroadenoma" & clinical$Histology!="Normal breast" ])), droplevels(clinical$Histology[ clinical$Histology!="Fibroadenoma" & clinical$Histology!="Normal breast" ])) )
fisher.test( table( droplevels(as.factor(clinical$ER.status[ clinical$Histology!="Fibroadenoma" & clinical$Histology!="Normal breast" ])), droplevels(clinical$Histology[ clinical$Histology!="Fibroadenoma" & clinical$Histology!="Normal breast" ])) )
### La probabilite de se tromper si on rejette H0 est de 90% (p=0,9079). Alors on accepte H0, donc il n'y a pas d'association entre ces deux variables (histology et ER.status).

### 5) tumor size et subtype (qualitative/qualitative)
#table(clinical$subtype)
#table(clinical$tumor.size.)
table(tumor.size.=clinical$tumor.size., subtype=clinical$subtype)
barplot( table(clinical$tumor.size., clinical$subtype), legend=T, xlab="subtype")
chisq.test( table(clinical$tumor.size, clinical$subtype) )
fisher.test( table(clinical$tumor.size, clinical$subtype) )
### La probabilite de se tromper si on rejette H0 est de 65% (p=0,6495). Alors on accepte H0, donc il n'y a pas d'association entre ces deux variables (tumor size et subtype). On constate ce resultat d'acceptation de H0 dans les deux tests (Fisher et Chi2)
