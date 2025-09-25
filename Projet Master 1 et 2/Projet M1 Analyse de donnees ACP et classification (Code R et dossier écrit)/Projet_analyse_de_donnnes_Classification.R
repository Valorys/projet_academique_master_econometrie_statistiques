library(readxl)
Donnée<-read_xlsx(file.choose())

colnames(Donnée)
names(Donnée)[names(Donnée) == "Dépenses publiques\r\n\r\n"] <- "Dépenses publiques"
names(Donnée)[names(Donnée) == "Infirmiers et personnels obstétriques\r\n\r\n"] <- "Infirmiers et personnels obstétriques"
names(Donnée)[names(Donnée) == "Espérance de vie à la naissance\r\n"] <- "Espérance"
names(Donnée)[names(Donnée) == "Eau\r\n"] <- "Eau"
colnames(Donnée)
BaseA<-na.omit(Donnée)

library(cluster)
library(FactoMineR)
library(factoextra)
library(NbClust)
library(VIM)

palet <- c("#1B9E77" ,"#D95F02" ,"#7570B3","#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")

BaseA<-as.data.frame(BaseA)
rownames(BaseA)<-BaseA[,2]
mortalite<-BaseA[,-c(1,2)]
summary(mortalite)
boxplot(mortalite)

res.pca <- PCA (mortalite,scale.unit=TRUE)
res.pca$eig

res<-barplot(res.pca$eig[,2],xlab="Dim.",ylab="Percentage of variance")
par(mfrow=c(1,2))
plot(res.pca,choix="var")
plot(res.pca,choix="ind",cex=0.8)

diag(var(mortalite))

# classification des pays
mortalite.cr <- scale(mortalite,center=TRUE,scale=TRUE)     
mortalite.d  <- dist(mortalite.cr,method="euc")         
mortalite.hca <- hclust(mortalite.d,method="ward.D2")       
barplot(rev(mortalite.hca$height)[1:30],xlab="Agg.",ylab="Delta I intra") #rev pour reverse on part de la derniere agrégation jusqu'à la première

x11()
barplot(rev(mortalite.hca$height)[1:30],xlab="Agg.",ylab="Delta I intra")
plot(mortalite.hca,hang=-1,cex=0.8)
#Combien de classes retenir sur la base de l'évolution du critère / sur la base de NbClust 
mortalite.NbClust<-NbClust(data=mortalite.cr,distance="euclidean",method="ward.D2")  

par(mfrow=c(1,1))

K=3
part<-cutree(mortalite.hca,K)

plot(mortalite.hca,hang=-1,cex=0.8)
rect.hclust(mortalite.hca,k=K)

part_fviz <- hcut(mortalite.d,stand=FALSE,k=K,isdiss=TRUE,hc_func="hclust",hc_method="ward.D2")
fviz_dend(part_fviz, rect = TRUE, cex = 0.5,k_colors =palet ,main=paste("Dendrogramme avec ",K," classes"))

part.cah <- cutree(mortalite.hca,k=K)
print("Partition obtenue par la CAH")
part.cah

print("Barycentres de la partition obtenue par la CAH")
bary.cah <- aggregate(mortalite.cr,by=list(part),mean)   
round(bary.cah,2)

# Qualité de la partition obtenue
# sum of squares
ss <- function(x) sum(scale(x, scale = FALSE)^2)
part.cah.ss <- ss(mortalite.cr)
# within sum of squares function somme des carrés intra pour chacune des classes
wss <- function(part,x) {li <- by(1:length(part),as.factor(part),FUN=list);
sum(unlist(lapply(li, function(g){ ss(x[g,])})))}
# within sum of squares
part.cah.wss <- wss(part,mortalite.cr)
paste('qualité de la partition égale à :',round(1-part.cah.wss/part.cah.ss,2))

# Consolidation avec les kmeans
res.kmeans<-kmeans(mortalite.cr, centers=bary.cah[,-1],algorithm="MacQueen")
res.kmeans
res.kmeans$iter

paste('qualité de la partition égale à :',round(res.kmeans$betweenss/res.kmeans$totss,2))

#6.1 Partition obtenue par les kmeans
part<- res.kmeans$cluster
table(part)

# plus proche individu du centre
for (h in 1:K) {
  li <- which(res.kmeans$cluster==h)
  if (length(li)==1)
    print(paste('Singleton',h,':',names(res.kmeans$cluster[li])))
  else {
    parangon <- which.min(as.matrix(dist(rbind(res.kmeans$centers[h,],mortalite.cr[li,])))[-1,1])
    print(paste('plus proche individu du barycentre de la classe',h,':',names(parangon)))
  }
}

#caractériser sur la base des barycentres
round(res.kmeans$centers,2)

# Visualisation des barycentres avec un diagramme en batons
X11()
barplot(res.kmeans$centers,col=palet[1:K],ylim=c(-2,4),beside=T,las=2)
legend(x="topright", legend=paste("C",1:K,sep=""), cex=0.8,fill=palet[1:K],bty="y") 

part <- res.kmeans$cluster

# Interprétation
# Visualisation de la partition comme variable qualitative supplémentaire de l'ACP
acp <- PCA(data.frame(Partition=part,mortalite),scale.unit = TRUE,quali.sup=1) 
rai=data.frame(comp=1:min(10,nrow(acp$eig)),cum=acp$eig[1:min(10,nrow(acp$eig)),3])
round(acp$eig,2)
## Visualisation du cercle des corrélations
choix.axes=c(1,2) 
pvar=fviz_pca_var(acp,axes = choix.axes,repel = TRUE,labelsize =4,arrowsize =1,circlesize=1)+theme_bw()+theme(axis.title = element_text(face="bold",size=10))
print(pvar)

##Visualisation de la partition
x11()
pind=fviz_pca_ind(acp,axes = choix.axes,label="all",repel=TRUE,pointsize =2,habillage = 1,
                  addEllipses = TRUE,palette=palet)+theme_bw()+theme(axis.title = element_text(face="bold",size=15))+coord_fixed()
print(pind)















