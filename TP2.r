
#==================Question 1================================
#install library

library("plot3D")


library("scatterplot3d") 

#lire les donnees du fichier
data = read.table("https://github.com/soukainaMajdoubi/analyse-de-donn-es/blob/master/data1TP2.txt", header = TRUE, sep = "\t", dec = ".")
print(prcomp(data))

#recuperer les donnees de chaque colonne
X <-data$Stature
Y <-data$Poids
Z <-data$Taille

#tracer en dimension  3 le nuage de 10 points
scatterplot3d(X, Y, Z,xlab="Stature",ylab="Poids",zlab="Taille")
scatter3D(X,Y,Z, xlab="Stature",ylab="Poids",zlab="Taille")
#===================Question 2================================

#ecrire le tableau centre B
B <- function(data){
  mean <- c()
  #calculer la moyenne de chaque colonne
  for (i in 1:3){
    col<-data[,i]
    m <- sum(col)/length(col)
    mean[i]<-m
}   
  
  print(mean)
  
  #calculer la valeur de chaque element du tableau B
  res <- matrix(NA,nrow=10,ncol=3,byrow = TRUE)
  for(i in 1:10){
    for(j in 1:3){
      re= (data[i,j]-mean[j])
      res[i,j]<- re
    }
  }
  print(res)
}
#appeler la fonction B
B(data)
#on peut aussi utiliser la fonction scale()
B=scale(data,scale=FALSE)
print(B)

#ecrire la matrice de covariance V
V = cov(data)
print(V)
#===================Question 3================================
#Diagonalisation
E=eigen(V)
#les valeurs propres mesurent la quantite de variance explique par chaque axe principal.
#Les valeurs propres sont grandes pour les premiers axes et petits pour les axes suivants.
#Autrement dit, les premiers axes correspondent aux directions portant la quantite maximale
#de variation contenue dans le jeu de donn?es.
#valeurs propres de V:
valeurV=E$values
#vecteurs propres de V
vecteurV=E$vectors

print(valeurV)
print(vecteurV)
#===================Question 4================================
#Nous examinons les valeurs propres pour determiner le nombre d'axes principaux ? 
#on a la valeurV[1]>valeurV[2]>valeurV[3]  (97>22>6) 
#les axes principaux sont les colonnes des vecteurs propres de V par l'ordre:
#- 1er axe = colonne 1 (le plus important)
#- 2eme axe = colonne 2
#- 3eme axe = colonne 3
#===================Question 5================================
#generer le tableau C qui contient les coordonnees des individus:
C1=B %*% vecteurV
#verifier avec la fonction princomp(A)$scores
C2=princomp(data)$scores
#C1=C2
print(C1)
print(C2)

#===================Question 6================================
#tracer le nuage de points
scatter3D(X,Y,Z, xlab="Stature",ylab="Poids",zlab="Taille")
#tracer le premier axe principal
scatter3D(x = c(0,-300*vecteurV[1,1]),y=c(0,-300*vecteurV[2,1]),z=c(0,-300*vecteurV[3,1]),add=TRUE,type="l")

#===================Question 7================================
#On represente les individus sur le premier plan factoriel (axes 1 et 2)
plot(C1[,1],C1[,2],xlab = "Comp.1",ylab = "Comp.2",type="n")
text(C1[,1],C1[,2],as.character(1:10))
abline(h=0, lty=2)
abline(v=0, lty=2)

#===================Question 8================================

#Les résultats obtenus semblent cohérents.

