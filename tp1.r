#===================QUESTION-1====================================================
#lire les donnees du fichier

data = read.table("data1TP1.txt", header = TRUE, sep = "\t", dec = ".")

#tracer en dimension 2

#Bas? sur cinq graphiques ci-dessous (les diagrammes de corr?lation), 
#on peut savoir que s'il existe une relation entre deux caract?res.
#Chaque ?l?ment i sur le diagramme est repr?sent? par le point de coordonn?es (Xi,Yi). 
#L'ensemble des points forme un nuage de points dont la forme permet de caract?riser la relation ? l'aide de trois crit?res : 
#- intensit? de la relation 
#- forme de la relation 
#- sens de la relation

#Variable A et l'?tiquette Y :
plot(data[,1],data[,6],xlab="A",ylab="Y",col="red")
#- Le nuage de point prend la forme d'une ligne ou d'une courbe dont les points s'?cartent peu. 
#- Lorsque la valeur de A augmente, la valeur de Y diminue (et inversement, lorsque A diminue, Y augmente)

#Variable B et l'?tiquette Y :
plot(data[,2],data[,6],xlab="B",ylab="Y",col="blue")
#- Le nuage de point prend la forme d'une ligne ou d'une courbe dont les points s'?cartent peu. 
#- Lorsque la valeur de B augmente, la valeur de Y augmente ?galement (et inversement, lorsque B diminue, le Y diminue)

#Variable C et l'?tiquette Y :
plot(data[,3],data[,6],xlab="C",ylab="Y",col="red")
#- Le nuage de point n'a pas de forme d?finie.
#- C et Y ne sont pas li?es l'une ? l'autre.

#Variable D et l'?tiquette Y :
plot(data[,4],data[,6],xlab="D",ylab="Y",col="black")
#- Le nuage de point prend la forme d'une courbe.
#- Lorsque la valeur de D augmente, la valeur de Y augmente ?galement (et inversement, lorsque D diminue, le Y diminue)

#Variable E et l'?tiquette Y :
plot(data[,5],data[,6],xlab="E",ylab="Y",col="green")
#- Le nuage de point prend la forme d'une courbe dont les points s'?cartent peu.
#- Lorsque la valeur de E augmente, la valeur de Y peut augmenter ou diminuer.

#===================QUESTION-2====================================================
#ecrire une fonction pour calculer le coefficient de Pearson
pearson <-function(data){
  for (i in 1:5){
    X<-data[,i]
    Y<-data[,6]
    r=cov(X,Y)/(sd(X)*sd(Y))
    print(r)
    
    #verifier avec cor(X,Y)
    print(cor(X,Y))
  }
}
#appeler la fonction Pearson
pearson(data)

#Variable A a la plus petite corr?lation, ?gale ? - 0.9722452 (rA <0). 
#La variable A est la seule variable parmi 5 variables ayant la relation n?gative 
#(les deux caract?res varient en sens inverse, c'est ? dire que lorsque la valeur de A augmente,
#la valeur de Y diminue (et inversement, lorsque A diminue, Y augmente)). 
#De plus,  rA est proche de -1,  c'est ? dire qu'il existe une forte relation lin?aire n?gative entre A et Y.

#===================QUESTION-3====================================================
#ecrire une fonction pour calculer le coefficient de Spearman
spearman <-function(data){
  for (i in 1:5){
    X<-data[,i]
    Y<-data[,6]
    N<-length(X)
    S<-0
    rangX<-rank(X)
    rangY<-rank(Y)
    
    for (j in 1:N){
      R=rangX[j]-rangY[j]
      S=S + R*R
    }
    re = 1-(6*S)/(N*N*N-N)
    print(re)
    #verifier avec cor()
    print(cor(X,Y,method = "spearman"))
  }
}

#appeler la fonction Spearman
spearman(data)

#La diff?rence entre deux m?thodes:
#- La corr?lation de Pearson ?value la relation lin?aire entre deux variables continues. 
#  Une relation est dite lin?aire lorsqu'une modification de l'une des variables est associ?e ? une modification proportionnelle de l'autre variable.
#- La corr?lation de Spearman ?value la relation monotone entre deux variables continues ou ordinales. 
#  Dans une relation monotone, les variables ont tendance ? changer ensemble, mais pas forc?ment ? une vitesse constante. 
#  Le coefficient de corr?lation de Spearman est fond? sur les valeurs class?es pour chaque variable plut?t que sur les donn?es brutes.
#Par exemple: 
#Dans la relation entre la variable D et Y, une variable augmente lorsque l'autre augmente,
#mais que cette augmentation n'est pas constante, le coefficient de corr?lation de Pearson est positif mais inf?rieur ? +1 (?gale ? 0.7513686). 
#Dans ce cas, le coefficient de Spearman est lui toujours ?gal ? +1.

#===================QUESTION-5====================================================


#extraire les données 
  data = read.table("data2TP1.txt",header=TRUE,sep="\t",dec=".")
#Diviser les données
  mar <-data[,1]
  aix <-data[,2]
#calculer la  moyenne 
  m1 <-mean(mar)
  m2 <-mean(aix)
#calculer le score  
  t=abs(m1-19)/(sd(mar)/sqrt(length(mar)))
  print(t)
  #Pour un degré de liberté (n-1) = 14 et un risque alpha fixé à 5% la valeur critique 
  #correspondante sur la table est : 2,145
  #On pose l'hypothèse H0 :
  #on a t= 2,177369 > 2,145  donc on rejette l’hypothèse 

#===================QUESTION-6====================================================
  t2=abs(m1-m2)/ sqrt( (sd(mar)^2 / length(mar)) + (sd(aix)^2 / length(aix)) )
  
  print(t2)
  #avec un degré de liberté  (n1+n2 -2) = 28 et  alpha = 0,05 . La valeur critique 
  #Correspondante est  2,048
  #on a t= 2.321494 > 2,048  donc on rejette l'hypothèse H0
#Pour p= 2% la valeur critique est égale à : 2,46 donc on accepete  l'hypothese car avec P l intervale d acceptation est plus grand et plus précis 

#===================QUESTION-7====================================================

  n=0
  tableau <- c(1528,106,117,381)
  for(i in 1:length(tableau)){
    n=n+tableau[i]
  }
  print(n)
  vtVL=(9/16)*n
  print(vtVL)
  vtVR=(3/16)*n
  print(vtVR)
  vtRL=(3/16)*n
  print(vtRL)
  vtRR=(1/16)*n
  print(vtRR)
  valeaurT <- c(vtVL,vtVR,vtRL,vtRR)
  
  k2=0
  for(i in 1:length(tableau)){
    k2 = k2 + ((tableau[i]-valeaurT[i])^2/valeaurT[i])  
    print(k2)
  }
  #On obtient 966,61, pour un degré de liberté de 3, et alpha 5% on devrait avoir 7.81
  #il ya une grande difference donc les valeurs de ratios ne sont pas bonnes
  
  
  #===================QUESTION-8====================================================  
#Test d'indépendance pour variables qualitatives
  
x2=0
#creer une matrice avec les données de forms 
vlt <- rbind (c(29,5,46), c(40,32,8), c(18,22,0))
#faire la somme de ces valeurs 
n <- sum(vlt)
print(n)

#la somme de chaque ligne et chaque colonne 
res <- apply(vlt, 1,sum )
resc <- apply(vlt , 2, sum)

#calculer les valeurs théoriques
v <- c()
for (i  in 1:length(res) ) {
  for(j in 1:length(resc)){
    r=(res[i]*resc[j])/n
    v <- c(v,r) 
  }
  
}
print(v)
print(vlt)
#calculer khi2
k=1
k2=0
for (i  in 1:3 ) {
  for(j in 1:3){
    k2 = k2 + ((vlt[i,j]-v[k])^2/v[k])  
    k=k+1
    print(k2)
  }
}
######### Color 

clr<- rbind (c(20,60), c(29,51), c(12,28))
print(clr)
n <- sum(clr)
print(n)
resc <- apply(clr, 1,sum )
rescc <- apply(clr , 2, sum)
vc <- c()
for (i  in 1:length(resc) ) {
  for(j in 1:length(rescc)){
    r=(resc[i]*rescc[j])/n
    vc <- c(vc,r) 
  }
  
}
print(vc)
k=1
k2=0
for (i  in 1:3 ) {
  for(j in 1:2){
    k2 = k2 + ((clr[i,j]-vc[k])^2/vc[k])  
    k=k+1
    print(k2)
  }
}
#forms : avec k=9 et alpha 5%  X^2= 75,1564 > 15,51 donc on rejette la variable de forme et dépendante donc importante pour détecter un mélanome
#couleur marron claire  : avec k=5 et alpha 5%  X^2= > 2,39 donc on accepte  la variable couleur et indépendante donc nest pas importante pour détecter un mélanome

#===================QUESTION-9====================================================

#Un test paramétrique requiert un modèle à fortes contraintes pour lequel les mesures doivent avoir été 
#réalisées dans une échelle au moins d'intervalle. Ces hypothèses sont d'autant plus difficiles à vérifier
#que les effectifs étudiés sont plus réduits.

#Un test non paramétrique est un test dont le modèle ne précise pas les conditions que doivent remplir les paramètres
#de la population dont a été extrait l'échantillon. Cependant certaines conditions d'application doivent être vérifiées. 
#Les échantillons considérées doivent être aléatoires  et simples et éventuellement indépendants les uns des autres.

#D'après les questions précedentes, le test t de Student pour échantillons indépendants n'est fiable que si les
#données associées à chaque échantillon suivent une distribution normale et si les variances des échantillons sont homogènes.

#===================QUESTION-10====================================================

#les données qualitatives fournissent une information enrichie, approfondie et diversifiée; elles reposent sur quelques
#individus ou quelques cas. Elles sont utiles quand on cherche à expliquer le comment et le pourquoi.
#on ne peut pas affecter des valeurs pour les données qalitatives
#Le coefficient de corrélation de Pearson permet d'analyser les relations linéaires et le coefficient de corrélation 
#de Spearman les relations non-linéaires ce qui permet de savoir s'il existe un lien entre deux variables quantitatives 
#donc on ne peut pas utiliser des données qualitatives.


