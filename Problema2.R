#Codigo para problema 2

mis_dades <- iris  #Recordem, per executar això fem ctrl + Enter
mis_dades


x <- mis_dades$Petal.Length #recordem que el $ el que fa es extreure només la columna que ens interessa
y <- mis_dades$Sepal.Length
#fixemnos que en realitat ens demana veure quina és la correlació entre el pètal i el sèpal ( és a dir que si un creix aviam si l'altre creix o es fa més petit o aviam que passa)
#per veure això fem un plot
plot(x,y)
#amb el plot veiem una clara tendència a que si creix el pètal (dada en x) també creix el sèpal (parlant de la llargada).
#ara ens tocarà fer la recta de regressió per mínims quadrats

#calculem m (el pendent) de la següent forma:

xbarra <- mean(x) #calculem la mitjana de les dades x
ybarra <- mean(y) #el mateix amb la y

#calculem la m amb la formula que ens ha donat el profe -> m= sumatori((xi-xbarra)*(yi-ybarra))/sumatori((xi-xbarra)^2)

m <- sum((x-xbarra)*(y-ybarra))/sum((x-xbarra)^2) #això ja és el resultat de la m

#ara trobem b -> seguim també la formula que dona el profe b = ybarra-m*xbarra

b <- ybarra- m*xbarra

#fem ara l'última pregunta (predicció quan Petal.Length = 1.5)

m*1.5+b

#també podem fer:

x_pred <- x #així ho estem fent de tots elvalors
y_pred <- m*x_pred+b

plot(x,y)
lines(x_pred,y_pred)

#amb això fem una linia de predicció feta per els valors del 0 al 10

#ara volem una mesura de com de bona és aquesta predicció --> per a això trobarem el "coeficient de determinació" que és allò  de R^2 que surt a les regressions lineals de google i tal

Rquadrat <- sum((y_pred-ybarra)^2)/sum((y-ybarra)^2)
Rquadrat

#aixo és el coeficient de determinació, amb el qual podem tobar el "coeficient de Correlació" (R sense el quadrat)
corr <- sqrt(Rquadrat)
corr


mod <-lm(y~x)

summary(mod)

#això ens calcula varies coses, entre elles el R al quadrat d'abans.

#també podem fer:
cor.test(x,y) #així trobem la correlació

