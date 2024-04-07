
# data
gitLink="https://vincentarelbundock.github.io/Rdatasets/csv/carData/Cowles.csv"
vol=read.csv(gitLink)[,-1] #no first column

# formatting:
vol[,c(3,4)]=lapply(vol[,c(3,4)],as.factor)

### semilla
set.seed(2019)

### first hypothesis
h1=formula(volunteer~sex)

#regression
rlog1=glm(h1, data=vol,family = binomial)
summary(rlog1)


# la representación numérica
as.numeric(as.factor(c("no","yes")))

vol$IsVolunteer=ifelse(vol$volunteer=="yes",T,F)
vol$IsVolunteer_1=as.integer(ifelse(vol$volunteer=="yes",1,0))
vol$IsVolunteer_claro=as.factor(ifelse(vol$volunteer=="yes",
                             "claro","nunca"))
# table(vol$IsVolunteer) 
# table(vol$IsVolunteer_1) 
# table(vol$IsVolunteer_claro) 

h1b=formula(IsVolunteer~sex)
h1c=formula(IsVolunteer_1~sex)
h1d=formula(IsVolunteer_claro~sex)

coef(glm(h1b, data=vol,family = binomial))

coef(glm(h1c, data=vol,family = binomial)) # igual al anterior

coef(glm(h1d, data=vol,family = binomial)) # cambio de signo

# cambio de referencia - la ref NO la vemos
vol$sex=relevel(vol$sex,ref = "male")

#regresion
rlog1=glm(h1, data=vol,family = binomial)
summary(rlog1)


sexF=coef(rlog1)["sexfemale"]
exp(sexF)

### second hypothesis
h2=formula(volunteer~sex + neuroticism)
rlog2=glm(h2, data=vol,family = binomial)
summary(rlog2)

h3=formula(volunteer~sex + neuroticism + extraversion)
rlog3=glm(h3, data=vol,family = binomial)


# como leer?
coef(rlog3)["sexfemale"] # no
exp(coef(rlog3)["sexfemale"]) # sí  / menos decimales: round(exp(coef(rlog3)["sexfemale"]),4)

abs(1-exp(coef(rlog3)["sexfemale"])) # abs(exp(coef(rlog3)["sexfemale"])-1)

# como sex es positivo:
100*abs(1-exp(coef(rlog3)["sexfemale"]))

# extraversion:
100*round(abs(1-exp(coef(rlog3)["extraversion"])),4)

# cual afecta más
vol$sexFem_num=ifelse(vol$sex=='male',0,1) # 1 es para mujer
sdVIs=apply(vol[,c("sexFem_num","neuroticism", "extraversion")],2,sd)
DF=list(LogitSt=sdVIs*coef(rlog3)[c(2,3,4)])%>%
       data.frame()

DF # DF tiene los coeficientes estandarizados

library(lmtest)
# mejor modelo
lrtest(rlog1,rlog2, rlog3) 

library(ggplot2)
dotwhisker::dwplot(list(Logit_I=rlog1,Logit_II=rlog2,Logit_III=rlog3),
                   exp=T) + #exponenciados!
            scale_y_discrete(labels=c("extraversion",
                                      "neuroticism",
                                      "sex (ref: male)")) +
            scale_color_discrete(name="Modelos para:\nSer Voluntario") +
            geom_vline(xintercept = 1,
                       colour = "grey60",
                       linetype = 2)

predictedProbs <- predict(rlog3, vol,type = "response")
# veamos algunos de ellos
head(predictedProbs)

vol$predicted=ifelse(predictedProbs>0.5,"yes","no")

head(vol[,c("predicted","volunteer")],10)


library(cvms)
ConfMatrix=confusion_matrix(predictions =  vol$predicted,
                      targets= vol$volunteer)
library(cvms)
plot_confusion_matrix(ConfMatrix,
                      class_order=c("yes","no"))

# La **Sensitividad** (VP/(VP+FN)): 
# Utilidad del modelo para determinar el "voluntario" 
# (modelo predice a un voluntario que realmente era voluntario en la data original').

ConfMatrix$Sensitivity

# La **Especificidad** (VN/(VN+FP)): 
# Utilidad del modelo para determinar el "no voluntario" 
# (modelo predice a un no voluntario que realmente no era voluntario en la data original').

ConfMatrix$Specificity


# Este índice vale **1** si la predicción es perfecta, 
# y **0** si fallan todas las predicciones.
ConfMatrix$Kappa

ToInput1 <- data.frame(sex=factor(c("female")), 
                      neuroticism=13, extraversion=8)
predict(object = rlog3, newdata = ToInput1, type = "response")

ToInput2 <- data.frame(sex=factor(c("female","male")), 
                      neuroticism=13, extraversion=8)
predict(object = rlog3, newdata = ToInput2, type = "response")

ToInput3 <- data.frame(sex=factor(c("female","female")), 
                      neuroticism=c(13,10), extraversion=c(8,21))
predict(object = rlog3, newdata = ToInput3, type = "response")

head(vol$predicted)


# interpracion usando marginal effects:
library(margins)
# 
margins(rlog3)


summary(margins(rlog3))

library(ggplot2)
base= ggplot(marginalsData,aes(x=factor, y=AME)) + geom_point()
base +  geom_errorbar(aes(ymin=lower, ymax=upper))


