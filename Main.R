library(tidyverse)
library(caret)
statistics<-read.csv("pokemon.csv") %>% as_tibble()
combates<-read.csv("combats.csv") %>% as_tibble()
test<-read.csv("tests.csv") %>% as_tibble()
combates$Winner<-as.integer(combates$Winner==combates$First_pokemon)

Sample1<-statistics[combates$First_pokemon,]
Sample2<-statistics[combates$Second_pokemon,]

Variable<-tibble(HP=Sample1$HP-Sample2$HP,
                Attack=Sample1$Attack-Sample2$Attack,
                Defense=Sample1$Defense-Sample2$Defense,
                Special_Attack=Sample1$Sp..Atk-Sample2$Sp..Atk,
                Special_Defense=Sample1$Sp..Def-Sample2$Sp..Def,
                Speed=Sample1$Speed-Sample2$Speed,
                Winner=combates$Winner,
                Legend=Sample1$Legendary)
Variable %>% 
ggplot(aes(x=Speed,y=Defense,color=factor(Winner)))+geom_point()

ggplot(Variable,aes(x=Winner,fill=as.factor(Winner)))+geom_bar()

model<-glm(Winner ~ HP+Attack+Defense+Special_Attack+Special_Defense+Speed, 
           data = Variable, family = binomial(link='logit'))

summary(model)
new<-data.frame(Winner=Variable[-"Winner"])

fitted.results <- predict(model,newdata=subset(Variable,select=-Winner),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
table(Variable$Winner,fitted.results)
Test1<-statistics[test$First_pokemon,]
Test2<-statistics[test$Second_pokemon,]

Variable2<-tibble(HP=Test1$HP-Test2$HP,
                 Attack=Test1$Attack-Test2$Attack,
                 Defense=Test1$Defense-Test2$Defense,
                 Special_Attack=Test1$Sp..Atk-Test2$Sp..Atk,
                 Special_Defense=Test1$Sp..Def-Test2$Sp..Def,
                 Speed=Test1$Speed-Test2$Speed,
                 Legend=Test1$Legendary)
test$c<-as.integer(test$c==test$a)
test.results <- predict(model,newdata=Variable2,type='response')
test.results <- ifelse(test.results > 0.5,1,0)
table(test$c,test.results)


