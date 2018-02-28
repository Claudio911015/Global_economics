library(tidyverse)
library(ggfortify)
library(caret)
statistics<-read.csv("pokemon.csv") %>% as_tibble()
combates<-read.csv("combats.csv") %>% as_tibble()

combates$Winner<-as.integer(combates$Winner==combates$First_pokemon)

Sample1<-statistics[combates$First_pokemon,]
Sample2<-statistics[combates$Second_pokemon,]

Variable<-tibble(HP=Sample1$HP-Sample2$HP,
                Attack=Sample1$Attack-Sample2$Attack,
                Defense=Sample1$Defense-Sample2$Defense,
                Special_Attack=Sample1$Sp..Atk-Sample2$Sp..Atk,
                Special_Defense=Sample1$Sp..Def-Sample2$Sp..Def,
                Speed=Sample1$Speed-Sample2$Speed,
                Winner=combates$Winner)

ggplot(Variable
       ,aes(x=Attack,y=Special_Attack,color=as.factor(Winner)))+geom_point()+
        facet_grid(Pokemon_1~Pokemon_2)

ggplot(Variable,aes(x=Winner,fill=as.factor(Winner)))+geom_bar()+facet_grid(Pokemon_1~Pokemon_2)

my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
fit <- train(Winner ~ HP + Attack+Defense+Special_Attack+
               Special_Defense+Speed, data = Variable,
             method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = F, linout = 1)

