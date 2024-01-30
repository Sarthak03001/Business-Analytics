library(tidyverse)
library(MASS)
library(ISLR)
library(ISLR2)

data("Auto")
Auto%>%
  ggplot(
    aes(x= horsepower, y= weight))+
  geom_point()+
  geom_smooth(
    method = "lm"
  )

lm(formula = Balance~Income, data = Credit)
Credit%>%
  ggplot(aes(x = Balance, y = Income))+
  geom_point()+
  geom_smooth(
    method = "lm"
  )



lmv<-Credit |>
  mutate(
    Student_category = case_when(
      Student == "Yes" ~ "1",
      Student == "No" ~ "0"
    )
  )

Credit<-mutate(Credit,
               Student_category = case_when(
                 Student == "Yes" ~ "1",
                 Student == "No" ~ "0"
               )
)
Credit
lmv
?Credit
credit_1<-select(Credit, )
new_nmod<-lm(formula = Balance~Income+ Student_category + Income*Student_category, data = Credit)
broom::tidy(new_nmod)
broom::glance(new_nmod)



## make a dummy for default

Default|>
  mutate(
    default_dumm = ifelse(
      default == "Yes",
      1,0
    )
  )-> def_dum

## regress dummy over balance and plot 

lm(default_dumm ~ balance, 
   data = def_dum)|>
  broom::augment()|>
  ggplot(aes(balance,default_dumm))+
  geom_point(alpha= 0.6)+
  geom_line(aes(balance, .fitted),
            colour = "red")+
  labs(
    title = "Linear regression fit to qualitative response",
    subtitle = "Yes =1, No = 0",
    y = "prob default status"
  )+
  theme_minimal() -> plot_linear

## Run the logistic regression

glm(
  default_dumm ~ balance,
  data = def_dum,
  family = binomial
)|>
  broom::augment(type.predict = "response")|>
  ggplot(aes(balance,default_dumm))+
  geom_point(alpha= 0.6)+
  geom_line(aes(balance, .fitted),
            colour = "red")+
  labs(
    title = "Logistic regression fit to qualitative response",
    subtitle = "Yes =1, No = 0",
    y = "prob default status"
  )+
  theme_minimal() -> logistic_plot

Credit<-mutate(Credit,
               Student_category = case_when(
                 Student == "Yes" ~ "1",
                 Student == "No" ~ "0"
               )
)
Credit


Default<-mutate(Default, 
                default_dumm = ifelse(
                  default == "Yes",
                  1,0
                ))
Default
names(Default)
px1<-2.7183^(-10.651330614	+ 5000*0.005498917)/(1-px)
px
2.7182818284590452353602874713527^(-10.651330614	+ 5000*0.005498917)/(1+2.7182818284590452353602874713527^(-10.651330614	+ 5000*0.005498917))





Credit<-
  mutate(Credit,
         mark_south = ifelse(
           Region == "South",
           1,0),
         mark_west = ifelse(
           Region == "West",
           1,0),
         mark_east = ifelse(
           Region == "East",
           1,0)
         
  )
Credit

glm(
  mark_south ~ Income + Balance + Student_category,
  data = Credit,
  family = binomial
) -> mod_logit
mod_logit
DescTools::PseudoR2(mod_logit,
                    which = c("McFadden", "CoxSnell",
                              "Nagelkerke", "Tjur"))

glm(
  mark_west ~ Income + Balance + Student_category,
  data = Credit,
  family = binomial
) -> mod_logit1
mod_logit1
DescTools::PseudoR2(mod_logit1,
                    which = c("McFadden", "CoxSnell",
                              "Nagelkerke", "Tjur"))
glm(
  mark_east ~ Income + Balance + Student_category,
  data = Credit,
  family = binomial
) -> mod_logit2
mod_logit2
DescTools::PseudoR2(mod_logit2,
                    which = c("McFadden", "CoxSnell",
                              "Nagelkerke", "Tjur"))
