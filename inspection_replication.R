### Load necessary libraries

library(tidyverse)
library(sandwich)
library(lmtest)
library(extrafont)
library(ggplot2)
library(survey)
library(haven)


### read the dataset

ndijul <- download.file("https://caucasusbarometer.org/downloads/NDI_2018_Jun_16.07.18_Public.dta",
              "NDI_2018_Jun_16.07.18_Public.dta", mode = "wb")

ndijul <- read_dta("NDI_2018_Jun_16.07.18_Public.dta")

## Recode variables. Note that in the original dataset question on the support of technical inspections are given in one variable. For analysis purposes we have grouped them

ndijul <- ndijul %>%
  mutate(q21=case_when(
    TECHINSP == 1 | TECHINSPRC == 1  ~ 1,
    TECHINSP == 0 | TECHINSPRC == 0  ~ 0,
    TRUE ~ NA_real_
    ),
    treat=case_when(
      TECHINSPRC == -4 ~ 1,
      TECHINSP == -4 ~ 0
    ),
    airquality = case_when(
      SATQAIR > -1 ~ SATQAIR,
      TRUE ~  NA_real_
    ),
    age = RESPAGE,
    edu = case_when(
      RESPEDU > 4 & RESPEDU < 7 ~ 1,
      TRUE ~ 0
      ),
    male = case_when(
      RESPSEX == 1 ~ 1,
      TRUE ~ 0
    ),
    carown = case_when(
      OWNCARS == 1 ~ 1,
      TRUE ~ 0
    ),
    party=factor(case_when(
      PARTYSUPP1 == 8 ~ 1,
      PARTYSUPP1 == 6 ~ 2,
      PARTYSUPP1 == 25 ~ 4,
      PARTYSUPP1 %in% c(-1, -2) ~ 5,
      TRUE ~ 3
    ), levels=c(1, 2, 3, 4, 5), labels=c("GD", "UNM", "Other", "None", "DK/RA")),
    settype=factor(SETTYPE, levels=c(1:4), labels = c("Capital", "Urban", "Rural", "Ethnic minorities")),
    aq=factor(airquality, levels=c(1:4), labels =c("Completely dissatisfied", "Somewhat dissatisfied", "Somewhat satisfied", "Completely satisfied"))
    )

### Check heterogeneous TEs:
### Air quality

prop.table(xtabs(WTIND~q21+aq, data=ndijul), 2)

mod_1 <- glm(q21~ carown+age+male+edu+settype+party+treat*aq,
              data=ndijul, family="binomial")

confint(mod_1)

coeftest(mod_1, vcov = vcovHC(mod_1))

exp(mod_1$coefficients)

pred_treat_aq <- ggpredict(mod_1, c("treat", "aq"))
plot(pred_treat_aq)

prop.table(xtabs(WTIND~q21+aq, data=ndijul[ndijul$treat==1, ]), 2)
prop.table(xtabs(WTIND~q21+aq, data=ndijul[ndijul$treat==0, ]), 2)

prop.table(xtabs(WTIND~q21+aq, data=ndijul), 2)

### carown

mod_2 <- glm(q21~ treat*carown+age+male+edu+settype+party+aq,
             data=ndijul, family="binomial")

confint(mod_2)

coeftest(mod_2, vcov = vcovHC(mod_2))
exp(mod_2$coefficients)

prop.table(xtabs(WTIND~q21+aq, data=ndijul[ndijul$treat==1, ]), 2)
prop.table(xtabs(WTIND~q21+aq, data=ndijul[ndijul$treat==0, ]), 2)

pred_treat_car <- ggpredict(mod_2, c("treat", "carown"))
plot(pred_treat_car)

### settype

mod_3 <- glm(q21~ carown+age+male+edu+treat*settype+party+aq,
             data=ndijul, family="binomial")

confint(mod_3)

coeftest(mod_3, vcov = vcovHC(mod_3))
exp(mod_3$coefficients)

pred_treat_set <- ggpredict(mod_3, c("treat", "settype"))
plot(pred_treat_set)

### party

mod_4 <- glm(q21~ carown+age+male+edu+settype+treat*party+aq,
             data=ndijul, family="binomial")

confint(mod_4)

coeftest(mod_4, vcov = vcovHC(mod_4))
exp(mod_4$coefficients)

pred_treat_party <- ggpredict(mod_4, c("treat", "party"))
plot(pred_treat_party)


### Make party chart

#### Define survey design

ndijul$psu[ndijul$psu == 302038] <- 302039
ndijul$q21[ndijul$q21 == -3] <- NA


ndi<-svydesign(id=~psu+id, strata=~substratum, 
               weights=~indwt, fpc=~npsuss+nhhpsu, 
               data=ndijul)


#### Make party crosstab

q21.party <- svyby(~q21, ~party, design=ndi, svymean, na.rm=TRUE)

#### Define default theme

theme_plot <- theme(
  axis.text.y = element_text(colour="black", size = 12, family = "Gill Sans MT"),
  axis.text.x = element_text(colour="black", size = 12, family="Gill Sans MT"),
  axis.title.x = element_text(size=12, family = "Helvetica-Normal"),
  axis.title.y = element_text(size=12, family = "Helvetica-Normal"),
  strip.text  = element_text(size=12, family = "Helvetica-Normal"),
  panel.border = element_rect(fill=NA, linetype = "solid", colour = "black"),
  panel.background = element_rect(fill = NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(hjust = 0.5, colour = "Black", size=12, family = "Helvetica-Normal"),
  plot.subtitle = element_text(hjust = 0.5, colour = "Black", size=12, family = "Helvetica-Normal"),
  legend.title = element_text(size=10, family = "Helvetica-Normal"),
  legend.text = element_text(size=12, family = "Gill Sans MT"),
  plot.caption = element_text(size=10, family = "Gill Sans MT")
)

#### Make chart

ggplot(q21.party, aes(x=party, y=q21 ))+
  geom_col(stat="identity", position="dodge", aes(fill=party))+
  labs(title="Is it necessary or unnecessary\nto introduce technical inspections for cars?\nBy Party affiliation (%)",
       subtitle="(CRRC/NDI Survey, June 2018)",
       caption="95% confidence intervals are reported")+
  geom_text(aes(x=party, y=q21,label=ifelse(q21 > 0.015, sprintf("%0.f", round(q21*100, digits = 0)), "")), family="Helvetica-Normal", position=position_dodge(width = 0.1), vjust=-1)+
  scale_fill_manual(values=c("#8da0cb","#fc8d62","#66c2a5","#a6d854", "#999999"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  geom_errorbar(data=q21.party, aes(ymin=q21-1.96*se, ymax=q21+1.96*se), position=position_dodge(width = 0.9), width=0.3)+
  theme_plot+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    legend.direction = "horizontal"
  )


ggsave("party_crosstab.png", height=100, width=150, unit="mm")

### Cross-tabulation by settlement

q21.settype <- svyby(~q21, ~settype, design=ndi, svymean, na.rm=TRUE)
