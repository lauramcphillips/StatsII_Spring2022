##########################
#### Replication Code
##########################

install.packages("stargazer")
library(stargazer)
library(foreign)
raw.data<-read.dta("potter_tavits_data.dta")
View(raw.data)

# drop outliers
campaigns <- subset(raw.data, postenp < 9.2)
View(campaigns)
# create post-1974 subset for endogeneity test
later1974<-subset(campaigns, demin>1973)
View(later1974)

# estimate Model 1 
library(arm)
full<-lm(postenp ~ fundparity4
         + demyears
         + fed 
         + pres 
         + log(avemag) 
         + fract 
         + log(avemag):fract, 
         data=campaigns)
display(full)	

# estimate Model 2
post1974<-lm(postenp ~ fundparity4
             + demyears
             + fed 
             + pres 
             + log(avemag) 
             + fract 
             + log(avemag):fract, 
             data=later1974)	
display(post1974)


stargazer(full, post1974)

# construct the endogeneity plot in Figure 1
plot(campaigns$fundparity4 
     ~ campaigns$preenp, 
     pch=20, col="grey20", cex=1.5, 
     xlab="Previous ENP", 
     ylab="Current Fund Parity Value")
display(lm(fundparity4 ~ preenp, data=campaigns))
abline(a=0.82, b=-0.04, lwd=2)	

#Europe vs not Europe
not_europe <- subset(campaigns, subset = !(cnty %in% c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France","Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania","Luxembourg","Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")))
View(not_europe)

europe <- subset(campaigns, subset = cnty %in% c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France","Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania","Luxembourg","Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden"))
View(europe)

europe_mod <- lm(postenp ~ fundparity4
                 + demyears
                 + fed 
                 + pres 
                 + log(avemag) 
                 + fract 
                 + log(avemag):fract, 
                 data=europe)
display(europe_mod)	

noteurope_mod <- lm(postenp ~ fundparity4
                    + demyears
                    + fed 
                    + pres 
                    + log(avemag) 
                    + fract 
                    + log(avemag):fract, 
                    data=not_europe)
display(noteurope_mod)
stargazer(europe_mod, noteurope_mod)

