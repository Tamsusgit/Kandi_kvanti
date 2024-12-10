### Regressio 1

rm(list = ls())

dat_RG <- read.csv("C:/Users/syree/Downloads/Rastegaisa.csv")
dat_SP <- read.csv("C:/Users/syree/Downloads/Singapore_adminareas.csv")
dat_ES <- read.csv("C:/Users/syree/Downloads/ESS7FI.csv")
dat_SW <- read.csv("C:/Users/syree/Downloads/SWI_test.csv")

### 1. tehtävä: kunkin muuttujan keskiarvo, keskihajonta, mediaani, minimiarvo, maksimiarvo sekä havaintomäärä

summary(dat_RG) ## Kaikki
summary (dat_RG$SR_vp)

sd(dat_RG$SR_vp) ## Keskihajonta, standard deviation

## 2. Tehtävä: Histogrammit

hist(dat_RG$SR_vp, breaks=20) ## Default usein 5

## 3. Tehtävä Scatterplotit

## MUISTA selittävä muuttuja ekana !!!

plot(dat_RG$tavg_summer, dat_RG$SR_vp)
plot(dat_RG$elevation, dat_RG$SR_vp)
plot(dat_RG$SWI, dat_RG$SR_vp)

# 4. Tehtävä: Korrelaatiot

# Pearsonit
cor(dat_RG$SR_vp, dat_RG$tavg_summer)
cor(dat_RG$SR_vp, dat_RG$elevation)
cor(dat_RG$SR_vp, dat_RG$SWI)

# Spearmanit
cor(dat_RG$SR_vp, dat_RG$tavg_summer, method = "spearman")


# 5. Tehtävä: Bivarianttiregressiomallit

# LM = lineaarinen malli
lämpömalli <- lm(dat_RG$SR_vp ~ dat_RG$tavg_summer)
summary(lämpömalli)


korkeusmalli <- lm(dat_RG$SR_vp ~ dat_RG$elevation)
summary(korkeusmalli)

## Regressiosuora asetetaan normi scatterplotille
## Selittävä muuttuja ekaks!
plot(dat_RG$tavg_summer, dat_RG$SR_vp)

## Regressiosuoran piirtäminen (koska regressiosuora muotoa a + bx)
abline (lämpömalli, col = "red")




# Korrelaation laskeminen:

# komennossa defaulttina Pearson 
# Ääriarvoja? Järjestysasteikollinen muuttuja? -> method = "Spearman"
cor(dat_RG$SR_vp, dat_RG$rr_annual)


# P-arvo saadaan korrelaation testaamisella, eli funktiolla...
cor.test(dat_RG$SR_vp, dat_RG$nivation)

cor.test(dat_RG$rr_annual, dat_RG$weathering)



## Loppu
muuttujamalli <- lm(dat_RG$weathering ~ dat_RG$rr_annual)
summary(muuttujamalli)


malli1 <- lm(SR_vp ~ rr_annual + weathering, data=dat_RG)
summary(malli1)

cor(malli1)





                               ##### ASSAROINTI ######

## VKO 2 #####

dat_ESS8 <- read.csv("C:/Users/syree/Downloads/ESS8e02_2_R.csv")
dat_rast <- read.csv("C:/Users/syree/Downloads/Rastegaisa (1).csv")
TPI <- dat_rast$TPI

TPI

TPI_cl <- cut(TPI, breaks=c(min(TPI), -0.1, 0.1, max(TPI)), labels = FALSE, include.lowest = TRUE)

TPI_f <- factor(TPI_cl)
TPI_f <- factor(TPI_cl, levels = c(1, 2, 3), labels = c("kuoppa", "tasainen", "kumpare"))
TPI_f

# OSA II
# Frekvenssit ja pylväskuvat
# MUUTA DOMICIL FACTOR-MUUTTUJAKS

dat_ESS8$domicil <- factor(dat_ESS8$domicil)


# Täl tulee rumat nimet
barplot(table(dat_ESS8$domicil))


#Nimetää uusiks
dat_ESS8$domicil <- factor((dat_ESS8$domicil), levels = levels(dat_ESS8$domicil)[c(1,4,5,2,3)], labels = c("City", "Suburbs", "Town", "Village", "Farm"))

table(dat_ESS8$domicil)

barplot(table(dat_ESS8$domicil))

# Vastemuuttujan keskiarvot selittävien muuttujien luokissa

aggregate(dat_ESS8$ccrdprs, by=list(dat_ESS8$domicil), FUN="mean")


# Histogrammit: vastemuuttuja selittävän muuttujan luokissa (Piilotettu)

install.packages("ggplot2")
library(ggplot2)

ggplot(data=dat_ESS8, aes(x=ccrdprs))+geom_histogram(stat="count")+facet_grid(~dat_ESS8$domicil)


# BOXPLOT

boxplot(dat_ESS8$ccrdprs~dat_ESS8$domicil)


#Regressioanalyysi
lm(dat_rast$SR_vp~TPI_f)

# Samat tiedot kun statassa saadaan:
summary(lm(dat_rast$SR_vp~TPI_f))


                                    ### VKO 3 ######
# MIN, MAX, MEDIAN, MEAN :
summary(dat_rast$SR_vp)

#Keskihajonta
sd(dat_rast$SR_vp)

# N vektoreille
NROW(dat_rast$SR_vp)

# Datasetille
nrow(dat_rast)


sademalli <- lm(dat_rast$SR_vp ~ dat_rast$rr_annual)
summary(sademalli)

eroosiomalli <- lm(dat_rast$SR_vp ~ dat_rast$weathering)
summary(eroosiomalli)

cor(dat_rast$SR_vp, dat_rast$rr_annual)

cor.test(dat_rast$SR_vp, dat_rast$rr_annual)

cor.test(dat_rast$rr_annual, dat_rast$weathering)

muuttujamalli <- lm(dat_rast$weathering ~ dat_rast$rr_annual)
summary(muuttujamalli)


malli1 <- lm(SR_vp ~ rr_annual + weathering, data=dat_rast)
summary(malli1)


# Korrelaatiokomento ei toimi, koska nivaatiossa NA-arvoja
cor(dat_rast$SR_vp, dat_rast$nivation)

dat_rast$nivation

# ->>Ratkee is.na-komennolla
dat_rast$nivation[is.na(dat_rast$nivation)] <- 0


raste <- read.csv("C:/Users/syree/Downloads/Rastegaisa (1).csv")


cor(x= raste$SR_vp, y= raste$nivation, use="complete.obs")
cor(x= raste$SR_vp, y= raste$nivation, use="na.or.complete")
cor(x= raste$SR_vp, y= raste$nivation, use="pairwise.complete.obs")



####  Tenttidata?
datt <- read.csv("C://Users/syree/Downloads/tenttidata_v1.csv")
?hist

hist(datt$etavih, main = "Etäisyys kotiosoitteesta lähimmälle viheralueelle",  xlab = "Etäisyys (metriä)",  col = "light green")

mean(datt$etavih)

sd(datt$etavih)

?boxplot

?shapiro.test
hist(dat_RG$tavg_summer)

plot(dat_rast$nivation, dat_rast$SR_vp)




