min(dat_RG$SR_vp)

max(dat_rast$SR_vp)

mean(dat_rast$SR_vp)

median(dat_rast$SR_vp)

sd(dat_rast$SR_vp)

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

cor(malli1)

     #### TENTTI ##########


datt <- read.csv("C://Users/syree/Downloads/tenttidata_v1.csv")
?hist

hist(datt$etavih, main = "Etäisyys kotiosoitteesta lähimmälle viheralueelle",  xlab = "Etäisyys (metriä)",  col = "light green")

mean(datt$etavih)

sd(datt$etavih)

?boxplot

?shapiro.test
hist(dat_RG$tavg_summer)

plot(rastegaisa$tavg_summer, rastegaisa$SR_vp)


##### ASSAROINTI ######

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


# Histogrammit: vastemuuttuja selittävän muuttujan luokissa

install.packages("ggplot2")
library(ggplot2)

ggplot(data=dat_ESS8, aes(x=ccrdprs))+geom_histogram(stat="count")+facet_grid(~dat_ESS8$domicil)


# BOXPLOT

boxplot(dat_ESS8$ccrdprs~dat_ESS8$domicil)


#Regressioanalyysi
lm(dat_rast$SR_vp~TPI_f)

# Samat tiedot kun statassa saadaan:
summary(lm(dat_rast$SR_vp~TPI_f))


