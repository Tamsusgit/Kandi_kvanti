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
