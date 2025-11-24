# # 1. koodit kirjoitetaan skript-tiedostoon
# 2. koodi "ajetaan" ctrl+R
# 3. hashtagilla kommentit tai koodin inaktivointi. Käytä runsaasti!
# 4. Uus komento --> uus rivi. voi rivittää sulkuviivalla
# 5. siistiä koodia
# 6. 3:nnen kirjaimen kohdalla Rstudio yrittää arvata
# 7. muista sulkea sulut
# 8. isot ja pienet kirjaimet eri asia !
# 9. +-merkki tarkottaa, että komento on vajaa. Jos haluat peruuttaa, paina esc.
# 10. Muista värien merkitys!

# Tyhjennä kaikki
rm(list = ls())

# Hakasuluilla voidaan poimia lukuja vektoreista: 
vek[2:5] tai vek[1,5] tai vek[-5]

# Poimii vain ensimmäisen arvon
datastreams$`@iot.id`[1]

vek1 == median(vek1) # etsii vek1:n luvuista true/false
vek2[vek2==max(vek2)] # vek2:n maksimi
# ------------------ MATRIISISSA pitää ilmottaa sarake ja rivi! esim mat[1,1] ------------------
length() #kertoo monestako havainnosta vektori koostuu
# ------------- DATA -----------------------
datt <- read.csv("C://...tenttidata_v1.csv")
head(dat, 10) #näyttää vain 10 ylimmät rivit datasta
# vastakohta:
tail()
summary(dat) # tiivistää, str() on structure
dat$Area_km2 
# eli ==
dat[ , 6] 
# eli ==
dat[ , "Area_km2"]

nrow(dat) # rivit, 
ncol(dat) # columnit


# Matriisi on numeerinen taulukkomuotoinen funktio, kun taas dataframe voi sisältää myös tekstiä.


hist(area, main ="", xlab = "koko (km2)", ylab = "frekvenssi")
table(dat$REGION_code)

quantile(area, c(0.2, 0.5, 0.8))
write.csv(results, "C//...results.csv")
dat$Area_name[dat$Area_km2==max(dat$Area_km2)] # == A:lta suurimman alueen nimi

# ------------------- KOLMAS VIIKKO -------------------------
shapiro.test # testisuure mahollisimman lähellä 1 = muuttuja normaalisti jakautunut
#kertoo myös p-arvon (todennäkösyys että nollahypoteesi on tosi)

Pnorm() #  tietyn havainnon todnäk, KUN ilmiö noudattaa normjak KATO HELP-FILE, PÄIVITÄ JUTUT!
Qnorm() # sama, mut syötetään todnäk

log() #vinosta jakaumasta normaalimpi MUTTA MUISTA, arvot annetaan logaritmimuunnettuina seuraavanlaisesti!!
pnorm(log(100), mean(log(dat$PET)), sd(log(dat$PET)))
#kun taas
Qnorm(0,01, mean(log(dat$PET)), sd(log(dat$PET))) -> TÄSTÄ EI TULE OIKEAA ARVOA
#VAAN
exp(toi arvo), #saadaan muunnettua logaritmiarvosta normaaliks

#Keskivirhe=keskihajonta/havaintojen lkm neliöjuuri
elev_SE <-- sd(dat$elevation)/ sqrt(length(dat$elevation)) #length: vektorin pituus

#Luottamusväli kertoo, mitä arvoja muuttuja saa 95% todnäköisyydellä
elev_CI <- 1.96*elev_SE

mean(dat$elevation) +/- elev_CI #luottamusvälin päätepisteet: ylä-/alaraja (korkeimmat kohat)

 # ------------------------------- NELJÄS VIIKKO -----------------------------------

cor() # laskee korrelaation, cor.test() tilastollisen merkittävyyden, voi lisätä method = spearman
nrow() # saadaan havaintojen lukumäärä (laskee rivit)
nivaatio <- dat_RG$nivation[!is.na(dat_RG$nivation)] #poistaa kaikki NA-arvot, ! on negaatio


# ---------------- Histogrammit ryhmittäin: -------------------------------------------
# Itse komento: määritä data, kohtaan "x=" tulee vastemuuttuja, kohtaan facet_grid tulee ryhmittelymuuttuja
ggplot(data=iris,aes(x=Sepal.Length))+geom_histogram()+facet_grid(~Species)
#Keskiarvot ryhmittäin
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)

####### REGRESSIO #############

# Muuttujien luokittelu 4 kategoriseen luokkaan arvojen perusteella
# TPI luokiteltava arvot min...-0,1 luokkaan 1, -0,1...0,1 luokkaan 2 jne
# MUista nimetä uus TPI
TPI_cl <- cut(TPI, breaks=c(min(TPI), -0,1, 0,1, max(TPI), labels = FALSE, include.lowest = TRUE)
TPI_cl[TPI_cl==1] <- 2 #Muuttujan luokkien päivittäminen 1->2

# Tehdään TPI_cl:sta Järjestysasteikollinen muuttuja
TPI_f <- factor(TPI_cl)

#Voidaan vielä muokata luokille nimet seuraavasti
TPI_f <- factor(TPI_cl, levels = c(1, 2, 3), labels = c("kuoppa", "tasainen", "kumpare"))
              
              
#Regressioanalyysi niin, että ensin vaste ja ~:n jälkeen selittävät muuttujat, intercept = vakiotermi
sademalli <- lm(dat_RG$SR_vp ~ dat_RG$rr_annual)
summary(sademalli) # saadaan samat tiedot kun statassa

malli1 <- lm(SR_vp ~ rr_annual + weathering, data=dat_RG) #KOlmen muuttujan


# ------------------------------------------------------------ TÖISTÄ -----------------------------------------------------

# Katso, mitä funktioita paketissa on
ls("package:sensorThings4R")





 

