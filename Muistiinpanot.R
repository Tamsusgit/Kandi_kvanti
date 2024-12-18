# # 1. koodit kirjoitetaan skript-tiedostoon
# 2. koodi "ajetaan" ctrl+R
# 3. hashtagilla kommentit tai koodin inaktivointi. K�yt� runsaasti!
# 4. Uus komento --> uus rivi. voi rivitt�� sulkuviivalla
# 5. siisti� koodia
# 6. 3:nnen kirjaimen kohdalla Rstudio yritt�� arvata
# 7. muista sulkea sulut
# 8. isot ja pienet kirjaimet eri asia !
# 9. +-merkki tarkottaa, ett� komento on vajaa. Jos haluat peruuttaa, paina esc.
# 10. Muista v�rien merkitys!

# Hakasuluilla voidaan poimia lukuja vektoreista: vek[2:5] tai vek[1,5] tai vek[-5]
# vek1 == median(vek1) etsii vek1:n luvuista true/false
# vek2[vek2==max(vek2)] vek2:n maksimi
MATRIISISSA pit�� ilmottaa sarake ja rivi! esim mat[1,1]
# length() kertoo monestako havainnosta vektori koostuu
DATA
# datt <- read.csv("C://Users/syree/Downloads/tenttidata_v1.csv")
# head(dat, 10) n�ytt�� vain 10 ylimm�t rivit datasta
# vastakohta tail()
# summary(dat) tiivist��, str() on structure
# dat$Area_km2 eli dat[ , 6] eli dat[ , "Area_km2]
# nrow(dat) rivit, ncol(dat), columnit
Matriisi on numeerinen taulukkomuotoinen funktio, kun taas dataframe voi
sis�lt�� my�s teksti�.


hist(area, main ="", xlab = "koko (km2)", ylab = "frekvenssi")
table(dat$REGION_code)

# quantile(area, c(0.2, 0.5, 0.8))
# write.csv(results, C//pl��pl��_results.csv)
dat$Area_name[dat$Area_km2==max(dat$Area_km2)] == A:lta suurimman alueen nimi

KOLMAS VIIKKO
# shapiro.test : testisuure mahollisimman l�hell� 1 = muuttuja normaalisti jakautunut
#kertoo my�s p-arvon (todenn�k�syys ett� nollahypoteesi on tosi)

#Pnorm() : tietyn havainnon todn�k, KUN ilmi� noudattaa normjak KATO HELP-FILE, P�IVIT� JUTUT!
#Qnorm(): sama, mut sy�tet��n todn�k

#log(): vinosta jakaumasta normaalimpi MUTTA MUISTA, arvot annetaan logaritmimuunnettuina seuraavanlaisesti!!
pnorm(log(100), mean(log(dat$PET)), sd(log(dat$PET)))
#kun taas
Qnorm(0,01, mean(log(dat$PET)), sd(log(dat$PET))) -> T�ST� EI TULE OIKEAA ARVOA
#VAAN
exp(toi arvo), #saadaan muunnettua logaritmiarvosta normaaliks

#Keskivirhe=keskihajonta/havaintojen lkm neli�juuri
elev_SE <-- sd(dat$elevation)/ sqrt(length(dat$elevation)) #length: vektorin pituus

#Luottamusv�li kertoo, mit� arvoja muuttuja saa 95% todn�k�isyydell�
elev_CI <- 1.96*elev_SE

mean(dat$elevation) +/- elev_CI #luottamusv�lin p��tepisteet: yl�-/alaraja (korkeimmat kohat)

NELJ�S VIIKKO

# cor() laskee korrelaation, cor.test() tilastollisen merkitt�vyyden, voi lis�t� method = spearman
# nrow() saadaan havaintojen lukum��r� (laskee rivit)
nivaatio <- dat_RG$nivation[!is.na(dat_RG$nivation)] #poistaa kaikki NA-arvot, ! on negaatio
# Histogrammit ryhmitt�in:
# Itse komento: m��rit� data, kohtaan "x=" tulee vastemuuttuja, kohtaan facet_grid tulee ryhmittelymuuttuja
ggplot(data=iris,aes(x=Sepal.Length))+geom_histogram()+facet_grid(~Species)
#Keskiarvot ryhmitt�in
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)

####### REGRESSIO #############

# Muuttujien luokittelu 4 kategoriseen luokkaan arvojen perusteella
# TPI luokiteltava arvot min...-0,1 luokkaan 1, -0,1...0,1 luokkaan 2 jne
# MUista nimet� uus TPI
TPI_cl <- cut(TPI, breaks=c(min(TPI), -0,1, 0,1, max(TPI), labels = FALSE, include.lowest = TRUE)
TPI_cl[TPI_cl==1] <- 2 #Muuttujan luokkien p�ivitt�minen 1->2

# Tehd��n TPI_cl:sta J�rjestysasteikollinen muuttuja
TPI_f <- factor(TPI_cl)

#Voidaan viel� muokata luokille nimet seuraavasti
TPI_f <- factor(TPI_cl, levels = c(1, 2, 3), labels = c("kuoppa", "tasainen", "kumpare"))
              
              
#Regressioanalyysi niin, ett� ensin vaste ja ~:n j�lkeen selitt�v�t muuttujat, intercept = vakiotermi
sademalli <- lm(dat_RG$SR_vp ~ dat_RG$rr_annual)
summary(sademalli) # saadaan samat tiedot kun statassa

malli1 <- lm(SR_vp ~ rr_annual + weathering, data=dat_RG) #KOlmen muuttujan


 
