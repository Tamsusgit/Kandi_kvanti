## Regressioviikko 3
## 28.4.2023

laji <- dat_rast$SR_vp
el <- dat_rast$elevation
tavg <- dat_rast$tavg_summer

malli1 <- lm(laji ~ el + tavg)

summary(malli1)

