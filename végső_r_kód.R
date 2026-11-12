#betöltöm a packageket adatbehíváshoz
library(foreign)
library(haven)
library (cluster)
library (nnet)
library (brglm2) #ez nincs meg
library (survey)


#beolvasom az adatfájlt
adat <- read_sav(file.choose(""))

#megnézem sikerült-e ->igen, mehetek tovább
View(adat)

#kiszűröm az általam választott 50-70-es korcsoportot
#amin dolgozni akarok
adat_szurt <- adat[
  adat$ELETKOR_KITOLTO >= 50 &
    adat$ELETKOR_KITOLTO <= 70,
]

#Megnézem a megoszlást
nrow(adat_szurt)
#összesen 1991 válaszadó tartozott ebbe a korcsoportba
summary(adat_szurt$ELETKOR_KITOLTO)

#megnézem hány missing van egy kérdésen belül
table(adat_szurt$SPOP, useNA = "ifany")
#spopra 35 missing van (-1-es érték)
table(adat_szurt$AEROBEPAR, useNA = "ifany")
#aerobeparra 17 ('NA' érték)

#megnézem a kereszttáblájuk cellaszámait
tab <- table(adat_szurt$SPOP,
             adat_szurt$AEROBEPAR,
             useNA = "ifany")
tab

round(prop.table(tab) * 100, 1) #megoszlás százalékosan
#az egyik cella értéke 98, a többijé min. 5 szöröse ennek
#de annyira nem vélem elenyészőnek, hogy ne dolgozhassak vele
#aki sportol 150 percet és aerobot is végez -> 510 fő (25.6%)
#aki sportol 150 percet és aerobot nem végez -> 98 fő (4.9%)
#aki nem sportol 150 percet de aerobot végez -> 546 fő (27.4%)
#aki nem sportol 150 percet és aerobot sem végez -> 801 fő (40.2%)
#+36 missing ->ell.: összesen ez 1991 fő tehát jó az eloszlás

#############VÁLTOZÓKAT VALIDÁLOM
##############SPOP - sportol hetente legalább 10 percet
#PE6 -Egy átlagos héten hány napon sportol, tölti szabadidejét mozgással legalább 10 percen át, megszakítás nélkül?
summary(adat_szurt$PE6)
# Sportidő átlaga/mediánja SPOP szerint:
aggregate(PE6 ~ SPOP,
          data = adat_szurt,
          FUN = median,
          na.rm = TRUE)
# Logikai check: sportidő > 0 vagy sem
table(adat_szurt$SPOP,
      adat_szurt$PE6 > 0,
      useNA = "ifany")
#jól van, mert aki 0 napot sportol az 1347
#Spop = 0 1347  tehát ez pontosan ugyanaz az 1347 ember
#validnak tűnik a változó

#################AEROBEPAR - Aerob sporttevékenység (gyaloglás, kerékpározás és szabadidős mozgás) legalább heti 150 perc
summary(adat_szurt$HEPAMIN)
# Sportidő átlaga/mediánja SPOP szerint:
aggregate(HEPAMIN ~ AEROBEPAR,
          data = adat_szurt,
          FUN = median,
          na.rm = TRUE)
# Logikai check: szabadidős mozgás > 149 perc vagy sem

table(adat_szurt$AEROBEPAR,
      adat_szurt$HEPAMIN > 149,
      useNA = "ifany")

#jól van, mert aki 149 percnél kevesebbet mozog az 913 fő
#Aerobepar-nál kiesik a korábbi kereszttáblából az, aki nem
#válaszolt a spop kérdésre, de egyébként élesen kettéválik a
#149 + és a 149- csoport
#validnak tűnik a változó

#létrehozom 4 kategóriájú függő változómat
adat_szurt$mozgas <- NA
#-1 -et kihagyom, azt úgyse írom bele a kategorizálásba

adat_szurt$mozgas[adat_szurt$SPOP == 1 & adat_szurt$AEROBEPAR == 1] <- 1
adat_szurt$mozgas[adat_szurt$SPOP == 0 & adat_szurt$AEROBEPAR == 1] <- 2
adat_szurt$mozgas[adat_szurt$SPOP == 1 & adat_szurt$AEROBEPAR == 0] <- 3
adat_szurt$mozgas[adat_szurt$SPOP == 0 & adat_szurt$AEROBEPAR == 0] <- 4

#mozgas = 1 -> nagyon aktívak
#mozgas = 2 -> nem sportol, de mozog
#mozgas = 3 -> sportol, de nem mozog
#mozgas = 4 -> se nem sportol, se nem mozog

table(adat_szurt$mozgas, useNA = "ifany")
#36  missing lett és a csoportelemszámok is rendben vannak
#elkészült a függő változóm, amivel dolgozni fogok
#még egy utolsó ellenőrzés

table(adat_szurt$SPOP, adat_szurt$AEROBEPAR)
table(adat_szurt$mozgas, useNA = "ifany")


#############################Független változók

#----------------------------------------------------------------------------
####iskolai végzettség - ISK4_KITOLTO
table(adat_szurt$ISK4_KITOLTO, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$ISK4_KITOLTO,
      useNA = "ifany")

#problémás az egyik csoportban csak 7 személy van

#alapfokú végzettségű és sportol, de nem mozognál 7-en vannak 
#meg lehet oldani úgy, hogy 3 csoportos legyen a végzettség de akkor is 24 lesz a legkisebb csoport.
#megcsinálom azért az új változót, majd kitaláljuk használjam-e vagy se

adat_szurt$iskola3 <- NA

adat_szurt$iskola3[adat_szurt$ISK4_KITOLTO %in% c(1,2)] <- 1
adat_szurt$iskola3[adat_szurt$ISK4_KITOLTO == 3] <- 2
adat_szurt$iskola3[adat_szurt$ISK4_KITOLTO == 4] <- 3

table(adat_szurt$iskola3, useNA = "ifany")
table(adat_szurt$mozgas, adat_szurt$iskola3)
#ekkor:
#iskola3 = 1 -> nincs érettségije
#iskola3 = 2 -> van érettségije
#iskola3 = 3 -> diplomája van

#------------------------------------------------------
#Nem - HH_3_KITOLTO

table(adat_szurt$HH_3_KITOLTO, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$HH_3_KITOLTO,
      useNA = "ifany")
#ez egész jó

#----------------------------------------------------
#gazdasági aktivitás - MAINSTAT_EU
table(adat_szurt$MAINSTAT_EU, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$MAINSTAT_EU,
      useNA = "ifany")
#ezt össze lehet vonni 3 csoporttá
adat_szurt$gazd_akt3 <- NA

adat_szurt$gazd_akt3[adat_szurt$MAINSTAT_EU == 10] <- 1
adat_szurt$gazd_akt3[adat_szurt$MAINSTAT_EU %in% c(20,40,50,60)] <- 2
adat_szurt$gazd_akt3[adat_szurt$MAINSTAT_EU == 30] <- 3

table(adat_szurt$gazd_akt3, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$gazd_akt3,
      useNA = "ifany")
#van így is egy 7 elemszámú cella, esetleg a nyugdíjas is lehet még
#munkanélkülibe de egyelőre így hagyom
#gazd_akt3 = 1 ->foglalkoztatott
#gazd_akt3 = 2 -> munkanélküli
#gazd_akt3 = 3 -> nyugdíjas


#----------------------------------------------------
#településtípus - TTIPMSZ

table(adat_szurt$TTIPMSZ, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$TTIPMSZ,
      useNA = "ifany")
#ez is egész jó ezt hagyona  legkisebb elemszám 21

#--------------------------------------------------
#fizikai fájdalom miatti akadályozottság -PN2
table(adat_szurt$PN2, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$PN2,
      useNA = "ifany")
#itt is vannak nagyon kicsi cellaszámok, szóval ebből is 3 csoportot képzek

adat_szurt$akadalyozott <- NA

adat_szurt$akadalyozott[adat_szurt$PN2 %in% c(1,2)] <- 1
adat_szurt$akadalyozott[adat_szurt$PN2 == 3] <- 2
adat_szurt$akadalyozott[adat_szurt$PN2 %in% c(4,5)] <- 3

table(adat_szurt$akadalyozott, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$akadalyozott,
      useNA = "ifany")
#akadalyozott = 1 -> inkább kevésbé akadályozott
#akadalyozott = 2 ->közepesen akadályozott
#akadalyozott = 3 -> inkább akadályozott
#továbbra is van 3-mas elemszámű cella, ugyanakkor természetes,
#hogy aki akadályozott, abból kevés aki sportol de nem mozog

#-----------------------------------------------------
#hosszan fennálló korlátozottság - KORL

table(adat_szurt$KORL, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$KORL,
      useNA = "ifany")
#itt 5 a legkisebb csoport, itt is akár ösze is vonhatok
#létrehozom a változót meglátjuk mit használok majd
adat_szurt$korlatozott <- NA

adat_szurt$korlatozott[adat_szurt$KORL %in% c(1,2)] <- 1
adat_szurt$korlatozott[adat_szurt$KORL == 3] <- 2

table(adat_szurt$korlatozott, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$korlatozott,
      useNA = "ifany")
#korlatozott = 1 -> korlátozott személy valamilyen mértékben
#korlatozott = 2 -> nem korlátozott személy

#-----------------------------------------------
#testtömegindex - BMI4
table(adat_szurt$BMI4, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$BMI4,
      useNA = "ifany")

#ezt nem tudom tudom-e használni, azok a kategóriák, 
#amiket össze kéne vonni (sovány - normál) annyira nem érzem úgy
#hogy összevonhatóak, de van 1 és 5 elemszámú cella is

#----------------------------------------------------
#orvos által megállapított krónikus betegség - ORVOS_BETEGSEG_KRONIKUS

table(adat_szurt$ORVOS_BETEGSEG_KRONIKUS, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$ORVOS_BETEGSEG_KRONIKUS,
      useNA = "ifany")
#volt ilyen vagy nem volt ilyen betegségére lehet kategorizálni
#mert most nagyon sok rossz cella van meg amúgy is ez érdekel igazán minket
adat_szurt$kronikus <- NA

adat_szurt$kronikus[adat_szurt$ORVOS_BETEGSEG_KRONIKUS %in% c(1,2,3)] <- 1
adat_szurt$kronikus[adat_szurt$ORVOS_BETEGSEG_KRONIKUS == 4] <- 2

table(adat_szurt$kronikus, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$kronikus,
      useNA = "ifany")
#kronikus = 1 -> volt krónikus betegsége
#kronikus = 2 -> nem volt ilyen betegsége
#18 a legkisebb elemszám szerintem jó lesz

#------------------------------------------------------
#általános egészség - HS1
table(adat_szurt$HS1, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$HS1,
      useNA = "ifany")

#itt is 3 csoport
adat_szurt$alt_egs <- NA

adat_szurt$alt_egs[adat_szurt$HS1 %in% c(1,2)] <- 1
adat_szurt$alt_egs[adat_szurt$HS1 == 3] <- 2
adat_szurt$alt_egs[adat_szurt$HS1 %in% c(4,5)] <- 3

table(adat_szurt$alt_egs, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$alt_egs,
      useNA = "ifany")

#alt_egs = 1 -> inkább jó
#alt_egs = 2 -> kielégítő
#alt_egs = 3 -> inkább rossz
#legkisebb elemszám 8

#------------------------------------------
#ülő/ fekvő idő egy átlagos napon -  SEDBEH

summary(adat_szurt$SEDBEH)
#okés itt létre kellene hozni kategóriákat
#vagy szétbontok majd minden csoportot a kategóriák mentén és úgy nézem meg
adat_szurt$fekv_id3 <- NA_integer_
adat_szurt$fekv_id3[adat_szurt$SEDBEH > 0 & adat_szurt$SEDBEH < 241] <- 1
adat_szurt$fekv_id3[adat_szurt$SEDBEH > 240 & adat_szurt$SEDBEH < 481] <- 2
adat_szurt$fekv_id3[adat_szurt$SEDBEH > 480] <- 3

table(adat_szurt$fekv_id3, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$fekv_id3,
      useNA = "ifany")

#fekv_id3 = 1 -> legfeljebb 4 órát tölt üléssel vagy fekvéssel per nap (1–240 perc)
#fekv_id3 = 2 -> 4 óránál több, de legfeljebb 8 óra (241–480 perc)
#fekv_id3= 3 -> több mint nyolc órát tölt üléssel vagy fekvéssel per nap
#legkisebb elemszám 24

#-----------------------------------------------------------
#INU-1- anyagi helyzet

table(adat_szurt$INU_1, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$INU_1,
      useNA = "ifany")

#legkisebb elemszám 2, tehát összevonok
adat_szurt$anyagi_3 <- NA

adat_szurt$anyagi_3[adat_szurt$INU_1 %in% c(1,2)] <- 1
adat_szurt$anyagi_3[adat_szurt$INU_1 == 3] <- 2
adat_szurt$anyagi_3[adat_szurt$INU_1 %in% c(4,5)] <- 3

table(adat_szurt$anyagi_3, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$anyagi_3,
      useNA = "ifany")
#anyagi_3 = 1 -> inkább jó
#anyagi_3 = 2 -> megfelelő
#anyagi_3 = 3 -> inkább rossz
#legkisebb elemszám: 7
#---------------------------------------------------
#JIU_1 -> élettel való elégedettség

summary(adat_szurt$JIU_1)

table(adat_szurt$JIU_1, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$JIU_1,
      useNA = "ifany")
#kérdés, hogy kategorizáljak-e vagy jó folytonosként
#megcsinálom a biztonság kedvéért ezt is 3 kategóriára
adat_szurt$elegedett3 <- NA

adat_szurt$elegedett3[adat_szurt$JIU_1 > 0 & adat_szurt$JIU_1 < 5] <- 1
adat_szurt$elegedett3[adat_szurt$JIU_1 == 5] <- 2
adat_szurt$elegedett3[adat_szurt$JIU_1 > 5] <- 3

table(adat_szurt$elegedett3, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$elegedett3,
      useNA = "ifany")

#elegedett3 = 1 -> inkább nem elégedett
#elegedett3 = 2 -> elégedett
#elegedett3 = 3 -> inkább elégedett
#legkisebb elemszám 3
#csoportosítottam úgy is, hogy 4- 5- 6 legyen a középső tartomány de akkor
#a legkisebb elemszám 1 lett, ezért emellett döntöttem

#-------------------------------------------------
#mennyit tehet az egészségéért -> HSU_2
table(adat_szurt$HSU_2, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$HSU_2,
      useNA = "ifany")

#bináris változót képzek, mert van 0-ás csoport (de amúgy nem lenne muszáj)

adat_szurt$tehet_e2 <- NA

adat_szurt$tehet_e2[adat_szurt$HSU_2 %in% c(1,2)] <- 1
adat_szurt$tehet_e2[adat_szurt$HSU_2 %in% c(3,4)] <-  2

table(adat_szurt$tehet_e2, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$tehet_e2,
      useNA = "ifany")

#tehet_e2 = 1 -> tehet az egészségéért
#tehet_e2 = 2 -> nem nagyon tehet az egészségéért

#--------------------------------------------
#milyen gyakran okoz gondot a fáradtság - MH1d
table(adat_szurt$MH1d, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$MH1d,
      useNA = "ifany")
#érvényes válaszok között a legkisebb elemszám 4, ezért szerintem nem vonok össze semmit

#--------------------------------------------------
#milyen depressziója van - mhsumd
table(adat_szurt$MHSUMD)
#ebből is egy binárisat képzek, hogy van-e depresssziója vagy nincs
adat_szurt$depresszio <- NA

adat_szurt$depresszio[adat_szurt$MHSUMD == 1 ] <- 1
adat_szurt$depresszio[adat_szurt$MHSUMD >1] <-  2

table(adat_szurt$depresszio)
table(adat_szurt$mozgas,
      adat_szurt$depresszio,
      useNA = "ifany")

#depresszio = 1 -> nincs depressziója
#depresszio = 2 -> van valamilyen erősségű depressziója
#legkisebb elemszám 14

#-------------------------------------------------
#gondoz,ápol-e valakit - IC1
table(adat_szurt$IC1)
#itt is kapásból binárist csinálok arra, hogy ápol-e valakit a kérdezett

adat_szurt$apol <- NA_integer_
adat_szurt$apol[adat_szurt$IC1 %in% c(1, 2, 3) ] <- 1
adat_szurt$apol[adat_szurt$IC1 >3] <-  2

table(adat_szurt$apol)
table(adat_szurt$mozgas,
      adat_szurt$apol,
      useNA = "ifany")

#apol= 1 -> igen, ápol valakit valamilyen okból
#apol= 2 -> nem ápol senkit
#legkisebb elemszám = 26

#--------------------------------------------------
#gyerekek száma - GYER
table(adat_szurt$GYER)
#ebből is binárist csinálok: van gyereke aki vele él? 
adat_szurt$van_gyer <- NA

adat_szurt$van_gyer[adat_szurt$GYER == 0 ] <- 1
adat_szurt$van_gyer[adat_szurt$GYER > 0] <-  2

table(adat_szurt$van_gyer)
table(adat_szurt$mozgas,
      adat_szurt$van_gyer,
      useNA = "ifany")

#van_gyer = 1 -> nincs gyereke aki vele él
#van_gyer = 2 -> van gyereke aki vele él
#legkisebb elemszám 39

#-----------------------------------------------
#társas támogatottság mértéke - OSS
table(adat_szurt$OSS)
table(adat_szurt$mozgas,
      adat_szurt$OSS,
      useNA = "ifany")
#teljesen jó, legkisebb elemszám 32
##################################################################


#####################################MOZGAS VÁLTOZÓ VALIDÁLÁSA

#jól fedi a mozgás változó a mozgást?

#mozgás változóm a klaszter alakító változók mentén
adat_szurt$PE2[adat_szurt$PE2 %in% c(-8, -9)] <- NA
adat_szurt$PE4[adat_szurt$PE4 %in% c(-8, -9)] <- NA
adat_szurt$PE6[adat_szurt$PE6 %in% c(-8, -9)] <- NA
adat_szurt$PE8[adat_szurt$PE8 %in% c(-8, -9)] <- NA

mozgas_PE2 <- table(adat_szurt$mozgas, adat_szurt$PE2)
mozgas_PE4 <-table(adat_szurt$mozgas, adat_szurt$PE4)
mozgas_PE6 <-table(adat_szurt$mozgas, adat_szurt$PE6)
mozgas_PE8 <-table(adat_szurt$mozgas, adat_szurt$PE8)

mozgas_PE2
mozgas_PE4
mozgas_PE6
mozgas_PE8

# A mozgas kategóriájának előrejelzése a PE-változóból -> lambda értékek

lambda_mozgas <- function(tab) {
  n <- sum(tab)
  E1 <- n - max(rowSums(tab))
  E2 <- n - sum(apply(tab, 2, max))
  return((E1 - E2) / E1)
}

lambda_mozgas(mozgas_PE2)
lambda_mozgas(mozgas_PE4)
lambda_mozgas(mozgas_PE6)
lambda_mozgas(mozgas_PE8)


#khí négyzettel ellenőrizni

mozgas_x_PE2 <- chisq.test(mozgas_PE2, correct = FALSE)
mozgas_x_PE4 <- chisq.test(mozgas_PE4, correct = FALSE)
mozgas_x_PE6 <- chisq.test(mozgas_PE6, correct = FALSE)
mozgas_x_PE8 <- chisq.test(mozgas_PE8, correct = FALSE)

#szignifikanciák
mozgas_x_PE2
mozgas_x_PE4
mozgas_x_PE6
mozgas_x_PE8

#esetszámok
mozgas_x_PE2$expected
mozgas_x_PE4$expected
mozgas_x_PE6$expected
mozgas_x_PE8$expected

#kapcsolat erőssége
sqrt(as.numeric(mozgas_x_PE2$statistic) /
       (sum(mozgas_PE2) * (min(dim(mozgas_PE2)) - 1)))

sqrt(as.numeric(mozgas_x_PE4$statistic) /
       (sum(mozgas_PE4) * (min(dim(mozgas_PE4)) - 1)))

sqrt(as.numeric(mozgas_x_PE6$statistic) /
       (sum(mozgas_PE6) * (min(dim(mozgas_PE6)) - 1)))

sqrt(as.numeric(mozgas_x_PE8$statistic) /
       (sum(mozgas_PE8) * (min(dim(mozgas_PE8)) - 1)))


#################################################################
##-----------------------------------------------PAM KLASZTEREZÉS
#mozgást mutató változók missingelése -> ezekből lesz a PAM

adat_szurt$PE1[adat_szurt$PE1 %in% c(-8, -9)] <- NA
#adat_szurt$PE2[adat_szurt$PE2 %in% c(-8, -9)] <- NA
adat_szurt$PE3[adat_szurt$PE3 %in% c(-8, -9)] <- NA
#adat_szurt$PE4[adat_szurt$PE4 %in% c(-8, -9)] <- NA
adat_szurt$PE5[adat_szurt$PE5 %in% c(-8, -9)] <- NA
#adat_szurt$PE6[adat_szurt$PE6 %in% c(-8, -9)] <- NA
#adat_szurt$PE8[adat_szurt$PE8 %in% c(-8, -9)] <- NA


#numeric kell hogy legyenek
adat_szurt$PE1 <- as.numeric(adat_szurt$PE1)
adat_szurt$PE2 <- as.numeric(adat_szurt$PE2)
adat_szurt$PE3 <- as.numeric(adat_szurt$PE3)
adat_szurt$PE4 <- as.numeric(adat_szurt$PE4)
adat_szurt$PE6 <- as.numeric(adat_szurt$PE6)
adat_szurt$PE8 <- as.numeric(adat_szurt$PE8)

#csak a heti adatokkal

valtozok_heti <- c( "PE2", "PE4", "PE6", "PE8")

pam_adat_heti <- complete.cases(adat_szurt[,valtozok_heti])

sum(pam_adat_heti) 
sum(!pam_adat_heti)
#1928 eset marad meg így és 63 esik ki, ez nagyon jó!

gower_heti <- daisy(
  adat_szurt[pam_adat_heti, valtozok_heti],
  metric = "gower")

#PAM klaszterok létrehozása 2-3-4-5 csoporttal
pam2_heti <- pam(gower_heti, k=2, diss = TRUE)
pam3_heti <- pam(gower_heti, k=3, diss = TRUE)
pam4_heti <- pam(gower_heti, k=4, diss = TRUE)
pam5_heti <- pam(gower_heti, k=5, diss = TRUE)

#kiértékelem
c(
  pam2_heti$silinfo$avg.width,
  pam3_heti$silinfo$avg.width,
  pam4_heti$silinfo$avg.width,
  pam5_heti$silinfo$avg.width)

#0.42 a legjobb, ez olyan közepes elválaszthatóságot jelent
table(pam4_heti$clustering)
table(pam5_heti$clustering)
table(pam3_heti$clustering)
#sehogy se túl jók az elemszámok ezért megyek a négyes csoporttal
#első csoportban vannak 722-en, a másodikban 403-an a harmadikban 565-en és a negyedikben 238-an
#ez közepesen gyengén választja el a csoportokat, valamit még próbálkozok

#elmentem
klaszterhez <- complete.cases(adat_szurt[,valtozok_heti]) #kevesebbre van klaszter mint a teljes szűrt adat
adat_szurt$pam_klaszter <- NA_integer_
adat_szurt$pam_klaszter[klaszterhez] <- pam4_heti$clustering


#megnézem a klaszterek medoidjait
#adatbázis amin futott a PAM
pam4_adat <- adat_szurt[pam_adat_heti, valtozok_heti, drop=FALSE]
#medoidok
pam4_heti$medoids #13. 41. 1753. és 1377. sorok lettek a medoidok
#mi jellemzi ezeket
pam4_adat[c(13 , 41, 1753, 1377), c("PE2", "PE4", "PE6", "PE8")]

#mozgás változó mentén
pam_x_mozgas <- table(adat_szurt$mozgas, adat_szurt$pam_klaszter)
#######################################################szignifikancia
fo_teszt <- chisq.test(pam_x_mozgas, correct = FALSE)
fo_teszt #szignifikáns

#cellaszámok
fo_teszt$expected

#erősség (cramer's v)
sqrt(as.numeric(fo_teszt$statistic) /
       (sum(pam_x_mozgas) * (min(dim(pam_x_mozgas)) - 1)))

#mozgási csoportonkénti százalékos megoszlás
round(prop.table(pam_x_mozgas, 1) * 100, 1)

################mit jelentenek a klaszter csoportjaim? 
#pe2
#hány nap fordul elő, hogy 10 percet gyalogol/hét
table(adat_szurt$pam_klaszter, adat_szurt$PE2)

#PE4 
#hány nap fordul elő, hogy 10 percet kerékpározik/hét
table(adat_szurt$pam_klaszter, adat_szurt$PE4)

#PE6 
#hány nap fordul elő, hogy 10 percet sportol/hét
table(adat_szurt$pam_klaszter, adat_szurt$PE6)

#PE8 
#hány nap fordul elő, hogy 10 percet izomerősít/hét
table(adat_szurt$pam_klaszter, adat_szurt$PE8)


############################CSOPORTOK DEMOGRÁFIA MENTÉN

table(adat_szurt$pam_klaszter, adat_szurt$HH_3_KITOLTO)

table(adat_szurt$pam_klaszter, adat_szurt$iskola3)

table(adat_szurt$pam_klaszter, adat_szurt$gazd_akt3)

table(adat_szurt$pam_klaszter, adat_szurt$TTIPMSZ)

aggregate (ELETKOR_KITOLTO ~ pam_klaszter, data = adat_szurt, FUN = mean, na.rm = TRUE)
#nincs kimondott kocsoport, mindenhol 60 körüli az életkor

##############################################MULTINOMIÁLIS REGRESSZIÓK
###első körben menézem külön-külön van-e összefüggés a mozgás és az egyes változók között

###################################nem x mozgas változó
nem <- table(adat_szurt$mozgas,adat_szurt$HH_3_KITOLTO)
nem_teszt <- chisq.test(nem, correct = FALSE)
nem_teszt
sqrt(as.numeric(nem_teszt$statistic) /
       (sum(nem) * (min(dim(nem)) - 1)))
sum(nem_teszt$expected < 5)

###################################nem x klaszter változó

nem_k <- table(adat_szurt$pam_klaszter,adat_szurt$HH_3_KITOLTO)
nem_k_teszt <- chisq.test(nem_k, correct = FALSE)
nem_k_teszt
sqrt(as.numeric(nem_k_teszt$statistic) /
       (sum(nem_k) * (min(dim(nem_k)) - 1)))

###################################végzettség x mozgas változó
isk <- table(adat_szurt$mozgas,adat_szurt$iskola3)
isk_teszt <- chisq.test(isk, correct = FALSE)
isk_teszt
sqrt(as.numeric(isk_teszt$statistic) /
       (sum(isk) * (min(dim(isk)) - 1)))

###################################végzettség x klaszter változó

isk_k <- table(adat_szurt$pam_klaszter,adat_szurt$iskola3)
isk_k_teszt <- chisq.test(isk_k, correct = FALSE)
isk_k_teszt
sqrt(as.numeric(isk_k_teszt$statistic) /
       (sum(isk_k) * (min(dim(isk_k)) - 1)))


###################################gazdasági aktivitás x mozgas változó
gazd <- table(adat_szurt$mozgas,adat_szurt$gazd_akt3)
gazd_teszt <- chisq.test(gazd, correct = FALSE)
gazd_teszt
sqrt(as.numeric(gazd_teszt$statistic) /
       (sum(gazd) * (min(dim(gazd)) - 1)))

###################################gazdasági aktivitás x klaszter változó

gazd_k <- table(adat_szurt$pam_klaszter,adat_szurt$gazd_akt3)
gazd_k_teszt <- chisq.test(gazd_k, correct = FALSE)
gazd_k_teszt
sqrt(as.numeric(gazd_k_teszt$statistic) /
       (sum(gazd_k) * (min(dim(gazd_k)) - 1)))

###################################TELTIP  x mozgas változó
ttip <- table(adat_szurt$mozgas,adat_szurt$TTIPMSZ)
ttip_teszt <- chisq.test(ttip, correct = FALSE)
ttip_teszt
sqrt(as.numeric(ttip_teszt$statistic) /
       (sum(ttip) * (min(dim(ttip)) - 1)))

###################################TELTIP x klaszter változó

ttip_k <- table(adat_szurt$pam_klaszter,adat_szurt$TTIPMSZ)
ttip_k_teszt <- chisq.test(ttip_k, correct = FALSE)
ttip_k_teszt
sqrt(as.numeric(ttip_k_teszt$statistic) /
       (sum(ttip_k) * (min(dim(ttip_k)) - 1)))


###################################akadalyozott  x mozgas változó
akad <- table(adat_szurt$mozgas,adat_szurt$akadalyozott)
akad_teszt <- chisq.test(akad, correct = FALSE)
akad_teszt
sqrt(as.numeric(akad_teszt$statistic) /
       (sum(akad) * (min(dim(akad)) - 1)))

###################################akadalyozott x klaszter változó

akad_k <- table(adat_szurt$pam_klaszter,adat_szurt$akadalyozott)
akad_k_teszt <- chisq.test(akad_k, correct = FALSE)
akad_k_teszt
sqrt(as.numeric(akad_k_teszt$statistic) /
       (sum(akad_k) * (min(dim(akad_k)) - 1)))


###################################korlatozott  x mozgas változó
korl <- table(adat_szurt$mozgas,adat_szurt$korlatozott)
korl_teszt <- chisq.test(korl, correct = FALSE)
korl_teszt
sqrt(as.numeric(korl_teszt$statistic) /
       (sum(korl) * (min(dim(korl)) - 1)))

###################################korlatozott x klaszter változó

korl_k <- table(adat_szurt$pam_klaszter,adat_szurt$korlatozott)
korl_k_teszt <- chisq.test(korl_k, correct = FALSE)
korl_k_teszt
sqrt(as.numeric(korl_k_teszt$statistic) /
       (sum(korl_k) * (min(dim(korl_k)) - 1)))


###################################kronikus  x mozgas változó
kron <- table(adat_szurt$mozgas,adat_szurt$kronikus)
kron_teszt <- chisq.test(kron, correct = FALSE)
kron_teszt
sqrt(as.numeric(kron_teszt$statistic) /
       (sum(kron) * (min(dim(kron)) - 1)))

###################################kronikus x klaszter változó

kron_k <- table(adat_szurt$pam_klaszter,adat_szurt$kronikus)
kron_k_teszt <- chisq.test(kron_k, correct = FALSE)
kron_k_teszt
sqrt(as.numeric(kron_k_teszt$statistic) /
       (sum(kron_k) * (min(dim(kron_k)) - 1)))

###################################általános egs  x mozgas változó
alt <- table(adat_szurt$mozgas,adat_szurt$alt_egs)
alt_teszt <- chisq.test(alt, correct = FALSE)
alt_teszt
sqrt(as.numeric(alt_teszt$statistic) /
       (sum(alt) * (min(dim(alt)) - 1)))

###################################általános egs x klaszter változó

alt_k <- table(adat_szurt$pam_klaszter,adat_szurt$alt_egs)
alt_k_teszt <- chisq.test(alt_k, correct = FALSE)
alt_k_teszt
sqrt(as.numeric(alt_k_teszt$statistic) /
       (sum(alt_k) * (min(dim(alt_k)) - 1)))

###################################fekvő idő  x mozgas változó
fekv <- table(adat_szurt$mozgas,adat_szurt$fekv_id3)
fekv_teszt <- chisq.test(fekv, correct = FALSE)
fekv_teszt
sqrt(as.numeric(fekv_teszt$statistic) /
       (sum(fekv) * (min(dim(fekv)) - 1)))

###################################fekvő idő x klaszter változó

fekv_k <- table(adat_szurt$pam_klaszter,adat_szurt$fekv_id3)
fekv_k_teszt <- chisq.test(fekv_k, correct = FALSE)
fekv_k_teszt
sqrt(as.numeric(fekv_k_teszt$statistic) /
       (sum(fekv_k) * (min(dim(fekv_k)) - 1)))

###################################anyagi helyzet  x mozgas változó
anyagi <- table(adat_szurt$mozgas,adat_szurt$anyagi_3)
anyagi_teszt <- chisq.test(anyagi, correct = FALSE)
anyagi_teszt
sqrt(as.numeric(anyagi_teszt$statistic) /
       (sum(anyagi) * (min(dim(anyagi)) - 1)))

###################################anyagi helyzet x klaszter változó

anyagi_k <- table(adat_szurt$pam_klaszter,adat_szurt$anyagi_3)
anyagi_k_teszt <- chisq.test(anyagi_k, correct = FALSE)
anyagi_k_teszt
sqrt(as.numeric(anyagi_k_teszt$statistic) /
       (sum(anyagi_k) * (min(dim(anyagi_k)) - 1)))

###################################elégedett élettel  x mozgas változó
elgdt <- table(adat_szurt$mozgas,adat_szurt$elegedett3)
elgdt_teszt <- chisq.test(elgdt, correct = FALSE)
elgdt_teszt
sqrt(as.numeric(elgdt_teszt$statistic) /
       (sum(elgdt) * (min(dim(elgdt)) - 1)))

###################################elégedett élettel x klaszter változó

elgdt_k <- table(adat_szurt$pam_klaszter,adat_szurt$elegedett3)
elgdt_k_teszt <- chisq.test(elgdt_k, correct = FALSE)
elgdt_k_teszt
sqrt(as.numeric(elgdt_k_teszt$statistic) /
       (sum(elgdt_k) * (min(dim(elgdt_k)) - 1)))

###################################tehet-e egészségéért  x mozgas változó
tehet <- table(adat_szurt$mozgas,adat_szurt$tehet_e2)
tehet_teszt <- chisq.test(tehet, correct = FALSE)
tehet_teszt
sqrt(as.numeric(tehet_teszt$statistic) /
       (sum(tehet) * (min(dim(tehet)) - 1)))

###################################tehet-e egészségéért x klaszter változó

tehet_k <- table(adat_szurt$pam_klaszter,adat_szurt$tehet_e2)
tehet_k_teszt <- chisq.test(tehet_k, correct = FALSE)
tehet_k_teszt
sqrt(as.numeric(tehet_k_teszt$statistic) /
       (sum(tehet_k) * (min(dim(tehet_k)) - 1)))

###################################depresszio  x mozgas változó
depr <- table(adat_szurt$mozgas,adat_szurt$depresszio)
depr_teszt <- chisq.test(depr, correct = FALSE)
depr_teszt
sqrt(as.numeric(depr_teszt$statistic) /
       (sum(depr) * (min(dim(depr)) - 1)))

###################################depresszio x klaszter változó

depr_k <- table(adat_szurt$pam_klaszter,adat_szurt$depresszio)
depr_k_teszt <- chisq.test(depr_k, correct = FALSE)
depr_k_teszt
sqrt(as.numeric(depr_k_teszt$statistic) /
       (sum(depr_k) * (min(dim(depr_k)) - 1)))


###################################apol vkit  x mozgas változó
ap_e <- table(adat_szurt$mozgas,adat_szurt$apol)
ap_e_teszt <- chisq.test(ap_e, correct = FALSE)
ap_e_teszt
sqrt(as.numeric(ap_e_teszt$statistic) /
       (sum(ap_e) * (min(dim(ap_e)) - 1)))

###################################apol vkit x klaszter változó

ap_e_k <- table(adat_szurt$pam_klaszter,adat_szurt$apol)
ap_e_k_teszt <- chisq.test(ap_e_k, correct = FALSE)
ap_e_k_teszt
sqrt(as.numeric(ap_e_k_teszt$statistic) /
       (sum(ap_e_k) * (min(dim(ap_e_k)) - 1)))

###################################van-e gyereke  x mozgas változó
gyerek <- table(adat_szurt$mozgas,adat_szurt$van_gyer)
gyerek_teszt <- chisq.test(gyerek, correct = FALSE)
gyerek_teszt
sqrt(as.numeric(gyerek_teszt$statistic) /
       (sum(gyerek) * (min(dim(gyerek)) - 1)))

###################################van-e gyereke x klaszter változó

gyerek_k <- table(adat_szurt$pam_klaszter,adat_szurt$van_gyer)
gyerek_k_teszt <- chisq.test(gyerek_k, correct = FALSE)
gyerek_k_teszt
sqrt(as.numeric(gyerek_k_teszt$statistic) /
       (sum(gyerek_k) * (min(dim(gyerek_k)) - 1)))

###################################társas támogatottság  x mozgas változó
trs <- table(adat_szurt$mozgas,adat_szurt$OSS)
trs_teszt <- chisq.test(trs, correct = FALSE)
trs_teszt
sqrt(as.numeric(trs_teszt$statistic) /
       (sum(trs) * (min(dim(trs)) - 1)))

###################################társas támogatottság x klaszter változó

trs_k <- table(adat_szurt$pam_klaszter,adat_szurt$OSS)
trs_k_teszt <- chisq.test(trs_k, correct = FALSE)
trs_k_teszt
sqrt(as.numeric(trs_k_teszt$statistic) /
       (sum(trs_k) * (min(dim(trs_k)) - 1)))



#######################################még nincs eredmény a package nélkül
####################################################################################
###########################MULTINOMINÁLIS REGRESSZIÓ LÉTREHOZÁSA###################
#változó csoportok:
#----demográfia ---
#anyagi_3 , HH_3_kitolto, gazd_akt3, iskola3, TTIPMSZ

#----fizikai állapot ---
#akadalyozott, korlatozott, kronikus, alt_egs

#----élethelyzet---
#apol, van_gyer, OSS, fekv_id3

#----mentális állapot ---
#tehet_e2, depresszio, elegedett_3

##################################################
nrow(adat_szurt)
dim(adat_szurt)
#1991 x 657 soros az adat_szurt

valtozok_m <- c("mozgas",  "pam_klaszter", "HH_3_KITOLTO", "iskola3", "TTIPMSZ", "gazd_akt3", "anyagi_3", "akadalyozott", "korlatozott",
                "kronikus", "alt_egs", "apol", "van_gyer", "OSS", "fekv_id3", "tehet_e2", "depresszio", "elegedett3")
adat_m <- na.omit(adat_szurt[valtozok_m])

dim(adat_m)
View(adat_m) #a tényleges elemszámot a dim(adat_m) mutatja
colSums(is.na(adat_szurt[,valtozok_m]))
#az elemszám a hiányzó válaszok miatt változhat


# A többkategóriás magyarázó változók legyenek faktorok,
# hogy R NE feltételezzen lineáris hatást az 1-2-3 kategóriák között.
# A faktorizálás csak az adat_m modelladatot érinti, a korábbi elemzéseket nem.
kat_valtozok <- c("HH_3_KITOLTO", "iskola3", "TTIPMSZ", "gazd_akt3",
                  "anyagi_3", "akadalyozott", "korlatozott", "kronikus",
                  "alt_egs", "apol", "van_gyer", "OSS", "fekv_id3",
                  "tehet_e2", "depresszio", "elegedett3")
adat_m[kat_valtozok] <- lapply(adat_m[kat_valtozok], factor)

# Függő változók referenciakategóriái:
adat_m$mozgas <- relevel(factor(adat_m$mozgas), ref = "4")
adat_m$pam_klaszter <- relevel(factor(adat_m$pam_klaszter), ref = "3")

# Ellenőrzés: minden változó faktorszintje és a modell esetszáma
lapply(adat_m[c("mozgas", "pam_klaszter", kat_valtozok)], levels)
nrow(adat_m)

#mozgás és demográfiai változók - 1. mozgás modell
mozgas1 <- brmultinom(
  mozgas ~ HH_3_KITOLTO + iskola3 + TTIPMSZ + gazd_akt3 + anyagi_3,
  data = adat_m,
  trace = TRUE,
  type = "AS_MEAN",
  control = brglmControl(maxit=50))

summary(mozgas1)
exp(coef(mozgas1))
exp(confint(mozgas1))

#klaszter és demográfiai változók - 1. klaszter modell
klaszter1 <- brmultinom(
  pam_klaszter ~ HH_3_KITOLTO + iskola3 + TTIPMSZ + gazd_akt3 + anyagi_3,
  data = adat_m,
  type = "AS_mean",
  control = brglmControl(maxit=50))

summary(klaszter1)
exp(coef(klaszter1))
exp(confint(klaszter1))

#mozgás és demográfiai + fizikai állapot - 2. mozgás modell
mozgas2 <- brmultinom(
  mozgas ~ HH_3_KITOLTO + iskola3 + TTIPMSZ + gazd_akt3 + anyagi_3 + akadalyozott + korlatozott
  + kronikus + alt_egs,
  data = adat_m,
  type = "AS_mean",
  control = brglmControl(maxit=50))

summary(mozgas2)
exp(coef(mozgas2))
exp(confint(mozgas2))


#klaszter és demográfiai + fizikai állapot - 2. klaszter modell
klaszter2 <- brmultinom(
  pam_klaszter ~ HH_3_KITOLTO + iskola3 + TTIPMSZ + gazd_akt3 + anyagi_3 + akadalyozott + korlatozott
  + kronikus + alt_egs,
  data = adat_m,
  type = "AS_mean",
  control = brglmControl(maxit=50))

summary(klaszter2)
exp(coef(klaszter2))
exp(confint(klaszter2))

#mozgás és demográfia + fizikai állapot + élethelyzet - 3. mozgás modell
mozgas3 <- brmultinom(
  mozgas ~ HH_3_KITOLTO + iskola3 + TTIPMSZ + gazd_akt3 + anyagi_3 + akadalyozott + korlatozott
  + kronikus + alt_egs + apol + van_gyer + OSS + fekv_id3,
  data = adat_m,
  type = "AS_mean",
  control = brglmControl(maxit=50))

summary(mozgas3)
exp(coef(mozgas3))
exp(confint(mozgas3))

#klaszter és demográfia + fizikai állapot + élethelyzet - 3. klaszter modell
klaszter3 <- brmultinom(
  pam_klaszter ~ HH_3_KITOLTO + iskola3 + TTIPMSZ + gazd_akt3 + anyagi_3 + akadalyozott + korlatozott
  + kronikus + alt_egs + apol + van_gyer + OSS + fekv_id3,
  data = adat_m,
  type = "AS_mean",
  control = brglmControl(maxit=50))

summary(klaszter3)
exp(coef(klaszter3))
exp(confint(klaszter3))

#mozgás és demográfia + fizikai állapot + élethelyzet + mentális állapot - 4. mozgás modell
mozgas4 <- brmultinom(
  mozgas ~ HH_3_KITOLTO + iskola3 + TTIPMSZ + gazd_akt3 + anyagi_3 + akadalyozott + korlatozott
  + kronikus + alt_egs + apol + van_gyer + OSS + fekv_id3 + tehet_e2 + depresszio + elegedett3,
  data = adat_m,
  type = "AS_mean",
  control = brglmControl(maxit=50))

summary(mozgas4)
exp(coef(mozgas4))
exp(confint(mozgas4))


#klaszter és demográfia + fizikai állapot + élethelyzet + mentális állapot - 4. klaszter modell

klaszter4 <- brmultinom(
  pam_klaszter ~ HH_3_KITOLTO + iskola3 + TTIPMSZ + gazd_akt3 + anyagi_3 + akadalyozott + korlatozott
  + kronikus + alt_egs + apol + van_gyer + OSS + fekv_id3 + tehet_e2 + depresszio + elegedett3,
  data = adat_m,
  type = "AS_mean",
  control = brglmControl(maxit=50))

summary(klaszter4)
exp(coef(klaszter4))
exp(confint(klaszter4))

