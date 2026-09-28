#betöltöm a packageket adatbehíváshoz
library(foreign)
library(haven)

#beolvasom az adatfájlt
adat <- read_sav(file.choose("ELEF_2019_kutatoszobai_allomany"))

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
#gazd_akt3 = 2 -> nem foglalkoztatott valamilyen okból
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
adat_szurt$fekv_id3
adat_szurt$fekv_id3[adat_szurt$SEDBEH > 0 & adat_szurt$SEDBEH < 241] <- 1
adat_szurt$fekv_id3[adat_szurt$SEDBEH > 240 & adat_szurt$SEDBEH < 481] <- 2
adat_szurt$fekv_id3[adat_szurt$SEDBEH > 480] <- 3

table(adat_szurt$fekv_id3, useNA = "ifany")

table(adat_szurt$mozgas,
      adat_szurt$fekv_id3,
      useNA = "ifany")

#fekv_id3 = 1 -> négy órát vagy kevesebbet tölt fekvéssel vagy üléssel per nap
#fekv_id3 = 2 -> négynél több, de nyolc vagy kevesebb órát tölt így per nap
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



