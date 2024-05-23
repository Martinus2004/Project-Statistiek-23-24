# 3.1 Data inlezen en manipuleren

people <- read.csv("~/School/Unief/2de jaar/Semester 2/Statistiek/Project/Project-Statistiek-23-24/people2324.dat", sep=";", na.strings="-", stringsAsFactors=TRUE)

# geef categorische vars passende labels: geslacht, opleidingsniveau,
# betaald werk, rol in het gezin

people$ind_gender = factor(people$ind_gender, levels = c(1, 2), labels = c("man","vrouw"))
people$ind_edu = factor(people$ind_edu, levels = c(1, 2, 3), labels = c("minder dan SO","diploma SO", "hoger diploma"), ordered = TRUE)
people$ind_atwork = factor(people$ind_atwork, levels = c(0, 1), labels = c("nee", "ja"))
people$hh_pos = factor(people$hh_pos, levels = c(1, 2, 3), labels = c("geen inwonende partner",
                                                                      "samenwonend met partner",
                                                                      "woont bij ouders"))

# nieuwe veranderlijken: hh_parent = "heeft de persoon minstens één kind < 18 j?" en
# hh_alone = "woont de persoon alleen?"

people$hh_parent = factor(people$hh_nchild >= 1, levels = c(FALSE, TRUE), labels = c("nee", "ja"))
people$hh_alone = factor(people$hh_nadult == 1 & people$hh_nchild == 0, levels = c(FALSE, TRUE), labels = c("nee", "ja"))

attach(people)

# 3.2 Beschrijvende statistiek:

# ind_gender = kwalitatief nominaal 
table(ind_gender); table(ind_gender)/length(ind_gender) * 100; barplot(table(ind_gender))

# ind_age = kwantitatief continue (redelijk normaal verdeeld)
table(cut(ind_age, breaks = 20)); hist(ind_age); plot(ecdf(ind_age))
mean(ind_age); sd(ind_age); sd(ind_age)/sqrt(length(ind_age)); range(ind_age); boxplot(ind_age)

# ind_edu = kwalitatief ordinaal
table(ind_edu); table(ind_edu)/length(ind_edu) * 100; barplot(table(ind_edu))

# ind_happy = kwantitatief continue (linksscheve verdeling)(Noah denk eerder kwalitatief ordinaal)
table(ind_happy); hist(ind_happy); plot(ecdf(ind_happy))
mean(ind_happy); sd(ind_happy);sd(ind_happy)/sqrt(length(ind_happy)); range(ind_happy); boxplot(ind_happy)

# ind_atwork = kwalitatief nominaal
table(ind_atwork); table(ind_atwork)/length(ind_atwork) * 100; barplot(table(ind_atwork))

# ind_income = kwantitatief continue (rechtsscheef verdeeld)
hist(ind_income); plot(ecdf(ind_income))
mean(ind_income); sd(ind_income);sd(ind_income)/sqrt(length(ind_income)); range(ind_income); boxplot(ind_income)
hist(log(ind_income))
#bij een log transformatie normaler verdeeld

# hh_pos = kwalitatief nominaal
table(hh_pos); table(hh_pos)/length(hh_pos) * 100; barplot(table(hh_pos))

# hh_nadult = kwantitatief discreet
table(hh_nadult); table(hh_nadult)/length(hh_nadult) * 100; barplot(table(hh_nadult))

# hh_nchild = kwantitatief discreet
table(hh_nchild); table(hh_nchild)/length(hh_nchild) * 100; barplot(table(hh_nchild))

# hh_income = kwantitatief continue (logaritmisch verdeeld)
hist(hh_income); plot(ecdf(hh_income))
mean(hh_income); sd(hh_income);sd(hh_income)/sqrt(length(hh_income)); range(hh_income); boxplot(hh_income)
hist(log(hh_income))
#log transformatie zorgt dat het normaler verdeeld is

# health_fys = kwantitatief continue (linksscheef verdeeld) (is een kwalitatief ordinaal want het is met een schaal)
hist(health_fys); plot(ecdf(health_fys))
mean(health_fys, na.rm = TRUE); sd(health_fys, na.rm = TRUE); sd(health_fys, na.rm = TRUE)/sqrt(length(health_fys)); range(health_fys, na.rm = TRUE); boxplot(health_fys, na.rm = TRUE)

# health_emo = kwantitatief continue (linksscheef verdeeld) (is een kwalitatief ordinaal want het is met een schaal)
hist(health_emo); plot(ecdf(health_emo))
mean(health_emo, na.rm = TRUE); sd(health_emo, na.rm = TRUE); sd(health_emo, na.rm = TRUE)/sqrt(length(health_emo)); range(health_emo, na.rm = TRUE); boxplot(health_emo, na.rm = TRUE)

# leis_time = kwantitatief continue (logaritmisch verdeeld)
hist(leis_time); plot(ecdf(leis_time))
mean(leis_time, na.rm = TRUE); sd(leis_time, na.rm = TRUE); sd(leis_time, na.rm = TRUE)/sqrt(length(leis_time)); range(leis_time, na.rm = TRUE); boxplot(leis_time)
hist(log(leis_time))
#door een log transformatie normaler verdeeld

# hh_parent = kwantitatief discreet
table(hh_parent); table(hh_parent)/length(hh_parent) * 100; barplot(table(hh_parent))

# hh_alone = kwantitatief discreet
table(hh_alone); table(hh_alone)/length(hh_alone) * 100; barplot(table(hh_alone))

#extra deeltje hoe respondeert geluk (ind_happy) met andere kenmerken
plot(ind_gender,ind_happy)
mean(ind_happy[ind_gender=="man"])
mean(ind_happy[ind_gender=="vrouw"])

plot(ind_age,ind_happy)
happy=cut(ind_happy, c(0,10,20,30,40,50,60,70,80,90,Inf))
age=cut(ind_age,c(30,40,50,60,70,Inf))
table(age,happy);plot(age,happy)

plot(ind_edu,ind_happy) 
mean(ind_happy[ind_edu=="minder dan SO"],na.rm = TRUE)
mean(ind_happy[ind_edu=="diploma SO"],na.rm = TRUE)
mean(ind_happy[ind_edu=="hoger diploma"],na.rm = TRUE)

plot(ind_atwork,ind_happy)
mean(ind_happy[ind_atwork=="ja"],na.rm = TRUE)
mean(ind_happy[ind_atwork=="nee"],na.rm = TRUE)

plot(ind_income,ind_happy)

plot(hh_pos,ind_happy)
mean(ind_happy[hh_pos=="geen inwonende partner"],na.rm = TRUE)
mean(ind_happy[hh_pos=="samenwonend met partner"],na.rm = TRUE)
mean(ind_happy[hh_pos=="woont bij ouders"],na.rm = TRUE)

plot(hh_nadult,ind_happy)
mean(ind_happy[hh_nadult=="0"],na.rm = TRUE)
mean(ind_happy[hh_nadult=="1"],na.rm = TRUE)
mean(ind_happy[hh_nadult=="2"],na.rm = TRUE)
mean(ind_happy[hh_nadult=="3"],na.rm = TRUE)
mean(ind_happy[hh_nadult=="4"],na.rm = TRUE)
mean(ind_happy[hh_nadult=="5"],na.rm = TRUE)
mean(ind_happy[hh_nadult=="6"],na.rm = TRUE)
mean(ind_happy[hh_nadult=="7"],na.rm = TRUE)

plot(hh_nchild,ind_happy)
mean(ind_happy[hh_nchild=="O"],na.rm = TRUE)
mean(ind_happy[hh_nchild=="1"],na.rm = TRUE)
mean(ind_happy[hh_nchild=="2"],na.rm = TRUE)
mean(ind_happy[hh_nchild=="3"],na.rm = TRUE)
mean(ind_happy[hh_nchild=="4"],na.rm = TRUE)
mean(ind_happy[hh_nchild=="5"],na.rm = TRUE)

plot(hh_income,ind_happy)

plot(health_fys,ind_happy)

plot(health_emo,ind_happy)

plot(leis_time,ind_happy)

plot(hh_parent,ind_happy)
mean(ind_happy[hh_parent=="ja"])
mean(ind_happy[hh_parent=="nee"],na.rm = TRUE)

plot(hh_alone,ind_happy)
mean(ind_happy[hh_alone=="ja"])
mean(ind_happy[hh_alone=="nee"])


# 3.3 Inferentie:

# 3.3.1: kenmerken van de steekproef

# - proportie vrouwen en mannen in 2016 van onderzoek == proportie volgens StatBel?
# StatBel: 4 368 849 mannen en 4 613 480  vrouwen = totaal 8 982 329 volwassenen

# X = geslacht respondent, succes = "vrouw" of succes = "man"

#p0 = 4 613 480 / 8 982 329 = proportie vrouwen populatie of 4 368 849 / 8 982 329 = proportie mannen populatie

table(ind_gender); table(ind_gender)/length(ind_gender)
data.frame("man SB" = 4368849/8982329, "vrouw SB" = 4613480/8982329)

binom.test(914, length(ind_gender), p = 4613480/8982329, alternative = "two.sided")

binom.test(818, length(ind_gender), p = 4368849/8982329, alternative = "two.sided")

# p-waarde >> 0.05 en p0 in betrouwbaarheidsinterval, dus H1 verwerpen en H0 aanvaarden
# besluit: de geobserveerde proportie vrouwen (en ook mannen) verschilt niet significant van de proportie die Statbel geeft


# - verdeling leeftijd in 2016 van onderzoek == verdeling volgens StatBel?

leeftijdscategorieën = cut(ind_age, breaks = c(min(ind_age), 30, 40, 50, 60, 70, max(ind_age) + 1), right = FALSE)
table(leeftijdscategorieën)
verwachte_frequenties = c(0.19, 0.16, 0.17, 0.18, 0.14, 0.16) 

# alle verwachte frequenties > 5 dus oké

leeftijd_verdeling_test = chisq.test(table(leeftijdscategorieën), p = verwachte_frequenties); leeftijd_verdeling_test

leeftijd_verdeling_test$observed
leeftijd_verdeling_test$expected
leeftijd_verdeling_test$residuals

# p-waarde is praktisch 0, dus de verdeling vd steekproef wijkt sterk significant af van verdeling bij de belgen

# de 18-30 jaar groep is zeer sterk ondervertegenwoordigd maw véél te weinig van die leeftijdscategorie bevraagd
# de andere leeftijdscategorieen buiten 30-40 jaar is beetje oververtegenwoordigd maw beetje teveel mensen bevraagd van die groep


# - gemiddeld individuele netto belastbaar inkomen per inwoner in 2016 == gemiddelde volgens StatBel?


# test voor één gemiddelde
# X = gemiddeld netto belastbaar inkomen per inwoner in 2016
# H0: mu_X = mu_0 = 1485.33, # H1: mu_X != mu_0 = 1485.33 

gemiddeld_inkomen_2016 = mean(ind_income)
mu_0 = 1485.33

# CLS geldt want n >>> 30
length(ind_income)

t.test(ind_income, mu = mu_0, alternative = "two.sided")
t.test(ind_income, mu = mu_0, alternative = "greater")

# p-waarde praktisch 0, dus H0 verwerpen maw gemiddeld inkomen is significant hoger dan volgens StatBel


# 3.3.2: gemiddelde geluksscore


# Verschilt de score voor geluk naargelang het geslacht van de respondent? 

# test voor twee gemiddelden, ongepaarde groepen want we verdelen één veranderlijke in 2 groepen
# X = gelukscore van vrouwen, Y = gelukscore van mannen
# H0: mu_X = mu_Y, H1: mu_X != mu_Y   
geluksscore_man = ind_happy[ind_gender == "man"]
geluksscore_vrouw = ind_happy[ind_gender == "vrouw"]
mean(geluksscore_man); mean(geluksscore_vrouw)

# 0) geldt CLS
length(na.omit(geluksscore_man));length(na.omit(geluksscore_vrouw)) 
# Ja want beide n >>> 30

# 1) Normaliteit?

shapiro.test(geluksscore_man); shapiro.test(geluksscore_vrouw)
boxplot(ind_happy ~ ind_gender, xlab = "geslacht", ylab = "geluksscore"); abline(a=mean(geluksscore_man), b=0, col="blue", lwd = 2); abline(a=mean(geluksscore_vrouw), b=0, col="pink", lwd = 2)
legend("bottom", inset=c(0, -0.225), legend=c("gemiddelde geluksscore mannen", "gemiddelde geluksscore vrouwen"),
       col=c("blue", "pink"), lty = c(1, 1), lwd=c(2, 2), cex=0.8, box.lty=0, bg="transparent", ncol=2, xpd=TRUE,
       text.width=strwidth("gemiddelde geluksscore mannen   ") * 0.8)

qqnorm(geluksscore_man); qqline(geluksscore_man); qqnorm(geluksscore_vrouw); qqline(geluksscore_vrouw)
# beide p-waarden bijna 0, dus wijken té sterk af van normaal verdeeld voor de F-test, we zien ook linksscheve verdeling
# op histogrammen en boxplots

# 2) t-test ongepaard met ongelijke varianties

t.test(geluksscore_man, geluksscore_vrouw, paired = FALSE, var.equal = FALSE, alternative = "two.sided")

# p-waarde: 0.3778 >> 0.05 dus we verwerpen H1, de afwijking is toeval
# de boxplots hebben een blauwe en rode rechte, die de verschillende gemiddelden voorstellen, deze liggen bijna op elkaar (klein verschil)
# Besluit: obv de steekproef vinden we geen significante afwijking tussen de gelukscores van mannen en vrouwen



# Naargelang men betaald werk uitvoert? 

# test voor twee gemiddelden, ongepaarde groepen want we verdelen één veranderlijke in 2 groepen
# X = gelukscore van personen met betaald werk, Y = gelukscore van personen zonder betaald werk
# H0: mu_X = mu_Y, H1: mu_X != mu_Y   
geluksscore_betaald_werk = ind_happy[ind_atwork == "ja"]
geluksscore_geen_betaald_werk = ind_happy[ind_atwork == "nee"]
mean(geluksscore_betaald_werk, na.rm = TRUE); mean(geluksscore_geen_betaald_werk, na.rm = TRUE)

# 0) geldt CLS
length(na.omit(geluksscore_betaald_werk))
length(na.omit(geluksscore_geen_betaald_werk))
# Ja want beide n >>> 30

# 1) Normaliteit?

shapiro.test(geluksscore_betaald_werk); shapiro.test(geluksscore_geen_betaald_werk)
boxplot(ind_happy ~ ind_atwork, xlab = "betaald werk", ylab = "geluksscore"); abline(a=mean(geluksscore_betaald_werk,na.rm = TRUE), b=0, col="red", lwd = 2); abline(a=mean(geluksscore_geen_betaald_werk, na.rm = TRUE), b=0, col="blue", lwd = 2) 
legend("bottom", inset=c(0, -0.225), legend=c("gemiddelde geluksscore betaald werk", "gemiddelde geluksscore geen betaald werk"),
       col=c("red", "blue"), lty = c(1, 1), lwd=c(2, 2), cex=0.8, box.lty=0, bg="transparent", ncol=2, xpd=TRUE,
       text.width=strwidth("gemiddelde geluksscore geen betaald werk") * 0.9)

qqnorm(geluksscore_betaald_werk); qqline(geluksscore_betaald_werk); qqnorm(geluksscore_geen_betaald_werk); qqline(geluksscore_geen_betaald_werk)

# beide p-waarden bijna 0, dus wijken té sterk af van normaal verdeeld voor de F-test, we zien ook rechtsscheve verdeling
# op histogrammen en boxplots

# 2) t-test ongepaard met ongelijke varianties

t.test(geluksscore_betaald_werk, geluksscore_geen_betaald_werk, paired = FALSE, var.equal = FALSE, alternative = "two.sided")

# p-waarde: 1.658 * 10^-7 <<< 0.05 dus we verwerpen H0, de afwijking is geen toeval maar significant
# rode rechte vs. blauwe rechte, groter verschil dan vorige test
# Besluit: obv de steekproef vinden we een significant verschil tussen de geluksscores van mensen met of zonder betaald werk

# Is er een verschil tussen ouders met kinderen jonger dan 18 jaar naargelang ze een inwonende
# partner hebben?

# test voor twee gemiddelde, ongepaarde groepen want we delen "geluksscore van ouders met kind" op in twee groepen
# X = geluksscore van ouders met kind < 18 j én inwonende partner, Y = geluksscore van ouders met kind < 18 j zonder inwonende partner
# H0: mu_X = mu_Y, H1: mu_X != mu_Y   
geluksscore_jongkind_en_samenwonend = ind_happy[hh_parent == "ja" & hh_pos == "samenwonend met partner"]
geluksscore_jongkind_en_alleenwonend = ind_happy[hh_parent == "ja" & hh_pos == "geen inwonende partner"]
mean(geluksscore_jongkind_en_samenwonend, na.rm = TRUE); mean(geluksscore_jongkind_en_alleenwonend, na.rm = TRUE)

# 0) geldt CLS
length(na.omit(geluksscore_jongkind_en_samenwonend)); length(na.omit(geluksscore_jongkind_en_alleenwonend))
# Ja want beide n >>> 30

# 1) Normaliteit?

shapiro.test(geluksscore_jongkind_en_samenwonend); shapiro.test(geluksscore_jongkind_en_alleenwonend)

boxplot(geluksscore_jongkind_en_samenwonend, geluksscore_jongkind_en_alleenwonend, names = c("samenwonend", "alleenstaand"), ylab = "geluksscore")

abline(a=mean(geluksscore_jongkind_en_samenwonend, na.rm =TRUE), b=0, col= "red", lwd = 2); abline(a= mean(geluksscore_jongkind_en_alleenwonend, na.rm = TRUE), b=0, col = "blue", lwd = 2)

legend("bottomleft", inset=c(0.1, -0.3), legend=c("gemiddelde geluksscore samenwonende ouders", "gemiddelde geluksscore alleenstaande ouders"),
       col=c("red", "blue"), lty = c(1, 1), lwd=c(2, 2), cex=0.8, box.lty=0, bg="transparent", xpd=TRUE,
       text.width=strwidth("gemiddelde geluksscore alleenstaande ouders") * 0.8)
qqnorm(geluksscore_jongkind_en_samenwonend); qqline(geluksscore_jongkind_en_samenwonend); qqnorm(geluksscore_jongkind_en_alleenwonend); qqline(geluksscore_jongkind_en_alleenwonend)

# beide p-waarden bijna 0, dus wijken té sterk af van normaal verdeeld voor de F-test, we zien ook rechtsscheve verdeling
# op histogrammen en boxplots

# 2) t-test ongepaard met ongelijke varianties

t.test(geluksscore_jongkind_en_samenwonend, geluksscore_jongkind_en_alleenwonend, paired = FALSE, var.equal = FALSE, alternative = "two.sided")

# p-waarde: 1.185 * 10^-5 <<< 0.05 dus we verwerpen H0, de afwijking is geen toeval maar significant
# Besluit: obv de steekproef vinden we een significant verschil tussen de gelukscores van mensen met een kind die samen wonen met een partner


# 3.3.3 associatie tussen veranderlijken

# Ga na of er afhankelijkheid is tussen de geluksscore enerzijds en de (niet-binaire) veranderlijken anderzijds.

# correlatietesten tussen geluksscore ~ leeftijd, netto inkomen, aantal -18, aantal +18, gezinsinkomen,
# fysieke gezondheid, emotionele gezondheid, aantal uur vrije tijd

# 1) Normaliteit testen

shapiro.test(ind_happy) # => wijkt té sterk af van normale verdeling dus steeds spearman test obv rangen
cor.test(ind_happy, ind_age, method = "spearman"); plot(ind_happy, ind_age)
cor.test(ind_happy, ind_income, method = "spearman"); plot(ind_happy, ind_income)
cor.test(ind_happy, hh_nchild, method = "spearman"); plot(ind_happy, hh_nchild)
cor.test(ind_happy, hh_nadult, method = "spearman"); plot(ind_happy, hh_nadult)
cor.test(ind_happy, hh_income, method = "spearman"); plot(ind_happy, hh_income)
cor.test(ind_happy, health_fys, method = "spearman"); plot(ind_happy, health_fys)
cor.test(ind_happy, health_emo, method = "spearman"); plot(ind_happy, health_emo)
cor.test(ind_happy, leis_time, method = "spearman"); plot(ind_happy, leis_time)
par(mfrow=c(1,4))
plot(ind_age,ind_happy, xlab = "leeftijd", ylab = "geluksscore")
abline(lm(ind_happy~ind_age), col='red')
plot(ind_income,ind_happy, xlab = "persoonlijk netto inkomen (euro / maand)", ylab = "geluksscore")
abline(lm(ind_happy~ind_income), col='red')
boxplot(ind_happy~hh_nchild, xlab = "aantal kinderen in het huishouden", ylab = "geluksscore")
abline(lm(ind_happy~hh_nchild), col='red')
boxplot(ind_happy~hh_nadult, xlab = "aantal volwassenen in het huishouden", ylab = "geluksscore")
abline(lm(ind_happy~hh_nadult), col='red')
plot(hh_income,ind_happy, xlab = "beschikbaar gezinsinkomen (euro / maand)", ylab = "geluksscore")
abline(lm(ind_happy~hh_income), col='red')
plot(health_fys,ind_happy, xlab = "fysieke gezondheid", ylab = "geluksscore")
abline(lm(ind_happy~health_fys), col='red')
plot(health_emo,ind_happy, xlab = "emotionele gezondheid", ylab = "geluksscore")
abline(lm(ind_happy~health_emo), col='red')
plot(leis_time,ind_happy, xlab = "tijd besteed aan ontspanning (uur / week)", ylab = "geluksscore")
abline(lm(ind_happy~leis_time), col='red')
#aantal gevallen waar er ties zijn per significante variabele
sum(table(ind_income) > 1);sum(table(hh_nadult) > 1);sum(table(hh_income) > 1);sum(table(health_fys) > 1);sum(table(health_emo) > 1)



#3.3.4 Verklaren van de gelukscore
# Eenvoudig regressiemodel voor geluk in functie van het totale beschikbare inkomen van het gezin en het tweede in functie van het tiendelige logaritmevan dat inkomen.
model1=lm(ind_happy~hh_income)
summary(model1)
plot(ind_happy~hh_income, xlab = "beschikbaar gezinsinkomen (euro / maand)", ylab = "geluksscore")
abline(model1, col='red')
x_i1 = model1$model[,2]
y_i1 = model1$model[,1]
betrouwbh1 = predict(model1, interval = "confidence", level = 0.95)
predictie1 = predict(model1, interval = "prediction", level = 0.95)
lines(sort(x_i1), betrouwbh1[order(x_i1), 2], col='blue')
lines(sort(x_i1), betrouwbh1[order(x_i1), 3], col='blue')
lines(sort(x_i1), predictie1[order(x_i1), 2], col='green')
lines(sort(x_i1), predictie1[order(x_i1), 3], col='green')

legend("topleft", inset=c(-0.1, -0.2), legend=c("betrouwbaarheidsbanden", "voorspellingsbanden"),
       col=c("blue", "green"), lty = c(1, 1), lwd=c(2, 2), cex=0.8, box.lty=0, bg="transparent", xpd=TRUE)

legend("topright", inset=c(-0.05, -0.2), legend=c("voorspelde geluksscores respondenten 25503 en 27010", "werkelijke geluksscores respondenten 25503 en 27010"),
       col=c("purple", "orange"), pch = c(16, 16), cex=0.8, box.lty=0, bg="transparent", xpd=TRUE)


#Multiple R-squared:  0.02271,	Adjusted R-squared:  0.02215, F-statistic: 40.21 on 1 and 1730 DF,  p-value: 2.906e-10, t-waarde=6.341
#kleine p-waarde en R-squared dus weinig verklaring van het geluk

model2=lm(ind_happy~log10(hh_income))
summary(model2)
plot(ind_happy~log10(hh_income), xlab = bquote("beschikbaar gezinsinkomen (euro / maand) (" * log[10] * " schaal)"), ylab = "geluksscore")
abline(model2, col='red')
x_i2 = model2$model[,2]
y_i2 = model2$model[,1]
betrouwbh2 = predict(model2, interval = "confidence", level = 0.95)
predictie2 = predict(model2, interval = "prediction", level = 0.95)
lines(sort(x_i2), betrouwbh2[order(x_i2), 2], col='blue')
lines(sort(x_i2), betrouwbh2[order(x_i2), 3], col='blue')
lines(sort(x_i2), predictie2[order(x_i2), 2], col='green')
lines(sort(x_i2), predictie2[order(x_i2), 3], col='green')

legend("topleft", inset=c(-0.1, -0.2), legend=c("betrouwbaarheidsbanden", "voorspellingsbanden"),
       col=c("blue", "green"), lty = c(1, 1), lwd=c(2, 2), cex=0.8, box.lty=0, bg="transparent", xpd=TRUE)

legend("topright", inset=c(-0.05, -0.2), legend=c("voorspelde geluksscores respondenten 25503 en 27010", "werkelijke geluksscores respondenten 25503 en 27010"),
       col=c("purple", "orange"), pch = c(16, 16), cex=0.8, box.lty=0, bg="transparent", xpd=TRUE)


#Multiple R-squared:  0.04422,	Adjusted R-squared:  0.04367, F-statistic: 80.04 on 1 and 1730 DF,  p-value: < 2.2e-16, t-waarde=8.947
#kleine p-waarde en R-squared dus weinig verklaring van het geluk





# meervoudige regressie

geluksscore_meerv_model = lm(ind_happy ~ ind_age + ind_income + hh_income + health_fys + health_emo + leis_time)

geluksscore_continue = data.frame(ind_age, ind_income, hh_income, health_fys, health_emo, leis_time)

plot(geluksscore_continue)
cor(geluksscore_continue)

summary(geluksscore_meerv_model)
geluksscore_meerv_model = update(geluksscore_meerv_model, .~.-ind_age); summary(geluksscore_meerv_model)
geluksscore_meerv_model = update(geluksscore_meerv_model, .~.-ind_income); summary(geluksscore_meerv_model)
geluksscore_meerv_model = update(geluksscore_meerv_model, .~.-leis_time); summary(geluksscore_meerv_model)
geluksscore_meerv_model = update(geluksscore_meerv_model, .~.-health_fys); summary(geluksscore_meerv_model)

# modelveronderstellingen meervoudig model

par(mfrow=c(2, 2))
plot(geluksscore_meerv_model)
par(mfrow=c(1, 1))

# residuen lijken mooi verspreid rond de nullijn te liggen, de rode trendlijn sluit redelijk mooi aan bij die nullijn
# de varianties van de residuen lijkt niet constant, ook de normaliteit lijkt af te wijken

# ingezoomd residuplot
geluksscore_residus = geluksscore_meerv_model$residuals
y_values = geluksscore_meerv_model$fitted.values

plot(y_values, geluksscore_residus)
abline(h= 0, col = "red")

# normaliteit residus nagaan

qqnorm(geluksscore_residus); qqline(geluksscore_residus, col = "red")
shapiro.test(geluksscore_residus)

# er zijn lange staarten die afwijken van de qqline, ook naar rechts toe systematisch erboven
# shapiro wilk doet ons H0 verwerpen, dus wijkt volgens de test sterk af van normaliteit ondanks zwakkere test is, 

par(mfrow=c(1,3))
boxplot(ind_happy); boxplot(hh_income); boxplot(health_emo)
par(mfrow=c(1,1))
# op boxplots zien we nogmaals dat ze alle drie scheef verdeeld zijn, aangezien hh_income rechtscheef lijkt log daar enkel te gaan helpen
# de andere zijn linksscheef, dus zal waarschijnlijk niet helpen

geluksscore_residus = geluksscore_meerv_model$residuals
x_hh_income = geluksscore_meerv_model$model$hh_income

plot(x_hh_income, geluksscore_residus)
abline(h= 0, col = "red")

geluksscore_loghhinkomen_model = update(geluksscore_meerv_model, .~.-hh_income + log10(hh_income))

summary(geluksscore_loghhinkomen_model)

# modelveronderstellingen nieuw model

par(mfrow=c(2, 2))
plot(geluksscore_loghhinkomen_model)
par(mfrow=c(1, 1))

# ingezoomd residuplot
geluksscore_loghhinkomen_residus = geluksscore_loghhinkomen_model$residuals
y_values = geluksscore_loghhinkomen_model$fitted.values

plot(y_values, geluksscore_loghhinkomen_residus)
abline(h= 0, col = "red")

# normaliteit residus nagaan

qqnorm(geluksscore_loghhinkomen_residus); qqline(geluksscore_loghhinkomen_residus, col = "red")
shapiro.test(geluksscore_loghhinkomen_residus)


par(mfrow=c(2, 3))

plot(geluksscore_meerv_model, which = 1, main = "geluksscore ~ hh_income")
plot(geluksscore_meerv_model, which = 2, main = "geluksscore ~ hh_income")
plot(geluksscore_meerv_model, which = 3, main = "geluksscore ~ hh_income")

plot(geluksscore_loghhinkomen_model, which = 1, main = "geluksscore ~ log10 hh_income")
plot(geluksscore_loghhinkomen_model, which = 2, main = "geluksscore ~ log10 hh_income")
plot(geluksscore_loghhinkomen_model, which = 3, main = "geluksscore ~ log10 hh_income")

par(mfrow=c(1, 1))



# afzonderlijke vergelijking naargelang geslacht? 

geluksscore_geslacht_groepen = update(geluksscore_meerv_model, .~.*ind_gender)
summary(geluksscore_geslacht_groepen)

geluksscore_geslacht_groepen = update(geluksscore_geslacht_groepen, .~.-hh_income:ind_gender)
summary(geluksscore_geslacht_groepen)

geluksscore_geslacht_groepen = update(geluksscore_geslacht_groepen, .~.-health_emo:ind_gender)
summary(geluksscore_geslacht_groepen)



par(mfrow = c(1,2))

plot(ind_happy ~ health_emo)

abline(lm(ind_happy[ind_gender == "man"] ~ health_emo[ind_gender == "man"]), col = 'blue')
abline(lm(ind_happy[ind_gender == "vrouw"] ~ health_emo[ind_gender == "vrouw"]), col = 'pink')

points(health_emo[ind_gender == "man"], ind_happy[ind_gender == "man"], col = "blue")
points(health_emo[ind_gender == "vrouw"], ind_happy[ind_gender == "vrouw"], col = "pink")

plot(ind_happy ~ log10(hh_income))

abline(lm(ind_happy[ind_gender == "man"] ~ log10(hh_income)[ind_gender == "man"]), col = 'blue')
abline(lm(ind_happy[ind_gender == "vrouw"] ~ log10(hh_income)[ind_gender == "vrouw"]), col = 'pink')


points(log10(hh_income)[ind_gender == "man"], ind_happy[ind_gender == "man"], col = "blue")
points(log10(hh_income)[ind_gender == "vrouw"], ind_happy[ind_gender == "vrouw"], col = "pink")

par(mfrow = c(1,1))

#25503
ind_happy[ind_ID == 25503] #=66
predict(geluksscore_loghhinkomen_model,data.frame(health_emo=58,hh_income=1954), level = 0.95)
predict(geluksscore_loghhinkomen_model,data.frame(health_emo=58,hh_income=1954),
        interval="confidence",level = 0.95)
predict(geluksscore_loghhinkomen_model,data.frame(health_emo=58,hh_income=1954),
        interval="prediction", level = 0.95)
# waarde=64.9363, confidence interval [64,24277; 65,62982], predictie interval [41,97872; 87,89387]
#27010
ind_happy[ind_ID == 27010] #=82
predict(geluksscore_loghhinkomen_model,data.frame(health_emo=13,hh_income=87), level = 0.95)
predict(geluksscore_loghhinkomen_model,data.frame(health_emo=13,hh_income=87),
        interval="confidence",level = 0.95)
predict(geluksscore_loghhinkomen_model,data.frame(health_emo=13,hh_income=87),
        interval="prediction", level = 0.95)
# score=41.18274, confidence interval =[37,72663; 44.63885], predictie interval=  [17.97683; 64.38864]



