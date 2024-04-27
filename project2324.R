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

#p0 = 4 368 849 / 8 982 329 = proportie vrouwen populatie

table(ind_gender); table(ind_gender)/length(ind_gender)
data.frame("man SB" = 4368849/8982329, "vrouw SB" = 4613480/8982329)

binom.test(length(ind_gender[ind_gender == "man"]), length(ind_gender), p = 4368849/8982329, alternative = "two.sided")
2*(1 - pbinom(length(ind_gender[ind_gender == "man"]) - 1 , length(ind_gender), 4368849/8982329))

binom.test(length(ind_gender[ind_gender == "vrouw"]), length(ind_gender), p = 4613480/8982329, alternative = "two.sided")
2*(1 - pbinom(length(ind_gender[ind_gender == "vrouw"]) - 1 , length(ind_gender), 4613480/8982329))
# p-value = 0.2486, probability of success mannen = 0.4722864, probability of success vrouwen = 0.5277136, grote p-waarde => er is geen signififcant verschil

# - verdeling leeftijd in 2016 van onderzoek == verdeling volgens StatBel?

age_ranges = cut(ind_age, breaks = c(0, 30, 40, 50, 60, 70, Inf), right = FALSE)
table(age_ranges)
age_ranges_abs_freq = c(141, 289, 331, 335, 303, 333)


expected_age_ranges_rel_freq = c(0.19, 0.16, 0.17, 0.18, 0.14, 0.16) 

# alle verwachte frequenties > 5 dus oké

leeftijd_verdeling_test = chisq.test(age_ranges_abs_freq, p = expected_age_ranges_rel_freq)

leeftijd_verdeling_test$observed
leeftijd_verdeling_test$expected
leeftijd_verdeling_test$residuals

# p-waarde is praktisch 0, dus de verdeling vd steekproef wijkt sterk significant af van verdeling bij de belgen

# de 0-30 jaar groep is zeer sterk ondervertegenwoordigd maw véél te weinig van die leeftijdscategorie bevraagd
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
length(geluksscore_man);length(geluksscore_vrouw) 
# Ja want beide n >>> 30

# 1) Normaliteit?

shapiro.test(geluksscore_man); shapiro.test(geluksscore_vrouw)
boxplot(ind_happy ~ ind_gender, xlab = "geslacht", ylab = "geluksscore")

# beide p-waarden bijna 0, dus wijken té sterk af van normaal verdeeld voor de F-test, we zien ook rechtsscheve verdeling
# op histogrammen en boxplots

# 2) t-test ongepaard met ongelijke varianties

t.test(geluksscore_man, geluksscore_vrouw, paired = FALSE, var.equal = FALSE, alternative = "two.sided")

# p-waarde: 0.3778 >> 0.05 dus we verwerpen H1, de afwijking is toeval
# Besluit: obv de steekproef vinden we geen significante afwijking tussen de gelukscores van mannen en vrouwen



# Naargelang men betaald werk uitvoert? 

# test voor twee gemiddelden, ongepaarde groepen want we verdelen één veranderlijke in 2 groepen
# X = gelukscore van personen met betaald werk, Y = gelukscore van personen zonder betaald werk
# H0: mu_X = mu_Y, H1: mu_X != mu_Y   
geluksscore_betaald_werk = ind_happy[ind_atwork == "ja"]
geluksscore_geen_betaald_werk = ind_happy[ind_atwork == "nee"]
mean(geluksscore_betaald_werk, na.rm = TRUE); mean(geluksscore_geen_betaald_werk, na.rm = TRUE)

# 0) geldt CLS
length(geluksscore_betaald_werk)
length(geluksscore_geen_betaald_werk) 
# Ja want beide n >>> 30

# 1) Normaliteit?

shapiro.test(geluksscore_betaald_werk); shapiro.test(geluksscore_geen_betaald_werk)
boxplot(ind_happy ~ ind_atwork, xlab = "betaald werk", ylab = "geluksscore")

# beide p-waarden bijna 0, dus wijken té sterk af van normaal verdeeld voor de F-test, we zien ook rechtsscheve verdeling
# op histogrammen en boxplots

# 2) t-test ongepaard met ongelijke varianties

t.test(geluksscore_betaald_werk, geluksscore_geen_betaald_werk, paired = FALSE, var.equal = FALSE, alternative = "two.sided")

# p-waarde: 1.658 * 10^-7 <<< 0.05 dus we verwerpen H0, de afwijking is geen toeval maar significant
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
length(geluksscore_jongkind_en_samenwonend); length(geluksscore_jongkind_en_alleenwonend)
# Ja want beide n >>> 30

# 1) Normaliteit?

shapiro.test(geluksscore_jongkind_en_samenwonend); shapiro.test(geluksscore_jongkind_en_alleenwonend)
boxplot(geluksscore_jongkind_en_samenwonend, geluksscore_jongkind_en_alleenwonend, names = c("samenwonend", "alleenwonend"), ylab = "geluksscore")

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



#3.3.4 Verklaren van de gelukscore
# Eenvoudig regressiemodel voor geluk in functie van het totale beschikbare inkomen van het gezin en het tweede in functie van het tiendelige logaritmevan dat inkomen.
model1=lm(ind_happy,hh_income)
summary(model1)
plot(ind_happy~hh_income)
abline(model1, col='red')
#Multiple R-squared:  0.02271,	Adjusted R-squared:  0.02215, F-statistic: 40.21 on 1 and 1730 DF,  p-value: 2.906e-10, t-waarde=6.341
#kleine p-waarde en R-squared dus weinig verklaring van het geluk

