##lab 8
#zad1
#konieczna bliblioteka moments
p.opisowe=function(x){
  library(moments)
  średnia=mean(x)
  kwartyl.1=quantile(x,0.25)
  mediana=median(x)
  kwartyl.3=quantile(x,0.75)
  min=min(x)
  max=max(x)
  rozstęp.empiryczny=max(x)-min(x)
  rozstęp.kwartylowy=IQR(x)
  odchylenie.standardowe=sd(x)
  współczynnik.zmienności=sd(x)/mean(x)
  skośność=skewness(x)
  kurtoza=kurtosis(x)
  round(rbind(średnia,kwartyl.1,mediana,kwartyl.3,min,max,rozstęp.empiryczny,rozstęp.kwartylowy,odchylenie.standardowe,współczynnik.zmienności,skośność,kurtoza),2)
}
p.opisowe(Ankieta$Waga)

#zad2
#a)
p.opisowe(Ankieta.M$Wzrost)
#KWARTYL DOLNY - wzrost 25% mężczyzn w badanej grupie nie przekroczył 178.00cm
#ODCHYLENIE STANDARDOWE - wzrost mężczyzn odchylał się od średniej wagi przeciętnie o około 5.77
#WSPÓŁCZYNNIK ZMIENNOŚCI - udział odchylenia standardowego wzrostu w wartości średniej wynosi 0.03, co znaczy że mężczyźni są słabo zróżnicowani pod względem wzrostu
#WSPÓŁCZYNNIK SKOŚNOŚCI - rozkład wzrostu mężczyzn charakteryzuje się słabą asymetrią lewostronną

#b)
p.opisowe(Ankieta$Średnia)
#MEDIANA - Średnia ocen z kursów 50% studentów w badanej grupie nie przekroczyła 3.90
#KWARTYL GÓRNY - Średnia ocen z kursów 75% studentów w badanej grupie nie przekroczyła 4.30

#c)
p.opisowe(Ankieta$L.godzin)
#ŚREDNIA - średnia ilość czasu spędzona przy komputerze w badanej grupie skupia się wokół wartości 6.47
#KURTOZA - rozkład ilości czasu spędzonej przy komputerze charakteryzuje się wyższym skupieniem wokół średniej ilości czasu spędzonego przed komputerem niż rozkład normalny


#LAB 9
#zad 1
#a) H0 - brak różnic, rozkład wzrostu jest normalny
#   H1 - ~H0
shapiro.test(Ankieta.M$Wzrost)
# w = 0.97595  p-value = 0.3587 alfa(poziom istotności) = 0.01
#Wniosek: alfa<p-value, więc na poziomie istotności 0.01 nie ma podstaw do odrzucenia H0, że rozkłąd wzrostu w populacji mężczyzn jest zgodny z rozkładem normalnym.

#b) Wyznaczanie przedziału ufności dla wartości średniej w populacji (m)
t.test(Ankieta.M$Wzrost, conf.level = 0.95)
#przedział -  179.9683 ; 183.1487
#INTERPRETACJA - Przedział liczbowy o końcach 179.9683 i 183.1487 z prawdopodobieństwem 0.95 obejmuje nieznaną średnią wartość w populacji mężczyzn studiujących na pierwszym roku WI

t.test(Ankieta.M$Wzrost, conf.level = 0.98)
#PRZEDZIAŁ -  179.6564 183.4606
#WNIOSEK - Zwiększenie poziomu ufności rozszerza przedział ufności.

#c)
przedział.odchylenie(Ankieta.M$Wzrost, 0.97)
#INTERPRETACJA - Przedział liczbowy o końcach 4.7553cm i 7.3104 cm z poziomem ufności 0.97 obejmuje rzeczywistą wartość odchylenia standardowego.)
p.opisowe(Ankieta.M$Wzrost)

#d)
#H0 - ŚREDNI WZROST = 181CM
#H1 - ŚREDNI WZROST >181CM
t.test(Ankieta.M$Wzrost, mu = 181, conf.level = 0.01)
#p-value = 0.4841 alfa = 0.01
#WNIOSEK - alfa < p-value, więc nie mamy podstaw do odrzucenia H0. Nie można zatem potwierdzić hipotezy, że średni wzrost w populacji studentów pierwszego roku jest większy niż 181 cm

#zad 2
#a)
table(Ankieta$Sz.średnia)
prop.test(21, 59, conf.level = 0.98)
#INTERPRETACJA - Przedział ufności dla Przedział liczbowy o końcach przedziału 0.2216589 i 0.515768 obejmuje odsetek osób, które ukońćzyły LO(RM) w przedziale z ufnością 0.98

#b)test istotności dla wskaźnika struktury(odsetka, frakcji) w populacji
#H0 - p=20%
#H1 - p>20%
prop.test(21, 59, p = 0.2, alternative = 'g')
#X-Squared (stat.test chi^2) = 8.018
#alfa = 0.05 p-value = 0.002316
#WNIOSEK - alfa > p-value, więc odrzucamy H0 i przyjmujemy H1, więc odsetek osób które ukończyły LO(RM) jest istotnie większy niż 20% na poziomie istotności 0.05

#Zad 3
#a)
#H0 - brak różnic, rozkład normalny
#H1 - ~H0
shapiro.test(Ankieta.K$Waga)
#w = 0.95486 p-value = 0.7794 alfa = 0.05
#WNIOSEK - alfa < p-value, brak podstaw do odrzucenia hipotezy H0, więc rozkład wagi kobiet na poziomie istotności 0.05 jest normalny

#b) przedział ufności dla średniej wagi kobiet
t.test(Ankieta.K$Waga, conf.level = 0.98)
#przedział ufności:  47.89815 63.76851
#WNIOSEK: - Przedział liczbowy o końcach 47.89815 i 63.76851 z prawdopodobieństwem 0.98 obejmuje nieznaną wartość średniej wagi wśród studentek na 1 roku WI

#c) przedział ufności dla odchylenia standardowego wagi
przedział.odchylenie(Ankieta.K$Waga, 0.95)
#przedział: 3.6057, 14.1673
#WNIOSEK - Przedział liczbowy od3.6057 do 14.1673 z prawdopodobieństwem 0.95 obejmuje nieznaną rzeczywistą wartość odchylenia standardowego wagi studentek 1-go roku WI.

#d) test dla 1 średniej w populacji
#H0 - średnia waga = 65
#H0 - średnia waga < 65
t.test(Ankieta.K$Waga, mu = 65, conf.level = 0.05, alternative = 'l')
#alfa = 0.05 p-value = 0.005779
#WNIOSEK - alfa > p-value, więc odrzucamy hipotezę H0 i przyjmujemy H1, czyli możemy potwierdzić że średnia waga studentek pierwszego roku WI przy poziomie istotności 0.05 jest mniejsza niż 65kg.

#Zad 4
table(Ankieta$M.zamieszkania)
#a)przedział ufności dla wskaźnika struktury(odsetka frakcji elementów wyróżnionych) w populacji
prop.test(23, 59, conf.level = 0.995)
#INTERPRETACJA(bez jednostek) - Przedział liczbowy o końcach 0.2280294 i 0.5787470 obejmuje odsetek osób, które mieszkają w akademiku podczas studiowania.

#b) 
#H0 - p = 30%
#H1 - ~H0
prop.test(23, 59, p = 0.3, conf.level = 0.05, alternative = 'two.sided')
#stat.test chi^2 = 1.8596
#p-value = 0.1727 alfa = 0.05
#WNIOSEK - alfa<p-value, więc nie ma podstaw do odrzucenia H0, nie można zatem potwierdzić hipotezy że odsetek osób mieszkających w akademiku różni się istotnie od 30%


#LAB 10
#zad 1 stawiamy 4 hipotezy
#a)
#H00 - rokład wzrostu kobiet jest normalny
#H01 - ~H00
#H10 - rozkład wzrostu mężczyzn jest normalny
#H11 - ~H11
by(Ankieta$Wzrost, Ankieta$Płeć, shapiro.test)
#KOBIETY = p-value = 0.8157, alfa = 0.01
#MĘŻCZYŹNI = P-value = 0.3587 alfa = 0.01
#WNIOSEK - Na poziomie istotności 0.01 nie ma podstaw do odrzucenia hipotez H00 i H10, tak więc rozkład wzrostu w populacji obu grup jest normalny.

#b)
#H0 -  (sigma.k)^2 = (sigma.m)^2
#H1 - ~H0
var.test(Ankieta$Wzrost ~ Ankieta$Płeć)
var.test(data = Ankieta, Wzrost~Płeć)
#F = 0.72605
#p-value = 0.786 alfa = 0.01
#WNIOSEK - alfa<p-value, więc brak podstaw do odrzucenia hipotezy H0, zatem różnice między wariancjami w obu populacjach nie są istotne

#c) test Studenta dla dwóch średnich bez poprawki Welcha
#H0 - m.K = m.M
#H1 - m.K < m.M
t.test(data=Ankieta, Wzrost~Płeć, alternative='l', var.equal = TRUE)
#p-value = 0.0000003332 alfa = 0.01
#WNIOSEK - alfa>p-value, więc na poziomie istotności odrzucamy hipotezę H0 i przyjmujemy H1, czyli średni wzrost w populacji studentek jest niższy niż średni wzrost w populacji studentów.

#zad 2
table(Ankieta$M.zamieszkania, Ankieta$Sz.średnia)
#1 - mieszkający z rodziną
#2 - mieszkający poza domem
#H0 - p1 = p2
#H1 - p1>p2
prop.test(c(9 ,12), c(26, 33), alternative = 'g')
#p-value = 0.5 alfa = 0.01
#WNIOSEK - alfa < p-value, więc na poziomie istotności 0.01 nie ma podstaw do odzucenia H0, tak więc odsetki nie różnią sięistotnie dla mieszkających podczas studiowania z rodziną oraz poza domem rodzinnym

#zad 3
#test dla dwóch średnich(próby zależne)
#x1 = poziom choleterolu przed kuracją
x1 = c(225,236,312,238,241,196,205,259,218)
#x2 - poziom cholesterolu po kuracjii
x2 = c(216,195,245,235,221,170,180,265,179)
#założenie: normalność różnicy rozkładu zmiennych
#H0: ROZKŁAD JEST NORMALNY
#H1: ~H0
shapiro.test(x1-x2)
#p-value = 0.8858 alfa = 0.05
#WNIOSEK - alfa<p-value, więc nie ma podstaw do odrzucenia H0

#test studenta dla dwóch średnich(próby zależne)
#H0: m1-m2 = 0
#H1: m1-m2>0
t.test(x1, x2, paired=T, alternative = 'g')
#p-value = 0.004942 alfa - 0.01
#WNIOSEK - alfa>p-value, więc odrzucamy hipotezę H0 i przyjmujemy H1, czyli na poziomie istotności 0.01 można stwierdzić, że leczenie statynami skutecznie wpływa na leczenie cholesterolu

#zad 4 test dla dwóch średnich (próby niezależne)
#H00 - rozkład wagi studentów jest normalny
#H01 - ~H00
#H10 - rozkłąd wagi studentek jest normalny 
#H11 - ~H10
by(Ankieta$Waga, Ankieta$Płeć, shapiro.test)
#KOBIETY - p-value = 0.7794 alfa = 0.05
#MĘŻCZYŹNI - p-value = 0.8044 alfa 0.05
#WNIOSEK - W obu przypadkach alfa<p-value, więc na poziomie istotności 0.05 nie ma podstaw do odrzucenia H00 i H10, czyli rozkłady wagi studentek i studentów są normalne.

#sprawdzenie jednorodności wariancji
#H0 = (sigma.k)^2 = (sigma.m)^2
#H1 = ~H0
var.test(Ankieta$Waga ~ Ankieta$Płeć)
#p-value = 0.09827 alfa = 0.05
#Wniosek = alfa<p-value, więc nie ma podstaw do odrzucenia hipotezy H0, czyli na poziomie istotności 0.05 różnice między obiema wariancjami nie sąstatystycznie istotne

#test studenta bez poprawki Welcha
#H0 = m.K = m.M
#H1 = ~H0
t.test(data = Ankieta, Waga~Płeć, var.equal = TRUE)
#p-value = 0.00001251 alfa = 0.05
#WNIOSEK = alfa>p-value, więc na poziomie istotności 0.05 odrzucamy hipotezę H0 i przyjmujemy H1, czyli średnia waga studentek i studentów nie jest taka sama.

#zad 5
#zweryfikować hipoteze, że odsetek absolwentów TI jest mniejszy w grupie osób mieszkających poza domem rodzinnym
#1. mieszkający poza domem rodzinnym
#2. mieszkający z rodzicami
#H0 = p1 = p2
#H1 = p1 < p2
table(Ankieta$M.zamieszkania, Ankieta$Sz.średnia)
prop.test(c(12, 16), c(33, 26), alternative = 'l')
#chi^2 = 2.7555
#p-value = 0.04846 alfa = 0.01
#wniosek: alfa<p-value, więc na poziomie istotności 0.01 nie ma podstaw do odrzucenia hipotezy H0, czyli odsetki absolwentów TI nie różniły się istotnie dla osób, które podczas studiowania mieszkały poza domem rodzinnym oraz dla tych mieszkających z rodziną.

#Zad 6
#x1 wagi kobiet przed odchudzaniem, x2 wagi kobiet po odchudzaniu
x1 = c(87.5,56,67,82.5,92,59,90.5,80.5,65,92)
x2 = c(86,54,66,83,87,62,87,90,61,70)

#założęnie: normalność różnicy rozkłądu zmiennych
#H0 - rozkład różnicy zmiennych jest normalny
#H1 - ~H0
shapiro.test(x1-x2)
#p-value = 0.04333 alfa = 0.01
#WNIOSEK - alfa<p-value, czyli na poziomie istotności 0.01 nie ma podstaw do odrzucenia hipotezy H0, czyli rozkład różnicy zmiennych jest normalny

#test studenta dla dwóch średnich(próba zależna)
#H0 - m1-m2 = 0
#H1 - m1 - m2 > 0
t.test(x1, x2, paired = TRUE, alternative = 'g')
#p-value = 0.1655, alfa = 0.01
#WNIOSEK: alfa<p-value, więc na poziomie istotności 0.01 brak podstaw do odrzucenia H0, więc można stwierdzić, że dieta nie wpływa istotnie na wagę w populacji kobiet.

#zad 7.
#1. Kobiety
#2. Mężczyźni
#H0 = p1 = p2
#H1 = p1<p2
table(Ankieta$M.zamieszkania, Ankieta$Płeć)
prop.test(c(3, 20), c(6, 53), alternative = 'l')
#chi^2 = 0.020223 p-value = 0.5565 alfa = 0.02
#WNIOSEK - alfa<p-value, więc na poziomie istotności 0.02 nie ma podstaw do odrzucenia H0, zatem odsetek osób mieszkających w akademiku nie różni się istotnie dla grupy kobiet oraz grupy mężczyzn.

#zad 9.
#x1 = wydajność pracowników przed szkoleniem
#x2 = wydajność pracowników po szkoleniu 
x1 = c(46,34,40,42,41,48,49,33)
x2 = c(40,41,42,55,51,42,33,44)
#próby zależne od siebie
#sprawdzenie normalności różnicy
#H0 = rozkład różnicy jest normalny
#H1 = ~H0
shapiro.test(x1-x2)
#p-value = 0.3579 alfa = 0.02
#wniosek: alfa<p-value, więc na poziomie istotności 0.02 nie ma podstaw do odrzucenia hipotezy H0, czyli rozkład różnicy zmiennych jest normalny

#test studenta dla dwóch średnich
#H0 = m1-m2 = 0
#H0 = m1-m2 > 0
t.test(x1, x2, paired = TRUE, alternative = 'l')
#p-value = 0.3313 alfa = 0.02
#WNIOSEK: alfa<p-value, więc na poziomie istotności 0.02 nie ma podstaw do odrzucenia hipotezy H0, zatem możemy stwierdzić, że szkolenie nie było efektywne.


#Lab 11
#zad 1.
#trzy próby - I KOMBINACJA, II KOMBINACJA, III GRUPA KONTROLNA
#jeżeli mamy trzy próby, to wszystkie wyniki wpisujemy do wektora oddzielając je przecinkami
#x1 = próby 
x1 = c(2.6,2.4,2.0,1.8,2.2,1.5,1.5,1.4,1.2,1.5,1.2,1.5,0.8,1.0)
#x2 = zmienna grupująca
x2 = c(rep('1',5), rep('2', 4), rep('3',5))

#a)badanie normalności rozkładu

#h00: rozkłąd plonów przy nawożeniu I jest normalny
#H01: ~H00

#H10: rozkład plonów przy nawożeniu II jest normalny
#H11: ~H10

#H20: rozkład plonów w grupie kontrolnej jest normalny
#H21: ~H20

by(x1, x2, shapiro.test)
#kombinacja I: p-value = 0.9672
#kombinacja II: p-value = 0.1612
#kombinacja III: p-value = 0.4292
#alfa = 0.05
#WNIOSEK = W każdym przypadku alfa<p-value, więc na poziomie istotności 0.05 nie ma podstaw do odrzucenia żadnej z hipotez H0, czyli rozkład plonów każdej kombinacji jest normalny

#b) badanie jednorodności wariancji (jednorodność = sigma)
#H0 = (sigma1)^2=(sigma2)^2=(sigma3)^2
#H1 = ~H0
bartlett.test(x1 ~ x2)
#p-value = 0.3986 alfa = 0.05
#WNIOSEK: alfa<p-value, więc na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy H0

#c)test ANOVA
#H0: m1=m2=m3 brak wpływu nawozu na ilość plonó
#H1: ~H0 wpływ istotny

#f value -> F
#Pr(>F) -> p-value

anova(aov(x1~x2))
#F = 18.053
#p-value(Pr(<F)) = 0.0003356
#alfa = 0.05
#WNIOSEK: alfa>p-value, więc z poziomem istotności 0.05 odrzucamy hipotezę H0 i przyjmujemy H1, czyli nawożenie ma istotny wpływ na plon rzepaku jarego.

#Zad 2
#jedna grupa - studenci, jedna zmienna - czas przed kompem

#sprawdzenie normalności rozkładu:
#H0 - rozkład jest normalny
#H1 - ~H0
shapiro.test(Ankieta$L.godzin)
#W - 0.90712 p-value = 0.0002724, alfa = 0.05
#WNIOSEK: alfa>p-value, więc na poziomie istotności 0.05 odrzucamy hipotezę H0 i przyjmujemy H1, czyli rozkład nie jest normalny.

#rozkład nie jest normalny, więc korzystamy z testu Wilcoxona dla jednej populacji
#H0 = me = 5.5
#H1 = me > 5.5 
#mu = 5.5
wilcox.test(Ankieta$L.godzin, mu = 5.5, alternative = 'g')
#p=value = 0.02371 alfa - 0.05
#WNIOSEK - alfa>p-value, więc na poziomie istotności 0.05 odrzucamy hipotezę H0 i przyjmujemy H1, czyli studenci I roku WI przeciętnie spędzają przed komputerem więcej niż 5.5 godziny na dobę.

#zad 3.
#dwie próby niezależne
#x1 = miesięczy utarg w obu miastach
#x2 = zmienna mierzalna
x1 = c(7.6,12.0,7.3,11.3,7.0,10.8,6.5,8.1,3.2,8.7,8.8,11.7,12.7,18.5,3.3,6.7,8.6,6.9,3.8,3.7)
x2 = c(rep("A", 13), rep("B", 7))

#badanie normalności rozkładów
#H00: rozkład utargów w mieście A jest normalny
#H01: ~H00

#H10: rozkład utargów w mieście B jest normalny
#H11: ~H10

by(x1, x2, shapiro.test)
#miasto A: p-value = 0.5341
#miasto B: p-value = 0.01989
#alfa = 0.05
#WNIOSEK: W przypadku miasta A nie ma podstaw do odrzucenia H0, a w przypadku miasta B odrzucamy H0 na rzecz H1

#H0 A = B
#H1 A > B
wilcox.test(x1 ~ x2, alternative = 'g')
#p-value = 0.0674 alfa = 0.05
#WNIOSEK: alfa<p_value, więc na poziomie istotności 0.05 nie ma podstaw do odrzucenia hipotezy H0, zatem nie można potwierdzić, że miesięczne utargi w mieście A są większe niż w mieście B


#Zad 4 
#x1 - 1 dzien eksperymentu
#x2 - 10 dzien eksperymentu
x1 = c(85,122,162,206,121,250,200,156)
x2 = c(87,82,158,96,131,121,194,130)
#dwie próby zależne
#badanie czy rozkład różnicy zmiennych jest normalny

#H0 - rozkład różnicy x1 i x2 jest normalny
#H1 - ~H0

shapiro.test(x1-x2)
#p-value = 0.04487 alfa = 0.05
#Wniosek: alfa>p-value, więc na poziomie istotności 0.05 odrzucamy hipotezę H0 i przyjmujemy H1

#H0 = me1-me2 = 0
#H1 = me1-me2 >0
wilcox.test(x1, x2, paired=TRUE, alternative = 'g')
#p-value = 0.03946 alfa = 0.05
#WNIOSEK = alfa>p-value, więc na poziomie istotności 0.05 odrzucamy hipotezę H0 i przyjmujemy H0, zatem czas potrzebny myszom na wyjście z labiryntu skraca się po 10 dniach treningu.

#zad 5
#x1 = frekwencja we wszystkich miastach
#x2 = zmienna grupująca
x1 = c(32.5, 40.8, 41.7, 41.2, 37.9, 38.3, 42, 39.8, 43.1, 42.6,38.9, 43.1, 40.4, 41.8, 42, 39, 43.7, 40, 39.7, 43, 43.1,43.9, 44.2, 45.2, 44.6, 42.5, 43.4, 44.8, 42.8, 43.1, 44.8, 45)
x2 = c(rep('WRO', 10), rep('WAR', 11), rep('KRK', 11))

#test jednorodności wariancji
#H0 = (sigmaWRO)^2 = (sigmaWAR)^2 = (sigmaKRK)^2
#H1 = ~H0
bartlett.test(x1~x2)
#p-value = 0.002775 alfa = 0.05
#WNIOSEK = alfa> p-value, więc na poziomie istotności 0.05 odrzucamy hipotezę H0 na rzecz H1

#test nieparametryczny Kruskala = Wallisa
#H0 = meWRO = meWAR = meKRK
#H1 = ~H-
kruskal.test(x1~x2)
#p-value = 0.0002259, alfa = 0.05
#WNIOSEK = alfa>p-value, więc na poziomie istotności 0.05 odrzucamy hipotezę H0 na rzecz H1, tzn że frekwencja w wyborach prezydenckich dla podanych miast nie była taka sama.


#LAB 12
#dwie cechy mierzalne - regresja
#wysokość niezależna:
x = c(150,300,450,600,750,900,1200,1500,1800)
#temperatura zależna
y = c(27.2,26.0,24.2,22.5,21.7,20.5,19.0,17.4,16.0)
#a) wykres punktów empirycznych (diagram korelacyjny)
plot(x,y)

#b)oszacowanie wzoru liniowej funkcji regresji
lm(y~ x)
#wzór liniowej funkcji regresji to: y = 27.322424 - 0.007 * x

#interpretacja współczynnika regresji: -0.007 oznacza, że wraz ze wzrostem wysokości o 100 metrów, spada średnio biorąc temperatura powietrza o około 0.007 stopni celsjisza

#c) wykres wyznaczonej w b) funkcji regresji
abline(lm(y~ x))


#d) test istotności współczynnika regresji
#H0 - beta1 = 0 (współczynnik regresji nieistotny)
#H1 - ~H0 (regresja istotna)

summary(lm(y~ x))
#p-value = 0.00000148 alfa - 0.03
#WNIOSEK - alfa>p-value, więc na poziomie istotności 0.03 odrzucamy hipotezę H0 na rzecz H1, zatem regresja temperatury względem wysokości jest istotna

#zad 2
#dwie cechy mierzalne -korelacja

#a)
#współczynnik korelacji = r
cor(Ankieta$Waga, Ankieta$Wzrost)
#r = 0.5004893
#INTERPRETACJA: Współczynnik korelacji wynosi 0.5004893, to znaczy że między wzrostem a wagą studentów istnieje korelacja dodatnia umiarkowana, czyli im większa waga to większy wzrost studenta

#b) poziom istotności dla współczynnika korelacji w populacji (ro)
#ufność = 0.95
cor.test(Ankieta$Waga, Ankieta$Wzrost, conf.level=0.95)
#INTERPRETACJA: Przedział liczbowy od 0.28 do 0.67 z prawdopodobieństwem 0.95 obejmuje nieznany współczynnik korelacji między wagą a wzrostem w populacji studentów I-go roku WI

#c) 
#H0 - RO = 0
#H1 - RO > 0
cor.test(Ankieta$Waga, Ankieta$Wzrost, conf.level = 0.01, alternative = 'g')
#p-value = 0.0000271 alfa = 0.01
#WNIOSEK: alfa>p-value, więc na poziomie istotności 0.01 odrzucamy hipotezę H0 i przyjmujemy H1, zatem korelacja między wagą a wzrostem studentów jest istotnie większa od zera.

#Zad 3
#dwie cechy niemierzalne - test niezależności chi^2
#H0 = cechy są niezależne
#H1 = ~H0

chisq.test(matrix(c(152,52,8,188),2,2))
#p-value = 2.2e-16 (bardzo małe) alfa - 0.01
#WNIOSEK: alfa>p-value, więc na poziomie istotności 0,01 odrzucamy hipotezę H0 na rzecz H1, to znaczy że rośliny cebuli wychodowane z większych cebulek wyrastają częściej w pędy kwiatostanowe.

#zad 4
#badamy niezależność
#H0 - cechy są niezależne
#H1 - ~H0
chisq.test(Ankieta$Sz.średnia, Ankieta$Płeć)
#p-value = 0.04881 alfa = 0.05
#WNIOSEK: alfa>p-value, więc na poziomie istotności 0.05 odrzucamy hipotezę H0 na rzecz H1, czyli prawdą jest, że istnieje związek między płcią studentów a ukończoną przez nich szkołą.

#zad 5
#a) Badanie współczynnika korelacji
cor(Ankieta$MSzS1, Ankieta$Algebra)
#r = 0.55
#INTERPRETACJA: Współczynnik korelacji wynosi 0.55, tzn że korelacja jest umiarkowana dodatnia, czyli im lepsza ocena z MSzS, tym lepsza ocena z Algebry

#b)
#H0 = RO = 0
#H1 = ~H0
cor.test(Ankieta$MSzS1, Ankieta$Algebra, alternative = 'two.sided')
#p-value = 0.000005847 alfa = 0.01
#WNIOSEK: alfa>p-value, więc na poziomie istotności 0.01 odrzucamy hipotezę h0 na rzecz h1, czyli korelacja między ocenami z MSzS a ocenami z algebry istotnie różni się od 0

#zad 6
#dwie cechy mierzalne - regresja
#x wielkość produkcji niezależna
#y liczba braków zależna
x = c(0.8, 1.2, 1.6, 1.8, 2.2, 1.6, 2.4, 2.0)
y = c(6, 10, 12, 15, 18, 15, 20, 16)

#a)
cor(x, y)
# r = 0.98
#INTERPRETACJA: Współczynnik korelacji wynosi 0.98, co znaczy że między ilością wielkością produkcji a liczbą braków występuje bardzo silna korelacja dodatnia, tzn im większa produkcja tym więcej braków.

#b) przedział ufności dla współczynnika korelacji w populacji (ro)
cor.test(x, y, conf.level = 0.98)
#INTERPRETACJA: Przedział liczbowy od  0.8581196 do 0.9976229 z ufnościa 0.98 obejmuje nieznany współczynnik między wielkością produkcji a liczbą braków w populacji części zamiennych.

#c) 
lm(y~ x)
#intercept = -0.3438 współczynnik regresji = 8.4375
#wzór y= 8.4375x - 0.3438

#INTERPRETACJA: Współczynnik regresji równy 8,4375 oznacza, że ze wzrostem produkcji o tysiąc sztuk, liczba braków wzrasta o około 8 sztuk

#d) test istotności współczynnika regresji

#H0 Beta1=0 (współczynnik regresji nieistotny)
#H1 ~H0 (regresja istotna)
summary(lm(y~x))
#p-value = 0.0000166 alfa = 0.01
#WNIOSEK: alfa>p-value, więc na poziomie istotności 0.01 odrzucamy hipotezę H0 na rzecz H1, czyli regresja liczby braków względem wielkości produkcji jest istotna.

#zad 7 test niezależności chi^2
#H0 - zmienne są niezależne
#H1 - ~H0
chisq.test(matrix(c(12,8,18,11,18,19,11,23,16,21,15,10,16,17,10,15),4,4))
#p-value = 0.1291 alfa = 0.05
#WNIOSEK: alfa<p-value, więc na poziomie istotności 0.05 nie ma podstaw do odrzucenia H0, co znaczy że nie można potwierdzić hipotezy że grubość książki wpływa na jej popularność

