# instalacja i załadowanie niezbędnych pakietów
install.packages("knitr")
library(knitr)
install.packages("tidyverse")
library(tidyverse)
install.packages("gridExtra")
library(gridExtra)
install.packages("psych")
library(psych)
install.packages("ipred")
library(ipred)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# Wstęp
## Opis problemu
Celem niniejszego opracowania jest budowa klasyfikatora, który pozwoli możliwie dokładnie przewidzieć decyzję dotyczącą przyznania pożyczki klientom pewnej firmy pożyczkowej.

## Baza danych
Podstawą analizy jest baza danych klientów firmy pożyczkowej.

# wczytanie danych
dane <- read.csv("pozyczki.csv", na.strings = "")

# wyświetlenie pierwszych wierszy danych
kable(head(dane))

## Zmienne
Zmienną objaśnianą jest zmienna `Loan_Status` określająca, czy danej osobie przyznano pożyczkę (Y/N).

Zmienne objaśniające to głównie zmienne socjo-demograficzne oraz informacje finansowe.

# nazwy zmiennych objaśniających
data.frame(zmienna = names(dane)[2:12])

Pierwsza kolumna w bazie danych (`Loan_ID`) określa numer identyfikacyjny, ją możemy pominąć.
# usunięcie pierwszej kolumny
dane <- dane %>%
  select(-1)

# wymiary bazy danych
dim(dane)

Mamy 614 obserwacji (klientów firmy) opisanych przez 12 zmiennych.

# Analiza eksploracyjna
Rozpoczynamy od analizy struktury poszczególnych zmiennych. Dla zmiennych jakościowych sporządzamy
wykres słupkowy rozkładu procentowego, dla zmiennych ilościowych obliczamy podstawowe 
miary rozkładu i sporządzamy histogram.

## Loan Status
Zmienna `Loan_Status` to zmienna jakościowa wyrażona na skali nominalnej.

# wykres słupkowy dla zmiennej Loan_Status
tab <- as.data.frame(100*prop.table(table(dane$Loan_Status)))
ggplot(tab, aes(x = Var1, y = Freq)) + 
  geom_col(fill = "#FFFF99", colour = "black") +
  geom_text(aes(label = paste0(round(Freq,1),"%")), 
            stat = "identity", size = 5, 
            fontface = "bold", position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(colour = "black", size = 10),
        plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(title = "Loan Status",
       y = "%")

W badanej grupie klientów firmy znalazło się aż 68,7% osób z przyznaną pożyczką. 
Decyzję odmowną uzyskało 31,3% z nich.

Dla wszystkich jakościowych zmiennych objaśniających sporządza się analogiczny wykres. 
Aby nie tworzyć osobnego dla każdej z nich, a tym samym nie mnożyć ich ilości, 
sporządzimy jeden łączny.

# Jakościowe zmienne objaśniające

# wykres słupkowy dla zmiennej Gender
tab <- as.data.frame(100*prop.table(table(dane$Gender)))
plot1 <- ggplot(tab, aes(x = Var1, y = Freq)) + 
  geom_col(fill = "#FFFF99", colour = "black") +
  geom_text(aes(label = paste0(round(Freq,1),"%")), 
            stat = "identity", size = 4, 
            fontface = "bold", position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(colour = "black", size = 10),
        plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(title = "Gender",
       y = "%")

# wykres słupkowy dla zmiennej Married
tab <- as.data.frame(100*prop.table(table(dane$Married)))
plot2 <- ggplot(tab, aes(x = Var1, y = Freq)) + 
  geom_col(fill = "#FFFF99", colour = "black") +
  geom_text(aes(label = paste0(round(Freq,1),"%")), 
            stat = "identity", size = 4, 
            fontface = "bold", position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(colour = "black", size = 10),
        plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(title = "Married",
       y = "%")

# wykres słupkowy dla zmiennej Dependents
tab <- as.data.frame(100*prop.table(table(dane$Dependents)))
plot3 <- ggplot(tab, aes(x = Var1, y = Freq)) + 
  geom_col(fill = "#FFFF99", colour = "black") +
  geom_text(aes(label = paste0(round(Freq,1),"%")), 
            stat = "identity", size = 3, 
            fontface = "bold", position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(colour = "black", size = 10),
        plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(title = "Dependents",
       y = "%")

# wykres słupkowy dla zmiennej Education
tab <- as.data.frame(100*prop.table(table(dane$Education)))
plot4 <- ggplot(tab, aes(x = Var1, y = Freq)) + 
  geom_col(fill = "#FFFF99", colour = "black") +
  geom_text(aes(label = paste0(round(Freq,1),"%")), 
            stat = "identity", size = 4, 
            fontface = "bold", position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(colour = "black", size = 10),
        plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(title = "Education",
       y = "%")

# wykres słupkowy dla zmiennej Self_Employed
tab <- as.data.frame(100*prop.table(table(dane$Self_Employed)))
plot5 <- ggplot(tab, aes(x = Var1, y = Freq)) + 
  geom_col(fill = "#FFFF99", colour = "black") +
  geom_text(aes(label = paste0(round(Freq,1),"%")), 
            stat = "identity", size = 4, 
            fontface = "bold", position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(colour = "black", size = 10),
        plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(title = "Self Employed",
       y = "%")

# wykres słupkowy dla zmiennej Credit_History
tab <- as.data.frame(100*prop.table(table(dane$Credit_History)))
plot6 <- ggplot(tab, aes(x = Var1, y = Freq)) + 
  geom_col(fill = "#FFFF99", colour = "black") +
  geom_text(aes(label = paste0(round(Freq,1),"%")), 
            stat = "identity", size = 4, 
            fontface = "bold", position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(colour = "black", size = 10),
        plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(title = "Credit History",
       y = "%")

# wykres słupkowy dla zmiennej Property_Area
tab <- as.data.frame(100*prop.table(table(dane$Property_Area)))
plot7 <- ggplot(tab, aes(x = Var1, y = Freq)) + 
  geom_col(fill = "#FFFF99", colour = "black") +
  geom_text(aes(label = paste0(round(Freq,1),"%")), 
            stat = "identity", size = 4, 
            fontface = "bold", position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(colour = "black", size = 10),
        plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(title = "Property Area",
       y = "%")

# wykres wspólny
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, nrow = 3)

W przypadku historii kredytowej nie mamy zaetykietowanych odpowiedzi, 
ale najprawdopodobniej oznaczają one 0=no, 1=yes.

Poszczególne zmienne cechuje bardzo duża dysproporcja w rozkładach procentowych kategorii.

# Ilościowe zmienne objaśniające
Dla ilościowych zmiennych objaśniających obliczamy najpierw podstawowe miary rozkładu.

# statystyki opisowe
kable(describe(dane[,6:9])[c(2:5,8,9,11,12)])

W przypadku zmiennych `LoanAmount` i `Loan_Amount_Term` mamy braki danych (n<614). 
W powyższej tabeli zawarte są podstawowe miary położenia, rozproszenia, asymterii i skupienia.

Średni dochód aplikującego o pożyczkę w badanej grupie klientów to 5403,46 dolarów, 
przy czym dochody poszczególnych osób różnią się od średniej przeciętnie o 6109,04 dolarów. 
Dla połowy osób dochód nie przekracza 3812,50 dolarów. 
Najmniejsza zaobserwowana wartość to 150 dolarów, a największa aż 81000 dolarów. 
W rozkładzie występuje skrajna asymetria prawostronna, jest on też wyższy i smuklejszy 
od rozkładu normalnego.

Dla pozostałych zmiennych wyniki interpretuje się analogicznie. 
Zakresy osiąganych wartości są tutaj sensowne, nie ma żadnych wartości ujemnych. 
Jedyny problem stanowią bardzo duże wartości skupienia i kurtozy, na które wpływ 
mają najprawdopodobniej obserwacje odstające.

Do prezentacji graficznej sporządzamy histogramy.

# histogramy
plot1 <- ggplot(dane, aes(x = ApplicantIncome)) + 
  geom_histogram(colour = "black", fill = "#FFFF99", bins = 10) +
  labs(title = "Applicant Income", 
       x = "dollars", y = "n") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

plot2 <- ggplot(dane, aes(x = CoapplicantIncome)) + 
  geom_histogram(colour = "black", fill = "#FFFF99", bins = 10) +
  labs(title = "Coapplicant Income", 
       x = "dollars", y = "n") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

plot3 <- ggplot(dane, aes(x = LoanAmount)) + 
  geom_histogram(colour = "black", fill = "#FFFF99", bins = 10) +
  labs(title = "Loan Amount", 
       x = "dollars", y = "n") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

plot4 <- ggplot(dane, aes(x = Loan_Amount_Term)) + 
  geom_histogram(colour = "black", fill = "#FFFF99", bins = 10) +
  labs(title = "Loan Amount Term", 
       x = "months", y = "n") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)

Rozkłady zmiennych są bardzo nieregularne. 
W przypadku trzech pierwszych mamy skrajną asymetrię prawostronną (wydłużone ramię z prawej strony histogramu). 
Dla ostatniej zmiennej asymetria jest lewostronna.



