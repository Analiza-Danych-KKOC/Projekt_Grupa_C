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

Rozkłady zmiennych są bardzo nieregularne. W przypadku trzech pierwszych mamy skrajną 
asymetrię prawostronną (wydłużone ramię z prawej strony histogramu). 
Dla ostatniej zmiennej asymetria jest lewostronna.

