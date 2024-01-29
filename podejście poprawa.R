install.packages(c("knitr", "dplyr", "assertive"))
install.packages("validate")
library(knitr)
library(dplyr)
library(assertive)
library(validate)
install.packages("assertr")
library(assertr)
install.packages("parallel")
library(parallel)
install.packages("tidyverse")
library(tidyverse)


#Wprowadzenie danych do R 
dane <- read.csv("pozyczki.csv", na.strings = "")
pozyczki<-data.frame(dane)
head(pozyczki[1:13],4)
str(pozyczki)

#Czyszcznie danych 
## Zmienne
Zmienną objaśnianą jest zmienna `Loan_Status` określająca, 
czy danej osobie przyznano pożyczkę (Y/N).

Zmienne objaśniające to głównie zmienne socjo-demograficzne oraz informacje finansowe.

# Nazwy zmiennych objaśniających
data.frame(zmienna = names(pozyczki)[2:12])

Pierwsza kolumna w bazie danych (`Loan_ID`) określa numer identyfikacyjny, ją możemy pominąć.
# usunięcie pierwszej kolumny
pozyczki <- pozyczki %>%
  select(-1)

#Dodanie kolumny pomocnicczej 
pozyczki <- pozyczki %>%
  mutate(ID = 1:n())

??validator
#Walidacja danych
rules <- validator(
  , Gender %in% c("Male", "Female"),
  , Married %in% c("Yes", "No"),
  , Loan_Status %in% c("Y", "N"),
  , ApplicantIncome >= 0
)

cf <- confront(pozyczki, rules, key="ID")
summary(cf)

barplot(cf, main="pozyczki")

as.data.frame(cf) %>% head()

#Lokalizacja błędów 
??locate_errors
help(locate_errors)
error_locations <- locate_errors(pozyczki, rules)
summary(error_locations)

#Zmiana błędów na NA 
fix_data <- replace(pozyczki, rules)
NA_1 <- sum(is.na(pozyczki))
NA_2 <- sum(is.na(fix_data))
NA_3 <- data.frame(NA_1, NA_2)
kable(NA_3, format = "html", caption = "Brakujące dane") %>%
  kable_styling("striped", "hover", "condensed", full_width = F, position = "left")
))
#Dedukcyjne czyszczenie danych 
lr_imputed <- impute_lr(fixable_data, rules)
cells(start=pozyczki, fixable=fix_data, impute_lr=lr_imputed, compare = 'sequental')
#Konfrontacja danych z zasadami po imputacji 
cf_after_imputation <- confront(lr_imputed, rules, key="ID")
summary(cf_after_imputation)
barplot(cf_after_imputation)

#Brakujące obserwacje 
sum(is.na(lr_imputed))

#Obserwacje odstające 
boxplot(lr_imputed$)
plot(lr_imputed$)










