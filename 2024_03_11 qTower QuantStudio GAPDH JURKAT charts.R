library(ggplot2)
library(readr)
library(ggrepel)
library(stats)
library(ggsignif)
library(dplyr)

setwd("~/Documents/R")

# Wykres Ct z qTower------------------------------------------------------------
#Tworzenie wykresu z podanych przez qTower wartości ct

# Wczytanie danych
dane_ct <- read_csv("2024_02_27_GAPDH_CT.csv")

# Dodanie kolumny "Row" zawierającej pierwsze litery z kolumny "Well"
dane_ct$Row <- substr(dane_ct$Well, 1, 1)

# Przypisanie kolumny do zmiennej
dane_qt <- dane_ct[, c("Row", setdiff(names(dane_ct), "Row"))]

# Test ANOVA qTower
wynik_anova_qt <- aov(Ct ~ factor(Row), data = dane_qt)
print(wynik_anova_qt)
summary(wynik_anova_qt)

# Test post-hoc Tukey qTower
wynik_tukey_qt <- TukeyHSD(wynik_anova_qt)
print(wynik_tukey_qt)

# Wykres punktowy z ramką-wąsami
wykres_qtow <- ggplot(dane_qt, aes(x = factor(Row), y = Ct)) +
  geom_boxplot() +
  geom_point() + # Ew. rozsunięcie punktów: geom_point(position = position_jitter(width = 0.2), alpha = 0.5))
  labs(title = "GAPDH Ct on qTower3",
       x = "Row",
       y = "Ct") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # Tytuł wykresu centralnie

print(wykres_qtow)

wykres_qtow_stars <- wykres_qtow + 
  annotate("text", x = c(3, 4, 5), y = c(17.1, 16.87, 16.35), label = "*", size = 12, fontface = "bold")
print(wykres_qtow_stars)

# Wykres Ct z QuantStudio-------------------------------------------------------
# Tworzenie wykresu z podanych przez QuantStudio wartości ct

# Wczytanie danych
dane_qs <- read_csv("GAPDH_ct_QS.csv")

# Dodanie kolumny "Row" zawierającej pierwsze litery z kolumny "Well"
dane_qst <- dane_qs
dane_qst$Row <- substr(dane_qst$Well, 1, 1)

# Zmiana nazwy kolumny
dane_qst <- rename(dane_qst, Ct = CT)

# Test ANOVA
wynik_anova_qst <- aov(Ct ~ factor(Row), data = dane_qst)
print(wynik_anova_qst)
summary(wynik_anova_qst)

# Wykres punktowy z ramką-wąsami
wykres_qst <- ggplot(dane_qst, aes(x = factor(Row), y = Ct)) +
  geom_boxplot() +
  geom_point() +
  labs(title = "GAPDH Ct on QuantStudio",
       x = "Row",
       y = "CT") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # Tytuł wykresu centralnie

# Wyświetlenie wykresu
print(wykres_qst)

# Łączenie wykresów qtow i qst przy pomocy biblioteki patchwork
# Metoda kiepska bo trzeba by ręcznie ustawiać skalę y, poniważ on tutaj rozciąga
library(patchwork)
#patchwork_plot <- wykres_qtow_stars + wykres_qst
#print(patchwork_plot)

# Łączenie wykresów qtow i qst za pomocą facet_grid()---------------------------
# Dodanie kolumny System odróżniającej qTower od QuantStudio
dane_qst$System <- "QuantStudio"
dane_qt$System <- "qTower3"


combined_plot_color <- ggplot() +
  geom_boxplot(data = dane_qst, aes(x = factor(Row), y = Ct)) +
  geom_point(data = dane_qst, aes(x = factor(Row), y = Ct, color = factor(Row)), position = position_jitter(width = 0.25), alpha = 0.3) +
  geom_boxplot(data = dane_qt, aes(x = factor(Row), y = Ct)) +
  geom_point(data = dane_qt, aes(x = factor(Row), y = Ct, color = factor(Row)), position = position_jitter(width = 0.25), alpha = 0.3) +
  labs(title = "Comparison of GAPDH Ct",
       x = "Row",
       y = "Ct") +
  theme(plot.title = element_text(hjust = 0.5)) + # Tytuł wykresu centralnie
  facet_grid(. ~ System) +
  scale_color_manual(values = c("A" = "blue", "B" = "red", "C" = "magenta", "F" = "green", "G" = "orange", "H" = "purple")) +
  labs(color = "Row")


print(combined_plot_color)


# Utworzenie ramki danych z gwiazdkami przypisanymi do System (qTower3)
gwiazdki <- data.frame(System = c("qTower3","qTower3", "qTower3"), label = c("*", "*", "*"))

# Dołożenie gwiazdek do wykresu tylko do części qTower3
combined_plot_color +
  geom_text(x = c(4, 5, 6), y = c(17.2, 16.97, 16.45), aes(label = label), data = gwiazdki, size = 8)
