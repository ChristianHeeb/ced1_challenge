<br>
<img style="background: #FDE70F;" src="https://www.fhnw.ch/de/++theme++web16theme/assets/media/img/fachhochschule-nordwestschweiz-fhnw-logo.svg" alt="FHNW Logo" height="50rem" id="logo">
<br><br>Challenge: Cross Selling in Banking (CED1)
================
Gisler Luca, Heeb Christian, Studer Aaron, Bécheiraz Léonie
14.06.23

<style type="text/css" media="screen">
h1, h2, h3 {
    color: #4D4D4D;
   }

.list-group-item.active {
    background-color: #FDE70F;
    border-color: #FDE70F;
    color: #4D4D4D;
    font-weight: bold;
}

#logo {
    padding-left: 1rem;
}

</style>

## Aufgabenstellung

### Allgemein

Eine tschechische Bank möchte ihre Dienstleistungen für Privatkunden
verbessern und “interessante Kundengruppen” identifizieren. Die
Geschäftsleitung hat keine präzise Vorstellung, möchte aber zusätzliches
Business generieren ohne unnötige Risiken einzugehen und Verluste
einzufahren.

Der analytische Auftrag umfasst die folgenden Aufgaben:

- Qualität und Repräsentativität der Daten zu überprüfen
- Die Verteilung der einzelnen Datenattribute zu erheben
- Deren Veränderung über die Zeit zu analysieren
- Korrelationen zwischen verschiedenen Datenattributen zu quantifizieren
  und zu visualisieren sowie Hypothesen hinsichtlich optimaler
  Produktverkauf / -nutzung zu erstellen

### Datengrundlage

Wir erhalten die Daten von einer Tschechischen Bank. Die Datengrundlage
ist auf [dieser
Webseite](https://sorry.vse.cz/~berka/challenge/PAST/index.html)
beschrieben.

### Datenbeschreibung

Die Datengrundlage enthält 8 verschiedene Tabellen (Data Frames) im .csv
Format mit Total 47 Attributen. Diese Tabellen mit den jeweiligen
Attributen werden hier genauer beschrieben.

Relation disposition (df_raw_disposition): disp_id: record identifier  
client_id: identification of a client  
account_id: identification of an account  
type: type of disposition (owner/user) only owner can issue permanent
orders and ask for a loan

#### ERD Daten IST-Zustand

![ERD Daten IST-Zustand](../Ressources/IST-Zustand.png)

## Setup

Notwendige Pakete laden

### Setup Account Data Frame

In diesem Schritt wird das Data Frame “Account” vorbereitet und der
‘Transform’-Schritt wird durchgeführt.

``` r
df_raw_account <- read.csv("/Users/christianheeb/CrossSellingInBanking/CSIB/CSIB/CrossSellingInBanking/xselling_banking_data/account.csv", header = TRUE, sep = ";")

str(df_raw_account)
```

    ## 'data.frame':    4500 obs. of  4 variables:
    ##  $ account_id : int  576 3818 704 2378 2632 1972 1539 793 2484 1695 ...
    ##  $ district_id: int  55 74 55 16 24 77 1 47 74 76 ...
    ##  $ frequency  : chr  "POPLATEK MESICNE" "POPLATEK MESICNE" "POPLATEK MESICNE" "POPLATEK MESICNE" ...
    ##  $ date       : int  930101 930101 930101 930101 930102 930102 930103 930103 930103 930103 ...

Es wird wie folgt die Spalte ‘date’ von dem Type Integer zu dem Typ Date
umgewandelt. Dabei brauchen wir die Funktionalitäten von
[Lubridate](https://lubridate.tidyverse.org/). Die Spalte ‘frequency’
ist eine kategoriale Variable mit Tschechischen Werten, daher
transformieren wir auch diese Werte auf Englisch.

``` r
df_account <- df_raw_account %>%
  mutate(date = ymd(date)) %>%
  mutate(frequency = case_when(frequency == "POPLATEK MESICNE" ~ "Monthly",
                              frequency == "POPLATEK TYDNE" ~ "Weekly",
                              frequency == "POPLATEK PO OBRATU" ~ "After_Transaction")
  ) %>%
  arrange(account_id)

rm(df_raw_account)
```

### Setup Client Data Frame

In diesem Schritt wird das Data Frame Account vorbereitet und der
‘Transform’-Schritt wird durchgeführt.

``` r
df_raw_client <- read.csv("/Users/christianheeb/CrossSellingInBanking/CSIB/CSIB/CrossSellingInBanking/xselling_banking_data/client.csv", header = TRUE, sep = ";")

str(df_raw_client)
```

    ## 'data.frame':    5369 obs. of  3 variables:
    ##  $ client_id   : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ birth_number: int  706213 450204 406009 561201 605703 190922 290125 385221 351016 430501 ...
    ##  $ district_id : int  18 1 1 5 5 12 15 51 60 57 ...

Es wird wie folgt die Spalte ‘birth_number’ von dem Type Integer zu dem
Typ Date umgewandelt, zusätzlich nennen wir die Spalte neu
‘dateofbirth’. Dabei brauchen wir die Funktionalitäten von
[Lubridate](https://lubridate.tidyverse.org/). Es wird eine neue Spalte
‘sex’ hinzugefügt, mithilfe der Dokumentation der Daten kennen wir die
Kondition, welches Geschlecht der Kunde hat.

``` r
df_client <- df_raw_client %>%
  mutate(dateofbirth = case_when(
    strtoi(substr(as.character(birth_number), 3, 3)) > 1 ~ (ymd(birth_number - 5000)),
    TRUE ~ (ymd(birth_number)),
  )) %>%
  mutate(sex = case_when(
    strtoi(substr(as.character(birth_number), 3, 3)) > 1 ~ "Female",
    TRUE  ~ "Male"
  ))

df_client <- df_client %>%
 mutate(dateofbirth = case_when(
   year(ymd(dateofbirth)) > 2000 ~ ymd(dateofbirth) - years(100),
   TRUE ~ ymd(dateofbirth)
 )) %>%
 select(client_id, district_id, dateofbirth, sex) %>%
 arrange(client_id) 

rm(df_raw_client)
```

### Setup Disposition Data Frame

In diesem Schritt wird das Data Frame Disposition vorbereitet und der
‘Transform’-Schritt wird durchgeführt.

``` r
df_raw_disposition <- read.csv("/Users/christianheeb/CrossSellingInBanking/CSIB/CSIB/CrossSellingInBanking/xselling_banking_data/disp.csv", header = TRUE, sep = ";")

str(df_raw_disposition)
```

    ## 'data.frame':    5369 obs. of  4 variables:
    ##  $ disp_id   : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ client_id : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ account_id: int  1 2 2 3 3 4 5 6 7 8 ...
    ##  $ type      : chr  "OWNER" "OWNER" "DISPONENT" "OWNER" ...

In dem Date Frame disposition müssen keine weitere Schritte erledigt
werden für das Transformieren der Daten.

``` r
df_disposition <- df_raw_disposition %>%
 select(disp_id, client_id, account_id, type) %>%
 arrange(disp_id) 

rm(df_raw_disposition)
```

### Setup Order Data Frame

In diesem Schritt wird das Data Frame Order vorbereitet und der
‘Transform’-Schritt wird durchgeführt.

``` r
df_raw_perm_order <- read.csv("/Users/christianheeb/CrossSellingInBanking/CSIB/CSIB/CrossSellingInBanking/xselling_banking_data/order.csv", header = TRUE, sep = ";")

str(df_raw_perm_order)
```

    ## 'data.frame':    6471 obs. of  6 variables:
    ##  $ order_id  : int  29401 29402 29403 29404 29405 29406 29407 29408 29409 29410 ...
    ##  $ account_id: int  1 2 2 3 3 3 4 4 5 6 ...
    ##  $ bank_to   : chr  "YZ" "ST" "QR" "WX" ...
    ##  $ account_to: int  87144583 89597016 13943797 83084338 24485939 59972357 26693541 5848086 37390208 44486999 ...
    ##  $ amount    : num  2452 3373 7266 1135 327 ...
    ##  $ k_symbol  : chr  "SIPO" "UVER" "SIPO" "SIPO" ...

In dem Data Frame perm_order müssen wir die kategoriale Variable
‘k_symbol’ noch übersetzen, da der Name ‘k_symbol’ nicht ausschlaggeben
ist, benennen wir die Spalte zu ‘payment_type’ um.

``` r
df_perm_order <- df_raw_perm_order %>%
 mutate(payment_type = case_when(k_symbol == "POJISTNE" ~ "INSURRANCE",
                              k_symbol == "SIPO" ~ "HOUSEHOLD",
                              k_symbol == "LEASING" ~ "LEASING",
                              k_symbol == "UVER" ~ "LOAN",
                              TRUE ~ "UNKNOWN")
 ) %>%
 select(order_id, account_id, bank_to, account_to, amount, payment_type) %>%
 arrange(order_id) 

rm(df_raw_perm_order)
```

### Setup Transaction Data Frame

In diesem Schritt wird das Data Frame Transaction vorbereitet und der
‘Transform’-Schritt wird durchgeführt.

``` r
df_raw_transaction <- read.csv("/Users/christianheeb/CrossSellingInBanking/CSIB/CSIB/CrossSellingInBanking/xselling_banking_data/trans.csv", header = TRUE, sep = ";")

str(df_raw_transaction)
```

    ## 'data.frame':    1056320 obs. of  10 variables:
    ##  $ trans_id  : int  695247 171812 207264 1117247 579373 771035 452728 725751 497211 232960 ...
    ##  $ account_id: int  2378 576 704 3818 1972 2632 1539 2484 1695 793 ...
    ##  $ date      : int  930101 930101 930101 930101 930102 930102 930103 930103 930103 930103 ...
    ##  $ type      : chr  "PRIJEM" "PRIJEM" "PRIJEM" "PRIJEM" ...
    ##  $ operation : chr  "VKLAD" "VKLAD" "VKLAD" "VKLAD" ...
    ##  $ amount    : num  700 900 1000 600 400 1100 600 1100 200 800 ...
    ##  $ balance   : num  700 900 1000 600 400 1100 600 1100 200 800 ...
    ##  $ k_symbol  : chr  "" "" "" "" ...
    ##  $ bank      : chr  "" "" "" "" ...
    ##  $ account   : int  NA NA NA NA NA NA NA NA NA NA ...

Es wird wie folgt die Spalte ‘date’ von dem Type Integer zu dem Typ Date
umgewandelt. Dabei brauchen wir die Funktionalitäten von
[Lubridate](https://lubridate.tidyverse.org/). Die Spalte ‘type’ muss
von Tschechisch noch auf Englisch übersetzt werden. Dasselbe zählt auch
für die Spalte ‘operation’ und ‘k_symbol’. Jedoch benennen wir die
Splate ‘k_symbol’ noch um in die neue Spalte ‘characterization’.

``` r
df_transaction <- df_raw_transaction %>%
 mutate(date = ymd(date)) %>%
 mutate(type = case_when(type == "PRIJEM" ~ "CREDIT",
                         type == "VYDAJ" ~ "WITHDRAWAL") 
 ) %>%
 mutate(operation = case_when(operation == "VYBER KARTOU" ~ "CREDIT CARD WITHDRAWAL",
                              operation == "VKLAD" ~ "CASH CREDIT",
                              operation == "PREVOD Z UCTU" ~ "COLLECTION OTHER BANK",
                              operation == "VYBER" ~ "CASH WIDTHDRAWAL",
                              operation == "PREVOD NA UCET" ~ "REMITTANCE OTHER BANK")
 ) %>%
 mutate(characterization = case_when(k_symbol == "POJISTNE" ~ "INSURRANCE PAYMENT",
                              k_symbol == "SLUZBY" ~ "STATEMENT PAYMENT",
                              k_symbol == "UROK" ~ "CREDIT INTEREST",
                              k_symbol == "SANKC. UROK" ~ "SANCTION INTEREST",
                              k_symbol == "SIPO" ~ "HOUSEHOLD",
                              k_symbol == "DUCHOD" ~ "OLD AGE PENSION",
                              k_symbol == "UVER" ~ "LOAN PAYMENT")
 ) %>%
 select(trans_id, account_id, date, type, operation, amount, balance, characterization, bank, account) %>%
 arrange(trans_id) 

rm(df_raw_transaction)
```

### Setup Loan Data Frame

In diesem Schritt wird das Data Frame Loan vorbereitet und der
‘Transform’-Schritt wird durchgeführt.

``` r
df_raw_loan <- read.csv("/Users/christianheeb/CrossSellingInBanking/CSIB/CSIB/CrossSellingInBanking/xselling_banking_data/loan.csv", header = TRUE, sep = ";")

str(df_raw_loan)
```

    ## 'data.frame':    682 obs. of  7 variables:
    ##  $ loan_id   : int  5314 5316 6863 5325 7240 6687 7284 6111 7235 5997 ...
    ##  $ account_id: int  1787 1801 9188 1843 11013 8261 11265 5428 10973 4894 ...
    ##  $ date      : int  930705 930711 930728 930803 930906 930913 930915 930924 931013 931104 ...
    ##  $ amount    : int  96396 165960 127080 105804 274740 87840 52788 174744 154416 117024 ...
    ##  $ duration  : int  12 36 60 36 60 24 12 24 48 24 ...
    ##  $ payments  : num  8033 4610 2118 2939 4579 ...
    ##  $ status    : chr  "B" "A" "A" "A" ...

Es wird wie folgt die Spalte ‘date’ von dem Type Integer zu dem Typ Date
umgewandelt. Dabei brauchen wir die Funktionalitäten von
[Lubridate](https://lubridate.tidyverse.org/). Die Spalte ‘status’
besitzt Enum-Werte. Diese Werte transformieren wir von A, B, C und D zu
den entsprechenden Bedeutungen auf English.

``` r
df_loan <- df_raw_loan %>%
 mutate(date = ymd(date)) %>%
 mutate(status = case_when(status == "A" ~ "CONTRACT FINISHED PAYED",
                              status == "B" ~ "CONTRACT FINISHED UNPAID",
                              status == "C" ~ "CONTRACT OPEN OK",
                              status == "D" ~ "CONTRACT OPEN INDEBT",
                              TRUE ~ "")
 ) %>%
 select(loan_id, account_id, date, amount, duration, payments, status) %>%
 arrange(loan_id) 

rm(df_raw_loan)
```

### Setup Credit Card Data Frame

In diesem Schritt wird das Data Frame Credit Card vorbereitet und der
‘Transform’-Schritt wird durchgeführt.

``` r
df_raw_credit_card <- read.csv("/Users/christianheeb/CrossSellingInBanking/CSIB/CSIB/CrossSellingInBanking/xselling_banking_data/card.csv", header = TRUE, sep = ";")

str(df_raw_credit_card)
```

    ## 'data.frame':    892 obs. of  4 variables:
    ##  $ card_id: int  1005 104 747 70 577 377 721 437 188 13 ...
    ##  $ disp_id: int  9285 588 4915 439 3687 2429 4680 2762 1146 87 ...
    ##  $ type   : chr  "classic" "classic" "classic" "classic" ...
    ##  $ issued : chr  "931107 00:00:00" "940119 00:00:00" "940205 00:00:00" "940208 00:00:00" ...

Es wird wie folgt die Spalte ‘issued’ von dem Type Character zu dem Typ
Date umgewandelt. Dabei brauchen wir die Funktionalitäten von
[Lubridate](https://lubridate.tidyverse.org/). Die Werte der Spalte
‘type’ werden in Upper Case umgeschrieben für eine klarere Übersicht von
kategorialen Variablen über alle Data Frames.

``` r
df_credit_card <- df_raw_credit_card %>%
 mutate(issued  = ymd(as.integer(substr(issued, 0, 6)))) %>%
 mutate(type = case_when(type == "junior" ~ "JUNIOR",
                              type == "classic" ~ "CLASSIC",
                              type == "gold" ~ "GOLD",
                              TRUE ~ "")
 ) %>%
 select(card_id, disp_id, type, issued) %>%
 arrange(card_id) 

rm(df_raw_credit_card)
```

### Setup District Data Frame

In diesem Schritt wird das Data Frame District vorbereitet und der
‘Transform’-Schritt wird durchgeführt.

``` r
df_raw_district <- read.csv("/Users/christianheeb/CrossSellingInBanking/CSIB/CSIB/CrossSellingInBanking/xselling_banking_data/district.csv", header = TRUE, sep = ";")

str(df_raw_district)
```

    ## 'data.frame':    77 obs. of  16 variables:
    ##  $ A1 : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ A2 : chr  "Hl.m. Praha" "Benesov" "Beroun" "Kladno" ...
    ##  $ A3 : chr  "Prague" "central Bohemia" "central Bohemia" "central Bohemia" ...
    ##  $ A4 : int  1204953 88884 75232 149893 95616 77963 94725 112065 81344 92084 ...
    ##  $ A5 : int  0 80 55 63 65 60 38 95 61 55 ...
    ##  $ A6 : int  0 26 26 29 30 23 28 19 23 29 ...
    ##  $ A7 : int  0 6 4 6 4 4 1 7 4 4 ...
    ##  $ A8 : int  1 2 1 2 1 2 3 1 2 3 ...
    ##  $ A9 : int  1 5 5 6 6 4 6 8 6 5 ...
    ##  $ A10: num  100 46.7 41.7 67.4 51.4 51.5 63.4 69.4 55.3 46.7 ...
    ##  $ A11: int  12541 8507 8980 9753 9307 8546 9920 11277 8899 10124 ...
    ##  $ A12: chr  "0.29" "1.67" "1.95" "4.64" ...
    ##  $ A13: num  0.43 1.85 2.21 5.05 4.43 4.02 2.87 1.44 3.97 0.54 ...
    ##  $ A14: int  167 132 111 109 118 126 130 127 149 141 ...
    ##  $ A15: chr  "85677" "2159" "2824" "5244" ...
    ##  $ A16: int  99107 2674 2813 5892 3040 3120 4846 4987 2487 4316 ...

In dem Date Frame ‘district’ müssen wir alle Spalten neu benennnen, da
die einzelnen Spalten aus der Datenquelle keine Aussage über die Daten
drinhat. Daher geben wir jeder Spalte einen passenden Namen.

``` r
df_district <- df_raw_district %>%
 mutate(district_id  = A1) %>%
 mutate(name = A2) %>%
 mutate(region = A3) %>%
 mutate(inhabitants = A4) %>%
 mutate(municipalities_under_499_inhabitants = A5) %>%
 mutate(municipalities_500_to_1999_inhabitants = A6) %>%
 mutate(municipalities_2000_to_9999_inhabitants = A7) %>%
 mutate(municipalities_over_10000_inhabitants = A7) %>%
 mutate(cities = A9) %>%
 mutate(ratio_urban_inhabitants = A10) %>%
 mutate(average_salary = A11) %>%
 mutate(unemployment_rate_95 = A12) %>%
 mutate(unemployment_rate_96 = A13) %>%
 mutate(enterpreneurs_per_1000_inhabitants = A14) %>%
 mutate(commited_crimes_95 = A15) %>%
 mutate(commited_crimes_96 = A16) %>%
 select(district_id, name, region, inhabitants, municipalities_under_499_inhabitants, municipalities_500_to_1999_inhabitants, municipalities_2000_to_9999_inhabitants, municipalities_over_10000_inhabitants, cities, ratio_urban_inhabitants, average_salary, unemployment_rate_95, unemployment_rate_96, enterpreneurs_per_1000_inhabitants, commited_crimes_95, commited_crimes_96) %>%
 arrange(district_id) 

rm(df_raw_district)
```

### ERD Ist-Zustand

In dem nachfolgend Enitity Relation Diagramm sieht man die Elemente der
transformierten Daten.

![ERD Daten IST-Zustand](../Ressources/SOLL-Zustand.png)

## Tabellen zusammenführen[^1]

### Tabellen vorbereiten[^2]

Die einzelnen Tabellen werden vor dem Zusammenführen so vorbereitet,
dass pro Account nur eine Zeile vorhanden ist.

### Data Frame “Permanent Order”[^3]

``` r
summary(df_perm_order)
```

    ##     order_id       account_id      bank_to            account_to      
    ##  Min.   :29401   Min.   :    1   Length:6471        Min.   :     399  
    ##  1st Qu.:31188   1st Qu.: 1223   Class :character   1st Qu.:24159184  
    ##  Median :32988   Median : 2433   Mode  :character   Median :49756062  
    ##  Mean   :33778   Mean   : 2962                      Mean   :49399037  
    ##  3rd Qu.:34786   3rd Qu.: 3646                      3rd Qu.:74000448  
    ##  Max.   :46338   Max.   :11362                      Max.   :99994199  
    ##      amount      payment_type      
    ##  Min.   :    1   Length:6471       
    ##  1st Qu.: 1242   Class :character  
    ##  Median : 2596   Mode  :character  
    ##  Mean   : 3281                     
    ##  3rd Qu.: 4614                     
    ##  Max.   :14882

``` r
str(df_perm_order)
```

    ## 'data.frame':    6471 obs. of  6 variables:
    ##  $ order_id    : int  29401 29402 29403 29404 29405 29406 29407 29408 29409 29410 ...
    ##  $ account_id  : int  1 2 2 3 3 3 4 4 5 6 ...
    ##  $ bank_to     : chr  "YZ" "ST" "QR" "WX" ...
    ##  $ account_to  : int  87144583 89597016 13943797 83084338 24485939 59972357 26693541 5848086 37390208 44486999 ...
    ##  $ amount      : num  2452 3373 7266 1135 327 ...
    ##  $ payment_type: chr  "HOUSEHOLD" "LOAN" "HOUSEHOLD" "HOUSEHOLD" ...

Neuorganisation der Werte aus “payment_type” in Spalten mit der Summe
der Beträge “amount” und ergänzenden Spalten mit der Information,
wieviele Aufträge des gleichen Typs vorhanden sind. Die Spalten
“order_id”, “bank_to” und “account_to” werden weggelassen, da diese für
die zukünftigen Analysen nicht benötigt werden.

``` r
# summe der Beträge
df_perm_order_mod <- df_perm_order %>% 
  group_by(
    account_id,
    payment_type
  ) %>% 
  mutate(amount_sum = sum(amount)) %>% 
  group_by(
    account_id,
    payment_type,
    amount_sum
  ) %>% 
  count() %>% 
  rename(payment_type_num = n) %>% 
  pivot_wider(names_from = payment_type, values_from = c(amount_sum, payment_type_num)) %>% 
  na.replace(., 0) %>% 
  ungroup()

# str(df_perm_order_mod)

if (!grepl("order_", names(df_perm_order_mod)[2])) {
names(df_perm_order_mod) <- paste0("order_", names(df_perm_order_mod))
}
```

Überprüfung der bisher nicht beachteten Spalten “bank_to” und
“account_to”

``` r
df_perm_order %>% 
  group_by(
    bank_to, 
    account_to
  ) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  head()
```

    ## # A tibble: 6 × 3
    ##   bank_to account_to     n
    ##   <chr>        <int> <int>
    ## 1 AB        79838293     2
    ## 2 AB        96968262     2
    ## 3 AB        99149345     2
    ## 4 EF         1838881     2
    ## 5 EF         2692229     2
    ## 6 EF        29934013     2

``` r
df_perm_order %>% 
  filter(
    account_to == 79838293
  )
```

    ##   order_id account_id bank_to account_to amount payment_type
    ## 1    29433         25      AB   79838293   1110      UNKNOWN
    ## 2    40359       7424      AB   79838293   1110      UNKNOWN

Es gibt Überweisungen, welche von verschiedenen Konten (maximal 2) auf
dasselbe Ziel-Konto einzahlen. Für die Analysen, welche geplant sind,
ist dieser Umstand nicht relevant. Deshalb werden diese Informationen
vorerst weggelassen. Falls nötig, können sie zu einem späteren Zeitpunkt
immer noch dazu genommen werden.

### Data Frame “Account”[^4]

Das Data Frame Account steht im Zentrum und beinhaltet die
Schlüsselfelder zu fast allen weiteren Tabellen. Deshalb beginnen wir
mit dem Account.

``` r
summary(df_account)
```

    ##    account_id     district_id     frequency              date           
    ##  Min.   :    1   Min.   : 1.00   Length:4500        Min.   :1993-01-01  
    ##  1st Qu.: 1183   1st Qu.:13.00   Class :character   1st Qu.:1993-12-27  
    ##  Median : 2368   Median :38.00   Mode  :character   Median :1996-01-02  
    ##  Mean   : 2786   Mean   :37.31                      Mean   :1995-08-08  
    ##  3rd Qu.: 3552   3rd Qu.:60.00                      3rd Qu.:1996-11-01  
    ##  Max.   :11382   Max.   :77.00                      Max.   :1997-12-29

Die Werte der Spalte “frequency” werden in Faktoren umgewandelt. Zudem
werden zusätzliche Spalten für die Kontoeröffnungsdaten erstellt (-\>
Eröffnungsjahr als “opening_year”, Eröffnungsmonat als “opening_month”)
und die Spalte “date” in “opening_date” geändert. Anschliessend wird
wieder allen Spalten das Präfix “account.” erteilt.

``` r
df_account_mod <- df_account %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(opening_year = year(date)) %>%
  mutate(opening_month = month(date)) %>%
  rename("opening_date" = "date")



if (!grepl("account_", names(df_account_mod)[2])) {
names(df_account_mod) <- paste0("account_", names(df_account_mod))
}
```

#### Erstes Zusammenführen[^5]

Erstes zusammenfügen der modifizierten Data Frames “df_account_mod” und
“df_perm_order_mod” zum neuen Data Frame “df_mod”.

``` r
df_mod <- left_join(df_account_mod, df_perm_order_mod, by = c( "account_account_id" = "order_account_id"))

summary(df_mod)
```

    ##  account_account_id account_district_id         account_frequency
    ##  Min.   :    1      Min.   : 1.00       After_Transaction:  93   
    ##  1st Qu.: 1183      1st Qu.:13.00       Monthly          :4167   
    ##  Median : 2368      Median :38.00       Weekly           : 240   
    ##  Mean   : 2786      Mean   :37.31                                
    ##  3rd Qu.: 3552      3rd Qu.:60.00                                
    ##  Max.   :11382      Max.   :77.00                                
    ##                                                                  
    ##  account_opening_date account_opening_year account_opening_month
    ##  Min.   :1993-01-01   Min.   :1993         Min.   : 1.000       
    ##  1st Qu.:1993-12-27   1st Qu.:1993         1st Qu.: 4.000       
    ##  Median :1996-01-02   Median :1996         Median : 7.000       
    ##  Mean   :1995-08-08   Mean   :1995         Mean   : 6.574       
    ##  3rd Qu.:1996-11-01   3rd Qu.:1996         3rd Qu.: 9.000       
    ##  Max.   :1997-12-29   Max.   :1997         Max.   :12.000       
    ##                                                                 
    ##  order_amount_sum_HOUSEHOLD order_amount_sum_LOAN order_amount_sum_INSURRANCE
    ##  Min.   :    0              Min.   :   0.0        Min.   :    0.0            
    ##  1st Qu.: 1809              1st Qu.:   0.0        1st Qu.:    0.0            
    ##  Median : 3054              Median :   0.0        Median :    0.0            
    ##  Mean   : 3716              Mean   : 807.7        Mean   :  182.8            
    ##  3rd Qu.: 5282              3rd Qu.:   0.0        3rd Qu.:    0.0            
    ##  Max.   :14882              Max.   :9910.0        Max.   :12504.0            
    ##  NA's   :742                NA's   :742           NA's   :742                
    ##  order_amount_sum_UNKNOWN order_amount_sum_LEASING
    ##  Min.   :    0.0          Min.   :   0.0          
    ##  1st Qu.:    0.0          1st Qu.:   0.0          
    ##  Median :    0.0          Median :   0.0          
    ##  Mean   :  740.3          Mean   : 202.1          
    ##  3rd Qu.:  597.8          3rd Qu.:   0.0          
    ##  Max.   :12925.0          Max.   :4975.2          
    ##  NA's   :742              NA's   :742             
    ##  order_payment_type_num_HOUSEHOLD order_payment_type_num_LOAN
    ##  Min.   :0.0000                   Min.   :0.0000             
    ##  1st Qu.:1.0000                   1st Qu.:0.0000             
    ##  Median :1.0000                   Median :0.0000             
    ##  Mean   :0.9319                   Mean   :0.1908             
    ##  3rd Qu.:1.0000                   3rd Qu.:0.0000             
    ##  Max.   :2.0000                   Max.   :1.0000             
    ##  NA's   :742                      NA's   :742                
    ##  order_payment_type_num_INSURRANCE order_payment_type_num_UNKNOWN
    ##  Min.   :0.0000                    Min.   :0.000                 
    ##  1st Qu.:0.0000                    1st Qu.:0.000                 
    ##  Median :0.0000                    Median :0.000                 
    ##  Mean   :0.1416                    Mean   :0.367                 
    ##  3rd Qu.:0.0000                    3rd Qu.:1.000                 
    ##  Max.   :1.0000                    Max.   :2.000                 
    ##  NA's   :742                       NA's   :742                   
    ##  order_payment_type_num_LEASING
    ##  Min.   :0.0000                
    ##  1st Qu.:0.0000                
    ##  Median :0.0000                
    ##  Mean   :0.0907                
    ##  3rd Qu.:0.0000                
    ##  Max.   :1.0000                
    ##  NA's   :742

Die NA’s in den Spalten von “order.” bedeuten, dass bei diesen Konten
keine Daueraufträge hinderlegt sind. Die Zahl 0 (Null) bedeutet, dass
zwar Daueraufträge hinterlegt sind, aber nicht zu diesem Themenbereich.

Nun können die nicht mehr benötigten Tabellen aus dem Global Environment
entfernt werden.

``` r
rm(df_account, df_account_mod, df_perm_order, df_perm_order_mod)
```

### Data Frame “Loan”[^6]

Im weiteren Schritt wird das Data Frame “Loan” vorbereitet. Übersicht
über das Data Frame

``` r
glimpse(df_loan)
```

    ## Rows: 682
    ## Columns: 7
    ## $ loan_id    <int> 4959, 4961, 4962, 4967, 4968, 4973, 4986, 4988, 4989, 4990,…
    ## $ account_id <int> 2, 19, 25, 37, 38, 67, 97, 103, 105, 110, 132, 173, 176, 22…
    ## $ date       <date> 1994-01-05, 1996-04-29, 1997-12-08, 1998-10-14, 1998-04-19…
    ## $ amount     <int> 80952, 30276, 30276, 318480, 110736, 165960, 102876, 265320…
    ## $ duration   <int> 24, 12, 12, 60, 48, 24, 12, 36, 48, 36, 12, 12, 12, 12, 36,…
    ## $ payments   <dbl> 3373, 2523, 2523, 5308, 2307, 6915, 8573, 7370, 7348, 4516,…
    ## $ status     <chr> "CONTRACT FINISHED PAYED", "CONTRACT FINISHED UNPAID", "CON…

Folgendes soll angepasst werden: - die Spalte “status” wird in Faktor
umgewandelt.

- Umbenennen der Spalten:
  - “duration” zu “duration_in_month”
  - “date” zu “start_date”
  - “amount” zu “total_amount”
  - “payments” zu “redemption_amount”
- Zusätzliche Spalte:
  - “duration_in_years” generiert aus “duration_in_month” mit den
    Ganzzahlen für

    - 12 = 1,

    - 24 = 2,

    - 36 = 3,

    - 48 = 4,

    - 60 = 5

  - “end_date” gerechnet aus “start_date” plus “duration_in_years”

  - “in_dept” generiert aus der Spalte “status” für

    “CONTRACT FINISHED PAYED” und “CONTRACT OPEN OK” = “NO”

    “CONTRACT FINISHED UNPAID” und “CONTRACT OPEN INDEBT” = “YES”

``` r
df_loan_mod <- df_loan %>%
  rename(
    "duration_in_month" = "duration",
    "start_date" = "date",
    "total_amount" = "amount",
    "redemption_amount" = "payments"
    ) %>%
  mutate(
    duration_in_years = case_when(duration_in_month == 12 ~ 1,
                                  duration_in_month == 24 ~ 2,
                                  duration_in_month == 36 ~ 3,
                                  duration_in_month == 48 ~ 4,
                                  duration_in_month == 60 ~ 5
  )) %>%
  mutate(end_date = start_date + years(duration_in_years)) %>%
  mutate(in_dept = if_else(c(status == "CONTRACT FINISHED PAYED" |
                             status == "CONTRACT OPEN OK"), FALSE, TRUE)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  relocate(end_date, .after = start_date) %>%
  relocate(starts_with("duration"), .after = end_date)

  levels(df_loan_mod$status)
```

    ## [1] "CONTRACT FINISHED PAYED"  "CONTRACT FINISHED UNPAID"
    ## [3] "CONTRACT OPEN INDEBT"     "CONTRACT OPEN OK"

Überprüfung, ob ein Account mehrere Darlehen hat.

``` r
df_loan_mod %>%
  group_by(account_id) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>% 
  head(3)
```

    ## # A tibble: 3 × 2
    ##   account_id     n
    ##        <int> <int>
    ## 1          2     1
    ## 2         19     1
    ## 3         25     1

Da es pro Account nur jeweils 1 Darlehensvertrag gibt, braucht es für
diese Tabelle keine weiteren Anpassungen. Es wird nur noch das Präfix
“loan.” hizugefügt, bevor ein Anfügen an die Gesamttabelle erfolgt.

``` r
if (!grepl("loan_", names(df_loan_mod)[2])) {
names(df_loan_mod) <- paste0("loan_", names(df_loan_mod))
}
```

#### Zweites Zusammenführen

Loan wird zum Data Frame “df_mod” hinzugefügt.

#### Bedeutung der NA’s[^7]

Die NA’s in den Spalten von “loan\_” bedeuten, dass bei diesen Konten
keine Darlehen hinderlegt sind, analog den Informationen aus den Spalten
“order\_”

Entfernen der nicht mehr benötigten Tabellen aus dem Global Environment.

``` r
rm(df_loan, df_loan_mod)
```

### Data Frame “disposition”

Übersicht über das Data Frame

``` r
glimpse(df_disposition)
```

    ## Rows: 5,369
    ## Columns: 4
    ## $ disp_id    <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, …
    ## $ client_id  <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, …
    ## $ account_id <int> 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 8, 9, 10, 11, 12, 12, 13, 13,…
    ## $ type       <chr> "OWNER", "OWNER", "DISPONENT", "OWNER", "DISPONENT", "OWNER…

#### Anpassungen

- Werte in der Spalte type umbenennen (Disponent zu User)

- neue Spalte mit Anzahl Benutzer pro Konto (account_user_num)

- Werte der Spalte type in neue Spalten aufteilen mit Bezeichnung und
  Werten aus client_id und disp_id

``` r
df_disposition_mod <- df_disposition %>% 
   mutate(type = ifelse(c(type == "OWNER"), "OWNER", "USER")) %>% 
   pivot_wider(names_from = type, values_from = c(client_id, disp_id))

#Spalte mit Anzahl User pro Account
df_disposition_num_user <- df_disposition %>% 
  group_by(
    account_id
  ) %>% 
  count() %>% 
  rename(account_num_of_user = n) %>% 
  ungroup()

#Anfügen der neuen Spalte "account.num_of_user" an das df_disposition_mod
df_disposition_mod <- left_join(df_disposition_mod, df_disposition_num_user, by = "account_id")

summary(df_disposition_mod)
```

    ##    account_id    client_id_OWNER client_id_USER  disp_id_OWNER  
    ##  Min.   :    1   Min.   :    1   Min.   :    3   Min.   :    1  
    ##  1st Qu.: 1183   1st Qu.: 1425   1st Qu.: 1393   1st Qu.: 1425  
    ##  Median : 2368   Median : 2861   Median : 2687   Median : 2861  
    ##  Mean   : 2786   Mean   : 3381   Mean   : 3245   Mean   : 3359  
    ##  3rd Qu.: 3552   3rd Qu.: 4287   3rd Qu.: 4070   3rd Qu.: 4287  
    ##  Max.   :11382   Max.   :13998   Max.   :13956   Max.   :13690  
    ##                                  NA's   :3631                   
    ##   disp_id_USER   account_num_of_user
    ##  Min.   :    3   Min.   :1.000      
    ##  1st Qu.: 1393   1st Qu.:1.000      
    ##  Median : 2687   Median :1.000      
    ##  Mean   : 3221   Mean   :1.193      
    ##  3rd Qu.: 4070   3rd Qu.:1.000      
    ##  Max.   :13648   Max.   :2.000      
    ##  NA's   :3631

#### Bedeutung der NA’s

Die NA’s in den Spalten mit den User-Daten kommen daher, dass bei diesen
Accounts nur 1 Benutzer (Owner) eingetragen ist.

Wiederum wird den Spalten ein Präfix hinzugefügt (disp.)

``` r
if (!grepl("disp_", names(df_disposition_mod )[2])) {
names(df_disposition_mod) <- paste0("disp_", names(df_disposition_mod))
}
```

Drittes Zusammenführen

``` r
df_mod <- left_join(df_mod, df_disposition_mod, by = c("account_account_id" = "disp_account_id"))

glimpse(df_mod)
```

    ## Rows: 4,500
    ## Columns: 30
    ## $ account_account_id                <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1…
    ## $ account_district_id               <int> 18, 1, 5, 12, 15, 51, 60, 57, 70, 54…
    ## $ account_frequency                 <fct> Monthly, Monthly, Monthly, Monthly, …
    ## $ account_opening_date              <date> 1995-03-24, 1993-02-26, 1997-07-07,…
    ## $ account_opening_year              <dbl> 1995, 1993, 1997, 1996, 1997, 1994, …
    ## $ account_opening_month             <dbl> 3, 2, 7, 2, 5, 9, 11, 9, 1, 8, 10, 4…
    ## $ order_amount_sum_HOUSEHOLD        <dbl> 2452, 7266, 1135, 3363, 2668, 3954, …
    ## $ order_amount_sum_LOAN             <dbl> 0.0, 3372.7, 0.0, 0.0, 0.0, 0.0, 0.0…
    ## $ order_amount_sum_INSURRANCE       <dbl> 0, 0, 3539, 0, 0, 0, 0, 0, NA, 0, 0,…
    ## $ order_amount_sum_UNKNOWN          <dbl> 0, 0, 327, 0, 0, 0, 0, 6712, NA, 0, …
    ## $ order_amount_sum_LEASING          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, NA, 1344, 0,…
    ## $ order_payment_type_num_HOUSEHOLD  <dbl> 1, 1, 1, 2, 1, 1, 1, 1, NA, 1, 1, 2,…
    ## $ order_payment_type_num_LOAN       <dbl> 0, 1, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0,…
    ## $ order_payment_type_num_INSURRANCE <dbl> 0, 0, 1, 0, 0, 0, 0, 0, NA, 0, 0, 0,…
    ## $ order_payment_type_num_UNKNOWN    <dbl> 0, 0, 1, 0, 0, 0, 0, 1, NA, 0, 0, 0,…
    ## $ order_payment_type_num_LEASING    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, NA, 1, 0, 0,…
    ## $ loan_loan_id                      <int> NA, 4959, NA, NA, NA, NA, NA, NA, NA…
    ## $ loan_start_date                   <date> NA, 1994-01-05, NA, NA, NA, NA, NA,…
    ## $ loan_end_date                     <date> NA, 1996-01-05, NA, NA, NA, NA, NA,…
    ## $ loan_duration_in_month            <int> NA, 24, NA, NA, NA, NA, NA, NA, NA, …
    ## $ loan_duration_in_years            <dbl> NA, 2, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ loan_total_amount                 <int> NA, 80952, NA, NA, NA, NA, NA, NA, N…
    ## $ loan_redemption_amount            <dbl> NA, 3373, NA, NA, NA, NA, NA, NA, NA…
    ## $ loan_status                       <fct> NA, CONTRACT FINISHED PAYED, NA, NA,…
    ## $ loan_in_dept                      <lgl> NA, FALSE, NA, NA, NA, NA, NA, NA, N…
    ## $ disp_client_id_OWNER              <int> 1, 2, 4, 6, 7, 8, 9, 10, 12, 13, 14,…
    ## $ disp_client_id_USER               <int> NA, 3, 5, NA, NA, NA, NA, 11, NA, NA…
    ## $ disp_disp_id_OWNER                <int> 1, 2, 4, 6, 7, 8, 9, 10, 12, 13, 14,…
    ## $ disp_disp_id_USER                 <int> NA, 3, 5, NA, NA, NA, NA, 11, NA, NA…
    ## $ disp_account_num_of_user          <int> 1, 2, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, …

Entfernen der nicht mehr benötigten Tabellen

``` r
rm(df_disposition, df_disposition_mod, df_disposition_num_user)
```

### Data Frame “credit card”

``` r
glimpse(df_credit_card)
```

    ## Rows: 892
    ## Columns: 4
    ## $ card_id <int> 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19…
    ## $ disp_id <int> 9, 19, 41, 42, 51, 56, 60, 76, 77, 79, 83, 87, 112, 114, 116, …
    ## $ type    <chr> "GOLD", "CLASSIC", "GOLD", "CLASSIC", "JUNIOR", "CLASSIC", "JU…
    ## $ issued  <date> 1998-10-16, 1998-03-13, 1995-09-03, 1998-11-26, 1995-04-24, 1…

Hier braucht es nur eine Umformung der Spalte “type” in Faktor und das
Anfügen des Präfixes (card.)

``` r
df_credit_card_mod <- df_credit_card %>% 
   mutate(across(where(is.character), as.factor))

if (!grepl("card_", names(df_credit_card_mod)[2])) {
names(df_credit_card_mod) <- paste0("card_", names(df_credit_card_mod))
}
    
glimpse(df_credit_card_mod)
```

    ## Rows: 892
    ## Columns: 4
    ## $ card_card_id <int> 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 1…
    ## $ card_disp_id <int> 9, 19, 41, 42, 51, 56, 60, 76, 77, 79, 83, 87, 112, 114, …
    ## $ card_type    <fct> GOLD, CLASSIC, GOLD, CLASSIC, JUNIOR, CLASSIC, JUNIOR, CL…
    ## $ card_issued  <date> 1998-10-16, 1998-03-13, 1995-09-03, 1998-11-26, 1995-04-…

Viertes Zusammenführen

``` r
df_mod <- left_join(df_mod, df_credit_card_mod, by = c("disp_disp_id_OWNER" = "card_disp_id"))
 
glimpse(df_mod)
```

    ## Rows: 4,500
    ## Columns: 33
    ## $ account_account_id                <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1…
    ## $ account_district_id               <int> 18, 1, 5, 12, 15, 51, 60, 57, 70, 54…
    ## $ account_frequency                 <fct> Monthly, Monthly, Monthly, Monthly, …
    ## $ account_opening_date              <date> 1995-03-24, 1993-02-26, 1997-07-07,…
    ## $ account_opening_year              <dbl> 1995, 1993, 1997, 1996, 1997, 1994, …
    ## $ account_opening_month             <dbl> 3, 2, 7, 2, 5, 9, 11, 9, 1, 8, 10, 4…
    ## $ order_amount_sum_HOUSEHOLD        <dbl> 2452, 7266, 1135, 3363, 2668, 3954, …
    ## $ order_amount_sum_LOAN             <dbl> 0.0, 3372.7, 0.0, 0.0, 0.0, 0.0, 0.0…
    ## $ order_amount_sum_INSURRANCE       <dbl> 0, 0, 3539, 0, 0, 0, 0, 0, NA, 0, 0,…
    ## $ order_amount_sum_UNKNOWN          <dbl> 0, 0, 327, 0, 0, 0, 0, 6712, NA, 0, …
    ## $ order_amount_sum_LEASING          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, NA, 1344, 0,…
    ## $ order_payment_type_num_HOUSEHOLD  <dbl> 1, 1, 1, 2, 1, 1, 1, 1, NA, 1, 1, 2,…
    ## $ order_payment_type_num_LOAN       <dbl> 0, 1, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0,…
    ## $ order_payment_type_num_INSURRANCE <dbl> 0, 0, 1, 0, 0, 0, 0, 0, NA, 0, 0, 0,…
    ## $ order_payment_type_num_UNKNOWN    <dbl> 0, 0, 1, 0, 0, 0, 0, 1, NA, 0, 0, 0,…
    ## $ order_payment_type_num_LEASING    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, NA, 1, 0, 0,…
    ## $ loan_loan_id                      <int> NA, 4959, NA, NA, NA, NA, NA, NA, NA…
    ## $ loan_start_date                   <date> NA, 1994-01-05, NA, NA, NA, NA, NA,…
    ## $ loan_end_date                     <date> NA, 1996-01-05, NA, NA, NA, NA, NA,…
    ## $ loan_duration_in_month            <int> NA, 24, NA, NA, NA, NA, NA, NA, NA, …
    ## $ loan_duration_in_years            <dbl> NA, 2, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ loan_total_amount                 <int> NA, 80952, NA, NA, NA, NA, NA, NA, N…
    ## $ loan_redemption_amount            <dbl> NA, 3373, NA, NA, NA, NA, NA, NA, NA…
    ## $ loan_status                       <fct> NA, CONTRACT FINISHED PAYED, NA, NA,…
    ## $ loan_in_dept                      <lgl> NA, FALSE, NA, NA, NA, NA, NA, NA, N…
    ## $ disp_client_id_OWNER              <int> 1, 2, 4, 6, 7, 8, 9, 10, 12, 13, 14,…
    ## $ disp_client_id_USER               <int> NA, 3, 5, NA, NA, NA, NA, 11, NA, NA…
    ## $ disp_disp_id_OWNER                <int> 1, 2, 4, 6, 7, 8, 9, 10, 12, 13, 14,…
    ## $ disp_disp_id_USER                 <int> NA, 3, 5, NA, NA, NA, NA, 11, NA, NA…
    ## $ disp_account_num_of_user          <int> 1, 2, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, …
    ## $ card_card_id                      <int> NA, NA, NA, NA, NA, NA, 1, NA, NA, N…
    ## $ card_type                         <fct> NA, NA, NA, NA, NA, NA, GOLD, NA, NA…
    ## $ card_issued                       <date> NA, NA, NA, NA, NA, NA, 1998-10-16,…

Überprüfen, ob alle Kreditkarten übernommen wurden

``` r
summary(df_mod$card_type)
```

    ## CLASSIC    GOLD  JUNIOR    NA's 
    ##     659      88     145    3608

Gesamthaft wurden 892 Kreditkarten-Informationen übernommen.

Entfernen der nicht mehr benötigten Tabellen.

``` r
rm(df_credit_card, df_credit_card_mod)
```

### Data Frame “client”

``` r
glimpse(df_client)
```

    ## Rows: 5,369
    ## Columns: 4
    ## $ client_id   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
    ## $ district_id <int> 18, 1, 1, 5, 5, 12, 15, 51, 60, 57, 57, 40, 54, 76, 21, 21…
    ## $ dateofbirth <date> 1970-12-13, 1945-02-04, 1940-10-09, 1956-12-01, 1960-07-0…
    ## $ sex         <chr> "Female", "Male", "Female", "Male", "Female", "Male", "Mal…

Bei dieser Tabelle wird das Geschlecht in Faktoren umgewandelt und eine
zusätzliche Spalte für das Alter bei der Kontoeröffnung erstellt. Diese
zusätzliche Spalte kann erst nach dem Zusammenfügen mit den
Account-Daten generiert werden. Danach können die Informationen in
df_mod eingefügt werden. Dazu werden zwei verschiedene Tabellen
erstellt, eine für Owner und eine für den User.

``` r
df_client_mod <- df_client %>% 
 mutate(across(where(is.character), as.factor))
 
df_client_user <- df_client_mod
df_client_owner <- df_client_mod

if (!grepl("user_", names(df_client_user)[2])) {
names(df_client_user) <- paste0("user_", names(df_client_user))
}
 
if (!grepl("owner_", names(df_client_owner)[2])) {
names(df_client_owner) <- paste0("owner_", names(df_client_owner))
}
```

Fünftes Zusammenführen

``` r
df_mod <- left_join(df_mod, df_client_owner, by = c("disp_client_id_OWNER" = "owner_client_id"))
df_mod <- left_join(df_mod, df_client_user, by = c("disp_client_id_USER" = "user_client_id"))
```

Entfernen der nicht mehr benötigten Tabellen “client”

``` r
rm(df_client, df_client_mod, df_client_owner, df_client_user)
```

Spalten werden neu angeordnet und wo sinnvoll, umbenannt:

Umbenennen:

- disp_client_id_OWNER wird zu owner_client_id

- disp_client_id_USER wird zu user_client_id

- disp_disp_id_OWNER wird zu owner_disp_id

- disp_disp_id_USER wird zu user_disp_id

- card_card_id wird zu card_id

- loan_loan_id wird zu loan_id

- account_account_id wird zu account_id

- order_amount_sum_HOUSEHOLD wird zu owner_client_id

- order_amount_sum_INSURRANCE wird zu order_amount_insurrance

- order_amount_sum_LOAN wird zu order_amount_loan

- order_amount_sum_UNKNOWN wird zu order_amount_unknown

- order_amount_sum_LEASING wird zu order_amount_leasing

- order_payment_type_num_HOUSEHOLD wird zu order_num_household

- order_payment_type_num_INSURRANCE wird zu order_num_insurrance

- order_payment_type_num_LOAN wird zu order_num_loan

- order_payment_type_num_UNKNOWN wird zu order_num_unknown

- order_payment_type_num_LEASING wird zu order_num_leasing

``` r
# Spalten umbenennen und neu anordnen
df_mod  <- df_mod %>%
 rename(
   owner_client_id = disp_client_id_OWNER,
   user_client_id = disp_client_id_USER,
   owner_disp_id = disp_disp_id_OWNER,
   user_disp_id = disp_disp_id_USER,
   card_id = card_card_id,
   loan_id = loan_loan_id,
   account_id = account_account_id,
   account_num_of_user = disp_account_num_of_user,
   order_total_amount_household = order_amount_sum_HOUSEHOLD,
   order_total_amount_insurrance = order_amount_sum_INSURRANCE,
   order_total_amount_loan = order_amount_sum_LOAN,
   order_total_amount_unknown = order_amount_sum_UNKNOWN,
   order_total_amount_leasing = order_amount_sum_LEASING,
   order_num_household = order_payment_type_num_HOUSEHOLD,
   order_num_insurrance = order_payment_type_num_INSURRANCE,
   order_num_loan = order_payment_type_num_LOAN,
   order_num_unknown = order_payment_type_num_UNKNOWN,
   order_num_leasing = order_payment_type_num_LEASING
 ) 

#zusätzliche Spalte erstellen für das Alter der Owner bei der Kontoeröffnung.
df_mod <- df_mod %>% 
  mutate(owner_age_at_account_opening = trunc((owner_dateofbirth %--% account_opening_date) / years(1))) %>% 
  mutate(user_age_at_account_opening = trunc((user_dateofbirth %--% account_opening_date) / years(1)))
```

### Data Frame “District”

Übersicht über das Data Frame.

``` r
glimpse(df_district)
```

    ## Rows: 77
    ## Columns: 16
    ## $ district_id                             <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,…
    ## $ name                                    <chr> "Hl.m. Praha", "Benesov", "Ber…
    ## $ region                                  <chr> "Prague", "central Bohemia", "…
    ## $ inhabitants                             <int> 1204953, 88884, 75232, 149893,…
    ## $ municipalities_under_499_inhabitants    <int> 0, 80, 55, 63, 65, 60, 38, 95,…
    ## $ municipalities_500_to_1999_inhabitants  <int> 0, 26, 26, 29, 30, 23, 28, 19,…
    ## $ municipalities_2000_to_9999_inhabitants <int> 0, 6, 4, 6, 4, 4, 1, 7, 4, 4, …
    ## $ municipalities_over_10000_inhabitants   <int> 0, 6, 4, 6, 4, 4, 1, 7, 4, 4, …
    ## $ cities                                  <int> 1, 5, 5, 6, 6, 4, 6, 8, 6, 5, …
    ## $ ratio_urban_inhabitants                 <dbl> 100.0, 46.7, 41.7, 67.4, 51.4,…
    ## $ average_salary                          <int> 12541, 8507, 8980, 9753, 9307,…
    ## $ unemployment_rate_95                    <chr> "0.29", "1.67", "1.95", "4.64"…
    ## $ unemployment_rate_96                    <dbl> 0.43, 1.85, 2.21, 5.05, 4.43, …
    ## $ enterpreneurs_per_1000_inhabitants      <int> 167, 132, 111, 109, 118, 126, …
    ## $ commited_crimes_95                      <chr> "85677", "2159", "2824", "5244…
    ## $ commited_crimes_96                      <int> 99107, 2674, 2813, 5892, 3040,…

unemployment_rate_95 in dbl commited_crimes_95 in int anschliessend alle
chr in factor

``` r
df_district <- df_district %>% 
  mutate(across(c(unemployment_rate_95), as.double),
         across(c(commited_crimes_95), as.integer),
         across(where(is.character), as.factor))

glimpse(df_district)
```

    ## Rows: 77
    ## Columns: 16
    ## $ district_id                             <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,…
    ## $ name                                    <fct> Hl.m. Praha, Benesov, Beroun, …
    ## $ region                                  <fct> Prague, central Bohemia, centr…
    ## $ inhabitants                             <int> 1204953, 88884, 75232, 149893,…
    ## $ municipalities_under_499_inhabitants    <int> 0, 80, 55, 63, 65, 60, 38, 95,…
    ## $ municipalities_500_to_1999_inhabitants  <int> 0, 26, 26, 29, 30, 23, 28, 19,…
    ## $ municipalities_2000_to_9999_inhabitants <int> 0, 6, 4, 6, 4, 4, 1, 7, 4, 4, …
    ## $ municipalities_over_10000_inhabitants   <int> 0, 6, 4, 6, 4, 4, 1, 7, 4, 4, …
    ## $ cities                                  <int> 1, 5, 5, 6, 6, 4, 6, 8, 6, 5, …
    ## $ ratio_urban_inhabitants                 <dbl> 100.0, 46.7, 41.7, 67.4, 51.4,…
    ## $ average_salary                          <int> 12541, 8507, 8980, 9753, 9307,…
    ## $ unemployment_rate_95                    <dbl> 0.29, 1.67, 1.95, 4.64, 3.85, …
    ## $ unemployment_rate_96                    <dbl> 0.43, 1.85, 2.21, 5.05, 4.43, …
    ## $ enterpreneurs_per_1000_inhabitants      <int> 167, 132, 111, 109, 118, 126, …
    ## $ commited_crimes_95                      <int> 85677, 2159, 2824, 5244, 2616,…
    ## $ commited_crimes_96                      <int> 99107, 2674, 2813, 5892, 3040,…

Erstellen des Data Frames für den Import in das “df_mod”

``` r
df_district_mod <- df_district

if (!grepl("district_", names(df_district_mod )[2])) {
names(df_district_mod) <- paste0("district_", names(df_district_mod))
}

#Auswahl der Spalten für den Übertrag
df_district_select <- df_district_mod %>% 
  select(
    district_district_id,
    district_name,
    district_region,
    district_average_salary,
    district_inhabitants
  )

#Erstellen von zwei Data Frames für Account und Owner
df_district_account <- df_district_select
if (!grepl("account_", names(df_district_account )[2])) {
names(df_district_account) <- paste0("account_", names(df_district_account))
}

df_district_owner <- df_district_select
if (!grepl("owner_", names(df_district_owner )[2])) {
names(df_district_owner) <- paste0("owner_", names(df_district_owner))
}
```

Die vorbereiteten Observationen können nun dem df_mod hinzugefügt
werden.

``` r
df_mod <- left_join(df_mod, df_district_account, by = c("account_district_id" = "account_district_district_id"))

df_mod <- left_join(df_mod, df_district_owner, by = c("account_district_id" = "owner_district_district_id"))
```

Entfernen der nicht mehr benötigten Tabellen.

``` r
rm(df_district_account, df_district_owner, df_district_select)
#das df_district wird vorerst noch belassen, ev. wird es in einem späteren Schritt nochmals verwendet
```

Für das konsolidierte Data Frame werden die Spalten neu angeordnet.

``` r
#dieser Code verwenden, um das df_cons zu erstellen
df_mod <- df_mod %>%
  relocate(starts_with("account_")) %>% 
  relocate(starts_with("owner_"), .after = last_col()) %>%
  relocate(starts_with("user_"), .after = last_col()) %>%
  relocate(contains("id"), .after = last_col()) %>% 
  relocate(account_id)

glimpse(df_mod)
```

    ## Rows: 4,500
    ## Columns: 49
    ## $ account_id                      <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,…
    ## $ account_frequency               <fct> Monthly, Monthly, Monthly, Monthly, Mo…
    ## $ account_opening_date            <date> 1995-03-24, 1993-02-26, 1997-07-07, 1…
    ## $ account_opening_year            <dbl> 1995, 1993, 1997, 1996, 1997, 1994, 19…
    ## $ account_opening_month           <dbl> 3, 2, 7, 2, 5, 9, 11, 9, 1, 8, 10, 4, …
    ## $ account_num_of_user             <int> 1, 2, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2,…
    ## $ account_district_name           <fct> Pisek, Hl.m. Praha, Kolin, Pribram, Ce…
    ## $ account_district_region         <fct> south Bohemia, Prague, central Bohemia…
    ## $ account_district_average_salary <int> 8968, 12541, 9307, 8754, 9045, 8541, 8…
    ## $ account_district_inhabitants    <int> 70699, 1204953, 95616, 107870, 58796, …
    ## $ order_total_amount_household    <dbl> 2452, 7266, 1135, 3363, 2668, 3954, 48…
    ## $ order_total_amount_loan         <dbl> 0.0, 3372.7, 0.0, 0.0, 0.0, 0.0, 0.0, …
    ## $ order_total_amount_insurrance   <dbl> 0, 0, 3539, 0, 0, 0, 0, 0, NA, 0, 0, 0…
    ## $ order_total_amount_unknown      <dbl> 0, 0, 327, 0, 0, 0, 0, 6712, NA, 0, 0,…
    ## $ order_total_amount_leasing      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, NA, 1344, 0, 0…
    ## $ order_num_household             <dbl> 1, 1, 1, 2, 1, 1, 1, 1, NA, 1, 1, 2, 1…
    ## $ order_num_loan                  <dbl> 0, 1, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0…
    ## $ order_num_insurrance            <dbl> 0, 0, 1, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0…
    ## $ order_num_unknown               <dbl> 0, 0, 1, 0, 0, 0, 0, 1, NA, 0, 0, 0, 0…
    ## $ order_num_leasing               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, NA, 1, 0, 0, 0…
    ## $ loan_start_date                 <date> NA, 1994-01-05, NA, NA, NA, NA, NA, N…
    ## $ loan_end_date                   <date> NA, 1996-01-05, NA, NA, NA, NA, NA, N…
    ## $ loan_duration_in_month          <int> NA, 24, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ loan_duration_in_years          <dbl> NA, 2, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ loan_total_amount               <int> NA, 80952, NA, NA, NA, NA, NA, NA, NA,…
    ## $ loan_redemption_amount          <dbl> NA, 3373, NA, NA, NA, NA, NA, NA, NA, …
    ## $ loan_status                     <fct> NA, CONTRACT FINISHED PAYED, NA, NA, N…
    ## $ loan_in_dept                    <lgl> NA, FALSE, NA, NA, NA, NA, NA, NA, NA,…
    ## $ card_type                       <fct> NA, NA, NA, NA, NA, NA, GOLD, NA, NA, …
    ## $ card_issued                     <date> NA, NA, NA, NA, NA, NA, 1998-10-16, N…
    ## $ owner_dateofbirth               <date> 1970-12-13, 1945-02-04, 1956-12-01, 1…
    ## $ owner_sex                       <fct> Female, Male, Male, Male, Male, Female…
    ## $ owner_age_at_account_opening    <dbl> 24, 48, 40, 76, 68, 56, 61, 52, 11, 22…
    ## $ owner_district_name             <fct> Pisek, Hl.m. Praha, Kolin, Pribram, Ce…
    ## $ owner_district_region           <fct> south Bohemia, Prague, central Bohemia…
    ## $ owner_district_average_salary   <int> 8968, 12541, 9307, 8754, 9045, 8541, 8…
    ## $ owner_district_inhabitants      <int> 70699, 1204953, 95616, 107870, 58796, …
    ## $ user_dateofbirth                <date> NA, 1940-10-09, 1960-07-03, NA, NA, N…
    ## $ user_sex                        <fct> NA, Female, Female, NA, NA, NA, NA, Fe…
    ## $ user_age_at_account_opening     <dbl> NA, 52, 37, NA, NA, NA, NA, 45, NA, NA…
    ## $ account_district_id             <int> 18, 1, 5, 12, 15, 51, 60, 57, 70, 54, …
    ## $ loan_id                         <int> NA, 4959, NA, NA, NA, NA, NA, NA, NA, …
    ## $ card_id                         <int> NA, NA, NA, NA, NA, NA, 1, NA, NA, NA,…
    ## $ owner_client_id                 <int> 1, 2, 4, 6, 7, 8, 9, 10, 12, 13, 14, 1…
    ## $ owner_disp_id                   <int> 1, 2, 4, 6, 7, 8, 9, 10, 12, 13, 14, 1…
    ## $ owner_district_id               <int> 18, 1, 5, 12, 15, 51, 60, 57, 40, 54, …
    ## $ user_client_id                  <int> NA, 3, 5, NA, NA, NA, NA, 11, NA, NA, …
    ## $ user_disp_id                    <int> NA, 3, 5, NA, NA, NA, NA, 11, NA, NA, …
    ## $ user_district_id                <int> NA, 1, 5, NA, NA, NA, NA, 57, NA, NA, …

#### Erste Analysen

Sonderstellung: Bedeutung des Account Owner

Wie wir bei der Datenbeschreibung bereits gelesen haben, enthält
disposition_type die Information über die Rechte der Konten. Deshalb
wurde der Eintrag beim Aufbereiten des df_disposition von “disponent” in
“user” geändert. Denn nur der “owner” hat die nötigen Berechtigungen, um
Daueraufträge zu erteilen und Darlehen zu beantragen. So wird die
Auswertung übersichtlicher.

Auf Duplikate überprüfen

``` r
n_distinct(df_mod)
```

    ## [1] 4500

Es sind keine Duplikate vorhanden.

### Data Frame “transaction”

Das Data Frame Transaction wird vorerst separat aufbereitet und ersten
Analysen unterzogen. Aus den folgenden Analysen werden neue Spalten
generiert, welche dann in einem weiteren Schritt in den konsolidierten
Datensatz df_cons übernommen werden können. Auch wird während den
Analysen entschieden, ob zusätzlich zum konsolidierten Datensatz noch
ein Datensatz mit Transaktionsdaten bestehen bleibt, oder ob alles in
df_cons zusammengeführt wird.

Übersicht über die Tabelle df_transaction.

``` r
glimpse(df_transaction)
```

    ## Rows: 1,056,320
    ## Columns: 10
    ## $ trans_id         <int> 1, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,…
    ## $ account_id       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ date             <date> 1995-03-24, 1995-04-13, 1995-05-13, 1995-06-13, 1995…
    ## $ type             <chr> "CREDIT", "CREDIT", "CREDIT", "CREDIT", "CREDIT", "CR…
    ## $ operation        <chr> "CASH CREDIT", "COLLECTION OTHER BANK", "COLLECTION O…
    ## $ amount           <dbl> 1000, 3679, 3679, 3679, 3679, 3679, 3679, 3679, 3679,…
    ## $ balance          <dbl> 1000.0, 4679.0, 20977.2, 26835.2, 30414.8, 28902.7, 2…
    ## $ characterization <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ bank             <chr> "", "AB", "AB", "AB", "AB", "AB", "AB", "AB", "AB", "…
    ## $ account          <int> NA, 41403269, 41403269, 41403269, 41403269, 41403269,…

#### Dateitypen anpassen

Characters in Faktoren ändern.

``` r
df_transaction_mod <- df_transaction %>% 
  mutate(across(where(is.character), as.factor))

summary(df_transaction_mod)
```

    ##     trans_id         account_id         date                    type       
    ##  Min.   :      1   Min.   :    1   Min.   :1993-01-01   CREDIT    :405083  
    ##  1st Qu.: 430263   1st Qu.: 1204   1st Qu.:1996-01-16   WITHDRAWAL:634571  
    ##  Median : 858506   Median : 2434   Median :1997-04-10   NA's      : 16666  
    ##  Mean   :1335311   Mean   : 2937   Mean   :1997-01-04                      
    ##  3rd Qu.:2060979   3rd Qu.: 3660   3rd Qu.:1998-02-28                      
    ##  Max.   :3682987   Max.   :11382   Max.   :1998-12-31                      
    ##                                                                            
    ##                   operation          amount           balance      
    ##  CASH CREDIT           :156743   Min.   :    0.0   Min.   :-41126  
    ##  CASH WIDTHDRAWAL      :434918   1st Qu.:  135.9   1st Qu.: 22402  
    ##  COLLECTION OTHER BANK : 65226   Median : 2100.0   Median : 33143  
    ##  CREDIT CARD WITHDRAWAL:  8036   Mean   : 5924.1   Mean   : 38518  
    ##  REMITTANCE OTHER BANK :208283   3rd Qu.: 6800.0   3rd Qu.: 49604  
    ##  NA's                  :183114   Max.   :87400.0   Max.   :209637  
    ##                                                                    
    ##            characterization       bank           account        
    ##  CREDIT INTEREST   :183114          :782812   Min.   :       0  
    ##  STATEMENT PAYMENT :155832   QR     : 22285   1st Qu.:17828584  
    ##  HOUSEHOLD         :118065   AB     : 21720   Median :45750951  
    ##  OLD AGE PENSION   : 30338   ST     : 21711   Mean   :45670919  
    ##  INSURRANCE PAYMENT: 18500   YZ     : 21582   3rd Qu.:72013407  
    ##  (Other)           : 15157   GH     : 21499   Max.   :99994199  
    ##  NA's              :535314   (Other):164711   NA's   :760931

``` r
glimpse(df_transaction_mod)
```

    ## Rows: 1,056,320
    ## Columns: 10
    ## $ trans_id         <int> 1, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,…
    ## $ account_id       <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ date             <date> 1995-03-24, 1995-04-13, 1995-05-13, 1995-06-13, 1995…
    ## $ type             <fct> CREDIT, CREDIT, CREDIT, CREDIT, CREDIT, CREDIT, CREDI…
    ## $ operation        <fct> CASH CREDIT, COLLECTION OTHER BANK, COLLECTION OTHER …
    ## $ amount           <dbl> 1000, 3679, 3679, 3679, 3679, 3679, 3679, 3679, 3679,…
    ## $ balance          <dbl> 1000.0, 4679.0, 20977.2, 26835.2, 30414.8, 28902.7, 2…
    ## $ characterization <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ bank             <fct> , AB, AB, AB, AB, AB, AB, AB, AB, AB, AB, AB, AB, AB,…
    ## $ account          <int> NA, 41403269, 41403269, 41403269, 41403269, 41403269,…

``` r
rm(df_transaction)
```

#### Spalten umbenennen

- bank wird zu bank_name

- account wird zu account_nr

- type wird zu cashflow

``` r
df_transaction_mod <- df_transaction_mod %>% 
  rename(
    bank_name = bank,
    account_nr = account,
    cashflow = type
  )
```

#### Untersuchen der NA’s

Als erstes beginnen wir mit der neu benannten Spalte cashflow

``` r
df_transaction_mod %>% 
  filter(is.na(cashflow)) %>% 
  summary()
```

    ##     trans_id         account_id         date                  cashflow    
    ##  Min.   :    493   Min.   :    2   Min.   :1993-03-26   CREDIT    :    0  
    ##  1st Qu.: 505006   1st Qu.: 1725   1st Qu.:1996-02-20   WITHDRAWAL:    0  
    ##  Median : 988418   Median : 3370   Median :1997-05-24   NA's      :16666  
    ##  Mean   :1231199   Mean   : 4140   Mean   :1997-02-01                     
    ##  3rd Qu.:1850479   3rd Qu.: 6265   3rd Qu.:1998-04-04                     
    ##  Max.   :3431140   Max.   :11382   Max.   :1998-12-30                     
    ##                                                                           
    ##                   operation         amount         balance      
    ##  CASH CREDIT           :    0   Min.   : 2001   Min.   :-10955  
    ##  CASH WIDTHDRAWAL      :16666   1st Qu.: 6738   1st Qu.: 28803  
    ##  COLLECTION OTHER BANK :    0   Median :12103   Median : 41894  
    ##  CREDIT CARD WITHDRAWAL:    0   Mean   :12517   Mean   : 45290  
    ##  REMITTANCE OTHER BANK :    0   3rd Qu.:18081   3rd Qu.: 59785  
    ##                                 Max.   :24997   Max.   :174283  
    ##                                                                 
    ##            characterization   bank_name       account_nr   
    ##  CREDIT INTEREST   :    0          :16666   Min.   : NA    
    ##  HOUSEHOLD         :    0   AB     :    0   1st Qu.: NA    
    ##  INSURRANCE PAYMENT:    0   CD     :    0   Median : NA    
    ##  LOAN PAYMENT      :    0   EF     :    0   Mean   :NaN    
    ##  OLD AGE PENSION   :    0   GH     :    0   3rd Qu.: NA    
    ##  (Other)           :    0   IJ     :    0   Max.   : NA    
    ##  NA's              :16666   (Other):    0   NA's   :16666

Die NA’s in “cashflow” und “characterization” sind Geldausgänge und
können deshalb als Werte in die beiden Spalten imputiert werden. Bei der
Spalte “bank_name” fällt auf, dass die Werte leer sind. Deshalb wird
dort mit NA’s ergänzt. Es könnte sich bei diesen Geldbezügen um Bezüge
an den Automaten handeln. Dies kann aber erst überprüft werden, wenn
diese Informationen mit den Beobachtungen aus der Tabelle mit den
Kreditkarten-Informationen abgeglichen werden kann. Diese Analyse kann
zu einem späteren Zeitpunkt vorgenommen werden.

#### Imputieren

``` r
#Liste für die gezielte Imputation in "characterization" mit "CASH WIDTHDRAWAL"
list_na_characterization_for_cashwidthrawal <- df_transaction_mod %>% 
  filter(is.na(cashflow)) %>% 
  select(trans_id)

cashwidthrawals <- df_transaction_mod %>% 
  filter(trans_id %in% list_na_characterization_for_cashwidthrawal$trans_id)
  
cashwidthrawals$characterization <- replace_na("CASH WIDTHDRAWAL")
head(cashwidthrawals)
```

    ##   trans_id account_id       date cashflow        operation amount balance
    ## 1      493          2 1993-08-22     <NA> CASH WIDTHDRAWAL  21286 27852.9
    ## 2      495          2 1993-12-21     <NA> CASH WIDTHDRAWAL  13396 30816.9
    ## 3      497          2 1993-07-19     <NA> CASH WIDTHDRAWAL  13145 49180.5
    ## 4      503          2 1994-09-20     <NA> CASH WIDTHDRAWAL  12041 25605.9
    ## 5      508          2 1995-10-17     <NA> CASH WIDTHDRAWAL   9336 26735.7
    ## 6      509          2 1995-04-20     <NA> CASH WIDTHDRAWAL   6011 33611.4
    ##   characterization bank_name account_nr
    ## 1 CASH WIDTHDRAWAL                   NA
    ## 2 CASH WIDTHDRAWAL                   NA
    ## 3 CASH WIDTHDRAWAL                   NA
    ## 4 CASH WIDTHDRAWAL                   NA
    ## 5 CASH WIDTHDRAWAL                   NA
    ## 6 CASH WIDTHDRAWAL                   NA

``` r
rm(cashwidthrawals)
```

#### Cashflow mit “IN” und “OUT” mutieren

Nebst dem mutieren werden die NA’s mit “OUT” imputiert. Dies geht aus
der vorgängigen Analyse hervor.

``` r
df_transaction_mod <- df_transaction_mod %>% 
  mutate(cashflow = case_when(cashflow == "WITHDRAWAL" ~ "OUT",
                              cashflow == "CREDIT" ~ "IN",
                              TRUE ~ "OUT")) %>%
  mutate(across(where(is.character), as.factor))

head(df_transaction_mod)
```

    ##   trans_id account_id       date cashflow             operation amount balance
    ## 1        1          1 1995-03-24       IN           CASH CREDIT   1000  1000.0
    ## 2        5          1 1995-04-13       IN COLLECTION OTHER BANK   3679  4679.0
    ## 3        6          1 1995-05-13       IN COLLECTION OTHER BANK   3679 20977.2
    ## 4        7          1 1995-06-13       IN COLLECTION OTHER BANK   3679 26835.2
    ## 5        8          1 1995-07-13       IN COLLECTION OTHER BANK   3679 30414.8
    ## 6        9          1 1995-08-13       IN COLLECTION OTHER BANK   3679 28902.7
    ##   characterization bank_name account_nr
    ## 1             <NA>                   NA
    ## 2             <NA>        AB   41403269
    ## 3             <NA>        AB   41403269
    ## 4             <NA>        AB   41403269
    ## 5             <NA>        AB   41403269
    ## 6             <NA>        AB   41403269

#### Entfernen der nicht mehr benötigten data frames.

``` r
rm(list_na_characterization_for_cashwidthrawal, list_transid_for_imputation)
```

#### Vobereitung des Datum-Attributes {Year}\_Q{Quarter} per transaction

``` r
df_transaction_mod <- df_transaction_mod %>% 
  mutate(quarter = zoo::as.yearqtr(date))
```

#### Bilanz Ende Jahr

#### Erstellen zusätzlicher Spalten “year” und “month”

Im Data Frame “df_transaction_mod” werden zwei zusätzliche Spalten für
das Jahr der Transaktion sowie für den Monat erstellt.

``` r
df_transaction_mod <- df_transaction_mod %>% 
  mutate(
    year = year(date),
    month = month(date)
  )

head(df_transaction_mod)
```

    ##   trans_id account_id       date cashflow             operation amount balance
    ## 1        1          1 1995-03-24       IN           CASH CREDIT   1000  1000.0
    ## 2        5          1 1995-04-13       IN COLLECTION OTHER BANK   3679  4679.0
    ## 3        6          1 1995-05-13       IN COLLECTION OTHER BANK   3679 20977.2
    ## 4        7          1 1995-06-13       IN COLLECTION OTHER BANK   3679 26835.2
    ## 5        8          1 1995-07-13       IN COLLECTION OTHER BANK   3679 30414.8
    ## 6        9          1 1995-08-13       IN COLLECTION OTHER BANK   3679 28902.7
    ##   characterization bank_name account_nr quarter year month
    ## 1             <NA>                   NA 1995 Q1 1995     3
    ## 2             <NA>        AB   41403269 1995 Q2 1995     4
    ## 3             <NA>        AB   41403269 1995 Q2 1995     5
    ## 4             <NA>        AB   41403269 1995 Q2 1995     6
    ## 5             <NA>        AB   41403269 1995 Q3 1995     7
    ## 6             <NA>        AB   41403269 1995 Q3 1995     8

#### durchschnittliche Bilanz

``` r
years_avg <- df_transaction_mod %>% 
  group_by(
    account_id,
    cashflow,
    year
  ) %>% 
  summarise(amount_mean = mean(amount)) %>% 
  pivot_wider(names_from = cashflow, values_from = amount_mean) %>% 
  mutate(balance_sheet = IN - OUT) %>% 
  ungroup()

balance_sheet <- years_avg %>% 
    select(
      account_id,
      year,
      balance_sheet
    ) %>% 
    arrange(year) %>% 
    pivot_wider(names_from = year, values_from = balance_sheet) %>% 
    arrange(account_id)
  
if (!grepl("balance_sheet_end_of.", names(balance_sheet)[2])) {
names(balance_sheet) <- paste0("balance_sheet_end_of.", names(balance_sheet))
}

head(years_avg)
```

    ## # A tibble: 6 × 5
    ##   account_id  year     IN   OUT balance_sheet
    ##        <int> <dbl>  <dbl> <dbl>         <dbl>
    ## 1          1  1995  2360. 1744.          616.
    ## 2          1  1996  1726. 1235.          491.
    ## 3          1  1997  1825. 1274.          551.
    ## 4          1  1998  1817. 1263.          553.
    ## 5          2  1993 10395. 6276.         4118.
    ## 6          2  1994 11027. 4255.         6772.

``` r
head(balance_sheet)
```

    ## # A tibble: 6 × 7
    ##   balance_sheet_end_of.account_id balance_sheet_end_of.…¹ balance_sheet_end_of…²
    ##                             <int>                   <dbl>                  <dbl>
    ## 1                               1                     NA                     NA 
    ## 2                               2                   4118.                  6772.
    ## 3                               3                     NA                     NA 
    ## 4                               4                     NA                     NA 
    ## 5                               5                     NA                     NA 
    ## 6                               6                     NA                     NA 
    ## # ℹ abbreviated names: ¹​balance_sheet_end_of.1993, ²​balance_sheet_end_of.1994
    ## # ℹ 4 more variables: balance_sheet_end_of.1995 <dbl>,
    ## #   balance_sheet_end_of.1996 <dbl>, balance_sheet_end_of.1997 <dbl>,
    ## #   balance_sheet_end_of.1998 <dbl>

Für die Analysen werden noch die jeweiligen mittleren Werte der
Kontostände benötigt. Dazu wird ein zusätzliches Data Frame mit den
Mittelwerten erstellt.

``` r
transaction_cashflow_per_month_and_year_mean <- df_transaction_mod %>%
  group_by(
    year,
    month,
    account_id,
    cashflow,
    ) %>%
  arrange(year, month) %>%
  summarise(amount_mean = mean(amount)) %>%
  pivot_wider(names_from = cashflow, values_from = amount_mean) %>% 
  mutate(IN = replace_na(IN, 0)) %>% 
  mutate(OUT = replace_na(OUT, 0)) %>% 
  mutate(balance = IN - OUT) 

balance_per_month_mean <- transaction_cashflow_per_month_and_year_mean %>% 
  select(
    account_id,
    year,
    month,
    balance
  ) %>% 
  pivot_wider(names_from = c(year,month), values_from = balance)

head(balance_per_month_mean)
```

    ## # A tibble: 6 × 73
    ## # Groups:   account_id [6]
    ##   account_id `1993_1` `1993_2` `1993_3` `1993_4` `1993_5` `1993_6` `1993_7`
    ##        <int>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ## 1          9      400    3902.    2218    -1757.   -1752.   -2295.    -673.
    ## 2        163      900    6698     6698     2855.    3294.    1634.    1872.
    ## 3        192      300    5859     5859     5859     5859    -1005.    1436.
    ## 4        212      200   10021.   -4817.  -15183.  -11402    19735.   16257.
    ## 5        280      300    6087.     964.    1279.   -6210.    2753.    3518.
    ## 6        346      800    7066.    2129.   -5155.   -8549.    1994.    5579.
    ## # ℹ 65 more variables: `1993_8` <dbl>, `1993_9` <dbl>, `1993_10` <dbl>,
    ## #   `1993_11` <dbl>, `1993_12` <dbl>, `1994_1` <dbl>, `1994_2` <dbl>,
    ## #   `1994_3` <dbl>, `1994_4` <dbl>, `1994_5` <dbl>, `1994_6` <dbl>,
    ## #   `1994_7` <dbl>, `1994_8` <dbl>, `1994_9` <dbl>, `1994_10` <dbl>,
    ## #   `1994_11` <dbl>, `1994_12` <dbl>, `1995_1` <dbl>, `1995_2` <dbl>,
    ## #   `1995_3` <dbl>, `1995_4` <dbl>, `1995_5` <dbl>, `1995_6` <dbl>,
    ## #   `1995_7` <dbl>, `1995_8` <dbl>, `1995_9` <dbl>, `1995_10` <dbl>, …

#### Wichtige Erkenntnis

Eine Überprüfung der Werte hat ergeben, dass dies so nicht gerechnet
werden Kann. Es entstehen dabei Rechnungsfehler, da zum Beispiel bei
einem Eingang mit mehreren Werten der Durchschnitt berechnet wird und
wenn dann ein Ausgang mit nur einem Wert vorhanden ist, dieser abgezogen
wird, stimmt das Verhältnis der Beträge nicht mehr. Daher wird nur die
Berechnung mit den totalen verwendet und damit weiter gearbeitet!

Balance Data Frames entfernen

``` r
rm(balance_per_month_mean, balance_per_month_total_lag, balance_sheet_per_years_total, transaction_cashflow_per_month_and_year_mean, transaction_cashflow_per_month_and_year_total)
```

#### Kontostand Ende Monat

Als erstes werden die Kontostände per Ende jeden Monats berechnet. Dazu
müssen die entstehenden NA’s mit der Zahl Null ersetzt werden, da ja bei
diesen Kontoständen sowie Geldein-/ausgängen 0 CZK vorhanden sind.

``` r
transaction_cashflow_per_month_and_year_total <- df_transaction_mod %>%
  group_by(
    year,
    month,
    account_id,
    cashflow,
    ) %>%
  arrange(year, month) %>%
  summarise(amount_total = sum(amount)) %>%
  pivot_wider(names_from = cashflow, values_from = amount_total) %>%
  mutate(IN = replace_na(IN, 0)) %>%
  mutate(OUT = replace_na(OUT, 0)) %>%
  mutate(balance_sheet = IN - OUT) %>% 
  ungroup()

head(transaction_cashflow_per_month_and_year_total)
```

    ## # A tibble: 6 × 6
    ##    year month account_id    IN   OUT balance_sheet
    ##   <dbl> <dbl>      <int> <dbl> <dbl>         <dbl>
    ## 1  1993     1          9   400     0           400
    ## 2  1993     1        163   900     0           900
    ## 3  1993     1        192   300     0           300
    ## 4  1993     1        212   200     0           200
    ## 5  1993     1        280   300     0           300
    ## 6  1993     1        346   800     0           800

Leider ist kein Erfolg zu verzeichnen.

#### lead und lag

Weiterer Versuch mit lead() und lag() den Kontostand Ende Monat
auszurechnen:

``` r
balance_per_month_total_lead <- transaction_cashflow_per_month_and_year_total %>% 
  arrange(account_id) %>% 
  mutate(account_id_lead = dplyr::lead(account_id)) %>% 
  mutate(account_id_lead = replace_na(account_id_lead, 0)) %>% 
  mutate(balance_lead = dplyr::lead(balance_sheet)) %>% 
  mutate(balance_lead = replace_na(balance_lead, 0)) %>%
  mutate(balance_per_month = ifelse(c(account_id == account_id_lead), balance_sheet + balance_lead, balance_sheet)) %>% 
  relocate(account_id_lead)
  

balance_per_month_total_lag <- transaction_cashflow_per_month_and_year_total %>% 
  arrange(account_id) %>% 
  mutate(account_id_lag = dplyr::lag(account_id)) %>% 
  mutate(account_id_lag = replace_na(account_id_lag, 0)) %>% 
  relocate(account_id_lag) %>% 
  mutate(row_num = 1: nrow(.)) %>%
  mutate(row_num_lag = dplyr::lag(row_num)) %>% 
  mutate(balance_per_month = ifelse(c(account_id != account_id_lag), balance_sheet, 0)) %>%   mutate(balance_per_month = ifelse(c(account_id == account_id_lag), balance_per_month[row_num_lag] + balance_sheet, balance_per_month))

head(balance_per_month_total_lead)
```

    ## # A tibble: 6 × 9
    ##   account_id_lead  year month account_id     IN   OUT balance_sheet balance_lead
    ##             <int> <dbl> <dbl>      <int>  <dbl> <dbl>         <dbl>        <dbl>
    ## 1               1  1995     3          1  1000     0          1000        16298.
    ## 2               1  1995     4          1 16298.    0         16298.        5858 
    ## 3               1  1995     5          1  5858     0          5858         3580.
    ## 4               1  1995     6          1  3780.  200          3580.       -1512.
    ## 5               1  1995     7          1  3788. 5300         -1512.       -3736.
    ## 6               1  1995     8          1  3778. 7515.        -3736.         604.
    ## # ℹ 1 more variable: balance_per_month <dbl>

``` r
head(balance_per_month_total_lag)
```

    ## # A tibble: 6 × 10
    ##   account_id_lag  year month account_id     IN   OUT balance_sheet row_num
    ##            <int> <dbl> <dbl>      <int>  <dbl> <dbl>         <dbl>   <int>
    ## 1              0  1995     3          1  1000     0          1000        1
    ## 2              1  1995     4          1 16298.    0         16298.       2
    ## 3              1  1995     5          1  5858     0          5858        3
    ## 4              1  1995     6          1  3780.  200          3580.       4
    ## 5              1  1995     7          1  3788. 5300         -1512.       5
    ## 6              1  1995     8          1  3778. 7515.        -3736.       6
    ## # ℹ 2 more variables: row_num_lag <int>, balance_per_month <dbl>

Auch dies führt nicht zum gewünschten Ergebnis.

#### Suche nach einer anderen Möglichkeit für die monatlichen Kontoauszügen

Im ursprünglichen Datensatz “df_transaction_mod” sind die Kontostände
nach jeder Transaktion vorhanden. Es sollte möglich sein, anhand des
letzten Datums herauszufinden, welches die letzte Transaktion war und
somit welches der Kontostand Ende Monat ist. Dazu wird ein
Zufallsgenerator erstellt, um verschiedene Konten genauer zu
untersuchen.

``` r
#Zufallsnummer
n = runif(1, min = 0, max = nrow(df_transaction_mod))

df_transaction_mod %>%
  arrange(date) %>%
  filter(
    account_id == account_id[n],
    year == 1995,
    month == 3
  )
```

    ##   trans_id account_id       date cashflow             operation amount balance
    ## 1   314422       1073 1995-03-03      OUT      CASH WIDTHDRAWAL  720.0 22469.6
    ## 2   314262       1073 1995-03-13       IN           CASH CREDIT 5765.0 28234.6
    ## 3   314322       1073 1995-03-13      OUT REMITTANCE OTHER BANK 3843.0 24391.6
    ## 4   314509       1073 1995-03-24      OUT      CASH WIDTHDRAWAL  700.0 23691.6
    ## 5   314421       1073 1995-03-27      OUT      CASH WIDTHDRAWAL 3000.0 20691.6
    ## 6   314453       1073 1995-03-31      OUT      CASH WIDTHDRAWAL   14.6 20779.4
    ## 7  3565844       1073 1995-03-31       IN                  <NA>  102.5 20794.0
    ##    characterization bank_name account_nr quarter year month
    ## 1              <NA>                   NA 1995 Q1 1995     3
    ## 2              <NA>                   NA 1995 Q1 1995     3
    ## 3         HOUSEHOLD        OP   88794773 1995 Q1 1995     3
    ## 4              <NA>                   NA 1995 Q1 1995     3
    ## 5              <NA>                   NA 1995 Q1 1995     3
    ## 6 STATEMENT PAYMENT                   NA 1995 Q1 1995     3
    ## 7   CREDIT INTEREST                   NA 1995 Q1 1995     3

``` r
rm(n)
```

Es fällt auf, dass die letzte Transaktion immer der Betrag 14.6 ist. Es
ist immer ein OUT. Diese Hypothese wird nun nochmals überprüft.

``` r
df_transaction_mod %>% 
  filter(
    amount == 14.6
  ) %>%
  head()
```

    ##   trans_id account_id       date cashflow        operation amount balance
    ## 1      159          1 1995-08-31      OUT CASH WIDTHDRAWAL   14.6 21487.3
    ## 2      160          1 1995-09-30      OUT CASH WIDTHDRAWAL   14.6 22091.2
    ## 3      161          1 1995-10-31      OUT CASH WIDTHDRAWAL   14.6 20494.1
    ## 4      162          1 1995-11-30      OUT CASH WIDTHDRAWAL   14.6 19021.6
    ## 5      163          1 1995-12-31      OUT CASH WIDTHDRAWAL   14.6 18173.0
    ## 6      164          1 1996-01-31      OUT CASH WIDTHDRAWAL   14.6 13579.5
    ##    characterization bank_name account_nr quarter year month
    ## 1 STATEMENT PAYMENT                   NA 1995 Q3 1995     8
    ## 2 STATEMENT PAYMENT                   NA 1995 Q3 1995     9
    ## 3 STATEMENT PAYMENT                   NA 1995 Q4 1995    10
    ## 4 STATEMENT PAYMENT                   NA 1995 Q4 1995    11
    ## 5 STATEMENT PAYMENT                   NA 1995 Q4 1995    12
    ## 6 STATEMENT PAYMENT                   NA 1996 Q1 1996     1

#### Kontogebühren

Es handelt sich hier vermutlich um Kontogebühren, welche als letzte
Transaktion getätigt werden. Nun wird noch nach dem Wert “STATEMENT
PAYMENT” gefiltert.

``` r
df_transaction_mod %>%
  filter(
    characterization == "STATEMENT PAYMENT"
  ) %>%
  arrange(desc(amount))
```

Die Beträge sind nicht immer gleich hoch. Dies soll genauer untersucht
werden. Damit die verschiedenen vorkommenden Beträge aufgelistet werden
können, wird eine zusätzliche Spalte generiert, in der die Werte zu
Faktoren umgewandelt werden. Anschliessend können die Levels angezeigt
werden. Die Hypothese ist, dass die Höhe er Beträge Aussagen macht, ob
es sich dabei um Privat- oder Geschäftskunden handelt.

``` r
df_statement_payment_levels <- df_transaction_mod %>% 
  filter(
    characterization == "STATEMENT PAYMENT"
  ) %>% 
  mutate(amount_levels = as.factor(amount))

levels(df_statement_payment_levels$amount_levels)
```

Es gibt gesamthahft 3 verschieden hohe Gebühren für die Konten: 14.60,
30, 100.

Entfernen des df_statement_payment_levels

``` r
rm(df_statement_payment_levels)
```

Bei einer späteren Analyse kann versucht werden, die Gebührenhöhe zu
erklären. Dabei können folgende Fragen beantwortet werden:

- Handelt es sich bei den Konten mit der Gebühr 100 um Geschäftskonten?

- Sind diese Konten in einer bestimmten Filiale oder einem bestimmten
  District?

- Wurden die Kontogebühren über die Jahre erhöht?

Es wird nun aber weiter am Datensatz für die monatlichen Gebühren
gearbeitet. Dafür muss überprüft werden, ob immer Ende Monat eine
Kontogebühr abgebucht wurde.

#### Erstellen des Datensatzes für die monatlichen Kontostände

statement_payment

``` r
statement_payment <- df_transaction_mod %>%
  filter(
    characterization == "STATEMENT PAYMENT"
  ) %>%
  mutate(
    statement_payment = date
  ) %>%
  select(
    trans_id,
    statement_payment
  )

head(statement_payment)
```

    ##   trans_id statement_payment
    ## 1      159        1995-08-31
    ## 2      160        1995-09-30
    ## 3      161        1995-10-31
    ## 4      162        1995-11-30
    ## 5      163        1995-12-31
    ## 6      164        1996-01-31

balance_per_month

``` r
balance_per_month <- df_transaction_mod %>%
  select(
    trans_id
  )

balance_per_month <- left_join(balance_per_month, statement_payment, by = "trans_id")

rm(statement_payment)
head(balance_per_month)
```

    ##   trans_id statement_payment
    ## 1        1              <NA>
    ## 2        5              <NA>
    ## 3        6              <NA>
    ## 4        7              <NA>
    ## 5        8              <NA>
    ## 6        9              <NA>

Test für die Darstellung eines zufälligen Accounts in dem
‘df_transaction_mod’ DF.

``` r
#Zufallsnummer
n = runif(1, min = 0, max = nrow(df_transaction_mod))

df_transaction_mod %>%
  filter(
    account_id == account_id[n]
  ) %>%
  arrange(date)

rm(n)
```

Es gibt Monate, in denen keine Kontogebühren verrechnet wurden. Die
Hypothese dazu ist, dass ein Neukunde die ersten drei Monate keine
Gebühren zahlt. Dies soll überprüft werden.

#### letzter Tag im Monat

last_day_in_month

``` r
last_day_in_month <- df_transaction_mod %>% 
  group_by(
    account_id,
    year,
    month
  ) %>% 
  mutate(last_day = max(date)) %>% 
  ungroup()

head(last_day_in_month)
```

    ## # A tibble: 6 × 14
    ##   trans_id account_id date       cashflow operation             amount balance
    ##      <int>      <int> <date>     <fct>    <fct>                  <dbl>   <dbl>
    ## 1        1          1 1995-03-24 IN       CASH CREDIT             1000   1000 
    ## 2        5          1 1995-04-13 IN       COLLECTION OTHER BANK   3679   4679 
    ## 3        6          1 1995-05-13 IN       COLLECTION OTHER BANK   3679  20977.
    ## 4        7          1 1995-06-13 IN       COLLECTION OTHER BANK   3679  26835.
    ## 5        8          1 1995-07-13 IN       COLLECTION OTHER BANK   3679  30415.
    ## 6        9          1 1995-08-13 IN       COLLECTION OTHER BANK   3679  28903.
    ## # ℹ 7 more variables: characterization <fct>, bank_name <fct>,
    ## #   account_nr <int>, quarter <yearqtr>, year <dbl>, month <dbl>,
    ## #   last_day <date>

``` r
last_day_in_month <- last_day_in_month %>% 
  filter(
    date == last_day
  ) %>% 
  arrange(
    account_id,
    date
  )
last_day_in_month
```

    ## # A tibble: 357,571 × 14
    ##    trans_id account_id date       cashflow operation        amount balance
    ##       <int>      <int> <date>     <fct>    <fct>             <dbl>   <dbl>
    ##  1        1          1 1995-03-24 IN       CASH CREDIT      1000     1000 
    ##  2  3530438          1 1995-04-30 IN       <NA>               19.2  17298.
    ##  3  3530439          1 1995-05-31 IN       <NA>               79    23156.
    ##  4  3530440          1 1995-06-30 IN       <NA>              101.   26736.
    ##  5  3530441          1 1995-07-31 IN       <NA>              109.   25224.
    ##  6      159          1 1995-08-31 OUT      CASH WIDTHDRAWAL   14.6  21487.
    ##  7  3530442          1 1995-08-31 IN       <NA>               99.2  21502.
    ##  8      160          1 1995-09-30 OUT      CASH WIDTHDRAWAL   14.6  22091.
    ##  9  3530443          1 1995-09-30 IN       <NA>               91.5  22106.
    ## 10      161          1 1995-10-31 OUT      CASH WIDTHDRAWAL   14.6  20494.
    ## # ℹ 357,561 more rows
    ## # ℹ 7 more variables: characterization <fct>, bank_name <fct>,
    ## #   account_nr <int>, quarter <yearqtr>, year <dbl>, month <dbl>,
    ## #   last_day <date>

Bei der Kontoeröffnung ist der Amount gleich hoch wie die Balance. Diese
Information kann in “characterization” imputiert werden.

#### Kontoeröffnung

account_opening

``` r
account_opening <- df_transaction_mod %>%
  filter(
    amount == balance,
    cashflow == "IN"
  ) %>%
  mutate(
    account_opening = date
  )

head(account_opening)
```

    ##   trans_id account_id       date cashflow   operation amount balance
    ## 1        1          1 1995-03-24       IN CASH CREDIT   1000    1000
    ## 2      276          2 1993-02-26       IN CASH CREDIT   1100    1100
    ## 3      837          3 1997-07-07       IN CASH CREDIT   1000    1000
    ## 4     1017          4 1996-02-21       IN CASH CREDIT    800     800
    ## 5     1237          5 1997-05-30       IN CASH CREDIT    600     600
    ## 6     1356          6 1994-09-27       IN CASH CREDIT    900     900
    ##   characterization bank_name account_nr quarter year month account_opening
    ## 1             <NA>                   NA 1995 Q1 1995     3      1995-03-24
    ## 2             <NA>                   NA 1993 Q1 1993     2      1993-02-26
    ## 3             <NA>                   NA 1997 Q3 1997     7      1997-07-07
    ## 4             <NA>                   NA 1996 Q1 1996     2      1996-02-21
    ## 5             <NA>                   NA 1997 Q2 1997     5      1997-05-30
    ## 6             <NA>                   NA 1994 Q3 1994     9      1994-09-27

Die Erkenntnisse aus den Kontoeröffnungen werden nun dem Data Frame
“balance_per_month” angefügt.

``` r
foo <- account_opening %>%
  select(
    trans_id,
    account_opening
  )

balance_per_month <- left_join(balance_per_month, foo, by = "trans_id")

rm(foo)
head(balance_per_month)
```

    ##   trans_id statement_payment account_opening
    ## 1        1              <NA>      1995-03-24
    ## 2        5              <NA>            <NA>
    ## 3        6              <NA>            <NA>
    ## 4        7              <NA>            <NA>
    ## 5        8              <NA>            <NA>
    ## 6        9              <NA>            <NA>

``` r
foo <- last_day_in_month %>%
  select(
    trans_id,
    last_day
  )

balance_per_month <- left_join(balance_per_month, foo, by = "trans_id")
rm(foo)
head(balance_per_month)
```

    ##   trans_id statement_payment account_opening   last_day
    ## 1        1              <NA>      1995-03-24 1995-03-24
    ## 2        5              <NA>            <NA>       <NA>
    ## 3        6              <NA>            <NA>       <NA>
    ## 4        7              <NA>            <NA>       <NA>
    ## 5        8              <NA>            <NA>       <NA>
    ## 6        9              <NA>            <NA>       <NA>

``` r
balance_per_month <- left_join(balance_per_month, df_transaction_mod, by = "trans_id")
head(balance_per_month)
```

    ##   trans_id statement_payment account_opening   last_day account_id       date
    ## 1        1              <NA>      1995-03-24 1995-03-24          1 1995-03-24
    ## 2        5              <NA>            <NA>       <NA>          1 1995-04-13
    ## 3        6              <NA>            <NA>       <NA>          1 1995-05-13
    ## 4        7              <NA>            <NA>       <NA>          1 1995-06-13
    ## 5        8              <NA>            <NA>       <NA>          1 1995-07-13
    ## 6        9              <NA>            <NA>       <NA>          1 1995-08-13
    ##   cashflow             operation amount balance characterization bank_name
    ## 1       IN           CASH CREDIT   1000  1000.0             <NA>          
    ## 2       IN COLLECTION OTHER BANK   3679  4679.0             <NA>        AB
    ## 3       IN COLLECTION OTHER BANK   3679 20977.2             <NA>        AB
    ## 4       IN COLLECTION OTHER BANK   3679 26835.2             <NA>        AB
    ## 5       IN COLLECTION OTHER BANK   3679 30414.8             <NA>        AB
    ## 6       IN COLLECTION OTHER BANK   3679 28902.7             <NA>        AB
    ##   account_nr quarter year month
    ## 1         NA 1995 Q1 1995     3
    ## 2   41403269 1995 Q2 1995     4
    ## 3   41403269 1995 Q2 1995     5
    ## 4   41403269 1995 Q2 1995     6
    ## 5   41403269 1995 Q3 1995     7
    ## 6   41403269 1995 Q3 1995     8

auf Duplikate überprüfen

``` r
num <- balance_per_month %>%
  filter(
    !is.na(last_day),
    is.na(statement_payment),
    is.na(account_opening)
    ) %>%
  group_by(
    account_id,
    year,
    month
  ) %>%
  ungroup()

nrow(num)
```

    ## [1] 198722

``` r
n_distinct <- num %>% select(account_id, date) %>% n_distinct()

n_distinct
```

    ## [1] 181902

Es gibt einige Daten, an denen mehrere Zahlungen am selben Tag
erfolgten.

#### Einzelzahlungen am letzten Tag des Monats

Alle Kontostände pro Monatsende mit nur einer Transaktion können
entsprechend ergänzt werden.

balance_per_month

``` r
foo <- balance_per_month %>%
  filter(
    !is.na(last_day),
    is.na(statement_payment),
    is.na(account_opening)
    ) %>%
  group_by(
    account_id,
    year,
    month
  ) %>%
  count() %>%
  ungroup() %>%
  filter(
    n == 1
  ) %>%
  rename(is_unique = n)

balance_per_month <- left_join(balance_per_month, foo, by = c("account_id", "year", "month"))

rm(foo)

head(balance_per_month)
```

    ##   trans_id statement_payment account_opening   last_day account_id       date
    ## 1        1              <NA>      1995-03-24 1995-03-24          1 1995-03-24
    ## 2        5              <NA>            <NA>       <NA>          1 1995-04-13
    ## 3        6              <NA>            <NA>       <NA>          1 1995-05-13
    ## 4        7              <NA>            <NA>       <NA>          1 1995-06-13
    ## 5        8              <NA>            <NA>       <NA>          1 1995-07-13
    ## 6        9              <NA>            <NA>       <NA>          1 1995-08-13
    ##   cashflow             operation amount balance characterization bank_name
    ## 1       IN           CASH CREDIT   1000  1000.0             <NA>          
    ## 2       IN COLLECTION OTHER BANK   3679  4679.0             <NA>        AB
    ## 3       IN COLLECTION OTHER BANK   3679 20977.2             <NA>        AB
    ## 4       IN COLLECTION OTHER BANK   3679 26835.2             <NA>        AB
    ## 5       IN COLLECTION OTHER BANK   3679 30414.8             <NA>        AB
    ## 6       IN COLLECTION OTHER BANK   3679 28902.7             <NA>        AB
    ##   account_nr quarter year month is_unique
    ## 1         NA 1995 Q1 1995     3        NA
    ## 2   41403269 1995 Q2 1995     4         1
    ## 3   41403269 1995 Q2 1995     5         1
    ## 4   41403269 1995 Q2 1995     6         1
    ## 5   41403269 1995 Q3 1995     7         1
    ## 6   41403269 1995 Q3 1995     8         1

#### mehrere Transaktionen am selben Tag

Nun werden alle Zeilen gekennzeichnet, welche mehrere Transaktionen am
Ende des Monats haben und noch nicht zugeordnet werden konnten.

``` r
foo <- balance_per_month %>%
  filter(
    !is.na(last_day),
    is.na(statement_payment),
    is.na(account_opening)
    ) %>%
  group_by(
    account_id,
    year,
    month
  ) %>%
  count() %>%
  ungroup() %>%
  filter(
    n > 1
  ) %>%
  rename(is_not_unique = n)

balance_per_month <- left_join(balance_per_month, foo, by = c("account_id", "year", "month"))

rm(foo)

head(balance_per_month)
```

    ##   trans_id statement_payment account_opening   last_day account_id       date
    ## 1        1              <NA>      1995-03-24 1995-03-24          1 1995-03-24
    ## 2        5              <NA>            <NA>       <NA>          1 1995-04-13
    ## 3        6              <NA>            <NA>       <NA>          1 1995-05-13
    ## 4        7              <NA>            <NA>       <NA>          1 1995-06-13
    ## 5        8              <NA>            <NA>       <NA>          1 1995-07-13
    ## 6        9              <NA>            <NA>       <NA>          1 1995-08-13
    ##   cashflow             operation amount balance characterization bank_name
    ## 1       IN           CASH CREDIT   1000  1000.0             <NA>          
    ## 2       IN COLLECTION OTHER BANK   3679  4679.0             <NA>        AB
    ## 3       IN COLLECTION OTHER BANK   3679 20977.2             <NA>        AB
    ## 4       IN COLLECTION OTHER BANK   3679 26835.2             <NA>        AB
    ## 5       IN COLLECTION OTHER BANK   3679 30414.8             <NA>        AB
    ## 6       IN COLLECTION OTHER BANK   3679 28902.7             <NA>        AB
    ##   account_nr quarter year month is_unique is_not_unique
    ## 1         NA 1995 Q1 1995     3        NA            NA
    ## 2   41403269 1995 Q2 1995     4         1            NA
    ## 3   41403269 1995 Q2 1995     5         1            NA
    ## 4   41403269 1995 Q2 1995     6         1            NA
    ## 5   41403269 1995 Q3 1995     7         1            NA
    ## 6   41403269 1995 Q3 1995     8         1            NA

``` r
balance_per_month %>%
  filter(
    !is.na(is_not_unique),
    !is.na(last_day),
    is.na(statement_payment),
    is.na(account_opening)
  ) %>%
  arrange(
    account_id,
    date
  ) %>%
  arrange(desc(is_not_unique))
```

Alle diese Versuche führen nicht zum Ziel.

Entfernen der nicht mehr benötigten Tabelle

``` r
rm(account_opening, balance_per_month, balance_per_month_total_lag, balance_per_month_total_lead, balance_sheet, last_day_in_month, num, years_avg, n_distinct, n_row_na, transaction_cashflow_per_month_and_year_total)
```

#### Weiterer Versuch, den Kontoendstand pro Monat zu eruieren.

\[^39\] Dazu wird der jeweilige Kontoendstand des Vormonats genommen und
von diesem alle Geldein- sowie Ausgänge dazu gerechnet beziehungsweise
abgezogen. Wir erstellen ein Hilf-Data-Frame für die Geldein- und
Ausgänge, in diesem Date Frame werden nach der zuweisung der Werte
jeglich NA’s zu einem numeric values 0 umformatiert. Nachfolgend wir
eine Balance für jeden Monat erstellt.

``` r
transaction_cashflow_per_month_and_year <- df_transaction_mod %>%
  group_by(
    account_id,
    cashflow,
    year,
    month
  ) %>%
  summarise(amount_sum = sum(amount)) %>%
  # arrange(year, month) %>%
  pivot_wider(names_from = c(cashflow,year,month), values_from = amount_sum)

head(transaction_cashflow_per_month_and_year)
```

    ## # A tibble: 6 × 145
    ## # Groups:   account_id [6]
    ##   account_id IN_1995_3 IN_1995_4 IN_1995_5 IN_1995_6 IN_1995_7 IN_1995_8
    ##        <int>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1          1     1000     16298.     5858      3780.     3788.     3778.
    ## 2          2    20371.    20382.    20358.    30497.    20398.    20366.
    ## 3          3       NA        NA        NA        NA        NA        NA 
    ## 4          4       NA        NA        NA        NA        NA        NA 
    ## 5          5       NA        NA        NA        NA        NA        NA 
    ## 6          6     6776.     6787.     6795.     6784.     6789.     6799 
    ## # ℹ 138 more variables: IN_1995_9 <dbl>, IN_1995_10 <dbl>, IN_1995_11 <dbl>,
    ## #   IN_1995_12 <dbl>, IN_1996_1 <dbl>, IN_1996_2 <dbl>, IN_1996_3 <dbl>,
    ## #   IN_1996_4 <dbl>, IN_1996_5 <dbl>, IN_1996_6 <dbl>, IN_1996_7 <dbl>,
    ## #   IN_1996_8 <dbl>, IN_1996_9 <dbl>, IN_1996_10 <dbl>, IN_1996_11 <dbl>,
    ## #   IN_1996_12 <dbl>, IN_1997_1 <dbl>, IN_1997_2 <dbl>, IN_1997_3 <dbl>,
    ## #   IN_1997_4 <dbl>, IN_1997_5 <dbl>, IN_1997_6 <dbl>, IN_1997_7 <dbl>,
    ## #   IN_1997_8 <dbl>, IN_1997_9 <dbl>, IN_1997_10 <dbl>, IN_1997_11 <dbl>, …

``` r
transaction_cashflow_per_month_and_year_modified <- transaction_cashflow_per_month_and_year %>%
  replace(is.na(transaction_cashflow_per_month_and_year), 0) 

head(transaction_cashflow_per_month_and_year_modified)
```

    ## # A tibble: 6 × 145
    ## # Groups:   account_id [6]
    ##   account_id IN_1995_3 IN_1995_4 IN_1995_5 IN_1995_6 IN_1995_7 IN_1995_8
    ##        <int>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1          1     1000     16298.     5858      3780.     3788.     3778.
    ## 2          2    20371.    20382.    20358.    30497.    20398.    20366.
    ## 3          3        0         0         0         0         0         0 
    ## 4          4        0         0         0         0         0         0 
    ## 5          5        0         0         0         0         0         0 
    ## 6          6     6776.     6787.     6795.     6784.     6789.     6799 
    ## # ℹ 138 more variables: IN_1995_9 <dbl>, IN_1995_10 <dbl>, IN_1995_11 <dbl>,
    ## #   IN_1995_12 <dbl>, IN_1996_1 <dbl>, IN_1996_2 <dbl>, IN_1996_3 <dbl>,
    ## #   IN_1996_4 <dbl>, IN_1996_5 <dbl>, IN_1996_6 <dbl>, IN_1996_7 <dbl>,
    ## #   IN_1996_8 <dbl>, IN_1996_9 <dbl>, IN_1996_10 <dbl>, IN_1996_11 <dbl>,
    ## #   IN_1996_12 <dbl>, IN_1997_1 <dbl>, IN_1997_2 <dbl>, IN_1997_3 <dbl>,
    ## #   IN_1997_4 <dbl>, IN_1997_5 <dbl>, IN_1997_6 <dbl>, IN_1997_7 <dbl>,
    ## #   IN_1997_8 <dbl>, IN_1997_9 <dbl>, IN_1997_10 <dbl>, IN_1997_11 <dbl>, …

``` r
transaction_mod_balance_per_month <- transaction_cashflow_per_month_and_year_modified %>%
  mutate(account_id = account_id) %>%
  mutate(balance_1993_1 = IN_1993_1 - OUT_1993_1) %>%
  mutate(balance_1993_2 = balance_1993_1 + (IN_1993_2 - OUT_1993_2)) %>%
  mutate(balance_1993_3 = balance_1993_2 + (IN_1993_3 - OUT_1993_3)) %>%
  mutate(balance_1993_4 = balance_1993_3 + (IN_1993_4 - OUT_1993_4)) %>%
  mutate(balance_1993_5 = balance_1993_4 + (IN_1993_5 - OUT_1993_5)) %>%
  mutate(balance_1993_6 = balance_1993_5 + (IN_1993_6 - OUT_1993_6)) %>%
  mutate(balance_1993_7 = balance_1993_6 + (IN_1993_7 - OUT_1993_7)) %>%
  mutate(balance_1993_8 = balance_1993_7 + (IN_1993_8 - OUT_1993_8)) %>%
  mutate(balance_1993_9 = balance_1993_8 + (IN_1993_9 - OUT_1993_9)) %>%
  mutate(balance_1993_10 = balance_1993_9 + (IN_1993_10 - OUT_1993_10)) %>%
  mutate(balance_1993_11 = balance_1993_10 + (IN_1993_11 - OUT_1993_11)) %>%
  mutate(balance_1993_12 = balance_1993_11 + (IN_1993_12 - OUT_1993_12)) %>%
  mutate(balance_1994_1 = balance_1993_12 + (IN_1994_1 - OUT_1994_1)) %>%
  mutate(balance_1994_2 = balance_1994_1 + (IN_1994_2 - OUT_1994_2)) %>%
  mutate(balance_1994_3 = balance_1994_2 + (IN_1994_3 - OUT_1994_3)) %>%
  mutate(balance_1994_4 = balance_1994_3 + (IN_1994_4 - OUT_1994_4)) %>%
  mutate(balance_1994_5 = balance_1994_4 + (IN_1994_5 - OUT_1994_5)) %>%
  mutate(balance_1994_6 = balance_1994_5 + (IN_1994_6 - OUT_1994_6)) %>%
  mutate(balance_1994_7 = balance_1994_6 + (IN_1994_7 - OUT_1994_7)) %>%
  mutate(balance_1994_8 = balance_1994_7 + (IN_1994_8 - OUT_1994_8)) %>%
  mutate(balance_1994_9 = balance_1994_8 + (IN_1994_9 - OUT_1994_9)) %>%
  mutate(balance_1994_10 = balance_1994_9 + (IN_1994_10 - OUT_1994_10)) %>%
  mutate(balance_1994_11 = balance_1994_10 + (IN_1994_11 - OUT_1994_11)) %>%
  mutate(balance_1994_12 = balance_1994_11 + (IN_1994_12 - OUT_1994_12)) %>%
  mutate(balance_1995_1 = balance_1994_12 + (IN_1995_1 - OUT_1995_1)) %>%
  mutate(balance_1995_2 = balance_1995_1 + (IN_1995_2 - OUT_1995_2)) %>%
  mutate(balance_1995_3 = balance_1995_2 + (IN_1995_3 - OUT_1995_3)) %>%
  mutate(balance_1995_4 = balance_1995_3 + (IN_1995_4 - OUT_1995_4)) %>%
  mutate(balance_1995_5 = balance_1995_4 + (IN_1995_5 - OUT_1995_5)) %>%
  mutate(balance_1995_6 = balance_1995_5 + (IN_1995_6 - OUT_1995_6)) %>%
  mutate(balance_1995_7 = balance_1995_6 + (IN_1995_7 - OUT_1995_7)) %>%
  mutate(balance_1995_8 = balance_1995_7 + (IN_1995_8 - OUT_1995_8)) %>%
  mutate(balance_1995_9 = balance_1995_8 + (IN_1995_9 - OUT_1995_9)) %>%
  mutate(balance_1995_10 = balance_1995_9 + (IN_1995_10 - OUT_1995_10)) %>%
  mutate(balance_1995_11 = balance_1995_10 + (IN_1995_11 - OUT_1995_11)) %>%
  mutate(balance_1995_12 = balance_1995_11 + (IN_1995_12 - OUT_1995_12)) %>%
  mutate(balance_1996_1 = balance_1995_12 + (IN_1996_1 - OUT_1996_1)) %>%
  mutate(balance_1996_2 = balance_1996_1 + (IN_1996_2 - OUT_1996_2)) %>%
  mutate(balance_1996_3 = balance_1996_2 + (IN_1996_3 - OUT_1996_3)) %>%
  mutate(balance_1996_4 = balance_1996_3 + (IN_1996_4 - OUT_1996_4)) %>%
  mutate(balance_1996_5 = balance_1996_4 + (IN_1996_5 - OUT_1996_5)) %>%
  mutate(balance_1996_6 = balance_1996_5 + (IN_1996_6 - OUT_1996_6)) %>%
  mutate(balance_1996_7 = balance_1996_6 + (IN_1996_7 - OUT_1996_7)) %>%
  mutate(balance_1996_8 = balance_1996_7 + (IN_1996_8 - OUT_1996_8)) %>%
  mutate(balance_1996_9 = balance_1996_8 + (IN_1996_9 - OUT_1996_9)) %>%
  mutate(balance_1996_10 = balance_1996_9 + (IN_1996_10 - OUT_1996_10)) %>%
  mutate(balance_1996_11 = balance_1996_10 + (IN_1996_11 - OUT_1996_11)) %>%
  mutate(balance_1996_12 = balance_1996_11 + (IN_1996_12 - OUT_1996_12)) %>%
  mutate(balance_1997_1 = balance_1996_12 + (IN_1997_1 - OUT_1997_1)) %>%
  mutate(balance_1997_2 = balance_1997_1 + (IN_1997_2 - OUT_1997_2)) %>%
  mutate(balance_1997_3 = balance_1997_2 + (IN_1997_3 - OUT_1997_3)) %>%
  mutate(balance_1997_4 = balance_1997_3 + (IN_1997_4 - OUT_1997_4)) %>%
  mutate(balance_1997_5 = balance_1997_4 + (IN_1997_5 - OUT_1997_5)) %>%
  mutate(balance_1997_6 = balance_1997_5 + (IN_1997_6 - OUT_1997_6)) %>%
  mutate(balance_1997_7 = balance_1997_6 + (IN_1997_7 - OUT_1997_7)) %>%
  mutate(balance_1997_8 = balance_1997_7 + (IN_1997_8 - OUT_1997_8)) %>%
  mutate(balance_1997_9 = balance_1997_8 + (IN_1997_9 - OUT_1997_9)) %>%
  mutate(balance_1997_10 = balance_1997_9 + (IN_1997_10 - OUT_1997_10)) %>%
  mutate(balance_1997_11 = balance_1997_10 + (IN_1997_11 - OUT_1997_11)) %>%
  mutate(balance_1997_12 = balance_1997_11 + (IN_1997_12 - OUT_1997_12)) %>%
  mutate(balance_1998_1 = balance_1997_12 + (IN_1998_1 - OUT_1998_1)) %>%
  mutate(balance_1998_2 = balance_1998_1 + (IN_1998_2 - OUT_1998_2)) %>%
  mutate(balance_1998_3 = balance_1998_2 + (IN_1998_3 - OUT_1998_3)) %>%
  mutate(balance_1998_4 = balance_1998_3 + (IN_1998_4 - OUT_1998_4)) %>%
  mutate(balance_1998_5 = balance_1998_4 + (IN_1998_5 - OUT_1998_5)) %>%
  mutate(balance_1998_6 = balance_1998_5 + (IN_1998_6 - OUT_1998_6)) %>%
  mutate(balance_1998_7 = balance_1998_6 + (IN_1998_7 - OUT_1998_7)) %>%
  mutate(balance_1998_8 = balance_1998_7 + (IN_1998_8 - OUT_1998_8)) %>%
  mutate(balance_1998_9 = balance_1998_8 + (IN_1998_9 - OUT_1998_9)) %>%
  mutate(balance_1998_10 = balance_1998_9 + (IN_1998_10 - OUT_1998_10)) %>%
  mutate(balance_1998_11 = balance_1998_10 + (IN_1998_11 - OUT_1998_11)) %>%
  mutate(balance_1998_12 = balance_1998_11 + (IN_1998_12 - OUT_1998_12)) %>%
  select(account_id, balance_1993_1, balance_1993_2,balance_1993_3, balance_1993_4, balance_1993_5, balance_1993_6, balance_1993_7, balance_1993_8, balance_1993_9, balance_1993_10, balance_1993_11, balance_1993_12, balance_1994_1, balance_1994_2, balance_1994_3, balance_1994_4, balance_1994_5, balance_1994_6, balance_1994_7, balance_1994_8, balance_1994_9, balance_1994_10, balance_1994_11, balance_1994_12, balance_1995_1, balance_1995_2, balance_1995_3, balance_1995_4, balance_1995_5, balance_1995_6, balance_1995_7, balance_1995_8, balance_1995_9, balance_1995_10,balance_1995_11,balance_1995_12, balance_1996_1, balance_1996_2, balance_1996_3, balance_1996_4, balance_1996_5, balance_1996_6, balance_1996_7, balance_1996_8, balance_1996_9, balance_1996_10, balance_1996_11, balance_1996_12, balance_1997_1, balance_1997_2, balance_1997_3, balance_1997_4, balance_1997_5, balance_1997_6, balance_1997_7, balance_1997_8, balance_1997_9, balance_1997_10, balance_1997_11, balance_1997_12,balance_1998_1, balance_1998_2, balance_1998_3, balance_1998_4, balance_1998_5, balance_1998_6, balance_1998_7, balance_1998_8, balance_1998_9, balance_1998_10, balance_1998_11, balance_1998_12)

# str(transaction_cashflow_per_month_and_year)
# 
# 
# df_mod <- left_join(df_mod, transaction_mod_balance_per_month, by = c("account_id" = "account_id"))
```

#### Überprüfen, ob die Überlegung so stimmt

\[^41\] Für den jeweiligen Kontostand Ende Monat wird eine Datumsangabe
ohne Lücken benötigt. Somit kann jederzeit auch den Kontostand am Ende
jeden Tages ausgeben werden. Überprüfen, ob die Datumsangaben lückenlos
sind.

``` r
df_transaction_mod %>% 
  filter(
    year == 1995,
    account_id == 18
    ) %>% 
  group_by(date) %>% 
  count() %>% 
  nrow()
```

    ## [1] 32

Am Beispiel des Accounts Nr. 18 ist klar zu erkennen, dass das Datum
(mit dem dazugehörenden Kontostand) nur erfasst wurde, wenn auch eine
Zahlung getätigt wurde.

Nun wird nochmals dieselbe Kontrolle mit dem Account Nr. 18 gemacht wie
vor der Anreicherung und mit dem Code von Aaron verglichen.

``` r
# Überprüfen, ob der Code von Aaron stimmt:
transaction_mod_balance_per_month %>% 
  filter(
    account_id == 18
  ) %>% 
  select(
    balance_1993_5,
    balance_1993_6,
    balance_1993_7,
    balance_1993_8
  )
```

    ## # A tibble: 1 × 5
    ## # Groups:   account_id [1]
    ##   account_id balance_1993_5 balance_1993_6 balance_1993_7 balance_1993_8
    ##        <int>          <dbl>          <dbl>          <dbl>          <dbl>
    ## 1         18           1100          14600          17200          20700

``` r
df_transaction_mod %>% 
  filter(
    account_id == 18
  ) %>% 
  arrange(date)
```

    ##     trans_id account_id       date cashflow        operation  amount  balance
    ## 1       4152         18 1993-05-26       IN      CASH CREDIT  1100.0   1100.0
    ## 2       4391         18 1993-06-25       IN      CASH CREDIT 13500.0  14600.0
    ## 3       4392         18 1993-07-25       IN      CASH CREDIT  2600.0  17200.0
    ## 4       4393         18 1993-08-24       IN      CASH CREDIT  3500.0  20700.0
    ## 5       4394         18 1993-09-23       IN      CASH CREDIT  2500.0  23200.0
    ## 6       4395         18 1993-10-23       IN      CASH CREDIT   200.0  23400.0
    ## 7       4396         18 1993-12-22      OUT CASH WIDTHDRAWAL  1900.0  21500.0
    ## 8       4155         18 1994-01-17       IN      CASH CREDIT 13402.0  34902.0
    ## 9       4397         18 1994-01-21      OUT CASH WIDTHDRAWAL  6000.0  28902.0
    ## 10   3459193         18 1994-01-31       IN             <NA>    42.7  28944.7
    ## 11      4398         18 1994-02-20      OUT CASH WIDTHDRAWAL  1000.0  27944.7
    ## 12   3459194         18 1994-02-28       IN             <NA>   120.4  28065.1
    ## 13      4173         18 1994-03-21       IN      CASH CREDIT  7706.0  35771.1
    ## 14      4399         18 1994-03-22      OUT CASH WIDTHDRAWAL  6600.0  29171.1
    ## 15      4163         18 1994-03-25       IN      CASH CREDIT 44907.0  74078.1
    ## 16      4171         18 1994-03-28       IN      CASH CREDIT 35017.0 109095.1
    ## 17   3459195         18 1994-03-31       IN             <NA>   181.9 109277.0
    ## 18      4400         18 1994-04-21      OUT CASH WIDTHDRAWAL 41000.0  68277.0
    ## 19   3459196         18 1994-04-30       IN             <NA>   454.6  68731.6
    ## 20      4154         18 1994-05-03       IN      CASH CREDIT 10248.0  78979.6
    ## 21      4161         18 1994-05-10       IN      CASH CREDIT 11859.0  90838.6
    ## 22      4401         18 1994-05-21      OUT CASH WIDTHDRAWAL 33400.0  57438.6
    ## 23      4336         18 1994-05-31      OUT CASH WIDTHDRAWAL    30.0  57759.8
    ## 24   3459197         18 1994-05-31       IN             <NA>   351.2  57789.8
    ## 25      4174         18 1994-06-04       IN      CASH CREDIT 40088.0  97847.8
    ## 26      4232         18 1994-06-05      OUT CASH WIDTHDRAWAL 18365.0  79482.8
    ## 27      4300         18 1994-06-05      OUT CASH WIDTHDRAWAL  7200.0  72282.8
    ## 28      4273         18 1994-06-11      OUT CASH WIDTHDRAWAL 70200.0   2082.8
    ## 29      4402         18 1994-06-20       IN      CASH CREDIT 12100.0  14182.8
    ## 30      4337         18 1994-06-30      OUT CASH WIDTHDRAWAL    30.0  14269.4
    ## 31   3459198         18 1994-06-30       IN             <NA>   116.6  14299.4
    ## 32      4176         18 1994-07-07       IN      CASH CREDIT 34562.0  48831.4
    ## 33      4225         18 1994-07-15      OUT CASH WIDTHDRAWAL 10061.0  38770.4
    ## 34      4403         18 1994-07-20      OUT CASH WIDTHDRAWAL  5100.0  33670.4
    ## 35      4226         18 1994-07-30      OUT CASH WIDTHDRAWAL 10753.0  22917.4
    ## 36      4338         18 1994-07-31      OUT CASH WIDTHDRAWAL    30.0  22991.7
    ## 37   3459199         18 1994-07-31       IN             <NA>   104.3  23021.7
    ## 38      4230         18 1994-08-12      OUT CASH WIDTHDRAWAL 22416.0    575.7
    ## 39      4165         18 1994-08-14       IN      CASH CREDIT 28115.0  28690.7
    ## 40      4162         18 1994-08-17       IN      CASH CREDIT 33035.0  61725.7
    ## 41      4404         18 1994-08-19      OUT CASH WIDTHDRAWAL 14400.0  47325.7
    ## 42      4164         18 1994-08-21       IN      CASH CREDIT  5816.0  53141.7
    ## 43      4167         18 1994-08-28       IN      CASH CREDIT 39105.0  92246.7
    ## 44      4339         18 1994-08-31      OUT CASH WIDTHDRAWAL    30.0  92397.3
    ## 45   3459200         18 1994-08-31       IN             <NA>   180.6  92427.3
    ## 46      4158         18 1994-09-08       IN      CASH CREDIT  9977.0 102374.3
    ## 47      4229         18 1994-09-10      OUT CASH WIDTHDRAWAL 20399.0  81975.3
    ## 48      4405         18 1994-09-18      OUT CASH WIDTHDRAWAL 25900.0  56075.3
    ## 49      4166         18 1994-09-20       IN      CASH CREDIT 18362.0  74437.3
    ## 50      4298         18 1994-09-23      OUT CASH WIDTHDRAWAL  3900.0  70537.3
    ## 51      4159         18 1994-09-26       IN      CASH CREDIT 33153.0 103690.3
    ## 52      4160         18 1994-09-27       IN      CASH CREDIT 42022.0 155603.3
    ## 53      4168         18 1994-09-27       IN      CASH CREDIT  9891.0 113581.3
    ## 54      4406         18 1994-09-27      OUT CASH WIDTHDRAWAL 51300.0 104303.3
    ## 55      4407         18 1994-09-27      OUT CASH WIDTHDRAWAL 51400.0  52903.3
    ## 56      4340         18 1994-09-30      OUT CASH WIDTHDRAWAL    30.0  53199.7
    ## 57   3459201         18 1994-09-30       IN             <NA>   326.3  53229.7
    ## 58      4169         18 1994-10-04       IN      CASH CREDIT 31440.0  84639.7
    ## 59      4175         18 1994-10-05       IN      CASH CREDIT 13242.0  97881.7
    ## 60      4408         18 1994-10-18      OUT CASH WIDTHDRAWAL 33000.0  64881.7
    ## 61      4296         18 1994-10-23      OUT CASH WIDTHDRAWAL  4200.0  60681.7
    ## 62      4156         18 1994-10-24       IN      CASH CREDIT 24534.0  85215.7
    ## 63      4341         18 1994-10-31      OUT CASH WIDTHDRAWAL    30.0  85528.6
    ## 64   3459202         18 1994-10-31       IN             <NA>   343.0  85558.6
    ## 65      4153         18 1994-11-01       IN      CASH CREDIT 39962.0 125490.6
    ## 66      4409         18 1994-11-01      OUT CASH WIDTHDRAWAL 25600.0  99890.6
    ## 67      4157         18 1994-11-09       IN      CASH CREDIT 36880.0 136770.6
    ## 68      4410         18 1994-11-09      OUT CASH WIDTHDRAWAL 51900.0  84870.6
    ## 69      4411         18 1994-11-17      OUT CASH WIDTHDRAWAL 28200.0  56670.6
    ## 70      4301         18 1994-11-20      OUT CASH WIDTHDRAWAL 12600.0  44070.6
    ## 71      4342         18 1994-11-30      OUT CASH WIDTHDRAWAL    30.0  44394.5
    ## 72   3459203         18 1994-11-30       IN             <NA>   353.8  44424.5
    ## 73      4299         18 1994-12-01      OUT CASH WIDTHDRAWAL  5100.0  39294.5
    ## 74      4285         18 1994-12-08      OUT CASH WIDTHDRAWAL  6900.0  32394.5
    ## 75      4412         18 1994-12-17       IN      CASH CREDIT  1700.0  34094.5
    ## 76      4172         18 1994-12-25       IN      CASH CREDIT 47679.0  81773.5
    ## 77      4170         18 1994-12-29       IN      CASH CREDIT 18099.0  99872.5
    ## 78      4343         18 1994-12-31      OUT CASH WIDTHDRAWAL    30.0 100044.1
    ## 79   3459204         18 1994-12-31       IN             <NA>   201.6 100074.1
    ## 80      4413         18 1995-01-16      OUT CASH WIDTHDRAWAL 33600.0  66444.1
    ## 81      4280         18 1995-01-25      OUT CASH WIDTHDRAWAL  7200.0  59244.1
    ## 82      4303         18 1995-01-27      OUT CASH WIDTHDRAWAL  3600.0  55644.1
    ## 83      4344         18 1995-01-31      OUT CASH WIDTHDRAWAL    30.0  55989.8
    ## 84   3459205         18 1995-01-31       IN             <NA>   375.8  56019.8
    ## 85      4414         18 1995-02-15      OUT CASH WIDTHDRAWAL  8000.0  47989.8
    ## 86      4345         18 1995-02-28      OUT CASH WIDTHDRAWAL    30.0  48191.6
    ## 87   3459206         18 1995-02-28       IN             <NA>   231.8  48221.6
    ## 88      4415         18 1995-03-17      OUT CASH WIDTHDRAWAL  5400.0  42791.6
    ## 89      4346         18 1995-03-31      OUT CASH WIDTHDRAWAL    30.0  42961.3
    ## 90   3459207         18 1995-03-31       IN             <NA>   199.7  42991.3
    ## 91      4305         18 1995-04-07      OUT CASH WIDTHDRAWAL 13800.0  29161.3
    ## 92      4416         18 1995-04-16       IN      CASH CREDIT  4600.0  33761.3
    ## 93      4234         18 1995-04-30      OUT CASH WIDTHDRAWAL 23073.0  10844.4
    ## 94      4347         18 1995-04-30      OUT CASH WIDTHDRAWAL    30.0  10814.4
    ## 95   3459208         18 1995-04-30       IN             <NA>   156.2  33917.4
    ## 96      4417         18 1995-05-16       IN      CASH CREDIT 12300.0  23114.4
    ## 97      4348         18 1995-05-31      OUT CASH WIDTHDRAWAL    30.0  23130.6
    ## 98   3459209         18 1995-05-31       IN             <NA>    46.2  23160.6
    ## 99      4235         18 1995-06-10      OUT CASH WIDTHDRAWAL  4335.0  18795.6
    ## 100     4418         18 1995-06-15       IN      CASH CREDIT  8900.0  27695.6
    ## 101     4349         18 1995-06-30      OUT CASH WIDTHDRAWAL    30.0  27771.5
    ## 102  3459210         18 1995-06-30       IN             <NA>   105.9  27801.5
    ## 103     4419         18 1995-07-15       IN      CASH CREDIT  3300.0  31071.5
    ## 104     4350         18 1995-07-31      OUT CASH WIDTHDRAWAL    30.0  31157.0
    ## 105  3459211         18 1995-07-31       IN             <NA>   115.5  31187.0
    ## 106     4302         18 1995-08-04      OUT CASH WIDTHDRAWAL  6600.0  24557.0
    ## 107     4420         18 1995-08-14       IN      CASH CREDIT  5100.0  29657.0
    ## 108     4351         18 1995-08-31      OUT CASH WIDTHDRAWAL    30.0  29752.5
    ## 109  3459212         18 1995-08-31       IN             <NA>   125.5  29782.5
    ## 110     4421         18 1995-09-13       IN      CASH CREDIT  7300.0  37052.5
    ## 111     4352         18 1995-09-30      OUT CASH WIDTHDRAWAL    30.0  37146.7
    ## 112  3459213         18 1995-09-30       IN             <NA>   124.2  37176.7
    ## 113     4422         18 1995-10-13      OUT CASH WIDTHDRAWAL  1000.0  36146.7
    ## 114     4353         18 1995-10-31      OUT CASH WIDTHDRAWAL    30.0  36270.5
    ## 115  3459214         18 1995-10-31       IN             <NA>   153.8  36300.5
    ## 116     4304         18 1995-11-07      OUT CASH WIDTHDRAWAL  6900.0  29370.5
    ## 117     4236         18 1995-11-11      OUT CASH WIDTHDRAWAL 10802.0  18568.5
    ## 118     4423         18 1995-11-12       IN      CASH CREDIT 12500.0  31068.5
    ## 119     4354         18 1995-11-30      OUT CASH WIDTHDRAWAL    30.0  31172.5
    ## 120  3459215         18 1995-11-30       IN             <NA>   134.0  31202.5
    ## 121     4424         18 1995-12-12       IN      CASH CREDIT  6500.0  37672.5
    ## 122     4286         18 1995-12-14      OUT CASH WIDTHDRAWAL  4200.0  33472.5
    ## 123     4355         18 1995-12-31      OUT CASH WIDTHDRAWAL    30.0  33576.9
    ## 124  3459216         18 1995-12-31       IN             <NA>   134.5  33606.9
    ## 125     4183         18 1996-01-02       IN      CASH CREDIT 26914.0  60490.9
    ## 126     4245         18 1996-01-06      OUT CASH WIDTHDRAWAL  3124.0  57366.9
    ## 127     4425         18 1996-01-11      OUT CASH WIDTHDRAWAL  5800.0  51566.9
    ## 128     4281         18 1996-01-25      OUT CASH WIDTHDRAWAL 29400.0  22166.9
    ## 129     4356         18 1996-01-31      OUT CASH WIDTHDRAWAL    30.0  22314.9
    ## 130  3459217         18 1996-01-31       IN             <NA>   177.9  22344.9
    ## 131     4178         18 1996-02-07       IN      CASH CREDIT 28206.0  50520.9
    ## 132     4426         18 1996-02-10      OUT CASH WIDTHDRAWAL  3300.0  47220.9
    ## 133     4243         18 1996-02-12      OUT CASH WIDTHDRAWAL 16729.0  30491.9
    ## 134     4307         18 1996-02-25      OUT CASH WIDTHDRAWAL  5100.0  25391.9
    ## 135     4357         18 1996-02-29      OUT CASH WIDTHDRAWAL    30.0  25478.4
    ## 136  3459218         18 1996-02-29       IN             <NA>   116.5  25508.4
    ## 137     4179         18 1996-03-03       IN      CASH CREDIT 10377.0  35855.4
    ## 138     4427         18 1996-03-11       IN      CASH CREDIT  5700.0  41555.4
    ## 139     4242         18 1996-03-21      OUT CASH WIDTHDRAWAL  4665.0  36890.4
    ## 140     4358         18 1996-03-31      OUT CASH WIDTHDRAWAL    30.0  37003.8
    ## 141  3459219         18 1996-03-31       IN             <NA>   143.4  37033.8
    ## 142     4428         18 1996-04-10       IN      CASH CREDIT  2100.0  39103.8
    ## 143     4359         18 1996-04-30      OUT CASH WIDTHDRAWAL    30.0  39226.9
    ## 144  3459220         18 1996-04-30       IN             <NA>   153.2  39256.9
    ## 145     4240         18 1996-05-01      OUT CASH WIDTHDRAWAL 10631.0  28595.9
    ## 146     4184         18 1996-05-09       IN      CASH CREDIT 31542.0  60137.9
    ## 147     4429         18 1996-05-10      OUT CASH WIDTHDRAWAL  6800.0  53337.9
    ## 148     4241         18 1996-05-17      OUT CASH WIDTHDRAWAL 10934.0  42403.9
    ## 149     4238         18 1996-05-19      OUT CASH WIDTHDRAWAL  2002.0  40401.9
    ## 150     4306         18 1996-05-19      OUT CASH WIDTHDRAWAL 10500.0  29901.9
    ## 151     4309         18 1996-05-26      OUT CASH WIDTHDRAWAL 12300.0  17601.9
    ## 152     4360         18 1996-05-31      OUT CASH WIDTHDRAWAL    30.0  17734.6
    ## 153  3459221         18 1996-05-31       IN             <NA>   162.7  17764.6
    ## 154     4181         18 1996-06-06       IN      CASH CREDIT  5721.0  23455.6
    ## 155     4430         18 1996-06-09       IN      CASH CREDIT 10300.0  33755.6
    ## 156     4177         18 1996-06-11       IN      CASH CREDIT 25379.0  59134.6
    ## 157     4275         18 1996-06-13      OUT CASH WIDTHDRAWAL 46500.0  12634.6
    ## 158     4361         18 1996-06-30      OUT CASH WIDTHDRAWAL    30.0  12680.2
    ## 159  3459222         18 1996-06-30       IN             <NA>    75.6  12710.2
    ## 160     4180         18 1996-07-03       IN      CASH CREDIT 22941.0  35621.2
    ## 161     4431         18 1996-07-09       IN      CASH CREDIT  4700.0  40321.2
    ## 162     4308         18 1996-07-11      OUT CASH WIDTHDRAWAL 11400.0  28921.2
    ## 163     4244         18 1996-07-17      OUT CASH WIDTHDRAWAL 17207.0  11714.2
    ## 164     4310         18 1996-07-23      OUT CASH WIDTHDRAWAL  5100.0   6614.2
    ## 165     4362         18 1996-07-31      OUT CASH WIDTHDRAWAL    30.0   6647.9
    ## 166  3459223         18 1996-07-31       IN             <NA>    63.7   6677.9
    ## 167     4432         18 1996-08-08       IN      CASH CREDIT 20200.0  26847.9
    ## 168     4248         18 1996-08-09      OUT CASH WIDTHDRAWAL  6812.0  20035.9
    ## 169     4363         18 1996-08-31      OUT CASH WIDTHDRAWAL    30.0  20073.3
    ## 170  3459224         18 1996-08-31       IN             <NA>    67.4  20103.3
    ## 171     4433         18 1996-09-07       IN      CASH CREDIT  9400.0  29473.3
    ## 172     4182         18 1996-09-16       IN      CASH CREDIT  5664.0  35137.3
    ## 173     4239         18 1996-09-19      OUT CASH WIDTHDRAWAL  6936.0  28201.3
    ## 174     4364         18 1996-09-30      OUT CASH WIDTHDRAWAL    30.0  28273.0
    ## 175  3459225         18 1996-09-30       IN             <NA>   101.7  28303.0
    ## 176     4434         18 1996-10-07       IN      CASH CREDIT  5600.0  33873.0
    ## 177     4250         18 1996-10-11      OUT CASH WIDTHDRAWAL 16053.0  17820.0
    ## 178     4365         18 1996-10-31      OUT CASH WIDTHDRAWAL    30.0  17876.5
    ## 179  3459226         18 1996-10-31       IN             <NA>    86.5  17906.5
    ## 180     4435         18 1996-11-06       IN      CASH CREDIT 11000.0  28876.5
    ## 181     4249         18 1996-11-15      OUT CASH WIDTHDRAWAL  7163.0  21713.5
    ## 182     4366         18 1996-11-30      OUT CASH WIDTHDRAWAL    30.0  21764.5
    ## 183  3459227         18 1996-11-30       IN             <NA>    81.1  21794.5
    ## 184     4436         18 1996-12-06       IN      CASH CREDIT 11800.0  33564.5
    ## 185     4287         18 1996-12-08      OUT CASH WIDTHDRAWAL  6900.0  26664.5
    ## 186     4367         18 1996-12-31      OUT CASH WIDTHDRAWAL    30.0  26739.0
    ## 187  3459228         18 1996-12-31       IN             <NA>   104.5  26769.0
    ## 188     4194         18 1997-01-05       IN      CASH CREDIT 40814.0  67553.0
    ## 189     4437         18 1997-01-05      OUT CASH WIDTHDRAWAL  9700.0  57853.0
    ## 190     4251         18 1997-01-08      OUT CASH WIDTHDRAWAL 23508.0  34345.0
    ## 191     4259         18 1997-01-11      OUT CASH WIDTHDRAWAL 20589.0  13756.0
    ## 192     4316         18 1997-01-11      OUT CASH WIDTHDRAWAL  2100.0  11656.0
    ## 193     4282         18 1997-01-15      OUT CASH WIDTHDRAWAL  9300.0   2356.0
    ## 194     4368         18 1997-01-31      OUT CASH WIDTHDRAWAL    30.0   2388.2
    ## 195  3459229         18 1997-01-31       IN             <NA>    62.2   2418.2
    ## 196     4438         18 1997-02-04       IN      CASH CREDIT 21400.0  23788.2
    ## 197     4201         18 1997-02-08       IN      CASH CREDIT  8093.0  31881.2
    ## 198     4255         18 1997-02-12      OUT CASH WIDTHDRAWAL 10024.0  21857.2
    ## 199     4369         18 1997-02-28      OUT CASH WIDTHDRAWAL    30.0  21901.7
    ## 200  3459230         18 1997-02-28       IN             <NA>    74.5  21931.7
    ## 201     4262         18 1997-03-04      OUT CASH WIDTHDRAWAL 11668.0  10233.7
    ## 202     4439         18 1997-03-06       IN      CASH CREDIT 16300.0  26533.7
    ## 203     4199         18 1997-03-11       IN      CASH CREDIT 11971.0  38504.7
    ## 204     4313         18 1997-03-15      OUT CASH WIDTHDRAWAL  3600.0  34904.7
    ## 205     4370         18 1997-03-31      OUT CASH WIDTHDRAWAL    30.0  35005.3
    ## 206  3459231         18 1997-03-31       IN             <NA>   130.6  35035.3
    ## 207     4440         18 1997-04-05       IN      CASH CREDIT  4800.0  39805.3
    ## 208     4260         18 1997-04-07      OUT CASH WIDTHDRAWAL  4809.0  34996.3
    ## 209     4254         18 1997-04-08      OUT CASH WIDTHDRAWAL 16005.0  18991.3
    ## 210     4198         18 1997-04-27       IN      CASH CREDIT  6194.0  25185.3
    ## 211     4371         18 1997-04-30      OUT CASH WIDTHDRAWAL    30.0  25251.0
    ## 212  3459232         18 1997-04-30       IN             <NA>    95.7  25281.0
    ## 213     4257         18 1997-05-02      OUT CASH WIDTHDRAWAL 15031.0  10220.0
    ## 214     4441         18 1997-05-05       IN      CASH CREDIT 16200.0  26420.0
    ## 215     4256         18 1997-05-08      OUT CASH WIDTHDRAWAL  7182.0  19238.0
    ## 216     4311         18 1997-05-11      OUT CASH WIDTHDRAWAL  7200.0  12038.0
    ## 217     4372         18 1997-05-31      OUT CASH WIDTHDRAWAL    30.0  12071.3
    ## 218  3459233         18 1997-05-31       IN             <NA>    63.4  12101.3
    ## 219     4203         18 1997-06-04       IN      CASH CREDIT 35942.0  48013.3
    ## 220     4442         18 1997-06-04      OUT CASH WIDTHDRAWAL   800.0  47213.3
    ## 221     4276         18 1997-06-08      OUT CASH WIDTHDRAWAL 37500.0   9713.3
    ## 222     4373         18 1997-06-30      OUT CASH WIDTHDRAWAL    30.0   9742.9
    ## 223  3459234         18 1997-06-30       IN             <NA>    59.6   9772.9
    ## 224     4443         18 1997-07-04       IN      CASH CREDIT 21900.0  31642.9
    ## 225     4314         18 1997-07-12      OUT CASH WIDTHDRAWAL 14100.0  17542.9
    ## 226     4312         18 1997-07-20      OUT CASH WIDTHDRAWAL  8100.0   9442.9
    ## 227     4204         18 1997-07-26       IN      CASH CREDIT 36830.0  46272.9
    ## 228     4374         18 1997-07-31      OUT CASH WIDTHDRAWAL    30.0  46318.3
    ## 229  3459235         18 1997-07-31       IN             <NA>    75.4  46348.3
    ## 230     4444         18 1997-08-03       IN      CASH CREDIT  5300.0  51618.3
    ## 231     4197         18 1997-08-07       IN      CASH CREDIT 10329.0  61947.3
    ## 232     4189         18 1997-08-13       IN      CASH CREDIT 32927.0  94874.3
    ## 233     4253         18 1997-08-14      OUT CASH WIDTHDRAWAL 10910.0  83964.3
    ## 234     4193         18 1997-08-15       IN      CASH CREDIT 43341.0 127305.3
    ## 235     4445         18 1997-08-15      OUT CASH WIDTHDRAWAL 59800.0  67505.3
    ## 236     4375         18 1997-08-31      OUT CASH WIDTHDRAWAL    30.0  67866.7
    ## 237  3459236         18 1997-08-31       IN             <NA>   391.4  67896.7
    ## 238     4185         18 1997-09-02       IN      CASH CREDIT 15276.0  83142.7
    ## 239     4446         18 1997-09-02      OUT CASH WIDTHDRAWAL 15900.0  67242.7
    ## 240     4186         18 1997-09-04       IN      CASH CREDIT  9983.0  77225.7
    ## 241     4258         18 1997-09-10      OUT CASH WIDTHDRAWAL  7024.0  70201.7
    ## 242     4188         18 1997-09-17       IN      CASH CREDIT 30849.0 101050.7
    ## 243     4376         18 1997-09-30      OUT CASH WIDTHDRAWAL    30.0 101379.4
    ## 244  3459237         18 1997-09-30       IN             <NA>   358.7 101409.4
    ## 245     4447         18 1997-10-02      OUT CASH WIDTHDRAWAL 20900.0  80479.4
    ## 246     4191         18 1997-10-04       IN      CASH CREDIT 23233.0 103712.4
    ## 247     4202         18 1997-10-09       IN      CASH CREDIT 47466.0 151178.4
    ## 248     4448         18 1997-10-09      OUT CASH WIDTHDRAWAL 48700.0 102478.4
    ## 249     4196         18 1997-10-29       IN      CASH CREDIT 39930.0 142408.4
    ## 250     4449         18 1997-10-29      OUT CASH WIDTHDRAWAL 45300.0  97108.4
    ## 251     4187         18 1997-10-30       IN      CASH CREDIT 39438.0 136546.4
    ## 252     4450         18 1997-10-30      OUT CASH WIDTHDRAWAL 31900.0 104646.4
    ## 253     4377         18 1997-10-31      OUT CASH WIDTHDRAWAL    30.0 105181.8
    ## 254  3459238         18 1997-10-31       IN             <NA>   565.5 105211.8
    ## 255     4192         18 1997-11-01       IN      CASH CREDIT 38505.0 143686.8
    ## 256     4451         18 1997-11-01      OUT CASH WIDTHDRAWAL 40600.0 103086.8
    ## 257     4452         18 1997-11-01      OUT CASH WIDTHDRAWAL 24700.0  78386.8
    ## 258     4190         18 1997-11-13       IN      CASH CREDIT 23510.0 101896.8
    ## 259     4195         18 1997-11-26       IN      CASH CREDIT 31179.0 133075.8
    ## 260     4453         18 1997-11-26      OUT CASH WIDTHDRAWAL 46900.0  86175.8
    ## 261     4378         18 1997-11-30      OUT CASH WIDTHDRAWAL    30.0  86557.7
    ## 262  3459239         18 1997-11-30       IN             <NA>   411.9  86587.7
    ## 263     4454         18 1997-12-01      OUT CASH WIDTHDRAWAL 13700.0  72857.7
    ## 264     4252         18 1997-12-08      OUT CASH WIDTHDRAWAL  2464.0  70393.7
    ## 265     4288         18 1997-12-12      OUT CASH WIDTHDRAWAL  6000.0  64393.7
    ## 266     4200         18 1997-12-20       IN      CASH CREDIT 23368.0  87761.7
    ## 267     4261         18 1997-12-23      OUT CASH WIDTHDRAWAL 13558.0  74203.7
    ## 268     4379         18 1997-12-31      OUT CASH WIDTHDRAWAL    30.0  69883.9
    ## 269     4455         18 1997-12-31      OUT CASH WIDTHDRAWAL  4600.0  69913.9
    ## 270  3459240         18 1997-12-31       IN             <NA>   310.2  74513.9
    ## 271     4283         18 1998-01-01      OUT CASH WIDTHDRAWAL 15300.0  54583.9
    ## 272     4456         18 1998-01-30       IN      CASH CREDIT  5600.0  60183.9
    ## 273     4380         18 1998-01-31      OUT CASH WIDTHDRAWAL    30.0  60456.8
    ## 274  3459241         18 1998-01-31       IN             <NA>   302.9  60486.8
    ## 275     4317         18 1998-02-07      OUT CASH WIDTHDRAWAL  2700.0  57756.8
    ## 276     4381         18 1998-02-28      OUT CASH WIDTHDRAWAL    30.0  57974.4
    ## 277  3459242         18 1998-02-28       IN             <NA>   247.7  58004.4
    ## 278     4457         18 1998-03-01       IN      CASH CREDIT  4600.0  62574.4
    ## 279     4211         18 1998-03-20       IN      CASH CREDIT 24754.0  87328.4
    ## 280     4265         18 1998-03-25      OUT CASH WIDTHDRAWAL  3200.0  84128.4
    ## 281     4382         18 1998-03-31      OUT CASH WIDTHDRAWAL    30.0  75080.3
    ## 282     4458         18 1998-03-31      OUT CASH WIDTHDRAWAL  9300.0  75110.3
    ## 283  3459243         18 1998-03-31       IN             <NA>   281.9  84410.3
    ## 284     4271         18 1998-04-01      OUT CASH WIDTHDRAWAL  7338.0  67742.3
    ## 285     4270         18 1998-04-09      OUT CASH WIDTHDRAWAL 24439.0  43303.3
    ## 286     4318         18 1998-04-12      OUT CASH WIDTHDRAWAL  5700.0  37603.3
    ## 287     4383         18 1998-04-30      OUT CASH WIDTHDRAWAL    30.0  51382.1
    ## 288     4459         18 1998-04-30       IN      CASH CREDIT 13600.0  51412.1
    ## 289  3459244         18 1998-04-30       IN             <NA>   208.8  37812.1
    ## 290     4269         18 1998-05-15      OUT CASH WIDTHDRAWAL 20698.0  30684.1
    ## 291     4268         18 1998-05-25      OUT CASH WIDTHDRAWAL  2476.0  28208.1
    ## 292     4210         18 1998-05-26       IN      CASH CREDIT 43352.0  71560.1
    ## 293     4460         18 1998-05-30      OUT CASH WIDTHDRAWAL  2200.0  69360.1
    ## 294     4384         18 1998-05-31      OUT CASH WIDTHDRAWAL    30.0  69552.5
    ## 295  3459245         18 1998-05-31       IN             <NA>   222.4  69582.5
    ## 296     4277         18 1998-06-29      OUT CASH WIDTHDRAWAL 10500.0  63952.5
    ## 297     4461         18 1998-06-29       IN      CASH CREDIT  4900.0  74452.5
    ## 298     4385         18 1998-06-30      OUT CASH WIDTHDRAWAL    30.0  64206.3
    ## 299  3459246         18 1998-06-30       IN             <NA>   283.8  64236.3
    ## 300     4263         18 1998-07-24      OUT CASH WIDTHDRAWAL  9030.0  55176.3
    ## 301     4207         18 1998-07-27       IN      CASH CREDIT 41449.0  96625.3
    ## 302     4462         18 1998-07-29      OUT CASH WIDTHDRAWAL 16900.0  79725.3
    ## 303     4386         18 1998-07-31      OUT CASH WIDTHDRAWAL    30.0  79973.2
    ## 304  3459247         18 1998-07-31       IN             <NA>   277.9  80003.2
    ## 305     4209         18 1998-08-03       IN      CASH CREDIT  9956.0  89929.2
    ## 306     4463         18 1998-08-28      OUT CASH WIDTHDRAWAL 14300.0  75629.2
    ## 307     4387         18 1998-08-31      OUT CASH WIDTHDRAWAL    30.0  75925.4
    ## 308  3459248         18 1998-08-31       IN             <NA>   326.2  75955.4
    ## 309     4464         18 1998-09-27      OUT CASH WIDTHDRAWAL  9600.0  66325.4
    ## 310     4388         18 1998-09-30      OUT CASH WIDTHDRAWAL    30.0  66605.5
    ## 311  3459249         18 1998-09-30       IN             <NA>   310.1  66635.5
    ## 312     4212         18 1998-10-22       IN      CASH CREDIT 22395.0  89000.5
    ## 313     4465         18 1998-10-27      OUT CASH WIDTHDRAWAL 18400.0  70600.5
    ## 314     4389         18 1998-10-31      OUT CASH WIDTHDRAWAL    30.0  70845.6
    ## 315  3459250         18 1998-10-31       IN             <NA>   275.1  70875.6
    ## 316     4267         18 1998-11-07      OUT CASH WIDTHDRAWAL  2306.0  68539.6
    ## 317     4264         18 1998-11-11      OUT CASH WIDTHDRAWAL 16426.0  52113.6
    ## 318     4208         18 1998-11-18       IN      CASH CREDIT 24250.0  76363.6
    ## 319     4213         18 1998-11-20       IN      CASH CREDIT 23894.0 100257.6
    ## 320     4466         18 1998-11-26      OUT CASH WIDTHDRAWAL 24300.0  75957.6
    ## 321     4390         18 1998-11-30      OUT CASH WIDTHDRAWAL    30.0  76229.0
    ## 322  3459251         18 1998-11-30       IN             <NA>   301.3  76259.0
    ## 323     4289         18 1998-12-02      OUT CASH WIDTHDRAWAL  2100.0  74129.0
    ## 324     4206         18 1998-12-11       IN      CASH CREDIT 32595.0 106724.0
    ## 325     4266         18 1998-12-16      OUT CASH WIDTHDRAWAL  5475.0 101249.0
    ## 326     4205         18 1998-12-18       IN      CASH CREDIT 42360.0 143609.0
    ## 327     4467         18 1998-12-18      OUT CASH WIDTHDRAWAL 62400.0  81209.0
    ## 328  3459252         18 1998-12-31       IN             <NA>   348.8  81557.7
    ##      characterization bank_name account_nr quarter year month
    ## 1                <NA>                   NA 1993 Q2 1993     5
    ## 2                <NA>                   NA 1993 Q2 1993     6
    ## 3                <NA>                   NA 1993 Q3 1993     7
    ## 4                <NA>                   NA 1993 Q3 1993     8
    ## 5                <NA>                   NA 1993 Q3 1993     9
    ## 6                <NA>                   NA 1993 Q4 1993    10
    ## 7                <NA>                   NA 1993 Q4 1993    12
    ## 8                <NA>                   NA 1994 Q1 1994     1
    ## 9                <NA>                   NA 1994 Q1 1994     1
    ## 10    CREDIT INTEREST                   NA 1994 Q1 1994     1
    ## 11               <NA>                   NA 1994 Q1 1994     2
    ## 12    CREDIT INTEREST                   NA 1994 Q1 1994     2
    ## 13               <NA>                   NA 1994 Q1 1994     3
    ## 14               <NA>                   NA 1994 Q1 1994     3
    ## 15               <NA>                   NA 1994 Q1 1994     3
    ## 16               <NA>                   NA 1994 Q1 1994     3
    ## 17    CREDIT INTEREST                   NA 1994 Q1 1994     3
    ## 18               <NA>                   NA 1994 Q2 1994     4
    ## 19    CREDIT INTEREST                   NA 1994 Q2 1994     4
    ## 20               <NA>                   NA 1994 Q2 1994     5
    ## 21               <NA>                   NA 1994 Q2 1994     5
    ## 22               <NA>                   NA 1994 Q2 1994     5
    ## 23  STATEMENT PAYMENT                   NA 1994 Q2 1994     5
    ## 24    CREDIT INTEREST                   NA 1994 Q2 1994     5
    ## 25               <NA>                   NA 1994 Q2 1994     6
    ## 26               <NA>                   NA 1994 Q2 1994     6
    ## 27               <NA>                   NA 1994 Q2 1994     6
    ## 28               <NA>                   NA 1994 Q2 1994     6
    ## 29               <NA>                   NA 1994 Q2 1994     6
    ## 30  STATEMENT PAYMENT                   NA 1994 Q2 1994     6
    ## 31    CREDIT INTEREST                   NA 1994 Q2 1994     6
    ## 32               <NA>                   NA 1994 Q3 1994     7
    ## 33               <NA>                   NA 1994 Q3 1994     7
    ## 34               <NA>                   NA 1994 Q3 1994     7
    ## 35               <NA>                   NA 1994 Q3 1994     7
    ## 36  STATEMENT PAYMENT                   NA 1994 Q3 1994     7
    ## 37    CREDIT INTEREST                   NA 1994 Q3 1994     7
    ## 38               <NA>                   NA 1994 Q3 1994     8
    ## 39               <NA>                   NA 1994 Q3 1994     8
    ## 40               <NA>                   NA 1994 Q3 1994     8
    ## 41               <NA>                   NA 1994 Q3 1994     8
    ## 42               <NA>                   NA 1994 Q3 1994     8
    ## 43               <NA>                   NA 1994 Q3 1994     8
    ## 44  STATEMENT PAYMENT                   NA 1994 Q3 1994     8
    ## 45    CREDIT INTEREST                   NA 1994 Q3 1994     8
    ## 46               <NA>                   NA 1994 Q3 1994     9
    ## 47               <NA>                   NA 1994 Q3 1994     9
    ## 48               <NA>                   NA 1994 Q3 1994     9
    ## 49               <NA>                   NA 1994 Q3 1994     9
    ## 50               <NA>                   NA 1994 Q3 1994     9
    ## 51               <NA>                   NA 1994 Q3 1994     9
    ## 52               <NA>                   NA 1994 Q3 1994     9
    ## 53               <NA>                   NA 1994 Q3 1994     9
    ## 54               <NA>                   NA 1994 Q3 1994     9
    ## 55               <NA>                   NA 1994 Q3 1994     9
    ## 56  STATEMENT PAYMENT                   NA 1994 Q3 1994     9
    ## 57    CREDIT INTEREST                   NA 1994 Q3 1994     9
    ## 58               <NA>                   NA 1994 Q4 1994    10
    ## 59               <NA>                   NA 1994 Q4 1994    10
    ## 60               <NA>                   NA 1994 Q4 1994    10
    ## 61               <NA>                   NA 1994 Q4 1994    10
    ## 62               <NA>                   NA 1994 Q4 1994    10
    ## 63  STATEMENT PAYMENT                   NA 1994 Q4 1994    10
    ## 64    CREDIT INTEREST                   NA 1994 Q4 1994    10
    ## 65               <NA>                   NA 1994 Q4 1994    11
    ## 66               <NA>                   NA 1994 Q4 1994    11
    ## 67               <NA>                   NA 1994 Q4 1994    11
    ## 68               <NA>                   NA 1994 Q4 1994    11
    ## 69               <NA>                   NA 1994 Q4 1994    11
    ## 70               <NA>                   NA 1994 Q4 1994    11
    ## 71  STATEMENT PAYMENT                   NA 1994 Q4 1994    11
    ## 72    CREDIT INTEREST                   NA 1994 Q4 1994    11
    ## 73               <NA>                   NA 1994 Q4 1994    12
    ## 74               <NA>                   NA 1994 Q4 1994    12
    ## 75               <NA>                   NA 1994 Q4 1994    12
    ## 76               <NA>                   NA 1994 Q4 1994    12
    ## 77               <NA>                   NA 1994 Q4 1994    12
    ## 78  STATEMENT PAYMENT                   NA 1994 Q4 1994    12
    ## 79    CREDIT INTEREST                   NA 1994 Q4 1994    12
    ## 80               <NA>                   NA 1995 Q1 1995     1
    ## 81               <NA>                   NA 1995 Q1 1995     1
    ## 82               <NA>                   NA 1995 Q1 1995     1
    ## 83  STATEMENT PAYMENT                   NA 1995 Q1 1995     1
    ## 84    CREDIT INTEREST                   NA 1995 Q1 1995     1
    ## 85               <NA>                   NA 1995 Q1 1995     2
    ## 86  STATEMENT PAYMENT                   NA 1995 Q1 1995     2
    ## 87    CREDIT INTEREST                   NA 1995 Q1 1995     2
    ## 88               <NA>                   NA 1995 Q1 1995     3
    ## 89  STATEMENT PAYMENT                   NA 1995 Q1 1995     3
    ## 90    CREDIT INTEREST                   NA 1995 Q1 1995     3
    ## 91               <NA>                   NA 1995 Q2 1995     4
    ## 92               <NA>                   NA 1995 Q2 1995     4
    ## 93               <NA>                   NA 1995 Q2 1995     4
    ## 94  STATEMENT PAYMENT                   NA 1995 Q2 1995     4
    ## 95    CREDIT INTEREST                   NA 1995 Q2 1995     4
    ## 96               <NA>                   NA 1995 Q2 1995     5
    ## 97  STATEMENT PAYMENT                   NA 1995 Q2 1995     5
    ## 98    CREDIT INTEREST                   NA 1995 Q2 1995     5
    ## 99               <NA>                   NA 1995 Q2 1995     6
    ## 100              <NA>                   NA 1995 Q2 1995     6
    ## 101 STATEMENT PAYMENT                   NA 1995 Q2 1995     6
    ## 102   CREDIT INTEREST                   NA 1995 Q2 1995     6
    ## 103              <NA>                   NA 1995 Q3 1995     7
    ## 104 STATEMENT PAYMENT                   NA 1995 Q3 1995     7
    ## 105   CREDIT INTEREST                   NA 1995 Q3 1995     7
    ## 106              <NA>                   NA 1995 Q3 1995     8
    ## 107              <NA>                   NA 1995 Q3 1995     8
    ## 108 STATEMENT PAYMENT                   NA 1995 Q3 1995     8
    ## 109   CREDIT INTEREST                   NA 1995 Q3 1995     8
    ## 110              <NA>                   NA 1995 Q3 1995     9
    ## 111 STATEMENT PAYMENT                   NA 1995 Q3 1995     9
    ## 112   CREDIT INTEREST                   NA 1995 Q3 1995     9
    ## 113              <NA>                   NA 1995 Q4 1995    10
    ## 114 STATEMENT PAYMENT                   NA 1995 Q4 1995    10
    ## 115   CREDIT INTEREST                   NA 1995 Q4 1995    10
    ## 116              <NA>                   NA 1995 Q4 1995    11
    ## 117              <NA>                   NA 1995 Q4 1995    11
    ## 118              <NA>                   NA 1995 Q4 1995    11
    ## 119 STATEMENT PAYMENT                   NA 1995 Q4 1995    11
    ## 120   CREDIT INTEREST                   NA 1995 Q4 1995    11
    ## 121              <NA>                   NA 1995 Q4 1995    12
    ## 122              <NA>                   NA 1995 Q4 1995    12
    ## 123 STATEMENT PAYMENT                   NA 1995 Q4 1995    12
    ## 124   CREDIT INTEREST                   NA 1995 Q4 1995    12
    ## 125              <NA>                   NA 1996 Q1 1996     1
    ## 126              <NA>                   NA 1996 Q1 1996     1
    ## 127              <NA>                   NA 1996 Q1 1996     1
    ## 128              <NA>                   NA 1996 Q1 1996     1
    ## 129 STATEMENT PAYMENT                   NA 1996 Q1 1996     1
    ## 130   CREDIT INTEREST                   NA 1996 Q1 1996     1
    ## 131              <NA>                   NA 1996 Q1 1996     2
    ## 132              <NA>                   NA 1996 Q1 1996     2
    ## 133              <NA>                   NA 1996 Q1 1996     2
    ## 134              <NA>                   NA 1996 Q1 1996     2
    ## 135 STATEMENT PAYMENT                   NA 1996 Q1 1996     2
    ## 136   CREDIT INTEREST                   NA 1996 Q1 1996     2
    ## 137              <NA>                   NA 1996 Q1 1996     3
    ## 138              <NA>                   NA 1996 Q1 1996     3
    ## 139              <NA>                   NA 1996 Q1 1996     3
    ## 140 STATEMENT PAYMENT                   NA 1996 Q1 1996     3
    ## 141   CREDIT INTEREST                   NA 1996 Q1 1996     3
    ## 142              <NA>                   NA 1996 Q2 1996     4
    ## 143 STATEMENT PAYMENT                   NA 1996 Q2 1996     4
    ## 144   CREDIT INTEREST                   NA 1996 Q2 1996     4
    ## 145              <NA>                   NA 1996 Q2 1996     5
    ## 146              <NA>                   NA 1996 Q2 1996     5
    ## 147              <NA>                   NA 1996 Q2 1996     5
    ## 148              <NA>                   NA 1996 Q2 1996     5
    ## 149              <NA>                   NA 1996 Q2 1996     5
    ## 150              <NA>                   NA 1996 Q2 1996     5
    ## 151              <NA>                   NA 1996 Q2 1996     5
    ## 152 STATEMENT PAYMENT                   NA 1996 Q2 1996     5
    ## 153   CREDIT INTEREST                   NA 1996 Q2 1996     5
    ## 154              <NA>                   NA 1996 Q2 1996     6
    ## 155              <NA>                   NA 1996 Q2 1996     6
    ## 156              <NA>                   NA 1996 Q2 1996     6
    ## 157              <NA>                   NA 1996 Q2 1996     6
    ## 158 STATEMENT PAYMENT                   NA 1996 Q2 1996     6
    ## 159   CREDIT INTEREST                   NA 1996 Q2 1996     6
    ## 160              <NA>                   NA 1996 Q3 1996     7
    ## 161              <NA>                   NA 1996 Q3 1996     7
    ## 162              <NA>                   NA 1996 Q3 1996     7
    ## 163              <NA>                   NA 1996 Q3 1996     7
    ## 164              <NA>                   NA 1996 Q3 1996     7
    ## 165 STATEMENT PAYMENT                   NA 1996 Q3 1996     7
    ## 166   CREDIT INTEREST                   NA 1996 Q3 1996     7
    ## 167              <NA>                   NA 1996 Q3 1996     8
    ## 168              <NA>                   NA 1996 Q3 1996     8
    ## 169 STATEMENT PAYMENT                   NA 1996 Q3 1996     8
    ## 170   CREDIT INTEREST                   NA 1996 Q3 1996     8
    ## 171              <NA>                   NA 1996 Q3 1996     9
    ## 172              <NA>                   NA 1996 Q3 1996     9
    ## 173              <NA>                   NA 1996 Q3 1996     9
    ## 174 STATEMENT PAYMENT                   NA 1996 Q3 1996     9
    ## 175   CREDIT INTEREST                   NA 1996 Q3 1996     9
    ## 176              <NA>                   NA 1996 Q4 1996    10
    ## 177              <NA>                   NA 1996 Q4 1996    10
    ## 178 STATEMENT PAYMENT                   NA 1996 Q4 1996    10
    ## 179   CREDIT INTEREST                   NA 1996 Q4 1996    10
    ## 180              <NA>                   NA 1996 Q4 1996    11
    ## 181              <NA>                   NA 1996 Q4 1996    11
    ## 182 STATEMENT PAYMENT                   NA 1996 Q4 1996    11
    ## 183   CREDIT INTEREST                   NA 1996 Q4 1996    11
    ## 184              <NA>                   NA 1996 Q4 1996    12
    ## 185              <NA>                   NA 1996 Q4 1996    12
    ## 186 STATEMENT PAYMENT                   NA 1996 Q4 1996    12
    ## 187   CREDIT INTEREST                   NA 1996 Q4 1996    12
    ## 188              <NA>                   NA 1997 Q1 1997     1
    ## 189              <NA>                   NA 1997 Q1 1997     1
    ## 190              <NA>                   NA 1997 Q1 1997     1
    ## 191              <NA>                   NA 1997 Q1 1997     1
    ## 192              <NA>                   NA 1997 Q1 1997     1
    ## 193              <NA>                   NA 1997 Q1 1997     1
    ## 194 STATEMENT PAYMENT                   NA 1997 Q1 1997     1
    ## 195   CREDIT INTEREST                   NA 1997 Q1 1997     1
    ## 196              <NA>                   NA 1997 Q1 1997     2
    ## 197              <NA>                   NA 1997 Q1 1997     2
    ## 198              <NA>                   NA 1997 Q1 1997     2
    ## 199 STATEMENT PAYMENT                   NA 1997 Q1 1997     2
    ## 200   CREDIT INTEREST                   NA 1997 Q1 1997     2
    ## 201              <NA>                   NA 1997 Q1 1997     3
    ## 202              <NA>                   NA 1997 Q1 1997     3
    ## 203              <NA>                   NA 1997 Q1 1997     3
    ## 204              <NA>                   NA 1997 Q1 1997     3
    ## 205 STATEMENT PAYMENT                   NA 1997 Q1 1997     3
    ## 206   CREDIT INTEREST                   NA 1997 Q1 1997     3
    ## 207              <NA>                   NA 1997 Q2 1997     4
    ## 208              <NA>                   NA 1997 Q2 1997     4
    ## 209              <NA>                   NA 1997 Q2 1997     4
    ## 210              <NA>                   NA 1997 Q2 1997     4
    ## 211 STATEMENT PAYMENT                   NA 1997 Q2 1997     4
    ## 212   CREDIT INTEREST                   NA 1997 Q2 1997     4
    ## 213              <NA>                   NA 1997 Q2 1997     5
    ## 214              <NA>                   NA 1997 Q2 1997     5
    ## 215              <NA>                   NA 1997 Q2 1997     5
    ## 216              <NA>                   NA 1997 Q2 1997     5
    ## 217 STATEMENT PAYMENT                   NA 1997 Q2 1997     5
    ## 218   CREDIT INTEREST                   NA 1997 Q2 1997     5
    ## 219              <NA>                   NA 1997 Q2 1997     6
    ## 220              <NA>                   NA 1997 Q2 1997     6
    ## 221              <NA>                   NA 1997 Q2 1997     6
    ## 222 STATEMENT PAYMENT                   NA 1997 Q2 1997     6
    ## 223   CREDIT INTEREST                   NA 1997 Q2 1997     6
    ## 224              <NA>                   NA 1997 Q3 1997     7
    ## 225              <NA>                   NA 1997 Q3 1997     7
    ## 226              <NA>                   NA 1997 Q3 1997     7
    ## 227              <NA>                   NA 1997 Q3 1997     7
    ## 228 STATEMENT PAYMENT                   NA 1997 Q3 1997     7
    ## 229   CREDIT INTEREST                   NA 1997 Q3 1997     7
    ## 230              <NA>                   NA 1997 Q3 1997     8
    ## 231              <NA>                   NA 1997 Q3 1997     8
    ## 232              <NA>                   NA 1997 Q3 1997     8
    ## 233              <NA>                   NA 1997 Q3 1997     8
    ## 234              <NA>                   NA 1997 Q3 1997     8
    ## 235              <NA>                   NA 1997 Q3 1997     8
    ## 236 STATEMENT PAYMENT                   NA 1997 Q3 1997     8
    ## 237   CREDIT INTEREST                   NA 1997 Q3 1997     8
    ## 238              <NA>                   NA 1997 Q3 1997     9
    ## 239              <NA>                   NA 1997 Q3 1997     9
    ## 240              <NA>                   NA 1997 Q3 1997     9
    ## 241              <NA>                   NA 1997 Q3 1997     9
    ## 242              <NA>                   NA 1997 Q3 1997     9
    ## 243 STATEMENT PAYMENT                   NA 1997 Q3 1997     9
    ## 244   CREDIT INTEREST                   NA 1997 Q3 1997     9
    ## 245              <NA>                   NA 1997 Q4 1997    10
    ## 246              <NA>                   NA 1997 Q4 1997    10
    ## 247              <NA>                   NA 1997 Q4 1997    10
    ## 248              <NA>                   NA 1997 Q4 1997    10
    ## 249              <NA>                   NA 1997 Q4 1997    10
    ## 250              <NA>                   NA 1997 Q4 1997    10
    ## 251              <NA>                   NA 1997 Q4 1997    10
    ## 252              <NA>                   NA 1997 Q4 1997    10
    ## 253 STATEMENT PAYMENT                   NA 1997 Q4 1997    10
    ## 254   CREDIT INTEREST                   NA 1997 Q4 1997    10
    ## 255              <NA>                   NA 1997 Q4 1997    11
    ## 256              <NA>                   NA 1997 Q4 1997    11
    ## 257              <NA>                   NA 1997 Q4 1997    11
    ## 258              <NA>                   NA 1997 Q4 1997    11
    ## 259              <NA>                   NA 1997 Q4 1997    11
    ## 260              <NA>                   NA 1997 Q4 1997    11
    ## 261 STATEMENT PAYMENT                   NA 1997 Q4 1997    11
    ## 262   CREDIT INTEREST                   NA 1997 Q4 1997    11
    ## 263              <NA>                   NA 1997 Q4 1997    12
    ## 264              <NA>                   NA 1997 Q4 1997    12
    ## 265              <NA>                   NA 1997 Q4 1997    12
    ## 266              <NA>                   NA 1997 Q4 1997    12
    ## 267              <NA>                   NA 1997 Q4 1997    12
    ## 268 STATEMENT PAYMENT                   NA 1997 Q4 1997    12
    ## 269              <NA>                   NA 1997 Q4 1997    12
    ## 270   CREDIT INTEREST                   NA 1997 Q4 1997    12
    ## 271              <NA>                   NA 1998 Q1 1998     1
    ## 272              <NA>                   NA 1998 Q1 1998     1
    ## 273 STATEMENT PAYMENT                   NA 1998 Q1 1998     1
    ## 274   CREDIT INTEREST                   NA 1998 Q1 1998     1
    ## 275              <NA>                   NA 1998 Q1 1998     2
    ## 276 STATEMENT PAYMENT                   NA 1998 Q1 1998     2
    ## 277   CREDIT INTEREST                   NA 1998 Q1 1998     2
    ## 278              <NA>                   NA 1998 Q1 1998     3
    ## 279              <NA>                   NA 1998 Q1 1998     3
    ## 280              <NA>                   NA 1998 Q1 1998     3
    ## 281 STATEMENT PAYMENT                   NA 1998 Q1 1998     3
    ## 282              <NA>                   NA 1998 Q1 1998     3
    ## 283   CREDIT INTEREST                   NA 1998 Q1 1998     3
    ## 284              <NA>                   NA 1998 Q2 1998     4
    ## 285              <NA>                   NA 1998 Q2 1998     4
    ## 286              <NA>                   NA 1998 Q2 1998     4
    ## 287 STATEMENT PAYMENT                   NA 1998 Q2 1998     4
    ## 288              <NA>                   NA 1998 Q2 1998     4
    ## 289   CREDIT INTEREST                   NA 1998 Q2 1998     4
    ## 290              <NA>                   NA 1998 Q2 1998     5
    ## 291              <NA>                   NA 1998 Q2 1998     5
    ## 292              <NA>                   NA 1998 Q2 1998     5
    ## 293              <NA>                   NA 1998 Q2 1998     5
    ## 294 STATEMENT PAYMENT                   NA 1998 Q2 1998     5
    ## 295   CREDIT INTEREST                   NA 1998 Q2 1998     5
    ## 296              <NA>                   NA 1998 Q2 1998     6
    ## 297              <NA>                   NA 1998 Q2 1998     6
    ## 298 STATEMENT PAYMENT                   NA 1998 Q2 1998     6
    ## 299   CREDIT INTEREST                   NA 1998 Q2 1998     6
    ## 300              <NA>                   NA 1998 Q3 1998     7
    ## 301              <NA>                   NA 1998 Q3 1998     7
    ## 302              <NA>                   NA 1998 Q3 1998     7
    ## 303 STATEMENT PAYMENT                   NA 1998 Q3 1998     7
    ## 304   CREDIT INTEREST                   NA 1998 Q3 1998     7
    ## 305              <NA>                   NA 1998 Q3 1998     8
    ## 306              <NA>                   NA 1998 Q3 1998     8
    ## 307 STATEMENT PAYMENT                   NA 1998 Q3 1998     8
    ## 308   CREDIT INTEREST                   NA 1998 Q3 1998     8
    ## 309              <NA>                   NA 1998 Q3 1998     9
    ## 310 STATEMENT PAYMENT                   NA 1998 Q3 1998     9
    ## 311   CREDIT INTEREST                   NA 1998 Q3 1998     9
    ## 312              <NA>                   NA 1998 Q4 1998    10
    ## 313              <NA>                   NA 1998 Q4 1998    10
    ## 314 STATEMENT PAYMENT                   NA 1998 Q4 1998    10
    ## 315   CREDIT INTEREST                   NA 1998 Q4 1998    10
    ## 316              <NA>                   NA 1998 Q4 1998    11
    ## 317              <NA>                   NA 1998 Q4 1998    11
    ## 318              <NA>                   NA 1998 Q4 1998    11
    ## 319              <NA>                   NA 1998 Q4 1998    11
    ## 320              <NA>                   NA 1998 Q4 1998    11
    ## 321 STATEMENT PAYMENT                   NA 1998 Q4 1998    11
    ## 322   CREDIT INTEREST                   NA 1998 Q4 1998    11
    ## 323              <NA>                   NA 1998 Q4 1998    12
    ## 324              <NA>                   NA 1998 Q4 1998    12
    ## 325              <NA>                   NA 1998 Q4 1998    12
    ## 326              <NA>                   NA 1998 Q4 1998    12
    ## 327              <NA>                   NA 1998 Q4 1998    12
    ## 328   CREDIT INTEREST                   NA 1998 Q4 1998    12

Das Resultat des Codes sieht korrekt aus. Es wird versucht, den Code
selber umzuschreiben um dies später im Rahmen von grossen Daten anwenden
zu können.

Entfernen der nicht mehr benötigten Tabellen

``` r
rm(transaction_cashflow_per_month_and_year, transaction_mod_balance_per_month, transaction_cashflow_per_month_and_year_modified)
```

#### Vierteljähriges Einkommen und Ausgaben per Jahr

``` r
transaction_cashflow_per_quarter_and_year <- df_transaction_mod %>%
  group_by(
    account_id,
    cashflow,
    quarter
  ) %>%
  summarise(amount_sum = sum(amount)) %>%
  # arrange(year, month) %>%
  pivot_wider(names_from = c(cashflow, quarter), values_from = amount_sum)

str(transaction_cashflow_per_quarter_and_year)
```

    ## gropd_df [4,500 × 49] (S3: grouped_df/tbl_df/tbl/data.frame)
    ##  $ account_id : int [1:4500] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ IN_1995 Q1 : num [1:4500] 1000 61771 NA NA NA ...
    ##  $ IN_1995 Q2 : num [1:4500] 25936 71238 NA NA NA ...
    ##  $ IN_1995 Q3 : num [1:4500] 11337 61109 NA NA NA ...
    ##  $ IN_1995 Q4 : num [1:4500] 11292 71244 NA NA NA ...
    ##  $ IN_1996 Q1 : num [1:4500] 11527 67130 NA 6353 NA ...
    ##  $ IN_1996 Q2 : num [1:4500] 12633 85484 NA 16659 NA ...
    ##  $ IN_1996 Q3 : num [1:4500] 14664 61236 NA 16930 NA ...
    ##  $ IN_1996 Q4 : num [1:4500] 11242 74693 NA 16979 NA ...
    ##  $ IN_1997 Q1 : num [1:4500] 14103 67288 NA 16899 NA ...
    ##  $ IN_1997 Q2 : num [1:4500] 11196 71410 NA 16910 5617 ...
    ##  $ IN_1997 Q3 : num [1:4500] 12733 71454 31522 16910 15051 ...
    ##  $ IN_1997 Q4 : num [1:4500] 11243 71425 26617 16926 15283 ...
    ##  $ IN_1998 Q1 : num [1:4500] 11712 62443 28529 16855 15357 ...
    ##  $ IN_1998 Q2 : num [1:4500] 11235 73126 27567 16913 15393 ...
    ##  $ IN_1998 Q3 : num [1:4500] 11248 61314 26313 16968 15382 ...
    ##  $ IN_1998 Q4 : num [1:4500] 11220 71350 32510 17044 15402 ...
    ##  $ OUT_1995 Q2: num [1:4500] 200 68258 NA NA NA ...
    ##  $ OUT_1995 Q3: num [1:4500] 15981 68340 NA NA NA ...
    ##  $ OUT_1995 Q4: num [1:4500] 15210 61196 NA NA NA ...
    ##  $ OUT_1996 Q1: num [1:4500] 13270 67842 NA NA NA ...
    ##  $ OUT_1996 Q2: num [1:4500] 12320 72164 NA NA NA ...
    ##  $ OUT_1996 Q3: num [1:4500] 12640 80627 NA 17355 NA ...
    ##  $ OUT_1996 Q4: num [1:4500] 17350 53942 NA 14623 NA ...
    ##  $ OUT_1997 Q1: num [1:4500] 15510 75466 NA 23123 NA ...
    ##  $ OUT_1997 Q2: num [1:4500] 8570 89178 NA 17703 NA ...
    ##  $ OUT_1997 Q3: num [1:4500] 9860 46808 2400 15033 NA ...
    ##  $ OUT_1997 Q4: num [1:4500] 13200 69642 28990 16853 8390 ...
    ##  $ OUT_1998 Q1: num [1:4500] 11100 87261 31967 19033 16768 ...
    ##  $ OUT_1998 Q2: num [1:4500] 9080 53642 23367 12213 13448 ...
    ##  $ OUT_1998 Q3: num [1:4500] 14170 65840 15247 11973 18588 ...
    ##  $ OUT_1998 Q4: num [1:4500] 12395 72737 19992 10718 12203 ...
    ##  $ IN_1993 Q1 : num [1:4500] NA 25050 NA NA NA ...
    ##  $ IN_1993 Q2 : num [1:4500] NA 71240 NA NA NA ...
    ##  $ IN_1993 Q3 : num [1:4500] NA 61191 NA NA NA ...
    ##  $ IN_1993 Q4 : num [1:4500] NA 71204 NA NA NA ...
    ##  $ IN_1994 Q1 : num [1:4500] NA 61061 NA NA NA ...
    ##  $ IN_1994 Q2 : num [1:4500] NA 71238 NA NA NA ...
    ##  $ IN_1994 Q3 : num [1:4500] NA 61111 NA NA NA ...
    ##  $ IN_1994 Q4 : num [1:4500] NA 71243 NA NA NA ...
    ##  $ OUT_1993 Q2: num [1:4500] NA 51000 NA NA NA ...
    ##  $ OUT_1993 Q3: num [1:4500] NA 84707 NA NA NA ...
    ##  $ OUT_1993 Q4: num [1:4500] NA 65138 NA NA NA ...
    ##  $ OUT_1994 Q1: num [1:4500] NA 57860 NA NA NA ...
    ##  $ OUT_1994 Q2: num [1:4500] NA 72960 NA NA NA ...
    ##  $ OUT_1994 Q3: num [1:4500] NA 64701 NA NA NA ...
    ##  $ OUT_1994 Q4: num [1:4500] NA 64060 NA NA NA ...
    ##  $ OUT_1995 Q1: num [1:4500] NA 61060 NA NA NA ...
    ##  $ OUT_1993 Q1: num [1:4500] NA NA NA NA NA NA NA NA NA NA ...
    ##  - attr(*, "groups")= tibble [4,500 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ account_id: int [1:4500] 1 2 3 4 5 6 7 8 9 10 ...
    ##   ..$ .rows     : list<int> [1:4500] 
    ##   .. ..$ : int 1
    ##   .. ..$ : int 2
    ##   .. ..$ : int 3
    ##   .. ..$ : int 4
    ##   .. ..$ : int 5
    ##   .. ..$ : int 6
    ##   .. ..$ : int 7
    ##   .. ..$ : int 8
    ##   .. ..$ : int 9
    ##   .. ..$ : int 10
    ##   .. ..$ : int 11
    ##   .. ..$ : int 12
    ##   .. ..$ : int 13
    ##   .. ..$ : int 14
    ##   .. ..$ : int 15
    ##   .. ..$ : int 16
    ##   .. ..$ : int 17
    ##   .. ..$ : int 18
    ##   .. ..$ : int 19
    ##   .. ..$ : int 20
    ##   .. ..$ : int 21
    ##   .. ..$ : int 22
    ##   .. ..$ : int 23
    ##   .. ..$ : int 24
    ##   .. ..$ : int 25
    ##   .. ..$ : int 26
    ##   .. ..$ : int 27
    ##   .. ..$ : int 28
    ##   .. ..$ : int 29
    ##   .. ..$ : int 30
    ##   .. ..$ : int 31
    ##   .. ..$ : int 32
    ##   .. ..$ : int 33
    ##   .. ..$ : int 34
    ##   .. ..$ : int 35
    ##   .. ..$ : int 36
    ##   .. ..$ : int 37
    ##   .. ..$ : int 38
    ##   .. ..$ : int 39
    ##   .. ..$ : int 40
    ##   .. ..$ : int 41
    ##   .. ..$ : int 42
    ##   .. ..$ : int 43
    ##   .. ..$ : int 44
    ##   .. ..$ : int 45
    ##   .. ..$ : int 46
    ##   .. ..$ : int 47
    ##   .. ..$ : int 48
    ##   .. ..$ : int 49
    ##   .. ..$ : int 50
    ##   .. ..$ : int 51
    ##   .. ..$ : int 52
    ##   .. ..$ : int 53
    ##   .. ..$ : int 54
    ##   .. ..$ : int 55
    ##   .. ..$ : int 56
    ##   .. ..$ : int 57
    ##   .. ..$ : int 58
    ##   .. ..$ : int 59
    ##   .. ..$ : int 60
    ##   .. ..$ : int 61
    ##   .. ..$ : int 62
    ##   .. ..$ : int 63
    ##   .. ..$ : int 64
    ##   .. ..$ : int 65
    ##   .. ..$ : int 66
    ##   .. ..$ : int 67
    ##   .. ..$ : int 68
    ##   .. ..$ : int 69
    ##   .. ..$ : int 70
    ##   .. ..$ : int 71
    ##   .. ..$ : int 72
    ##   .. ..$ : int 73
    ##   .. ..$ : int 74
    ##   .. ..$ : int 75
    ##   .. ..$ : int 76
    ##   .. ..$ : int 77
    ##   .. ..$ : int 78
    ##   .. ..$ : int 79
    ##   .. ..$ : int 80
    ##   .. ..$ : int 81
    ##   .. ..$ : int 82
    ##   .. ..$ : int 83
    ##   .. ..$ : int 84
    ##   .. ..$ : int 85
    ##   .. ..$ : int 86
    ##   .. ..$ : int 87
    ##   .. ..$ : int 88
    ##   .. ..$ : int 89
    ##   .. ..$ : int 90
    ##   .. ..$ : int 91
    ##   .. ..$ : int 92
    ##   .. ..$ : int 93
    ##   .. ..$ : int 94
    ##   .. ..$ : int 95
    ##   .. ..$ : int 96
    ##   .. ..$ : int 97
    ##   .. ..$ : int 98
    ##   .. ..$ : int 99
    ##   .. .. [list output truncated]
    ##   .. ..@ ptype: int(0) 
    ##   ..- attr(*, ".drop")= logi TRUE

``` r
transaction_cashflow_per_quarter_and_year_modified <- transaction_cashflow_per_quarter_and_year %>%
  replace(is.na(transaction_cashflow_per_quarter_and_year), 0) 

transaction_cashflow_per_quarter_and_year_modified %>%
  mutate(account_id = account_id) %>%
  mutate('1993_Q1_Expenses' = 'OUT_1993 Q1') %>%
  mutate('1993_Q2_Expenses' = 'OUT_1993 Q2') %>%
  mutate('1993_Q3_Expenses' = 'OUT_1993 Q3') %>%
  mutate('1993_Q4_Expenses' = 'OUT_1993 Q4') %>%
  mutate('1994_Q1_Expenses' = 'OUT_1994 Q1') %>%
  mutate('1994_Q2_Expenses' = 'OUT_1994 Q2') %>%
  mutate('1994_Q3_Expenses' = 'OUT_1994 Q3') %>%
  mutate('1994_Q4_Expenses' = 'OUT_1994 Q4') %>%
  mutate('1995_Q1_Expenses' = 'OUT_1995 Q1') %>%
  mutate('1995_Q2_Expenses' = 'OUT_1995 Q2') %>%
  mutate('1995_Q3_Expenses' = 'OUT_1995 Q3') %>%
  mutate('1995_Q4_Expenses' = 'OUT_1995 Q4') %>%
  mutate('1996_Q1_Expenses' = 'OUT_1996 Q1') %>%
  mutate('1996_Q2_Expenses' = 'OUT_1996 Q2') %>%
  mutate('1996_Q3_Expenses' = 'OUT_1996 Q3') %>%
  mutate('1996_Q4_Expenses' = 'OUT_1996 Q4') %>%
  mutate('1997_Q1_Expenses' = 'OUT_1997 Q1') %>%
  mutate('1997_Q2_Expenses' = 'OUT_1997 Q2') %>%
  mutate('1997_Q3_Expenses' = 'OUT_1997 Q3') %>%
  mutate('1997_Q4_Expenses' = 'OUT_1997 Q4') %>%
  mutate('1998_Q1_Expenses' = 'OUT_1998 Q1') %>%
  mutate('1998_Q2_Expenses' = 'OUT_1998 Q2') %>%
  mutate('1998_Q3_Expenses' = 'OUT_1998 Q3') %>%
  mutate('1998_Q4_Expenses' = 'OUT_1998 Q4') %>%
  mutate('1993_Q1_Income' = 'IN_1993 Q1') %>%
  mutate('1993_Q2_Income' = 'IN_1993 Q2') %>%
  mutate('1993_Q3_Income' = 'IN_1993 Q3') %>%
  mutate('1993_Q4_Income' = 'IN_1993 Q4') %>%
  mutate('1994_Q1_Income' = 'IN_1994 Q1') %>%
  mutate('1994_Q2_Income' = 'IN_1994 Q2') %>%
  mutate('1994_Q3_Income' = 'IN_1994 Q3') %>%
  mutate('1994_Q4_Income' = 'IN_1994 Q4') %>%
  mutate('1995_Q1_Income' = 'IN_1995 Q1') %>%
  mutate('1995_Q2_Income' = 'IN_1995 Q2') %>%
  mutate('1995_Q3_Income' = 'IN_1995 Q3') %>%
  mutate('1995_Q4_Income' = 'IN_1995 Q4') %>%
  mutate('1996_Q1_Income' = 'IN_1996 Q1') %>%
  mutate('1996_Q2_Income' = 'IN_1996 Q2') %>%
  mutate('1996_Q3_Income' = 'IN_1996 Q3') %>%
  mutate('1996_Q4_Income' = 'IN_1996 Q4') %>%
  mutate('1997_Q1_Income' = 'IN_1997 Q1') %>%
  mutate('1997_Q2_Income' = 'IN_1997 Q2') %>%
  mutate('1997_Q3_Income' = 'IN_1997 Q3') %>%
  mutate('1997_Q4_Income' = 'IN_1997 Q4') %>%
  mutate('1998_Q1_Income' = 'IN_1998 Q1') %>%
  mutate('1998_Q2_Income' = 'IN_1998 Q2') %>%
  mutate('1998_Q3_Income' = 'IN_1998 Q3') %>%
  mutate('1998_Q4_Income' = 'IN_1998 Q4') %>%
  select(account_id, '1993_Q1_Expenses', '1993_Q2_Expenses', '1993_Q3_Expenses', '1993_Q4_Expenses', '1994_Q1_Expenses', '1994_Q2_Expenses', '1994_Q3_Expenses', '1994_Q4_Expenses', '1995_Q1_Expenses', '1995_Q2_Expenses', '1995_Q3_Expenses', '1995_Q4_Expenses', '1996_Q1_Expenses', '1996_Q2_Expenses', '1996_Q3_Expenses', '1996_Q4_Expenses', '1997_Q1_Expenses', '1997_Q2_Expenses', '1997_Q3_Expenses', '1997_Q4_Expenses', '1998_Q1_Expenses', '1998_Q2_Expenses', '1998_Q3_Expenses', '1998_Q4_Expenses', '1993_Q1_Income', '1993_Q2_Income', '1993_Q3_Income', '1993_Q4_Income', '1994_Q1_Income', '1994_Q2_Income', '1994_Q3_Income', '1994_Q4_Income', '1995_Q1_Income', '1995_Q2_Income', '1995_Q3_Income', '1995_Q4_Income', '1996_Q1_Income', '1996_Q2_Income', '1996_Q3_Income', '1996_Q4_Income', '1997_Q1_Income', '1997_Q2_Income', '1997_Q3_Income', '1997_Q4_Income', '1998_Q1_Income', '1998_Q2_Income', '1998_Q3_Income', '1998_Q4_Income')
```

    ## # A tibble: 4,500 × 49
    ## # Groups:   account_id [4,500]
    ##    account_id `1993_Q1_Expenses` `1993_Q2_Expenses` `1993_Q3_Expenses`
    ##         <int> <chr>              <chr>              <chr>             
    ##  1          1 OUT_1993 Q1        OUT_1993 Q2        OUT_1993 Q3       
    ##  2          2 OUT_1993 Q1        OUT_1993 Q2        OUT_1993 Q3       
    ##  3          3 OUT_1993 Q1        OUT_1993 Q2        OUT_1993 Q3       
    ##  4          4 OUT_1993 Q1        OUT_1993 Q2        OUT_1993 Q3       
    ##  5          5 OUT_1993 Q1        OUT_1993 Q2        OUT_1993 Q3       
    ##  6          6 OUT_1993 Q1        OUT_1993 Q2        OUT_1993 Q3       
    ##  7          7 OUT_1993 Q1        OUT_1993 Q2        OUT_1993 Q3       
    ##  8          8 OUT_1993 Q1        OUT_1993 Q2        OUT_1993 Q3       
    ##  9          9 OUT_1993 Q1        OUT_1993 Q2        OUT_1993 Q3       
    ## 10         10 OUT_1993 Q1        OUT_1993 Q2        OUT_1993 Q3       
    ## # ℹ 4,490 more rows
    ## # ℹ 45 more variables: `1993_Q4_Expenses` <chr>, `1994_Q1_Expenses` <chr>,
    ## #   `1994_Q2_Expenses` <chr>, `1994_Q3_Expenses` <chr>,
    ## #   `1994_Q4_Expenses` <chr>, `1995_Q1_Expenses` <chr>,
    ## #   `1995_Q2_Expenses` <chr>, `1995_Q3_Expenses` <chr>,
    ## #   `1995_Q4_Expenses` <chr>, `1996_Q1_Expenses` <chr>,
    ## #   `1996_Q2_Expenses` <chr>, `1996_Q3_Expenses` <chr>, …

``` r
# head(transaction_cashflow_per_quarter_and_year_modified)
# 
# df_mod <- left_join(df_mod, transaction_cashflow_per_quarter_and_year_modified, by = c("account_id" = "account_id"))
```

Entfernen der nicht mehr benötigten Tabellen

``` r
rm(transaction_cashflow_per_quarter_and_year, transaction_cashflow_per_quarter_and_year_modified)
```

#### Kontostand am Ende des Tages

Überblick über die Anzahl Transaktionen am selben Tag über alle
Observationen

``` r
df_transaction_mod %>%
  group_by(
    account_id,
    date
    ) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>% 
  head()
```

    ## # A tibble: 6 × 3
    ##   account_id date           n
    ##        <int> <date>     <int>
    ## 1       1274 1998-11-30     7
    ## 2       1498 1998-08-31     7
    ## 3       7418 1997-01-31     7
    ## 4       7418 1997-02-28     7
    ## 5       7418 1997-03-31     7
    ## 6       7418 1997-04-30     7

Es gibt mehrere Tage und unterschiedliche Konten, welche mehr als eine
Transaktion pro Tag verzeichnet hat. Oft sind diese Tage am Ende eines
Monats. Im vorliegenden Datensatz sind es maximal 7 Transaktionen pro
Tag. Die Überlegung ist nun folgende: da es sich beim vorliegenden
Datensatz bei der Balance um den Betrag nach der Transaktion handelt,
kann in der Umkehr der Saldo vor der Transaktion ausgerechnet werden.
Dieser ausgerechnete Saldo kann im Anschluss mit dem Saldo des
bestehenden Datensatzes abgeglichen werden. Somit ist schnell klar,
welche Transaktion die Letzte des Tage ist und welchen Saldo das Konto
am Ende des Tages aufweist.

Dazu werden zwei Data Frames erstellt. Eines, so wie es in den Daten
bereits vorhanden ist und das zweite mit gerechneten Kontoständen, um
die Reihenfolge zu eruieren. Im gleichen Zug werden noch Spalten für
Soll (“outgoes”) und Haben (“credit”) erstellt.

``` r
#Erstellen der neuen Spalten outgoes und incomes
df_transaction_mod <- df_transaction_mod %>%
  mutate(outgoes = ifelse(cashflow == "OUT", amount, 0)) %>%
  mutate(incomes = ifelse(cashflow == "IN", amount, 0)) %>%
  mutate(balance_before_transaction = if_else(cashflow == "IN", balance - amount, balance + amount)) %>% 
  mutate(across(c(outgoes, incomes), as.integer))
  
  head(df_transaction_mod)
```

    ##   trans_id account_id       date cashflow             operation amount balance
    ## 1        1          1 1995-03-24       IN           CASH CREDIT   1000  1000.0
    ## 2        5          1 1995-04-13       IN COLLECTION OTHER BANK   3679  4679.0
    ## 3        6          1 1995-05-13       IN COLLECTION OTHER BANK   3679 20977.2
    ## 4        7          1 1995-06-13       IN COLLECTION OTHER BANK   3679 26835.2
    ## 5        8          1 1995-07-13       IN COLLECTION OTHER BANK   3679 30414.8
    ## 6        9          1 1995-08-13       IN COLLECTION OTHER BANK   3679 28902.7
    ##   characterization bank_name account_nr quarter year month outgoes incomes
    ## 1             <NA>                   NA 1995 Q1 1995     3       0    1000
    ## 2             <NA>        AB   41403269 1995 Q2 1995     4       0    3679
    ## 3             <NA>        AB   41403269 1995 Q2 1995     5       0    3679
    ## 4             <NA>        AB   41403269 1995 Q2 1995     6       0    3679
    ## 5             <NA>        AB   41403269 1995 Q3 1995     7       0    3679
    ## 6             <NA>        AB   41403269 1995 Q3 1995     8       0    3679
    ##   balance_before_transaction
    ## 1                        0.0
    ## 2                     1000.0
    ## 3                    17298.2
    ## 4                    23156.2
    ## 5                    26735.8
    ## 6                    25223.7

#### Test am Account 1274

Der Account 1274 weist am 1998-11-30 sieben Zahlungen auf. Nun soll
mittels Code die letzte Zahlung eruiert werden und somit auch der
Kontostand am Ende des Tages.

``` r
account1274 <- df_transaction_mod %>% 
  filter(
    account_id == 1274,
    date == "1998-11-30"
  ) %>% 
  arrange(date)

account1274
```

    ##   trans_id account_id       date cashflow        operation  amount  balance
    ## 1   374222       1274 1998-11-30       IN      CASH CREDIT 29788.0 144218.0
    ## 2   374227       1274 1998-11-30       IN      CASH CREDIT 19676.0 114109.2
    ## 3   374305       1274 1998-11-30      OUT CASH WIDTHDRAWAL  2700.0  94118.0
    ## 4   374342       1274 1998-11-30      OUT CASH WIDTHDRAWAL    30.0  94088.0
    ## 5   374375       1274 1998-11-30      OUT CASH WIDTHDRAWAL 26900.0 117318.0
    ## 6   374376       1274 1998-11-30      OUT CASH WIDTHDRAWAL 20500.0  96818.0
    ## 7  3446481       1274 1998-11-30       IN             <NA>   320.8 114430.0
    ##    characterization bank_name account_nr quarter year month outgoes incomes
    ## 1              <NA>                   NA 1998 Q4 1998    11       0   29788
    ## 2              <NA>                   NA 1998 Q4 1998    11       0   19676
    ## 3              <NA>                   NA 1998 Q4 1998    11    2700       0
    ## 4 STATEMENT PAYMENT                   NA 1998 Q4 1998    11      30       0
    ## 5              <NA>                   NA 1998 Q4 1998    11   26900       0
    ## 6              <NA>                   NA 1998 Q4 1998    11   20500       0
    ## 7   CREDIT INTEREST                   NA 1998 Q4 1998    11       0     320
    ##   balance_before_transaction
    ## 1                   114430.0
    ## 2                    94433.2
    ## 3                    96818.0
    ## 4                    94118.0
    ## 5                   144218.0
    ## 6                   117318.0
    ## 7                   114109.2

Um einen Kontrollwert zu erhalten, werden die Kontobewegungen von Hand
ausgerechnet. Somit dient das Ergebnis als Referenzwert für die spätere
Codekontrolle.

``` r
account1274_helper <- account1274 %>% 
  mutate(balance_helper = if_else(cashflow == "IN", balance - amount, balance + amount)) %>% 
  select(
    trans_id,
    account_id,
    date,
    outgoes,
    incomes,
    balance_helper
  ) %>% 
  rename(balance = balance_helper) %>% 
  arrange(balance)

account1274_first <- account1274 %>% 
  select(
    trans_id,
    account_id,
    date,
    outgoes,
    incomes,
    balance
  ) %>% 
  arrange(balance)

account1274_first
```

    ##   trans_id account_id       date outgoes incomes  balance
    ## 1   374342       1274 1998-11-30      30       0  94088.0
    ## 2   374305       1274 1998-11-30    2700       0  94118.0
    ## 3   374376       1274 1998-11-30   20500       0  96818.0
    ## 4   374227       1274 1998-11-30       0   19676 114109.2
    ## 5  3446481       1274 1998-11-30       0     320 114430.0
    ## 6   374375       1274 1998-11-30   26900       0 117318.0
    ## 7   374222       1274 1998-11-30       0   29788 144218.0

``` r
account1274_helper
```

    ##   trans_id account_id       date outgoes incomes  balance
    ## 1   374342       1274 1998-11-30      30       0  94118.0
    ## 2   374227       1274 1998-11-30       0   19676  94433.2
    ## 3   374305       1274 1998-11-30    2700       0  96818.0
    ## 4  3446481       1274 1998-11-30       0     320 114109.2
    ## 5   374222       1274 1998-11-30       0   29788 114430.0
    ## 6   374376       1274 1998-11-30   20500       0 117318.0
    ## 7   374375       1274 1998-11-30   26900       0 144218.0

Die letzte Transaktion an diesem Tag ist die Transaktions ID 374342 mit
einer Abbuchung von 30 CZK und dem Kontoendstand von 94088.0 CZK. Dies
wird nun versucht mittels Code zu erreichen.

``` r
account1274_first %>% 
  anti_join(account1274_helper, by = c("account_id", "date", "balance"))
```

    ##   trans_id account_id       date outgoes incomes balance
    ## 1   374342       1274 1998-11-30      30       0   94088

Die Balance wird nun gerundet um Nachkommastellen zu eliminieren.

``` r
account1274_round <- account1274 %>% 
  mutate(balance = round(balance)) %>% 
  mutate(balance_before_transaction_round = round(balance_before_transaction)) %>% 
  mutate(amount_round = round(amount)) %>% 
  relocate(balance,
           balance_before_transaction,
           balance_before_transaction_round,
           amount,
           amount_round,
           date,
           cashflow)
```

#### Erkenntnis zur Rundung von amount und balance

Wenn die Rundung der beiden Spalten schon vor dem Rechnen ausgeführt
wird, werden falsche Endbeträge ausgegeben. Das Runden darf erst am
Schluss geschehen. Beim Runden vor dem Ausrechnen der Saldi weisen die
ausgerechneten Saldi Rundungsfehler auf.

#### Erstellen zusätzlicher Spalten für die Saldo-Berechnung[^8]

Um diesen Rundungsfehler zu umgehen, werden zwei zusätzliche Spalten
erstellt mit den berechneten Saldi vor und nach der Transaktion.

``` r
df_transaction_mod <- df_transaction_mod %>% 
  mutate(balance_before_transaction = ifelse(cashflow == "IN", balance - amount, balance + amount)) %>% 
  mutate(balance = balance) %>% 
  relocate(balance,
           balance,
           balance_before_transaction)
```

Die neu berechneten Saldi werden auf das Beispiel mit dem Account 1274
angewendet und zum Schluss gerundet.

``` r
rm(account1274_first, account1274_helper)

account1274_helper <- account1274 %>% 
  select(
    account_id,
    date,
    balance_before_transaction
  ) %>% 
  rename(balance = balance_before_transaction) %>% 
  mutate(balance = round(balance)) %>% 
  arrange(balance)

account1274_first <- account1274 %>% 
   select(
    account_id,
    date,
    balance
  ) %>% 
  rename(balance = balance) %>% 
  mutate(balance = round(balance)) %>% 
  arrange(balance)

account1274_first
```

    ##   account_id       date balance
    ## 1       1274 1998-11-30   94088
    ## 2       1274 1998-11-30   94118
    ## 3       1274 1998-11-30   96818
    ## 4       1274 1998-11-30  114109
    ## 5       1274 1998-11-30  114430
    ## 6       1274 1998-11-30  117318
    ## 7       1274 1998-11-30  144218

``` r
account1274_helper
```

    ##   account_id       date balance
    ## 1       1274 1998-11-30   94118
    ## 2       1274 1998-11-30   94433
    ## 3       1274 1998-11-30   96818
    ## 4       1274 1998-11-30  114109
    ## 5       1274 1998-11-30  114430
    ## 6       1274 1998-11-30  117318
    ## 7       1274 1998-11-30  144218

Entfernen der nicht mehr benötigeten Tabellen

``` r
rm(account1274, account1274_first, account1274_helper)
```

zweite Überprüfung mit dem Konto 1274 ohne Datumseinschränkung

``` r
# Ergänzen des bestehenden Datensatzes mit der zusätzlichen Spalte balance_end_day (Saldo nach Transaktion) und Filtern des Accounts auf die Nr. 1274
account1274_round   <- df_transaction_mod %>% 
  filter(account_id == 1274) %>% 
  select(
    trans_id,
    account_id,
    date,
    balance
  ) %>% 
  mutate(balance = round(balance))

# Erstellen des Hilfsdatensatzes für die Funktion anti_join()
account1274_helper_round <- df_transaction_mod %>% 
  filter(account_id == 1274) %>% 
  select(
    account_id,
    date,
    balance_before_transaction
  ) %>% 
  mutate(balance = round(balance_before_transaction))

# Erstellen des ersten Datensatzes ohne die Inhalte des Zweiten.
account1274_result_round   <- account1274_round %>% 
  anti_join(account1274_helper_round, by = c("account_id", "date", "balance"))

# Zählen, wieviele Transkationen an einem Tag vorkommen
account1274_result_round %>% 
  group_by(date) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head()
```

    ## # A tibble: 6 × 2
    ## # Groups:   date [6]
    ##   date           n
    ##   <date>     <int>
    ## 1 1996-08-19     1
    ## 2 1996-09-09     1
    ## 3 1996-09-11     1
    ## 4 1996-09-18     1
    ## 5 1996-09-22     1
    ## 6 1996-09-23     1

Dies scheint gut funktioniert zu haben.

Entfernen der nicht mehr benötigten Tabellen aus dem Global Environment

``` r
# Entfernen der vorher generierten Datensätzen
rm(account1247_round, account1274_helper_round, account1274_result, account1274_result_round, account1274_round)
```

Anwenden auf das gesamte Data Frame df_transaction_mod

``` r
# Überprüfung auf mehrfache Transaktionen am gleichen Tag pro Account
df_transaction_mod %>% 
  group_by(
    account_id,
    date
  ) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head()
```

    ## # A tibble: 6 × 3
    ## # Groups:   account_id, date [6]
    ##   account_id date           n
    ##        <int> <date>     <int>
    ## 1       1274 1998-11-30     7
    ## 2       1498 1998-08-31     7
    ## 3       7418 1997-01-31     7
    ## 4       7418 1997-02-28     7
    ## 5       7418 1997-03-31     7
    ## 6       7418 1997-04-30     7

``` r
# Erstellen der ersten Tabelle für den Vergleich
transaction_first <- df_transaction_mod %>% 
  mutate(balance_round = round(balance))

# Erstellen der Vergleichstabelle
transaction_helper <- df_transaction_mod %>% 
  mutate(balance_round = round(balance_before_transaction)) %>% 
  select(
    account_id,
    date,
    balance_round
  )
```

#### Funktion anti_join()

Nun können die beiden Tabellen verglichen werden. Der jeweilige Betrag
im Data Frame transaction_first welcher nicht auch im transaction_helper
vorkommt, ist der Tagesendstand. Dies wird mit der Funkton anti_join
gebildet.

``` r
transaction_result <- transaction_first %>%
  anti_join(transaction_helper, by = c("account_id", "date", "balance_round"))
```

#### Überprüfung

Für die erste Kontrolle werden wiederum die Account_id und das Datum
gruppiert und gezählt, wieviele Transaktionen pro Tag in der Tabelle
enthalten sind.

``` r
transaction_result %>%
  group_by(
    account_id,
    date
    ) %>%
  count() %>%
  arrange(desc(n)) %>% 
  ungroup()
```

    ## # A tibble: 814,645 × 3
    ##    account_id date           n
    ##         <int> <date>     <int>
    ##  1       9814 1998-11-30     3
    ##  2          2 1994-06-12     2
    ##  3         17 1998-08-31     2
    ##  4         88 1996-11-30     2
    ##  5        151 1994-01-31     2
    ##  6        167 1998-08-31     2
    ##  7        264 1997-10-31     2
    ##  8        297 1998-01-31     2
    ##  9        339 1998-10-12     2
    ## 10        378 1998-11-12     2
    ## # ℹ 814,635 more rows

Es sind immer noch mehrere Transaktionen pro Tag vorhanden. Es könnte
sich hier um Fehlerhafte Daten der ursprungs Information handeln. Dies
wird nun genauer untersucht.

#### Untersuchen der Abweichung

Dafür wird der Datensatz df_transaction_mod auf die account id 9814 und
das Datum 1998-11-30 gefiltert.

``` r
# Überprüfen der Daten im df_trans_mod
df_transaction_mod %>% 
  filter(
    account_id == 9814,
    date == "1998-11-30"
  )
```

    ##    balance balance_before_transaction trans_id account_id       date cashflow
    ## 1  86604.4                   106822.4  2963679       9814 1998-11-30      OUT
    ## 2  86574.4                    86604.4  2963759       9814 1998-11-30      OUT
    ## 3 105977.4                   105577.5  3457207       9814 1998-11-30       IN
    ## 4 106376.6                   105977.5  3515135       9814 1998-11-30       IN
    ## 5 106822.4                   106376.5  3682838       9814 1998-11-30       IN
    ##          operation  amount  characterization bank_name account_nr quarter year
    ## 1 CASH WIDTHDRAWAL 20218.0              <NA>                   NA 1998 Q4 1998
    ## 2 CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA 1998 Q4 1998
    ## 3             <NA>   399.9   CREDIT INTEREST                   NA 1998 Q4 1998
    ## 4             <NA>   399.1   CREDIT INTEREST                   NA 1998 Q4 1998
    ## 5             <NA>   445.9   CREDIT INTEREST                   NA 1998 Q4 1998
    ##   month outgoes incomes
    ## 1    11   20218       0
    ## 2    11      30       0
    ## 3    11       0     399
    ## 4    11       0     399
    ## 5    11       0     445

#### Überprüfen auf Rundungs- oder Rechenfehler

![Vergleich der Saldi per Hand](../Ressources/account%20id%209814.png)

``` r
#Überprüfen der Daten nach dem Vergleich
transaction_result %>% 
  filter(
    account_id == 9814,
    date == "1998-11-30"
  )
```

    ##    balance balance_before_transaction trans_id account_id       date cashflow
    ## 1  86574.4                    86604.4  2963759       9814 1998-11-30      OUT
    ## 2 105977.4                   105577.5  3457207       9814 1998-11-30       IN
    ## 3 106376.6                   105977.5  3515135       9814 1998-11-30       IN
    ##          operation amount  characterization bank_name account_nr quarter year
    ## 1 CASH WIDTHDRAWAL   30.0 STATEMENT PAYMENT                   NA 1998 Q4 1998
    ## 2             <NA>  399.9   CREDIT INTEREST                   NA 1998 Q4 1998
    ## 3             <NA>  399.1   CREDIT INTEREST                   NA 1998 Q4 1998
    ##   month outgoes incomes balance_round
    ## 1    11      30       0         86574
    ## 2    11       0     399        105977
    ## 3    11       0     399        106377

Es bleiben wegen fehlerhaften Ursprungsdaten Transaktionen übrig. Aus
früheren Analysen wissen wir, dass jeweils die Zahlung mit dem Eintrag
“STATEMENT PAYMENT” in der Spalte “characterization” die letzte Zahlung
ist (Kontogebühren).

Diese Erkenntnis wird nun eingesetzt. Als erstes werden die letzten
Transaktionen gekennzeichnet.

``` r
# Zählen, wieviele Transaktionen pro Tag vorhanden sind und filtern nach den Daten mit nur einer Transaktion
transaction_count <- transaction_result %>% 
  group_by(
    account_id,
    date
  ) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n == 1)

# Erstellen des Data Frame mit den letzten Transaktionen am Tag
last_transaction <- transaction_result %>% 
  semi_join(transaction_count, by = c("account_id", "date"))
```

``` r
# Filtern nach Daten mit mehr als einer Transaktion pro Tag.
more_than_one_count <- transaction_result %>% 
  group_by(
    account_id,
    date
  ) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n > 1)

# Erstellen des Datensatzes mit mehr als einer Transaktion, welche fehlerhafte Grunddaten haben
more_than_one <- transaction_result %>% 
  semi_join(more_than_one_count, by = c("account_id", "date"))

# Filtern nach Zahlungen der Kontogebühren (dies sind die letzten Zahlungen am Tag)
only_statement_payment <- more_than_one %>%
  filter(characterization == "STATEMENT PAYMENT")
```

``` r
# Kontogebühren dem bestehenden Data Frame last_transaction anfügen

last_transaction <- last_transaction %>% 
  bind_rows(only_statement_payment)
```

Nun müssen noch die letzten Daten mit Mehrfachzahlungen untersucht
werden.

``` r
more_than_one_count_rest <- more_than_one_count %>% 
  anti_join(only_statement_payment, by = c("account_id", "date"))

more_than_one_rest <- more_than_one %>% 
  semi_join(more_than_one_count_rest, by = c("account_id", "date"))

# wie verhält es sich, wenn die Beträge gerundet werden?
ceiling_balance <- more_than_one_rest %>% 
  mutate(before_ceiling = ceiling(balance_before_transaction)) %>% 
  mutate(after_ceiling = ceiling(balance))

ceilingA <- ceiling_balance %>% 
  mutate(balance_match = after_ceiling)

ceilingB <- ceiling_balance %>% 
  mutate(balance_match = before_ceiling)

ceiling_result <- ceilingA %>% 
  anti_join(ceilingB, by = c("account_id", "date", "balance_match"))

ceiling_result_count <- ceiling_result %>% 
  group_by(
    account_id,
    date
  ) %>% 
  count() %>% 
  ungroup()
  
ceiling_one_count <- ceiling_result_count %>% 
  filter(n == 1)

ceiling_more_count <- ceiling_result_count %>% 
  filter(n > 1)

ceiling_one <- ceiling_result %>% 
  semi_join(ceiling_one_count, by = c("account_id", "date"))

# Observationen aus ceiling_one an last_transaction anfügen
last_transaction <- last_transaction %>% 
  bind_rows(ceiling_one)
```

``` r
ceiling_more <- ceiling_result %>% 
  semi_join(ceiling_more_count, by = c("account_id", "date"))
head(ceiling_more)
```

    ##    balance balance_before_transaction trans_id account_id       date cashflow
    ## 1 114398.5                    67591.5    99072        339 1998-10-12       IN
    ## 2  51197.4                   106597.4    99301        339 1998-10-12      OUT
    ## 3  25772.8                    24072.8   158877        532 1996-02-29       IN
    ## 4 139040.5                    99957.5   481806       1641 1996-06-30       IN
    ## 5  77663.7                   139363.7   482113       1641 1996-06-30      OUT
    ## 6 123421.5                    78834.5   962259       3279 1996-06-30       IN
    ##               operation amount characterization bank_name account_nr quarter
    ## 1 COLLECTION OTHER BANK  46807             <NA>        GH   68440270 1998 Q4
    ## 2      CASH WIDTHDRAWAL  55400             <NA>                   NA 1998 Q4
    ## 3           CASH CREDIT   1700             <NA>                   NA 1996 Q1
    ## 4           CASH CREDIT  39083             <NA>                   NA 1996 Q2
    ## 5      CASH WIDTHDRAWAL  61700             <NA>                   NA 1996 Q2
    ## 6           CASH CREDIT  44587             <NA>                   NA 1996 Q2
    ##   year month outgoes incomes balance_round before_ceiling after_ceiling
    ## 1 1998    10       0   46807        114398          67592        114399
    ## 2 1998    10   55400       0         51197         106598         51198
    ## 3 1996     2       0    1700         25773          24073         25773
    ## 4 1996     6       0   39083        139040          99958        139041
    ## 5 1996     6   61700       0         77664         139364         77664
    ## 6 1996     6       0   44587        123422          78835        123422
    ##   balance_match
    ## 1        114399
    ## 2         51198
    ## 3         25773
    ## 4        139041
    ## 5         77664
    ## 6        123422

Bei den letzten Zahlungen ist keine Reihenfolge mehr ersichtlich. Um
dies zu klären werden alle Zahlungen zu den jeweiligen Konten und Tagen
genommen und damit nochmals ein komplettes ceiling und anti_join()
durchgeführt.

``` r
ceiling_more <- df_transaction_mod %>% 
  semi_join(ceiling_more_count, by = c("account_id", "date"))


ceiling_more %>% 
  group_by(
    account_id,
    date
  )%>% 
  count() %>% 
  ungroup()
```

    ## # A tibble: 15 × 3
    ##    account_id date           n
    ##         <int> <date>     <int>
    ##  1        339 1998-10-12     3
    ##  2        532 1996-02-29     3
    ##  3        808 1996-05-31     5
    ##  4       1641 1996-06-30     4
    ##  5       3279 1996-06-30     4
    ##  6       7418 1997-12-31     7
    ##  7       7957 1996-09-30     4
    ##  8       8039 1997-10-31     4
    ##  9       8899 1998-01-12     4
    ## 10       9750 1995-01-31     5
    ## 11       9814 1995-03-31     3
    ## 12      10558 1997-09-30     5
    ## 13      10656 1998-02-28     3
    ## 14      10857 1998-09-30     5
    ## 15      10973 1994-10-12     4

``` r
ceiling_more_A <- ceiling_more %>% 
  mutate(ceil = ceiling(balance)) 

ceiling_more_B <- ceiling_more %>% 
  mutate(ceil = ceiling(balance_before_transaction))

ceiling_last_result <- ceiling_more_A %>% 
  anti_join(ceiling_more_B, by = c("account_id", "date", "ceil"))

ceiling_last_result %>% 
  group_by(
    account_id,
    date
  )%>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))
```

    ## # A tibble: 15 × 3
    ##    account_id date           n
    ##         <int> <date>     <int>
    ##  1        339 1998-10-12     1
    ##  2        532 1996-02-29     1
    ##  3        808 1996-05-31     1
    ##  4       1641 1996-06-30     1
    ##  5       3279 1996-06-30     1
    ##  6       7418 1997-12-31     1
    ##  7       7957 1996-09-30     1
    ##  8       8039 1997-10-31     1
    ##  9       8899 1998-01-12     1
    ## 10       9750 1995-01-31     1
    ## 11       9814 1995-03-31     1
    ## 12      10558 1997-09-30     1
    ## 13      10656 1998-02-28     1
    ## 14      10857 1998-09-30     1
    ## 15      10973 1994-10-12     1

``` r
rm(ceiling_balance, ceiling_more, ceiling_more_A, ceiling_more_B, ceiling_more_count, ceiling_one, ceiling_one_count, ceiling_result, ceiling_result_count, ceilingA,ceilingB)
```

``` r
# Observationen aus ceiling_one an last_transaction anfügen
last_transaction <- last_transaction %>% 
  bind_rows(ceiling_last_result)

last_transaction %>% 
  group_by(
    account_id,
    date
  )%>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))
```

    ## # A tibble: 814,645 × 3
    ##    account_id date           n
    ##         <int> <date>     <int>
    ##  1          1 1995-03-24     1
    ##  2          1 1995-04-13     1
    ##  3          1 1995-04-23     1
    ##  4          1 1995-04-30     1
    ##  5          1 1995-05-13     1
    ##  6          1 1995-05-23     1
    ##  7          1 1995-05-31     1
    ##  8          1 1995-06-13     1
    ##  9          1 1995-06-22     1
    ## 10          1 1995-06-30     1
    ## # ℹ 814,635 more rows

#### Überprüfung der Daten

Das Datum wird nun überprüft, ob nichts verloren ging

``` r
control <- df_transaction_mod %>% 
  anti_join(last_transaction, by = c("account_id", "date"))

control %>% 
  arrange(account_id, date)
```

    ##      balance balance_before_transaction trans_id account_id       date cashflow
    ## 1    32836.2                    32036.2     2227          9 1996-03-17       IN
    ## 2    32036.2                    32836.2     2344          9 1996-03-17      OUT
    ## 3     6097.3                     6111.9     4793         19 1997-07-31      OUT
    ## 4     6111.9                     6096.8  3445030         19 1997-07-31       IN
    ## 5     6096.8                     6082.7  3681196         19 1997-07-31       IN
    ## 6     7492.0                     7506.6     4807         19 1998-09-30      OUT
    ## 7     7521.6                     7492.1  3445045         19 1998-09-30       IN
    ## 8     7506.6                     7513.8  3445046         19 1998-09-30      OUT
    ## 9     7492.1                     7464.6  3681211         19 1998-09-30       IN
    ## 10    7513.7                     7521.6  3681212         19 1998-09-30      OUT
    ## 11      95.9                      110.5    65576        218 1998-01-31      OUT
    ## 12     110.5                      -15.2  3445241        218 1998-01-31       IN
    ## 13      95.7                       95.9  3445242        218 1998-01-31      OUT
    ## 14   60614.5                    60629.1    65577        218 1998-02-28      OUT
    ## 15   60629.1                    60450.7  3445243        218 1998-02-28       IN
    ## 16   60613.7                    60614.5  3445244        218 1998-02-28      OUT
    ## 17   13040.9                    13055.5    74333        245 1995-02-28      OUT
    ## 18   13055.5                    13040.8  3445268        245 1995-02-28       IN
    ## 19   15681.1                    15695.7    74343        245 1995-12-31      OUT
    ## 20   15695.7                    15652.0  3445278        245 1995-12-31       IN
    ## 21   15681.0                    15681.1  3445279        245 1995-12-31      OUT
    ## 22   25103.9                    25203.9    82813        277 1995-05-31      OUT
    ## 23   25203.9                    25103.5  3539702        277 1995-05-31       IN
    ## 24   22960.5                    22360.5    88343        297 1998-01-18       IN
    ## 25   22360.5                    22960.5    88384        297 1998-01-18      OUT
    ## 26   48618.4                    47318.4    94152        320 1997-10-18       IN
    ## 27   47318.4                    48618.4    94186        320 1997-10-18      OUT
    ## 28    -592.8                     -578.2   120967        405 1998-02-28      OUT
    ## 29    -578.2                     -581.7  3543942        405 1998-02-28       IN
    ## 30    -593.1                     -592.9  3543943        405 1998-02-28      OUT
    ## 31    2225.3                     2239.9   145706        487 1997-05-31      OUT
    ## 32    2239.9                     2224.6  3546773        487 1997-05-31       IN
    ## 33     238.2                      252.8   151858        507 1998-09-30      OUT
    ## 34     252.8                      238.2  3445659        507 1998-09-30       IN
    ## 35   33802.3                    33002.3   165358        553 1995-12-18       IN
    ## 36   33002.3                    33802.3   165441        553 1995-12-18      OUT
    ## 37   31253.0                    29353.0   167994        562 1994-09-18       IN
    ## 38   29353.0                    31253.0   168231        562 1994-09-18      OUT
    ## 39    3359.4                     3374.0   184144        625 1996-12-31      OUT
    ## 40    3374.0                     3358.9  3551008        625 1996-12-31       IN
    ## 41   35456.6                    35471.2   184147        625 1997-03-31      OUT
    ## 42   35471.2                    35457.2  3551011        625 1997-03-31       IN
    ## 43     594.2                      608.8   187945        635 1996-05-31      OUT
    ## 44     608.8                      593.9  3551520        635 1996-05-31       IN
    ## 45    -439.0                     -424.4   187952        635 1996-12-31      OUT
    ## 46    -424.4                     -430.3  3551529        635 1996-12-31       IN
    ## 47    -439.2                     -439.0  3551530        635 1996-12-31      OUT
    ## 48   22999.8                    29299.8   206510        701 1996-01-24      OUT
    ## 49   29299.8                    22999.8   206662        701 1996-01-24       IN
    ## 50   23481.6                    24981.6   231862        789 1995-09-04      OUT
    ## 51   24981.6                    23481.6   232006        789 1995-09-04       IN
    ## 52    8123.5                     8138.1   236671        808 1996-07-31      OUT
    ## 53    8144.1                     8124.3  3446024        808 1996-07-31       IN
    ## 54    8151.0                     8163.7  3446025        808 1996-07-31      OUT
    ## 55    8163.7                     8144.1  3681286        808 1996-07-31       IN
    ## 56    8138.1                     8151.0  3681287        808 1996-07-31      OUT
    ## 57   22201.4                    23601.4   271218        929 1994-01-29      OUT
    ## 58   23601.4                    22201.4   271359        929 1994-01-29       IN
    ## 59    -470.9                     -456.3   277492        949 1997-01-31      OUT
    ## 60    -456.3                     -501.6  3446130        949 1997-01-31       IN
    ## 61    -471.2                     -470.9  3446131        949 1997-01-31      OUT
    ## 62   34794.5                    33494.5   312046       1065 1996-01-13       IN
    ## 63   33494.5                    34794.5   312132       1065 1996-01-13      OUT
    ## 64    2424.7                     2439.3   366900       1247 1996-07-31      OUT
    ## 65    2439.3                     2424.9  3446339       1247 1996-07-31       IN
    ## 66    6182.9                     6212.9   368188       1251 1996-07-31      OUT
    ## 67    6212.9                     6182.8  3571686       1251 1996-07-31       IN
    ## 68   24222.0                    24322.0   373144       1270 1997-02-28      OUT
    ## 69   24322.0                    24221.5  3572251       1270 1997-02-28       IN
    ## 70    -461.7                     -431.7   373644       1272 1994-10-31      OUT
    ## 71    -431.7                     -492.8  3446401       1272 1994-10-31       IN
    ## 72    -462.0                     -461.7  3446402       1272 1994-10-31      OUT
    ## 73   18748.5                    23521.5   400993       1366 1995-02-09      OUT
    ## 74   18747.5                    18748.5   401137       1366 1995-02-09      OUT
    ## 75   22538.5                    27311.5   401014       1366 1996-11-09      OUT
    ## 76   22537.5                    22538.5   401158       1366 1996-11-09      OUT
    ## 77   32012.5                    36785.5   401028       1366 1998-01-09      OUT
    ## 78   32011.5                    32012.5   401172       1366 1998-01-09      OUT
    ## 79   26062.5                    30835.5   401030       1366 1998-03-09      OUT
    ## 80   26061.5                    26062.5   401174       1366 1998-03-09      OUT
    ## 81    3173.6                     3188.2   428440       1455 1998-08-31      OUT
    ## 82    3188.2                     3174.0  3446677       1455 1998-08-31       IN
    ## 83   31400.3                    31500.3   449195       1527 1994-09-30      OUT
    ## 84   31500.3                    31423.4  3446965       1527 1994-09-30       IN
    ## 85   31399.9                    31400.3  3446966       1527 1994-09-30      OUT
    ## 86    -532.6                     8767.4   452863       1539 1993-09-30      OUT
    ## 87    -632.6                     -532.6   453075       1539 1993-09-30      OUT
    ## 88    8713.1                     -486.9   453146       1539 1993-09-30       IN
    ## 89    8767.4                     8713.1  3447029       1539 1993-09-30       IN
    ## 90    -633.3                     -632.6  3447030       1539 1993-09-30      OUT
    ## 91    6159.2                     6259.2   465388       1583 1998-01-31      OUT
    ## 92    6259.2                     6208.9  3447152       1583 1998-01-31       IN
    ## 93    6158.7                     6159.2  3447153       1583 1998-01-31      OUT
    ## 94   16619.6                    18419.6   514092       1759 1997-01-19      OUT
    ## 95   18419.6                    16619.6   514163       1759 1997-01-19       IN
    ## 96     -63.1                      -33.1   524033       1787 1997-03-31      OUT
    ## 97     -33.1                      -44.7  3447265       1787 1997-03-31       IN
    ## 98     -63.4                      -63.1  3447266       1787 1997-03-31      OUT
    ## 99    -405.8                     5332.2   555208       1890 1998-03-31      OUT
    ## 100   -435.8                     -405.8   555255       1890 1998-03-31      OUT
    ## 101   5332.2                     5232.3  3447376       1890 1998-03-31       IN
    ## 102   -436.0                     -435.9  3447377       1890 1998-03-31      OUT
    ## 103  16908.0                    17508.0   578402       1969 1995-01-27      OUT
    ## 104  17508.0                    16908.0   578506       1969 1995-01-27       IN
    ## 105  60146.9                    32215.9   595739       2028 1998-11-30       IN
    ## 106  60147.4                    60177.4   595879       2028 1998-11-30      OUT
    ## 107  60177.4                    60146.9  3460880       2028 1998-11-30       IN
    ## 108   -642.0                     -542.0   627718       2144 1997-11-30      OUT
    ## 109   -542.0                     -544.6  3447685       2144 1997-11-30       IN
    ## 110   -642.3                     -642.0  3447686       2144 1997-11-30      OUT
    ## 111  32029.6                    32044.2   633962       2165 1998-08-31      OUT
    ## 112  31929.6                    32029.6   633982       2165 1998-08-31      OUT
    ## 113  32044.2                    31929.6  3601829       2165 1998-08-31       IN
    ## 114    724.0                      738.6   662271       2260 1997-06-30      OUT
    ## 115    738.6                      723.9  3447861       2260 1997-06-30       IN
    ## 116   -645.2                     -630.6   662272       2260 1997-07-31      OUT
    ## 117   -630.6                     -638.0  3447862       2260 1997-07-31       IN
    ## 118   -645.3                     -645.1  3447863       2260 1997-07-31      OUT
    ## 119   1749.9                     1764.5   698931       2390 1998-06-30      OUT
    ## 120   1764.5                     1750.1  3609106       2390 1998-06-30       IN
    ## 121  54063.8                    18809.8   733273       2506 1997-09-30       IN
    ## 122  54063.8                    54163.8   733421       2506 1997-09-30      OUT
    ## 123  54163.8                    54063.9  3613139       2506 1997-09-30       IN
    ## 124  12891.4                    12890.6  3614737       2549 1993-12-31       IN
    ## 125   2738.2                     2752.8   761621       2599 1996-01-31      OUT
    ## 126   2752.8                     2737.7  3448231       2599 1996-01-31       IN
    ## 127  29536.5                    33018.5   841701       2865 1998-05-11      OUT
    ## 128  29535.5                    29536.5   841845       2865 1998-05-11      OUT
    ## 129   6230.6                     6245.2   885897       3018 1998-04-30      OUT
    ## 130   6245.2                     6230.8  3448603       3018 1998-04-30       IN
    ## 131  74589.2                    74619.2   902456       3074 1995-12-31      OUT
    ## 132  74619.2                    74434.3  3448680       3074 1995-12-31       IN
    ## 133  74588.8                    74589.2  3448681       3074 1995-12-31      OUT
    ## 134   6556.9                     6571.5   912153       3109 1998-01-31      OUT
    ## 135   6571.5                     6537.1  3448789       3109 1998-01-31       IN
    ## 136   6556.7                     6556.9  3448790       3109 1998-01-31      OUT
    ## 137   3383.2                     3397.8   913731       3113 1998-09-30      OUT
    ## 138   3397.8                     3383.1  3633318       3113 1998-09-30       IN
    ## 139  24039.7                    24054.3   930596       3171 1994-02-28      OUT
    ## 140  24054.3                    24040.0  3635248       3171 1994-02-28       IN
    ## 141  -1546.1                    -1531.5   960921       3273 1998-06-30      OUT
    ## 142  -1545.7                    -1613.0  3448957       3273 1998-06-30       IN
    ## 143  -1531.5                    -1508.3  3448958       3273 1998-06-30      OUT
    ## 144  -1480.5                    -1545.7  3681723       3273 1998-06-30       IN
    ## 145  -1508.2                    -1480.4  3681724       3273 1998-06-30      OUT
    ## 146  15715.2                    15729.8   978094       3335 1996-07-31      OUT
    ## 147  15729.8                    15715.0  3640769       3335 1996-07-31       IN
    ## 148  53249.3                    53349.3  1001479       3418 1998-09-30      OUT
    ## 149  53349.3                    53249.2  3643202       3418 1998-09-30       IN
    ## 150  54736.1                    54136.1  1009697       3449 1998-01-15       IN
    ## 151  54136.1                    54736.1  1009738       3449 1998-01-15      OUT
    ## 152    197.1                      211.7  1129359       3859 1997-12-31      OUT
    ## 153    211.7                      204.7  3449657       3859 1997-12-31       IN
    ## 154    197.0                      197.0  3449658       3859 1997-12-31      OUT
    ## 155 115510.0                    87110.0  1147400       3925 1997-06-08       IN
    ## 156  87110.0                   115510.0  1147667       3925 1997-06-08      OUT
    ## 157  14913.3                    14927.9  1152994       3943 1996-07-31      OUT
    ## 158  14927.9                    14912.9  3660590       3943 1996-07-31       IN
    ## 159  24537.3                    24637.3  1170436       4013 1995-03-31      OUT
    ## 160  24637.3                    24536.6  3662281       4013 1995-03-31       IN
    ## 161  30323.6                    30338.2  1192741       4082 1997-01-31      OUT
    ## 162  30345.8                    30324.1  3450126       4082 1997-01-31       IN
    ## 163  30338.2                    30345.8  3450127       4082 1997-01-31      OUT
    ## 164   -373.0                     -358.4  1214632       4156 1998-09-30      OUT
    ## 165   -358.4                     -404.1  3450262       4156 1998-09-30       IN
    ## 166   -373.2                     -373.1  3450263       4156 1998-09-30      OUT
    ## 167   2989.8                     3004.4  1265639       4328 1998-11-30      OUT
    ## 168   3004.4                     2989.8  3469874       4328 1998-11-30       IN
    ## 169   2428.6                     2443.2  1273934       4356 1997-12-31      OUT
    ## 170   2443.2                     2428.8  3450405       4356 1997-12-31       IN
    ## 171   1586.5                     1601.1  1353143       4618 1998-02-28      OUT
    ## 172   1601.1                     1603.8  3450843       4618 1998-02-28      OUT
    ## 173   1603.8                     1587.6  3450844       4618 1998-02-28       IN
    ## 174   1585.9                     1586.5  3450845       4618 1998-02-28      OUT
    ## 175   3709.2                     3723.8  1395351       4753 1998-07-31      OUT
    ## 176   3723.8                     3722.8  3451031       4753 1998-07-31       IN
    ## 177   3709.0                     3709.2  3451032       4753 1998-07-31      OUT
    ## 178   -178.7                     -164.1  1399817       4768 1998-05-31      OUT
    ## 179   -164.1                     -190.3  3451064       4768 1998-05-31       IN
    ## 180   -178.9                     -178.7  3451065       4768 1998-05-31      OUT
    ## 181  36851.9                    14563.9  1466526       4995 1995-06-30       IN
    ## 182  36923.7                    36953.7  1466742       4995 1995-06-30      OUT
    ## 183  36953.7                    36851.9  3444437       4995 1995-06-30       IN
    ## 184  36923.6                    36923.7  3444438       4995 1995-06-30      OUT
    ## 185  42146.8                    42846.8  1476695       5033 1998-08-17      OUT
    ## 186  42846.8                    42146.8  1476806       5033 1998-08-17       IN
    ## 187    683.2                      697.8  1486361       5065 1996-12-31      OUT
    ## 188    697.8                      661.6  3444529       5065 1996-12-31       IN
    ## 189    683.1                      683.2  3444530       5065 1996-12-31      OUT
    ## 190   2659.0                     2673.6  1503252       5125 1998-10-31      OUT
    ## 191   2673.6                     2672.5  3451352       5125 1998-10-31       IN
    ## 192   2658.7                     2659.0  3451353       5125 1998-10-31      OUT
    ## 193   -388.7                     -374.1  1504572       5129 1994-11-30      OUT
    ## 194   -374.1                    -8574.1  1504640       5129 1994-11-30       IN
    ## 195  -8574.1                    -8604.4  3451375       5129 1994-11-30       IN
    ## 196   -388.7                     -388.7  3451376       5129 1994-11-30      OUT
    ## 197  20270.2                    20370.2  1524075       5194 1997-04-30      OUT
    ## 198  20370.2                    20269.5  3451454       5194 1997-04-30       IN
    ## 199  24919.9                    25019.9  1535218       5228 1996-07-31      OUT
    ## 200  25019.9                    24920.4  3669612       5228 1996-07-31       IN
    ## 201   2930.4                     2945.0  1555741       5290 1998-09-30      OUT
    ## 202   2945.0                     2929.5  3669983       5290 1998-09-30       IN
    ## 203  -1292.7                    -1299.7  3451783       5366 1997-03-31       IN
    ## 204  -1299.5                    -1292.7  3451784       5366 1997-03-31      OUT
    ## 205   8094.1                     8072.0  3452026       5387 1998-12-31       IN
    ## 206   8093.7                     8094.0  3452027       5387 1998-12-31      OUT
    ## 207   9828.2                     9842.8  1602959       5442 1994-05-31      OUT
    ## 208   9842.8                     9809.5  3670851       5442 1994-05-31       IN
    ## 209   9827.8                     9828.2  3670852       5442 1994-05-31      OUT
    ## 210  21068.0                    21082.6  1602969       5442 1995-03-31      OUT
    ## 211  21082.6                    21008.3  3670867       5442 1995-03-31       IN
    ## 212  21067.5                    21068.1  3670868       5442 1995-03-31      OUT
    ## 213   -454.7                     -440.1  1602997       5442 1997-07-31      OUT
    ## 214   -440.1                     -446.8  3670900       5442 1997-07-31       IN
    ## 215   -454.8                     -454.7  3670901       5442 1997-07-31      OUT
    ## 216  33279.9                    33294.5  1602998       5442 1997-08-31      OUT
    ## 217  33298.1                    33280.2  3670902       5442 1997-08-31       IN
    ## 218  33294.5                    33298.1  3670903       5442 1997-08-31      OUT
    ## 219   4586.2                     4600.8  1611091       5472 1998-11-30      OUT
    ## 220   4600.8                     4584.2  3452206       5472 1998-11-30       IN
    ## 221   4585.8                     4586.2  3452207       5472 1998-11-30      OUT
    ## 222    113.1                      127.7  1614998       5483 1995-01-31      OUT
    ## 223    127.7                       -8.5  3452230       5483 1995-01-31       IN
    ## 224    113.1                      113.1  3452231       5483 1995-01-31      OUT
    ## 225   -970.9                     -940.9  1684085       5708 1998-10-31      OUT
    ## 226   -940.9                     -978.7  3452582       5708 1998-10-31       IN
    ## 227   -971.3                     -971.0  3452583       5708 1998-10-31      OUT
    ## 228  18799.8                    18814.4  1733241       5878 1997-07-31      OUT
    ## 229  18814.4                    18770.8  3444660       5878 1997-07-31       IN
    ## 230  18799.7                    18799.8  3444661       5878 1997-07-31      OUT
    ## 231  -3262.2                    -3247.6  1787877       6064 1998-01-31      OUT
    ## 232  -3242.3                    -3262.3  3453190       6064 1998-01-31       IN
    ## 233  -3247.6                    -3242.3  3453191       6064 1998-01-31      OUT
    ## 234   3606.0                     3574.6  3453203       6064 1998-12-31       IN
    ## 235   3605.8                     3606.0  3453204       6064 1998-12-31      OUT
    ## 236   8272.1                     8286.7  1855577       6281 1997-10-31      OUT
    ## 237   8289.7                     8271.8  3453460       6281 1997-10-31       IN
    ## 238   8286.7                     8289.7  3453461       6281 1997-10-31      OUT
    ## 239  26436.3                    26536.3  1891203       6400 1997-01-31      OUT
    ## 240  26477.1                    23277.1  1891241       6400 1997-01-31       IN
    ## 241  26536.3                    26477.1  3676197       6400 1997-01-31       IN
    ## 242  26435.5                    26436.3  3676198       6400 1997-01-31      OUT
    ## 243   2572.6                     2587.2  1908104       6463 1996-09-30      OUT
    ## 244   2587.2                     2572.6  3453569       6463 1996-09-30       IN
    ## 245   4172.3                     4186.9  1908105       6463 1996-10-31      OUT
    ## 246   4186.9                     4172.5  3453570       6463 1996-10-31       IN
    ## 247   9166.4                     9196.4  1921623       6512 1998-09-30      OUT
    ## 248   9196.4                     9166.0  3453760       6512 1998-09-30       IN
    ## 249  -1078.9                    -1064.3  1951336       6609 1996-06-30      OUT
    ## 250  -1064.3                    -1078.7  3677222       6609 1996-06-30       IN
    ## 251  -1079.3                    -1078.9  3677223       6609 1996-06-30      OUT
    ## 252  -9843.3                    -9828.7  2247222       7418 1997-02-28      OUT
    ## 253  -9799.8                    -9817.6  3454507       7418 1997-02-28       IN
    ## 254  -9818.6                    -9808.5  3454508       7418 1997-02-28      OUT
    ## 255  -9843.1                    -9860.9  3488155       7418 1997-02-28       IN
    ## 256  -9828.7                    -9818.6  3488156       7418 1997-02-28      OUT
    ## 257  -9817.6                    -9843.0  3682486       7418 1997-02-28       IN
    ## 258  -9808.5                    -9799.8  3682487       7418 1997-02-28      OUT
    ## 259   2266.3                     2280.9  2247224       7418 1997-04-30      OUT
    ## 260   2273.3                     2269.2  3454511       7418 1997-04-30       IN
    ## 261   2245.9                     2256.4  3454512       7418 1997-04-30      OUT
    ## 262   2269.2                     2265.1  3488159       7418 1997-04-30       IN
    ## 263   2256.4                     2266.3  3488160       7418 1997-04-30      OUT
    ## 264   2280.9                     2273.3  3682490       7418 1997-04-30       IN
    ## 265   2245.7                     2245.9  3682491       7418 1997-04-30      OUT
    ## 266   9365.0                     9379.6  2256508       7445 1998-01-31      OUT
    ## 267   9365.2                     9334.3  3454604       7445 1998-01-31       IN
    ## 268   9379.6                     9365.2  3488455       7445 1998-01-31       IN
    ## 269  14460.4                    14560.4  2489596       8212 1994-06-30      OUT
    ## 270  14560.4                    14388.3  3455386       8212 1994-06-30       IN
    ## 271  14459.6                    14460.0  3455387       8212 1994-06-30      OUT
    ## 272  14388.3                    14218.1  3497754       8212 1994-06-30       IN
    ## 273  14460.0                    14460.4  3497755       8212 1994-06-30      OUT
    ## 274   5041.2                     5055.8  2593505       8566 1996-07-31      OUT
    ## 275   5055.8                     5040.7  3476067       8566 1996-07-31       IN
    ## 276   5040.7                     5040.3  3501788       8566 1996-07-31       IN
    ## 277    648.8                    15503.8  2699811       8934 1994-04-30      OUT
    ## 278    634.2                      648.8  2699951       8934 1994-04-30      OUT
    ## 279  15503.8                    15369.4  3455885       8934 1994-04-30       IN
    ## 280    634.0                      634.2  3455886       8934 1994-04-30      OUT
    ## 281  15369.4                    15235.0  3505363       8934 1994-04-30       IN
    ## 282    633.7                      633.9  3505364       8934 1994-04-30      OUT
    ## 283  72681.7                    72696.3  2707565       8957 1994-08-31      OUT
    ## 284  72696.3                    72394.7  3455960       8957 1994-08-31       IN
    ## 285  72681.7                    72681.7  3455961       8957 1994-08-31      OUT
    ## 286  72394.8                    72076.5  3505591       8957 1994-08-31       IN
    ## 287   7265.0                     7279.6  2726231       9017 1998-07-31      OUT
    ## 288   7235.5                     7209.3  3456089       9017 1998-07-31       IN
    ## 289   7279.6                     7265.2  3506455       9017 1998-07-31       IN
    ## 290   7265.2                     7235.6  3682661       9017 1998-07-31       IN
    ## 291  24678.8                    24693.4  2733250       9041 1997-01-31      OUT
    ## 292  24686.6                    24675.4  3456140       9041 1997-01-31       IN
    ## 293  24677.8                    24678.3  3456141       9041 1997-01-31      OUT
    ## 294  24693.4                    24686.6  3506862       9041 1997-01-31       IN
    ## 295  24678.3                    24678.8  3506863       9041 1997-01-31      OUT
    ## 296  11046.3                    11076.3  2735949       9051 1998-03-31      OUT
    ## 297  11076.3                    11075.0  3456220       9051 1998-03-31       IN
    ## 298  11045.9                    11046.3  3456221       9051 1998-03-31      OUT
    ## 299  11075.0                    11073.7  3507087       9051 1998-03-31       IN
    ## 300   1547.0                     1577.0  2735956       9051 1998-10-31      OUT
    ## 301   1577.0                     1558.8  3456230       9051 1998-10-31       IN
    ## 302   1546.8                     1546.9  3456231       9051 1998-10-31      OUT
    ## 303   1558.9                     1543.5  3507095       9051 1998-10-31       IN
    ## 304   1546.9                     1547.0  3507096       9051 1998-10-31      OUT
    ## 305  13755.3                    13769.9  2761982       9138 1997-04-30      OUT
    ## 306  13770.4                    13721.8  3478942       9138 1997-04-30       IN
    ## 307  13754.9                    13755.3  3478943       9138 1997-04-30      OUT
    ## 308  13721.7                    13674.9  3508157       9138 1997-04-30       IN
    ## 309  13769.9                    13770.3  3508158       9138 1997-04-30      OUT
    ## 310    391.9                      406.5  2820490       9337 1994-01-31      OUT
    ## 311    407.0                      307.0  3456562       9337 1994-01-31       IN
    ## 312    406.6                      406.8  3456563       9337 1994-01-31      OUT
    ## 313    307.0                      247.9  3510171       9337 1994-01-31       IN
    ## 314    406.8                      407.0  3510172       9337 1994-01-31      OUT
    ## 315    247.8                      144.2  3682690       9337 1994-01-31       IN
    ## 316    391.8                      391.9  3682691       9337 1994-01-31      OUT
    ## 317  28578.8                    28593.4  2820524       9337 1996-11-30      OUT
    ## 318  28593.4                    28492.7  3456601       9337 1996-11-30       IN
    ## 319  28578.5                    28578.9  3456602       9337 1996-11-30      OUT
    ## 320  28377.5                    28279.7  3510210       9337 1996-11-30       IN
    ## 321  28578.1                    28578.5  3510211       9337 1996-11-30      OUT
    ## 322  28492.8                    28377.5  3682728       9337 1996-11-30       IN
    ## 323   8488.3                     8502.9  2820526       9337 1997-01-31      OUT
    ## 324   8466.4                     8445.2  3456604       9337 1997-01-31       IN
    ## 325   8502.9                     8502.9  3456605       9337 1997-01-31      OUT
    ## 326   8445.2                     8431.5  3510213       9337 1997-01-31       IN
    ## 327   8488.2                     8488.2  3510214       9337 1997-01-31      OUT
    ## 328   8502.9                     8466.4  3682730       9337 1997-01-31       IN
    ## 329   4640.0                     4654.6  2866436       9484 1998-11-30      OUT
    ## 330   4640.2                     4625.2  3456746       9484 1998-11-30       IN
    ## 331   4654.6                     4640.2  3511645       9484 1998-11-30       IN
    ## 332  21141.9                    21241.9  2906805       9627 1996-06-30      OUT
    ## 333  21241.9                    21142.2  3513038       9627 1996-06-30       IN
    ## 334  52556.1                    52570.7  2908304       9633 1997-02-28      OUT
    ## 335  52570.7                    52471.6  3456820       9633 1997-02-28       IN
    ## 336  52555.9                    52556.1  3456821       9633 1997-02-28      OUT
    ## 337  52471.7                    52380.2  3513111       9633 1997-02-28       IN
    ## 338   4593.8                     4608.4  2908941       9635 1995-04-30      OUT
    ## 339   4594.3                     4580.2  3456875       9635 1995-04-30       IN
    ## 340   4608.4                     4594.3  3513158       9635 1995-04-30       IN
    ## 341   -149.9                     -135.3  2984685       9883 1996-06-30      OUT
    ## 342   -135.3                     -135.9  3515669       9883 1996-06-30       IN
    ## 343   -150.1                     -149.9  3515670       9883 1996-06-30      OUT
    ## 344  11825.6                    11840.2  3055761      10131 1995-12-31      OUT
    ## 345  11815.2                    11748.5  3457526      10131 1995-12-31       IN
    ## 346  11840.2                    11840.3  3457527      10131 1995-12-31      OUT
    ## 347  11840.3                    11815.2  3518442      10131 1995-12-31       IN
    ## 348  11825.6                    11825.7  3518443      10131 1995-12-31      OUT
    ## 349   2725.8                     2740.4  3116904      10343 1998-04-30      OUT
    ## 350   2740.4                     2725.8  3457880      10343 1998-04-30       IN
    ## 351   2725.8                     2721.2  3520304      10343 1998-04-30       IN
    ## 352    783.9                      813.9  3249149      10788 1996-04-30      OUT
    ## 353    790.2                      764.8  3458359      10788 1996-04-30       IN
    ## 354    813.9                      790.2  3458360      10788 1996-04-30       IN
    ## 355    783.6                      783.9  3458361      10788 1996-04-30      OUT
    ## 356   5335.1                     5349.7  3268788      10857 1998-04-30      OUT
    ## 357   5334.1                     5317.8  3458416      10857 1998-04-30       IN
    ## 358   5333.8                     5335.1  3458417      10857 1998-04-30      OUT
    ## 359   5349.7                     5334.1  3525008      10857 1998-04-30       IN
    ## 360  14033.9                    14048.5  3414697      11333 1996-01-31      OUT
    ## 361  14048.6                    14006.1  3459025      11333 1996-01-31       IN
    ## 362  14048.5                    14048.6  3459026      11333 1996-01-31      OUT
    ## 363  14006.1                    13966.9  3529591      11333 1996-01-31       IN
    ## 364  14033.8                    14033.9  3529592      11333 1996-01-31      OUT
    ## 365  20875.3                    20889.9  3431224      11382 1997-03-31      OUT
    ## 366  20840.9                    20789.4  3459084      11382 1997-03-31       IN
    ## 367  20874.8                    20875.3  3459085      11382 1997-03-31      OUT
    ## 368  20889.9                    20841.0  3530288      11382 1997-03-31       IN
    ##                  operation  amount   characterization bank_name account_nr
    ## 1              CASH CREDIT   800.0               <NA>                   NA
    ## 2         CASH WIDTHDRAWAL   800.0               <NA>                   NA
    ## 3         CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 4                     <NA>    15.1    CREDIT INTEREST                   NA
    ## 5                     <NA>    14.1    CREDIT INTEREST                   NA
    ## 6         CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 7                     <NA>    29.5    CREDIT INTEREST                   NA
    ## 8         CASH WIDTHDRAWAL     7.2  SANCTION INTEREST                   NA
    ## 9                     <NA>    27.5    CREDIT INTEREST                   NA
    ## 10        CASH WIDTHDRAWAL     7.9  SANCTION INTEREST                   NA
    ## 11        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 12                    <NA>   125.7    CREDIT INTEREST                   NA
    ## 13        CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 14        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 15                    <NA>   178.4    CREDIT INTEREST                   NA
    ## 16        CASH WIDTHDRAWAL     0.8  SANCTION INTEREST                   NA
    ## 17        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 18                    <NA>    14.7    CREDIT INTEREST                   NA
    ## 19        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 20                    <NA>    43.7    CREDIT INTEREST                   NA
    ## 21        CASH WIDTHDRAWAL     0.1  SANCTION INTEREST                   NA
    ## 22        CASH WIDTHDRAWAL   100.0  STATEMENT PAYMENT                   NA
    ## 23                    <NA>   100.4    CREDIT INTEREST                   NA
    ## 24             CASH CREDIT   600.0               <NA>                   NA
    ## 25        CASH WIDTHDRAWAL   600.0               <NA>                   NA
    ## 26             CASH CREDIT  1300.0               <NA>                   NA
    ## 27        CASH WIDTHDRAWAL  1300.0               <NA>                   NA
    ## 28        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 29                    <NA>     3.5    CREDIT INTEREST                   NA
    ## 30        CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 31        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 32                    <NA>    15.3    CREDIT INTEREST                   NA
    ## 33        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 34                    <NA>    14.6    CREDIT INTEREST                   NA
    ## 35             CASH CREDIT   800.0               <NA>                   NA
    ## 36        CASH WIDTHDRAWAL   800.0               <NA>                   NA
    ## 37             CASH CREDIT  1900.0               <NA>                   NA
    ## 38        CASH WIDTHDRAWAL  1900.0               <NA>                   NA
    ## 39        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 40                    <NA>    15.1    CREDIT INTEREST                   NA
    ## 41        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 42                    <NA>    14.0    CREDIT INTEREST                   NA
    ## 43        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 44                    <NA>    14.9    CREDIT INTEREST                   NA
    ## 45        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 46                    <NA>     5.9    CREDIT INTEREST                   NA
    ## 47        CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 48        CASH WIDTHDRAWAL  6300.0               <NA>                   NA
    ## 49             CASH CREDIT  6300.0               <NA>                   NA
    ## 50        CASH WIDTHDRAWAL  1500.0               <NA>                    0
    ## 51             CASH CREDIT  1500.0               <NA>                   NA
    ## 52        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 53                    <NA>    19.8    CREDIT INTEREST                   NA
    ## 54        CASH WIDTHDRAWAL    12.7  SANCTION INTEREST                   NA
    ## 55                    <NA>    19.6    CREDIT INTEREST                   NA
    ## 56        CASH WIDTHDRAWAL    12.9  SANCTION INTEREST                   NA
    ## 57        CASH WIDTHDRAWAL  1400.0               <NA>                   NA
    ## 58             CASH CREDIT  1400.0               <NA>                   NA
    ## 59        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 60                    <NA>    45.3    CREDIT INTEREST                   NA
    ## 61        CASH WIDTHDRAWAL     0.3  SANCTION INTEREST                   NA
    ## 62             CASH CREDIT  1300.0               <NA>                   NA
    ## 63        CASH WIDTHDRAWAL  1300.0               <NA>                   NA
    ## 64        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 65                    <NA>    14.4    CREDIT INTEREST                   NA
    ## 66        CASH WIDTHDRAWAL    30.0  STATEMENT PAYMENT                   NA
    ## 67                    <NA>    30.1    CREDIT INTEREST                   NA
    ## 68        CASH WIDTHDRAWAL   100.0  STATEMENT PAYMENT                   NA
    ## 69                    <NA>   100.5    CREDIT INTEREST                   NA
    ## 70        CASH WIDTHDRAWAL    30.0  STATEMENT PAYMENT                   NA
    ## 71                    <NA>    61.1    CREDIT INTEREST                   NA
    ## 72        CASH WIDTHDRAWAL     0.3  SANCTION INTEREST                   NA
    ## 73   REMITTANCE OTHER BANK  4773.0          HOUSEHOLD        GH   26513684
    ## 74   REMITTANCE OTHER BANK     1.0 INSURRANCE PAYMENT        UV   48775595
    ## 75   REMITTANCE OTHER BANK  4773.0          HOUSEHOLD        GH   26513684
    ## 76   REMITTANCE OTHER BANK     1.0 INSURRANCE PAYMENT        UV   48775595
    ## 77   REMITTANCE OTHER BANK  4773.0          HOUSEHOLD        GH   26513684
    ## 78   REMITTANCE OTHER BANK     1.0 INSURRANCE PAYMENT        UV   48775595
    ## 79   REMITTANCE OTHER BANK  4773.0          HOUSEHOLD        GH   26513684
    ## 80   REMITTANCE OTHER BANK     1.0 INSURRANCE PAYMENT        UV   48775595
    ## 81        CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 82                    <NA>    14.2    CREDIT INTEREST                   NA
    ## 83        CASH WIDTHDRAWAL   100.0  STATEMENT PAYMENT                   NA
    ## 84                    <NA>    76.9    CREDIT INTEREST                   NA
    ## 85        CASH WIDTHDRAWAL     0.4  SANCTION INTEREST                   NA
    ## 86        CASH WIDTHDRAWAL  9300.0               <NA>                    0
    ## 87        CASH WIDTHDRAWAL   100.0  STATEMENT PAYMENT                   NA
    ## 88             CASH CREDIT  9200.0               <NA>                   NA
    ## 89                    <NA>    54.3    CREDIT INTEREST                   NA
    ## 90        CASH WIDTHDRAWAL     0.7  SANCTION INTEREST                   NA
    ## 91        CASH WIDTHDRAWAL   100.0  STATEMENT PAYMENT                   NA
    ## 92                    <NA>    50.3    CREDIT INTEREST                   NA
    ## 93        CASH WIDTHDRAWAL     0.5  SANCTION INTEREST                   NA
    ## 94        CASH WIDTHDRAWAL  1800.0               <NA>                   NA
    ## 95             CASH CREDIT  1800.0               <NA>                   NA
    ## 96        CASH WIDTHDRAWAL    30.0  STATEMENT PAYMENT                   NA
    ## 97                    <NA>    11.6    CREDIT INTEREST                   NA
    ## 98        CASH WIDTHDRAWAL     0.3  SANCTION INTEREST                   NA
    ## 99        CASH WIDTHDRAWAL  5738.0               <NA>                   NA
    ## 100       CASH WIDTHDRAWAL    30.0  STATEMENT PAYMENT                   NA
    ## 101                   <NA>    99.9    CREDIT INTEREST                   NA
    ## 102       CASH WIDTHDRAWAL     0.1  SANCTION INTEREST                   NA
    ## 103       CASH WIDTHDRAWAL   600.0               <NA>                   NA
    ## 104            CASH CREDIT   600.0               <NA>                   NA
    ## 105            CASH CREDIT 27931.0               <NA>                   NA
    ## 106       CASH WIDTHDRAWAL    30.0  STATEMENT PAYMENT                   NA
    ## 107                   <NA>    30.5    CREDIT INTEREST                   NA
    ## 108       CASH WIDTHDRAWAL   100.0  STATEMENT PAYMENT                   NA
    ## 109                   <NA>     2.6    CREDIT INTEREST                   NA
    ## 110       CASH WIDTHDRAWAL     0.3  SANCTION INTEREST                   NA
    ## 111       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 112       CASH WIDTHDRAWAL   100.0               <NA>                   NA
    ## 113                   <NA>   114.6    CREDIT INTEREST                   NA
    ## 114       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 115                   <NA>    14.7    CREDIT INTEREST                   NA
    ## 116       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 117                   <NA>     7.4    CREDIT INTEREST                   NA
    ## 118       CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 119       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 120                   <NA>    14.4    CREDIT INTEREST                   NA
    ## 121            CASH CREDIT 35254.0               <NA>                   NA
    ## 122       CASH WIDTHDRAWAL   100.0  STATEMENT PAYMENT                   NA
    ## 123                   <NA>    99.9    CREDIT INTEREST                   NA
    ## 124                   <NA>     0.8    CREDIT INTEREST                   NA
    ## 125       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 126                   <NA>    15.1    CREDIT INTEREST                   NA
    ## 127  REMITTANCE OTHER BANK  3482.0          HOUSEHOLD        QR   75203019
    ## 128  REMITTANCE OTHER BANK     1.0               <NA>        OP   25167072
    ## 129       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 130                   <NA>    14.4    CREDIT INTEREST                   NA
    ## 131       CASH WIDTHDRAWAL    30.0  STATEMENT PAYMENT                   NA
    ## 132                   <NA>   184.9    CREDIT INTEREST                   NA
    ## 133       CASH WIDTHDRAWAL     0.4  SANCTION INTEREST                   NA
    ## 134       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 135                   <NA>    34.4    CREDIT INTEREST                   NA
    ## 136       CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 137       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 138                   <NA>    14.7    CREDIT INTEREST                   NA
    ## 139       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 140                   <NA>    14.3    CREDIT INTEREST                   NA
    ## 141       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 142                   <NA>    67.3    CREDIT INTEREST                   NA
    ## 143       CASH WIDTHDRAWAL    23.2  SANCTION INTEREST                   NA
    ## 144                   <NA>    65.2    CREDIT INTEREST                   NA
    ## 145       CASH WIDTHDRAWAL    27.8  SANCTION INTEREST                   NA
    ## 146       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 147                   <NA>    14.8    CREDIT INTEREST                   NA
    ## 148       CASH WIDTHDRAWAL   100.0  STATEMENT PAYMENT                   NA
    ## 149                   <NA>   100.1    CREDIT INTEREST                   NA
    ## 150            CASH CREDIT   600.0               <NA>                   NA
    ## 151 CREDIT CARD WITHDRAWAL   600.0               <NA>                    0
    ## 152       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 153                   <NA>     7.0    CREDIT INTEREST                   NA
    ## 154       CASH WIDTHDRAWAL     0.0  SANCTION INTEREST                   NA
    ## 155            CASH CREDIT 28400.0               <NA>                   NA
    ## 156       CASH WIDTHDRAWAL 28400.0               <NA>                   NA
    ## 157       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 158                   <NA>    15.0    CREDIT INTEREST                   NA
    ## 159       CASH WIDTHDRAWAL   100.0  STATEMENT PAYMENT                   NA
    ## 160                   <NA>   100.7    CREDIT INTEREST                   NA
    ## 161       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 162                   <NA>    21.7    CREDIT INTEREST                   NA
    ## 163       CASH WIDTHDRAWAL     7.6  SANCTION INTEREST                   NA
    ## 164       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 165                   <NA>    45.7    CREDIT INTEREST                   NA
    ## 166       CASH WIDTHDRAWAL     0.1  SANCTION INTEREST                   NA
    ## 167       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 168                   <NA>    14.6    CREDIT INTEREST                   NA
    ## 169       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 170                   <NA>    14.4    CREDIT INTEREST                   NA
    ## 171       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 172       CASH WIDTHDRAWAL     2.7  SANCTION INTEREST                   NA
    ## 173                   <NA>    16.2    CREDIT INTEREST                   NA
    ## 174       CASH WIDTHDRAWAL     0.6  SANCTION INTEREST                   NA
    ## 175       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 176                   <NA>     1.0    CREDIT INTEREST                   NA
    ## 177       CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 178       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 179                   <NA>    26.2    CREDIT INTEREST                   NA
    ## 180       CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 181            CASH CREDIT 22288.0               <NA>                   NA
    ## 182       CASH WIDTHDRAWAL    30.0  STATEMENT PAYMENT                   NA
    ## 183                   <NA>   101.8    CREDIT INTEREST                   NA
    ## 184       CASH WIDTHDRAWAL     0.1  SANCTION INTEREST                   NA
    ## 185       CASH WIDTHDRAWAL   700.0               <NA>                    0
    ## 186            CASH CREDIT   700.0               <NA>                   NA
    ## 187       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 188                   <NA>    36.2    CREDIT INTEREST                   NA
    ## 189       CASH WIDTHDRAWAL     0.1  SANCTION INTEREST                   NA
    ## 190       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 191                   <NA>     1.1    CREDIT INTEREST                   NA
    ## 192       CASH WIDTHDRAWAL     0.3  SANCTION INTEREST                   NA
    ## 193       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 194            CASH CREDIT  8200.0               <NA>                   NA
    ## 195                   <NA>    30.3    CREDIT INTEREST                   NA
    ## 196       CASH WIDTHDRAWAL     0.0  SANCTION INTEREST                   NA
    ## 197       CASH WIDTHDRAWAL   100.0  STATEMENT PAYMENT                   NA
    ## 198                   <NA>   100.7    CREDIT INTEREST                   NA
    ## 199       CASH WIDTHDRAWAL   100.0  STATEMENT PAYMENT                   NA
    ## 200                   <NA>    99.5    CREDIT INTEREST                   NA
    ## 201       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 202                   <NA>    15.5    CREDIT INTEREST                   NA
    ## 203                   <NA>     7.0    CREDIT INTEREST                   NA
    ## 204       CASH WIDTHDRAWAL     6.8  SANCTION INTEREST                   NA
    ## 205                   <NA>    22.1    CREDIT INTEREST                   NA
    ## 206       CASH WIDTHDRAWAL     0.3  SANCTION INTEREST                   NA
    ## 207       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 208                   <NA>    33.3    CREDIT INTEREST                   NA
    ## 209       CASH WIDTHDRAWAL     0.4  SANCTION INTEREST                   NA
    ## 210       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 211                   <NA>    74.3    CREDIT INTEREST                   NA
    ## 212       CASH WIDTHDRAWAL     0.6  SANCTION INTEREST                   NA
    ## 213       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 214                   <NA>     6.7    CREDIT INTEREST                   NA
    ## 215       CASH WIDTHDRAWAL     0.1  SANCTION INTEREST                   NA
    ## 216       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 217                   <NA>    17.9    CREDIT INTEREST                   NA
    ## 218       CASH WIDTHDRAWAL     3.6  SANCTION INTEREST                   NA
    ## 219       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 220                   <NA>    16.6    CREDIT INTEREST                   NA
    ## 221       CASH WIDTHDRAWAL     0.4  SANCTION INTEREST                   NA
    ## 222       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 223                   <NA>   136.2    CREDIT INTEREST                   NA
    ## 224       CASH WIDTHDRAWAL     0.0  SANCTION INTEREST                   NA
    ## 225       CASH WIDTHDRAWAL    30.0  STATEMENT PAYMENT                   NA
    ## 226                   <NA>    37.8    CREDIT INTEREST                   NA
    ## 227       CASH WIDTHDRAWAL     0.3  SANCTION INTEREST                   NA
    ## 228       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 229                   <NA>    43.6    CREDIT INTEREST                   NA
    ## 230       CASH WIDTHDRAWAL     0.1  SANCTION INTEREST                   NA
    ## 231       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 232                   <NA>    20.0    CREDIT INTEREST                   NA
    ## 233       CASH WIDTHDRAWAL     5.3  SANCTION INTEREST                   NA
    ## 234                   <NA>    31.4    CREDIT INTEREST                   NA
    ## 235       CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 236       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 237                   <NA>    17.9    CREDIT INTEREST                   NA
    ## 238       CASH WIDTHDRAWAL     3.0  SANCTION INTEREST                   NA
    ## 239       CASH WIDTHDRAWAL   100.0  STATEMENT PAYMENT                   NA
    ## 240            CASH CREDIT  3200.0               <NA>                   NA
    ## 241                   <NA>    59.2    CREDIT INTEREST                   NA
    ## 242       CASH WIDTHDRAWAL     0.8  SANCTION INTEREST                   NA
    ## 243       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 244                   <NA>    14.6    CREDIT INTEREST                   NA
    ## 245       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 246                   <NA>    14.4    CREDIT INTEREST                   NA
    ## 247       CASH WIDTHDRAWAL    30.0  STATEMENT PAYMENT                   NA
    ## 248                   <NA>    30.4    CREDIT INTEREST                   NA
    ## 249       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 250                   <NA>    14.4    CREDIT INTEREST                   NA
    ## 251       CASH WIDTHDRAWAL     0.4  SANCTION INTEREST                   NA
    ## 252       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 253                   <NA>    17.8    CREDIT INTEREST                   NA
    ## 254       CASH WIDTHDRAWAL    10.1  SANCTION INTEREST                   NA
    ## 255                   <NA>    17.8    CREDIT INTEREST                   NA
    ## 256       CASH WIDTHDRAWAL    10.1  SANCTION INTEREST                   NA
    ## 257                   <NA>    25.4    CREDIT INTEREST                   NA
    ## 258       CASH WIDTHDRAWAL     8.7  SANCTION INTEREST                   NA
    ## 259       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 260                   <NA>     4.1    CREDIT INTEREST                   NA
    ## 261       CASH WIDTHDRAWAL    10.5  SANCTION INTEREST                   NA
    ## 262                   <NA>     4.1    CREDIT INTEREST                   NA
    ## 263       CASH WIDTHDRAWAL     9.9  SANCTION INTEREST                   NA
    ## 264                   <NA>     7.6    CREDIT INTEREST                   NA
    ## 265       CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 266       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 267                   <NA>    30.9    CREDIT INTEREST                   NA
    ## 268                   <NA>    14.4    CREDIT INTEREST                   NA
    ## 269       CASH WIDTHDRAWAL   100.0  STATEMENT PAYMENT                   NA
    ## 270                   <NA>   172.1    CREDIT INTEREST                   NA
    ## 271       CASH WIDTHDRAWAL     0.4  SANCTION INTEREST                   NA
    ## 272                   <NA>   170.2    CREDIT INTEREST                   NA
    ## 273       CASH WIDTHDRAWAL     0.4  SANCTION INTEREST                   NA
    ## 274       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 275                   <NA>    15.1    CREDIT INTEREST                   NA
    ## 276                   <NA>     0.4    CREDIT INTEREST                   NA
    ## 277       CASH WIDTHDRAWAL 14855.0               <NA>                   NA
    ## 278       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 279                   <NA>   134.4    CREDIT INTEREST                   NA
    ## 280       CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 281                   <NA>   134.4    CREDIT INTEREST                   NA
    ## 282       CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 283       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 284                   <NA>   301.6    CREDIT INTEREST                   NA
    ## 285       CASH WIDTHDRAWAL     0.0  SANCTION INTEREST                   NA
    ## 286                   <NA>   318.3    CREDIT INTEREST                   NA
    ## 287       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 288                   <NA>    26.2    CREDIT INTEREST                   NA
    ## 289                   <NA>    14.4    CREDIT INTEREST                   NA
    ## 290                   <NA>    29.6    CREDIT INTEREST                   NA
    ## 291       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 292                   <NA>    11.2    CREDIT INTEREST                   NA
    ## 293       CASH WIDTHDRAWAL     0.5  SANCTION INTEREST                   NA
    ## 294                   <NA>     6.8    CREDIT INTEREST                   NA
    ## 295       CASH WIDTHDRAWAL     0.5  SANCTION INTEREST                   NA
    ## 296       CASH WIDTHDRAWAL    30.0  STATEMENT PAYMENT                   NA
    ## 297                   <NA>     1.3    CREDIT INTEREST                   NA
    ## 298       CASH WIDTHDRAWAL     0.4  SANCTION INTEREST                   NA
    ## 299                   <NA>     1.3    CREDIT INTEREST                   NA
    ## 300       CASH WIDTHDRAWAL    30.0  STATEMENT PAYMENT                   NA
    ## 301                   <NA>    18.2    CREDIT INTEREST                   NA
    ## 302       CASH WIDTHDRAWAL     0.1  SANCTION INTEREST                   NA
    ## 303                   <NA>    15.4    CREDIT INTEREST                   NA
    ## 304       CASH WIDTHDRAWAL     0.1  SANCTION INTEREST                   NA
    ## 305       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 306                   <NA>    48.6    CREDIT INTEREST                   NA
    ## 307       CASH WIDTHDRAWAL     0.4  SANCTION INTEREST                   NA
    ## 308                   <NA>    46.8    CREDIT INTEREST                   NA
    ## 309       CASH WIDTHDRAWAL     0.4  SANCTION INTEREST                   NA
    ## 310       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 311                   <NA>   100.0    CREDIT INTEREST                   NA
    ## 312       CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 313                   <NA>    59.1    CREDIT INTEREST                   NA
    ## 314       CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 315                   <NA>   103.6    CREDIT INTEREST                   NA
    ## 316       CASH WIDTHDRAWAL     0.1  SANCTION INTEREST                   NA
    ## 317       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 318                   <NA>   100.7    CREDIT INTEREST                   NA
    ## 319       CASH WIDTHDRAWAL     0.4  SANCTION INTEREST                   NA
    ## 320                   <NA>    97.8    CREDIT INTEREST                   NA
    ## 321       CASH WIDTHDRAWAL     0.4  SANCTION INTEREST                   NA
    ## 322                   <NA>   115.3    CREDIT INTEREST                   NA
    ## 323       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 324                   <NA>    21.2    CREDIT INTEREST                   NA
    ## 325       CASH WIDTHDRAWAL     0.0  SANCTION INTEREST                   NA
    ## 326                   <NA>    13.7    CREDIT INTEREST                   NA
    ## 327       CASH WIDTHDRAWAL     0.0  SANCTION INTEREST                   NA
    ## 328                   <NA>    36.5    CREDIT INTEREST                   NA
    ## 329       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 330                   <NA>    15.0    CREDIT INTEREST                   NA
    ## 331                   <NA>    14.4    CREDIT INTEREST                   NA
    ## 332       CASH WIDTHDRAWAL   100.0  STATEMENT PAYMENT                   NA
    ## 333                   <NA>    99.7    CREDIT INTEREST                   NA
    ## 334       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 335                   <NA>    99.1    CREDIT INTEREST                   NA
    ## 336       CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 337                   <NA>    91.5    CREDIT INTEREST                   NA
    ## 338       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 339                   <NA>    14.1    CREDIT INTEREST                   NA
    ## 340                   <NA>    14.1    CREDIT INTEREST                   NA
    ## 341       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 342                   <NA>     0.6    CREDIT INTEREST                   NA
    ## 343       CASH WIDTHDRAWAL     0.2  SANCTION INTEREST                   NA
    ## 344       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 345                   <NA>    66.7    CREDIT INTEREST                   NA
    ## 346       CASH WIDTHDRAWAL     0.1  SANCTION INTEREST                   NA
    ## 347                   <NA>    25.1    CREDIT INTEREST                   NA
    ## 348       CASH WIDTHDRAWAL     0.1  SANCTION INTEREST                   NA
    ## 349       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 350                   <NA>    14.6    CREDIT INTEREST                   NA
    ## 351                   <NA>     4.6    CREDIT INTEREST                   NA
    ## 352       CASH WIDTHDRAWAL    30.0  STATEMENT PAYMENT                   NA
    ## 353                   <NA>    25.4    CREDIT INTEREST                   NA
    ## 354                   <NA>    23.7    CREDIT INTEREST                   NA
    ## 355       CASH WIDTHDRAWAL     0.3  SANCTION INTEREST                   NA
    ## 356       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 357                   <NA>    16.3    CREDIT INTEREST                   NA
    ## 358       CASH WIDTHDRAWAL     1.3  SANCTION INTEREST                   NA
    ## 359                   <NA>    15.6    CREDIT INTEREST                   NA
    ## 360       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 361                   <NA>    42.5    CREDIT INTEREST                   NA
    ## 362       CASH WIDTHDRAWAL     0.1  SANCTION INTEREST                   NA
    ## 363                   <NA>    39.2    CREDIT INTEREST                   NA
    ## 364       CASH WIDTHDRAWAL     0.1  SANCTION INTEREST                   NA
    ## 365       CASH WIDTHDRAWAL    14.6  STATEMENT PAYMENT                   NA
    ## 366                   <NA>    51.5    CREDIT INTEREST                   NA
    ## 367       CASH WIDTHDRAWAL     0.5  SANCTION INTEREST                   NA
    ## 368                   <NA>    48.9    CREDIT INTEREST                   NA
    ##     quarter year month outgoes incomes
    ## 1   1996 Q1 1996     3       0     800
    ## 2   1996 Q1 1996     3     800       0
    ## 3   1997 Q3 1997     7      14       0
    ## 4   1997 Q3 1997     7       0      15
    ## 5   1997 Q3 1997     7       0      14
    ## 6   1998 Q3 1998     9      14       0
    ## 7   1998 Q3 1998     9       0      29
    ## 8   1998 Q3 1998     9       7       0
    ## 9   1998 Q3 1998     9       0      27
    ## 10  1998 Q3 1998     9       7       0
    ## 11  1998 Q1 1998     1      14       0
    ## 12  1998 Q1 1998     1       0     125
    ## 13  1998 Q1 1998     1       0       0
    ## 14  1998 Q1 1998     2      14       0
    ## 15  1998 Q1 1998     2       0     178
    ## 16  1998 Q1 1998     2       0       0
    ## 17  1995 Q1 1995     2      14       0
    ## 18  1995 Q1 1995     2       0      14
    ## 19  1995 Q4 1995    12      14       0
    ## 20  1995 Q4 1995    12       0      43
    ## 21  1995 Q4 1995    12       0       0
    ## 22  1995 Q2 1995     5     100       0
    ## 23  1995 Q2 1995     5       0     100
    ## 24  1998 Q1 1998     1       0     600
    ## 25  1998 Q1 1998     1     600       0
    ## 26  1997 Q4 1997    10       0    1300
    ## 27  1997 Q4 1997    10    1300       0
    ## 28  1998 Q1 1998     2      14       0
    ## 29  1998 Q1 1998     2       0       3
    ## 30  1998 Q1 1998     2       0       0
    ## 31  1997 Q2 1997     5      14       0
    ## 32  1997 Q2 1997     5       0      15
    ## 33  1998 Q3 1998     9      14       0
    ## 34  1998 Q3 1998     9       0      14
    ## 35  1995 Q4 1995    12       0     800
    ## 36  1995 Q4 1995    12     800       0
    ## 37  1994 Q3 1994     9       0    1900
    ## 38  1994 Q3 1994     9    1900       0
    ## 39  1996 Q4 1996    12      14       0
    ## 40  1996 Q4 1996    12       0      15
    ## 41  1997 Q1 1997     3      14       0
    ## 42  1997 Q1 1997     3       0      14
    ## 43  1996 Q2 1996     5      14       0
    ## 44  1996 Q2 1996     5       0      14
    ## 45  1996 Q4 1996    12      14       0
    ## 46  1996 Q4 1996    12       0       5
    ## 47  1996 Q4 1996    12       0       0
    ## 48  1996 Q1 1996     1    6300       0
    ## 49  1996 Q1 1996     1       0    6300
    ## 50  1995 Q3 1995     9    1500       0
    ## 51  1995 Q3 1995     9       0    1500
    ## 52  1996 Q3 1996     7      14       0
    ## 53  1996 Q3 1996     7       0      19
    ## 54  1996 Q3 1996     7      12       0
    ## 55  1996 Q3 1996     7       0      19
    ## 56  1996 Q3 1996     7      12       0
    ## 57  1994 Q1 1994     1    1400       0
    ## 58  1994 Q1 1994     1       0    1400
    ## 59  1997 Q1 1997     1      14       0
    ## 60  1997 Q1 1997     1       0      45
    ## 61  1997 Q1 1997     1       0       0
    ## 62  1996 Q1 1996     1       0    1300
    ## 63  1996 Q1 1996     1    1300       0
    ## 64  1996 Q3 1996     7      14       0
    ## 65  1996 Q3 1996     7       0      14
    ## 66  1996 Q3 1996     7      30       0
    ## 67  1996 Q3 1996     7       0      30
    ## 68  1997 Q1 1997     2     100       0
    ## 69  1997 Q1 1997     2       0     100
    ## 70  1994 Q4 1994    10      30       0
    ## 71  1994 Q4 1994    10       0      61
    ## 72  1994 Q4 1994    10       0       0
    ## 73  1995 Q1 1995     2    4773       0
    ## 74  1995 Q1 1995     2       1       0
    ## 75  1996 Q4 1996    11    4773       0
    ## 76  1996 Q4 1996    11       1       0
    ## 77  1998 Q1 1998     1    4773       0
    ## 78  1998 Q1 1998     1       1       0
    ## 79  1998 Q1 1998     3    4773       0
    ## 80  1998 Q1 1998     3       1       0
    ## 81  1998 Q3 1998     8      14       0
    ## 82  1998 Q3 1998     8       0      14
    ## 83  1994 Q3 1994     9     100       0
    ## 84  1994 Q3 1994     9       0      76
    ## 85  1994 Q3 1994     9       0       0
    ## 86  1993 Q3 1993     9    9300       0
    ## 87  1993 Q3 1993     9     100       0
    ## 88  1993 Q3 1993     9       0    9200
    ## 89  1993 Q3 1993     9       0      54
    ## 90  1993 Q3 1993     9       0       0
    ## 91  1998 Q1 1998     1     100       0
    ## 92  1998 Q1 1998     1       0      50
    ## 93  1998 Q1 1998     1       0       0
    ## 94  1997 Q1 1997     1    1800       0
    ## 95  1997 Q1 1997     1       0    1800
    ## 96  1997 Q1 1997     3      30       0
    ## 97  1997 Q1 1997     3       0      11
    ## 98  1997 Q1 1997     3       0       0
    ## 99  1998 Q1 1998     3    5738       0
    ## 100 1998 Q1 1998     3      30       0
    ## 101 1998 Q1 1998     3       0      99
    ## 102 1998 Q1 1998     3       0       0
    ## 103 1995 Q1 1995     1     600       0
    ## 104 1995 Q1 1995     1       0     600
    ## 105 1998 Q4 1998    11       0   27931
    ## 106 1998 Q4 1998    11      30       0
    ## 107 1998 Q4 1998    11       0      30
    ## 108 1997 Q4 1997    11     100       0
    ## 109 1997 Q4 1997    11       0       2
    ## 110 1997 Q4 1997    11       0       0
    ## 111 1998 Q3 1998     8      14       0
    ## 112 1998 Q3 1998     8     100       0
    ## 113 1998 Q3 1998     8       0     114
    ## 114 1997 Q2 1997     6      14       0
    ## 115 1997 Q2 1997     6       0      14
    ## 116 1997 Q3 1997     7      14       0
    ## 117 1997 Q3 1997     7       0       7
    ## 118 1997 Q3 1997     7       0       0
    ## 119 1998 Q2 1998     6      14       0
    ## 120 1998 Q2 1998     6       0      14
    ## 121 1997 Q3 1997     9       0   35254
    ## 122 1997 Q3 1997     9     100       0
    ## 123 1997 Q3 1997     9       0      99
    ## 124 1993 Q4 1993    12       0       0
    ## 125 1996 Q1 1996     1      14       0
    ## 126 1996 Q1 1996     1       0      15
    ## 127 1998 Q2 1998     5    3482       0
    ## 128 1998 Q2 1998     5       1       0
    ## 129 1998 Q2 1998     4      14       0
    ## 130 1998 Q2 1998     4       0      14
    ## 131 1995 Q4 1995    12      30       0
    ## 132 1995 Q4 1995    12       0     184
    ## 133 1995 Q4 1995    12       0       0
    ## 134 1998 Q1 1998     1      14       0
    ## 135 1998 Q1 1998     1       0      34
    ## 136 1998 Q1 1998     1       0       0
    ## 137 1998 Q3 1998     9      14       0
    ## 138 1998 Q3 1998     9       0      14
    ## 139 1994 Q1 1994     2      14       0
    ## 140 1994 Q1 1994     2       0      14
    ## 141 1998 Q2 1998     6      14       0
    ## 142 1998 Q2 1998     6       0      67
    ## 143 1998 Q2 1998     6      23       0
    ## 144 1998 Q2 1998     6       0      65
    ## 145 1998 Q2 1998     6      27       0
    ## 146 1996 Q3 1996     7      14       0
    ## 147 1996 Q3 1996     7       0      14
    ## 148 1998 Q3 1998     9     100       0
    ## 149 1998 Q3 1998     9       0     100
    ## 150 1998 Q1 1998     1       0     600
    ## 151 1998 Q1 1998     1     600       0
    ## 152 1997 Q4 1997    12      14       0
    ## 153 1997 Q4 1997    12       0       7
    ## 154 1997 Q4 1997    12       0       0
    ## 155 1997 Q2 1997     6       0   28400
    ## 156 1997 Q2 1997     6   28400       0
    ## 157 1996 Q3 1996     7      14       0
    ## 158 1996 Q3 1996     7       0      15
    ## 159 1995 Q1 1995     3     100       0
    ## 160 1995 Q1 1995     3       0     100
    ## 161 1997 Q1 1997     1      14       0
    ## 162 1997 Q1 1997     1       0      21
    ## 163 1997 Q1 1997     1       7       0
    ## 164 1998 Q3 1998     9      14       0
    ## 165 1998 Q3 1998     9       0      45
    ## 166 1998 Q3 1998     9       0       0
    ## 167 1998 Q4 1998    11      14       0
    ## 168 1998 Q4 1998    11       0      14
    ## 169 1997 Q4 1997    12      14       0
    ## 170 1997 Q4 1997    12       0      14
    ## 171 1998 Q1 1998     2      14       0
    ## 172 1998 Q1 1998     2       2       0
    ## 173 1998 Q1 1998     2       0      16
    ## 174 1998 Q1 1998     2       0       0
    ## 175 1998 Q3 1998     7      14       0
    ## 176 1998 Q3 1998     7       0       1
    ## 177 1998 Q3 1998     7       0       0
    ## 178 1998 Q2 1998     5      14       0
    ## 179 1998 Q2 1998     5       0      26
    ## 180 1998 Q2 1998     5       0       0
    ## 181 1995 Q2 1995     6       0   22288
    ## 182 1995 Q2 1995     6      30       0
    ## 183 1995 Q2 1995     6       0     101
    ## 184 1995 Q2 1995     6       0       0
    ## 185 1998 Q3 1998     8     700       0
    ## 186 1998 Q3 1998     8       0     700
    ## 187 1996 Q4 1996    12      14       0
    ## 188 1996 Q4 1996    12       0      36
    ## 189 1996 Q4 1996    12       0       0
    ## 190 1998 Q4 1998    10      14       0
    ## 191 1998 Q4 1998    10       0       1
    ## 192 1998 Q4 1998    10       0       0
    ## 193 1994 Q4 1994    11      14       0
    ## 194 1994 Q4 1994    11       0    8200
    ## 195 1994 Q4 1994    11       0      30
    ## 196 1994 Q4 1994    11       0       0
    ## 197 1997 Q2 1997     4     100       0
    ## 198 1997 Q2 1997     4       0     100
    ## 199 1996 Q3 1996     7     100       0
    ## 200 1996 Q3 1996     7       0      99
    ## 201 1998 Q3 1998     9      14       0
    ## 202 1998 Q3 1998     9       0      15
    ## 203 1997 Q1 1997     3       0       7
    ## 204 1997 Q1 1997     3       6       0
    ## 205 1998 Q4 1998    12       0      22
    ## 206 1998 Q4 1998    12       0       0
    ## 207 1994 Q2 1994     5      14       0
    ## 208 1994 Q2 1994     5       0      33
    ## 209 1994 Q2 1994     5       0       0
    ## 210 1995 Q1 1995     3      14       0
    ## 211 1995 Q1 1995     3       0      74
    ## 212 1995 Q1 1995     3       0       0
    ## 213 1997 Q3 1997     7      14       0
    ## 214 1997 Q3 1997     7       0       6
    ## 215 1997 Q3 1997     7       0       0
    ## 216 1997 Q3 1997     8      14       0
    ## 217 1997 Q3 1997     8       0      17
    ## 218 1997 Q3 1997     8       3       0
    ## 219 1998 Q4 1998    11      14       0
    ## 220 1998 Q4 1998    11       0      16
    ## 221 1998 Q4 1998    11       0       0
    ## 222 1995 Q1 1995     1      14       0
    ## 223 1995 Q1 1995     1       0     136
    ## 224 1995 Q1 1995     1       0       0
    ## 225 1998 Q4 1998    10      30       0
    ## 226 1998 Q4 1998    10       0      37
    ## 227 1998 Q4 1998    10       0       0
    ## 228 1997 Q3 1997     7      14       0
    ## 229 1997 Q3 1997     7       0      43
    ## 230 1997 Q3 1997     7       0       0
    ## 231 1998 Q1 1998     1      14       0
    ## 232 1998 Q1 1998     1       0      20
    ## 233 1998 Q1 1998     1       5       0
    ## 234 1998 Q4 1998    12       0      31
    ## 235 1998 Q4 1998    12       0       0
    ## 236 1997 Q4 1997    10      14       0
    ## 237 1997 Q4 1997    10       0      17
    ## 238 1997 Q4 1997    10       3       0
    ## 239 1997 Q1 1997     1     100       0
    ## 240 1997 Q1 1997     1       0    3200
    ## 241 1997 Q1 1997     1       0      59
    ## 242 1997 Q1 1997     1       0       0
    ## 243 1996 Q3 1996     9      14       0
    ## 244 1996 Q3 1996     9       0      14
    ## 245 1996 Q4 1996    10      14       0
    ## 246 1996 Q4 1996    10       0      14
    ## 247 1998 Q3 1998     9      30       0
    ## 248 1998 Q3 1998     9       0      30
    ## 249 1996 Q2 1996     6      14       0
    ## 250 1996 Q2 1996     6       0      14
    ## 251 1996 Q2 1996     6       0       0
    ## 252 1997 Q1 1997     2      14       0
    ## 253 1997 Q1 1997     2       0      17
    ## 254 1997 Q1 1997     2      10       0
    ## 255 1997 Q1 1997     2       0      17
    ## 256 1997 Q1 1997     2      10       0
    ## 257 1997 Q1 1997     2       0      25
    ## 258 1997 Q1 1997     2       8       0
    ## 259 1997 Q2 1997     4      14       0
    ## 260 1997 Q2 1997     4       0       4
    ## 261 1997 Q2 1997     4      10       0
    ## 262 1997 Q2 1997     4       0       4
    ## 263 1997 Q2 1997     4       9       0
    ## 264 1997 Q2 1997     4       0       7
    ## 265 1997 Q2 1997     4       0       0
    ## 266 1998 Q1 1998     1      14       0
    ## 267 1998 Q1 1998     1       0      30
    ## 268 1998 Q1 1998     1       0      14
    ## 269 1994 Q2 1994     6     100       0
    ## 270 1994 Q2 1994     6       0     172
    ## 271 1994 Q2 1994     6       0       0
    ## 272 1994 Q2 1994     6       0     170
    ## 273 1994 Q2 1994     6       0       0
    ## 274 1996 Q3 1996     7      14       0
    ## 275 1996 Q3 1996     7       0      15
    ## 276 1996 Q3 1996     7       0       0
    ## 277 1994 Q2 1994     4   14855       0
    ## 278 1994 Q2 1994     4      14       0
    ## 279 1994 Q2 1994     4       0     134
    ## 280 1994 Q2 1994     4       0       0
    ## 281 1994 Q2 1994     4       0     134
    ## 282 1994 Q2 1994     4       0       0
    ## 283 1994 Q3 1994     8      14       0
    ## 284 1994 Q3 1994     8       0     301
    ## 285 1994 Q3 1994     8       0       0
    ## 286 1994 Q3 1994     8       0     318
    ## 287 1998 Q3 1998     7      14       0
    ## 288 1998 Q3 1998     7       0      26
    ## 289 1998 Q3 1998     7       0      14
    ## 290 1998 Q3 1998     7       0      29
    ## 291 1997 Q1 1997     1      14       0
    ## 292 1997 Q1 1997     1       0      11
    ## 293 1997 Q1 1997     1       0       0
    ## 294 1997 Q1 1997     1       0       6
    ## 295 1997 Q1 1997     1       0       0
    ## 296 1998 Q1 1998     3      30       0
    ## 297 1998 Q1 1998     3       0       1
    ## 298 1998 Q1 1998     3       0       0
    ## 299 1998 Q1 1998     3       0       1
    ## 300 1998 Q4 1998    10      30       0
    ## 301 1998 Q4 1998    10       0      18
    ## 302 1998 Q4 1998    10       0       0
    ## 303 1998 Q4 1998    10       0      15
    ## 304 1998 Q4 1998    10       0       0
    ## 305 1997 Q2 1997     4      14       0
    ## 306 1997 Q2 1997     4       0      48
    ## 307 1997 Q2 1997     4       0       0
    ## 308 1997 Q2 1997     4       0      46
    ## 309 1997 Q2 1997     4       0       0
    ## 310 1994 Q1 1994     1      14       0
    ## 311 1994 Q1 1994     1       0     100
    ## 312 1994 Q1 1994     1       0       0
    ## 313 1994 Q1 1994     1       0      59
    ## 314 1994 Q1 1994     1       0       0
    ## 315 1994 Q1 1994     1       0     103
    ## 316 1994 Q1 1994     1       0       0
    ## 317 1996 Q4 1996    11      14       0
    ## 318 1996 Q4 1996    11       0     100
    ## 319 1996 Q4 1996    11       0       0
    ## 320 1996 Q4 1996    11       0      97
    ## 321 1996 Q4 1996    11       0       0
    ## 322 1996 Q4 1996    11       0     115
    ## 323 1997 Q1 1997     1      14       0
    ## 324 1997 Q1 1997     1       0      21
    ## 325 1997 Q1 1997     1       0       0
    ## 326 1997 Q1 1997     1       0      13
    ## 327 1997 Q1 1997     1       0       0
    ## 328 1997 Q1 1997     1       0      36
    ## 329 1998 Q4 1998    11      14       0
    ## 330 1998 Q4 1998    11       0      15
    ## 331 1998 Q4 1998    11       0      14
    ## 332 1996 Q2 1996     6     100       0
    ## 333 1996 Q2 1996     6       0      99
    ## 334 1997 Q1 1997     2      14       0
    ## 335 1997 Q1 1997     2       0      99
    ## 336 1997 Q1 1997     2       0       0
    ## 337 1997 Q1 1997     2       0      91
    ## 338 1995 Q2 1995     4      14       0
    ## 339 1995 Q2 1995     4       0      14
    ## 340 1995 Q2 1995     4       0      14
    ## 341 1996 Q2 1996     6      14       0
    ## 342 1996 Q2 1996     6       0       0
    ## 343 1996 Q2 1996     6       0       0
    ## 344 1995 Q4 1995    12      14       0
    ## 345 1995 Q4 1995    12       0      66
    ## 346 1995 Q4 1995    12       0       0
    ## 347 1995 Q4 1995    12       0      25
    ## 348 1995 Q4 1995    12       0       0
    ## 349 1998 Q2 1998     4      14       0
    ## 350 1998 Q2 1998     4       0      14
    ## 351 1998 Q2 1998     4       0       4
    ## 352 1996 Q2 1996     4      30       0
    ## 353 1996 Q2 1996     4       0      25
    ## 354 1996 Q2 1996     4       0      23
    ## 355 1996 Q2 1996     4       0       0
    ## 356 1998 Q2 1998     4      14       0
    ## 357 1998 Q2 1998     4       0      16
    ## 358 1998 Q2 1998     4       1       0
    ## 359 1998 Q2 1998     4       0      15
    ## 360 1996 Q1 1996     1      14       0
    ## 361 1996 Q1 1996     1       0      42
    ## 362 1996 Q1 1996     1       0       0
    ## 363 1996 Q1 1996     1       0      39
    ## 364 1996 Q1 1996     1       0       0
    ## 365 1997 Q1 1997     3      14       0
    ## 366 1997 Q1 1997     3       0      51
    ## 367 1997 Q1 1997     3       0       0
    ## 368 1997 Q1 1997     3       0      48

``` r
check_balance <- control %>% 
  mutate(balance_A = ceiling(balance)) %>% 
  mutate(balance_B = ceiling(balance_before_transaction)) %>% 
  select(
    account_id,
    date,
    balance_A,
    balance_B
  ) %>% 
  arrange(account_id, date)

head(check_balance)
```

    ##   account_id       date balance_A balance_B
    ## 1          9 1996-03-17     32837     32037
    ## 2          9 1996-03-17     32037     32837
    ## 3         19 1997-07-31      6098      6112
    ## 4         19 1997-07-31      6112      6097
    ## 5         19 1997-07-31      6097      6083
    ## 6         19 1998-09-30      7492      7507

``` r
rm(check_balance)

controlA <- control %>% 
  mutate(balance = ceiling(balance))

controlB <- control %>% 
  mutate(balance = ceiling(balance_before_transaction))

control_result <- controlA %>% 
  anti_join(controlB, by = c("account_id", "date", "balance"))

control_result_count <- control_result %>% 
  group_by(
    account_id,
    date
  )%>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))

head(control_result_count)
```

    ## # A tibble: 6 × 3
    ##   account_id date           n
    ##        <int> <date>     <int>
    ## 1       5442 1995-03-31     2
    ## 2         19 1997-07-31     1
    ## 3         19 1998-09-30     1
    ## 4        218 1998-02-28     1
    ## 5        245 1995-12-31     1
    ## 6        405 1998-02-28     1

Filtern und Anfügen der Zahlungen

``` r
control_result_one_count <- control_result_count %>%
  filter(n == 1)
# 
control_result_one <- control_result %>%
  semi_join(control_result_one_count, by = c("account_id", "date"))
# 
# Observationen aus control_result_one an last_transaction anfügen
last_transaction <- last_transaction %>%
  bind_rows(control_result_one)

last_transaction %>%
  group_by(
    account_id,
    date
  )%>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>% 
  head()
```

    ## # A tibble: 6 × 3
    ##   account_id date           n
    ##        <int> <date>     <int>
    ## 1          1 1995-03-24     1
    ## 2          1 1995-04-13     1
    ## 3          1 1995-04-23     1
    ## 4          1 1995-04-30     1
    ## 5          1 1995-05-13     1
    ## 6          1 1995-05-23     1

``` r
control_result_more_count <- control_result_count %>%
  filter(n > 1)
# 
control_result_more <- control_result %>%
  semi_join(control_result_more_count, by = c("account_id", "date"))
# 
control_result_last <- control_result_more %>%
  filter(trans_id == 3670868)
# 
# Observationen aus control_result_last an last_transaction anfügen
last_transaction <- last_transaction %>%
  bind_rows(control_result_last)
```

``` r
rm(control, control_result, control_result_count, control_result_more, control_result_more_count, control_result_last, control_result_one, control_result_one_count, controlA, controlB)
```

``` r
rm(transaction_helper, transaction_result, transaction_first, transaction_count)
```

``` r
rm(more_than_one, more_than_one_count, more_than_one_count_rest, more_than_one_rest, only_statement_payment)
```

Tage mit mehr als einer Transaktion pro Tag: Überprüfung

``` r
control <- df_transaction_mod %>% 
  anti_join(last_transaction, by = c("account_id", "date")) %>% 
  arrange(account_id, date)

# head(control)
control
```

    ##      balance balance_before_transaction trans_id account_id       date cashflow
    ## 1    32836.2                    32036.2     2227          9 1996-03-17       IN
    ## 2    32036.2                    32836.2     2344          9 1996-03-17      OUT
    ## 3       95.9                      110.5    65576        218 1998-01-31      OUT
    ## 4      110.5                      -15.2  3445241        218 1998-01-31       IN
    ## 5       95.7                       95.9  3445242        218 1998-01-31      OUT
    ## 6    13040.9                    13055.5    74333        245 1995-02-28      OUT
    ## 7    13055.5                    13040.8  3445268        245 1995-02-28       IN
    ## 8    25103.9                    25203.9    82813        277 1995-05-31      OUT
    ## 9    25203.9                    25103.5  3539702        277 1995-05-31       IN
    ## 10   22960.5                    22360.5    88343        297 1998-01-18       IN
    ## 11   22360.5                    22960.5    88384        297 1998-01-18      OUT
    ## 12   48618.4                    47318.4    94152        320 1997-10-18       IN
    ## 13   47318.4                    48618.4    94186        320 1997-10-18      OUT
    ## 14     238.2                      252.8   151858        507 1998-09-30      OUT
    ## 15     252.8                      238.2  3445659        507 1998-09-30       IN
    ## 16   33802.3                    33002.3   165358        553 1995-12-18       IN
    ## 17   33002.3                    33802.3   165441        553 1995-12-18      OUT
    ## 18   31253.0                    29353.0   167994        562 1994-09-18       IN
    ## 19   29353.0                    31253.0   168231        562 1994-09-18      OUT
    ## 20    -439.0                     -424.4   187952        635 1996-12-31      OUT
    ## 21    -424.4                     -430.3  3551529        635 1996-12-31       IN
    ## 22    -439.2                     -439.0  3551530        635 1996-12-31      OUT
    ## 23   22999.8                    29299.8   206510        701 1996-01-24      OUT
    ## 24   29299.8                    22999.8   206662        701 1996-01-24       IN
    ## 25   23481.6                    24981.6   231862        789 1995-09-04      OUT
    ## 26   24981.6                    23481.6   232006        789 1995-09-04       IN
    ## 27   22201.4                    23601.4   271218        929 1994-01-29      OUT
    ## 28   23601.4                    22201.4   271359        929 1994-01-29       IN
    ## 29   34794.5                    33494.5   312046       1065 1996-01-13       IN
    ## 30   33494.5                    34794.5   312132       1065 1996-01-13      OUT
    ## 31    2424.7                     2439.3   366900       1247 1996-07-31      OUT
    ## 32    2439.3                     2424.9  3446339       1247 1996-07-31       IN
    ## 33    6182.9                     6212.9   368188       1251 1996-07-31      OUT
    ## 34    6212.9                     6182.8  3571686       1251 1996-07-31       IN
    ## 35   24222.0                    24322.0   373144       1270 1997-02-28      OUT
    ## 36   24322.0                    24221.5  3572251       1270 1997-02-28       IN
    ## 37    3173.6                     3188.2   428440       1455 1998-08-31      OUT
    ## 38    3188.2                     3174.0  3446677       1455 1998-08-31       IN
    ## 39   16619.6                    18419.6   514092       1759 1997-01-19      OUT
    ## 40   18419.6                    16619.6   514163       1759 1997-01-19       IN
    ## 41     -63.1                      -33.1   524033       1787 1997-03-31      OUT
    ## 42     -33.1                      -44.7  3447265       1787 1997-03-31       IN
    ## 43     -63.4                      -63.1  3447266       1787 1997-03-31      OUT
    ## 44   16908.0                    17508.0   578402       1969 1995-01-27      OUT
    ## 45   17508.0                    16908.0   578506       1969 1995-01-27       IN
    ## 46    -642.0                     -542.0   627718       2144 1997-11-30      OUT
    ## 47    -542.0                     -544.6  3447685       2144 1997-11-30       IN
    ## 48    -642.3                     -642.0  3447686       2144 1997-11-30      OUT
    ## 49   32029.6                    32044.2   633962       2165 1998-08-31      OUT
    ## 50   31929.6                    32029.6   633982       2165 1998-08-31      OUT
    ## 51   32044.2                    31929.6  3601829       2165 1998-08-31       IN
    ## 52     724.0                      738.6   662271       2260 1997-06-30      OUT
    ## 53     738.6                      723.9  3447861       2260 1997-06-30       IN
    ## 54    -645.2                     -630.6   662272       2260 1997-07-31      OUT
    ## 55    -630.6                     -638.0  3447862       2260 1997-07-31       IN
    ## 56    -645.3                     -645.1  3447863       2260 1997-07-31      OUT
    ## 57   54063.8                    18809.8   733273       2506 1997-09-30       IN
    ## 58   54063.8                    54163.8   733421       2506 1997-09-30      OUT
    ## 59   54163.8                    54063.9  3613139       2506 1997-09-30       IN
    ## 60    6230.6                     6245.2   885897       3018 1998-04-30      OUT
    ## 61    6245.2                     6230.8  3448603       3018 1998-04-30       IN
    ## 62    6556.9                     6571.5   912153       3109 1998-01-31      OUT
    ## 63    6571.5                     6537.1  3448789       3109 1998-01-31       IN
    ## 64    6556.7                     6556.9  3448790       3109 1998-01-31      OUT
    ## 65    3383.2                     3397.8   913731       3113 1998-09-30      OUT
    ## 66    3397.8                     3383.1  3633318       3113 1998-09-30       IN
    ## 67   24039.7                    24054.3   930596       3171 1994-02-28      OUT
    ## 68   24054.3                    24040.0  3635248       3171 1994-02-28       IN
    ## 69   53249.3                    53349.3  1001479       3418 1998-09-30      OUT
    ## 70   53349.3                    53249.2  3643202       3418 1998-09-30       IN
    ## 71   54736.1                    54136.1  1009697       3449 1998-01-15       IN
    ## 72   54136.1                    54736.1  1009738       3449 1998-01-15      OUT
    ## 73  115510.0                    87110.0  1147400       3925 1997-06-08       IN
    ## 74   87110.0                   115510.0  1147667       3925 1997-06-08      OUT
    ## 75    -373.0                     -358.4  1214632       4156 1998-09-30      OUT
    ## 76    -358.4                     -404.1  3450262       4156 1998-09-30       IN
    ## 77    -373.2                     -373.1  3450263       4156 1998-09-30      OUT
    ## 78    2989.8                     3004.4  1265639       4328 1998-11-30      OUT
    ## 79    3004.4                     2989.8  3469874       4328 1998-11-30       IN
    ## 80    2428.6                     2443.2  1273934       4356 1997-12-31      OUT
    ## 81    2443.2                     2428.8  3450405       4356 1997-12-31       IN
    ## 82    -178.7                     -164.1  1399817       4768 1998-05-31      OUT
    ## 83    -164.1                     -190.3  3451064       4768 1998-05-31       IN
    ## 84    -178.9                     -178.7  3451065       4768 1998-05-31      OUT
    ## 85   36851.9                    14563.9  1466526       4995 1995-06-30       IN
    ## 86   36923.7                    36953.7  1466742       4995 1995-06-30      OUT
    ## 87   36953.7                    36851.9  3444437       4995 1995-06-30       IN
    ## 88   36923.6                    36923.7  3444438       4995 1995-06-30      OUT
    ## 89   42146.8                    42846.8  1476695       5033 1998-08-17      OUT
    ## 90   42846.8                    42146.8  1476806       5033 1998-08-17       IN
    ## 91     683.2                      697.8  1486361       5065 1996-12-31      OUT
    ## 92     697.8                      661.6  3444529       5065 1996-12-31       IN
    ## 93     683.1                      683.2  3444530       5065 1996-12-31      OUT
    ## 94    2659.0                     2673.6  1503252       5125 1998-10-31      OUT
    ## 95    2673.6                     2672.5  3451352       5125 1998-10-31       IN
    ## 96    2658.7                     2659.0  3451353       5125 1998-10-31      OUT
    ## 97    -388.7                     -374.1  1504572       5129 1994-11-30      OUT
    ## 98    -374.1                    -8574.1  1504640       5129 1994-11-30       IN
    ## 99   -8574.1                    -8604.4  3451375       5129 1994-11-30       IN
    ## 100   -388.7                     -388.7  3451376       5129 1994-11-30      OUT
    ## 101  -1292.7                    -1299.7  3451783       5366 1997-03-31       IN
    ## 102  -1299.5                    -1292.7  3451784       5366 1997-03-31      OUT
    ## 103   -454.7                     -440.1  1602997       5442 1997-07-31      OUT
    ## 104   -440.1                     -446.8  3670900       5442 1997-07-31       IN
    ## 105   -454.8                     -454.7  3670901       5442 1997-07-31      OUT
    ## 106    113.1                      127.7  1614998       5483 1995-01-31      OUT
    ## 107    127.7                       -8.5  3452230       5483 1995-01-31       IN
    ## 108    113.1                      113.1  3452231       5483 1995-01-31      OUT
    ## 109  18799.8                    18814.4  1733241       5878 1997-07-31      OUT
    ## 110  18814.4                    18770.8  3444660       5878 1997-07-31       IN
    ## 111  18799.7                    18799.8  3444661       5878 1997-07-31      OUT
    ## 112  -3262.2                    -3247.6  1787877       6064 1998-01-31      OUT
    ## 113  -3242.3                    -3262.3  3453190       6064 1998-01-31       IN
    ## 114  -3247.6                    -3242.3  3453191       6064 1998-01-31      OUT
    ## 115   3606.0                     3574.6  3453203       6064 1998-12-31       IN
    ## 116   3605.8                     3606.0  3453204       6064 1998-12-31      OUT
    ## 117   2572.6                     2587.2  1908104       6463 1996-09-30      OUT
    ## 118   2587.2                     2572.6  3453569       6463 1996-09-30       IN
    ## 119   4172.3                     4186.9  1908105       6463 1996-10-31      OUT
    ## 120   4186.9                     4172.5  3453570       6463 1996-10-31       IN
    ## 121  -9843.3                    -9828.7  2247222       7418 1997-02-28      OUT
    ## 122  -9799.8                    -9817.6  3454507       7418 1997-02-28       IN
    ## 123  -9818.6                    -9808.5  3454508       7418 1997-02-28      OUT
    ## 124  -9843.1                    -9860.9  3488155       7418 1997-02-28       IN
    ## 125  -9828.7                    -9818.6  3488156       7418 1997-02-28      OUT
    ## 126  -9817.6                    -9843.0  3682486       7418 1997-02-28       IN
    ## 127  -9808.5                    -9799.8  3682487       7418 1997-02-28      OUT
    ## 128   2266.3                     2280.9  2247224       7418 1997-04-30      OUT
    ## 129   2273.3                     2269.2  3454511       7418 1997-04-30       IN
    ## 130   2245.9                     2256.4  3454512       7418 1997-04-30      OUT
    ## 131   2269.2                     2265.1  3488159       7418 1997-04-30       IN
    ## 132   2256.4                     2266.3  3488160       7418 1997-04-30      OUT
    ## 133   2280.9                     2273.3  3682490       7418 1997-04-30       IN
    ## 134   2245.7                     2245.9  3682491       7418 1997-04-30      OUT
    ## 135  14460.4                    14560.4  2489596       8212 1994-06-30      OUT
    ## 136  14560.4                    14388.3  3455386       8212 1994-06-30       IN
    ## 137  14459.6                    14460.0  3455387       8212 1994-06-30      OUT
    ## 138  14388.3                    14218.1  3497754       8212 1994-06-30       IN
    ## 139  14460.0                    14460.4  3497755       8212 1994-06-30      OUT
    ## 140    648.8                    15503.8  2699811       8934 1994-04-30      OUT
    ## 141    634.2                      648.8  2699951       8934 1994-04-30      OUT
    ## 142  15503.8                    15369.4  3455885       8934 1994-04-30       IN
    ## 143    634.0                      634.2  3455886       8934 1994-04-30      OUT
    ## 144  15369.4                    15235.0  3505363       8934 1994-04-30       IN
    ## 145    633.7                      633.9  3505364       8934 1994-04-30      OUT
    ## 146  72681.7                    72696.3  2707565       8957 1994-08-31      OUT
    ## 147  72696.3                    72394.7  3455960       8957 1994-08-31       IN
    ## 148  72681.7                    72681.7  3455961       8957 1994-08-31      OUT
    ## 149  72394.8                    72076.5  3505591       8957 1994-08-31       IN
    ## 150   1547.0                     1577.0  2735956       9051 1998-10-31      OUT
    ## 151   1577.0                     1558.8  3456230       9051 1998-10-31       IN
    ## 152   1546.8                     1546.9  3456231       9051 1998-10-31      OUT
    ## 153   1558.9                     1543.5  3507095       9051 1998-10-31       IN
    ## 154   1546.9                     1547.0  3507096       9051 1998-10-31      OUT
    ## 155    391.9                      406.5  2820490       9337 1994-01-31      OUT
    ## 156    407.0                      307.0  3456562       9337 1994-01-31       IN
    ## 157    406.6                      406.8  3456563       9337 1994-01-31      OUT
    ## 158    307.0                      247.9  3510171       9337 1994-01-31       IN
    ## 159    406.8                      407.0  3510172       9337 1994-01-31      OUT
    ## 160    247.8                      144.2  3682690       9337 1994-01-31       IN
    ## 161    391.8                      391.9  3682691       9337 1994-01-31      OUT
    ## 162  28578.8                    28593.4  2820524       9337 1996-11-30      OUT
    ## 163  28593.4                    28492.7  3456601       9337 1996-11-30       IN
    ## 164  28578.5                    28578.9  3456602       9337 1996-11-30      OUT
    ## 165  28377.5                    28279.7  3510210       9337 1996-11-30       IN
    ## 166  28578.1                    28578.5  3510211       9337 1996-11-30      OUT
    ## 167  28492.8                    28377.5  3682728       9337 1996-11-30       IN
    ## 168   8488.3                     8502.9  2820526       9337 1997-01-31      OUT
    ## 169   8466.4                     8445.2  3456604       9337 1997-01-31       IN
    ## 170   8502.9                     8502.9  3456605       9337 1997-01-31      OUT
    ## 171   8445.2                     8431.5  3510213       9337 1997-01-31       IN
    ## 172   8488.2                     8488.2  3510214       9337 1997-01-31      OUT
    ## 173   8502.9                     8466.4  3682730       9337 1997-01-31       IN
    ## 174  11825.6                    11840.2  3055761      10131 1995-12-31      OUT
    ## 175  11815.2                    11748.5  3457526      10131 1995-12-31       IN
    ## 176  11840.2                    11840.3  3457527      10131 1995-12-31      OUT
    ## 177  11840.3                    11815.2  3518442      10131 1995-12-31       IN
    ## 178  11825.6                    11825.7  3518443      10131 1995-12-31      OUT
    ## 179   2725.8                     2740.4  3116904      10343 1998-04-30      OUT
    ## 180   2740.4                     2725.8  3457880      10343 1998-04-30       IN
    ## 181   2725.8                     2721.2  3520304      10343 1998-04-30       IN
    ## 182    783.9                      813.9  3249149      10788 1996-04-30      OUT
    ## 183    790.2                      764.8  3458359      10788 1996-04-30       IN
    ## 184    813.9                      790.2  3458360      10788 1996-04-30       IN
    ## 185    783.6                      783.9  3458361      10788 1996-04-30      OUT
    ## 186  14033.9                    14048.5  3414697      11333 1996-01-31      OUT
    ## 187  14048.6                    14006.1  3459025      11333 1996-01-31       IN
    ## 188  14048.5                    14048.6  3459026      11333 1996-01-31      OUT
    ## 189  14006.1                    13966.9  3529591      11333 1996-01-31       IN
    ## 190  14033.8                    14033.9  3529592      11333 1996-01-31      OUT
    ##                  operation  amount  characterization bank_name account_nr
    ## 1              CASH CREDIT   800.0              <NA>                   NA
    ## 2         CASH WIDTHDRAWAL   800.0              <NA>                   NA
    ## 3         CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 4                     <NA>   125.7   CREDIT INTEREST                   NA
    ## 5         CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA
    ## 6         CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 7                     <NA>    14.7   CREDIT INTEREST                   NA
    ## 8         CASH WIDTHDRAWAL   100.0 STATEMENT PAYMENT                   NA
    ## 9                     <NA>   100.4   CREDIT INTEREST                   NA
    ## 10             CASH CREDIT   600.0              <NA>                   NA
    ## 11        CASH WIDTHDRAWAL   600.0              <NA>                   NA
    ## 12             CASH CREDIT  1300.0              <NA>                   NA
    ## 13        CASH WIDTHDRAWAL  1300.0              <NA>                   NA
    ## 14        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 15                    <NA>    14.6   CREDIT INTEREST                   NA
    ## 16             CASH CREDIT   800.0              <NA>                   NA
    ## 17        CASH WIDTHDRAWAL   800.0              <NA>                   NA
    ## 18             CASH CREDIT  1900.0              <NA>                   NA
    ## 19        CASH WIDTHDRAWAL  1900.0              <NA>                   NA
    ## 20        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 21                    <NA>     5.9   CREDIT INTEREST                   NA
    ## 22        CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA
    ## 23        CASH WIDTHDRAWAL  6300.0              <NA>                   NA
    ## 24             CASH CREDIT  6300.0              <NA>                   NA
    ## 25        CASH WIDTHDRAWAL  1500.0              <NA>                    0
    ## 26             CASH CREDIT  1500.0              <NA>                   NA
    ## 27        CASH WIDTHDRAWAL  1400.0              <NA>                   NA
    ## 28             CASH CREDIT  1400.0              <NA>                   NA
    ## 29             CASH CREDIT  1300.0              <NA>                   NA
    ## 30        CASH WIDTHDRAWAL  1300.0              <NA>                   NA
    ## 31        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 32                    <NA>    14.4   CREDIT INTEREST                   NA
    ## 33        CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA
    ## 34                    <NA>    30.1   CREDIT INTEREST                   NA
    ## 35        CASH WIDTHDRAWAL   100.0 STATEMENT PAYMENT                   NA
    ## 36                    <NA>   100.5   CREDIT INTEREST                   NA
    ## 37        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 38                    <NA>    14.2   CREDIT INTEREST                   NA
    ## 39        CASH WIDTHDRAWAL  1800.0              <NA>                   NA
    ## 40             CASH CREDIT  1800.0              <NA>                   NA
    ## 41        CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA
    ## 42                    <NA>    11.6   CREDIT INTEREST                   NA
    ## 43        CASH WIDTHDRAWAL     0.3 SANCTION INTEREST                   NA
    ## 44        CASH WIDTHDRAWAL   600.0              <NA>                   NA
    ## 45             CASH CREDIT   600.0              <NA>                   NA
    ## 46        CASH WIDTHDRAWAL   100.0 STATEMENT PAYMENT                   NA
    ## 47                    <NA>     2.6   CREDIT INTEREST                   NA
    ## 48        CASH WIDTHDRAWAL     0.3 SANCTION INTEREST                   NA
    ## 49        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 50        CASH WIDTHDRAWAL   100.0              <NA>                   NA
    ## 51                    <NA>   114.6   CREDIT INTEREST                   NA
    ## 52        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 53                    <NA>    14.7   CREDIT INTEREST                   NA
    ## 54        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 55                    <NA>     7.4   CREDIT INTEREST                   NA
    ## 56        CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA
    ## 57             CASH CREDIT 35254.0              <NA>                   NA
    ## 58        CASH WIDTHDRAWAL   100.0 STATEMENT PAYMENT                   NA
    ## 59                    <NA>    99.9   CREDIT INTEREST                   NA
    ## 60        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 61                    <NA>    14.4   CREDIT INTEREST                   NA
    ## 62        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 63                    <NA>    34.4   CREDIT INTEREST                   NA
    ## 64        CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA
    ## 65        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 66                    <NA>    14.7   CREDIT INTEREST                   NA
    ## 67        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 68                    <NA>    14.3   CREDIT INTEREST                   NA
    ## 69        CASH WIDTHDRAWAL   100.0 STATEMENT PAYMENT                   NA
    ## 70                    <NA>   100.1   CREDIT INTEREST                   NA
    ## 71             CASH CREDIT   600.0              <NA>                   NA
    ## 72  CREDIT CARD WITHDRAWAL   600.0              <NA>                    0
    ## 73             CASH CREDIT 28400.0              <NA>                   NA
    ## 74        CASH WIDTHDRAWAL 28400.0              <NA>                   NA
    ## 75        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 76                    <NA>    45.7   CREDIT INTEREST                   NA
    ## 77        CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 78        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 79                    <NA>    14.6   CREDIT INTEREST                   NA
    ## 80        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 81                    <NA>    14.4   CREDIT INTEREST                   NA
    ## 82        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 83                    <NA>    26.2   CREDIT INTEREST                   NA
    ## 84        CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA
    ## 85             CASH CREDIT 22288.0              <NA>                   NA
    ## 86        CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA
    ## 87                    <NA>   101.8   CREDIT INTEREST                   NA
    ## 88        CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 89        CASH WIDTHDRAWAL   700.0              <NA>                    0
    ## 90             CASH CREDIT   700.0              <NA>                   NA
    ## 91        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 92                    <NA>    36.2   CREDIT INTEREST                   NA
    ## 93        CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 94        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 95                    <NA>     1.1   CREDIT INTEREST                   NA
    ## 96        CASH WIDTHDRAWAL     0.3 SANCTION INTEREST                   NA
    ## 97        CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 98             CASH CREDIT  8200.0              <NA>                   NA
    ## 99                    <NA>    30.3   CREDIT INTEREST                   NA
    ## 100       CASH WIDTHDRAWAL     0.0 SANCTION INTEREST                   NA
    ## 101                   <NA>     7.0   CREDIT INTEREST                   NA
    ## 102       CASH WIDTHDRAWAL     6.8 SANCTION INTEREST                   NA
    ## 103       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 104                   <NA>     6.7   CREDIT INTEREST                   NA
    ## 105       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 106       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 107                   <NA>   136.2   CREDIT INTEREST                   NA
    ## 108       CASH WIDTHDRAWAL     0.0 SANCTION INTEREST                   NA
    ## 109       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 110                   <NA>    43.6   CREDIT INTEREST                   NA
    ## 111       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 112       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 113                   <NA>    20.0   CREDIT INTEREST                   NA
    ## 114       CASH WIDTHDRAWAL     5.3 SANCTION INTEREST                   NA
    ## 115                   <NA>    31.4   CREDIT INTEREST                   NA
    ## 116       CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA
    ## 117       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 118                   <NA>    14.6   CREDIT INTEREST                   NA
    ## 119       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 120                   <NA>    14.4   CREDIT INTEREST                   NA
    ## 121       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 122                   <NA>    17.8   CREDIT INTEREST                   NA
    ## 123       CASH WIDTHDRAWAL    10.1 SANCTION INTEREST                   NA
    ## 124                   <NA>    17.8   CREDIT INTEREST                   NA
    ## 125       CASH WIDTHDRAWAL    10.1 SANCTION INTEREST                   NA
    ## 126                   <NA>    25.4   CREDIT INTEREST                   NA
    ## 127       CASH WIDTHDRAWAL     8.7 SANCTION INTEREST                   NA
    ## 128       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 129                   <NA>     4.1   CREDIT INTEREST                   NA
    ## 130       CASH WIDTHDRAWAL    10.5 SANCTION INTEREST                   NA
    ## 131                   <NA>     4.1   CREDIT INTEREST                   NA
    ## 132       CASH WIDTHDRAWAL     9.9 SANCTION INTEREST                   NA
    ## 133                   <NA>     7.6   CREDIT INTEREST                   NA
    ## 134       CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA
    ## 135       CASH WIDTHDRAWAL   100.0 STATEMENT PAYMENT                   NA
    ## 136                   <NA>   172.1   CREDIT INTEREST                   NA
    ## 137       CASH WIDTHDRAWAL     0.4 SANCTION INTEREST                   NA
    ## 138                   <NA>   170.2   CREDIT INTEREST                   NA
    ## 139       CASH WIDTHDRAWAL     0.4 SANCTION INTEREST                   NA
    ## 140       CASH WIDTHDRAWAL 14855.0              <NA>                   NA
    ## 141       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 142                   <NA>   134.4   CREDIT INTEREST                   NA
    ## 143       CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA
    ## 144                   <NA>   134.4   CREDIT INTEREST                   NA
    ## 145       CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA
    ## 146       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 147                   <NA>   301.6   CREDIT INTEREST                   NA
    ## 148       CASH WIDTHDRAWAL     0.0 SANCTION INTEREST                   NA
    ## 149                   <NA>   318.3   CREDIT INTEREST                   NA
    ## 150       CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA
    ## 151                   <NA>    18.2   CREDIT INTEREST                   NA
    ## 152       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 153                   <NA>    15.4   CREDIT INTEREST                   NA
    ## 154       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 155       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 156                   <NA>   100.0   CREDIT INTEREST                   NA
    ## 157       CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA
    ## 158                   <NA>    59.1   CREDIT INTEREST                   NA
    ## 159       CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA
    ## 160                   <NA>   103.6   CREDIT INTEREST                   NA
    ## 161       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 162       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 163                   <NA>   100.7   CREDIT INTEREST                   NA
    ## 164       CASH WIDTHDRAWAL     0.4 SANCTION INTEREST                   NA
    ## 165                   <NA>    97.8   CREDIT INTEREST                   NA
    ## 166       CASH WIDTHDRAWAL     0.4 SANCTION INTEREST                   NA
    ## 167                   <NA>   115.3   CREDIT INTEREST                   NA
    ## 168       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 169                   <NA>    21.2   CREDIT INTEREST                   NA
    ## 170       CASH WIDTHDRAWAL     0.0 SANCTION INTEREST                   NA
    ## 171                   <NA>    13.7   CREDIT INTEREST                   NA
    ## 172       CASH WIDTHDRAWAL     0.0 SANCTION INTEREST                   NA
    ## 173                   <NA>    36.5   CREDIT INTEREST                   NA
    ## 174       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 175                   <NA>    66.7   CREDIT INTEREST                   NA
    ## 176       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 177                   <NA>    25.1   CREDIT INTEREST                   NA
    ## 178       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 179       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 180                   <NA>    14.6   CREDIT INTEREST                   NA
    ## 181                   <NA>     4.6   CREDIT INTEREST                   NA
    ## 182       CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA
    ## 183                   <NA>    25.4   CREDIT INTEREST                   NA
    ## 184                   <NA>    23.7   CREDIT INTEREST                   NA
    ## 185       CASH WIDTHDRAWAL     0.3 SANCTION INTEREST                   NA
    ## 186       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 187                   <NA>    42.5   CREDIT INTEREST                   NA
    ## 188       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 189                   <NA>    39.2   CREDIT INTEREST                   NA
    ## 190       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ##     quarter year month outgoes incomes
    ## 1   1996 Q1 1996     3       0     800
    ## 2   1996 Q1 1996     3     800       0
    ## 3   1998 Q1 1998     1      14       0
    ## 4   1998 Q1 1998     1       0     125
    ## 5   1998 Q1 1998     1       0       0
    ## 6   1995 Q1 1995     2      14       0
    ## 7   1995 Q1 1995     2       0      14
    ## 8   1995 Q2 1995     5     100       0
    ## 9   1995 Q2 1995     5       0     100
    ## 10  1998 Q1 1998     1       0     600
    ## 11  1998 Q1 1998     1     600       0
    ## 12  1997 Q4 1997    10       0    1300
    ## 13  1997 Q4 1997    10    1300       0
    ## 14  1998 Q3 1998     9      14       0
    ## 15  1998 Q3 1998     9       0      14
    ## 16  1995 Q4 1995    12       0     800
    ## 17  1995 Q4 1995    12     800       0
    ## 18  1994 Q3 1994     9       0    1900
    ## 19  1994 Q3 1994     9    1900       0
    ## 20  1996 Q4 1996    12      14       0
    ## 21  1996 Q4 1996    12       0       5
    ## 22  1996 Q4 1996    12       0       0
    ## 23  1996 Q1 1996     1    6300       0
    ## 24  1996 Q1 1996     1       0    6300
    ## 25  1995 Q3 1995     9    1500       0
    ## 26  1995 Q3 1995     9       0    1500
    ## 27  1994 Q1 1994     1    1400       0
    ## 28  1994 Q1 1994     1       0    1400
    ## 29  1996 Q1 1996     1       0    1300
    ## 30  1996 Q1 1996     1    1300       0
    ## 31  1996 Q3 1996     7      14       0
    ## 32  1996 Q3 1996     7       0      14
    ## 33  1996 Q3 1996     7      30       0
    ## 34  1996 Q3 1996     7       0      30
    ## 35  1997 Q1 1997     2     100       0
    ## 36  1997 Q1 1997     2       0     100
    ## 37  1998 Q3 1998     8      14       0
    ## 38  1998 Q3 1998     8       0      14
    ## 39  1997 Q1 1997     1    1800       0
    ## 40  1997 Q1 1997     1       0    1800
    ## 41  1997 Q1 1997     3      30       0
    ## 42  1997 Q1 1997     3       0      11
    ## 43  1997 Q1 1997     3       0       0
    ## 44  1995 Q1 1995     1     600       0
    ## 45  1995 Q1 1995     1       0     600
    ## 46  1997 Q4 1997    11     100       0
    ## 47  1997 Q4 1997    11       0       2
    ## 48  1997 Q4 1997    11       0       0
    ## 49  1998 Q3 1998     8      14       0
    ## 50  1998 Q3 1998     8     100       0
    ## 51  1998 Q3 1998     8       0     114
    ## 52  1997 Q2 1997     6      14       0
    ## 53  1997 Q2 1997     6       0      14
    ## 54  1997 Q3 1997     7      14       0
    ## 55  1997 Q3 1997     7       0       7
    ## 56  1997 Q3 1997     7       0       0
    ## 57  1997 Q3 1997     9       0   35254
    ## 58  1997 Q3 1997     9     100       0
    ## 59  1997 Q3 1997     9       0      99
    ## 60  1998 Q2 1998     4      14       0
    ## 61  1998 Q2 1998     4       0      14
    ## 62  1998 Q1 1998     1      14       0
    ## 63  1998 Q1 1998     1       0      34
    ## 64  1998 Q1 1998     1       0       0
    ## 65  1998 Q3 1998     9      14       0
    ## 66  1998 Q3 1998     9       0      14
    ## 67  1994 Q1 1994     2      14       0
    ## 68  1994 Q1 1994     2       0      14
    ## 69  1998 Q3 1998     9     100       0
    ## 70  1998 Q3 1998     9       0     100
    ## 71  1998 Q1 1998     1       0     600
    ## 72  1998 Q1 1998     1     600       0
    ## 73  1997 Q2 1997     6       0   28400
    ## 74  1997 Q2 1997     6   28400       0
    ## 75  1998 Q3 1998     9      14       0
    ## 76  1998 Q3 1998     9       0      45
    ## 77  1998 Q3 1998     9       0       0
    ## 78  1998 Q4 1998    11      14       0
    ## 79  1998 Q4 1998    11       0      14
    ## 80  1997 Q4 1997    12      14       0
    ## 81  1997 Q4 1997    12       0      14
    ## 82  1998 Q2 1998     5      14       0
    ## 83  1998 Q2 1998     5       0      26
    ## 84  1998 Q2 1998     5       0       0
    ## 85  1995 Q2 1995     6       0   22288
    ## 86  1995 Q2 1995     6      30       0
    ## 87  1995 Q2 1995     6       0     101
    ## 88  1995 Q2 1995     6       0       0
    ## 89  1998 Q3 1998     8     700       0
    ## 90  1998 Q3 1998     8       0     700
    ## 91  1996 Q4 1996    12      14       0
    ## 92  1996 Q4 1996    12       0      36
    ## 93  1996 Q4 1996    12       0       0
    ## 94  1998 Q4 1998    10      14       0
    ## 95  1998 Q4 1998    10       0       1
    ## 96  1998 Q4 1998    10       0       0
    ## 97  1994 Q4 1994    11      14       0
    ## 98  1994 Q4 1994    11       0    8200
    ## 99  1994 Q4 1994    11       0      30
    ## 100 1994 Q4 1994    11       0       0
    ## 101 1997 Q1 1997     3       0       7
    ## 102 1997 Q1 1997     3       6       0
    ## 103 1997 Q3 1997     7      14       0
    ## 104 1997 Q3 1997     7       0       6
    ## 105 1997 Q3 1997     7       0       0
    ## 106 1995 Q1 1995     1      14       0
    ## 107 1995 Q1 1995     1       0     136
    ## 108 1995 Q1 1995     1       0       0
    ## 109 1997 Q3 1997     7      14       0
    ## 110 1997 Q3 1997     7       0      43
    ## 111 1997 Q3 1997     7       0       0
    ## 112 1998 Q1 1998     1      14       0
    ## 113 1998 Q1 1998     1       0      20
    ## 114 1998 Q1 1998     1       5       0
    ## 115 1998 Q4 1998    12       0      31
    ## 116 1998 Q4 1998    12       0       0
    ## 117 1996 Q3 1996     9      14       0
    ## 118 1996 Q3 1996     9       0      14
    ## 119 1996 Q4 1996    10      14       0
    ## 120 1996 Q4 1996    10       0      14
    ## 121 1997 Q1 1997     2      14       0
    ## 122 1997 Q1 1997     2       0      17
    ## 123 1997 Q1 1997     2      10       0
    ## 124 1997 Q1 1997     2       0      17
    ## 125 1997 Q1 1997     2      10       0
    ## 126 1997 Q1 1997     2       0      25
    ## 127 1997 Q1 1997     2       8       0
    ## 128 1997 Q2 1997     4      14       0
    ## 129 1997 Q2 1997     4       0       4
    ## 130 1997 Q2 1997     4      10       0
    ## 131 1997 Q2 1997     4       0       4
    ## 132 1997 Q2 1997     4       9       0
    ## 133 1997 Q2 1997     4       0       7
    ## 134 1997 Q2 1997     4       0       0
    ## 135 1994 Q2 1994     6     100       0
    ## 136 1994 Q2 1994     6       0     172
    ## 137 1994 Q2 1994     6       0       0
    ## 138 1994 Q2 1994     6       0     170
    ## 139 1994 Q2 1994     6       0       0
    ## 140 1994 Q2 1994     4   14855       0
    ## 141 1994 Q2 1994     4      14       0
    ## 142 1994 Q2 1994     4       0     134
    ## 143 1994 Q2 1994     4       0       0
    ## 144 1994 Q2 1994     4       0     134
    ## 145 1994 Q2 1994     4       0       0
    ## 146 1994 Q3 1994     8      14       0
    ## 147 1994 Q3 1994     8       0     301
    ## 148 1994 Q3 1994     8       0       0
    ## 149 1994 Q3 1994     8       0     318
    ## 150 1998 Q4 1998    10      30       0
    ## 151 1998 Q4 1998    10       0      18
    ## 152 1998 Q4 1998    10       0       0
    ## 153 1998 Q4 1998    10       0      15
    ## 154 1998 Q4 1998    10       0       0
    ## 155 1994 Q1 1994     1      14       0
    ## 156 1994 Q1 1994     1       0     100
    ## 157 1994 Q1 1994     1       0       0
    ## 158 1994 Q1 1994     1       0      59
    ## 159 1994 Q1 1994     1       0       0
    ## 160 1994 Q1 1994     1       0     103
    ## 161 1994 Q1 1994     1       0       0
    ## 162 1996 Q4 1996    11      14       0
    ## 163 1996 Q4 1996    11       0     100
    ## 164 1996 Q4 1996    11       0       0
    ## 165 1996 Q4 1996    11       0      97
    ## 166 1996 Q4 1996    11       0       0
    ## 167 1996 Q4 1996    11       0     115
    ## 168 1997 Q1 1997     1      14       0
    ## 169 1997 Q1 1997     1       0      21
    ## 170 1997 Q1 1997     1       0       0
    ## 171 1997 Q1 1997     1       0      13
    ## 172 1997 Q1 1997     1       0       0
    ## 173 1997 Q1 1997     1       0      36
    ## 174 1995 Q4 1995    12      14       0
    ## 175 1995 Q4 1995    12       0      66
    ## 176 1995 Q4 1995    12       0       0
    ## 177 1995 Q4 1995    12       0      25
    ## 178 1995 Q4 1995    12       0       0
    ## 179 1998 Q2 1998     4      14       0
    ## 180 1998 Q2 1998     4       0      14
    ## 181 1998 Q2 1998     4       0       4
    ## 182 1996 Q2 1996     4      30       0
    ## 183 1996 Q2 1996     4       0      25
    ## 184 1996 Q2 1996     4       0      23
    ## 185 1996 Q2 1996     4       0       0
    ## 186 1996 Q1 1996     1      14       0
    ## 187 1996 Q1 1996     1       0      42
    ## 188 1996 Q1 1996     1       0       0
    ## 189 1996 Q1 1996     1       0      39
    ## 190 1996 Q1 1996     1       0       0

``` r
controlA <- control 

controlB <- control %>% 
  mutate(balance = balance_before_transaction)

control_result <- controlA %>% 
  anti_join(controlB, by = c("account_id", "date", "balance"))

control_result_count <- control_result %>% 
  group_by(
    account_id,
    date
  )%>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))

head(control_result_count)
```

    ## # A tibble: 6 × 3
    ##   account_id date           n
    ##        <int> <date>     <int>
    ## 1       7418 1997-02-28     4
    ## 2       9337 1994-01-31     4
    ## 3       9337 1996-11-30     4
    ## 4      10131 1995-12-31     4
    ## 5       7418 1997-04-30     3
    ## 6       8934 1994-04-30     3

``` r
control_result_one_count <- control_result_count %>%
  filter(n == 1)
# 
control_result_one <- control_result %>%
  semi_join(control_result_one_count, by = c("account_id", "date"))
# 
# Observationen aus control_result_one an last_transaction anfügen
last_transaction <- last_transaction %>%
  bind_rows(control_result_one)

last_transaction %>%
  group_by(
    account_id,
    date
  )%>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>% 
  head()
```

    ## # A tibble: 6 × 3
    ##   account_id date           n
    ##        <int> <date>     <int>
    ## 1          1 1995-03-24     1
    ## 2          1 1995-04-13     1
    ## 3          1 1995-04-23     1
    ## 4          1 1995-04-30     1
    ## 5          1 1995-05-13     1
    ## 6          1 1995-05-23     1

``` r
control_result_more_count <- control_result_count %>%
  filter(n > 1)
# 
control_result_more <- control_result %>%
  semi_join(control_result_more_count, by = c("account_id", "date")) %>% 
  arrange(account_id, date)
```

``` r
# Filtern nach Zahlungen der Kontogebühren (dies sind die letzten Zahlungen am Tag)
only_statement_payment <- control_result_more %>%
  filter(characterization == "STATEMENT PAYMENT")
```

``` r
# Kontogebühren dem bestehenden Data Frame last_transaction anfügen

last_transaction <- last_transaction %>% 
  bind_rows(only_statement_payment)
```

``` r
rm(control, control_result, control_result_count, control_result_more, control_result_more_count, control_result_one, control_result_one_count, controlA, controlB, only_statement_payment)
```

``` r
control <- df_transaction_mod %>% 
  anti_join(last_transaction, by = c("account_id", "date")) %>% 
  arrange(account_id, date)

# head(control)
control
```

    ##     balance balance_before_transaction trans_id account_id       date cashflow
    ## 1   22960.5                    22360.5    88343        297 1998-01-18       IN
    ## 2   22360.5                    22960.5    88384        297 1998-01-18      OUT
    ## 3   48618.4                    47318.4    94152        320 1997-10-18       IN
    ## 4   47318.4                    48618.4    94186        320 1997-10-18      OUT
    ## 5   33802.3                    33002.3   165358        553 1995-12-18       IN
    ## 6   33002.3                    33802.3   165441        553 1995-12-18      OUT
    ## 7   31253.0                    29353.0   167994        562 1994-09-18       IN
    ## 8   29353.0                    31253.0   168231        562 1994-09-18      OUT
    ## 9   22999.8                    29299.8   206510        701 1996-01-24      OUT
    ## 10  29299.8                    22999.8   206662        701 1996-01-24       IN
    ## 11  23481.6                    24981.6   231862        789 1995-09-04      OUT
    ## 12  24981.6                    23481.6   232006        789 1995-09-04       IN
    ## 13  22201.4                    23601.4   271218        929 1994-01-29      OUT
    ## 14  23601.4                    22201.4   271359        929 1994-01-29       IN
    ## 15  34794.5                    33494.5   312046       1065 1996-01-13       IN
    ## 16  33494.5                    34794.5   312132       1065 1996-01-13      OUT
    ## 17  16619.6                    18419.6   514092       1759 1997-01-19      OUT
    ## 18  18419.6                    16619.6   514163       1759 1997-01-19       IN
    ## 19  16908.0                    17508.0   578402       1969 1995-01-27      OUT
    ## 20  17508.0                    16908.0   578506       1969 1995-01-27       IN
    ## 21  32029.6                    32044.2   633962       2165 1998-08-31      OUT
    ## 22  31929.6                    32029.6   633982       2165 1998-08-31      OUT
    ## 23  32044.2                    31929.6  3601829       2165 1998-08-31       IN
    ## 24  54736.1                    54136.1  1009697       3449 1998-01-15       IN
    ## 25  54136.1                    54736.1  1009738       3449 1998-01-15      OUT
    ## 26 115510.0                    87110.0  1147400       3925 1997-06-08       IN
    ## 27  87110.0                   115510.0  1147667       3925 1997-06-08      OUT
    ## 28   2989.8                     3004.4  1265639       4328 1998-11-30      OUT
    ## 29   3004.4                     2989.8  3469874       4328 1998-11-30       IN
    ## 30  36851.9                    14563.9  1466526       4995 1995-06-30       IN
    ## 31  36923.7                    36953.7  1466742       4995 1995-06-30      OUT
    ## 32  36953.7                    36851.9  3444437       4995 1995-06-30       IN
    ## 33  36923.6                    36923.7  3444438       4995 1995-06-30      OUT
    ## 34  42146.8                    42846.8  1476695       5033 1998-08-17      OUT
    ## 35  42846.8                    42146.8  1476806       5033 1998-08-17       IN
    ## 36    683.2                      697.8  1486361       5065 1996-12-31      OUT
    ## 37    697.8                      661.6  3444529       5065 1996-12-31       IN
    ## 38    683.1                      683.2  3444530       5065 1996-12-31      OUT
    ## 39   -454.7                     -440.1  1602997       5442 1997-07-31      OUT
    ## 40   -440.1                     -446.8  3670900       5442 1997-07-31       IN
    ## 41   -454.8                     -454.7  3670901       5442 1997-07-31      OUT
    ## 42  18799.8                    18814.4  1733241       5878 1997-07-31      OUT
    ## 43  18814.4                    18770.8  3444660       5878 1997-07-31       IN
    ## 44  18799.7                    18799.8  3444661       5878 1997-07-31      OUT
    ## 45   2572.6                     2587.2  1908104       6463 1996-09-30      OUT
    ## 46   2587.2                     2572.6  3453569       6463 1996-09-30       IN
    ## 47   2266.3                     2280.9  2247224       7418 1997-04-30      OUT
    ## 48   2273.3                     2269.2  3454511       7418 1997-04-30       IN
    ## 49   2245.9                     2256.4  3454512       7418 1997-04-30      OUT
    ## 50   2269.2                     2265.1  3488159       7418 1997-04-30       IN
    ## 51   2256.4                     2266.3  3488160       7418 1997-04-30      OUT
    ## 52   2280.9                     2273.3  3682490       7418 1997-04-30       IN
    ## 53   2245.7                     2245.9  3682491       7418 1997-04-30      OUT
    ## 54    648.8                    15503.8  2699811       8934 1994-04-30      OUT
    ## 55    634.2                      648.8  2699951       8934 1994-04-30      OUT
    ## 56  15503.8                    15369.4  3455885       8934 1994-04-30       IN
    ## 57    634.0                      634.2  3455886       8934 1994-04-30      OUT
    ## 58  15369.4                    15235.0  3505363       8934 1994-04-30       IN
    ## 59    633.7                      633.9  3505364       8934 1994-04-30      OUT
    ## 60   1547.0                     1577.0  2735956       9051 1998-10-31      OUT
    ## 61   1577.0                     1558.8  3456230       9051 1998-10-31       IN
    ## 62   1546.8                     1546.9  3456231       9051 1998-10-31      OUT
    ## 63   1558.9                     1543.5  3507095       9051 1998-10-31       IN
    ## 64   1546.9                     1547.0  3507096       9051 1998-10-31      OUT
    ## 65   2725.8                     2740.4  3116904      10343 1998-04-30      OUT
    ## 66   2740.4                     2725.8  3457880      10343 1998-04-30       IN
    ## 67   2725.8                     2721.2  3520304      10343 1998-04-30       IN
    ## 68    783.9                      813.9  3249149      10788 1996-04-30      OUT
    ## 69    790.2                      764.8  3458359      10788 1996-04-30       IN
    ## 70    813.9                      790.2  3458360      10788 1996-04-30       IN
    ## 71    783.6                      783.9  3458361      10788 1996-04-30      OUT
    ##                 operation  amount  characterization bank_name account_nr
    ## 1             CASH CREDIT   600.0              <NA>                   NA
    ## 2        CASH WIDTHDRAWAL   600.0              <NA>                   NA
    ## 3             CASH CREDIT  1300.0              <NA>                   NA
    ## 4        CASH WIDTHDRAWAL  1300.0              <NA>                   NA
    ## 5             CASH CREDIT   800.0              <NA>                   NA
    ## 6        CASH WIDTHDRAWAL   800.0              <NA>                   NA
    ## 7             CASH CREDIT  1900.0              <NA>                   NA
    ## 8        CASH WIDTHDRAWAL  1900.0              <NA>                   NA
    ## 9        CASH WIDTHDRAWAL  6300.0              <NA>                   NA
    ## 10            CASH CREDIT  6300.0              <NA>                   NA
    ## 11       CASH WIDTHDRAWAL  1500.0              <NA>                    0
    ## 12            CASH CREDIT  1500.0              <NA>                   NA
    ## 13       CASH WIDTHDRAWAL  1400.0              <NA>                   NA
    ## 14            CASH CREDIT  1400.0              <NA>                   NA
    ## 15            CASH CREDIT  1300.0              <NA>                   NA
    ## 16       CASH WIDTHDRAWAL  1300.0              <NA>                   NA
    ## 17       CASH WIDTHDRAWAL  1800.0              <NA>                   NA
    ## 18            CASH CREDIT  1800.0              <NA>                   NA
    ## 19       CASH WIDTHDRAWAL   600.0              <NA>                   NA
    ## 20            CASH CREDIT   600.0              <NA>                   NA
    ## 21       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 22       CASH WIDTHDRAWAL   100.0              <NA>                   NA
    ## 23                   <NA>   114.6   CREDIT INTEREST                   NA
    ## 24            CASH CREDIT   600.0              <NA>                   NA
    ## 25 CREDIT CARD WITHDRAWAL   600.0              <NA>                    0
    ## 26            CASH CREDIT 28400.0              <NA>                   NA
    ## 27       CASH WIDTHDRAWAL 28400.0              <NA>                   NA
    ## 28       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 29                   <NA>    14.6   CREDIT INTEREST                   NA
    ## 30            CASH CREDIT 22288.0              <NA>                   NA
    ## 31       CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA
    ## 32                   <NA>   101.8   CREDIT INTEREST                   NA
    ## 33       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 34       CASH WIDTHDRAWAL   700.0              <NA>                    0
    ## 35            CASH CREDIT   700.0              <NA>                   NA
    ## 36       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 37                   <NA>    36.2   CREDIT INTEREST                   NA
    ## 38       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 39       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 40                   <NA>     6.7   CREDIT INTEREST                   NA
    ## 41       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 42       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 43                   <NA>    43.6   CREDIT INTEREST                   NA
    ## 44       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 45       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 46                   <NA>    14.6   CREDIT INTEREST                   NA
    ## 47       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 48                   <NA>     4.1   CREDIT INTEREST                   NA
    ## 49       CASH WIDTHDRAWAL    10.5 SANCTION INTEREST                   NA
    ## 50                   <NA>     4.1   CREDIT INTEREST                   NA
    ## 51       CASH WIDTHDRAWAL     9.9 SANCTION INTEREST                   NA
    ## 52                   <NA>     7.6   CREDIT INTEREST                   NA
    ## 53       CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA
    ## 54       CASH WIDTHDRAWAL 14855.0              <NA>                   NA
    ## 55       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 56                   <NA>   134.4   CREDIT INTEREST                   NA
    ## 57       CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA
    ## 58                   <NA>   134.4   CREDIT INTEREST                   NA
    ## 59       CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA
    ## 60       CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA
    ## 61                   <NA>    18.2   CREDIT INTEREST                   NA
    ## 62       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 63                   <NA>    15.4   CREDIT INTEREST                   NA
    ## 64       CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA
    ## 65       CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA
    ## 66                   <NA>    14.6   CREDIT INTEREST                   NA
    ## 67                   <NA>     4.6   CREDIT INTEREST                   NA
    ## 68       CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA
    ## 69                   <NA>    25.4   CREDIT INTEREST                   NA
    ## 70                   <NA>    23.7   CREDIT INTEREST                   NA
    ## 71       CASH WIDTHDRAWAL     0.3 SANCTION INTEREST                   NA
    ##    quarter year month outgoes incomes
    ## 1  1998 Q1 1998     1       0     600
    ## 2  1998 Q1 1998     1     600       0
    ## 3  1997 Q4 1997    10       0    1300
    ## 4  1997 Q4 1997    10    1300       0
    ## 5  1995 Q4 1995    12       0     800
    ## 6  1995 Q4 1995    12     800       0
    ## 7  1994 Q3 1994     9       0    1900
    ## 8  1994 Q3 1994     9    1900       0
    ## 9  1996 Q1 1996     1    6300       0
    ## 10 1996 Q1 1996     1       0    6300
    ## 11 1995 Q3 1995     9    1500       0
    ## 12 1995 Q3 1995     9       0    1500
    ## 13 1994 Q1 1994     1    1400       0
    ## 14 1994 Q1 1994     1       0    1400
    ## 15 1996 Q1 1996     1       0    1300
    ## 16 1996 Q1 1996     1    1300       0
    ## 17 1997 Q1 1997     1    1800       0
    ## 18 1997 Q1 1997     1       0    1800
    ## 19 1995 Q1 1995     1     600       0
    ## 20 1995 Q1 1995     1       0     600
    ## 21 1998 Q3 1998     8      14       0
    ## 22 1998 Q3 1998     8     100       0
    ## 23 1998 Q3 1998     8       0     114
    ## 24 1998 Q1 1998     1       0     600
    ## 25 1998 Q1 1998     1     600       0
    ## 26 1997 Q2 1997     6       0   28400
    ## 27 1997 Q2 1997     6   28400       0
    ## 28 1998 Q4 1998    11      14       0
    ## 29 1998 Q4 1998    11       0      14
    ## 30 1995 Q2 1995     6       0   22288
    ## 31 1995 Q2 1995     6      30       0
    ## 32 1995 Q2 1995     6       0     101
    ## 33 1995 Q2 1995     6       0       0
    ## 34 1998 Q3 1998     8     700       0
    ## 35 1998 Q3 1998     8       0     700
    ## 36 1996 Q4 1996    12      14       0
    ## 37 1996 Q4 1996    12       0      36
    ## 38 1996 Q4 1996    12       0       0
    ## 39 1997 Q3 1997     7      14       0
    ## 40 1997 Q3 1997     7       0       6
    ## 41 1997 Q3 1997     7       0       0
    ## 42 1997 Q3 1997     7      14       0
    ## 43 1997 Q3 1997     7       0      43
    ## 44 1997 Q3 1997     7       0       0
    ## 45 1996 Q3 1996     9      14       0
    ## 46 1996 Q3 1996     9       0      14
    ## 47 1997 Q2 1997     4      14       0
    ## 48 1997 Q2 1997     4       0       4
    ## 49 1997 Q2 1997     4      10       0
    ## 50 1997 Q2 1997     4       0       4
    ## 51 1997 Q2 1997     4       9       0
    ## 52 1997 Q2 1997     4       0       7
    ## 53 1997 Q2 1997     4       0       0
    ## 54 1994 Q2 1994     4   14855       0
    ## 55 1994 Q2 1994     4      14       0
    ## 56 1994 Q2 1994     4       0     134
    ## 57 1994 Q2 1994     4       0       0
    ## 58 1994 Q2 1994     4       0     134
    ## 59 1994 Q2 1994     4       0       0
    ## 60 1998 Q4 1998    10      30       0
    ## 61 1998 Q4 1998    10       0      18
    ## 62 1998 Q4 1998    10       0       0
    ## 63 1998 Q4 1998    10       0      15
    ## 64 1998 Q4 1998    10       0       0
    ## 65 1998 Q2 1998     4      14       0
    ## 66 1998 Q2 1998     4       0      14
    ## 67 1998 Q2 1998     4       0       4
    ## 68 1996 Q2 1996     4      30       0
    ## 69 1996 Q2 1996     4       0      25
    ## 70 1996 Q2 1996     4       0      23
    ## 71 1996 Q2 1996     4       0       0

outgoes und incomes summiert auf den Tag

``` r
outgoes_incomes_sum <- control %>% 
  select(
    account_id,
    date, 
    cashflow,
    amount
  ) %>%
  group_by(
    account_id,
    date,
    cashflow
  ) %>%
  summarise(amount_sum = sum(amount)) %>% 
  pivot_wider(names_from = cashflow, values_from = amount_sum) %>% 
  na.replace(., 0) %>% 
  rename(outgoes = OUT,
         incomes = IN)
  
 head(outgoes_incomes_sum)
```

    ## # A tibble: 6 × 4
    ## # Groups:   account_id, date [6]
    ##   account_id date       incomes outgoes
    ##        <int> <date>       <dbl>   <dbl>
    ## 1        297 1998-01-18     600     600
    ## 2        320 1997-10-18    1300    1300
    ## 3        553 1995-12-18     800     800
    ## 4        562 1994-09-18    1900    1900
    ## 5        701 1996-01-24    6300    6300
    ## 6        789 1995-09-04    1500    1500

``` r
 # rm(control)
```

Bei den meisten Transaktionen handelt es sich um Zahlungen in jeweils
gleicher Höhe, sodass zu Beginn des Tages und am Ende des Tages wieder
derselbe Kontostand wie am Vortag herrscht. Es gibt aber auch Tage, an
denen IN und OUT nicht gleich hoch sind. Diese werden genauer
untersucht.

Unterschiedliches IN und OUT

``` r
outgoes_incomes_not_equal <- outgoes_incomes_sum %>% 
  filter(outgoes != incomes)
 
head(outgoes_incomes_not_equal)
```

    ## # A tibble: 6 × 4
    ## # Groups:   account_id, date [6]
    ##   account_id date       incomes outgoes
    ##        <int> <date>       <dbl>   <dbl>
    ## 1       4995 1995-06-30 22390.     30.1
    ## 2       5065 1996-12-31    36.2    14.7
    ## 3       5442 1997-07-31     6.7    14.7
    ## 4       5878 1997-07-31    43.6    14.7
    ## 5       7418 1997-04-30    15.8    35.2
    ## 6       8934 1994-04-30   269.  14870

``` r
# rm(outgoes_incomes_sum)
```

``` r
amount_not_equal <- df_transaction_mod %>%
  semi_join(outgoes_incomes_not_equal, by = c("account_id", "date")) %>% 
  arrange(account_id, date)

head(amount_not_equal)
```

    ##   balance balance_before_transaction trans_id account_id       date cashflow
    ## 1 36851.9                    14563.9  1466526       4995 1995-06-30       IN
    ## 2 36923.7                    36953.7  1466742       4995 1995-06-30      OUT
    ## 3 36953.7                    36851.9  3444437       4995 1995-06-30       IN
    ## 4 36923.6                    36923.7  3444438       4995 1995-06-30      OUT
    ## 5   683.2                      697.8  1486361       5065 1996-12-31      OUT
    ## 6   697.8                      661.6  3444529       5065 1996-12-31       IN
    ##          operation  amount  characterization bank_name account_nr quarter year
    ## 1      CASH CREDIT 22288.0              <NA>                   NA 1995 Q2 1995
    ## 2 CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA 1995 Q2 1995
    ## 3             <NA>   101.8   CREDIT INTEREST                   NA 1995 Q2 1995
    ## 4 CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA 1995 Q2 1995
    ## 5 CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA 1996 Q4 1996
    ## 6             <NA>    36.2   CREDIT INTEREST                   NA 1996 Q4 1996
    ##   month outgoes incomes
    ## 1     6       0   22288
    ## 2     6      30       0
    ## 3     6       0     101
    ## 4     6       0       0
    ## 5    12      14       0
    ## 6    12       0      36

Die letzten Zahlungen können von Hand gefiltert werden und dann dem
gesamten DF angefügt werden. ev. kommt mir noch eine bessere Idee

``` r
amount_not_equal
```

    ##    balance balance_before_transaction trans_id account_id       date cashflow
    ## 1  36851.9                    14563.9  1466526       4995 1995-06-30       IN
    ## 2  36923.7                    36953.7  1466742       4995 1995-06-30      OUT
    ## 3  36953.7                    36851.9  3444437       4995 1995-06-30       IN
    ## 4  36923.6                    36923.7  3444438       4995 1995-06-30      OUT
    ## 5    683.2                      697.8  1486361       5065 1996-12-31      OUT
    ## 6    697.8                      661.6  3444529       5065 1996-12-31       IN
    ## 7    683.1                      683.2  3444530       5065 1996-12-31      OUT
    ## 8   -454.7                     -440.1  1602997       5442 1997-07-31      OUT
    ## 9   -440.1                     -446.8  3670900       5442 1997-07-31       IN
    ## 10  -454.8                     -454.7  3670901       5442 1997-07-31      OUT
    ## 11 18799.8                    18814.4  1733241       5878 1997-07-31      OUT
    ## 12 18814.4                    18770.8  3444660       5878 1997-07-31       IN
    ## 13 18799.7                    18799.8  3444661       5878 1997-07-31      OUT
    ## 14  2266.3                     2280.9  2247224       7418 1997-04-30      OUT
    ## 15  2273.3                     2269.2  3454511       7418 1997-04-30       IN
    ## 16  2245.9                     2256.4  3454512       7418 1997-04-30      OUT
    ## 17  2269.2                     2265.1  3488159       7418 1997-04-30       IN
    ## 18  2256.4                     2266.3  3488160       7418 1997-04-30      OUT
    ## 19  2280.9                     2273.3  3682490       7418 1997-04-30       IN
    ## 20  2245.7                     2245.9  3682491       7418 1997-04-30      OUT
    ## 21   648.8                    15503.8  2699811       8934 1994-04-30      OUT
    ## 22   634.2                      648.8  2699951       8934 1994-04-30      OUT
    ## 23 15503.8                    15369.4  3455885       8934 1994-04-30       IN
    ## 24   634.0                      634.2  3455886       8934 1994-04-30      OUT
    ## 25 15369.4                    15235.0  3505363       8934 1994-04-30       IN
    ## 26   633.7                      633.9  3505364       8934 1994-04-30      OUT
    ## 27  1547.0                     1577.0  2735956       9051 1998-10-31      OUT
    ## 28  1577.0                     1558.8  3456230       9051 1998-10-31       IN
    ## 29  1546.8                     1546.9  3456231       9051 1998-10-31      OUT
    ## 30  1558.9                     1543.5  3507095       9051 1998-10-31       IN
    ## 31  1546.9                     1547.0  3507096       9051 1998-10-31      OUT
    ## 32  2725.8                     2740.4  3116904      10343 1998-04-30      OUT
    ## 33  2740.4                     2725.8  3457880      10343 1998-04-30       IN
    ## 34  2725.8                     2721.2  3520304      10343 1998-04-30       IN
    ## 35   783.9                      813.9  3249149      10788 1996-04-30      OUT
    ## 36   790.2                      764.8  3458359      10788 1996-04-30       IN
    ## 37   813.9                      790.2  3458360      10788 1996-04-30       IN
    ## 38   783.6                      783.9  3458361      10788 1996-04-30      OUT
    ##           operation  amount  characterization bank_name account_nr quarter year
    ## 1       CASH CREDIT 22288.0              <NA>                   NA 1995 Q2 1995
    ## 2  CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA 1995 Q2 1995
    ## 3              <NA>   101.8   CREDIT INTEREST                   NA 1995 Q2 1995
    ## 4  CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA 1995 Q2 1995
    ## 5  CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA 1996 Q4 1996
    ## 6              <NA>    36.2   CREDIT INTEREST                   NA 1996 Q4 1996
    ## 7  CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA 1996 Q4 1996
    ## 8  CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA 1997 Q3 1997
    ## 9              <NA>     6.7   CREDIT INTEREST                   NA 1997 Q3 1997
    ## 10 CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA 1997 Q3 1997
    ## 11 CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA 1997 Q3 1997
    ## 12             <NA>    43.6   CREDIT INTEREST                   NA 1997 Q3 1997
    ## 13 CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA 1997 Q3 1997
    ## 14 CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA 1997 Q2 1997
    ## 15             <NA>     4.1   CREDIT INTEREST                   NA 1997 Q2 1997
    ## 16 CASH WIDTHDRAWAL    10.5 SANCTION INTEREST                   NA 1997 Q2 1997
    ## 17             <NA>     4.1   CREDIT INTEREST                   NA 1997 Q2 1997
    ## 18 CASH WIDTHDRAWAL     9.9 SANCTION INTEREST                   NA 1997 Q2 1997
    ## 19             <NA>     7.6   CREDIT INTEREST                   NA 1997 Q2 1997
    ## 20 CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA 1997 Q2 1997
    ## 21 CASH WIDTHDRAWAL 14855.0              <NA>                   NA 1994 Q2 1994
    ## 22 CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA 1994 Q2 1994
    ## 23             <NA>   134.4   CREDIT INTEREST                   NA 1994 Q2 1994
    ## 24 CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA 1994 Q2 1994
    ## 25             <NA>   134.4   CREDIT INTEREST                   NA 1994 Q2 1994
    ## 26 CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA 1994 Q2 1994
    ## 27 CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA 1998 Q4 1998
    ## 28             <NA>    18.2   CREDIT INTEREST                   NA 1998 Q4 1998
    ## 29 CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA 1998 Q4 1998
    ## 30             <NA>    15.4   CREDIT INTEREST                   NA 1998 Q4 1998
    ## 31 CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA 1998 Q4 1998
    ## 32 CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA 1998 Q2 1998
    ## 33             <NA>    14.6   CREDIT INTEREST                   NA 1998 Q2 1998
    ## 34             <NA>     4.6   CREDIT INTEREST                   NA 1998 Q2 1998
    ## 35 CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA 1996 Q2 1996
    ## 36             <NA>    25.4   CREDIT INTEREST                   NA 1996 Q2 1996
    ## 37             <NA>    23.7   CREDIT INTEREST                   NA 1996 Q2 1996
    ## 38 CASH WIDTHDRAWAL     0.3 SANCTION INTEREST                   NA 1996 Q2 1996
    ##    month outgoes incomes
    ## 1      6       0   22288
    ## 2      6      30       0
    ## 3      6       0     101
    ## 4      6       0       0
    ## 5     12      14       0
    ## 6     12       0      36
    ## 7     12       0       0
    ## 8      7      14       0
    ## 9      7       0       6
    ## 10     7       0       0
    ## 11     7      14       0
    ## 12     7       0      43
    ## 13     7       0       0
    ## 14     4      14       0
    ## 15     4       0       4
    ## 16     4      10       0
    ## 17     4       0       4
    ## 18     4       9       0
    ## 19     4       0       7
    ## 20     4       0       0
    ## 21     4   14855       0
    ## 22     4      14       0
    ## 23     4       0     134
    ## 24     4       0       0
    ## 25     4       0     134
    ## 26     4       0       0
    ## 27    10      30       0
    ## 28    10       0      18
    ## 29    10       0       0
    ## 30    10       0      15
    ## 31    10       0       0
    ## 32     4      14       0
    ## 33     4       0      14
    ## 34     4       0       4
    ## 35     4      30       0
    ## 36     4       0      25
    ## 37     4       0      23
    ## 38     4       0       0

#### Hilfe in Excel

DF in Excel rausschreiben lassen und dort die trans_id rauslesen

``` r
write.csv(amount_not_equal, file = "foo.csv")
foo <- read.csv("foo.csv")
```

``` r
transid <- df_transaction_mod %>% 
  semi_join(foo, by = "trans_id")

transid
```

    ##    balance balance_before_transaction trans_id account_id       date cashflow
    ## 1  36851.9                    14563.9  1466526       4995 1995-06-30       IN
    ## 2  36923.7                    36953.7  1466742       4995 1995-06-30      OUT
    ## 3    683.2                      697.8  1486361       5065 1996-12-31      OUT
    ## 4   -454.7                     -440.1  1602997       5442 1997-07-31      OUT
    ## 5  18799.8                    18814.4  1733241       5878 1997-07-31      OUT
    ## 6   2266.3                     2280.9  2247224       7418 1997-04-30      OUT
    ## 7    648.8                    15503.8  2699811       8934 1994-04-30      OUT
    ## 8    634.2                      648.8  2699951       8934 1994-04-30      OUT
    ## 9   1547.0                     1577.0  2735956       9051 1998-10-31      OUT
    ## 10  2725.8                     2740.4  3116904      10343 1998-04-30      OUT
    ## 11   783.9                      813.9  3249149      10788 1996-04-30      OUT
    ## 12 36953.7                    36851.9  3444437       4995 1995-06-30       IN
    ## 13 36923.6                    36923.7  3444438       4995 1995-06-30      OUT
    ## 14   697.8                      661.6  3444529       5065 1996-12-31       IN
    ## 15   683.1                      683.2  3444530       5065 1996-12-31      OUT
    ## 16 18814.4                    18770.8  3444660       5878 1997-07-31       IN
    ## 17 18799.7                    18799.8  3444661       5878 1997-07-31      OUT
    ## 18  2273.3                     2269.2  3454511       7418 1997-04-30       IN
    ## 19  2245.9                     2256.4  3454512       7418 1997-04-30      OUT
    ## 20 15503.8                    15369.4  3455885       8934 1994-04-30       IN
    ## 21   634.0                      634.2  3455886       8934 1994-04-30      OUT
    ## 22  1577.0                     1558.8  3456230       9051 1998-10-31       IN
    ## 23  1546.8                     1546.9  3456231       9051 1998-10-31      OUT
    ## 24  2740.4                     2725.8  3457880      10343 1998-04-30       IN
    ## 25   790.2                      764.8  3458359      10788 1996-04-30       IN
    ## 26   813.9                      790.2  3458360      10788 1996-04-30       IN
    ## 27   783.6                      783.9  3458361      10788 1996-04-30      OUT
    ## 28  2269.2                     2265.1  3488159       7418 1997-04-30       IN
    ## 29  2256.4                     2266.3  3488160       7418 1997-04-30      OUT
    ## 30 15369.4                    15235.0  3505363       8934 1994-04-30       IN
    ## 31   633.7                      633.9  3505364       8934 1994-04-30      OUT
    ## 32  1558.9                     1543.5  3507095       9051 1998-10-31       IN
    ## 33  1546.9                     1547.0  3507096       9051 1998-10-31      OUT
    ## 34  2725.8                     2721.2  3520304      10343 1998-04-30       IN
    ## 35  -440.1                     -446.8  3670900       5442 1997-07-31       IN
    ## 36  -454.8                     -454.7  3670901       5442 1997-07-31      OUT
    ## 37  2280.9                     2273.3  3682490       7418 1997-04-30       IN
    ## 38  2245.7                     2245.9  3682491       7418 1997-04-30      OUT
    ##           operation  amount  characterization bank_name account_nr quarter year
    ## 1       CASH CREDIT 22288.0              <NA>                   NA 1995 Q2 1995
    ## 2  CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA 1995 Q2 1995
    ## 3  CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA 1996 Q4 1996
    ## 4  CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA 1997 Q3 1997
    ## 5  CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA 1997 Q3 1997
    ## 6  CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA 1997 Q2 1997
    ## 7  CASH WIDTHDRAWAL 14855.0              <NA>                   NA 1994 Q2 1994
    ## 8  CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA 1994 Q2 1994
    ## 9  CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA 1998 Q4 1998
    ## 10 CASH WIDTHDRAWAL    14.6 STATEMENT PAYMENT                   NA 1998 Q2 1998
    ## 11 CASH WIDTHDRAWAL    30.0 STATEMENT PAYMENT                   NA 1996 Q2 1996
    ## 12             <NA>   101.8   CREDIT INTEREST                   NA 1995 Q2 1995
    ## 13 CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA 1995 Q2 1995
    ## 14             <NA>    36.2   CREDIT INTEREST                   NA 1996 Q4 1996
    ## 15 CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA 1996 Q4 1996
    ## 16             <NA>    43.6   CREDIT INTEREST                   NA 1997 Q3 1997
    ## 17 CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA 1997 Q3 1997
    ## 18             <NA>     4.1   CREDIT INTEREST                   NA 1997 Q2 1997
    ## 19 CASH WIDTHDRAWAL    10.5 SANCTION INTEREST                   NA 1997 Q2 1997
    ## 20             <NA>   134.4   CREDIT INTEREST                   NA 1994 Q2 1994
    ## 21 CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA 1994 Q2 1994
    ## 22             <NA>    18.2   CREDIT INTEREST                   NA 1998 Q4 1998
    ## 23 CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA 1998 Q4 1998
    ## 24             <NA>    14.6   CREDIT INTEREST                   NA 1998 Q2 1998
    ## 25             <NA>    25.4   CREDIT INTEREST                   NA 1996 Q2 1996
    ## 26             <NA>    23.7   CREDIT INTEREST                   NA 1996 Q2 1996
    ## 27 CASH WIDTHDRAWAL     0.3 SANCTION INTEREST                   NA 1996 Q2 1996
    ## 28             <NA>     4.1   CREDIT INTEREST                   NA 1997 Q2 1997
    ## 29 CASH WIDTHDRAWAL     9.9 SANCTION INTEREST                   NA 1997 Q2 1997
    ## 30             <NA>   134.4   CREDIT INTEREST                   NA 1994 Q2 1994
    ## 31 CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA 1994 Q2 1994
    ## 32             <NA>    15.4   CREDIT INTEREST                   NA 1998 Q4 1998
    ## 33 CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA 1998 Q4 1998
    ## 34             <NA>     4.6   CREDIT INTEREST                   NA 1998 Q2 1998
    ## 35             <NA>     6.7   CREDIT INTEREST                   NA 1997 Q3 1997
    ## 36 CASH WIDTHDRAWAL     0.1 SANCTION INTEREST                   NA 1997 Q3 1997
    ## 37             <NA>     7.6   CREDIT INTEREST                   NA 1997 Q2 1997
    ## 38 CASH WIDTHDRAWAL     0.2 SANCTION INTEREST                   NA 1997 Q2 1997
    ##    month outgoes incomes
    ## 1      6       0   22288
    ## 2      6      30       0
    ## 3     12      14       0
    ## 4      7      14       0
    ## 5      7      14       0
    ## 6      4      14       0
    ## 7      4   14855       0
    ## 8      4      14       0
    ## 9     10      30       0
    ## 10     4      14       0
    ## 11     4      30       0
    ## 12     6       0     101
    ## 13     6       0       0
    ## 14    12       0      36
    ## 15    12       0       0
    ## 16     7       0      43
    ## 17     7       0       0
    ## 18     4       0       4
    ## 19     4      10       0
    ## 20     4       0     134
    ## 21     4       0       0
    ## 22    10       0      18
    ## 23    10       0       0
    ## 24     4       0      14
    ## 25     4       0      25
    ## 26     4       0      23
    ## 27     4       0       0
    ## 28     4       0       4
    ## 29     4       9       0
    ## 30     4       0     134
    ## 31     4       0       0
    ## 32    10       0      15
    ## 33    10       0       0
    ## 34     4       0       4
    ## 35     7       0       6
    ## 36     7       0       0
    ## 37     4       0       7
    ## 38     4       0       0

``` r
last_transaction %>% 
  group_by(account_id, date) %>% 
  count() %>% 
  arrange(desc(n))
```

    ## # A tibble: 814,743 × 3
    ## # Groups:   account_id, date [814,743]
    ##    account_id date           n
    ##         <int> <date>     <int>
    ##  1          1 1995-03-24     1
    ##  2          1 1995-04-13     1
    ##  3          1 1995-04-23     1
    ##  4          1 1995-04-30     1
    ##  5          1 1995-05-13     1
    ##  6          1 1995-05-23     1
    ##  7          1 1995-05-31     1
    ##  8          1 1995-06-13     1
    ##  9          1 1995-06-22     1
    ## 10          1 1995-06-30     1
    ## # ℹ 814,733 more rows

``` r
last_transaction <- last_transaction %>% 
  mutate(balance_end_day = balance) %>% 
  select(
    trans_id,
    balance_end_day
  )

last_transaction_all <- df_transaction_mod %>% 
  left_join(last_transaction, by = "trans_id") %>% 
  select(
    trans_id,
    balance_end_day
  ) 

summary(last_transaction_all)
```

    ##     trans_id       balance_end_day 
    ##  Min.   :      1   Min.   :-41126  
    ##  1st Qu.: 430263   1st Qu.: 22275  
    ##  Median : 858506   Median : 32865  
    ##  Mean   :1335311   Mean   : 37750  
    ##  3rd Qu.:2060979   3rd Qu.: 49003  
    ##  Max.   :3682987   Max.   :162192  
    ##                    NA's   :241577

Die NA’s in balance_end_day entstanden bei den Transaktionen, welche an
Tagen mit Mehrfachzahlungen vor der letzten Zahlung getätigt wurden oder
an Tagen, bei denen der Kontostand am Vortag gleich hoch (outgoes und
Kredit gleich hoch).

Für die Tage mit gleich hohem outgoes und Kredit wird eine entsprechende
Spalte hinzugefügt. Dies aber erst, wenn die Kontoendstände dem gesamten
df_transaction_mod angefügt wurde.

Anfügen der Tagessaldi an das bestehende df_transaction_mod

``` r
# unnötige Spalen entfernen und neue Informationen anfügen 
df_transaction_mod <- df_transaction_mod %>% 
  select(balance:account_nr) %>% 
  select(-c(balance_before_transaction)) %>% 
  left_join(last_transaction_all, by = "trans_id") %>% 
  arrange(account_id, date) %>% 
  relocate(starts_with("balance"), .after = date)
```

Entfernen nicht mehr benötigter Data Frames aus dem Global Environment

``` r
rm(amount_not_equal, ceiling_last_result, control, outgoes_incomes_not_equal, outgoes_incomes_sum, foo, last_transaction, last_transaction_all, transid)
```

#### Neue Spalten:

outgoes und incomes pro Tag

Die vorangegangenen Analysen haben gezeigt, dass es Tage gibt, an denen
outgoes und incomes gleich hoch sind. Um die Umsätze eines Kontos zu
sehen werden nun zwei zusätzliche Spalten erstellt mit der outgoes- und
incomes-summe des jeweiligen Tages. Dieses Ergebnis wird dann auch
wieder dem df_transaction_mod angefügt.

``` r
outgoes_incomes_sum <- df_transaction_mod %>% 
  select(
    account_id,
    date, 
    cashflow,
    amount
  ) %>%
  group_by(
    account_id,
    date,
    cashflow
  ) %>%
  summarise(amount_sum = sum(amount)) %>% 
  pivot_wider(names_from = cashflow, values_from = amount_sum) %>% 
  na.replace(., 0) %>% 
  rename(outgoes = OUT,
         incomes = IN)
  
 head(outgoes_incomes_sum)
```

    ## # A tibble: 6 × 4
    ## # Groups:   account_id, date [6]
    ##   account_id date       incomes outgoes
    ##        <int> <date>       <dbl>   <dbl>
    ## 1          1 1995-03-24  1000         0
    ## 2          1 1995-04-13  3679         0
    ## 3          1 1995-04-23 12600         0
    ## 4          1 1995-04-30    19.2       0
    ## 5          1 1995-05-13  3679         0
    ## 6          1 1995-05-23  2100         0

Gleich hohe incomes und outgoes:

Um die gleich hohen Beträge in outgoes und Kredit am selben Tag für das
gleiche Konto schnell zu finden, wird eine zusätzliche Spalte erstellt.

``` r
outgoes_incomes_sum  <- outgoes_incomes_sum %>% 
  mutate(outgoes_incomes_equal = ifelse(outgoes == incomes, TRUE, FALSE))

head(outgoes_incomes_sum)
```

    ## # A tibble: 6 × 5
    ## # Groups:   account_id, date [6]
    ##   account_id date       incomes outgoes outgoes_incomes_equal
    ##        <int> <date>       <dbl>   <dbl> <lgl>                
    ## 1          1 1995-03-24  1000         0 FALSE                
    ## 2          1 1995-04-13  3679         0 FALSE                
    ## 3          1 1995-04-23 12600         0 FALSE                
    ## 4          1 1995-04-30    19.2       0 FALSE                
    ## 5          1 1995-05-13  3679         0 FALSE                
    ## 6          1 1995-05-23  2100         0 FALSE

Überprüfen, wieviele Tage gleich hohe outgoes und Kredit haben.

``` r
outgoes_incomes_sum %>% 
  filter(outgoes_incomes_equal == TRUE) %>% 
  head()
```

    ## # A tibble: 6 × 5
    ## # Groups:   account_id, date [6]
    ##   account_id date       incomes outgoes outgoes_incomes_equal
    ##        <int> <date>       <dbl>   <dbl> <lgl>                
    ## 1          9 1996-03-17   800     800   TRUE                 
    ## 2        297 1998-01-18   600     600   TRUE                 
    ## 3        320 1997-10-18  1300    1300   TRUE                 
    ## 4        507 1998-09-30    14.6    14.6 TRUE                 
    ## 5        553 1995-12-18   800     800   TRUE                 
    ## 6        562 1994-09-18  1900    1900   TRUE

Dies stimmt mit den vorangegangenen Analysen überein. Nun werden die
neuen Informationen an das gesamte Data Frame Transaction angefügt.

``` r
df_transaction_mod <- df_transaction_mod %>% 
  left_join(outgoes_incomes_sum, by = c("account_id", "date")) %>% 
  arrange(account_id, date)

df_transaction_mod %>% 
  filter(outgoes_incomes_equal == TRUE) %>% 
  head()
```

    ##   trans_id account_id       date balance balance_end_day cashflow
    ## 1     2227          9 1996-03-17 32836.2              NA       IN
    ## 2     2344          9 1996-03-17 32036.2         32036.2      OUT
    ## 3    88343        297 1998-01-18 22960.5              NA       IN
    ## 4    88384        297 1998-01-18 22360.5              NA      OUT
    ## 5    94152        320 1997-10-18 48618.4              NA       IN
    ## 6    94186        320 1997-10-18 47318.4              NA      OUT
    ##          operation amount characterization bank_name account_nr incomes outgoes
    ## 1      CASH CREDIT    800             <NA>                   NA     800     800
    ## 2 CASH WIDTHDRAWAL    800             <NA>                   NA     800     800
    ## 3      CASH CREDIT    600             <NA>                   NA     600     600
    ## 4 CASH WIDTHDRAWAL    600             <NA>                   NA     600     600
    ## 5      CASH CREDIT   1300             <NA>                   NA    1300    1300
    ## 6 CASH WIDTHDRAWAL   1300             <NA>                   NA    1300    1300
    ##   outgoes_incomes_equal
    ## 1                  TRUE
    ## 2                  TRUE
    ## 3                  TRUE
    ## 4                  TRUE
    ## 5                  TRUE
    ## 6                  TRUE

``` r
head(df_transaction_mod)
```

    ##   trans_id account_id       date balance balance_end_day cashflow
    ## 1        1          1 1995-03-24  1000.0          1000.0       IN
    ## 2        5          1 1995-04-13  4679.0          4679.0       IN
    ## 3      199          1 1995-04-23 17279.0         17279.0       IN
    ## 4  3530438          1 1995-04-30 17298.2         17298.2       IN
    ## 5        6          1 1995-05-13 20977.2         20977.2       IN
    ## 6      200          1 1995-05-23 23077.2         23077.2       IN
    ##               operation  amount characterization bank_name account_nr incomes
    ## 1           CASH CREDIT  1000.0             <NA>                   NA  1000.0
    ## 2 COLLECTION OTHER BANK  3679.0             <NA>        AB   41403269  3679.0
    ## 3           CASH CREDIT 12600.0             <NA>                   NA 12600.0
    ## 4                  <NA>    19.2  CREDIT INTEREST                   NA    19.2
    ## 5 COLLECTION OTHER BANK  3679.0             <NA>        AB   41403269  3679.0
    ## 6           CASH CREDIT  2100.0             <NA>                   NA  2100.0
    ##   outgoes outgoes_incomes_equal
    ## 1       0                 FALSE
    ## 2       0                 FALSE
    ## 3       0                 FALSE
    ## 4       0                 FALSE
    ## 5       0                 FALSE
    ## 6       0                 FALSE

Bei den beiden Transaktionen 2344 und 151858 darf kein Betrag in der
Spalte balance_end_day enthalten sein, da ja die Balance gleich hoch wie
am Vortag ist. Deshalb werden diese beiden Beträge aus der Spalte
gelöscht und mit NA ersetzt.

``` r
df_transaction_mod <- df_transaction_mod %>% 
  mutate(balance_end_day = ifelse(trans_id == 2344, NA, balance_end_day)) %>% 
  mutate(balance_end_day = ifelse(trans_id == 151858, NA, balance_end_day))

df_transaction_mod %>% 
  filter(outgoes_incomes_equal == TRUE) %>% 
  head()
```

    ##   trans_id account_id       date balance balance_end_day cashflow
    ## 1     2227          9 1996-03-17 32836.2              NA       IN
    ## 2     2344          9 1996-03-17 32036.2              NA      OUT
    ## 3    88343        297 1998-01-18 22960.5              NA       IN
    ## 4    88384        297 1998-01-18 22360.5              NA      OUT
    ## 5    94152        320 1997-10-18 48618.4              NA       IN
    ## 6    94186        320 1997-10-18 47318.4              NA      OUT
    ##          operation amount characterization bank_name account_nr incomes outgoes
    ## 1      CASH CREDIT    800             <NA>                   NA     800     800
    ## 2 CASH WIDTHDRAWAL    800             <NA>                   NA     800     800
    ## 3      CASH CREDIT    600             <NA>                   NA     600     600
    ## 4 CASH WIDTHDRAWAL    600             <NA>                   NA     600     600
    ## 5      CASH CREDIT   1300             <NA>                   NA    1300    1300
    ## 6 CASH WIDTHDRAWAL   1300             <NA>                   NA    1300    1300
    ##   outgoes_incomes_equal
    ## 1                  TRUE
    ## 2                  TRUE
    ## 3                  TRUE
    ## 4                  TRUE
    ## 5                  TRUE
    ## 6                  TRUE

``` r
rm(outgoes_incomes_sum)
```

``` r
# save(df_transaction_mod, file = "df_transaction_mod.RData")
```

Damit nicht immer alles neu geladen werden muss…

``` r
# load("df_transaction_mod.RData")
```

#### Datumslücken beheben

Da nach wie vor nur das Datum mit einer Transaktion im
df_transaction_mod vorhanden ist und somit der Zeitverlauf lückenhaft,
wird nun das Datum ergänzt. So wird jeder Tag ersichtlich sein, auch
wenn keine Transaktion stattgefunden hat. Die Account ID und der
Kontostand pro Tag werden auch ergänzt.

``` r
# load("df_transaction_mod.RData")
date_complete <- df_transaction_mod %>%
  arrange(
    account_id,
    date, 
    balance_end_day
  ) %>% 
  group_by(account_id) %>% 
  mutate(first_transaction = min(date)) %>%
  mutate(last_transaction = max(date)) %>%
  # complete(date = seq.Date(min(date), as.Date("1998/12/31"), by = "day")) %>%
  complete(date = seq.Date(as.Date("1993/01/01"), as.Date("1998/12/31"), by = "day")) %>%
  fill(balance_end_day, first_transaction, last_transaction) %>% 
  ungroup()

# date_complete$balance <- na.replace(date_complete$balance, 0)
date_complete[, c("balance", "balance_end_day", "outgoes", "incomes", "amount")] <- na.replace(date_complete[, c("balance", "balance_end_day", "outgoes", "incomes", "amount")], 0)

firstlasttrans <- date_complete %>% 
  select(
    account_id,
    first_transaction,
    last_transaction
  ) %>% 
  drop_na() %>% 
  group_by(account_id) %>% 
  slice_head(n = 1) %>% 
  mutate(duration = (last_transaction - first_transaction)) %>% 
  arrange(duration)

date_complete <- date_complete %>% 
  select(-c(first_transaction, last_transaction)) %>% 
  left_join(firstlasttrans, by = "account_id")

rm(firstlasttrans)
```

``` r
df_transaction_complete <- date_complete

head(date_complete)
```

    ## # A tibble: 6 × 17
    ##   account_id date       trans_id balance balance_end_day cashflow operation
    ##        <int> <date>        <int>   <dbl>           <dbl> <fct>    <fct>    
    ## 1          1 1993-01-01       NA       0               0 <NA>     <NA>     
    ## 2          1 1993-01-02       NA       0               0 <NA>     <NA>     
    ## 3          1 1993-01-03       NA       0               0 <NA>     <NA>     
    ## 4          1 1993-01-04       NA       0               0 <NA>     <NA>     
    ## 5          1 1993-01-05       NA       0               0 <NA>     <NA>     
    ## 6          1 1993-01-06       NA       0               0 <NA>     <NA>     
    ## # ℹ 10 more variables: amount <dbl>, characterization <fct>, bank_name <fct>,
    ## #   account_nr <int>, incomes <dbl>, outgoes <dbl>,
    ## #   outgoes_incomes_equal <lgl>, first_transaction <date>,
    ## #   last_transaction <date>, duration <drtn>

``` r
rm(df_transaction_mod)
```

Die NA’s in den restlichen Spalten entstehen, da an diesen Tagen keine
Transaktionen stattgefunden haben. Dies wird so belassen und wenn nötig
zu einem späteren Zeitpunkt angepasst.

#### Kontrolle am Account Nr. 18

``` r
df_transaction_complete %>%
  filter(account_id == 18,
         year(date) == 1993) %>%
  arrange(date) %>% 
  group_by(month(date)) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  head()
```

    ## # A tibble: 6 × 18
    ##   account_id date       trans_id balance balance_end_day cashflow operation
    ##        <int> <date>        <int>   <dbl>           <dbl> <fct>    <fct>    
    ## 1         18 1993-01-31       NA       0               0 <NA>     <NA>     
    ## 2         18 1993-02-28       NA       0               0 <NA>     <NA>     
    ## 3         18 1993-03-31       NA       0               0 <NA>     <NA>     
    ## 4         18 1993-04-30       NA       0               0 <NA>     <NA>     
    ## 5         18 1993-05-31       NA       0            1100 <NA>     <NA>     
    ## 6         18 1993-06-30       NA       0           14600 <NA>     <NA>     
    ## # ℹ 11 more variables: amount <dbl>, characterization <fct>, bank_name <fct>,
    ## #   account_nr <int>, incomes <dbl>, outgoes <dbl>,
    ## #   outgoes_incomes_equal <lgl>, first_transaction <date>,
    ## #   last_transaction <date>, duration <drtn>, `month(date)` <dbl>

``` r
# save(df_transaction_complete, file = "df_transaction_complete.RData")
```

``` r
df_transaction_complete %>% 
  filter(account_id == 18) %>%
  arrange(date) %>% 
  select(
    account_id, 
    date, 
    trans_id,
    balance,
    balance_end_day
  ) %>% 
  drop_na() %>% 
  head()
```

    ## # A tibble: 6 × 5
    ##   account_id date       trans_id balance balance_end_day
    ##        <int> <date>        <int>   <dbl>           <dbl>
    ## 1         18 1993-05-26     4152    1100            1100
    ## 2         18 1993-06-25     4391   14600           14600
    ## 3         18 1993-07-25     4392   17200           17200
    ## 4         18 1993-08-24     4393   20700           20700
    ## 5         18 1993-09-23     4394   23200           23200
    ## 6         18 1993-10-23     4395   23400           23400

#### Balance per month

Neues Data Frame erstellen mit den monatlichen Kontoständen

``` r
balance_per_month <- df_transaction_complete %>%
  group_by(
    account_id,
    year(date),
    month(date)
  ) %>%
  slice_tail(n = 1) %>%
  ungroup() %>% 
  arrange(date)

balance_per_month <- balance_per_month %>% 
  select(
    account_id,
    date,
    balance_end_day
  ) %>%
  pivot_wider(names_from = date, values_from = balance_end_day) %>% 
  arrange(account_id)


balance_per_month %>% 
  filter(
    account_id == 18
  )
```

    ## # A tibble: 1 × 73
    ##   account_id `1993-01-31` `1993-02-28` `1993-03-31` `1993-04-30` `1993-05-31`
    ##        <int>        <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
    ## 1         18            0            0            0            0         1100
    ## # ℹ 67 more variables: `1993-06-30` <dbl>, `1993-07-31` <dbl>,
    ## #   `1993-08-31` <dbl>, `1993-09-30` <dbl>, `1993-10-31` <dbl>,
    ## #   `1993-11-30` <dbl>, `1993-12-31` <dbl>, `1994-01-31` <dbl>,
    ## #   `1994-02-28` <dbl>, `1994-03-31` <dbl>, `1994-04-30` <dbl>,
    ## #   `1994-05-31` <dbl>, `1994-06-30` <dbl>, `1994-07-31` <dbl>,
    ## #   `1994-08-31` <dbl>, `1994-09-30` <dbl>, `1994-10-31` <dbl>,
    ## #   `1994-11-30` <dbl>, `1994-12-31` <dbl>, `1995-01-31` <dbl>, …

``` r
# save(balance_per_month, file = "balance_per_month.RData")


df_mod <- df_mod %>% 
  select(-starts_with("balance_"),
         -starts_with("IN_"),
         -starts_with("OUT_"))
```

#### Anfügen der monatliche Kontostände an das df_mod

Ergänzen der Kolonnen im balance_per_month mit dem Präfix balance\_ für
die spätere Übersicht im konsolidierten Datensatz. Anschliessend wird
die monatlichen Kontostände dem df_mod mit left_join hinzugefügt.

``` r
# Kolonnen werden mit dem Präfix ergänzt
if (!grepl("balance_", names(balance_per_month)[2])) {
names(balance_per_month) <- paste0("balance_", names(balance_per_month))
}
df_mod <- df_mod %>% 
  left_join(balance_per_month, by = c("account_id" = "balance_account_id"))
```

#### Quartalsweise outgoes und incomes[^9]

Für die spätere Berechnung der Umsätze und Bilanzen werden die outgoes
und incomes monatlich zusammengefasst.

``` r
date_complete <- date_complete %>% 
  mutate(quarter = quarter(date, type = "year.quarter")) %>% 
  mutate(semester = semester(date, with_year = TRUE)) %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date))
  
outgoes_incomes_quarterly <- date_complete %>% 
  select(
    account_id,
    quarter, 
    cashflow,
    amount
  ) %>%
  group_by(
    account_id,
    quarter,
    cashflow
  ) %>%
  summarise(amount_sum = sum(amount)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = cashflow, values_from = amount_sum) %>% 
  rename(incomes_quarterly = IN,
         outgoes_quarterly = OUT) %>%
  na_replace(., 0) %>% 
  select(
    account_id,
    quarter,
    incomes_quarterly,
    outgoes_quarterly
  ) %>% 
  arrange(quarter) %>% 
  pivot_wider(names_from = "quarter", values_from = c("incomes_quarterly", "outgoes_quarterly")) %>% 
  arrange(account_id)

  
 head(outgoes_incomes_quarterly)
```

    ## # A tibble: 6 × 49
    ##   account_id incomes_quarterly_1993.1 incomes_quarterly_1993.2
    ##        <int>                    <dbl>                    <dbl>
    ## 1          1                       0                        0 
    ## 2          2                   25050.                   71240.
    ## 3          3                       0                        0 
    ## 4          4                       0                        0 
    ## 5          5                       0                        0 
    ## 6          6                       0                        0 
    ## # ℹ 46 more variables: incomes_quarterly_1993.3 <dbl>,
    ## #   incomes_quarterly_1993.4 <dbl>, incomes_quarterly_1994.1 <dbl>,
    ## #   incomes_quarterly_1994.2 <dbl>, incomes_quarterly_1994.3 <dbl>,
    ## #   incomes_quarterly_1994.4 <dbl>, incomes_quarterly_1995.1 <dbl>,
    ## #   incomes_quarterly_1995.2 <dbl>, incomes_quarterly_1995.3 <dbl>,
    ## #   incomes_quarterly_1995.4 <dbl>, incomes_quarterly_1996.1 <dbl>,
    ## #   incomes_quarterly_1996.2 <dbl>, incomes_quarterly_1996.3 <dbl>, …

``` r
 # save(outgoes_incomes_quarterly, file = "outgoes_incomes_quarterly.RData")
```

``` r
df_mod <- df_mod %>% 
  left_join(outgoes_incomes_quarterly, by = "account_id")
```

#### Jährliche Einkünfte und Ausgaben[^10]

``` r
outgoes_incomes_yearly <- date_complete %>% 
  select(
    account_id,
    year, 
    cashflow,
    amount
  ) %>%
  group_by(
    account_id,
    year,
    cashflow
  ) %>%
  summarise(amount_sum = sum(amount)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = cashflow, values_from = amount_sum) %>% 
  rename(incomes_yearly = IN,
         outgoes_yearly = OUT) %>%
  na_replace(., 0) %>% 
  select(
    account_id,
    year,
    incomes_yearly,
    outgoes_yearly
  ) %>% 
  arrange(year) %>% 
  pivot_wider(names_from = "year", values_from = c("incomes_yearly", "outgoes_yearly")) %>% 
  arrange(account_id)

  
 head(outgoes_incomes_yearly)
```

    ## # A tibble: 6 × 13
    ##   account_id incomes_yearly_1993 incomes_yearly_1994 incomes_yearly_1995
    ##        <int>               <dbl>               <dbl>               <dbl>
    ## 1          1                  0                   0               49564.
    ## 2          2             228685.             264653.             265362.
    ## 3          3                  0                   0                   0 
    ## 4          4                  0                   0                   0 
    ## 5          5                  0                   0                   0 
    ## 6          6                  0               20907               81372.
    ## # ℹ 9 more variables: incomes_yearly_1996 <dbl>, incomes_yearly_1997 <dbl>,
    ## #   incomes_yearly_1998 <dbl>, outgoes_yearly_1993 <dbl>,
    ## #   outgoes_yearly_1994 <dbl>, outgoes_yearly_1995 <dbl>,
    ## #   outgoes_yearly_1996 <dbl>, outgoes_yearly_1997 <dbl>,
    ## #   outgoes_yearly_1998 <dbl>

``` r
 # save(outgoes_incomes_yearly, file = "outgoes_incomes_yearly.RData")
 # save(date_complete, file = "date_complete.RData")
```

``` r
df_mod <- df_mod %>% 
  left_join(outgoes_incomes_yearly, by = "account_id")

# save(balance_per_month, file = "balance_per_month.RData")
# save(date_complete, file = "date_complete.RData")
rm(outgoes_incomes_quarterly, outgoes_incomes_yearly, balance_per_month)
rm(date_complete)
```

neues DF mit den ein und ausgängen pro Monat (Umsatz) ev. zusätzliche
Spalte mit Info ob der Account im Minus ist oder nicht

Accountgruppen erstellen, wie lange der Account aktiv war (z.B. halbes
Jahr, Jahr, etc.)

## Erstellen des konsolidierten Data Frames[^11]

Das konsolidierte Data Frame wird und dem Namen **df_cons** gespeichert.

``` r
df_cons <- df_mod
# save(df_cons, file = "df_cons.RData")
# save(df_mod, file = "df_mod.RData")
```

Übersicht über die Struktur des konsolidierten Datensatzes.

## Datengrundlage[^12]

Die uns zu Grunde liegenden Daten lassen sich als von der Bank sowie als
exogen erhoben unterscheiden.

a\. Wie können die Daten noch weiter gruppiert und angeordnet werden, um
eine optimale Grundlage für die Analyse zu bilden?

Die Daten können je nach Typ und Struktur noch weiter gruppiert werden
um für folgende Analysen eine optimale Grundlage zu schaffen.
Gruppierungen sind von Interesse, da sie eine Systematischen Vorteil
liefern können.

Das war unsere Ausgangslage: ![an image caption Source: ERD
Ausgangslage.](CrossSellingInBanking%5CRessources%5CSOLL-Zustand.png)

Ein paar Beispiele für solche weitere Gruppierungen sind:

- Einkommen pro Monat
- Einkommen Quartalsweise
- Einkommen Jährlich
- Ausgaben pro Monat
- Ausgaben Quartalsweise
- Ausgaben Jährlich
- Kontostand pro Monat
- Kontostand Quartalsweise
- Kontostand Jährlich

Mit diesen sind die Daten in einer für uns und die Bank in einer
nützlichen Form. Diese Erkenntnisse sind dann in den Konsolidierten
Datensatz eingeflossen. Diese Gruppierten Daten sind für uns von
Interesse, da wir mit Ihnen effizient, klar und strukturiert arbeiten
können. Beim Erstellen des **df_cons** sind unsere Gedanken und
Änderungen eingeflossen.

b\. Wie umfassend sind die Stichproben der Daten der Bank? Und trifft
dies für alle einzelnen Datensätze zu?

Weshalb eine Überprüfung der Stichprobengrösse? - Ein zu kleiner
Stichprobenumfang kann dazu führen, dass nicht die gewünschte Tiefe des
eigentlich angestrebten erreicht wird. - Ein zu großer Stichprobenumfang
kann dazu führen, dass unnötig Ressourcen verschwendet werden.

Wir bemühen uns, genügend Proben zu haben, um einen Effekt vernünftig zu
erkennen, wenn er wirklich vorhanden ist, ohne begrenzte Ressourcen für
zu viele Proben zu verschwenden.

Deshalb die “Sample Size Calculation”

Bevölkerung in Tschechien:

1993: 10.33 Mio 1994: 10.33 Mio 1995: 10.33 Mio 1996: 10.32 Mio 1997:
10.30 Mio 1998: 10.29 Mio 1999: 10.28 Mio

Quelle:
(<https://www.laenderdaten.info/Europa/Tschechien/bevoelkerungswachstum.php>)

Sind Durchschnittlich: 10.3114 Mio Confidence Level: 95% Margin of
Error: 3 %

Berechnung: (Z-Score)2 x StdDev x (1-StdDev)/(margin of error)2

Ideale Sample Grösse: 1067

Der zur Verfügung gestellte Anzahl Accounts hat eine Grösse von 4500
Zeilen, was in ungefähr dem vierfachen der berechneten optimalen
Stichprobengrösse entspricht. Dies bedeutet, dass wir von mehr
Beobachtungen als nötig sprechen.

relation account (4500 objects in the file ACCOUNT.ASC) -\> Wie im
Beispiel oben behandelt relation client (5369 objects in the file
CLIENT.ASC) -\> Gleicher Fall von einer Ausreichender Stichprobengrösse.
relation disposition (5369 objects in the file DISP.ASC) -\> Gleicher
Fall von einer Ausreichender Stichprobengrösse. relation permanent order
(6471 objects in the file ORDER.ASC) -\> Gleicher Fall von einer
Ausreichender Stichprobengrösse. relation transaction (1056320 objects
in the file TRANS.ASC) -\> Da die Transaktionen unregelmässig und
mehrmals möglich sind haben wir hier eine sehr hohe Anzahl
Observationen. relation loan (682 objects in the file LOAN.ASC) -\> Um
eine Aussage über die Richtige Anzahl Kredite machen zu können, bräuchte
man die gesamte Anzahl von Krediten von 1993-1999. relation credit card
(892 objects in the file CARD.ASC) -\> Um eine Aussage über die Richtige
Anzahl Kreditkarten machen zu können, bräuchte man die gesamte Anzahl
von Kreditkarten von 1993-1999. relation demographic data (77 objects in
the file DISTRICT.ASC) -\> Die Distrikte werden nicht in Stichproben
erfasst, also werden alle Distrikte so wiedergegeben wie sie in
Wirklichkeit sind.

### Zudem wird die Distribution der Datensätze und deren Qualität überprüft.

Zur Überprüfung der Qualität gehört: - Zuverlässigkeit - Relevanz -
Verständlichkeit - Korrektheit - Aktualität

Zuverlässigkeit: Da dies Eine Challenge ist die bereits dutzende male
erarbeitet wurde, und von Fachhoschulen, Unis und weiteren Institutionen
verwendet wird, haben wir eine Zuverlässige Quelle. Relevanz: Diese
Daten sind für uns relevant, da diese Daten unseren Informationsbedarf
abdecken. Verständlichkeit: Durch die Transformation der Daten ins
Englische kam die Verständlichkeit. Denn Tschechisch wird bei uns in der
Gruppe nicht gesprochen. Aktualität: Diese Daten stammen aus den Jahren
1993-1999 und sind für die heutige Zeit nicht mehr aktuell, da sich
dieses Gebiet stark digitalisiert hat und somit nicht mehr das selbe ist
wie dazumal

Da wir mit den zur Verfügung gestellten Daten arbeiten, und keinen
Einfluss auf die Qualitätansprüche bei der Erhebung haben können wir
Falsche oder Fehlende Daten verändern oder entfernen.

c\. Die Datenattribute werden auf ihre Verteilung sowie ihre Veränderung
über die Zeit analysiert. Dabei werden technische Mittel der
explorativen Datenanalyse angewendet.

Eine Überprüfung ist die Veränderung des Alters bei der Eröffnung eines
Kontos über die Zeit:

``` r
ggplot(df_cons, aes(x = account_opening_year, y = as.numeric(owner_age_at_account_opening)))+
  geom_jitter()+
  geom_smooth()+
  labs(title = "Veränderung des Alters bei Eröffnung über die Zeit",
              subtitle = "1993-1997")+
  xlab("Jahr")+
  ylab("Alter bei Kontoeröffnung in Jahren")
```

![](notebook_files/figure-gfm/unnamed-chunk-166-1.png)<!-- -->

Als erstes kann man bei den minimalen Werten einen Trend ablesen, Das
minimale Alter bei der Eröffnung wurde wurde über die Jahre konstant
höher. 1993 hatten noch Kinder ab 10 Jahren bereits ein Konto, im Jahr
1997 lagen die jüngsten Kontoeröffner bei rund 15 Jahren. Dieser Trend
lässt sich auch an der Alters Obergrenze entnehmen. Bei den älteren
Menschen kann das ein Zeichen davon sein, dass die Älteren Menschen
immer älter werden. auch gab es in den Jahren 1993 und 1995 vermehrte
Kontoeröffnungen.

Kann man einen Trend bezüglich des Geschlechts der Kontoinhaber
feststellen?

``` r
ggplot(df_cons, aes(x = account_opening_year, fill = owner_sex))+
  geom_bar()
```

![](notebook_files/figure-gfm/unnamed-chunk-167-1.png)<!-- -->

Wir können keinen Trend bezüglich den Geschlechtern und der Veränderung
über die Zeit erkennen. Wir erkennen das es eine in etwa gleiche
Verteilung der Konten über beide Geschlechter gibt.

i\. Jegliche Datenattribute werden innerhalb des eigenen Datenrahmens
analysiert.

Diese Analyse und Transformation wurde beim Sichten der Daten bereits
erledigt, somit konnten bereits zu Beginn die Richtigen Datentypen und
Formate gewählt werden. Des Weiteren wurden für den Konsolidierten
Datensatz noch weitere Attribute erstellt, gruppiert und transformiert.
Dies wurde gemacht um eine optimale Grundlage für die weiteren Analysen
zu haben.

## Ausführliche Analysen

``` r
# load("df_cons.RData")
# load("date_complete.RData")
# load("df_transaction_mod.RData")
```

### Zusätzliche Spalten[^13]

Für die weiteren Analysen werden zusätzliche Spalten benötigt.

#### Spalte aktuelles Alter des Kontoinhabers (owner_current_age)[^14]

Neue Spalte “owner_current_age” für das aktuelle Alter des Kunden am
01.01.1999 Eine Spalte für die Altersgruppen: “0-17” “18-30” “31-40”
“41-50” “51-60” “61-70” “70+”

``` r
# aktuelles Alter der Kunden und Altersgruppen
df_cons <- df_cons %>% 
  mutate(owner_current_age = trunc((owner_dateofbirth %--% as.Date("1999-01-01")) / years(1))) %>%
  mutate(age_groups = case_when(
    owner_current_age < 18 ~ "0-17",
    owner_current_age >= 18 & owner_current_age <= 30 ~ "18-30",
    owner_current_age > 30 & owner_current_age <= 40 ~ "31-40",
    owner_current_age > 40 & owner_current_age <= 50 ~ "41-50",
    owner_current_age > 50 & owner_current_age <= 60 ~ "51-60",
    owner_current_age > 30 & owner_current_age <= 70 ~ "61-70",
    owner_current_age > 70  ~ "70+"
  )) %>% 
  mutate(age_groups = as.factor(age_groups))
```

#### Spalte Kreditkarte (has_creditcard)[^15]

Erstellen einer neuen Spalte “has_creditcard” mit dem logischen Wert
TRUE wenn ein Konto mit einer Kreditkarte verbunden ist.

``` r
df_cons <- df_cons %>% 
  mutate(has_creditcard = ifelse(is.na(card_id), FALSE, TRUE))
```

#### Spalte Girokonto (is_checking_account)[^16]

Die uns zugrunde liegenden Daten sagen leider nichts über die Kontoart
aus. Deshalb muss dies mittels Analyse bestimmt werden. Folgende
Gedanken dazu: ein Girokonto hat regelmässige Transaktionen. Es gibt im
Durchschnitt mindestens je eine Transaktion (in und out) pro Monat.

Erstellen einer neuen Spalte “is_checking_account” mit dem logischen
Wert TRUE wenn es sich beim Konto um ein Girokonto handelt.

``` r
# Spalte für die Information, ob es Girokonto (is_checking_account) ist oder nicht
checking_account <- df_transaction_complete %>% 
  drop_na(trans_id) %>% 
  group_by(
    account_id,
    year(date),
    cashflow
  ) %>% 
    count() %>% 
  group_by(
    account_id,
    cashflow
  ) %>% 
  summarise(mean_n = round(mean(n), digits = 0)) %>% 
  pivot_wider(names_from = cashflow, values_from = mean_n) %>% 
  mutate(is_checking_account = ifelse(IN > 12 & OUT > 12, TRUE, FALSE)) %>% 
  select(account_id, is_checking_account) %>% 
  ungroup()

set.seed(256)
slice_sample(checking_account, n = 5)
```

    ## # A tibble: 5 × 2
    ##   account_id is_checking_account
    ##        <int> <lgl>              
    ## 1        666 TRUE               
    ## 2       2199 TRUE               
    ## 3       4269 TRUE               
    ## 4       1466 FALSE              
    ## 5       6636 TRUE

``` r
# Anfügen der neuen Spalte an des konsolidierte Data Frame
df_cons <- df_cons %>% 
  left_join(checking_account, by = "account_id")



# df_cons speichern für ein späteres laden
# save(df_cons, file = "df_cons.RData")
rm(checking_account)

df_cons %>% 
  select(account_id, is_checking_account) %>% 
  head(5)
```

    ##   account_id is_checking_account
    ## 1          1                TRUE
    ## 2          2                TRUE
    ## 3          3                TRUE
    ## 4          4                TRUE
    ## 5          5                TRUE

#### Spalte durchschnittliche Ausgaben (outgoes_avg)[^17]

Um einem Kunden ein Cross Selling Angebot machen zu können, welches auch
dem Kunden einen Mehrwert bietet, muss ein regelmässiges Einkommen
vorhanden sein.

Regelmässiges Einkommen

``` r
# Durchschnittliche Ausgaben berechnen
outperyear <- df_cons %>% 
  select(
    account_id,
    starts_with("outgoes_yearly_")
  ) %>% 
  pivot_longer(starts_with("outgoes_yearly_"), names_to = "outgoes_per_year", values_to = "amount", values_drop_na = FALSE) %>% 
  filter(amount != 0) %>% 
  group_by(account_id) %>% 
  summarise(outgoes_avg = round(mean(amount), digits = 0))

# Anfügen der neuen Spalte an das df_cons
df_cons <- df_cons %>% 
  left_join(outperyear, by = "account_id")
  
rm(outperyear)
```

#### Spalte Durchschnittseinkommen (income_avg)[^18]

Um einem Kunden ein Cross Selling Angebot machen zu können, welches auch
dem Kunden einen Mehrwert bietet, muss ein regelmässiges Einkommen
vorhanden sein.

Regelmässiges Einkommen

``` r
# Durchschnittliches Einkommen berechnen
inperyear <- df_cons %>% 
  select(
    account_id,
    starts_with("incomes_yearly_")
  ) %>% 
  pivot_longer(starts_with("incomes_yearly_"), names_to = "income_per_year", values_to = "amount", values_drop_na = FALSE) %>% 
  filter(amount != 0) %>% 
  group_by(account_id) %>% 
  summarise(income_avg = round(mean(amount), digits = 0))

# Anfügen der neuen Spalte an das df_cons
df_cons <- df_cons %>% 
  left_join(inperyear, by = "account_id")
```

#### Spalte für regelmässiges Einkommen (is_regular_income)[^19]

Erstellen einer neuen Spalte mit der Information, ob das Konto über
regelmässige, monatliche Lohnzahlungen verfügt. Deshalb ist eine
Bedingung, dass es sich dabei um ein Girokont handelt.

``` r
df_cons <- df_cons %>% 
  mutate(is_regular_income = is_checking_account)
```

#### Spalte regelmässiges Einkommen mind. 80% (is_regular_income_80)[^20]

Um die Kreditkartenausgaben oder Darlehen decken zu können braucht der
Kunde ein regelmässiges Einkommen, welches über dem ortsüblichen
Einkommen liegt.

Erstellen einer neuen Spalte “regular_income” mit dem logischen Wert
TRUE wenn auf dem Konto regelmässig Beträge über dem Existenzminimum
vorhanden sind. Für diese Information werden alle Konten mit
regelmässigem Einkommen und einem Betrag von 10% unter dem ortsüblichen
Einkommensdurchschnitt ermittelt.

Datenbeschrieb - is_regular_income_80 für durchschnittliche Einkommen
mind. 80% vom ortüblichen Durchschnitt

``` r
# neue Spalte für regelmässiges Einkommen von mind. 80% des ortsüblichen Durchschnitts
df_cons <- df_cons %>% 
  mutate(is_regular_income_80 = ifelse(((income_avg * 0.2) >= owner_district_average_salary & is_regular_income == TRUE), TRUE, FALSE)) 

# Anzahl der Accounts mit regelmässigen monatlichen Einkommen von mind. 80%
foo80 <- df_cons %>% 
  filter(is_regular_income_80 == TRUE) %>% 
  nrow()

paste("Es gibt", foo80, "Accounts mit einem regelmässigen Einkommen von mindestens 80% des ortsüblichen Durchschnittsalärs", sep = " ")
```

    ## [1] "Es gibt 3789 Accounts mit einem regelmässigen Einkommen von mindestens 80% des ortsüblichen Durchschnittsalärs"

``` r
rm(foo80)
```

#### Spalte Beratung vor Ort (consultation_on_site)

Erstellen einer neuen Spalte “consultation_on_site” mit dem logischen
Wert TRUE wenn das Konto eine positive Vermögensentwicklung über die
gesamte Laufzeit aufweist.

``` r
# erstellen der neuen Spalte
df_cons <- df_cons %>% 
  mutate(consultation_on_site = ifelse(owner_district_name == account_district_name, TRUE, FALSE))
```

### Einkommensentwicklung und -verteilung[^21]

Das Augenmerk soll nun auf die Salärentwicklung und -verteilung
gerichtet werden.

Eine Übersicht der Einkommen ergibt sich aus dem summary() der Spalte
“incomes_per_year”.

``` r
# Übersicht über die Verteilung der Jahreseinkommen
summary(df_cons$income_avg)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    8678   61772  145873  179698  256796  773324

Vom Mindestbetrag bis zum Höchstbetrag sind grosse Unterschiede zu
erkennen. Die Verteilung der Beträge wird in einem Density-Plot
deutlich.

``` r
df_cons %>% 
  ggplot(aes(income_avg)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(fill = "cyan", alpha = 0.3) +
  theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 15)
    ) +
    ggtitle("Yearly income average") +
  labs(
    x = "Amount [CZK]",
    # y = "",
    subtitle = "Time span: 1993 - 1998, across all accounts"
  ) +
  scale_x_continuous(labels = comma, limits = c(0, 1000000)) +
  scale_y_continuous(labels = comma)
```

![](notebook_files/figure-gfm/unnamed-chunk-178-1.png)<!-- -->

Die meisten Accounts weisen ein Durchschnittseinkommen um 100k CZK auf.
Die Stufe bei 250k CZK deutet auf eine grössere Anzahl Accounts hin,
welche um diesen Bereich ihr Einkommen haben. Danach nimmt die Anzahl
Accounts mit hohen bis sehr hohen Beträgen deutlich ab. Die Verteilung
ist hier rechtsschief.

``` r
# library(tidyquant)
skew_incomes <- df_cons %>% 
  summarise(skew_builtin = skewness(income_avg))

skew_incomes
```

    ##   skew_builtin
    ## 1     1.086697

``` r
rm(skew_incomes)
```

Die Kennzahl (skewness) ist 1.08 und somit grösser als 0. Dies heisst,
dass die Verteilung der Einkommen eine positive Schiefe (rechtsschief)
aufweist, was im Plot sehr gut zu erkennen ist.

#### Entwicklung Jahreseinkommen[^22]

Mittels Visualisierung mit einem Boxplot wird die Entwicklung des
Jahreseinkommens deutlich.

``` r
# Jahreseinkommen für Visualisierung umformen
inperyear <- df_cons %>% 
  select(
    account_id,
    starts_with("incomes_yearly_")
  ) %>% 
  pivot_longer(starts_with("incomes_yearly_"), names_to = "income_per_year", values_to = "amount", values_drop_na = FALSE)

# Visualisierung
inperyear %>% 
  filter(amount > 0) %>% 
ggplot(aes(x = income_per_year, y = amount, fill = as.factor(income_per_year))) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha = 0.6, option = "A") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 15)
    ) +
    ggtitle("Incomes average") +
  labs(
    x = "",
    y = "Amount per year [CZK]",
    subtitle = "Time span: 1993 - 1998, across all accounts"
  ) +
  scale_x_discrete(labels = c("incomes_yearly_1993" = "1993",
                                                 "incomes_yearly_1994" = "1994",
                                                 "incomes_yearly_1995" = "1995",
                                                 "incomes_yearly_1996" = "1996",
                                                 "incomes_yearly_1997" = "1997",
                                                 "incomes_yearly_1998" = "1998"))
```

![](notebook_files/figure-gfm/unnamed-chunk-180-1.png)<!-- -->

Wie beim Density-Plot (Verteilung) kommt deutlich zum Ausdruck, dass die
meisten Konten Einkommen um 125’000 CZK aufweisen. Die
Einkommensentwicklung ist nicht so leicht zu erkennen. Ein geringer
Anstieg über die beobachteten fünf Jahre lässt sich ausmachen mit
Ausnahme des Jahres 1996.

``` r
inperyear %>% 
  filter(amount > 0) %>% 
  summary()
```

    ##    account_id    income_per_year        amount         
    ##  Min.   :    1   Length:17543       Min.   :      1.3  
    ##  1st Qu.: 1170   Class :character   1st Qu.:  59571.6  
    ##  Median : 2373   Mode  :character   Median : 125672.9  
    ##  Mean   : 2796                      Mean   : 183974.9  
    ##  3rd Qu.: 3570                      3rd Qu.: 258750.1  
    ##  Max.   :11382                      Max.   :1040784.7

Für die nächste Analyse werden alle Einkommen unter einem Betrag von
100’000 CZK weggelassen.

``` r
# Visualisierung alle Einkommen unter 100000 CZK
inperyear %>% 
  filter(amount > 0,
         amount < 100000) %>% 
ggplot(aes(x = income_per_year, y = amount, fill = as.factor(income_per_year))) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha = 0.6, option = "A") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 15)
    ) +
    ggtitle("Incomes average") +
  labs(
    x = "",
    y = "Amount per year [CZK]",
    subtitle = "Amount under 100,000 CZK, across all accounts"
  ) +
  scale_x_discrete(labels = c("incomes_yearly_1993" = "1993",
                                                 "incomes_yearly_1994" = "1994",
                                                 "incomes_yearly_1995" = "1995",
                                                 "incomes_yearly_1996" = "1996",
                                                 "incomes_yearly_1997" = "1997",
                                                 "incomes_yearly_1998" = "1998")) +
  scale_y_continuous(labels = comma)
```

![](notebook_files/figure-gfm/unnamed-chunk-182-1.png)<!-- -->

Der grösste Anteil der Konten weist ein Einkommen zwischen 50-80K auf.

``` r
# Visualisierung alle Einkommen zwischen 100000 - 320000 CZK
inperyear %>% 
  filter(amount > 100000,
         amount < 320000) %>% 
ggplot(aes(x = income_per_year, y = amount, fill = as.factor(income_per_year))) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha = 0.6, option = "A") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 15)
    ) +
    ggtitle("Incomes average") +
  labs(
    x = "",
    y = "Amount per year [CZK]",
    subtitle = "Amount between 100,000 & 320,000 CZK, across all accounts"
  ) +
  scale_x_discrete(labels = c("incomes_yearly_1993" = "1993",
                                                 "incomes_yearly_1994" = "1994",
                                                 "incomes_yearly_1995" = "1995",
                                                 "incomes_yearly_1996" = "1996",
                                                 "incomes_yearly_1997" = "1997",
                                                 "incomes_yearly_1998" = "1998")) +
  scale_y_continuous(labels = comma)
```

![](notebook_files/figure-gfm/unnamed-chunk-183-1.png)<!-- -->

``` r
rm(inperyear)
```

Auch hier ist wieder dieselbe Entwicklung zu erkennen wie schon bei den
vorangeganenen Visualisierungen.

#### Spalte Salärgruppen (salary_groups)[^23]

Folgende Gruppen können nun in einer neuen Spalte “salary_groups”
erstellt werden: - lower_income für ein Jahreseinkommen bis 60K -
lower_middle_income für ein Jahreseinkommen von 60K bis 110K -
middle_income für ein Jahreseinkommen von 110K bis 145K -
upper_middle_income für ein Jahreseinkommen von 145K bis 220K -
upper_income für ein Jahreseinkommen von 220K bis 320K - high_income für
ein Jahreseinkommen über 320K

``` r
# Einkommensgruppen erstellen
foo <- df_cons %>% 
  select(account_id, is_checking_account, income_avg) %>% 
  filter(is_checking_account == TRUE) %>% 
  mutate(salary_groups = case_when(
    income_avg < 60000 ~ "lower-income",
    income_avg >= 60000 & income_avg < 120000 ~ "lower-middle-income",
    income_avg >= 120000 & income_avg < 180000 ~ "middle-income",
    income_avg >= 180000 & income_avg < 240000 ~ "upper-middle-income",
    income_avg >= 240000 & income_avg < 300000 ~ "upper-income",
    income_avg >= 300000 ~ "high-income"
  )) %>% 
  select(account_id, salary_groups)

# An das df_cons anfügen
df_cons <- df_cons %>% 
  left_join(foo, by = "account_id") %>% 
  mutate(salary_groups = as.factor(salary_groups))

# Reihenfolge der Levels festlegen
df_cons$salary_groups <- factor(df_cons$salary_groups, levels = c("lower-income", "lower-middle-income", "middle-income", "upper-middle-income", "upper-income", "high-income"))

summary(df_cons$salary_groups) 
```

    ##        lower-income lower-middle-income       middle-income upper-middle-income 
    ##                 920                 918                 614                 614 
    ##        upper-income         high-income                NA's 
    ##                 474                 797                 163

``` r
# Visualisierung
df_cons %>% 
  filter(!is.na(salary_groups)) %>% 
  ggplot(aes(salary_groups, fill = salary_groups)) +
  geom_bar() +
  scale_fill_viridis_d(alpha = 0.6, option = "L") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 15),
      axis.text.x = element_text(size = 8)
    ) +
  labs(
    title = "Number of Accounts per Salary Groups",
    x = ""
  ) 
```

![](notebook_files/figure-gfm/unnamed-chunk-184-1.png)<!-- -->

``` r
rm(foo)
```

Auffällig ist die hohe Anzahl Konten der höchsten Einkommen

#### Spalte für Vermögensentwicklung (assets_dev)[^24]

Aus Bankensicht ist es auch relevant, wie sich das Vermögen entwickelt.
Dazu wurde der Kontostand Ende 1998 in der Spalte “assets_dev”
gespeichert.

``` r
# erstellen der beiden Spalten 
df_cons <- df_cons %>% 
  mutate(assets_dev = `balance_1998-12-31`) 
```

Inspizieren der Spalte mit dem Betrag der Vermögensentwicklung

``` r
summary(df_cons$assets_dev)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -25821   23075   38495   43811   60589  138317

#### durchschnittliche Vermögensentwicklung (pro Jahr)[^25]

Die jährliche Vermögensentwicklung bewegt sich in den Jahren 1993-1998
zwischen -25821 und 138317.

Die meisten Accounts weisen im Jahresdurchschnitt eine positive
Vermögensentwicklung auf.

#### Spalte Gruppe der Vermögensentwicklung (assets_dev_groups)[^26]

Es wird eine zusätzliche Spalte erstellt für die Information, ob das
Konto eine positive, neutrale oder negative Entwicklung über die
Laufzeit erfährt.

``` r
df_cons <- df_cons %>% 
  mutate(assets_dev_groups = case_when(
    assets_dev < 0 ~ "negative", 
    assets_dev == 0 ~ "neutral", 
    assets_dev > 0 ~ "positive")) %>% 
  mutate(assets_dev_groups = as.factor(assets_dev_groups))

df_cons %>% 
  select(account_id, assets_dev_groups) %>% 
  head(., 3)
```

    ##   account_id assets_dev_groups
    ## 1          1          positive
    ## 2          2          positive
    ## 3          3          positive

#### Vermögensentwicklung über die Jahre[^27]

``` r
# erstellen des Dataframes um die Verteilung über die Jahre aufzuzeigen
 df <- df_cons %>% 
  select(account_id, assets_dev_groups,
    starts_with("balance")) %>% 
  select(account_id, assets_dev_groups, contains("-12-")) %>%
  rename("1993" = "balance_1993-12-31", 
         "1994" = "balance_1994-12-31",
         "1995" = "balance_1995-12-31",
         "1996" = "balance_1996-12-31",
         "1997" = "balance_1997-12-31",
         "1998" = "balance_1998-12-31") %>% 
  pivot_longer(starts_with("199"), names_to = "year", values_to = "amount", values_drop_na = FALSE) %>%
  filter(amount != 0) 

# Visualisierung über alle Accounts, Jahre und Beträge
df %>% 
  ggplot(aes(x = year, y = amount, fill = as.factor(year))) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha = 0.6, option = "A") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 15)
    ) +
    ggtitle("Assets development") +
  labs(
    x = "",
    y = "amount [CZK]",
    subtitle = "Time span: 1993 - 1998, across all accounts with assets dev between -25k and 60k CZK"
  ) +
  scale_y_continuous(labels = comma)
```

![](notebook_files/figure-gfm/unnamed-chunk-188-1.png)<!-- -->

``` r
# Ausreisser beseitigen Mithilfe der Interquartilsabstandsmethode 
df_no_outliers <- subset(df, df$amount > (quantile(df$amount, .25) - 1.5*IQR(df$amount)) & df$amount < (quantile(df$amount, .75) + 1.5*IQR(df$amount)))

# Visualisierung mit eingeschränktem Betrag
df_no_outliers %>% 
  filter(amount < 60000) %>% 
  ggplot(aes(x = year, y = amount, fill = as.factor(year))) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha = 0.6, option = "A") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 15)
    ) +
    ggtitle("Assets development") +
  labs(
    x = "",
    y = "amount [CZK]",
    subtitle = "Time span: 1993 - 1998, across all accounts without outliers"
  ) +
  scale_y_continuous(labels = comma)
```

![](notebook_files/figure-gfm/unnamed-chunk-188-2.png)<!-- -->

``` r
rm(df_no_outliers)
```

Nach entfernen der Outliers ist deutlich zu erkennen, dass die
Vermögensentwicklung leicht zu nimmt im positiven Bereich aber auch
deutlich abnimmt im negativen Bereich. Durch die negative Entwicklung
sind im negativen Bereich Outliers entstanden.

#### Geschichtliches Intermezzo

Im Jahr 1993 gab es die meisten positiven Vermögensentwicklungen. Danach
gehen die Vermögen zurück analog der Einkommensentwicklung. Diese
Entwicklung könnte auch einen geschichtlichen Zusammenhang haben. Am 1.
Januar 1993 wurde die heutige Tschechische Republik gegründet. Der noch
junge souveräne Staat könnte wirtschaftlich in der Findungsphase gewesen
sein, denn auch die Währung wurde mit der Tschechischen Krone am 8.
Februar 1993 neu eingeführt. Gemäss geschichtlichen Überlieferungen
verlief die Umstellung der Währung von der Tschechoslowakischen Krone
auf die Tschechische Krone reibungsfrei. Der Bankencrash 1997 war das
Ende des wirtschaftlichen Aufschwungs. 12 Finanzinstitute mussten
Insolvenz anmelden. Unsere Bank scheint nicht von der Insolvenz
betroffen zu sein, denn die vorliegenden Daten reichen bis Ende 1998.
Die darauf folgende Rezession der tschechischen Wirtschaft könnte eine
Erklärung für die rückläufige Vermögensentwicklung in den Jahren 1997
und 1998 sein.

#### Vermögensentwicklung auf die einzelnen Jahre

``` r
# Visualisierung auf 
df %>% 
  ggplot(aes(x = amount, fill = as.factor(assets_dev_groups))) +
  geom_histogram(position = "fill", alpha = 0.4) +
  facet_wrap(~ year, 
             scales = "free_y") +
  theme(
      legend.title = element_blank(),
      plot.title = element_text(size = 15), 
      axis.text.x = element_text(angle = 30, hjust = 1)
    ) +
    ggtitle("Assets development") +
  labs(
    x = "amount [CZK]",
    y = "percent",
    fill = "Assets development",
    subtitle = "across all accounts per year"
  ) +
  scale_x_continuous(labels = comma)
```

![](notebook_files/figure-gfm/unnamed-chunk-189-1.png)<!-- -->

``` r
rm(df)
```

Erstaundlich ist, dass trotz Rezession sehr viele Accounts eine positive
Entwicklung verzeichnen. Die negative Entwicklung ist vor allem in den
Jahren 1997 und 1998, als die Rezession begann, zu sehen.

Durchschnittseinkommen auf die Vermögensentwicklung aufgeteilt

``` r
ggplot(df_cons, aes(x = income_avg, fill = as.factor(assets_dev_groups))) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
    theme(
      plot.title = element_text(size = 15), 
    ) +
    ggtitle("Distribution of income among the assets development groups") +
  labs(
    x = "amount [CZK]",
    fill = "Assets development",
    subtitle = "Time span: 1993 - 1998, across all accounts",
    legend = "Assets development"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)
```

![](notebook_files/figure-gfm/unnamed-chunk-190-1.png)<!-- -->

Das Einkommen mit dem Logarithmus angepasst.

``` r
ggplot(df_cons, aes(x = income_avg, fill = as.factor(assets_dev_groups))) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
    theme(
      plot.title = element_text(size = 15), 
    ) +
    ggtitle("Distribution of income among the assets development groups") +
  labs(
    x = "amount (log transformed) [CZK]",
    fill = "Assets development",
    subtitle = "Time span: 1993 - 1998, across all accounts",
    legend = "Assets development"
  ) +
  scale_x_log10()
```

![](notebook_files/figure-gfm/unnamed-chunk-191-1.png)<!-- -->

Mit dem Logarithmus wurden Einkommen transformiert um die Verteilung der
Beträge besser ersichtlich zu machen. Die Dichte entspricht nun den
“neuen” Werten und es ist gut zu erkennen, dass die Accounts mit höheren
Einkommen häufiger negative Vermögensentwicklungen aufweisen. Die
Verteilung der negativen Vermögensentwicklung ist unimodal leicht
linksschief und die der positiven Vermögensentwicklung bimodal ebenfalls
mit einer leichten Tendez zu linksschief.

#### Vemögensentwicklung unterteilt auf die Einkommensgruppen[^28]

``` r
# df_cons$salary_groups <- factor(df_cons$salary_groups, levels = c("lower_income", "lower_middle_income", "middle_income", "upper_middle_income", "upper_income", "high_income"))

df_cons %>% 
  filter(!is.na(salary_groups)) %>% 
  ggplot(aes(x = assets_dev, fill = assets_dev_groups)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ salary_groups) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    ggtitle("Assets development avg from 1993 - 1998") +
  labs(
    x = "amount [CZK]",
    fill = "Assets dev",
    subtitle = "across all accounts with assets dev from 60k down to -25k CZK",
    legend = "Assets development"
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma)
```

![](notebook_files/figure-gfm/unnamed-chunk-192-1.png)<!-- -->

Auffällig ist das mittlere Einkommen: sehr viele Konten weisen negative
Vermögensentwilcklung auf

### Verteilung der Konten auf die Kunden[^29]

Ein Kunde kann mehrere Konten benutzen. Für die Entwicklung von
Kundengruppen ist es wichtig, die bis dahin gewonnenen Erkenntnisse auf
die einzelnen Kunden anzuwenden.

``` r
# Zählen, wieviele Konten pro Owner registriert sind.
owner_num_accounts <- df_cons %>% 
  select(
    account_id,
    owner_client_id
  ) %>% 
  group_by(owner_client_id) %>% 
  count()

# Ausgeben der Kunden mit den meisten verbundenen Konten
owner_num_accounts %>% 
  arrange(desc(n)) %>% 
  head()
```

    ## # A tibble: 6 × 2
    ## # Groups:   owner_client_id [6]
    ##   owner_client_id     n
    ##             <int> <int>
    ## 1               1     1
    ## 2               2     1
    ## 3               4     1
    ## 4               6     1
    ## 5               7     1
    ## 6               8     1

Jeder Owner hat nur ein Konto auf sich registriert. Nun wird geschaut,
ob der User mehrere Konten auf sich registriert hat.

``` r
# Zählen, wieviele Konten pro User registriert sind.
user_num_accounts <- df_cons %>% 
  select(
    account_id,
    user_client_id
  ) %>% 
  group_by(user_client_id) %>% 
  count()

# Ausgeben der Kunden mit den meisten verbundenen Konten
user_num_accounts %>% 
  arrange(desc(n)) %>% 
  head()
```

    ## # A tibble: 6 × 2
    ## # Groups:   user_client_id [6]
    ##   user_client_id     n
    ##            <int> <int>
    ## 1             NA  3631
    ## 2              3     1
    ## 3              5     1
    ## 4             11     1
    ## 5             16     1
    ## 6             18     1

Auch jeder User hat nur ein Konto zu Mitbenützung. Die NA’s sind bei den
Accounts, die nur einen Owner haben aber keinen zusätzlichen User.

Nun stellt sich die Frage, ob es Benutzer gibt, die Owner und User sind.

``` r
# Überprüfen, ob ein Owner auch bei einem anderen Konto User ist
df_cons %>% 
  filter(owner_client_id %in% user_client_id) %>% 
  nrow()
```

    ## [1] 0

Es gibt keine Owner, welche bei einem anderen Account als User
eingetragen sind. Wie sieht es bei den Usern aus?

``` r
# Überprüfen, ob ein User auch bei einem anderen Konto Owner ist
df_cons %>% 
  filter(user_client_id %in% owner_client_id) %>% 
  nrow()
```

    ## [1] 0

``` r
rm(owner_num_accounts, user_num_accounts)
```

Es sind auch keine User bei einem anderen Konto als Owner eingetragen.

### Kontoarten[^30]

Wir haben in vorhergehenden Analysen gesehen, dass es Konten gibt,
welche nicht dem Girokonto zugeordnet werden können. Diese Konten haben
keine häufigen und regelmässigen Transaktionen. Es könnte sich hierbei
um Sparkonten handeln. Diese Konten wollen wir nun genauer anschauen.

#### Sparkonten[^31]

Die Sparkonten haben die Eigenschaft, dass sie mit keinen Daueraufträgen
oder Darlehen verbunden sind.

Inspizieren der Konten, welche keine Girokontos sind und daher
Sparkonten sein könnten.

Die meisten Konten wurden um 1996 eröffnet. Für Sparkonto spricht, dass
bis auf ein Konto alle eine positive Vermögensentwicklung aufweisen.
Dagegen spricht aber wieder, dass bei einigen Konten Daueraufträge
eingerichtet oder mit einem Darlehen verbunden sind. Folgehypothese: es
sind einerseits Sparkonten (solche ohne Daueraufträge oder Darlehen) und
die restlichen Darlehenskonten, verbunden mit Daueraufträgen und oder
Darlehen.

#### Spalte Sparkonto (is_savings_account)[^32]

``` r
df_cons <- df_cons %>% 
  mutate(is_savings_account = ifelse(is_checking_account == FALSE & is.na(loan_id) & is.na(order_num_household), TRUE, FALSE))

summary(df_cons$is_savings_account)
```

    ##    Mode   FALSE    TRUE 
    ## logical    4395     105

#### Spalte Darlehenskonto (is_loan_account)[^33]

``` r
df_cons <- df_cons %>% 
  mutate(is_loan_account = ifelse(is_checking_account == FALSE & is_savings_account == FALSE, TRUE, FALSE))

summary(df_cons$is_loan_account)
```

    ##    Mode   FALSE    TRUE 
    ## logical    4442      58

#### Vermögensentwicklung bei Konten mit hohem Eikommen[^34]

``` r
df_cons %>% 
  filter(salary_groups == "high-income") %>% 
  top_n(3, assets_dev)
```

    ##   account_id account_frequency account_opening_date account_opening_year
    ## 1       2245           Monthly           1993-03-30                 1993
    ## 2       8132            Weekly           1993-04-17                 1993
    ## 3       8856           Monthly           1994-12-18                 1994
    ##   account_opening_month account_num_of_user account_district_name
    ## 1                     3                   1              Rokycany
    ## 2                     4                   1                 Kolin
    ## 3                    12                   1         Cesky Krumlov
    ##   account_district_region account_district_average_salary
    ## 1            west Bohemia                            8843
    ## 2         central Bohemia                            9307
    ## 3           south Bohemia                            9045
    ##   account_district_inhabitants order_total_amount_household
    ## 1                        45714                            0
    ## 2                        95616                           NA
    ## 3                        58796                           NA
    ##   order_total_amount_loan order_total_amount_insurrance
    ## 1                  7248.8                             0
    ## 2                      NA                            NA
    ## 3                      NA                            NA
    ##   order_total_amount_unknown order_total_amount_leasing order_num_household
    ## 1                          0                          0                   0
    ## 2                         NA                         NA                  NA
    ## 3                         NA                         NA                  NA
    ##   order_num_loan order_num_insurrance order_num_unknown order_num_leasing
    ## 1              1                    0                 0                 0
    ## 2             NA                   NA                NA                NA
    ## 3             NA                   NA                NA                NA
    ##   loan_start_date loan_end_date loan_duration_in_month loan_duration_in_years
    ## 1      1995-01-16    1999-01-16                     48                      4
    ## 2            <NA>          <NA>                     NA                     NA
    ## 3            <NA>          <NA>                     NA                     NA
    ##   loan_total_amount loan_redemption_amount      loan_status loan_in_dept
    ## 1            347952                   7249 CONTRACT OPEN OK        FALSE
    ## 2                NA                     NA             <NA>           NA
    ## 3                NA                     NA             <NA>           NA
    ##   card_type card_issued owner_dateofbirth owner_sex
    ## 1      <NA>        <NA>        1964-10-30    Female
    ## 2      <NA>        <NA>        1946-09-17    Female
    ## 3      GOLD  1998-07-27        1963-01-09      Male
    ##   owner_age_at_account_opening owner_district_name owner_district_region
    ## 1                           28            Rokycany          west Bohemia
    ## 2                           46               Kolin       central Bohemia
    ## 3                           31       Cesky Krumlov         south Bohemia
    ##   owner_district_average_salary owner_district_inhabitants user_dateofbirth
    ## 1                          8843                      45714             <NA>
    ## 2                          9307                      95616             <NA>
    ## 3                          9045                      58796             <NA>
    ##   user_sex user_age_at_account_opening account_district_id loan_id card_id
    ## 1     <NA>                          NA                  29    5418      NA
    ## 2     <NA>                          NA                   5      NA      NA
    ## 3     <NA>                          NA                  15      NA    1076
    ##   owner_client_id owner_disp_id owner_district_id user_client_id user_disp_id
    ## 1            2719          2719                29             NA           NA
    ## 2           10046          9738                 5             NA           NA
    ## 3           10912         10604                15             NA           NA
    ##   user_district_id balance_1993-01-31 balance_1993-02-28 balance_1993-03-31
    ## 1               NA                  0                  0                300
    ## 2               NA                  0                  0                  0
    ## 3               NA                  0                  0                  0
    ##   balance_1993-04-30 balance_1993-05-31 balance_1993-06-30 balance_1993-07-31
    ## 1              10700              15700            48461.1            45603.0
    ## 2                700              25240            43780.6            47305.7
    ## 3                  0                  0                0.0                0.0
    ##   balance_1993-08-31 balance_1993-09-30 balance_1993-10-31 balance_1993-11-30
    ## 1            35492.1            42876.4            33854.3            54142.4
    ## 2            46941.8            23964.3            38642.4            37482.8
    ## 3                0.0                0.0                0.0                0.0
    ##   balance_1993-12-31 balance_1994-01-31 balance_1994-02-28 balance_1994-03-31
    ## 1            38153.3            24060.1            39391.0              56443
    ## 2            21625.3             8084.7             3634.6              16063
    ## 3                0.0                0.0                0.0                  0
    ##   balance_1994-04-30 balance_1994-05-31 balance_1994-06-30 balance_1994-07-31
    ## 1            26990.0            76628.9            45388.2            60429.8
    ## 2            29366.9            32144.6            57553.7            57948.3
    ## 3                0.0                0.0                0.0                0.0
    ##   balance_1994-08-31 balance_1994-09-30 balance_1994-10-31 balance_1994-11-30
    ## 1            90667.3            44575.1            87530.4            45355.5
    ## 2            60437.5            73113.9            70370.0            59464.6
    ## 3                0.0                0.0                0.0                0.0
    ##   balance_1994-12-31 balance_1995-01-31 balance_1995-02-28 balance_1995-03-31
    ## 1            70846.3            46071.1            80105.5            77133.9
    ## 2            57941.3            13410.1            50999.7            68696.2
    ## 3              200.0            40813.9            58563.0            43996.4
    ##   balance_1995-04-30 balance_1995-05-31 balance_1995-06-30 balance_1995-07-31
    ## 1            50244.7            38564.1            63809.2            44664.1
    ## 2            75836.2            81160.3            84947.9            73085.7
    ## 3            46706.2            30707.5            25460.2            16069.9
    ##   balance_1995-08-31 balance_1995-09-30 balance_1995-10-31 balance_1995-11-30
    ## 1            48392.5            58777.8            70931.9            41309.1
    ## 2            70727.4            64679.1            75780.3            77024.6
    ## 3             4091.8            45844.0            75441.3           105520.7
    ##   balance_1995-12-31 balance_1996-01-31 balance_1996-02-29 balance_1996-03-31
    ## 1            67237.9            32473.3           103389.7            93664.9
    ## 2            90988.7            41574.5            68210.8            84020.6
    ## 3            75103.9            43906.4            60678.5            62896.6
    ##   balance_1996-04-30 balance_1996-05-31 balance_1996-06-30 balance_1996-07-31
    ## 1            61079.9            41619.6            38639.9            44263.1
    ## 2            91385.6            71389.5            77502.3            75725.4
    ## 3            86240.8            94380.0            79730.3            91782.0
    ##   balance_1996-08-31 balance_1996-09-30 balance_1996-10-31 balance_1996-11-30
    ## 1            85399.2            92646.8            71672.9            88434.5
    ## 2            71806.3            76348.0            53504.6            53176.2
    ## 3            57812.1            51987.5            81064.5           119543.3
    ##   balance_1996-12-31 balance_1997-01-31 balance_1997-02-28 balance_1997-03-31
    ## 1            88937.8            41299.9            38695.0            41455.3
    ## 2            61306.7            62498.4            67749.3            60092.9
    ## 3            59827.6            42953.4            68722.1            41592.1
    ##   balance_1997-04-30 balance_1997-05-31 balance_1997-06-30 balance_1997-07-31
    ## 1            31919.3            33585.6            34957.8            35332.8
    ## 2            72947.6            74960.9            71184.0            75407.4
    ## 3            39523.5            82391.5           111050.7            82198.7
    ##   balance_1997-08-31 balance_1997-09-30 balance_1997-10-31 balance_1997-11-30
    ## 1            32901.6            33568.3            33335.1            34004.3
    ## 2            78167.2            79838.8            80424.2            87033.0
    ## 3            50240.9           132286.0           100180.9            64653.9
    ##   balance_1997-12-31 balance_1998-01-31 balance_1998-02-28 balance_1998-03-31
    ## 1            35777.8            99913.6           103602.2           110568.8
    ## 2            65188.1            69342.5            85916.0            89090.8
    ## 3           108181.7            54677.7            80176.2            65914.4
    ##   balance_1998-04-30 balance_1998-05-31 balance_1998-06-30 balance_1998-07-31
    ## 1           100690.1            94573.5            46103.2            97459.6
    ## 2            87437.8            71033.8            90642.3            95815.7
    ## 3           105470.9            74225.9            98199.8           104005.9
    ##   balance_1998-08-31 balance_1998-09-30 balance_1998-10-31 balance_1998-11-30
    ## 1            90940.2            84631.6            84976.2            87836.6
    ## 2           103225.9           112389.5           111297.5            90693.8
    ## 3           117577.7            97514.2           103484.5           103569.5
    ##   balance_1998-12-31 incomes_quarterly_1993.1 incomes_quarterly_1993.2
    ## 1           125279.8                      300                  72561.1
    ## 2           124602.8                        0                  63880.6
    ## 3           138317.2                        0                      0.0
    ##   incomes_quarterly_1993.3 incomes_quarterly_1993.4 incomes_quarterly_1994.1
    ## 1                  46715.4                  89966.2                 159467.5
    ## 2                  76256.8                  88786.2                  92624.4
    ## 3                      0.0                      0.0                      0.0
    ##   incomes_quarterly_1994.2 incomes_quarterly_1994.3 incomes_quarterly_1994.4
    ## 1                 106752.0                 229614.7                 129882.1
    ## 2                  96040.5                  76662.4                  89510.4
    ## 3                      0.0                      0.0                    200.0
    ##   incomes_quarterly_1995.1 incomes_quarterly_1995.2 incomes_quarterly_1995.3
    ## 1                    98408                  52561.6                  75558.9
    ## 2                    88667                  89604.6                  77006.3
    ## 3                   168011                  85107.6                  83510.7
    ##   incomes_quarterly_1995.4 incomes_quarterly_1996.1 incomes_quarterly_1996.2
    ## 1                 101250.4                 152516.3                  20465.2
    ## 2                  89756.4                  77900.9                  89671.9
    ## 3                 291737.7                 127230.5                 138095.5
    ##   incomes_quarterly_1996.3 incomes_quarterly_1996.4 incomes_quarterly_1997.1
    ## 1                 118574.2                 124294.3                  20807.8
    ## 2                  76835.7                 100218.8                  82076.2
    ## 3                  75801.1                 153082.9                  52808.3
    ##   incomes_quarterly_1997.2 incomes_quarterly_1997.3 incomes_quarterly_1997.4
    ## 1                  22004.8                  20400.7                  25499.9
    ## 2                  89581.1                  76644.8                  89639.3
    ## 3                 120502.4                 231179.2                 144039.6
    ##   incomes_quarterly_1998.1 incomes_quarterly_1998.2 incomes_quarterly_1998.3
    ## 1                 162217.2                 241035.7                 257406.7
    ## 2                  80992.7                  89700.5                  77337.2
    ## 3                  63592.6                 111629.2                 113565.4
    ##   incomes_quarterly_1998.4 outgoes_quarterly_1993.1 outgoes_quarterly_1993.2
    ## 1                 177904.8                        0                    24400
    ## 2                  90473.4                        0                    20100
    ## 3                 196232.1                        0                        0
    ##   outgoes_quarterly_1993.3 outgoes_quarterly_1993.4 outgoes_quarterly_1994.1
    ## 1                    52300                  94689.2                 141177.8
    ## 2                    96073                  91125.0                  98186.6
    ## 3                        0                      0.0                      0.0
    ##   outgoes_quarterly_1994.2 outgoes_quarterly_1994.3 outgoes_quarterly_1994.4
    ## 1                 117806.8                 230427.8                 103610.8
    ## 2                  54550.0                  61102.0                 104683.0
    ## 3                      0.0                      0.0                      0.0
    ##   outgoes_quarterly_1995.1 outgoes_quarterly_1995.2 outgoes_quarterly_1995.3
    ## 1                  92120.4                  65886.2                  80590.2
    ## 2                  77912.0                  73353.0                  97275.0
    ## 3                 124214.6                 103643.8                  63126.8
    ##   outgoes_quarterly_1995.4 outgoes_quarterly_1996.1 outgoes_quarterly_1996.2
    ## 1                  92790.2                 126089.2                  75490.2
    ## 2                  63447.0                  84869.0                  96190.0
    ## 3                 262477.8                 139437.8                 121261.8
    ##   outgoes_quarterly_1996.3 outgoes_quarterly_1996.4 outgoes_quarterly_1997.1
    ## 1                  64567.2                 128003.2                  68290.2
    ## 2                  77990.0                 115260.0                  83290.0
    ## 3                 103543.8                 145242.8                  71043.8
    ##   outgoes_quarterly_1997.2 outgoes_quarterly_1997.3 outgoes_quarterly_1997.4
    ## 1                  28502.2                  21790.2                  23290.2
    ## 2                  78490.0                  67990.0                 104290.0
    ## 3                  51043.8                 209943.8                 168143.8
    ##   outgoes_quarterly_1998.1 outgoes_quarterly_1998.2 outgoes_quarterly_1998.3
    ## 1                  87426.2                 305501.2                 218878.2
    ## 2                  57090.0                  88149.0                  55590.0
    ## 3                 105859.8                  79343.8                 114250.8
    ##   outgoes_quarterly_1998.4 incomes_yearly_1993 incomes_yearly_1994
    ## 1                 137256.6            209542.7            625716.3
    ## 2                  78260.0            228923.6            354837.7
    ## 3                 155429.2                 0.0               200.0
    ##   incomes_yearly_1995 incomes_yearly_1996 incomes_yearly_1997
    ## 1            327778.9            415850.0             88713.2
    ## 2            345034.3            344627.3            337941.4
    ## 3            628367.0            494210.0            548529.5
    ##   incomes_yearly_1998 outgoes_yearly_1993 outgoes_yearly_1994
    ## 1            838564.4            171389.2            593023.2
    ## 2            338503.8            207298.0            318521.6
    ## 3            485019.3                 0.0                 0.0
    ##   outgoes_yearly_1995 outgoes_yearly_1996 outgoes_yearly_1997
    ## 1              331387            394149.8            141872.8
    ## 2              311987            374309.0            334060.0
    ## 3              553463            509486.2            500175.2
    ##   outgoes_yearly_1998 owner_current_age age_groups has_creditcard
    ## 1            749062.2                34      31-40          FALSE
    ## 2            279089.0                52      51-60          FALSE
    ## 3            454883.6                35      31-40           TRUE
    ##   is_checking_account outgoes_avg income_avg is_regular_income
    ## 1                TRUE      396814     417694              TRUE
    ## 2                TRUE      304211     324978              TRUE
    ## 3                TRUE      504502     431265              TRUE
    ##   is_regular_income_80 consultation_on_site salary_groups assets_dev
    ## 1                 TRUE                 TRUE   high-income   125279.8
    ## 2                 TRUE                 TRUE   high-income   124602.8
    ## 3                 TRUE                 TRUE   high-income   138317.2
    ##   assets_dev_groups is_savings_account is_loan_account
    ## 1          positive              FALSE           FALSE
    ## 2          positive              FALSE           FALSE
    ## 3          positive              FALSE           FALSE

Die Top 3 Accounts werden von Personen betrieben, welche alle in der
Region Böhmen gemeldet sind. Das Alter der Konto-owner ist von 28 bis 46
jährig.

#### Altesverteilung der Kontoinhaber aufgeteilt auf den Einkommensklassen[^35]

``` r
df_cons %>% 
  group_by(
    !is.na(salary_groups)
  ) %>% 
  summarise(owner_age_median = median(owner_age_at_account_opening)) %>% 
  arrange(owner_age_median)
```

    ## # A tibble: 2 × 2
    ##   `!is.na(salary_groups)` owner_age_median
    ##   <lgl>                              <dbl>
    ## 1 FALSE                                 33
    ## 2 TRUE                                  40

Bei den hohen Einkommen ist der Altersdurchschnitt am tiefsten. Dies
widerspricht der allgemeinen Annahme, dass eine Person mit zunehmenden
Alter auch mehr Einkommen hat.

### Bargeldbezüge ohne Angabe des Verwendungszwecks[^36]

Vorbereiten des Datensatzes.

``` r
owner_district <- df_cons %>% 
  select(
    account_id,
    owner_district_id,
    account_district_id, 
    owner_district_name,
    owner_district_region,
    account_district_name, 
    account_district_region
  )

# trans_correlation <- date_complete %>% 
#   left_join(owner_district, by = "account_id")

trans_correlation <- df_transaction_complete %>% 
  left_join(owner_district, by = "account_id")

head(owner_district, 3)
```

    ##   account_id owner_district_id account_district_id owner_district_name
    ## 1          1                18                  18               Pisek
    ## 2          2                 1                   1         Hl.m. Praha
    ## 3          3                 5                   5               Kolin
    ##   owner_district_region account_district_name account_district_region
    ## 1         south Bohemia                 Pisek           south Bohemia
    ## 2                Prague           Hl.m. Praha                  Prague
    ## 3       central Bohemia                 Kolin         central Bohemia

``` r
head(trans_correlation, 3)
```

    ## # A tibble: 3 × 23
    ##   account_id date       trans_id balance balance_end_day cashflow operation
    ##        <int> <date>        <int>   <dbl>           <dbl> <fct>    <fct>    
    ## 1          1 1993-01-01       NA       0               0 <NA>     <NA>     
    ## 2          1 1993-01-02       NA       0               0 <NA>     <NA>     
    ## 3          1 1993-01-03       NA       0               0 <NA>     <NA>     
    ## # ℹ 16 more variables: amount <dbl>, characterization <fct>, bank_name <fct>,
    ## #   account_nr <int>, incomes <dbl>, outgoes <dbl>,
    ## #   outgoes_incomes_equal <lgl>, first_transaction <date>,
    ## #   last_transaction <date>, duration <drtn>, owner_district_id <int>,
    ## #   account_district_id <int>, owner_district_name <fct>,
    ## #   owner_district_region <fct>, account_district_name <fct>,
    ## #   account_district_region <fct>

``` r
rm(owner_district)
```

#### Varialbelbeschreibung (Levels)[^37]

Folgende Levels sind vorhanden:

- “CASH CREDIT” -\> Bargeldeinzahlung
- “CASH WIDTHDRAWAL” -\> Bargeldbezug
- “COLLECTION OTHER BANK” -\> Abbuchung einer Fremdbank
- “CREDIT CARD WITHDRAWAL” -\> Kreditkartenbezug
- “REMITTANCE OTHER BANK” -\> Überweisung an/von einer Fremdbank

Abfrage der Levels in der Spalte “operation”

``` r
levels(trans_correlation$operation)
```

    ## [1] "CASH CREDIT"            "CASH WIDTHDRAWAL"       "COLLECTION OTHER BANK" 
    ## [4] "CREDIT CARD WITHDRAWAL" "REMITTANCE OTHER BANK"

Datensatz filtern nach Bargeldbezügen.

``` r
cashwidthdrawals <- trans_correlation %>% 
  filter(operation == "CASH WIDTHDRAWAL")

rm(trans_correlation)

set.seed(26345)
slice_sample(cashwidthdrawals, n = 5)
```

    ## # A tibble: 5 × 23
    ##   account_id date       trans_id balance balance_end_day cashflow operation     
    ##        <int> <date>        <int>   <dbl>           <dbl> <fct>    <fct>         
    ## 1       1944 1993-09-30   571489  35204.          35204. OUT      CASH WIDTHDRA…
    ## 2        163 1996-02-29    48987  20457.          20457. OUT      CASH WIDTHDRA…
    ## 3       3020 1996-11-30   886253  28401.          28401. OUT      CASH WIDTHDRA…
    ## 4       3045 1998-12-19   893451  17892.          17892. OUT      CASH WIDTHDRA…
    ## 5        427 1996-05-14   127482  52433.          52433. OUT      CASH WIDTHDRA…
    ## # ℹ 16 more variables: amount <dbl>, characterization <fct>, bank_name <fct>,
    ## #   account_nr <int>, incomes <dbl>, outgoes <dbl>,
    ## #   outgoes_incomes_equal <lgl>, first_transaction <date>,
    ## #   last_transaction <date>, duration <drtn>, owner_district_id <int>,
    ## #   account_district_id <int>, owner_district_name <fct>,
    ## #   owner_district_region <fct>, account_district_name <fct>,
    ## #   account_district_region <fct>

Die Beträge mit “STATEMENT PAYMENT” aus der Spalte “characterization”
sind Gebühren und werden aus der Analyse ausgeschlossen.

``` r
cashwidthdrawals <- cashwidthdrawals %>% 
  filter(characterization != "STATEMENT PAYMENT")
  

set.seed(26345)
slice_sample(cashwidthdrawals, n = 5)
```

    ## # A tibble: 5 × 23
    ##   account_id date       trans_id balance balance_end_day cashflow operation     
    ##        <int> <date>        <int>   <dbl>           <dbl> <fct>    <fct>         
    ## 1       4774 1994-06-19  1401399  22378.          22378. OUT      CASH WIDTHDRA…
    ## 2        347 1996-03-31  3445387   -925.           -940  OUT      CASH WIDTHDRA…
    ## 3       2346 1998-12-31  3607597  -2236.          -2236. OUT      CASH WIDTHDRA…
    ## 4      10694 1997-01-02  3222633  40943.          40943. OUT      CASH WIDTHDRA…
    ## 5       6609 1996-09-30  3677226  -1147           -1159. OUT      CASH WIDTHDRA…
    ## # ℹ 16 more variables: amount <dbl>, characterization <fct>, bank_name <fct>,
    ## #   account_nr <int>, incomes <dbl>, outgoes <dbl>,
    ## #   outgoes_incomes_equal <lgl>, first_transaction <date>,
    ## #   last_transaction <date>, duration <drtn>, owner_district_id <int>,
    ## #   account_district_id <int>, owner_district_name <fct>,
    ## #   owner_district_region <fct>, account_district_name <fct>,
    ## #   account_district_region <fct>

Ebenso verhält es sich bei “SANCTION INTEREST”. Auch diese Beobachtungen
werden ausgeschlossen.

``` r
cashwidthdrawals <- cashwidthdrawals %>% 
  filter(characterization != "SANCTION INTEREST")

set.seed(26345)
slice_sample(cashwidthdrawals, n = 5)
```

    ## # A tibble: 5 × 23
    ##   account_id date       trans_id balance balance_end_day cashflow operation     
    ##        <int> <date>        <int>   <dbl>           <dbl> <fct>    <fct>         
    ## 1       4774 1995-12-12  1401417  65742.          56054. OUT      CASH WIDTHDRA…
    ## 2       6118 1996-07-05  1804072  66200.          66200. OUT      CASH WIDTHDRA…
    ## 3      10018 1997-03-08  3021228   7843.           7843. OUT      CASH WIDTHDRA…
    ## 4        773 1996-04-01   227360  41443.          41443. OUT      CASH WIDTHDRA…
    ## 5       5011 1996-11-11  1470609   9189            9189  OUT      CASH WIDTHDRA…
    ## # ℹ 16 more variables: amount <dbl>, characterization <fct>, bank_name <fct>,
    ## #   account_nr <int>, incomes <dbl>, outgoes <dbl>,
    ## #   outgoes_incomes_equal <lgl>, first_transaction <date>,
    ## #   last_transaction <date>, duration <drtn>, owner_district_id <int>,
    ## #   account_district_id <int>, owner_district_name <fct>,
    ## #   owner_district_region <fct>, account_district_name <fct>,
    ## #   account_district_region <fct>

Überblick über die Bargeldbezüge “CASH WIDTHDRAWAL”

``` r
summary(cashwidthdrawals)
```

    ##    account_id         date               trans_id          balance      
    ##  Min.   :   19   Min.   :1993-05-10   Min.   :   4635   Min.   :-18391  
    ##  1st Qu.: 3037   1st Qu.:1995-11-30   1st Qu.: 890906   1st Qu.:  9063  
    ##  Median : 5387   Median :1997-02-28   Median :1584462   Median : 22262  
    ##  Mean   : 5548   Mean   :1996-12-04   Mean   :1652646   Mean   : 27628  
    ##  3rd Qu.: 7828   3rd Qu.:1998-02-09   3rd Qu.:2374350   3rd Qu.: 39342  
    ##  Max.   :11325   Max.   :1998-12-22   Max.   :3412050   Max.   :126774  
    ##                                                                         
    ##  balance_end_day  cashflow                    operation        amount     
    ##  Min.   :-18391   IN :   0   CASH CREDIT           :   0   Min.   :  200  
    ##  1st Qu.:  8957   OUT:2834   CASH WIDTHDRAWAL      :2834   1st Qu.: 4400  
    ##  Median : 22124              COLLECTION OTHER BANK :   0   Median : 6900  
    ##  Mean   : 27301              CREDIT CARD WITHDRAWAL:   0   Mean   : 7913  
    ##  3rd Qu.: 38656              REMITTANCE OTHER BANK :   0   3rd Qu.:11700  
    ##  Max.   :126774                                            Max.   :18600  
    ##                                                                           
    ##            characterization   bank_name      account_nr    incomes       
    ##  CREDIT INTEREST   :   0           :2834   Min.   :0    Min.   :    0.0  
    ##  HOUSEHOLD         :2811    AB     :   0   1st Qu.:0    1st Qu.:    0.0  
    ##  INSURRANCE PAYMENT:  23    CD     :   0   Median :0    Median :    0.0  
    ##  LOAN PAYMENT      :   0    EF     :   0   Mean   :0    Mean   :  867.8  
    ##  OLD AGE PENSION   :   0    GH     :   0   3rd Qu.:0    3rd Qu.:    0.0  
    ##  SANCTION INTEREST :   0    IJ     :   0   Max.   :0    Max.   :70616.0  
    ##  STATEMENT PAYMENT :   0    (Other):   0                                 
    ##     outgoes      outgoes_incomes_equal first_transaction   
    ##  Min.   :  200   Mode :logical         Min.   :1993-01-03  
    ##  1st Qu.: 4600   FALSE:2834            1st Qu.:1993-06-08  
    ##  Median : 7200                         Median :1993-12-11  
    ##  Mean   : 8582                         Mean   :1994-08-04  
    ##  3rd Qu.:12200                         3rd Qu.:1995-08-30  
    ##  Max.   :63000                         Max.   :1997-10-14  
    ##                                                            
    ##  last_transaction       duration        owner_district_id account_district_id
    ##  Min.   :1998-11-30   Length:2834       Min.   : 1.00     Min.   : 1.00      
    ##  1st Qu.:1998-12-31   Class :difftime   1st Qu.:17.00     1st Qu.:15.00      
    ##  Median :1998-12-31   Mode  :numeric    Median :44.00     Median :31.00      
    ##  Mean   :1998-12-28                     Mean   :40.31     Mean   :37.92      
    ##  3rd Qu.:1998-12-31                     3rd Qu.:63.00     3rd Qu.:66.00      
    ##  Max.   :1998-12-31                     Max.   :74.00     Max.   :74.00      
    ##                                                                              
    ##       owner_district_name   owner_district_region     account_district_name
    ##  Hl.m. Praha    : 468     north Moravia:677       Hl.m. Praha    : 468     
    ##  Cesky Krumlov  : 162     Prague       :468       Cesky Krumlov  : 162     
    ##  Karvina        : 158     south Moravia:435       Karvina        : 158     
    ##  Ostrava - mesto: 144     west Bohemia :410       Ostrava - mesto: 144     
    ##  Olomouc        : 120     south Bohemia:390       Olomouc        : 120     
    ##  Tachov         : 114     east Bohemia :237       Tachov         : 114     
    ##  (Other)        :1668     (Other)      :217       (Other)        :1668     
    ##   account_district_region
    ##  north Moravia:677       
    ##  Prague       :468       
    ##  south Moravia:435       
    ##  west Bohemia :410       
    ##  south Bohemia:390       
    ##  east Bohemia :237       
    ##  (Other)      :217

Die Bargeldbezüge mit der Bezeichnung “CASH WIDTHDRAWAL” haben alle eine
zugewiesene Verwendung. Entweder für das Wohnen (Household) oder die
Versicherung (Insurrance Payment). Es handelt sich hierbei um 2834
Transaktionen. Dies ist auf den gesamten Datensatz gesehen sehr wenig.
Trotzdem werden diese Zahlungen genauer untersucht.

``` r
cashwidthdrawals %>% 
  group_by(account_id,
           characterization) %>% 
  mutate(amount = mean(amount)) %>% 
ggplot(aes(x = date, y = amount, color = characterization)) +
  geom_point(position = "jitter") +
  scale_fill_viridis_d(alpha = 0.6, option = "A") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15)
    ) +
    ggtitle("Distribution of cash widthrawals") +
  labs(
    color = "Purpose",
    x = "",
    y = "Amount per purchase [CZK]",
    subtitle = "Time span: 1993 - 1998, across all accounts"
    )
```

![](notebook_files/figure-gfm/unnamed-chunk-208-1.png)<!-- -->

Die Beträge für Versicherungszwecke sind alle auf einem sehr tiefen
Niveau. Die höheren Beträge (ab 10’000 CZK) werden nun genauer
untersucht.

#### Bargeldbezüge über 10’000 CZK[^38]

Anzahl Bezüge mit anteilmässiger Verteilung auf die Regionen.

``` r
# Filtern auf Beträge über 10k
cashwidthdrawals_10 <- cashwidthdrawals %>%
  filter(amount >= 10000) %>%
  mutate(Region = as.factor(owner_district_region))

#Visualisierung in einer Heatmap
cashwidthdrawals_10 %>% 
  mutate(year = year(date)) %>% 
  group_by(year, Region) %>% 
  summarise(amount = round(mean(amount), digits = 0)) %>%
  ggplot(aes(year, Region, fill = amount)) +
  geom_tile() +
  scale_fill_gradient(
    low = "white", 
    high = "blue") +
  theme(legend.position = "none") +
  labs(
    title = "Cashwithdrawals mean Amount [CZK]",
    subtitle = "Distribution per year and region",
    x = "",
    y = ""
    ) +
  geom_text(aes(label = comma(amount)), size = 3, hjust = 0.5)
```

![](notebook_files/figure-gfm/unnamed-chunk-209-1.png)<!-- -->

``` r
rm(cashwidthdrawals, cashwidthdrawals_10)
```

Die hohen Bezüge sind über das ganze Land verteilt. Die Region West
Böhmen weist am häufigsten, Nord und Zentral Böhmen am wenigsten hohe
Bargeldbezüge aus.

#### Das Gebiet Moravia (Mähren)[^39]

In der Region Moravia sind einige wichtige Wirtschaftszentren
angesiedelt. Auch die Landwirtschaft spielt eine wichtige Rolle vor
allem der Weinbau. Über 90% der Weinberge befinden sich im Gebiet
Mähren. Das Gebiet verfügt weiter über reichlich Braunkohle und Erdöl.
Auch die Automobil- und die tschechische Feuerwaffenindustrie haben ihre
Produktionsstätten in dieser Region. Das Gebiet um Zlín ist für
Flugzeughersteller bekannt. Mehrere Firmen haben dort ihren Sitz.

### Kreditkarten[^40]

Wieviele Accounts haben eine Kreditkarte?

``` r
df_cons %>% 
  filter(!is.na(card_id)) %>% 
  count()
```

    ##     n
    ## 1 892

Nur 892 Accounts von gesamthaft 4500 sind mit einer Kreditkarte
verbunden.

``` r
df_cons %>% 
  ggplot(aes(x = has_creditcard, fill = has_creditcard)) +
  geom_bar() +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 15)
  ) +
  labs(
    title = "Num of existing accounts linked to a credit card",
    x = "",
    y = ""
  ) +
  scale_x_discrete(labels = c("FALSE" = "no creditcard",
                              "TRUE" = "with creditcard")) +
  geom_text(stat = 'count', aes(label =..count..), vjust = 3, size = 5)
```

![](notebook_files/figure-gfm/unnamed-chunk-211-1.png)<!-- -->

#### Analysen zur Kreditkarte[^41]

Wieviele Kreditkarten werden benutzt?

``` r
# Accounts mit Kreditkarten
cc <- df_cons %>% 
  filter(has_creditcard == TRUE)

# filtern nach Accounts mit Kreditkarten
cc_trans <- df_transaction_complete %>% 
  filter(account_id %in% cc$account_id,
         operation == "CREDIT CARD WITHDRAWAL") %>% 
  group_by(account_id) %>% 
  count() %>% 
  rename(cardwidthdrawals_num = n)

# Ausgabe der Anzahl Accounts
cc_trans %>% 
  nrow()
```

    ## [1] 807

``` r
# Anfügen der neuen Spalte an das df_cons
df_cons <- df_cons %>% 
  left_join(cc_trans, by = "account_id")
```

807 Accounts von 892 benutzen ihre Kreditkarte.

Konten mit Kreditkarte, welche aber die Kreditkarte noch nicht
eingesetzt haben.

``` r
cc_without_trans <- cc %>% 
  anti_join(cc_trans, by = "account_id")

dim(cc_without_trans)
```

    ## [1]  85 195

Erster Einsatz der Kreditkarten.

``` r
foo <- df_cons %>% 
  select(account_id, account_opening_year)

# Erstellen neuer Spalten für die Differenz zwischen Accounteröffnung und erstem Einsatz der Kreditkarte
cc_first <- df_transaction_complete %>%
  left_join(foo, by = "account_id") %>%  
  filter(account_id %in% cc$account_id,
         operation == "CREDIT CARD WITHDRAWAL") %>% 
  group_by(account_id) %>% 
  arrange(date) %>% 
  slice_head(n = 1) %>% 
  mutate(first_trans = year(date)) %>% 
  select(account_id,
         amount, 
         first_trans,
         account_opening_year) %>% 
  mutate(diff_year = first_trans - account_opening_year)

rm(foo)

# Zählen, in welchem Jahr wieviele Kreditkarten das erste Mal eingesetzt wurden.
cc_first %>% 
  group_by(first_trans) %>% 
  count() %>% 
  arrange(desc(n))
```

    ## # A tibble: 5 × 2
    ## # Groups:   first_trans [5]
    ##   first_trans     n
    ##         <dbl> <int>
    ## 1        1998   408
    ## 2        1997   224
    ## 3        1996   105
    ## 4        1995    53
    ## 5        1994    17

``` r
# Visualisierung erste Bezüge mit der Karte
cc_first %>% 
  ggplot(aes(first_trans, fill = as.factor(first_trans))) +
  geom_bar()
```

![](notebook_files/figure-gfm/unnamed-chunk-214-1.png)<!-- -->

An der Verteilung auf die Jahre ist sehr gut zu erkennen, dass die
Kreditkarte immer mehr an Beliebtheit gewonnen hat. Nun ist der richtige
Zeitpunkt gekommen, um die Kreditkarte bei der breiten Masse der
Kundschaft beliebt zu machen.

Wieviele Jahre sind verstrichen nach Kontoeröffnung bis zum ersten
Einsatz der Kreditkarte?

``` r
cc_first %>% 
  group_by(diff_year) %>% 
  count() %>% 
  arrange(desc(diff_year))
```

    ## # A tibble: 6 × 2
    ## # Groups:   diff_year [6]
    ##   diff_year     n
    ##       <dbl> <int>
    ## 1         5    55
    ## 2         4    75
    ## 3         3   129
    ## 4         2   252
    ## 5         1   284
    ## 6         0    12

``` r
# Entfernen der nicht mehr benötigten Data Frames
rm(cc, cc_first, cc_trans, cc_without_trans)
```

Es sind nicht nur Neukunden, welche eine Kreditkarte besitzen.

Überblick über die Höhe der jeweiligen Bezüge.

``` r
# nach Kreditkartenbezügen filtern und neue Spalte für Durchschnittsbetrag 
cardwidthdrawals <- df_transaction_complete %>% 
  filter(operation == "CREDIT CARD WITHDRAWAL") %>% 
  group_by(account_id) %>% 
  summarise(cardwidthdrawals_amount_mean = mean(amount))

# neue Info an df_cons anfügen
df_cons <- df_cons %>% 
  left_join(cardwidthdrawals, by = "account_id") 

df_cons %>% 
  select(cardwidthdrawals_amount_mean) %>% 
  summary()
```

    ##  cardwidthdrawals_amount_mean
    ##  Min.   : 100                
    ##  1st Qu.:1939                
    ##  Median :2280                
    ##  Mean   :2355                
    ##  3rd Qu.:2700                
    ##  Max.   :6600                
    ##  NA's   :3693

Die entstandenen NA’s im df_cons sind in der Spalte
“cardwidthdrawals_amount_mean” überall dort entstanden wo keine
Kreditkartenbezüge registriert sind.

Um den Konten, welche zwar mit einer Kreditkarte verbunden sind aber
noch keine Bezüge gemacht haben, werden die NA-Werte durch 0 ersetzt.
Somit weist die Spalte nur noch NA’s auf, wenn keine Kreditkarte
verbunden ist.

``` r
# Filtern der Kreditkartenbezüge für die Imputation mit 0
foo <- df_cons %>% 
  filter(!is.na(card_id),
         is.na(cardwidthdrawals_amount_mean))

# Wert in der neuen Spalte anpassen
df_cons <- df_cons %>% 
  mutate(cardwidthdrawals_amount_mean = ifelse(account_id %in% foo$account_id, 0, cardwidthdrawals_amount_mean)) 

# Übersicht über die Kreditkartenbezüge
df_cons %>% 
  ggplot(aes(cardwidthdrawals_amount_mean, fill = owner_district_region)) +
  geom_boxplot()
```

![](notebook_files/figure-gfm/unnamed-chunk-217-1.png)<!-- -->

``` r
df_cons %>% 
  ggplot(aes(cardwidthdrawals_amount_mean, fill = "red")) +
  geom_histogram(binwidth = 25)
```

![](notebook_files/figure-gfm/unnamed-chunk-217-2.png)<!-- -->

``` r
rm(foo)  
```

Der durchschnittliche Betrag liegt bei 2200 CZK, der höchste bei 8000
CZK. Der Höchste Betrag ist vermutlich im Zusammenhang mit der
Kreditkartenlimite. In unseren Daten sind gesamthaft 8036
Kreditkartenbezüge verzeichnet. Auch hier werden wiederum die höheren
Bezüge ab einem Betrag von 4’000 CZK genauer untersucht.

``` r
df_cons %>% 
  filter(cardwidthdrawals_amount_mean  > 4000) %>% 
  # mutate(Region = as.factor(owner_district_region)) %>% 
  ggplot(., aes(x = cardwidthdrawals_amount_mean, fill = "blue")) +
    geom_histogram(binwidth = 25) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15),
      legend.position = "none"
    ) +
    ggtitle("Creditcard widthrawals") +
  labs(
    x = "Amount per purchase [CZK]",
    y = "count",
    subtitle = "Time span: 1993 - 1998, across all accounts"
    )
```

![](notebook_files/figure-gfm/unnamed-chunk-218-1.png)<!-- -->

Die Bezüge über 4000 CZK sind vereinzelt und knapp über 7000 CZK
verteilt.

Verteilung aller Kreditkartenbesitzern auf die Regionen.

``` r
# Visualisierung Verteilung auf die Regionen
df_cons %>% 
  mutate(Region = as.factor(owner_district_region))%>% 
  filter(!is.na(card_id)) %>% 
  ggplot(., aes(Region, fill = Region)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    ggtitle("Creditcard holder per regions") +
  labs(
    x = "",
    subtitle = "Time span: 1993 - 1998"
    )
```

![](notebook_files/figure-gfm/unnamed-chunk-219-1.png)<!-- -->

Die Kreditkartenbesitzer sind über das ganze Land verteilt. Die Regionen
Moravia und Prag fallen durch die meisten und south Bohemia durch die
wenigsten Besitzer auf.

Verteilung der Kreditkartenbesitzer auf die jeweiligen Regionen, mit
Fokus auf die Bezüge über 4k CZK

``` r
df_cons %>% 
  mutate(Region = as.factor(owner_district_region))%>% 
  filter(cardwidthdrawals_amount_mean  > 4000) %>% 
  ggplot(., aes(Region, fill = Region)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    ggtitle("card holders per region") +
  labs(
    x = "",
    subtitle = "Time span: 1993 - 1998, cardwidthdrawals with amounts over 4k CZK"
    )
```

![](notebook_files/figure-gfm/unnamed-chunk-220-1.png)<!-- -->

Hier fällt wiederum die Region Moravia mit den meisten hohen
Kreditkartenbezügen auf.

#### Top 3 der höchsten Kreditkartenbezüge[^42]

``` r
df_cons %>% 
  top_n(3, cardwidthdrawals_amount_mean) %>% 
  select(
    owner_dateofbirth,
    owner_sex, 
    owner_district_region,
    cardwidthdrawals_amount_mean,
    card_type,
    
  )
```

    ##   owner_dateofbirth owner_sex owner_district_region
    ## 1        1935-10-16      Male         south Moravia
    ## 2        1963-12-18      Male                Prague
    ## 3        1972-08-12    Female         south Moravia
    ##   cardwidthdrawals_amount_mean card_type
    ## 1                         5900      GOLD
    ## 2                         6160      GOLD
    ## 3                         6600      GOLD

Hier fällt auf, dass alle Top 3 die Kreditkarte Gold besitzen.

Verteilung der Kreditkartenbezüge über das Jahr gesehen.

``` r
df_transaction_complete %>%
  filter(operation == "CREDIT CARD WITHDRAWAL") %>%
  ggplot(mapping = aes(x = date, fill = "green")) +
  geom_histogram(binwidth = 10) + #binwidth = 5
  # facet_wrap(~ operation, ncol = 2, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribution of Cardwithdrawals from 1993 to 1999 ", 
       subtitle = "existing clientele",
       x = "Years",
       y = "Count"
       ) +
  scale_x_date(date_labels="%y",date_breaks ="1 year", date_minor_breaks = "1 month") +
  theme(axis.text.x = element_text(size = 8, angle=90,vjust =0.2))
```

![](notebook_files/figure-gfm/unnamed-chunk-222-1.png)<!-- -->

Deutlich zu sehen ist hier die Häufung der Verwendung der Kreditkarte
jeweils um den Jahreswechsel. Diese Erkenntnis kann für Angebote im
Zusammenhang mit Kreditkartenzahlungen verwendet werden.

Entfernen der nicht mehr benötigten Data Frames.

``` r
rm(cardwidthdrawals, cardwidthdrawals_4, cashwidthdrawals, cashwidthdrawals_10, date_complete, trans_correlation, owner_district)
```

### CrossSelling Produkt Kreditkarte[^43]

#### Ziel[^44]

Wenn ein Kunde bereits ein Girokonto hat aber noch nicht mit einer
Kreditkarte verbunden ist, ein Angebot für die Kreditkarte unterbreiten.
Bei Neueröffnung eines Girokontos die Kreditkarte gleich mitanbieten.

#### Voraussetzung an Konto/Kunde[^45]

Das Angebot einer Kreditkarte sollte für den Kunden sowie für die Bank
einen Mehrwert bedeuten. Darum ist es sehr wichtig, die Kriterien,
welche einen “guten” Kreditkartenkunde ausmacht, festzulegen. Die
bestehende Kundschaft wird dann auf diese Kriterien hin geprüft und im
Anschluss können gezielt Angebote an die Kundschaft getragen werden. Für
Neukunden werden diese Beurteilungskriterien soweit als möglich
angewendet.

Kriterien an den Kunden für das Angebot einer Kreditkarte:

Folgende Punkte müssen erfüllt sein:

- -Kontoart: Girokonto

- regelmässiges “normales” Einkommen

- positive Vermögensentwicklung

#### Vermögensentwicklung[^46]

Die Vermögensentwicklung darf nicht negativ sein, da ansonsten die
Kredite nicht mehr abbezahlt werden können. Die Spalte mit den
Informationen zur Vermögensentwicklung wurde bereits erstellt.

#### Kundengruppe Kreditkartenangebote[^47]

Wir rekapitulieren nochmals die Voraussetzungen für diese Kundengruppe:
Erfüllt müssen sein: - Kontoart: Girokonto - regelmässiges “normales”
Einkommen - Vermögensentwicklung nicht im Minus

Zusammenstellen der Kundengruppe für Kreditkartenangebote

``` r
# List of customers for credit card offers over 80% average salary
cust_cc_offer_lst_80 <- df_cons %>% 
 filter(
    is_checking_account == TRUE,
    is_regular_income_80 == TRUE,
    assets_dev > 0,
    # assets_dev_groups == "positive",
    has_creditcard == FALSE
  )

foo80 <- nrow(cust_cc_offer_lst_80)


paste(foo80, "Accounts mit einem regelmässigen Einkommen von 80% des ortsüblichen Einkommens erfüllen die Anforderungen für ein Kreditkartenangebot", sep = " ")
```

    ## [1] "2894 Accounts mit einem regelmässigen Einkommen von 80% des ortsüblichen Einkommens erfüllen die Anforderungen für ein Kreditkartenangebot"

``` r
rm(foo80)
```

#### Visualisierung der Kundengruppen “Kreditkartenangebot”[^48]

Für die Kundengruppe mit mind. 80% ortsübliches Durchschnittseinkommen.

``` r
# Altersverteilung
plt_age_cc_80 <- cust_cc_offer_lst_80 %>% 
  ggplot(aes(owner_current_age, fill = as.factor(owner_district_region))) +
  geom_boxplot(varwidth = TRUE) +
  scale_fill_viridis_d(alpha = 0.7, option = "A") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      plot.title = element_text(size = 15)
    ) +
    ggtitle("Clients age") +
  labs(
    x = "",
    fill = "Region")
```

``` r
# Verteilung auf die Regionen
plt_region_cc_80 <- cust_cc_offer_lst_80 %>% 
  ggplot(aes(x = owner_district_region, fill = as.factor(owner_district_region))) +
  geom_bar()+
  scale_fill_viridis_d(alpha = 0.9, option = "A") +
    theme_gray() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 15),
      axis.text.x = element_text(size = 5),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    ) +
    ggtitle("Clients per Region") +
  labs(
    x = "",
    y = "",
    fill = "Region"
  ) + 
  scale_x_discrete("Region", labels = c("central Bohemia" = "c. Boh.",
                                        "west Bohemia" = "w. Boh.", 
                                        "east Bohemia" = "e. Boh.",
                                        "north Bohemia" = "n. Boh.",
                                        "Prague" = "Prague",
                                        "south Bohemia" = "s. Boh.",
                                        "north Moravia" = "n. Mor.",
                                        "south Moravia" = "s. Mor."))+
  geom_text(stat = 'count', aes(label =..count..), vjust = -0.3, size = 3) +
  scale_y_continuous(limits = c(0, 600), breaks = 0:600)
```

``` r
# Einkommenshöhe
plt_income_cc_80 <- cust_cc_offer_lst_80 %>% 
 ggplot(aes(x = income_avg, y = owner_district_region, fill = as.factor(owner_district_region))) +
    geom_density_ridges(alpha=0.6,  stat="binline", bins=20) +
    theme_ridges() +
  scale_fill_viridis_d(alpha = 0.6, option = "A") +
    # theme_minimal() +
    theme(
      legend.position = "none",
      # axis.text.y = element_blank(),
      plot.title = element_text(size = 15),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 9),
      plot.subtitle = element_text(size = 9)
    ) +
  labs(
    title = "Clients average salary",
    x = "amount [CZK]",
    y = "",
    subtitle = "per year"
  )
```

``` r
# Jahr der Kontoeröffung
plt_year_account_cc_80 <- cust_cc_offer_lst_80 %>% 
  ggplot(aes(year(account_opening_date), fill = as.factor(year(account_opening_date)))) +
  geom_bar() +
  scale_fill_viridis_d(alpha = 0.9, option = "H") +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none",
      plot.title = element_text(size = 15),
      plot.subtitle = element_text(size = 10)
    ) +
    ggtitle("Account opening") +
  labs(
    x = "",
    y = "",
    # fill = "Region",
    subtitle = "Number of openings per year"
  ) +
  geom_text(stat = 'count', aes(label =..count..), vjust = -0.3, size = 3) +
  scale_y_continuous(limits = c(0, 950), breaks = 0:950)
```

#### Zusammenfassung der Gruppe “Kreditkartenangebote”[^49]

Erstellen eines verschachtelten Plots.

``` r
# Create multi-panel Plot with patchwork
((plt_year_account_cc_80|plt_region_cc_80)/(plt_income_cc_80|plt_age_cc_80)) +
  plot_annotation(title = "Existing clients for credit card offer [min. 80% salary]", 
                  theme = theme(plot.title = element_text(size = 15))) & theme(plot.title = element_text(hjust = 0.5),
                                                                               plot.subtitle = element_text(hjust = 0.5))
```

![](notebook_files/figure-gfm/unnamed-chunk-229-1.png)<!-- -->

Entfernen der nicht mehr benötigten Plots aus dem Global Environment

``` r
rm(plt_age_cc_80, plt_income_cc_80, plt_region_cc_80, plt_year_account_cc_80)
```

#### Art der Kontaktaufnahme mit dieser Kundengruppe[^50]

Wir leben in einem Zeitalter, in dem die Kunden gerne persönlich in der
Filiale beraten werden. Diese persönliche Beratung bietet sich vor allem
dann an, wenn der Kunde im selben Distrikt wohnt wie seine Filiale
angesiedelt ist.

Die benötigte Information ist bereits in einer Spalte
“consultation_on_site” enthalten.

``` r
# Spalten selektieren und Erstellen der Liste für Kunden 80%
cust_cc_offer_lst_80 <- df_cons %>% 
  filter(account_id %in% cust_cc_offer_lst_80$account_id) %>% 
  select(-contains("quarterly"),
         -starts_with("balance_"),
         -contains("yearly"))

# Zählen, wieviel vor Ort beraten werden
cust_cc_offer_lst_80 %>% 
  group_by(consultation_on_site) %>% 
  count()
```

    ## # A tibble: 1 × 2
    ## # Groups:   consultation_on_site [1]
    ##   consultation_on_site     n
    ##   <lgl>                <int>
    ## 1 TRUE                  2894

#### Spalte für Gruppe Kreditkartenangebote (is_card_offer)

``` r
# neue Spalte
df_cons <- df_cons %>% 
  mutate(is_card_offer = ifelse(account_id %in% cust_cc_offer_lst_80$account_id, TRUE, FALSE))

rm(cust_cc_offer_lst_80)
```

#### Resultat / Empfehlung[^51]

Für die aquise neuer Kreditkartenkunden unter den bestehenden Kunden
diese persönlich zu einem Beratungsgespräch einladen. (Plot mit
Verteilung Wohnohrt und Filiale) Weiter den bestehenden
Kreditkartenkunden bereits in der Vorweihnachtszeit spezielle Angebote
für den Einsatz der Kreditkarte machen. Dies könnte sein: Spezialrabatte
wenn mit der Kreditkarte bezahlt wird oder Zinsen für einen Monat
aussetzen wenn in diesem Monat ab einem bestimmten Betrag mit der
Kreditkarte bezahlt wird.

### Entscheidungsbaum Kreditkarten[^52]

Entscheidungsbaum für Cross Selling Angebote wie in diesem Fall zum
Thema Kreditkarte

``` r
hascard <- df_cons %>% 
  filter(has_creditcard == TRUE)

nocard <- df_cons %>% 
  filter(has_creditcard == FALSE) %>% 
  slice_sample(n = 892)

df_tree <- hascard %>% 
  bind_rows(nocard)

rm(hascard, nocard)
```

``` r
# relevante Spalten wählen
df_tree <- df_tree %>% 
  select(
    has_creditcard,
    assets_dev,
    owner_current_age,
    owner_sex, 
    income_avg
    ) %>%
  rename(
    card = has_creditcard,
    assets = assets_dev,
    age = owner_current_age,
    sex = owner_sex,
    income = income_avg,
    ) %>%
  mutate(across(where(is.logical), as.factor)) 

str(df_tree) 
```

    ## 'data.frame':    1784 obs. of  5 variables:
    ##  $ card  : Factor w/ 2 levels "FALSE","TRUE": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ assets: num  68496 39314 112406 50114 60766 ...
    ##  $ age   : num  63 56 30 63 19 38 18 31 42 29 ...
    ##  $ sex   : Factor w/ 2 levels "Female","Male": 2 2 2 2 1 2 2 1 1 1 ...
    ##  $ income: num  210089 137529 278699 366062 290245 ...

Trainingsdatensatz erstellen und partitionieren

``` r
# zufällig Spalten auswählen für den Traninigsdatensatz

set.seed(42) # Zufallszahlen festlegen
index_train <- sample(x = 1:4500,
               size = trunc(.75 * 4500))

tree_train <- df_tree %>%
filter(row_number() %in% index_train)

tree_test <- df_tree %>%
filter(!row_number() %in% index_train)

rm(index_train) # Das Objekt wieder löschen, da nicht mehr benötigt

head(tree_train)
```

    ##   card   assets age    sex income
    ## 1 TRUE  68495.5  63   Male 210089
    ## 2 TRUE  39313.9  56   Male 137529
    ## 3 TRUE 112405.5  30   Male 278699
    ## 4 TRUE  50114.2  63   Male 366062
    ## 5 TRUE  98222.5  18   Male 236761
    ## 6 TRUE  56145.9  31 Female 235599

``` r
dim(tree_test)
```

    ## [1] 428   5

``` r
# dim(mytree_test)
```

Datenbeschreibung:

- card = has_creditcard mit den Werten “Yes”, “No”

- assets = assets_dev mit numerischen Werten

- income = income_avg mit dem durchschnittlichen Einkommen

- age = owner_current_age mit dem Alter des Kunden am 1.1.1999

- sex = owner_sex Geschlecht des Kunden

Statistik über das Geschlecht erstellen

``` r
table(tree_train$sex)
```

    ## 
    ## Female   Male 
    ##    658    698

Proportionale Verteilung des Geschlechts

``` r
prop.table(table(tree_train$sex))
```

    ## 
    ##    Female      Male 
    ## 0.4852507 0.5147493

Einfache Analysen erstellen

``` r
summary(tree_train$age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   16.00   28.00   41.00   41.88   54.00   80.00

``` r
table(tree_train$card)
```

    ## 
    ## FALSE  TRUE 
    ##   663   693

``` r
summary(tree_train$assets)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -15432   28668   49460   51109   69879  138317

``` r
summary(tree_train$income)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    8980   82376  196262  215326  289044  772343

#### Modell trainieren[^53]

``` r
set.seed(123)
ct_tree <- rpart(card ~ . , data = tree_train, method = "class", control = rpart.control(cp = 0.003), parms = list(split = "gini"))
```

Modell analysieren

``` r
ct_tree
```

    ## n= 1356 
    ## 
    ## node), split, n, loss, yval, (yprob)
    ##       * denotes terminal node
    ## 
    ##     1) root 1356 663 TRUE (0.48893805 0.51106195)  
    ##       2) assets< 32453.4 404  65 FALSE (0.83910891 0.16089109)  
    ##         4) income< 289066.5 355  32 FALSE (0.90985915 0.09014085) *
    ##         5) income>=289066.5 49  16 TRUE (0.32653061 0.67346939)  
    ##          10) income< 390361 25  12 TRUE (0.48000000 0.52000000)  
    ##            20) assets< 25463.5 14   5 FALSE (0.64285714 0.35714286) *
    ##            21) assets>=25463.5 11   3 TRUE (0.27272727 0.72727273) *
    ##          11) income>=390361 24   4 TRUE (0.16666667 0.83333333) *
    ##       3) assets>=32453.4 952 324 TRUE (0.34033613 0.65966387)  
    ##         6) assets< 51378.6 312 154 FALSE (0.50641026 0.49358974)  
    ##          12) income< 200839.5 178  67 FALSE (0.62359551 0.37640449)  
    ##            24) age>=21.5 145  45 FALSE (0.68965517 0.31034483)  
    ##              48) assets< 42437.05 70  14 FALSE (0.80000000 0.20000000) *
    ##              49) assets>=42437.05 75  31 FALSE (0.58666667 0.41333333)  
    ##                98) income< 126951.5 44  13 FALSE (0.70454545 0.29545455) *
    ##                99) income>=126951.5 31  13 TRUE (0.41935484 0.58064516)  
    ##                 198) age< 47.5 18   8 FALSE (0.55555556 0.44444444) *
    ##                 199) age>=47.5 13   3 TRUE (0.23076923 0.76923077) *
    ##            25) age< 21.5 33  11 TRUE (0.33333333 0.66666667) *
    ##          13) income>=200839.5 134  47 TRUE (0.35074627 0.64925373)  
    ##            26) assets>=36329 109  43 TRUE (0.39449541 0.60550459)  
    ##              52) assets< 39256.55 12   4 FALSE (0.66666667 0.33333333) *
    ##              53) assets>=39256.55 97  35 TRUE (0.36082474 0.63917526)  
    ##               106) sex=Female 41  20 FALSE (0.51219512 0.48780488)  
    ##                 212) assets>=44319.65 26   9 FALSE (0.65384615 0.34615385) *
    ##                 213) assets< 44319.65 15   4 TRUE (0.26666667 0.73333333) *
    ##               107) sex=Male 56  14 TRUE (0.25000000 0.75000000) *
    ##            27) assets< 36329 25   4 TRUE (0.16000000 0.84000000) *
    ##         7) assets>=51378.6 640 166 TRUE (0.25937500 0.74062500)  
    ##          14) income< 419616 521 150 TRUE (0.28790787 0.71209213)  
    ##            28) income>=406296 9   2 FALSE (0.77777778 0.22222222) *
    ##            29) income< 406296 512 143 TRUE (0.27929688 0.72070312)  
    ##              58) assets< 83678.35 395 123 TRUE (0.31139241 0.68860759)  
    ##               116) assets>=81568.35 16   6 FALSE (0.62500000 0.37500000) *
    ##               117) assets< 81568.35 379 113 TRUE (0.29815303 0.70184697)  
    ##                 234) assets< 71387.8 274  91 TRUE (0.33211679 0.66788321)  
    ##                   468) income>=60141 260  90 TRUE (0.34615385 0.65384615)  
    ##                     936) income< 112884 36  16 FALSE (0.55555556 0.44444444)  
    ##                      1872) assets< 57469.25 13   2 FALSE (0.84615385 0.15384615) *
    ##                      1873) assets>=57469.25 23   9 TRUE (0.39130435 0.60869565) *
    ##                     937) income>=112884 224  70 TRUE (0.31250000 0.68750000)  
    ##                      1874) income>=147226.5 200  67 TRUE (0.33500000 0.66500000)  
    ##                        3748) assets>=68541.2 23  11 FALSE (0.52173913 0.47826087)  
    ##                          7496) age< 36.5 7   1 FALSE (0.85714286 0.14285714) *
    ##                          7497) age>=36.5 16   6 TRUE (0.37500000 0.62500000) *
    ##                        3749) assets< 68541.2 177  55 TRUE (0.31073446 0.68926554)  
    ##                          7498) income< 176129 21  10 FALSE (0.52380952 0.47619048)  
    ##                           14996) sex=Female 11   3 FALSE (0.72727273 0.27272727) *
    ##                           14997) sex=Male 10   3 TRUE (0.30000000 0.70000000) *
    ##                          7499) income>=176129 156  44 TRUE (0.28205128 0.71794872) *
    ##                      1875) income< 147226.5 24   3 TRUE (0.12500000 0.87500000) *
    ##                   469) income< 60141 14   1 TRUE (0.07142857 0.92857143) *
    ##                 235) assets>=71387.8 105  22 TRUE (0.20952381 0.79047619) *
    ##              59) assets>=83678.35 117  20 TRUE (0.17094017 0.82905983) *
    ##          15) income>=419616 119  16 TRUE (0.13445378 0.86554622) *

Mit summary analysieren

CP Statistik

Der Cp Wert ist ein Prozessfähigkeitsindex und berechnet das Verhältnis
zwischen der Streuung und der Toleranzbreite eines Prozesses. Der Cp
Wert dividiert den Bereich zwischen dem oberem und unterem Grenzwert des
Prozesses durch die Standardabweichung.

``` r
printcp(ct_tree)
```

    ## 
    ## Classification tree:
    ## rpart(formula = card ~ ., data = tree_train, method = "class", 
    ##     parms = list(split = "gini"), control = rpart.control(cp = 0.003))
    ## 
    ## Variables actually used in tree construction:
    ## [1] age    assets income sex   
    ## 
    ## Root node error: 663/1356 = 0.48894
    ## 
    ## n= 1356 
    ## 
    ##          CP nsplit rel error  xerror     xstd
    ## 1 0.4132730      0   1.00000 1.00000 0.027764
    ## 2 0.0331825      1   0.58673 0.60483 0.025347
    ## 3 0.0256410      3   0.52036 0.59427 0.025217
    ## 4 0.0165913      4   0.49472 0.57919 0.025024
    ## 5 0.0045249      5   0.47813 0.55505 0.024698
    ## 6 0.0037707      9   0.46003 0.56259 0.024802
    ## 7 0.0030166     13   0.44495 0.56259 0.024802
    ## 8 0.0030000     27   0.40121 0.58220 0.025063

#### Ausdrucken des Cp Wertes[^54]

``` r
# Relativer Fehler der Kreuzvalidierung 
# Obere x-Achse zeigt hier Anzahl Blätter (upper = "size"), alternativ wählbar sind die Anzahl Splits (upper = "splits")
plotcp(ct_tree, upper = "size")
```

![](notebook_files/figure-gfm/unnamed-chunk-247-1.png)<!-- -->

Fürs Pruning des Baumes wird versucht, den Wert so zu wählen, dass der
Baum nicht zu gross und nicht zu klein wird. Dazu kann man sich am
“Knick” des Ellbogens (Visualisierung oben) orientieren.

``` r
rpart.plot(ct_tree)
```

![](notebook_files/figure-gfm/unnamed-chunk-248-1.png)<!-- -->

Der gerade gezeigte Baum ist viel zu gross und wird so auch
unübersichtlich.

``` r
tree_train %>%
count(card) %>%
mutate(prop = n / sum(n))
```

    ##    card   n      prop
    ## 1 FALSE 663 0.4889381
    ## 2  TRUE 693 0.5110619

Erstellen eines “geschnittenen” Baumes

``` r
set.seed(123)
ct_tree_pruned <- rpart(card ~ . , data = tree_train, method = "class", control = rpart.control(cp = 0.0036), parms = list(split = "gini"))
```

Visualisieren

``` r
plotcp(ct_tree_pruned, upper = "size")
```

![](notebook_files/figure-gfm/unnamed-chunk-251-1.png)<!-- -->

Nun ist der Baum viel übersichtlicher und brauchbar.

``` r
rpart.plot(ct_tree_pruned)
```

![](notebook_files/figure-gfm/unnamed-chunk-252-1.png)<!-- -->

Vorhersage des Modells

``` r
ct_tree_predict_train <- predict(ct_tree_pruned, tree_train, type = "class")

confusion_matrix_train <- table(tree_train$card, ct_tree_predict_train)

accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)

confusion_matrix_train
```

    ##        ct_tree_predict_train
    ##         FALSE TRUE
    ##   FALSE   442  221
    ##   TRUE     74  619

``` r
accuracy_train
```

    ## [1] 0.7824484

Testen des Modells mit Testdaten

``` r
ct_tree_predict_test <- predict(ct_tree_pruned, tree_test, type = "class")

confusion_matrix_test <- table(tree_test$card, ct_tree_predict_test)

accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)

accuracy_test
```

    ## [1] 0.7546729

Mit den Testdaten sinkt die Accuracy von 0.78 auf 0.75.

``` r
confusion_matrix_test
```

    ##        ct_tree_predict_test
    ##         FALSE TRUE
    ##   FALSE   144   85
    ##   TRUE     20  179

``` r
rm(tree_train, tree_test)
```

### Cross-Selling Produkt Umkehrhypothek (HELOC)[^55]

Eine weitere Möglichkeit für Cross Selling ist die Umkehrhypothek
(HELOC, home equity line of credit). Sie bietet Personen im Rentenalter
mehr finanzielle Flexibilität.

Durch eine erneute Erhöhung oder Neuaufnahme auf das eigene Wohnobjekt
kann der Kunde im Rentenalter Geld flüssig machen um z.B.
Instandhaltungsarbeiten am Wohnobjekt zu finanzieren.

Bedingung ist, dass der Kunde die Liegenschaft selbst bewohnt und sie
bereits abbezahlt ist oder nur noch mit einer geringen Hypothek behaftet
ist.

Die Umkehrhypothek muss erst zurückbezahlt werden, wenn der Kunde ablebt
oder die Liegenschaft verkauft wird.

Leider geben die vorliegenden Daten zu wenig genaue Informationen zu
diesen Voraussetzungen. Trotzdem habe ich versucht, anhand der zugrunde
liegenden Daten gezielt nach Hinweisen zu suchen.

Zum Beispiel habe ich die Informationen der Transaktionen für Household
oder auch die Observationen der Orders mit der Bezeichnung Household
verwendet, um Erkenntnisse über mögliche Hausbesitzer zu erhalten.

#### Annahme[^56]

Die Information, wer diese mögliche Liegenschaft effektiv nutzt oder ob
es sich überhaupt um eine Liegenschaft handelt, enzieht sich unserer
Kenntnis. Die folgenden Analysen wurden in der Annahme gemacht, dass es
sich A um Liegenschaften handelt und B diese auch durch die Kunden
selbst genutzt werden.

#### Voraussetzung[^57]

- Kunde muss im Pensionsalter sein
- Kunde hat bereits Wohneigentum und nutzt dieses auch selbst
- das Wohneigentum ist abbezahlt oder die Hypthek nur noch sehr gering
- das Konto darf keine negative Vermögensentwicklung aufweisen
- der Kunde hat keine weiteren Darlehensverpflichtungen, welche nicht
  beglichen werden könnten

#### Rentenalter[^58]

Das Rentenalter tritt ab dem 65 Lebensjahr ein. Dies für beide
Geschlechter gleich.

Erstellen einer neuen Spalte “is_retirement_age” mit dem logischen Wert
TRUE wenn der Kunde im Rentenalter ist.

``` r
df_cons <- df_cons %>% 
  mutate(is_retirement_age = ifelse(owner_current_age >= 65, TRUE, FALSE))

foo <- df_cons %>% 
  filter(is_retirement_age == TRUE) %>% 
  nrow()

paste("Unter der Kundschaft befinden sich", foo, "Kunden mit Zeichnungsrecht im Rentenalter.")
```

    ## [1] "Unter der Kundschaft befinden sich 639 Kunden mit Zeichnungsrecht im Rentenalter."

``` r
rm(foo)
```

#### Wohneigentum[^59]

Ob ein Kunde Wohneigentum hat, ist aus den vorliegenden Daten nicht
abschliessend festzustellen. Wir können aber annehmen, dass Kunden mit
dem Zahlungshinweis Household oder mit einem Darlehen für Household
Wohneigentum besitzen.

Erstellen einer neuen Spalte “has_res_prop” mit dem logischen Wert TRUE
wenn der Kunde Transaktionen für Household hat.

``` r
hh_payment <- df_transaction_complete %>% 
  filter(characterization == "HOUSEHOLD",
         cashflow == "OUT") %>% 
  group_by(account_id) %>% 
  slice_head(n = 1)

df_cons_hh <- df_cons %>% 
  filter(!is.na(order_num_household)) %>% 
  mutate(has_res_prop = ifelse(account_id %in% hh_payment$account_id | order_num_household > 0, TRUE, FALSE)) %>% 
  na_replace(has_res_prop, FALSE) %>% 
  select(account_id, has_res_prop)

df_cons <- df_cons %>% 
  left_join(df_cons_hh) 

df_cons <- df_cons %>% 
  mutate(isNA = is.na(has_res_prop))

df_cons <- df_cons %>% 
  mutate(has_res_prop = ifelse(isNA == FALSE, has_res_prop, FALSE))
  
  
df_cons %>% 
  filter(is.na(has_res_prop)) %>% 
  nrow()
```

    ## [1] 0

``` r
df_cons %>% 
  filter(has_res_prop == TRUE) %>% 
  nrow()
```

    ## [1] 3393

``` r
df_cons_hh %>% 
  filter(has_res_prop == TRUE) %>% 
  nrow()
```

    ## [1] 3393

``` r
df_cons <- df_cons %>% 
  select(-isNA)

rm(hh_payment, df_cons_hh)
```

#### Liste Kunden für Angebot der Umkehrhypothek[^60]

Home Equity Line of Credit

Filtern nach den Voraussetzungen

- Kunde muss im Pensionsalter sein
- Kunde hat bereits Wohneigentum und nutzt dieses auch selbst
- das Wohneigentum ist abbezahlt oder die Hypthek nur noch sehr gering
- das Konto darf keine negative Vermögensentwicklung aufweisen
- der Kunde hat keine weiteren Darlehensverpflichtungen, welche nicht
  beglichen werden könnten

``` r
heloc_cust_list <- df_cons %>% 
  filter(
    is_retirement_age == TRUE, 
    has_res_prop == TRUE,
    loan_in_dept == FALSE | is.na(loan_in_dept),
    assets_dev_groups == "positive"
    )

foo <- heloc_cust_list %>% 
  nrow()

heloc_cust_list %>% 
  group_by(consultation_on_site) %>% 
  count()
```

    ## # A tibble: 1 × 2
    ## # Groups:   consultation_on_site [1]
    ##   consultation_on_site     n
    ##   <lgl>                <int>
    ## 1 TRUE                   635

#### Spalte Angebot Umkehrhypothek (heloc_offer)[^61]

``` r
# Spalte für Angebot Umkehrhypothek
df_cons <- df_cons %>% 
  mutate(heloc_offer = ifelse(account_id %in% heloc_cust_list$account_id, TRUE, FALSE))
```

``` r
paste("Für das Angebot der Umkehrhypothek kommen", foo, "Kunden in Frage. Alle Kunden können persönlich vor Ort beraten werden.")
```

    ## [1] "Für das Angebot der Umkehrhypothek kommen 635 Kunden in Frage. Alle Kunden können persönlich vor Ort beraten werden."

``` r
rm(heloc_cust_list)
rm(foo)
```

#### Weitere Cross-Selling Produkte[^62]

Weitere Cross-Selling Produkte könnten sein: Darlehens- und
Leasingsangebote, Versicherungsangebote, Anlagepakete (Aktien,
Obligationen), Devisenhandel

#### Client Analytical Record[^63]

Das Data Frame df_cons wurde während der verschiedenen Analysen immer
wieder mit relevanten Daten angereichert, somit sollte es als Client
Analytical Record dienen.

## Weitereführende Analysen

### Analyse von Kundenattributen

Wann hat die Bank neue Kunden gewonnen und wie sieht diese Verteilung
über die Zeit aus?[^64]

##### Gesamtübersicht dern neu eröffneten Konten in den letzten Jahren

``` r
ggplot(data = df_cons, aes(x = account_opening_date)) +
    geom_histogram(binwidth = 20, stat = "bin", fill = "#CD950C") +
    labs(title = "Opening of new bank Accounts", 
           subtitle = "From the Year 1993 to 1998 ", 
           caption = "Source: PKDD’99 Discovery Challenge",
           x = "Year", y = "Number of openings") +
    theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 60, vjust = 0.5)) +
    scale_x_date(breaks = scales::breaks_pretty(10))
```

![](notebook_files/figure-gfm/unnamed-chunk-263-1.png)<!-- -->

Das Histogramm zeigt die Jährliche Übersicht der neu gewonnenen Kunden
in den Jahren 1993 bis 1998

#### Detaillierten Jahresansicht

``` r
# account_opening_date nach Jahr sortieren
df_cons %>%
      ggplot(df_cons, mapping = aes(as.Date(account_opening_date), fill = as.factor(account_opening_year))) +
      geom_histogram() +
      facet_wrap(~ account_opening_year, ncol = 3, scales = "free_x") +
      coord_cartesian(ylim = c(0, 80)) +
      theme(legend.position = "none") +
      labs(title = "Opening of new bank Accounts", 
           subtitle = "Subdivided by Year", 
           caption = "Source: PKDD’99 Discovery Challenge",
           x = "", y = "Number of openings") +
      theme(axis.text.x = element_text(size = 6, angle = 60, vjust = 0.5)) +
      scale_x_date(breaks = scales::breaks_pretty(10))
```

![](notebook_files/figure-gfm/unnamed-chunk-264-1.png)<!-- -->

Detaillierte Überischt der Neukunden unterteilt nach Jahren.

#### Altersverteilung der Kunden[^65]

``` r
# Calculate age of Customers with Lubridate
# to keep a reasonable age of the cusomers, we calculated their age as of the following date: 31.12.1999
df_cons$age_dec1999 <- trunc((df_cons$owner_dateofbirth %--% "1999-12-31") / years(1))

#Relocate rows so new column "current_age" is next to dateofbirth
df_cons <- df_cons %>%
   relocate(age_dec1999, .after = owner_dateofbirth)
 
 
#Create Barplot with distrbution of age 
df_cons %>%
   group_by(account_id) %>%
   filter(row_number() == 1)
```

    ## # A tibble: 4,500 × 202
    ## # Groups:   account_id [4,500]
    ##    account_id account_frequency account_opening_date account_opening_year
    ##         <int> <fct>             <date>                              <dbl>
    ##  1          1 Monthly           1995-03-24                           1995
    ##  2          2 Monthly           1993-02-26                           1993
    ##  3          3 Monthly           1997-07-07                           1997
    ##  4          4 Monthly           1996-02-21                           1996
    ##  5          5 Monthly           1997-05-30                           1997
    ##  6          6 Monthly           1994-09-27                           1994
    ##  7          7 Monthly           1996-11-24                           1996
    ##  8          8 Monthly           1995-09-21                           1995
    ##  9          9 Monthly           1993-01-27                           1993
    ## 10         10 Monthly           1996-08-28                           1996
    ## # ℹ 4,490 more rows
    ## # ℹ 198 more variables: account_opening_month <dbl>, account_num_of_user <int>,
    ## #   account_district_name <fct>, account_district_region <fct>,
    ## #   account_district_average_salary <int>, account_district_inhabitants <int>,
    ## #   order_total_amount_household <dbl>, order_total_amount_loan <dbl>,
    ## #   order_total_amount_insurrance <dbl>, order_total_amount_unknown <dbl>,
    ## #   order_total_amount_leasing <dbl>, order_num_household <dbl>, …

``` r
   ggplot(data = df_cons, aes(age_dec1999)) +
   geom_bar(fill = "#88B8AC") +
   labs(title = "Distribution of Customer Age as of 31. Dec 1999", 
        subtitle = "Male and Female combined",
        caption = "Source: PKDD’99 Discovery Challenge",
        x ="Age in Years",
        y = "Number of Customers") +
   scale_x_continuous(breaks = scales::breaks_pretty(10))
```

![](notebook_files/figure-gfm/unnamed-chunk-265-1.png)<!-- -->

Der Barplot zeigt die die Verteilung der Kundenalters. Da eine
Darstellung mit dem Kundenalter im Jahr 2023 nur bedingt aussagekräftig
wäre, wurde als Stichtag der 31.12.1999 definiert.

#### Altersverteilung beim eröffnen des Kontos

``` r
   ggplot(data = df_cons, aes(owner_age_at_account_opening)) +
   geom_bar(fill = "#88B8AC") +
   labs(title = "Age of Customer at opening of Account ", 
        subtitle = "Male and Female combined",
        caption = "Source: PKDD’99 Discovery Challenge",
        x ="Age in Years",
        y = "Number of Customers") +
   scale_x_continuous(breaks = scales::breaks_pretty(10))
```

![](notebook_files/figure-gfm/unnamed-chunk-266-1.png)<!-- -->

Der Barplot zeigt die die Verteilung der Kundenalters beim eröffnen des
Kontos.

#### Verteilung der Kreditkartentypen [^66]

``` r
# Preparation for the visualization of the distribution of credit cards by the credit card type and the sex of the customer.
df_credit_card_by_type <- df_cons %>%
  filter(!is.na(card_id)) %>%
  select(card_type, owner_sex) %>%
  # group_by to get the count of each credit card by type and sex of the customer.
  group_by(owner_sex, card_type) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  #Prepare the frequency as a character to display the exact information in the graph.
  mutate(prop = paste0((round(freq, 3))," %")) %>%
  ungroup()

df_credit_card_by_type
```

    ## # A tibble: 6 × 5
    ##   owner_sex card_type     n   freq prop   
    ##   <fct>     <fct>     <int>  <dbl> <chr>  
    ## 1 Female    CLASSIC     317 0.749  0.749 %
    ## 2 Female    GOLD         35 0.0827 0.083 %
    ## 3 Female    JUNIOR       71 0.168  0.168 %
    ## 4 Male      CLASSIC     342 0.729  0.729 %
    ## 5 Male      GOLD         53 0.113  0.113 %
    ## 6 Male      JUNIOR       74 0.158  0.158 %

``` r
#Display of the distribution of the credit cards by type and sex.
ggplot(df_credit_card_by_type, aes(fill = card_type, x = owner_sex, y = n)) +
  labs(title = "Distribution of Credit Card Type per Sex", subtitle = "The percentage displayed on the plot is the frequency of the type over the whole population.") +
  geom_col(position = "stack") + 
  geom_text(aes(label = prop), position = position_stack(vjust = 0.5)) +
  xlab("Sex") +
  ylab("Count") +
  labs(fill = "Card Type")
```

![](notebook_files/figure-gfm/unnamed-chunk-267-1.png)<!-- -->

``` r
#Cleanup data frame.
rm(df_credit_card_by_type)
```

In dieser Grafik sehen wir die relative Verteilung von Kreditkarten
typen gruppiert nach den Geschlechtern. Dabei sehen wir das die grösste
Menge der Kunden eine Kreditkarte vom Type ‘Classic’ haben. Wenn man
genauer die Unterschiede zwischen den beiden Geschlechtern anschaut,
sieht man, dass ein grössere Anteil der Männlichen Kundschaft eine
Kreditkarte mit dem Typ ‘Gold’ besitzen. Jedoch gleicht sich das ganze
aus, da die Weibliche Kundschaft einen grösseren relativen Anteil an
Kreditkarten vom Type ‘Junior’ und ‘Classic’ haben. Wenn man die beiden
Balken vergleicht wird festgestellt, dass mehr männliche Kunden einen
Kreditkarte besitzen.

#### Clustering des Einkommens und der Ausgaben[^67]

``` r
df_cons_metrics <- df_cons %>%
  dplyr::select(starts_with(c("income", "outgoes")))

str(df_cons_metrics)
```

    ## 'data.frame':    4500 obs. of  62 variables:
    ##  $ incomes_quarterly_1993.1: num  0 25050 0 0 0 ...
    ##  $ incomes_quarterly_1993.2: num  0 71240 0 0 0 ...
    ##  $ incomes_quarterly_1993.3: num  0 61191 0 0 0 ...
    ##  $ incomes_quarterly_1993.4: num  0 71204 0 0 0 ...
    ##  $ incomes_quarterly_1994.1: num  0 61061 0 0 0 ...
    ##  $ incomes_quarterly_1994.2: num  0 71238 0 0 0 ...
    ##  $ incomes_quarterly_1994.3: num  0 61111 0 0 0 ...
    ##  $ incomes_quarterly_1994.4: num  0 71243 0 0 0 ...
    ##  $ incomes_quarterly_1995.1: num  1000 61771 0 0 0 ...
    ##  $ incomes_quarterly_1995.2: num  25936 71238 0 0 0 ...
    ##  $ incomes_quarterly_1995.3: num  11337 61109 0 0 0 ...
    ##  $ incomes_quarterly_1995.4: num  11292 71244 0 0 0 ...
    ##  $ incomes_quarterly_1996.1: num  11526 67130 0 6353 0 ...
    ##  $ incomes_quarterly_1996.2: num  12633 85484 0 16659 0 ...
    ##  $ incomes_quarterly_1996.3: num  14664 61236 0 16930 0 ...
    ##  $ incomes_quarterly_1996.4: num  11242 74693 0 16979 0 ...
    ##  $ incomes_quarterly_1997.1: num  14103 67288 0 16899 0 ...
    ##  $ incomes_quarterly_1997.2: num  11196 71410 0 16910 5617 ...
    ##  $ incomes_quarterly_1997.3: num  12733 71455 31522 16910 15051 ...
    ##  $ incomes_quarterly_1997.4: num  11243 71425 26617 16926 15283 ...
    ##  $ incomes_quarterly_1998.1: num  11712 62443 28529 16855 15357 ...
    ##  $ incomes_quarterly_1998.2: num  11235 73126 27567 16913 15393 ...
    ##  $ incomes_quarterly_1998.3: num  11248 61314 26313 16968 15382 ...
    ##  $ incomes_quarterly_1998.4: num  11220 71350 32510 17044 15402 ...
    ##  $ incomes_yearly_1993     : num  0 228685 0 0 0 ...
    ##  $ incomes_yearly_1994     : num  0 264653 0 0 0 ...
    ##  $ incomes_yearly_1995     : num  49564 265362 0 0 0 ...
    ##  $ incomes_yearly_1996     : num  50066 288543 0 56920 0 ...
    ##  $ incomes_yearly_1997     : num  49275 281578 58140 67645 35951 ...
    ##  $ incomes_yearly_1998     : num  45415 268232 114919 67781 61534 ...
    ##  $ income_avg              : num  48580 266176 86530 64115 48742 ...
    ##  $ outgoes_quarterly_1993.1: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ outgoes_quarterly_1993.2: num  0 51000 0 0 0 ...
    ##  $ outgoes_quarterly_1993.3: num  0 84707 0 0 0 ...
    ##  $ outgoes_quarterly_1993.4: num  0 65138 0 0 0 ...
    ##  $ outgoes_quarterly_1994.1: num  0 57860 0 0 0 ...
    ##  $ outgoes_quarterly_1994.2: num  0 72960 0 0 0 ...
    ##  $ outgoes_quarterly_1994.3: num  0 64701 0 0 0 ...
    ##  $ outgoes_quarterly_1994.4: num  0 64060 0 0 0 ...
    ##  $ outgoes_quarterly_1995.1: num  0 61060 0 0 0 ...
    ##  $ outgoes_quarterly_1995.2: num  200 68258 0 0 0 ...
    ##  $ outgoes_quarterly_1995.3: num  15981 68340 0 0 0 ...
    ##  $ outgoes_quarterly_1995.4: num  15210 61196 0 0 0 ...
    ##  $ outgoes_quarterly_1996.1: num  13270 67842 0 0 0 ...
    ##  $ outgoes_quarterly_1996.2: num  12320 72164 0 0 0 ...
    ##  $ outgoes_quarterly_1996.3: num  12640 80627 0 17355 0 ...
    ##  $ outgoes_quarterly_1996.4: num  17350 53942 0 14623 0 ...
    ##  $ outgoes_quarterly_1997.1: num  15510 75466 0 23123 0 ...
    ##  $ outgoes_quarterly_1997.2: num  8570 89178 0 17703 0 ...
    ##  $ outgoes_quarterly_1997.3: num  9860 46808 2400 15033 0 ...
    ##  $ outgoes_quarterly_1997.4: num  13200 69642 28990 16853 8390 ...
    ##  $ outgoes_quarterly_1998.1: num  11100 87261 31967 19033 16768 ...
    ##  $ outgoes_quarterly_1998.2: num  9080 53642 23367 12213 13448 ...
    ##  $ outgoes_quarterly_1998.3: num  14170 65840 15247 11973 18588 ...
    ##  $ outgoes_quarterly_1998.4: num  12395 72737 19992 10718 12203 ...
    ##  $ outgoes_yearly_1993     : num  0 200845 0 0 0 ...
    ##  $ outgoes_yearly_1994     : num  0 259581 0 0 0 ...
    ##  $ outgoes_yearly_1995     : num  31391 258854 0 0 0 ...
    ##  $ outgoes_yearly_1996     : num  55579 274574 0 31978 0 ...
    ##  $ outgoes_yearly_1997     : num  47139 281093 31390 72711 8390 ...
    ##  $ outgoes_yearly_1998     : num  46745 279480 90573 53937 61007 ...
    ##  $ outgoes_avg             : num  45213 259071 60981 52875 34698 ...

``` r
# Scale the data down
df_cons_metrics_scaled <- scale(df_cons_metrics)

# K-Means Clustering (Example with K = 20)
fitK <- kmeans(df_cons_metrics_scaled, 20)

str(fitK)
```

    ## List of 9
    ##  $ cluster     : int [1:4500] 5 19 5 5 5 12 3 2 13 20 ...
    ##  $ centers     : num [1:20, 1:62] -0.167 -0.167 -0.167 -0.167 -0.16 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : chr [1:20] "1" "2" "3" "4" ...
    ##   .. ..$ : chr [1:62] "incomes_quarterly_1993.1" "incomes_quarterly_1993.2" "incomes_quarterly_1993.3" "incomes_quarterly_1993.4" ...
    ##  $ totss       : num 278938
    ##  $ withinss    : num [1:20] 1728 2193 1721 3236 1027 ...
    ##  $ tot.withinss: num 75054
    ##  $ betweenss   : num 203884
    ##  $ size        : int [1:20] 211 175 169 70 1088 49 89 105 108 341 ...
    ##  $ iter        : int 9
    ##  $ ifault      : int 0
    ##  - attr(*, "class")= chr "kmeans"

``` r
cluster_metrics <- plot(df_cons_metrics_scaled, col = fitK$cluster)
```

![](notebook_files/figure-gfm/unnamed-chunk-268-1.png)<!-- -->

``` r
# let's find out the best value for k in the kmean function
k <- list()
for(i in 1:30){
  k[[i]] <- kmeans(df_cons_metrics_scaled, i)
}

# Calculate the squares in the clusters which will later be used to define the best value for k.
betweenss_totss <- list()
for(i in 1:30){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}

# Display the calculated values that can be used to define a optimial value for k.
cluster_overview <- plot(1:30, betweenss_totss, type = "b",
     ylab = "Between SS / Total SS", xlab = "Clusters (k)")
```

![](notebook_files/figure-gfm/unnamed-chunk-268-2.png)<!-- -->

Nach dem Graph sollten wir nun entweder nach der Elbow- oder
Silhouette-Methode definieren mit wie vielen Cluster, dass wir arbeiten
werden. Nach der Elbow-Methode habe wir Potenzial für k = 8, k = 15 und
k = 25. Wir bauen die Cluster Analyse nun mit 15 Clusters für eine
mittelmässig granulare Lösung.

Source
(<https://medium.com/analytics-vidhya/how-to-determine-the-optimal-k-for-k-means-708505d204eb>)

``` r
#Distribution of the data points for the three custers with the different k-values. The data points are coloured by the cluster number.
for (i in c(8, 15, 25)){
  distributed_clusters_with_coloring <- plot(df_cons_metrics_scaled, col = k[[i]]$cluster)
  
  distributed_clusters_with_coloring
}
```

![](notebook_files/figure-gfm/unnamed-chunk-269-1.png)<!-- -->![](notebook_files/figure-gfm/unnamed-chunk-269-2.png)<!-- -->![](notebook_files/figure-gfm/unnamed-chunk-269-3.png)<!-- -->

``` r
# Display of the cluster (k=8). with autoplot
fitK <- kmeans(df_cons_metrics_scaled, 8)
autoplot(fitK, df_cons_metrics_scaled, frame=TRUE)
```

![](notebook_files/figure-gfm/unnamed-chunk-269-4.png)<!-- -->

``` r
#Display the centers of the clusters.
fitK$centers
```

    ##   incomes_quarterly_1993.1 incomes_quarterly_1993.2 incomes_quarterly_1993.3
    ## 1              -0.15866414               -0.2194288               -0.1374050
    ## 2              -0.10992774               -0.1813243               -0.2183607
    ## 3               0.66387250                1.1128548                1.2977572
    ## 4              -0.13445046               -0.1582004               -0.1490267
    ## 5              -0.16713869               -0.2505693               -0.2813814
    ## 6              -0.08053561                0.4084174                1.0433634
    ## 7              -0.16713869               -0.2507281               -0.3065146
    ## 8               4.46181416                5.4823348                5.3661589
    ##   incomes_quarterly_1993.4 incomes_quarterly_1994.1 incomes_quarterly_1994.2
    ## 1              -0.12191764              -0.25424105              -0.03999945
    ## 2              -0.27469621              -0.28483045              -0.31954646
    ## 3               1.77373432               1.60217540               1.60242845
    ## 4              -0.06868529               0.02078412               0.16262931
    ## 5              -0.36561084              -0.39281328              -0.41731020
    ## 6               1.86618429               3.55176956               3.53206872
    ## 7              -0.38104502              -0.41343042              -0.43622546
    ## 8               4.28966189               2.87173464               2.95001528
    ##   incomes_quarterly_1994.3 incomes_quarterly_1994.4 incomes_quarterly_1995.1
    ## 1                0.2186836                0.6354567                1.8510205
    ## 2               -0.3157254               -0.3480667               -0.3535538
    ## 3                1.3618859                1.4019348                1.1447890
    ## 4                0.2259255                0.4034090                0.4582416
    ## 5               -0.3939105               -0.4336068               -0.4767569
    ## 6                3.7346838                3.3883252                2.9418321
    ## 7               -0.4399526               -0.4758052               -0.4991034
    ## 8                2.6328582                2.5610840                2.3926516
    ##   incomes_quarterly_1995.2 incomes_quarterly_1995.3 incomes_quarterly_1995.4
    ## 1                2.2118211                2.4691965                2.6121107
    ## 2               -0.3883429               -0.3883438               -0.4224960
    ## 3                1.1473849                1.0430383                0.9962080
    ## 4                0.6408995                0.7098381                0.7913419
    ## 5               -0.4936783               -0.4701063               -0.3633467
    ## 6                2.7645687                2.6259909                2.6080558
    ## 7               -0.5314590               -0.5449421               -0.5220342
    ## 8                2.2325942                2.0267345                1.7414309
    ##   incomes_quarterly_1996.1 incomes_quarterly_1996.2 incomes_quarterly_1996.3
    ## 1               2.44332481                2.0952397                2.1254148
    ## 2              -0.42658423               -0.5007563               -0.5151304
    ## 3               0.86812232                0.8541011                0.6403778
    ## 4               0.62365173                0.6045967                0.4212722
    ## 5              -0.02428955                0.4608904                0.8591596
    ## 6               2.31452674                1.9253444                1.6730016
    ## 7              -0.45167123               -0.3022147               -0.1762714
    ## 8               2.11694805                1.9706159                1.7842238
    ##   incomes_quarterly_1996.4 incomes_quarterly_1997.1 incomes_quarterly_1997.2
    ## 1               1.66934844               1.71043296                1.6216455
    ## 2              -0.57388804              -0.58940412               -0.6537687
    ## 3               0.60136218               0.52926537                0.5181838
    ## 4               0.38433300               0.21825072                0.2358173
    ## 5               1.26955648               1.76337710                1.7575985
    ## 6               1.44833711               1.55326989                1.4053019
    ## 7              -0.03550118              -0.05186981                0.1381919
    ## 8               1.77398228               1.62654302                1.5996962
    ##   incomes_quarterly_1997.3 incomes_quarterly_1997.4 incomes_quarterly_1998.1
    ## 1                1.7512014               1.52543148               1.54629124
    ## 2               -0.6203260              -0.67539494              -0.63880802
    ## 3                0.3838481               0.38454945               0.31533562
    ## 4                0.1035376               0.09763329               0.04433021
    ## 5                1.7703726               1.67927198               1.62195924
    ## 6                1.4989041               1.54610049               1.23665052
    ## 7                0.1754924               0.34633550               0.36843278
    ## 8                1.2102216               1.55869110               1.54495559
    ##   incomes_quarterly_1998.2 incomes_quarterly_1998.3 incomes_quarterly_1998.4
    ## 1               1.39834049               1.41014121               1.23224624
    ## 2              -0.66960237              -0.63467115              -0.67744718
    ## 3               0.37756591               0.31353987               0.39847243
    ## 4               0.06443199               0.01051838               0.09801391
    ## 5               1.63031399               1.73801392               1.62391504
    ## 6               1.42082181               1.26695841               1.27878105
    ## 7               0.41790071               0.37156255               0.45301983
    ## 8               1.40448271               1.34702076               1.44484202
    ##   incomes_yearly_1993 incomes_yearly_1994 incomes_yearly_1995
    ## 1          -0.1770815           0.1744169           2.4983137
    ## 2          -0.2510853          -0.3446482          -0.4234970
    ## 3           1.5611438           1.6085548           1.1764797
    ## 4          -0.1393863           0.2300908           0.7126402
    ## 5          -0.3360914          -0.4438788          -0.4891233
    ## 6           1.2266266           3.8369919           2.9704066
    ## 7          -0.3510956          -0.4789369          -0.5708578
    ## 8           5.6586386           2.9712114           2.2734868
    ##   incomes_yearly_1996 incomes_yearly_1997 incomes_yearly_1998 income_avg
    ## 1           2.3440538           1.8487700          1.53457795  1.8819824
    ## 2          -0.5742449          -0.7123242         -0.72106038 -0.7943967
    ## 3           0.8336774           0.5072828          0.38712739  0.5236657
    ## 4           0.5708365           0.1825616          0.06019249  0.1587151
    ## 5           0.7541051           1.9516267          1.81881476  1.8409675
    ## 6           2.0672582           1.6807650          1.43225739  1.9158739
    ## 7          -0.2646812           0.1750929          0.44369483  0.3595797
    ## 8           2.1588468           1.6788338          1.57809984  2.0336951
    ##   outgoes_quarterly_1993.1 outgoes_quarterly_1993.2 outgoes_quarterly_1993.3
    ## 1              -0.07474642              -0.17764362               -0.1584193
    ## 2              -0.07184140              -0.15581559               -0.2081606
    ## 3               0.15614592               0.71078292                1.1180650
    ## 4              -0.06907451              -0.15257130               -0.1687376
    ## 5              -0.07474642              -0.17764362               -0.2363987
    ## 6              -0.06995467               0.06314481                0.6186909
    ## 7              -0.07474642              -0.17764362               -0.2468507
    ## 8               3.46284062               5.76439917                5.8000407
    ##   outgoes_quarterly_1993.4 outgoes_quarterly_1994.1 outgoes_quarterly_1994.2
    ## 1               -0.1216106               -0.2101327              -0.19868582
    ## 2               -0.2613888               -0.2930475              -0.30352983
    ## 3                1.5558330                1.7555760               1.65518525
    ## 4               -0.1459556               -0.0463595               0.05230633
    ## 5               -0.3020436               -0.3883708              -0.39924314
    ## 6                1.6538524                3.1843835               3.54130123
    ## 7               -0.3229204               -0.3999652              -0.41771411
    ## 8                4.7732441                3.1983157               2.97116613
    ##   outgoes_quarterly_1994.3 outgoes_quarterly_1994.4 outgoes_quarterly_1995.1
    ## 1               0.06876093                0.5527106                1.5004433
    ## 2              -0.32195871               -0.3421100               -0.3599418
    ## 3               1.41485439                1.3643169                1.3418219
    ## 4               0.18288409                0.3108643                0.4418531
    ## 5              -0.38616555               -0.4124674               -0.4770700
    ## 6               3.84459154                3.5161196                3.0961744
    ## 7              -0.41535100               -0.4535168               -0.5107853
    ## 8               2.65616638                2.7911950                2.3653287
    ##   outgoes_quarterly_1995.2 outgoes_quarterly_1995.3 outgoes_quarterly_1995.4
    ## 1                2.1057313                2.4362564                2.7044405
    ## 2               -0.3755880               -0.3901219               -0.4129160
    ## 3                1.1719330                1.0508196                0.9644167
    ## 4                0.5169567                0.6670090                0.7518928
    ## 5               -0.4852667               -0.4702194               -0.4227531
    ## 6                2.8904218                2.6142081                2.7637944
    ## 7               -0.5080283               -0.5160640               -0.5401255
    ## 8                2.2749702                2.0696184                1.9238504
    ##   outgoes_quarterly_1996.1 outgoes_quarterly_1996.2 outgoes_quarterly_1996.3
    ## 1                2.4904837                2.1675099                2.1643506
    ## 2               -0.4293223               -0.4724718               -0.5085938
    ## 3                0.9840837                0.8890886                0.6938342
    ## 4                0.7800674                0.6366113                0.4778590
    ## 5               -0.2136693                0.3233224                0.7784634
    ## 6                2.3546111                2.1410421                1.6270928
    ## 7               -0.5233740               -0.4024052               -0.2204936
    ## 8                2.1030909                2.0607308                1.8269265
    ##   outgoes_quarterly_1996.4 outgoes_quarterly_1997.1 outgoes_quarterly_1997.2
    ## 1                1.8247385               1.73302649               1.65040926
    ## 2               -0.5526573              -0.60833903              -0.63093492
    ## 3                0.6125352               0.60651571               0.54638237
    ## 4                0.3956241               0.32794244               0.22884711
    ## 5                1.1759885               1.66536093               1.72760507
    ## 6                1.5852820               1.46475061               1.47057189
    ## 7               -0.1104947              -0.05375182               0.07020629
    ## 8                1.8041011               1.61499243               1.62955778
    ##   outgoes_quarterly_1997.3 outgoes_quarterly_1997.4 outgoes_quarterly_1998.1
    ## 1                1.7516713               1.57605839               1.60322699
    ## 2               -0.6238228              -0.66665604              -0.68533852
    ## 3                0.3810069               0.40645851               0.33215820
    ## 4                0.1337579               0.09426323               0.07768743
    ## 5                1.8130165               1.72044018               1.68332069
    ## 6                1.5193392               1.61997137               1.21318607
    ## 7                0.1494423               0.28112878               0.43231107
    ## 8                1.2287674               1.63538326               1.53949463
    ##   outgoes_quarterly_1998.2 outgoes_quarterly_1998.3 outgoes_quarterly_1998.4
    ## 1               1.30311098               1.51713199               1.23484587
    ## 2              -0.65578744              -0.64381434              -0.64639334
    ## 3               0.39522716               0.28217077               0.34773589
    ## 4               0.04800643               0.02641459               0.02033449
    ## 5               1.60016341               1.70929295               1.72024677
    ## 6               1.48308726               1.19871674               1.34207384
    ## 7               0.41090371               0.39065521               0.39950240
    ## 8               1.31110806               1.44900093               1.42360737
    ##   outgoes_yearly_1993 outgoes_yearly_1994 outgoes_yearly_1995
    ## 1          -0.1645851          0.07192412           2.3965044
    ## 2          -0.2406353         -0.34217160          -0.4187267
    ## 3           1.3158206          1.66696109           1.2240169
    ## 4          -0.1725521          0.14302136           0.6497558
    ## 5          -0.2754518         -0.42990941          -0.5032717
    ## 6           0.9936517          3.82147777           3.0830192
    ## 7          -0.2888056         -0.45767997          -0.5639746
    ## 8           6.0169062          3.13401189           2.3394992
    ##   outgoes_yearly_1996 outgoes_yearly_1997 outgoes_yearly_1998 outgoes_avg
    ## 1           2.4315694           1.8711545          1.55875467   1.8984868
    ## 2          -0.5578284          -0.7072915         -0.72701820  -0.7820066
    ## 3           0.8928343           0.5397555          0.37658690   0.5036876
    ## 4           0.6407874           0.2160930          0.04732936   0.1485094
    ## 5           0.6108716           1.9348865          1.85413864   1.8572476
    ## 6           2.1680880           1.6979352          1.45268333   1.9553404
    ## 7          -0.3478311           0.1293210          0.45130514   0.3329391
    ## 8           2.2004832           1.7078660          1.57835646   1.9852050

``` r
# Display of the cluster (k=15). with autoplot
fitK <- kmeans(df_cons_metrics_scaled, 15)
autoplot(fitK, df_cons_metrics_scaled, frame=TRUE)
```

![](notebook_files/figure-gfm/unnamed-chunk-269-5.png)<!-- -->

``` r
# Display of the cluster (k=25). with autoplot
fitK <- kmeans(df_cons_metrics_scaled, 25)
autoplot(fitK, df_cons_metrics_scaled, frame=TRUE)
```

![](notebook_files/figure-gfm/unnamed-chunk-269-6.png)<!-- -->

Für die Cluster Analyse müssen wir die Clusters Vergleichen von
verschiedenen k-Werten. Dazu haben wir die drei Werte 8, 15 und 25
benutzt. Die genauen Graphen sind nachfolgend aufgelistet: 1. Der Plot
wurde dargestellt um die verschiedene Datenpunkte im Cluster
darzustellen die Punkte sind nach den zugewiesen Cluster eingefärbt. Es
wird mit einem k-Wert von 8 gearbeitet. 2. Der Plot wurde dargestellt um
die verschiedene Datenpunkte im Cluster darzustellen die Punkte sind
nach den zugewiesen Cluster eingefärbt. Es wird mit einem k-Wert von 15
gearbeitet. 3. Der Plot wurde dargestellt um die verschiedene
Datenpunkte im Cluster darzustellen die Punkte sind nach den zugewiesen
Cluster eingefärbt. Es wird mit einem k-Wert von 25 gearbeitet.

Nach der Analyse werden die Clusters mithilfe von Autplot visuell noch
weiter dargestellt.

4.  Der Autplot zeigt die verschiedene Cluster 2 dimensional an und
    bildet eine Areal um die einzelnen Datenpunkte von einem Cluster.
    Diese Clusteriung wurde mit 8 Clustern aufgebaut.
5.  Der Autplot zeigt die verschiedene Cluster 2 dimensional an und
    bildet eine Areal um die einzelnen Datenpunkte von einem Cluster.
    Diese Clusteriung wurde mit 15 Clustern aufgebaut.
6.  Der Autplot zeigt die verschiedene Cluster 2 dimensional an und
    bildet eine Areal um die einzelnen Datenpunkte von einem Cluster.
    Diese Clusteriung wurde mit 25 Clustern aufgebaut.

Wir sehen klar das die Datenpunkte in den Clusters überlappend sind,
daher kann man erkenne, dass die Funktion mit kmeans Probleme hat bei
der Bildung von Clusters oder das die Daten in einer Komplexität
herkommen, wodurch sie von dem Algoritmus nicht korrekt zugewiesen
werden können.

Clustering with the yearly income avg and outcome avg of the customer

``` r
#Select the income and outgoes for the clustering
df_cons_metrics <- df_cons %>%
  dplyr::select(starts_with(c("income_avg", "outgoes_avg")))

str(df_cons_metrics)
```

    ## 'data.frame':    4500 obs. of  2 variables:
    ##  $ income_avg : num  48580 266176 86530 64115 48742 ...
    ##  $ outgoes_avg: num  45213 259071 60981 52875 34698 ...

``` r
#Scale the metrics down for smaller margins between the data points.
df_cons_metrics_scaled <- scale(df_cons_metrics)

# K-Means Clustering (Example with K = 10)
fitK <- kmeans(df_cons_metrics_scaled, 10)

str(fitK)
```

    ## List of 9
    ##  $ cluster     : int [1:4500] 5 2 7 7 5 7 9 9 7 10 ...
    ##  $ centers     : num [1:10, 1:2] 2.091 0.524 1.496 -0.485 -1 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : chr [1:10] "1" "2" "3" "4" ...
    ##   .. ..$ : chr [1:2] "income_avg" "outgoes_avg"
    ##  $ totss       : num 8998
    ##  $ withinss    : num [1:10] 23.96 20.98 26.92 9.39 12.16 ...
    ##  $ tot.withinss: num 200
    ##  $ betweenss   : num 8798
    ##  $ size        : int [1:10] 221 497 246 416 857 120 837 318 513 475
    ##  $ iter        : int 6
    ##  $ ifault      : int 0
    ##  - attr(*, "class")= chr "kmeans"

``` r
cluster_metrics <- plot(df_cons_metrics_scaled, col = fitK$cluster)
```

![](notebook_files/figure-gfm/unnamed-chunk-270-1.png)<!-- -->

``` r
# let's find out the best value for k in the kmean function
k <- list()
for(i in 1:30){
  k[[i]] <- kmeans(df_cons_metrics_scaled, i)
}

betweenss_totss <- list()
for(i in 1:30){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}

cluster_overview <- plot(1:30, betweenss_totss, type = "b",
     ylab = "Between SS / Total SS", xlab = "Clusters (k) for avg income/outgoes")
```

![](notebook_files/figure-gfm/unnamed-chunk-270-2.png)<!-- -->

``` r
autoplot(fitK, df_cons_metrics_scaled, frame=TRUE)
```

![](notebook_files/figure-gfm/unnamed-chunk-270-3.png)<!-- -->

``` r
fitK$centers
```

    ##    income_avg outgoes_avg
    ## 1   2.0912734   2.0794692
    ## 2   0.5237310   0.4930156
    ## 3   1.4963879   1.5163130
    ## 4  -0.4845339  -0.4925856
    ## 5  -0.9997416  -0.9883964
    ## 6   2.8183115   2.9146626
    ## 7  -0.7947281  -0.7806110
    ## 8   0.9378810   0.9163386
    ## 9   0.1530445   0.1406618
    ## 10 -0.1726325  -0.1801609

Wir sehen hier eine weiteres Clustering Verfahren, dabei versuchen wir
Clusters aufzubauen nur mit den jährlichen Einkommen und Ausgaben von
Kunden. Der Scatter-plot zeigt eine saubere Auftrennung der Datenpunkte
in den verschiedenen CLusters.

Für die Bestimmung der Clusters wird wieder die Elbow-Methode gebraucht
in dem zweiten Graphen sind man dazu zwei Möglichkeiten nämlich mit dem
Wert K = 5 und K = 10.

In dem dritten Graphen sieht man eine klare, aber eng beieinandere
Verteilung der Datenpunkte in den einzelnen Cluster. Die Clusters
könnten nun weiterhin gebraucht werden für eine Analyse der einzelnen
Datenpunkte (Kunden) in den Clusters. Man kann weitere Angebote
analysieren, die diese Kunden brauchen und den Kunden empfehlen, die
sich in dem selben Cluster befinden, die jedoch dieses Produkt noch
nicht brauchen.

#### Beliebtester Modus einer Transaktion?[^68]

``` r
# df_transaction_type_count <- df_transaction_complete %>%
#   filter(!is.na(trans_id)) %>%
#   group_by(characterization, quarter) %>%
#   mutate(count = sum(n())) %>%
#   select(count, characterization, quarter) %>%
#   distinct()

#Load the data frame 'df_transaction_complete' for the following visualization.
save(df_transaction_complete, file = "df_transaction_complete.RData")
load("df_transaction_complete.RData")

#add the quarter attribute to generate a overview by the quarters in each year.
df_transaction_complete <- df_transaction_complete %>% 
  mutate(quarter = quarter(date, type = "year.quarter"))

# Filter empty transaction, group the data frame by the charachterization and the quarter to get the count of each transaction type in each quarter. 
df_transaction_type_count <- df_transaction_complete %>%
  filter(!is.na(trans_id)) %>%
  group_by(characterization, quarter) %>%
  mutate(count = sum(n())) %>%
  select(count, characterization, quarter) %>%
  distinct()

df_transaction_type_count
```

    ## # A tibble: 189 × 3
    ## # Groups:   characterization, quarter [189]
    ##    count characterization  quarter
    ##    <int> <fct>               <dbl>
    ##  1 19176 <NA>                1995.
    ##  2 14845 <NA>                1995.
    ##  3  5474 CREDIT INTEREST     1995.
    ##  4 15239 <NA>                1995.
    ##  5  5938 CREDIT INTEREST     1995.
    ##  6  5166 STATEMENT PAYMENT   1995.
    ##  7  3766 HOUSEHOLD           1995.
    ##  8  4120 HOUSEHOLD           1995.
    ##  9 18468 <NA>                1995.
    ## 10  5609 STATEMENT PAYMENT   1995.
    ## # ℹ 179 more rows

``` r
#Display of the transaction type data
ggplot(df_transaction_type_count, aes(fill = characterization, x = quarter, y = count)) +
  labs(title = "Distribution of transaction types per quarters (1993 Q1 - 1998 Q4)", subtitle = "") +
  geom_bar(position="stack", stat="identity") +
  # geom_text(aes(label = frequency), position = position_stack(vjust = 0.5), size = 2) +
  xlab("Quarter") +
  ylab("Count") +
  labs(fill = "Transaction Type") +
  facet_wrap(vars(characterization), ncol = 1)
```

![](notebook_files/figure-gfm/unnamed-chunk-271-1.png)<!-- -->

Wir sehen in dieser Visualisierung die Entwicklung der Transaktionen
bezogene auf die Zeit (Nach den Quartalen). Über alle Typen sehen wir
einen ganz klaren Anstieg der Transaktionen, dass kann durch den Zuwachs
neuer Kundschaft enstanden sein. Desweiteren sehen wir das die meisten
Transaktionen den Typ ‘NA’ aufweisen. Dieses Verhalten liegt sehr
wahrscheinlich davon ab, dass der Typ von der Transaktion nur bei
grösseren Transaktionen aufgenommen wurde oder bei der Ausführung der
Transaktion am Schalter. Da die definierten Typen ‘Credit Interest’,
‘Houhsehold’ und ‘Statement Payment’ am meisten gebraucht wurden. Könnte
man neue Angebote entwickeln die genau Vorteile bei diesen
Transaktionstypen aufweisen.

### Kartografische Darstellung von Analysen

#### Verteilung der Kundschaft auf die Distrikte in Tschechien.[^69]

Vorbereitung der externen geografischen Daten für die Visualisierung in
einer Kartenansicht.[^70]

``` r
# 
#Preparation of the geographical data of the RCzechia package.
geographical_data <- RCzechia::okresy("low")

# Analysis of the structure of the imported geographical data frame.
str(geographical_data)
```

    ## Classes 'sf' and 'data.frame':   77 obs. of  7 variables:
    ##  $ KOD_OKRES  : chr  "40169" "40177" "40185" "40193" ...
    ##  $ KOD_LAU1   : chr  "CZ0201" "CZ0202" "CZ0203" "CZ0204" ...
    ##  $ NAZ_LAU1   : chr  "Benešov" "Beroun" "Kladno" "Kolín" ...
    ##  $ KOD_KRAJ   : chr  "3026" "3026" "3026" "3026" ...
    ##  $ KOD_CZNUTS3: chr  "CZ020" "CZ020" "CZ020" "CZ020" ...
    ##  $ NAZ_CZNUTS3: chr  "Středočeský kraj" "Středočeský kraj" "Středočeský kraj" "Středočeský kraj" ...
    ##  $ geometry   :sfc_GEOMETRY of length 77; first list element: List of 1
    ##   ..$ :List of 1
    ##   .. ..$ : num [1:88, 1:2] 14.9 14.9 14.9 14.8 14.8 ...
    ##   ..- attr(*, "class")= chr [1:3] "XY" "MULTIPOLYGON" "sfg"
    ##  - attr(*, "sf_column")= chr "geometry"
    ##  - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA
    ##   ..- attr(*, "names")= chr [1:6] "KOD_OKRES" "KOD_LAU1" "NAZ_LAU1" "KOD_KRAJ" ...

``` r
# load("df_district_mod.RData")

# str(df_district_mod)
# 
# df_district_mod <- df_district_mod %>% mutate(
#   district_name = as.character(district_name),
#   district_name = ifelse(district_name == "Hl.m. Praha", "Praha", district_name)
#   )
# 
# df_district_mod <- df_district_mod %>%
#   arrange(district_name)
# 
# geographical_data <- geographical_data %>%
#   arrange(NAZ_LAU1)

str(df_cons$account_district_name)
```

    ##  Factor w/ 77 levels "Benesov","Beroun",..: 47 18 30 55 10 69 56 19 27 5 ...

``` r
# The data frame have different values for the district Praha therefore we will change the value in the consolidated data frame from 'H1.m. Praha' to 'Praha'. After the mutation we arrange the data frame by the district name, which will later be used to bind the values.
df_district_mod <- df_cons %>% mutate(
  district_name = as.character(account_district_name),
  district_name = ifelse(district_name == "Hl.m. Praha", "Praha", district_name)
  ) %>%
  arrange(district_name)

#Arrange the data frame to bind it easier in later steps.
geographical_data <- geographical_data %>%
  arrange(NAZ_LAU1)
```

Mithilfe des Data Frames ‘geographical_data’ wird uns in den folgenden
R-Snippets ermöglicht Informationen zu den Distrikten in einer Heatmap
darzustellen bei der das ganze Land untertrennt durch die Staaten
dargestellt wird.

#### Anzahl der Kunden in den einzelnen Distrikten.[^71]

``` r
# Preparation of the data frame that will be used to display the count of customers in each district
df_customers_per_district <- df_district_mod %>%
  group_by(district_name) %>%
  mutate(count = sum(n())) %>%
  select(district_name, count) %>%
  distinct()

df_customers_per_district
```

    ## # A tibble: 77 × 2
    ## # Groups:   district_name [77]
    ##    district_name    count
    ##    <chr>            <int>
    ##  1 Benesov             42
    ##  2 Beroun              50
    ##  3 Blansko             50
    ##  4 Breclav             44
    ##  5 Brno - mesto       128
    ##  6 Brno - venkov       53
    ##  7 Bruntal             43
    ##  8 Ceska Lipa          48
    ##  9 Ceske Budejovice    41
    ## 10 Cesky Krumlov       50
    ## # ℹ 67 more rows

``` r
# df_customers_per_district <- df_customers_per_district %>% mutate(
#     district_name = as.character(account_district_name),
#     district_name = ifelse(account_district_name == "Hl.m. Praha", "Praha", district_name)
#   ) %>%
#   arrange(account_district_name)

#add the native original name to the metric data frame.
df_customers_per_district <- cbind(df_customers_per_district, original_district_name = geographical_data$NAZ_LAU1) %>%
  select(district_name, original_district_name, count)

# We bind the geometry information from the data frame geographical_data with our metric data frame.
df_customers_per_district_incl_geo_data <- left_join(df_customers_per_district, geographical_data, by = c("original_district_name" = "NAZ_LAU1"))

ggplot(data = df_customers_per_district_incl_geo_data) +
  geom_sf(aes(fill = count, geometry = geometry), colour = NA) +
  geom_sf(data = republika("low"), color = "gray30", fill = NA) +
  scale_fill_viridis_c(trans = "log", labels = scales::comma) +
  labs(title = "Distribution of customer location in the districts",
       fill = "Customers") +
  theme(legend.text.align = 1,
        legend.title.align = 0.5)
```

![](notebook_files/figure-gfm/unnamed-chunk-273-1.png)<!-- -->

Wir sehen in dieser Heatmap die Verteilung von Kunden in den einzelnen
Distrikten. Wir haben das Land mithilfe der Geometry Formeln des
Packages ‘RCzechia’ erstellt. Wir sehen das die meisten Kunden sich in
dem Staat ‘Hl.m. Praha’ befinden. Eine grössere Dichte besteht auch im
Osten des Landes. Aus dem Graph können wir ableiten, in welchen Staaten
es sich lohnt die bereits vorhandenen Kundschaft weiter auszubauen, da
die Infrastruktur schon besteht.

#### Durchschnittslohn in den verschiedenen Distrikten[^72]

``` r
# df_district_mod_names <- cbind(df_district_mod, original_name = geographical_data$NAZ_LAU1) %>%
#   select(account_district_name, original_name, account_district_average_salary)

# Prepare the data frame with the average_salary and the name of the district.
df_district_mod_names <- df_district_mod %>%
  select(account_district_name, account_district_average_salary) %>%
  distinct() %>%
  cbind(original_name = geographical_data$NAZ_LAU1)

# Join the prepared data frame by the native original district name with the data frame from RCzechia.
df_average_salary_per_district_including_geo_data <- left_join(df_district_mod_names, geographical_data, by = c("original_name" = "NAZ_LAU1"))

ggplot_average_salary_per_district <- ggplot(data = df_average_salary_per_district_including_geo_data) +
  geom_sf(aes(fill = account_district_average_salary, geometry= geometry), colour = NA) +
  geom_sf(data = republika("low"), color = "gray30", fill = NA) +
  scale_fill_viridis_c(trans = "log", labels = scales::comma) +
  labs(title = "Average Salary per District",
       fill = "Average Salary (CZK)") +
  theme(legend.text.align = 1,
        legend.title.align = 0.5)
```

Average Salary berechnet durch die von Kunden getätigten Transaktionen
in den verschiedenen Distrikten[^73]

``` r
# Join the two data frames to add the district name to the transactions
df_transaction_complete_with_district_information <- left_join(df_transaction_complete, df_cons %>% select(account_id, account_district_name), by = "account_id")

# Prepare the data frame with the data that will be displayed by grouping it by the district name and rounding the values.
incomes_average_overall_with_district_information <- df_transaction_complete_with_district_information %>%
  filter(incomes != 0) %>%
  group_by(
    account_district_name
  ) %>%
  summarise(incomes_avg = round(mean(incomes), digits = 0)) %>% 
  ungroup()

#add the native original name to the prepared dataframe to add the geometry data from RCzechia package which has already been prepared in a previous chunk.
df_transaction_complete_with_district_information <- left_join(incomes_average_overall_with_district_information, df_district_mod_names %>% select(account_district_name, original_name), by = "account_district_name")

#Join the map data frame with the prepared transaction data frame by using the native district name.
df_average_income_per_district_including_geo_data <- left_join(df_transaction_complete_with_district_information, geographical_data, by = c("original_name" = "NAZ_LAU1"))

ggplot_average_income_calculated <- ggplot(data = df_average_income_per_district_including_geo_data) +
  geom_sf(aes(fill = incomes_avg, geometry= geometry), colour = NA) +
  geom_sf(data = republika("low"), color = "gray30", fill = NA) +
  scale_fill_viridis_c(trans = "log", labels = scales::comma) +
  labs(title = "Average Income calulcated from transactions per District",
       fill = "Average Income (CZK)") +
  theme(legend.text.align = 1,
        legend.title.align = 0.5)
```

#### Vergleich Average Einkommen von den Distrikten und dem

Durchschnittlichen Einkommen der Kunden:

``` r
# Create a nested plot to see the comparison between the heatmaps created from the customer transactions and the average salary from the district.
nested_income_and_salary_plot <- (ggplot_average_income_calculated/ggplot_average_salary_per_district)

nested_income_and_salary_plot
```

![](notebook_files/figure-gfm/unnamed-chunk-276-1.png)<!-- -->

In den beiden Graphen sieht man beim ersten Blick klar, das sich die
beiden Fill-Attribute (Average Salary und Average Income) verschiedene
Skalen brauchen, dies kann zuerst ein bisschen täuschend erscheinen. Das
Average Income der Kunden der Bank liegt deutlich tiefer als das
allgmeine durschnittliche Einkommen (Average Salary) in den Distrikten.
Dies kann von folgenden Gründen abhängig sein: - Ein Kunde kann sein
Konto weiterhin erstellt haben, aber auch nach einem Datum x ein neues
Lohnkonto bei einer weiteren Bank eröffnen. - Die Kunden kommen aus den
tieferverdienenden Segmenten.

Aufräumen nach der Darstellungen in der Kartenübersicht

``` r
# save(df_district_mod_names, file = "df_district_mod_names.RData")
rm(df_district_mod_names, df_transaction_complete_with_district_information, incomes_average_overall_with_district_information, df_transaction_complete_with_district_information, df_average_income_per_district_including_geo_data)
```

xi\. Können neue Sonderangebote/Pakete geschaffen werden für die am
meist verdienenden Kunden?[^74]

Wie beim Density-Plot (Verteilung) kommt deutlich zum Ausdruck, dass die
meisten Konten Einkommen bis 320’000 CZK aufweisen. Die
Einkommensentwicklung ist nicht so leicht zu erkennen. Ein geringer
Anstieg über die beobachteten fünf Jahre lässt sich ausmachen mit
Ausnahme des Jahres 1996. Für die nächste Analyse werden alle Accounts
genommen, die ein Einkommen von mehr als 320’000 CZK haben.

Wir greifen hier auf die bereits vorhanden Analyse zu von Leonie.

``` r
str(df_cons %>% select(!starts_with("balance")) %>% select(!starts_with("incomes_quarterly")) %>% select(!starts_with("outgoes_quarterly")))
```

    ## 'data.frame':    4500 obs. of  82 variables:
    ##  $ account_id                     : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ account_frequency              : Factor w/ 3 levels "After_Transaction",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ account_opening_date           : Date, format: "1995-03-24" "1993-02-26" ...
    ##  $ account_opening_year           : num  1995 1993 1997 1996 1997 ...
    ##  $ account_opening_month          : num  3 2 7 2 5 9 11 9 1 8 ...
    ##  $ account_num_of_user            : int  1 2 2 1 1 1 1 2 1 1 ...
    ##  $ account_district_name          : Factor w/ 77 levels "Benesov","Beroun",..: 47 18 30 55 10 69 56 19 27 5 ...
    ##  $ account_district_region        : Factor w/ 8 levels "central Bohemia",..: 6 5 1 1 6 2 7 7 4 7 ...
    ##  $ account_district_average_salary: int  8968 12541 9307 8754 9045 8541 8441 8720 10177 9897 ...
    ##  $ account_district_inhabitants   : int  70699 1204953 95616 107870 58796 121947 110643 161954 285387 387570 ...
    ##  $ order_total_amount_household   : num  2452 7266 1135 3363 2668 ...
    ##  $ order_total_amount_loan        : num  0 3373 0 0 0 ...
    ##  $ order_total_amount_insurrance  : num  0 0 3539 0 0 ...
    ##  $ order_total_amount_unknown     : num  0 0 327 0 0 ...
    ##  $ order_total_amount_leasing     : num  0 0 0 0 0 ...
    ##  $ order_num_household            : num  1 1 1 2 1 1 1 1 NA 1 ...
    ##  $ order_num_loan                 : num  0 1 0 0 0 0 0 0 NA 0 ...
    ##  $ order_num_insurrance           : num  0 0 1 0 0 0 0 0 NA 0 ...
    ##  $ order_num_unknown              : num  0 0 1 0 0 0 0 1 NA 0 ...
    ##  $ order_num_leasing              : num  0 0 0 0 0 0 0 0 NA 1 ...
    ##  $ loan_start_date                : Date, format: NA "1994-01-05" ...
    ##  $ loan_end_date                  : Date, format: NA "1996-01-05" ...
    ##  $ loan_duration_in_month         : int  NA 24 NA NA NA NA NA NA NA NA ...
    ##  $ loan_duration_in_years         : num  NA 2 NA NA NA NA NA NA NA NA ...
    ##  $ loan_total_amount              : int  NA 80952 NA NA NA NA NA NA NA NA ...
    ##  $ loan_redemption_amount         : num  NA 3373 NA NA NA ...
    ##  $ loan_status                    : Factor w/ 4 levels "CONTRACT FINISHED PAYED",..: NA 1 NA NA NA NA NA NA NA NA ...
    ##  $ loan_in_dept                   : logi  NA FALSE NA NA NA NA ...
    ##  $ card_type                      : Factor w/ 3 levels "CLASSIC","GOLD",..: NA NA NA NA NA NA 2 NA NA NA ...
    ##  $ card_issued                    : Date, format: NA NA ...
    ##  $ owner_dateofbirth              : Date, format: "1970-12-13" "1945-02-04" ...
    ##  $ age_dec1999                    : num  29 54 43 80 70 61 64 56 18 25 ...
    ##  $ owner_sex                      : Factor w/ 2 levels "Female","Male": 1 2 2 2 2 1 2 2 2 1 ...
    ##  $ owner_age_at_account_opening   : num  24 48 40 76 68 56 61 52 11 22 ...
    ##  $ owner_district_name            : Factor w/ 77 levels "Benesov","Beroun",..: 47 18 30 55 10 69 56 19 27 5 ...
    ##  $ owner_district_region          : Factor w/ 8 levels "central Bohemia",..: 6 5 1 1 6 2 7 7 4 7 ...
    ##  $ owner_district_average_salary  : int  8968 12541 9307 8754 9045 8541 8441 8720 10177 9897 ...
    ##  $ owner_district_inhabitants     : int  70699 1204953 95616 107870 58796 121947 110643 161954 285387 387570 ...
    ##  $ user_dateofbirth               : Date, format: NA "1940-10-09" ...
    ##  $ user_sex                       : Factor w/ 2 levels "Female","Male": NA 1 1 NA NA NA NA 1 NA NA ...
    ##  $ user_age_at_account_opening    : num  NA 52 37 NA NA NA NA 45 NA NA ...
    ##  $ account_district_id            : int  18 1 5 12 15 51 60 57 70 54 ...
    ##  $ loan_id                        : int  NA 4959 NA NA NA NA NA NA NA NA ...
    ##  $ card_id                        : int  NA NA NA NA NA NA 1 NA NA NA ...
    ##  $ owner_client_id                : int  1 2 4 6 7 8 9 10 12 13 ...
    ##  $ owner_disp_id                  : int  1 2 4 6 7 8 9 10 12 13 ...
    ##  $ owner_district_id              : int  18 1 5 12 15 51 60 57 40 54 ...
    ##  $ user_client_id                 : int  NA 3 5 NA NA NA NA 11 NA NA ...
    ##  $ user_disp_id                   : int  NA 3 5 NA NA NA NA 11 NA NA ...
    ##  $ user_district_id               : int  NA 1 5 NA NA NA NA 57 NA NA ...
    ##  $ incomes_yearly_1993            : num  0 228685 0 0 0 ...
    ##  $ incomes_yearly_1994            : num  0 264653 0 0 0 ...
    ##  $ incomes_yearly_1995            : num  49564 265362 0 0 0 ...
    ##  $ incomes_yearly_1996            : num  50066 288543 0 56920 0 ...
    ##  $ incomes_yearly_1997            : num  49275 281578 58140 67645 35951 ...
    ##  $ incomes_yearly_1998            : num  45415 268232 114919 67781 61534 ...
    ##  $ outgoes_yearly_1993            : num  0 200845 0 0 0 ...
    ##  $ outgoes_yearly_1994            : num  0 259581 0 0 0 ...
    ##  $ outgoes_yearly_1995            : num  31391 258854 0 0 0 ...
    ##  $ outgoes_yearly_1996            : num  55579 274574 0 31978 0 ...
    ##  $ outgoes_yearly_1997            : num  47139 281093 31390 72711 8390 ...
    ##  $ outgoes_yearly_1998            : num  46745 279480 90573 53937 61007 ...
    ##  $ owner_current_age              : num  28 53 42 79 69 60 63 55 17 24 ...
    ##  $ age_groups                     : Factor w/ 7 levels "0-17","18-30",..: 2 5 4 7 6 5 6 5 1 2 ...
    ##  $ has_creditcard                 : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ is_checking_account            : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##  $ outgoes_avg                    : num  45213 259071 60981 52875 34698 ...
    ##  $ income_avg                     : num  48580 266176 86530 64115 48742 ...
    ##  $ is_regular_income              : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##  $ is_regular_income_80           : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##  $ consultation_on_site           : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##  $ salary_groups                  : Factor w/ 6 levels "lower-income",..: 1 5 2 2 1 2 4 4 2 4 ...
    ##  $ assets_dev                     : num  13466 42628 51096 33721 28088 ...
    ##  $ assets_dev_groups              : Factor w/ 2 levels "negative","positive": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ is_savings_account             : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ is_loan_account                : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ cardwidthdrawals_num           : int  NA NA NA NA NA NA 1 NA NA NA ...
    ##  $ cardwidthdrawals_amount_mean   : num  NA NA NA NA NA NA 5900 NA NA NA ...
    ##  $ is_card_offer                  : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##  $ is_retirement_age              : logi  FALSE FALSE FALSE TRUE TRUE FALSE ...
    ##  $ has_res_prop                   : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##  $ heloc_offer                    : logi  FALSE FALSE FALSE TRUE TRUE FALSE ...

``` r
account_ids_incomes_over_320K <- df_cons %>%
  filter(income_avg > 320000) %>%
  distinct()

str(account_ids_incomes_over_320K)
```

    ## 'data.frame':    689 obs. of  202 variables:
    ##  $ account_id                     : int  25 30 31 34 47 52 66 67 68 72 ...
    ##  $ account_frequency              : Factor w/ 3 levels "After_Transaction",..: 2 2 2 3 2 2 1 2 1 3 ...
    ##  $ account_opening_date           : Date, format: "1996-07-28" "1995-03-04" ...
    ##  $ account_opening_year           : num  1996 1995 1994 1993 1993 ...
    ##  $ account_opening_month          : num  7 3 4 9 8 1 12 10 12 6 ...
    ##  $ account_num_of_user            : int  1 1 1 1 1 1 1 1 1 2 ...
    ##  $ account_district_name          : Factor w/ 77 levels "Benesov","Beroun",..: 16 33 16 7 23 25 59 25 34 18 ...
    ##  $ account_district_region        : Factor w/ 8 levels "central Bohemia",..: 4 3 4 4 2 6 2 6 3 5 ...
    ##  $ account_district_average_salary: int  9893 9198 9893 8110 8390 8427 9060 8427 9065 12541 ...
    ##  $ account_district_inhabitants   : int  228848 159617 228848 106054 77917 93931 78955 93931 114006 1204953 ...
    ##  $ order_total_amount_household   : num  6817 0 10202 502 6940 ...
    ##  $ order_total_amount_loan        : num  2523 0 0 0 0 ...
    ##  $ order_total_amount_insurrance  : num  164 0 0 0 141 0 0 0 137 0 ...
    ##  $ order_total_amount_unknown     : num  1110 0 0 6070 183 ...
    ##  $ order_total_amount_leasing     : num  0 2480 0 1479 0 ...
    ##  $ order_num_household            : num  1 0 1 1 1 1 0 1 1 1 ...
    ##  $ order_num_loan                 : num  1 0 0 0 0 0 0 1 0 0 ...
    ##  $ order_num_insurrance           : num  1 0 0 0 1 0 0 0 1 0 ...
    ##  $ order_num_unknown              : num  1 0 0 1 2 1 0 1 2 1 ...
    ##  $ order_num_leasing              : num  0 1 0 1 0 0 1 0 0 1 ...
    ##  $ loan_start_date                : Date, format: "1997-12-08" NA ...
    ##  $ loan_end_date                  : Date, format: "1998-12-08" NA ...
    ##  $ loan_duration_in_month         : int  12 NA NA NA NA NA NA 24 NA NA ...
    ##  $ loan_duration_in_years         : num  1 NA NA NA NA NA NA 2 NA NA ...
    ##  $ loan_total_amount              : int  30276 NA NA NA NA NA NA 165960 NA NA ...
    ##  $ loan_redemption_amount         : num  2523 NA NA NA NA ...
    ##  $ loan_status                    : Factor w/ 4 levels "CONTRACT FINISHED PAYED",..: 1 NA NA NA NA NA NA 1 NA NA ...
    ##  $ loan_in_dept                   : logi  FALSE NA NA NA NA NA ...
    ##  $ card_type                      : Factor w/ 3 levels "CLASSIC","GOLD",..: NA NA NA 1 NA NA 1 NA 2 NA ...
    ##  $ card_issued                    : Date, format: NA NA ...
    ##  $ owner_dateofbirth              : Date, format: "1962-02-09" "1973-07-23" ...
    ##  $ age_dec1999                    : num  37 26 31 64 51 51 43 55 30 38 ...
    ##  $ owner_sex                      : Factor w/ 2 levels "Female","Male": 2 1 2 2 1 1 1 1 1 2 ...
    ##  $ owner_age_at_account_opening   : num  34 21 26 58 44 48 37 50 25 34 ...
    ##  $ owner_district_name            : Factor w/ 77 levels "Benesov","Beroun",..: 16 33 16 7 23 25 59 25 34 18 ...
    ##  $ owner_district_region          : Factor w/ 8 levels "central Bohemia",..: 4 3 4 4 2 6 2 6 3 5 ...
    ##  $ owner_district_average_salary  : int  9893 9198 9893 8110 8390 8427 9060 8427 9065 12541 ...
    ##  $ owner_district_inhabitants     : int  228848 159617 228848 106054 77917 93931 78955 93931 114006 1204953 ...
    ##  $ user_dateofbirth               : Date, format: NA NA ...
    ##  $ user_sex                       : Factor w/ 2 levels "Female","Male": NA NA NA NA NA NA NA NA NA 1 ...
    ##  $ user_age_at_account_opening    : num  NA NA NA NA NA NA NA NA NA 26 ...
    ##  $ account_district_id            : int  68 36 68 67 45 16 48 16 37 1 ...
    ##  $ loan_id                        : int  4962 NA NA NA NA NA NA 4973 NA NA ...
    ##  $ card_id                        : int  NA NA NA 4 NA NA 10 NA 11 NA ...
    ##  $ owner_client_id                : int  31 38 39 42 55 61 77 78 79 85 ...
    ##  $ owner_disp_id                  : int  31 38 39 42 55 61 77 78 79 85 ...
    ##  $ owner_district_id              : int  68 36 68 68 45 16 1 16 37 1 ...
    ##  $ user_client_id                 : int  NA NA NA NA NA NA NA NA NA 86 ...
    ##  $ user_disp_id                   : int  NA NA NA NA NA NA NA NA NA 86 ...
    ##  $ user_district_id               : int  NA NA NA NA NA NA NA NA NA 1 ...
    ##  $ balance_1993-01-31             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ balance_1993-02-28             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ balance_1993-03-31             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ balance_1993-04-30             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ balance_1993-05-31             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ balance_1993-06-30             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ balance_1993-07-31             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ balance_1993-08-31             : num  0 0 0 0 35657 ...
    ##  $ balance_1993-09-30             : num  0 0 0 600 65545 ...
    ##  $ balance_1993-10-31             : num  0 0 0 29605 79714 ...
    ##  $ balance_1993-11-30             : num  0 0 0 41197 87323 ...
    ##  $ balance_1993-12-31             : num  0 0 0 56168 36086 ...
    ##  $ balance_1994-01-31             : num  0 0 0 55344 19202 ...
    ##  $ balance_1994-02-28             : num  0 0 0 53429 52154 ...
    ##  $ balance_1994-03-31             : num  0 0 0 47633 68188 ...
    ##  $ balance_1994-04-30             : num  0 0 800 45295 75755 ...
    ##  $ balance_1994-05-31             : num  0 0 42081 20956 79942 ...
    ##  $ balance_1994-06-30             : num  0 0 64264 20808 61009 ...
    ##  $ balance_1994-07-31             : num  0 0 53193 19565 69550 ...
    ##  $ balance_1994-08-31             : num  0 0 90294 33632 74012 ...
    ##  $ balance_1994-09-30             : num  0 0 45444 42342 80500 ...
    ##  $ balance_1994-10-31             : num  0 0 28010 35869 25515 ...
    ##  $ balance_1994-11-30             : num  0 0 22104 41444 51772 ...
    ##  $ balance_1994-12-31             : num  0 0 39476 43573 48541 ...
    ##  $ balance_1995-01-31             : num  0 0 57178 57434 25524 ...
    ##  $ balance_1995-02-28             : num  0 0 26427 61918 53685 ...
    ##  $ balance_1995-03-31             : num  0 400 31845 53193 27904 ...
    ##  $ balance_1995-04-30             : num  0 27921 62463 61681 29982 ...
    ##  $ balance_1995-05-31             : num  0 93214 73552 54776 37552 ...
    ##  $ balance_1995-06-30             : num  0 45811 50680 78144 31124 ...
    ##  $ balance_1995-07-31             : num  0 46374 65623 68980 30638 ...
    ##  $ balance_1995-08-31             : num  0 30896 45078 74123 37012 ...
    ##  $ balance_1995-09-30             : num  0 40225 59552 71253 18568 ...
    ##  $ balance_1995-10-31             : num  0 31415 71566 73186 28992 ...
    ##  $ balance_1995-11-30             : num  0 38871 65110 70814 24152 ...
    ##  $ balance_1995-12-31             : num  0 57652 67360 85811 44368 ...
    ##  $ balance_1996-01-31             : num  0 21403 97841 51694 19812 ...
    ##  $ balance_1996-02-29             : num  0 21277 69503 60683 29230 ...
    ##  $ balance_1996-03-31             : num  0 34149 80095 67488 32187 ...
    ##  $ balance_1996-04-30             : num  0 21269 90326 62612 35453 ...
    ##  $ balance_1996-05-31             : num  0 34027 80191 67920 31462 ...
    ##  $ balance_1996-06-30             : num  0 28222 32565 55415 32643 ...
    ##  $ balance_1996-07-31             : num  900 72939 62080 46088 33203 ...
    ##  $ balance_1996-08-31             : num  16700 102989 28119 58550 38166 ...
    ##  $ balance_1996-09-30             : num  58136 62600 31719 62954 41146 ...
    ##  $ balance_1996-10-31             : num  87590 39743 77665 34441 39430 ...
    ##  $ balance_1996-11-30             : num  69208 50200 75442 34145 38004 ...
    ##  $ balance_1996-12-31             : num  63154 34788 39641 42176 43032 ...
    ##  $ balance_1997-01-31             : num  29352 29765 47686 46670 27786 ...
    ##   [list output truncated]

``` r
frequency_card_type_accounts_over_320K <- account_ids_incomes_over_320K %>%
  group_by(card_type) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

frequency_card_type_accounts_over_320K
```

    ## # A tibble: 4 × 3
    ##   card_type     n   freq
    ##   <fct>     <int>  <dbl>
    ## 1 CLASSIC     203 0.295 
    ## 2 GOLD         29 0.0421
    ## 3 JUNIOR       39 0.0566
    ## 4 <NA>        418 0.607

Mithilfer der Häufigkeitstabelle erhalten wir eine klare Übersicht über
die Verteilung der Kreditkarten Typen von den Grossverdienern. Wir
sehene das ca. 29.46% der Kunden eine Kreditkarte vom Type ‘Classic’
besitzen und das ca. 60% der Kunden in diesem Segment keine Kreditkarten
von dieser Bank besitzen. Daher kann nun ein Angebot erstellt werden,
das zusammen mit der Aktivierung einer Kreditkarte eingelöst werden
kann. Dadurch können weitere Kunden angezogen werden und auch die Kunden
in diesem Segmente könnten sich entscheiden eine Kreditkarte zu
beantragen.

### Korrelationen zwischen Datenattributen:

#### Wahrscheinlichkeit dass ein Kunden seinen Kredit zurückbezahlen wird

Ziel dieses Abschnittes ist es, Prognosen für die Bank zu erarbeiten, um
festzustellen, ob Kunden ihre Kredite zurückzahlen können. Die
Kreditwürdigkeit des Kunden kann mithilfe der Analyse festgestellt
werden und bei bereits laufenden Krediten können Massnahmen getroffen
werden.[^75]

``` r
df_cons %>%
  drop_na(loan_status) %>%
  group_by(loan_status) %>%
  ggplot(aes(x = reorder(loan_status, loan_status,
                         function(x)-length(x)),
                         fill = loan_status)) +
  geom_bar() +
  theme(legend.position = "none") +
  labs(title = "Number and Status of Loans", 
        subtitle = "", 
        caption = "Source: PKDD’99 Discovery Challenge",
        x = "", y = "Number of loans",
        fill = paste("Loan status")) +
  coord_flip()
```

![](notebook_files/figure-gfm/unnamed-chunk-279-1.png)<!-- -->

Das Balkendiagramm zeigt die Anzahl der gewährten Kredite inklusive
deren Status. Daraus wird ersichtlich, dass der grösste Teil der Kunden
ihren Kredit bereits erfolgreich zurückzahlen wird, oder ihn bereits
zurückbezahlt hat.

``` r
# Preparation of Plots

# Calculate customer age at getting loan
df_cons$age_at_loan <- trunc((df_cons$owner_dateofbirth %--% df_cons$loan_start_date) / years(1))

#Relocate rows so new column "age_at_loan" is next to owner_dateofbirth
df_cons <- df_cons %>%
   relocate(age_at_loan, .after = card_issued)

# Divide yearly income column with yearly loan column
df_cons$loan_per_year <- df_cons$loan_total_amount / df_cons$loan_duration_in_years
df_cons <- df_cons %>%
   relocate(loan_per_year, .after = loan_redemption_amount)

df_cons$ratio_loan_to_sallary <-df_cons$income_avg / df_cons$loan_per_year  
df_cons <- df_cons %>%
   relocate(ratio_loan_to_sallary, .after = income_avg)



# Create Plots
loanplot1 <- df_cons %>%
  drop_na(loan_status) %>%
  ggplot(df_cons, mapping = aes(x = loan_duration_in_month, y = loan_status, fill = loan_status)) +
  geom_boxplot() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        plot.background = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(title = "Loan Duration in Months", 
           subtitle = "", 
           caption = "",
           y = "", x = "Months") +
  coord_flip()


loanplot2 <- df_cons %>%
  drop_na(loan_status) %>%
  ggplot(df_cons, mapping = aes(x = loan_total_amount, y = loan_status, fill = loan_status)) +
  geom_boxplot() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        plot.background = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_continuous(breaks = scales::breaks_pretty(10)) +
  labs(title = "Loan total amount", 
           subtitle = "", 
           caption = "",
           y = "", x = "Loan in CZK") +
  coord_flip()


loanplot3 <- df_cons %>%
  drop_na(loan_status) %>%
  ggplot(df_cons, mapping = aes(x = age_at_loan, y = loan_status, fill = loan_status)) +
  geom_boxplot() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        plot.background = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(title = "Customer Age at beginning of Loan", 
           subtitle = "", 
           caption = "",
           y = "", x = "Years") +
  coord_flip()


loanplot4 <- df_cons %>%
  drop_na(loan_status) %>%
  ggplot(df_cons, mapping = aes(x = ratio_loan_to_sallary, y = loan_status, fill = loan_status)) +
  geom_boxplot() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        plot.background = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(title = "Ratio Salary to Loan", 
           subtitle = "Average annual salary divided by annual loan charge", 
           caption = "Source: PKDD’99 Discovery Challenge",
           y = "", x = "Ratio") +
  coord_flip()


loanplot5 <- df_cons %>%
  drop_na(loan_status) %>%
  ggplot(df_cons, mapping = aes(x = loan_start_date, y = loan_total_amount, color = loan_status)) +
  geom_point(size = 0.7) +
  geom_smooth(method="lm", se = FALSE, size = 0.6) +
  scale_y_continuous(breaks = scales::breaks_pretty(10)) +
  scale_x_date(breaks = scales::breaks_pretty(6)) +
  labs(title = "Loans granted throughout the Years", 
           subtitle = "", 
           caption = "",
           y = "Amount of Loan", x = "Years",
           color = "Status of loan") +
  theme(plot.background = element_rect(colour = "black", fill=NA, size=0.5)) 


# Create multi-panel Plot with patchwork
nested_loan_plot <- (loanplot5/(loanplot1|loanplot2|loanplot3|loanplot4)) 
nested_loan_plot
```

![](notebook_files/figure-gfm/unnamed-chunk-280-1.png)<!-- -->

Die Abbildung oben zeigt fünf verschiedene Plots, welche alle
Zusammenhänge im Kreditstatus der Kunden aufzeigen. Ziel ist es daraus
Erkenntnisse zu gewinnen, um festzustellen welches die Gründe sind,
damit jemand seinen Kredit nicht zurückbezahlten kann. Der Scatterplot
zeigt auf wie sich die höhen der einzelnen Kredite in den Jahren 1993
bis 1999 verändert hat. Zusätzlich wird farblich der jeweilige Status
des Kredites hervorgehoben.

Die Boxplots in der unteren Reihe stellen den Kreditstatus gegenüber
verschiedenen Attributen dar. Dabei sind in den Boxplots 1 -3 keine
Besonderheiten bezüglich des Kreditstatus gegenüber den jeweiligen
Attributen festzustellen. Einzig in Plot 4 (“Ratio Salary to Loan”)
sieht man, dass das Verhältnis von Kredit zu durchschnittlichem
Jahreseinkommen bei den unbezahlten (“Contract Finished unpaid”) und den
Krediten, welche im Verzug sind (“Contract open indept”) tiefer ist.
Dies bedeutet, dass die Kreditsumme einen höheren Anteil des jeweiligen
Jahreseinkommens ausmacht und somit auch schneller zu einer Belastung
wird, was ein Grund für das nicht- oder zu spät bezahlen der Kredite
sein könnte.

#### Decision Tree und Random Forest Modelle zur Vorhersage der Kreditwürdigkeit[^76]

Für einen einfacheren Umgang mit den Daten wird eine Kopie des
Originalen Datensatzes mit allen relevanten Attributen erstellt.

``` r
# Preparation of Dataframe with 
df_cons_predict <- df_cons %>%
  drop_na(loan_status) %>%
  select(c(1, 23, 25, 27, 29, 34, 35, 189 ,190, 191)) 

df_cons_predict$loan_in_dept <- gsub("TRUE", "Yes",
                               gsub("FALSE", "No", df_cons_predict$loan_in_dept))

df_cons_predict$loan_in_dept <- as.factor(df_cons_predict$loan_in_dept)
  
# Checking Data
summary(df_cons_predict)
```

    ##    account_id    loan_duration_in_month loan_total_amount loan_per_year   
    ##  Min.   :    2   Min.   :12.00          Min.   :  4980    Min.   :  3648  
    ##  1st Qu.: 2967   1st Qu.:24.00          1st Qu.: 66732    1st Qu.: 29724  
    ##  Median : 5738   Median :36.00          Median :116928    Median : 47208  
    ##  Mean   : 5824   Mean   :36.49          Mean   :151410    Mean   : 50288  
    ##  3rd Qu.: 8686   3rd Qu.:48.00          3rd Qu.:210654    3rd Qu.: 69762  
    ##  Max.   :11362   Max.   :60.00          Max.   :590820    Max.   :118920  
    ##  loan_in_dept  age_dec1999    owner_sex    outgoes_avg       income_avg    
    ##  No :606      Min.   :19.0   Female:348   Min.   : 32387   Min.   : 42898  
    ##  Yes: 76      1st Qu.:30.0   Male  :334   1st Qu.:185409   1st Qu.:191488  
    ##               Median :41.0                Median :275410   Median :283026  
    ##               Mean   :40.9                Mean   :298179   Mean   :304100  
    ##               3rd Qu.:52.0                3rd Qu.:409474   3rd Qu.:414870  
    ##               Max.   :64.0                Max.   :682931   Max.   :714076  
    ##  ratio_loan_to_sallary
    ##  Min.   : 0.6959      
    ##  1st Qu.: 4.8175      
    ##  Median : 6.2682      
    ##  Mean   : 7.9244      
    ##  3rd Qu.: 8.9870      
    ##  Max.   :69.9117

Aufteilung der Daten in Trainings- und Testsätze

Die split Methode wird verwendet, um die Daten in Trainings- und
Testsätze aufzuteilen. Der Datensatz wird in loan_train und loan_test
unterteilt. Das loan_test Set wird verwendet um festzustellen, ob das
Modell die richtigen Ausgaben vorhersagt.  
Es wird empfohlen, die Trainingsmengen größer zu halten als die
Testmengen.

Trainingsdatensatz: Der Trainingsdatensatz wird für die Anpassung des
Modells verwendet. Der Datensatz, auf dem das Modell trainiert wird.

Testdatensatz: Der Testdatensatz ist eine Teilmenge des
Trainingsdatensatzes, die verwendet wird, um eine genaue Bewertung der
endgültigen Anpassung des Modells zu erhalten.

Unausgeglichener Datensatz

``` r
prop.table(table(df_cons_predict$loan_in_dept))
```

    ## 
    ##       No      Yes 
    ## 0.888563 0.111437

``` r
table(df_cons_predict$loan_in_dept)
```

    ## 
    ##  No Yes 
    ## 606  76

Wie mit prop.table dargestellt, ist die Yes und No Verteilung im
Datensatz sehr unausgeglichen, von den 682 Beobachtungen sind 606 als No
deklariert. Dies liegt hauptsächlich daran, dass die meisten Kunden ihre
Kredite zurückzahlen. Dieses Problem kann mit over- oder undersamplig
gelöst werden. Durch oversampling werden nun künstliche Yes Werte
generiert um einen ausgeglichenen Datensatz zu erstellen.

``` r
data_balanced_over <- ovun.sample(loan_in_dept ~ ., data = df_cons_predict, method = "over",N = 1212)$data
```

Im Code bezieht sich N auf die Anzahl der Beobachtungen welche das
angepasst Datensatz nach dem oversampling haben wird. Da sich im
originalen Datensatz 606 Yes Werte befinden, sollten die No Werte im
angepasste Datensatz diesen Wert nicht übersteigen. Allerdings sollten
auch nicht zu viele Yes Werte künstlich generiert werden, da dies die
Aussagekraft des Modells verschlechtern kann. Somit erscheint ein Wert
von N = 1212 als sinnvoll.

``` r
prop.table(table(data_balanced_over$loan_in_dept))
```

    ## 
    ##  No Yes 
    ## 0.5 0.5

``` r
table(data_balanced_over$loan_in_dept)
```

    ## 
    ##  No Yes 
    ## 606 606

``` r
df_cons_predict <- data_balanced_over
```

Der angepasste Datensatz kann jetzt in in Trainings- und Testsätze
aufgeteilt werden.

``` r
# splitting the data into training and test sets
set.seed(321)
sample <- sample.int(n = nrow(df_cons_predict), size = floor(.80*nrow(df_cons_predict)), replace = F)
loan_train <- df_cons_predict[sample, ]
loan_test  <- df_cons_predict[-sample, ]


# Size of traiing and test set
dim(loan_train)
```

    ## [1] 969  10

``` r
dim(loan_test)
```

    ## [1] 243  10

Verteilung der Yes und No Werte im Trainings- und Testdatensatz

``` r
prop.table(table(loan_train$loan_in_dept))
```

    ## 
    ##       No      Yes 
    ## 0.498452 0.501548

``` r
prop.table(table(loan_test$loan_in_dept))
```

    ## 
    ##        No       Yes 
    ## 0.5061728 0.4938272

##### Decision Tree / Entscheidungsbaum[^77]

Ein Entscheidungsbaum ist eine Art des supervised machine learning, das
zur Kategorisierung oder zur Erstellung von Vorhersagen auf der
Grundlage der Antworten auf eine frühere Reihe von Fragen verwendet
wird.

Durch die Anwendung dieses Modells erhoffen wir uns Attribute zu
definieren welche ein prägende Rolle spielen bei der Rückzahlung von
Krediten. Dazu wird ein Entscheidungsbaum mithilfe der loan_train Daten
erstellt.

``` r
# Building Decision Tree
set.seed(321)
dtree_loan <- rpart(loan_in_dept ~ loan_duration_in_month + loan_per_year + age_dec1999 + owner_sex + income_avg + outgoes_avg, 
               data = loan_train, 
               method = "class",
               control = rpart.control(cp = 0.0001))

# Plotting first draft of Tree, digits argument rounds numbers acording to value (-3) 
rpart.plot(dtree_loan, digits=-3)
```

![](notebook_files/figure-gfm/unnamed-chunk-287-1.png)<!-- -->

Kreuzvalidierung zur Bestimmung der optimalen Größe des
Klassifikationsbaumes. Die Baumgrösse kann gesteuert werden, indem ein
sogenannter cp-Werte (complexity parameter) festgesetzt wird.

``` r
plotcp(dtree_loan)
```

![](notebook_files/figure-gfm/unnamed-chunk-288-1.png)<!-- -->

``` r
printcp(dtree_loan)
```

    ## 
    ## Classification tree:
    ## rpart(formula = loan_in_dept ~ loan_duration_in_month + loan_per_year + 
    ##     age_dec1999 + owner_sex + income_avg + outgoes_avg, data = loan_train, 
    ##     method = "class", control = rpart.control(cp = 1e-04))
    ## 
    ## Variables actually used in tree construction:
    ## [1] age_dec1999            income_avg             loan_duration_in_month
    ## [4] loan_per_year          outgoes_avg            owner_sex             
    ## 
    ## Root node error: 483/969 = 0.49845
    ## 
    ## n= 969 
    ## 
    ##            CP nsplit rel error  xerror     xstd
    ## 1  0.26086957      0   1.00000 1.07867 0.032133
    ## 2  0.02898551      1   0.73913 0.73913 0.031089
    ## 3  0.02360248      2   0.71014 0.63975 0.030036
    ## 4  0.02277433     14   0.38302 0.53623 0.028521
    ## 5  0.02070393     15   0.36025 0.51139 0.028087
    ## 6  0.01863354     16   0.33954 0.50518 0.027974
    ## 7  0.00966184     18   0.30228 0.42857 0.026415
    ## 8  0.00931677     22   0.26294 0.40787 0.025938
    ## 9  0.00828157     24   0.24431 0.39545 0.025639
    ## 10 0.00724638     27   0.21946 0.39130 0.025537
    ## 11 0.00517598     29   0.20497 0.40166 0.025789
    ## 12 0.00414079     31   0.19462 0.40166 0.025789
    ## 13 0.00207039     32   0.19048 0.40787 0.025938
    ## 14 0.00069013     36   0.18219 0.39959 0.025740
    ## 15 0.00010000     39   0.18012 0.39545 0.025639

Die Spalten in der Ausgabe listen die cp-Werte, die zugehörige Anzahl
der Splits (nsplit), den relativen Fehler (rel error) bezogen auf den
Fehler im Wurzelknoten auf. Wie man der Tabelle, entnehmen kann, ist der
xerror + xstd bei 24 Blättern, also bei 23 Splits am kleinsten. Daraus
können wir einen optimalen cp-Wert von 0.00828157 ablesen.

``` r
# Decision Tree with modifyled cp-Value
set.seed(321)
dtree_pruned <- rpart(loan_in_dept ~ loan_duration_in_month + loan_per_year + age_dec1999 + owner_sex + income_avg + outgoes_avg, 
               data = loan_train, 
               method = "class",
               control = rpart.control(cp = 0.00828157))

# Plotting pruned Tree
rpart.plot(dtree_pruned, split.prefix = "if ", fallen.leaves = TRUE, shadow.col = "gray", digits=-3, main = "Is Customer in Dept?\n(Training Data)")
```

![](notebook_files/figure-gfm/unnamed-chunk-289-1.png)<!-- -->

``` r
# Numerical output of the tree
#dtree_pruned
```

Genauigkeit des Modells mit predict-Funktion und Confusion-Matrix
bestimmen

``` r
dtred_pred_train <- predict(dtree_pruned, loan_train, type="class")

# Creating Confusion Matrix
confusion_matrix_train  <- table(loan_train$loan_in_dept, dtred_pred_train)
confusion_matrix_train
```

    ##      dtred_pred_train
    ##        No Yes
    ##   No  402  81
    ##   Yes  25 461

``` r
# Calculating accuracy 
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)



paste("Genauigkeit des Modells: ", round((accuracy_train * 100), digits = 2), "%" )
```

    ## [1] "Genauigkeit des Modells:  89.06 %"

Mithilfe der Confusion Matrix können wir unser Klassifikationsmodell
evaluieren. Die zweidimensionale Matrix stellt die vorhergesagten
Klassen steht sowie die tatsächlichen Klassen dar. Daraus kann abgelesen
werden, dass das Modell einen grossen Teil richtig klassifiziert hat.
Lediglich 108 Kunden wurden falsch klassifiziert.

  
Testen des Modells mithilfe des loan_test Datensatzes

``` r
dtree_pred_test <- predict(dtree_pruned, loan_test, type="class")

# Creating Confusion Matrix
confusion_matrix_test  <- table(loan_test$loan_in_dept, dtree_pred_test)
confusion_matrix_test
```

    ##      dtree_pred_test
    ##        No Yes
    ##   No   82  41
    ##   Yes   7 113

``` r
# Calculating accuracy 
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)

paste("Genauigkeit des Modells: ", round((accuracy_test * 100), digits = 2), "%" )
```

    ## [1] "Genauigkeit des Modells:  80.25 %"

##### Random Forest[^78]

Bei Random Forests werden viele Entscheidungsbäume gleichzeitig
erstellt, wobei bei jedem Baum nur eine Teilmenge der Daten genutzt
wird. Für die Prognose der Klasse eines neuen Objekts wird eine
Mehrheitsentscheidung der Bäume gefällt (majority vote). Es erfolgt kein
Pruning der einzelnen Bäume, da man davon ausgeht, dass sich
Overﬁttingprobleme aufgrund der verschiedenen Teilmengen ausgleichen und
daher kein Problem mehr darstellen.

Für den Aufbau des Random Forest erfolgt eine Zufallsauswahl auf zwei
Ebenen:

1.  Es wird bei jedem Split nur eine zufällig ausgewählte Teilmenge der
    möglichen Splitvariablen benutzt.

2.  Für jeden Baum werden Bootstrap-Stichproben aus der gesamten Datei
    gezogen. Bei Bootstrap-Stichproben werden aus der gesamten Datei mit
    n Objekten nach dem Zufallsprinzip Stichproben wiederum der Größe n
    gezogen. Die Ziehung erfolgt mit Zurücklegen, d. h. dasselbe Objekt
    kann auch mehrfach in eine Stichprobe gelangen.

Erstellen des Modells mit den loan_train Daten.

``` r
set.seed(321)
# Creating of Random Forest Model (default Number of Trees = 100, na.action = na.omit removes incomplete cases)
rf_model <- randomForest(as.factor(loan_in_dept) ~ loan_duration_in_month + loan_per_year + age_dec1999 + owner_sex + income_avg + outgoes_avg, 
                   data = loan_train,
                   na.action = na.omit,
                   importance=TRUE)

# Output the total error rate
plot(rf_model)
```

![](notebook_files/figure-gfm/unnamed-chunk-292-1.png)<!-- -->

Der Ausgabe können wir die Gesamtfehlerrate (schwarze Linie), die Rate
der fälschlicherweise als korrekt prognostizierten (grüne Linie) und die
fälschlicherweise als falsch prognostizierten (rote Linie) Objekte
entnehmen. In der Graﬁk sind die Werte für random forests mit 500 Bäumen
dargestellt. Bei spätestens 150 Bäumen stabilisieren sich alle Werte.
Daher führen wir die Analyse nochmals mit 150 Bäumen durch.

``` r
set.seed(321)
rf_model_150 <- randomForest(as.factor(loan_in_dept) ~ loan_duration_in_month + loan_per_year + age_dec1999 + owner_sex + income_avg + outgoes_avg, 
                   data = loan_train,
                   na.action = na.omit,
                   importance = TRUE,
                   ntree = 150)

rf_model_150
```

    ## 
    ## Call:
    ##  randomForest(formula = as.factor(loan_in_dept) ~ loan_duration_in_month +      loan_per_year + age_dec1999 + owner_sex + income_avg + outgoes_avg,      data = loan_train, importance = TRUE, ntree = 150, na.action = na.omit) 
    ##                Type of random forest: classification
    ##                      Number of trees: 150
    ## No. of variables tried at each split: 2
    ## 
    ##         OOB estimate of  error rate: 3.41%
    ## Confusion matrix:
    ##      No Yes class.error
    ## No  452  31 0.064182195
    ## Yes   2 484 0.004115226

``` r
# Output the total error rate
plot(rf_model_150)
```

![](notebook_files/figure-gfm/unnamed-chunk-293-1.png)<!-- -->

Das Summary zeigt verschiedenen Angaben des Modells wie Art des Modells,
Anzahl Trees (von uns auf 150 gesetzt), sowie die OOB estimate of error
rate in Prozent. OOB steht für Out-Of-Bag Samples. OOB Samples sind
solche welche nicht im Bootstraped Dataset aufgenommen wurden.

Genauigkeit des Modells mit predict-Funktion und Confusion-Matrix and
den test Daten bestimmen

``` r
rf_pred_test <- predict(rf_model_150, loan_test)

# Creating Confusion Matrix
rf_confusion_matrix_test  <- table(loan_test$loan_in_dept, rf_pred_test)
rf_confusion_matrix_test
```

    ##      rf_pred_test
    ##        No Yes
    ##   No  114   9
    ##   Yes   0 120

``` r
# Calculating accuracy 
rf_accuracy_test <- sum(diag(rf_confusion_matrix_test)) / sum(rf_confusion_matrix_test)


paste("Genauigkeit des Modells: ", round((rf_accuracy_test * 100), digits = 2), "%" )
```

    ## [1] "Genauigkeit des Modells:  96.3 %"

Mithilfe der Confusion Matrix können wir unser Klassifikationsmodell
evaluieren. Die zweidimensionale Matrix stellt die vorhergesagten
Klassen steht sowie die tatsächlichen Klassen dar. Daraus kann abgelesen
werden, dass das Modell einen grossen Teil richtig klassifiziert hat.

Anwenden des Modells auf den Gesamten Datensatz

``` r
rf_pred_all <- predict(rf_model_150, df_cons_predict)

# Creating Confusion Matrix
rf_confusion_matrix_all  <- table(df_cons_predict$loan_in_dept, rf_pred_all)
rf_confusion_matrix_all
```

    ##      rf_pred_all
    ##        No Yes
    ##   No  596  10
    ##   Yes   0 606

``` r
# Calculating accuracy 
rf_accuracy_all <- sum(diag(rf_confusion_matrix_all)) / sum(rf_confusion_matrix_all)



paste("Genauigkeit des Modells: ", round((rf_accuracy_all * 100), digits = 2), "%" )
```

    ## [1] "Genauigkeit des Modells:  99.17 %"

Wichtigkeit der Variablen (Importance)

Neben der Bewertung der Gesamtleistung des Modells ist es auch nützlich,
die relative Bedeutung der verschiedenen Eingabevariablen für die
Vorhersagen des Modells zu untersuchen. Indem wir die Verbesserung mit
den Trainingsiterationen untersuchen, können wir auch Einblicke in den
Lernprozess des Modells gewinnen.

Das angegebene Maß für die relative Wichtigkeit ist der Gesamtrückgang
der Knotenverunreinigungen durch die Aufteilung auf diese Variable,
gemittelt über alle Bäume.  

``` r
# Calculate  importance of Model
importance(rf_model_150, type = 2)
```

    ##                        MeanDecreaseGini
    ## loan_duration_in_month         31.66852
    ## loan_per_year                 155.03698
    ## age_dec1999                    73.66312
    ## owner_sex                      14.35168
    ## income_avg                     97.12963
    ## outgoes_avg                   109.77285

In unseren Modellen ist die wichtigste Variable loan_per_year und die
unwichtigste ist owner_sex.

Alternativ kann die importance auch in einem Variable Importance Plot
dargestellt werden

``` r
varImpPlot(rf_model_150, pch = 20)
```

![](notebook_files/figure-gfm/unnamed-chunk-297-1.png)<!-- -->

Der Variable Importance Plot ist ein grundlegendes Ergebnis des Random
Forest und zeigt für jede Variable, wie wichtig sie für die
Klassifizierung der Daten ist.  
Die Grafik “Mean Decrease Accuracy” drückt aus, wie viel Genauigkeit das
Modell durch den Ausschluss jeder Variable verliert. Je mehr die
Genauigkeit leidet, desto wichtiger ist die Variable für eine
erfolgreiche Klassifizierung. Die Variablen werden in absteigender
Reihenfolge ihrer Bedeutung dargestellt.  
Der “Mean Decrease Gini” Koeffizient ist ein Maß dafür, wie jede
Variable zur Homogenität der Knoten und Blätter im Random Forest
beiträgt. Ein Knoten wird als “reiner” betrachtet, wenn er nur Instanzen
einer Klasse enthält. Ein Knoten ist umso reiner, je niedriger der
Gini-Koeffizient ist. Der Gini-Koeffizient berechnet sich aus der Summe
der Wahrscheinlichkeiten, dass eine zufällig ausgewählte Instanz in
einem Knoten der Klasse i zugeordnet ist, multipliziert mit der
Wahrscheinlichkeit, dass eine zufällig ausgewählte Instanz nicht der
Klasse i zugeordnet ist. Für Entscheidungsbaum-Algorithmen, welche die
Gini-Impurity als Auswahlkriterium verwenden, werden bei jeder Stufe des
Baumes die Merkmale ausgewählt, die dazu beitragen, die Gini-Impurity zu
minimieren, also die Knotenreinheit zu maximieren, so das sich in jedem
Knoten eine Klasse bildet.  

Quelle: Von der Hude, M. (2020). *Predictive Analytics und Data Mining*
(S. 137 -166). Springer

#### Wie sieht das Verhältnis der der Verwendung von verschiedenen Zahlungsmittel in Abhängigkeit der Zeit aus?[^79]

``` r
df_transaction_complete %>%
  filter(operation %in% c("CASH CREDIT", "CASH WIDTHDRAWAL", "COLLECTION OTHER BANK", "CREDIT CARD WITHDRAWAL", "REMITTANCE OTHER BANK")) %>%
  ggplot(mapping = aes(x = date, fill = operation)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~ operation, ncol = 2, scales = "free_y") +
  labs(title = "Overwiev distribution of Transaction methods used from 1993 to 1999 ", 
       subtitle = "Y-axis set independently",
       caption = "Source: PKDD’99 Discovery Challenge",
       fill = "Method of Transaction" ) +
  xlab("Years") +
  ylab("Count") +
  theme(legend.position = "none") +
  scale_x_date(date_labels="%y",date_breaks ="1 year", date_minor_breaks = "1 month") +
  theme(axis.text.x = element_text(size = 8, angle=90,vjust =0.2))
```

![](notebook_files/figure-gfm/unnamed-chunk-298-1.png)<!-- -->

Der Facet-plot zeigt eine Übersicht der verwendeten Transaktionsmethoden
von 1993 bis 1999. In der Grafik wird ersichtlich, dass alle
Transaktionen kontinuierlich zugenommen haben und einem monatlichen oder
Jährlichen Zyklus folgen. Der Rückgang der Bezüge im Bereich “Credit
Card Withdrawal” und “Cash Withdrawal” im Jahr 1999 ist damit zu
begründen, dass wahrscheinlich noch weitere Bezüge nach dem
Jahreswechsel getätigt wurden, da die gespeicherten Daten aber nur bis
zum 31.12.1999 reichen, wurden diese Bezüge nicht aufgezeichnet.

#### Analyse der Kundengruppe welche eine Gold-Kreditkarte verwendet[^80]

``` r
# Overview of Card tpyes used
df_cons %>%
  group_by(card_type) %>%
  ggplot(aes(x = card_type, fill = card_type)) +
  geom_bar() +
  geom_text(aes(label=..count..), vjust = "bottom", stat = 'count') +
  labs(title = "Distribution of Credit Card Types", 
       subtitle = "",
       caption = "Source: PKDD’99 Discovery Challenge",
       fill = "Type of Credit Card" ) +
  xlab("Type of Credit Card") +
  ylab("Number of Users") +
  theme(legend.position = "none")
```

![](notebook_files/figure-gfm/unnamed-chunk-299-1.png)<!-- -->

Das Balkendiagramm zeigt die Verteilung der verschiedenen Kreditkarten
Typen. 3608 der insgesamt 4500 Kunden besitzen keine Kreditkarte oder
sie ist nicht angegeben. Von den Kreditkartenbesitzern besitzt der
grösste Teil eine “Classic” Karte (659 Kunden). Insgesamt 145 Kunden
besitzen eine “Junior” Karte. Diese ist nur für Kunden unter 25 Jahren
verfügbar. Die restlichen Kunden besitzen eine “Gold” Kreditkarte. Da
diese Kundengruppe für die Bank interessant ist, möchten wir sie im
nächsten Abschnitt näher untersuchen.

``` r
# Summarize all Cusotmers with Gold Card and list Attributes
gold_card <- df_cons %>%
  filter(card_type == "GOLD")
  
# Create Picture of average Gold Card user
cardplot1 <- gold_card %>%
  ggplot(gold_card, mapping = aes(x = age_dec1999, color = age_dec1999)) +
  geom_boxplot(fill = "#CE76FC") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        plot.background = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(title = "Age of Cardholder", 
           subtitle = "", 
           caption = "",
           y = "", x = "Age") +
  coord_flip()

cardplot4 <- gold_card %>%
  ggplot(gold_card, mapping = aes(x = owner_sex, fill = owner_sex)) +
  geom_bar() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        plot.background = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(title = "Distribution of Gender ", 
           subtitle = "", 
           caption = "Source: PKDD’99 Discovery Challenge",
           y = "Count", x = "")
 
cardplot2 <- gold_card %>%
  ggplot(gold_card, mapping = aes(x = income_avg / 12)) +
  geom_boxplot(fill = "#FC7870") +
  xlim(0,100000) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        plot.background = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(title = "Monthly income of Cardholder", 
           subtitle = "", 
           caption = "",
           y = "", x = "Income [CZK]") +
  coord_flip()

cardplot3 <- gold_card %>%
  ggplot(gold_card, mapping = aes(x = outgoes_avg / 12, fill = age_dec1999)) +
  geom_boxplot(fill = "#FF64B0") +
  xlim(0,100000) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        plot.background = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(title = "Monthly expenses by cardholder", 
           subtitle = "", 
           caption = "",
           y = "", x = "Expenses [CZK]") +
  coord_flip()

cardplot5 <- gold_card %>%
  group_by(account_district_region) %>%
  mutate(count_region =n()) %>%
  ggplot(gold_card, mapping = aes(x = reorder(account_district_region, -count_region), fill = account_district_region)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 10, angle = 60, vjust = 0.9, hjust = 0.9),
        legend.position = "none",
        plot.background = element_rect(colour = "black", fill = NA, size = 0.5)) +
  labs(title = "Region of Residence", 
           subtitle = "", 
           caption = "Source: PKDD’99 Discovery Challenge",
           y = "Count", x = "")

nested_cardplot <- (cardplot1|cardplot2|cardplot3|cardplot4/(cardplot5)) 
nested_cardplot
```

![](notebook_files/figure-gfm/unnamed-chunk-300-1.png)<!-- -->

Die Abbildung oben zeigt fünf verschiedene Plots, welche alle Attribute
von Kunden mit Gold-Status Kreditkarten aufzeigen. Ziel ist es
aufzuzeigen, welche Durchschnittliche Eigenschaften eine solcher Kunde
aufweist, um herauszufinden welche weiteren Kunden für diese spezielle
Kreditkarte in Frage kommen.

### Korrelationsanalyse zwischen den Bankdaten und den

soziodemografischen Daten.[^81]

#### Distribution von Kunden in den Distrikten im Bezug

zu den restlichen Einwohnern

``` r
#create new df
df_owner_over_district_size <- df_cons %>% 
  #select the wanted Informations Age, district, sex for both owner and user
  select(
    account_district_inhabitants,
    account_district_name)

#count ammount of accountsi per district
new_version_count <- df_owner_over_district_size %>% count(account_district_name) %>% 
  arrange(desc(n))

#remove doublicates
df_owner_over_district_size <- df_owner_over_district_size %>% 
  distinct(account_district_inhabitants, .keep_all = TRUE) %>% 
  arrange(desc(account_district_inhabitants))

#put the two dftogether to get the right variables
combined_owner_over_district_size <- full_join(df_owner_over_district_size, new_version_count, by = "account_district_name" ) %>% 
  distinct(account_district_inhabitants, .keep_all = TRUE) %>% 
  relocate(n, .after = account_district_inhabitants) %>% 
  arrange(desc(account_district_inhabitants))

#check data
sum(combined_owner_over_district_size$n)
```

    ## [1] 4500

``` r
#calculate percentage of accountowner in every district
combined_owner_over_district_size <- combined_owner_over_district_size %>% 
  mutate(percentage_of_district_own_account = n/account_district_inhabitants*100)


#show the districts with the lowest percentage
head(combined_owner_over_district_size %>% arrange(percentage_of_district_own_account),30)
```

    ##    account_district_inhabitants   n account_district_name
    ## 1                        177686  41      Ceske Budejovice
    ## 2                        161954  41               Hodonin
    ## 3                        161227  42            Novy Jicin
    ## 4                        170449  45         Plzen - mesto
    ## 5                        148545  40                Vsetin
    ## 6                        109164  32               Jihlava
    ## 7                        145688  44      Uherske Hradiste
    ## 8                        161854  49        Hradec Kralove
    ## 9                        182027  56                 Opava
    ## 10                       125236  39              Chomutov
    ## 11                       149893  48                Kladno
    ## 12                       114006  37            Litomerice
    ## 13                       162580  53             Pardubice
    ## 14                       387570 128          Brno - mesto
    ## 15                       157042  53         Brno - venkov
    ## 16                       114200  39                Znojmo
    ## 17                       119895  41                  Most
    ## 18                       122603  42          Karlovy Vary
    ## 19                       128118  44               Teplice
    ## 20                       124605  44               Breclav
    ## 21                       159617  57               Liberec
    ## 22                       228848  83       Frydek - Mistek
    ## 23                       133777  49                 Decin
    ## 24                       138032  51                Prerov
    ## 25                       118650  45        Usti nad Labem
    ## 26                       117897  45                Trebic
    ## 27                       226122  88               Olomouc
    ## 28                       107870  42               Pribram
    ## 29                        94812  38               Sokolov
    ## 30                       121947  49               Trutnov
    ##    percentage_of_district_own_account
    ## 1                          0.02307441
    ## 2                          0.02531583
    ## 3                          0.02605023
    ## 4                          0.02640086
    ## 5                          0.02692787
    ## 6                          0.02931369
    ## 7                          0.03020153
    ## 8                          0.03027420
    ## 9                          0.03076467
    ## 10                         0.03114121
    ## 11                         0.03202284
    ## 12                         0.03245443
    ## 13                         0.03259934
    ## 14                         0.03302629
    ## 15                         0.03374893
    ## 16                         0.03415061
    ## 17                         0.03419659
    ## 18                         0.03425691
    ## 19                         0.03434334
    ## 20                         0.03531158
    ## 21                         0.03571048
    ## 22                         0.03626861
    ## 23                         0.03662812
    ## 24                         0.03694795
    ## 25                         0.03792668
    ## 26                         0.03816891
    ## 27                         0.03891704
    ## 28                         0.03893576
    ## 29                         0.04007931
    ## 30                         0.04018139

``` r
#show the districts with highest percentage
head(combined_owner_over_district_size %>% arrange(desc(percentage_of_district_own_account)),30)
```

    ##    account_district_inhabitants   n account_district_name
    ## 1                         42821  48               Jesenik
    ## 2                         51428  55            Prachatice
    ## 3                         51313  50                Tachov
    ## 4                         45714  43              Rokycany
    ## 5                         58796  50         Cesky Krumlov
    ## 6                         53921  37              Rakovnik
    ## 7                         95616  65                 Kolin
    ## 8                         78955  53   Rychnov nad Kneznou
    ## 9                         75232  50                Beroun
    ## 10                        70699  46                 Pisek
    ## 11                        75637  48         Praha - zapad
    ## 12                        85852  54                 Louny
    ## 13                        72541  45         Plzen - sever
    ## 14                        58400  36             Domazlice
    ## 15                        77963  48            Kutna Hora
    ## 16                        67298  39           Plzen - jih
    ## 17                        87419  50                  Cheb
    ## 18                        74062  42             Pelhrimov
    ## 19                        86513  49                Vyskov
    ## 20                        81344  46               Nymburk
    ## 21                        93931  52     Jindrichuv Hradec
    ## 22                       285387 152               Karvina
    ## 23                        77917  41                 Jicin
    ## 24                       108871  57              Kromeriz
    ## 25                       112709  59                Nachod
    ## 26                        92084  48        Praha - vychod
    ## 27                        70646  36            Strakonice
    ## 28                        95907  48        Havlickuv Brod
    ## 29                       112065  56        Mlada Boleslav
    ## 30                        75685  37                Semily
    ##    percentage_of_district_own_account
    ## 1                          0.11209453
    ## 2                          0.10694563
    ## 3                          0.09744119
    ## 4                          0.09406309
    ## 5                          0.08503980
    ## 6                          0.06861891
    ## 7                          0.06798025
    ## 8                          0.06712684
    ## 9                          0.06646108
    ## 10                         0.06506457
    ## 11                         0.06346100
    ## 12                         0.06289894
    ## 13                         0.06203388
    ## 14                         0.06164384
    ## 15                         0.06156767
    ## 16                         0.05795120
    ## 17                         0.05719580
    ## 18                         0.05670924
    ## 19                         0.05663889
    ## 20                         0.05654996
    ## 21                         0.05535979
    ## 22                         0.05326101
    ## 23                         0.05262010
    ## 24                         0.05235554
    ## 25                         0.05234719
    ## 26                         0.05212632
    ## 27                         0.05095830
    ## 28                         0.05004848
    ## 29                         0.04997100
    ## 30                         0.04888683

Wir sehen In den grösseren Distrikten ist gibt es Prozentual weniger
Kunden als in den kleinen. Als direkter Vergleich: Ceske Budejovice der
Bezirk mit dem kleinsten Anteil an Kontoinhabern hat eine Einwohnerzahl
von Insgesamt 177’686. Und der Bezirk mit dem grössten Anteil hat eine
Grösse von 42’821 Einwohnern. Ein Ansatz für die Bank ist, die Präsenz
in den Distrikten mit niedrigem Prozentsatz ihre Präsenz zu verstärken.
Um somit mehr Kunden in diesen Bezirken zu generieren. Zusätzlich kann
bei bestehenden Kunden ein Profitprogramm Eingeführt werden um neue
Kunden zu akquirieren Damit ist gemeint, dass bestehende Kunden durch
Akquisition gewisse Vorteile erhalten können.

#### Entwicklung des Einkommens im Bezug zu begangenen Straftaten[^82]

Für diese Aufgabe beschränken wir uns auf die Jahre 1995 und 1996, da
und nur in diesen Jahren die Arbeitslosenrate und Kriminalität zur
Verfügung steht.

``` r
#prepare new df to join with df_district_mod
district_new_name <- df_district %>% 
  rename(account_district_name = name)


#join dfcons with district_new_names
df_crimes_cons <- inner_join(df_cons, district_new_name, by = "account_district_name") %>% 
  filter(unemployment_rate_95 != "NA",
         unemployment_rate_96 != "NA",
         commited_crimes_95 != "NA",
         commited_crimes_96 != "NA",
         salary_groups != "NA")

 
#check data district number comparison
head(df_crimes_cons)
```

    ##   account_id account_frequency account_opening_date account_opening_year
    ## 1          1           Monthly           1995-03-24                 1995
    ## 2          2           Monthly           1993-02-26                 1993
    ## 3          3           Monthly           1997-07-07                 1997
    ## 4          4           Monthly           1996-02-21                 1996
    ## 5          5           Monthly           1997-05-30                 1997
    ## 6          6           Monthly           1994-09-27                 1994
    ##   account_opening_month account_num_of_user account_district_name
    ## 1                     3                   1                 Pisek
    ## 2                     2                   2           Hl.m. Praha
    ## 3                     7                   2                 Kolin
    ## 4                     2                   1               Pribram
    ## 5                     5                   1         Cesky Krumlov
    ## 6                     9                   1               Trutnov
    ##   account_district_region account_district_average_salary
    ## 1           south Bohemia                            8968
    ## 2                  Prague                           12541
    ## 3         central Bohemia                            9307
    ## 4         central Bohemia                            8754
    ## 5           south Bohemia                            9045
    ## 6            east Bohemia                            8541
    ##   account_district_inhabitants order_total_amount_household
    ## 1                        70699                         2452
    ## 2                      1204953                         7266
    ## 3                        95616                         1135
    ## 4                       107870                         3363
    ## 5                        58796                         2668
    ## 6                       121947                         3954
    ##   order_total_amount_loan order_total_amount_insurrance
    ## 1                     0.0                             0
    ## 2                  3372.7                             0
    ## 3                     0.0                          3539
    ## 4                     0.0                             0
    ## 5                     0.0                             0
    ## 6                     0.0                             0
    ##   order_total_amount_unknown order_total_amount_leasing order_num_household
    ## 1                          0                          0                   1
    ## 2                          0                          0                   1
    ## 3                        327                          0                   1
    ## 4                          0                          0                   2
    ## 5                          0                          0                   1
    ## 6                          0                          0                   1
    ##   order_num_loan order_num_insurrance order_num_unknown order_num_leasing
    ## 1              0                    0                 0                 0
    ## 2              1                    0                 0                 0
    ## 3              0                    1                 1                 0
    ## 4              0                    0                 0                 0
    ## 5              0                    0                 0                 0
    ## 6              0                    0                 0                 0
    ##   loan_start_date loan_end_date loan_duration_in_month loan_duration_in_years
    ## 1            <NA>          <NA>                     NA                     NA
    ## 2      1994-01-05    1996-01-05                     24                      2
    ## 3            <NA>          <NA>                     NA                     NA
    ## 4            <NA>          <NA>                     NA                     NA
    ## 5            <NA>          <NA>                     NA                     NA
    ## 6            <NA>          <NA>                     NA                     NA
    ##   loan_total_amount loan_redemption_amount loan_per_year
    ## 1                NA                     NA            NA
    ## 2             80952                   3373         40476
    ## 3                NA                     NA            NA
    ## 4                NA                     NA            NA
    ## 5                NA                     NA            NA
    ## 6                NA                     NA            NA
    ##               loan_status loan_in_dept card_type card_issued age_at_loan
    ## 1                    <NA>           NA      <NA>        <NA>          NA
    ## 2 CONTRACT FINISHED PAYED        FALSE      <NA>        <NA>          48
    ## 3                    <NA>           NA      <NA>        <NA>          NA
    ## 4                    <NA>           NA      <NA>        <NA>          NA
    ## 5                    <NA>           NA      <NA>        <NA>          NA
    ## 6                    <NA>           NA      <NA>        <NA>          NA
    ##   owner_dateofbirth age_dec1999 owner_sex owner_age_at_account_opening
    ## 1        1970-12-13          29    Female                           24
    ## 2        1945-02-04          54      Male                           48
    ## 3        1956-12-01          43      Male                           40
    ## 4        1919-09-22          80      Male                           76
    ## 5        1929-01-25          70      Male                           68
    ## 6        1938-02-21          61    Female                           56
    ##   owner_district_name owner_district_region owner_district_average_salary
    ## 1               Pisek         south Bohemia                          8968
    ## 2         Hl.m. Praha                Prague                         12541
    ## 3               Kolin       central Bohemia                          9307
    ## 4             Pribram       central Bohemia                          8754
    ## 5       Cesky Krumlov         south Bohemia                          9045
    ## 6             Trutnov          east Bohemia                          8541
    ##   owner_district_inhabitants user_dateofbirth user_sex
    ## 1                      70699             <NA>     <NA>
    ## 2                    1204953       1940-10-09   Female
    ## 3                      95616       1960-07-03   Female
    ## 4                     107870             <NA>     <NA>
    ## 5                      58796             <NA>     <NA>
    ## 6                     121947             <NA>     <NA>
    ##   user_age_at_account_opening account_district_id loan_id card_id
    ## 1                          NA                  18      NA      NA
    ## 2                          52                   1    4959      NA
    ## 3                          37                   5      NA      NA
    ## 4                          NA                  12      NA      NA
    ## 5                          NA                  15      NA      NA
    ## 6                          NA                  51      NA      NA
    ##   owner_client_id owner_disp_id owner_district_id user_client_id user_disp_id
    ## 1               1             1                18             NA           NA
    ## 2               2             2                 1              3            3
    ## 3               4             4                 5              5            5
    ## 4               6             6                12             NA           NA
    ## 5               7             7                15             NA           NA
    ## 6               8             8                51             NA           NA
    ##   user_district_id balance_1993-01-31 balance_1993-02-28 balance_1993-03-31
    ## 1               NA                  0                  0                0.0
    ## 2                1                  0               1100            25049.5
    ## 3                5                  0                  0                0.0
    ## 4               NA                  0                  0                0.0
    ## 5               NA                  0                  0                0.0
    ## 6               NA                  0                  0                0.0
    ##   balance_1993-04-30 balance_1993-05-31 balance_1993-06-30 balance_1993-07-31
    ## 1                0.0                0.0                0.0                0.0
    ## 2            34394.9            37175.6            45289.5            39068.9
    ## 3                0.0                0.0                0.0                0.0
    ## 4                0.0                0.0                0.0                0.0
    ## 5                0.0                0.0                0.0                0.0
    ## 6                0.0                0.0                0.0                0.0
    ##   balance_1993-08-31 balance_1993-09-30 balance_1993-10-31 balance_1993-11-30
    ## 1                0.0                  0                0.0                0.0
    ## 2            26991.8              21774            27830.4            31824.9
    ## 3                0.0                  0                0.0                0.0
    ## 4                0.0                  0                0.0                0.0
    ## 5                0.0                  0                0.0                0.0
    ## 6                0.0                  0                0.0                0.0
    ##   balance_1993-12-31 balance_1994-01-31 balance_1994-02-28 balance_1994-03-31
    ## 1                0.0                0.0                0.0                0.0
    ## 2            27840.6            20038.7            26229.6            31041.5
    ## 3                0.0                0.0                0.0                0.0
    ## 4                0.0                0.0                0.0                0.0
    ## 5                0.0                0.0                0.0                0.0
    ## 6                0.0                0.0                0.0                0.0
    ##   balance_1994-04-30 balance_1994-05-31 balance_1994-06-30 balance_1994-07-31
    ## 1                0.0                0.0                  0                0.0
    ## 2            33174.2            20293.4              29320            30924.1
    ## 3                0.0                0.0                  0                0.0
    ## 4                0.0                0.0                  0                0.0
    ## 5                0.0                0.0                  0                0.0
    ## 6                0.0                0.0                  0                0.0
    ##   balance_1994-08-31 balance_1994-09-30 balance_1994-10-31 balance_1994-11-30
    ## 1                0.0                0.0                0.0                0.0
    ## 2            32849.5            25730.3            32043.2            31064.2
    ## 3                0.0                0.0                0.0                0.0
    ## 4                0.0                0.0                0.0                0.0
    ## 5                0.0                0.0                0.0                0.0
    ## 6                0.0              900.0             7569.0            14238.0
    ##   balance_1994-12-31 balance_1995-01-31 balance_1995-02-28 balance_1995-03-31
    ## 1                0.0                0.0                0.0             1000.0
    ## 2            32913.6            20414.8            28107.6            33625.1
    ## 3                0.0                0.0                0.0                0.0
    ## 4                0.0                0.0                0.0                0.0
    ## 5                0.0                0.0                0.0                0.0
    ## 6            20907.0            27576.0            23866.7            26673.8
    ##   balance_1995-04-30 balance_1995-05-31 balance_1995-06-30 balance_1995-07-31
    ## 1            17298.2            23156.2            26735.8            25223.7
    ## 2            25928.2            23033.1            36605.0            37949.4
    ## 3                0.0                0.0                0.0                0.0
    ## 4                0.0                0.0                0.0                0.0
    ## 5                0.0                0.0                0.0                0.0
    ## 6            29492.5            28219.3            27134.5            29955.3
    ##   balance_1995-08-31 balance_1995-09-30 balance_1995-10-31 balance_1995-11-30
    ## 1            21487.3            22091.2            20494.1            19021.6
    ## 2            21281.9            29374.3            26842.9            35058.9
    ## 3                0.0                0.0                0.0                0.0
    ## 4                0.0                0.0                0.0                0.0
    ## 5                0.0                0.0                0.0                0.0
    ## 6            31085.7            33722.5            32569.2            32365.2
    ##   balance_1995-12-31 balance_1996-01-31 balance_1996-02-29 balance_1996-03-31
    ## 1            18173.0            13579.5            14951.6            16429.8
    ## 2            39422.1            22777.6            38168.4            38710.1
    ## 3                0.0                0.0                0.0                0.0
    ## 4                0.0                0.0              800.0             6353.0
    ## 5                0.0                0.0                0.0                0.0
    ## 6            33804.7            26064.5            25594.8            27848.3
    ##   balance_1996-04-30 balance_1996-05-31 balance_1996-06-30 balance_1996-07-31
    ## 1            15709.7            14425.4            16743.4            18038.5
    ## 2            22887.6            24191.8            52030.1            35141.7
    ## 3                0.0                0.0                0.0                0.0
    ## 4            11906.0            17459.0            23012.0            25432.0
    ## 5                0.0                0.0                0.0                0.0
    ## 6            29148.4            31977.3            23186.6            25990.6
    ##   balance_1996-08-31 balance_1996-09-30 balance_1996-10-31 balance_1996-11-30
    ## 1            19176.7            18767.8            16783.5            15268.3
    ## 2            44765.0            32639.9            45566.7            49514.0
    ## 3                0.0                0.0                0.0                0.0
    ## 4            23612.7            22587.0            24863.0            27148.5
    ## 5                0.0                0.0                0.0                0.0
    ## 6            28806.3            31633.6            32791.1            35634.9
    ##   balance_1996-12-31 balance_1997-01-31 balance_1997-02-28 balance_1997-03-31
    ## 1            12659.9            12817.7            14588.3            11252.6
    ## 2            53390.9            29803.1            39009.4            45213.2
    ## 3                0.0                0.0                0.0                0.0
    ## 4            24943.0            15977.2            18225.4            18719.5
    ## 5                0.0                0.0                0.0                0.0
    ## 6            33509.2            23601.0            26406.0            29223.2
    ##   balance_1997-04-30 balance_1997-05-31 balance_1997-06-30 balance_1997-07-31
    ## 1            12514.6            12611.1            13878.7            16655.2
    ## 2            53469.9            57214.0            27445.6            48172.9
    ## 3                0.0                0.0                0.0             8526.0
    ## 4            20979.2            20927.0            17926.3            20182.8
    ## 5                0.0              600.0             5617.0            10634.0
    ## 6            32051.8            34892.4            34731.0            33815.0
    ##   balance_1997-08-31 balance_1997-09-30 balance_1997-10-31 balance_1997-11-30
    ## 1            15864.7            16752.0            18036.8            18221.7
    ## 2            43745.6            52092.3            51644.9            51794.9
    ## 3            23906.7            29122.3            29847.2            30059.1
    ## 4            19441.5            19803.9            20764.9            22389.3
    ## 5            15651.0            20668.0            25673.8            28121.1
    ## 6            35540.5            38395.2            41261.9            44140.3
    ##   balance_1997-12-31 balance_1998-01-31 balance_1998-02-28 balance_1998-03-31
    ## 1            14795.7            13174.0            14942.6            15408.1
    ## 2            53875.6            31781.7            40876.4            29057.3
    ## 3            26749.4            12394.5            15137.8            23312.0
    ## 4            19876.7            13216.1            15452.5            17698.5
    ## 5            27561.0            22076.0            24508.0            26149.9
    ## 6            45988.4            35809.1            38664.3            39286.3
    ##   balance_1998-04-30 balance_1998-05-31 balance_1998-06-30 balance_1998-07-31
    ## 1            15450.4            16729.5            17563.4            18851.2
    ## 2            40558.4            36767.7            48541.6            50313.9
    ## 3            23780.0            25567.8            27511.8            31121.4
    ## 4            19151.8            21413.2            22399.0            22831.9
    ## 5            26397.2            26445.4            28095.0            26448.0
    ## 6            42156.1            42794.1            35585.7            37238.4
    ##   balance_1998-08-31 balance_1998-09-30 balance_1998-10-31 balance_1998-11-30
    ## 1            19870.8            14641.9            15917.4            16686.9
    ## 2            42865.7            44015.5            46065.1            36583.3
    ## 3            34044.3            38577.8            42730.5            47194.5
    ## 4            25108.5            27394.4            29690.0            31995.0
    ## 5            23792.4            24889.3            27333.3            29687.2
    ## 6            40099.4            42972.2            42328.0            45209.9
    ##   balance_1998-12-31 incomes_quarterly_1993.1 incomes_quarterly_1993.2
    ## 1            13466.5                      0.0                      0.0
    ## 2            42628.1                  25049.5                  71240.1
    ## 3            51096.1                      0.0                      0.0
    ## 4            33720.7                      0.0                      0.0
    ## 5            28088.3                      0.0                      0.0
    ## 6            47668.0                      0.0                      0.0
    ##   incomes_quarterly_1993.3 incomes_quarterly_1993.4 incomes_quarterly_1994.1
    ## 1                      0.0                      0.0                      0.0
    ## 2                  61191.3                  71204.4                  61060.8
    ## 3                      0.0                      0.0                      0.0
    ## 4                      0.0                      0.0                      0.0
    ## 5                      0.0                      0.0                      0.0
    ## 6                      0.0                      0.0                      0.0
    ##   incomes_quarterly_1994.2 incomes_quarterly_1994.3 incomes_quarterly_1994.4
    ## 1                      0.0                      0.0                      0.0
    ## 2                  71238.2                  61111.1                  71243.1
    ## 3                      0.0                      0.0                      0.0
    ## 4                      0.0                      0.0                      0.0
    ## 5                      0.0                      0.0                      0.0
    ## 6                      0.0                    900.0                  20007.0
    ##   incomes_quarterly_1995.1 incomes_quarterly_1995.2 incomes_quarterly_1995.3
    ## 1                   1000.0                  25935.8                  11336.6
    ## 2                  61771.3                  71237.7                  61109.2
    ## 3                      0.0                      0.0                      0.0
    ## 4                      0.0                      0.0                      0.0
    ## 5                      0.0                      0.0                      0.0
    ## 6                  20184.0                  20366.4                  20393.8
    ##   incomes_quarterly_1995.4 incomes_quarterly_1996.1 incomes_quarterly_1996.2
    ## 1                  11291.7                  11526.5                  12633.4
    ## 2                  71243.6                  67129.8                  85483.8
    ## 3                      0.0                      0.0                      0.0
    ## 4                      0.0                   6353.0                  16659.0
    ## 5                      0.0                      0.0                      0.0
    ## 6                  20428.1                  20359.3                  20364.1
    ##   incomes_quarterly_1996.3 incomes_quarterly_1996.4 incomes_quarterly_1997.1
    ## 1                  14664.3                  11241.8                  14102.6
    ## 2                  61236.5                  74692.8                  67288.1
    ## 3                      0.0                      0.0                      0.0
    ## 4                  16929.8                  16978.7                  16899.2
    ## 5                      0.0                      0.0                      0.0
    ## 6                  20352.8                  20431.5                  20349.8
    ##   incomes_quarterly_1997.2 incomes_quarterly_1997.3 incomes_quarterly_1997.4
    ## 1                  11195.9                  12733.2                  11243.4
    ## 2                  71410.3                  71454.5                  71425.1
    ## 3                      0.0                  31522.4                  26617.3
    ## 4                  16909.7                  16910.3                  16925.6
    ## 5                   5617.0                  15051.0                  15282.9
    ## 6                  20423.6                  20450.0                  20538.9
    ##   incomes_quarterly_1998.1 incomes_quarterly_1998.2 incomes_quarterly_1998.3
    ## 1                  11712.2                  11235.1                  11248.2
    ## 2                  62442.6                  73126.2                  61313.8
    ## 3                  28529.4                  27566.7                  26312.8
    ## 4                  16854.6                  16913.4                  16968.3
    ## 5                  15356.8                  15393.0                  15382.1
    ## 6                  20493.7                  20525.1                  20492.4
    ##   incomes_quarterly_1998.4 outgoes_quarterly_1993.1 outgoes_quarterly_1993.2
    ## 1                  11219.8                        0                        0
    ## 2                  71349.7                        0                    51000
    ## 3                  32510.4                        0                        0
    ## 4                  17044.4                        0                        0
    ## 5                  15402.2                        0                        0
    ## 6                  20557.0                        0                        0
    ##   outgoes_quarterly_1993.3 outgoes_quarterly_1993.4 outgoes_quarterly_1994.1
    ## 1                      0.0                      0.0                      0.0
    ## 2                  84706.8                  65137.8                  57859.9
    ## 3                      0.0                      0.0                      0.0
    ## 4                      0.0                      0.0                      0.0
    ## 5                      0.0                      0.0                      0.0
    ## 6                      0.0                      0.0                      0.0
    ##   outgoes_quarterly_1994.2 outgoes_quarterly_1994.3 outgoes_quarterly_1994.4
    ## 1                      0.0                      0.0                      0.0
    ## 2                  72959.9                  64700.9                  64059.9
    ## 3                      0.0                      0.0                      0.0
    ## 4                      0.0                      0.0                      0.0
    ## 5                      0.0                      0.0                      0.0
    ## 6                      0.0                      0.0                      0.0
    ##   outgoes_quarterly_1995.1 outgoes_quarterly_1995.2 outgoes_quarterly_1995.3
    ## 1                      0.0                    200.0                  15981.2
    ## 2                  61059.9                  68257.9                  68339.9
    ## 3                      0.0                      0.0                      0.0
    ## 4                      0.0                      0.0                      0.0
    ## 5                      0.0                      0.0                      0.0
    ## 6                  14417.2                  19905.8                  13805.8
    ##   outgoes_quarterly_1995.4 outgoes_quarterly_1996.1 outgoes_quarterly_1996.2
    ## 1                  15209.8                  13269.8                  12319.8
    ## 2                  61195.9                  67841.8                  72163.8
    ## 3                      0.0                      0.0                      0.0
    ## 4                      0.0                      0.0                      0.0
    ## 5                      0.0                      0.0                      0.0
    ## 6                  20345.8                  26315.8                  25025.8
    ##   outgoes_quarterly_1996.3 outgoes_quarterly_1996.4 outgoes_quarterly_1997.1
    ## 1                  12639.8                  17349.8                  15509.8
    ## 2                  80626.8                  53941.8                  75465.8
    ## 3                      0.0                      0.0                      0.0
    ## 4                  17354.8                  14622.8                  23122.8
    ## 5                      0.0                      0.0                      0.0
    ## 6                  11905.8                  18555.8                  24635.8
    ##   outgoes_quarterly_1997.2 outgoes_quarterly_1997.3 outgoes_quarterly_1997.4
    ## 1                   8569.8                   9859.8                  13199.8
    ## 2                  89177.8                  46807.8                  69641.8
    ## 3                      0.0                   2400.0                  28990.2
    ## 4                  17702.8                  15032.8                  16852.8
    ## 5                      0.0                      0.0                   8389.8
    ## 6                  14915.8                  16785.8                  12945.8
    ##   outgoes_quarterly_1998.1 outgoes_quarterly_1998.2 outgoes_quarterly_1998.3
    ## 1                  11099.8                   9079.8                  14169.8
    ## 2                  87260.8                  53641.8                  65839.8
    ## 3                  31966.8                  23366.8                  15246.8
    ## 4                  19032.8                  12212.8                  11972.8
    ## 5                  16767.8                  13447.8                  18587.8
    ## 6                  27195.8                  24225.8                  13105.8
    ##   outgoes_quarterly_1998.4 incomes_yearly_1993 incomes_yearly_1994
    ## 1                  12395.2                 0.0                 0.0
    ## 2                  72737.2            228685.3            264653.2
    ## 3                  19992.2                 0.0                 0.0
    ## 4                  10718.2                 0.0                 0.0
    ## 5                  12203.2                 0.0                 0.0
    ## 6                  15861.2                 0.0             20907.0
    ##   incomes_yearly_1995 incomes_yearly_1996 incomes_yearly_1997
    ## 1             49564.1             50066.0             49275.1
    ## 2            265361.8            288542.9            281578.0
    ## 3                 0.0                 0.0             58139.7
    ## 4                 0.0             56920.5             67644.8
    ## 5                 0.0                 0.0             35950.9
    ## 6             81372.3             81507.7             81762.3
    ##   incomes_yearly_1998 outgoes_yearly_1993 outgoes_yearly_1994
    ## 1             45415.3                 0.0                 0.0
    ## 2            268232.3            200844.6            259580.6
    ## 3            114919.3                 0.0                 0.0
    ## 4             67780.7                 0.0                 0.0
    ## 5             61534.1                 0.0                 0.0
    ## 6             82068.2                 0.0                 0.0
    ##   outgoes_yearly_1995 outgoes_yearly_1996 outgoes_yearly_1997
    ## 1             31391.0             55579.2             47139.2
    ## 2            258853.6            274574.2            281093.2
    ## 3                 0.0                 0.0             31390.2
    ## 4                 0.0             31977.6             72711.2
    ## 5                 0.0                 0.0              8389.8
    ## 6             68474.6             81803.2             69283.2
    ##   outgoes_yearly_1998 owner_current_age age_groups has_creditcard
    ## 1             46744.6                28      18-30          FALSE
    ## 2            279479.6                53      51-60          FALSE
    ## 3             90572.6                42      41-50          FALSE
    ## 4             53936.6                79        70+          FALSE
    ## 5             61006.6                69      61-70          FALSE
    ## 6             80388.6                60      51-60          FALSE
    ##   is_checking_account outgoes_avg income_avg ratio_loan_to_sallary
    ## 1                TRUE       45213      48580                    NA
    ## 2                TRUE      259071     266176              6.576144
    ## 3                TRUE       60981      86530                    NA
    ## 4                TRUE       52875      64115                    NA
    ## 5                TRUE       34698      48742                    NA
    ## 6                TRUE       74987      69524                    NA
    ##   is_regular_income is_regular_income_80 consultation_on_site
    ## 1              TRUE                 TRUE                 TRUE
    ## 2              TRUE                 TRUE                 TRUE
    ## 3              TRUE                 TRUE                 TRUE
    ## 4              TRUE                 TRUE                 TRUE
    ## 5              TRUE                 TRUE                 TRUE
    ## 6              TRUE                 TRUE                 TRUE
    ##         salary_groups assets_dev assets_dev_groups is_savings_account
    ## 1        lower-income    13466.5          positive              FALSE
    ## 2        upper-income    42628.1          positive              FALSE
    ## 3 lower-middle-income    51096.1          positive              FALSE
    ## 4 lower-middle-income    33720.7          positive              FALSE
    ## 5        lower-income    28088.3          positive              FALSE
    ## 6 lower-middle-income    47668.0          positive              FALSE
    ##   is_loan_account cardwidthdrawals_num cardwidthdrawals_amount_mean
    ## 1           FALSE                   NA                           NA
    ## 2           FALSE                   NA                           NA
    ## 3           FALSE                   NA                           NA
    ## 4           FALSE                   NA                           NA
    ## 5           FALSE                   NA                           NA
    ## 6           FALSE                   NA                           NA
    ##   is_card_offer is_retirement_age has_res_prop heloc_offer district_id
    ## 1          TRUE             FALSE         TRUE       FALSE          18
    ## 2          TRUE             FALSE         TRUE       FALSE           1
    ## 3          TRUE             FALSE         TRUE       FALSE           5
    ## 4          TRUE              TRUE         TRUE        TRUE          12
    ## 5          TRUE              TRUE         TRUE        TRUE          15
    ## 6          TRUE             FALSE         TRUE       FALSE          51
    ##            region inhabitants municipalities_under_499_inhabitants
    ## 1   south Bohemia       70699                                   60
    ## 2          Prague     1204953                                    0
    ## 3 central Bohemia       95616                                   65
    ## 4 central Bohemia      107870                                   84
    ## 5   south Bohemia       58796                                   22
    ## 6    east Bohemia      121947                                   37
    ##   municipalities_500_to_1999_inhabitants
    ## 1                                     13
    ## 2                                      0
    ## 3                                     30
    ## 4                                     29
    ## 5                                     16
    ## 6                                     28
    ##   municipalities_2000_to_9999_inhabitants municipalities_over_10000_inhabitants
    ## 1                                       2                                     2
    ## 2                                       0                                     0
    ## 3                                       4                                     4
    ## 4                                       6                                     6
    ## 5                                       7                                     7
    ## 6                                       7                                     7
    ##   cities ratio_urban_inhabitants average_salary unemployment_rate_95
    ## 1      4                    65.3           8968                 2.83
    ## 2      1                   100.0          12541                 0.29
    ## 3      6                    51.4           9307                 3.85
    ## 4      6                    58.0           8754                 3.83
    ## 5      5                    51.9           9045                 3.13
    ## 6     11                    70.5           8541                 2.51
    ##   unemployment_rate_96 enterpreneurs_per_1000_inhabitants commited_crimes_95
    ## 1                 3.35                                131               1740
    ## 2                 0.43                                167              85677
    ## 3                 4.43                                118               2616
    ## 4                 4.31                                137               3804
    ## 5                 3.60                                124               1845
    ## 6                 2.97                                131               3496
    ##   commited_crimes_96
    ## 1               1910
    ## 2              99107
    ## 3               3040
    ## 4               3868
    ## 5               1879
    ## 6               3839

``` r
# Einkommen pro Monat 
incomes_per_month <- df_transaction_complete %>%
  group_by(
    account_id,
    year(date),
    month(date)
  ) %>%
  summarise(incomes_per_month = round(sum(incomes), digits = 0)) %>% 
  ungroup()

# monatliche Einkommen und Ausgaben und die Vermögensentwicklung
in_out_monthly <- incomes_per_month %>% 
  filter(incomes_per_month != 0) %>% 
  rename(year = `year(date)`,
         month = `month(date)`)


in_out_monthly <- incomes_per_month %>% 
  filter(incomes_per_month != 0) %>% 
  rename(year = `year(date)`,
         month = `month(date)`) 

in_out_yearly <- in_out_monthly %>% 
  group_by(account_id, year) %>% 
  summarise(incomes_per_year = round(mean(incomes_per_month), digits = 0) * 12) %>% 
  ungroup()



# Anfügen der Information an die Monatsübersicht
in_out_monthly <- in_out_monthly %>% 
  left_join(in_out_yearly, by = c("account_id", "year"))

in_out_yearly <- in_out_monthly %>% 
  group_by(account_id, year) %>% 
  summarise(incomes_per_year = round(mean(incomes_per_month), digits = 0) * 12) %>% 
  ungroup()


#prepare unemploymentrate for 1995 1996

in_out_yearly_crime_unemployment <- in_out_yearly [!(in_out_yearly$year=="1993" | in_out_yearly$year=="1994" | in_out_yearly$year=="1997" | in_out_yearly$year=="1998"),]

#check Data
in_out_yearly_crime_unemployment
```

    ## # A tibble: 5,840 × 3
    ##    account_id  year incomes_per_year
    ##         <int> <dbl>            <dbl>
    ##  1          1  1995            60120
    ##  2          1  1996            66600
    ##  3          2  1995           550404
    ##  4          2  1996           371988
    ##  5          4  1996            62736
    ##  6          6  1995            82716
    ##  7          6  1996            89652
    ##  8          7  1996           209424
    ##  9          8  1995           229728
    ## 10          8  1996           578292
    ## # ℹ 5,830 more rows

Es wurden die nötigen Spalten in einem neuen DF hinzugefügt. Damit mit
der Kriminalität und Arbeitslosenrate gearbeitet werden kann.

``` r
#plot salary 1995-1996 change appearence
in_out_yearly_crime_unemployment %>% 
  ggplot(aes(x = as.factor(year), y = incomes_per_year, fill = year)) +
  geom_boxplot() +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 15)
    ) +
    ggtitle("Incomes development") +
  labs(
    x = "",
    y = "Amount per year [CZK]",
    subtitle = "Time span: 1995-1996, across all accounts")
```

![](notebook_files/figure-gfm/unnamed-chunk-303-1.png)<!-- -->

``` r
#plot unemploymentrate 1995
df_crimes_cons %>%
  ggplot(aes(x= unemployment_rate_95,
         group = salary_groups,
         fill = salary_groups))+
  labs(title = "Unemployment rate 1995",
       subtitle = "grouped by salary groups",
       x ="Unemployment Rate [%] ",
       y = "Density",
       fill = "Salary groups")+

  geom_density(adjust=1.5, alpha=.4)
```

![](notebook_files/figure-gfm/unnamed-chunk-303-2.png)<!-- -->

``` r
#plot unemploymentrate 1996
df_crimes_cons %>%
  ggplot(aes(x= unemployment_rate_96,
         group = salary_groups,
         fill = salary_groups))+
  labs(title = "Unemployment rate 1996",
       subtitle = "grouped by salary groups",
       x ="Unemployment Rate [%]",
       y = "Density",
       fill = "Salary groups")+

  geom_density(adjust=1.5, alpha=.4)
```

![](notebook_files/figure-gfm/unnamed-chunk-303-3.png)<!-- -->

Erkenntnisse bezüglich dem Einkommen, wie bereits in einer anderen
Fragestellung erläutert, gäbe es 1996 einen leichten Einbruch. Im
Vergleich mit der Arbeitslosenrate, sehen wir, dass in den höheren
Einkommensgruppen Kategorie im Jahr 1996 einen leichten Anstieg bei der
Rate von 7.5% gab. Dies kann einen grossen Einfluss gehabt haben, denn
die höheren Einkommensgruppen machen eben durch Ihre Grösse ein
Grossteil des Volumens aus. Als die Arbeitslosenrate im Jahr 1995 im
gleichen Bereich eine Abflachung im Schrumpfen der Arbeitslosigkeit
erfuhr, ist sie im 1996 bei den höheren Einkommen noch einen leichten
Anstieg, mit angestiegen.

Überprüfung ob die Kriminalitätsrate einen Einfluss auf die
Einkommensentwicklung hat.

``` r
#plot salary 1995-1996 change appearence
in_out_yearly_crime_unemployment %>%
ggplot(aes(x = as.factor(year), y = incomes_per_year, fill = year)) +
  geom_boxplot() +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 15)
    ) +
    ggtitle("Incomes development") +
  labs(
    x = "",
    y = "Amount per year [CZK]",
    subtitle = "Time span: 1995-1996, across all accounts")
```

![](notebook_files/figure-gfm/unnamed-chunk-304-1.png)<!-- -->

``` r
#plot crime rate 1995
df_crimes_cons %>%
  ggplot(aes(x= commited_crimes_95,
         group = salary_groups,
         fill = salary_groups))+
  labs(title = "Crime rate 1995",
       subtitle = "grouped by salary groups",
       x ="Crime Rate ",
       fill = "Salary groups")+

  geom_density(adjust=1.5, alpha=.4)
```

![](notebook_files/figure-gfm/unnamed-chunk-304-2.png)<!-- -->

``` r
#plot crime rate 1996
df_crimes_cons %>%
  ggplot(aes(x= commited_crimes_96,
         group = salary_groups,
         fill = salary_groups))+
  labs(title = "Crime rate 1996",
       subtitle = "grouped by salary groups",
       x ="Crime Rate",
       fill = "Salary groups")+

  geom_density(adjust=1.5, alpha=.4)
```

![](notebook_files/figure-gfm/unnamed-chunk-304-3.png)<!-- -->

Zwischen dem Einkommen und Kriminalitätsrate erkennen wir hier keinen
Zusammenhang. Die Kriminalitätsrate verändert sich von 1995 auf 1996
kaum. Somit ist die Einkommensentwicklung unabhängig von der
Kriminalitätsrate zu betrachten.

v\. Kann man Unterschiede herleiten, um Geschäfts- und Privatkunden zu
unterteilen?

[^83] [^84]: Erstellt von Luca Gisler

Diese Frage ist in diesem Sinne interessant, dass man im Bereich des
Cross Sellings unterschiedliche Angebote für Geschäfts- und Privatkunden
ausarbeiten kann. Denn diese zwei Kundengruppen haben andere Ansprüche
und eine andere Beziehung zur Bank.

Eine Möglichkeit um herauszufinden ob es ein Geschäftskonto ist, wenn
viele Nutzer einer Karte die gleiche Adresse besitzen. Dann kann man
darauf schliessen, dass dies der Sitz und die Adresse der Firma ist.

Da wir über keine Adressen verfügen ist dieser Ansatz eine Sackgasse.

Kann man anhand von Transaktionen ein Geschäftskonto erkennen?

INSURRANCE - Ein Unternehmen muss auch Versicherungen bezahlen, deshalb
ist dieser Transaktionstyp kein sicheres Ausscheide Kriterium.
HOUSEHOLD - Hat ein Unternehmen Ausgaben die den Haushalt betreffen? Ein
Unternehmen zahlt Löhne, Mieten, Versicherungen. Wir vermuten dass ein
Unternehmen keine Ausgaben im Bereich Haushalt hat. Aber was ist wenn
eine Festlichkeit ansteht? Dann fallen diese Ausgaben auch in den
Bereich des Haushaltes. Also kein Kriterium für das sichere
Identifizieren. LEASING - Ein Unternehmen kann auch ein Auto leasen,
also ist das kein sicheres Ausscheide Kriterium.

Ist es möglich anhand der Owner/User Kombination ein Geschäftliches
Konto zu erkennen?

Wenn es viele User mit unterschiedlichen Adressen auf einen Owner gibt,
könnte dies ein Indiz dafür sein, ein Geschäftskonto zu sein.

Auch diesen Ansatz wurde Verworfen, bei den Owner / User ist und ein
Muster Aufgefallen.

Für dieses Beispiel wurde die Alter bei Eröffnung des Kontos verglichen.

``` r
#filter for the ages 30 and 50
age_correaltions_owner_user <-df_cons %>% filter((owner_age_at_account_opening == "50" | owner_age_at_account_opening == "30")) %>% 
  filter(user_age_at_account_opening != "NA") %>%  #remove all NA for this example
  #select the wanted Informations Age, district, sex for both owner and user
  select(
    owner_age_at_account_opening,
    owner_sex,
    user_age_at_account_opening,
    user_sex,
    owner_district_id,
    user_district_id
  )
#show the first 30 entries
head(age_correaltions_owner_user, 30)
```

    ##    owner_age_at_account_opening owner_sex user_age_at_account_opening user_sex
    ## 1                            50    Female                          57     Male
    ## 2                            30      Male                          25   Female
    ## 3                            30      Male                          26   Female
    ## 4                            30      Male                          31   Female
    ## 5                            50      Male                          58   Female
    ## 6                            30    Female                          34     Male
    ## 7                            30    Female                          37     Male
    ## 8                            30    Female                          23     Male
    ## 9                            30      Male                          23   Female
    ## 10                           30    Female                          37     Male
    ## 11                           50    Female                          56     Male
    ## 12                           50    Female                          52     Male
    ## 13                           30    Female                          29     Male
    ## 14                           30    Female                          24     Male
    ## 15                           30    Female                          27     Male
    ## 16                           30    Female                          22     Male
    ## 17                           30      Male                          32   Female
    ## 18                           30    Female                          31     Male
    ## 19                           50    Female                          50     Male
    ## 20                           50      Male                          55   Female
    ## 21                           30      Male                          27   Female
    ## 22                           30    Female                          37     Male
    ## 23                           50    Female                          45     Male
    ## 24                           50      Male                          45   Female
    ## 25                           30    Female                          33     Male
    ## 26                           50    Female                          43     Male
    ## 27                           50      Male                          58   Female
    ## 28                           30    Female                          31     Male
    ## 29                           30      Male                          36   Female
    ## 30                           50    Female                          48     Male
    ##    owner_district_id user_district_id
    ## 1                 40               40
    ## 2                 31               31
    ## 3                  1                1
    ## 4                 56               56
    ## 5                 67               67
    ## 6                 52               52
    ## 7                 10               10
    ## 8                 70               70
    ## 9                 70               70
    ## 10                60               60
    ## 11                74               74
    ## 12                19               19
    ## 13                64               64
    ## 14                52               52
    ## 15                70               70
    ## 16                46               46
    ## 17                36               36
    ## 18                72               72
    ## 19                31               31
    ## 20                64               64
    ## 21                18               18
    ## 22                 1                1
    ## 23                64               64
    ## 24                72               72
    ## 25                60               60
    ## 26                66               66
    ## 27                27               27
    ## 28                52               52
    ## 29                71               71
    ## 30                70               70

Wir haben die Alter für diese Tabelle auf 30 und 50 festgelegt, und
können somit dieses erkannte Muster aufzeigen. Das Alter des Konto
Inhabers und das Alter des weiteren Benutzer (falls vorhanden) bewegt
sich im gleichen Altersbereich mit gewissen Abweichungen. Das Geschlecht
ergänzt sich auch so, dass wir annehmen können, dass diese Verbindung
eine Private und somit keine Geschäftliche Beziehung darstellt. Diese
Theorie wird noch davon gestützt, dass die Owner und User immer im
gleichen District Wohnhaft sind.

Aufgrund dieser Erkenntnisse sind wir auf den Schluss gekommen, dass man
Geschäfts- und Privatkunden anhand unserer Erkenntnisse nicht
unterteilen kann.

vi\. Wie sieht die Entwicklung der Kredite in Bezug auf Typ und Dauer
aus? Kann aus der Grafik ein Trend ausgelesen werden?

[^85] [^86]: Erstellt von Luca Gisler

``` r
#prepare informations, removed NAs and plot
Loan_trend_over_time <- df_cons %>%
  filter(loan_status != "NA") %>%
  ggplot(Loan_trend_over_time, mapping = aes(x = loan_start_date,
                                y = loan_duration_in_month)) +
  
  #add jittering for visability
  geom_jitter()+
  #add geom_smooth for visual trend changes
  geom_smooth()+
  #new Title, axis-labels
  labs(title = "Loan duration over Time")+
  xlab("Years")+
  ylab("Loan duration in Month")

#print plot
Loan_trend_over_time
```

![](notebook_files/figure-gfm/unnamed-chunk-306-1.png)<!-- -->

Was direkt auffällt ist, dass wir über die Zeit eine Zunahme an neuen
neuen Krediten erkennen. Die Dauer der Kredite hat sich über dir Zeit
nicht spürbar verändert. Um das Jahr 1996 gab es Durchschnittlich
weniger Kredite mit kurzer Dauer als vor und nach dieser Zeit. An der
Regressionslinie ist ein leichter Aufwärtstrend ersichtlich, welche
einen kleinen Dämpfer hatte und zwar um die Jahre 1007 und 1998.

Über den Typ von Krediten kann keine Aussage getroffen werden, da diese
nicht ersichtlich sind. Deshalb können nur Veränderungen über die Zeit
Analysiert werden.

``` r
#prepare informations, removed NAs and plot
Loan_trend_over_time_ammount <- df_cons %>%
  filter(loan_status != "NA") %>%
  ggplot(Loan_trend_over_time_ammount, mapping = aes(x = loan_start_date,
                                y = loan_total_amount)) +
  
  #add jittering
  geom_jitter()+
  #add geom_smooth for for visual trends
  geom_smooth()+
  scale_y_continuous(labels=comma)+
  #create new title and axis-labels
  labs(title = "Loan um over time")+
  xlab("Years")+
  ylab("Value of Loans [CZK]")

#print plot
Loan_trend_over_time_ammount
```

![](notebook_files/figure-gfm/unnamed-chunk-307-1.png)<!-- -->

Die Kreditsumme hat sich über die Zeit nicht wesentlich verändert. Wir
sehen einen Stabiles verhalten der Summe von Krediten. Daraus schliessen
wir, dass es keine wesentlichen Veränderungen im Bereich der
Kreditvergabe, aber auch von Seitens Kreditnehmer gab. Das Volumen der
Kredite hat im in der Grafik von vorhin vergrössert.

#### Aufzeigen von Zusammenhänge im Kreditverhalten der Kunden

Gibt es einen Zusammenhang zwischen den unterschiedlichen Regionen und
dem Kreditverhalten (Kreditgrösse, Dauer) von den Kunden? Für eine
Optimierung der lokalen Werbekampagnen und Angebote in den
entsprechenden Regionen(Cross Selling)[^87]

``` r
#filter the NAs and create plot
district_loan_correlation <- df_cons %>% 
  filter(!is.na(loan_start_date)) %>%
  ggplot(aes (x = account_district_region,
              y = loan_total_amount))+
  
  #add title and axis names, labels for CZK not in hex
  geom_boxplot()+
  scale_y_continuous(labels=comma)+
  labs(title = "Loan differences in districts",
       subtitle = "Loan size [CZK]")+
  xlab("regions")+
  ylab("Loan size [CZK]")+
  theme(axis.text.x = element_text(angle = 45))

#creat plotly plot, for more informations by hoverinfo
ggplotly(district_loan_correlation)
```

<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-08e5a9dc2ddb2d5090f9" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-08e5a9dc2ddb2d5090f9">{"x":{"data":[{"x":[5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],"y":[80952,70656,78936,228276,268320,234336,247728,48672,258444,31248,415800,76416,110136,20196,168984,194280,323472,428784,80952,209040,288360,66696,390660,320976,100260,44940,252060,24516,77184,208320,89280,276084,74352,136344,419880,76944,68832,36396,94488,215388,71460,34572,99216,271584,17184,157068,131292,203940,76380,316140,165168,61056,31176,274740,39576,74916,63576,212976,37944,101520,279072,270648,108144,18564,120888,31728,36168,11736,44952,231696,218064,77808,5148,140400,321936,99936,482940,38148,91584,566640,125340,88272,197808,45720,110736,91248,318480,163332,30276,396120,53076,269964,140688,52788,87216,352704,21396,24312,111384,220620,117024,115812,170256,152160,321180,265320,60156,140064,156096,98304,266220,66840,116832,128988,44376,333648,173016,86880,22968,321240,140688,165960,140880,189696,191880,97632,407952,192552,159744,86184,85716,110736,130464,45336,232560,154416,30276,421008,125472,170832,41256,177504,91632,182040,299088,193968,42840,161496,260400,230400,81144,215616,97392,41988,29448,94512,348120,168264,33144,210384,285240,182628,42816,51696,112752,100128,247920,309552,90156,221280,194880,45768,177744,230220,48744,27456,53232,335184,541200,102876,123600,253200,22260,35904,55632,89880,87528,39576,100224,41904,68124,196224,99336,178500,380160,170448,202848,374760,45024,66480,68112,188532,388512,30276,495180,398640,60000,30276,89040,104712,259740,267600,92400,83016,68400,49872,88200,162624,73260,141948,172272,151728,76992,49440,231168,49488,129408,59448,59760,31140,305424,309600,57060,232560,465504,154656,86184,139464,76680,17508,78696,173808,45768,109344,311232,155616,187104,74040,399120,146064,128544,214596,194256,437460,134784,84120,122472,86184,220428,23412,217152,141240,23328,50976,165120,135360,124344,28248,390000,220440,165960,72120,137880,103680,38520,79608,191088,69624,265320,7656,213300,185640,293160,35856,50112,309660,41952,48624,133968,219456,184620,334620,22932,30012,339168,158184,63360,253200,48312,204000,174048,32640,189300,197748,468060,191580,141648,77640,390096,36204,156420,158976,95400,113436,369000,153936,71976,238716,207264,314520,149340,189792,117420,158760,174960,100800,317460,181020,240528,74736,150912,385560,115992,93960,127080,78600,290700,42900,359760,27984,331584,209184,277884,288720,98832,127200,71064,67464,155760,88440,198240,10944,174744,106128,22104,185952,25944,24180,130896,239460,162468,31260,68544,160920,91200,271260,64860,247920,96768,20832,57120,87816,125472,43200,42384,73056,16032,176148,123408,46320,21852,133848,123216,155880,17952,162576,212400,284280,327660,23628,73836,48096,15420,300660,128268,289920,475680,186384,67464,123300,119136,87360,91152,83280,253560,44304,293880,176328,69600,240900,179328,143676,139536,244368,444864,200976,84288,294384,23520,360000,357840,210744,96168,58764,189696,24792,47304,205200,288468,88608,98184,23052,61320,93036,100080,4980,44640,44640,244560,93888,68664,51360,590820,74772,170700,41856,153504,204780,253512,75624,150180,47016,192744,104808,164196,368880,225420,11400,272220,82896,20352,14028,385104,71460,473280,187224,65184,110112,84600,249792,89340,52512,12792,185544,56100,83628,99300,102240,54024,167100,55368,121896,67608,188688,106944,349392,63972,8616,194160,372120,87216,320592,100980,49320,186300,417600,23184,227880,21924,276300,307200,151560,113796,465072,276660,76104,83628,192744,165780,208128,14628,67980,174840,18324,188616,89340,85632,113544,405780,37908,79632,39024,182640,43164,46176,33984,116496,389136,335280,280440,116040,71820,54396,148920,123696,66432,61560,72504,36684,49320,260640,79344,18720,30060,202788,249312,215040,26208,15192,385584,96396,153144,224604,239160,222180,82128,248148,152460,64656,107352,417060,72408,97980,174744,136368,156672,19044,52128,300204,51408,92976,79920,99696,97224,347952,300600,88704,37812,90180,196080,84288,78720,95808,59136,104712,272220,29784,321360,164052,12540,74688,213300,105804,196800,464520,96984,314688,177804,262980,199680,63252,50460,221880,61656,44088,74124,137904,64224,151200,341280,149040,160656,33348,57360,99744,225504,93600,285600,98592,77544,31620,87360,20220,104964,504000,270384,63312,66696,460980,230220,402000,290820,85860,208332,194940,76908,19248,38496,144960,59448,360864,133800,93888,80340,45456,76080,87840,538500,107640,53472,233424,44628,40632,172080,108720,189060,52236,21072,155640,317520,155616,143904,49044,69360,22356,272520,126600,79824,86616,331560,148140,392460,97176,129312,466608,67320,260028,39168,164256,91632,68340,139488],"hoverinfo":"y","type":"box","fillcolor":"rgba(255,255,255,1)","marker":{"opacity":null,"outliercolor":"rgba(0,0,0,1)","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)"},"size":5.66929133858268},"line":{"color":"rgba(51,51,51,1)","width":1.88976377952756},"showlegend":false,"xaxis":"x","yaxis":"y","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":74.9563569618931,"l":66.4840182648402},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Loan differences in districts","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,8.6],"tickmode":"array","ticktext":["central Bohemia","east Bohemia","north Bohemia","north Moravia","Prague","south Bohemia","south Moravia","west Bohemia"],"tickvals":[1,2,3,4,5,6,7,8],"categoryorder":"array","categoryarray":["central Bohemia","east Bohemia","north Bohemia","north Moravia","Prague","south Bohemia","south Moravia","west Bohemia"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"regions","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-24312,620112],"tickmode":"array","ticktext":["0","200,000","400,000","600,000"],"tickvals":[-3.63797880709171e-12,200000,400000,600000],"categoryorder":"array","categoryarray":["0","200,000","400,000","600,000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Loan size [CZK]","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"36bb79db8c14":{"x":{},"y":{},"type":"box"}},"cur_data":"36bb79db8c14","visdat":{"36bb79db8c14":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

In den Distrikten haben wir einen MIN/MAX unterschied im Median von
58’782 CZK (MAX: east Bohemia 155’178, MIN: west Bohemia 96’396). Das
ist ein grosser Unterschied, auf was ist dieser zurückzuführen?

``` r
#filter the NAs and create plot
district_loan_correlation_duartion <- df_cons %>% 
  filter(!is.na(loan_start_date)) %>%
  ggplot(aes (x = account_district_region,
              y = loan_duration_in_month))+
  
  #add title and axis names, labels for CZK not in hex  
  geom_boxplot()+
  scale_y_continuous(labels=comma)+
  labs(title = "Loan differences in districts",
       subtitle = "Loan duration")+
  xlab("region")+
  ylab("Loan duration [Monat]")+
  theme(axis.text.x = element_text(angle = 45))

#creat plotly plot, for more informations by hoverinfo
ggplotly(district_loan_correlation_duartion)
```

<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-835a92d6f5a33cd07ebf" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-835a92d6f5a33cd07ebf">{"x":{"data":[{"x":[5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],"y":[24,48,12,36,60,48,48,36,36,12,60,24,24,36,24,60,48,48,24,24,60,24,60,48,60,12,60,12,36,48,12,36,24,24,60,12,36,12,24,36,36,12,36,48,12,36,36,60,12,60,36,48,24,60,12,36,12,36,36,12,48,36,36,12,36,24,12,24,24,36,48,12,12,36,48,24,60,12,24,60,60,48,24,12,48,48,60,36,12,60,12,36,48,12,48,48,12,12,24,60,24,36,24,60,60,36,36,24,36,12,60,24,12,36,12,48,36,12,36,60,48,24,60,24,60,12,48,24,48,36,36,48,48,24,48,48,12,48,24,48,12,48,12,60,36,36,12,36,60,60,36,48,12,12,12,12,60,36,12,48,60,36,12,24,36,24,48,48,12,48,48,12,48,36,36,12,48,48,60,12,60,60,60,48,24,24,24,12,48,12,12,48,24,60,60,48,48,60,48,24,12,36,48,12,60,60,60,12,60,24,60,48,12,12,12,12,36,48,60,36,24,48,24,60,48,24,24,24,24,60,48,48,12,48,48,48,12,36,24,12,36,24,12,12,48,48,24,12,60,24,24,36,24,60,24,24,24,12,36,12,36,24,12,36,60,48,36,24,48,60,36,12,60,60,12,24,48,36,36,24,36,60,60,24,24,60,12,24,48,48,60,60,36,12,48,36,60,60,24,60,48,48,60,36,60,60,24,12,48,12,60,48,36,36,60,24,24,36,48,60,60,48,60,60,24,24,60,60,48,36,36,60,36,60,60,24,60,12,60,12,48,48,36,48,48,48,36,12,24,12,24,36,24,36,12,48,24,60,24,60,36,12,24,36,24,60,60,48,36,48,48,12,24,24,12,48,48,36,24,24,36,24,48,24,12,36,36,60,60,12,36,24,12,60,36,48,48,48,12,36,48,24,24,48,60,24,60,36,48,60,48,36,24,48,48,48,48,48,12,48,48,36,12,12,24,24,24,36,36,12,24,12,60,12,48,12,24,24,60,36,24,12,60,36,60,48,48,60,36,24,60,12,36,12,36,60,60,12,60,12,48,12,48,12,60,24,12,24,36,48,60,12,12,36,60,36,60,60,12,60,24,24,24,48,24,48,36,24,48,60,48,48,60,36,60,60,12,36,36,60,60,24,36,48,36,24,36,36,60,48,12,12,60,12,24,60,48,36,60,12,24,12,48,12,48,48,24,48,48,60,60,36,36,60,48,24,12,24,12,12,36,48,60,60,36,48,60,36,24,48,12,36,36,60,60,24,36,60,48,36,60,24,60,24,24,36,36,24,36,24,24,60,48,24,48,60,48,12,36,48,48,48,12,48,24,60,24,60,36,12,12,60,36,24,60,36,48,36,60,60,12,60,60,24,24,36,24,24,36,60,48,24,12,12,24,48,24,60,24,36,60,24,12,12,60,48,48,24,60,36,60,60,60,36,36,12,12,12,48,24,48,24,36,12,12,12,24,60,24,12,36,12,12,48,60,60,36,24,24,60,48,24,12,48,36,60,24,12,12,60,36,60,24,24,48,36,36,24,48,12,60,24],"hoverinfo":"y","type":"box","fillcolor":"rgba(255,255,255,1)","marker":{"opacity":null,"outliercolor":"rgba(0,0,0,1)","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)"},"size":5.66929133858268},"line":{"color":"rgba(51,51,51,1)","width":1.88976377952756},"showlegend":false,"xaxis":"x","yaxis":"y","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":74.9563569618931,"l":37.2602739726027},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Loan differences in districts","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,8.6],"tickmode":"array","ticktext":["central Bohemia","east Bohemia","north Bohemia","north Moravia","Prague","south Bohemia","south Moravia","west Bohemia"],"tickvals":[1,2,3,4,5,6,7,8],"categoryorder":"array","categoryarray":["central Bohemia","east Bohemia","north Bohemia","north Moravia","Prague","south Bohemia","south Moravia","west Bohemia"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"region","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[9.6,62.4],"tickmode":"array","ticktext":["10","20","30","40","50","60"],"tickvals":[10,20,30,40,50,60],"categoryorder":"array","categoryarray":["10","20","30","40","50","60"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Loan duration [Monat]","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"36bb129dc2ca":{"x":{},"y":{},"type":"box"}},"cur_data":"36bb129dc2ca","visdat":{"36bb129dc2ca":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

East Bohemia ist mit einem Median von 48 Monaten (4 Jahre) Kreditdauer
weit über dem anderen Regionen. In den anderen Regionen ist der Median
genau bei 36 Monaten (3 Jahre) Kreditdauer. In Kombination mit der für
Tschechien überdurchschnittlichen Kreditgrösse, kann man das in den
Zusammenhang bringen, das die Dauer und die Kreditgrösse zusammenhängen.

Jetzt werden wir uns die Monatliche Summe pro Region betrachten.

``` r
#filter the NAs and create plot
district_loan_correlation_monthly_amount <- df_cons %>% 
  filter(!is.na(loan_start_date)) %>%
  ggplot(aes (x = account_district_region,
              y = loan_total_amount/loan_duration_in_month))+
  
  #add title and axis names, labels for CZK not in hex  
  geom_boxplot()+
  scale_y_continuous(labels=comma)+
  labs(title = "Loan differences in districts")+
  xlab("regions")+
  ylab("Loan size / Month [CZK]")+
  theme(axis.text.x = element_text(angle = 45))

#creat plotly plot, for more informations by hoverinfo
ggplotly(district_loan_correlation_monthly_amount)
```

<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-d1cf52d9779a0d23a085" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-d1cf52d9779a0d23a085">{"x":{"data":[{"x":[5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],"y":[3373,1472,6578,6341,4472,4882,5161,1352,7179,2604,6930,3184,4589,561,7041,3238,6739,8933,3373,8710,4806,2779,6511,6687,1671,3745,4201,2043,2144,4340,7440,7669,3098,5681,6998,6412,1912,3033,3937,5983,1985,2881,2756,5658,1432,4363,3647,3399,6365,5269,4588,1272,1299,4579,3298,2081,5298,5916,1054,8460,5814,7518,3004,1547,3358,1322,3014,489,1873,6436,4543,6484,429,3900,6707,4164,8049,3179,3816,9444,2089,1839,8242,3810,2307,1901,5308,4537,2523,6602,4423,7499,2931,4399,1817,7348,1783,2026,4641,3677,4876,3217,7094,2536,5353,7370,1671,5836,4336,8192,4437,2785,9736,3583,3698,6951,4806,7240,638,5354,2931,6915,2348,7904,3198,8136,8499,8023,3328,2394,2381,2307,2718,1889,4845,3217,2523,8771,5228,3559,3438,3698,7636,3034,8308,5388,3570,4486,4340,3840,2254,4492,8116,3499,2454,7876,5802,4674,2762,4383,4754,5073,3568,2154,3132,4172,5165,6449,7513,4610,4060,3814,3703,6395,1354,2288,1109,6983,9020,8573,2060,4220,371,748,2318,3745,3647,3298,2088,3492,5677,4088,4139,2975,6336,3551,4226,6246,938,2770,5676,5237,8094,2523,8253,6644,1000,2523,1484,4363,4329,5575,7700,6918,5700,4156,2450,3388,1221,3943,7178,3161,3208,824,4816,2062,5392,2477,2490,519,6363,6450,4755,4845,9698,3222,7182,3874,3195,1459,2186,7242,3814,9112,6484,3242,7796,6170,6652,6086,5356,5961,8094,7291,5616,3505,5103,7182,6123,1951,6032,5885,1944,1416,2752,2820,3454,1177,8125,3674,4610,6010,2298,1728,3210,3317,3981,1934,7370,319,5925,3094,4886,1494,2088,5161,3496,2026,2791,4572,3077,5577,637,2501,7066,4394,1056,4220,2013,3400,3626,680,3155,5493,7801,3193,5902,6470,8127,3017,2607,3312,2650,3151,6150,6414,2999,6631,4318,5242,2489,3954,1957,2646,7290,4200,5291,3017,5011,2076,4192,6426,3222,1566,2118,3275,4845,3575,5996,2332,6908,4358,7719,6015,2059,2650,1974,5622,6490,7370,8260,304,7281,2948,1842,3874,1081,403,5454,3991,4513,2605,2856,4470,3800,4521,1081,5165,2688,434,1190,7318,5228,1800,3532,1522,334,4893,5142,1930,607,5577,2567,6495,1496,4516,5900,4738,5461,1969,2051,2004,1285,5011,3563,6040,9910,3883,5622,3425,2482,3640,3798,1735,4226,1846,4898,4898,1450,4015,3736,3991,5814,5091,9268,4187,1756,6133,1960,7500,7455,5854,8014,4897,7904,1033,1971,5700,8013,7384,4091,1921,1022,7753,2085,415,1860,1860,4076,2608,2861,4280,9847,2077,2845,872,3198,3413,7042,3151,2503,3918,5354,8734,4561,6148,3757,950,4537,6908,424,1169,8023,5955,7888,7801,5432,4588,2350,5204,1489,4376,1066,5154,935,2323,1655,1704,4502,2785,2307,5079,2817,3931,4456,7279,1777,359,4045,6202,1817,6679,1683,1370,3105,6960,1932,6330,609,4605,5120,6315,3161,9689,7685,3171,2323,5354,2763,4336,1219,5665,2914,1527,7859,1489,1784,3154,6763,3159,3318,3252,3805,3597,962,708,4854,8107,6985,4674,1934,1995,1511,2482,2577,2768,5130,3021,3057,4110,7240,1653,312,501,5633,5194,3584,728,633,8033,8033,4254,6239,3986,3703,3422,6893,2541,1347,2982,6951,3017,1633,7281,5682,4352,529,2172,8339,2142,3874,1332,2077,4051,7249,5010,1848,3151,2505,4085,1756,1640,7984,1232,4363,4537,1241,5356,4557,1045,6224,3555,2939,8200,7742,2694,6556,4939,4383,3328,5271,841,3698,2569,1837,2059,5746,2676,4200,5688,3105,6694,2779,4780,4156,4698,3900,4760,4108,2154,527,3640,1685,8747,8400,5633,1319,2779,7683,6395,6700,4847,1431,5787,5415,6409,1604,3208,3020,2477,7518,5575,2608,6695,3788,6340,3660,8975,4485,4456,6484,3719,3386,3585,1812,3151,1451,878,6485,5292,3242,5996,4087,1445,621,4542,5275,6652,7218,5526,4115,6541,4049,5388,9721,1870,7223,1632,3422,7636,1139,5812],"hoverinfo":"y","type":"box","fillcolor":"rgba(255,255,255,1)","marker":{"opacity":null,"outliercolor":"rgba(0,0,0,1)","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)"},"size":5.66929133858268},"line":{"color":"rgba(51,51,51,1)","width":1.88976377952756},"showlegend":false,"xaxis":"x","yaxis":"y","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":74.9563569618931,"l":60.6392694063927},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Loan differences in districts","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,8.6],"tickmode":"array","ticktext":["central Bohemia","east Bohemia","north Bohemia","north Moravia","Prague","south Bohemia","south Moravia","west Bohemia"],"tickvals":[1,2,3,4,5,6,7,8],"categoryorder":"array","categoryarray":["central Bohemia","east Bohemia","north Bohemia","north Moravia","Prague","south Bohemia","south Moravia","west Bohemia"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"regions","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-176.3,10390.3],"tickmode":"array","ticktext":["0","2,500","5,000","7,500","10,000"],"tickvals":[0,2500,5000,7500,10000],"categoryorder":"array","categoryarray":["0","2,500","5,000","7,500","10,000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Loan size / Month [CZK]","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"36bb785f3e41":{"x":{},"y":{},"type":"box"}},"cur_data":"36bb785f3e41","visdat":{"36bb785f3e41":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

Damit wir einen Einheitlichen Vergleich erstellen können, stellen wir
die Monatlichen Kreditkosten dar. Des weiteren Vergleichen wir jetzt
diese Grafik mit dem Durchschnittlichen Distrikt Einkommen

Berechnung des Einkommens auf die Region.

``` r
#calculate average salary per region for compariswon with loan
df_account_region_average_salary <- df_cons %>% 
  group_by(account_district_region) %>% 
  summarise(account_region_average_salary = mean(account_district_average_salary)) %>% 
  arrange(desc(account_region_average_salary))

 

#print the calculated values
df_account_region_average_salary
```

    ## # A tibble: 8 × 2
    ##   account_district_region account_region_average_salary
    ##   <fct>                                           <dbl>
    ## 1 Prague                                         12541 
    ## 2 north Moravia                                   9410.
    ## 3 central Bohemia                                 9409.
    ## 4 north Bohemia                                   9310.
    ## 5 west Bohemia                                    9018.
    ## 6 south Moravia                                   8895.
    ## 7 south Bohemia                                   8817.
    ## 8 east Bohemia                                    8626.

Wir sehen, Prag hat das höchste Einkommen aller Regionen in Tschechien.
Im Vergleich mit den monatlichen Kosten für einen Kredit ist Prag nicht
oben ausgerissen wie hier beim Lohn. Das heisst, in Prag wird MEHR
verdient und somit könnten auch die Angebote in höheren Preisklassen
beworben werden. In Prag könnte die Bank für ihre “STANDARD” und GOLD”
Kreditkarten Werbung machen. Zusätzlich kann die Bank ihre Angebote im
Bereich der Hypotheken bewerben, denn die Städte werden auf Grund der
guten Angebote weiter wachsen, und somit ist der Markt für Hypotheken
auch ein Boomendes Geschäft. Durch die Vergabe von mehr Krediten
verdient die Bank an den Zinsen und kann somit ihren Gewinn steigern und
das Angebot verbessern und wo nötig erweitern.

## Summary

Ziel dieser Gruppenarbeit war es, aufgrund des vorgegebenen Datensatzes
Einsicht in die Transaktionsvorgänge einer Bank zu erhalten, um daraus
Erkenntnisse zu gewinnen welche für die Bank im Bereich des
cross-selling von Relevanz sein könnten.

In einem ersten Schritt wurden dazu nach erster Sichtung der Daten,
Fragestellungen erarbeitet, welche die Gruppe in dieser Arbeit
beantworten möchte.

Da der Datensatz von einer Tschechischen Bank stammt und somit auf
Tschechisch beschrieben war, musste er in einem ersten Schritt übersetzt
werden. Weiter bestand der Datensatz aus mehreren Dateien, welche in
einem nächsten Schritt konsolidiert, und zu einem einzigen Datensatz
kombiniert wurden.

Der zusammengefasste, konsolidiert Datensatz konnte anschliessend mit
verschiedenen Vorgehensweisen der explorativen Datenanalyse bearbeitet
werden.

Dazu wurden die Hypothesen unter den Gruppenmittgliedern aufgeteilt und
grösstenteils einzeln erarbeitet. Um eine Kontinuität innerhalb der
Arbeit zu erhalten, besprachen wir uns mehrere male bezüglich des
weiteren Vorgehens. Weiter wurde der erstelle Code in der Gruppe
besprochen und verbessert.

Durch diese Gruppenarbeit haben wir gelernt, wie wichtig es ist, unsere
Vorgehensweise sorgfältig zu planen und zu gestalten, um
sicherzustellen, dass arbeiten nicht mehrere Male oder Parallel
durchgeführt werden. Die Arbeit im Team hat es uns ermöglicht, uns
gegenseitig mit Ideen zu versorgen und von einer Vielfalt von
Perspektiven zu profitieren.

Die Fähigkeiten, die wir in den Bereichen R, Data Wrangling,
explorativen Datenanalyse sowie supervised learning erworben haben,
werden uns sicher bei zukünftigen Arbeiten sowie im Berufsalltag von
Nutzen sein.

Wir freuen uns darauf, die Erkenntnisse dieser Arbeit am 27.01.2023 zu
präsentieren.

[^1]: Erstellt von Léonie Bécheiraz

[^2]: Erstellt von Léonie Bécheiraz

[^3]: Erstellt von Léonie Bécheiraz

[^4]: Erstellt von Léonie Bécheiraz

[^5]: Erstellt von Léonie Bécheiraz

[^6]: Erstellt von Léonie Bécheiraz

[^7]: Erstellt von Léonie Bécheiraz

[^8]: Erstellt von Léonie Bécheiraz

[^9]: Erstellt von Léonie Bécheiraz

[^10]: Erstellt von Léonie Bécheiraz

[^11]: Erstellt von Léonie Bécheiraz

[^12]: Erstellt von Luca Gisler

[^13]: Erstellt von Léonie Bécheiraz

[^14]: Erstellt von Léonie Bécheiraz

[^15]: Erstellt von Léonie Bécheiraz

[^16]: Erstellt von Léonie Bécheiraz

[^17]: Erstellt von Léonie Bécheiraz

[^18]: Erstellt von Léonie Bécheiraz

[^19]: Erstellt von Léonie Bécheiraz

[^20]: Erstellt von Léonie Bécheiraz

[^21]: Erstellt von Léonie Bécheiraz

[^22]: Erstellt von Léonie Bécheiraz

[^23]: Erstellt von Luca Gisler

[^24]: Erstellt von Léonie Bécheiraz

[^25]: Erstellt von Léonie Bécheiraz

[^26]: Erstellt von Léonie Bécheiraz

[^27]: Erstellt von Léonie Bécheiraz

[^28]: Erstellt von Aaron Studer

[^29]: Erstellt von Aaron Studer

[^30]: Erstellt von Aaron Studer

[^31]: Erstellt von Léonie Bécheiraz

[^32]: Erstellt von Aaron Studer

[^33]: Erstellt von Aaron Studer

[^34]: Erstellt von Aaron Studer

[^35]: Erstellt von Aaron Studer

[^36]: Erstellt von Aaron Studer

[^37]: Erstellt von Aaron Studer

[^38]: Erstellt von Léonie Bécheiraz

[^39]: Erstellt von Léonie Bécheiraz

[^40]: Erstellt von Léonie Bécheiraz

[^41]: Erstellt von Léonie Bécheiraz

[^42]: Erstellt von Léonie Bécheiraz

[^43]: Erstellt von Léonie Bécheiraz

[^44]: Erstellt von Léonie Bécheiraz

[^45]: Erstellt von Léonie Bécheiraz

[^46]: Erstellt von Léonie Bécheiraz

[^47]: Erstellt von Léonie Bécheiraz

[^48]: Erstellt von Léonie Bécheiraz

[^49]: Erstellt von Léonie Bécheiraz

[^50]: Erstellt von Léonie Bécheiraz

[^51]: Erstellt von Léonie Bécheiraz

[^52]: Erstellt von Léonie Bécheiraz

[^53]: Erstellt von Léonie Bécheiraz

[^54]: Erstellt von Léonie Bécheiraz

[^55]: Erstellt von Léonie Bécheiraz

[^56]: Erstellt von Léonie Bécheiraz

[^57]: Erstellt von Léonie Bécheiraz

[^58]: Erstellt von Léonie Bécheiraz

[^59]: Erstellt von Léonie Bécheiraz

[^60]: Erstellt von Léonie Bécheiraz

[^61]: Erstellt von Léonie Bécheiraz

[^62]: Erstellt von Léonie Bécheiraz

[^63]: Erstellt von Léonie Bécheiraz

[^64]: Erstellt von Christian Heeb

[^65]: Erstellt von Christian Heeb

[^66]: Erstellt von Aaron Studer

[^67]: Erstellt von Aaron Studer

[^68]: Erstellt von Aaron Studer

[^69]: Erstellt von Aaron Studer

[^70]: Erstellt von Aaron Studer

[^71]: Erstellt von Aaron Studer

[^72]: Erstellt von Aaron Studer

[^73]: Erstellt von Luca Gisler

[^74]: Erstellt von Aaron Studer

[^75]: Erstellt von Christian Heeb

[^76]: Erstellt von Christian Heeb

[^77]: Erstellt von Christian Heeb

[^78]: Erstellt von Christian Heeb

[^79]: Erstellt von Christian Heeb

[^80]: Erstellt von Christian Heeb

[^81]: Erstellt von Luca Gisler

[^82]: Erstellt von Luca Gisler

[^83]: Erstellt von Luca Gisler

[^84]: Erstellt von Luca Gisler

[^85]: Erstellt von Luca Gisler

[^86]: Erstellt von Luca Gisler

[^87]: Erstellt von Luca Gisler
