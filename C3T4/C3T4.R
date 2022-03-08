G.JOHNSON
C3T4 - 28-JAN-2022

## Load the packages
library(tidyverse)
#library(readxl)
#library(readr)
library(knitr)
library(ggplot2)
#library(lubridate)
library(arules)
library(arulesViz)
library(plyr)

# load CSV with transactions
Trans <- read.transactions("ElectronidexTransactions2017.csv",
                  format = "basket",
                  sep = ",",
                  rm.duplicates = FALSE)



# Getting to know the transactional data
summary(Trans)
#transactions as itemMatrix in sparse format with
#9835 rows (elements/itemsets/transactions) and
#125 columns (items) and a density of 0.03506172 
#
#most frequent items:
#  iMac                HP Laptop CYBERPOWER Gamer Desktop 
#2519                     1909                     1809 
#Apple Earpods        Apple MacBook Air                  (Other) 
#1715                     1530                    33622 
#
#element (itemset/transaction) length distribution:
#  sizes
#0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16 
#2 2163 1647 1294 1021  856  646  540  439  353  247  171  119   77   72   56   41 
#17   18   19   20   21   22   23   25   26   27   29   30 
#26   20   10   10   10    5    3    1    1    3    1    1 
#
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   2.000   3.000   4.383   6.000  30.000 
#
#includes extended item information - examples:
#  labels
#1 1TB Portable External Hard Drive
#2 2TB Portable External Hard Drive
#3                   3-Button Mouse

#Density - 9835 X 125 X 0.0035
inspect(Trans) # View of the transactions
length(Trans) # Number of Transactions
size(Trans) # number of items per transaction
LIST(Trans) # Lists the transactions by conversion
itemLabels(Trans) # To see the item labels


#Visualize items in dataset
itemFrequencyPlot(Trans, topN = 20, type = "absolute") # See how frequent an item is purchased; Top 20
image(Trans)
image(sample(Trans, 20))


# Create some rules
# use the Apriori algorithm in Arules library to mine frequent itemsets and association rules. 
#The algorithm employs level-wise search for frequent itemsets.
#Pass supp=0.001 and conf=0.8 to return all the rules that have a support of at least 0.1% and confidence of at least 80%.
#We sort the rules by decreasing confidence.

rules <- apriori(Trans, parameter = list(supp=0.02, conf=0.5))
rules <- sort(rules, by="confidence", decreasing = TRUE)
summary(rules)
#set of 1 rule

#Increase 'conf' value to get 'strong' rules
rules2 <- apriori(Trans, parameter = list(supp=0.001, conf=0.8))
rules2 <- sort(rules2, by="confidence", decreasing = TRUE)
summary(rules2)
#set of 635 rules
#
#rule length distribution (lhs + rhs):sizes
#3   4   5   6 
#30 303 269  33 
#
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3.00    4.00    4.00    4.48    5.00    6.00 
#
#summary of quality measures:
#  support           confidence        coverage             lift            count      
#Min.   :0.001017   Min.   :0.8000   Min.   :0.001017   Min.   : 3.123   Min.   :10.00  
#1st Qu.:0.001017   1st Qu.:0.8333   1st Qu.:0.001220   1st Qu.: 3.304   1st Qu.:10.00  
#Median :0.001118   Median :0.8462   Median :0.001322   Median : 3.579   Median :11.00  
#Mean   :0.001268   Mean   :0.8661   Mean   :0.001475   Mean   : 4.042   Mean   :12.47  
#3rd Qu.:0.001322   3rd Qu.:0.9091   3rd Qu.:0.001627   3rd Qu.: 4.416   3rd Qu.:13.00  
#Max.   :0.005287   Max.   :1.0000   Max.   :0.006507   Max.   :10.069   Max.   :52.00  
#
#mining info:
#  data ntransactions support confidence                                                              call
#Trans          9835   0.001        0.8 apriori(data = Trans, parameter = list(supp = 0.001, conf = 0.8))


#Increase 'conf' value to get 'strong' rules
rules3 <- apriori(Trans, parameter = list(supp=0.001, conf=0.85))
rules3 <- sort(rules3, by="confidence", decreasing = TRUE)
summary(rules3)
#set of 301 rules

rules4 <- apriori(Trans, parameter = list(supp=0.001, conf=0.9))
rules4 <- sort(rules4, by="confidence", decreasing = TRUE)
summary(rules4)
#set of 197 rules

rules5 <- apriori(Trans, parameter = list(supp=0.001, conf=0.95))
rules5 <- sort(rules5, by="confidence", decreasing = TRUE)
summary(rules5)
#set of 43 rules

rules6 <- apriori(Trans, parameter = list(supp=0.1, conf=0.001))
rules6 <- sort(rules6, by="confidence", decreasing = TRUE)
summary(rules6)
#set of 10 rules

### Are there redundant rules? True means Redunant / False means none
is.redundant(rules) #no
is.redundant(rules2) #yes
is.redundant(rules3) #yes
is.redundant(rules4) #yes
is.redundant(rules5) #yes
is.redundant(rules6) #no

#prune redundant
rules2 <- rules2[!is.redundant(rules2)]
rules3 <- rules3[!is.redundant(rules3)]
rules4 <- rules4[!is.redundant(rules4)]
rules5 <- rules5[!is.redundant(rules5)]

arules::inspect(rules2) #inspect
arules::inspect(rules3) #inspect
arules::inspect(rules4) #inspect
arules::inspect(rules5) #inspect

is.redundant(rules2) #double check redundants are removed
is.redundant(rules3) #double check redundants are removed
is.redundant(rules4) #double check redundants are removed
is.redundant(rules5) #double check redundants are removed


#Print top 10 rules sorted by support
top.support <- sort(rules2, decreasing = TRUE, na.last = NA, by = "support")
inspect(head(top.support, 20))
#lhs                                                                                                  rhs                          support     confidence coverage    lift     count
#[1]  {Brother Printer, Halter Acrylic Monitor Stand}                                                 => {iMac}                    0.001118454 1          0.001118454 3.904327 11   
#[2]  {ASUS Monitor, Mackie CR Speakers, ViewSonic Monitor}                                           => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[3]  {Apple Magic Keyboard, Rii LED Gaming Keyboard & Mouse Combo, ViewSonic Monitor}                => {iMac}                    0.001728521 1          0.001728521 3.904327 17   
#[4]  {ASUS Monitor, Koss Home Headphones, Microsoft Office Home and Student 2016}                    => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[5]  {Acer Aspire, Koss Home Headphones, ViewSonic Monitor}                                          => {HP Laptop}               0.001220132 1          0.001220132 5.151912 12   
#[6]  {Dell Desktop, Koss Home Headphones, ViewSonic Monitor}                                         => {HP Laptop}               0.001118454 1          0.001118454 5.151912 11   
#[7]  {ASUS 2 Monitor, Dell Desktop, Logitech Keyboard}                                               => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[8]  {Alienware Laptop, ASUS Desktop, Lenovo Desktop Computer}                                       => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[9]  {Brother Printer, Dell Desktop, Epson Printer}                                                  => {iMac}                    0.001118454 1          0.001118454 3.904327 11   
#[10] {Apple Magic Keyboard, Brother Printer, ViewSonic Monitor}                                      => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[11] {ASUS Desktop, Dell Desktop, Microsoft Office Home and Student 2016}                            => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[12] {Intel Desktop, iPad Pro, Microsoft Office Home and Student 2016}                               => {iMac}                    0.001118454 1          0.001118454 3.904327 11   
#[13] {Acer Aspire, ASUS 2 Monitor, Intel Desktop}                                                    => {HP Laptop}               0.001016777 1          0.001016777 5.151912 10   
#[14] {ASUS Monitor, Intel Desktop, ViewSonic Monitor}                                                => {Lenovo Desktop Computer} 0.001118454 1          0.001118454 6.754808 11   
#[15] {ASUS 2 Monitor, Computer Game, Dell Desktop}                                                   => {HP Laptop}               0.001016777 1          0.001016777 5.151912 10   
#[16] {Dell Desktop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                               => {ViewSonic Monitor}       0.001118454 1          0.001118454 9.064516 11   
#[17] {Acer Aspire, Apple MacBook Pro, HP Black & Tri-color Ink, HP Laptop}                           => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[18] {Dell Desktop, iMac, Lenovo Desktop Computer, Logitech MK270 Wireless Keyboard and Mouse Combo} => {HP Laptop}               0.001118454 1          0.001118454 5.151912 11   
#[19] {Acer Desktop, Dell Desktop, HDMI Cable 6ft, iMac}                                              => {HP Laptop}               0.001321810 1          0.001321810 5.151912 13   
#[20] {Dell Desktop, Etekcity Power Extension Cord Cable, Lenovo Desktop Computer, ViewSonic Monitor} => {iMac}                    0.001220132 1          0.001220132 3.904327 12   

top.support <- sort(rules, decreasing = TRUE, na.last = NA, by = "support")
inspect(head(top.support, 20))
#lhs                                          rhs   support    confidence coverage   lift     count
#[1] {HP Laptop, Lenovo Desktop Computer} => {iMac} 0.02308083 0.5        0.04616167 1.952164 227  

top.support <- sort(rules3, decreasing = TRUE, na.last = NA, by = "support")
inspect(head(top.support, 20))
#    lhs                                                                                                   rhs                 support     confidence coverage    lift     count
#[1]  {Acer Aspire, Dell Desktop, iMac, ViewSonic Monitor}                                               => {HP Laptop}         0.003762074 0.9024390  0.004168785 4.649286 37   
#[2]  {Dell Desktop, Mackie CR Speakers}                                                                 => {iMac}              0.002643620 0.8666667  0.003050330 3.383750 26   
#[3]  {Computer Game, Dell Desktop, ViewSonic Monitor}                                                   => {HP Laptop}         0.002643620 0.8666667  0.003050330 4.464990 26   
#[4]  {Dell KM117 Wireless Keyboard & Mouse, iPhone Charger Cable}                                       => {Apple MacBook Air} 0.002033554 0.9523810  0.002135231 6.122004 20   
#[5]  {ASUS Monitor, Dell Desktop, Lenovo Desktop Computer, ViewSonic Monitor}                           => {iMac}              0.001931876 0.9047619  0.002135231 3.532486 19   
#[6]  {Dell Desktop, Lenovo Desktop Computer, Microsoft Office Home and Student 2016, ViewSonic Monitor} => {iMac}              0.001931876 0.8636364  0.002236909 3.371919 19   
#[7]  {Acer Aspire, Acer Desktop, Dell Desktop, ViewSonic Monitor}                                       => {HP Laptop}         0.001931876 0.8636364  0.002236909 4.449379 19   
#[8]  {Dell Desktop, Lenovo Desktop Computer, Samsung Monitor, ViewSonic Monitor}                        => {iMac}              0.001830198 0.9473684  0.001931876 3.698836 18   
#[9]  {Alienware Laptop, LG Monitor}                                                                     => {HP Laptop}         0.001830198 0.8571429  0.002135231 4.415925 18   
#[10] {ASUS 2 Monitor, Intel Desktop, ViewSonic Monitor}                                                 => {HP Laptop}         0.001830198 0.8571429  0.002135231 4.415925 18   
#[11] {Apple MacBook Air, Dell Desktop, Microsoft Office Home and Student 2016}                          => {iMac}              0.001830198 0.8571429  0.002135231 3.346566 18   
#[12] {Acer Desktop, Dell Desktop, HP Monitor, Lenovo Desktop Computer}                                  => {HP Laptop}         0.001830198 0.8571429  0.002135231 4.415925 18   
#[13] {Apple Magic Keyboard, Rii LED Gaming Keyboard & Mouse Combo, ViewSonic Monitor}                   => {iMac}              0.001728521 1.0000000  0.001728521 3.904327 17   
#[14] {ASUS 2 Monitor, Dell Desktop, Lenovo Desktop Computer, Microsoft Office Home and Student 2016}    => {iMac}              0.001728521 0.9444444  0.001830198 3.687420 17   
#[15] {Epson Printer, iMac, Lenovo Desktop Computer, ViewSonic Monitor}                                  => {HP Laptop}         0.001728521 0.8947368  0.001931876 4.609605 17   
#[16] {Dell Desktop, HP Monitor, Lenovo Desktop Computer, ViewSonic Monitor}                             => {HP Laptop}         0.001728521 0.8947368  0.001931876 4.609605 17   
#[17] {Belkin Mouse Pad, Otium Wireless Sports Bluetooth Headphone}                                      => {HP Laptop}         0.001728521 0.8500000  0.002033554 4.379125 17   
#[18] {Dell Desktop, Intel Desktop, Microsoft Office Home and Student 2016}                              => {iMac}              0.001728521 0.8500000  0.002033554 3.318678 17   
#[19] {Apple Magic Keyboard, ASUS Chromebook, Dell Desktop}                                              => {iMac}              0.001728521 0.8500000  0.002033554 3.318678 17   
#[20] {Acer Aspire, Backlit LED Gaming Keyboard, ViewSonic Monitor}                                      => {HP Laptop}         0.001728521 0.8500000  0.002033554 4.379125 17     

top.support <- sort(rules4, decreasing = TRUE, na.last = NA, by = "support")
inspect(head(top.support, 20))
#     lhs                                                                                                               rhs                 support     confidence coverage    lift     count
#[1]  {Acer Aspire, Dell Desktop, iMac, ViewSonic Monitor}                                                           => {HP Laptop}         0.003762074 0.9024390  0.004168785 4.649286 37   
#[2]  {Dell KM117 Wireless Keyboard & Mouse, iPhone Charger Cable}                                                   => {Apple MacBook Air} 0.002033554 0.9523810  0.002135231 6.122004 20   
#[3]  {ASUS Monitor, Dell Desktop, Lenovo Desktop Computer, ViewSonic Monitor}                                       => {iMac}              0.001931876 0.9047619  0.002135231 3.532486 19   
#[4]  {Dell Desktop, Lenovo Desktop Computer, Samsung Monitor, ViewSonic Monitor}                                    => {iMac}              0.001830198 0.9473684  0.001931876 3.698836 18   
#[5]  {Apple Magic Keyboard, Rii LED Gaming Keyboard & Mouse Combo, ViewSonic Monitor}                               => {iMac}              0.001728521 1.0000000  0.001728521 3.904327 17   
#[6]  {ASUS 2 Monitor, Dell Desktop, Lenovo Desktop Computer, Microsoft Office Home and Student 2016}                => {iMac}              0.001728521 0.9444444  0.001830198 3.687420 17   
#[7]  {Etekcity Power Extension Cord Cable, HP Laptop, Lenovo Desktop Computer, ViewSonic Monitor}                   => {iMac}              0.001626843 1.0000000  0.001626843 3.904327 16   
#[8]  {ASUS Desktop, Dell Desktop, iPad Pro}                                                                         => {iMac}              0.001525165 0.9375000  0.001626843 3.660307 15   
#[9]  {3-Button Mouse, Acer Desktop, ASUS Monitor}                                                                   => {HP Laptop}         0.001525165 0.9375000  0.001626843 4.829917 15   
#[10] {Acer Aspire, Apple Magic Keyboard, Dell Desktop, ViewSonic Monitor}                                           => {HP Laptop}         0.001423488 1.0000000  0.001423488 5.151912 14   
#[11] {Dell Desktop, Mackie CR Speakers, ViewSonic Monitor}                                                          => {HP Laptop}         0.001423488 0.9333333  0.001525165 4.808451 14   
#[12] {Dell Desktop, Mackie CR Speakers, ViewSonic Monitor}                                                          => {iMac}              0.001423488 0.9333333  0.001525165 3.644039 14   
#[13] {HP Laptop, Lenovo Desktop Computer, Logitech MK550 Wireless Wave Keyboard and Mouse Combo, ViewSonic Monitor} => {iMac}              0.001423488 0.9333333  0.001525165 3.644039 14   
#[14] {Dell Desktop, HP Monitor, iMac, Lenovo Desktop Computer, ViewSonic Monitor}                                   => {HP Laptop}         0.001423488 0.9333333  0.001525165 4.808451 14   
#[15] {Acer Desktop, Dell Desktop, HDMI Cable 6ft, iMac}                                                             => {HP Laptop}         0.001321810 1.0000000  0.001321810 5.151912 13   
#[16] {Acer Monitor, Cambridge Bluetooth Speaker, Dell Desktop}                                                      => {HP Laptop}         0.001321810 0.9285714  0.001423488 4.783918 13   
#[17] {ASUS Monitor, Brother Printer, Dell Desktop}                                                                  => {iMac}              0.001321810 0.9285714  0.001423488 3.625447 13   
#[18] {Apple Earpods, Apple Magic Keyboard, ASUS Monitor}                                                            => {HP Laptop}         0.001321810 0.9285714  0.001423488 4.783918 13   
#[19] {HP Laptop, Lenovo Desktop Computer, Mackie CR Speakers, ViewSonic Monitor}                                    => {iMac}              0.001321810 0.9285714  0.001423488 3.625447 13   
#[20] {iMac, Lenovo Desktop Computer, Mackie CR Speakers, ViewSonic Monitor}                                         => {HP Laptop}         0.001321810 0.9285714  0.001423488 4.783918 13    

top.support <- sort(rules5, decreasing = TRUE, na.last = NA, by = "support")
inspect(head(top.support, 40))
#    lhs                                                                                                rhs                       support     confidence coverage    lift     count
#[1]  {Dell KM117 Wireless Keyboard & Mouse, iPhone Charger Cable}                                    => {Apple MacBook Air}       0.002033554 0.952381   0.002135231 6.122004 20   
#[2]  {Apple Magic Keyboard, Rii LED Gaming Keyboard & Mouse Combo, ViewSonic Monitor}                => {iMac}                    0.001728521 1.000000   0.001728521 3.904327 17   
#[3]  {Etekcity Power Extension Cord Cable, HP Laptop, Lenovo Desktop Computer, ViewSonic Monitor}    => {iMac}                    0.001626843 1.000000   0.001626843 3.904327 16   
#[4]  {Acer Aspire, Apple Magic Keyboard, Dell Desktop, ViewSonic Monitor}                            => {HP Laptop}               0.001423488 1.000000   0.001423488 5.151912 14   
#[5]  {Acer Desktop, Dell Desktop, HDMI Cable 6ft, iMac}                                              => {HP Laptop}               0.001321810 1.000000   0.001321810 5.151912 13   
#[6]  {Acer Aspire, Koss Home Headphones, ViewSonic Monitor}                                          => {HP Laptop}               0.001220132 1.000000   0.001220132 5.151912 12   
#[7]  {Dell Desktop, Etekcity Power Extension Cord Cable, Lenovo Desktop Computer, ViewSonic Monitor} => {iMac}                    0.001220132 1.000000   0.001220132 3.904327 12   
#[8]  {Apple Magic Keyboard, ASUS Monitor, HP Laptop, Microsoft Office Home and Student 2016}         => {iMac}                    0.001220132 1.000000   0.001220132 3.904327 12   
#[9]  {Brother Printer, Halter Acrylic Monitor Stand}                                                 => {iMac}                    0.001118454 1.000000   0.001118454 3.904327 11   
#[10] {Dell Desktop, Koss Home Headphones, ViewSonic Monitor}                                         => {HP Laptop}               0.001118454 1.000000   0.001118454 5.151912 11   
#[11] {Brother Printer, Dell Desktop, Epson Printer}                                                  => {iMac}                    0.001118454 1.000000   0.001118454 3.904327 11   
#[12] {Intel Desktop, iPad Pro, Microsoft Office Home and Student 2016}                               => {iMac}                    0.001118454 1.000000   0.001118454 3.904327 11   
#[13] {ASUS Monitor, Intel Desktop, ViewSonic Monitor}                                                => {Lenovo Desktop Computer} 0.001118454 1.000000   0.001118454 6.754808 11   
#[14] {Dell Desktop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                               => {ViewSonic Monitor}       0.001118454 1.000000   0.001118454 9.064516 11   
#[15] {Dell Desktop, iMac, Lenovo Desktop Computer, Logitech MK270 Wireless Keyboard and Mouse Combo} => {HP Laptop}               0.001118454 1.000000   0.001118454 5.151912 11   
#[16] {AOC Monitor, Dell Desktop, Lenovo Desktop Computer, ViewSonic Monitor}                         => {iMac}                    0.001118454 1.000000   0.001118454 3.904327 11   
#[17] {Acer Aspire, Computer Game, Dell Desktop, ViewSonic Monitor}                                   => {HP Laptop}               0.001118454 1.000000   0.001118454 5.151912 11   
#[18] {Apple Magic Keyboard, ASUS Monitor, Dell Desktop, HP Monitor}                                  => {HP Laptop}               0.001118454 1.000000   0.001118454 5.151912 11   
#[19] {Acer Desktop, HP Laptop, HP Monitor, Lenovo Desktop Computer, ViewSonic Monitor}               => {iMac}                    0.001118454 1.000000   0.001118454 3.904327 11   
#[20] {ASUS Monitor, Mackie CR Speakers, ViewSonic Monitor}                                           => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   

top.support <- sort(rules6, decreasing = TRUE, na.last = NA, by = "support")
inspect(head(top.support, 20))
#lhs    rhs                        support   confidence coverage lift count
#[1]  {}  => {iMac}                     0.2561261 0.2561261  1        1    2519 
#[2]  {}  => {HP Laptop}                0.1941027 0.1941027  1        1    1909 
#[3]  {}  => {CYBERPOWER Gamer Desktop} 0.1839349 0.1839349  1        1    1809 
#[4]  {}  => {Apple Earpods}            0.1743772 0.1743772  1        1    1715 
#[5]  {}  => {Apple MacBook Air}        0.1555669 0.1555669  1        1    1530 
#[6]  {}  => {Lenovo Desktop Computer}  0.1480427 0.1480427  1        1    1456 
#[7]  {}  => {Dell Desktop}             0.1340112 0.1340112  1        1    1318 
#[8]  {}  => {Apple MacBook Pro}        0.1105236 0.1105236  1        1    1087 
#[9]  {}  => {ViewSonic Monitor}        0.1103203 0.1103203  1        1    1085 
#[10] {}  => {Acer Desktop}             0.1018810 0.1018810  1        1    1002 

#Print top 10 rules sorted by confidence
top.confidence <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 20))
#lhs                                          rhs    support    confidence coverage   lift     count
#[1] {HP Laptop, Lenovo Desktop Computer} => {iMac} 0.02308083 0.5        0.04616167 1.952164 227  

top.confidence <- sort(rules2, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 30))
#lhs                                                                                                rhs                       support     confidence coverage    lift     count
#[1]  {Brother Printer, Halter Acrylic Monitor Stand}                                                 => {iMac}                    0.001118454 1          0.001118454 3.904327 11   
#[2]  {ASUS Monitor, Mackie CR Speakers, ViewSonic Monitor}                                           => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[3]  {Apple Magic Keyboard, Rii LED Gaming Keyboard & Mouse Combo, ViewSonic Monitor}                => {iMac}                    0.001728521 1          0.001728521 3.904327 17   
#[4]  {ASUS Monitor, Koss Home Headphones, Microsoft Office Home and Student 2016}                    => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[5]  {Acer Aspire, Koss Home Headphones, ViewSonic Monitor}                                          => {HP Laptop}               0.001220132 1          0.001220132 5.151912 12   
#[6]  {Dell Desktop, Koss Home Headphones, ViewSonic Monitor}                                         => {HP Laptop}               0.001118454 1          0.001118454 5.151912 11   
#[7]  {ASUS 2 Monitor, Dell Desktop, Logitech Keyboard}                                               => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[8]  {Alienware Laptop, ASUS Desktop, Lenovo Desktop Computer}                                       => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[9]  {Brother Printer, Dell Desktop, Epson Printer}                                                  => {iMac}                    0.001118454 1          0.001118454 3.904327 11   
#[10] {Apple Magic Keyboard, Brother Printer, ViewSonic Monitor}                                      => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[11] {ASUS Desktop, Dell Desktop, Microsoft Office Home and Student 2016}                            => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[12] {Intel Desktop, iPad Pro, Microsoft Office Home and Student 2016}                               => {iMac}                    0.001118454 1          0.001118454 3.904327 11   
#[13] {Acer Aspire, ASUS 2 Monitor, Intel Desktop}                                                    => {HP Laptop}               0.001016777 1          0.001016777 5.151912 10   
#[14] {ASUS Monitor, Intel Desktop, ViewSonic Monitor}                                                => {Lenovo Desktop Computer} 0.001118454 1          0.001118454 6.754808 11   
#[15] {ASUS 2 Monitor, Computer Game, Dell Desktop}                                                   => {HP Laptop}               0.001016777 1          0.001016777 5.151912 10   
#[16] {Dell Desktop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                               => {ViewSonic Monitor}       0.001118454 1          0.001118454 9.064516 11   
#[17] {Acer Aspire, Apple MacBook Pro, HP Black & Tri-color Ink, HP Laptop}                           => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[18] {Dell Desktop, iMac, Lenovo Desktop Computer, Logitech MK270 Wireless Keyboard and Mouse Combo} => {HP Laptop}               0.001118454 1          0.001118454 5.151912 11   
#[19] {Acer Desktop, Dell Desktop, HDMI Cable 6ft, iMac}                                              => {HP Laptop}               0.001321810 1          0.001321810 5.151912 13   
#[20] {Dell Desktop, Etekcity Power Extension Cord Cable, Lenovo Desktop Computer, ViewSonic Monitor} => {iMac}                    0.001220132 1          0.001220132 3.904327 12   
#[21] {Etekcity Power Extension Cord Cable, HP Laptop, Lenovo Desktop Computer, ViewSonic Monitor}    => {iMac}                    0.001626843 1          0.001626843 3.904327 16   
#[22] {Apple Magic Keyboard, ASUS Desktop, Dell Desktop, Lenovo Desktop Computer}                     => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[23] {AOC Monitor, ASUS Monitor, HP Laptop, ViewSonic Monitor}                                       => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[24] {AOC Monitor, Dell Desktop, Lenovo Desktop Computer, ViewSonic Monitor}                         => {iMac}                    0.001118454 1          0.001118454 3.904327 11   
#[25] {Acer Aspire, Computer Game, Dell Desktop, ViewSonic Monitor}                                   => {HP Laptop}               0.001118454 1          0.001118454 5.151912 11   
#[26] {Epson Printer, HP Monitor, Lenovo Desktop Computer, ViewSonic Monitor}                         => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[27] {Apple Magic Keyboard, ASUS Monitor, HP Laptop, LG Monitor}                                     => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[28] {Apple Magic Keyboard, ASUS Monitor, Dell Desktop, Microsoft Office Home and Student 2016}      => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[29] {Apple Magic Keyboard, ASUS Monitor, HP Laptop, Microsoft Office Home and Student 2016}         => {iMac}                    0.001220132 1          0.001220132 3.904327 12   
#[30] {Apple Magic Keyboard, ASUS Monitor, Dell Desktop, HP Monitor}                                  => {HP Laptop}               0.001118454 1          0.001118454 5.151912 11   
 

top.confidence <- sort(rules3, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 50))
#    lhs                                                                                                               rhs                       support     confidence coverage    lift     count
#[1]  {Brother Printer, Halter Acrylic Monitor Stand}                                                                => {iMac}                    0.001118454 1.0000000  0.001118454 3.904327 11   
#[2]  {ASUS Monitor, Mackie CR Speakers, ViewSonic Monitor}                                                          => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[3]  {Apple Magic Keyboard, Rii LED Gaming Keyboard & Mouse Combo, ViewSonic Monitor}                               => {iMac}                    0.001728521 1.0000000  0.001728521 3.904327 17   
#[4]  {ASUS Monitor, Koss Home Headphones, Microsoft Office Home and Student 2016}                                   => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[5]  {Acer Aspire, Koss Home Headphones, ViewSonic Monitor}                                                         => {HP Laptop}               0.001220132 1.0000000  0.001220132 5.151912 12   
#[6]  {Dell Desktop, Koss Home Headphones, ViewSonic Monitor}                                                        => {HP Laptop}               0.001118454 1.0000000  0.001118454 5.151912 11   
#[7]  {ASUS 2 Monitor, Dell Desktop, Logitech Keyboard}                                                              => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[8]  {Alienware Laptop, ASUS Desktop, Lenovo Desktop Computer}                                                      => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[9]  {Brother Printer, Dell Desktop, Epson Printer}                                                                 => {iMac}                    0.001118454 1.0000000  0.001118454 3.904327 11   
#[10] {Apple Magic Keyboard, Brother Printer, ViewSonic Monitor}                                                     => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[11] {ASUS Desktop, Dell Desktop, Microsoft Office Home and Student 2016}                                           => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[12] {Intel Desktop, iPad Pro, Microsoft Office Home and Student 2016}                                              => {iMac}                    0.001118454 1.0000000  0.001118454 3.904327 11   
#[13] {Acer Aspire, ASUS 2 Monitor, Intel Desktop}                                                                   => {HP Laptop}               0.001016777 1.0000000  0.001016777 5.151912 10   
#[14] {ASUS Monitor, Intel Desktop, ViewSonic Monitor}                                                               => {Lenovo Desktop Computer} 0.001118454 1.0000000  0.001118454 6.754808 11   
#[15] {ASUS 2 Monitor, Computer Game, Dell Desktop}                                                                  => {HP Laptop}               0.001016777 1.0000000  0.001016777 5.151912 10   
#[16] {Dell Desktop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                                              => {ViewSonic Monitor}       0.001118454 1.0000000  0.001118454 9.064516 11   
#[17] {Acer Aspire, Apple MacBook Pro, HP Black & Tri-color Ink, HP Laptop}                                          => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[18] {Dell Desktop, iMac, Lenovo Desktop Computer, Logitech MK270 Wireless Keyboard and Mouse Combo}                => {HP Laptop}               0.001118454 1.0000000  0.001118454 5.151912 11   
#[19] {Acer Desktop, Dell Desktop, HDMI Cable 6ft, iMac}                                                             => {HP Laptop}               0.001321810 1.0000000  0.001321810 5.151912 13   
#[20] {Dell Desktop, Etekcity Power Extension Cord Cable, Lenovo Desktop Computer, ViewSonic Monitor}                => {iMac}                    0.001220132 1.0000000  0.001220132 3.904327 12   
#[21] {Etekcity Power Extension Cord Cable, HP Laptop, Lenovo Desktop Computer, ViewSonic Monitor}                   => {iMac}                    0.001626843 1.0000000  0.001626843 3.904327 16   
#[22] {Apple Magic Keyboard, ASUS Desktop, Dell Desktop, Lenovo Desktop Computer}                                    => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[23] {AOC Monitor, ASUS Monitor, HP Laptop, ViewSonic Monitor}                                                      => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[24] {AOC Monitor, Dell Desktop, Lenovo Desktop Computer, ViewSonic Monitor}                                        => {iMac}                    0.001118454 1.0000000  0.001118454 3.904327 11   
#[25] {Acer Aspire, Computer Game, Dell Desktop, ViewSonic Monitor}                                                  => {HP Laptop}               0.001118454 1.0000000  0.001118454 5.151912 11   
#[26] {Epson Printer, HP Monitor, Lenovo Desktop Computer, ViewSonic Monitor}                                        => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[27] {Apple Magic Keyboard, ASUS Monitor, HP Laptop, LG Monitor}                                                    => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[28] {Apple Magic Keyboard, ASUS Monitor, Dell Desktop, Microsoft Office Home and Student 2016}                     => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[29] {Apple Magic Keyboard, ASUS Monitor, HP Laptop, Microsoft Office Home and Student 2016}                        => {iMac}                    0.001220132 1.0000000  0.001220132 3.904327 12   
#[30] {Apple Magic Keyboard, ASUS Monitor, Dell Desktop, HP Monitor}                                                 => {HP Laptop}               0.001118454 1.0000000  0.001118454 5.151912 11   
#[31] {Acer Desktop, Apple Magic Keyboard, ASUS Monitor, ViewSonic Monitor}                                          => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[32] {Apple Earpods, Backlit LED Gaming Keyboard, CYBERPOWER Gamer Desktop, iMac}                                   => {HP Laptop}               0.001016777 1.0000000  0.001016777 5.151912 10   
#[33] {Acer Desktop, Apple Magic Keyboard, Dell Desktop, HP Monitor}                                                 => {HP Laptop}               0.001016777 1.0000000  0.001016777 5.151912 10   
#[34] {3-Button Mouse, Acer Aspire, Apple Magic Keyboard, CYBERPOWER Gamer Desktop}                                  => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[35] {Acer Aspire, Apple Magic Keyboard, Dell Desktop, ViewSonic Monitor}                                           => {HP Laptop}               0.001423488 1.0000000  0.001423488 5.151912 14   
#[36] {3-Button Mouse, Acer Aspire, Dell Desktop, ViewSonic Monitor}                                                 => {HP Laptop}               0.001016777 1.0000000  0.001016777 5.151912 10   
#[37] {CYBERPOWER Gamer Desktop, Dell Desktop, Samsung Monitor, ViewSonic Monitor}                                   => {iMac}                    0.001016777 1.0000000  0.001016777 3.904327 10   
#[38] {Acer Desktop, HP Laptop, HP Monitor, Lenovo Desktop Computer, ViewSonic Monitor}                              => {iMac}                    0.001118454 1.0000000  0.001118454 3.904327 11   
#[39] {Dell KM117 Wireless Keyboard & Mouse, iPhone Charger Cable}                                                   => {Apple MacBook Air}       0.002033554 0.9523810  0.002135231 6.122004 20   
#[40] {Dell Desktop, Lenovo Desktop Computer, Samsung Monitor, ViewSonic Monitor}                                    => {iMac}                    0.001830198 0.9473684  0.001931876 3.698836 18   
#[41] {ASUS 2 Monitor, Dell Desktop, Lenovo Desktop Computer, Microsoft Office Home and Student 2016}                => {iMac}                    0.001728521 0.9444444  0.001830198 3.687420 17   
#[42] {ASUS Desktop, Dell Desktop, iPad Pro}                                                                         => {iMac}                    0.001525165 0.9375000  0.001626843 3.660307 15   
#[43] {3-Button Mouse, Acer Desktop, ASUS Monitor}                                                                   => {HP Laptop}               0.001525165 0.9375000  0.001626843 4.829917 15   
#[44] {Dell Desktop, Mackie CR Speakers, ViewSonic Monitor}                                                          => {HP Laptop}               0.001423488 0.9333333  0.001525165 4.808451 14   
#[45] {Dell Desktop, Mackie CR Speakers, ViewSonic Monitor}                                                          => {iMac}                    0.001423488 0.9333333  0.001525165 3.644039 14   
#[46] {HP Laptop, Lenovo Desktop Computer, Logitech MK550 Wireless Wave Keyboard and Mouse Combo, ViewSonic Monitor} => {iMac}                    0.001423488 0.9333333  0.001525165 3.644039 14   
#[47] {Dell Desktop, HP Monitor, iMac, Lenovo Desktop Computer, ViewSonic Monitor}                                   => {HP Laptop}               0.001423488 0.9333333  0.001525165 4.808451 14   
#[48] {Acer Monitor, Cambridge Bluetooth Speaker, Dell Desktop}                                                      => {HP Laptop}               0.001321810 0.9285714  0.001423488 4.783918 13   
#[49] {ASUS Monitor, Brother Printer, Dell Desktop}                                                                  => {iMac}                    0.001321810 0.9285714  0.001423488 3.625447 13   
#[50] {Apple Earpods, Apple Magic Keyboard, ASUS Monitor}                                                            => {HP Laptop}               0.001321810 0.9285714  0.001423488 4.783918 13   

top.confidence <- sort(rules4, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 50))
# same as rules3

top.confidence <- sort(rules5, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 43))
#    lhs                                                                                                rhs                       support     confidence coverage    lift     count
#[1]  {Brother Printer, Halter Acrylic Monitor Stand}                                                 => {iMac}                    0.001118454 1.000000   0.001118454 3.904327 11   
#[2]  {ASUS Monitor, Mackie CR Speakers, ViewSonic Monitor}                                           => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[3]  {Apple Magic Keyboard, Rii LED Gaming Keyboard & Mouse Combo, ViewSonic Monitor}                => {iMac}                    0.001728521 1.000000   0.001728521 3.904327 17   
#[4]  {ASUS Monitor, Koss Home Headphones, Microsoft Office Home and Student 2016}                    => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[5]  {Acer Aspire, Koss Home Headphones, ViewSonic Monitor}                                          => {HP Laptop}               0.001220132 1.000000   0.001220132 5.151912 12   
#[6]  {Dell Desktop, Koss Home Headphones, ViewSonic Monitor}                                         => {HP Laptop}               0.001118454 1.000000   0.001118454 5.151912 11   
#[7]  {ASUS 2 Monitor, Dell Desktop, Logitech Keyboard}                                               => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[8]  {Alienware Laptop, ASUS Desktop, Lenovo Desktop Computer}                                       => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[9]  {Brother Printer, Dell Desktop, Epson Printer}                                                  => {iMac}                    0.001118454 1.000000   0.001118454 3.904327 11   
#[10] {Apple Magic Keyboard, Brother Printer, ViewSonic Monitor}                                      => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[11] {ASUS Desktop, Dell Desktop, Microsoft Office Home and Student 2016}                            => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[12] {Intel Desktop, iPad Pro, Microsoft Office Home and Student 2016}                               => {iMac}                    0.001118454 1.000000   0.001118454 3.904327 11   
#[13] {Acer Aspire, ASUS 2 Monitor, Intel Desktop}                                                    => {HP Laptop}               0.001016777 1.000000   0.001016777 5.151912 10   
#[14] {ASUS Monitor, Intel Desktop, ViewSonic Monitor}                                                => {Lenovo Desktop Computer} 0.001118454 1.000000   0.001118454 6.754808 11   
#[15] {ASUS 2 Monitor, Computer Game, Dell Desktop}                                                   => {HP Laptop}               0.001016777 1.000000   0.001016777 5.151912 10   
#[16] {Dell Desktop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                               => {ViewSonic Monitor}       0.001118454 1.000000   0.001118454 9.064516 11   
#[17] {Acer Aspire, Apple MacBook Pro, HP Black & Tri-color Ink, HP Laptop}                           => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[18] {Dell Desktop, iMac, Lenovo Desktop Computer, Logitech MK270 Wireless Keyboard and Mouse Combo} => {HP Laptop}               0.001118454 1.000000   0.001118454 5.151912 11   
#[19] {Acer Desktop, Dell Desktop, HDMI Cable 6ft, iMac}                                              => {HP Laptop}               0.001321810 1.000000   0.001321810 5.151912 13   
#[20] {Dell Desktop, Etekcity Power Extension Cord Cable, Lenovo Desktop Computer, ViewSonic Monitor} => {iMac}                    0.001220132 1.000000   0.001220132 3.904327 12   
#[21] {Etekcity Power Extension Cord Cable, HP Laptop, Lenovo Desktop Computer, ViewSonic Monitor}    => {iMac}                    0.001626843 1.000000   0.001626843 3.904327 16   
#[22] {Apple Magic Keyboard, ASUS Desktop, Dell Desktop, Lenovo Desktop Computer}                     => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[23] {AOC Monitor, ASUS Monitor, HP Laptop, ViewSonic Monitor}                                       => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[24] {AOC Monitor, Dell Desktop, Lenovo Desktop Computer, ViewSonic Monitor}                         => {iMac}                    0.001118454 1.000000   0.001118454 3.904327 11   
#[25] {Acer Aspire, Computer Game, Dell Desktop, ViewSonic Monitor}                                   => {HP Laptop}               0.001118454 1.000000   0.001118454 5.151912 11   
#[26] {Epson Printer, HP Monitor, Lenovo Desktop Computer, ViewSonic Monitor}                         => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[27] {Apple Magic Keyboard, ASUS Monitor, HP Laptop, LG Monitor}                                     => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[28] {Apple Magic Keyboard, ASUS Monitor, Dell Desktop, Microsoft Office Home and Student 2016}      => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[29] {Apple Magic Keyboard, ASUS Monitor, HP Laptop, Microsoft Office Home and Student 2016}         => {iMac}                    0.001220132 1.000000   0.001220132 3.904327 12   
#[30] {Apple Magic Keyboard, ASUS Monitor, Dell Desktop, HP Monitor}                                  => {HP Laptop}               0.001118454 1.000000   0.001118454 5.151912 11   
#[31] {Acer Desktop, Apple Magic Keyboard, ASUS Monitor, ViewSonic Monitor}                           => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[32] {Apple Earpods, Backlit LED Gaming Keyboard, CYBERPOWER Gamer Desktop, iMac}                    => {HP Laptop}               0.001016777 1.000000   0.001016777 5.151912 10   
#[33] {Acer Desktop, Apple Magic Keyboard, Dell Desktop, HP Monitor}                                  => {HP Laptop}               0.001016777 1.000000   0.001016777 5.151912 10   
#[34] {3-Button Mouse, Acer Aspire, Apple Magic Keyboard, CYBERPOWER Gamer Desktop}                   => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[35] {Acer Aspire, Apple Magic Keyboard, Dell Desktop, ViewSonic Monitor}                            => {HP Laptop}               0.001423488 1.000000   0.001423488 5.151912 14   
#[36] {3-Button Mouse, Acer Aspire, Dell Desktop, ViewSonic Monitor}                                  => {HP Laptop}               0.001016777 1.000000   0.001016777 5.151912 10   
#[37] {CYBERPOWER Gamer Desktop, Dell Desktop, Samsung Monitor, ViewSonic Monitor}                    => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[38] {Acer Desktop, HP Laptop, HP Monitor, Lenovo Desktop Computer, ViewSonic Monitor}               => {iMac}                    0.001118454 1.000000   0.001118454 3.904327 11   
#[39] {Dell KM117 Wireless Keyboard & Mouse, iPhone Charger Cable}                                    => {Apple MacBook Air}       0.002033554 0.952381   0.002135231 6.122004 20   

top.confidence <- sort(rules6, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 10))
#10 rules with no LHS
 

#Print top 10 rules sorted by lift
top.lift <- sort(rules, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 20))
#lhs                                     rhs    support    confidence coverage   lift     count
#[1] {HP Laptop, Lenovo Desktop Computer} => {iMac} 0.02308083 0.5        0.04616167 1.952164 227  

top.lift <- sort(rules2, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 20))
#lhs                                                                                                    rhs                          support     confidence coverage     lift      count
#[1]  {Apple MacBook Pro, HP Black & Tri-color Ink, HP Laptop, iMac}                                    => {Acer Aspire}             0.001016777 0.8333333  0.001220132  10.068591 10   
#[2]  {Dell Desktop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                                 => {ViewSonic Monitor}       0.001118454 1.0000000  0.001118454  9.064516  11   
#[3]  {Dell Desktop, Lenovo Desktop Computer, Mackie CR Speakers}                                       => {ViewSonic Monitor}       0.001220132 0.9230769  0.001321810  8.367246  12   
#[4]  {Dell Desktop, Etekcity Power Extension Cord Cable, HP Laptop, iMac, Lenovo Desktop Computer}     => {ViewSonic Monitor}       0.001118454 0.9166667  0.001220132  8.309140  11   
#[5]  {Acer Aspire, Apple Earpods, HP Laptop, HP Monitor}                                               => {ViewSonic Monitor}       0.001016777 0.9090909  0.001118454  8.240469  10   
#[6]  {HP Laptop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                                    => {ViewSonic Monitor}       0.001321810 0.8666667  0.001525165  7.855914  13   
#[7]  {Acer Aspire, Dell Desktop, Epson Printer, HP Laptop}                                             => {ViewSonic Monitor}       0.001220132 0.8571429  0.001423488  7.769585  12   
#[8]  {Acer Aspire, ASUS Chromebook, Dell Desktop, HP Laptop}                                           => {ViewSonic Monitor}       0.001220132 0.8571429  0.001423488  7.769585  12   
#[9]  {Dell Desktop, HP Laptop, iMac, Mackie CR Speakers}                                               => {ViewSonic Monitor}       0.001321810 0.8125000  0.001626843  7.364919  13   
#[10] {Acer Aspire, HP Laptop, Koss Home Headphones}                                                    => {ViewSonic Monitor}       0.001220132 0.8000000  0.001525165  7.251613  12   
#[11] {Belkin Mouse Pad, Microsoft Office Home and Student 2016, Rii LED Gaming Keyboard & Mouse Combo} => {Dell Desktop}            0.001016777 0.9090909  0.001118454  6.783694  10   
#[12] {ASUS 2 Monitor, HP Laptop, Lenovo Desktop Computer, Microsoft Office Home and Student 2016}      => {Dell Desktop}            0.001016777 0.9090909  0.001118454  6.783694  10   
#[13] {ASUS Chromebook, CYBERPOWER Gamer Desktop, HP Laptop, iMac, Lenovo Desktop Computer}             => {Dell Desktop}            0.001016777 0.9090909  0.001118454  6.783694  10   
#[14] {ASUS Monitor, Intel Desktop, ViewSonic Monitor}                                                  => {Lenovo Desktop Computer} 0.001118454 1.0000000  0.001118454  6.754808  11   
#[15] {Acer Monitor, Cambridge Bluetooth Speaker, HP Laptop}                                            => {Dell Desktop}            0.001321810 0.8666667  0.001525165  6.467122  13   
#[16] {HP Laptop, HP Monitor, Logitech MK270 Wireless Keyboard and Mouse Combo}                         => {Dell Desktop}            0.001220132 0.8571429  0.001423488  6.396055  12   
#[17] {Apple MacBook Pro, HP Laptop, iMac, Lenovo Desktop Computer, ViewSonic Monitor}                  => {Dell Desktop}            0.001220132 0.8571429  0.001423488  6.396055  12   
#[18] {Acer Aspire, HP Monitor, Logitech MK270 Wireless Keyboard and Mouse Combo}                       => {Dell Desktop}            0.001118454 0.8461538  0.001321810  6.314054  11   
#[19] {Acer Desktop, HDMI Cable 6ft, HP Laptop, Lenovo Desktop Computer}                                => {Dell Desktop}            0.001118454 0.8461538  0.001321810  6.314054  11   
#[20] {ASUS Monitor, HP Laptop, iMac, Lenovo Desktop Computer, Microsoft Office Home and Student 2016}  => {Dell Desktop}            0.001118454 0.8461538  0.001321810  6.314054  11   
 
top.lift <- sort(rules3, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 20))
#    lhs                                                                                                  rhs                       support     confidence coverage    lift     count
#[1]  {Dell Desktop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                                 => {ViewSonic Monitor}       0.001118454 1.0000000  0.001118454 9.064516 11   
#[2]  {Dell Desktop, Lenovo Desktop Computer, Mackie CR Speakers}                                       => {ViewSonic Monitor}       0.001220132 0.9230769  0.001321810 8.367246 12   
#[3]  {Dell Desktop, Etekcity Power Extension Cord Cable, HP Laptop, iMac, Lenovo Desktop Computer}     => {ViewSonic Monitor}       0.001118454 0.9166667  0.001220132 8.309140 11   
#[4]  {Acer Aspire, Apple Earpods, HP Laptop, HP Monitor}                                               => {ViewSonic Monitor}       0.001016777 0.9090909  0.001118454 8.240469 10   
#[5]  {HP Laptop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                                    => {ViewSonic Monitor}       0.001321810 0.8666667  0.001525165 7.855914 13   
#[6]  {Acer Aspire, Dell Desktop, Epson Printer, HP Laptop}                                             => {ViewSonic Monitor}       0.001220132 0.8571429  0.001423488 7.769585 12   
#[7]  {Acer Aspire, ASUS Chromebook, Dell Desktop, HP Laptop}                                           => {ViewSonic Monitor}       0.001220132 0.8571429  0.001423488 7.769585 12   
#[8]  {Belkin Mouse Pad, Microsoft Office Home and Student 2016, Rii LED Gaming Keyboard & Mouse Combo} => {Dell Desktop}            0.001016777 0.9090909  0.001118454 6.783694 10   
#[9]  {ASUS 2 Monitor, HP Laptop, Lenovo Desktop Computer, Microsoft Office Home and Student 2016}      => {Dell Desktop}            0.001016777 0.9090909  0.001118454 6.783694 10   
#[10] {ASUS Chromebook, CYBERPOWER Gamer Desktop, HP Laptop, iMac, Lenovo Desktop Computer}             => {Dell Desktop}            0.001016777 0.9090909  0.001118454 6.783694 10   
#[11] {ASUS Monitor, Intel Desktop, ViewSonic Monitor}                                                  => {Lenovo Desktop Computer} 0.001118454 1.0000000  0.001118454 6.754808 11   
#[12] {Acer Monitor, Cambridge Bluetooth Speaker, HP Laptop}                                            => {Dell Desktop}            0.001321810 0.8666667  0.001525165 6.467122 13   
#[13] {HP Laptop, HP Monitor, Logitech MK270 Wireless Keyboard and Mouse Combo}                         => {Dell Desktop}            0.001220132 0.8571429  0.001423488 6.396055 12   
#[14] {Apple MacBook Pro, HP Laptop, iMac, Lenovo Desktop Computer, ViewSonic Monitor}                  => {Dell Desktop}            0.001220132 0.8571429  0.001423488 6.396055 12   
#[15] {ASUS 2 Monitor, Dell Desktop, HDMI Cable 6ft}                                                    => {Lenovo Desktop Computer} 0.001118454 0.9166667  0.001220132 6.191907 11   
#[16] {ASUS Monitor, Dell Desktop, iMac, Panasonic In-Ear Headphone}                                    => {Lenovo Desktop Computer} 0.001118454 0.9166667  0.001220132 6.191907 11   
#[17] {HP Wireless Mouse, Logitech MK550 Wireless Wave Keyboard and Mouse Combo}                        => {Lenovo Desktop Computer} 0.001016777 0.9090909  0.001118454 6.140734 10   
#[18] {Acer Desktop, iMac, Panasonic In-Ear Headphone, Samsung Monitor}                                 => {Lenovo Desktop Computer} 0.001016777 0.9090909  0.001118454 6.140734 10   
#[19] {Apple Magic Keyboard, ASUS 2 Monitor, HP Laptop, Intel Desktop}                                  => {Lenovo Desktop Computer} 0.001016777 0.9090909  0.001118454 6.140734 10   
#[20] {AOC Monitor, ASUS Monitor, Dell Desktop, HP Laptop}                                              => {Lenovo Desktop Computer} 0.001016777 0.9090909  0.001118454 6.140734 10  

top.lift <- sort(rules4, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 20))
#    lhs                                                                                                  rhs                       support     confidence coverage    lift     count
#[1]  {Dell Desktop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                                 => {ViewSonic Monitor}       0.001118454 1.0000000  0.001118454 9.064516 11   
#[2]  {Dell Desktop, Lenovo Desktop Computer, Mackie CR Speakers}                                       => {ViewSonic Monitor}       0.001220132 0.9230769  0.001321810 8.367246 12   
#[3]  {Dell Desktop, Etekcity Power Extension Cord Cable, HP Laptop, iMac, Lenovo Desktop Computer}     => {ViewSonic Monitor}       0.001118454 0.9166667  0.001220132 8.309140 11   
#[4]  {Acer Aspire, Apple Earpods, HP Laptop, HP Monitor}                                               => {ViewSonic Monitor}       0.001016777 0.9090909  0.001118454 8.240469 10   
#[5]  {Belkin Mouse Pad, Microsoft Office Home and Student 2016, Rii LED Gaming Keyboard & Mouse Combo} => {Dell Desktop}            0.001016777 0.9090909  0.001118454 6.783694 10   
#[6]  {ASUS 2 Monitor, HP Laptop, Lenovo Desktop Computer, Microsoft Office Home and Student 2016}      => {Dell Desktop}            0.001016777 0.9090909  0.001118454 6.783694 10   
#[7]  {ASUS Chromebook, CYBERPOWER Gamer Desktop, HP Laptop, iMac, Lenovo Desktop Computer}             => {Dell Desktop}            0.001016777 0.9090909  0.001118454 6.783694 10   
#[8]  {ASUS Monitor, Intel Desktop, ViewSonic Monitor}                                                  => {Lenovo Desktop Computer} 0.001118454 1.0000000  0.001118454 6.754808 11   
#[9]  {ASUS 2 Monitor, Dell Desktop, HDMI Cable 6ft}                                                    => {Lenovo Desktop Computer} 0.001118454 0.9166667  0.001220132 6.191907 11   
#[10] {ASUS Monitor, Dell Desktop, iMac, Panasonic In-Ear Headphone}                                    => {Lenovo Desktop Computer} 0.001118454 0.9166667  0.001220132 6.191907 11   
#[11] {HP Wireless Mouse, Logitech MK550 Wireless Wave Keyboard and Mouse Combo}                        => {Lenovo Desktop Computer} 0.001016777 0.9090909  0.001118454 6.140734 10   
#[12] {Acer Desktop, iMac, Panasonic In-Ear Headphone, Samsung Monitor}                                 => {Lenovo Desktop Computer} 0.001016777 0.9090909  0.001118454 6.140734 10   
#[13] {Apple Magic Keyboard, ASUS 2 Monitor, HP Laptop, Intel Desktop}                                  => {Lenovo Desktop Computer} 0.001016777 0.9090909  0.001118454 6.140734 10   
#[14] {AOC Monitor, ASUS Monitor, Dell Desktop, HP Laptop}                                              => {Lenovo Desktop Computer} 0.001016777 0.9090909  0.001118454 6.140734 10   
#[15] {AOC Monitor, Apple Magic Keyboard, Dell Desktop, iMac}                                           => {Lenovo Desktop Computer} 0.001016777 0.9090909  0.001118454 6.140734 10   
#[16] {Apple Magic Keyboard, Belkin Mouse Pad, Dell Desktop, ViewSonic Monitor}                         => {Lenovo Desktop Computer} 0.001016777 0.9090909  0.001118454 6.140734 10   
#[17] {Dell KM117 Wireless Keyboard & Mouse, iPhone Charger Cable}                                      => {Apple MacBook Air}       0.002033554 0.9523810  0.002135231 6.122004 20   
#[18] {Acer Aspire, Koss Home Headphones, ViewSonic Monitor}                                            => {HP Laptop}               0.001220132 1.0000000  0.001220132 5.151912 12   
#[19] {Dell Desktop, Koss Home Headphones, ViewSonic Monitor}                                           => {HP Laptop}               0.001118454 1.0000000  0.001118454 5.151912 11   
#[20] {Acer Aspire, ASUS 2 Monitor, Intel Desktop}                                                      => {HP Laptop}               0.001016777 1.0000000  0.001016777 5.151912 10   

top.lift <- sort(rules5, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 40))
#    lhs                                                                                                rhs                       support     confidence coverage    lift     count
#[1]  {Dell Desktop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                               => {ViewSonic Monitor}       0.001118454 1.000000   0.001118454 9.064516 11   
#[2]  {ASUS Monitor, Intel Desktop, ViewSonic Monitor}                                                => {Lenovo Desktop Computer} 0.001118454 1.000000   0.001118454 6.754808 11   
#[3]  {Dell KM117 Wireless Keyboard & Mouse, iPhone Charger Cable}                                    => {Apple MacBook Air}       0.002033554 0.952381   0.002135231 6.122004 20   
#[4]  {Acer Aspire, Koss Home Headphones, ViewSonic Monitor}                                          => {HP Laptop}               0.001220132 1.000000   0.001220132 5.151912 12   
#[5]  {Dell Desktop, Koss Home Headphones, ViewSonic Monitor}                                         => {HP Laptop}               0.001118454 1.000000   0.001118454 5.151912 11   
#[6]  {Acer Aspire, ASUS 2 Monitor, Intel Desktop}                                                    => {HP Laptop}               0.001016777 1.000000   0.001016777 5.151912 10   
#[7]  {ASUS 2 Monitor, Computer Game, Dell Desktop}                                                   => {HP Laptop}               0.001016777 1.000000   0.001016777 5.151912 10   
#[8]  {Dell Desktop, iMac, Lenovo Desktop Computer, Logitech MK270 Wireless Keyboard and Mouse Combo} => {HP Laptop}               0.001118454 1.000000   0.001118454 5.151912 11   
#[9]  {Acer Desktop, Dell Desktop, HDMI Cable 6ft, iMac}                                              => {HP Laptop}               0.001321810 1.000000   0.001321810 5.151912 13   
#[10] {Acer Aspire, Computer Game, Dell Desktop, ViewSonic Monitor}                                   => {HP Laptop}               0.001118454 1.000000   0.001118454 5.151912 11   
#[11] {Apple Magic Keyboard, ASUS Monitor, Dell Desktop, HP Monitor}                                  => {HP Laptop}               0.001118454 1.000000   0.001118454 5.151912 11   
#[12] {Apple Earpods, Backlit LED Gaming Keyboard, CYBERPOWER Gamer Desktop, iMac}                    => {HP Laptop}               0.001016777 1.000000   0.001016777 5.151912 10   
#[13] {Acer Desktop, Apple Magic Keyboard, Dell Desktop, HP Monitor}                                  => {HP Laptop}               0.001016777 1.000000   0.001016777 5.151912 10   
#[14] {Acer Aspire, Apple Magic Keyboard, Dell Desktop, ViewSonic Monitor}                            => {HP Laptop}               0.001423488 1.000000   0.001423488 5.151912 14   
#[15] {3-Button Mouse, Acer Aspire, Dell Desktop, ViewSonic Monitor}                                  => {HP Laptop}               0.001016777 1.000000   0.001016777 5.151912 10   
#[16] {Brother Printer, Halter Acrylic Monitor Stand}                                                 => {iMac}                    0.001118454 1.000000   0.001118454 3.904327 11   
#[17] {ASUS Monitor, Mackie CR Speakers, ViewSonic Monitor}                                           => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[18] {Apple Magic Keyboard, Rii LED Gaming Keyboard & Mouse Combo, ViewSonic Monitor}                => {iMac}                    0.001728521 1.000000   0.001728521 3.904327 17   
#[19] {ASUS Monitor, Koss Home Headphones, Microsoft Office Home and Student 2016}                    => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   
#[20] {ASUS 2 Monitor, Dell Desktop, Logitech Keyboard}                                               => {iMac}                    0.001016777 1.000000   0.001016777 3.904327 10   

top.lift <- sort(rules6, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 20))
#no LHS

inspect(rules2) #print all
inspect(rules2[1:10]) #print only the top 10 rules
#lhs                                                                                              rhs                 support     confidence coverage    lift      count
#[1]  {Apple MacBook Pro, HP Black & Tri-color Ink, HP Laptop, iMac}                                => {Acer Aspire}       0.001016777 0.8333333  0.001220132 10.068591 10   
#[2]  {Dell Desktop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                             => {ViewSonic Monitor} 0.001118454 1.0000000  0.001118454  9.064516 11   
#[3]  {Dell Desktop, Lenovo Desktop Computer, Mackie CR Speakers}                                   => {ViewSonic Monitor} 0.001220132 0.9230769  0.001321810  8.367246 12   
#[4]  {Dell Desktop, Etekcity Power Extension Cord Cable, HP Laptop, iMac, Lenovo Desktop Computer} => {ViewSonic Monitor} 0.001118454 0.9166667  0.001220132  8.309140 11   
#[5]  {Acer Aspire, Apple Earpods, HP Laptop, HP Monitor}                                           => {ViewSonic Monitor} 0.001016777 0.9090909  0.001118454  8.240469 10   
#[6]  {HP Laptop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                                => {ViewSonic Monitor} 0.001321810 0.8666667  0.001525165  7.855914 13   
#[7]  {Acer Aspire, Dell Desktop, Epson Printer, HP Laptop}                                         => {ViewSonic Monitor} 0.001220132 0.8571429  0.001423488  7.769585 12   
#[8]  {Acer Aspire, ASUS Chromebook, Dell Desktop, HP Laptop}                                       => {ViewSonic Monitor} 0.001220132 0.8571429  0.001423488  7.769585 12   
#[9]  {Dell Desktop, HP Laptop, iMac, Mackie CR Speakers}                                           => {ViewSonic Monitor} 0.001321810 0.8125000  0.001626843  7.364919 13   
#[10] {Acer Aspire, HP Laptop, Koss Home Headphones}                                                => {ViewSonic Monitor} 0.001220132 0.8000000  0.001525165  7.251613 12   

# See specific item's rules with subset()
ItemRules2 <- subset(rules2, items %in% "iMac")
?subset
inspect(ItemRules2[1:15])
#  lhs                                                                                                rhs                    support     confidence coverage    lift     count
#[1]  {Brother Printer, Halter Acrylic Monitor Stand}                                                 => {iMac}              0.001118454 1          0.001118454 3.904327 11   
#[2]  {ASUS Monitor, Mackie CR Speakers, ViewSonic Monitor}                                           => {iMac}              0.001016777 1          0.001016777 3.904327 10   
#[3]  {Apple Magic Keyboard, Rii LED Gaming Keyboard & Mouse Combo, ViewSonic Monitor}                => {iMac}              0.001728521 1          0.001728521 3.904327 17   
#[4]  {ASUS Monitor, Koss Home Headphones, Microsoft Office Home and Student 2016}                    => {iMac}              0.001016777 1          0.001016777 3.904327 10   
#[5]  {ASUS 2 Monitor, Dell Desktop, Logitech Keyboard}                                               => {iMac}              0.001016777 1          0.001016777 3.904327 10   
#[6]  {Alienware Laptop, ASUS Desktop, Lenovo Desktop Computer}                                       => {iMac}              0.001016777 1          0.001016777 3.904327 10   
#[7]  {Brother Printer, Dell Desktop, Epson Printer}                                                  => {iMac}              0.001118454 1          0.001118454 3.904327 11   
#[8]  {Apple Magic Keyboard, Brother Printer, ViewSonic Monitor}                                      => {iMac}              0.001016777 1          0.001016777 3.904327 10   
#[9]  {ASUS Desktop, Dell Desktop, Microsoft Office Home and Student 2016}                            => {iMac}              0.001016777 1          0.001016777 3.904327 10   
#[10] {Intel Desktop, iPad Pro, Microsoft Office Home and Student 2016}                               => {iMac}              0.001118454 1          0.001118454 3.904327 11   
#[11] {Dell Desktop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                               => {ViewSonic Monitor} 0.001118454 1          0.001118454 9.064516 11   
#[12] {Acer Aspire, Apple MacBook Pro, HP Black & Tri-color Ink, HP Laptop}                           => {iMac}              0.001016777 1          0.001016777 3.904327 10   
#[13] {Dell Desktop, iMac, Lenovo Desktop Computer, Logitech MK270 Wireless Keyboard and Mouse Combo} => {HP Laptop}         0.001118454 1          0.001118454 5.151912 11   
#[14] {Acer Desktop, Dell Desktop, HDMI Cable 6ft, iMac}                                              => {HP Laptop}         0.001321810 1          0.001321810 5.151912 13   
#[15] {Dell Desktop, Etekcity Power Extension Cord Cable, Lenovo Desktop Computer, ViewSonic Monitor} => {iMac}              0.001220132 1          0.001220132 3.904327 12   

ItemRules3 <- subset(rules2, items %in% "HP Laptop")
inspect(ItemRules3[1:15])
#  lhs                                                                                                rhs            support     confidence coverage    lift     count
#[1]  {Acer Aspire, Koss Home Headphones, ViewSonic Monitor}                                          => {HP Laptop} 0.001220132 1          0.001220132 5.151912 12   
#[2]  {Dell Desktop, Koss Home Headphones, ViewSonic Monitor}                                         => {HP Laptop} 0.001118454 1          0.001118454 5.151912 11   
#[3]  {Acer Aspire, ASUS 2 Monitor, Intel Desktop}                                                    => {HP Laptop} 0.001016777 1          0.001016777 5.151912 10   
#[4]  {ASUS 2 Monitor, Computer Game, Dell Desktop}                                                   => {HP Laptop} 0.001016777 1          0.001016777 5.151912 10   
#[5]  {Acer Aspire, Apple MacBook Pro, HP Black & Tri-color Ink, HP Laptop}                           => {iMac}      0.001016777 1          0.001016777 3.904327 10   
#[6]  {Dell Desktop, iMac, Lenovo Desktop Computer, Logitech MK270 Wireless Keyboard and Mouse Combo} => {HP Laptop} 0.001118454 1          0.001118454 5.151912 11   
#[7]  {Acer Desktop, Dell Desktop, HDMI Cable 6ft, iMac}                                              => {HP Laptop} 0.001321810 1          0.001321810 5.151912 13   
#[8]  {Etekcity Power Extension Cord Cable, HP Laptop, Lenovo Desktop Computer, ViewSonic Monitor}    => {iMac}      0.001626843 1          0.001626843 3.904327 16   
#[9]  {AOC Monitor, ASUS Monitor, HP Laptop, ViewSonic Monitor}                                       => {iMac}      0.001016777 1          0.001016777 3.904327 10   
#[10] {Acer Aspire, Computer Game, Dell Desktop, ViewSonic Monitor}                                   => {HP Laptop} 0.001118454 1          0.001118454 5.151912 11   
#[11] {Apple Magic Keyboard, ASUS Monitor, HP Laptop, LG Monitor}                                     => {iMac}      0.001016777 1          0.001016777 3.904327 10   
#[12] {Apple Magic Keyboard, ASUS Monitor, HP Laptop, Microsoft Office Home and Student 2016}         => {iMac}      0.001220132 1          0.001220132 3.904327 12   
#[13] {Apple Magic Keyboard, ASUS Monitor, Dell Desktop, HP Monitor}                                  => {HP Laptop} 0.001118454 1          0.001118454 5.151912 11   
#[14] {Apple Earpods, Backlit LED Gaming Keyboard, CYBERPOWER Gamer Desktop, iMac}                    => {HP Laptop} 0.001016777 1          0.001016777 5.151912 10   
#[15] {Acer Desktop, Apple Magic Keyboard, Dell Desktop, HP Monitor}                                  => {HP Laptop} 0.001016777 1          0.001016777 5.151912 10   

ItemRules4 <- subset(rules2, items %in% "Acer Aspire")
inspect(ItemRules4[1:15])
#lhs                                                                                    rhs            support     confidence coverage    lift     count
#[1]  {Acer Aspire, Koss Home Headphones, ViewSonic Monitor}                            => {HP Laptop} 0.001220132 1.0000000  0.001220132 5.151912 12   
#[2]  {Acer Aspire, ASUS 2 Monitor, Intel Desktop}                                      => {HP Laptop} 0.001016777 1.0000000  0.001016777 5.151912 10   
#[3]  {Acer Aspire, Apple MacBook Pro, HP Black & Tri-color Ink, HP Laptop}             => {iMac}      0.001016777 1.0000000  0.001016777 3.904327 10   
#[4]  {Acer Aspire, Computer Game, Dell Desktop, ViewSonic Monitor}                     => {HP Laptop} 0.001118454 1.0000000  0.001118454 5.151912 11   
#[5]  {3-Button Mouse, Acer Aspire, Apple Magic Keyboard, CYBERPOWER Gamer Desktop}     => {iMac}      0.001016777 1.0000000  0.001016777 3.904327 10   
#[6]  {Acer Aspire, Apple Magic Keyboard, Dell Desktop, ViewSonic Monitor}              => {HP Laptop} 0.001423488 1.0000000  0.001423488 5.151912 14   
#[7]  {3-Button Mouse, Acer Aspire, Dell Desktop, ViewSonic Monitor}                    => {HP Laptop} 0.001016777 1.0000000  0.001016777 5.151912 10   
#[8]  {Acer Aspire, iMac, Microsoft Office Home and Student 2016, ViewSonic Monitor}    => {HP Laptop} 0.001321810 0.9285714  0.001423488 4.783918 13   
#[9]  {Acer Aspire, Acer Desktop, Dell Desktop, iMac, ViewSonic Monitor}                => {HP Laptop} 0.001321810 0.9285714  0.001423488 4.783918 13   
#[10] {Acer Aspire, Intel Desktop, ViewSonic Monitor}                                   => {HP Laptop} 0.001220132 0.9230769  0.001321810 4.755611 12   
#[11] {Acer Aspire, Dell Desktop, Epson Printer, ViewSonic Monitor}                     => {HP Laptop} 0.001220132 0.9230769  0.001321810 4.755611 12   
#[12] {Acer Aspire, ASUS 2 Monitor, Dell Desktop, ViewSonic Monitor}                    => {HP Laptop} 0.001220132 0.9230769  0.001321810 4.755611 12   
#[13] {Acer Aspire, ASUS 2 Monitor, Dell Desktop, Lenovo Desktop Computer}              => {iMac}      0.001220132 0.9230769  0.001321810 3.603994 12   
#[14] {Acer Aspire, Apple Magic Keyboard, iMac, Microsoft Office Home and Student 2016} => {HP Laptop} 0.001220132 0.9230769  0.001321810 4.755611 12   
#[15] {Acer Aspire, Apple Magic Keyboard, iMac, Intel Desktop}                          => {HP Laptop} 0.001118454 0.9166667  0.001220132 4.722586 11   

ItemRules5 <- subset(rules2, items %in% "ViewSonic Monitor")
inspect(ItemRules5[1:15])
#lhs                                                                                                  rhs                          support     confidence coverage    lift     count
#[1]  {ASUS Monitor, Mackie CR Speakers, ViewSonic Monitor}                                           => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[2]  {Apple Magic Keyboard, Rii LED Gaming Keyboard & Mouse Combo, ViewSonic Monitor}                => {iMac}                    0.001728521 1          0.001728521 3.904327 17   
#[3]  {Acer Aspire, Koss Home Headphones, ViewSonic Monitor}                                          => {HP Laptop}               0.001220132 1          0.001220132 5.151912 12   
#[4]  {Dell Desktop, Koss Home Headphones, ViewSonic Monitor}                                         => {HP Laptop}               0.001118454 1          0.001118454 5.151912 11   
#[5]  {Apple Magic Keyboard, Brother Printer, ViewSonic Monitor}                                      => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[6]  {ASUS Monitor, Intel Desktop, ViewSonic Monitor}                                                => {Lenovo Desktop Computer} 0.001118454 1          0.001118454 6.754808 11   
#[7]  {Dell Desktop, iMac, Lenovo Desktop Computer, Mackie CR Speakers}                               => {ViewSonic Monitor}       0.001118454 1          0.001118454 9.064516 11   
#[8]  {Dell Desktop, Etekcity Power Extension Cord Cable, Lenovo Desktop Computer, ViewSonic Monitor} => {iMac}                    0.001220132 1          0.001220132 3.904327 12   
#[9]  {Etekcity Power Extension Cord Cable, HP Laptop, Lenovo Desktop Computer, ViewSonic Monitor}    => {iMac}                    0.001626843 1          0.001626843 3.904327 16   
#[10] {AOC Monitor, ASUS Monitor, HP Laptop, ViewSonic Monitor}                                       => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[11] {AOC Monitor, Dell Desktop, Lenovo Desktop Computer, ViewSonic Monitor}                         => {iMac}                    0.001118454 1          0.001118454 3.904327 11   
#[12] {Acer Aspire, Computer Game, Dell Desktop, ViewSonic Monitor}                                   => {HP Laptop}               0.001118454 1          0.001118454 5.151912 11   
#[13] {Epson Printer, HP Monitor, Lenovo Desktop Computer, ViewSonic Monitor}                         => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[14] {Acer Desktop, Apple Magic Keyboard, ASUS Monitor, ViewSonic Monitor}                           => {iMac}                    0.001016777 1          0.001016777 3.904327 10   
#[15] {Acer Aspire, Apple Magic Keyboard, Dell Desktop, ViewSonic Monitor}                            => {HP Laptop}               0.001423488 1          0.001423488 5.151912 14 




# There are redudant rules present in Itemrules2
?is.redundant

is.redundant(ItemRules2)
#[1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[23] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
#[45] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
#[67] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[89] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[111] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[133] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
#[155] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[177] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[199] FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[221] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[243] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[265] FALSE  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[287] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[309] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE
#[331] FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
#[353] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
#[375] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[397] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[419] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[441] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[463] FALSE FALSE FALSE FALSE

#prune redundants
ItemRules2 <- ItemRules2[!is.redundant(ItemRules2)]
is.redundant(ItemRules2)

#Visualize results

#Plot rules*
####
plot(rules2, jitter = 0)
plot(rules2)

plot(rules2, method = "graph",
     control = list(type="items"))

plot(rules2, method = "graph", limit = 20)
plot(rules2, method = "graph", limit = 10)

plot(rules2[1:11], method = "grouped",
     control = list(type="items"))  
###
plot(rules3, jitter = 0)
plot(rules3)

plot(rules3, method = "graph",
     control = list(type="items"))

plot(rules3, method = "graph", limit = 20)
plot(rules3, method = "graph", limit = 10)

plot(rules3[1:11], method = "grouped",
     control = list(type="items"))   
###
plot(rules4, jitter = 0)
plot(rules4)

plot(rules4, method = "graph",
     control = list(type="items"))

plot(rules4, method = "graph", limit = 20)
plot(rules4, method = "graph", limit = 10)

plot(rules4[1:11], method = "grouped",
     control = list(type="items"))   
###
plot(rules5, jitter = 0)
plot(rules5)

plot(rules5, method = "graph",
     control = list(type="items"))

plot(rules5, method = "graph", limit = 20)
plot(rules5, method = "graph", limit = 10)

plot(rules5, method = "grouped",
     control = list(type="items"))   
###
#Best rule is all 3 are high
#high support: should apply to a large amount of cases
#high confidence: should be correct often
#high lift: indicates it is not just a coincidence


