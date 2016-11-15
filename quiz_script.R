library(tidyverse)
library(apaTables)

my.data <- read_csv("reg_quiz2_data.csv")
glimpse(my.data)

apa.cor.table(my.data)
apa.cor.table(my.data, filename="Table1_APA.doc", table.number=1)
#step 3 done
psych::pairs.panels(as.data.frame(my.data))

#QUESTION 4
#SE predicting academic success (aSuc) -> does SE predict aSuc above and beyond emotional state?

#a) regression to establish the extent SE predicts unique variance in aSuc above/beyond PAS
#IV = aSuc
#DV1 = selfEsteem
#DV2 = PAS
regression.a.PAS <- lm(aSuc ~ PAS, data = my.data)
apa.reg.table(regression.a.PAS)
apa.reg.table(regression.a.PAS, filename="Table2_APA.doc", table.number=2)
#sr2 is .10, CI [.03, .19} R2 is .098, CI [.03, .19]

regression.a <- lm(aSuc ~ PAS + selfEsteem, data=my.data)
apa.reg.table(regression.a)
apa.reg.table(regression.a, filename="Table3_APA.doc", table.number=3)
#sr2 for SE is .22, CI [12, .33], sr2 for PAS is .07 {.01, .14}, R2 is .32, CI [.21, .41]


regression.b <- lm(aSuc ~ NAS + selfEsteem, data=my.data)
apa.reg.table(regression.b)
apa.reg.table(regression.b, filename="Table4_APA.doc", table.number=4)
#sr2 for SE is .23, CI [.12, .33], R2 = .28, CI[.17, .37]


#BLOCK REGRESSION
block1 = lm(aSuc ~ NAS + PAS, data=my.data)
block2 = lm(aSuc ~ NAS + PAS + selfEsteem, data=my.data)

apa.reg.table(block1, block2)
apa.reg.table(block1, block2, filename="Table5_APA.doc", table.number=5)
#sr2 for SE is .21, CI .11, .31








