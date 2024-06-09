setwd("~/Dokumente/GitHub/statistics_shiny/uebung-4")

#Little Helper

library(shinyWidgets)

#https://shinyapps.dreamrs.fr/shinyWidgets/




Bsp1: Residuen sind nicht NV. 

Bsp2: Residuen sind nicht homoskedastisch. 

Bsp5: „Wir sehen, dass hier BMI und skin miteinander korrelieren sowie npreg und age. Da der Datensatz recht groß ist, haben wir uns dazu entschieden erstmal zu schauen, ob diese Koeffizienten überhaupt etwas zum Modell beitragen und erstmal ein generalisiertes lineares Modell zu erstellen. “ Sie dürfen keine erklärenden Variablen die stark miteinander Korrelieren ins Modell aufnehmen weil es gegen Modellanforderungen verstößt. Der Ansatz „erstmal zu schauen“ geht daher nicht. Die Modellgleichung wäre aussagekräftiger wenn Sie den Logarithmus heraus rechnen. Interpretation des Ergebnisses fehlt. 