data_csv <- read.csv("C:\\Users\\vinci\\RStudio\\Dataset_nutrienti.csv", header=TRUE, stringsAsFactors=FALSE)
#Per leggere file bisogna utilizzare il simbolo "\\" al posto di "\"
#Bisogna specificare il tipo di file(in questo caso .csv)

View(data_csv)

df = subset(data_csv, select = -c(COUNTRY, INDICATOR, NUTRIENTS, TIME, Unit.Code, PowerCode.Code, Reference.Period.Code, Reference.Period, Flag.Codes, Flags))
#eliminazione delle colonne ridondanti

View(df)