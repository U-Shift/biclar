
piggyback::pb_list()
piggyback::pb_download("TRIPSmode.Rds")
od_lisbon = readRDS("TRIPSmode.Rds")
summary(od_lisbon)
# Origem            Destino              modo              viagens        
# Length:970         Length:970         Length:970         Min.   :     1.9  
# Class :character   Class :character   Class :character   1st Qu.:    47.5  
# Mode  :character   Mode  :character   Mode  :character   Median :   332.0  
# Mean   :  5499.3  
# 3rd Qu.:  1620.9  
# Max.   :342412.4  