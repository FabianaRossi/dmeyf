#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd( "~/buckets/b1/" )

#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
dataset  <- fread( "datasetsOri/paquete_premium.csv.gz" )

setorder(  dataset,  numero_de_cliente, -foto_mes )   #ordeno, pero a la inversa

dataset[   , morire := 0 ]
dataset[ clase_ternaria=="BAJA+1" , morire := 1 ]  #si tengo un BAJA+1 , ese mes se que voy a morir

dataset[  , morire := cummax( morire ), numero_de_cliente ]   #calculo el maximo acumulado hace atras
dataset[  , meses_muerte := cumsum( morire ), numero_de_cliente ]   #calculo la suma acumulada


dataset[  meses_muerte==0,  meses_muerte := NA ]
dataset[  , morire := NULL ]

fwrite( dataset,
        file="datasets/paquete_premium_meses_muerte.txt.gz",
        sep="\t" )


