require("data.table")
require("randomForest")


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

setwd( "~/buckets/b1/" )

#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
dataset  <- fread( "datasets/---.gz", stringsAsFactors= TRUE)
gc()

desde <- 202001
hasta <- 202011
meses_malos <- 202006
kcont_subsampling <- 0.01

#Defino los datos donde entreno, con subsampling de los CONTINUA
vector_azar  <- runif( nrow(dataset) )

dataset[    foto_mes>= desde  &
              foto_mes<= hasta & 
              !( foto_mes %in% meses_malos ) & 
              ( clase_ternaria=="BAJA+1" | vector_azar < kcont_subsampling ) ]  


fwrite( dataset,file="datasets/---.gz",sep="\t" )
