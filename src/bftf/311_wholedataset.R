#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


#Aqui comienza el programa
setwd("~/buckets/b1/")

dataset  <- fread( "./datasetsOri/paquete_premium.csv.gz" )

campos_buenos  <- setdiff(  colnames( dataset),  c("numero_de_cliente","foto_mes","clase_ternaria" ) )

pdf("./work/densidades_wholedataset.pdf")
for( campo in  campos_buenos )
{
  cat( campo, "  " )
  
  interesante <- dataset[ , get(campo) ]
  boxplot(interesante ~ factor(dataset$foto_mes ))
}
dev.off()
