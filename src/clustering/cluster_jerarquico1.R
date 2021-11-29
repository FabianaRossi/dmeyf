require("data.table")
require("randomForest")


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

setwd( "~/buckets/b1/" )

#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
dataset  <- fread( "datasets/dataset_epic_v963.csv.gz", stringsAsFactors= TRUE)
gc()

#achico el dataset
dataset[  ,  azar := runif( nrow(dataset) ) ]
dataset  <-  dataset[  clase_ternaria =="BAJA+1"  & foto_mes>=202001  & foto_mes<=202011, ]
gc()
#strsplit(colnames(dataset), "\\s")

#vector_de_colnames <- "numero_de_cliente
# foto_mes
# cliente_edad
# cliente_antiguedad
# mrentabilidad
# mrentabilidad_annual
# mcomisiones
# mactivos_margen
# mpasivos_margen
# cproductos
# tpaquete1
# tpaquete2
# ccuenta_corriente
# mcuenta_corriente_adicional
# mcuenta_corriente
# ccaja_ahorro
# mcaja_ahorro
# mcaja_ahorro_adicional
# mcaja_ahorro_dolares
# mdescubierto_preacordado
# mcuentas_saldo
# ctarjeta_debito_transacciones
# mautoservicio
# ctarjeta_visa
# ctarjeta_visa_transacciones
# mtarjeta_visa_consumo
# ctarjeta_master
# mtarjeta_master_consumo
# cprestamos_personales
# mprestamos_personales
# cprestamos_prendarios
# mprestamos_prendarios
# cprestamos_hipotecarios
# mprestamos_hipotecarios
# cplazo_fijo
# mplazo_fijo_pesos
# cinversion1
# minversion1_pesos
# minversion1_dolares
# cseguro_auto
# ccaja_seguridad
# cpayroll_trx
# mpayroll
# mpayroll2
# cpayroll2_trx
# mcuenta_debitos_automaticos
# mttarjeta_visa_debitos_automaticos
# cpagodeservicios
# mpagodeservicios
# mpagomiscuentas
# ccajeros_propios_descuentos
# mcajeros_propios_descuentos
# ctarjeta_visa_descuentos
# mtarjeta_visa_descuentos
# ctarjeta_master_descuentos
# mtarjeta_master_descuentos
# mcomisiones_mantenimiento
# ccomisiones_otras
# mcomisiones_otras
# cforex_buy
# mforex_buy
# mtransferencias_recibidas
# ccheques_depositados
# mcheques_depositados
# ccheques_emitidos
# mcheques_emitidos
# ccheques_depositados_rechazados
# mcheques_depositados_rechazados
# ccheques_emitidos_rechazados
# mcheques_emitidos_rechazados
# tcallcenter
# thomebanking
# chomebanking_transacciones
# ccajas_otras
# ctrx_quarter
# cmobile_app_trx
# Master_status
# Master_mfinanciacion_limite
# Master_Fvencimiento
# Master_Finiciomora
# Master_msaldototal
# Master_msaldopesos
# Master_mconsumosdolares
# Master_mlimitecompra
# Master_madelantopesos
# Master_fechaalta
# Master_cadelantosefectivo
# Visa_status
# Visa_mfinanciacion_limite
# Visa_Fvencimiento
# Visa_Finiciomora
# Visa_msaldototal
# Visa_msaldopesos
# Visa_mconsumospesos
# Visa_mconsumosdolares
# Visa_mlimitecompra
# Visa_madelantodolares
# Visa_fultimo_cierre
# Visa_mpagospesos"

#j <- strsplit(vector_de_colnames, "\\s")[[1]]
#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset[ , which(sapply(dataset, anyNA)) := NULL]
dataset  <- na.roughfix( dataset )
gc()

campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
                     "Master_fechaalta", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos",
                     "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
                     "Visa_mpagospesos", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
                     "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
                     "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
                     "mcomisiones", "ccomisiones_otras", "Master_status",
                     "mpagomiscuentas", "mcomisiones_tend","mactivos_margen_tend","mpasivos_margen_tend","cproductos_tend","ccuenta_corriente_tend",
                     "mcaja_ahorro_tend","mdescubierto_preacordado_tend","mcuentas_saldo_tend","mtarjeta_visa_consumo_tend","mprestamos_personales_tend",
                     "mprestamos_prendarios_tend","mprestamos_hipotecarios_tend","mpayroll_tend","mpagodeservicios_tend","mcomisiones_mantenimiento_tend",
                     "mtransferencias_recibidas_tend","mtransferencias_emitidas_tend","mcheques_depositados_rechazados_tend","ccallcenter_transacciones_tend",
                     "ccajas_consultas_tend","ctrx_quarter_tend","Master_status_tend","Master_mpagominimo_tend","Visa_delinquency_tend",
                     "Visa_status_tend","Visa_msaldototal_tend","Visa_mconsumospesos_tend","mrentabilidad_annual_nrango","mcomisiones_nrango",
                     "mactivos_margen_nrango","mpasivos_margen_nrango","mcaja_ahorro_nrango","cprestamos_personales_nrango","mpayroll_nrango",
                     "mtarjeta_visa_descuentos_nrango","mtarjeta_master_descuentos_nrango","Visa_msaldototal_nrango","Master_mconsumototal_nrango")
# cosasdif <- setdiff( campos_buenos,  j )


#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[ , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox = TRUE )

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )


pdf( paste0( paste0("./work/cluster_jerarquico1.pdf" ) ))
plot( hclust.rf )
dev.off()


h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=6 & distintos <=7 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)

  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]

  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#ahora a mano veo las variables
dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter


