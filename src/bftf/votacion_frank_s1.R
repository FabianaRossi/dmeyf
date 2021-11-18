rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")
require("lightgbm")
require("DiceKriging")
require("mlrMBO")

switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "M:\\" },   #Windows
         Darwin  = { directory.root  <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root  <-  "~/buckets/b1/" } #Google Cloud
         )

setwd( directory.root )

kscript         <- "votacion_frank_s1"
st_path    <- "./datasets/dataset_stacking_v001.csv.gz"

dataset <- fread(st_path)

Rango  <- function( dataset, cols )
{dataset[ , paste0( cols, "_rango") := lapply( .SD, frankv, na.last="keep", ties.method="dense" ), 
                 by= foto_mes,
                 .SDcols= cols]
        ReportarCampos( dataset )}

cols_analiticas  <- setdiff( colnames(dataset),  c("numero_de_cliente","foto_mes","clase_ternaria") )

Rango(dataset, cols_analiticas)

dataset1  <- copy(  dataset[  , c("numero_de_cliente","foto_mes","clase_ternaria"),  with=FALSE] )
dataset <- dataset[ , grepl( "_rango" , names( dataset ) ) ]

library(dplyr)

dataset_rk <- dataset %>% mutate(suma= rowSums(.)) 
dataset_rank <- cbind(dataset1,dataset_rk)
dataset_rank <- dataset_rank%>% arrange(desc(suma))



