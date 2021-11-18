#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#4 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

setwd("/Users/FR/Desktop/salidas de kaggle/lgbm")
require("data.table")

version  <- "v_manual1"  #cambiar cada vez, asi se tiene versionado del dataset


library(stringr)

load_data <- function(path) { 
        files <- dir(path, pattern = '\\.csv', full.names = T)
        nombres_files <- sapply(strsplit(files, ""), function(x) paste0(x[49:80], collapse=""))
        nombres_files <- str_extract(nombres_files, '.*(?=\\.csv)')
        file1 <- files[1]
        files <- files[-1]
        table <- read.table(file1, sep=',')
        tables <- lapply(files, function(x) read.table(x, sep=',')[,2])
        do.call(cbind, tables)
        df <- cbind(table,tables)
        colnames(df) <- as.character(df[1, ])
        df <- df[-1,]
        colnames(df) <- c('numero_de_cliente',nombres_files)
        df
}


path <- "/Users/FR/Desktop/salidas de kaggle/lgbm"

data <- load_data(path)

fwrite( data,
        file=paste0( "./dataset_stacking_", version, ".csv"),
        sep="," )

