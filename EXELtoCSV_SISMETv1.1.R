#######################################################
##Este programa es converte de exel a csv por clumnas,#
##las variables prcp, tmax, tmin.                     #
##javterg@                                            #
#######################################################

##input library
library(xlsx)
library(stringr)
library(tidyverse)
library(tidyr)

##################################################################################
#FILE EXEL INPUT
ruta_carpeta <- "/media/vboxuser/SAMSUNG/temp_clear2023/DataBase_csv/exel_BOLest/"
#FILE CSV OUTPUT
ruta_out <-  "/media/vboxuser/SAMSUNG/temp_clear2023/DataBase_csv/out_csvDataBOL/"
###################################################################################


#LIST OF FILES
list_est <- dir(ruta_carpeta, pattern = '.xls')

#LAT LON FUNC
geogra_conv <- function(text ){
 list_txt <- strsplit(text, ' ')
 grades <-list_txt[[1]][1]
 min    <-list_txt[[1]][2]
 seg    <-list_txt[[1]][3]
 g_num <- as.numeric(str_replace(grades,'º',''))
 m_num <- as.numeric(str_replace(min,  "'",''))
 s_num <- as.numeric(str_replace(seg, '\"', ''))
 decimals <- g_num + (m_num/60)+ (s_num/3600)
return(decimals)
 }

CEST<-c()
CPROV <- c()
CLAT <- c()
CLON <- c()
CALT <- c()

sheet <- c(1,2,3)
estacion <- list_est[5]
for(estacion in list_est){
  ruta <- paste(ruta_carpeta, estacion,sep = '')

  df_prcp <- read.xlsx(ruta, sheet[1], header=FALSE)
  df_tmax <- read.xlsx(ruta, sheet[2], header=FALSE)
  df_tmin <- read.xlsx(ruta, sheet[3], header=FALSE)
  
  #ATRIBUTES 
  estacion <- as.character(df_prcp[1,3])
  provincia <- as.character(df_prcp[2,3])
  lat_sud    <- geogra_conv(as.character(df_prcp[1,12]))
  long_oeste <- geogra_conv(as.character(df_prcp[2,12]))
  alt  <- as.numeric(df_prcp[3,12])
  print(c(estacion, provincia,lat_sud, long_oeste, alt))
  
  CEST<- append(CEST, estacion)
  CPROV <- append(CPROV,provincia)
  CLAT <- append( CLAT, lat_sud)
  CLON <- append(CLON, long_oeste)
  CALT <- append(CALT, alt)
  
  #DATAFRAME FIN 
  cdate_fin <- seq.Date(from = as.Date('1940-01-01'), to = as.Date('2023-12-31'), by = 'days')
  cdate_tri <- separate(data.frame(date= as.character(cdate_fin)),col=date,into=c("yy","mm","dd"),sep="-", convert = TRUE  )
  
  #UNIQUE YEAR COUNT MONTHS  
  tot_day <- cdate_tri %>% group_by(yy,mm) %>% summarise(dd = n())
  ####################################################################
  data_all <-  function(df, tot_day){
    dim <- dim(df)
    df1 <- sapply(df[6:dim[1],1:dim[2]],as.numeric) 
    #EMPY VECTORS
    var_c<-c()
    yy_c<-c()
    mm_c<-c()
    dd_c<-c()
    
    #DIM
    nxn <- dim(df1)[1]
    if(is.null(nxn)){nxn<-0}
    
    if (is.null(nxn) | (nxn<=2)){
      DA <- data.frame("yy"=NULL,"mm"=NULL,"dd"=NULL,"var"=NULL)
    }else{
      for (i in seq(nxn)){
        #YY MM OF EST
        yymm_est <-c(df1[[i, 1]], df1[[i, 2]])
        #IDX
        id <- which((tot_day$yy==yymm_est[1] ) & (tot_day$mm==yymm_est[2]))  
        dn <-tot_day$dd[id]
        y_est <-rep(yymm_est[1], dn)
        m_est <-rep(yymm_est[2], dn)
        d_est <-seq(1,dn)  
        var_est <- array(df1[i,3:(dn+2)])  
        #APPEND VAR
        var_c <- append(var_c, var_est)
        yy_c <- append(yy_c, y_est)
        mm_c <- append(mm_c, m_est)
        dd_c <- append(dd_c, d_est)
      }
      DA <- data.frame("yy"=yy_c,"mm"=mm_c,"dd"=dd_c,"var"=var_c) 
    }
    
    return(DA) 
  }

  #DATAFRAME
  DFN <- data.frame("date"=cdate_fin)  
  # P TN TM
  DA_PR <- data_all(df_prcp,tot_day)
  DA_TN <- data_all(df_tmin,tot_day)
  DA_TM <- data_all(df_tmax,tot_day)
  
  #FOR PRCP
  if(is.null(DA_PR$var)){
    END_PR <- DFN
    END_PR["var"]<- NA 
  }else{
    DA_PR <- unite(DA_PR, col = "date",c("yy","mm","dd") ,sep="-")
    DA_PR["date"] <- as.Date(DA_PR$date)
    END_PR <- full_join(DFN, DA_PR)
  }
  #FOR TMIN
  if(is.null(DA_TN$var)){
    END_TN <- DFN
    END_TN["var"]<- NA 
  }else{
    DA_TN <- unite(DA_TN, col = "date",c("yy","mm","dd") ,sep="-")
    DA_TN["date"] <- as.Date(DA_TN$date)
    END_TN <- full_join(DFN, DA_TN)
  }
  #FOR TMAX
  if(is.null(DA_TM$var)){
    END_TM <- DFN
    END_TM["var"]<- NA 
  }else{
    DA_TM <- unite(DA_TM, col = "date",c("yy","mm","dd") ,sep="-")
    DA_TM["date"] <- as.Date(DA_TM$date)
    END_TM <- full_join(DFN, DA_TM)
  }
  
  
  DF_END <- data.frame("DATE"=DFN$date,"PRCP"=END_PR$var,"TM"=END_TM$var,"TN"=END_TN$var )
  write.table(DF_END, file = paste(ruta_out,estacion,'.csv', sep=''), sep = ',', row.names = FALSE, col.names = TRUE)
  rm(list = c('END_TM','END_TN','END_PR',"DF_END",'DA_PR','DA_TN','DA_TM'))
}


METAD <-  data.frame("ESTACION" = CEST,"PROVINCIA"= CPROV, "LAT"= CLAT, "LONG" = CLON,"ALT"= CALT)
write.table(METAD, file = paste(ruta_out,'METADATA_BOL.csv', sep=''), sep = ',', row.names = FALSE, col.names = TRUE)
rm(list = c('METAD'))
print("DONE RIGTH ALL EST .XSL IN .CVS.............................................................................")

