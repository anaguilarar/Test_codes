
rm(list=ls())

library("stringr")
#### Setting the Workspace

  dirCSV="D:/ToBackup/Documents/Projects/Analogues/"

#### Reading the CSV file that contains the coordinates

setwd(dirCSV)

Raw_data=read.csv("TesacCoordinates.csv")

Raw_data$CORDENADA=paste0(Raw_data$Latitude," - ",Raw_data$Longitude)

nameCoorColumn="CORDENADA"

Raw_data[,nameCoorColumn]=as.character(Raw_data[,nameCoorColumn])


## strange Values Remmoving

Pattern="'"
Pattern2='\"'


Raw_data[,nameCoorColumn]=gsub("[´]","'",Raw_data[,nameCoorColumn])
Raw_data[,nameCoorColumn]=gsub("[?]","'",Raw_data[,nameCoorColumn])



###Processing Decimal degree data
DD=m[1]
Cleaning_DecimalDegree= function(DD){
  DD=gsub("°","",DD)
  if(str_detect(DD,",")&str_locate(DD,",")[1]<4)
  {
    DD=`str_sub<-`(DD,str_locate(DD,",")[1],str_locate(DD,",")[1],".")
  }
  DD=gsub(",","",DD)
  return(abs(as.double(DD)))
}


Coord_D_DMS=Raw_data[!(str_detect(Raw_data[,nameCoorColumn],Pattern)|str_detect(Raw_data[,nameCoorColumn],Pattern2)),nameCoorColumn]
pos_D_DMS=row.names(Raw_data)[!(str_detect(Raw_data[,nameCoorColumn],Pattern)|str_detect(Raw_data[,nameCoorColumn],Pattern2))]
Coord_D_DMS_F=gsub(" ", "", Coord_D_DMS)
Coord_D_DMS_F=gsub("[:°]", " ", Coord_D_DMS_F)
Coord_D_DMS_F=gsub("-", " ", Coord_D_DMS_F)
Coord_D_DMS_F=gsub("[W N w n o O]", " ", Coord_D_DMS_F)
j=4
Coord_D_DMS_F2=str_split(Coord_D_DMS_F,pattern = "[ ]")
j=Coord_D_DMS_F2[[1]]
Coord_DD_Correc=as.data.frame(do.call(rbind,lapply (Coord_D_DMS_F2, function(j){
  obs=""
  m=j[!j %in% c("",",","-",":")]
  if(!str_detect(m,"([[:digit:]])")){m=""}
  longitud=0
  latitud=0
  if(length(m)==2 )
  {
    i=1
    for(i in 1:2)
    {
      DD=Cleaning_DecimalDegree(m[i])
      if(DD>=70)
        longitud=DD
      else
        latitud=DD
    }
    Coordinates=c(longitud,latitud)
  }else if(length(m)>2 & length(m)<5){
    if(length(which(as.double(m[which(str_detect(m,"([.])"))])>1))>=1){
      m=paste0(m[which((str_detect(m,"([.])")))-1],"°",m[which(str_detect(m,"([.])"))])
      for(i in 1:length(m))
      {
        DM=CleaningMinutesDegrees(m[i])
        if(DM>=70)
          longitud=DM
        else
          latitud=DM
      }
    }
    Coordinates=c(longitud,latitud)
  }else{
    Coordinates=c(longitud,latitud)
  }
  return(c(Coordinates,obs))
})))
z=18

dim(Coord_DD_Correc)
length(pos_D_DMS)
Coord_DD_Correc[["Pos"]]=pos_D_DMS

###Processing DSM data

#Removing W and N characteres
DMS=m[i]
DM=auxVec[i]

CleaningMinutesDegrees= function(DM){
  Degpos=str_locate(DM,"°")
  Degr=gsub(",","",str_sub(DM,1,(Degpos[1]-1)))
  if(str_length(DM)>(Degpos[1]))
  {
    Minutes=str_sub(DM,(str_locate(DM,"°")[1]+1),-1)
    Valid=gsub("[.,'¨]","",Minutes)
    Minute=ifelse((as.double(Valid)/10)/60>1,as.double(Valid)/100,as.double(Valid)/10)
    Minute=ifelse((as.double(Minute))/60>1,as.double(Minute)/10,as.double(Minute)/1)
    CoordVal=abs(as.double(Degr))+as.double(Minute)/60
  }else{
    CoordVal=Cleaning_DecimalDegree(DM)
  }
}

CleaningMinutesSecondsDegrees= function(DMS){
  if(str_detect(DMS,"°") & str_detect(DMS,"'"))
  {
    Degpos=str_locate(DMS,"°")
    Degr=gsub(",","",str_sub(DMS,1,(Degpos[1]-1)))
    MinPos=str_locate(DMS,"'")
    Minut=str_sub(DMS,(Degpos[1]+1),(MinPos[1]-1))
    if(str_length(DMS)>=(MinPos[1]+1))
    {
      Seconds=str_sub(DMS,(str_locate(DMS,"'")[1]+1),-1)
      Valid=gsub("['°¨]","",Seconds)
      pattern="."
      pattern=str_extract(Valid,"[[:punct:]]")
      if(!is.na(pattern)){
        if(str_locate(Valid,paste0("[",pattern,"]"))[1]<=3)
        {
          Dec=str_sub(Valid,1,(str_locate(Valid,paste0("[",pattern,"]"))[1]-1))
          Valid2=gsub("[,.]","",Valid)
          Sec_Dec=str_sub(Valid2,(str_locate(Valid,paste0("[",pattern,"]"))[1]),-1)
          Second=as.double(Dec)+abs((ifelse(as.double(Sec_Dec)>100,as.double(Sec_Dec)/1000,as.double(Sec_Dec)/100)))
        }
      }else{
        if(as.double(Valid)>60)
        {
            Second=ifelse((as.double(Valid)/10)/60>1,as.double(Valid)/100,as.double(Valid)/10)
        }else{
            Second=Valid
        } 
      }
       
#       
      CoordVal=abs(as.double(Degr))+as.double(Minut)/60+as.double(Second)/3600
    }else{
      CoordVal=abs(as.double(Degr))+as.double(Minut)/60
    }
  }else if(str_detect(DMS,"°") & str_locate(DMS,"°")[1]>=3){
      CoordVal=CleaningMinutesDegrees(DMS)
  }
  if(!str_detect(DMS,"°") & (str_locate(DMS,"'")[1]>1 & str_locate(DMS,"'")[1]<5)){
        Modif=`str_sub<-`(DMS,str_locate(DMS,"'")[1],str_locate(DMS,"'")[1],"°")
        CoordVal=CleaningMinutesSecondsDegrees(Modif)
  } 
  return(CoordVal) 
}


Coord_DMS=Raw_data[str_detect(Raw_data[,nameCoorColumn],Pattern)|str_detect(Raw_data[,nameCoorColumn],Pattern2),nameCoorColumn]
pos_DMS=row.names(Raw_data)[str_detect(Raw_data[,nameCoorColumn],Pattern)|str_detect(Raw_data[,nameCoorColumn],Pattern2)]
Raw_data_Orig=Coord_DMS

Coord_DMS=gsub("''",'\"',Coord_DMS)
Coord_DMS=gsub("[º]","°",Coord_DMS)
Raw_data_Orig[42]


Coord_DMS_F=gsub(" ", "", Coord_DMS)
Coord_DMS_F=gsub("[-:]", "", Coord_DMS_F)
Coord_DMS_F=gsub("[W N w n o O]", " ", Coord_DMS_F)
# Coord_DMS_F2=gsub('', " ", Coord_DMS_F)

l=Coord_DMS_F[143]
Coord_DMS_F2=do.call(rbind,lapply(Coord_DMS_F, function(l){
  ext=str_locate_all(l,"\"")
  if(length(ext[[1]])>=2 & ext[[1]][1]<8)
    return(gsub('\"', "'", l))
  else 
    return(gsub('\"', " ", l))
}))



Coord_DMS_F3=str_split(Coord_DMS_F2,pattern = " ")

j=42
Coord_DMS_Correc=as.data.frame(do.call(rbind,lapply (Coord_DMS_F3, function (j){
  m=j[!j %in% c("",",","-",":")]
  longitud=0
  latitud=0
  obs=""
  if(length(m)==2)
  {
    i=2
    for(i in 1:length(m))
    {
      DMS=CleaningMinutesSecondsDegrees(m[i])
      if(DMS>=70)
        longitud=DMS
      else
        latitud=DMS
    }
  }else if(length(m)==1 & str_count(m,"'")<=2& str_locate_all(m,"'")[[1]][2]>str_locate(m,"'")[[1]][1] & str_length(m)>15 & !str_detect(m,"°")){
    aux=str_sub(m,1,4)
    pattern=","
    pattern=str_extract(aux,"[[:punct:]]")
    if(str_locate(m,pattern)[1]<=3){
      m=gsub(paste0("[",pattern,"]"),"°",m)
      auxVec=array()
      if(length(str_locate_all(m,"°")[[1]])>6){
        auxVec[1]=str_sub(m,(str_locate_all(m,"°")[[1]][3]-2),-1)
        auxVec[2]=str_sub(m,1,(str_locate_all(m,"°")[[1]][3]-3))
      }else{
        auxVec[1]=str_sub(m,(str_locate_all(m,"°")[[1]][2]-2),-1)
        auxVec[2]=str_sub(m,1,(str_locate_all(m,"°")[[1]][2]-3))
      }
      for(i in 1:2)
      {
        DMS=CleaningMinutesSecondsDegrees(auxVec[i])
        if(DMS>=70)
          longitud=DMS
        else
          latitud=DMS
      }
    }
  }else if(length(m)==1 & str_count(m,"'")>4){
    
    m=`str_sub<-`(m,str_locate(m,"'")[1],str_locate(m,"'")[1],"°")
    m=`str_sub<-`(m,str_locate_all(m,"'")[[1]][3],str_locate_all(m,"'")[[1]][3],"°")
    auxVec=array()
    auxVec[1]=str_sub(m,(str_locate_all(m,"°")[[1]][2]-2),-1)
    auxVec[2]=str_sub(m,1,(str_locate_all(m,"°")[[1]][2]-3))
    for(i in 1:2)
    {
      DMS=CleaningMinutesSecondsDegrees(auxVec[i])
      if(DMS>=70)
        longitud=DMS
      else
        latitud=DMS
    }
  }else if(length(m)==1 & str_locate_all(m,"°")[[1]][2]>str_locate(m,"°")[[1]][1])
  {
    auxVec=array()
    auxVec[1]=str_sub(m,(str_locate_all(m,"°")[[1]][2]-2),-1)
    auxVec[2]=str_sub(m,1,(str_locate_all(m,"°")[[1]][2]-3))
    for(i in 1:2)
    {
      DMS=CleaningMinutesSecondsDegrees(auxVec[i])
      if(DMS>=70)
        longitud=DMS
      else
        latitud=DMS
    }
  }else if(length(m)==1){
    DMS=CleaningMinutesSecondsDegrees(m)
    if(DMS>=70)
      longitud=DMS
    else
      latitud=DMS
    obs="Solo un dato"
  }else if(length(m)>2){
    m=m[str_detect(m,"°")]
    for(i in 1:length(m))
    {
      DMS=CleaningMinutesSecondsDegrees(m[i])
      if(DMS>=70)
        longitud=DMS
      else
        latitud=DMS
    }
    obs="Hay separación"
  }
  
  return(c(longitud,latitud,obs))
})))
z=3

for(z in 1:length(Coord_DMS_F3)){
  j=Coord_DMS_F3[[z]]
  
  cat(c(m,longitud,latitud,z,obs))
  cat("\n")
}
  


length(pos_DMS)
dim(Coord_DMS_Correc)
Coord_DMS_Correc[["Pos"]]=pos_DMS

### Joining and exporting the final File

CoordDef=rbind(Coord_DMS_Correc,Coord_DD_Correc)
CoordDef$Pos=as.integer(CoordDef$Pos)
CoordDef=CoordDef[order(CoordDef$Pos),]
Raw_data$Pos=as.integer(row.names(Raw_data))
Raw_data=Raw_data[order(Raw_data$Pos),]



write.csv(plyr::join_all(list(Raw_data,CoordDef),by="Pos"),file="AMTEC_CaribeHumedo_Coord_Corr.csv")

#################### pureba 222


