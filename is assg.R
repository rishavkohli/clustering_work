library(plyr)

index1<-0
index2<-0
j<-0
k<-0
#reading csv file from system
mal<-read.csv(file.choose(),header = T,sep = "\t")
count(mal,"Label")

#separating malacious data and non_malacious data
for(i in 1:length(mal[,1]))
 {  if(mal[i,"Label"]==1)
  {  j<-j+1
     index1[j]<-i
  } 
  else
  {
     k<-k+1
     index2[k]<-i
  }
  
 }
d1<-mal[-index1,]
count(d1,"Label")

d2<-mal[-index2,]
count(d2,"Label")
length(d1[,1])
d3<-data.frame()
d3<-d1[sample(1:length(d1[,1]),250000),] #taking almost same no of data rows as of malacious data

data0<-rbind(d2,d3) # merging mal. and non-mal. data into one


#defining data type of each column

data0$Source_byte<-as.numeric(data0$Source_byte)
data0$Destination_bytes<-as.numeric(data0$Destination_bytes)
data0$Dst_host_count<-as.numeric(data0$Dst_host_count)
data0$Dst_host_srv_count<-as.numeric(data0$Dst_host_srv_count)
data0$Destination_port_number<-as.numeric(data0$Destination_port_number)
data0$Source_port_number<-as.numeric(data0$Source_port_number)
data0$Service_type<-as.character(data0$Service_type)
data0$Flag<-as.character(data0$Flag)
data0$IDS_detection<-as.character(data0$IDS_detection)
data0$Malware<-as.character(data0$Malware)
data0$Ashula_detection<-as.character(data0$Ashula_detection)
data0$Source_IP_address<-as.character(data0$Source_IP_address)
data0$Destination_IP_address<-as.character(data0$Destination_IP_address)
data0$Type<-as.character(data0$Type)
data0$IP_count<-as.numeric(data0$IP_count)


# to count max. count of an element in a column 
max_count<-function(q)
{
  temp<-count(data0, q)
  v1<-temp[,"freq"]
  temp1<-0
  temp2<-0
  temp2<-as.vector(v1)
  l<-length(temp2)
  i<-1
  while(i<=l)
   {  if(temp1 <= temp2[i])
    {  temp1<-temp2[i]
    }
      i<-i+1
   }
  return(temp1)
}

#count standard deviation of a column

stand_dev<-function(q)
{ xx<-data0[[q]]
  sd1<-sd(xx)
  return(sd1)
}


#filtering data 
select_features<-function()
{
  cont_data<-c("Duration","Source_byte","Destination_bytes","IP_count","Same_srv_rate","S_error_rate","Srv_serror_rate","Dst_host_count","Dst_host_srv_count","Dst_host_same_src_port_rate","Dst_host_serror_rate","Dst_host_srv_serror_rate","Destination_port_number","Source_port_number")
  cat_data<-c("Service_type","Flag","IDS_detection","Malware","Ashula_detection","Source_IP_address","Destination_IP_address","Type")
  i=1
  #sapply(df.final,class)
  j=1
  features<-c()
  while(i <= length(cont_data))
   { t<-0
     t<- stand_dev(cont_data[i])
     if(t>2)
      {  features[j]<-cont_data[i]
        j<-j+1
      }
     i<-i+1
   }
  i=1
  j<-length(features)
  while(i <= length(cat_data))
    { t<-0
      t<- max_count(cat_data[i])
    if(t<(0.7*length(data0[,1])))
      {  features[j]<-cat_data[i]
         j<-j+1
      }
    i<-i+1
    }
  return(features)
}





sf1<-select_features()
sf1

#rejecting destination and source ip address as it is of no use and an overhead
sf1<-sf1[1:8]
sf1<-append(sf1,c("Label"),length(sf1))
sf1
sef1.data<-data.frame()
sef1.data <- subset(data0, select=sf1)



sapply(sef1.data,class)
unique(sef1.data[,9])
count(sef1.data,"Label")



# converting catagorical data into continous data

  i<-8
  fac_val1<-0
  fac_val1<-unique(sef1.data[,i])
  fac_val1<-as.vector(fac_val1)
  for(j in 1:length(fac_val1))
  { 
    for(k in 1:length(sef1.data[,i]))
    { if(sef1.data[k,i]==fac_val1[j])
       { sef1.data[k,i]<-j
       }
    }
  }

sef1.data$Service_type<-as.numeric(sef1.data$Service_type)
#sef1.data$Flag<-as.numeric(sef1.data$Flag)
#sef1.data$Label<-as.numeric(sef1.data$Label)
#sef1.data$Malware<-data0$Malware
#unique(sef1.data[,10])
#count(sef1.data,"Malware")




head<-colnames(sef1.data)
head
sapply(sef1.data,class)
#taking 60% of data for clustering and rest 40% is used for prediction
clus.data<-sef1.data[sample(1:length(sef1.data[,1]),(0.6*length(sef1.data[,1]))),]
count(clus.data,"Label")


#head<-head[! head %in%  "Destination_bytes" ]
#sef1.data <- subset(sef1.data, select=head)
#clus.data <- subset(clus.data, select=head)


sapply(clus.data,class)
#cl<-kmeans(clus.data[1:8],4)
##clus.data$clust<-cl$clust
#cl<-kmeans(clus.data[1:8],6)
#clus.data[sample(1:100,6),]
#count(clus.data, "clust")
#plot(Service_type~Flag,sef1.data,col=sef1.data$clust, pch=20)
#cl$centers
#length(sef1.data[,1])



# seperating data in 3 clusters
cl<-kmeans(clus.data[1:8],3)
clus.data$clust<-cl$clust

clus.data[sample(1:100,6),] #sample 
count(clus.data, "clust")


sapply(clus.data,class)
cl$centers

#sapply(cl$centers[1,head[1]],class)


#data for prediction
test.data<-sef1.data[sample(1:length(sef1.data[,1]),(0.4*length(sef1.data[,1]))),]
count(test.data,"Label")

sapply(test.data,class)


#checking cluster if it is malacious or not

c1_nomal<-0
c2_nomal<-0
c3_nomal<-0
c4_nomal<-0
c5_nomal<-0
c6_nomal<-0

c1_mal<-0
c2_mal<-0
c3_mal<-0
c4_mal<-0
c5_mal<-0
c6_mal<-0


for(i in 1:length(clus.data[,1]))
{  
  if(clus.data[i,"clust"]==1)
     { if(clus.data[i,"Label"]!=1)
        { c1_mal<-c1_mal+1
        }
       else
        { c1_nomal<-c1_nomal+1
        }
     }
  
  if(clus.data[i,"clust"]==2)
    {  if(clus.data[i,"Label"]!=1)
        { c2_mal<-c2_mal+1
        }
       else
        { c2_nomal<-c2_nomal+1
        }
    }
  
  if(clus.data[i,"clust"]==3)
    {  if(clus.data[i,"Label"]!=1)
        { c3_mal<-c3_mal+1
        }
       else
        { c3_nomal<-c3_nomal+1
        }
    }
  
  if(clus.data[i,"clust"]==4)
  {  if(clus.data[i,"Label"]!=1)
     { c4_mal<-c4_mal+1
     }
    else
     { c4_nomal<-c4_nomal+1
     }
  }
  
  if(clus.data[i,"clust"]==5)
  {  if(clus.data[i,"Label"]!=1)
      {c5_mal<-c5_mal+1
      }
    else
      { c5_nomal<-c5_nomal+1
      }
  }
  
  if(clus.data[i,"clust"]==6)
  {  if(clus.data[i,"Label"]!=1)
     {c6_mal<-c6_mal+1
     }
    else
     { c6_nomal<-c6_nomal+1
     }
    
  }
}
malware_cl1<-(c1_mal/(c1_mal+c1_nomal))*100
malware_cl2<-(c2_mal/(c2_mal+c2_nomal))*100
malware_cl3<-(c3_mal/(c3_mal+c3_nomal))*100
malware_cl1
malware_cl2
malware_cl3

# cluster 2 is malacious

head<-colnames(test.data)
head
head<-head[1:8]
#predicting data is malacious or not acc. to distance from cluster
for(i in 1:length(test.data[,1]))
{ sum_c1<-0
  sum_c2<-0
  sum_c3<-0
#sum_c4<-0
#sum_c5<-0
#sum_c6<-0
  for(j in 1:length(head))
  {
    diff_c1<-test.data[i,head[j]]-cl$centers[1,head[j]]
    diff_c2<-test.data[i,head[j]]-cl$centers[2,head[j]]
    diff_c3<-test.data[i,head[j]]-cl$centers[3,head[j]]
 #  diff_c4<-test.data[i,head[j]]-cl$centers[4,head[j]]
  # diff_c5<-test.data[i,head[j]]-cl$centers[5,head[j]]
   #diff_c6<-test.data[i,head[j]]-cl$centers[6,head[j]]
    if(diff_c1<0)
      { diff_c1=0-diff_c1
      }
    
    if(diff_c2<0)
      {diff_c2=0-diff_c2
      }
    
    if(diff_c3<0)
      {diff_c3=0-diff_c3
      }
    
#   if(diff_c4<0)
#   {diff_c4=0-diff_c4}
   
#   if(diff_c5<0)
#   {diff_c5=0-diff_c5}
    
 #  if(diff_c6<0)
#   {diff_c6=0-diff_c6}
   
   
    sum_c1=sum_c1+diff_c1
    sum_c2=sum_c2+diff_c2
    sum_c3=sum_c3+diff_c3
#    sum_c4=sum_c4+diff_c4
 #   sum_c5=sum_c5+diff_c5
  #  sum_c6=sum_c6+diff_c6
    
  }
     if(sum_c1==min(sum_c1,sum_c2,sum_c3))
      { test.data[i,"clust"]=1
      }
     if(sum_c2==min(sum_c1,sum_c2,sum_c3))
      { test.data[i,"clust"]=2
      }
     if(sum_c3==min(sum_c1,sum_c2,sum_c3))
      { test.data[i,"clust"]=3
      }
#if(sum_c4==min(sum_c1,sum_c2,sum_c3,sum_c4,sum_c5,sum_c6))
#{ test.data[i,"clust"]=4}

#if(sum_c5==min(sum_c1,sum_c2,sum_c3,sum_c4,sum_c5,sum_c6))
#{ test.data[i,"clust"]=5}

#if(sum_c6==min(sum_c1,sum_c2,sum_c3,sum_c4,sum_c5,sum_c6))
#{ test.data[i,"clust"]=6}

}




count(test.data, "clust")
count(test.data,"Label")

#making confusion matrix

pos_pos<-0
pos_neg<-0
neg_pos<-0
neg_neg<-0

for(i in 1:length(test.data[,1]))
{
  if(test.data[i,"Label"]!=1 && test.data[i,"clust"]==1 )
  {pos_pos<-pos_pos+1}
  if(((test.data[i,"Label"]==1) ||(test.data[i,"Label"]==0) ) && test.data[i,"clust"]==1)
  {neg_pos<-neg_pos+1}
  if(test.data[i,"Label"]!=1 && test.data[i,"clust"]==2)
  {pos_neg<-pos_neg+1}
  if(test.data[i,"Label"]!=1 && test.data[i,"clust"]==3)
  {pos_neg<-pos_neg+1}
  if(((test.data[i,"Label"]==1) ||(test.data[i,"Label"]==0) ) && test.data[i,"clust"]==2)
  {neg_neg<-neg_neg+1}
  if(((test.data[i,"Label"]==1) ||(test.data[i,"Label"]==0) ) && test.data[i,"clust"]==3)
  {neg_neg<-neg_neg+1}
 # if(test.data[i,"Label"]!=1 && test.data[i,"clust"]==4 )
#  {pos_pos<-pos_pos+1}
#  if(((test.data[i,"Label"]==1) ||(test.data[i,"Label"]==0) ) && test.data[i,"clust"]==4)
#  {neg_pos<-neg_pos+1}
  
  
  
  
}

#precision
pre<-(pos_pos/(pos_pos+neg_pos))
print("precision:::")
print(pre)

