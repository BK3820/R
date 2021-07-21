</pre>
c1=cluster(c2,0.5,10,2,10,"years","wtd1","wtd2",y)
Error in cluster(c2, 0.5, 10, 2, 10, "years", "wtd1", "wtd2", y) :
unused arguments ("years", "wtd1", "wtd2", y)
but I run that R modified file only, that to up to this code only…
 
install.packages("rgl")
install.packages("dbscan")
install.packages("cluster")
library(dbscan)
library(rgl)
library(cluster)
cluster=function(x,eps,mpt,dim,k)
{
if(dim==1)
{
x[is.na(x)] <- round(mean(x, na.rm = TRUE))
}else
{
for(i in 1:dim)
{
x[,i][is.na(x[,i])] <- round(mean(x[,i], na.rm = TRUE))
}
}
if(dim==1) n=length(x)
if(dim>1) n=length(x[,1])
d=dbscan(x,eps,mpt)
k=max(d$cluster)
if(k==0)
{
print("Cannot Run DBSCAN. Please Check Values")
stop()
}
 
 
mean=array(dim=c(k,dim))
seq1=c(1:k)/(k+1)
seq2=c(k:1)/(k+1)
mean[,1]=quantile(x[,1],seq1)
 
if(dim==2)
{
c=cor(x[,1],x[,2])
if(c>0)
{
mean[,2]=quantile(x[,2],seq1)
}else
{
mean[,2]=quantile(x[,2],seq2)
}
}
 
if(dim>2)
{
a=array(dim=c((dim-1),(dim-1)))
for(i in 1:(dim-1))
{
for(j in 2:dim)
{
a[i,j-1]=cor(x[,i],x[,j])
}
}
 
for(i in 1:(dim-1))
{
for(j in 2:dim)
{
if(a[i,(j-1)]>0)
{
mean[,j]=quantile(x[,j],seq1)
} else
{
mean[,j]=quantile(x[,j],seq2)
}
}
}
}
Kclus=array()
clus=array(list())
for(i in 1:n)
{
dist=array()
for(j in 1:k)
{
dist[j]=dist(rbind(as.vector(x[i,]),as.vector(mean[j,])))
}
m=which(dist==min(dist))
clus[m]=list(unlist(c(clus[m],i)))
Kclus[i]=m
for(i in 1:dim)
{
mean[m,i]=mean(c(mean[m,i],x[unlist(clus[m]),i]))
}
}
ssw=0
for (i in 1:k)
{
for(j in 1: length(unlist(clus[i])))
{
for(l in 1:dim)
{
ssw=ssw+((x[unlist(clus[i])[j],l]-mean[i,l])**2)
}
}
}
tss=0
for(i in 1:dim)
{
tss=tss+sum((x[,i]-mean(x[,i]))**2)
}
ssb=tss-ssw
 
if (dim<=2)
{
par(mfrow=c(1,2))
clusplot(x,Kclus,main="k-means clusters")
 
 
clusplot(x,d$cluster,main="dbscan clusters")
}
if(dim>2)
{
open3d()
clk <- as.factor(Kclus)
plot3d(x, col=clk, main="k-means clusters")
open3d()
cld <- as.factor((d$cluster)+1)
plot3d(x, col=cld, main="dbscan clusters")
}
 
print("dbscan cluster")
print(d)
print("k-means cluster")
num=unlist(lapply(clus,length))
da=data.frame(1:k,num)
colnames(da)=c("Cluster Number", "No of elements")
print(da)
print("SSB as a percent Of TSS")
s=ssb*100/tss
print(s)
}
<pre>
