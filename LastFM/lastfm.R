#データセットの読み込み
artists<-read.delim2("artists.dat")
user_artists<-read.delim2("user_artists.dat")

#縦がユーザ横がアーティストIDの行列を作成します。
user_artist.d<-matrix(0,nrow=length(unique(user_artists$userID)),ncol=length(unique(user_artists$artistID)))
colnames(user_artist.d)<-sort(unique(user_artists$artistID))
rownames(user_artist.d)<-sort(unique(user_artists$userID))

#データをCASTします。
for(i in 1:nrow(user_artists)){
   userID<-user_artists[i,"userID"]
   artistID<-user_artists[i,"artistID"]
   user_artist.d[paste(userID, sep='"'),paste(artistID, sep='"')]<-1
}


source("innerproduct.R")
artist.cross<-innserproduct(user_artist.d)


install.packages("lsa")
library(lsa)

artist.cos<-cosine(user_artist.d)


#行列を非類似度行列に変換
artist.dis<-1-artist.cos

artist.cos.res <- which(artist.cos>=0.6, arr.ind=TRUE) 
artist.cross.res <- which(artist.cross>=5, arr.ind=TRUE) 

artist.res<-merge(artist.cross.res,artist.cos.res)


artist.cos.name<-rownames(artist.cos)
artist.d<-data.frame(artist1=artist.cos.name[artist.res[,1]],artist2=artist.cos.name[artist.res[,2]])

#rm(tmp)
tmp<-merge(artist.d, artists,by.x="artist1", by.y="id")
colnames(tmp)<-c("artist1","artist2","artist1name" )
kekka<-merge(tmp, artists,by.x="artist2", by.y="id")
nrow(kekka)


artist.kekka<-matrix(0,length(unique(kekka$artist1name)),length(unique(kekka$artist1name)))
colnames(artist.kekka)<-unique(kekka$artist1name)
rownames(artist.kekka)<-unique(kekka$artist1name)
for(i in 1:nrow(kekka)){
   cos<-artist.cos[paste(kekka[i,"artist1"], sep='"'),paste(kekka[i,"artist2"], sep='"')]
   artist.kekka[paste(kekka[i,"artist1name"], sep='"'),paste(kekka[i,"name"], sep='"')]<-cos
}
artist.dis<-1-artist.kekka
diag(artist.dis)<-0
artist.hcl<-hclust(as.dist(artist.dis),method="average")
plot(artist.hcl,cex=0.8,lwd=0.1)

