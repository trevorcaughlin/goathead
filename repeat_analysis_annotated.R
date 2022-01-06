library("sp")
library("rgdal")
library("raster")
library("rgeos")
library("lubridate")

setwd("C:/Users/trevorcaughlin/Dropbox/Goatheads")

pts=readOGR("repeat_routes_not_filtered_for_distance",dsn=".")

plts0=readOGR("repeat_plots_all_covariates",dsn=".")
plts<-plts0

plts$plot_area<-gArea(plts,byid=TRUE)

pt10<-pts[which(pts@data$distance<10),] #collectively decided on with group
#4387 points
table(pt10$join_Name) #check of how many are in each plot


pt20<-pt10

which(pt20$join_id %in% plts$id==F)
#checking whether there are any ids that are present in the point dataset but missing from plots

unique(pt20$Date)

pt20$Date=as.Date(pt20$Date) #making sure R recognizes this as a date

pt20<-pt20[order(pt20$Date),] #this isn't necessary but maybe helps viewing

#date processing

setwd("C:/Users/trevorcaughlin/Dropbox/Goatheads/goathead_repeat_routes_again")

route_dat<-read.csv("interval_try.csv")

route_dat$Walker<-trimws(route_dat$Walker)

ori_intervals<-as.Date(route_dat$Final.date.intervals,origin = "1899-12-30")

start_day<-ymd("2020-05-22")
end_day<-ymd("2020-10-17")

#intervals<-ori_intervals
intervals<-seq(from=start_day,to=end_day,by=7)

walk_dates<-as.Date(route_dat$Date,origin = "1899-12-30")

int_vec<-vector("list",length(intervals)-1) #vector of intervals. length is minus 1 because it is intervals

for(i in 1:length(int_vec)){
  int_vec[[i]]<-interval(intervals[i], intervals[i+1]) #the i+1 here ensures that the end_day is put in correctly
}

#21 intervals

membership<-rep(NA,times=length(walk_dates)) #there are all the dates that were walked

#this is so we can know which data is missing for each walk
for(k in 1:length(walk_dates)){
  
  turtle_vector<-rep(NA,times=length(int_vec)) #for each walk date, it is putting it into an interval category
  
  for(l in 1:length(int_vec)){
    
    turtle_vector[l]<-walk_dates[k] %within% int_vec[[l]] 
    
  }
  if(sum(turtle_vector)==0){ #for any that are not in any of the walk dates
    membership[k]<-NA
  }
  else{
    membership[k]<-which(turtle_vector==T)[1] #storing the numeric id of the interval(from 1 to 21)
  }
}

walk_dates[which(is.na(membership==T))] #Only one walk date was missing from this

membership[which(route_dat$Walker=="Caughlin")]
membership[which(route_dat$Walker=="Zaiats")]
membership[which(route_dat$Walker=="Louis")]
membership[which(route_dat$Walker=="Matt C")]
membership[which(route_dat$Walker=="Kolarik")]


###next:clean up dates a bit?
##take the first date of belonging using [1]
#i=37 is a good place to start
ab_mat<-matrix(NA,nrow=nrow(plts),ncol=length(int_vec))

for(i in 1:nrow(plts)){

  pl_sub<-plts@data[i,] #this is a single plot
  
if(pl_sub$id %in% pt20$join_id ==F){ #if no plots are in pt20, this sets it equal to 0
  ab_mat[i,]=rep(0,times=length(int_vec))
}

    else{
  
    
pt20sub<-pt20@data[which(pt20@data$join_id %in% pl_sub$id),] #this is a subset of points to the pl_sub

col_vec<-rep(NA,times=nrow(pt20sub))

  for(j in 1:length(int_vec)){
    for(k in 1:nrow(pt20sub)){
  
    if(pt20sub$Date[k] %within% int_vec[[j]]==T){
      
      col_vec[k]<-j #this forces it to be the second one that is recorded (if it is on one of the end dates of the interval)
    #I think this is ok--it is at least as arbitrary as doing the first date?
      #alternately: you could add minutes or hours to dates and have intervals be at midnight, while collection days are at noon
      }
      else{next}

  }
  }

ab_mat[i,unique(col_vec)]=1
  
}
}


ab_mat<-data.frame(ab_mat)

for(l in 1:ncol(ab_mat)){
  colnames(ab_mat)[l]<-as.character(int_vec[[l]])
}


ab_mat[which(is.na(ab_mat),arr.ind=T)]<-0

#now you need to go through and make it so that there are NAs for the intervals that were not censused#

#this is taking out all of the periods that lack censuses for that time period
#decision point
#decision point
#decision point

ab_pro<-ab_mat

ab_pro[which(plts$Name=="Trevor"), which(c(1:ncol(ab_mat)) %in% membership[which(route_dat$Walker=="Caughlin")] ==F)]<- NA 
ab_pro[which(plts$Name=="Andrii"), which(c(1:ncol(ab_mat)) %in% membership[which(route_dat$Walker=="Zaiats")] ==F)]<- NA 
#ab_pro[which(plts$Name=="kolarik_east"), which(c(1:ncol(ab_mat)) %in% membership[which(route_dat$Walker=="Kolarik")] ==F)]<- NA 
#ab_pro[which(plts$Name=="kolarik_west"), which(c(1:ncol(ab_mat)) %in% membership[which(route_dat$Walker=="Kolarik")] ==F)]<- NA 
ab_pro[which(plts$Name=="Jochems_FederalWay"), which(c(1:ncol(ab_mat)) %in% membership[which(route_dat$Walker=="Louis")] ==F)]<- NA 
ab_pro[which(plts$Name=="MattCPath2"), which(c(1:ncol(ab_mat)) %in% membership[which(route_dat$Walker=="Matt C")] ==F)]<- NA 

sum(ab_mat)
#1143 points

sum(ab_mat)-sum(na.omit(unlist(ab_pro)))
#lost 36 points. these are points that are located in the NA columns that were set as NA because they weren't recorded as in the plots.


#this below is a fix to that--assuming off points are correct.
#assume off points are correct (or bridge gap between censuses? this is a question for analysis)
ab_pro2<-matrix(NA,ncol=ncol(ab_pro),nrow=nrow(ab_pro))

for(m in 1:nrow(ab_pro)){
  for(n in 1:ncol(ab_pro)){
    
    if(is.na(ab_pro[m,n])==T & ab_mat[m,n]==1) {
      ab_pro2[m,n]<-1
    }
    else{
      ab_pro2[m,n]<-ab_pro[m,n] #assuming all other points are NA
    }
    }
}


sum(ab_mat)-sum(na.omit(c(ab_pro2)))
#
colonization<-matrix(NA,ncol=ncol(ab_pro),nrow=nrow(ab_pro))
extinction<-matrix(NA,ncol=ncol(ab_pro),nrow=nrow(ab_pro))


for(p in 1:nrow(ab_pro)){
  for(q in 2:ncol(ab_pro)){
    
if(is.na(ab_pro2[p,q-1])==T){
  colonization[p,q]=NA
  extinction[p,q]=NA}
    
   else{
     if(ab_pro2[p,q-1]==1){
       colonization[p,q]=NA
       extinction[p,q]=1-ab_pro2[p,q]}
     
       else{
         colonization[p,q]=ab_pro2[p,q]
         extinction[p,q]=NA
         }
        }
    }
}



repplts=do.call("rbind", replicate(n=ncol(ab_pro2), plts@data, simplify = FALSE))

repplts$colonization<-c(colonization)
repplts$extinction<-c(extinction)

repplts$time_interval<-rep(colnames(ab_mat),each=nrow(ab_pro2))
repplts$time_num<-as.numeric(as.factor(repplts$time_interval))

stdize<-function(x) {(x-mean(x))/(2*sd(x))}

repplts$land_value_std<-stdize(repplts$join_TOTAL/repplts$join_ACRES)
repplts$bare_std<-stdize(repplts$lc_30_geo)
repplts$betweenness_std<-stdize(repplts$join_BC)

repplts$parcel<-as.factor(repplts$join_PARCE)
repplts$time_interval<-as.factor(repplts$time_interval)
repplts$block_id<-as.factor(repplts$Name)

colonizer_stats<-repplts[-which(is.na(repplts$colonization)==T),]
extinction_stats<-repplts[-which(is.na(repplts$extinction)==T),]

sum(colonizer_stats$colonization)
sum(extinction_stats$extinction)


library("brms")

#test whether including plot area affects things
#test whether id of block affects things
#test autoregressive effects

#
#http://paul-buerkner.github.io/brms/reference/ar.html

  
  
  co2<-colonizer_stats[which(colonizer_stats$block_id=="Jochems_FederalWay"),]
  sum(co2$colonization)/nrow(co2)
  
  co2<-colonizer_stats[which(colonizer_stats$block_id=="Andrii"),]
  sum(co2$colonization)/nrow(co2)
  
  co2<-colonizer_stats[which(colonizer_stats$block_id=="Trevor"),]
  sum(co2$colonization)/nrow(co2)
  
  
  co2<-colonizer_stats[which(colonizer_stats$block_id=="MattCPath2"),]
  sum(co2$colonization)/nrow(co2)
  
  co2<-colonizer_stats[which(colonizer_stats$block_id=="kolarik_east"),]
  sum(co2$colonization)/nrow(co2)
  co2<-colonizer_stats[which(colonizer_stats$block_id=="kolarik_west"),]
  sum(co2$colonization)/nrow(co2)
  
  write.csv(colonizer_stats,file="colonization.csv",row.names=F)
  write.csv(extinction_stats,file="extinction.csv",row.names=F)
  
  #colonizer_stats<-colonizer_stats[-which(colonizer_stats$block_id %in% c("kolarik_west","kolarik_east")),]
  
  #colonizer_stats<-colonizer_stats[-which(colonizer_stats$block_id %in% c("MattCPath2")),]
  
  cor(colonizer_stats$land_value_std,colonizer_stats$betweenness_std)
  cor(colonizer_stats$land_value_std,colonizer_stats$b_bare_std)
  cor(colonizer_stats$land_value_std,colonizer_stats$bare_std)
  cor(extinction_stats$land_value_std,extinction_stats$bare_std)
  cor(extinction_stats$land_value_std,extinction_stats$betweenness_std)
  cor(extinction_stats$bare_std,extinction_stats$betweenness_std)
  
  
  library("brms")
  
  mextinct<-brm(extinction~land_value_std+bare_std+betweenness_std
                +(1|parcel)+(1|time_interval),data=extinction_stats,
                family="bernoulli",cores=4,
                set_prior(horseshoe(df=1)),control=list(adapt_delta = 0.9))
  
  
  
  mcolonization<-brm(colonization~land_value_std+bare_std+betweenness_std
                     +(1|parcel)+(1|time_interval),data=colonizer_stats,
                     family="bernoulli",cores=4,
                     set_prior(horseshoe(df=1)),control=list(adapt_delta = 0.9))
  
  library(lme4)
  mcolonization<-glmer(colonization~land_value_std+bare_std+betweenness_std
                     +(1|parcel)+(1|time_interval),data=colonizer_stats,
                     family="binomial")
  