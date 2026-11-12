library(data.table)
library(ggplot2)
out <- 'generated_by_codex/monitor20'
r <- fread('TcEight_example/Monitor20_07_25.txt',header=FALSE)
p <- tstrsplit(r$V2,' ')
t <- as.POSIXct(paste(sprintf('%04d-%02d-%02d',2000+as.integer(p[[3]]),match(p[[2]],month.abb),as.integer(p[[1]])),r$V3),tz='UTC')
stopifnot(ncol(r)==42,all(r$V6==20),!anyNA(t),all(diff(as.numeric(t))==60))
r[,`:=`(time=t-60,valid=V4==1)]
d <- melt(r,id.vars=c('time','valid'),measure.vars=paste0('V',11:42),value.name='value')
d[,channel:=as.integer(sub('V','',variable))-10L]
d[valid==FALSE,value:=NA_real_]
d[,day:=as.Date(time,tz='UTC')]
cov <- unique(d[,.(time,day,valid)])[,.(minutes=.N,valid_minutes=sum(valid)),by=day]
cov[,complete:=minutes==1440 & valid_minutes==1440]
fwrite(cov,file.path(out,'daily_coverage.csv'))
daily <- d[,.(total=sum(value,na.rm=TRUE),valid_minutes=sum(!is.na(value))),by=.(channel,day)]
daily <- merge(daily,cov[,.(day,complete)],by='day')
fwrite(daily,file.path(out,'daily_channel_totals.csv'))
active <- d[,.(peak=max(value,na.rm=TRUE)),by=channel][peak>0,sort(channel)]
origin <- as.POSIXct('2026-07-23 06:00:00',tz='UTC')
last <- floor(as.numeric(difftime(max(d[valid==TRUE,time])+60,origin,units='secs'))/86400)
png(file.path(out,'Monitor20_actogram.png'),width=3000,height=3400,res=160)
par(mfrow=c(4,4),mar=c(2.8,3.6,2,1),oma=c(3,0,4,0))
for(ch in active){
 a<-d[channel==ch]; x<-as.numeric(difftime(a$time,origin,units='hours')); peak<-max(a$value,na.rm=TRUE)
 plot(NA,xlim=c(0,48),ylim=c(last+.5,-.5),axes=FALSE,xlab='Hours since 06:00 reference',ylab='',xaxs='i',yaxs='i')
 for(day in 0:last){
  rect(0,day-.43,48,day+.43,col='#DBEAF2',border=NA)
  ix<-which(a$valid & x<day*24+48 & x+1/60>day*24)
  l<-pmax(0,x[ix]-day*24); rr<-pmin(48,x[ix]+1/60-day*24)
  rect(l,day-.43,rr,day+.43,col='white',border=NA)
  pos<-which(a$value[ix]>0)
  if(length(pos)) rect(l[pos],day+.4-.8*a$value[ix][pos]/peak,rr[pos],day+.4,col='#294E70',border=NA)
 }
 abline(v=seq(0,48,12),col='#b6c1cb',lwd=.5)
 axis(1,at=seq(0,48,12),cex.axis=.8)
 axis(2,at=0:last,labels=format(as.Date(origin,tz='UTC')+0:last,'%d %b'),las=1,cex.axis=.65)
 box(); title(main=sprintf('Channel %02d | max = %g',ch,peak),cex.main=.95)
}
mtext('Monitor20: double-plotted raw channel recordings',side=3,outer=TRUE,line=2,cex=1.5,font=2)
mtext('23 July - 11 August 2026 | Each row spans 48 hours',side=3,outer=TRUE,line=.4)
mtext('Blue: unavailable data | Independent channel scales | No light/dark schedule inferred',side=1,outer=TRUE,line=.5,cex=.9)
mtext('16 nonzero channels shown; 16 zero-only channels omitted. Readings appear to be environmental measurements.',side=1,outer=TRUE,line=1.8,cex=.9)
dev.off()
theme0<-theme_minimal(base_size=12)+theme(plot.title=element_text(face='bold'),panel.grid.minor=element_blank(),plot.caption=element_text(hjust=0))
vd<-daily[complete==TRUE]
vd[,label:=factor(sprintf('%02d',channel),levels=sprintf('%02d',1:32))]
v<-ggplot(vd,aes(label,total))+geom_violin(data=vd[channel %in% active],fill='#9bb9d2',color='#294e70',scale='width')+geom_point(position=position_jitter(width=.1,height=0,seed=20),size=1,alpha=.6)+stat_summary(fun=median,geom='point',shape=23,fill='white',size=2)+scale_x_discrete(limits=sprintf('%02d',1:32),drop=FALSE)+labs(title='Monitor20: distributions of daily channel totals',subtitle='24 July - 10 August 2026; 15 complete valid days',x='Original channel',y='Sum of raw values per day (units unverified)',caption='Each point is one day; diamonds mark medians. Zero-only channels show points only.\nChannels are unassigned; these readings appear to be environmental measurements.')+theme0
ggsave(file.path(out,'Monitor20_violin_plot.png'),v,width=15,height=8,dpi=180)
z<-d[day %in% cov[complete==TRUE,day]]
z[,zs:=if(sd(value)>0)(value-mean(value))/sd(value) else NA_real_,by=channel]
z[,bin:=(as.integer(format(time,'%H',tz='UTC'))*60+as.integer(format(time,'%M',tz='UTC'))) %/%30]
zd<-z[,.(mean_z=mean(zs)),by=.(channel,day,bin)]
profile<-zd[,.(mean_z=mean(mean_z),sd_between_days=sd(mean_z),n_days=.N),by=.(channel,bin)]
profile[,hour:=bin/2+.25]
fwrite(profile,file.path(out,'z_score_profiles.csv'),na='NA')
profile<-profile[channel %in% active]; profile[,label:=sprintf('Channel %02d',channel)]
zp<-ggplot(profile,aes(hour,mean_z))+geom_hline(yintercept=0,color='#b6c1cb')+geom_line(color='#294e70')+facet_wrap(~label,ncol=4)+scale_x_continuous(breaks=seq(0,24,6),limits=c(0,24))+labs(title='Monitor20: standardized daily channel profiles',subtitle='15 complete valid days; 30-minute clock bins',x='Recorded clock time (hours)',y='Mean within-channel z-score',caption='Each channel standardized using its mean and sample SD; daily profiles receive equal weight.\n16 zero-only channels have undefined z-scores and are omitted. Units and sensor assignments remain unverified.')+theme0
ggsave(file.path(out,'Monitor20_z_score_plot.png'),zp,width=13,height=11,dpi=180)
