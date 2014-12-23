setwd("E:/Dropbox/touchparse_stroke")
source("C:/R.source/my.fig.R")
source("C:/R.source/common.R")
source("C:/R.source/legend_col.R")
source("C:/R.source/rey.f.R")
source("C:/R.source/yx.common.R")
# utils:::menuInstallPkgs()
# install.packages("")
require(Hmisc)
require(plotrix)
library(Hmisc)
library(plotrix)

name.d = c("time", "type", "event", "value")
name.d2 = c("t", "sl", "id", "lv", "x", "y", "prs")
name.d4 = c("i", "type", "multi", "id", "near.id",
            "sum.l", "sum.t", "mean.prs", "sd.prs",
            "mean.s", "mean.v", "sd.v",
            "xmean", "ymean", "xsd", "ysd",
            "acc", "accmax", "accmean", "accend",
            "rt", "disp", "dd", "over")
#name.std = c("t", "rt", "type", "x", "y", "ex", "ey", "h")
name.std = c("t", "at", "rt", "order", "gn", "pn", "p", "pc")
none = -9



# utils:::menuInstallPkgs()
#設定讀取路徑

#每個玩家遊戲時間的資料
gdir <- "fnstd/"

#原始log檔
ldir <- "fnlog/"

#處理後的log檔
adir <- "d2/"


#設定參數
ntect = -9 #設定none detect為-9
time_limit = 60 #game log檔至少要60S以上
gcode = read.table("gn_new.txt", header=T, sep="\t", as.is=T, encoding="UTF-8") #讀取game列表
p.list=c('1201sub01','1202sub01','1202sub02', '1203sub01', '1205sub01','1205sub02') #設定player列表


for(p in p.list){
  
  #讀取game name, start time, end time 到gl
  #stdand data
  fn=paste("fnstd/",p,".csv",sep="")
  std = read.table(fn, header=F, as.is=T, sep=",")
  names(std) = name.std
  sink("std.txt")
  for(i in unique(std$order)){
    i.s = min(which(std$order==i))
    i.e = max(which(std$order==i))
    cat(std$pn[i.s], std$t[i.s], std$t[i.e],"\n")
  }
  sink()
  
  
  #替gl加上遊戲代號 (game code)
  gl=data.frame()
  gl=read.table("std.txt")
  names(gl)=c("pn", "st", "et") #設定column name
  gc <- sapply(1:len(gl$pn),function(x) gcode$gc[gcode$pn==gl$pn[x]])
  gl$code <- gc
  gl <- gl[order(gl$st),]
  #替gl加上遊戲評價 (game evaluation)
  fn=paste("fnstd/",p,"_e.csv",sep="")
  std_e = read.table(fn, header=F, as.is=T)
  names(std_e) = c("all", "action", "content") #設定column name
  gl <- cbind(gl,std_e)
  #save the std data
  fn.std = paste("std/",p,".csv",sep="")
  write.table(gl, file = fn.std)
  
  
  #讀取touch log data到d
  fn=paste("fnlog/",p,".s",sep="")
  alld = read.table(fn, header=F, as.is = T)
  alld = alld[,-1]
  alld[1] = unlist(strsplit(as.character(alld[,1]), "]"))
  names(alld)=name.d
  for(i.g in 1:nrow(gl)){
    i.s = min(which(as.numeric(alld$time)>gl$st[i.g]))       #這玩家在這款遊戲的起始rownumber
    i.e = max(which(as.numeric(alld$time)<gl$et[i.g]))       #這玩家在這款遊戲的結束rownumber
    name.info = paste(p ,gl$code[i.g],sep=" ")
#     cat(name.info, gl$st[i.g], alld$time[i.s], gl$et[i.g], alld$time[i.e], "\n",sep=" ")
    cat(name.info, i.s, i.e, "\n",sep=" ")
    
    if(i.e<=i.s) {        #若結束時間比起始時間早，bug 跳掉
      print("Start, End time wrong" )
      next
    }
    
    if(gl$et[i.g]-gl$st[i.g]<time_limit) {        #若結束時間比起始時間早，bug 跳掉
      print("log time is too short in this game" )
      next
    }
    
    d = alld[i.s:i.e,]
    source("p_sony_log.R")
    fn.d2 = paste("d2/",p,"/",p,"_",gl$code[i.g],".txt",sep="")
    write.table(d2, file = fn.d2)
  }
  
}  
