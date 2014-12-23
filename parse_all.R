#setwd("E:/Dropbox/touchparse_stroke")
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
name.d5 = c("i", "id", "t", "l","prs")
name.d7 = c("i", "id", "order", "prs", "mprs", "tlab", "x1", "y1",
            "dnum", "lacc", "multi", "main")
name.nd7 = c("i", "id", "order", "prs", "mprs", "tlab", "x1", "y1",
             "dnum", "lacc", "multi", "main", "first.rf",
             "o.prs", "o.x", "o.y",
             "rel.t", "rel.prs", "rel.x", "rel.y", "rel.d")
name.std = c("t", "rt", "lt", "type", "s", "x", "y", "ex", "ey", "h")

none = -9
col.name = c("red", "sienna1", "gold", "palegreen3", "cornflowerblue",  
             "cyan3")
mypch = c(2,3,4,7,9,22)

p.list=c('1201sub01','1202sub01','1202sub02')
fn = list.files("fnlog")
# 1st parst and feature parse=======
for(p in p.list){
  
  # touch log data
  fn=paste("fnlog/",p,"_02.s",sep="")
  d = read.table(fn, header=F, as.is = T)
  d = d[,-1]
  d[1] = unlist(strsplit(as.character(d[,1]), "]"))
  source("p_sony_log.R")
  fn.d2 = paste("d2/",p,"/",p,"_",gl$code[i.g],"_02.txt",sep="")
  write.table(d2, file = fn.d2)

  fn.d2 = paste("d2/",fn[f],sep="")
  d2 = read.table(file = fn.d2)
  
  #stdand data
  fn=paste("fnstd/",p,"_02.csv",sep="")
  std = read.csv(fn, header=F, as.is=T, encoding="UTF-8")
  names(std) = name.std
  l.type = unique(std$type)
  l.hand = unique(std$h)
  for(h in l.hand){
    for(t in l.type){
      id.t = which(std$type==t)
      id.h = which(std$h==h)
      id = intersect(id.t, id.h)
      if(len(id)){
        substd = std[id,]
        fn.d4 = paste("d4/", gsub(".s", "", fn[f], fixed=T),
                      "_", h, "_", t, ".txt", sep="")
        source("p_feature.R")
      }
    }
  }
  
}

# multitouch========================
for(f in 1:len(fn)){
  
  fn.d2 = paste("d2/",fn[f],sep="")
  d2 = read.table(file = fn.d2)
  
  #stdand data  
  std = read.csv(fn.std[f], header=F, as.is=T, encoding="UTF-8")
  names(std) = name.std
  l.type = unique(std$type)
  l.hand = unique(std$h)
  for(h in l.hand){
    for(t in l.type){
      id.t = which(std$type==t)
      id.h = which(std$h==h)
      id = intersect(id.t, id.h)
      if(len(id)){
        substd = std[id,]
        
        # Find the main id (the nearest touch is main.id)
        fn.d4 = paste("d4/", gsub(".s", "", fn[f], fixed=T),
                      "_", h, "_", t, ".txt", sep="")
        d4 = read.table(file = fn.d4)
        
        fn.d7 = paste("d7/", gsub(".s", "", fn[f], fixed=T),
                      "_", h, "_", t, ".txt", sep="")
        source("p_multi.R")
      }
    }
  }
  
}

fns = list.files("d7", full=T)
for(f in 1:len(fns)){
  d7 = read.table(fns[f])
  names(d7) = name.d7
  
  nd7={}
  for(j in unique(d7$i)){
    isd = which(d7$i == j)
    
    if(d7$main[isd[1]] == "R")
      first.rf = "R"
    else
      first.rf = "f"
    
    #       main = order(d7$main.key[isd])
    o.prs = order(d7$prs[isd], decreasing = T)
    
    rel.t = d7$tlab[isd]-min(d7$tlab[isd])
    
    rel.prs = d7$prs[isd]/max(d7$prs[isd])
    rel.d = d7$dnum[isd]/max(d7$dnum[isd])
    rel.x = d7$x1[isd]-d7$x1[isd[1]]
    rel.y = d7$y1[isd]-d7$y1[isd[1]]
    o.x = order(d7$x1[isd], decreasing = T)
    o.y = order(d7$y1[isd], decreasing = T)
    
    td7 = cbind(d7[isd,], first.rf, o.prs, o.x, o.y,
                rel.t, rel.prs, rel.x, rel.y, rel.d)
    nd7 = rbind(nd7, td7)
    
  }
  fn.d7 = gsub("d7", "nd7", fns[f])
  write.table(nd7, file = fn.d7)
}

# draw trajectory ===============
for(f in 1:35){
  
  fn.d2 = paste("d2/",fn[f],sep="")
  d2 = read.table(file = fn.d2)
  
  #stdand data  
  std = read.csv(fn.std[f], header=F, as.is=T, encoding="UTF-8")
  names(std) = name.std
  source("p_draw.R")

}

# draw time vs slop================= 
for(f in 1:len(fn)){
  
  fn.d2 = paste("d2/",fn[f],sep="")
  d2 = read.table(file = fn.d2)
  
  #stdand data  
  std = read.csv(fn.std[f], header=F, as.is=T, encoding="UTF-8")
  names(std) = name.std
  l.type = unique(std$type)
  l.hand = unique(std$h)
  for(h in l.hand){
    for(t in l.type){
      id.t = which(std$type==t)
      id.h = which(std$h==h)
      id = intersect(id.t, id.h)
      if(len(id)){
        substd = std[id,]
        fn.d5 = paste("d5/", gsub(".s", "", fn[f], fixed=T),
                      "_", h, "_", t, ".txt", sep="")
        source("p_draw_ts.R")
      }
    }
  }
  
}


l.type = c("tap", "lp", "sld")
l.hand = c("a", "aw", "u")
name.type = c("tap", "long press", "slide")
name.hand = c("affected hand", "affected hand with assist", "unaffected hand")
pic.name = paste("pic/time_vs_drift.png", sep="")
png(file = pic.name, width = 900, height = 900)
par(mfrow=c(3,3))
for(lt in 1:len(l.type)){
  d={}
  for(lh in 1:len(l.hand)){
    fn = paste("d5all/",l.hand[lh],"_", l.type[lt], ".txt",sep="")
    d5 = read.table(fn)
    names(d5) = name.d5
    
    
    if(l.type[lt] == "tap"){
      xlim = range(at$t)
      ylim = range(at$l)
    }
    if(l.type[lt]=="sld"){
      xlim = range(as$t)
      ylim = range(as$l)
    }
    if(l.type[lt]=="lp"){
      xlim = range(al$t)
      ylim = range(al$l)
    }
    
    plot(d5$t, d5$l, type="n", 
         #          log="y",
         main=paste(name.type[lt], "using", name.hand[lh]),
         xlim = xlim, 
         ylim = ylim,
         xlab = "time (sec)",
         ylab = "drifting distance (pixel)")
    for(i in unique(d5$id)){
      isd = which(d5$id == i)
      lines(d5$t[isd], d5$l[isd], lwd = 0.5, col = 3)
    }
    abline(h=12, col = "gray60", lty = 2, lwd = 2)
  }
  
}
dev.off()

library(plotrix)
fns = list.files("d5", full=T,pattern="point")
subfns = list.files("d5",pattern="point")
for(fn in 75:len(fns)){
  d5 = read.table(fns[fn])
  names(d5) = name.d5
  for(i in unique(d5$i))
  {
    isd = which(d5$i == i)
#     xlim = range(al$t)
#     ylim = range(al$prs)
    
    main = paste(gsub(".txt","",subfns[fn]), "_", i, sep="")
    pic.name = paste("pic_prs_tap/", main, ".png", sep="")
    png(file = pic.name, width = 800, height = 600)


    twoord.plot(d5$t[isd], d5$l[isd],d5$t[isd], d5$prs[isd],
                xlab="time (sec)", 
                ylab="movement distance (pixel)",
                rylab="pressure",
                main=main, type=c("s","s"), lpch=1, rpch=10,
                do.first="plot_bg();grid(col=\"white\",lty=1)")
    abline(v=0.5, col = 4, lty = 2, lwd = 2)
    dev.off()
  }
  
}

for(lt in 1:len(l.type)){
  d={}
  for(lh in 1:len(l.hand)){
    fn = paste("d5all/",l.hand[lh],"_", l.type[lt], ".txt",sep="")
    d5 = read.table(fn)
    names(d5) = name.d5
    
    
    if(l.type[lt] == "tap"){
      xlim = range(at$t)
      ylim = range(at$prs)
    }
    if(l.type[lt]=="sld"){
      xlim = range(as$t)
      ylim = range(as$prs)
    }
    if(l.type[lt]=="lp"){
      xlim = range(al$t)
      ylim = range(al$prs)
    }
    
    main=paste(name.type[lt], "using", name.hand[lh])
    pic.name = paste("pic/", main,"time_vs_press.png", sep="")
    png(file = pic.name, width = 900, height = 900)
    plot(d5$t, d5$prs, type="n", 
         #          log="y",
         main=main,
         xlim = xlim, 
         ylim = ylim,
         xlab = "time (sec)",
         ylab = "pressure")
    for(i in unique(d5$id)){
      isd = which(d5$id == i)
      lines(d5$t[isd], d5$prs[isd], lwd = 0.5, col = 3)
    }
    abline(h=0.5, col = "gray60", lty = 2, lwd = 2)
    dev.off()
  }
  
}

# read all data=====================
fns = list.files("d4", full=T)
ut = ul = us = at = al = as = awt = awl = aws = data.frame()
ut = at = awt = data.frame()
for(fn in fns)
{
  d4 = read.table(fn)
  names(d4) = name.d4
  if(len(grep("_u_", fn))){
    if(len(grep("point",fn))){
      ut = rbind(ut, d4)
    }else if(len(grep("line", fn))){
      us = rbind(us, d4)
    }else{
      ul = rbind(ul, d4)
    }
    
  }else if(len(grep("_a_", fn))){
    if(len(grep("point", fn))){
      at = rbind(at, d4)
    }else if(len(grep("line", fn))){
      as = rbind(as, d4)
    }else{
      al = rbind(al, d4)
    }
    
  }else{
    if(len(grep("point", fn))){
      awt = rbind(awt, d4)
    }else if(len(grep("line", fn))){
      aws = rbind(aws, d4)
    }else{
      awl = rbind(awl, d4)
    }
    
  }
}


# write all data=====================
fns = list.files("d4use", full=T)
ut = ul = us = at = al = as = awt = awl = aws = data.frame()
for(fn in fns)
{
  d4 = read.table(fn)
  names(d4) = name.d4
  if(len(grep("_u_", fn))){
    if(len(grep("point", fn))){
      ut = rbind(ut, d4)
      fn.dall = paste("dall/u_tap.txt",sep="")
      write.table(ut, file = fn.dall)
    }else if(len(grep("line", fn))){
      us = rbind(us, d4)
      fn.dall = paste("dall/u_sld.txt",sep="")
      write.table(us, file = fn.dall)
    }else{
      ul = rbind(ul, d4)
      fn.dall = paste("dall/u_lp.txt",sep="")
      write.table(ul, file = fn.dall)
    }
      
  }else if(len(grep("_a_", fn))){
    if(len(grep("point", fn))){
      at = rbind(at, d4)
      fn.dall = paste("dall/a_tap.txt",sep="")
      write.table(at, file = fn.dall)
    }else if(len(grep("line", fn))){
      as = rbind(as, d4)
      fn.dall = paste("dall/a_sld.txt",sep="")
      write.table(as, file = fn.dall)
    }else{
      al = rbind(al, d4)
      fn.dall = paste("dall/a_lp.txt",sep="")
      write.table(al, file = fn.dall)
    }
      
  }else{
    if(len(grep("point", fn))){
      awt = rbind(awt, d4)
      fn.dall = paste("dall/aw_tap.txt",sep="")
      write.table(awt, file = fn.dall)
    }else if(len(grep("line", fn))){
      aws = rbind(aws, d4)
      fn.dall = paste("dall/aw_sld.txt",sep="")
      write.table(aws, file = fn.dall)
    }else{
      awl = rbind(awl, d4)
      fn.dall = paste("dall/aw_lp.txt",sep="")
      write.table(awl, file = fn.dall)
    }
      
  }
}

af.s = af.t = {}
for(fn in fns)
{
  d4 = read.table(fn)
  names(d4) = name.d4
  if(len(grep("line", fn))){
    rect.sld = mean(d4$rt)
    speed.sld = mean(d4$mean.s)
    accs.sld = mean(d4$acc)
    acce.sld = mean(d4$acce)
    sr.sld = 1-len(union(which(d4$acc > 48), which(d4$accend > 48)))/nrow(d4)
#     f.s = c(avg.rt, avg.speed, sr)
#     af.s = rbind(af.s, f.s)
  }else if(len(grep("point", fn))){
    rect.tap = mean(d4$rt)
    durt.tap = mean(d4$sum.t)
    acc.tap = mean(d4$acc) 
    sr.tap = 1-len(union(union(which(d4$acc > 48), which(d4$sum.l > 12)), which(d4$time > 0.5)))/nrow(d4)
#     f.t = c(avg.rt, avg.d, avg.acc, sr)
#     af.t = rbind(af.t, f.t)
  }else
    next
  write.table(af.s, file="recommend.sld.txt", row.names=F, col.names=F)
  write.table(af.t, file="recommend.tap.txt", row.names=F, col.names=F)
}
af.s = read.table(file="recommend.sld.txt")
af.t = read.table(file="recommend.tap.txt")

# write all data=====================
fns = list.files("d4normal", full=T)
rt = rl = rs = lt = ll = ls = data.frame()
for(fn in fns)
{
  d4 = read.table(fn)
  names(d4) = name.d4
  if(len(grep("_u_", fn))){
    if(len(grep("point", fn))){
      rt = rbind(rt, d4)
      fn.dall = paste("dall/r_tap.txt",sep="")
      write.table(rt, file = fn.dall)
    }else if(len(grep("line", fn))){
      rs = rbind(rs, d4)
      fn.dall = paste("dall/r_sld.txt",sep="")
      write.table(rs, file = fn.dall)
    }else{
      rl = rbind(rl, d4)
      fn.dall = paste("dall/r_lp.txt",sep="")
      write.table(rl, file = fn.dall)
    }
    
  }else{
    if(len(grep("point", fn))){
      lt = rbind(lt, d4)
      fn.dall = paste("dall/l_tap.txt",sep="")
      write.table(lt, file = fn.dall)
    }else if(len(grep("line", fn))){
      ls = rbind(ls, d4)
      fn.dall = paste("dall/l_sld.txt",sep="")
      write.table(ls, file = fn.dall)
    }else{
      ll = rbind(ll, d4)
      fn.dall = paste("dall/l_lp.txt",sep="")
      write.table(ll, file = fn.dall)
    }
    
  }
}

# write all data=====================
fns = list.files("d5", full=T)
ut = ul = us = at = al = as = awt = awl = aws = data.frame()
for(fn in fns)
{
  d4 = read.table(fn)
  names(d4)= name.d5
  
  if(len(grep("_u_", fn))){
    if(len(grep("point", fn))){
      ut = rbind(ut, d4)
      fn.dall = paste("d5all/u_tap.txt",sep="")
      write.table(ut, file = fn.dall)
    }else if(len(grep("line", fn))){
      us = rbind(us, d4)
      fn.dall = paste("d5all/u_sld.txt",sep="")
      write.table(us, file = fn.dall)
    }else{
      ul = rbind(ul, d4)
      fn.dall = paste("d5all/u_lp.txt",sep="")
      write.table(ul, file = fn.dall)
    }
    
  }else if(len(grep("_a_", fn))){
    if(len(grep("point", fn))){
      at = rbind(at, d4)
      fn.dall = paste("d5all/a_tap.txt",sep="")
      write.table(at, file = fn.dall)
    }else if(len(grep("line", fn))){
      as = rbind(as, d4)
      fn.dall = paste("d5all/a_sld.txt",sep="")
      write.table(as, file = fn.dall)
    }else{
      al = rbind(al, d4)
      fn.dall = paste("d5all/a_lp.txt",sep="")
      write.table(al, file = fn.dall)
    }
    
  }else{
    if(len(grep("point", fn))){
      awt = rbind(awt, d4)
      fn.dall = paste("d5all/aw_tap.txt",sep="")
      write.table(awt, file = fn.dall)
    }else if(len(grep("line", fn))){
      aws = rbind(aws, d4)
      fn.dall = paste("d5all/aw_sld.txt",sep="")
      write.table(aws, file = fn.dall)
    }else{
      awl = rbind(awl, d4)
      fn.dall = paste("d5all/aw_lp.txt",sep="")
      write.table(awl, file = fn.dall)
    }
    
  }
}

# write all data=====================
fns = list.files("nd7", full=T)
ut = ul = us = at = al = as = awt = awl = aws = data.frame()
for(fn in fns)
{
  d4 = read.table(fn)
  names(d4) = name.nd7

  if(len(grep("_u_", fn))){
    if(len(grep("point", fn))){
      ut = rbind(ut, d4)
      fn.dall = paste("nd7all/u_tap.txt",sep="")
      write.table(ut, file = fn.dall)
    }else if(len(grep("line", fn))){
      us = rbind(us, d4)
      fn.dall = paste("nd7all/u_sld.txt",sep="")
      write.table(us, file = fn.dall)
    }else{
      ul = rbind(ul, d4)
      fn.dall = paste("nd7all/u_lp.txt",sep="")
      write.table(ul, file = fn.dall)
    }
    
  }else if(len(grep("_a_", fn))){
    if(len(grep("point", fn))){
      at = rbind(at, d4)
      fn.dall = paste("nd7all/a_tap.txt",sep="")
      write.table(at, file = fn.dall)
    }else if(len(grep("line", fn))){
      as = rbind(as, d4)
      fn.dall = paste("nd7all/a_sld.txt",sep="")
      write.table(as, file = fn.dall)
    }else{
      al = rbind(al, d4)
      fn.dall = paste("nd7all/a_lp.txt",sep="")
      write.table(al, file = fn.dall)
    }
    
  }else{
    if(len(grep("point", fn))){
      awt = rbind(awt, d4)
      fn.dall = paste("nd7all/aw_tap.txt",sep="")
      write.table(awt, file = fn.dall)
    }else if(len(grep("line", fn))){
      aws = rbind(aws, d4)
      fn.dall = paste("nd7all/aw_sld.txt",sep="")
      write.table(aws, file = fn.dall)
    }else{
      awl = rbind(awl, d4)
      fn.dall = paste("nd7all/aw_lp.txt",sep="")
      write.table(awl, file = fn.dall)
    }
    
  }
}

# compare the different main id in d4
fnsd4 = list.files("use", full=T)
c=call=0
for(fn in 1:len(fnsd4)){
  d4 = read.table(fnsd4[fn])
  names(d4)=name.d4
  c = c+len(which(d4$id!=d4$near.id))
  call = call+nrow(d4)
}
c/call


# draw right finger rate (all)======
pic.name = paste("pic/obs_multi_touch_a.png", sep="")
png(file = pic.name, width = 600, height = 900)
fns = list.files("nd7all", full=T, pattern="a_")
par(mfrow=c(3,2))
for(f in fns){
  d7 = read.table(f, header = T)
  names(d7) = name.nd7
  mainset = setdiff(which(d7$main=="R"), which(d7$multi==0))
  
  o.touch={}
  sort(unique(d7$order))
  for(k in 1:5){
    o.touch = c(o.touch,
                len(intersect(mainset,which(d7$order==k))))
  }
  
  o.prs={}
  sort(unique(d7$o.prs))
  for(k in 1:5){
    o.prs = c(o.prs,
              len(intersect(mainset,which(d7$o.prs==k))))
  }
  
  
  if(len(grep("sld",f)))
    main="slide"
  if(len(grep("tap",f)))
    main="tap"
  if(len(grep("lp",f)))
    main="longpress"
  
  name.order = c('1','2','3','4','5')
  
  b = barplot(o.touch/sum(o.touch), col = col.name, 
              main = paste(main, "touch order"),
              ylim =c(0,1.2), yaxt="n",
              ylab ="Right finger rate (%)",
              names.arg = name.order)
  axis(2, seq(0,1,0.2), labels=seq(0,100,20))
  box()
  pct <- paste(round(o.touch*100/sum(o.touch),2),"%",sep="") # ad % to labels 
  text(b, o.touch/sum(o.touch), pct, pos = 3, cex=1)
  
  
  b = barplot(o.prs/sum(o.prs), col = col.name, 
              main = paste(main, "pressure order"),
              ylim =c(0,1.2), yaxt="n",
              ylab ="Right finger rate (%)",
              names.arg = name.order)
  axis(2, seq(0,1,0.2), labels=seq(0,100,20))
  box()
  pct <- paste(round(o.prs*100/sum(o.prs),2),"%",sep="") # ad % to labels 
  text(b, o.prs/sum(o.prs), pct, pos = 3, cex=1)
  
}
dev.off()

#plot CDF===============================
col.name = c("red", "sienna1", "gold", "palegreen3", "cornflowerblue", "cyan3")
tap.fac = c("sum.l", "sum.t", "acc")
tap.thd = c(12, 0.5, 48)
tap.fac.name = c("drifting","touch_duration","landing_error")
tap.fac.xlab = c("distance (pixel)","time (sec)", "distance (pixel)")

col=c(rgb(161/255, 38/255, 145/255),
rgb(255/255, 128/255, 11/255),
rgb(51/255, 149/255, 224/255),
rgb(55/255, 147/255, 48/255))
a$x[which(a$y==0.9)]

#affected tap vs unaffected tap vs affected with visual assist tap 
for(f in 1:len(tap.fac)){
  pic.name = paste("pic/obs_tap_", tap.fac.name[f], "_all.png", sep="")
  ## set the font family to "serif"
  ## saving defaults in `op`
  op <- par(family = "serif")
  #   png(file = pic.name, width = 800, height = 600)
  
  d4 = at
  names(d4) = name.d4
  i.f = which(names(d4)==tap.fac[f])

  if(f == 2)
    xmax = range(ut[,i.f], at[,i.f], na.rm = T)
  else
    xmax =c(0,150)
  
  a = Ecdf(d4[,i.f], col = col[3], lwd = 2,
#            main = paste("tap", tap.fac.name[f], "CDF"),
#            q=c(0.9)
           xlab = tap.fac.xlab[f], 
           ylab = "Cumulative Precentage",
           xlim = xmax, subtitles=F)
  
#   d4 = awt
#   names(d4) = name.d4
#   Ecdf(d4[,i.f], col = col[3], lwd = 2, add = TRUE)

  d4 = ut
  names(d4) = name.d4
  Ecdf(d4[,i.f], col = col[2], lwd = 2, add = TRUE, subtitles=F)
  
  d4 = rt
  names(d4) = name.d4
  Ecdf(d4[,i.f], col = col[1], lwd = 2, add = TRUE, subtitles=F)
  
#   d4 = lt
#   names(d4) = name.d4
#   Ecdf(d4[,i.f], col = 5, lwd = 2, add = TRUE)
  
  
  abline(v=tap.thd[f], col = 1, lty = 2, lwd = 1)
  tt = paste("Default =", tap.thd[f])
  text(tap.thd[f], 1, tt, pos = 4, col = 1)
  
  list = c("normal's handedness", "patients' unaffected hand",
           "patients' affected hand")
  legend("right", list, lty = 1, lwd = 3,
         col = col, bty='n')
## reset plotting parameters  
par(op)
  
#   dev.off()
}

for(f in 1:len(tap.fac)){
  pic.name = paste("pic/obs_tap_", tap.fac.name[f], "_all.png", sep="")
  ## set the font family to "serif"
  ## saving defaults in `op`
  op <- par(family = "serif")
  #   png(file = pic.name, width = 800, height = 600)
  
  d4 = at
  names(d4) = name.d4
  i.f = which(names(d4)==tap.fac[f])
    
  plot(ecdf(d4[,i.f]), col=col[3], main=NA, 
       do.points = F, lwd=2, verticals = T,
       xlab = tap.fac.xlab[f],
       yaxt="n", ylab="Cumulative percentage")
  write.csv(d4[,i.f], file = "at.csv")
  
  axis(2,seq(0,1,0.2),labels=c("0%", "20%", "40%", "60%", "80%", "100%"),
       cex.axis=1, lwd=0.5)
  
  d4 = ut
  names(d4) = name.d4
  plot(ecdf(d4[,i.f]), col=col[2], add = T, do.points = F, lwd=2, verticals = T)
  write.csv(d4[,i.f], file = "ut.csv")
  
  d4 = rt
  names(d4) = name.d4
  plot(ecdf(d4[,i.f]), col=col[1], add = T, do.points = F, lwd=2, verticals = T)
  write.csv(d4[,i.f], file = "rt.csv")
  
  abline(v=tap.thd[f], col = 1, lty = 2, lwd = 1)
  tt = paste("Default =", tap.thd[f])
  text(tap.thd[f], 0.4, tt, pos = 4, col = 1)
  
  list = c("Normal's handedness", "Patients' unaffected hand",
           "Patients' affected hand")
  legend(600,0.5, list, lty = 1, lwd = 3,
         col = col, bty='n')
  ## reset plotting parameters  
  par(op)
  
  #   dev.off()
}

#affected longpress vs unaffected longpress vs affected with visual assist longpress

for(f in 1:len(tap.fac)){
  pic.name = paste("pic/obs_tap_", tap.fac.name[f], "_all.png", sep="")
  ## set the font family to "serif"
  ## saving defaults in `op`
  op <- par(family = "serif")
  #   png(file = pic.name, width = 800, height = 600)
  
  d4 = al
  names(d4) = name.d4
  i.f = which(names(d4)==tap.fac[f])
  
  if(f == 2)
    xmax = range(ut[,i.f], at[,i.f], na.rm = T)
  else
    xmax =c(0,150)
  
  a = Ecdf(d4[,i.f], col = col[3], lwd = 2,
           #            main = paste("tap", tap.fac.name[f], "CDF"),
           #            q=c(0.9)
           xlab = tap.fac.xlab[f], 
           ylab = "Cumulative Precentage",
           xlim = xmax, subtitles=F)
  
  #   d4 = awt
  #   names(d4) = name.d4
  #   Ecdf(d4[,i.f], col = col[3], lwd = 2, add = TRUE)
  
  d4 = ul
  names(d4) = name.d4
  Ecdf(d4[,i.f], col = col[2], lwd = 2, add = TRUE, subtitles=F)
  
  d4 = rl
  names(d4) = name.d4
  Ecdf(d4[,i.f], col = col[1], lwd = 2, add = TRUE, subtitles=F)
  
  #   d4 = ll
  #   names(d4) = name.d4
  #   Ecdf(d4[,i.f], col = 5, lwd = 2, add = TRUE)
  
  
  abline(v=tap.thd[f], col = 1, lty = 2, lwd = 1)
  tt = paste("Default =", tap.thd[f])
  text(tap.thd[f], 1, tt, pos = 4, col = 1)
  
  list = c("normal's handedness", "patients' unaffected hand",
           "patients' affected hand")
  legend("right", list, lty = 1, lwd = 3,
         col = col, bty='n')
  ## reset plotting parameters  
  par(op)
  
  #   dev.off()
}

for(f in 1:len(tap.fac)){
  pic.name = paste("pic/obs_tap_", tap.fac.name[f], "_all.png", sep="")
  ## set the font family to "serif"
  ## saving defaults in `op`
  op <- par(family = "serif")
  #   png(file = pic.name, width = 800, height = 600)
  
  d4 = al
  names(d4) = name.d4
  i.f = which(names(d4)==tap.fac[f])
  write.csv(d4[,i.f], file = "al.csv")
    
  plot(ecdf(d4[,i.f]), col=col[3], main=NA, 
       do.points = F, lwd=2, verticals = T,
       xlab = tap.fac.xlab[f],
       yaxt="n", ylab="Cumulative percentage")
  
  axis(2,seq(0,1,0.2),labels=c("0%", "20%", "40%", "60%", "80%", "100%"),
       cex.axis=1, lwd=0.5)
  
  d4 = ul
  names(d4) = name.d4
  plot(ecdf(d4[,i.f]), col=col[2], add = T, do.points = F, lwd=2, verticals = T)
  write.csv(d4[,i.f], file = "ul.csv")
  
  d4 = rl
  names(d4) = name.d4
  plot(ecdf(d4[,i.f]), col=col[1], add = T, do.points = F, lwd=2, verticals = T)
  write.csv(d4[,i.f], file = "rl.csv")
  
  abline(v=tap.thd[f], col = 1, lty = 2, lwd = 1)
  tt = paste("Default =", tap.thd[f])
  text(tap.thd[f], 0.2, tt, pos = 4, col = 1)
  
  list = c("Normal's handedness", "Patients' unaffected hand",
           "Patients' affected hand")
  legend(400,0.5, list, lty = 1, lwd = 3,
         col = col, bty='n')
  ## reset plotting parameters  
  par(op)
  
  #   dev.off()
}

sld.fac = c("sum.l", "acc", "accmax", "accend", "dd", "over")
sld.thd = c(12, 48, 48, 48, 0, 0)
sld.fac.name = c("under_slop", "landing_error", 
                 "trajectory_maxerror", "trajectory_enderror",
                 "dd_ratio","over_ratio")
sld.fac.xlab = c("distance (pixel)", "distance (pixel)", "distance (pixel)",
                 "distance (pixel)", "ratio", "ratio")

#affected slide vs unaffected slide vs affected with visual assist slide
for(f in 1:len(sld.fac)){
  pic.name = paste("pic/obs_sld_", sld.fac.name[f], "_all.png", sep="")
#   png(file = pic.name, width = 800, height = 600)
  
  d4 = as
  names(d4) = name.d4
  i.f = which(names(d4)==sld.fac[f])
  xmax = range(us[,i.f], as[,i.f], aws[,i.f], na.rm = T)
  if(f == 2 || f == 3 || f == 4)
    xmax =c(0,150)
  
  
  a = Ecdf(d4[,i.f], col = col[4], lwd = 2,
           main = paste(sld.fac.name[f], "CDF"), 
           xlab = sld.fac.xlab[f], ylab = "Cumulative Precentage",
           xlim = xmax)
  
  d4 = us
  names(d4) = name.d4
  Ecdf(d4[,i.f], col = col[2], lwd = 2,add = TRUE)
       
  
  d4 = aws
  names(d4) = name.d4
  Ecdf(d4[,i.f], col = col[3], lwd = 2,add = TRUE)
  
  d4 = rs
  names(d4) = name.d4
  Ecdf(d4[,i.f], col = col[1], lwd = 2,add = TRUE)
  
#   d4 = ls
#   names(d4) = name.d4
#   Ecdf(d4[,i.f], col = 5, lwd = 2,add = TRUE)
  
  
  abline(v=sld.thd[f], col = col[5], lty = 2, lwd = 2)
  tt = paste("Default =", sld.thd[f])
  text(sld.thd[f], 1, tt, pos = 2, col = col[5])
  
  list = c("normal's handedness", "unaffected slide",
           "affected  with assist slide", "affected slide")
  legend("right", list, lty = 1, lwd = 3,
         col = col, bty='n')
  
#   dev.off()
}


# acc fac
# acc fac (all subject barplot)
tap.fac = c("sum.l", "sum.t", "acc", "multi")
tap.thd = c(12, 0.5, 48, 1)
sld.fac = c("sum.l", "acc", "accmax", "accend", "multi")
sld.thd = c(12, 48, 48, 48, 1)
col.name = c("indianred1", "#FFFFCC", "#FED9A6","darkseagreen1")
col.name = c("lightskyblue","cyan3","gold","sienna1")
l.type = c("tap", "lp", "sld")
l.hand = c("a", "aw", "u", "r")
name.type = c("tap", "long press", "slide")
name.hand = c("affected hand", "affected hand with assist", "unaffected hand",
              "normal's handedness")
for(lt in 1:len(l.type)){
  pic.name = paste("pic/obs_", l.type[lt], "_error_bar.png", sep="")
#   png(file = pic.name, width = 900, height = 400)
  d={}
  for(lh in 1:len(l.hand)){
    fn = paste("dall/",l.hand[lh],"_", l.type[lt], ".txt",sep="")
    d4 = read.table(fn)
    names(d4) = name.d4
    
    if(l.type[lt] == "tap"){
      leg.name = c("overslop", "overtime", "landing error",
                   "unintended multitouch")
      acc.fac = tap.fac
      acc.thd = tap.thd
    }
    if(l.type[lt] == "lp"){
      leg.name = c("overslop", "undertime", "landing error",
                   "unintended multitouch")
      acc.fac = tap.fac
      acc.thd = tap.thd
    }
    if(l.type[lt] == "sld"){
      leg.name = c("underslop", "landing error", "trajectory max error",
                   "trajectory end error","unintended multitouch")
      acc.fac = sld.fac
      acc.thd = sld.thd
    }
    
    fac = {}
    for(f in 1:len(acc.fac)){
      i.f = which(names(d4)==acc.fac[f])
      
      if(l.type[lt]=="sld" && f==1){
        fac = c(fac, len(which(d4[,i.f] < acc.thd[f])))
      }else if(l.type[lt]=="lp" && f==2){
        fac = c(fac, len(which(d4[,i.f] < acc.thd[f])))
      }else{
        fac = c(fac, len(which(d4[,i.f] >= acc.thd[f])))
      }
    }
    d = cbind(d, fac/nrow(d4))
  }
  
  if(lt==3)
    col.name = c("lightskyblue","cyan3","gold","sienna1","indianred1")
  pic.name = paste(name.type[lt], "error rate")
  b = barplot(as.matrix(d), col = col.name, 
            main = pic.name, ylim =c(0,1.2), yaxt="n",
            names.arg = name.hand,
            beside = T,
            ylab ="Error rate (%)")

  axis(2, seq(0,1,0.2), labels=seq(0,100,20))
  pct <- paste(round(d*100),"%",sep="") # ad % to labels
  text(b, as.matrix(d), pct, pos = 3, cex=0.7)
  
  legend("topright", leg.name, fill = col.name, bty='n')
#   dev.off()
}

#venn diagram
require(VennDiagram)
library(VennDiagram)
for(lt in 1:len(l.type)){
#   pic.name = paste("pic/obs_", l.type[lt], "_error_bar.png", sep="")
#   png(file = pic.name, width = 900, height = 400)
  d={}
  for(lh in 1:len(l.hand)){
    fn = paste("dall/",l.hand[lh],"_", l.type[lt], ".txt",sep="")
    d4 = read.table(fn)
    names(d4) = name.d4
    
    if(l.type[lt] == "tap"){
      leg.name = c("overslop", "overtime", "landing error",
                   "unintended multitouch")
      acc.fac = tap.fac
      acc.thd = tap.thd
    }
    if(l.type[lt] == "lp"){
      leg.name = c("overslop", "undertime", "landing error",
                   "unintended multitouch")
      acc.fac = tap.fac
      acc.thd = tap.thd
    }
    if(l.type[lt] == "sld"){
      leg.name = c("underslop", "landing error", "trajectory max error",
                   "trajectory end error","unintended multitouch")
      acc.fac = sld.fac
      acc.thd = sld.thd
    }
    
    i.f = which(names(d4)==acc.fac[1])
    A = as.numeric(which(d4[,i.f] >= acc.thd[1]))
    i.f = which(names(d4)==acc.fac[2])
    B = as.numeric(which(d4[,i.f] >= acc.thd[2]))
    i.f = which(names(d4)==acc.fac[3])
    C = as.numeric(which(d4[,i.f] >= acc.thd[3]))
    i.f = which(names(d4)==acc.fac[4])
    D = as.numeric(which(d4[,i.f] >= acc.thd[4]))
    
    venn.plot <- venn.diagram(
      x = list(
        "overslop" = A,
        "overtime" = B,
        "landing error" = C
      ),
      euler.d = TRUE,
      filename = "taperror3.png",
      cex = 1,
      cat.cex = 1,
      cat.pos = 0
    );
    
    
    
    
    
    venn.plot <- venn.diagram(
      x = list(
        "overslop" = A,
        D = D,
        "overtime" = B,
        "landing error" = C
      ),
      filename = "taperror4.png",
      col = "transparent",
      fill = c("cornflowerblue", "green", "yellow", "darkorchid1"),
      alpha = 0.50,
      label.col = c("orange", "white", "darkorchid4", "white",
                    "white", "white", "white", "white", "darkblue", "white",
                    "white", "white", "white", "darkgreen", "white"),
      cex = 1.5,
      fontfamily = "serif",
      fontface = "bold",
      cat.col = c("darkblue", "darkgreen", "orange", "darkorchid4"),
      cat.cex = 1.5,
      cat.pos = 0,
      cat.dist = 0.07,
      cat.fontfamily = "serif",
      rotation.degree = 270,
      margin = 0.2
    );
    
    require(venneuler)
    library(venneuler)
#     mn=max(len(A), len(B), len(C), len(D))
#     A=c(A, rep(0,mn-len(A)))
#     B=c(B, rep(0,mn-len(B)))
#     C=c(C, rep(0,mn-len(C)))
#     D=c(D, rep(0,mn-len(D)))
    m <- data.frame(A=A, B=B, C=C, D=D)
    # without weights
    v <- venneuler(m > 0)
    plot(v)
    # with weights
    v <- venneuler(m)
    plot(v)
    
    fac = {}
    for(f in 1:len(acc.fac)){
      i.f = which(names(d4)==acc.fac[f])
      
      if(l.type[lt]=="sld" && f==1){
        fac = c(fac, which(d4[,i.f] < acc.thd[f]))
      }else if(l.type[lt]=="lp" && f==2){
        fac = c(fac, which(d4[,i.f] < acc.thd[f]))
      }else{
        fac = c(fac, which(d4[,i.f] >= acc.thd[f]))
      }
      
    }

  }
  
  
  if(lt==3)
    col.name = c("lightskyblue","cyan3","gold","sienna1","indianred1")
  pic.name = paste(name.type[lt], "error rate")
  b = barplot(as.matrix(d), col = col.name, 
              main = pic.name, ylim =c(0,1.2), yaxt="n",
              names.arg = name.hand,
              beside = T,
              ylab ="Error rate (%)")
  
  axis(2, seq(0,1,0.2), labels=seq(0,100,20))
  pct <- paste(round(d*100,2),"%",sep="") # ad % to labels
  text(b, as.matrix(d), pct, pos = 3, cex=0.7)
  
  legend("topleft", leg.name, fill = col.name, bty='n')
#   dev.off()
}

# acc fac (all subject pieplot)
col.name = c("lightskyblue","cyan3","gold","sienna1","red", 
             "darkseagreen1")
for(lt in 1:len(l.type)){
  pic.name = paste("pic/obs_", l.type[lt], "_error_pie.png", sep="")
#   png(file = pic.name, width = 800, height = 800)
  mm <- matrix(c(1,2,3,4,5,5),nrow = 3,ncol = 2,byrow = TRUE)
  layout(mat = mm, heights = c(0.45,0.45,0.1))
  for(lh in 1:len(l.hand)){
    fn = paste("dall/",l.hand[lh],"_", l.type[lt], ".txt",sep="")
    d4 = read.table(fn)
    if(l.type[lt]=="tap"){
      l=which(d4$sum.l >= 12)
      t=which(d4$sum.t >= 0.5)
      a=which(d4$acc >= 48)
      m=which(d4$multi >= 1)
      
      d.l = d.t = d.a = d.m = {}
      for(i in l){
        len.t = any(i==t)
        len.a = any(i==a)
        len.m = any(i==m)
        if(len.t || len.a || len.m)
          d.l = c(d.l,i)
      }
      for(i in t){
        len.l = any(i==l)
        len.a = any(i==a)
        len.m = any(i==m)
        if(len.t || len.a || len.m)
          d.t = c(d.t,i)
      }
      for(i in a){
        len.t = any(i==t)
        len.l = any(i==l)
        len.m = any(i==m)
        if(len.t || len.a || len.m)
          d.a = c(d.a,i)
      }
      for(i in m){
        len.t = any(i==t)
        len.a = any(i==a)
        len.l = any(i==l)
        if(len.t || len.a || len.m)
          d.m = c(d.m,i)
      }
      n.l = len(setdiff(l, d.l))
      n.t = len(setdiff(t, d.t))
      n.a = len(setdiff(a, d.a))
      n.m = len(setdiff(m, d.m))  
      all = len(union(union(union(l,t), a), m))
      
      d = c(n.l, n.t, n.a, n.m, 
            all-sum(n.l, n.t, n.a, n.m), nrow(d4)-all)
      names(d) = c("overslop", "overtime", "landing error", 
                   "unintended multitouch", "multi error", "Success")
    }
    
    if(l.type[lt]=="lp"){
      l=which(d4$sum.l >= 12)
      t=which(d4$sum.t <= 0.5)
      a=which(d4$acc >= 48)
      m=which(d4$multi >= 1)
      
      d.l = d.t = d.a = d.m = {}
      for(i in l){
        len.t = any(i==t)
        len.a = any(i==a)
        len.m = any(i==m)
        if(len.t || len.a || len.m)
          d.l = c(d.l,i)
      }
      for(i in t){
        len.l = any(i==l)
        len.a = any(i==a)
        len.m = any(i==m)
        if(len.t || len.a || len.m)
          d.t = c(d.t,i)
      }
      for(i in a){
        len.t = any(i==t)
        len.l = any(i==l)
        len.m = any(i==m)
        if(len.t || len.a || len.m)
          d.a = c(d.a,i)
      }
      for(i in m){
        len.t = any(i==t)
        len.a = any(i==a)
        len.l = any(i==l)
        if(len.t || len.a || len.m)
          d.m = c(d.m,i)
      }
      n.l = len(setdiff(l, d.l))
      n.t = len(setdiff(t, d.t))
      n.a = len(setdiff(a, d.a))
      n.m = len(setdiff(m, d.m))  
      all = len(union(union(union(l,t), a), m))
      
      d = c(n.l, n.t, n.a, n.m, 
            all-sum(n.l, n.t, n.a, n.m), nrow(d4)-all)
      names(d) = c("overslop", "undertime", "landing error", 
                   "unintended multitouch", "multi error", "Success")
    }
    
    if(l.type[lt]=="sld"){
      l=which(d4$sum.l <= 12)
      a=which(d4$acc >= 48)
#       am=which(d4$accmax >= 48)
      am=0
      ae=which(d4$accend >= 48)
      m=which(d4$multi >= 1)
      
      d.l = d.a = d.am = d.ae = d.m = {}
      for(i in l){
        len.a = any(i==a)
        len.am = any(i==am)
        len.m = any(i==m)
        len.ae = any(i==ae)
        if(len.a || len.am || len.m || len.ae)
          d.l = c(d.l,i)
      }
      for(i in a){
        len.l = any(i==l)
        len.am = any(i==am)
        len.m = any(i==m)
        len.ae = any(i==ae)
        if(len.l || len.am || len.m || len.ae)
          d.a = c(d.a,i)
      }
      for(i in am){
        len.l = any(i==l)
        len.a = any(i==a)
        len.m = any(i==m)
        len.ae = any(i==ae)
        if(len.l || len.a || len.m || len.ae)
          d.am = c(d.am,i)
      }
      for(i in ae){
        len.l = any(i==l)
        len.a = any(i==a)
        len.m = any(i==m)
        len.ae = any(i==am)
        if(len.l || len.a || len.m || len.am)
          d.ae = c(d.ae,i)
      }
      for(i in m){
        len.l = any(i==l)
        len.a = any(i==a)
        len.am = any(i==am)
        len.ae = any(i==ae)
        if(len.l || len.a || len.am || len.ae)
          d.m = c(d.m,i)
      }
      n.l = len(setdiff(l, d.l))
      n.a = len(setdiff(a, d.a))
      n.am = len(setdiff(am, d.am))
      n.ae = len(setdiff(ae, d.ae))
      n.m = len(setdiff(m, d.m))  
      all = len(union(union(union(union(l,a), am), m), ae))
      d = c(n.l, n.a, n.am, n.ae, n.m, 
            all-sum(n.l, n.a, n.am, n.m, n.ae), nrow(d4)-all)
      names(d) = c("underslop", "landing error", "trajectory max error",
                   "end error","unintended multitouch",
                   "multi errors", "success")
      
      col.name = c("lightskyblue","cyan3","gold","sienna1",
                   "indianred1", "red","darkseagreen1")
    }
    
    pct <- round(d/sum(d)*100)
    pct <- paste(pct,"%",sep="") # ad % to labels 
    par(mar = c(1,0,1,0))
    pie(d, labels=pct, col=col.name, 
        main = paste(name.type[lt], "using", name.hand[lh]),
        border=0, init.angle=90, radius = 0.8)
    
  }

  par(mar = c(1,0,1,0))
  plot(1, type = "n", axes=FALSE, xlab="", ylab="")
  legend("right",inset = 0, names(d), fill = col.name,
         bty='n', horiz = T)
#   dev.off()
}


#simulation====================================
col.name = c("red", "cyan3", "indianred1", "slateblue3")
tap.fac = c("sum.l", "sum.t", "acc")
tap.thd = c(12, 0.5, 48)
tap.fac.name = c("drifting","touch_duration","landing_error")

for(f in 1:len(tap.fac)){
  pic.name = paste("pic/sim_tap_", tap.fac.name[f], "_all.png", sep="")
#   png(file = pic.name, width = 800, height = 600)
  
  d4 = at
  names(d4) = name.d4
  i.f = which(names(d4)==tap.fac[f])
  xmax = range(at[,i.f], as[,i.f], na.rm = T)
  if(f == 1 || f == 3)
    xmax =c(0,150)
  
  iddf=which.min(abs(a$x - tap.thd[f])) 
  a = Ecdf(d4[,i.f], col = col.name[1],
           main = paste("tap",tap.fac.name[f]), 
           xlab = tap.fac.xlab[f],
           ylab = "Error rate",
           xlim = xmax, what='1-F',
           q=c(a$y[iddf], 0.1))
  tt = paste("Default =", tap.thd[f])
  text(a$x[iddf], a$y[iddf], tt, pos = 4)

  id10=which.min(abs(a$y - 0.1)) 
  tt = paste("10% =", round(a$x[id10],1))
  text(a$x[id10], a$y[id10], tt, pos = 3)
  
  d4 = rt
  names(d4) = name.d4
  i.f = which(names(d4)==tap.fac[f])
  Ecdf(d4[,i.f], col = col.name[2], add = T, what='1-F')
  
  if(f == 1){
    d4 = as
    names(d4) = name.d4
    i.f = which(names(d4)==tap.fac[f])
    Ecdf(d4[,i.f], col = col.name[3], add = T)
    
    d4 = rs
    names(d4) = name.d4
    i.f = which(names(d4)==tap.fac[f])
    Ecdf(d4[,i.f], col = col.name[4], add = T)
  }

  if(f == 2){
    d4 = al
    names(d4) = name.d4
    i.f = which(names(d4)==tap.fac[f])
    Ecdf(d4[,i.f], col = col.name[3], add = T)
    
    d4 = rl
    names(d4) = name.d4
    i.f = which(names(d4)==tap.fac[f])
    Ecdf(d4[,i.f], col = col.name[4], add = T)
  }
  
  
  abline(v=tap.thd[f], col = "gray", lty = 2, lwd = 2)
  tt = paste("Default =", tap.thd[f])
  text(tap.thd[f], 1, tt, pos = 4)
  
  id10=which.min(abs(a$y - 0.1)) 
  abline(v=a$x[id10], col = "gray", lty = 2, lwd = 2)
  tt = paste("10% =", round(a$x[id10],1))
  text(a$x[id10], 1, tt, pos = 2)
  
  list = c("affected tap", "normal tap")
  if(f == 1)
    list = c("affected tap", "normal tap", "affected slide", "normal slide")
  if(f == 2)
    list = c("affected tap", "normal tap", "affected longpress", "normal longpress")
    
  legend("right", list, lty = 1, lwd = 3,
         col = col.name[1:len(list)], bty='n')
  
#   dev.off()
}

for(f in 1:len(tap.fac)){
  pic.name = paste("pic/sim_lp_", tap.fac.name[f], "_all.png", sep="")
#   png(file = pic.name, width = 800, height = 600)
  
  d4 = al
  names(d4) = name.d4
  i.f = which(names(d4)==tap.fac[f])
  xmax = range(al[,i.f], as[,i.f], na.rm = T)
  if(f == 1 || f == 3)
    xmax =c(0,200)
  
  if(f==2){
    a = Ecdf(d4[,i.f], col = col.name[1],
             main = paste("longpress",tap.fac.name[f]), 
             xlab = tap.fac.xlab[f],
             ylab = "Error rate",
             xlim = xmax)
    d4 = rl
    names(d4) = name.d4
    i.f = which(names(d4)==tap.fac[f])
    Ecdf(d4[,i.f], col = col.name[2], add = T)
  }else{
    iddf=which.min(abs(a$x - tap.thd[f])) 
    a = Ecdf(d4[,i.f], col = col.name[1],
             main = paste("longpress",tap.fac.name[f]), 
             xlab = tap.fac.xlab[f],
             ylab = "Error rate",
             xlim = xmax, what='1-F',
             q=c(a$y[iddf], 0.1))
    tt = paste("Default =", tap.thd[f])
    text(a$x[iddf], a$y[iddf], tt, pos = 4)
    
    id10=which.min(abs(a$y - 0.1)) 
    tt = paste("10% =", round(a$x[id10],1))
    text(a$x[id10], a$y[id10], tt, pos = 3)
    
    d4 = rl
    names(d4) = name.d4
    i.f = which(names(d4)==tap.fac[f])
    Ecdf(d4[,i.f], col = col.name[2], add = T, what='1-F')
  }
  
  if(f == 1){
    d4 = as
    names(d4) = name.d4
    i.f = which(names(d4)==tap.fac[f])
    Ecdf(d4[,i.f], col = col.name[3], add = T)
    
    d4 = rs
    names(d4) = name.d4
    i.f = which(names(d4)==tap.fac[f])
    Ecdf(d4[,i.f], col = col.name[4], add = T)
  }
  
  if(f == 2){
    d4 = at
    names(d4) = name.d4
    i.f = which(names(d4)==tap.fac[f])
    Ecdf(d4[,i.f], col = col.name[3], add = T, what='1-F')
    
    d4 = rt
    names(d4) = name.d4
    i.f = which(names(d4)==tap.fac[f])
    Ecdf(d4[,i.f], col = col.name[4], add = T, what='1-F')
  }
  
  
  abline(v=tap.thd[f], col = "gray", lty = 2, lwd = 2)
  tt = paste("Default =", tap.thd[f])
  text(tap.thd[f], 1, tt, pos = 4)

  id10=which.min(abs(a$y - 0.1)) 
  abline(v=a$x[id10], col = "gray", lty = 2, lwd = 2)
  tt = paste("10% =", round(a$x[id10],1))
  text(a$x[id10], 1, tt, pos = 4)
  
  list = c("affected longpress", "normal longpress")
  if(f == 1)
    list = c("affected longpress", "normal longpress", "affected slide", "normal slide")
  if(f == 2)
    list = c("affected longpress", "normal longpress", "affected tap", "normal tap")
  
  legend("right", list, lty = 1, lwd = 3,
         col = col.name[1:len(list)], bty='n')
  
#   dev.off()  
}

sld.fac = c("sum.l", "acc", "accmax", "accend")
sld.thd = c(12, 48, 48, 48)
sld.fac.name = c("under_slop", "landing_error", 
                 "trajectory_maxerror", "trajectory_enderror")
for(f in 1:len(sld.fac)){
  pic.name = paste("pic/sim_sld_", sld.fac.name[f], "_all.png", sep="")
#   png(file = pic.name, width = 800, height = 600)
  
  d4 = as
  names(d4) = name.d4
  i.f = which(names(d4)==sld.fac[f])
  xmax = range(as[,i.f], at[,i.f], na.rm = T)
  if(f == 1 || f == 3)
    xmax =c(0,200)
  
  a = Ecdf(d4[,i.f], col = col.name[1],
           main = paste("slide",sld.fac.name[f]), 
           xlab = sld.fac.xlab[f],
           ylab = "Error rate",
           xlim = xmax, what='1-F')
  
  d4 = rs
  names(d4) = name.d4
  i.f = which(names(d4)==sld.fac[f])
  Ecdf(d4[,i.f], col = col.name[2], add = T, what='1-F')
  
  if(f == 1){
    d4 = at
    names(d4) = name.d4
    i.f = which(names(d4)==sld.fac[f])
    Ecdf(d4[,i.f], col = col.name[3], add = T)
    
    d4 = rt
    names(d4) = name.d4
    i.f = which(names(d4)==sld.fac[f])
    Ecdf(d4[,i.f], col = col.name[4], add = T)
  }
  
  
  abline(v=sld.thd[f], col = "gray", lty = 2, lwd = 2)
  tt = paste("Default =", sld.thd[f])
  text(sld.thd[f], 0.9, tt, pos = 4)
  
  id10=which.min(abs(a$y - 0.1)) 
  abline(v=a$x[id10], col = "gray", lty = 2, lwd = 2)
  tt = paste("10% =", round(a$x[id10],1))
  text(a$x[id10], 1, tt, pos = 4)
  
  if(f == 1)
    list = c("affected slide", "normal slide", "affected tap", "normal tap")
  else
    list = c("affected slide", "normal slide")
  legend("right", list, lty = 1, lwd = 3,
         col = col.name[1:len(list)], bty='n')
  
#   dev.off()
  
  
}
