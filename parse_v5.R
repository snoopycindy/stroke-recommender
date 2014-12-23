#setwd("E:/Dropbox/touchparse_stroke")
source("C:/R.source/my.fig.R")
source("C:/R.source/common.R")
source("C:/R.source/legend_col.R")
source("C:/R.source/rey.f.R")
source("C:/R.source/yx.common.R")


samsung=1
fns = list.files("fnlog", full=T)
name = c("time", "type", "event", "value")
event = c("t", "sl", "id", "x", "y", "prs")
none = -9


d2 = d3 = d4 = game.info = data.frame()
game.name = {}
#names(d2) = event

for(fn in fns)
{
  if(samsung || sonyz)
  {
    d = read.table(fn, header=F, as.is = T)
    d = d[,-1]
    d[1] = unlist(strsplit(as.character(d[,1]), "]"))
  }else{
    d = read.table(fn, header=F, as.is = T)
    d$V1 = unlist(strsplit(as.character(d$V1), ":"))
    d$V1 = gsub("-", ".", d$V1)
  }
  
  names(d) =name
  d$value = (paste("0x", d$value, sep=""))
  t = sl = id = x = y = prs = none
  
  d2 = data.frame()
  for(i in 1:nrow(d))
  {
    t = d$time[i]
  
    if(d$event[i] == "SYN_REPORT"){
      d2 = rbind(d2, cbind(t, sl, id, x, y, prs))
      t = sl = id = x = y = prs = none
    }else if(d$event[i] == "SYN_MT_REPORT"){
      next
    }else if(d$event[i] == "ABS_MT_SLOT"){
      if(i == 1){
        sl = as.numeric(d$value[i])
      }else if(d$event[i-1] != "SYN_REPORT"){
        d2 = rbind(d2, cbind(t, sl, id, x, y, prs))
        t = sl = id = x = y = prs = none
        sl = as.numeric(d$value[i])
      }else{
        sl = as.numeric(d$value[i])
      }
    }else if (d$event[i] == "ABS_MT_TRACKING_ID"){
      if(d$value[i] == "0xffffffff")
        id = none
      else
        id = as.numeric(d$value[i])
    }else if (d$event[i] == "ABS_MT_POSITION_X"){
      x = as.numeric(d$value[i])
    }else if (d$event[i] == "ABS_MT_POSITION_Y"){
      y = as.numeric(d$value[i])
    }else if (d$event[i] == "ABS_MT_PRESSURE"){
      prs = as.numeric(d$value[i])
    }else{
      next()
    }
  }
  
  # change the data type===============
  d2$t = as.numeric(as.character(d2$t))
  d2$x = as.numeric(as.character(d2$x))
  d2$y = as.numeric(as.character(d2$y))
  d2$prs = as.numeric(as.character(d2$prs))
  
  
  # 處理 multi- actions: 把屬於那個slot的所有records，其sl改為它的slot==========
  mul.but=which(d2$sl!=none)
  for(s in mul.but){
    if(s==nrow(d2))   #若此multi action是最後一個record, 不記錄
      next
    ev.act=which(d2$id[(s+1):nrow(d2)]!=none)
    sl.act=which(d2$sl[(s+1):nrow(d2)]!=none)
    if(is.na(rey.min(ev.act,sl.act)))
      d2$sl[s:nrow(d2)]=d2$sl[s]                         #後面都是ntect的資料，表示到最後都屬於這個multi-action
    else{
      e=rey.min(ev.act,sl.act)
      d2$sl[s:(s+e-1)]=d2$sl[s]
    }
  } 
  
  
  
  
  #消除x,y,prs等於none的狀況
  for(dn in c('x','y','prs')){
    dn.n=which(d2[,dn]==none)
    for(s in dn.n){
      if(s==1)
        next  #若此x.none是第一筆資料，不處理
      sl.s=intersect(which(d2$sl[1:s]== d2$sl[s]), which(d2[,dn]!=none))
      sl.n=intersect(which(d2$sl[1:s]==none), which(d2[,dn]!=none))
      if(is.na(rey.max(sl.s,sl.n)))
        next
      else
        d2[s,dn]=d2[rey.max(sl.s,sl.n),dn]
      
    }
  }
  
  # 如果還有None 輸出
  for(i in 2:nrow(d2))
  {
    if(d2$x[i]==none) print("terrible x")
    if(d2$y[i]==none) print("terrible y")
    if(d2$prs[i]==none) print("terrible prs")
  }
  
  
  # Parse information==================
  isd = which(d2$id != none)
  d3 = data.frame()
  thd.dis = 60  #if "sum.l>thd.dis" means that the type is belong to "slide"  
  if(len(isd) == 1)
  {
    d.x = diff(d2$x)
    d.y = diff(d2$y)
    vec = cbind(d.x, d.y)
    
    # Velocity=========================
    part.l = sqrt(d.x^2+d.y^2)
    part.t = diff(d2$t)
    sum.l = sum(part.l)
    sum.t = sum(part.t)
    mean.v = sum.l/sum.t
    sd.v = sd(part.l/part.t)
    
    if(sum.l < thd.dis)
    {
      type = "tap"
      mean.v = sd.v = NA
    }else{
      type = "slide"
      mean.v = sum.l/sum.t
      sd.v = sd(part.l/part.t)
    }
    
    # Trajetory Smoothness=============
    part.a = {}
    if(len(d.x)==1 || sum.l<thd.dis)
      part.a = c(part.a, NA)
    else
    {
      for(i in 1:(len(d.x)-1))
      {
        x.y = as.numeric(vec[i]%*%vec[i+1])
        xpy = part.l[i] * part.l[i+1]
        
        if(xpy==0)
          next()
        else
          part.a = c(part.a, acos(x.y/xpy)/2/pi*360)
      }
    }
    sd.a = sd(diff(part.a))
    
    # Pressure=========================
    mean.prs = mean(d2$prs)
    sd.prs = sd(d2$prs)
    
    d3 = cbind(sum.l, sum.t, mean.v, sd.v, sd.a, mean.prs, sd.prs, type)
    d3 = as.data.frame(d3)
    
    
  }else{ #more than "one" touch action
    
    for( i in 1:len(isd) )
    {
      sp = isd[i]
      if(i == len(isd))
        ep = nrow(d2)
      else
        ep = isd[i+1]-1
      
      d.x = diff(d2$x[sp:ep])
      d.y = diff(d2$y[sp:ep])
      vec = cbind(d.x, d.y)
      
      
      # Velocity=======================
      part.l = sqrt(d.x^2+d.y^2)
      part.t = diff(d2$t[sp:ep])
      sum.l = sum(part.l)
      sum.t = diff(range(d2$t[sp:ep]))
      if(sum.l < thd.dis)
      {
        type = "tap"
        sd.v = mean.v = NA
        if(i != len(isd))
        {
          t.l = dis2D(d2$x[ep], d2$y[ep], d2$x[ep+1], d2$y[ep+1])
          t.t = d2$t[ep+1]-d2$t[ep]
          mean.v = t.l/t.t
        }else mean.v = 0
        
      }else{
        type = "slide"
        mean.v = sum.l/sum.t
        sd.v = sd(part.l/part.t)
      }
      
      # Trajetory Smothness============
      part.a = {}
      if(len(d.x)==1 || sum.l<thd.dis)
        part.a = c(part.a, NA)
      else
      {
        for(i in 1:(len(d.x)-1))
        {
          x.y = as.numeric(vec[i]%*%vec[i+1])
          xpy = part.l[i] * part.l[i+1]
          
          if(xpy==0)
            next()
          else
            part.a = c(part.a, acos(x.y/xpy)/2/pi*360)
        }
      }
      sd.a = sd(diff(part.a))
      
      # Pressure=======================
      mean.prs = mean(d2$prs[sp:ep])
      sd.prs = sd(d2$prs[sp:ep])
      
      d3 = rbind(d3, cbind(sum.l, sum.t, mean.v, sd.v, sd.a, mean.prs, sd.prs, type))
    }
  }
  
  #save data===========================
  if(0)
  {
    fname = strsplit(strsplit(fn, "/")[[1]][2], "\\.")[[1]][1]
    name.pic = paste("parsedata/", fname, ".pdf", sep="")
    name.info = paste("parsedata/", fname, ".txt", sep="")
    write.table(d3, name.info, sep = "\t")
  }
  
  
  # change the data type===============
  d3$sum.l = as.numeric(as.character(d3$sum.l))
  d3$sum.t = as.numeric(as.character(d3$sum.t))
  d3$mean.v = as.numeric(as.character(d3$mean.v))
  d3$sd.v = as.numeric(as.character(d3$sd.v))
  d3$sd.a = as.numeric(as.character(d3$sd.a))
  d3$mean.prs = as.numeric(as.character(d3$mean.prs))
  d3$sd.prs = as.numeric(as.character(d3$sd.prs))
  
  t.sld = as.numeric(which(d3$type=="slide"))
  t.tap = as.numeric(which(d3$type=="tap"))
  d4 = data.frame()
  s.ratio = len(t.sld)/(len(t.sld)+len(t.tap))
  t.ratio = len(t.tap)/(len(t.sld)+len(t.tap))
  # slide==============================
  if(len(t.sld)>len(t.tap))
  {
    s.md = mean(d3$sum.l[t.sld])    #Mean distance 
    s.ms = mean(d3$mean.v[t.sld])   #Mean speed 
    s.ss = mean(d3$sd.v[t.sld])     #Speed stalibility 
    s.mp = mean(d3$mean.prs[t.sld]) #Mean pressure 
    s.ps = mean(d3$sd.prs[t.sld])   #Pressure stalibility 
    s.ts = mean(d3$sd.a[t.sld])     #Trajectory smothness
    d4 = c(s.ratio, t.ratio, s.md, s.ms, s.mp, s.ps, s.ts)
  }else{# tap================================
        t.md = mean(d3$sum.l[t.tap])    #Mean distance
        t.ms = mean(d3$mean.v[t.tap])   #Mean speed
        #     t.ss = sd(d3$mean.v[t.tap])     #Speed stalibility 
        t.mp = mean(d3$mean.prs[t.tap]) #Mean pressure 
        t.ps = sd(d3$mean.prs[t.tap])   #Pressure stalibility
        t.s = sd(d3$sum.l[t.tap])       #Stalibility 
        d4 = c(s.ratio, t.ratio, t.md, t.ms, t.mp, t.ps, t.s)
  }
  
  if(1){
  #pause
  #readline()
  # draw pic===========================
  p <- cut(d2$prs, nrow(d2), label = FALSE)
  #colr <- rev(heat.colors(nrow(d2))) get the color for different prs 
  col.len = 2*nrow(d2)
  col.st = floor(col.len/4) - 1
  col.ed = col.len - col.st
  colr = rev(heat.colors(col.len)[col.st:col.ed])
  size.p = 2*(d2$prs-min(d2$prs))/diff(range(d2$prs))+0.3
  
  
#   pdf(name.pic)
#   par(mfrow=c(1,2))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(1,3))
  
  # pic1:Type of event=================
  slide = len(which(d3$type == "slide"))
  tap = len(which(d3$type == "tap"))
  t.e = cbind(slide, tap)
  barplot(t.e, col=c("mistyrose"), main = "Event Type")
  
  # pic2:xy trajetory and pressure=====
  plot(d2$x, d2$y, col = colr[p], cex = size.p, pch = 16,  
       ylim=rev(range(d2$y)), #because the pad y is from top to down
       xlab="x", ylab ="y", main = "Finger Trajectory and Pressure",)
  
  col=4
  for(i in 1:nrow(d2)) #start point
  {
    if(d2$id[i] != none)
      points(d2$x[i], d2$y[i], pch = 8, col = col, cex = .8)
  }
  
  legend(max(d2$x), min(d2$y), "start point", pch = 8, col=col, cex = .8, xjust = 0.8, yjust = 0.4, bty="n"
         , text.col=col)
  legend.col(col = colr, lev = d2$prs)
  
#   dev.off()
  
  #pause
  #readline()
#   game.info = rbind(game.info, d4)
#   game.name = c(game.name, fname)
  }
  
#   aa = ecdf(d3$sum.l)
#   summary(aa)
#   plot(aa , lwd = 2) ; mtext("lwd = 2", adj = 1)
#   xx <- unique(sort(c(seq(-3, 2, length = 201), knots(aa))))
#   lines(xx, aa(xx), col = "blue")
#   abline(v = knots(aa), lty = 2, col = "gray70")
  
}











