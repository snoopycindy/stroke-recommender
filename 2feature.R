setwd("E:/Dropbox/workspace/p_recommend_stroke/src")
source("../R.src/common.R")
source("../R.src/my.fig.R")
source("../R.src/my.legend.R")
source("../R.src/legend_col.R")
source("../R.src/rey.f.R")
source("../R.src/yx.common.R")
# utils:::menuInstallPkgs()
# install.packages("")
require(Hmisc)
require(plotrix)
library(Hmisc)
library(plotrix)

name.d = c("time", "type", "event", "value")
name.d2 = c("t", "sl", "id", "lv", "x", "y", "prs")


#origin data
odir <- "../origin data/"

#處理後的log檔
cdir <- "../complete data/"

#遊戲跟其code的列表
gn <- "../gn.txt"



#設定參數
ntect = -9 #設定none detect為-9
time_limit = 60 #game log檔至少要60S以上
gcode = read.table(gn, header=T, sep="\t", as.is=T, encoding="UTF-8") #讀取game列表
p.list=c('1201sub01','1202sub01','1202sub02', '1203sub01', '1205sub01','1205sub02') #設定player列表

#table:game name vs game code 
gcode = read.table(gn, header=T, sep="\t", as.is=T, encoding="UTF-8") #讀取game列表
cdir_path = paste(cdir, "d2", sep="")
allsub = list.files(path=cdir_path, full=T)
name.allsub=list.files(path=cdir_path)


# 1st Parse info - yx ==================
for(game in gcode$gc){
  fn.d3 = paste(cdir, "d3/",  game, ".txt", sep="")
  sink(fn.d3)
  for(sub in 1:len(allsub)) {
    # list all subject files =================
    fn.full = list.files(allsub[sub], full=T)         #list all the path
    fn.part = list.files(allsub[sub])                 #list file names only
    
    # find the game log =================  
    n = grep(game, fn.part)   #回傳fn.part的位置，如果吻合
    if(length(n)==0) 
      next
    
    # read file =================
    d2 = read.table(fn.full[n], header=T)
    names(d2)<-name.d2 #time, multi-event, event, pos.x, pos.y, press
    
    #clean the record which is in the prev game===============
    if(d2$id[1]==ntect){  # if the first record is not a start of actions
      act.last=which(d2$id!=ntect)[1]-1
      if(is.na(act.last))
        stop()
      d2 <- d2[-(1:act.last),]
    }
    
    #若x,y,prs為ntect(因為資料在上個遊戲)，拿掉data
    d2 <- d2[d2$x!=ntect & d2$y!=ntect,]
    d2$prs[d2$prs==ntect] <- NA
    
    
    #if "sum.l>thd.dis" means that the type is belong to "slide"  
    thd.dis = 48  
    # To find the id list in each trace
    id.list = unique(d2$id)
    
    for(j in id.list)
    {
      isd = which(d2$id == j)
      dx = diff(d2$x[isd])
      dy = diff(d2$y[isd])
      vec = cbind(dx, dy)
      st = d2$t[min(isd)]
      et = d2$t[max(isd)]
      # Velocity & Speed =======================
      part.l = sqrt(dx^2+dy^2)
      part.t = diff(d2$t[isd])
      sum.l = sum(part.l)
      sum.t = diff(range(d2$t[isd]))
      if(sum.l < thd.dis){
        type="tap"
        mean.s = NA
        mean.v = NA
        sd.v = NA
      }else{
        type="sld"
        mean.s = sum.l/sum.t
        part.v = part.l/part.t
        mean.v = mean(part.v, na.rm = T)
        sd.v = sd(part.v, na.rm = T)
      } 
      
      # Pressure=========================
      mean.prs = mean(d2$prs[isd])
      sd.prs = sd(d2$prs[isd], na.rm = T)
      # game feature belong to one subject
      d3 = c(game, name.allsub[sub], type, sum.l, st, et, sum.t, 
             mean.prs, sd.prs, mean.v, sd.v)
      cat(d3, "\n")
    }
  }
  sink()
}

# 2nd Parse info - yx ==================
fn.d5 = paste(cdir, "d5.txt", sep="")
sink(fn.d5)
for(game in gcode$gc){
  
  fn.d3 = paste(cdir, "d3/",  game, ".txt", sep="")
  d3 = read.table(file = fn.d3)
  names(d3) = c("gn", "sub", "type", "sum.l", "st", "et", "sum.t", 
                "mean.prs", "sd.prs", "mean.v", "sd.v")
  sub.list = unique(d3$sub)
  if(len(grep("tap",game))){
    t.tap=1
    t.sld=0
  }else{
    t.tap=0
    t.sld=1
  }
  
  fn.d4 = paste(cdir, "d4/",  game, ".txt", sep="")
#   sink(fn.d4)
  for(s in sub.list){
    isd = which(d3$sub == s)
    t.s = d3$st[min(isd)]
    t.e = d3$et[max(isd)]
    t.all = t.e-t.s
    num.act = len(isd)
    frq.act = num.act/t.all
    
    id.tap = which(d3$type[isd]=="tap")
    num.tap = len(id.tap)
    frq.tap = num.tap/t.all
    ratio.tap = num.tap/num.act
    dur.tap = mean(d3$sum.t[id.tap], na.rm=T)
    
    id.sld = which(d3$type[isd]=="sld")
    num.sld = len(id.sld)
    frq.sld = num.sld/t.all
    ratio.sld = num.sld/num.act
    dis.sld = mean(d3$sum.l[id.sld], na.rm=T)
    d4 = c(game, s, t.all, num.act, frq.act, 
           num.tap, frq.tap, ratio.tap, dur.tap,
           num.sld, frq.sld, ratio.sld, dis.sld, 
           t.tap, t.sld)
    cat(d4, "\n")
  }
#   sink()
}
sink()

# linear regression ==================
fn.d5 = paste(cdir, "d5.txt", sep="")
d5 = read.table(fn.d5)
names(d5) = c("gn", "sub", "t.all", "num.act", "frq.act", 
              "num.tap", "frq.tap", "ratio.tap", "dur.tap",
              "num.sld", "frq.sld", "ratio.sld", "dis.sld", "t.tap", "t.sld")
for(sub in p.list){
  data.now = d5[which(d5$sub==sub),]
  fn.std = paste(cdir, "std/", sub, ".csv", sep="")
  std = read.table(fn.std)
  std = std[,-(1:3)]
  game.list = unique(data.now$gn)
  d6 = data.frame()
  for(game in game.list){
    n = grep(game, std$code)   #回傳fn.part的位置，如果吻合
    if(length(n)==0) 
      next
    d6 = rbind(d6, std[n,])
  }
  d6 = d6[,-1]
  d6 = cbind(data.now, d6)
  fn.d6 = paste(cdir, "d6/",  sub, ".txt", sep="")
  write.table(d6, fn.d6)
  
}

# train a linear regression model for each subject
library("ppls")
f.list = c(4:15,17)
for(sub in p.list){
  fn.d6 = paste(cdir, "d6/",  sub, ".txt", sep="")
  d6 = read.table(fn.d6)
  # remove the lines in this dataframe that contain NAs across all columns
  d6 = d6[complete.cases(d6),]
  # normalize by each feature
  d6[,f.list] = apply(d6[,f.list],2, normalize.vector) 
  
  # calculate the weight (sld/tap) of each feature
  w.sld = w.tap = {}
  id.sld = grep("sld",d6$gn)
  id.tap = grep("tap",d6$gn)
  for(f in f.list){
    w.sld = c(w.sld, sum(d6[id.sld,f]*d6$action)/nrow(d6))
    w.tap = c(w.tap, sum(d6[id.tap,f]*d6$action)/nrow(d6))
  }
  cat(w.sld, "\n")
  cat(w.tap, "\n")
  
  pred.sld = apply(d6[id.sld, f.list]*w.sld, 1, sum)
  pred.tap = apply(d6[id.tap, f.list]*w.tap, 1, sum)
  
  # Multiple (Linear) Regression=====================
  f.list = c(4:15,17)
  d6 = read.table(fn.d6)
  # remove the lines in this dataframe that contain NAs across all columns
  d6 = d6[complete.cases(d6),f.list]
  d.sld = d6[id.sld, ]
  d.tap = d6[id.tap, ]
  
#   fit.sld <- lm(action ~ ., data = d.sld)
#   fit.tap <- lm(action ~ ., data = d.tap)
#   fit = fit.tap
  fit<-lm(action ~ ., data = d6)
  
  summary(fit) # show results
  coefficients(fit) # model coefficients
  confint(fit.sld, level=0.95) # CIs for model parameters 
  fitted(fit) # predicted values
  residuals(fit) # residuals
  anova(fit) # anova table 
  vcov(fit) # covariance matrix for model parameters 
  influence(fit) # regression diagnostics
  
  # K-fold cross-validation
  library(DAAG)
  cv.lm(df=d6, fit, m=3) # 3 fold cross-validation
  
  # compare models
  fit1 <- lm(y ~ x1 + x2 + x3 + x4, data=mydata)
  fit2 <- lm(y ~ x1 + x2)
  anova(fit1, fit2)

 
}


# recommendlab=================================
library(recommenderlab) 
d7=data.frame()
for(sub in p.list){
  fn.d6 = paste(cdir, "d6/",  sub, ".txt", sep="")
  d6 = read.table(fn.d6)
  d7 = rbind(d7, d6)
}

user.rate = c(2, 1, 17)
group.rate = c(1,4:15)
dist(as.matrix(d7[,group.rate]), method = "cosine")
## coerce into a realRatingMAtrix
r.u <- as(d7[,user.rate], "realRatingMatrix")
## between sld items
dissimilarity(r.u[,1:25], method = "pearson", which = "items")
similarity(r.u[,1:25], method = "pearson", which = "items")
## between tap items
dissimilarity(r.u[,26:50], method = "pearson", which = "items")
similarity(r.u[,26:50], method = "pearson", which = "items")




## create 90/10 split (known/unknown) for the first 500 users in Jester5k
e <- evaluationScheme(r, method="split", train=0.9,
                      k=1, given=15)
e
## create a user-based CF recommender using training data
r1 <- Recommender(getData(e, "train"), "UBCF")

p <- predict(r1, getData(e, "known"), type="ratings")
p
## compute error metrics averaged per user and then averaged over all
## recommendations
calcPredictionAccuracy(p, getData(e, "unknown"))
head(calcPredictionAccuracy(p, getData(e, "unknown"), byUser=TRUE))

## evaluate topNLists instead (you need to specify given and goodRating!)
p <- predict(r1, getData(e, "known"), type="topNList")
p
calcPredictionAccuracy(p, getData(e, "unknown"), given=15, goodRating=5)




e <- evaluationScheme(r, method="split", train=0.9,
                      k=1, given=3)
e
## create a user-based CF recommender using training data
r2 <- Recommender(getData(e, "train"), "UBCF")
## create predictions for the test data using known ratings (see given above)
p <- predict(r2, getData(e, "known"), type="topNList", n=10)
p
calcPredictionAccuracy(p, getData(e, "unknown"), given=3, goodRating=3)


# k-means =========================
d5 = read.table(fn.d5)
names(d5) = c("gn", "sub", "t.all", "num.act", "frq.act", 
              "num.tap", "frq.tap", "ratio.tap", "dur.tap",
              "num.sld", "frq.sld", "ratio.sld", "dis.sld")
game.list = unique(d5$gn)
plot(d5$t, d5$prs, type="n", 
     xlim = range(d5$num.tap), 
     ylim = range(d5$num.sld),
     xlab = "num.tap",
     ylab = "num.sld")

for(g in 1:50){
  isd = which(d5$gn==unique(d5$gn)[g])
  if(g<26) col=1 else col=2
  points(d5$num.tap[isd], d5$num.sld[isd], col=col)
}

label_f <- function(x){
  l = grep("sld", x)
  if(len(l))
    y=1
  else
    y=0
}

y = sapply(d5$gn, label_f)

  
f.list = c(4:13)
d6 = d5[-268,f.list]
d6 = scale(d6)
cl <- kmeans(d6, 2)
plot(d5, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8, cex = 2)
ss <- function(d5) sum(scale(d5, scale = FALSE)^2)

