Facebook粉絲團分析（分析專頁名稱為朱立倫的FB粉絲專頁）
================

分析朱立倫臉書粉絲專業，資料分析區間為2016/01/01-2016/4/9。

``` r
if (!require('Rfacebook')){
    install.packages("Rfacebook")
    library(Rfacebook)
}
```

    ## Loading required package: Rfacebook

    ## Warning: package 'Rfacebook' was built under R version 3.2.4

    ## Loading required package: httr

    ## Warning: package 'httr' was built under R version 3.2.4

    ## Loading required package: rjson

    ## Loading required package: httpuv

    ## Warning: package 'httpuv' was built under R version 3.2.4

    ## 
    ## Attaching package: 'Rfacebook'

    ## The following object is masked from 'package:methods':
    ## 
    ##     getGroup

讀取朱立倫粉絲團資料
--------------------

``` r
token<-'CAACEdEose0cBAKgLDL114Dpe1oUfhZCUF3Cy0K8s1ZAndZBlKaYSZC6fouqkfODu6QSfmRcJ6ZCaspuZCDIaaqarGOLtohyfwQRXZB7h6182TlSLzAq2ZBjTFrR37jpALaIRZBDX2rV40oU0HnLvngZAsIJdryReOztcV12P3aML3Wcd8gui7hEhFnUWwiwsYIQof6nBQEPILrdlunrSxufPVW'
totalPage<-NULL
lastDate<-Sys.Date()
DateVectorStr<-as.character(seq(as.Date("2016-01-01"),lastDate,by="5 days"))
for(i in 1:(length(DateVectorStr)-1)){
    tempPage<-getPage("llchu", token,
                      since = DateVectorStr[i],until = DateVectorStr[i+1])
    totalPage<-rbind(totalPage,tempPage)
}
```

    ## 14 posts 13 posts 25 posts 4 posts 5 posts 5 posts 7 posts 5 posts 1 posts 6 posts 5 posts 3 posts 4 posts 6 posts 4 posts 8 posts 5 posts 5 posts 4 posts

``` r
nrow(totalPage)
```

    ## [1] 129

從2016/01/01至2016/04/09，共有129篇貼文!

每日發文數分析:
---------------

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PostCount<-aggregate(id~dateTPE,totalPage,length)
library(knitr)
kable(head(PostCount[order(PostCount$id,decreasing = T),]))
```

|     | dateTPE    |   id|
|-----|:-----------|----:|
| 12  | 2016-01-12 |    7|
| 13  | 2016-01-13 |    5|
| 14  | 2016-01-14 |    5|
| 15  | 2016-01-15 |    5|
| 65  | 2016-03-20 |    4|
| 1   | 2016-01-01 |    3|

在2016/01/12發文數最多，共有7篇，因時間近選舉日，都是呼籲民眾出來投票或是說台灣人真的很棒或是拜票文。

每日獲得讚數分析:
-----------------

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
LikeCount<-aggregate(likes_count~dateTPE,totalPage,sum)
library(knitr)
kable(head(LikeCount[order(LikeCount$likes_count,decreasing = T),]))
```

|     | dateTPE    |  likes\_count|
|-----|:-----------|-------------:|
| 15  | 2016-01-15 |        247024|
| 12  | 2016-01-12 |        198144|
| 16  | 2016-01-16 |        166772|
| 14  | 2016-01-14 |        113835|
| 13  | 2016-01-13 |        113799|
| 9   | 2016-01-09 |        105459|

在2016/01/16 獲得讚數最多，達24萬人次，因為是選舉日當天，所以大家都很關注總統候選人的各個消息。

每日評論數分析
--------------

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
CommentsCount<-aggregate(comments_count~dateTPE,totalPage,sum)
library(knitr)
kable(head(CommentsCount[order(CommentsCount$comments_count,decreasing = T),]))
```

|     | dateTPE    |  comments\_count|
|-----|:-----------|----------------:|
| 15  | 2016-01-15 |            39219|
| 16  | 2016-01-16 |            21211|
| 12  | 2016-01-12 |             7210|
| 14  | 2016-01-14 |             4248|
| 13  | 2016-01-13 |             4015|
| 1   | 2016-01-01 |             3865|

在2016/01/16獲得評論數最多，高達3萬九千多人次，因為當天為選舉日，所以大家意見都很多。

每日分享數分析
--------------

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
SharesCount<-aggregate(shares_count~dateTPE,totalPage,sum)
library(knitr)
kable(head(SharesCount[order(SharesCount$shares_count,decreasing = T),]))
```

|     | dateTPE    |  shares\_count|
|-----|:-----------|--------------:|
| 15  | 2016-01-15 |          11713|
| 12  | 2016-01-12 |           7004|
| 1   | 2016-01-01 |           4563|
| 13  | 2016-01-13 |           2780|
| 6   | 2016-01-06 |           2746|
| 16  | 2016-01-16 |           2727|

在2016/01/15文章分享數最多，高達1萬1千多人次，因為大選前一天，PO文皆屬於積極、正面、時事種類，可能獲得較多支持者認同並分享。
