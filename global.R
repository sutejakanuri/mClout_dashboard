library(plotly)
#library(rCharts)
library(dplyr)
#require(xlsx)
library(lubridate)
library(shinyBS)
library(shinyjs)
library(readxl)
library(DT)


####################################################
#This method fetches week entries for the drop down 
#Screen: 1
###################################################
create_link<-function(df){
  sprintf('<a href="https://www.google.com/#q=%s" target="_blank">Info</a>',val)
}

fetch_week_dropdown <- function(Post_Data){
  
  Post_Data$Date=gsub("/", "-", Post_Data$Date)
  Post_Data$Date=dmy(Post_Data$Date)
  Post_Data=Post_Data[ order(Post_Data$Date , decreasing = FALSE ),]
  week <- data.frame(Dates = Post_Data$Date, Week = format(Post_Data$Date, format = "%W"))
  week <- arrange(week, (week$Dates))
  week=week[!duplicated(week), ]
  
  diplay_week <- week %>%
    group_by(week$Week) %>%
    arrange((week$Dates)) %>%
    filter(row_number()==1 | row_number()==n())%>%
    ungroup
  
  
  #display_week = as.data.frame(display_week)
  display_week=diplay_week[,c(1,2)]
  display_week = as.data.frame(display_week)
  display_week$month = months(display_week$Dates)
  display_week$day   = day(display_week$Dates)
  display_week$date = paste(display_week$month, display_week$day, sep=" ")
  display_week = as.data.frame(display_week)
  if (nrow(display_week)==0) {  return() }
  res <- aggregate(display_week$date, list(as.character(display_week$Week)), paste, collapse=" - ")
  res1<- aggregate(display_week$Dates, list(as.character(display_week$Week)), paste, collapse=" to ")
  names(res) <- c('week','date_range')
  names(res1) <- c('week','raw_date_range')
  df <- merge(res, res1, by.x = "week", by.y = "week")
  
  
  return (df)
  
}



get_markettrend_data <- function(FB_Post_Data,start_date,end_date){
  
  FB_Post_Data$Market= as.character(lapply(strsplit(as.character(FB_Post_Data$Label1), split="_"),tail, n=1))
  unique_markets =  unique(as.character(lapply(strsplit(as.character(FB_Post_Data$Label1), split="_"),tail, n=1)))
 
  
  FB_Post_Data$Date=gsub("/", "-", FB_Post_Data$Date)
  FB_Post_Data$Date=dmy(FB_Post_Data$Date)
  
   FB_Post_Data = subset(FB_Post_Data, Date>=start_date & Date <=end_date)
  #print (names(subset_by_week))
  return (FB_Post_Data)
  
}


filter_by_date = function (df, start_date,end_date){
  if (nrow(df)==0) { return() }
  df$Date=gsub("/", "-", df$Date)
  df$Date=dmy(df$Date)
  df = subset(df, Date>=start_date & Date <=end_date)
  return (df)
}


map_month <-function(month){
  
  return (match( tolower(month), tolower(month.abb)))
  
}


#This method is used to create a stats data frame in the screen 3

get_stats_df <- function (FB_Summary_Data,TW_Page_Data,IG_Page_Data,IG_summary_Data,FB_Page_Data,infleuncer_name){
  
  
  FB = FB_Summary_Data[FB_Summary_Data$Name == infleuncer_name,]
  FB=FB[rowSums(is.na(FB))!=ncol(FB), ]
  TW = TW_Page_Data[TW_Page_Data$Username == infleuncer_name,]
  TW=TW[rowSums(is.na(TW))!=ncol(TW), ]
  IG = IG_Page_Data[IG_Page_Data$Username == infleuncer_name,]
  IG=IG[rowSums(is.na(IG))!=ncol(IG), ]
  
  IG_summary = IG_summary_Data[IG_summary_Data$Username == infleuncer_name,]
  IG_summary=IG_summary[rowSums(is.na(IG))!=ncol(IG), ]
  
  FB_Page_Data = FB_Page_Data[FB_Page_Data$Username == infleuncer_name,]
  FB_Page_Data=FB_Page_Data[rowSums(is.na(FB))!=ncol(FB), ]
  
  if (nrow(FB)==0){
    followers_FB=0
    average_ER_FB=0
  }else{
    followers_FB=sum(FB_Page_Data$Fans)
    average_ER_FB=sum(FB$`Average Page ER`)
  }
  
  if (nrow(TW)==0){
    followers_TW=0
    average_ER_TW=0
  }else{
    followers_TW=sum(TW$Followers)
    average_ER_TW=sum(sum(TW$Replies),sum(TW$Favorites),sum(TW$Retweets))
  }
  
  if (nrow(IG)==0){
    followers_IG=0
    average_ER_IG=0
  }else{
    followers_IG=sum(IG$Followers)
    average_ER_IG = sum(IG_summary$`ER Rate`)
  }
  df_fb<-data.frame("Facebook",followers_FB,average_ER_FB)
  names(df_fb)<-c("Platform","Followers","AVG ER")
  
  df_ig<-data.frame("Instagram",followers_IG,average_ER_IG)
  names(df_ig)<-c("Platform","Followers","AVG ER")
  
  df_tw<-data.frame("Twitter",followers_TW,average_ER_TW)
  names(df_tw)<-c("Platform","Followers","AVG ER")
  
  df=rbind(df_fb,df_ig,df_tw)
  
}

get_EER_instagram <- function(IG_Page_Data ,IG_Post_Data){
  
  IG_Page_Data["Earned_Effective_Reach"] = 0.035 * IG_Page_Data["Followers"] 
  IG_Post_Data_image_df = IG_Post_Data[grepl("image|carousel", IG_Post_Data$Type)==TRUE,]
  IG_Post_Data_image_df=merge(IG_Post_Data_image_df,IG_Page_Data, by  = c("Username","Date"))
  IG_Post_Data_image_df = IG_Post_Data_image_df[,c("Date","Earned_Effective_Reach" )]
  
  IG_Post_Data_video_df = IG_Post_Data[(grepl("video", IG_Post_Data$Type)==TRUE),]
  IG_Post_Data_video_df["Earned_Effective_Reach"] = 0.25 * IG_Post_Data_video_df["Views"] 
  IG_Post_Data_video_df = IG_Post_Data_video_df[,c("Date","Earned_Effective_Reach" )]
  
  IG_Post_Data_video_df=IG_Post_Data_video_df[, c("Date","Earned_Effective_Reach" )]
  
  IG_Post_Data_sk2 = rbind(IG_Post_Data_image_df,IG_Post_Data_video_df)
  return(IG_Post_Data_sk2)
}
get_EER_twitter <- function(TW_Page_Data ,TW_Post_Data){
  
  TW_Page_Data["Earned_Effective_Reach"] = 0.054 * TW_Page_Data["Followers"] 
  TW_Post_Data_image_df = TW_Post_Data[grepl(paste(c("Link","Photo","Text"), collapse = "|"), TW_Post_Data$Type)==TRUE,]
  TW_Post_Data_image_df=merge(TW_Post_Data_image_df,TW_Page_Data, by  = c("Username","Date"))
  TW_Post_Data_image_df=TW_Post_Data_image_df[, c("Date","Earned_Effective_Reach" )]
  
  return (TW_Post_Data_image_df)
}

get_stats_df_JP <- function (TW_Page_Data,IG_Page_Data,IG_summary_Data,infleuncer_name){
  
  
  TW = TW_Page_Data[TW_Page_Data$Username == infleuncer_name,]
  TW=TW[rowSums(is.na(TW))!=ncol(TW), ]
  IG = IG_Page_Data[IG_Page_Data$Username == infleuncer_name,]
  IG=IG[rowSums(is.na(IG))!=ncol(IG), ]
  
  IG_summary = IG_summary_Data[IG_summary_Data$Username == infleuncer_name,]
  IG_summary=IG_summary[rowSums(is.na(IG))!=ncol(IG), ]
  
  if (nrow(TW)==0){
    followers_TW=0
    average_ER_TW=0
  }else{
    followers_TW=sum(TW$Followers)
    average_ER_TW=sum(sum(TW$Replies),sum(TW$Favorites),sum(TW$Retweets))
  }
  
  if (nrow(IG)==0){
    followers_IG=0
    average_ER_IG=0
  }else{
    followers_IG=sum(IG$Followers)
    average_ER_IG = sum(IG_summary$`ER Rate`)
  }
  
  df_ig<-data.frame("Instagram",followers_IG,average_ER_IG)
  names(df_ig)<-c("Platform","Followers","AVG ER")
  
  df_tw<-data.frame("Twitter",followers_TW,average_ER_TW)
  names(df_tw)<-c("Platform","Followers","AVG ER")
  
  df=rbind(df_ig,df_tw)
  return (df)
  
}
add_EER <- function(Page_Data,Post_Data,platform){
  if (platform == "TW"){
    Page_Data["Earned_Effective_Reach"] = 0.054 * Page_Data["Followers"] 
    Post_Data_image_df = Post_Data[grepl(paste(c("Link","Photo","Text"), collapse = "|"), Post_Data$Type)==TRUE,]
    Post_Data_image_df=merge(Post_Data_image_df,Page_Data, by  = c("Username","Date"))
    
    Post_Data_video_df = Post_Data[grepl("Video", Post_Data$Type)==TRUE,]
    Post_Data_video_df=merge(Post_Data_video_df,Page_Data, by  = c("Username","Date"))
    if (nrow(Post_Data_video_df)!=0){
      Post_Data_video_df$Earned_Effective_Reach=0
      df = rbind(Post_Data_image_df,Post_Data_video_df)
    }else{
      df=Post_Data_image_df
    }
    df$Followers.y=NULL
    df$Day.y = NULL
    names(df)[4] = "Day"
    names(df)[17] = "Followers"
    df=select(df, Date, Text, Username,`Engagement Rate`,sentiment_keyword,mscore,`Post Image`,`Reply Count`,`Favorite Count`,`Retweet Count`,Earned_Effective_Reach,`Media Url`)
  }
  if (platform == "IG"){
    
    Page_Data["Earned_Effective_Reach"] = 0.035 * Page_Data["Followers"] 
    Post_Data_image_df = Post_Data[grepl("image|carousel", Post_Data$Type)==TRUE,]
    Post_Data_image_df=merge(Post_Data_image_df,Page_Data, by  = c("Username","Date"))
    Post_Data_image_df = select(Post_Data_image_df,Username,Date,Caption,Engagement,`Engagement Rate`,sentiment_keyword,mscore,`Media URL`,Earned_Effective_Reach,Media )
    
    Post_Data_video_df = Post_Data[(grepl("video", Post_Data$Type)==TRUE),]
    Post_Data_video_df["Earned_Effective_Reach"] = 0.25 * Post_Data_video_df["Views"] 
    Post_Data_video_df = select(Post_Data_video_df,Username,Date,Caption,Engagement,`Engagement Rate`,sentiment_keyword,mscore,`Media URL`,Earned_Effective_Reach,Media )
    
    IG_Post_Data_sk2 = rbind(Post_Data_image_df,Post_Data_video_df)
    df=IG_Post_Data_sk2
  }
  
  return(df)
}
add_mscore <- function(df,type){
  df$sentiment_keyword=""
  df$weight=0
  
  df[df$Sentiment <=39,"sentiment_keyword"] = "NEGATIVE"
  #df[df$Sentiment <=39,"weight"] = -1

  df[df$Sentiment >= 40 & df$Sentiment <=49,"sentiment_keyword" ] = "NEUTRAL"
  #df[data$Sentiment <=39,"weight"] = 0

  df[df$Sentiment >=49,"sentiment_keyword"] = "POSITIVE"
  #df[df$Sentiment <=39,"weight"] = 1
  
  df[df$sentiment_keyword=="POSITIVE","weight"] = 1
  df[df$sentiment_keyword=="NEUTRAL","weight"] = 0
  df[df$sentiment_keyword=="NEGATIVE","weight"] = -1
  
  #View(df)
  if (type == "FB"){
    
    # if (df$sentiment >=0 && df$sentiment <= 39){
    #   df$weight=-1
    #   df$sentiment_keyword="NEGATIVE"
    # }else if(df$sentiment >=40 && df$sentiment <= 49){
    #   df$weight=0
    #   df$sentiment_keyword="NEUTRAL"
    # }else {
    #   df$weight=1
    #   df$sentiment_keyword="POSITIVE"
    # }
    
    
    df$mscore = df$`Reaction Likes` + df$`Reaction Love`+df$`Reaction Haha`+df$`Reaction Wow` - df$`Reaction Sad`-df$`Reaction Anger` + (df$Comments * df$sentiment * df$weight) + df$Shares
  }
  if (type =="IG"){
      
    # if (df$Sentiment >=0 && df$Sentiment <= 39){
    #   df$weight=-1
    #   df$sentiment_keyword="NEGATIVE"
    # }else if(df$Sentiment >=40 && df$Sentiment <= 49){
    #   df$weight=0
    #   df$sentiment_keyword="NEUTRAL"
    # }else {
    #   df$weight=1
    #   df$sentiment_keyword="POSITIVE"
    # }
    
    
    
    df$mscore = df$Likes + (df$Comments*df$Sentiment*df$weight)
  }
  
  if (type == "TW"){
    
    
    # if (df$Sentiment >=0 && df$Sentiment <= 39){
    #   df$weight=-1
    #   df$sentiment_keyword="NEGATIVE"
    # }else if(df$Sentiment >=40 && df$Sentiment <= 49){
    #   df$weight=0
    #   df$sentiment_keyword="NEUTRAL"
    # }else {
    #   df$weight=1
    #   df$sentiment_keyword="POSITIVE"
    # }
    
    df$mscore = df$`Favorite Count` + (df$`Reply Count`* df$Sentiment * df$weight) + df$`Retweet Count`
  }
  
  View(df)
  return (df)
  
}


add_logo <- function(df){
  
  height = " height=\"52\"></img>"
  
  fb_imageName= "facebook.png"
  fb_imageName=paste0("\"",fb_imageName,"\"")
  fb_imageName=paste0("<img src=",fb_imageName,height)


  in_imageName= "instagram.png"
  in_imageName=paste0("\"",in_imageName,"\"")
  in_imageName=paste0("<img src=",in_imageName,height)


  tw_imageName= "twitter.png"
  tw_imageName=paste0("\"",tw_imageName,"\"")
  tw_imageName=paste0("<img src=",tw_imageName,height)
  
  df$type[df$PLATFORM=='TWITTER'] <- tw_imageName
  df$type[df$PLATFORM=='FACEBOOK'] <- fb_imageName
  df$type[df$PLATFORM=='INSTAGRAM'] <- in_imageName
  
  return(df)

  # df$PLATFORM = NULL
  # names(df)=c("DATE", "DESCRIPTION", "KOL", "EARNED EFFECTIVE REACH","ENGAGEMENT","REACTIONS","SENTIMENT","M-SCORE","POST","PLATFORM")
  # df <- df[,c("DATE", "POST", "DESCRIPTION","KOL", "PLATFORM","EARNED EFFECTIVE REACH","ENGAGEMENT","REACTIONS","SENTIMENT","M-SCORE")]
  
  
}