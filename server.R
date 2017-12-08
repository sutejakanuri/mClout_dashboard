
#path="C:/Users/suteja.kanuri/Documents/MC/mFluence/Copy of Report-Multiple-09-06-2017-to-28-06-2017 (003).xlsx"
# FB_Post_Data  =read_excel(path, sheet = "FB Post Data", encoding="UTF-8", stringsAsFactors=FALSE)
# FB_Post_Data$Market= as.character(lapply(strsplit(as.character(FB_Post_Data$Label1), split="_"),tail, n=1))
# unique_markets =  unique(as.character(lapply(strsplit(as.character(FB_Post_Data$Label1), split="_"),tail, n=1)))
options(shiny.maxRequestSize = 30*1024^2)
server=shinyServer(function(input, output, session){

  v <- reactiveValues(data = NULL)
  
  observeEvent(input$button1,{
    # print ("button1")
    v$button_clicked = "button1"
    v$data <- reactive_get_influencer1_stats()
    # v$platform_params <- c("Facebook","Twitter","Instagram")
    # v$display_type <- c("Reach","Engagement","Engagment Rate",'Sentiment','M-Score')
    # v$display_typeofposts <-  c("All", "SK-II Posts","Non SK-II Posts")
    # v$display_month <-  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    # v$historical_trend = "HISTORICAL TREND"
    # v$button_clicked = "button1"
    # v$title_sk2_post = "TOP POSTS FOR SK-II"
    # v$title_top_comments = "TOP COMMENTS"
    reactive_display()
  })
  
  observeEvent(input$button2,{
    # print ("button2")
    reactive_display()
    v$button_clicked = "button2"
    v$data <- reactive_get_influencer2_stats()
    
  })
  observeEvent(input$button3,{
    # print ("button3")
    reactive_display()
    v$button_clicked = "button3"
    v$data <- reactive_get_influencer3_stats()
  })
  observeEvent(input$button4,{
    # print ("button4")
    reactive_display()
    v$button_clicked = "button4"
    v$data <- reactive_get_influencer4_stats()
    
  })
  observeEvent(input$button5,{
    # print ("button5")
    reactive_display()
    v$button_clicked = "button5"
    v$data <- reactive_get_influencer5_stats()
  })
  observeEvent(input$button6,{
    # print ("button6")
    reactive_display()
    v$button_clicked = "button6"
    v$data <- reactive_get_influencer6_stats()
  })
  observeEvent(input$button7,{
    # print ("button7")
    reactive_display()
    v$button_clicked = "button7"
    v$data <- reactive_get_influencer7_stats()
  })
  observeEvent(input$button8,{
    # print ("button8")
    reactive_display()    
    v$button_clicked = "button8"
    v$data <- reactive_get_influencer8_stats()
  })
  observeEvent(input$button9,{
    # print ("button9")
    reactive_display()
    v$button_clicked = "button9"
    v$data <- reactive_get_influencer9_stats()
  })
  observeEvent(input$button10,{
    # print ("button10")
    reactive_display()
    v$button_clicked = "button10"
    v$data <- reactive_get_influencer10_stats()
  })
  
  output$influencer_stats_table <- renderDataTable({
    if (is.null(v$data)) return()
    v$data
    #View(v$data)

  },options = list(dom = 't',autoWidth = FALSE))
  
  # output$influencer_stats_table2 <- renderDataTable({
  #   if (is.null(v$abc)) return()
  #   v$abc
  #   
  # },options = list(dom = 't'))
  
  #HERE
  output$sk2_posts_table <- DT::renderDataTable({
    
    req(input$typeofposts_ht)
    #get_sk2_data=NULL
    
    if(input$typeofposts_ht!='SK-II Posts')return()
    if (input$typeofposts_ht=='SK-II Posts'){
      get_sk2_data = get_sk2_data()
      # print("nrow(get_sk2_data) from this")
      # print (length(get_sk2_data))
      #DT::datatable(get_sk2_data,escape=F, options = list(pageLength = 5,searching = FALSE))
      DT::datatable(get_sk2_data,escape=F, options = list(
        language = list(
          zeroRecords = "No records to display for the selected filter! Please change the filter options and try again."),
        
        pageLength = 5,searching = FALSE,columnDefs = list(list(
        targets = 3,
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 6 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 60) + '...</span>' : data;",
          "}")
      ))))%>% formatStyle(
        'M-SCORE',
        backgroundColor = "#FFC6DE"
      )%>% formatStyle(
        'SENTIMENT',
        color = styleEqual(c("POSITIVE", "NEGATIVE", "NEUTRAL"), c('#2a9a2a', '#f40939','#FFC200'))
      )%>% formatStyle(
        'ENGAGEMENT', 'SENTIMENT',
        color = styleEqual(c("POSITIVE", "NEGATIVE", "NEUTRAL"), c('#2a9a2a', '#f40939','#FFC200'))
      )%>% formatStyle(
        'ENGAGEMENT RATE', 'SENTIMENT',
        color = styleEqual(c("POSITIVE", "NEGATIVE", "NEUTRAL"), c('#2a9a2a', '#f40939','#FFC200'))
      )%>% formatStyle(
        'M-SCORE', 'SENTIMENT',
        color = styleEqual(c("POSITIVE", "NEGATIVE", "NEUTRAL"), c('#2a9a2a', '#f40939','#FFC200'))
      )
  
      
    } 
    })

  
  
  
  
  output$most_recent_post_table <- DT::renderDataTable({
    
    # FB_Post_Data  = reactive_FB_Post_Data()
    # FB_Post_Data = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    # FB_Post_Data = add_mscore(FB_Post_Data,"FB")
    # FB_Post_Data$platform = "FACEBOOK"
    # FB_Post_Data = select(FB_Post_Data,Date,post_text,Name,platform,post_impressions_unique,Engagement,Likes,mscore,post_image)
    # names(FB_Post_Data) = c("DATE", "POST", "KOL", "PLATFORM","REACH","ENGAGEMENT","REACTIONS","M-SCORE","URL")
    
    
    TW_Post_Data  = reactive_TW_Post_Data()
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    TW_Post_Data = add_mscore(TW_Post_Data,"TW")
    TW_Post_Data$Engagement=  TW_Post_Data["Reply Count"] + TW_Post_Data["Favorite Count"] + TW_Post_Data["Retweet Count"]
    TW_Post_Data=as.data.frame(TW_Post_Data)
    TW_Post_Data$platform = "TWITTER"
    TW_Post_Data$Engagement = TW_Post_Data$Engagement$`Reply Count`
    
    #TW_Post_Data$reach = 0
    # TW_Post_Data = select(TW_Post_Data,Date,Text,Username,platform,reach,Engagement,`Favorite Count`,sentiment_keyword,mscore,`Post Image`)
    # TW_Post_Data$Engagement = TW_Post_Data$Engagement$`Reply Count`
    # names(TW_Post_Data) = c("DATE", "POST", "KOL", "PLATFORM","REACH","ENGAGEMENT","REACTIONS","SENTIMENT","M-SCORE","URL")
    TW_Page_Data = reactive_TW_Page_Data()
    TW_EER = add_EER(TW_Page_Data,TW_Post_Data,"TW")
    TW_Post_Data = merge(TW_Post_Data,TW_EER)
    TW_Post_Data = select(TW_Post_Data,Date,Text,Username,platform,
                          Earned_Effective_Reach,Engagement,`Favorite Count`,sentiment_keyword,mscore,`Post Image`)
    names(TW_Post_Data) = c("DATE", "POST", "KOL", "PLATFORM","EARNED EFFECTIVE REACH","ENGAGEMENT","REACTIONS","SENTIMENT","M-SCORE","URL")
    
    IG_Post_Data  = reactive_IG_Post_Data()
    IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    IG_Post_Data = add_mscore(IG_Post_Data,"IG")
    IG_Post_Data$platform="INSTAGRAM"
    
    IG_Page_Data = reactive_IG_Page_Data()
    IG_EER = add_EER(IG_Page_Data,IG_Post_Data,"IG")
    IG_Post_Data = merge(IG_Post_Data,IG_EER)
    IG_Post_Data = select(IG_Post_Data,Date,Caption,Username,platform,Earned_Effective_Reach,Engagement,Likes,sentiment_keyword,mscore,`Media URL`)
    names(IG_Post_Data) = c("DATE", "POST", "KOL", "PLATFORM","EARNED EFFECTIVE REACH","ENGAGEMENT","REACTIONS","SENTIMENT","M-SCORE","URL")
    # IG_Post_Data$reach=0
    # IG_Post_Data = select(IG_Post_Data,Date,Caption,Username,platform,reach,Engagement,Likes,sentiment_keyword,mscore,`Media URL`)
    # names(IG_Post_Data) = c("DATE", "POST", "KOL", "PLATFORM","REACH","ENGAGEMENT","REACTIONS","SENTIMENT","M-SCORE","URL")
    
    most_recent_posts = rbind(TW_Post_Data,IG_Post_Data)
    
    height = " width=\"52\" height=\"52\"></img>"
    most_recent_posts$picture_str = paste0("\"",most_recent_posts$URL,"\"")
    most_recent_posts$IMAGE = paste("<img class='img-circle' src=",most_recent_posts$picture_str,height)
    most_recent_posts$URL = NULL
    most_recent_posts$picture_str=NULL
    names(most_recent_posts)=c("DATE", "DESCRIPTION", "KOL", "PLATFORM","EARNED EFFECTIVE REACH","ENGAGEMENT","REACTIONS","SENTIMENT","M-SCORE","POST")
    
    most_recent_posts=add_logo(most_recent_posts)
    most_recent_posts$PLATFORM = NULL
    names(most_recent_posts)=c("DATE", "DESCRIPTION", "KOL", "EARNED EFFECTIVE REACH","ENGAGEMENT","REACTIONS","SENTIMENT","M-SCORE","POST","PLATFORM")
    most_recent_posts <- most_recent_posts[,c("DATE", "POST", "DESCRIPTION","KOL", "PLATFORM","EARNED EFFECTIVE REACH","ENGAGEMENT","REACTIONS","SENTIMENT","M-SCORE")]
    
    
    most_recent_posts$DATE=gsub("/", "-", most_recent_posts$DATE)
    most_recent_posts$DATE=dmy(most_recent_posts$DATE)
    most_recent_posts=most_recent_posts[ order(most_recent_posts$DATE , decreasing = TRUE ),]
    most_recent_posts$DATE=gsub("-", "/", most_recent_posts$DATE)
    
    #View(most_recent_posts)
    DT::datatable(most_recent_posts,escape=F, options = list(scrollX = TRUE,pageLength = 6,searching = FALSE,columnDefs = list(list(
      targets = 3,
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 6 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
        "}")
    ))))%>% formatStyle(
      'M-SCORE',
      backgroundColor = "#FFC6DE"
    )%>% formatStyle(
      'SENTIMENT',
      color = styleEqual(c("POSITIVE", "NEGATIVE", "NEUTRAL"), c('#2a9a2a', '#f40939','#FFC200'))
    )%>% formatStyle(
      'ENGAGEMENT', 'SENTIMENT',
      color = styleEqual(c("POSITIVE", "NEGATIVE", "NEUTRAL"), c('#2a9a2a', '#f40939','#FFC200'))
    )%>% formatStyle(
      'REACTIONS', 'SENTIMENT',
      color = styleEqual(c("POSITIVE", "NEGATIVE", "NEUTRAL"), c('#2a9a2a', '#f40939','#FFC200'))
    )%>% formatStyle(
      'M-SCORE', 'SENTIMENT',
      color = styleEqual(c("POSITIVE", "NEGATIVE", "NEUTRAL"), c('#2a9a2a', '#f40939','#FFC200'))
    )
    #, callback = JS('table.page(3).draw(false);')) 
    
  }  )
  
  
  
  
    
  output$top_comments_table <- DT::renderDataTable({
    
    req(input$typeofposts_ht)
    
    if(input$typeofposts_ht!='SK-II Posts')return()
    if (input$typeofposts_ht=='SK-II Posts'){
      data = get_top_comments_table()
      
    }
    # View(data)
    # data=data[complete.cases(data), ]
    DT::datatable(data,escape=F, options = list(
      language = list(
        zeroRecords = "No records to display for the selected filter! Please change the filter options and try again."),
      
      lengthChange = TRUE,pageLength = 10,searching = FALSE))
                                                
  })
  
  
  
  
  reactive_display = reactive ({
    
    
    #v$platform_params <- c("Facebook","Twitter","Instagram")
    v$platform_params <- c("Twitter","Instagram")
    v$display_type <- c("Earned Effective Reach","Engagement","Engagement Rate",'Sentiment','M-Score')
    v$display_typeofposts <-  c("SK-II Posts","Non SK-II Posts")
    v$display_month <-  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    v$historical_trend = "HISTORICAL TREND"
    v$button_clicked = "button1"
    v$title_sk2_post = "TOP POSTS FOR SK-II"
    v$title_top_comments = "TOP COMMENTS"
    v$instagram_EER = NULL
    v$twitter_EER = NULL
    v$instagram_E = NULL
    v$twitter_E = NULL
    
    
  })
  
  get_top_comments_table = reactive({
    
    button_number=as.numeric(unlist(strsplit(v$button_clicked,"button"))[-1])
    top10 = reactive_return_getTop10Infleuncer()
    influencer_name = as.character(top10[button_number,1])
    
    
    
    req(input$platform_ht)
    req(input$month_ht)
    req(input$year_ht)
    
    # print (influencer_name)
    # print(input$platform_ht)
    # print(input$month_ht)
    # print(input$year_ht)
    
    if(input$platform_ht == 'Facebook'){
      FB_Comment_Data  = reactive_FB_Comments_Data()
      FB_Comment_Data = subset(FB_Comment_Data,tolower(FB_Comment_Data$Name) == tolower(influencer_name) )
      FB_Comment_Data=subset(FB_Comment_Data, as.numeric(sapply(strsplit(as.character(FB_Comment_Data$Date), split="/"),tail, n=1)) == input$year_ht )
      FB_Comment_Data = subset(FB_Comment_Data,tolower(month.abb[as.numeric(sapply(strsplit(as.character(FB_Comment_Data$Date), split="/"),'[',2))]) == tolower(input$month_ht) )
      data=select(FB_Comment_Data,Message,Date)
      data = data[rev(order(data$Date)),c('Message')]
      
      
    }
    if(input$platform_ht == 'Twitter'){
      TW_Comment_Data  = reactive_TW_Comments_Data()
      
      TW_Comment_Data = subset(TW_Comment_Data,tolower(TW_Comment_Data$Username) == tolower(influencer_name) )
      TW_Comment_Data=subset(TW_Comment_Data, as.numeric(sapply(strsplit(as.character(TW_Comment_Data$Date), split="/"),tail, n=1)) == input$year_ht )
      TW_Comment_Data = subset(TW_Comment_Data,tolower(month.abb[as.numeric(sapply(strsplit(as.character(TW_Comment_Data$Date), split="/"),'[',2))]) == tolower(input$month_ht) )
      data=select(TW_Comment_Data,Text,Date)
      data = data[rev(order(data$Date)),c('Text')]
    }
    if(input$platform_ht == 'Instagram'){
      IG_Comment_Data  = reactive_IG_Comments_Data()
      
      IG_Comment_Data = subset(IG_Comment_Data,tolower(IG_Comment_Data$Username) == tolower(influencer_name) )
      IG_Comment_Data=subset(IG_Comment_Data, as.numeric(sapply(strsplit(as.character(IG_Comment_Data$Date), split="/"),tail, n=1)) == input$year_ht )
      IG_Comment_Data = subset(IG_Comment_Data,tolower(month.abb[as.numeric(sapply(strsplit(as.character(IG_Comment_Data$Date), split="/"),'[',2))]) == tolower(input$month_ht) )
      data=select(IG_Comment_Data,Date,Message)
      data = data[rev(order(data$Date)),c('Message')]
    }
    
    names(data) = c("Comments")
    return (data)
  })
  
  get_sk2_data = reactive({
    
    req(input$platform_ht)
    req(input$metric_ht)
    req(input$typeofposts_ht)
    req(input$month_ht)
    req(input$year_ht)
    data=NULL
    button_number=as.numeric(unlist(strsplit(v$button_clicked,"button"))[-1])
    top10 = reactive_return_getTop10Infleuncer()
    influencer_name = as.character(top10[button_number,1])
    
    # if(input$platform_ht == 'Facebook'){
    #   FB_Post_Data  = reactive_FB_Post_Data()
    #   FB_Post_Data = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    #   FB_Post_Data = add_mscore(FB_Post_Data,"FB")
    #   
    #   FB_Post_Data = subset(FB_Post_Data,tolower(FB_Post_Data$Name) == tolower(influencer_name) )
    #   if(nrow(FB_Post_Data)!=0){
    #   
    #         FB_Post_Data=subset(FB_Post_Data, as.numeric(sapply(strsplit(as.character(FB_Post_Data$Date), split="/"),tail, n=1)) == input$year_ht )
    #         if(nrow(FB_Post_Data)!=0){
    #             FB_Post_Data = subset(FB_Post_Data,tolower(month.abb[as.numeric(sapply(strsplit(as.character(FB_Post_Data$Date), split="/"),'[',2))]) == tolower(input$month_ht) )
    #             if(nrow(FB_Post_Data)!=0){
    #               data=select(FB_Post_Data, Date, post_text, post_impressions_unique, Engagement,`Engagement Rate`,sentiment,mscore,post_image)            
    #               names(data)=c('Date','Description','Reach','Engagement','Engagement Rate','Sentiment','M-SCORE',"URL")
    #               data=data[rev(order(data$Reach)),c('Date','Description','Reach','Engagement','Engagement Rate','Sentiment','M-SCORE','URL')]
    #             }
    #         }
    #     }
    # }
    
    if(input$platform_ht == 'Twitter'){
      TW_Post_Data  = reactive_TW_Post_Data()
      TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
      TW_Post_Data = add_mscore(TW_Post_Data,"TW")
      TW_Page_Data = reactive_TW_Page_Data()
      TW_Post_Data = add_EER(TW_Page_Data,TW_Post_Data,"TW")
      #View(TW_Post_Data)
      TW_Post_Data = subset(TW_Post_Data,TW_Post_Data$Username == influencer_name )
      # print (nrow(TW_Post_Data))
      if(nrow(TW_Post_Data)!=0){
            TW_Post_Data=subset(TW_Post_Data, as.numeric(sapply(strsplit(as.character(TW_Post_Data$Date), split="/"),tail, n=1)) == input$year_ht )
            # print (nrow(TW_Post_Data))
            if(nrow(TW_Post_Data)!=0){
              TW_Post_Data = subset(TW_Post_Data,tolower(month.abb[as.numeric(sapply(strsplit(as.character(TW_Post_Data$Date), split="/"),'[',2))]) == tolower(input$month_ht) )
              # print ("HERE")
              # print (nrow(TW_Post_Data))
              if(nrow(TW_Post_Data)!=0){
                # View(TW_Post_Data)
                #TW_Post_Data$Engagement = TW_Post_Data$Retweet.Count + TW_Post_Data$Retweet.Count + TW_Post_Data$Favorite.Count
                TW_Post_Data$Engagement=  TW_Post_Data["Reply Count"] + TW_Post_Data["Favorite Count"] + TW_Post_Data["Retweet Count"]
                TW_Post_Data = as.data.frame(TW_Post_Data)
                TW_Post_Data$Engagement=TW_Post_Data$Engagement$`Reply Count`
                #TW_Post_Data$Reach=0
                # print (length(TW_Post_Data))
                data=select(TW_Post_Data,Date,Text,Earned_Effective_Reach,Engagement,`Engagement Rate`,sentiment_keyword,mscore,`Post Image`)
                names(data)=c('Date','Description','Earned Effective Reach','Engagement','Engagement Rate','Sentiment','M-SCORE','URL')
                data=data[rev(order(data$Engagement)),c('Date','Description','Earned Effective Reach','Engagement','Engagement Rate','Sentiment','M-SCORE','URL')]
                # print(nrow(data))
              }
            }
         }    
    }
    
    else if(input$platform_ht == 'Instagram'){
      IG_Post_Data  = reactive_IG_Post_Data()
      IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
      IG_Post_Data = add_mscore(IG_Post_Data,"IG")
      IG_Page_Data = reactive_IG_Page_Data()
      IG_Post_Data = add_EER(IG_Page_Data,IG_Post_Data,"IG")
      
      IG_Post_Data = subset(IG_Post_Data,IG_Post_Data$Username == influencer_name )
      if(nrow(IG_Post_Data)!=0){
            IG_Post_Data=subset(IG_Post_Data, as.numeric(sapply(strsplit(as.character(IG_Post_Data$Date), split="/"),tail, n=1)) == input$year_ht )
            if(nrow(IG_Post_Data)!=0){
              IG_Post_Data = subset(IG_Post_Data,tolower(month.abb[as.numeric(sapply(strsplit(as.character(IG_Post_Data$Date), split="/"),'[',2))]) == tolower(input$month_ht) )
              if(nrow(IG_Post_Data)!=0){
              data=select(IG_Post_Data,Date,Caption,Earned_Effective_Reach,Engagement,`Engagement Rate`,sentiment_keyword,mscore,`Media URL`)
             
              names(data)=c('Date','Description','Earned Effective Reach','Engagement','Engagement Rate','Sentiment','M-SCORE','URL')
              
              data=data[rev(order(data$Engagement)),c('Date','Description','Earned Effective Reach','Engagement','Engagement Rate','Sentiment','M-SCORE','URL')]
              }
            }
      }
    }
    # print ("nrow(data)")
    # print (is.null(data))
    
    if(is.null(data)== FALSE  ){
      height = " height=\"52\" height=\"52\"></img>"
      data$picture_str = paste0("\"",data$URL,"\"")
      data$post = paste("<img class='img-circle' src=",data$picture_str,height)
      data$URL = NULL
      data$picture_str=NULL
      #print(paste0("<img src=",image,height))
      names(data)=c('DATE','DESCRIPTION','EARNED EFFECTIVE REACH','ENGAGEMENT','ENGAGEMENT RATE','SENTIMENT','M-SCORE','POST')
      data <- data[,c("DATE", "POST", "DESCRIPTION",'EARNED EFFECTIVE REACH','ENGAGEMENT','ENGAGEMENT RATE','SENTIMENT','M-SCORE')]
    }else{
      data=NULL
      data <- data.frame(matrix(ncol = 8, nrow = 0))
      x <- c("DATE", "POST", "DESCRIPTION",'EARNED EFFECTIVE REACH','ENGAGEMENT','ENGAGEMENT RATE','SENTIMENT','M-SCORE')
      colnames(data) <- x
      #View(data)
     
    }
    return (data)
    
  })
    
  
  
  
  ##########################
  
  #this is used to copy the uploaded input files to the data folder 'data' in real time
  observe({
    if (is.null(input$input_file_upload)) return()
    file.copy(input$input_file_upload$datapath, paste0("data/", input$input_file_upload$name))
  })
  
  v <- reactiveValues(data = NULL)
  
  
  
  observeEvent(input$button_view_file, {
    v$file_names <- list.files(path = "data/")
   
  })
  
  output$test <- renderText({ 
    if (is.null(v$file_names)) return()
    #v$file_names
    v$file_names
    
    
  })
  
  ######################### OUTPUT METHODS #######################
  
  output$abc_test <- renderText({ 
    
           
           paste0("Seq:", "d$seq", "<br>",
                  "Value:", "d$value", "<br>",
                  "Company:", "d$name")
    
  })
 
  #influencer screen
  output$influencer_name1 <- renderText({
    
    top10 = reactive_return_getTop10Infleuncer()
    Sys.setlocale(category = "LC_CTYPE", locale = "chs")
    
    as.character(top10[1,1])
    #"PUT THIS ON THE SCREEN"
    
    })
  output$influencer_name2 <- renderText({
    top10 = reactive_return_getTop10Infleuncer()
    as.character(top10[2,1])
    #"PUT THIS ON THE SCREEN"
  })
  
  output$influencer_name3 <- renderText({
    top10 = reactive_return_getTop10Infleuncer()
    as.character(top10[3,1])
    #"PUT THIS ON THE SCREEN"
  })
  output$influencer_name4 <- renderText({
    top10 = reactive_return_getTop10Infleuncer()
    as.character(top10[4,1])
    #"PUT THIS ON THE SCREEN"
  })
  
  output$influencer_name5 <- renderText({
    top10 = reactive_return_getTop10Infleuncer()
    as.character(top10[5,1])
    #"PUT THIS ON THE SCREEN"
  })
  
  output$influencer_name6 <- renderText({
    top10 = reactive_return_getTop10Infleuncer()
    as.character(top10[6,1])
    #"PUT THIS ON THE SCREEN"
  })
  
  output$influencer_name7 <- renderText({
    top10 = reactive_return_getTop10Infleuncer()
    as.character(top10[7,1])
    #"PUT THIS ON THE SCREEN"
  })
  
  output$influencer_name8 <- renderText({
    top10 = reactive_return_getTop10Infleuncer()
    as.character(top10[8,1])
    #"PUT THIS ON THE SCREEN"
  })
  output$influencer_name9 <- renderText({
    top10 = reactive_return_getTop10Infleuncer()
    as.character(top10[9,1])
    #"PUT THIS ON THE SCREEN"
  })
  output$influencer_name10 <- renderText({
    top10 = reactive_return_getTop10Infleuncer()
    as.character(top10[10,1])
    #"PUT THIS ON THE SCREEN"
  })
  
  output$twitter_details <- renderUI({
    
    if (is.null(v$button_clicked)) return()
    if (v$button_clicked == "button1"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[1,1])
    }
    if (v$button_clicked == "button2"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[2,1])
    }
    if (v$button_clicked == "button3"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[3,1])
    }
    if (v$button_clicked == "button4"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[4,1])
    }
    if (v$button_clicked == "button5"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[5,1])
    }
    if (v$button_clicked == "button6"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[6,1])
    }
    if (v$button_clicked == "button7"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[7,1])
    }
    if (v$button_clicked == "button8"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[8,1])
    }
    if (v$button_clicked == "button9"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[9,1])
    }
    if (v$button_clicked == "button10"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[10,1])
    }
    
    
    TW_Page_Data = reactive_TW_Page_Data()
    TW_Page_Data = subset(TW_Page_Data,TW_Page_Data$Username == influencer_name)
    
    if (nrow(TW_Page_Data)!=0){
      TW_Page_Data$Date=gsub("/", "-", TW_Page_Data$Date)
      TW_Page_Data$Date=dmy(TW_Page_Data$Date)
      TW_Page_Data=TW_Page_Data[ order(TW_Page_Data$Date , decreasing = TRUE ),]
      
      followers_TW = TW_Page_Data[1,"Followers"]
      
      
    }else{
      followers_TW=0
    }
    
    
    
    TW_Post_Data = reactive_TW_Post_Data()
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    TW_Post_Data$Engagement=  TW_Post_Data["Reply Count"] + TW_Post_Data["Favorite Count"] + TW_Post_Data["Retweet Count"]
    TW_Post_Data = as.data.frame(TW_Post_Data)
    TW_Post_Data$Engagement=TW_Post_Data$Engagement$`Reply Count`
    TW_Post_Data$Engagement_Rate = round(TW_Post_Data$Engagement/ TW_Post_Data$Followers,2)
    TW_Post_Data = subset(TW_Post_Data,TW_Post_Data$Username == influencer_name)
    if (nrow(TW_Post_Data)!=0){
      TW_Post_Data$Date=gsub("/", "-", TW_Post_Data$Date)
      TW_Post_Data$Date=dmy(TW_Post_Data$Date)
      TW_Post_Data=TW_Post_Data[ order(TW_Post_Data$Date , decreasing = TRUE ),]
      avg_ER_TW = TW_Post_Data[1,"Engagement_Rate"]
      
    }else{
      avg_ER_TW = 0
    }
    
    
    
    followers_TW = format(round(followers_TW/1000000, 2))
    followers_TW = paste(followers_TW,"M")
    
    followers_TW = paste("FOLLOWERS:",followers_TW)
    avg_ER_TW = paste("AVG ER:",avg_ER_TW,"%",sep="" )
    HTML (paste(followers_TW,avg_ER_TW, sep="<br/>"))
    
  })
  
  output$instagram_details <- renderUI({
    
    if (is.null(v$button_clicked)) return()
    if (v$button_clicked == "button1"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[1,1])
    }
    if (v$button_clicked == "button2"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[2,1])
    }
    if (v$button_clicked == "button3"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[3,1])
    }
    if (v$button_clicked == "button4"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[4,1])
    }
    if (v$button_clicked == "button5"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[5,1])
    }
    if (v$button_clicked == "button6"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[6,1])
    }
    if (v$button_clicked == "button7"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[7,1])
    }
    if (v$button_clicked == "button8"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[8,1])
    }
    if (v$button_clicked == "button9"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[9,1])
    }
    if (v$button_clicked == "button10"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[10,1])
    }
    IG_Page_Data = reactive_IG_Page_Data()
    IG_Page_Data = subset(IG_Page_Data,IG_Page_Data$Username == influencer_name)
    if (nrow(IG_Page_Data)!=0){
      IG_Page_Data$Date=gsub("/", "-", IG_Page_Data$Date)
      IG_Page_Data$Date=dmy(IG_Page_Data$Date)
      IG_Page_Data=IG_Page_Data[ order(IG_Page_Data$Date , decreasing = TRUE ),]
      followers_ig = IG_Page_Data[1,4]
      followers_ig = as.data.frame(followers_ig)
      followers_ig  = followers_ig$Followers
      
    }else{
      followers_ig=0
    }
    
   
    
    
    followers_ig = format(round(followers_ig/1000000, 2))
    followers_ig = paste(followers_ig,"M")
    
    followers_ig = paste("FOLLOWERS:",followers_ig)
    
    IG_Summary_Data =  reactive_IG_Summary_Data()
    if (nrow(IG_Summary_Data[IG_Summary_Data$Username==influencer_name,]) !=0){
      IG_Summary_Data = subset(IG_Summary_Data,IG_Summary_Data$Username == influencer_name)
      avg_ER_IG = IG_Summary_Data$`ER Rate`
    }else{
      avg_ER_IG = 0
    }
    avg_ER_IG = format(round(as.numeric(avg_ER_IG),  2))
    avg_ER_IG = paste("AVG ER:",avg_ER_IG,"%",sep="" )
    
    HTML (paste(followers_ig,avg_ER_IG, sep="<br/>"))
    
  })
  
  output$facebook_details <- renderUI({
    if (is.null(v$button_clicked)) return()
    if (v$button_clicked == "button1"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[1,1])
    }
    if (v$button_clicked == "button2"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[2,1])
    }
    if (v$button_clicked == "button3"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[3,1])
    }
    if (v$button_clicked == "button4"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[4,1])
    }
    if (v$button_clicked == "button5"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[5,1])
    }
    if (v$button_clicked == "button6"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[6,1])
    }
    if (v$button_clicked == "button7"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[7,1])
    }
    if (v$button_clicked == "button8"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[8,1])
    }
    if (v$button_clicked == "button9"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[9,1])
    }
    if (v$button_clicked == "button10"){
      
      top10 = reactive_return_getTop10Infleuncer()
      influencer_name=as.character(top10[10,1])
    }
      
      FB_Summary_Stats_Data =  reactive_FB_Summary_Stats()
      if (nrow(FB_Summary_Stats_Data[FB_Summary_Stats_Data$Name==influencer_name,]) !=0){
        FB_Summary_Stats_Data = subset(FB_Summary_Stats_Data,FB_Summary_Stats_Data$Name == influencer_name)
        avg_ER_FB = FB_Summary_Stats_Data$`Average Page ER`
      }else{
        avg_ER_FB = 0
      }
      
      FB_Page_Data = reactive_FB_Page_Data()
      FB_Page_Data = subset(FB_Page_Data,FB_Page_Data$Name == influencer_name)
      if (nrow(FB_Page_Data)!=0){
        FB_Page_Data$Date=gsub("/", "-", FB_Page_Data$Date)
        FB_Page_Data$Date=dmy(FB_Page_Data$Date)
        FB_Page_Data=FB_Page_Data[ order(FB_Page_Data$Date , decreasing = TRUE ),]
        followers_fb = FB_Page_Data[1,4]
        
      }else{
        followers_fb=0
      }
      
      
      followers_fb = format(round(followers_fb/1000000, 2))
      followers_fb = paste(followers_fb,"M")
      
      followers_fb_text = paste("FOLLOWERS:",followers_fb)
      avg_ER_FB_text = paste("AVG ER:",avg_ER_FB,"%",sep="" )
      
      HTML (paste(followers_fb_text,avg_ER_FB_text, sep="<br/>"))
    
    
  })
  

  

  output$selected_influencer_details <- renderUI({
    
    if (is.null(v$button_clicked)) return()
    
    if (v$button_clicked == "button1"){
      top10 = reactive_return_getTop10Infleuncer()
      name=as.character(top10[1,"Infleuncer"])
      score=as.character(top10[1,"MSCORE"])
    }
    else if(v$button_clicked == "button2"){
      top10 = reactive_return_getTop10Infleuncer()
      name=as.character(top10[2,"Infleuncer"])
      score=as.character(top10[2,"MSCORE"])
    }
    else if(v$button_clicked == "button3"){
      top10 = reactive_return_getTop10Infleuncer()
      name=as.character(top10[3,"Infleuncer"])
      score=as.character(top10[3,"MSCORE"])
    }
    else if(v$button_clicked == "button4"){
      top10 = reactive_return_getTop10Infleuncer()
      name=as.character(top10[4,"Infleuncer"])
      score=as.character(top10[4,"MSCORE"])
    }
    else if(v$button_clicked == "button5"){
      top10 = reactive_return_getTop10Infleuncer()
      name=as.character(top10[5,"Infleuncer"])
      score=as.character(top10[5,"MSCORE"])
    }
    else if(v$button_clicked == "button6"){
      top10 = reactive_return_getTop10Infleuncer()
      name=as.character(top10[6,"Infleuncer"])
      score=as.character(top10[6,"MSCORE"])
    }
    else if(v$button_clicked == "button7"){
      top10 = reactive_return_getTop10Infleuncer()
      name=as.character(top10[7,"Infleuncer"])
      score=as.character(top10[7,"MSCORE"])
    }
    else if(v$button_clicked == "button8"){
      top10 = reactive_return_getTop10Infleuncer()
      name=as.character(top10[8,"Infleuncer"])
      score=as.character(top10[8,"MSCORE"])
    }
    else if(v$button_clicked == "button9"){
      top10 = reactive_return_getTop10Infleuncer()
      name=as.character(top10[9,"Infleuncer"])
      score=as.character(top10[9,"MSCORE"])
    }
    else if(v$button_clicked == "button10"){
      top10 = reactive_return_getTop10Infleuncer()
      name=as.character(top10[10,"Infleuncer"])
      score=as.character(top10[10,"MSCORE"])
      
      
    }
    
    FB_Post_Data  = reactive_FB_Post_Data()
    TW_Post_Data  = reactive_TW_Post_Data()
    IG_Post_Data  = reactive_IG_Post_Data()
    total_post_count = nrow(FB_Post_Data)+nrow(TW_Post_Data)+nrow(IG_Post_Data)
    
    FB_sub = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    FB_sub = subset(FB_sub,FB_sub$Name == name )
    TW_sub = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    TW_sub = subset(TW_sub,TW_sub$Username == name )
    IG_sub = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    IG_sub = subset(IG_sub,IG_sub$Username == name )
    SK2_post_count = nrow(FB_sub)+nrow(TW_sub)+nrow(IG_sub)  
    
    score=paste("MSCORE-", score)
    percentage = format(round(SK2_post_count*100/ total_post_count , 2))
    percentage_text=paste("SK-II POSTS-", percentage, "%",sep="")
    
    name = paste("<b>",name,"</b>")
    score = paste("<b>",score,"</b>")
    percentage_text = paste("<b>",percentage_text,"</b>")
    HTML (paste(name, score,percentage_text, sep="<br/>"))
    
    
    
  })
  
  
  output$influencer_mscore1 <-renderText({
    
    top10 = reactive_return_getTop10Infleuncer()
    #top10= select(top10, Infleuncer, MSCORE)
    #df=aggregate(MSCORE ~ Infleuncer, data=data, FUN=mean)
    
    score=as.character(top10[1,"MSCORE"])
    paste("MSCORE-", score)
    
    
  })
  output$influencer_mscore2 <-renderText({
    
    top10 = reactive_return_getTop10Infleuncer()
  score=as.character(top10[2,"MSCORE"])
    paste("MSCORE-", score)
    
  })
  output$influencer_mscore3 <-renderText({
    
    top10 = reactive_return_getTop10Infleuncer()
    
    score=as.character(top10[3,"MSCORE"])
    paste("MSCORE-", score)
    
  })
  output$influencer_mscore4 <-renderText({
    
    top10 = reactive_return_getTop10Infleuncer()
    
    score=as.character(top10[4,"MSCORE"])
    paste("MSCORE-", score)
    
  })
  output$influencer_mscore4 <-renderText({
    
    top10 = reactive_return_getTop10Infleuncer()
    
    score=as.character(top10[4,"MSCORE"])
    paste("MSCORE-", score)
    
  })
  output$influencer_mscore5 <-renderText({
    
    top10 = reactive_return_getTop10Infleuncer()
    
    score=as.character(top10[5,"MSCORE"])
    paste("MSCORE-", score)
    
  })
  output$influencer_mscore6 <-renderText({
    
    top10 = reactive_return_getTop10Infleuncer()
    
    score=as.character(top10[6,"MSCORE"])
    paste("MSCORE-", score)
    
  })
  output$influencer_mscore7 <-renderText({
    
    top10 = reactive_return_getTop10Infleuncer()
    
    score=as.character(top10[7,"MSCORE"])
    paste("MSCORE-", score)
    
  })
  output$influencer_mscore8 <-renderText({
    
    top10 = reactive_return_getTop10Infleuncer()
    
    score=as.character(top10[8,"MSCORE"])
    paste("MSCORE-", score)
    
  })
  output$influencer_mscore9 <-renderText({
    
    top10 = reactive_return_getTop10Infleuncer()
    
    score=as.character(top10[9,"MSCORE"])
    paste("MSCORE-", score)
    
  })
  output$influencer_mscore10 <-renderText({
    
    top10 = reactive_return_getTop10Infleuncer()
    
    score=as.character(top10[10,"MSCORE"])
    paste("MSCORE-", score)
    
  })
  
  output$display_total_engagement <- renderText({ 
    TW_Post_Data  = reactive_TW_Post_Data()
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    IG_Post_Data  = reactive_IG_Post_Data()
    IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    engagmement_TW=sum(sum(TW_Post_Data$`Reply Count`),sum(TW_Post_Data$`Favorite Count`),sum(TW_Post_Data$`Retweet Count`))
    v$twitter_E= format(round(engagmement_TW/1000000, 2))
    engagmement_IG=sum(IG_Post_Data$Engagement)
    v$instagram_E = format(round(engagmement_IG/1000000, 2))
    total_engagement = engagmement_TW+engagmement_IG
    total_engagement = format(round(total_engagement/1000000, 2))
    paste(total_engagement,"MM")
    
  })
  
  output$tooltip_EER <- renderText({ 
    
    a="TOOLTIP of suteja"
    a
  })
  
  output$description_text <- renderUI({ 
    filtered_posts_df = filter_posts()
    text = as.character(filtered_posts_df[1,"DESCRIPTION"])
    HTML (text)
  })
  output$name <- renderUI({ 
    filtered_posts_df = filter_posts()
    influencer_name = as.character(filtered_posts_df[1,"KOL"])
    HTML(paste('<center>',influencer_name,'</center>'))
  })

  output$EER_test <- renderUI({ 
    filtered_posts_df = filter_posts()
    reach = as.character(filtered_posts_df[1,"EARNED EFFECTIVE REACH"])
    # HTML(paste('REACH:&nbsp; &emsp;',reach)),
    #HTML(paste('REACH:',reach))
    fluidRow( 
      column(5,HTML("REACH:")),
      column(3,HTML(reach))
    )
    
  })
  
  output$Engagement_test <- renderUI({ 
    filtered_posts_df = filter_posts()
    ENGAGEMENT = as.character(filtered_posts_df[1,"ENGAGEMENT"])
    #HTML(paste('ENGAGEMENT:&nbsp; &emsp;',ENGAGEMENT))
    fluidRow( 
      column(5,HTML("ENGAGEMENT:")),
      column(3,HTML(ENGAGEMENT))
    )
  })
  
  output$Engagement_rate_test <- renderUI({ 
    filtered_posts_df = filter_posts()
    ENGAGEMENT = as.character(filtered_posts_df[1,"ENGAGEMENT RATE"])
    #HTML(paste('ENGAGEMENT RATE:&nbsp; &emsp;',ENGAGEMENT))
    fluidRow( 
      column(5,HTML("ENGAGEMENT RATE:")),
      column(3,HTML(ENGAGEMENT))
    )
  })
  output$comments_test <- renderUI({ 
    filtered_posts_df = filter_posts()
    COMMENTS = as.character(filtered_posts_df[1,"COMMENTS"])
    # HTML(paste('COMMENTS:&nbsp; &emsp;',COMMENTS))
    fluidRow( 
      column(5,HTML("COMMENTS:")),
      column(3,HTML(COMMENTS))
    )
    
  })
  output$mscore_test <- renderUI({ 
    filtered_posts_df = filter_posts()
    SCORE = as.character(filtered_posts_df[1,"M-SCORE"])
    # HTML(paste('M-SCORE:&nbsp; &emsp;',SCORE))
   
    fluidRow( 
      # column(5,HTML(paste0("<div style='background-color:#FFC6DE","'>","M-SCORE:"))),
      
      column(5,HTML("M-SCORE:")),
      column(3,HTML(paste0("<div style='background-color:#FFC6DE","'>",SCORE)))
      #column(3,HTML(SCORE))
    )
  })
  
  output$Reactions_test <- renderUI({ 
    filtered_posts_df = filter_posts()
    REACTIONS = as.character(filtered_posts_df[1,"REACTIONS"])
    # HTML(paste('REACTIONS     :&nbsp; &emsp;',REACTIONS))
    fluidRow( 
      column(5,HTML("REACTIONS:")),
      column(3,HTML(REACTIONS))
    )
  })
  
  output$display_total_reach <- renderText({ 
    
    #INSTAGRAM
    IG_Page_Data = reactive_IG_Page_Data()
    IG_Post_Data = reactive_IG_Post_Data() 
      
    IG_Page_Data["Earned_Effective_Reach"] = 0.035 * IG_Page_Data["Followers"] 
    IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    
    IG_Post_Data_image_df = IG_Post_Data[grepl("image|carousel", IG_Post_Data$Type)==TRUE,]
    IG_Post_Data_image_df=merge(IG_Post_Data_image_df,IG_Page_Data, by  = c("Username","Date"))
    colnames(IG_Post_Data_image_df)[3] <- "Day"
    IG_Post_Data_image_df = IG_Post_Data_image_df[,c("Username","Date","Profile Image","Media URL","Earned_Effective_Reach" )]
    colnames(IG_Post_Data_image_df)[4] <- "Post Image"
    
    IG_Post_Data_video_df = IG_Post_Data[(grepl("video", IG_Post_Data$Type)==TRUE),]
    IG_Post_Data_video_df["Earned_Effective_Reach"] = 0.25 * IG_Post_Data_video_df["Views"] 
    IG_Post_Data_video_df = IG_Post_Data_video_df[,c("Username","Date","Profile Image","Media URL","Earned_Effective_Reach" )]
    
    IG_Post_Data_video_df=IG_Post_Data_video_df[, c("Username","Date","Profile Image","Media URL","Earned_Effective_Reach" )]
    colnames(IG_Post_Data_video_df)[4] <- "Post Image"
    
    IG_Post_Data_sk2 = rbind(IG_Post_Data_image_df,IG_Post_Data_video_df)
    IG_EER=sum(IG_Post_Data_sk2$Earned_Effective_Reach)
    v$instagram_EER = format(round(IG_EER/1000000, 2))
    
    #TWITTER
    TW_Page_Data = reactive_TW_Page_Data()
    TW_Post_Data = reactive_TW_Post_Data()
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    
    TW_Page_Data["Earned_Effective_Reach"] = 0.054 * TW_Page_Data["Followers"] 
    
    TW_Post_Data_image_df = TW_Post_Data[grepl(paste(c("Link","Photo","Text"), collapse = "|"), TW_Post_Data$Type)==TRUE,]
    
    TW_Post_Data_image_df=merge(TW_Post_Data_image_df,TW_Page_Data, by  = c("Username","Date"))
    TW_Post_Data_image_df=TW_Post_Data_image_df[, c("Username","Date","Profile Image","Post Image","Earned_Effective_Reach" )]
    
    TW_EER=sum(TW_Post_Data_image_df$Earned_Effective_Reach)
    v$twitter_EER = format(round(TW_EER/1000000, 2))
    
    total_EER = sum(TW_EER+IG_EER)
    
    
    reach = format(round(total_EER/1000000, 2))
    paste(reach,"MM")
    
  })
  
  output$top_platform <- renderText({
    list_details=reactive_top_influencer_image_url()
    platform = as.character(list_details[3])
    platform= paste0("**Top post and top influencer details are coming from the platform ",platform)
    platform
  })
  
  output$top_post_image <- renderUI({
    image_list=reactive_top_influencer_image_url()
    post_image = as.character(image_list[2])
    #tags$img(src=post_image,width = "150",height = "140")
    #HTML(paste('<center><img src=',post_image, ' width = "150",height = "140"></center>'))
    HTML(paste('<center><img class="img-circle" src=',post_image, ' width = "150",height = "140"></center>'))
    
  })
  
  
  output$top_infleuncer_image <- renderUI({
    image_list=reactive_top_influencer_image_url()
    profile_image = as.character(image_list[1])
    #tags$img(src=profile_image,width = "150",height = "140",style="img.center")
    #HTML(paste('<center><img src=',profile_image, ' width = "150",height = "140"></center>'))
    HTML(paste('<center><img class="img-circle" src=',profile_image, ' width = "150",height = "140"></center>'))
    
  })
  
  output$image_test <- renderUI({
    filtered_posts_df = filter_posts()
    type  =  filtered_posts_df[1,"TYPE"]
    time  =  filtered_posts_df[1,"TIME"]
    platform_image = as.character(filtered_posts_df[1,"PLATFORM"])
    date =  filtered_posts_df[1,"DATE"]
    HTML(paste(platform_image, type,'on ',date,'at ',time))
         #paste(type,'on ',date,'at ',time))
  })
  output$top_infleuncer_image_test <- renderUI({
    image_list=reactive_top_influencer_image_url()
    profile_image = as.character(image_list[1])
    filtered_posts_df = filter_posts()
    influencer_name = as.character(filtered_posts_df[1,"KOL"])
    #HTML (text)	
    #tags$img(src=profile_image,width = "150",height = "140",style="img.center")
    HTML(paste('<center><img src=',profile_image, ' width = "150",height = "140"></center>'))
    #HTML(paste('Influencer Name: ',influencer_name))
  })
  
  output$infleuncerImage1 <- renderUI({
    top10 = reactive_return_getTop10Infleuncer()
    url=as.character(top10[1,"PROFILE_URL"])
    #tags$img(src=url,width = "150",height = "120")
    HTML(paste('<center><img src=',url, ' width = "150",height = "120"></center>'))
  })

  output$infleuncerImage2 <- renderUI({
    top10 = reactive_return_getTop10Infleuncer()
    url=as.character(top10[2,"PROFILE_URL"])
    #tags$img(src=url,width = "150",height = "120")
    HTML(paste('<center><img src=',url, ' width = "150",height = "120"></center>'))
    
  })
  output$infleuncerImage3 <- renderUI({
    top10 = reactive_return_getTop10Infleuncer()
    #View(top10)
    url=as.character(top10[3,"PROFILE_URL"])
    print (url)
    # tags$img(src=url,width = "150",height = "120")
    HTML(paste('<center><img src=',url, ' width = "150",height = "120"></center>'))
    
  })
  output$infleuncerImage4 <- renderUI({
    top10 = reactive_return_getTop10Infleuncer()
    url=as.character(top10[4,"PROFILE_URL"])
    # tags$img(src=url,width = "150",height = "120")
    HTML(paste('<center><img src=',url, ' width = "150",height = "120"></center>'))
    
  })
  output$infleuncerImage5 <- renderUI({
    top10 = reactive_return_getTop10Infleuncer()
    url=as.character(top10[5,"PROFILE_URL"])
    # tags$img(src=url,width = "150",height = "120")
    HTML(paste('<center><img src=',url, ' width = "150",height = "120"></center>'))
    
  })
  output$infleuncerImage6 <- renderUI({
    top10 = reactive_return_getTop10Infleuncer()
    url=as.character(top10[6,"PROFILE_URL"])
    # tags$img(src=url,width = "150",height = "120")
    HTML(paste('<center><img src=',url, ' width = "150",height = "120"></center>'))
    
  })
  
  output$infleuncerImage7 <- renderUI({
    top10 = reactive_return_getTop10Infleuncer()
    url=as.character(top10[7,"PROFILE_URL"])
    # tags$img(src=url,width = "150",height = "120")
    HTML(paste('<center><img src=',url, ' width = "150",height = "120"></center>'))
    
  })
  output$infleuncerImage8 <- renderUI({
    top10 = reactive_return_getTop10Infleuncer()
    url=as.character(top10[8,"PROFILE_URL"])
    # tags$img(src=url,width = "150",height = "120")
    HTML(paste('<center><img src=',url, ' width = "150",height = "120"></center>'))
    
  })
  output$infleuncerImage9 <- renderUI({
    top10 = reactive_return_getTop10Infleuncer()
    url=as.character(top10[9,"PROFILE_URL"])
    # tags$img(src=url,width = "150",height = "120")
    HTML(paste('<center><img src=',url, ' width = "150",height = "120"></center>'))
    
  })
  output$infleuncerImage10 <- renderUI({
    top10 = reactive_return_getTop10Infleuncer()
    url=as.character(top10[10,"PROFILE_URL"])
    # tags$img(src=url,width = "150",height = "120")
    HTML(paste('<center><img src=',url, ' width = "150",height = "120"></center>'))
    
  })
  
  output$facebook_image <- renderUI({
    if (is.null(v$button_clicked)) return()
    tags$img(src="facebook.png",width = "40",height = "40")
    
  })
  output$twitter_image <- renderUI({
    if (is.null(v$button_clicked)) return()
    tags$img(src="twitter.png",width = "40",height = "40")
    
  })
  output$instagram_image <- renderUI({
    if (is.null(v$button_clicked)) return()
    tags$img(src="instagram.png",width = "40",height = "40")
    
  })
  output$selected_influencer_image <- renderUI({
    # top10 = reactive_return_getTop10Infleuncer()
    # url=as.character(top10[10,"URL"])
    if (is.null(v$button_clicked)) return()
    
    if (v$button_clicked == "button1"){
      top10 = reactive_return_getTop10Infleuncer()
      url=as.character(top10[1,"PROFILE_URL"])
      
    }
    else if(v$button_clicked == "button2"){
      top10 = reactive_return_getTop10Infleuncer()
      url=as.character(top10[2,"PROFILE_URL"])
      # print(url)
    }
    else if(v$button_clicked == "button3"){
      top10 = reactive_return_getTop10Infleuncer()
      url=as.character(top10[3,"PROFILE_URL"])
    }
    else if(v$button_clicked == "button4"){
      top10 = reactive_return_getTop10Infleuncer()
      url=as.character(top10[4,"PROFILE_URL"])
    }
    else if(v$button_clicked == "button5"){
      top10 = reactive_return_getTop10Infleuncer()
      url=as.character(top10[5,"PROFILE_URL"])
    }
    else if(v$button_clicked == "button6"){
      top10 = reactive_return_getTop10Infleuncer()
      url=as.character(top10[6,"PROFILE_URL"])
    }
    else if(v$button_clicked == "button7"){
      top10 = reactive_return_getTop10Infleuncer()
      url=as.character(top10[7,"PROFILE_URL"])
    }
    else if(v$button_clicked == "button8"){
      top10 = reactive_return_getTop10Infleuncer()
      url=as.character(top10[8,"PROFILE_URL"])
    }
    else if(v$button_clicked == "button9"){
      top10 = reactive_return_getTop10Infleuncer()
      url=as.character(top10[9,"PROFILE_URL"])
    }
    else if(v$button_clicked == "button10"){
      top10 = reactive_return_getTop10Infleuncer()
      url=as.character(top10[10,"PROFILE_URL"])
      
    }
    tags$img(src=url,width = "150",height = "120")
    
  })
  
  
  output$image_blah <- renderImage({
    return(list(
      src = "logo.jpg",
      contentType = "image/jpg",
      height = 185,
      alt = "Face"
    ))
  })
 
  
  output$barplot_markettrend <- renderPlotly({
    
    require(input$Week)
    require(input$Year)
    require(input$metric_mt)
    
    print("method barplot")
    flag=0
    
    if (is.null(reactive_return_week())) { return() }
    
    start_date = as.Date(unlist(strsplit((reactive_return_week()),"to"))[1]) 
    end_date = as.Date(unlist(strsplit((reactive_return_week()),"to"))[2])
    
    # FB_Post_Data = reactive_FB_Post_Data()
    # FB_Post_Data = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    # FB_Post_Data$year=as.character(lapply(strsplit(as.character(FB_Post_Data$Date), split="/"),tail, n=1))
    # FB_Post_Data=FB_Post_Data[FB_Post_Data[["year"]] == reactive_return_year(),]
    
    
    TW_Post_Data = reactive_TW_Post_Data()
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    TW_Post_Data$year=as.character(lapply(strsplit(as.character(TW_Post_Data$Date), split="/"),tail, n=1))
    TW_Post_Data=TW_Post_Data[TW_Post_Data[["year"]] == reactive_return_year(),]
    
    IG_Post_Data = reactive_IG_Post_Data()
    IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    IG_Post_Data$year=as.character(lapply(strsplit(as.character(IG_Post_Data$Date), split="/"),tail, n=1))
    IG_Post_Data=IG_Post_Data[IG_Post_Data[["year"]] == reactive_return_year(),]
    
    
    if (reactive_return_metric() == "Earned Effective Reach") { 
      
     
      # 
      # data=get_markettrend_data(FB_Post_Data,start_date,end_date)
      # data=select(data,Market,post_impressions_unique)
      
      
      data = reactive_get_weekly_trend()
      if (nrow(data)==0){  
        data=data.frame(matrix(ncol = 2, nrow = 0))
        names(data) = c("Market","Values")
        p <- plot_ly(
          x = data$Market,
          y = data$Values,
          type = "bar"
        ) 
        
        return (p)  
        
        }
      data = filter_by_date(data,start_date,end_date)
      if (nrow(data)==0){  
        data=data.frame(matrix(ncol = 2, nrow = 0))
        names(data) = c("Market","Values")
        p <- plot_ly(
          x = data$Market,
          y = data$Values,
          type = "bar"
        ) 
        
        return(p)
      
        }
      data=select(data,"Market","Earned_Effective_Reach")

      if (nrow(data)==0){  
        data=data.frame(matrix(ncol = 2, nrow = 0))
        names(data) = c("Market","Values")
        p <- plot_ly(
          x = data$Market,
          y = data$Values,
          type = "bar"
        ) 
        return(p)
        }
      data = aggregate(data$Earned_Effective_Reach, by=list(Market=data$Market), FUN=sum)
      if (nrow(data)==0){  
        
        data=data.frame(matrix(ncol = 2, nrow = 0))
        names(data) = c("Market","Values")
        p <- plot_ly(
          x = data$Market,
          y = data$Values,
          type = "bar"
        ) 
        return(p)
        }
      names(data) = c("Market","EER")
      japan_EER = as.numeric(data[1,2])
      data[nrow(data) + 1,] = c("Korea",japan_EER * .6514 )
      data[nrow(data) + 1,] = c("Taiwan",japan_EER * .0155)
      data[nrow(data) + 1,] = c("HongKong",japan_EER * .0032)
      data[nrow(data) + 1,] = c("Indonesia",japan_EER * 0.0133)
      data[nrow(data) + 1,] = c("Malaysia",japan_EER * 0.0103)
      data[nrow(data) + 1,] = c("Thailand",japan_EER * 0.0268)
      data[nrow(data) + 1,] = c("Singapore",japan_EER * 0.0013)
      data$EER = round(as.numeric(data$EER),2)
    }
    if (reactive_return_metric() == "Engagement"){
      
      # FB_Post_Data = select(FB_Post_Data, Date,Label1,Engagement)
      # FB_Post_Data = as.data.frame(FB_Post_Data)
      
      
      TW_Post_Data$Engagement=  TW_Post_Data["Reply Count"] + TW_Post_Data["Favorite Count"] + TW_Post_Data["Retweet Count"]
      TW_Post_Data = select(TW_Post_Data, Date,Label1,Engagement)
      TW_Post_Data = as.data.frame(TW_Post_Data)
      names(TW_Post_Data) = c("Date","Label1","Engagement")
      
      TW_Post_Data$Engagement=TW_Post_Data$Engagement$`Reply Count`
      
      IG_Post_Data = select(IG_Post_Data, Date,Label1,Engagement)
      IG_Post_Data = as.data.frame(IG_Post_Data)
      
      #data=rbind(TW_Post_Data,FB_Post_Data,IG_Post_Data)
      
      data=rbind(TW_Post_Data,IG_Post_Data)
      if (nrow(data)==0){ 
        
        data=data.frame(matrix(ncol = 2, nrow = 0))
        names(data) = c("Market","Values")
        p <- plot_ly(
          x = data$Market,
          y = data$Values,
          type = "bar"
        ) 
      return(p)
        }
      data=get_markettrend_data(data,start_date,end_date)
      if (nrow(data)==0){  
        data=data.frame(matrix(ncol = 2, nrow = 0))
        names(data) = c("Market","Values")
        p <- plot_ly(
          x = data$Market,
          y = data$Values,
          type = "bar"
        ) 
        
        return(p)
        }
      data=select(data,Market,Engagement)
      if (nrow(data)==0){  
        
        data=data.frame(matrix(ncol = 2, nrow = 0))
        names(data) = c("Market","Values")
        p <- plot_ly(
          x = data$Market,
          y = data$Values,
          type = "bar"
        )
      return(p)
        }
      data = aggregate(data$Engagement, by=list(Market=data$Market), FUN=sum)
      names(data) = c("Market","Engagement")
      japan_Engagement = as.numeric(data[1,2])
      data[nrow(data) + 1,] = c("Korea",japan_Engagement * 0.20 )
      data[nrow(data) + 1,] = c("Taiwan",japan_Engagement * 2.54)
      data[nrow(data) + 1,] = c("HongKong",japan_Engagement * 0.377)
      data[nrow(data) + 1,] = c("Indonesia",japan_Engagement * 0.189)
      data[nrow(data) + 1,] = c("Malaysia",japan_Engagement * 0.70)
      data[nrow(data) + 1,] = c("Thailand",japan_Engagement * 0.003)
      data[nrow(data) + 1,] = c("Singapore",japan_Engagement * 0.235)
      data$Engagement = round(as.numeric(data$Engagement),2)
    }
    #this metric is no more present in the dashboard
    if (reactive_return_metric() == "Engagement Rate"){
     
      
      FB_Post_Data = select(FB_Post_Data, Date,Label1,`Engagement Rate`)
      TW_Post_Data = select(TW_Post_Data, Date,Label1,`Engagement Rate`)
      IG_Post_Data = select(IG_Post_Data, Date,Label1,`Engagement Rate`)
      
      
      data=rbind(TW_Post_Data,FB_Post_Data,IG_Post_Data)
      if (is.null(data)) { 
        data=data.frame(matrix(ncol = 2, nrow = 0))
        names(data) = c("Market","Values")
        p <- plot_ly(
          x = data$Market,
          y = data$Values,
          type = "bar"
        )
        return(p)
        }
      data=get_markettrend_data(data,start_date,end_date)
      
    }
    
    # View(data)
    
    if (nrow(data)==0 ) {  
      data=data.frame(matrix(ncol = 2, nrow = 0))
    }
    names(data) = c("Market","Values")
    
    p <- plot_ly(
      x = data$Market,
      y = data$Values,
      type = "bar",
      marker = list(color = "#E1005D"),
      name = toupper(reactive_return_metric())
      ) %>% 
      layout(showlegend = FALSE)%>% 
        layout(
        yaxis = list(
          title = paste0(toupper(reactive_return_metric())," ")
          #color = "#ff9999"
        ),
        xaxis = list(
          title = "Country"
          #color = "#ff9999"
        )
      )%>% config(displayModeBar = F) %>%
                add_trace(x=1,y=1) %>% 
                     layout(autosize = T, list(l = 200,pad=10))
    p
    
  })
  
  
  
  output$test_chart <- renderPlot({
    dimple(mtcars,
           pMeasure="mpg",
           series="manuf",
           legend=TRUE, 
           chartType="pie",
           ringInnerRadius="50%")
  })
  
  output$select_year = renderUI({
    selectInput('Year', '', reactive_year(),width="120px")
  })
  
  output$select_year_mrp = renderUI({
    selectInput('Year_mrp', '', reactive_year(),width="120px")
  })
  output$select_infleuncer_mrp = renderUI({
    selectInput('Influencer_mrp', '', reactive_influencer_unique_names(),width="200px")
  })
  
  output$display_year = renderUI({
    
    if (length(v$platform_params)==0) return() #included this to avoid the rendering
    selectInput('year_ht', '', reactive_year_ht(),width="120px")
  })
  output$select_week = renderUI({
    
    selectInput('Week', '', reactive_week(),width="auto",choices= setNames(reactive_get_raw_date(), reactive_get_display_date()) )
  })
  
  output$select_metric = renderUI({
    selectInput('metric_mt', '', choices = c("Earned Effective Reach","Engagement"),width="auto" )
  })
  output$select_metric_mrp = renderUI({
    selectInput('metric_mrp', '', choices = c("Earned Effective Reach","Engagement","Sentiment","M-Score"),width="auto" )
  })
  TIP <- reactiveValues()
  observe({
    if (is.null(v$instagram_EER)) { return()}
    #insta <- paste("Instagram: ",v$instagram_EER, "MM" )
    insta <- paste(tags$img(src='instagram.png',width = "30",height = "30"),v$instagram_EER, "MM" )
    twitter <- paste(tags$img(src='twitter.png',width = "30",height = "30"),v$twitter_EER, "MM" )
    TIP$EER_text=HTML (paste(insta,twitter, sep="<br/><br/>"))
    
    TIP$Engagment_Text
    insta_E <- paste(tags$img(src='instagram.png',width = "30",height = "30"),v$instagram_E, "MM" )
    twitter_E <- paste(tags$img(src='twitter.png',width = "30",height = "30"),v$twitter_E, "MM" )
    TIP$Engagment_text=HTML (paste(insta_E,twitter_E, sep="<br/><br/>"))
    
  })

  output$EER_box = renderUI({
    tags$style(".fa-info-circle {color:#ffffff }")
    box(title=strong("EARNED EFFECTIVE REACH"),div(id="reach_text",h1(textOutput("display_total_reach")),style = "color: #E1005D;"),solidHeader = TRUE,width = 3,height=200 ,
        tipify(el = icon(name = "info-circle", lib = "font-awesome"), title = TIP$EER_text))
  })
  
  output$dynamic_UI_box <- renderUI({
    
    filtered_posts_df=filter_posts()
    # View(filtered_posts_df)
    option_selected = as.character(reactive_return_count_asset())
    if (grepl("All", option_selected)==TRUE){
      if (nrow(filtered_posts_df)!=0){
        count_selected = nrow(filtered_posts_df)
      }
      count_selected=0
    }else{
      count_selected= as.integer((unlist(strsplit(as.character(reactive_return_count_asset()), "\\s"))[1]))
    }
    
    if (nrow(filtered_posts_df) == 0){
      title=div("There are no posts for the selected filters! Please change filter options and try again.", align = "center")
      return (title)
    }
    else{
      if (grepl("All", option_selected)==TRUE){
        numBoxes=nrow(filtered_posts_df)
      }else{
          if (count_selected <=nrow(filtered_posts_df)){
            numBoxes=as.integer(reactive_return_count_asset())
          }else{
            numBoxes=nrow(filtered_posts_df)
          }
      }
      LL <- vector("list",numBoxes)
      for(i in 1:numBoxes){
        
        influencer_name = as.character(filtered_posts_df[i,"KOL"])
        date            = as.character(filtered_posts_df[i,"DATE"])
        description     = as.character(filtered_posts_df[i,"DESCRIPTION"])
        eer             = filtered_posts_df[i,"EARNED EFFECTIVE REACH"]
        engagement      = filtered_posts_df[i,"ENGAGEMENT"]
        engagement_rate = filtered_posts_df[i,"ENGAGEMENT RATE"]
        reactions       = (filtered_posts_df[i,"REACTIONS"])
        sentiment       = (filtered_posts_df[i,"SENTIMENT"])
        mscore          = (filtered_posts_df[i,"M-SCORE"])
        time            = as.character(filtered_posts_df[i,"TIME"])
        type            = as.character(filtered_posts_df[i,"TYPE"])
        comments        = (filtered_posts_df[i,"COMMENTS"])
        post_url        = as.character(filtered_posts_df[i,"POST"])
        platform        = as.character(filtered_posts_df[i,"PLATFORM"])
        sentiment        = as.character(filtered_posts_df[i,"SENTIMENT"])
        
        
        LL[[i]] <- column(4,box(
          
          HTML(paste('<center>',post_url,influencer_name,'</center>')),
          #HTML(paste('<center>',influencer_name,'</center>')),
          HTML(paste(platform, type,'on ',date,'at ',time)),
          tags$h5(description,style="text-overflow:ellipsis"),
          #HTML(description),br(),
          fluidRow( 
            column(7,HTML("EARNED EFFECTIVE REACH:")),
            column(3,HTML(eer))
          ),
          fluidRow( 
            column(7,HTML("ENGAGEMENT:")),
            column(3,HTML(engagement))
          ),
          fluidRow( 
            column(7,HTML("ENGAGEMENT RATE:")),
            column(3,HTML(engagement_rate))
          ),
          fluidRow( 
            column(7,HTML("REACTIONS:")),
            column(3,HTML(reactions))
          ),
          fluidRow( 
            column(7,HTML("COMMENTS:")),
            column(3,HTML(comments))
          ),
          fluidRow( 
            column(7,HTML("M-SCORE:")),
            column(3,HTML(mscore))
          ),
                                      
                                     
                                      
                                       #uiOutput("top_infleuncer_image_test"),
                                      # uiOutput("name"),
                                      # uiOutput("image_test"),
                                      # uiOutput("description_text"),br(),
                                      # uiOutput("EER_test"),
                                      # uiOutput("Engagement_test"),
                                      # uiOutput("Engagement_rate_test"),
                                      # uiOutput("comments_test"),
                                      # uiOutput("Reactions_test"),
                                      # uiOutput("mscore_test"),
                                      # uiOutput("Sentiment_text"),
        solidHeader = TRUE,width = 200,height=500))
      }
      return(LL)
    }
  })
  output$Engagment_box = renderUI({
    tags$style(".fa-info-circle {color:#ffffff }")
    box(title=strong("ENGAGEMENT"),div(id="engagement_text",h1(textOutput("display_total_engagement")),style = "color: #E1005D;"),solidHeader = TRUE,width = 3,height=200 ,
        tipify(el = icon(name = "info-circle", lib = "font-awesome"), title = TIP$Engagment_text))
  })
  
  output$select_market = renderUI({
    selectInput('Market_ps', '', reactive_market_ps(),width="120px")
  })
  output$select_month = renderUI({
    selectInput('Month_ps', '', choices = c("Jan","Feb","Mar","Apr","May","Jun",
                                              "Jul","Aug","Sep","Oct","Nov","Dec"),width="80px")
  })
  output$select_month_mrp = renderUI({
    selectInput('Month_mrp', '', choices = c("Jan","Feb","Mar","Apr","May","Jun",
                                            "Jul","Aug","Sep","Oct","Nov","Dec"),width="80px")
  })
  
  output$select_entries_mrp = renderUI({
    selectInput('count_mrp', '', choices = c("5 rows","10 rows","50 rows","All"),width="80px")
  })
  
  output$select_year_ps = renderUI({
    selectInput('Year_ps', '', reactive_year_ps(),width="120px")
  })
  
  output$display_platform = renderUI({
    if (length(v$platform_params)==0) return()
    selectInput('platform_ht', '', v$platform_params,width="150px")
  })
  
  output$display_dimension = renderUI({
    if (length(v$display_type)==0) return()
    selectInput('metric_ht', '', v$display_type,width="150px") #historic trend=ht
  })
  
  output$display_month = renderUI({
    if (length(v$display_month)==0) return()
    selectInput('month_ht', '', v$display_month,width="150px")
  })
  
  
  
  output$display_typeofposts = renderUI({
    if (length(v$display_typeofposts)==0) return()
    selectInput('typeofposts_ht', '', v$display_typeofposts,width="150px")
  })
  
  output$historical_trend_chart <- renderPlotly({
    
    #print (head(v$historical_data))
    #if (length(v$historical_data)==0) return()
    #data = v$historical_data
    # p <- plot_ly(economics, x = ~date, y = ~pop, mode = 'lines')
    # p
    
    req(input$platform_ht)
    req(input$metric_ht)
    req(input$typeofposts_ht)
    req(input$month_ht)
    req(input$year_ht)
    
    print(input$platform_ht)
    print(input$metric_ht)
    print(input$typeofposts_ht)
    print(input$month_ht)
    print(input$year_ht)
    #if (length(v$platform_params)==0) return()
    
    trend_data=get_historical_trend_data()
    # print(nrow(trend_data))
    if(is.null((trend_data)) ){
          print("trend_data is nUll")
          trend_data <- data.frame(matrix(ncol = 2, nrow = 0))
          x <- c("Metric_data", "Date")
          colnames(trend_data) <- x
         
          # p <- plot_ly(trend_data, y = trend_data[,c("Metric_data")], x = trend_data[,c("Date")], type = 'scatter', mode = 'lines',
          #          line = list(color = "#E1005D")) 
      
    }
    else{
          # print("in ELSE")
      
          names(trend_data) = c("Date","Metric_data")
          
          trend_data=as.data.frame(trend_data)
          # p <- plot_ly(trend_data, y = trend_data[,c("Metric_data")], x = trend_data[,c("Date")], type = 'scatter', mode = 'lines',
          #              line = list(color = "#E1005D")) 
    }
    
    p <- plot_ly(
      x = trend_data$Date,
      y = trend_data$Metric_data,
      name = "Trend Data",
      type = "bar",
      marker = list(color = "#E1005D")
      
    ) %>% 
      layout(
        yaxis = list(
          title = paste0(toupper(input$metric_ht))
          #color = "#ff9999"
        ),
        xaxis = list(
          title = "Date"
        )
      )%>% config(displayModeBar = F)  %>%
      layout(autosize = T, margin = 150)
    p
    
    
  })
  
  output$title_historical_trend <- renderText({
    
    if (length(v$historical_trend)==0) return()
    title <- v$historical_trend
    title
  })
  
 
  output$title_sk2_post <- renderText({
    
    req(input$typeofposts_ht)
    if(input$typeofposts_ht!='SK-II Posts')return()
    
    if (length(input$typeofposts_ht)==0){ return() }
    if (input$typeofposts_ht=='SK-II Posts'){
      if (length(v$title_sk2_post)==0) return()
      title <- v$title_sk2_post
    }
    title
  })
  
  output$title_top_comments <- renderText({
    
    req(input$typeofposts_ht)
    
    if(input$typeofposts_ht!='SK-II Posts')return()
    if (input$typeofposts_ht=='SK-II Posts'){
      if (length(v$title_top_comments)==0) return()
      title <- v$title_top_comments
    }
    title
  })
  
  output$post_summary_table <-  DT::renderDataTable({
    
    consolidated_data = NULL
    
    year = reactive_return_year_ps()
    month = reactive_return_month_ps()
    market = reactive_return_market_ps()
    
    # print(year)
    # print(month)
    # print(market)
    
    # FB_Post_Data = reactive_FB_Post_Data()
    # FB_Post_Data = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    # FB_Post_Data = add_mscore(FB_Post_Data,"FB")
    
    TW_Post_Data = reactive_TW_Post_Data()
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    TW_Post_Data = subset(TW_Post_Data,as.character(lapply(strsplit(as.character(TW_Post_Data$Label1), split="_"),tail, n=1))==market)
    TW_Post_Data = add_mscore(TW_Post_Data,"TW")
    TW_Page_Data = reactive_TW_Page_Data()
    TW_EER = add_EER(TW_Page_Data,TW_Post_Data,"TW")
    TW_Post_Data = merge(TW_Post_Data,TW_EER)
    
    IG_Post_Data = reactive_IG_Post_Data()
    IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    IG_Post_Data=subset(IG_Post_Data,as.character(lapply(strsplit(as.character(IG_Post_Data$Label1), split="_"),tail, n=1))==market)
    IG_Post_Data = add_mscore(IG_Post_Data,"IG")
    IG_Page_Data = reactive_IG_Page_Data()
    IG_EER = add_EER(IG_Page_Data,IG_Post_Data,"IG")
    IG_Post_Data = merge(IG_Post_Data,IG_EER)
    
    ###FB

    # #market filter
    # subset_FB=subset(FB_Post_Data,as.character(lapply(strsplit(as.character(FB_Post_Data$Label1), split="_"),tail, n=1))==market)
    # FB = data.frame(subset_FB$Date,subset_FB$post_text,subset_FB$Name,subset_FB$Engagement,subset_FB$`Engagement Rate`,subset_FB$sentiment_keyword,subset_FB$mscore,subset_FB$post_image)
    # names(FB) <- c('DATE','DESCRIPTION','KOL','ENGAGEMENT','ENGAGEMENT RATE','SENTIMENT','M-SCORE','URL')
    # #year filter
    # FB=na.omit(subset(FB,(as.character(lapply(strsplit(as.character(FB_Post_Data$Date), split="/"),tail, n=1)) == year)))
    # #month filter
    # month_data = as.numeric(sapply(strsplit(as.character(FB_Post_Data$Date), split="/"),'[',2))
    # FB = subset(FB,tolower(month.abb[month_data]) == tolower(month.abb[as.numeric(month)]) )
    # #FB = na.omit(FB)
   

    ##TWITTER


    TW_Post_Data$Engagement=  TW_Post_Data["Reply Count"] + TW_Post_Data["Favorite Count"] + TW_Post_Data["Retweet Count"]
    TW_Post_Data = as.data.frame(TW_Post_Data)
    TW_Post_Data$Engagement=TW_Post_Data$Engagement$`Reply Count`
    #subset_TW=subset(TW_Post_Data,as.character(lapply(strsplit(as.character(TW_Post_Data$Label1), split="_"),tail, n=1))==market)
    subset_TW=TW_Post_Data
    # #engageentrate
    TW = data.frame(subset_TW$Date,subset_TW$Text,subset_TW$Username,subset_TW$Earned_Effective_Reach,subset_TW$Engagement,subset_TW$`Engagement Rate`,subset_TW$sentiment_keyword,subset_TW$mscore,subset_TW$`Post Image`)
    names(TW) <- c('DATE','DESCRIPTION','KOL','EARNED EFFECTIVE REACH','ENGAGEMENT','ENGAGEMENT RATE','SENTIMENT','M-SCORE','URL')
    #year filter
    TW=subset(TW,(as.character(lapply(strsplit(as.character(TW_Post_Data$Date), split="/"),tail, n=1)) == year))
    #month filter
    month_data = as.numeric(sapply(strsplit(as.character(TW_Post_Data$Date), split="/"),'[',2))
    TW = subset(TW,tolower(month.abb[month_data]) == tolower(month.abb[as.numeric(month)]) )
    
    ##IG
    subset_IG=IG_Post_Data
    #subset_IG=subset(IG_Post_Data,as.character(lapply(strsplit(as.character(IG_Post_Data$Label1), split="_"),tail, n=1))==market)
    IG = data.frame(subset_IG$Date,subset_IG$Caption,subset_IG$Username,subset_IG$Earned_Effective_Reach,subset_IG$Engagement,subset_IG$`Engagement Rate`,subset_IG$sentiment_keyword,subset_IG$mscore,subset_IG$`Media URL`)
    names(IG) <- c('DATE','DESCRIPTION','KOL','EARNED EFFECTIVE REACH','ENGAGEMENT','ENGAGEMENT RATE','SENTIMENT','M-SCORE','URL')
    #year filter
    IG=subset(IG,(as.character(lapply(strsplit(as.character(IG_Post_Data$Date), split="/"),tail, n=1)) == year))
    #month filter
    month_data = as.numeric(sapply(strsplit(as.character(IG_Post_Data$Date), split="/"),'[',2))
    IG = subset(IG,tolower(month.abb[month_data]) == tolower(month.abb[as.numeric(month)]) )
    
    
    consolidated_data = rbind(TW,IG)
    
    consolidated_data=dplyr::arrange(consolidated_data, desc(DATE))
    
    if(nrow(consolidated_data)!=0){
              height = " width=\"50\" height=\"52\"></img>"
              consolidated_data$picture_str = paste0("\"",consolidated_data$URL,"\"")
              consolidated_data$POST = paste("<img class='img-circle' src=",consolidated_data$picture_str,height)
              #consolidated_data$POST = paste("<img src=",consolidated_data$picture_str,height)
              consolidated_data$URL = NULL
              consolidated_data$picture_str=NULL
              names(consolidated_data)= c('DATE','DESCRIPTION','KOL','EARNED EFFECTIVE REACH','ENGAGEMENT','ENGAGEMENT RATE','SENTIMENT','M-SCORE','POST')
              
              consolidated_data <- consolidated_data[,c('DATE','POST','DESCRIPTION','KOL','EARNED EFFECTIVE REACH','ENGAGEMENT','ENGAGEMENT RATE','SENTIMENT','M-SCORE')]
              consolidated_data=consolidated_data[complete.cases(consolidated_data), ]
             
              # colnames(consolidated_data)[c(1,2,3,4,5,6,7)] <- paste0('<span style="color:',c("#E1005D","#E1005D","#E1005D","#E1005D","#E1005D","#E1005D","#E1005D"),'">',colnames(consolidated_data)[c(1,2,3,4,5,6,7)],'</span>')
              # DT::datatable(consolidated_data,escape=F)
              
              # consolidated_data$IMAGE <- sprintf('![](beats.png)')
              
              # View(consolidated_data)
              
              # consolidated_data    
              
    }else{
      consolidated_data <- data.frame(matrix(ncol = 9, nrow = 0))
      x <- c('DATE','POST','DESCRIPTION','KOL','EARNED EFFECTIVE REACH','ENGAGEMENT','ENGAGEMENT RATE','SENTIMENT','M-SCORE')
      colnames(consolidated_data) <- x
      
  }
    #li=c('DATE','POST','DESCRIPTION','KOL','ENGAGEMENT','ENGAGEMENT RATE','SENTIMENT','M-SCORE')
    # colnames(iris_coloured)[c(1,3)] <- paste0('<span style="color:',c("red","blue"),'">',colnames(iris)[c(1,3)],'</span>')
    DT::datatable(consolidated_data,escape=F, options = list(scrollX = TRUE,language = list(
      zeroRecords = "No records to display for the selected filter! Please change the filter options and try again.")  ,pageLength = 10,searching = FALSE,columnDefs = list(list(
      targets = 3,
      
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 6 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
        "}")
      
    ))))%>% formatStyle(
      'M-SCORE',
      backgroundColor = "#FFC6DE"
    )%>% formatStyle(
      'SENTIMENT',
      color = styleEqual(c("POSITIVE", "NEGATIVE", "NEUTRAL"), c('#2a9a2a', '#f40939','#FFC200'))
    )%>% formatStyle(
      'ENGAGEMENT', 'SENTIMENT',
      color = styleEqual(c("POSITIVE", "NEGATIVE", "NEUTRAL"), c('#2a9a2a', '#f40939','#FFC200'))
    )%>% formatStyle(
      'ENGAGEMENT RATE', 'SENTIMENT',
      color = styleEqual(c("POSITIVE", "NEGATIVE", "NEUTRAL"), c('#2a9a2a', '#f40939','#FFC200'))
    )%>% formatStyle(
      'M-SCORE', 'SENTIMENT',
      color = styleEqual(c("POSITIVE", "NEGATIVE", "NEUTRAL"), c('#2a9a2a', '#f40939','#FFC200'))
    )
    #, callback = JS('table.page(3).draw(false);')) 
    
   
  })
    
  

                   
  
    
    
  
 
  
  
  
  
  ################### reactive data #####################
  
  reactive_FB_Post_Data = reactive({
    validate(
      need(input$input_file_upload$name != "", "Please wait till the file is being uploaded!")
    )
    
    #FB_Post_Data  =read_excel(input$input_file_upload$datapath, sheet = "FB Post Data")
    
    
    ext <- tools::file_ext(input$input_file_upload$name)
    file.rename(input$input_file_upload$datapath,
                paste(input$input_file_upload$datapath, ext, sep="."))
    FB_Post_Data=read_excel(paste(input$input_file_upload$datapath, ext, sep="."), sheet = "FB Post Data")
    
    return (FB_Post_Data)
  })
  
  reactive_FB_Comments_Data = reactive({
    validate(
      need(input$input_file_upload$name != "", "Please wait till the file is being uploaded!")
    )
    #FB_Comments_Data  =read_excel(input$input_file_upload$datapath, sheet = "FB Conversations")
    
    ext <- tools::file_ext(input$input_file_upload$name)
    file.rename(input$input_file_upload$datapath,
                paste(input$input_file_upload$datapath, ext, sep="."))
    FB_Comments_Data=read_excel(paste(input$input_file_upload$datapath, ext, sep="."), sheet = "FB Conversations")
    
    return (FB_Comments_Data)
  })
  
  reactive_TW_Comments_Data = reactive({
    validate(
      need(input$input_file_upload$name != "", "Please wait till the file is being uploaded!")
    )
    #TW_Comments_Data  =read_excel(input$input_file_upload$datapath, sheet = "TW Mentions Data")
    
    ext <- tools::file_ext(input$input_file_upload$name)
    file.rename(input$input_file_upload$datapath,
                paste(input$input_file_upload$datapath, ext, sep="."))
    TW_Comments_Data=read_excel(paste(input$input_file_upload$datapath, ext, sep="."), sheet = "TW Mentions Data")
    
    
    return (TW_Comments_Data)
  })
  
  reactive_IG_Comments_Data = reactive({
    validate(
      need(input$input_file_upload$name != "", "Please wait till the file is being uploaded!")
    )
    #IG_Comments_Data  =read_excel(input$input_file_upload$datapath, sheet = "IG Comments Data")
    ext <- tools::file_ext(input$input_file_upload$name)
    file.rename(input$input_file_upload$datapath,
                paste(input$input_file_upload$datapath, ext, sep="."))
    IG_Comments_Data=read_excel(paste(input$input_file_upload$datapath, ext, sep="."), sheet = "IG Comments Data")
    
    
    return (IG_Comments_Data)
  })

  
  reactive_TW_Post_Data <<- reactive({
    validate(
      need(input$input_file_upload$name != "", "Please wait till the file is being uploaded!")
    )
    #TW_Post_Data  = read_excel(input$input_file_upload$datapath, sheet = "TW Post Data")
    ext <- tools::file_ext(input$input_file_upload$name)
    file.rename(input$input_file_upload$datapath,
                paste(input$input_file_upload$datapath, ext, sep="."))
    TW_Post_Data=read_excel(paste(input$input_file_upload$datapath, ext, sep="."), sheet = "TW Post Data")
    
    
    return (TW_Post_Data)
  })
  
  reactive_IG_Post_Data <<- reactive({
    validate(
      need(input$input_file_upload$name != "", "Please wait till the file is being uploaded!")
    )
    #IG_Post_Data=read_excel(input$input_file_upload$datapath,sheet="IG Post Data")
    
    ext <- tools::file_ext(input$input_file_upload$name)
    file.rename(input$input_file_upload$datapath,
                paste(input$input_file_upload$datapath, ext, sep="."))
    IG_Post_Data=read_excel(paste(input$input_file_upload$datapath, ext, sep="."), sheet = "IG Post Data")
    
    return (IG_Post_Data)
  })
  
  reactive_IG_Summary_Data <<-reactive({
    
    validate(
      need(input$input_file_upload$name != "", "Please wait till the file is being uploaded!")
    )
    
    #IG_Summary_Data=read_excel(input$input_file_upload$datapath,sheet="IG Summary Data")
    
    ext <- tools::file_ext(input$input_file_upload$name)
    file.rename(input$input_file_upload$datapath,
                paste(input$input_file_upload$datapath, ext, sep="."))
    IG_Summary_Data=read_excel(paste(input$input_file_upload$datapath, ext, sep="."), sheet = "IG Summary Data")
    
    
    return (IG_Summary_Data)
  })
  
  reactive_FB_Page_Data<<-reactive ({
    
    validate(
      need(input$input_file_upload$name != "", "Please wait till the file is being uploaded!")
    )
    #FB_Page_Data=read_excel(input$input_file_upload$datapath,sheet="FB Page Data")
    
    ext <- tools::file_ext(input$input_file_upload$name)
    file.rename(input$input_file_upload$datapath,
                paste(input$input_file_upload$datapath, ext, sep="."))
    FB_Page_Data=read_excel(paste(input$input_file_upload$datapath, ext, sep="."), sheet = "FB Page Data")
    
    
    return (FB_Page_Data)
  })
  
  reactive_TW_Page_Data<<-reactive ({
    
    validate(
      need(input$input_file_upload$name != "", "Please wait till the file is being uploaded!")
    )
    #TW_Page_Data=read_excel(input$input_file_upload$datapath,sheet="TW Page Data")
    
    ext <- tools::file_ext(input$input_file_upload$name)
    file.rename(input$input_file_upload$datapath,
                paste(input$input_file_upload$datapath, ext, sep="."))
    TW_Page_Data=read_excel(paste(input$input_file_upload$datapath, ext, sep="."), sheet = "TW Page Data")
    
    
    
    return (TW_Page_Data)
  })
  
  reactive_FB_Summary_Stats<<-reactive ({
    
    validate(
      need(input$input_file_upload$name != "", "Please wait till the file is being uploaded!")
    )
    
    #FB_Summary_Stats=read_excel(input$input_file_upload$datapath,sheet="FB Summary Stats")
    ext <- tools::file_ext(input$input_file_upload$name)
    file.rename(input$input_file_upload$datapath,
                paste(input$input_file_upload$datapath, ext, sep="."))
    FB_Summary_Stats=read_excel(paste(input$input_file_upload$datapath, ext, sep="."), sheet = "FB Summary Stats")
    
    return (FB_Summary_Stats)
  })
  
  
  reactive_IG_Page_Data<<-reactive ({
    #IG_Page_Data=read_excel(input$input_file_upload$datapath,sheet="IG Page Data")
    
    validate(
      need(input$input_file_upload$name != "", "Please wait till the file is being uploaded!")
    )
    
    ext <- tools::file_ext(input$input_file_upload$name)
    file.rename(input$input_file_upload$datapath,
                paste(input$input_file_upload$datapath, ext, sep="."))
    IG_Page_Data=read_excel(paste(input$input_file_upload$datapath, ext, sep="."), sheet = "IG Page Data")
    
    
    return (IG_Page_Data)
  })
  
  reactive_return_year<<-reactive({
    return (input$Year)
    
  })
  
  reactive_return_count_asset<<-reactive({
    return (input$count_mrp)
  })
  
  reactive_return_Influencer_mrp<<-reactive({
    return (input$Influencer_mrp)
  })
  reactive_return_metric_mrp<<-reactive({
    return (input$metric_mrp)
  })
  reactive_return_Month_mrp<<-reactive({
    return (input$Month_mrp)
  })
  reactive_return_Year_mrp<<-reactive({
    return (input$Year_mrp)
  })
  
  reactive_return_metric<<-reactive({
    return (input$metric_mt)
    
  })
  
  reactive_return_week<<-reactive({
    return (input$Week)
    
  })
  
  reactive_influencer_unique_names<<-reactive({
    
    TW_Post_Data = reactive_TW_Post_Data()
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    TW_Post_Data = add_mscore(TW_Post_Data,"TW")
    
    IG_Post_Data = reactive_IG_Post_Data()
    IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    IG_Post_Data = add_mscore(IG_Post_Data,"IG")
    
    # FB=data.frame(FB_Post_Data$Name,FB_Post_Data$mscore,FB_Post_Data$post_image)
    # names(FB) = c('Infleuncer','MSCORE','URL')
    IG = data.frame(IG_Post_Data$Username,IG_Post_Data$mscore,IG_Post_Data$`Media URL`,IG_Post_Data$`Profile Image`)
    names(IG) = c('Infleuncer','MSCORE','POST_URL','PROFILE_URL')
    TW = data.frame(TW_Post_Data$Username,TW_Post_Data$mscore,TW_Post_Data$`Post Image`,TW_Post_Data$`Profile Image`)
    names(TW) = c('Infleuncer','MSCORE','POST_URL','PROFILE_URL')
    
    df_consolidated <- rbind(IG,TW)
    df_consolidated=df_consolidated[rev(order(df_consolidated$MSCORE)),c('Infleuncer','MSCORE','POST_URL','PROFILE_URL')]
    
    if (is.null(df_consolidated)) {   return() }
    df=aggregate(MSCORE ~ Infleuncer, data=df_consolidated, FUN=mean)
    data=df_consolidated[!duplicated(df_consolidated$Infleuncer), ]
    top_influencers =(merge(data, df, by = 'Infleuncer',sort=F))
    names(top_influencers) = c("Infleuncer","Total_mscore","POST_URL","PROFILE_URL","MSCORE")
    top_influencers$MSCORE =
      format(round(top_influencers$MSCORE, 2))
    top_influencers$Total_mscore=NULL
    
    return (as.character(unique(top_influencers$Infleuncer)))
    
    
    
    
  })
  reactive_year<<-reactive({
    FB_Post_Data = reactive_FB_Post_Data()
    FB_Post_Data = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    
    TW_Post_Data = reactive_TW_Post_Data()
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    
    IG_Post_Data = reactive_IG_Post_Data()
    IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    
    
    year_fb= unique(as.character(lapply(strsplit(as.character(FB_Post_Data$Date), split="/"),tail, n=1)))
    year_tw= unique(as.character(lapply(strsplit(as.character(TW_Post_Data$Date), split="/"),tail, n=1)))
    year_ig= unique(as.character(lapply(strsplit(as.character(IG_Post_Data$Date), split="/"),tail, n=1)))
    
    
    li = append(list(year_fb),list(year_tw))
    li=append(li, list(year_ig))
    year=unique(unlist(li))
    year = year[!is.na(year)]
    
    return (year)
  })
  
  reactive_year_ps<<-reactive({
    
    # FB_Post_Data = reactive_FB_Post_Data()
    # FB_Post_Data = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    
    TW_Post_Data = reactive_TW_Post_Data()
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    
    IG_Post_Data = reactive_IG_Post_Data()
    IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    
    
    # year_fb= unique(as.character(lapply(strsplit(as.character(FB_Post_Data$Date), split="/"),tail, n=1)))
    year_tw= unique(as.character(lapply(strsplit(as.character(TW_Post_Data$Date), split="/"),tail, n=1)))
    year_ig= unique(as.character(lapply(strsplit(as.character(IG_Post_Data$Date), split="/"),tail, n=1)))
    
    
    # li = append(list(year_fb),list(year_tw))
    # li=append(li, list(year_ig))
    
    
    li = append(list(year_ig),list(year_tw))
    year_dropdown=unique(unlist(li))
    year_dropdown = year_dropdown[!is.na(year_dropdown)]
    
    
    return (year_dropdown)
  })
  
 
  reactive_year_ht<<-reactive({
    
    require (input$platform_ht )
    if (length(input$platform_ht)==0) return()
    
    
    if(input$platform_ht == 'Facebook'){
      FB_Post_Data = reactive_FB_Post_Data()
      year= unique(as.character(lapply(strsplit(as.character(FB_Post_Data$Date), split="/"),tail, n=1)))
      
    }
    if (input$platform_ht == 'Instagram'){
      IG_Post_Data = reactive_IG_Post_Data()
      year= unique(as.character(lapply(strsplit(as.character(IG_Post_Data$Date), split="/"),tail, n=1)))
      
    }
    if (input$platform_ht == 'Twitter'){
      
      TW_Post_Data = reactive_TW_Post_Data()
      year= unique(as.character(lapply(strsplit(as.character(TW_Post_Data$Date), split="/"),tail, n=1)))
      
    }
    year=year[!is.na(year)]
    return (year)
  })
  
  reactive_top_influencer_image_url<<-reactive({ 
    
    # FB_Post_Data  = reactive_FB_Post_Data()
    # FB_Post_Data = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,c('post_impressions_unique','post_link','post_image')]
    # names(FB_Post_Data)=c('Reach','post_link','post_image')
    #web scraping logic to be in place
    #top_influencer_image_URL= as.character(FB_Post_Data[which.max(FB_Post_Data$Reach),"post_image"])
    
    IG_Page_Data = reactive_IG_Page_Data()
    IG_Post_Data = reactive_IG_Post_Data() 
    
    TW_Page_Data = reactive_TW_Page_Data()
    TW_Post_Data = reactive_TW_Post_Data()
    
    #INSTAGRAM
    IG_Page_Data["Earned_Effective_Reach"] = 0.035 * IG_Page_Data["Followers"] 
    IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    
    IG_Post_Data_image_df = IG_Post_Data[(grepl(paste(c("image","carousel"), collapse = "|"), IG_Post_Data$Type)==TRUE),]
    IG_Post_Data_image_df=merge(IG_Post_Data_image_df,IG_Page_Data, by  = c("Username","Date"))
    colnames(IG_Post_Data_image_df)[3] <- "Day"
    IG_Post_Data_image_df = IG_Post_Data_image_df[,c("Username","Date","Profile Image","Media URL","Earned_Effective_Reach" )]
    colnames(IG_Post_Data_image_df)[4] <- "Post Image"
    
    IG_Post_Data_video_df = IG_Post_Data[(grepl(paste(c("video"), collapse = "|"), IG_Post_Data$Type)==TRUE),]
    IG_Post_Data_video_df["Earned_Effective_Reach"] = 0.25 * IG_Post_Data_video_df["Views"] 
    IG_Post_Data_video_df = IG_Post_Data_video_df[,c("Username","Date","Profile Image","Media URL","Earned_Effective_Reach" )]
    
    IG_Post_Data_video_df=IG_Post_Data_video_df[, c("Username","Date","Profile Image","Media URL","Earned_Effective_Reach" )]
    colnames(IG_Post_Data_video_df)[4] <- "Post Image"
    
    IG_Post_Data_sk2 = rbind(IG_Post_Data_image_df,IG_Post_Data_video_df)
    IG_Post_Data_sk2$Platform = "Instagram"
    
    #TWITTER
    
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    
    TW_Page_Data["Earned_Effective_Reach"] = 0.054 * TW_Page_Data["Followers"] 
    
    
    TW_Post_Data_image_df = TW_Post_Data[grepl(paste(c("Link","Photo","Text"), collapse = "|"), TW_Post_Data$Type)==TRUE,]
    
    TW_Post_Data_image_df=merge(TW_Post_Data_image_df,TW_Page_Data, by  = c("Username","Date"))
    TW_Post_Data_image_df=TW_Post_Data_image_df[, c("Username","Date","Profile Image","Post Image","Earned_Effective_Reach" )]
    TW_Post_Data_image_df$Platform = "Twitter"
    
    df = rbind(TW_Post_Data_image_df,IG_Post_Data_sk2)
    df=df[ order(df$Earned_Effective_Reach , decreasing = TRUE ),]
    #the output is based on the highest EER and not date
    # df$Date=gsub("/", "-", df$Date)
    # df$Date=dmy(df$Date)
    # df=df[ order(df$Date , decreasing = TRUE ),]
    # df$Date=gsub("-", "/", df$Date)
    
    profile_image= df[1,"Profile Image"]
    post_image= df[1,"Post Image"]
    platform= df[1,"Platform"]
    
    return(list(profile_image,post_image,platform))
    
  })
  
  reactive_top_post_url<<-reactive({ 
    
    FB_Post_Data  = reactive_FB_Post_Data()
    FB_Post_Data = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,c('post_impressions_unique','post_link','post_image')]
    names(FB_Post_Data)=c('Reach','post_link','post_image')
    
    #after clarification - work on this
    IG_Post_Data  = reactive_IG_Post_Data()
    TW_Post_Data  = reactive_TW_Post_Data()
    
    #web scraping logic to be in place
    reactive_top_post_url= as.character(FB_Post_Data[which.max(FB_Post_Data$Reach),"post_image"])
    return(reactive_top_post_url)
    
  })

  #post summary - screen 1
  reactive_return_market_ps<<-reactive({
    return (input$Market_ps)
  })
  reactive_return_month_ps<<-reactive({
    
    return (map_month(input$Month_ps))
  })
  reactive_return_year_ps<<-reactive({
    return (input$Year_ps)
  })
  
  
  
  reactive_return_getTop10Infleuncer<<-reactive({
    
    
    # FB_Post_Data = reactive_FB_Post_Data()
    # FB_Post_Data = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    # FB_Post_Data = add_mscore(FB_Post_Data,"FB")
    
    TW_Post_Data = reactive_TW_Post_Data()
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    TW_Post_Data = add_mscore(TW_Post_Data,"TW")
    
    IG_Post_Data = reactive_IG_Post_Data()
    IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    IG_Post_Data = add_mscore(IG_Post_Data,"IG")
    
    # FB=data.frame(FB_Post_Data$Name,FB_Post_Data$mscore,FB_Post_Data$post_image)
    # names(FB) = c('Infleuncer','MSCORE','URL')
    IG = data.frame(IG_Post_Data$Username,IG_Post_Data$mscore,IG_Post_Data$`Media URL`,IG_Post_Data$`Profile Image`)
    names(IG) = c('Infleuncer','MSCORE','POST_URL','PROFILE_URL')
    TW = data.frame(TW_Post_Data$Username,TW_Post_Data$mscore,TW_Post_Data$`Post Image`,TW_Post_Data$`Profile Image`)
    names(TW) = c('Infleuncer','MSCORE','POST_URL','PROFILE_URL')
    
    df_consolidated <- rbind(IG,TW)
    df_consolidated=df_consolidated[rev(order(df_consolidated$MSCORE)),c('Infleuncer','MSCORE','POST_URL','PROFILE_URL')]
    
    if (is.null(df_consolidated)) {   return() }
    df=aggregate(MSCORE ~ Infleuncer, data=df_consolidated, FUN=mean)
    data=df_consolidated[!duplicated(df_consolidated$Infleuncer), ]
    top_influencers =(merge(data, df, by = 'Infleuncer',sort=F))
    names(top_influencers) = c("Infleuncer","Total_mscore","POST_URL","PROFILE_URL","MSCORE")
    top_influencers$MSCORE =
    format(round(top_influencers$MSCORE, 2))
    top_influencers$Total_mscore=NULL
    #top_influencers=df_consolidated[!duplicated(df_consolidated$Infleuncer), ]
    top10 <-  top_influencers[1:10,] 
    return (top10)
  })
  
  reactive_get_influencer1_stats<<-reactive({
    
    # FB_Summary_Data = reactive_FB_Summary_Stats()
    TW_Page_Data = reactive_TW_Page_Data()
    IG_Page_Data = reactive_IG_Page_Data()
    IG_summary_Data = reactive_IG_Summary_Data()
    # FB_Page_Data = reactive_FB_Page_Data()
    
    top10 = reactive_return_getTop10Infleuncer()
    infleuncer_name =  as.character(top10[1,1])
    #df=get_stats_df(FB_Summary_Data,TW_Page_Data,IG_Page_Data,IG_summary_Data,FB_Page_Data,infleuncer_name)
    df=get_stats_df_JP(TW_Page_Data,IG_Page_Data,IG_summary_Data,infleuncer_name)
    return (df)
    
  })
  
  reactive_get_influencer2_stats<<-reactive({
    
    # FB_Summary_Data = reactive_FB_Summary_Stats()
    TW_Page_Data = reactive_TW_Page_Data()
    IG_Page_Data = reactive_IG_Page_Data()
    IG_summary_Data = reactive_IG_Summary_Data()
    # FB_Page_Data = reactive_FB_Page_Data()
    
    top10 = reactive_return_getTop10Infleuncer()
    infleuncer_name =  as.character(top10[2,1])
    #df=get_stats_df(FB_Summary_Data,TW_Page_Data,IG_Page_Data,IG_summary_Data,FB_Page_Data,infleuncer_name)
    df=get_stats_df_JP(TW_Page_Data,IG_Page_Data,IG_summary_Data,infleuncer_name)
    return (df)
    
  })
  
  reactive_get_influencer3_stats<<-reactive({
    
    # FB_Summary_Data = reactive_FB_Summary_Stats()
    TW_Page_Data = reactive_TW_Page_Data()
    IG_Page_Data = reactive_IG_Page_Data()
    IG_summary_Data = reactive_IG_Summary_Data()
    # FB_Page_Data = reactive_FB_Page_Data()
    
    top10 = reactive_return_getTop10Infleuncer()
    infleuncer_name =  as.character(top10[3,1])
    #df=get_stats_df(FB_Summary_Data,TW_Page_Data,IG_Page_Data,IG_summary_Data,FB_Page_Data,infleuncer_name)
    df=get_stats_df_JP(TW_Page_Data,IG_Page_Data,IG_summary_Data,infleuncer_name)
    return (df)
    
  })
  
  reactive_get_influencer4_stats<<-reactive({
    
    # FB_Summary_Data = reactive_FB_Summary_Stats()
    TW_Page_Data = reactive_TW_Page_Data()
    IG_Page_Data = reactive_IG_Page_Data()
    IG_summary_Data = reactive_IG_Summary_Data()
    # FB_Page_Data = reactive_FB_Page_Data()
    
    top10 = reactive_return_getTop10Infleuncer()
    infleuncer_name =  as.character(top10[4,1])
    #df=get_stats_df(FB_Summary_Data,TW_Page_Data,IG_Page_Data,IG_summary_Data,FB_Page_Data,infleuncer_name)
    df=get_stats_df_JP(TW_Page_Data,IG_Page_Data,IG_summary_Data,infleuncer_name)
    return (df)
    
  })
  
  reactive_get_influencer5_stats<<-reactive({
    
    # FB_Summary_Data = reactive_FB_Summary_Stats()
    TW_Page_Data = reactive_TW_Page_Data()
    IG_Page_Data = reactive_IG_Page_Data()
    IG_summary_Data = reactive_IG_Summary_Data()
    # FB_Page_Data = reactive_FB_Page_Data()
    
    top10 = reactive_return_getTop10Infleuncer()
    infleuncer_name =  as.character(top10[5,1])
    #df=get_stats_df(FB_Summary_Data,TW_Page_Data,IG_Page_Data,IG_summary_Data,FB_Page_Data,infleuncer_name)
    df=get_stats_df_JP(TW_Page_Data,IG_Page_Data,IG_summary_Data,infleuncer_name)
    return (df)
    
  })
  
  reactive_get_influencer6_stats<<-reactive({
    # FB_Summary_Data = reactive_FB_Summary_Stats()
    TW_Page_Data = reactive_TW_Page_Data()
    IG_Page_Data = reactive_IG_Page_Data()
    IG_summary_Data = reactive_IG_Summary_Data()
    # FB_Page_Data = reactive_FB_Page_Data()
    
    top10 = reactive_return_getTop10Infleuncer()
    infleuncer_name =  as.character(top10[6,1])
    #df=get_stats_df(FB_Summary_Data,TW_Page_Data,IG_Page_Data,IG_summary_Data,FB_Page_Data,infleuncer_name)
    df=get_stats_df_JP(TW_Page_Data,IG_Page_Data,IG_summary_Data,infleuncer_name)
    return (df)
    
  })
  
  reactive_get_influencer7_stats<<-reactive({
    
    # FB_Summary_Data = reactive_FB_Summary_Stats()
    TW_Page_Data = reactive_TW_Page_Data()
    IG_Page_Data = reactive_IG_Page_Data()
    IG_summary_Data = reactive_IG_Summary_Data()
    # FB_Page_Data = reactive_FB_Page_Data()
    
    top10 = reactive_return_getTop10Infleuncer()
    infleuncer_name =  as.character(top10[7,1])
    #df=get_stats_df(FB_Summary_Data,TW_Page_Data,IG_Page_Data,IG_summary_Data,FB_Page_Data,infleuncer_name)
    df=get_stats_df_JP(TW_Page_Data,IG_Page_Data,IG_summary_Data,infleuncer_name)
    return (df)
    
  })
  
  reactive_get_influencer8_stats<<-reactive({
    
    # FB_Summary_Data = reactive_FB_Summary_Stats()
    TW_Page_Data = reactive_TW_Page_Data()
    IG_Page_Data = reactive_IG_Page_Data()
    IG_summary_Data = reactive_IG_Summary_Data()
    # FB_Page_Data = reactive_FB_Page_Data()
    
    top10 = reactive_return_getTop10Infleuncer()
    infleuncer_name =  as.character(top10[8,1])
    #df=get_stats_df(FB_Summary_Data,TW_Page_Data,IG_Page_Data,IG_summary_Data,FB_Page_Data,infleuncer_name)
    df=get_stats_df_JP(TW_Page_Data,IG_Page_Data,IG_summary_Data,infleuncer_name)
    return (df)
    
  })
  
  reactive_get_influencer9_stats<<-reactive({
    
    # FB_Summary_Data = reactive_FB_Summary_Stats()
    TW_Page_Data = reactive_TW_Page_Data()
    IG_Page_Data = reactive_IG_Page_Data()
    IG_summary_Data = reactive_IG_Summary_Data()
    # FB_Page_Data = reactive_FB_Page_Data()
    
    top10 = reactive_return_getTop10Infleuncer()
    infleuncer_name =  as.character(top10[9,1])
    #df=get_stats_df(FB_Summary_Data,TW_Page_Data,IG_Page_Data,IG_summary_Data,FB_Page_Data,infleuncer_name)
    df=get_stats_df_JP(TW_Page_Data,IG_Page_Data,IG_summary_Data,infleuncer_name)
    return (df)
    
  })
  
  reactive_get_influencer10_stats<<-reactive({
    
    # FB_Summary_Data = reactive_FB_Summary_Stats()
    TW_Page_Data = reactive_TW_Page_Data()
    IG_Page_Data = reactive_IG_Page_Data()
    IG_summary_Data = reactive_IG_Summary_Data()
    # FB_Page_Data = reactive_FB_Page_Data()
    
    top10 = reactive_return_getTop10Infleuncer()
    infleuncer_name =  as.character(top10[10,1])
    #df=get_stats_df(FB_Summary_Data,TW_Page_Data,IG_Page_Data,IG_summary_Data,FB_Page_Data,infleuncer_name)
    df=get_stats_df_JP(TW_Page_Data,IG_Page_Data,IG_summary_Data,infleuncer_name)
    return (df)
    
  })
  ########################################
  #Screen 1
  #populate the WEEK drop down
  #######################################
  reactive_week = reactive({
    
    require(input$Year)
    require(input$metric_mt)
    
    # FB_Post_Data = reactive_FB_Post_Data()
    # FB_Post_Data = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    
    TW_Post_Data = reactive_TW_Post_Data()
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    
    IG_Post_Data = reactive_IG_Post_Data()
    IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    
    # FB_Post_Data$year=as.character(lapply(strsplit(as.character(FB_Post_Data$Date), split="/"),tail, n=1))
    # FB_Post_Data=FB_Post_Data[FB_Post_Data[["year"]] == input$Year,]
    # FB_Post_Data=select(FB_Post_Data, Date)
    
    
    TW_Post_Data$year=as.character(lapply(strsplit(as.character(TW_Post_Data$Date), split="/"),tail, n=1))
    TW_Post_Data=TW_Post_Data[TW_Post_Data[["year"]] == input$Year,]
    TW_Post_Data=select(TW_Post_Data, Date)
    
    
    IG_Post_Data$year=as.character(lapply(strsplit(as.character(IG_Post_Data$Date), split="/"),tail, n=1))
    IG_Post_Data=IG_Post_Data[IG_Post_Data[["year"]] == input$Year,]
    IG_Post_Data=select(IG_Post_Data, Date)
    
    #df_fetch_week_dropdown = fetch_week_dropdown(rbind(FB_Post_Data,TW_Post_Data,IG_Post_Data))
    df_fetch_week_dropdown = fetch_week_dropdown(rbind(TW_Post_Data,IG_Post_Data))
    
    
    return (df_fetch_week_dropdown)

  })
  
  reactive_get_raw_date<<-reactive({
    df=reactive_week()
    return (df$raw_date_range)
  })
  
  reactive_get_display_date<<-reactive({
    df=reactive_week()
    return (df$date_range)
  })
  

  reactive_market_ps<<-reactive({
    
    # if (length(v$button_clicked)==0) return()
    # print(v$button_clicked)
    
    # FB_Post_Data = reactive_FB_Post_Data()
    # FB_Post_Data = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    
    TW_Post_Data = reactive_TW_Post_Data()
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    
    IG_Post_Data = reactive_IG_Post_Data()
    IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    
    #unique_markets_FB =  unique(as.character(lapply(strsplit(as.character(FB_Post_Data$Label1), split="_"),tail, n=1)))
    unique_markets_TW =  unique(as.character(lapply(strsplit(as.character(TW_Post_Data$Label1), split="_"),tail, n=1)))
    unique_markets_IG =  unique(as.character(lapply(strsplit(as.character(IG_Post_Data$Label1), split="_"),tail, n=1)))
    # li = append(list(unique_markets_FB),list(unique_markets_TW))
    # li=append(li, list(unique_markets_IG))
    
    li = append(list(unique_markets_IG),list(unique_markets_TW))
    market_dropdown=unique(unlist(li))
    market_dropdown = market_dropdown[!is.na(market_dropdown)]
    
    
    
    
    #market_df=data.frame(market_dropdown)
    
    return (market_dropdown)
    
  })
  
  reactive_get_weekly_trend<<-reactive({
    
    IG_Page_Data = reactive_IG_Page_Data()
    IG_Post_Data = reactive_IG_Post_Data() 
    TW_Page_Data = reactive_TW_Page_Data()
    TW_Post_Data = reactive_TW_Post_Data()
    
    IG_Page_Data["Earned_Effective_Reach"] = 0.035 * IG_Page_Data["Followers"] 
    IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    
    IG_Post_Data_image_df = IG_Post_Data[grepl("image|carousel", IG_Post_Data$Type)==TRUE,]
    IG_Post_Data_image_df=merge(IG_Post_Data_image_df,IG_Page_Data, by  = c("Username","Date"))
    colnames(IG_Post_Data_image_df)[3] <- "Day"
    IG_Post_Data_image_df = IG_Post_Data_image_df[,c("Label1","Date","Earned_Effective_Reach" )]
    
    IG_Post_Data_video_df = IG_Post_Data[(grepl("video", IG_Post_Data$Type)==TRUE),]
    IG_Post_Data_video_df["Earned_Effective_Reach"] = 0.25 * IG_Post_Data_video_df["Views"] 
    IG_Post_Data_video_df = IG_Post_Data_video_df[,c("Label1","Date","Earned_Effective_Reach" )]
    IG_Post_Data_video_df=IG_Post_Data_video_df[, c("Label1","Date","Earned_Effective_Reach" )]
    IG_Post_Data_sk2 = rbind(IG_Post_Data_image_df,IG_Post_Data_video_df)
    
    #TWITTER
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    TW_Page_Data["Earned_Effective_Reach"] = 0.054 * TW_Page_Data["Followers"] 
    TW_Post_Data_image_df = TW_Post_Data[grepl(paste(c("Link","Photo","Text"), collapse = "|"), TW_Post_Data$Type)==TRUE,]
    TW_Post_Data_image_df=merge(TW_Post_Data_image_df,TW_Page_Data, by  = c("Username","Date"))
    TW_Post_Data_image_df=TW_Post_Data_image_df[, c("Label1","Date","Earned_Effective_Reach" )]
    
    df = rbind(TW_Post_Data_image_df,IG_Post_Data_sk2)
    df$Market= as.character(lapply(strsplit(as.character(df$Label1), split="_"),tail, n=1))
    unique_markets =  unique(as.character(lapply(strsplit(as.character(df$Label1), split="_"),tail, n=1)))
    
    return (df)
  })
  
  
  filter_posts<<-reactive ({
    
    all_posts          =    get_all_posts()
    influencer_name    = reactive_return_Influencer_mrp()
    metric             = reactive_return_metric_mrp()
    month              = reactive_return_Month_mrp()
    
    
    #display_count      = as.integer((unlist(strsplit(as.character(reactive_return_count_asset()), "\\s"))[1]))
    year               = reactive_return_Year_mrp()
    
    if (metric == "Earned Effective Reach"){
      metric = 4
    }else if (metric == "Sentiment"){
      metric = 8
    }else if (metric == "Engagement"){
      metric = 5
    }else if (metric == "M-Score"){
      metric = 9
    }
    
    filter_posts = subset(all_posts,all_posts$KOL == influencer_name )
    # print("INFLUENCER")
    # print(influencer_name)
    # print (nrow(filter_posts))
   
    if (nrow(filter_posts)!=0){
      filter_posts=subset(filter_posts,(as.character(lapply(strsplit(as.character(filter_posts$DATE), split="/"),tail, n=1)) == year))
      # print("YEAR")
      # print (year)
      # print (nrow(filter_posts))
      if (nrow(filter_posts)!=0){
        filter_posts = subset(filter_posts,tolower(month.abb[as.numeric(sapply(strsplit(as.character(filter_posts$DATE), split="/"),'[',2))]) == tolower(month) )
        # print("month")
        # print (nrow(filter_posts))
        # print(metric)
        # print(class(metric))
        if (nrow(filter_posts)!=0){
          filter_posts=filter_posts[rev(order(filter_posts[metric])),]
          # filter_posts = filter_posts[1:display_count,]
          # filter_posts=filter_posts[complete.cases(filter_posts), ]
        }
       }
      }
    return (filter_posts)
  })
  
  get_all_posts<<-reactive ({
    TW_Post_Data  = reactive_TW_Post_Data()
    TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
    TW_Post_Data = add_mscore(TW_Post_Data,"TW")
    TW_Post_Data$Engagement=  TW_Post_Data["Reply Count"] + TW_Post_Data["Favorite Count"] + TW_Post_Data["Retweet Count"]
    TW_Post_Data=as.data.frame(TW_Post_Data)
    TW_Post_Data$PLATFORM = "TWITTER"
    TW_Post_Data$Engagement = TW_Post_Data$Engagement$`Reply Count`
    
    #TW_Post_Data$reach = 0
    # TW_Post_Data = select(TW_Post_Data,Date,Text,Username,platform,reach,Engagement,`Favorite Count`,sentiment_keyword,mscore,`Post Image`)
    # TW_Post_Data$Engagement = TW_Post_Data$Engagement$`Reply Count`
    # names(TW_Post_Data) = c("DATE", "POST", "KOL", "PLATFORM","REACH","ENGAGEMENT","REACTIONS","SENTIMENT","M-SCORE","URL")
    TW_Page_Data = reactive_TW_Page_Data()
    TW_EER = add_EER(TW_Page_Data,TW_Post_Data,"TW")
    TW_Post_Data = merge(TW_Post_Data,TW_EER)
    TW_Post_Data = select(TW_Post_Data,Date,Text,Username,PLATFORM,
                          Earned_Effective_Reach,Engagement,`Engagement Rate`,`Favorite Count`,sentiment_keyword,mscore,`Post Image`,Time,Type)
    
    names(TW_Post_Data) = c("DATE", "POST", "KOL", "PLATFORM","EARNED EFFECTIVE REACH","ENGAGEMENT","ENGAGEMENT RATE","REACTIONS","SENTIMENT","M-SCORE","URL","TIME","TYPE")
    
    TW_Post_Data$COMMENTS = NA
      
      
    IG_Post_Data  = reactive_IG_Post_Data()
    IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
    IG_Post_Data = add_mscore(IG_Post_Data,"IG")
    IG_Post_Data$PLATFORM="INSTAGRAM"
    
    IG_Page_Data = reactive_IG_Page_Data()
    
    IG_EER = add_EER(IG_Page_Data,IG_Post_Data,"IG")
    IG_Post_Data = merge(IG_Post_Data,IG_EER)
    IG_Post_Data = select(IG_Post_Data,Date,Caption,Username,PLATFORM,Earned_Effective_Reach,Engagement,`Engagement Rate`,Likes,sentiment_keyword,mscore,`Media URL`,Time,Type,Comments)
    names(IG_Post_Data) = c("DATE", "POST", "KOL", "PLATFORM","EARNED EFFECTIVE REACH","ENGAGEMENT","ENGAGEMENT RATE","REACTIONS","SENTIMENT","M-SCORE","URL","TIME","TYPE","COMMENTS")
    
    most_recent_posts = rbind(TW_Post_Data,IG_Post_Data)
    
    height = " width=\"200\", height=\"150\"></img>"
    most_recent_posts$picture_str = paste0("\"",most_recent_posts$URL,"\"")
    most_recent_posts$IMAGE = paste("<img src=",most_recent_posts$picture_str,height)
    most_recent_posts$URL = NULL
    most_recent_posts$picture_str=NULL
    names(most_recent_posts)=c("DATE", "DESCRIPTION", "KOL", "PLATFORM","EARNED EFFECTIVE REACH","ENGAGEMENT","ENGAGEMENT RATE","REACTIONS","SENTIMENT","M-SCORE","TIME","TYPE","COMMENTS","POST")
    
    most_recent_posts=add_logo(most_recent_posts)
    most_recent_posts$PLATFORM = NULL
    names(most_recent_posts)[14] = "PLATFORM"
    #names(most_recent_posts)=c("DATE", "DESCRIPTION", "KOL", "PLATFORM","EARNED EFFECTIVE REACH","ENGAGEMENT","REACTIONS","SENTIMENT","M-SCORE","POST","PLATFORM")
    #most_recent_posts <- most_recent_posts[,c("DATE", "POST", "DESCRIPTION","KOL", "PLATFORM","EARNED EFFECTIVE REACH","ENGAGEMENT","ENGAGEMENT RATE","REACTIONS","SENTIMENT","M-SCORE","TIME","TYPE")]
    
    return(most_recent_posts)
    
  })
  
  get_historical_trend_data<<-reactive ({
    
    #data= NULL
    
    #print (v$button_clicked)
    button_number=as.numeric(unlist(strsplit(v$button_clicked,"button"))[-1])
    #print(button_number)
    
    FB_Post_Data = reactive_FB_Post_Data()
    TW_Post_Data  = reactive_TW_Post_Data()
    IG_Post_Data  = reactive_IG_Post_Data()
    
    platform_type = input$platform_ht
    metric = input$metric_ht
    post_type = input$typeofposts_ht
    month = input$month_ht
    year = input$year_ht
    top10 = reactive_return_getTop10Infleuncer()
    influencer_name = as.character(top10[button_number,1])
    
    # print (platform_type)
    # print(metric)
    # print(post_type)
    # print(month)
    # print(year)
    # print(influencer_name)
    # 
    
    #############################logic begins##################################
    # 
    # if (platform_type == 'Facebook'){
    #   data= NULL
    #   FB_Post_Data = subset(FB_Post_Data,FB_Post_Data$Name == influencer_name )
    #   if (nrow(FB_Post_Data)!=0){
    #     FB_Post_Data=subset(FB_Post_Data, as.numeric(sapply(strsplit(as.character(FB_Post_Data$Date), split="/"),tail, n=1)) == year )
    #     if (nrow(FB_Post_Data)!=0){
    #       FB_Post_Data = subset(FB_Post_Data,tolower(month.abb[as.numeric(sapply(strsplit(as.character(FB_Post_Data$Date), split="/"),'[',2))]) == tolower(month) )
    #   
    #   if (nrow(FB_Post_Data)!=0){
    #     
    #     # print(nrow(FB_Post_Data))
    #       if (metric == 'Reach'){
    #         if (post_type == 'All'){
    #           
    #           data = FB_Post_Data[,c("Date","post_impressions_unique")]
    #           
    #         }else if (post_type == 'SK-II Posts'){
    #           subset = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    #           if (nrow(subset)!=0){
    #             data = subset[,c("Date","post_impressions_unique")]
    #           }
    #           
    #         }else if(post_type == 'Non SK-II Posts'){
    #           subset = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==FALSE,]
    #           if (nrow(subset)!=0){
    #             data = subset[,c("Date","post_impressions_unique")]
    #           }
    #         }
    #         
    #       }
    #       else if (metric == 'Engagement'){
    #         if (post_type == 'All'){
    #           data = FB_Post_Data[,c("Date","Engagement")]
    #         }else if (post_type == 'SK-II Posts'){
    #           subset = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    #           if (nrow(subset)!=0){
    #             data = subset[,c("Date","Engagement")]
    #           }
    #         }else if(post_type == 'Non SK-II Posts'){
    #           subset = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==FALSE,]
    #           if (nrow(subset)!=0){
    #             data = subset[,c("Date","Engagement")]
    #           }
    #         }
    #         
    #       }
    #       else if (metric == 'Engagment Rate')
    #       {
    #         
    #         if (post_type == 'All'){
    #           data = FB_Post_Data[,c("Date","Engagement Rate")]
    #         }else if (post_type == 'SK-II Posts'){
    #           subset = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    #           if (nrow(subset)!=0){
    #             data = subset[,c("Date","Engagement Rate")]
    #           }
    #         }else if(post_type == 'Non SK-II Posts'){
    #           subset = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==FALSE,]
    #           if (nrow(subset)!=0){
    #             data = subset[,c("Date","Engagement Rate")]
    #           }
    #         }
    #         
    #       }
    #       else if (metric == 'Sentiment'){
    #         
    #         if (post_type == 'All'){
    #           data = FB_Post_Data[,c("Date","sentiment")]
    #           
    #         }else if (post_type == 'SK-II Posts'){
    #           subset = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    #           if (nrow(subset)!=0){
    #             data = subset[,c("Date","sentiment")]
    #           }
    #           
    #         }else if(post_type == 'Non SK-II Posts'){
    #           subset = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==FALSE,]
    #           if (nrow(subset)!=0){
    #             data = subset[,c("Date","sentiment")]
    #           }
    #           
    #         }
    #         
    #       }
    #       else if (metric == 'M-Score'){
    #         FB_Post_Data = add_mscore(FB_Post_Data,"FB")
    #         if (post_type == 'All'){
    #           data = FB_Post_Data[,c("Date","mscore")]
    #         }else if (post_type == 'SK-II Posts'){
    #           subset = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==TRUE,]
    #           if (nrow(subset)!=0){
    #             data = subset[,c("Date","mscore")]
    #           }
    #           
    #         }else if(post_type == 'Non SK-II Posts'){
    #           subset = FB_Post_Data[grepl("sk2", FB_Post_Data$Label1)==FALSE,]
    #           if (nrow(subset)!=0){
    #             data = subset[,c("Date","mscore")]
    #           }
    #         }
    #         
    #       }
    #   }}}
    #   
    #   # print ("FACEBOOK")
    #   # print(nrow(data))
    #   
    # }
   if(platform_type == 'Twitter'){
      data= NULL
      TW_Post_Data = subset(TW_Post_Data,TW_Post_Data$Username == influencer_name )
      if (nrow(TW_Post_Data) !=0){
      TW_Post_Data=subset(TW_Post_Data, as.numeric(sapply(strsplit(as.character(TW_Post_Data$Date), split="/"),tail, n=1)) == year )
      if (nrow(TW_Post_Data) !=0){
      TW_Post_Data = subset(TW_Post_Data,tolower(month.abb[as.numeric(sapply(strsplit(as.character(TW_Post_Data$Date), split="/"),'[',2))]) == tolower(month) )
      print("Twitter")
      # print (nrow(TW_Post_Data))
      if (nrow(TW_Post_Data) !=0){
        
          if (metric == 'Earned Effective Reach'){
            if (post_type == 'All'){
              
              
              
            }else if (post_type == 'SK-II Posts'){
              
              TW_Page_Data = reactive_TW_Page_Data()
              #TW_Post_Data = reactive_TW_Post_Data()
              TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
              data= get_EER_twitter(TW_Page_Data ,TW_Post_Data)
              
            }else if(post_type == 'Non SK-II Posts'){
              TW_Page_Data = reactive_TW_Page_Data()
              #TW_Post_Data = reactive_TW_Post_Data()
              TW_Post_Data = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==FALSE,]
              data= get_EER_twitter(TW_Page_Data ,TW_Post_Data)
            }
            
          }
          else if (metric == 'Engagement'){
            TW_Post_Data$Engagement =sum(TW_Post_Data$`Reply Count`,TW_Post_Data$`Favorite Count`,TW_Post_Data$`Retweet Count`)
            
            if (post_type == 'All'){
              data = TW_Post_Data[,c("Date","Engagement")]
            }else if (post_type == 'SK-II Posts'){
              subset = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
              # print (nrow(subset))
              if (nrow(subset)!=0){
                data = subset[,c("Date","Engagement")]
              }
            }else if(post_type == 'Non SK-II Posts'){
              subset = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==FALSE,]
              if (nrow(subset)!=0){
                data = subset[,c("Date","Engagement")]
              }
            }
          }
            
          
          else if (metric == 'Engagement Rate')
          {
            if (post_type == 'All'){
              data = TW_Post_Data[,c("Date","Engagement Rate")]
            }else if (post_type  == 'SK-II Posts'){
              subset = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
              if (nrow(subset)!=0){
                data = select(subset,Date,`Engagement Rate`)
                data=as.data.frame(data)
              }
            }else if(post_type  == 'Non SK-II Posts'){
              if (nrow(subset)!=0){
                subset = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==FALSE,]
              }
              data = select(subset,Date,`Engagement Rate`)
              data=as.data.frame(data)
            }}
            
          
          else if (metric == 'Sentiment'){
            
            if (post_type == 'All'){
              data = TW_Post_Data[,c("Date","Sentiment")]
              
            }else if (post_type == 'SK-II Posts'){
              subset = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
              if (nrow(subset)!=0){
                data = subset[,c("Date","Sentiment")]
              }
              
            }else if(post_type == 'Non SK-II Posts'){
              
              subset = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==FALSE,]
              if (nrow(subset)!=0){
                data = subset[,c("Date","Sentiment")]
              }
              
            }
            
          }
          else if (metric == 'M-Score'){
            TW_Post_Data = add_mscore(TW_Post_Data,"TW")
            if (post_type == 'All'){
              data = TW_Post_Data[,c("Date","mscore")]
            }else if (post_type == 'SK-II Posts'){
              subset = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==TRUE,]
              if (nrow(subset)!=0){
                data = subset[,c("Date","mscore")]
              }
            }else if(post_type == 'Non SK-II Posts'){
              subset = TW_Post_Data[grepl("sk2", TW_Post_Data$Label1)==FALSE,]
              if (nrow(subset)!=0){
                data = subset[,c("Date","mscore")]
              }
            }
            
          }
          }
      }}
      
      }
      # print ("TWITTER")
      # print(nrow(data))
    
    else if
    (platform_type == 'Instagram'){
      data=NULL
      IG_Post_Data = subset(IG_Post_Data,IG_Post_Data$Username == influencer_name )
      if (nrow(IG_Post_Data) !=0){
      IG_Post_Data=subset(IG_Post_Data, as.numeric(sapply(strsplit(as.character(IG_Post_Data$Date), split="/"),tail, n=1)) == year )
      if (nrow(IG_Post_Data) !=0){
      IG_Post_Data = subset(IG_Post_Data,tolower(month.abb[as.numeric(sapply(strsplit(as.character(IG_Post_Data$Date), split="/"),'[',2))]) == tolower(month) )
      
      if (nrow(IG_Post_Data) !=0){
        
        if (metric == 'Earned Effective Reach'){
          IG_Page_Data = reactive_IG_Page_Data()
          #IG_Post_Data = reactive_IG_Post_Data() 
          if (post_type == 'All'){
          }else if (post_type == 'SK-II Posts'){
            IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
            data= get_EER_instagram(IG_Page_Data ,IG_Post_Data)
          }else if(post_type == 'Non SK-II Posts'){
            IG_Post_Data = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==FALSE,]
            data= get_EER_instagram(IG_Page_Data ,IG_Post_Data)
          }
          
        }
        else if (metric == 'Engagement'){
          
          if (post_type == 'All'){
            data = IG_Post_Data[,c("Date","Engagement")]
          }else if (post_type == 'SK-II Posts'){
            subset = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
            if (nrow(subset)!=0){
              data = subset[,c("Date","Engagement")]
            }
          }else if(post_type == 'Non SK-II Posts'){
            subset = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==FALSE,]
            if (nrow(subset)!=0){
              data = subset[,c("Date","Engagement")]
            }
          }
          
        }
        else if (metric == 'Engagement Rate')
        {
          
          if (post_type == 'All'){
            #data = IG_Post_Data[,c("Date","Engagement Rate")]
            data = select(IG_Post_Data,Date,`Engagement Rate`)
          }else if (post_type  == 'SK-II Posts'){
            subset = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
            if (nrow(subset)!=0){
              data = select(subset,Date,`Engagement Rate`)
            }
          }else if(post_type  == 'Non SK-II Posts'){
            if (nrow(subset)!=0){
              subset = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==FALSE,]
            }
            data = select(subset,Date,`Engagement Rate`)
          }
          
        }
        else if (metric == 'Sentiment'){
          
          if (post_type == 'All'){
            data = IG_Post_Data[,c("Date","Sentiment")]
            
          }else if (post_type == 'SK-II Posts'){
            subset = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
            if (nrow(subset)!=0){
              data = subset[,c("Date","Sentiment")]
            }
            
          }else if(post_type == 'Non SK-II Posts'){
            
            subset = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==FALSE,]
            if (nrow(subset)!=0){
              data = subset[,c("Date","Sentiment")]
            }
            
          }
          
        }
        else if (metric == 'M-Score'){
          IG_Post_Data = add_mscore(IG_Post_Data,"IG")
          if (post_type == 'All'){
            data = IG_Post_Data[,c("Date","mscore")]
            
          }else if (post_type == 'SK-II Posts'){
            subset = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==TRUE,]
            if (nrow(subset)!=0){
              data = subset[,c("Date","mscore")]
            }
            
          }else if(post_type == 'Non SK-II Posts'){
            subset = IG_Post_Data[grepl("sk2", IG_Post_Data$Label1)==FALSE,]
            if (nrow(subset)!=0){
              data = subset[,c("Date","mscore")]
            }
            
          }
          
        }
      }}
      }
      
    }
    
    if(is.null(data)) { return() }
    data$Date=gsub("/", "-", data$Date)
    data$Date=dmy(data$Date)
    data=data[ order(data$Date , decreasing = TRUE ),]
    data$Date=gsub("-", "/", data$Date)
    return (data)
   
    
    
  })
  

  
  ###### reactive data #####
  
})



