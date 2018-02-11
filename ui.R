library(shiny)
library(shinydashboard)

#defaultPlotlyConfiguration = { modeBarButtonsToRemove: ['sendDataToCloud', 'autoScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'lasso2d', 'select2d'], displaylogo: false, showTips: true };

# value = textOutput("tooltip_EER")
# print(value)
# value = as.character(value)
# val=unlist(strsplit(unlist(strsplit(value," "))[2],"="))[2]
# print("VALUE")
# print(as.character(value))



dashboardPage(
  #dashboardHeader(title = div(img(src="logo.jpg",height = 50, width = 100), "KOL & POST TRACKER"),titleWidth = 360),
  dashboardHeader(title = div(img(src="logo.jpg",height = 55, width = 160)),titleWidth = 180),
  dashboardSidebar(width = 180,
                   sidebarMenu(
                     #menuItem("Upload Input Files", tabName = "upload", icon = icon("upload")),
                     menuItem("Summary", tabName = "summary", icon = icon("th")),
                     menuItem("Post Drill Down", tabName = "post_drill_down", icon = icon("address-card-o")),
                     menuItem("Influencer Drill Down", tabName = "influencer", icon = icon("user-circle-o"))
                     
                     )
  ),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #E0005d;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #E0005d;
                              }
                              .skin-blue{
                              background-color: #F2F2F2;
                              }

                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #E0005d;
                              }        
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #F9F7F7;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #F2F2F2;
                              color:#E0005d
                              }
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #F2F2F2;
                              color: #777777;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #ff69b4;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #E0005d;
                              }

                              .content-wrapper,
                              .right-side ,
                              .skin-blue .wrapper{
                              #background-color: #F7F6F6;
                              background-image:url(background-image.jpg);
                              }
                              
                              
                              # /* toggle button when hovered  */                    
                              # .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              # background-color: #ff69b4;
                              # }
                              
                              .selectize-input {
                              white-space: nowrap;
                              }
                              .selectize-dropdown {
                              width: 600px !important;
                              }


                              #demographic {
                                  text-align: center;
                              }

                              #display_total_reach{
                                  text-align: center;
                              }

                              #display_total_engagement{
                                  text-align: center;
                              }

                              #likes_comments_shares{
                                  text-align: center;
                              }

                              div.box-header {
                                  text-align: center;
                              }

                              
                              
                             #barplot_markettrend,#post_summary_table,#historical_trend_chart,
                             #sk2_posts_table,#top_comments_table,#most_recent_post_table,#response_comments_table
                              {
                                 # box-sizing: border-box;
                                #width: 270px;
                                height: 300px;
                                #padding: 30px;
                                border-width:2px;
                                border-style:solid;
                                overflow: auto;
                                border-color: #D3D3D3;
                                padding-right: 1cm;
                                padding-left: 1cm;
                                background: white;

                              }
                           #barplot_markettrend{
                                padding-left: 40px;
                              }

                              hr {
                                  display: block;
                                  height: 0px;
                                  border: 0;
                                  border-top: 1px solid #E0005d;
                                  margin: 0em 0;
                                  padding: 0;
                                  padding-top:40px; 
                                  padding-left: 1cm;
                              }

                              .box{
                                background-color: #FFF3CC;
                              }
                              .dynamic_box,#dynamic_UI_box > div > div > div
                              {
                                background-color: #FFFFFF;
                              }
                              


                              .top-post-img, .top-infl-img {
                                    border: 25px solid #F2F2F2;
                                    border-colour : #F2F2F2;
                                    background-color: #FFFFFF;
                                    height: 100px;
                                    border-radius:700%;
                                    width: 200px;
                                    height: 180px;
                              }

                              .select-infl-img{
                                    border: 0px solid lightgray;
                                    border-radius: 50%;
                                    padding-top:20px;
                              }

                            #   #reach_text{
                            #       border: 35px solid lightgray;
                            #        #border-radius: 25px;
                            #       background-color: lightgray;
                            #       height: 100px;
                            #       border-radius:700%;
                            #       width: 200px;
                            #       height: 180px;
                            # 
                            #       text-align: center;
                            #       position: relative;
                            # 
                            # }

                             
#tip{
padding-left: 3cm;
padding-bottom:50cm;
}
#mydiv
{ 
  line-height: 80%;
  font-size:20px;

}
#mydiv a
{
  font-size:13px;
  color:black;
  #font-weight:bold;
}
#mydiv b{
  font-weight:bold;
}
#mydiv name{
  #font-weight:bold;
  color:black;
  font-size:14px;
}

div.myRow1{

  #box-sizing: border-box;
 width: 1200px;
height: 190px;
#padding: 30px;
border-width:2px;
border-style:solid;
overflow: auto;
border-color: #D3D3D3;
outline-offset:50px; 

}
div.summary_row{

  #box-sizing: border-box;
   width: 1200px;
  height: 270px;
  #padding: 30px;
  border-width:2px;
  border-style:solid;
  overflow: auto;
  border-color: #FFF3CC;
  outline-offset:50px; 
background-color:#FFF3CC;


}

div#selected_influencer_details,div#twitter_image,div#instagram_image,
div#instagram_details,div#twitter_details{
 padding-top:40px;
}

div#twitter_image,div#instagram_image{
padding-left:30px;
}

#summary_daterange > div > input[type="text"]:nth-child(1),#summary_daterange > div > input[type="text"]:nth-child(3),
#select_daterange_inf > div,#select_daterange_inf > div > input[type="text"]:nth-child(1),#select_daterange_inf > div > input[type="text"]:nth-child(3){
height:34px;
}


html *{
   font-family:Roboto Condensed, sans-serif;
}

div#historical_trend_chart{
width:5%;
height:10%
}

img.selected_influencer_image{
  border-radius: 90%;
  top: 23px;
  position: relative;
}
div#mscore_text{
background-color: #FFC6DE;
}

                             
                              
                              '))),
    
    
    tabItems(
      # tabItem(tabName = "upload",
      #         h2("Admin Access Only!!"),
      #         h2("Please upload all the input files"),
      #         fluidRow(
      #           box(
      #             fileInput("input_file_upload", "Choose the input data File",accept = c(".xlsx"),multiple=TRUE)
      #           )
      #         ),
      #         actionButton("button_view_file", "View Old Files"),
      #         #display the file names
      #         verbatimTextOutput("test")
      #         
      # ),
      tabItem(tabName = "summary",
              h3(strong("KOL AND POST TRACKER"),style = "padding-right: 10cm;color: #E1005D;font-family: 'Roboto Condensed', sans-serif;"),
              HTML('<hr>'),
              fluidRow(
                style = "padding-left:10px",
                column(5,h3(strong("TOPLINE"),style = "color: #BE9200;font-family: 'Roboto Condensed', sans-serif;font-size:24px")),
                column(2,offset=2,selectInput("market_wt", "", choices = c("Japan"),width="120px")),
                column(3,htmlOutput("summary_daterange_selector"))
                ),
             # box(title=strong("TOP INFLUENCER"),uiOutput("top_infleuncer_image"),solidHeader = TRUE,width = 3,height=250),
              #box(title=strong("TOP POST"),uiOutput("top_post_image"),solidHeader = TRUE,width = 3,height=250),
             box( title = uiOutput("top_influencer_text"),style = "font-family: 'Roboto Condensed', sans-serif;",uiOutput("top_infleuncer_image"),solidHeader = TRUE,width = 3,height=270),
             box(title = uiOutput("top_post_text"),style = "font-family: 'Roboto Condensed', sans-serif;",uiOutput("top_post_image"),solidHeader = TRUE,width = 3,height=270),
             uiOutput("EER_box"),
             uiOutput("Engagment_box"),
             # fluidRow(
             #      class = "summary_row",
             #      style = "padding-left:20px",
             #      box(title = uiOutput("top_influencer_text"),style = "font-family: 'Roboto Condensed', sans-serif;",uiOutput("top_infleuncer_image"),solidHeader = TRUE,width = 3,height=270),
             #      box(title = uiOutput("top_post_text"),style = "font-family: 'Roboto Condensed', sans-serif;",uiOutput("top_post_image"),solidHeader = TRUE,width = 3,height=270),
             #      uiOutput("EER_box"),
             #      uiOutput("Engagment_box")
             # ),
             
              #bsTooltip(id="reach_text","suteja",placement = "bottom", trigger = "hover", options = list(container = "body")),
              
              #box(title=strong("TOTAL ENGAGEMENT"),h1(textOutput("display_total_engagement"),style = "color: #E1005D;"),solidHeader = TRUE,width = 3,height=200),
              # h6(em(textOutput("top_platform"))),
              # h6(em("**All reach is aggregated by Sainsbury")),
              # h6(em("**Top influencer is based on the highest value of Total Average Reach")),
              # h6(em("**Top post is based on the highest value of Earned Effective Reach")),
              
             
             br(),br(),
             fluidRow(
                style = "padding-top:300px;padding-left:10px",
                column(4,h3(strong("MARKET TREND"),style = "color: #BE9200;font-family: 'Roboto Condensed', sans-serif;")),
                #column(3,selectInput("Type", "Type", choices = c("reach"))),
                column(offset=6,2,uiOutput('select_metric'))
                #column(3,uiOutput('select_week')),
                #column(3,htmlOutput("market_daterange_selector"))
                #column(2,uiOutput('select_year'))
              ), 
              
              #h2(textOutput("influencer_image_url")),
              style = "padding-left:20px",
              plotlyOutput("barplot_markettrend",height = 370),
              
             
              
              fluidRow(
                style = "padding-top:70px;padding-left:10px",
                column(3, h3(strong("POST SUMMARY"),style = "color: #BE9200;font-family: 'Roboto Condensed', sans-serif;")),
                
                #column(2,downloadButton("downloadData", "Export")),
                #column(offset=6,2,uiOutput('select_market')),
                #column(2,uiOutput('select_month')),
                #column(2,uiOutput('select_year_ps')),
                column(offset=7,2,downloadButton("downloadData", "Export")),
                #tags$style(type='text/css', "#select_market { width:100%; margin-top: 25px;}"),
                tags$style(type='text/css', "#downloadData { width:100%; margin-top: 20px;}")
              ),
             br(),
             #tags$head(tags$style("#post_summary_table {white-space: nowrap;}")),
             style = "padding-top:70px;padding-left:10px",
              DT::dataTableOutput('post_summary_table'),
              h1(strong("")),
              fluidRow(
                style = "padding-top:70px;padding-left:10px",
                column(3, h3(strong("RESPONSE COMMENTS"),style = "color: #BE9200;font-family: 'Roboto Condensed', sans-serif;")),
                column(offset=5,2,uiOutput('select_platform')),
                #column(3,htmlOutput("daterange_selector"))
                # column(3,dateRangeInput('dateRange',
                #                label = '',
                #                start = Sys.Date(), end = Sys.Date() + 6
                # )),
                column(2,downloadButton("downloadData_responsecomments", "Export")),
                tags$style(type='text/css', "#downloadData_responsecomments { width:100%; margin-top: 20px;}")
                
              ),
             style = "padding-left:40px",
              DT::dataTableOutput('response_comments_table')
              
              # h3(strong("WEEKLY TREND"),style = "color: #BE9200;"),
              # 
              # fluidRow(
              #   column(3,selectInput("Market_2", "Market", choices = c("Japan"),width="120px")),
              #   column(3,selectInput("Type_2", "Type", choices = c("reach"),width="120px")),
              #   column(3,selectInput("Week_2", "Week", choices = names(mtcars),width="120px")),
              #   column(3,selectInput("Year_2", "Year", choices = names(mtcars),width="120px"))
              # )
              
              
              
      ),
      tabItem(tabName = "post_drill_down",
              h3(strong("KOL AND POST TRACKER"),style = "padding-right: 10cm;color: #E1005D;font-family: 'Roboto Condensed', sans-serif;"),
              HTML('<hr>'),
              br(),
              fluidRow(
                style = "padding-left:10px",
                column(3,h3(strong("MOST RECENT POSTS"),style = "color: #BE9200;font-family: 'Roboto Condensed', sans-serif;")),
                column(offset=1,1,uiOutput('market_mostrecentposts')),
                column(2,uiOutput('type_mostrecentposts')),
                column(3,uiOutput('period_mostrecentposts')),
                column(2,downloadButton("downloadData_postdrilldown", "Export")),
                tags$style(type='text/css', "#downloadData_postdrilldown { width:100%; margin-top: 20px;}")
              ),
              # tags$head(tags$style("#table1  {wrap;  }")),
              br(),
              DT::dataTableOutput('most_recent_post_table'), br(),br(),
              fluidRow(
                #column(1,h6(strong("show"))),
                column(1,offset=7,uiOutput('select_entries_mrp')),
                column(2,uiOutput('select_infleuncer_mrp')),
                column(2,uiOutput('select_metric_mrp'))
                #column(1,uiOutput('select_month_mrp')),
                #column(1,uiOutput('select_year_mrp'))
              ),
              br(),
              div(uiOutput("dynamic_UI_box"))
              
      ),
      tabItem(tabName = "influencer",
              h3(strong("KOL AND POST TRACKER"),style = "padding-right: 10cm;position: absolute;margin-top: -30px; color: #E1005D;font-family: 'Roboto Condensed', sans-serif;"),
              HTML('<hr>'),
              br(),
              
              fluidRow(
                style = "padding-left:10px",
                column(4,h3(strong("INFLUENCER UNIVERSE"),style = "color: #BE9200;font-family: 'Roboto Condensed', sans-serif;")),
                column(offset=1,2,uiOutput("select_market_inf")),
                column(3,uiOutput("select_daterange_inf")),
                column(2,uiOutput("select_metric_inf"))
              ),
              br(),br(),
              useShinyjs(),
              #uiOutput("infleuncerImage2", height = 300),
              #tags$div(id = 'placeholder') 
              # tags$head(
              
              tags$style(HTML("
                              .round {
                              display:block;
                              height: 160px;
                              width: 160px;
                              border-radius: 100%;
                              border: 1px black;
                              
                              
                              }
                              
                              ")),
              fluidRow(
                style = "padding-left:40px",
                column(2, tags$button(
                  id = "button1",
                  class="round action-button",
                  uiOutput("infleuncerImage1")
                  
                )),
                column(2, tags$button(
                  id = "button2",
                  class="round action-button",
                  uiOutput("infleuncerImage2")
                  
                )),
                column(2, tags$button(
                  id = "button3",
                  class="round action-button",
                  uiOutput("infleuncerImage3")
                  
                )),
                column(2, tags$button(
                  id = "button4",
                  class="round action-button",
                  uiOutput("infleuncerImage4")
                  
                )),
                column(2, tags$button(
                  id = "button5",
                  class="round action-button",
                  uiOutput("infleuncerImage5")
                  
                ))
              ),
              fluidRow(
                style = "padding-left:40px",
                column(2,h5(textOutput("influencer_name1"))),
                column(2,h5(textOutput("influencer_name2"))),
                column(2,h5(textOutput("influencer_name3"))),
                column(2,h5(textOutput("influencer_name4"))),
                column(2,h5(textOutput("influencer_name5")))
              ),
              fluidRow(
                style = "padding-left:40px",
                column(2,div(h5(textOutput("influencer_mscore1")),style = "color: #E10051;")),
                column(2,div(h5(textOutput("influencer_mscore2")),style = "color: #E10051;")),
                column(2,div(h5(textOutput("influencer_mscore3")),style = "color: #E10051;")),
                column(2,div(h5(textOutput("influencer_mscore4")),style = "color: #E10051;")),
                column(2,div(h5(textOutput("influencer_mscore5")),style = "color: #E10051;"))
              ),
              fluidRow(
                style = "padding-left:40px",
                column(2, tags$button(
                  id = "button6",
                  class="round action-button",
                  uiOutput("infleuncerImage6")
                  
                )),
                column(2, tags$button(
                  id = "button7",
                  class="round action-button",
                  uiOutput("infleuncerImage7")
                  
                )),
                column(2, tags$button(
                  id = "button8",
                  class="round action-button",
                  uiOutput("infleuncerImage8")
                  
                )),
                column(2, tags$button(
                  id = "button9",
                  class="round action-button",
                  uiOutput("infleuncerImage9")
                  
                )),
                column(2, tags$button(
                  id = "button10",
                  class="round action-button",
                  uiOutput("infleuncerImage10")
                  
                ))
              ),
              fluidRow(
                column(2,h5(textOutput("influencer_name6"))),
                column(2,h5(textOutput("influencer_name7"))),
                column(2,h5(textOutput("influencer_name8"))),
                column(2,h5(textOutput("influencer_name9"))),
                column(2,h5(textOutput("influencer_name10")))
              ),
              fluidRow(
                style = "padding-left:40px",
                column(2,div(h5(textOutput("influencer_mscore6")),style = "color: #E10051;")),
                column(2,div(h5(textOutput("influencer_mscore7")),style = "color: #E10051;")),
                column(2,div(h5(textOutput("influencer_mscore8")),style = "color: #E10051;")),
                column(2,div(h5(textOutput("influencer_mscore9")),style = "color: #E10051;")),
                column(2,div(h5(textOutput("influencer_mscore10")),style = "color: #E10051;"))
              ),
              
              br(),br(),
              fluidRow(
                class = "myRow1",
                #column(2,htmlOutput("test_this"))
                style = "padding-left:40px",
                column(2,uiOutput("selected_influencer_image")),
                tags$head(tags$style("#selected_influencer_details{color: #E10051;}") ),
                column(2,htmlOutput("selected_influencer_details")),
                #column(1,uiOutput("facebook_image")),
                #column(1,htmlOutput("facebook_details")),
                column(1,uiOutput("twitter_image")),
                column(1,htmlOutput("twitter_details")),
                column(1,uiOutput("instagram_image")),
                column(1,htmlOutput("instagram_details"))
              ),
              #dataTableOutput('influencer_stats_table'),
              
              fluidRow(
                style = "padding-top:70px;padding-left:10px",
                column(3,h3(strong(textOutput("title_historical_trend")),style = "color: #BE9200;font-family: 'Roboto Condensed', sans-serif;")),
                column(offset=3,2,uiOutput('display_platform')),
                column(2,uiOutput('display_typeofposts')),
                column(2,uiOutput('display_dimension'))
                #column(2,uiOutput('display_month')),
                #column(2,uiOutput('display_year'))
              ),
              br(),
              plotlyOutput("historical_trend_chart"),
              fluidRow(
                style = "padding-top:70px;padding-left:10px",
                column(4,h3(strong(textOutput("title_sk2_post")),style = "color: #BE9200;font-family: 'Roboto Condensed', sans-serif;")),
                #column(offset=4,2,uiOutput('display_typeofpost_post')),
                column(offset=6,2,downloadButton("downloadData_sk2posts", "Export")),
                tags$style(type='text/css', "#downloadData_sk2posts { width:100%; margin-top: 20px;}")
              ),
              br(),
              DT::dataTableOutput('sk2_posts_table'),
              fluidRow(
                style = "padding-top:70px;padding-left:10px",
                column(3,h3(strong(textOutput("title_top_comments")),style = "color: #BE9200;font-family: 'Roboto Condensed', sans-serif;")),
                column(offset=7,2,downloadButton("downloadData_topcomments", "Export")),
                tags$style(type='text/css', "#downloadData_topcomments { width:100%; margin-top: 20px;}")
              ),
              style = "padding-top:50px;",
              br(),
              DT::dataTableOutput('top_comments_table')
              
              
              )
      
      
      
      
      
    )
    
    
    )
  
    )
    