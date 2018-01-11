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
  dashboardHeader(title = div(img(src="logo.jpg",height = 50, width = 100)),titleWidth = 180),
  dashboardSidebar(width = 180,
                   sidebarMenu(
                     #menuItem("Upload Input Files", tabName = "upload", icon = icon("upload")),
                     menuItem("Summary", tabName = "summary", icon = icon("th")),
                     menuItem("Influencer Drill Down", tabName = "influencer", icon = icon("user-circle-o")),
                     menuItem("Post Drill Down", tabName = "post_drill_down", icon = icon("address-card-o"))
                   )
  ),
  dashboardBody(
    # tags$style(type="text/css",
    #            ".shiny-output-error { visibility: hidden; }",
    #            ".shiny-output-error:before { visibility: hidden; }"
    # ),
    tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #F10F53;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #F10F53;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #F10F53;
                              }        
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #F9F7F7;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #F9F7F7;
                              color:#F10F53
                              }
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #F9F7F7;
                              color: #777777;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #ff69b4;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #F10F53;
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
              fluidRow(
                column(5,h3(strong("WEEKLY TOPLINE"),style = "color: #BE9200;")),
                column(2,offset=2,selectInput("market_wt", "", choices = c("Japan"),width="120px")),
                column(3,htmlOutput("summary_daterange_selector"))
                ),
              box(title=strong("TOP INFLUENCER"),uiOutput("top_infleuncer_image"),solidHeader = TRUE,width = 3,height=250),
              
              
              box(title=strong("TOP POST"),uiOutput("top_post_image"),solidHeader = TRUE,width = 3,height=250),
              
              
              #box(title=strong("TOTAL "),div(id="reach_text",h1(textOutput("display_total_reach")),style = "color: #E1005D;"),solidHeader = TRUE,width = 3,height=200 ),
              uiOutput("EER_box"),
              uiOutput("Engagment_box"),
              
              #bsTooltip(id="reach_text","suteja",placement = "bottom", trigger = "hover", options = list(container = "body")),
              
              #box(title=strong("TOTAL ENGAGEMENT"),h1(textOutput("display_total_engagement"),style = "color: #E1005D;"),solidHeader = TRUE,width = 3,height=200),
              br(),
              h6(em(textOutput("top_platform"))),
              h6(em("**Top influencer and top post are chosen based on the highest value of Earned Effective Reach")),
              fluidRow(
                column(4,h3(strong("MARKET TREND"),style = "color: #BE9200;")),
                #column(3,selectInput("Type", "Type", choices = c("reach"))),
                column(3,uiOutput('select_metric')),
                column(3,uiOutput('select_week')),
                column(2,uiOutput('select_year'))
              ), 
              
              #h2(textOutput("influencer_image_url")),
              
              plotlyOutput("barplot_markettrend",height = 370),
              
              
              
              fluidRow(
                column(6, h3(strong("POST SUMMARY"),style = "color: #BE9200;")),
                column(2,uiOutput('select_market')),
                column(2,uiOutput('select_month')),
                column(2,uiOutput('select_year_ps'))
              ),
              DT::dataTableOutput('post_summary_table'),
              
              fluidRow(
                column(6, h3(strong("RESPONSE COMMENTS"),style = "color: #BE9200;")),
                column(3,uiOutput('select_platform')),
                #column(3,htmlOutput("daterange_selector"))
                column(3,dateRangeInput('dateRange',
                               label = '',
                               start = Sys.Date(), end = Sys.Date() + 6
                ))
                
              ),
              DT::dataTableOutput('test_table')
              
              # h3(strong("WEEKLY TREND"),style = "color: #BE9200;"),
              # 
              # fluidRow(
              #   column(3,selectInput("Market_2", "Market", choices = c("Japan"),width="120px")),
              #   column(3,selectInput("Type_2", "Type", choices = c("reach"),width="120px")),
              #   column(3,selectInput("Week_2", "Week", choices = names(mtcars),width="120px")),
              #   column(3,selectInput("Year_2", "Year", choices = names(mtcars),width="120px"))
              # )
              
              
              
      ),
      tabItem(tabName = "influencer",
              #h3("WEEKLY TOPLINE",style = "font-family: 'Lobster', cursive;font-weight: 500; line-height: 1.1; color: #4d3a7d;")
              h3(strong("INFLUENCER UNIVERSE"),style = "color: #BE9200;"),
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
                column(2,h5(textOutput("influencer_name1"))),
                column(2,h5(textOutput("influencer_name2"))),
                column(2,h5(textOutput("influencer_name3"))),
                column(2,h5(textOutput("influencer_name4"))),
                column(2,h5(textOutput("influencer_name5")))
              ),
              fluidRow(
                column(2,div(h5(textOutput("influencer_mscore1")),style = "color: #E1005D;")),
                column(2,div(h5(textOutput("influencer_mscore2")),style = "color: #E1005D;")),
                column(2,div(h5(textOutput("influencer_mscore3")),style = "color: #E1005D;")),
                column(2,div(h5(textOutput("influencer_mscore4")),style = "color: #E1005D;")),
                column(2,div(h5(textOutput("influencer_mscore5")),style = "color: #E1005D;"))
              ),
              fluidRow(
                
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
                column(2,div(h5(textOutput("influencer_mscore6")),style = "color: #E1005D;")),
                column(2,div(h5(textOutput("influencer_mscore7")),style = "color: #E1005D;")),
                column(2,div(h5(textOutput("influencer_mscore8")),style = "color: #E1005D;")),
                column(2,div(h5(textOutput("influencer_mscore9")),style = "color: #E1005D;")),
                column(2,div(h5(textOutput("influencer_mscore10")),style = "color: #E1005D;"))
              ),
              
              br(),br(),
              fluidRow(
                column(2,uiOutput("selected_influencer_image")),
                tags$head(tags$style("#selected_influencer_details{color: #E1005D;}") ),
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
                column(2,h3(strong(textOutput("title_historical_trend")),style = "color: #BE9200;")),
                column(2,uiOutput('display_platform')),
                column(2,uiOutput('display_typeofposts')),
                column(2,uiOutput('display_dimension')),
                column(2,uiOutput('display_month')),
                column(2,uiOutput('display_year'))
              ),
              plotlyOutput("historical_trend_chart"),
              h3(strong(textOutput("title_sk2_post")),style = "color: #BE9200;"),
              DT::dataTableOutput('sk2_posts_table'),
              h3(strong(textOutput("title_top_comments")),style = "color: #BE9200;"),
              DT::dataTableOutput('top_comments_table')
              
              
      ),
      
      tabItem(tabName = "post_drill_down",
              h3(strong("MOST RECENT POSTS"),style = "color: #BE9200;"),
              # tags$head(tags$style("#table1  {wrap;  }")),
              
              DT::dataTableOutput('most_recent_post_table'), br(),br(),
              fluidRow(
                #column(1,h6(strong("show"))),
                column(1,offset=5,uiOutput('select_entries_mrp')),
                column(2,uiOutput('select_infleuncer_mrp')),
                column(2,uiOutput('select_metric_mrp')),
                column(1,uiOutput('select_month_mrp')),
                column(1,uiOutput('select_year_mrp'))
              ),
              br(),
              uiOutput("dynamic_UI_box")
              # fluidRow(
              #   column(4,box(title=strong("TOP INFLUENCER"),solidHeader = TRUE,width = 200,height=300)),
              #   column(4,box(title=strong("TOP INFLUENCER"),solidHeader = TRUE,width = 200,height=300)),
              #   column(4,box(title=strong("TOP INFLUENCER"),solidHeader = TRUE,width = 200,height=300))
              # ),
              # fluidRow(
              #   column(4,box(title=strong("TOP INFLUENCER"),solidHeader = TRUE,width = 200,height=300)),
              #   column(4,box(title=strong("TOP INFLUENCER"),solidHeader = TRUE,width = 200,height=300)),
              #   column(4,box(title=strong("TOP INFLUENCER"),solidHeader = TRUE,width = 200,height=300))
              # )
              
              
              
              
              
      )
      
      
      
      
      
    )
    
    
    )
  
    )
    