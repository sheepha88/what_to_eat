ui <- fluidPage(
  useShinyjs(),
  includeScript("www/custom.js"),

  theme = bs_theme(version = 5),


  #HTML function 지정
    tags$head(tags$script(type = "text/javascript", 
                        HTML("function getUploadTarget(value , id) {
        
        Shiny.setInputValue(value,id, {priority: 'event'});
        
        ;}"))
    ),

#    tags$head(tags$script(src="custom.js")),


    tags$style(
        HTML("
            .custom-font {
            font-size: 60px;
            font-family: 'French Script MT';
            font-weight: bold;
            text-align: center;
            background-color: #00BFE5;
            margin: 0;
            }
            .nav-container {
                dislplay: flex;
                flex-direction: row;
                width: 100%;
                margin: 0;
                padding: 0;
                list-style-type: none;
                background-color: grey;
            }
            .nav-item {
                padding: 15px;
                cursor: pointer;
            }
            .nav-item a {
                text-align: center;
                text-decoration: none;
                color: white;
            }
            .my-icon {
                color: white;
                font-size: 50px;
            }
            .refresh-icon {
                color: grey;
                font-size: 20px;
                display: flex;
            }
            .space {
                width: 4px;
                height: auto;
                display: inline-block;
            }
            .part_line {
                border-top: 1px solid #E8EBEE; 
                padding-top: 5px;
            }
        ")
    ),

    fluidRow(
        tags$div(
            tags$h1(
                "What to eat",
                class = "custom-font",
                actionButton("showNavbarButton","" , style = "float: left; background-color: #00BFE5;"  , class = "my-button" , icon = icon("bars", class = "my-icon") ),  
            ),
            tags$div(style = "background-color : #FAFBFC;",
                navbarPage(
                    id = "navbarPage",
                    title = "What to eat",
                    
                    #Home page
                    tabPanel("Home",
                        home_UI("Home")
                    ),
                    tabPanel("음식점 검색",
                        search_UI("Search")
                    ),
                    tabPanel(
                        title = "평점 & 리뷰작성",
                        value = "rating",
                        review_UI("Review")
                    ),
                    tabPanel(
                        title = "ADMIN",
                        value = "Admin",
                        Admin_UI("Admin")
                    )
                ),
                class = "navbar_class",
                # style="display: none"
            )
        ),

        # [delete]
        # actionButton(
        #     inputId = "go_to_rating",
        #     label = "Go to Rating Page",
        #     class = "btn-danger"
        # )
        
    )
)



    