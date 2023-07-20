ui <- fluidPage(
  useShinyjs(),
  
  #HTML function 지정
    tags$head(tags$script(type = "text/javascript", 
                        HTML("function getUploadTarget(value , id) {
        
        Shiny.setInputValue(value,id, {priority: 'event'});
        
        ;}"))
    ),


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
        ")
    ),

    fluidRow(
        tags$header(
            tags$h1(
                "What to eat",
                class = "custom-font",
                actionButton("showNavbarButton","" , style = "float: left; background-color: #00BFE5;"  , class = "my-button" , icon = icon("bars", class = "my-icon") ),  
            ),
            tags$div(
                navbarPage(
                    id = "navbarPage",
                    title = "What to eat",

                    #Home page
                    tabPanel("Home",
                        home_UI("Home")
                    ),
                    tabPanel("음식점 검색"),
                    tabPanel("평점 & 리뷰작성")
                ),
                class = "navbar_class",
                # style="display: none"
            )
        )
    )
)



    