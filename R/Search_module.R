# UI --------------------------------------------------------------------------------------------- #


search_UI <- function(id){
    ns <- NS(id)
    fluidPage(
        id = id,
        tags$div(style = "display: flex; justify-content: center; align-items: center;",  # 가운데 정렬 스타일 적용
            fluidRow(style = "width: 500px; background : white; border:2px solid #E8EBEE; border-radius: 10px;",
                tags$h3("음식점 검색" , style = "font-weight: bold;"),
                tags$div(
                    class = "part_line",
                    tags$h4(
                        "음식점 명", style = "margin-left: 10px; margin-bottom: 0px; " , 
                        textInput(inputId = "name" , label = NULL , width = "95%")
                    )
                ),
                tags$div(
                    class = "part_line",
                    tags$h4(
                        "카테고리", style = "margin-left: 10px; margin-bottom: 0px;",
                    ),
                    tags$div(style = "margin-left:10px; ",
                        checkboxGroupInput(
                            inputId = "category" , label = NULL,
                            choices = c("한식", "양식", "일식", "중식", "배달" , "기타"),
                            selected = NULL 
                        )
                    )
                ),
                tags$div(
                    class = "part_line",
                    tags$h4(
                        "메뉴", style = "margin-left: 10px; margin-bottom: 0px;",
                        textInput(inputId = "menu" , label = NULL, width = "95%")    
                    )
                ),
                tags$div(
                    class = "part_line",
                    tags$h4("평점", style = "margin-left: 10px; margin-bottom: 0px;",
                        tags$div(style = "font-size : 13px;",
                        checkboxGroupInput(inputId = "score", label = NULL ,  choices = c("TI" , "네이버") , inline=TRUE , selected = NULL )
                        ),
                    sliderInput(inputId = "score_range" , label = NULL , min = 0L , max = 5L , value=4L , step = 0.5L, width = "95%")    
                    )
                ),
                tags$div(
                    class = "part_line",
                    tags$h4("가격", style = "margin-left: 10px; margin-bottom: 0px;",
                    sliderInput(inputId = "price_range" , label = NULL , min = 0L , max = 50000L , value=10000L , step = 1000L, width = "95%")
                    )
                ),
                tags$div(
                    class = "part_line",
                    tags$h4("위치(m)", style = "margin-left: 10px; margin-bottom: 0px;",
                    sliderInput(inputId = "distance_range" , label = NULL , min = 0L , max = 2000L , value=1000L , step = 100L, width = "95%")
                    )
                ),
                tags$div(
                    class = "part_line",
                    style = "text-align: center; margin-bottom: 0px; ",
                    actionButton(
                        inputId="Done_search" , label = "검색",
                        style = "font-weight: bold; background-color:#F27F0E; margin-bottom: 10px;",
                        width = "20%"
                    )
                )
            )
        )
    )
}



# server ----------------------------------------------------------------------------------------- #