# UI --------------------------------------------------------------------------------------------- #

#@food_rec_module
home_UI <- function(id){
    ns <- NS(id)
    fluidPage(
        id = id,
        fluidRow(
            tags$h1("Home" , style = "font-weight: bold;"),
            tags$div(
                tags$h4("오늘의 추천음식점", style = "margin-left: 10px; margin-bottom: 0px;")
            ),
            tags$div(style = "margin-left: 10px; margin-bottom: 5px; border:1px solid black;",
                tags$span(style = "margin-top: 0px;",
                    food_rec_UI("food_rec")
                )
            ),
            tags$span(
                class = "space"
            ),
            tags$div(
                tags$h4("이번주의 식사기록", style = "margin-left: 10px;")
            ),
            tags$div(style = "margin-left: 10px; margin-bottom: 5px; border:1px solid black;",
                tags$span(style = "margin-top: 0px;",
                    week_his_UI("week_his")
                )
            ),
            tags$span(
                class = "space"
            ),
            tags$div(
                tags$h4("이전 추천음식점 리스트", style = "margin-left: 10px; margin-bottom: 0px;")
            ),
            tags$div(style = "margin-left: 10px; margin-bottom: 5px; border:1px solid black;",
                tags$span(style = "margin-top: 0px;",
                    res_his_UI("res_his")
                )
            )
        )
    )
}