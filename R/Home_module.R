# UI --------------------------------------------------------------------------------------------- #

#@food_rec_module
#@res_history_module
#@week_history_module
home_UI <- function(id){
    ns <- NS(id)
    fluidPage(style = "display: flex; justify-content: center; align-items: center; background : #FAFBFC;",
        id = id,
        fluidRow(style = "width: 500px; background : white; border:2px solid #E8EBEE; border-radius: 10px; ",
            tags$h3(HTML("&nbsp;Home") , style = "font-weight: bold;"),
            tags$div(class = "part_line",
                tags$h4(
                    "오늘의 추천음식점",
                    style = "margin-left: 10px; margin-bottom: 0px; ")
            ),
            tags$div(style = "margin-left: 10px; margin-bottom: 5px; border:1px ",
                tags$span(style = "margin-top: 0px;",
                    food_rec_UI("food_rec")
                )
            ),
            tags$span(
                class = "space"
            ),
            tags$div(class = "part_line",
                tags$h4("이번주의 식사기록", style = "margin-left: 10px; margin-bottom: 0px;")
            ),
            tags$div(style = "margin-left: 10px; margin-bottom: 5px; ",
                tags$span(style = "margin-top: 0px;",
                    week_his_UI("week_his")
                )
            ),
            tags$span(
                class = "space"
            ),
            tags$div(class = "part_line",
                tags$h4("이전 추천음식점 리스트", style = "margin-left: 10px; margin-bottom: 0px;")
            ),
            tags$div(style = "margin-left: 10px; margin-bottom: 5px; ",
                tags$span(style = "margin-top: 0px;",
                    res_his_UI("res_his")
                )
            )
        )
    )
}