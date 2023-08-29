# UI --------------------------------------------------------------------------------------------- #

#@food_rec_module
#@res_history_module
#@week_history_module
home_UI <- function(id){
    ns <- NS(id)
    fluidPage(
        style = "display: flex; justify-content: center; align-items: center;",
        id = id,
        class = "bg-white",
        fluidRow(
            class = "bg-white",
            style = "width: 500px; border:2px solid #E8EBEE; border-radius: 10px; ",
            tags$h3(
                HTML("Home"),
                class = "ms-2 mt-2 mb-2 fw-bold"
            ),
            fluidRow(
                class = "part_line",
                tags$h5("추천음식점",  class = "mt-2 mb-2")
            ),
            fluidRow(
                class = "ms-2 mb-1",
                food_rec_UI("food_rec")
            ),
            fluidRow(
                class = "part_line",
                tags$h5("이번주의 식사기록" , class = "mt-2 mb-2")
            ),
            fluidRow(
                class = "ms-2 mb-1",
                week_his_UI("week_his")
            ),
            fluidRow(
                class = "part_line",
                tags$h5("이전 추천음식점 리스트", class = "mt-2 mb-2")
            ),
            fluidRow(
                class = "ms-2 mb-1",
                res_his_UI("res_his")
            )
        )
    )
}