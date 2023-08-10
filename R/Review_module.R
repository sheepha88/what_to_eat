# UI --------------------------------------------------------------------------------------------- #
review_UI <- function(id){
    ns <- NS(id)
    fluidPage(
        id = id,
        tags$div(style = "display: flex; justify-content: center; align-items: center;",  # 가운데 정렬 스타일 적용
            fluidRow(style = "width: 500px; background : white; border:2px solid #E8EBEE; border-radius: 10px;",
                tags$h3(HTML("&nbsp;평점 & 리뷰작성") , style = "font-weight: bold;"),
                tags$div(
                    class = "part_line",
                    fluidRow(
                        column(
                            width = 12,
                            tags$h4(
                                "음식점 명", style = "margin-left: 10px; " ,
                            )
                        )
                    )
                ),
                tags$div(
                    class = "part_line",
                    fluidRow(
                        column(
                            width = 3,
                            "방문일"
                        )
                    ),
                    fluidRow(
                        column(
                            width = 3,
                            "참석자"
                        )
                    ),
                    fluidRow(
                        column(
                            width = 3,
                            "평점"
                        )
                    ),
                    fluidRow(
                        column(
                            width = 12,
                            "리뷰"
                        )
                    ),
                    fluidRow(
                        column(
                            width = 12,
                            "이미지 업로드"
                        )
                    )
                )
            )
        )
    )
}


# server ----------------------------------------------------------------------------------------- #
review_Server <- function(id){
    moduleServer(id , function(input , output , session){
        ns <- session$ns
        
    })
}