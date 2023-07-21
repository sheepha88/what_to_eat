server <- function(input, output, session) {
        #새로고침 버튼 타 모듈에서 이용을 위해 isolate

        isolate({
                session$userData[["refreshClicked"]] <- reactiveVal(0L)
                session$userData[["user_id"]] <- 1L # needs to be updated
        })

        #오늘의 추천음식점 출력
        food_rec_Server("food_rec")
        week_his_Server("week_his")
        res_his_Server("res_his")
        

}





