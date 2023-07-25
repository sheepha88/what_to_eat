server <- function(input, output, session) {
        #새로고침 버튼 타 모듈에서 이용을 위해 isolate

        isolate({
                session$userData[["refreshClicked"]] <- reactiveVal(0L)
                session$userData[["user_id"]] <- 1L # needs to be updated
                session$userData[["dbTable"]] <- getDBTable() #Table 전부 다 가져오는 함수
        })

        # 미래구조
        # db_table <- dataModule_Server("data_module", trigger) trogger = 로그인했을때
        # module_1("module1", db_table)
        # module_2("module2", db_table)
        
        
        #오늘의 추천음식점 출력
        food_rec_Server("food_rec")
        week_his_Server("week_his")
        res_his_Server("res_his")

}





