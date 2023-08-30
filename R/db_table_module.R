db_table_Server <- function(id, trigger){
    moduleServer(id , function(input , output , session){

        isolate({
            out <- reactiveVal(NULL)
        })

        observeEvent(trigger(), {

            out(NULL)
            
            # 음식점  테이블 출력
            #deleted = 0 : admin page에서 삭제처리하지 않은 것만 추출
            sql_res <- "SELECT * FROM res WHERE deleted = 0;"
            df_res <- dbGetQuery(con, sql_res) |> as.data.table()
            
            # 사용자 테이블 출력
            sql_user <- "SELECT * FROM user WHERE approved = 1;"
            df_user <- dbGetQuery(con, sql_user) |> as.data.table()
            
            # review 테이블 출력
            sql_review <- "SELECT * FROM review;"
            df_review <- dbGetQuery(con, sql_review) |> as.data.table()

            # recommend 테이블 출력
            sql_recommend <- "SELECT * FROM recommend;"
            df_recommend <- dbGetQuery(con, sql_recommend) |> as.data.table()

            # image 테이블 출력
            sql_image <- "SELECT * FROM image;"
            df_image <- dbGetQuery(con, sql_image) |> as.data.table()

            # 테이블 최신화
            db_table_list <- list()
            db_table_list$res <- df_res
            db_table_list$user <- df_user 
            db_table_list$review <- df_review
            db_table_list$recommend <- df_recommend
            db_table_list$image <- df_image

            # print("db_table reloading")

            out(db_table_list)
        })

        return( out )

    })
}