# UI --------------------------------------------------------------------------------------------- #
Admin_UI <- function(id ){
    ns <- NS(id)
    fluidPage(
        id = id,
        tags$div(style = "display: flex; justify-content: center; align-items: center;",  # 가운데 정렬 스타일 적용
            fluidRow(style = "width: 1500px; background : white; border:2px solid #E8EBEE; border-radius: 10px;",
                tags$h3(HTML("Data Add/Delete") , style = "font-weight: bold;"),
                tags$div(
                    class = "part_line",
                    fluidRow(
                        column(
                            width = 12,
                            actionButton(
                                inputId = ns("data_upload"),
                                label = "음식점 등록하기",
                                icon = icon("plus" , class = "me-1"),
                                class="btn btn-outline-secondary btn-sm admin-button-style"
                            ),
                            actionButton(
                                inputId = ns("data_delete"),
                                label = "음식점 삭제하기",
                                icon = icon("minus" , class = "me-1"),
                                class="btn btn-outline-secondary btn-sm admin-button-style"
                            ),
                            actionButton(
                                inputId = ns("data_edit"),
                                label = "음식점 수정하기",
                                icon = icon("edit" , class = "me-1"),
                                class="btn btn-outline-secondary btn-sm admin-button-style"
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    class = "ms-2 mt-4 mb-3 border-top",
                                    tags$div(
                                        class = "part_line",
                                        DT::dataTableOutput(
                                            ns("data_table")
                                        )
                                    ),
                                    tags$script(HTML(paste0('$(document).on("click", "input[type=checkbox]", function () {',
                                    'var checkboxes = document.getElementsByName("', ns("selected"), '");',
                                    'var checkboxesChecked = [];',
                                    'for (var i=0; i<checkboxes.length; i++) {',
                                    'if (checkboxes[i].checked) {',
                                    'checkboxesChecked.push(checkboxes[i].value);',
                                    '}',
                                    '}',
                                    'Shiny.setInputValue("', ns("checked_rows"), '", checkboxesChecked);',
                                    '})')))
                                )
                            )
                        )
                    )
                )
            )
        )
    )
}


# server ----------------------------------------------------------------------------------------- #
Admin_Server <- function(id, db_table){
    moduleServer(id , function(input , output , session){

        # 환경 셋업 ---------------------------------------------------------------------------------- #
        ns <- session$ns

        isolate({
            web_table <- reactiveVal() # web 테이블 placeholder
        })


        # web 테이블 셋업 ----------------------------------------------------------------------------- #
        observeEvent(db_table(), {

            req(db_table())
            df_res <- db_table()$res

            #df_res의 메뉴컬럼 -> 메뉴 , 가격(평균)으로 만들기
            ##가격 json -> 리스트
            df_res$price <- priceToList(df_res)
            ##메뉴 json -> 리스트
            df_res$menu <- menuToList(df_res) 

            # columns = c(res_name ,category , menu , price ,  rating_naver , distance)
            df_res <- df_res[,.(id ,res_name ,category , menu , price ,  rating_naver, url_naver , distance)]
            
            df_res[["Select"]]<-glue::glue(
                '<input type="checkbox" name="', ns("selected"), '"  value="{1:nrow(df_res)}"><br>'
            )
            df_res <- df_res[,c(9,1,2,3,4,5,6,7,8)]

            web_table(df_res)
        })


        # web 테이블 구현 ----------------------------------------------------------------------------- #    
        colums_names = c("선택","ID","음식점 이름" , "카테고리" , "메뉴"  ,"가격","평점" ,"네이버URL","위치(m)")
        output$data_table <- DT::renderDataTable(     
            web_table(),
            colnames = colums_names,
            escape = FALSE,
            selection = "none",
            options = list(
                pageLength = 20, Width = '1500px',
                columnDefs = list(
                    list( targets = 1, width = '50px'),
                    list( targets = 2, width = '50px'),
                    list( targets = 3, width = '200px'),
                    list( targets = 4, width = '200px'),
                    list( targets = 5, width = '200px'),
                    list( targets = 6, width = '200px'),
                    list( targets = 7, width = '200px'),
                    list( targets = 8, width = '200px'),
                    list( targets = 9, width = '200px'),
                    list( targets = "_all", className = 'dt-center')
                ),
                transpose = TRUE,
                scrollX = TRUE,
                dom = "ftp"
            )
        )

        # 음식점 추가 --------------------------------------------------------------------------------- #
        isolate({
            click_num <- reactiveVal(1L)
            add_result <- reactiveVal(FALSE)
        })        

        #modal open
        observeEvent(input$data_upload,{
            click_num(1L)
            add_result(FALSE)
            shinyjs::enable("yes")

            showModal(data_upload_modal(id = id)) 
        })

        #modal에서 메뉴가격 행 추가        
        observeEvent(input$addButton, {
            click_num(click_num()+1L)
            addNewRow(
                session = session,
                wrap_id = "space",
                i = click_num()
            )
        })

        #modal에서 메뉴가격 행 삭제
        observeEvent(input$deleteButton, {
            req(input$deleteButton, click_num() > 1L)
            removeUI( selector = paste0("#addRow_", click_num()) )
            click_num( click_num() - 1L )
        })


        #확인 누른 후 , 음식점 업로드
        observeEvent(input$yes, {
            res_name <- str_trim(input$input_res_name)
            res_category <- paste0(input$input_res_category, collapse = ",")
            res_naver_rating <- input$input_res_naver_rating
            res_distance <- input$input_res_distance
            naver_url <- str_trim(input$input_naver_url)
            
            # 메뉴 가격 json형식으로 변환
            menu_list <- lapply(
                seq_len(click_num()), 
                function(x){
                    paste0(
                        paste0("\"" , str_trim(input[[paste0("input_res_menu_", x)]]), "\""),
                        " : " , 
                        input[[paste0("input_res_price_" , x)]]
                    )
                }
            )

            menu_list_1 <- paste0(unlist(menu_list) , collapse = ",") 
            menu_price_list <- paste0("{" , menu_list_1 , "}")
           

            #MySQL 쿼리 작성
            sql_script <- glue("
                INSERT INTO res (
                    res_name, category, menu, rating_naver, url_naver, distance
                )
                VALUES (
                    '{res_name}', '{res_category}' , '{menu_price_list}', {res_naver_rating}, '{naver_url}', {res_distance}
                );
            ")

            #MySQL에서 쿼리 실행
            sql_result <- tryCatch(
                {
                    dbExecute(con, sql_script)
                    TRUE
                },
                error = function(e){
                    message("Fail to add a new restaurant to DB: ", res_name)
                    message(e)
                    return(FALSE)
                }
            )

            # 음식점 추가 성공 메시지 
            if ( sql_result ){ add_result(TRUE) }

            # 확인 버튼 disable
            shinyjs::disable("yes")
            
            # db table 리로딩
            reload_db_table(session)
        })

        #음식점 추가 성공 메시지
        output$data_upload_msg <- renderUI({
            req(add_result())
            tags$p(
                "새 음식점이 성공적으로 추가 되었습니다.",
                class = "fw-bold text-success mb-0"
            )
        })

        # 체크박스 처리 로직 ----------------------------------------------------------------------------- #        
        ##선택된 데이터 행의 DB id 획득 로직
        rows_id <- reactive({
            req(db_table())
            df_res <- db_table()$res

            ###행 선택이 되지 않은 경우, empty integer 벡터 리턴
            rows <- as.integer(input$checked_rows)
            if (length(rows) == 0L){ return(integer(0L)) }

            ###선택된 행의 DB id 리턴
            df_res[rows, id] 
        })

        # 음식점 삭제 --------------------------------------------------------------------------------- #
        isolate({
            del_results <- reactiveVal(FALSE)
        })

        ##음식점 삭제 확인 modal open
        observeEvent(input$data_delete, {
            del_results(FALSE)
            shinyjs::enable("delete_yes")

            showModal( delete_modal(id) )
        })

        ##음식점 삭제 버튼 클릭시
        observeEvent(input$delete_yes, {

            #삭제 대상 행 id
            del_target_ids <- rows_id()

            #선택된 행이 없을 경우, modal 닫기
            if ( length(del_target_ids)==0L ){
                removeModal()
                return()
            }
            
            #MySQL 쿼리 작성
            static_rows_num <- paste0(del_target_ids, collapse = ",")
            sql_script <- glue("UPDATE res SET deleted = 0 WHERE id IN ({static_rows_num});")

            #MySQL에서 쿼리 실행
            sql_result <- tryCatch(
                {
                    dbExecute(con, sql_script)
                    TRUE
                },
                error = function(e){
                    message("Fail to delete selected restaurant(s) from DB")
                    print(e)
                    return(FALSE)
                }
            )

            # 음식점 추가 성공 메시지 
            if ( sql_result ){ del_results(TRUE) }

            # 확인 버튼 disable
            shinyjs::disable("delete_yes")
            
            # db table 리로딩
            reload_db_table(session)

        })

        #음식점 삭제 성공 메시지
        output$data_delete_msg <- renderUI({
            req(del_results())
            tags$p(
                "선택한 음식점이 삭제 되었습니다.",
                class = "fw-bold text-success mb-0"
            )
        })


        # 음식점 수정 --------------------------------------------------------------------------------- #
        isolate({
            edit_menu <- reactiveVal()
            edit_menu_num <- reactiveVal()
            edit_results <- reactiveVal(FALSE)
        })
        

        #음식점 수정 버튼 클릭
        observeEvent(input$data_edit, {

            req(db_table())
            df_res <- db_table()$res            
            edit_menu(NULL)
            edit_menu_num(NULL)
            edit_results(FALSE)
            enable("edit_yes")

            ##수정 대상 DB id
            edit_target_id <- rows_id()

            ##체크박스 선택이 1개 이상일 경우, 종료
            if( length(edit_target_id) != 1L ){ return() }


            ##수정 modal open
            showModal( data_edit_modal(id) )

            ##음식점 이름
            updateTextInput(
                inputId = "edit_res_name",
                value = df_res[id==edit_target_id , res_name]
            )

            ##음식점 카테고리
            selected_list = df_res[id==edit_target_id, category]
            updateCheckboxGroupInput(
                inputId = "edit_res_category",
                choices = c("한식", "양식", "일식", "중식", "배달" , "기타"),
                selected = case_when(
                                grepl(",", selected_list) ~ unlist(strsplit(selected_list, ",")),
                                TRUE ~ selected_list
                            )
            )

            ##네이버 평점
            updateNumericInput(
                inputId = "edit_res_naver_rating",
                value = df_res[id==edit_target_id, rating_naver], 
                max = 5L
            )

            ##위치
            updateNumericInput(
                inputId = "edit_res_distance",
                value = df_res[id==edit_target_id, distance]
            )

            ##네이버 URL
            updateNumericInput(
                inputId = "edit_naver_url",
                value = df_res[id==edit_target_id, url_naver]
            )

            ##메뉴 테이블 생성
            edit_menu_str <- df_res[id==edit_target_id, menu] |> 
                gsub(pattern = "{", replacement = "", x = _, fixed = TRUE) |> 
                gsub(pattern = "}", replacement = "", x = _, fixed = TRUE) |> 
                gsub(pattern = "\"", replacement = "", x = _, fixed = TRUE) |> 
                (\(y) strsplit(x = y, split = ",", fixed = TRUE)[[1L]])()

            dish <- strsplit(x = edit_menu_str, split = ":", fixed = TRUE) |> 
                lapply(X = _, FUN = function(x) str_trim(x[1L])) |> 
                unlist()

            price <- strsplit(x = edit_menu_str, split = ":", fixed = TRUE) |> 
                lapply(X = _, FUN = function(x) str_trim(x[2L]) |>
                as.numeric()) |> 
                unlist()
            
            edit_menu_tmp <- data.table(dish=dish, price=price)
            edit_menu(edit_menu_tmp)
            edit_menu_num(edit_menu_tmp[, .N])


            ##첫번째 메뉴
            if ( edit_menu()[, .N] >= 1L ){
                updateTextInput(
                    inputId = "edit_input_res_menu_1",
                    value = edit_menu()[1L, dish]
                )

                updateNumericInput(
                    inputId = "edit_input_res_price_1",
                    value = edit_menu()[1L, price]
                )
            }

        })


        ##두번째 메뉴 부터 Rendering
        output$edit_UI <- renderUI({

            req( edit_menu(), edit_menu()[, .N] > 1L )

            out <- lapply(
                seq_len( edit_menu()[,.N] - 1L ) + 1L,
                function(j){
                    fluidRow(
                        id = paste0("edit_addRow_", j),
                        column(
                            width = 4,
                            offset = 3,
                            textInput(
                                inputId = ns(paste0("edit_input_res_menu_", j)),
                                label = NULL,
                                value = edit_menu()[j, dish]
                            )
                        ),
                        column(
                            width = 4,
                            numericInput(
                                inputId = ns(paste0("edit_input_res_price_", j)),
                                label = NULL,
                                value = edit_menu()[j, price]
                            )
                        )
                    )
                }
            )

            out 

        })

        ##modal에서 메뉴 가격 행 추가
        observeEvent(input$edit_addButton, {
            req(edit_menu_num())

            edit_menu_num( edit_menu_num() + 1L )                
            addNewRow(
                session = session,
                wrap_id = "edit_space",
                i = edit_menu_num()
            )
        })

        ##modal에서 메뉴 가격 행 삭제
        observeEvent(input$edit_deleteButton,{               
            req(edit_menu_num() > 1L)

            removeUI(selector = paste0("#edit_addRow_", edit_menu_num()) )
            edit_menu_num( edit_menu_num() - 1L )
        })


        #음식정 수정 확인 버튼 클릭
        observeEvent(input$edit_yes, {
            
            ##메뉴 가격 json형식으로 변환
            menu_list <- lapply(
                seq_len(edit_menu_num()), 
                function(x){
                    menu_name <- input[[paste0("edit_input_res_menu_", x)]]
                    if ( menu_name != "" ){
                        paste0(
                            paste0("\"" , str_trim(menu_name), "\""),
                            " : " , 
                            ifelse(
                                is.na(input[[paste0("edit_input_res_price_" , x)]]),
                                "NULL",
                                input[[paste0("edit_input_res_price_" , x)]]
                            )
                        )
                    }else{
                        NULL
                    }
                }
            )

            menu_list_1 <- paste0(unlist(menu_list) , collapse = ", ") 
            menu_price_list <- paste0("{" , menu_list_1 , "}")


            ##수정 대상 DB id
            edit_target_id <- rows_id()

            ##기본정보 획득
            res_name <- str_trim(input$edit_res_name)
            res_category <- paste0(input$edit_res_category, collapse = ",")
            res_naver_rating <- input$edit_res_naver_rating
            res_distance <- input$edit_res_distance
            naver_url <- str_trim(input$edit_naver_url)

            ##MySQL 쿼리 작성
            sql_script <- glue("
                UPDATE res 
                SET
                    res_name = '{res_name}', 
                    category = '{res_category}',
                    menu = '{menu_price_list}',
                    rating_naver = {res_naver_rating},
                    url_naver = '{naver_url}',
                    distance = {res_distance}
                
                WHERE
                    id = {edit_target_id};
            ") |> gsub(pattern = "NA", replacement = "NULL", x = _, fixed = TRUE)

            #MySQL에서 쿼리 실행
            sql_result <- tryCatch(
                {
                    dbExecute(con, sql_script)
                    TRUE
                },
                error = function(e){
                    message(e)
                    message("Fail to edit the selected restaurant from DB")
                    return(FALSE)
                }
            )

            # 음식점 추가 성공 메시지 
            if ( sql_result ){ edit_results(TRUE) }

            # 확인 버튼 disable
            shinyjs::disable("edit_yes")
            
            # db table 리로딩
            reload_db_table(session)

        })


        #음식점 수정 성공 메시지
        output$data_edit_msg <- renderUI({
            req(edit_results())
            tags$p(
                "음식점 정보를 성공적으로 수정하였습니다.",
                class = "fw-bold text-success mb-0"
            )
        })



    }
)}

