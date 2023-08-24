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
                                class="btn btn-outline-secondary btn-sm",
                                width = "18%",
                                style = "margin-right:10px;"
                            ),
                            actionButton(
                                inputId = ns("data_delete"),
                                label = "음식점 삭제하기",
                                icon = icon("minus" , class = "me-1"),
                                class="btn btn-outline-secondary btn-sm",
                                width = "18%",
                                style = "margin-right:10px;"
                            ),
                            actionButton(
                                inputId = ns("data_edit"),
                                label = "음식점 수정하기",
                                icon = icon("edit" , class = "me-1"),
                                class="btn btn-outline-secondary btn-sm",
                                width = "19%",
                                style = "margin-right:10px;"
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
Admin_Server <- function(id ){
    moduleServer(id , function(input , output , session){
        ns <- session$ns

        #데이터프레임 선언
        df_res <- session$userData[["dbTable"]]$res

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
        colums_names = c("선택","ID","음식점 이름" , "카테고리" , "메뉴"  ,"가격","평점" ,"네이버URL","위치(m)"    )
        output$data_table <- DT::renderDataTable(
                
                DT::datatable(df_res, escape=FALSE,  colnames = colums_names,
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
                                    list( targets = 9, width = '200px')
                                ),
                    transpose = TRUE,
                    scrollX = TRUE,
                    dom = "ftp",
                    columnDefs = list(list(className = 'dt-center', targets = "_all"))
                    
                )) ,  selection = 'none'
        )

        #modal open
        observeEvent(input$data_upload,{
            showModal(data_upload_modal(id = id))
        })

        #modal에서 메뉴가격 행 추가
        click_num <- reactiveVal(1L)
        observeEvent(input$addButton,{
            click_num(click_num()+1L)
            addNewRow(
                session = session,
                wrap_id = "space",
                i = click_num()
            )
        })


        #modal에서 메뉴가격 행 삭제
        observeEvent(input$deleteButton,{
            req(input$deleteButton, click_num() > 1L)
            removeUI( selector = paste0("#addRow_",click_num()) )
            click_num( click_num() - 1L )
        })


        #확인 누른 후 , 음식점 업로드
        observeEvent(input$yes,{
            res_name <- input$input_res_name
            res_category <- paste0(input$input_res_category , collapse = ",")
            res_naver_rating <- input$input_res_naver_rating
            res_distance <- input$input_res_distance
            naver_url <- input$input_naver_url
            
            # 메뉴 가격 json형식으로 변환
            menu_list <- lapply(seq(click_num()) , function(x){
                paste0(
                    paste0("\"" , input[[paste0("input_res_menu_" , x)]], "\""),
                     " : " , 
                    input[[paste0("input_res_price_" , x)]]
                    )
            })

            menu_list_1 <- paste0(unlist(menu_list) , collapse = ",") 
            menu_price_list <- paste0("{" , menu_list_1 , "}")
           

            #mysql 에 업로드
            sql_script <- glue("
                INSERT INTO what_to_eatDB.res (
                    res_name , category , menu , rating_naver ,url_naver ,distance
                    ) VALUES(
                    '{res_name}' ,'{res_category}' , '{menu_price_list}' , {res_naver_rating} , '{naver_url}' , {res_distance}
                    );
            ")
            
            # send query to MySQL
            dbExecute(con, sql_script)
            removeModal()
        })

        #음식점 삭제
        ##데이터 행 선택 후 선택된 값 저장
        rows_num <- reactiveVal(0L)
        observeEvent(input$checked_rows,{
            rows <- as.numeric(input$checked_rows)
            rows_num(as.vector(unlist(df_res[ rows, "id"])))
        })

        observeEvent(input$data_delete,{
            showModal(
                delete_modal(id)
            )
        })

        observeEvent(input$delete_yes,{
            print(rows_num())
            static_rows_num <- paste0(rows_num() , collapse = ",")
            sql_script <- glue("
                UPDATE what_to_eatDB.res SET deleted = 0 WHERE id IN ({static_rows_num})
            ")
            # send query to MySQL
            dbExecute(con, sql_script)
            print("삭제완료")
            removeModal()
        })

        #음식점 수정
        observeEvent(input$data_edit,{
            print(rows_num())
            showModal(
                modalDialog(
                    title = "음식점 수정",
                
                    fluidRow(
                        tags$p(
                            "업로드를 진행하시겠습니까?"
                        )
                    ),
                    size = "l",
                    easyClose = TRUE,
                    footer = tagList(
                    actionButton(
                        inputId = ns("edit"),
                        label = "수정",
                        class = "btn-primary",
                    ),
                    modalButton("취소")
                    )
                )
            )
        })
    }
)}