server <- function(input, output, session) {

	# 사용자 인증 ---------------------------------------------------------------------------------------- 증
    res_auth <- secure_server(
        check_credentials = checkCredentials(),
        timeout = 20
        #keep_token = TRUE
    )

	# 회원가입 ------------------------------------------------------------------------------------------ 입
    # 유효성 검사 결과 placeholder
	isolate({
		is_signup_info_valid <- reactiveVal(FALSE)
	})    

	# modal 닫기 클릭 시그널
    modal_closing <- reactive({
        list(input$signup_close_1, input$signup_close_2)
    })


    # 이메일 도메인 획득
    email_domain <- reactive({
        strsplit(x = input$user_email, split = "@", fixed = TRUE)[[1L]][2L]
    })

    # 비밀번호 입력/확인 일치 검증
    is_pwd_identical <- reactive({
        input$password_1 == input$password_2
    })

    # 중복 이메일 확인
    is_new_email <- reactive({        
        input_email <- str_trim(input$user_email)
        sql_script <- glue("SELECT COUNT(*) n_user FROM user WHERE email = '{input_email}'")
        sql_result <- tryCatch(
            { dbGetQuery(con, sql_script) },
            error = function(err){
                msg <- "Database Connection Error: Fail to access to 'user' table"
                # print `msg` so that we can find it in the logs
                message(msg)
                # print the actual error to log it
                message(err)
                # show error `msg` to user.  User can then tell us about error and we can
                # quickly identify where it came from based on the value in `msg`
                return(NULL)
            }
        )

        sql_result$n_user == 0L
    })


    # 필드 입력시, 유효성 에러 표시 제거 (if exists)
    observeEvent(input$user_name, {
        hideFeedback(inputId = "user_name")
    })

    observeEvent(input$user_email, {
        hideFeedback(inputId = "user_email")
    })

    observeEvent(input$password_1, {
        hideFeedback(inputId = "password_1")
        hideFeedback(inputId = "password_2")
    })

    observeEvent(input$password_2, {
        hideFeedback(inputId = "password_2")
    })

    # signup modal 열릴 때, 유효성 검사 결과 초기화(FALSE)
    observeEvent(input$create_account, {
        shinyjs::enable("signup_submit")
        is_signup_info_valid(FALSE)
    })

    # signup modal 닫을 때, input 값 초기화
    observeEvent(modal_closing(), {
                
        shinyjs::reset("user_name")
        shinyjs::reset("user_email")
        shinyjs::reset("password_1")
        shinyjs::reset("password_2")

        hideFeedback(inputId = "user_name")
        hideFeedback(inputId = "user_email")
        hideFeedback(inputId = "password_1")
        hideFeedback(inputId = "password_2")
    })    

    # 회원가입 신청 버튼 클릭시
    observeEvent(input$signup_submit, {

        is_valid <- TRUE
        
        # 유효성 검증: 이름 입력
        if ( input$user_name == "" ){
            feedbackDanger(
                inputId = "user_name",
                show = TRUE,
                text = "이름을 입력하지 않았습니다."
            )
            is_valid <- FALSE
        }

        # 유효성 검증: 이메일 주소 입력
        if ( input$user_email == "" ){
            feedbackDanger(
                inputId = "user_email",
                show = TRUE,
                text = "이메일을 입력하지 않았습니다."
            )

            is_valid <- FALSE
        
        # 유효성 검증: 회사 이메일
        }else if ( email_domain() %in% c("trialinformatics.com", "tiimage.com") == FALSE ){
            feedbackDanger(
                inputId = "user_email",
                show = TRUE,
                text = "회사 이메일이 아닙니다."
            )

            is_valid <- FALSE

        # 유효성 검증: 중복 회사 이메일
        }else if ( is_new_email() == FALSE ){
            feedbackDanger(
                inputId = "user_email",
                show = TRUE,
                text = "이미 등록된 이메일 입니다."
            )

            is_valid <- FALSE    
        }

        # 유효성 검증: 패스워드 입력
        if ( input$password_1 == "" ){
            feedbackDanger(
                inputId = "password_1",
                show = TRUE,
                text = "패스워드를 입력하지 않았습니다."
            )
            is_valid <- FALSE
        }

        if ( input$password_2 == "" ){
            feedbackDanger(
                inputId = "password_2",
                show = TRUE,
                text = "패스워드 확인을 입력하지 않았습니다."
            )
            is_valid <- FALSE
        }

        # 유효성 검증: 패스워드 일치 확인
        if ( input$password_1 != "" & input$password_2 != "" & is_pwd_identical() == FALSE ){
            feedbackDanger(
                inputId = "password_1",
                show = TRUE,
                text = "패스워드가 일치하지 않습니다."
            )
            feedbackDanger(
                inputId = "password_2",
                show = TRUE,
                text = "패스워드가 일치하지 않습니다."
            )
            is_valid <- FALSE
        }
        

        # 유효성 검증 결과가 fail일 경우: 중간 종료
        if (is_valid == FALSE) {
            return()
        }

        # 유효성 검증결과가 pass일 경우: 성공 메시지 전송 시그널
        is_signup_info_valid(TRUE)

        # disable 버튼
        shinyjs::disable("signup_submit")

        input_email <- str_trim(input$user_email)
        input_username <- str_trim(input$user_name)

        # 회원가입 신청 정보 DB 전송
        signup_sql_script <- glue(
            'INSERT INTO user (email, username, password, sys_role) VALUES
            ("{input_email}", "{input_username}", SHA("{input$password_1}"), "user");'
        )

        signup_sql_result <- tryCatch(
            {   
                dbExecute(con, signup_sql_script)
				TRUE
            },
            error = function(err){
                msg <- "Fail to send a sign-up request to the DB"
                # print `msg` so that we can find it in the logs
                message(msg)
                # print the actual error to log it
                message(err)
                # show error `msg` to user.  User can then tell us about error and we can
                # quickly identify where it came from based on the value in `msg`
                return(FALSE)
            }
        )

    })

    # 회원가입 신청 성공 메시지
    output$signup_msg <- renderUI({
        req( is_signup_info_valid() == TRUE )
        tags$p(
            tags$strong("회원가입이 성공적으로 신청되었습니다."),
            class = "text-success",
            style = "margin-top: 3rem; margin-bottom:0;"
        )

    })

	# shiny app server ------------------------------------------------------------------------------ #
	
	# 로그인 정보 획득
    
	observeEvent(input$shinymanager_where, {
		
		req(input$shinymanager_where == "application")
		login_info <- reactiveValuesToList(res_auth)
		session$userData[["user_id"]]( login_info$id )

		if ( login_info$sys_role == "admin" ){
			showTab(inputId = "navbarPage", target = "admin")
            shinyjs::show("hidden_admin_page")
            

		}else{
			hideTab(inputId = "navbarPage", target = "admin")
            
		}



	}, priority = 0L)    
	
	
	#새로고침 버튼 타 모듈에서 이용을 위해 isolate
	isolate({
		dbTable <- reactiveVal()

		session$userData[["refreshClicked"]] <- reactiveVal(0L)
		session$userData[["user_id"]] <- reactiveVal(0L) # needs to be updated
		session$userData[["dbTable"]] <- getDBTable() #Table 전부 다 가져오는 함수
		
		session$userData[["review_res_id"]] <- reactiveVal() #리뷰쓰려고하는 음식점
		session$userData[["review_with_res_id"]] <- reactiveVal(0L) 

		session$userData[["table_reload"]] <- reactiveVal(1L)
	})

	observeEvent(input$refreshDB, {
		# print("executed")
		session$userData[["dbTable"]] <- getDBTable()
		dbTable(getDBTable())
	})

	#db tables
	db_table <- db_table_Server("data_module", trigger = session$userData[["table_reload"]] )
	
	#Admin
	Admin_Server("Admin", db_table = db_table)

	#HOME tab_server
	food_rec_Server("food_rec", parent = session , db_table = db_table)
	week_his_Server("week_his")
	res_his_Server("res_his", parent = session)


	#음식점 검색 tab_server
	search_Server("Search" , db_table = db_table)

	#reivew , 평점 tab_server
	review_Server("Review"  , parent = session, db_table = db_table)



	#리뷰 with 레스토랑 아이디 indicator 초기화
	observeEvent(input$navbarPage, {

		req(input$shinymanager_where == "application", input$navbarPage)
		# cat("user_id = ", session$userData[["user_id"]](), fill = TRUE)
		# cat("Current page: ", input$navbarPage, fill = TRUE)

		if ( input$navbarPage != "rating" ){
			session$userData[["review_with_res_id"]](0L)
		}

	})


    observeEvent(input$title,{
        req(input$title)
        updateTabsetPanel(
            session = session,
            inputId = "navbarPage",
            selected = "home"
        )
    })

    # fab-button :  버튼 누를 시 이동 ------------------------------------------------------------- #

    observeEvent(input$go_to_home, {
        req(input$go_to_home)
        updateTabsetPanel(
            session = session,
            inputId = "navbarPage",
            selected = "home"
        )
    })

    observeEvent(input$go_to_review, {
        req(input$go_to_review)
        updateTabsetPanel(
            session = session,
            inputId = "navbarPage",
            selected = "rating"
        )
    })

    observeEvent(input$go_to_search, {
        req(input$go_to_search)
        updateTabsetPanel(
            session = session,
            inputId = "navbarPage",
            selected = "search"
        )
    })

    observeEvent(input$hidden_admin_page, {
        req(input$hidden_admin_page)
        updateTabsetPanel(
            session = session,
            inputId = "navbarPage",
            selected = "admin"
        )
    })


    #로그아웃
    observeEvent(input$logout, {
        req(input$logout)
        session$reload()
    })


    

}





