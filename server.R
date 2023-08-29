server <- function(input, output, session) {
	#새로고침 버튼 타 모듈에서 이용을 위해 isolate

	

	isolate({
		dbTable <- reactiveVal()

		session$userData[["refreshClicked"]] <- reactiveVal(0L)
		session$userData[["user_id"]] <- 1L # needs to be updated
		session$userData[["dbTable"]] <- getDBTable() #Table 전부 다 가져오는 함수
		session$userData[["review_res_id"]] <- reactiveVal() #리뷰쓰려고하는 음식점

		session$userData[["table_reload"]] <- reactiveVal(1L)
	})

	observeEvent(input$refreshDB, {
		print("executed")
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
	search_Server("Search")

	#reivew , 평점 tab_server
	review_Server("Review"  , parent = session, db_table = db_table)

}





