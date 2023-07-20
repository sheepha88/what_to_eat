# ------------------------------------------------------------------------------------------------ #
# library                                                                                          #
# ------------------------------------------------------------------------------------------------ #
library(shinyjs)
library(shiny)
library(glue)
library(RMySQL)
library(DBI)
library(data.table)
library(jsonlite)
library(rjson)
library(bslib)


# ------------------------------------------------------------------------------------------------ #
# mysql connect                                                                                    #
# ------------------------------------------------------------------------------------------------ #

# RMySQL connect --------------------------------------------------------------------------------- #

## setup connection
con <- dbConnect(
  RMySQL::MySQL(), 
  user="root", 
  password="0000",
  host="127.0.0.1", 
  dbname = "testdb",
  client.flag=CLIENT_MULTI_RESULTS
)

## preliminaries
dbExecute(con, "SET NAMES 'utf8mb4' COLLATE 'utf8mb4_unicode_ci';") # reset character set
dbExecute(con, "SET SQL_SAFE_UPDATES = 0;") # allowing update

# utf control ------------------------------------------------------------------------------------ #
# dbSendQuery(con, "SET NAMES utf8;") 
# dbSendQuery(con, "SET CHARACTER SET utf8;") 
# dbSendQuery(con, "SET character_set_connection=utf8;")
# legacy code

# ------------------------------------------------------------------------------------------------ #
# At the close of the app                                                                          #
# ------------------------------------------------------------------------------------------------ #
onStop(function() {
  cat("Closing App & Database Connection\n")
  #추후삭제 예정
  dbExecute(con, "DELETE FROM testdb.recommend")
  dbDisconnect(con)
  # poolClose(con)
})

# ------------------------------------------------------------------------------------------------ #
# utility functions                                                                                #
# ------------------------------------------------------------------------------------------------ #

# Row 추가 함수 -------------------------------------------------------------------------------------- #
addNewFile <- function(wrap_id, i){
  
  newFileUI <- fluidRow(
    id = paste0("addFile_",i),

    column(
      width = 1,
      tags$h4(i)
    ),

    column(
      width = 4,
      textInput(
        inputId = paste0("des_", i),
        label = "Description",
        value = ""
      )
    ),

    column(
      width = 4,
      fileInput(
        inputId = paste0("file_", i),
        label = "Upload File",
      ) |> shinyjs::disabled()
    ),

    column(
      width = 3,
      class = "align-self-center",
      downloadButton(
        outputId = paste0("down_",i),
        label = "Download File"
      ) |> shinyjs::disabled()
    )
  )
    
      insertUI(
        selector = paste0("#", wrap_id),
        where = "beforeEnd",
        ui = newFileUI
      )
}


