####################################################################################################
### Project  : ICT Industry 4.0s
### Script   : DCS(function).R
### Contents : Document Classifier System Functions
### Date     : 2019. 09.23 
####################################################################################################

####################################################################################################
### Setting up environment
####################################################################################################
# Packages
pkgs <- c("dplyr", "officer", "pdftools", "stringr", "openxlsx", "tm")
sapply(pkgs, require, character.only=TRUE)

####################################################################################################
### get_file_name(file_list) : Read file names from file list
####################################################################################################
get_file_name <- function(file_list){
  Name <- c()
  file_list <- unlist(file_list)
  for (i in 1:length(file_list)){
    file   <- file_list[i]
    n_char <- nchar(file)
    period_index <- 0
    
    for (j in n_char:1){
      if (substr(file, j, j) == "."){
        period_index <- j
        break}}
    Name <- c(Name, substr(file, 1, period_index-1))}
  return (Name)}

####################################################################################################
### get_file_extension(file_list) : Read file extensions from file list
####################################################################################################

get_file_extension <- function(file_list){
  Extension <- c()
  file_list <- unlist(file_list)
  for (i in 1:length(file_list)){
    file   <- file_list[i]
    n_char <- nchar(file)
    period_index <- 0
    
    for (j in n_char:1){
      if (substr(file, j, j) == "."){
        period_index <- j
        break}}
    Extension <- c(Extension, substr(file, period_index+1, n_char))}
  return (Extension)}

####################################################################################################
### read_text(name, extension): Read file texts from file name and file extension
####################################################################################################

# Read text
read_text <- function(folder_path, name, extension){
  text <- ""
  
  if(extension == "pdf"){
    pdf_file     <- str_c(folder_path, name, ".", extension)
    tryCatch(
      {tmp_text <- pdf_text(pdf_file)
       tmp_text <- gsub("\r", " ", tmp_text)
       tmp_text <- gsub("\n", " ", tmp_text)
       for (i in 1:NROW(tmp_text)){
         if(is.na(tmp_text[i])==FALSE){
          text <- str_c(text, tmp_text[i], sep=" ")}}},
      error   = function(e){},
      finally = return(text))}
    
  else if(extension == "pptx"){
    pptx_file <- str_c(folder_path, name, ".", extension)
    tmp_text  <- read_pptx(pptx_file) %>% pptx_summary() %>% select("text") %>% unlist()
    attributes(tmp_text) <- NULL
    
    for (i in 1:NROW(tmp_text)){
      if(is.na(tmp_text[i])==FALSE){
        text <- str_c(text, tmp_text[i], sep=" ")}}}
  
  else if(extension == "docx"){
    docx_file <- str_c(folder_path, name, ".", extension)
    tmp_text  <- read_docx(docx_file) %>% docx_summary() %>% select("text") %>% unlist()
    attributes(tmp_text) <- NULL
    
    for (i in 1:NROW(tmp_text)){
      if(is.na(tmp_text[i])==FALSE){
        text <- str_c(text, tmp_text[i], sep=" ")}}}
  
  else if(extension == "xlsx"){
    xlsx_file <- str_c(folder_path, name, ".", extension)
    tryCatch(
      {n_sheet=50
       tmp_xlsx <- read.xlsx(xlsx_file, sheet=1,
                             colNames=FALSE, skipEmptyRows=TRUE, skipEmptyCols=TRUE)
       for (j in 1:NROW(tmp_xlsx)){
         for (k in 1:NCOL(tmp_xlsx))
           if(is.na(tmp_xlsx[j, k])==FALSE){
             text <- str_c(text, tmp_xlsx[j, k], sep=" ")}}
  
       for (i in 2:n_sheet){
         tmp_xlsx <- read.xlsx(xlsx_file, sheet=i,
                               colNames=FALSE, skipEmptyRows=TRUE, skipEmptyCols=TRUE)
         for (j in 1:NROW(tmp_xlsx)){
           for (k in 1:NCOL(tmp_xlsx))
             if(is.na(tmp_xlsx[j, k])==FALSE){
               text <- str_c(text, tmp_xlsx[j, k], sep=" ")}}}},
      error   = function(e){},
      finally = return(text))}
  
  return (text)}

####################################################################################################
### get_folder_info(folder_path) : Read file extensions from file list and return 
####################################################################################################

get_folder_info <- function(folder_path){
  tmp       <- length(unlist(strsplit(folder_path, "/")))
  doc_type  <- unlist(strsplit(folder_path, "/"))[tmp]
  file_list <- list.files(folder_path)
  name      <- get_file_name(file_list)
  extension <- get_file_extension(file_list)
  df        <- data.frame()
  
  for (i in 1:NROW(name)){
    tmp <- data.frame(Path      = folder_path,
                      Type      = doc_type,
                      Name      = name[i],
                      Extension = extension[i],
                      text      = read_text(folder_path, name[i], extension[i]))
    df  <- rbind(df, tmp)}  
  
  return(df)}

####################################################################################################
### text_pre_processing(df) : Clean the text data
####################################################################################################

text_pre_processing <- function(df){
  df <- Corpus(DataframeSource(df))
  df <- tm_map(df, removeNumbers)
  df <- tm_map(df, removePunctuation)
  df <- tm_map(df, stripWhitespace)
  df <- tm_map(df, tolower)
  
  return (df)}

####################################################################################################
### create_dtm(df) : create the document term matrix
####################################################################################################
# Dictionary
word_list <- c("회의", "연차", "조퇴", "출장", "사유", "신청", "외근", "지각", "외출",
               "결근", "기안", "구매", "금액", "협조", "견적", "수량", "조건", "할인",
               "규격", "업무", "진행", "지출", "현금", "증빙", "카드", "품의", "발주",
               "선정", "계약", "제공", "귀사", "지급", "보증", "납품", "검수", "계약서",
               "경조사", "공급가", "영수증")

create_dtm <- function(df){
  tmp <- df %>% select(doc_id, text) %>% text_pre_processing %>% 
    DocumentTermMatrix(control=list(weighting=weightTf)) %>% as.matrix() %>% as.data.frame()
  #weighting=weightTF
  result  <- data.frame(doc_id = rownames(tmp))
  
  for (i in 1:NROW(word_list)){
    tryCatch(
      { result <- cbind(result, (tmp %>% select(word_list[i])))},
      error = function(e){})}
  
  for (i in 1:NROW(word_list)){
    if (word_list[i] %in% colnames(result) == FALSE){
      result <- result %>% mutate(word_list[i] <- 0)
      colnames(result)[NCOL(result)] <- word_list[i]}}
  
  doc_type        <- df %>% select(doc_id, Type)
  doc_type$doc_id <- as.character(doc_type$doc_id)
  result          <- full_join(result, doc_type)
  
  return (result) }

Doc_predict <- function(folder_list = folder_list){
  
  # train data frame
  df.tmp <- data.frame()
  
  for (i in 1:NROW(folder_list)){
    folder_path <- paste0(root_folder, "/", folder_list[i], "/")
    tmp         <- get_folder_info(folder_path)
    df.tmp      <- rbind(df.tmp, tmp)}
  
  df.tmp$doc_id <- 1:NROW(df.tmp)
  df.tmp        <- df.tmp %>% select(doc_id, Path, Type, Name, Extension, text)
  
  # Create the document term matrix and Remove the doc_id column
  df.tmp        <- create_dtm(df.tmp)
  df.tmp        <- df.tmp[, -1]
  
  # rearrange the column of df.tmp to equalize with dtm.all
  order <- which(colnames(df.tmp) == colnames(dtm.all[,-1])[1])
  for(i in 2:40){ order <- cbind(order, which(colnames(df.tmp) == colnames(dtm.all[,-1])[i])) }
  
  # set df.tmp as test data
  test <- df.tmp[, order]
  
  d.t  <- dtm.all[, -1]
  tmp  <- test
  a    <- data.frame()
  for (i in 1:ncol(tmp)) {if(tmp[ , i]!=0){a <- c(a, i)}}
  
  for(j in 1:(ncol(d.t)-1)){
    if (j %in% a) {d.t <- d.t[which(d.t[, j]!= 0), ]} 
    else          {d.t <- d.t[which(d.t[, j]== 0), ]}
  }
  
  test <- d.t[1, ]
  
  # load the result of Stack_XGB 
  test_st     <- data.frame(Type        = test$Type,
                            tr_predict  = predict(dt_fit, newdata = test),
                            rf_predict  = predict(rf_fit2, newdata = test),
                            xgb_predict = predict(xgb_fit, newdata = test),
                            svm_predict = predict(tuned.svm, newdata = test),
                            mdl_predict = predict(mdl_fit, newdata = test),
                            bag_predict = predict(bag_fit, newdata = test))
  
  result <- list(Prediction = predict(xgb_st, test_st),
                 Actual     = test_st$Type)
  
  return(result)
}

