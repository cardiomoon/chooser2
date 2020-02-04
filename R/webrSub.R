#'textInput with disply:inline block
#'@param inputId The input slot that will be used to access the value
#'@param label Display label for the control, or NULL for no label.
#'@param value Initial value.
#'@param width The width of the input in pixel
#' @param ... Further argument to be passed to textInput
#'@export
textInput2<-function (inputId, label, value = "",width=100,...)
{

    div(class="form-group shiny-input-container",style="display:inline-block;",
        tags$label(label, `for` = inputId, style="display:inline-block;"),
        tags$input(id = inputId, type = "text", class="form-control",value = value,
                   style=paste("width: ",width,"px; display:inline-block;",sep=""),...)
    )
}

#' Convert markdown string into html format
#' @param string The character string to convert
#' @export
md2html=function(string){
    result=length(unlist(gregexpr("`",string)))/2
    res=string
    for(i in 1:result){
        res=sub("`","<code>",res)
        res=sub("`","</code>",res)
    }
    res=paste0("<p>",res,"</p>")
    res
}

#'Marks the given text as HTML
#'@param string The character string to convert
#'@export
myp=function(string){
    HTML(md2html(string))
}

#' Paste string with comma
#' @param tempA a string
#' @param ... Further argument to be passed to paste()
#' @param sep separator
#' @export
mypaste=function(tempA,...,sep=","){
    result=""
    if(tempA=="")  result=paste0(...)
    else result=paste0(tempA,sep,...)
    result
}



#' Paste comma as a separator
#' @param ... Further argument to be passed to paste()
#' @export
pastecomma=function(...){
    paste(...,sep=",")
}


#' Delete all non-numeric character
#' @param x A character string to be converted
#' @export
make_numeric <- function(x) {
    as.numeric( gsub('[^0-9.]', '', x))
}

#' Make character vector from text
#' @param text A character string to be converted
#' @importFrom stringr str_trim
#' @export
text2vector=function(text){
    stringr::str_trim(unlist(strsplit(text,",")))
}

#'Extract extension form file name
#'@param filename file name
#'@export
file2ext=function(filename){
    namelist=unlist(strsplit(filename,".",fixed=TRUE))
    result=namelist[length(namelist)]
    return(tolower(result))
}

#'Import data file
#' @param file File to read
#' @param ... Further argument to be passed to read.csv or import
#' @importFrom rio import
#' @importFrom utils read.csv
#'@export
myimport=function(file,...){
    ext=file2ext(file$name)
    if(ext=="csv"){
        result<-tryCatch(read.csv(file$datapath),error=function(c) "error")
        if(class(result)!="data.frame"){
            result<-tryCatch(read.csv(file$datapath,fileEncoding = "euc-kr"),error=function(c) "error")
        }
    } else{
        result=rio::import(file$datapath,...)
    }
    result
}

#'Convert string vector to string
#'@param vars The character string to be converted
#'@param del Logical
#'@export
vector2str=function(vars,del=FALSE){
    if(length(vars)<1) return(NULL)
    if(del) {
        temp=paste0("c(-",vars[1])
        if(length(vars)>1) {
            for (i in 2:length(vars)) temp=paste0(temp,",-",vars[i])
        }
        temp=paste0(temp,")")

    } else{
        temp=paste0("c('",vars[1],"'")
        if(length(vars)>1) {
            for (i in 2:length(vars)) temp=paste0(temp,",'",vars[i],"'")
        }
        temp=paste0(temp,")")

    }
    temp
}

#'Convert string vector to formula
#'@param vars The character string to be converted
#'@export
vector2form=function(vars){
    if(length(vars)<1) return(NULL)
    temp=vars[1]
    if(length(vars)>1) {
        for (i in 2:length(vars)) temp=paste(temp,"+",vars[i])
    }
    temp
}

#' Extract labels from data
#' @param x Name of a column of data.frame
#' @export
extractLabels=function(x){
    result=NULL
    if(!is.null(names(attr(x,"labels")))) result=names(attr(x,"labels"))
    if(!is.null(names(attr(x,"value.labels")))) result=names(attr(x,"value.labels"))
    #if(is.null(result)) result=as.character(unique(x))
    result
}

#'Return All substring
#'@param x A character string
#'@param max maximal number of character allowed
allsubstr=function(x,max=NULL){
    res=c()
    if(is.null(max)) max=nchar(x)
    for(i in 1:max)  res=c(res,unique(substring(x, 1:(nchar(x)-i+1), i:nchar(x))))
    res
}

#'Return Longest common string bwteen two strings
#'@param a A character string
#'@param b A character string
myLCS_sub=function(a,b){
    n=min(nchar(a),nchar(b))
    result=intersect(allsubstr(a,n),allsubstr(b,n))
    if(length(result)==0) return(NULL)
    result[which.max(nchar(result))]
}

#'Return Longest common string
#'@param x A character vector
#'@export
myLCS=function(x){
    Reduce(myLCS_sub,x)
}


