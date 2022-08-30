## Arithmetic exercise for 1st grade student
## Copyright 2022   Tian Shixiong

#' @length  生成数字的大小，1 为 0-20，2 为 10-50，不指定为 0-50
#' @size    生成数字的个数
#' @num     组成算式的数字个数
#' @count   算式的数量

#' @return   a list 
#' @export
#' @examples
#'  play(count = 10, num = 2, length = 2)

# draw numbers
draw_number <- function(length, size) { 
    if(length >= 3 | length < 0) {
        print("请输入 1 或 2.")
        stop()
    } else if(length == 2) {
        sample(10:50, replace = TRUE, size = size)
    } else if (length == 1) {
        sample(0:20, replace = TRUE, size = size)
    } else {
        sample(0:50, replace = TRUE, size = size)
    }
}

# equation
equation <- function(num, length) {
    number <- draw_number(length = length, size = num)
    e <- paste0(sapply(number, 
                       function(i) {
                           paste0(i, sep = sample(c("+","-"), size=1))
                       }),
                collapse="")
    e <- strtrim(e, nchar(e)-1)
    e
}
        
# prompt

play <- function(count = 10, num = 2, length = 1) {
    t <- Sys.time()
    d <- data.frame()
    for (i in 1:count) {
        e <- equation(num = num, length = length)
        a <- readline(paste0(e," = ?"))
        d <- rbind(d, 
                   data.frame("算式" = e, 
                              "结果" = as.numeric(a),
                              "正确答案" = eval(parse(text=e)),
                              "正误" = NA
                              ))
}
    d[,4] <- d[,2] == d[,3]
    cuoti <- d[d[,4] == FALSE | is.na(d[,4]), 1]
    t1 <- Sys.time()
    s <- round(as.numeric(difftime(t1,t, units = "secs")),1)
    s <- paste0("用时：", s, "秒", collapse="")
    p <- paste0(mean(d[2] == d[3], na.rm = TRUE) * 100, "%")
    write.table(cuoti,
              file = "./cuoti.csv", 
              append = TRUE,
              col.names = FALSE,
              sep = "\t")
    # statistics
    return(list("结果" = d, "时间" = s, "正确率" = p))
}


