## Arithmetic exercise for 1st grade student
## Copyright 2022   Tian Shixiong

#' @length  生成数字的位数，1 为一位数, 2 为两位数
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
        print("仅限 1 位或 2 位数，请输入 1 或 2.")
        stop()
    } else if(length == 2) {
        sample(10:99, replace = TRUE, size = size)
    } else if (length == 1) {
        sample(0:9, replace = TRUE, size = size)
    } else {
        sample(0:99, replace = TRUE, size = size)
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
        d <- rbind(d, data.frame("算式" = e, "结果" = as.numeric(a)))
}
    t1 <- Sys.time()
    s <- round(as.numeric(difftime(t1,t, units = "secs")),1)
    s <- paste0("用时：", s, "秒", collapse="")
# statistics
    # how to transform character strings to equation and calculate it?
    return(list("结果" = d, "时间" = s))
}


