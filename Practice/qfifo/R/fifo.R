#' The constructor for fifoq
#' @return An S3 object of class fifoq
#' @examples
#' q <- qfifo()
#' @export
qfifo <- function(){
  # Creates an S3 class with attribute data as a list(to implement a queue)
  structure(list(data=list()),class="qfifo")
}

#' Add a value to the queue
#'
#' @param q is the current queue object
#' @param val is the value to be added to the queue
#'
#' @return The updated queue object
#' @export
#' @examples
#' q <- qfifo()
#' q <- add(q,1234)
add<-function(q,val){
  #Creating S3 generic function add
  UseMethod("add")
}

#' @export
add.qfifo<-function(q,val){
  # Add a new element to the queue
  q$data <- append(q$data, val, 0)
  q
}

#' Return the top value in the queue
#'
#' @param q is the current fifo queue object
#'
#' @return The top of the queue
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q,1234)
#' v <- top(q)
top<-function(q){
  #Creating S3 generic function add
  UseMethod("top")
}

#' @export
top.qfifo<-function(q){
  # Check if queue is empty. If so, stop the execution
  if(length(q$data) == 0)
    stop("No elements on the queue")
  # Return top element from the queue
  q$data[[length(q$data)]]
}

#' Delete the top element from the queue
#'
#' @param q is the current fifo queue object
#' @return The modified queue
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q,1234)
#' q <- add(q,5678)
#' q <- process(q)
process <- function(q){
  #Creating S3 generic function add
  UseMethod("process")
}

#' @export
process.qfifo <- function(q)
{
  # Removing the top value from the queue
  q$data[[length(q$data)]] <- NULL
  q
}
