Queue <- R6Class("Queue",
                 public = list(
                   name = NULL,
                   type = NULL,
                   popup_position = NULL,
                   initialize = function(name = "Queue", type = "popup", popup_position = "bottom-right") {
                     self$name <- name
                     self$type <- type
                     self$popup_position <- popup_position
                   },
                   enqueue = function(x, start_message = NULL, end_message = NULL){

                     if(!is.null(start_message)){
                       if(!is.null(self$type) && self$type == "popup")
                         toastr_info(message = start_message, title = self$name, timeOut = 10000, position = self$popup_position)
                       else
                         print(start_message)
                     }

                     private$items[[self$size() + 1]] <- list(task = x, start = start_message, end = end_message)
                     invisible(self)
                   },
                   dequeue = function(){
                     if(self$size() == 0)
                       stop(simpleError("No more items on stack"))

                     item <- private$items[[1]]

                     private$show_end_message(item)

                     private$items <- private$items[-1]
                     item
                   },
                   remove_at = function(id){
                     if(self$size() == 0)
                       stop(simpleError("No more items on stack"))
                     if(!between(id, 1, self$size()))
                       stop(simpleError("Index out of range"))

                     item <- private$items[[id]]

                     private$show_end_message(item)

                     private$items <- private$items[-id]
                     item
                   },
                   peek = function(){
                     if(self$size() == 0)
                       return(NULL)
                     item <- private$items[[1]]
                     item
                   },
                   queue = function(){
                     if(self$size() == 0)
                       return(NULL)

                     private$items
                   },
                   size = function(){
                     length(private$items)
                   }
                 ),
                 private = list(
                   items = list(),
                   show_end_message = function(item){
                     if(!is.null(item$end)){
                       if(!is.null(self$type) && self$type == "popup")
                         toastr_success(message = item$end, title = self$name, timeOut = 10000, position = self$popup_position)
                       else
                         print(item$end)
                     }
                   }
                 ))

