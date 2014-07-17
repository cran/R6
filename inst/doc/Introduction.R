## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(R6)

Person <- R6Class("Person",
  public = list(
    name = NA,
    hair = NA,
    initialize = function(name, hair) {
      if (!missing(name)) self$name <- name
      if (!missing(hair)) self$hair <- hair
      greet()
    },
    set_hair = function(val) {
      hair <<- val
    },
    greet = function() {
      cat(paste0("Hello, my name is ", name, ".\n"))
    }
  )
)

## ------------------------------------------------------------------------
ann <- Person$new("Ann", "black")
ann

## ------------------------------------------------------------------------
ann$hair
ann$greet()
ann$set_hair("red")
ann$hair

## ------------------------------------------------------------------------
Queue <- R6Class("Queue",
  public = list(
    initialize = function(...) {
      for (item in list(...)) {
        add(item)
      }
    },
    add = function(x) {
      queue <<- c(queue, list(x))
      invisible(self)
    },
    remove = function() {
      if (length() == 0) return(NULL)
      # Can use private$queue for explicit access
      head <- private$queue[[1]]
      private$queue <- private$queue[-1]
      head
    }
  ),
  private = list(
    queue = list(),
    length = function() base::length(queue)
  )
)

q <- Queue$new(5, 6, "foo")

## ------------------------------------------------------------------------
# Add and remove items
q$add("something")
q$add("another thing")
q$add(17)
q$remove()
q$remove()

## ----eval = FALSE--------------------------------------------------------
#  q$queue
#  #> NULL
#  q$length()
#  #> Error: attempt to apply non-function
#  
#  # Actually, there is a way:
#  q$private$queue
#  #> [[1]]
#  #> [1] "foo"
#  #>
#  #> [[2]]
#  #> [1] "something"
#  #>
#  #> [[3]]
#  #> [1] "another thing"
#  #>
#  #> [[4]]
#  #> [1] 17

## ------------------------------------------------------------------------
q$add(10)$add(11)$add(12)

## ------------------------------------------------------------------------
q$remove()
q$remove()
q$remove()
q$remove()

## ------------------------------------------------------------------------
Numbers <- R6Class("Numbers",
  public = list(
    x = 100
  ),
  active = list(
    x2 = function(value) {
      if (missing(value)) return(x * 2)
      else self$x <- value/2
    },
    rand = function() rnorm(1)
  )
)

n <- Numbers$new()
n$x

## ------------------------------------------------------------------------
n$x2

## ------------------------------------------------------------------------
n$x2 <- 1000
n$x

## ----eval=FALSE----------------------------------------------------------
#  n$rand
#  ## [1] 0.2648
#  n$rand
#  ## [1] 2.171
#  n$rand <- 3
#  ## Error: unused argument (quote(3))

## ------------------------------------------------------------------------
# Note that this isn't very efficient - it's just for illustrating inheritance.
HistoryQueue <- R6Class("HistoryQueue",
  inherit = Queue,
  public = list(
    show = function() {
      cat("Next item is at index", head_idx + 1, "\n")
      for (i in seq_along(queue)) {
        cat(i, ": ", queue[[i]], "\n", sep = "")
      }
    },
    remove = function() {
      if (length() - head_idx == 0) return(NULL)
      head_idx <<- head_idx + 1
      queue[[head_idx]]
    }
  ),
  private = list(
    head_idx = 0
  )
)

hq <- HistoryQueue$new(5, 6, "foo")
hq$show()
hq$remove()
hq$show()
hq$remove()

## ------------------------------------------------------------------------
CountingQueue <- R6Class("CountingQueue",
  inherit = Queue,
  public = list(
    add = function(x) {
      total <<- total + 1
      super$add(x)
    },
    get_total = function() total
  ),
  private = list(
    total = 0
  )
)

cq <- CountingQueue$new("x", "y")
cq$get_total()
cq$add("z")
cq$remove()
cq$remove()
cq$get_total()

