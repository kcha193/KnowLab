# Proto demo object
#
# Proto gives you the ability to create an object that gives you access by reference
# to its elements. It does this because a proto object is an environment.
#
# In addition, it gives you a way of creating functions (and allow them to be 
# grouped together as part of a proto object) which can work on the elements of 
# that object. It does this by automatically passing the object (refered to as
# the reciving object) as the first parameter of the function. This first parameter
# is typically given the name "." 
# 
#
# Author: Oliver Mannion
###############################################################################

library(proto)

Demo <- proto(expr = {  
			x <- 0
			
			#' Create new object
			#' 
			#' @examples
			#' demo <- new$new(x = 5)
			new <- function (., x) {
				proto(.,
						x=x
				)
			}
			
			clone <- function(.) as.proto(.$as.list(all.names=TRUE))
			
			class <- function(.) "Demo"
			
			setx <- function(., newx) {
				#modify x in receiving object
				.$x <- newx
			}
			
			setx2 <- function(., newx) {
				#looks up x using standard R scoping rules, ie: first in this method, 
				#or in the parent enviroment in which this function is defined, i.e: Demo
				#assignment perists for this method only. no side effects.
				x <- newx
			}

			setx3 <- function(., newx) {
				#looks up x using standard R scoping rules, ie: first in this method, 
				#or in the parent enviroment in which this function is defined, i.e: Demo
				#assigns x in the environment enclosing this, ie: Demo$x
				x <<- newx
			}
			
			getx <- function(.) {
				.$x
			}
			
			getx2 <- function(.) {
				#looks up x using standard R scoping rules, ie: first in this method, 
				#or in the parent enviroment in which this function is defined, i.e: Demo
				x
			}
			
			getGlobalx <- function(.) {
				# looks up .super in Demo, which points to R_GlobalEnv and returns x in
				# that environment
				.super$x
			}

			getSuperx <- function(.) {
				# looks up .super in receiving object, and returns x from it
				.$.super$x
			}
			
			
		})

	
demo <- Demo$new(1)

# get Demo / super object x value, ie: 0
Demo$x ; demo$getx2() ; demo$.super$x ; demo$getSuperx()  

# get demo x value, ie: 1
demo$x ; demo$getx() 

# set demo x value
demo$setx(1.5) ; demo$x ; Demo$x

# does nothing 
demo$setx2(2) ; demo$x ; Demo$x 

# set Demo / super object s value
demo$setx3(3) ; demo$x ; Demo$x

# returns x in R_GlobalEnv
demo$getGlobalx() 


#first argument (the demo proto object) already inserted
demo$getsuperx
str(demo$getsuperx)

#fuction without first arg inserted
str(with(demo, getsuperx))

with(demo, getsuperx)


