f <- function(x) {
	global.a <- 5

	g.inner <- function(y) {
		y + global.a
	}
	
	cat("g.inner(x):",g.inner(x),"\n")
	cat("g.outer(x):",g.outer(x),"\n")
	
}

g.outer <- function(y) {
	y + global.a
}

attach(list(a=1))

global.a <- 10
x <- 7

f(2)

rm(global.a)

f(2)


# <<- will modify the variable in the enclosing environment. To find the variable to modify, the following search order is used:
# outer functions, global evironment, attached environments.


detach(2)
attach(list(foo.c=0.2,foo.d=0.3,global.foo.e=4),name="foo")
global.a <- 10
global.b <- 5
global.foo.e <- 5

cat(" global.a:",global.a,"\n", "global.b:",global.b,"\n", "foo.c:",foo.c,"\n", "foo.d:",foo.d,"\n", "global.foo.e:", global.foo.e, "\n")
ff(2)
cat(" global.a:",global.a,"\n", "global.b:",global.b,"\n", "foo.c:",foo.c,"\n", "foo.d:",foo.d,"\n", "global.foo.e:", global.foo.e, "\n")

ff <- function(x) {
	global.a <- 5
	foo.c <- 0.4
	ff.a <- 1 
	ff.b <- 2
	
	g.inner <- function(y) {
		global.a <<- 20
		
		global.b <<- 0.1
		global.b <- 1
		g.inner.a <<- 1
		foo.c <<- 0.5
		foo.d <<- 0.8
		global.foo.e <<- 0.9
		ff.a <- 11
		ff.b <<- 12
		paste(" global.a:",global.a,"\n", "global.b:",global.b,"\n", "foo.c:",foo.c,"\n", "foo.d:",foo.d,"\n", "global.foo.e:", global.foo.e, "\n")
		#cat("y + a + b + c + d:", gettextf("%s + %s + %s + %s + %s", y,a,b,c,d), "\n")
		#y + a + b + c + d
		#y
	}
	
	cat("g.inner(x):\n",g.inner(x),"\n")
	#cat("g.outer(x):",g.outer(x),"\n")
	cat("ff.a", ff.a, "ff.b", ff.b , "\n")
	
	
	cat("end of ff:\n")
	cat(" global.a:",global.a,"\n", "global.b:",global.b,"\n", "foo.c:",foo.c,"\n", "foo.d:",foo.d,"\n", "global.foo.e:", global.foo.e, "\n")
	
	
}


