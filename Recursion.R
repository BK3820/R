recurse_fibonacci <- function(n) {
if(n <= 1) {
return(n)
} else {
return(recurse_fibonacci(n-1) + recurse_fibonacci(n-2))
}
}

nterms = as.integer(readline(prompt="How many terms? "))

if(nterms <= 0) {
print("Plese enter a positive integer")
} else {
print("Fibonacci sequence:")
for(i in 0:(nterms-1)) {
print(recurse_fibonacci(i))
}
}
