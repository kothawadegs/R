gcd <- function(a,b)		#function calculating gcd of two numbers
{
count = 0

	while(b != 0)
	{
                count <- count+1
		r <- a%%b
		a <- b
		b <- r
        }
        v <- c(a,count)		#vector returning gcd(a) & count
	return (v)
}

n <- scan(nmax=1)
t <- NULL
count <- NULL
	for (i in 0:n)			#loop creating n*n matrix
	{
		for (j in 0:n)
		{
		t = c(t,(gcd(i, j)[1]))	
                count <- c(count,gcd(i,j)[2])   	
		}	
	}
a <- matrix(t, nrow=n+1,ncol=n+1)	# n*n matrix  
a[which(a[,]!=1)] <- NA
image(a, main = "Graphics with Granddaddy", xlab = "n", ylab = "n", co = "skyblue")
cat("No of iterations for each pair\n")
print(count)
x <- which((a[,])==1)
no_of_coprime <- length(x)
total_no_pairs <- ((n+1)*(n+1))
ratio <- no_of_coprime/total_no_pairs
cat("Ratio of coprime to the total no of pairs\n")
print(ratio)
