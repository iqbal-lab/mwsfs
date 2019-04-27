fancy_sum<-function(n,s, rho)
{
  sum_of<-0
  j<-0
  for (j in 0:(n-s-1))
  {
    sum_of <- sum_of +  (choose(n-j-2, s-1) * (j+1)/(j+1+rho))
  }  
  return(sum_of)
}


#gene present in k of n genomes
#theta/2 is mutation rate
#rho/2 is rate of gene loss
mut_sfs_fixed_gene_freq <- function(k,n, theta, rho)
{
  
  counts<-c()
  for (s in 1:k)
  {
    ct<- (theta*k/(n*s))* fancy_sum(n,s,rho)  / choose(n-1,s)  
    append(counts, ct)
    print (ct)
  }
  
  return (counts)
}
