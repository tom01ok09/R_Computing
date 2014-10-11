Q1
(a)
> downtime = c(0, 1, 2, 12, 12, 14, 18, 21, 21, 23, 24, 25, 28, 29, 30, 30, 30, 33, 36, 44, 45, 47, 51)
> downtime
 [1]  0  1  2 12 12 14 18 21 21 23 24 25 28 29 30 30 30 33 36 44 45 47 51

(b)
> mean(downtime)
[1] 25.04348
> median(downtime)
[1] 25
> min(downtime)
[1] 0
> max(downtime)
[1] 51
> range(downtime)
[1]  0 51

(c)
> sd(downtime)
[1] 14.27164
> quantile(downtime,0.05)
 5% 
1.1 
> quantile(downtime,0.95)
 95% 
46.8 

(d)
> table(downtime)
downtime
 0  1  2 12 14 18 21 23 24 25 28 29 30 33 36 44 45 47 51 
 1  1  1  2  1  1  2  1  1  1  1  1  3  1  1  1  1  1  1 

(e)

Q2
(a)
> x=rep(0:4, each=5)
> x
 [1] 0 0 0 0 0 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4

(b)
> x=rep(seq(1,5),5)
> x
 [1] 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5

Q3
(a)
A = matrix(c(61,13,4, 175,21,18, 111,24,14, 124,23,18), nrow = 4, ncol = 3, byrow = TRUE,
               dimnames = list(c(),
                               c("x", "y", "z")))
(b)
> A[1,3]
z 
4 

Q4
HarSei = function(x){
y = 0
for(i in 1:x){
y = y +1/i}
print(y)}
> HarSei(500)
[1] 6.792823
> log(500)+0.6
[1] 6.814608


> HarSei(2000)
[1] 8.178368
> log(2000)+0.6
[1] 8.200902


> HarSei(8000)
[1] 9.564475
> log(8000)+0.6
[1] 9.587197


Q5
> x=0
> f=x^7+10000*x^6+1.06*x^5+10600*x^4+0.0605*x^3+605*x^2+0.0005*x+5
> tolerance=0.0000001
> iterations = 0
> while(abs(f)>tolerance){
+   f.prime=7*x^6+60000*x^5+5.3*x^4+42400*x^3+0.1815*x^2+1210*x+0.0005
+   x=x-f/f.prime
+   f= f=x^7+10000*x^6+1.06*x^5+10600*x^4+0.0605*x^3+605*x^2+0.0005*x+5
+   iterations = iterations + 1
+ }
> x
[1] -10000
> iterations
[1] 1


Q6
> results=read.table("D:/R/results.txt",header=T)
> attach(results)
> dev.new()
> par(mfrow=c(2, 2))
> boxplot(arch1~gender, main="Architecture Semester 1")
> boxplot(arch2~gender, main="Architecture Semester 2")
> boxplot(prog1~gender, main="Programming Semester 1")
> boxplot(prog2~gender, main="Programming Semester 2")

Q7
(a)
> factorial(4)
[1] 24
> factorial(50)
[1] 3.041409e+64
> factorial(5000)
[1] Inf
Warning message:
In factorial(5000) : value out of range in 'gammafn'

(b)
> factorial(4)/(factorial(2)*factorial(2))
[1] 6

> factorial(50)/(factorial(30)*factorial(20))
[1] 4.712921e+13

> factorial(5000)/(factorial(3000)*factorial(2000))
[1] NaN
Warning messages:
1: In factorial(5000) : value out of range in 'gammafn'
2: In factorial(3000) : value out of range in 'gammafn'
3: In factorial(2000) : value out of range in 'gammafn'

(c)
>  logfactorial.num=function(x){
+      logfactorial = 0
+      for(i in 1:x){
+        logfactorial = logfactorial+log(i)
+      }
+      return (logfactorial)
+ }
> logfactorial.num(5000)
[1] 37591.14
Ans:10^3759.14

> logCombination.num = function(n,m){
+   combination.num = 0
+   logfactorial.num(n)-(logfactorial.num(n-m)+logfactorial.num(m))
+ }
> logCombination.num = function(n,m){
+   combination.num = 0
+   combination.num = logfactorial.num(n)-(logfactorial.num(n-m)+logfactorial.num(m))
+   return(combination.num)
+ }
> logCombination.num(5000,2000)
[1] 3360.594
Ans:10^3360.594

Q8
(a)
> F1=1
> F2=1
> K=F1+F2
> while(K<100){
+   F1=F2
+   F2=K
+   K=F1+F2
+ }
> K
[1] 144

(b)
> F1=1
> F2=1
> K=F1+F2
> n=2
> while(K<100){
+   F1=F2
+   F2=K
+   K=F1+F2
+   n=n+1
+ }
> K
[1] 144
> n
[1] 11
(c)
> F1=1
> F2=1
> Fn=F1+F2
> for(i in 3:n){
+   F1=F2
+   F2=Fn
+   Fn=F1+F2
+   print(Fn)
+ }
[1] 3
[1] 5
[1] 8
[1] 13
[1] 21
[1] 34
[1] 55
[1] 89
[1] 144

Q9
prime.num = function(n){
  if(n>=2){
    print(2)
  }
  for(i in 2:n){
    isPrime=1;
    for(j in 2:sqrt(i)){
      if(i%%j==0){
        isPrime=0;
      }   
    }
    if(isPrime){
      print(i)
    }
  }
}
> prime.num(100)
[1] 2
[1] 3
[1] 5
[1] 7
[1] 11
[1] 13
[1] 17
[1] 19
[1] 23
[1] 29
[1] 31
[1] 37
[1] 41
[1] 43
[1] 47
[1] 53
[1] 59
[1] 61
[1] 67
[1] 71
[1] 73
[1] 79
[1] 83
[1] 89
[1] 97
> 


Q10
  y=function(x){
   if(x<=0){
     x=-x^3
     return(x)}
   if(x>0 && x<=1){
     x=x^2
     return(x)}
   if(x>1){
     x=sqrt(x)
     return(x)}
  }











