#' Find test statistic value from a given data and corresponding critical value
#'
#' More detailed description
#'
#' @param g1 a real vector
#' @param g2 a real vector
#' @param g3 a real vector
#' @param g4 a real vector
#' @param g5 a real vector
#' @param alpha real number between 0 and 1 called significance level
#'
#' @return numeric vector
#'
#' @examples
#' g1=c(1,2,3,2,0,1,4,2,3)
#' g2=c(1,3,1,2,5,2,1,7,8,1)
#' g3=c(1,1,2,4,5,6,2,5,7,2)
#' g4=c(1,2,1,3,1,5,1,3,2,3,0,2)
#' g5=c(2,4,2,7,3,7,1,2,3,5,7)
#'
#' Maxt5crit(g1,g2,g3,g4,g5,0.05)
#'
#'
#' @export
Maxt5crit<-function(g1,g2,g3,g4,g5,alpha)
{
  fun1<-function(n1,n2,n3,n4,n5,v1,v2,v3,v4,v5)
  {
    g1<-rnorm(n1,0,sqrt(v1));g2<-rnorm(n2,0,sqrt(v2));g3<-rnorm(n3,0,sqrt(v3))
    g4<-rnorm(n4,0,sqrt(v4));g5<-rnorm(n5,0,sqrt(v5))
    X1=mean(g1);X2=mean(g2);X3=mean(g3);X4=mean(g4);X5=mean(g5)
    s1=var(g1);s2=var(g2); s3=var(g3);s4=var(g4);s5=var(g5)
    v1=sqrt(s1/n1+s2/n2);v2=sqrt(s2/n2+s3/n3);v3=sqrt(s3/n3+s4/n4);v4=sqrt(s4/n4+s5/n5)
    T1=(X2-X1)/v1;T2=(X3-X2)/v2;T3=(X4-X3)/v3; T4=(X5-X4)/v4
    A=max(T1,T2,T3,T4,na.rm=FALSE)
    return(A)
  }
  fun2<-function(n1,n2,n3,n4,n5,alpha,v1,v2,v3,v4,v5)
  {
    x<-replicate(5000,fun1(n1,n2,n3,n4,n5,v1,v2,v3,v4,v5))
    y<-sort(x,decreasing=FALSE)
    m=(1-alpha)*500
    c<-y[m]
    return(c)
  }
  fun3<-function(n1,n2,n3,n4,n5,alpha,v1,v2,v3,v4,v5)
  {
    z=replicate(10,fun2(n1,n2,n3,n4,n5,alpha,v1,v2,v3,v4,v5))
    cri=mean(z)
    return(cri)
  }
  fun4<-function(g1,g2,g3,g4,g5)
  {
    X1=mean(g1);X2=mean(g2);X3=mean(g3);X4=mean(g4);X5=mean(g5)
    n1=length(g1);n2=length(g2);n3=length(g3);n4=length(g4);n5=length(g5)
    s1=var(g1);s2=var(g2);s3=var(g3);s4=var(g4);s5=var(g5)
    v1=sqrt(s1/n1+s2/n2);v2=sqrt(s2/n2+s3/n3);v3=sqrt(s3/n3+s4/n4);v4=sqrt(s4/n4+s5/n5)
    T1=(X2-X1)/v1;T2=(X3-X2)/v2;T3=(X4-X3)/v3; T4=(X5-X4)/v4
    A=max(T1,T2,T3,T4,na.rm=FALSE)
    return(A)
  }
  n1=length(g1);n2=length(g2);n3=length(g3);n4=length(g4);n5=length(g5)
  v1=var(g1);v2=var(g2);v3=var(g3);v4=var(g4);v5=var(g5)
  statistic_value=fun4(g1,g2,g3,g4,g5)
  set.seed(2)
  crit_value<-fun3(n1,n2,n3,n4,n5,alpha,v1,v2,v3,v4,v5)
  result=c(statistic_value, crit_value)
  return(result)
}
