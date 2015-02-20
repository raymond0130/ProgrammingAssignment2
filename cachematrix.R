##SHA-1标识符：7f657dd22ac20d22698c53b23f0057e1a12c09b7
## 创建可缓存逆矩阵的特殊“矩阵”对象
## 1、求逆矩阵
## 2、设置新矩阵与新的逆矩阵
## 3、设定新的逆矩阵
## 4、获取逆矩阵
makeCacheMatrix <- function(x = matrix()) {
 inv<-solve(x)
  
  set<-function(y)
  {
    x<<-y
    inv<<-solve(x)
  }
  
  get<-function() x
  
  setinv<-function(i) inv<<-i
  
  getinv<-function() inv
  
  list (
    set=set,
    get=get,
    setinv=setinv,
    getinv=getinv
  )
}


##计算上述makeCacheMatrix返回的特殊“矩阵"的逆矩阵
## 1、传递makeCacheMatrix的逆矩阵
## 2、如果逆矩阵不为空，输出
## 3、否则，传递矩阵，并计算新的逆矩阵，更新缓存

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
  
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinv(inv)
  inv
}
