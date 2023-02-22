object HelloWorld {
    def iteruj[A](n:Int, f:(A=>A)) : (A => A) = {
      if (n == 0) 
        return (x:A) => x
      else
        return (x:A) => iteruj(n-1, f)(f(x))
    }
    def main(args: Array[String]) {
      val add5 = (i:Int) => i+5
      println(iteruj(7, (i:Int) => i+1)(100))
      println(iteruj(7, iteruj(4,add5))(100))
    }
}