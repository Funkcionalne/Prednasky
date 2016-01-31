function addN(n) {
	return function(x) { return x+n };
}
function iteruj(n, f) {
   if (n === 0) 
      return function(x) {return x;};
   else
      return function(x) { return iteruj(n-1,f)(f(x)); };
}
add5 = addN(5);
add1 = addN(1);
iteruj(7,add5)(100)
iteruj(7,iteruj(4,add1))(100)
