function f() {
   y = 1
   return function (x) { return x + y }  -- closure, ktor� via�e y
}
function g() {
  y = 2
  h = f()  		-- v�sledkom je funkcia (closure), ktor� aplikujeme na 10
  console.log(h(10)) � ot�zka s akou hodnotou y sa vykon� s��tanie, 1 alebo 2
}
g()
