function f() {
   y = 1
   return function (x) { return x + y }  -- closure, ktorá viaže y
}
function g() {
  y = 2
  h = f()  		-- výsledkom je funkcia (closure), ktorú aplikujeme na 10
  console.log(h(10)) – otázka s akou hodnotou y sa vykoná sèítanie, 1 alebo 2
}
g()
