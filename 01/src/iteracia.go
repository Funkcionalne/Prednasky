package main

import "fmt"

type realnaFunckia /*=*/ func(float64) float64

func kompozicia(f, g realnaFunckia) realnaFunckia {
	return (func(x float64) float64 {// kompozicia(f,g) = f.g
		return f(g(x)) // tu vzniká v run-time nová funkcia, 				// ktorá nebola v èase kompilácie
	})
}						  // iteracia(n,f)=f^n
func iteracia(n int, f realnaFunckia) realnaFunckia {
	if n == 0 {
		return (func(x float64) float64 { return x }) //id
	} else {					
		return kompozicia(f, iteracia(n-1, f))
	}				// f . iter(n-1,f)
}

func main() {
  add1 := func(x float64) float64 { return x+1 }
  add5 := func(x float64) float64 { return x+5 }
  fmt.Println(iteracia(7, add5)(100));
  fmt.Println(iteracia(7, iteracia(4, add1))(100));
}
