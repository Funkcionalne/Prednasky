def addN(n):                	# vısledkom addN je funkcia, 
	return (lambda x:n+x) 	# ktorá k argumentu pripoèína N

add5 = addN(5)      	# toto je jedna funkcia x›5+x 	
add1 = addN(1)      	# toto je iná funkcia  y›1+y 	
				# … môem ich vyrobi neobmedzene ve¾a
print(add5(10))		# 15
print(add1(10))		# 11

def iteruj(n,f):		# vısledkom je funkcia fn
    if n == 0:
        return (lambda x:x)	# identita
    else:
        return(lambda x:f(iteruj(n-1,f)(x)))  # f(fn-1) = fn

add5SevenTimes = iteruj(7,add5)	# +5(+5(+5(+5(+5(+5(+5(100)))))))
print(add5SevenTimes(100))           # 135
