def cons(a, b):
    def pair(f):
        return f(a, b)
    return pair

def head(p):
    return p(lambda a,b :a)
    
def tail(p):
    return p(lambda a,b:b)

# print(head(cons(4,5)))
# print(tail(cons(4,5)))
#-----

def pair(a,b):
    return lambda f: f(a,b)

def fst(p):
    return p(lambda a,b: a)

def snd(p):
    return p(lambda a,b: a)

# print(fst(pair(4,5)))
