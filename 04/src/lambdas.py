print(map(lambda x: x*x, [1,2,3,4,5]))

print(list(map(lambda x: x*x, [1,2,3,4,5])))

print(list(filter(lambda y:y>10,map(lambda x: x*x, [1,2,3,4,5]))))

from functools import reduce
print(reduce((lambda x, y: x * y), [1, 2, 3, 4]))

print(reduce((lambda x, y: x + y), [1, 2, 3, 4]))

print(reduce((lambda x, y: x - y), [1, 2, 3, 4]))

#def compose(f, g):
#    return lambda *a, **b: f(g(*a, **b))

def compose(f, g):
    return lambda x: f(g(x))

print(compose(
            lambda x: x+1,
            lambda x: x*3
            )(10))

def composeMany(*fs):
    return reduce(compose, fs)

print(composeMany(
            lambda x: x+1,
            lambda x: x+2,
            lambda x: x*3
            )(10))
