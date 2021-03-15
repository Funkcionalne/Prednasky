// source : https://gist.github.com/joelnet/611d5d58164a2486e923ed8569d90fb8

// Y Combinator
const Y = a => (b => b (b)) (b => a (c => b (b) (c)))

// isomorphic Church encoding/decoding
const Church = {
  to: n => f => x => Array.from (Array (n)).reduce (f, x),
  from: f => f (x => x + 1) (0)
}

const True = a => b => a
const False = a => b => b
const nil = {}
const zero = _ => x => x
const one = f => x => f (x)
const two = f => x => f (f (x))
const three = f => x => f (f (f (x)))
const four = f => x => f (f (f (f (x))))
const five = f => x => f (f (f (f (f (x)))))
const six = f => x => f (f (f (f (f (f (x))))))
const seven = f => x => f (f (f (f (f (f (f (x)))))))
const eight = f => x => f (f (f (f (f (f (f (f (x))))))))
const nine = f => x => f (f (f (f (f (f (f (f (f (x)))))))))
const ten = f => x => f (f (f (f (f (f (f (f (f (f (x))))))))))

const succ = n => f => x => f (n (f) (x))
const pred = a => b => c => a (d => e => e (d (b))) (_ => c) (a => a)
const thunk = a => b => c => a (b)
const pair = a => b => c => c (a) (b)
const isZero = a => a (_ => False) (True)
const not = a => b => c => a (c) (b)
const gte = a => b => isZero (sub (b) (a))
const lt = a => b => not (gte (a) (b))

// math
const add = a => b => c => d => b (c) (a (c) (d))
const sub = a => b => b (pred) (a)
const mult = a => b => c => a (b (c))
const div = Y (f => q => a => b =>
  (lt (a) (b)
    (thunk (pair (q)) (a))
    (thunk (f (succ (q)) (sub (a) (b))) (b))
  ) (nil)
) (zero)

// display church pair for humans
const toHumanReadable = x => y =>
  `${Church.from(x)} remainder ${Church.from(y)}`

const fifteen =   add  (five)    (ten)   // Church (fifteen)
const thirteen =  sub  (fifteen) (two)   // Church (thirteen)
const twentyone = mult (three)   (seven) // Church (twentyone)

Church.from (fifteen)   //=> 15
Church.from (thirteen)  //=> 13
Church.from (twentyone) //=> 21

const result = div (ten) (three)
result (toHumanReadable) //=> "3 remainder 1"