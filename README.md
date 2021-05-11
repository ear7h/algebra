# algebra

A solver for algebraic fields, the typical grade school and physics algebra.

The solver uses principles from program synthesis; interepreting the
algebraic steps as program, the solver iterates the space of all possible
programs until a solution is found. The current implementation leaves a lot
to be desired, the search space gets noisy with rules like unfolding
constants (`2 => 1 + 1`) and periodic rules (`a + b => b + a => a + b`).

## examples

```
$ stack repl Solver.hs
*Algebra.Solver> a = Mul (Var "A") (Add (Var "B") (Const 3))
*Algebra.Solver> a
"A" * ("B" + 3)
*Algebra.Solver> b = (Add (Mul (Var "A") (Var "B")) (Mul (Var "A") (Add (Const 1) (Const 2))))
*Algebra.Solver> b
"A" * "B" + "A" * (1 + 2)
*Algebra.Solver> a =*> b
Just
	[ ("A" * ("B" + 3.0),"start")
	, ("A" * "B" + "A" * 3.0,"distribute")
	, ("A" * "B" + "A" * (3.0 + neg(2.0) + 2.0),"unfold add")
	, ("A" * "B" + "A" * (1.0 + 2.0),"fold add")
	]
*Algebra.Solver> (Var "F") =*= (Mul (Var "m") (Var "a")) $ (Var "a")
Just
	[ (("F","m" * "a"),"start")
	, (("m" * "a","F"),"flip")
	, (("a" * "m","F"),"commute")
	, (("a",rec("m") * "F"),"move multiplication")
	]
```

## intent

My intent with this library is to realize a program I wish I had when I
was taking my required physics classes, where most problems were really
just solving for a varible, given some known values spanning multiple
equations. Thus, another part that needs to be created is the equation
lookup.

Ultimately, I'd like to make a super-powered version of the units unix
program.

## related links

* https://github.com/oisdk/agda-ring-solver
* https://github.com/nadia-polikarpova/cse291-program-synthesis
* https://people.csail.mit.edu/asolar/SynthesisCourse/index.htm
* https://drops.dagstuhl.de/opus/volltexte/2020/11959/pdf/OASIcs-PLATEAU-2019-5.pdf

