import problems46_50_logic_and_codes._

//and(_, _) andThen not(_)

def f(s: String) = "f(" + s + ")"

def g(s: String) = "g(" + s + ")"

f _ compose g _

f _ andThen g _

// not(_) compose g _
(f _ compose g)("ffff")

proand(_,_)

(not _ compose not)(false)




