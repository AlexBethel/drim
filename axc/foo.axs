// Need tuples, f x is broken
def a (x: String) = 2 + a::b["xyz"] // foo
  + fn x y -> x + y + {a : b};

def b = let (x, y) = 20 in x + y;

def c = match x + 1 {
    a => {c: d, e: f},
};

class Cat a {
    def meow (cat: a);
}
