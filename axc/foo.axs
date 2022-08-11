// AlexScript test

extern def core::intrinsic::negate_u32 : U32 -> U32;
extern def core::intrinsic::pow_u32 : U32 -> U32 -> U32;
extern def core::intrinsic::mul_u32 : U32 -> U32 -> U32;
extern def core::intrinsic::div_u32 : U32 -> U32 -> U32;
extern def core::intrinsic::mod_u32 : U32 -> U32 -> U32;
extern def core::intrinsic::plus_u32 : U32 -> U32 -> U32;
extern def core::intrinsic::minus_u32 : U32 -> U32 -> U32;
extern def core::intrinsic::equal_u32 : U32 -> U32 -> U32;
extern def core::intrinsic::notEqual_u32 : U32 -> U32 -> U32;
extern def core::intrinsic::lessThan_u32 : U32 -> U32 -> U32;
extern def core::intrinsic::lessThanEq_u32 : U32 -> U32 -> U32;
extern def core::intrinsic::greaterThan_u32 : U32 -> U32 -> U32;
extern def core::intrinsic::greaterThanEq_u32 : U32 -> U32 -> U32;
extern def core::intrinsic::and_u32 : U32 -> U32 -> U32;
extern def core::intrinsic::or_u32 : U32 -> U32 -> U32;

extern data U32;

class Group n {
    def plus : n -> n -> n;
    def zero : n;
    def negate : n -> n;
}

class Group n => Ring n {
    def mul : n -> n -> n;
    def one : n;
}

instance Group U32 {
    def plus = core::intrinsic::plus_u32;
    def zero = 0;
    def negate = core::intrinsic::negate_u32;
}

instance Ring U32 {
    def mul = core::intrinsics::mul_u32;
    def one = 1;
}

extern def print_u32 : U32 -> ();

// Impurity expliot to chain together actions.
def progn (l: ()) (r: ()) : () = ();

def main : () =
  print_u32 (2 + 2);
