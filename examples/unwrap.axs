// Example of using type classes to implement `unwrap`.

// Types that can be unwrapped.
class Unwrap a {
    // The result that `unwrap`ping will produce when successfully applied.
    type Result;

    // Unwraps `self` into the result type, panicking on failure.
    def unwrap (self: a) -> Result;
}

// Either an `a`, or no data.
data Option a
  = Some a
  | None;

// `Option a` can be unwrapped to produce `a`.
instance Unwrap (Option a) {
    type Result = a;
    def unwrap self = match self {
        Some x -> x,
        None -> panic "Attempt to unwrap None",
    };
}

// Either an `a`, or an error of type `e` (which must be representable as a string, hence the `Show`
// bound).
data Result a (e: Show)
  = Ok a
  | Err e;

// `Result a e` can be unwrapped to produce `a`.
instance Unwrap (Result a e) {
    type Result = a;
    def unwrap self = match self {
        Ok x -> x,
        Err e -> panic ("Attempt to unwrap Err " ++ show e),
    }
}
