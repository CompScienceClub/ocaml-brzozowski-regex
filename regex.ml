module RegEx = struct

    (* Type definition *)
    type regexp = 
        | Void
        | Eps           
        | Char   of char
        | Concat of regexp * regexp 
        | Union  of regexp * regexp 
        | Star   of regexp 
    
    (* Concatenation mapping function *)
    let concat (r1: regexp) (r2: regexp) : regexp = 
        match r1, r2 with 
            | Eps, _  -> r2
            | _, Eps  -> r1
            | Void, _ -> Void
            | _, Void -> Void
            | _, _    -> Concat (r1, r2)
            
    (* Disjunctiive union mapping function *)
    let union (r1: regexp) (r2: regexp) : regexp = 
        match r1, r2 with 
            | Void, _ -> r2
            | _, Void -> r1
            | _, _    -> if r1 = r2 then r1 else Union (r1, r2)

    (* Kleene star mapping function *)
    let star (r: regexp) : regexp = 
        match r with 
            | Void    -> Void
            | Eps     -> Eps
            | Star r' -> Star r'
            | _       -> Star r
        
    (* Delta of r *)
    let rec delta (r: regexp) : regexp = 
        match r with 
            | Void            -> Void
            | Eps             -> Eps
            | Char _          -> Void
            | Concat (r1, r2) -> concat (delta r1) (delta r2)
            | Union  (r1, r2) -> union  (delta r1) (delta r2)
            | Star r          -> Eps
    
    (* Derivative of r with respect to c *)
    let rec deriv (r: regexp) (c: char) : regexp = 
        match r with 
            | Void            -> Void
            | Eps             -> Void
            | Char c'         -> if c = c' then Eps else Void 
            | Concat (r1, r2) -> union  (concat (deriv r1 c) r2) (concat (delta r1) (deriv r2 c))
            | Union  (r1, r2) -> union  (deriv r1 c) (deriv r2 c)
            | Star r          -> concat (deriv r c) (star r)
    
    (* Process character list recursively *)
    let rec derivs (r:regexp) (s: char list) : regexp = 
        match s with 
            | c::cs -> derivs (deriv r c) cs
            | _     -> r
            
    (* String to character list *)
    let explode (s: string) : char list = 
        let rec loop i cs = 
            if i < 0 then cs else loop (i - 1) ((String.get s i)::cs)
        in  loop (String.length s - 1) []
   
    (* Boolean match of string *)
    let matches (r: regexp) (s: string) : bool = 
        match (derivs r (explode s)) with
            | Eps -> true
            | _   -> false
    
end    
    
    
    
(* Example of use *)    
    
open Printf

let test_regex  = RegEx.Concat ( RegEx.Star ( RegEx.Char 'a'), RegEx.Char 'b') 
let test_string = Sys.argv.(1);;

printf "%B\n" (RegEx.matches test_regex test_string);;