let rec filter p = function
  | [] -> []
  | x :: xs ->
    if p x
      then x :: filter p xs
      else filter p xs;;

let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs;;

let rec foldr f v = function
  | [] -> v
  | x :: xs -> f x (foldr f v xs);;

let rec foldl f v = function
  | [] -> v
  | x :: xs -> foldl f (f v x) xs;;
