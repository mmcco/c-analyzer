type stdint_size = S_8 | S_16 | S_32 | S_64;;

type basic_type =
        | CChar
        | CShort
        | CInt
        | CLong
        | CLongLong
        (* stdint.h types *)
        | Int_t of stdint_size
        | Int_least_t of stdint_size
        | Int_fast_t of stdint_size
        | Intptr_t
        | Intmax_t
;;

(* TODO: additional attributes such as 'register' *)
(*
type int_type = {
        typ: basic_type;
        is_signed: bool;
};;
*)
type int_type = basic_type * bool;;

let size_to_str (s: stdint_size) : string =
        match s with
        | S_8 -> "8"
        | S_16 -> "16"
        | S_32 -> "32"
        | S_64 -> "64"
;;

let sign_to_str (s: bool) : string =
        if s then "u" else ""
;;

let int_type_to_str (i: int_type) : string =
        match i with
        | CChar, s ->     if s then "char" else "unsigned char"
        | CShort, s ->    if s then "short" else "unsigned short"
        | CInt, s ->      if s then "int" else "unsigned int"
        | CLong, s ->     if s then "long" else "unsigned long"
        | CLongLong, s -> if s then "long long" else "unsigned long long"
        (* stdint.h types *)
        | (Int_t sz), s ->
                        (sign_to_str s) ^ "int" ^ (size_to_str sz) ^ "_t"
        | (Int_least_t sz), s ->
                        (sign_to_str s) ^ "int_least" ^ (size_to_str sz) ^ "_t"
        | (Int_fast_t sz), s ->
                        (sign_to_str s) ^ "int_fast" ^ (size_to_str sz) ^ "_t"
        | Intptr_t, s -> if s then "intptr_t" else "uintptr_t"
        | Intmax_t, s -> if s then "intmax_t" else "uintmax_t"
;;

let promote (x: int_type) (y: int_type) : int_type =
        x
;;

let main() =
        let x: int_type = (Int_t S_16, false) in
        let y: int_type = (CLong, true) in
        let s: string = int_type_to_str(promote x y) in
        print_string s;
        print_string "\n";
;;

main()
