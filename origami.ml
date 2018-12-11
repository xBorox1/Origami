(* Autor : Michał Borowski *)
(* Code Review : Artur Matyjasek *)

(************************)
(*         TYPY         *)
(************************)

type point = float * float

type kartka = point -> int

(* Typ prostej skierowanej reprezentowany przez 2 różne punkty,
 * których kolejność ma znaczenie. *)
type line = point * point

(************************)
(*     KONSTRUKTORY     *)
(************************)

let prostokat (x1, y1) (x2, y2) =
        if x1 > x2 || y1 > y2 then failwith "Niepoprawny prostokąt."  
        else
        fun (x, y) ->
                if x >= x1 && x <= x2 && y >= y1 && y <= y2 then 1
                else 0

let kolko (xr, yr) r =
        if r < 0. then failwith "Niepoprawne koło."
        else
        fun (x, y) ->
                let dl = (x -. xr) *. (x -. xr) +. (y -. yr) *. (y -. yr) in
                if dl <= r *. r then 1
                else 0 

(************************)
(* FUNKCJE GEOMETRYCZNE *)
(************************)

let iloczyn_wektorowy (x1, y1) (x2, y2) (x3, y3) =
        (x1 *. y2) -. (x2 *. y1) +. (x2 *. y3) -. (x3 *. y2) +. (x3 *. y1) -. (x1 *. y3) 


(* Funkcja sprawdzająca czy dana liczba jest równa 0. *)
let check_zero x =
        x < 0.0000001 && x > -0.0000001

(* Funckja sprawdzająca czy punkt (x, y) jest na lewo od prostej l.
 * Zwraca 1 jeśli jest na lewo, 0 jeśli jest na niej i -1 jeśli jest na prawo. *)
let left p ((x1, y1), (x2, y2)) =
        let il = iloczyn_wektorowy (x1, y1) p (x2, y2) in
        if check_zero il then 0
        else if il < 0. then 1
        else -1

(* Odbicie symetryczne punktu względem prostej. *)        
let symetria (x, y) ((x1, y1), (x2, y2)) =
        if x1 = x2 then (2. *. x1 -. x, y)
        else if y1 = y2 then (x, 2. *. y1 -. y)
        else
                let a1 = (y1 -. y2) /. (x1 -. x2)
                in let a2 = (-1.) /. a1
                in let b1 = y1 -. x1 *. a1
                in let b2 = y -. x *. a2
                in let cx = (b2 -. b1) /. (a1 -. a2)
                in let cy = (a1 *. b2 -. b1 *. a2) /. (a1 -. a2)
                in ((2. *. cx -. x), (2. *. cy -. y))

(************************)
(*  FUNKCJE NA KARTCE   *)
(************************)

let zloz p1 p2 kartk =
        if p1 = p2 then failwith "Niepoprawna prosta."
        else
                let lin = (p1, p2) in
                fun p ->
                        let lef = left p lin in
                        if lef = 1 then kartk p + kartk (symetria p lin)
                        else if lef = 0 then  kartk p
                        else 0

let skladaj lst kartk =
        List.fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) kartk lst
