#load "origami.cmo";;
open Origami;;

let k = prostokat (0., 0.) (2., 2.);;
assert(k (0., 0.) = 1);;
assert(k (1., 1.) = 1);;
assert(k (2., 2.) = 1);;
assert(k (3., 3.) = 0);;
let k = zloz (1., 0.) (1., 2.) k;;
assert(k (0., 0.) = 2);;
assert(k (1., 1.) = 1);;
assert(k (2., 2.) = 0);;
assert(k (0.5, 0.5) = 2);;
let k = zloz (1., 1.) (0., 1.) k;;
assert(k (0., 0.) = 4);;
assert(k (1., 1.) = 1);;
assert(k (0., 1.) = 2);;
assert(k (2., 2.) = 0);;
let k = zloz (3., 3.) (3., 0.) k;;
assert(k (1., 1.) = 0);;
assert(k (6., 0.) = 4);;
assert(k (5., 1.) = 1);;
assert(k (6., 1.) = 2);;
assert(k (4., 2.) = 0);;
let k = kolko (0., 0.) 1.5;;
assert(k (0., 0.) = 1);;
assert(k (1.5, 1.5) = 0);;
assert(k (2., 2.) = 0);;
let l = [ ((3., 0.), (3., 1.)) ;
          ((2., 0.), (2., 1.)) ;
          ((1., 0.), (1., 1.)) ];;
let k = skladaj l (prostokat (0., 0.) (4., 1.));;
assert(k (0., 0.) = 3);;
assert(k (0.5, 0.5) = 4);;
assert(k (2., 0.) = 0);;