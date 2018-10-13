let silnia_jednoargumentowa n=
let rec silnia a n=
    if n=0 then a else
        silnia (a*n) (n-1)
in 
    silnia 1 n;;

silnia_jednoargumentowa (5);;


let fib n =
let rec fib_pom ta nastepna ile_do_przodu=
    assert (ta >= 0);
    if ile_do_przodu=0 then ta else
        fib_pom nastepna (ta+nastepna) (ile_do_przodu-1)
in
    fib_pom 0 1 n;;

let rec fib_wyk n=
    if n=0 then 0 else
        if n=1 then 1 else
            (fib_wyk (n-1))+(fib_wyk (n-2));;

(*teraz procedura odwracajaca liczbe *)

let odwroc_kolejnosc n=
    (*n sie zmniejsza acc rosnie   *)
    let rec odwroc_kolejnosc_pom n acc=
        if n=0 then acc else
            odwroc_kolejnosc_pom (n / 10) (10 * acc + (n mod 10))

    in 
        odwroc_kolejnosc_pom n 0;;    





    
