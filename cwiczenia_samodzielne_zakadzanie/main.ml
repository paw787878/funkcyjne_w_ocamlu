let nastepna_rzadka n =
    let rec n_r_pom pozostala_n potega_2 czy_bylo_00 mnoznik_00=
        if pozostala_n=0 && czy_bylo_00 then (n/mnoznik_00 +1)*mnoznik_00 else
        if pozostala_n=0 && not(czy_bylo_00) then potega_2 else 
            if (pozostala_n mod 4) = 0 && czy_bylo_00=false 
                then 
                    n_r_pom (pozostala_n/2) (2*potega_2) (true) (potega_2)
                else 
                    if (pozostala_n mod 4)= 3 
                        then 
                            n_r_pom (pozostala_n/2) (2*potega_2) (false) (-1)
                        else 
                            n_r_pom (pozostala_n/2) (2*potega_2) czy_bylo_00 mnoznik_00

    in n_r_pom  n 1 false (-1);;

let  ile_rzadkich_dodatnich_niewiekszych_od n=
    let rec pom pozostala_n odwrocone_n ile_wkladu nastepne_ile_wkladu suma ostatni_bit=
        (*print_endline ("fajnie"); *)
        (*teraz odwracamy *)
        if pozostala_n <> 0 then pom (pozostala_n/2) (2*odwrocone_n + (pozostala_n mod 2)) (nastepne_ile_wkladu) (nastepne_ile_wkladu+ile_wkladu) 0 0 else
        (*wiec teraz sie cofamy*)
        if odwrocone_n = 0 then suma else
        if odwrocone_n mod 2 =1 && ostatni_bit=0 then pom 0 (odwrocone_n/2) (nastepne_ile_wkladu-ile_wkladu) (ile_wkladu) (suma+ile_wkladu) 1 else
            pom 0 (odwrocone_n/2) (nastepne_ile_wkladu-ile_wkladu) (ile_wkladu) suma (odwrocone_n mod 2)
    in 
pom n 0 1 1 0 0;;

let czy_pierwsza n=
    if n=0 || n=1 then false else
    let rec pom dzielnik=
        if n mod dzielnik = 0 && dzielnik <> n then false else
        if dzielnik * dzielnik > n then true else
        pom (dzielnik+1)
    in 
        pom 2;;

let t=czy_pierwsza;;
