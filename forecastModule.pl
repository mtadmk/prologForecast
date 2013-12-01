:- module(fc,[ 
		
	]).

start :-
	write('Prognozowanie pogody.'), nl, nl,
	wykasujZapamietaneOdpowiedzi,
   	szukajUbioru.
   	
szukajUbioru :-
	podajSport(A),
	pogodaMozeByc(B),
	write('Wybrany sport to '), write(A), write('. Jutro bedzie '), write(B), nl,
	wyrokujUbior(A, B, C),
	wyjasnij(C). 
		
wyrokujUbior(bieganie, kapusniaczek, przeciwdeszczowa).
wyrokujUbior(bieganie, mzawka , przeciwdeszczowa).
wyrokujUbior(bieganie, deszcz , przeciwdeszczowa).
wyrokujUbior(bieganie, snieg , przeciwdeszczowa).
wyrokujUbior(bieganie, grad, zaniechaj).
wyrokujUbior(bieganie, silnyWiatr, windstopper).
wyrokujUbior(bieganie, wichura, windstopper).
wyrokujUbior(bieganie, bezchmurnie, koszulka).
wyrokujUbior(bieganie, upalnie, koszulka).
wyrokujUbior(bieganie, bezchmurnie, koszulka). 
wyrokujUbior(bieganie, _, bluza).

wyrokujUbior(rower, kapusniaczek, przeciwdeszczowa).
wyrokujUbior(rower, mzawka , przeciwdeszczowa).
wyrokujUbior(rower, deszcz , przeciwdeszczowa).
wyrokujUbior(rower, snieg , zaniechaj).
wyrokujUbior(rower, grad, zaniechaj).
wyrokujUbior(rower, mroz, termiczna).
wyrokujUbior(rower, silnyWiatr, zaniechaj).
wyrokujUbior(rower, wichura, zaniechaj).
wyrokujUbior(rower, bezchmurnie, krotkieSpodenki).
wyrokujUbior(rower, upalnie, koszulka).
wyrokujUbior(rower, bezchmurnie, koszulka).
wyrokujUbior(rower, _, bluza).

wyrokujUbior(plywanie, duzaWilgotnosc, koszulka) :- nl, !.
wyrokujUbior(plywanie, polnocny, palto) :- nl, !.
wyrokujUbior(plywanie, niskieCisnienie, przeciwdeszczowa) :- nl, !.
wyrokujUbior(plywanie, niskaWilgotnosc, krotkieSpodenki) :- nl, !.
wyrokujUbior(plywanie, _, zaniechaj) :- nl, !.

wyrokujUbior(sanki, poludniowy, windstopper) :- nl, !.
wyrokujUbior(sanki, deszcz, palto) :- nl, !.
wyrokujUbior(sanki, silnyWiatr, windstopper) :- nl, !.
wyrokujUbior(sanki, upalnie, invalid) :- nl, !.
wyrokujUbior(sanki, _, zaniechaj) :- nl, !.

wyrokujUbior(pilkaNozna, wysokieCisnienie, bluza) :- nl, !.
wyrokujUbior(pilkaNozna, grad, ocieplana) :- nl, !.
wyrokujUbior(pilkaNozna, snieg, bluza) :- nl, !.
wyrokujUbior(pilkaNozna, calkowiteZachmurzenie, palto) :- nl, !.
wyrokujUbior(pilkaNozna, _, zaniechaj) :- nl, !.

wyrokujUbior(_, _, invalid).

wyjasnij(przeciwdeszczowa) :-
	write('Najlepiej ubrac kurtke przeciwdeszczowa'),nl, !. 

wyjasnij(zaniechaj) :-
	write('Sugerowalbym zaniechac wszelkich sportow'),nl, !. 

wyjasnij(windstopper) :-
	write('Najlepszym wyjsciem jest przygotowac sobie windstopper'),nl, !. 

wyjasnij(termiczna) :-
	write('Bielizna termiczna bedzie najlepszym wyjsciem'),nl, !. 

wyjasnij(bluza) :-
	write('Bluza to to, co potrzebujesz'),nl, !. 

wyjasnij(koszulka) :-
	write('Najlepiej wybrac zwykla koszulke'),nl, !. 

wyjasnij(krotkieSpodenki) :-
	write('Dobrym wyjsciem byloby przygotowac sobie krotkie spodenki'),nl, !. 

wyjasnij(palto) :-
	write('Czas wyciagnac z szafy palto'),nl, !. 

wyjasnij(ocieplana) :-
	write('Bluza ocieplana bardzo sie przyda'),nl, !. 

wyjasnij(invalid) :-
	write('Nie moge przewidziec co powinienes wybrac'),nl, !. 

%useful

pogodaMozeByc(ulewa) :- 
	uzytkownikPowiedzial(silaWiatru, silnyWiatr),
	uzytkownikPowiedzial(poraRoku, wiosna), !. 
	
pogodaMozeByc(grad) :- 
	uzytkownikPowiedzial(poraRoku, jesien),
	uzytkownikPowiedzial(naslonecznienie, calkowiteZachmurzenie), !.
	
pogodaMozeByc(snieg) :- 
	uzytkownikPowiedzial(poraRoku, zima),
	uzytkownikPowiedzial(wilgotnosc, niskaWilgotnosc), !.
	
pogodaMozeByc(ulewa) :- 
	uzytkownikPowiedzial(kierunekWiatru, poludniowy),
	uzytkownikPowiedzial(temperatura, upalnie), !.

pogodaMozeByc(duzaWilgotnosc) :- 
	uzytkownikPowiedzial(poraRoku, jesien),
	uzytkownikPowiedzial(cisnienie, niskieCisnienie), !.
	
pogodaMozeByc(poludniowy) :- 
	uzytkownikPowiedzial(kierunekWiatru, poludniowy),
	uzytkownikPowiedzial(temperatura, umiarkowanieCieplo), !.
	
pogodaMozeByc(wysokieCisnienie) :- 
	uzytkownikPowiedzial(cisnienie, normalneCisnienie),
	uzytkownikPowiedzial(temperatura, mroz), !.

pogodaMozeByc(bezwietrznie) :- 
	uzytkownikPowiedzial(silaWiatru, bezwietrznie),						  
	uzytkownikPowiedzial(naslonecznienie, bezchmurnie), !.

pogodaMozeByc(upalnie) :- 
	uzytkownikPowiedzial(temperatura, umiarkowanieCieplo),
	uzytkownikPowiedzial(kierunekWiatru, poludniowy), !.

% WIATR

pogodaMozeByc(bezwietrznie) :- 
	uzytkownikPowiedzial(silaWiatru, bezwietrznie),
	uzytkownikPowiedzial(naslonecznienie, lekkieZachmurzenie), !.
							   					   
pogodaMozeByc(lekkiWiatr)  :- 
	uzytkownikPowiedzial(silaWiatru, bezwietrznie),
	uzytkownikPowiedzial(naslonecznienie, calkowiteZachmurzenie), !.
							   					   
pogodaMozeByc(lekkiWiatr) :- 
	uzytkownikPowiedzial(silaWiatru, lekkiWiatr),
	uzytkownikPowiedzial(naslonecznienie, bezchmurnie), !. 
							   
pogodaMozeByc(lekkiWiatr) :- 
	uzytkownikPowiedzial(silaWiatru, lekkiWiatr),
	uzytkownikPowiedzial(naslonecznienie, lekkieZachmurzenie), !.
							   
pogodaMozeByc(lekkiWiatr) :- 
	uzytkownikPowiedzial(silaWiatru, lekkiWiatr),
	uzytkownikPowiedzial(naslonecznienie, calkowiteZachmurzenie), !.
							   		 			    
pogodaMozeByc(silnyWiatr) :- 
	uzytkownikPowiedzial(silaWiatru, silnyWiatr),
	uzytkownikPowiedzial(naslonecznienie, bezchmurnie), !.
							   
pogodaMozeByc(silnyWiatr) :- 
	uzytkownikPowiedzial(silaWiatru, silnyWiatr),
	uzytkownikPowiedzial(naslonecznienie, lekkieZachmurzenie), !.
							   
pogodaMozeByc(silnyWiatr) :- 
	uzytkownikPowiedzial(silaWiatru, silnyWiatr),
	uzytkownikPowiedzial(naslonecznienie, calkowiteZachmurzenie), !.
							   
pogodaMozeByc(silnyWiatr) :- 
	uzytkownikPowiedzial(silaWiatru, wichura),
	uzytkownikPowiedzial(naslonecznienie, bezchmurnie), !.
							   
pogodaMozeByc(wichura) :- 
	uzytkownikPowiedzial(silaWiatru, wichura),
	uzytkownikPowiedzial(naslonecznienie, calkowiteZachmurzenie), !.
							   							   

pogodaMozeByc(bezchmurnie) :- 
	uzytkownikPowiedzial(naslonecznienie, bezchmurnie),
	uzytkownikPowiedzial(wilgotnosc, niska), !.
							 
pogodaMozeByc(lekkieZachmurzenie) :- 
	uzytkownikPowiedzial(slonecznienie, bezchmurnie),
	uzytkownikPowiedzial(wilgotnosc, normalna), !.
							 
pogodaMozeByc(lekkieZachmurzenie) :- 
	uzytkownikPowiedzial(naslonecznienie, bezchmurnie),
	uzytkownikPowiedzial(wilgotnosc, duza), !.
							 
pogodaMozeByc(lekkieZachmurzenie) :- 
	uzytkownikPowiedzial(naslonecznienie, lekkieZachmurzenie),
	uzytkownikPowiedzial(wilgotnosc, niska), !.
							 
pogodaMozeByc(lekkieZachmurzenie) :- 
	uzytkownikPowiedzial(naslonecznienie, lekkieZachmurzenie),
	uzytkownikPowiedzial(wilgotnosc, normalna), !.
							 
pogodaMozeByc(lekkieZachmurzenie) :- 
	uzytkownikPowiedzial(naslonecznienie, lekkieZachmurzenie),
	uzytkownikPowiedzial(wilgotnosc, duza), !.
							 
pogodaMozeByc(lekkieZachmurzenie) :- 
	uzytkownikPowiedzial(naslonecznienie, calkowiteZachmurzenie),
	uzytkownikPowiedzial(wilgotnosc, niska), !.
							 
pogodaMozeByc(calkowiteZachmurzenie) :- 
	uzytkownikPowiedzial(naslonecznienie, calkowiteZachmurzenie),
	uzytkownikPowiedzial(wilgotnosc, normalna), !.
							 
pogodaMozeByc(calkowiteZachmurzenie) :- 
	uzytkownikPowiedzial(naslonecznienie, calkowiteZachmurzenie),
	uzytkownikPowiedzial(wilgotnosc, duza), !.		

pogodaMozeByc(mroz) :- 
	uzytkownikPowiedzial(temperatura, mroz),
	uzytkownikPowiedzial(kierunekWiatru, polnocny), !.
						 			

% temperatura
pogodaMozeByc(mroz) :- 
	uzytkownikPowiedzial(temperatura, mroz),
	uzytkownikPowiedzial(kierunekWiatru, wschodni), !.

pogodaMozeByc(przymrozek) :- 
	uzytkownikPowiedzial(temperatura, mroz),
	uzytkownikPowiedzial(kierunekWiatru, poludniowy), !.

pogodaMozeByc(przymrozek) :- 
	uzytkownikPowiedzial(temperatura, mroz),
	uzytkownikPowiedzial(kierunekWiatru, zachodni), !.
					   		 
pogodaMozeByc(mroz) :- 
	uzytkownikPowiedzial(temperatura, przymrozek),
	uzytkownikPowiedzial(kierunekWiatru, polnocny), !.

pogodaMozeByc(przymrozek) :- 
	uzytkownikPowiedzial(temperatura, przymrozek),
	uzytkownikPowiedzial(kierunekWiatru, wschodni), !.

pogodaMozeByc(umiarkowanieCieplo) :- 
	uzytkownikPowiedzial(temperatura, przymrozek),
	uzytkownikPowiedzial(kierunekWiatru, poludniowy), !.

pogodaMozeByc(umiarkowanieCieplo) :- 
	uzytkownikPowiedzial(temperatura, przymrozek),
	uzytkownikPowiedzial(kierunekWiatru, zachodni), !.					   		 				   		 
					   		 
pogodaMozeByc(przymrozek) :- 
	uzytkownikPowiedzial(temperatura, umiarkowanieCieplo),
	uzytkownikPowiedzial(kierunekWiatru, polnocny), !.

pogodaMozeByc(umiarkowanieCieplo) :- 
	uzytkownikPowiedzial(temperatura, umiarkowanieCieplo),
	uzytkownikPowiedzial(kierunekWiatru, wschodni), !.

pogodaMozeByc(umiarkowanieCieplo) :- 
	uzytkownikPowiedzial(temperatura, umiarkowanieCieplo),
	uzytkownikPowiedzial(kierunekWiatru, zachodni), !.
				   		 
pogodaMozeByc(umiarkowanieCieplo) :- 
	uzytkownikPowiedzial(temperatura, upalnie),
	uzytkownikPowiedzial(kierunekWiatru, polnocny), !.

pogodaMozeByc(umiarkowanieCieplo) :- 
	uzytkownikPowiedzial(temperatura, upalnie),
	uzytkownikPowiedzial(kierunekWiatru, wschodni), !.

pogodaMozeByc(upalnie) :- 
	uzytkownikPowiedzial(temperatura, upalnie),
	uzytkownikPowiedzial(kierunekWiatru, poludniowy), !.

pogodaMozeByc(upalnie) :- 
	uzytkownikPowiedzial(temperatura, upalnie),
	uzytkownikPowiedzial(kierunekWiatru, zachodni), !.
					      
%cisnienie
pogodaMozeByc(niskieCisnienie) :- 
	uzytkownikPowiedzial(cisnienie, niskieCisnienie),
	uzytkownikPowiedzial(poraRoku, jesien), !.
	
pogodaMozeByc(normalneCisnienie) :- 
	uzytkownikPowiedzial(cisnienie, wysokieCisnienie),
	uzytkownikPowiedzial(naslonecznienie, lekieZachmurzenie), !.

pogodaMozeByc(niskieCisnienie) :- 
	uzytkownikPowiedzial(cisnienie, wysokieCisnienie),
	uzytkownikPowiedzial(wiatr, poludniowy), !.
	
pogodaMozeByc(wysokieCisnienie) :- 
	uzytkownikPowiedzial(poraRoku, jesien),
	uzytkownikPowiedzial(temperatura, mroz), !.
	
pogodaMozeByc(normalneCisnienie) :- 
	uzytkownikPowiedzial(poraRoku, lato),
	uzytkownikPowiedzial(opady, kapusniaczek), !.
	
%kierunek wiatru
pogodaMozeByc(polnocny) :- 
	uzytkownikPowiedzial(kierunekWiatru, poludniowy),
	uzytkownikPowiedzial(opady, ulewa), !.
	
pogodaMozeByc(wschodni) :- 
	uzytkownikPowiedzial(poraRoku, wiosna),
	uzytkownikPowiedzial(cisnienie, niskieCisnienie), !.
	
pogodaMozeByc(zachodni) :- 
	uzytkownikPowiedzial(kierunekWiatru, zachodni),
	uzytkownikPowiedzial(wilgotnosc, normalnaWilgotnosc), !.
%
pogodaMozeByc(polnocny) :- 
	uzytkownikPowiedzial(kierunekWiatru, polnocny),
	uzytkownikPowiedzial(cisnienie, normalneCisnienie), !.
	
pogodaMozeByc(poludniowy) :- 
	uzytkownikPowiedzial(kierunekWiatru, polnocny),
	uzytkownikPowiedzial(tempreatura, mroz), !.
	 
pogodaMozeByc(wschodni) :- 
	uzytkownikPowiedzial(kierunekWiatru, zachodni),
	uzytkownikPowiedzial(opady, grad), !.
	
pogodaMozeByc(zachodni) :- 
	uzytkownikPowiedzial(silaWiatru, silnyWiatr),
	uzytkownikPowiedzial(wilgotnosc, niskaWilgotnosc), !.
	
%opady
pogodaMozeByc(brakOpadow) :- 
	uzytkownikPowiedzial(opady, mzawka),
	uzytkownikPowiedzial(cisnienie, wysokieCisnienie), !.
	
pogodaMozeByc(kapusniaczek) :- 
	uzytkownikPowiedzial(opady, brakOpadow),
	uzytkownikPowiedzial(silaWiatru, silnyWiatr), !.
	
pogodaMozeByc(deszcz) :- 
	uzytkownikPowiedzial(naslonecznienie, lekkieZachmurzenie),
	uzytkownikPowiedzial(silaWiatru, lekkiWiatr), !.

pogodaMozeByc(brakOpadow) :- 
	uzytkownikPowiedzial(naslonecznienie, bezchmurnie),
	uzytkownikPowiedzial(silaWiatru, bezwietrznie), !.
	
pogodaMozeByc(mzawka) :- 
	uzytkownikPowiedzial(opady, brakOpadow),
	uzytkownikPowiedzial(poraRoku, jesien), !.
	
pogodaMozeByc(kapusniaczek) :- 
	uzytkownikPowiedzial(opady, deszcz),
	uzytkownikPowiedzial(wilgotnosc, normalnaWilgotnosc), !.
	
pogodaMozeByc(deszcz) :- 
	uzytkownikPowiedzial(temperatura, upalnie),
	uzytkownikPowiedzial(cisnienie, niskieCisnienie), !.
	
pogodaMozeByc(snieg) :- 
	uzytkownikPowiedzial(poraRoku, jesien),
	uzytkownikPowiedzial(kierunekWiatru, poludniowy), !.
	
pogodaMozeByc(grad) :- 
	uzytkownikPowiedzial(poraRoku, wiosna),
	uzytkownikPowiedzial(naslonecznienie, calkowiteZachmurzenie), !.

%wilgotnosc
pogodaMozeByc(duzaWilgotnosc) :- 
	uzytkownikPowiedzial(silaWiatru, bezwietrznie),
	uzytkownikPowiedzial(temperatura, upalnie), !.
	
pogodaMozeByc(normalnaWilgotnosc) :- 
	uzytkownikPowiedzial(cisnienie, normalneCisnienie),
	uzytkownikPowiedzial(poraRoku, lato), !.
	
pogodaMozeByc(normalnaWilgotnosc) :- 
	uzytkownikPowiedzial(opady, kapusniaczek),
	uzytkownikPowiedzial(silaWiatru, lekkiWiatr), !.
	
pogodaMozeByc(niskaWilgotnosc) :- 
	uzytkownikPowiedzial(poraRoku, lato),
	uzytkownikPowiedzial(temperatura, umiarkowanieCieplo), !.
					      
					      
	
:- dynamic([zapamietaneOdpowiedzi/2]).
					      
					      				      
wykasujZapamietaneOdpowiedzi :- retract(zapamietaneOdpowiedzi(_,_)),fail.
wykasujZapamietaneOdpowiedzi.


podajSport(A) :- 
		zapytajSport(sport, A),
		write(A).
		
					    
uzytkownikPowiedzial(Q,A) :- zapamietaneOdpowiedzi(Q,A), !.

uzytkownikPowiedzial(Q,A) :- \+ zapamietaneOdpowiedzi(Q,_),
                  nl,nl,
                  zapytaj(Q, A). 
                  
		
zapytajSport(sport, A) :-
	write('Podaj sport, dostepne wartosci to: bieganie, pilkaNozna, rower, sanki, plywanie'),
	nl,nl,
	read(A),
	asserta(zapamietaneOdpowiedzi(sport,A)).
	
zapytaj(silaWiatru, A) :-
	write('Podaj jaka jest sila wiatru, dostepne wartosci to: bezwietrznie, lekkiWiatr, silnyWiatr, wichura'),
	nl,nl,
	read(B),
	asserta(zapamietaneOdpowiedzi(silaWiatru,B)),
	A = B.
	
zapytaj(naslonecznienie, A) :- 
	write('Podaj jakie jest naslonecznienie, dostepne wartosci to: bezchmurnie, lekkieZachmurzenie, calkowiteZachmurzenie'),
	nl,nl,
	read(B),
	asserta(zapamietaneOdpowiedzi(naslonecznienie,B)),
	A = B.   	
	
zapytaj(poraRoku, A) :-
	write('Podaj jaka jest pora roku, dostepne wartosci to: wiosna, lato, jesien, zima'),
	nl,nl,
	read(B),
	asserta(zapamietaneOdpowiedzi(poraRoku,B)),
	A = B.
	
zapytaj(temperatura, A) :-
	write('Podaj jaka jest temperatura, dostepne wartosci to: mroz, przymrozek, umiarkowanieCieplo, upalnie'),
	nl,nl,
	read(B),
	asserta(zapamietaneOdpowiedzi(temperatura,B)),
	A = B.	
	
zapytaj(cisnienie, A) :-
	write('Podaj jakie jest cisnienie, dostepne wartosci to: niskieCisnienie, normalneCisnienie, wysokieCisnienie'),
	nl,nl,
	read(B),
	asserta(zapamietaneOdpowiedzi(cisnienie,B)),
	A = B.
	
zapytaj(kierunekWiatru, A) :-
	write('Podaj jaki jest kierunek wiatru, dostepne wartosci to: poludniowy, polnocny, zachodni, wschodni'),
	nl,nl,
	read(B),
	asserta(zapamietaneOdpowiedzi(kierunekWiatru,B)),
	A = B.
	
zapytaj(opady, A) :-
	write('Podaj jakie sa opady, dostepne wartosci to: brakOpadow, mzawka, kapusniaczek, desz, ulewa, grad, snieg'),
	nl,nl,
	read(B),
	asserta(zapamietaneOdpowiedzi(opady,B)),
	A = B.
	
zapytaj(wilgotnosc, A) :-
	write('Podaj jaka jest wilgotnosc, dostepne wartosci to: niskaWilgotnosc, normalnaWilgotnosc, duzaWilgotnosc'),
	nl,nl, 
	read(B),
	asserta(zapamietaneOdpowiedzi(wilgotnosc,B)),
	A = B.
				