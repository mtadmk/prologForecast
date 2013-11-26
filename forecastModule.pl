:- module(fc,[
		
	]).

start :-
	write('Prognozowanie pogody.'),nl,nl,
	wykasujZapamietaneOdpowiedzi,
   	szukajUbioru.
   	
szukajUbioru :-
	podajSport(A),
	pogodaMozeByc(B),
	write('wyrokuje z '), write(A), write(' i '), write(B), nl,
	wyrokujUbior(A,B,C),
%	write(C),nl,
	write('wyjasniam '),nl, 
	wyjasnij(C),
	fail. 

szukajUbioru.  
		
wyrokujUbior(bieganie, ulewa, przeciwdeszczowa).
wyrokujUbior(rower, grad, zaniechaj).
wyrokujUbior(hulajnoga, wichura, windstopper).
wyrokujUbior(rower, snieg, termiczna).
wyrokujUbior(brak, ulewa, przeciwdeszczowa).
wyrokujUbior(plywanie, duzaWilgotnosc, koszulka).
wyrokujUbior(sanki, poludniowy, windstopper).
wyrokujUbior(pilkaNozna, wysokieCisnienie, bluza).
wyrokujUbior(bieganie, bezwietrznie, bluza).
wyrokujUbior(bieganie, upalnie, krotkieSpodenki).
wyrokujUbior(brak, mzawka, przeciwdeszczowa).
wyrokujUbior(sanki, mroz, palto).
wyrokujUbior(rower, niskaWilgotnosc, ocieplana).
wyrokujUbior(_, _, invalid).


wyjasnij(przeciwdeszczowa) :-
	write('przeciwdeszczowa'),nl, !. 

wyjasnij(zaniechaj) :-
	write('zaniechaj'),nl, !. 

wyjasnij(windstopper) :-
	write('windstopper'),nl, !. 

wyjasnij(termiczna) :-
	write('termiczna'),nl, !. 

wyjasnij(bluza) :-
	write('bluza'),nl, !. 

wyjasnij(koszulka) :-
	write('koszulka'),nl, !. 


wyjasnij(krotkieSpodenki) :-
	write('krotkieSpodenki'),nl, !. 

wyjasnij(palto) :-
	write('palto'),nl, !. 

wyjasnij(ocieplana) :-
	write('ocieplana'),nl, !. 

wyjasnij(invalid) :-
	write('nie moge przewidziec'),nl, !. 

%useful

pogodaMozeByc(ulewa) :- 
	uzytkownikPowiedzial(silaWiatru, silnyWiatr),
	uzytkownikPowiedzial(poraRoku, wiosna), !. 
	
pogodaMozeByc(grad) :- 
	uzytkownikPowiedzial(poraRoku, jesien),
	uzytkownikPowiedzial(naslonecznienie, calkowiteZachmurzenie), !.
							   
pogodaMozeByc(wichura) :- 
	uzytkownikPowiedzial(silaWiatru, wichura),
	uzytkownikPowiedzial(naslonecznienie, lekkieZachmurzenie), !.
	
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
	uzytkownikPowiedzial(cisnienie, normalmneCisnienie),
	uzytkownikPowiedzial(temperatura, mroz), !.

pogodaMozeByc(bezwietrznie) :- 
	uzytkownikPowiedzial(silaWiatru, bezwietrznie),						  
	uzytkownikPowiedzial(naslonecznienie, bezchmurnie), !.

pogodaMozeByc(upalnie) :- 
	uzytkownikPowiedzial(temperatura, umiarkowanieCieplo),
	uzytkownikPowiedzial(kierunekWiatru, poludniowy), !.
	
pogodaMozeByc(mzawka) :- 
	uzytkownikPowiedzial(opady, brakOpadow),
	uzytkownikPowiedzial(naslonecznienie, slonecznie), !.
	
pogodaMozeByc(niskaWilgotnosc) :- 
	uzytkownikPowiedzial(poraRoku, zima),
	uzytkownikPowiedzial(silaWiatru, wichura), !.

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
		zapytaj(sport, A),
		write(A).
		
					    
uzytkownikPowiedzial(Q,A) :- zapamietaneOdpowiedzi(Q,A), !.%, write('pamietam'), nl, !.

uzytkownikPowiedzial(Q,A) :- \+ zapamietaneOdpowiedzi(Q,_),
                  nl,nl,
                  zapytaj(Q, A), 
%                  write('nie pamietam '),
%                  write(A),   
                  Odpowiedz = A,
                  asserta(zapamietaneOdpowiedzi(Q,Odpowiedz)). 
                  
		
zapytaj(sport, A) :-
	write('Podaj sport'),
	nl,nl,
	read(B),
	jakiSport(B, A).
	
zapytaj(silaWiatru, A) :-
	write('Podaj jaka jest sila wiatru'),
	nl,nl,
	read(B),
	jakaSilaWiatru(B,A).
	
zapytaj(naslonecznienie, A) :-
	write('Podaj jakie jest naslonecznienie'),
	nl,nl,
	read(B),
	jakieNaslonecznienie(B,A).     	
	
zapytaj(poraRoku, A) :-
	write('Podaj jaka jest pora roku'),
	nl,nl,
	read(B),
	jakaPoraRoku(B,A).
	
zapytaj(temperatura, A) :-
	write('Podaj jaka jest temperatura'),
	nl,nl,
	read(B),
	jakaTemperatura(B,A).	
	
zapytaj(cisnienie, A) :-
	write('Podaj jakie jest cisnienie'),
	nl,nl,
	read(B),
	jakieCisnienie(B,A).	
	
zapytaj(kierunekWiatru, A) :-
	write('Podaj jaki jest kierunek wiatru'),
	nl,nl,
	read(B),
	jakiKierunekWiatru(B,A).
	
zapytaj(opady, A) :-
	write('Podaj jakie s� opady'),
	nl,nl,
	read(B),
	jakieOpady(B,A).	
	
zapytaj(wilgotnosc, A) :-
	write('Podaj jaka jest wilgotnosc'),
	nl,nl, 
	read(B),
	jakaWilgotnosc(B, A).
					 
					 
opisz(bezwietrznie) :-
	nl,
	write('Bedzie bezwietrznie.'),nl.
	
jakaWilgotnosc(niska, niska) :- nl, !.	
jakaWilgotnosc(duza, duza) :- nl, !.
jakaWilgotnosc(_, normalna) :- nl, !.
	
jakiSport(bieganie, bieganie) :- nl, !.
jakiSport(rower, rower) :- nl, !.
jakiSport(hulajnoga, hulajnoga) :- nl, !.
jakiSport(plywanie, plywanie) :- nl, !.
jakiSport(sanki, sanki) :- nl, !.
jakiSport(pilkaNozna, pilkaNozna) :- nl, !.
jakiSport(_, brak) :- nl, !.

jakaSilaWiatru(lekkiWiatr, lekkiWiatr) :- nl, !.
jakaSilaWiatru(silnyWiatr, silnyWiatr) :- nl, !.
jakaSilaWiatru(wichura, wichura) :- nl, !.
jakaSilaWiatru(_, bezwietrznie) :- nl, !.

jakieNaslonecznienie(lekkieZachmurzenie,lekkieZachmurzenie) :- nl, !.
jakieNaslonecznienie(calkowiteZachmurzenie,calkowiteZachmurzenie) :- nl, !.
jakieNaslonecznienie(_,bezchmurnie) :- nl, !.

jakaPoraRoku(wiosna,wiosna) :- nl, !.
jakaPoraRoku(jesien,jesien) :- nl, !. 
jakaPoraRoku(zima,zima) :- nl, !.
jakaPoraRoku(_,lato) :- nl, !.

jakaTemperatura(mroz,mroz) :- nl, !.
jakaTemperatura(przymrozek,przymrozek) :- nl, !.
jakaTemperatura(upalnie,upalnie) :- nl, !.
jakaTemperatura(_,umiarkowanieCieplo) :- nl, !.

jakieCisnienie(niskieCisnienie,niskieCisnienie) :- nl, !.
jakieCisnienie(wysokieCisnienie,wysokieCisnienie) :- nl, !.
jakieCisnienie(_,normalneCisnienie) :- nl, !.

jakiKierunekWiatru(wschodni,wschodni) :- nl, !.
jakiKierunekWiatru(zachodni,zachodni) :- nl, !.
jakiKierunekWiatru(poludniowy,poludniowy) :- nl, !.
jakiKierunekWiatru(polnocny,polnocny) :- nl, !.
jakiKierunekWiatru(_,brak) :- nl, !.

jakieOpady(kapusniaczek,kapusniaczek) :- nl, !.
jakieOpady(ulewa,ulewa) :- nl, !.
jakieOpady(grad,grad) :- nl, !.
jakieOpady(deszcz,deszcz) :- nl, !.
jakieOpady(mzawka,mzawka) :- nl, !.
jakieOpady(_ ,brakOpadow) :- nl, !.


