location(toronto,canada). 
location(shanghai,china). 
location(nyc,usa). 
location(la,usa). 
location(austin,usa). 
location(edmonton,canada).
location(vancouver,canada).
location(chicago,usa). 
location(montreal,canada). 
location(hongkong,china). 
location(seoul,southKorea). 
location(icheon,southKorea).
location(pyongyang,northKorea). 
location(nampo,northKorea). 
location(london,uk). 
location(ireland,uk).

flight(aa300,americanAirlines,nyc,austin). 
flight(ac130,airCanada,toronto,la). 
flight(ua420,unitedAirlines,chicago,toronto). 
flight(ac200,airCanada,edmonton,vancouver).
flight(ac400,airCanada,edmonton,london). 
flight(ac300,unitedAirlines,toronto,shanghai). 
flight(ac140,airCanada,montreal,austin). 
flight(ac500,unitedAirlines,vancouver,nyc). 
flight(ac600,airCanada,vancouver,hongkong). 
flight(ac700,airCanada,toronto,vancouver). 
flight(ac800,airCanada,vancouver,shanghai). 

flight(ac900,airCanada,toronto,edmonton).
flight(ac1000,airCanada,toronto,montreal). 

dtime(ac130,toronto,1300). 
dtime(ua420, chicago,1530). 
dtime(aa300,nyc,1200). 
dtime(ac200,edmonton,0700). 
dtime(ac400,edmonton,0900). 
dtime(ac300,toronto,1730). 
dtime(ac140,montreal, 0630). 
dtime(ac500,vancouver,0830). 
dtime(ac600,vancouver, 1100). 
dtime(ac700,toronto,0200). 
dtime(ac800,vancouver,0545). 
dtime(ac900,toronto,0530). 
dtime(ac1000,toronto,0600).

atime(ac130,la,1430). 
atime(ua420, toronto ,1710). 
atime(aa300,austin,1410). 
atime(ac200,vancouver,1000). 
atime(ac400,london,1545). 
atime(ac300,shanghai,2300). 
atime(ac140,austin, 1330). 
atime(ac500,nyc,1245). 
atime(ac600,hongkong, 0500). 
atime(ac700,vancouver,0300). 
atime(ac800,shanghai,2300). 
atime(ac900,edmonton,0700).
atime(ac1000,montreal,0730).


article(a).
article(an).
article(any).
common_noun(flight, X) :- flight(X,B,C,D).
common_noun(city, X) :- location(X,A).
common_noun(country, X) :- location(A,X).
common_noun(time2,X) :- atime(X,B,C).
common_noun(time2,X) :- dtime(X,B,C).
common_noun(time2, X) :- dtime(A,B,C), atime(A,D,E), Flighttime is E - C, X = Flighttime.
preposition(to, A, X) :- flight(A,B,C,X).
preposition(to, A, X) :- flight(A,B,C,D), location(D,X).
preposition(from, A, X) :- flight(A,B,X,D).
preposition(from, A, X) :- flight(A,B,C,D), location(C,X).
preposition(in,X, A) :- location(X,A).
preposition(in,X, A) :- location(A,X).
preposition(with, Y,X) :- flight(X,A,B,C).
preposition(with, Y,X) :- flight(X,A,B,C).
preposition(with, Y,X) :- adjective(arrival, X).
preposition(with, Y,X) :- adjective(departure, X).
adjective(domestic, X) :- flight(X,B,C,D), location(C,F), location(D,F).
adjective(international, X) :- flight(X,B,C,D), location(C,X1), location(D,Y1), not X1 = Y1.
adjective(airCanada, X) :- flight(X,B,C,D), B = airCanada.
adjective(unitedAirlines, X) :- flight(X,B,C,D), B = unitedAirlines.
adjective(long, X) :- 
	flight(X,A,B,C), dtime(X,B,J), atime(X,C,K),
	flight(Y,M,B,C), dtime(Y,B,T), atime(Y,C,U),
	Flighttime1 is K - J, Flighttime2 is U - T,
	Flighttime1 > Flighttime2.
adjective(shortest, X) :- 
	flight(X,A,B,C), dtime(X,B,J), atime(X,C,K),
	flight(Y,M,B,C), dtime(Y,B,T), atime(Y,C,U),
	Flighttime1 is K - J, Flighttime2 is U - T,
	Flighttime1 < Flighttime2.
adjective(arrival, X) :- flight(X,B,C,D), atime(X,D,E).
adjective(departure, X) :- flight(X,B,C,D), dtime(X,C,E).
adjective(morning, X) :- flight(X,B,C,D), dtime(X,C,E), E >= 0500, E < 0900.
adjective(morning, X) :- flight(X,B,C,D), atime(X,D,E), E >= 0500, E < 0900.
adjective(day, X) :- flight(X,B,C,D), dtime(A,C,E), E >= 0900, E < 1200.
adjective(day, X) :- flight(X,B,C,D), atime(A,D,E), E >= 0900, E < 1200.
adjective(afternoon, X) :- flight(X,B,C,D), dtime(X,C,E), E >= 1200, E < 1700.
adjective(afternoon, X) :- flight(X,B,C,D), atime(X,D,E), E >= 1200, E < 1700.
adjective(evening, X) :- flight(X,B,C,D), dtime(A,C,E), E >= 1700, E < 2200.
adjective(evening, X) :- flight(X,B,C,D), atime(A,D,E), E >= 1700, E < 2200.

adjective(canadian,X) :- location(X,canada).

adjective(canadian,X) :- flight(X,B,C,D), location(C,canada).
adjective(canadian,X) :- flight(X,B,C,D), location(D,canada).

adjective(chinese,X) :- location(X,china).

adjective(chinese,X) :- flight(X,B,C,D), location(C,china).
adjective(chinese,X) :- flight(X,B,C,D), location(D,china).

adjective(american,X) :- location(X,usa).

adjective(american,X) :- flight(X,B,C,D), location(C,usa).
adjective(american,X) :- flight(X,B,C,D), location(D,usa).
adjective(european,X) :- location(X,uk).
adjective(european,X) :- flight(X,B,C,D), location(C,uk).
adjective(european,X) :- flight(X,B,C,D), location(D,uk).
adjective(korean,X) :- flight(X,B,C,D), location(C,southKorea).
adjective(korean,X) :- flight(X,B,C,D), location(D,southKorea).
adjective(korean,X) :- flight(X,B,C,D), location(C,northKorea).
adjective(korean,X) :- flight(X,B,C,D), location(D,northKorea).
proper_noun(canada).
proper_noun(X) :- not article(X), not common_noun(X,_), not adjective(X,_), not preposition(X,_,_).



what(Words,Ref) :- np(Words, Ref).
np([Name],Name) :- proper_noun(Name).
np([Art|Rest],Who) :- article(Art), np2(Rest,Who).   
np2([Adj|Rest],Who) :- adjective(Adj,Who), np2(Rest,Who).   
np2([Noun|Rest],Who) :- common_noun(Noun,Who), mods(Rest,Who).   
mods([],_).   
mods(Words,Who) :- append2(Start,End,Words),pp(Start,Who),mods(End,Who).             /* and an End list  */   
pp([Prep|Rest],Who) :- preposition(Prep,Who,Who2), np(Rest,Who2).  
append2([],X,X).  
append2([H|T],X,[H|Y]) :- append2(T,X,Y).

