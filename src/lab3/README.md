# Laboratorium nr 3 - programowanie procesów w Erlangu

## Przed zajęciami

Zapoznaj się z:

- Tworzeniem procesów.
- Rejestracja procesów.
- Komunikacja między procesami.

## Cele zajęć

- Poznanie sposobów wykorzystania Erlangowych procesów.

## Przebieg zajęć

### Ping - Pong

- Napisz moduł pingpong, który będzie eksportował funkcje:
  - start/0, która utworzy 2 procesy i zarejestruje je pod nazwami ping i pong,
  - stop/0, która zakończy oba procesy,
  - play/1, która wyśle wiadomość z liczbą całkowitą N do procesu ping.
- Po otrzymaniu wiadomości, proces ping ma rozpocząć wymianę N wiadomości z procesem pong. Przy odebraniu każdej wiadomości procesy mają wypisać na standardowe wyjście informację o przebiegu odbijania.
- Dla zwiększenia czytelności działania warto użyć funkcji timer:sleep(Milisec).
- Procesy ping i pong powinny samoczynnie kończyć działanie po 20 sekundach bezczynności.
- Zmodyfikuj proces ping by przechowywał stan - sumę wszystkich liczb z komunikatów, które otrzymał. Dodaj tę sumę do informacji wypisywanej po odebraniu komunikatu.

### Obliczenia równoległe

Dane są 2 listy punktów na płaszczyźnie 2D:

- lista lokalizacji czujników jakości powietrza, których jest np. 1000
- lista rozmieszczenia osób, które chcą zobaczyć jakikolwiek czujnik - jest ich np. 20000

Szukamy osoby, która jest najbliżej dowolnego czujnika.

- Zdefiniuj nowy moduł, np. sensor_dist.
- Przygotujmy dane - 2 listy par liczb całkowitych z zakresu 0-10000, wygenerowane losowo. Użyj list comprehensions i random:uniform(10000). Napisz funkcję pomocniczą get_rand_locations(Number).
- Napisz pomocniczą funkcję dist({X1, Y1}, {X2, Y2}), która zwróci dystans między punktami.

Wersja sekwencyjna poszukiwania:

- Napisz funkcję find_for_person(PersonLocation, SensorsLocations), która wyszuka i zwróci {dystans, {pozycjaOsoby, pozycjaCzujnika}}. Do wyznaczenie wszystkich dystansów można użyć list comprehension. Wyszukanie najbliższej może zrealizować funkcja lists:min.
- Napisz funkcję find_closest(PeopleLocations, SensorsLocations), która użyje poprzedniej funkcji i zwróci {dystans, {pozycjaOsoby, pozycjaCzujnika}}. Znów warto użyć list comprehension oraz lists:min.
- Zmierz czas wykonania funkcji

Wersja bardzo równoległa:

- Napisz wersję funkcji find_for_person(PersonLocation, SensorsLocations, ParentPID), która po obliczeniu wyniku odsyła go do ParentPID.
- Napisz funkcję find_closest_parallel, która:
  - uruchomi wyszukiwanie w osobnym procesie dla każdej osoby oddzielnie,
  - odbierz komunikaty, zbierz wszystkie wyniki od procesów w liście,
  - wyszuka wynik funkcją lists:min.
- Zarówno tworzenie procesów jak i odbieranie wyników można zrealizować używając list comprehension.
- Porównaj czas obliczeń.

### Serwer zanieczyszczeń

- Zaimplementuj moduł pollution_server, który będzie startował proces obsługujący funkcjonalność modułu pollution. Powinien działać analogicznie do serwera zmiennej globalnej - o bogatszej funkcjonalności.
- Dodatkowe funkcje eksportowane: start/0 i stop/0.
- Dodatkowa funkcja: init/0, która będzie wykonywana już w kontekście nowego procesu.
- Serwer powinien dostarczyć funkcje analogiczne do modułu pollution - ale każda z nich będzie miała o jeden argument mniej. Serwer ma wołać funkcje z modułu pollution.

### Zadanie domowe

- Dokończ moduł pollution_server.
- Zmodyfikuj testy, by działały z modułem pollution_server.
