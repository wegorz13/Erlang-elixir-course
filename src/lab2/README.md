# Laboratorium nr 2 - programowanie funkcyjne w Erlangu

## Przed zajęciami

Zapoznaj się z:

- Funkcjami wyższego rzędu.
  - składnia fun ... end
  - Funkcja jako argument innej funkcji.
  - lists:map/2, lists:filter/2, lists:foldl/3
- List comprehensions.
- Rekordami i mapami.

## Cele zajęć

Na dzisiejszych zajęciach poznamy funkcyjne sposoby rozwiązywania problemów w Erlangu. Wykorzystamy do tego list comprehensions jak i funkcje wyższego rzędu.

Część problemów do których rozwiązania używamy pętli **for** czy **while** można również wyrazić za pomocą powyższych narzędzi.

Zapoznamy się również z rekordami i zbudujemy moduł o zadanej funkcjonalności.

### QuickSort

Tą część wykonujemy w IntelliJ

Celem tego ćwiczenia będzie zaimplementowanie algorytmu quicksort. W module **qsort** utwórz następujące funkcje:

- Funkcja **less_than/2**, która dla listy i zadanego argumentu wybierze te elementy które są mniejsze od argumentu. Wykorzystaj list comprehensions.<code erlang> less_than(List, Arg) -> ... </code>
- Funkcja **grt_eq_than/2**, która dla listy i zadanego argumentu wybierze te elementy które są większe bądź równe od argumentu. Tutaj też wykorzystaj list comprehensions.<code erlang> grt_eq_than(List, Arg) -> ... </code>
- Funkcja **qs/1** implementująca algorytm quicksort:<code erlang>qs([Pivot|Tail]) -> qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) ) </code>

Mając zaimplementowanego quicksorta, dobrze byłoby sprawdzić jego działanie. W tym celu zaimplementuj funkcje pomocnicze ułatwiające testowanie.

- Funkcja **random_elems/3**, która zwróci listę losowych elementów z zakresu [Min,Max] o rozmiarze N.Wykorzystaj list comprehensions oraz **rand:uniform/1** i **lists:seq/2**.<code erlang>random_elems(N,Min,Max)-> ... </code>
- Funkcja **compare_speeds/3** która porówna prędkości działania podanych algorytmów sortujących dla zadanej listy. Dwa ostatnie parametry to funkcje. Wykorzystaj do tego funkcję **timer:tc**<code erlang>compare_speeds(List, Fun1, Fun2) -> ... </code> Interesujące nas dane wypisz na standardowe wyjście, formatując je funkcją **io:format/2**.

Następnie w Eshellu przetestuj funkcję **compare_speeds/3**, używając **qsort:qs1/** jak i **lists:sort/1**.

### Fun

Wszystkie zadania z tego punktu można wykonać w Eshellu.

- Zdefiniuj funkcję anonimową, która w ciągu znaków podmieni wszystkie "o" na "a", "e" na "o", pozostałe litery pozostawi bez zmian. Wykorzystaj w niej lists:map(fun,List).
- Zdefiniuj funkcję anonimową, która policzy ile liczb w zadanej liście jest podzielnych przez 3. Da się to zrobić używając lists:filter(pred,List) lub lists:foldl(fun/2,Init,List)
- Zrealizuj funkcjonalność liczenia średniej pomiarów (z poprzednich zajęć) z użyciem funów. Kolejne kroki to np:
  - Użycie funkcji lists:map(fun, Data) do wydobycia samych list odczytów z danych. Po tej operacji dane będą miały postać<code>[ [ {"PM10",53.4}, {"PM1", 22.3} ], [{"PM10",33.3}, {"PM1", 12.9} , {"PM25", 112.9} ], ... ]</code>
  - Listę list trzeba "spłaszczyć"; można to zrobić np używając lists:foldl(fun/2, [], Data). Po tej operacji dane będą miały postać <code>[ {"PM10",53.4}, {"PM1", 22.3}, {"PM10",33.3}, {"PM1", 12.9} , {"PM25", 112.9}, ... ]</code>
  - Dane trzeba odfiltrować, wybierając jedynie wpisy zawierające określony rodzaj zanieczyszczeń. Można to zrobić przy pomocy lists:filter, ale lepiej zadziała list comprehension. Po tej operacji dane będą miały postać <code>[ 53.4, 33.3, ... ]</code>
  - Na koniec można użyć lists:sum(List) i length(list).

### Pollution

Utwórz nowy moduł o nazwie pollution, który będzie zbierał i przetwarzał dane ze stacji mierzących jakość powietrza. Moduł powinien przechowywać:

- informacje o stacjach pomiarowych,
  - współrzędne geograficzne,
  - nazwy stacji pomiarowych,
- zmierzone wartości pomiarów, np stężenia pyłów PM10, PM2.5 czy wartości temperatury (wraz z datą i godziną pomiaru).

Nie powinno być możliwe:

- dodanie dwóch stacji pomiarowych o tej samej nazwie lub tych samych współrzędnych;
- dodanie dwóch pomiarów o tych samych:
  - współrzędnych,
  - dacie i godzinie,
  - typie (PM10, PM2.5, temperatura, ...);
- dodanie pomiaru do nieistniejącej stacji.

Zaprojektuj strukturę danych dla przechowywania takich informacji (jest kilka dobrych rozwiązań tego problemu).

Zaimplementuj funkcje w module //pollution//:

- create_monitor/0 - tworzy i zwraca nowy monitor zanieczyszczeń;
- add_station/3 - dodaje do monitora wpis o nowej stacji pomiarowej (nazwa i współrzędne geograficzne), zwraca zaktualizowany monitor;
- add_value/5 - dodaje odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru, wartość), zwraca zaktualizowany monitor;
- remove_value/4 - usuwa odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru), zwraca zaktualizowany monitor;
- get_one_value/4 - zwraca wartość pomiaru z zadanej stacji o zadanym typie i z zadanej daty;
- get_station_min/3 - zwraca minimalną wartość parametru z zadanej stacji i danego typu;
- get_daily_mean/3 - zwraca średnią wartość parametru danego typu, danego dnia na wszystkich stacjach;

Wskazówki:

- do przechowywania dat użyj struktur z modułu calendar (zob. calendar:local_time(). ),
- współrzędne geograficzne to np para (krotka) liczb,
- nazwy i typy reprezentuj ciągami znaków,
- używaj list comprehension, operacji na listach i funów bez ubolewania nad potencjalną stratą wydajności,
- w przypadku błędu wykonania funkcji zwracaj: {error, DaneBledu_NpOpis}.

Przetestuj działanie modułu.
_ P = pollution:create_monitor().
_ P1 = pollution:add_station("Aleja Słowackiego", {50.2345, 18.3445}, P).
_ P2 = pollution:add_value({50.2345, 18.3445}, calendar:local_time(), "PM10", 59, P1).
_ P3 = pollution:add_value("Aleja Słowackiego", calendar:local_time(), "PM2,5", 113, P2). \* ...

**Zabezpiecz moduł pollution - będzie on potrzebny na następnych zajęciach.**

### Zadanie domowe

- Dokończ moduł pollution.
- Uruchom testy. Warto rozważyć pisanie modułu w podejściu TDD.
