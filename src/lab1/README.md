# Laboratorium nr 1 - podstawy języka Erlang

W laboratorium dostępne będą komputery z potrzebnym oprogramowaniem.
Chcąc pracować na własnym komputerze, proszę zainstalować:

- Erlang OTP 26.x lub 27.x
- IntelliJ z wtyczką Erlang

## Cele zajęć

- zapoznanie z powłoką Erlanga
- poznanie podstaw składni języka
- programowanie modułów i funkcji

## Przebieg zajęć

### Struktury danych

- Uruchom erl w konsoli
- Zaproponuj strukturę, w której będzie się dało przechowywać odczyt ze stacji pomiaru jakości powietrza. Każdy pomiar ma zawierać:
  - nazwę stacji,
  - datę i czas wykonania
  - wartości pomiarów. Różne stacje są wyposażone w różną liczbę czujników równych rodzajów, np PM10, PM2.5, PM1, temperatura, ciśnienie, wilgotność, ...
- Utwórz przykładowe zmienne P1, P2, P3 związane z trzema przykładowymi pomiarami.
- Utwórz zmienną ListaPomiarow związaną z listą pomiarów.
- Utwórz jeszcze jedną zmienną P4 związaną z innym pomiarem
- Wykorzystując tylko zmienne ListaPomiarow oraz P4 utwórz listę NowaListaPomiarow zawierającą wszystkie produkty. Użyj definicji rekurencyjnej listy.
- Napisz wyrażenie, które ze zmienną NazwaP1 zwiąże nazwę stacji w zmiennej P1.

### Moduły i funkcje

- Uruchom IntelliJ, załóż nowy projekt Erlang,
- Utwórz nowy moduł, czyli nowy plik z rozszerzeniem .erl w katalogu src
- W module zaimplementuj funkcję power/2, która podniesie pierwszy argument do potęgi podanej w drugim parametrze.
- Przetestuj jej działanie w konsoli; w tym celu utwórz nową konfiguracje uruchomieniową typu Erlang Console.
- Utwórz nowy moduł o nazwie myLists. Zaimplementuj i przetestuj funkcje:
  - contains/2, która jako parametry weźmie listę i wartość, i zwróci true jeśli lista zawiera wartość.
  - duplicateElements/1, która zwróci listę zawierającą każdy z elementów dwukrotnie - [A, B, ...] zmienia w [A, A, B, B, ...].
  - sumFloats/1, która zsumuje elementy będące liczbami zmiennoprzecinkowymi.
- Zmodyfikuj funkcję sumFloats/1 by korzystała z rekurencji ogonowej.

### Kalkulator zanieczyszczenia

- Na potrzeby testowania zdefiniuj funkcję zwracającą własne, przykładowe dane, zgodne z zaproponowaną wcześniej strukturą. Dane powinny dotyczyć kilku dni, kilku stacji i kilku rodzajów pomiarów.
- Zdefiniuj moduł pozwalający na przetwarzanie danych o jakości powietrza na podstawie listy pomiarów. Moduł dostarczać ma funkcje:

```number_of_readings(Readings, Date) -> int
 calculate_max(Readings, Type) -> float
 calculate_mean(Readings, Type) -> float
```

- Funkcje mają być zabezpieczone przed podaniem nieistniejącego typu pomiaru.

### Zadanie domowe

- Dokończ zadania z zajęć.
- Moduł kalkulatora zanieczyszczeń przynieś na kolejne zajęcia.
