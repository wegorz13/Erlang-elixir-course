# Laboratorium nr 4 - wzorce projektowe i OTP

## Cele zajęć

- Zapoznanie z klastrami węzłów BEAM.
- Poznanie narzędzia rebar3.
- Zapoznanie z wzorcami projektowymi OTP: gen_server, supervisor, gen_statem, application

## Przebieg zajęć

### Distributed Erlang

W tej części trzeba będzie skorzystać z maszyn w laboratorium.

- uruchom węzeł erlanga, spróbuj połączyć się z węzłem uruchomionym przez prowadzącego
  - przyda się funkcja net_adm:ping(NodeName) oraz BIF nodes().
- napisz... szczegóły na laboratorium...

### rebar

- Przy pomocy rebar3 utwórz nowy projekt typu app w wybranym folderze.
- Przeanalizuj strukturę generowane przez rebar.
- Otwórz projekt w IntelliJ metodą New -> Project From Existing Sources...
  - wskaż folder z wygenerowanymi źródłami,
  - wybierz opcję Create project from existring sources,
  - nie zmieniaj jego lokalizacji.
- Skonfiguruj środowisko do współpracy z rebarem.
  - w Settings -> Other Settings -> Erlang External Tools skonfiguruj rebar3
  - w Settings -> Build... -> Compiler -> Erlang Compiler włącz budowanie rebarem
- Dodaj i uruchom konfigurację uruchomieniową typu Rebar startującą polecenie shell.
  - inne polecenia rebara: https://www.rebar3.org/docs/commands
- Uruchom observer:start(). i sprawdź czy Twoja aplikacja jest uruchomiona.
- W konsoli możesz wywołać application:stop(nazwaAplikacji).

### gen_server

- Dodaj do źródeł projektu moduł pollution_gen_server. Wykorzystaj szkielet OTP gen_server minimal.
- Zmodyfikuj pollution_gen_server by udostępniał API zgodne z modułem pollution z poprzednich zajęć:
  - **addStation/2**,
  - **addValue/4**,
  - **getOneValue/3**
  - ... pozostałe funkcje modułu pollution, ale wcześniej:
- Dodaj do interfejsu serwera funkcję **crash/0**, która spowoduje wykonanie niepoprawnej operacji w procesie serwera.

### supervisor

- Dodaj pollution_gen_server do listy serwerów startowanych i nadzorowanych przez domyślnego supervisora.
- Sprawdź teraz działanie funkcji **crash/0**.
- Co dzieje się z danymi w przypadku awarii serwera? Jak można naprawić ten problem?

### gen_statem

- Dodaj moduł pollution_value_collector_gen_statem, który będzie pozwalał na dodawanie wielu danych do ustalonej stacji poprzez kolejne wywołania funkcji.
- Moduł realizuje wzorzec OTP gen_statem; API modułu:
  - **start_link(), stop(),**
  - **set_station(...)**
  - **add_value(...)**
  - **store_data()**
- Trzy powyższe funkcje muszą być wykonane w kolejności, przy czym **add_value** może być wykonana wiele razy.
- Dopiero po wykonaniu **store_data** wszystkie pomiary są dodawane do serwera zanieczyszczeń, a pollution_value_collector wraca do stanu oczekiwania na wybór stacji.

### Zadanie domowe

- Dokończ budowanie kompletnej aplikacji.
- Sprawdź czy poprawnie działa i poprawnie obsługuje błędy.
