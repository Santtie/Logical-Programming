% -----------------------------------------------------
% TRAVEL PLANNING SYSTEM
% -----------------------------------------------------
% ruta(Origen, Destino, Medio, HoraSalida, HoraLlegada, CostoUSD, Disponible).
% -----------------------------------------------------

% --- Knowledge base (routes) ---
ruta(bogota, medellin, avion, 8.0, 9.0, 100, si).
ruta(bogota, medellin, bus, 6.0, 16.0, 40, si).
ruta(medellin, cartagena, bus, 6.0, 22.0, 80, si).
ruta(cartagena, santa_marta, bus, 14.0, 18.0, 30, si).
ruta(bogota, cartagena, avion, 9.0, 10.5, 130, si).
ruta(bogota, cali, avion, 10.0, 11.0, 90, si).
ruta(bogota, cali, bus, 5.0, 12.0, 35, si).
ruta(cali, medellin, bus, 8.0, 12.0, 25, si).
ruta(medellin, pereira, bus, 9.0, 12.0, 22, si).
ruta(pereira, manizales, bus, 13.0, 15.0, 12, si).
ruta(manizales, bogota, bus, 10.0, 14.0, 30, si).
ruta(bogota, bucaramanga, bus, 6.0, 12.0, 28, si).
ruta(bucaramanga, cartagena, avion, 10.0, 11.5, 120, si).
ruta(cartagena, barranquilla, bus, 8.0, 10.0, 12, si).
ruta(barranquilla, santa_marta, bus, 11.0, 13.0, 10, si).
ruta(cali, pasto, bus, 14.0, 22.0, 45, si).
ruta(pasto, ipiales, bus, 6.0, 9.0, 20, si).
ruta(quibdo, apartado, bus, 9.0, 11.0, 25, si).
ruta(apartado, monteria, bus, 12.0, 15.0, 30, si).
ruta(monteria, cartagena, bus, 16.0, 19.0, 25, si).

% -----------------------------------------------------
% BASIC HELPERS
% -----------------------------------------------------

% Get a direct route
direct_route(O, D, M, S, L, C, Disp) :-
    ruta(O, D, M, S, L, C, Disp).

% Get all direct routes between O and D
direct_routes(O, D, Routes) :-
    findall(route(O, D, M, S, L, C, Disp),
            direct_route(O, D, M, S, L, C, Disp),
            Routes).

% Calculate travel duration
route_duration(S, L, T) :-
    T is L - S.

% -----------------------------------------------------
% FIND ROUTES WITH STOPOVERS
% -----------------------------------------------------

% Base case: direct connection (O -> D)
route_path(O, D, _, [route(O, D, M, S, L, C, Disp)], T, C, Disp) :-
    ruta(O, D, M, S, L, C, Disp),
    route_duration(S, L, T).

% Recursive case: O -> X -> ... -> D
route_path(O, D, Visited, [route(O, X, M, S, L, C, Disp)|Rest], TT, TC, Av) :-
    ruta(O, X, M, S, L, C, Disp),
    \+ member(X, Visited),  % avoids cycles (don't revisit cities)
    route_path(X, D, [X|Visited], Rest, T2, C2, Av2),
    TT is (L - S) + T2,     % total travel time = this leg + next legs
    TC is C + C2,           % total cost = this leg + next legs
    (Disp = si, Av2 = si -> Av = si ; Av = no).  % available only if all are 'si'

% -----------------------------------------------------
% FIND ALL ROUTES BETWEEN TWO CITIES
% -----------------------------------------------------

find_all_routes(O, D, Routes) :-
    setof([Path, T, C, Disp],
          route_path(O, D, [O], Path, T, C, Disp),
          R),               % R is a list of lists
    convert_routes(R, Routes), !.
find_all_routes(_, _, []).  % if no routes found

% Convert from internal list format to readable route_info structure
convert_routes([], []).
convert_routes([[P, T, C, D]|Rest], [route_info(P, T, C, D)|Rt]) :-
    convert_routes(Rest, Rt).

% -----------------------------------------------------
% FILTERS AND CALCULATIONS
% -----------------------------------------------------

% Fastest route (shortest total time)
fastest_route(O, D, Best) :-
    find_all_routes(O, D, Routes),
    Routes \= [],
    sort(2, @=<, Routes, [Best|_]).

% Cheapest route (lowest total cost)
cheapest_route(O, D, Best) :-
    find_all_routes(O, D, Routes),
    Routes \= [],
    sort(3, @=<, Routes, [Best|_]).

% Filter available routes
filter_available_routes(Routes, Available) :-
    include(is_available, Routes, Available).
is_available(route_info(_, _, _, si)).

% Filter by departure time range
filter_routes_by_departure_range(Routes, Min, Max, Out) :-
    include(route_departure_in_range(Min, Max), Routes, Out).

route_departure_in_range(Min, Max, route_info([route(_, _, _, S, _, _, _)|_], _, _, _)) :-
    S >= Min, S =< Max.

% -----------------------------------------------------
% SUGGEST ALTERNATIVE ROUTES
% -----------------------------------------------------

suggest_alternatives(O, D, Alternatives) :-
    find_all_routes(O, D, Routes),
    filter_available_routes(Routes, Av),
    (Av \= [] -> Alternatives = Av ; Alternatives = Routes).

% -----------------------------------------------------
% PRINT RESULTS
% -----------------------------------------------------

print_route_info(route_info(Path, T, C, Disp)) :-
    format('Ruta encontrada:~n', []),
    print_path(Path),
    format('Tiempo total: ~1f h, Costo total: $~w, Disponible: ~w~n', [T, C, Disp]).

print_path([]).
print_path([route(O, D, M, S, L, C, Disp)|T]) :-
    format('  ~w -> ~w por ~w (Salida: ~w, Llegada: ~w, Costo: $~w, Disponible: ~w)~n',
           [O, D, M, S, L, C, Disp]),
    print_path(T).
