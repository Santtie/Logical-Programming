% Challenge 1: Database - Platforms
% Universidad EAFIT - Ingeniería de Sistemas
% Lenguajes y Paradigmas de Programación - Práctica 2 (Prolog)

% -----------------------------------------
% BASE DE DATOS (min 50 plataformas)
% -----------------------------------------

platform(asus, 'ASU-001', 2019, 4096, intel, 4, 500, laptop, nvidia, 2048).
platform(asus, 'ASU-002', 2022, 8192, amd, 6, 1000, pc, amd_gpu, 4096).
platform(lenovo, 'LEN-101', 2020, 2048, amd, 2, 128, tablet, intel_gpu, 512).
platform(lenovo, 'LEN-102', 2023, 16384, intel, 8, 2000, laptop, nvidia, 8192).
platform(dell, 'DEL-201', 2021, 8192, intel, 6, 512, pc, nvidia, 2048).
platform(acer, 'ACE-301', 2018, 1024, intel, 2, 64, tablet, intel_gpu, 256).
platform(hp, 'HP-401', 2024, 16384, amd, 8, 1000, pc, amd_gpu, 4096).
platform(asus, 'ASU-003', 2020, 4096, amd, 4, 256, laptop, nvidia, 2048).
platform(msi, 'MSI-501', 2022, 32768, intel, 12, 2000, pc, nvidia, 12288).
platform(apple, 'APP-601', 2021, 8192, apple, 8, 512, laptop, apple_gpu, 4096).
platform(apple, 'APP-602', 2024, 16384, apple, 10, 1000, pc, apple_gpu, 8192).
platform(samsung, 'SAM-701', 2022, 3072, qualcomm, 8, 128, tablet, mali, 1024).
platform(acer, 'ACE-302', 2020, 4096, intel, 4, 512, laptop, nvidia, 2048).
platform(dell, 'DEL-202', 2023, 6144, amd, 6, 256, pc, amd_gpu, 1024).
platform(hp, 'HP-402', 2017, 2048, intel, 2, 500, laptop, intel_gpu, 512).
platform(asus, 'ASU-004', 2016, 1024, intel, 2, 64, tablet, intel_gpu, 256).
platform(toshiba, 'TOS-901', 2015, 2048, intel, 2, 250, laptop, intel_gpu, 256).
platform(sony, 'SON-1001', 2021, 8192, intel, 6, 512, pc, amd_gpu, 2048).
platform(hp, 'HP-403', 2020, 4096, amd, 4, 128, laptop, amd_gpu, 1024).
platform(lenovo, 'LEN-103', 2018, 8192, intel, 4, 1000, pc, nvidia, 2048).
platform(msi, 'MSI-502', 2024, 65536, intel, 16, 3000, pc, nvidia, 16384).
platform(asus, 'ASU-005', 2022, 12288, intel, 8, 1000, laptop, nvidia, 6144).
platform(acer, 'ACE-304', 2023, 8192, amd, 6, 512, laptop, amd_gpu, 4096).
platform(razer, 'RAZ-1301', 2021, 16384, intel, 8, 2000, laptop, nvidia, 8192).
platform(hp, 'HP-404', 2022, 4096, amd, 4, 256, pc, amd_gpu, 2048).
platform(sony, 'SON-1002', 2023, 16384, intel, 8, 1000, pc, nvidia, 8192).
platform(asus, 'ASU-006', 2024, 32768, amd, 12, 2000, pc, amd_gpu, 8192).
platform(acer, 'ACE-305', 2019, 8192, amd, 6, 512, pc, amd_gpu, 4096).
platform(msi, 'MSI-503', 2016, 4096, intel, 4, 500, laptop, nvidia, 1024).
platform(gigabyte, 'GIG-1601', 2022, 16384, amd, 8, 1000, pc, amd_gpu, 6144).
platform(asus, 'ASU-007', 2013, 1024, intel, 1, 32, tablet, intel_gpu, 128).
platform(chuwi, 'CHW-1701', 2021, 4096, intel, 4, 128, tablet, intel_gpu, 512).
platform(hp, 'HP-405', 2018, 8192, amd, 4, 500, pc, amd_gpu, 2048).
platform(samsung, 'SAM-702', 2019, 6144, samsung_cpu, 8, 256, laptop, amd_gpu, 2048).
platform(asus, 'ASU-008', 2020, 3072, amd, 4, 128, tablet, amd_gpu, 512).
platform(mac, 'MAC-1102', 2020, 16384, apple, 8, 1000, laptop, apple_gpu, 8192).
platform(acer, 'ACE-307', 2024, 32768, intel, 12, 2000, pc, nvidia, 12288).
platform(msi, 'MSI-504', 2019, 8192, amd, 6, 512, laptop, amd_gpu, 4096).
platform(razer, 'RAZ-1302', 2015, 4096, intel, 4, 256, laptop, nvidia, 2048).
platform(benq, 'BEN-1901', 2018, 4096, intel, 4, 500, pc, intel_gpu, 1024).
platform(asus, 'ASU-009', 2021, 6144, amd, 6, 256, laptop, amd_gpu, 2048).

% -----------------------------------------
% CONSULTAS OBLIGATORIAS
% -----------------------------------------

% 1. Plataformas con CPU AMD adquiridas después de 2021
amd_after_2021(T, ID, Y, RAM, Cores) :-
platform(T, ID, Y, RAM, amd, Cores, _, _, _),
Y > 2021.

% 2. Tablets con más de 2GB de RAM
tablet_more_than_2gb(T, ID, RAM) :-
platform(T, ID, _, RAM, _, _, _, tablet, _, _),
RAM > 2048.

% 3. Discos entre 32GB y 512GB
disk_between_32_512(T, ID, Disk) :-
platform(T, ID, _, _, _, _, Disk, _, _, _),
Disk >= 32, Disk =< 512.

% 4. Cuántas plataformas son ASUS
count_asus(C) :-
findall(ID, platform(asus, ID, _, _, _, _, _, _, _, _), L),
length(L, C).

% 5. Cuántas laptops tienen >4GB RAM y <512GB disco
count_laptops_ram_disk(C) :-
findall(ID,
(platform(_, ID, _, RAM, _, _, Disk, laptop, _, _),
RAM > 4096, Disk < 512),
L),
length(L, C).

% -----------------------------------------
% CONSULTAS ADICIONALES (4 COMPLEJAS)
% -----------------------------------------

% 1. Equipos con >=6 núcleos y >=2048MB VRAM antes de 2022
high_cores_and_vram_before_2022(ID, Cores, VRAM, Year) :-
platform(_, ID, Year, _, _, Cores, _, _, _, VRAM),
Year < 2022, Cores >= 6, VRAM >= 2048.

% 2. Contar plataformas por tipo (pc, laptop, tablet, etc.)
count_by_type(Type, Count) :-
findall(ID, platform(_, ID, _, _, _, _, _, Type, _, _), L),
length(L, Count).

% 3. Top N por potencia (score = Cores*1000 + VRAM)
top_n_by_power(N, Result) :-
findall(Score-ID-T,
(platform(T, ID, _, _, _, Cores, _, _, _, VRAM),
Score is Cores*1000 + VRAM),
Pairs),
sort(0, @>=, Pairs, Sorted),
take_n(Sorted, N, Top),
maplist(writeln, Top),
Result = Top.

take_n(_, 0, []) :- !.
take_n([], _, []) :- !.
take_n([H|T], N, [H|R]) :- N1 is N-1, take_n(T, N1, R).

% 4. Plataformas adquiridas entre dos años con RAM mínima
platforms_acquired_between_with_min_ram(From, To, MinRAM, List) :-
findall(ID,
(platform(_, ID, Y, RAM, _, _, _, _, _, _),
Y >= From, Y =< To, RAM >= MinRAM),
List).