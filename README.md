# Práctica 2 — Programación Lógica (Prolog)

## 📘 Curso

Lenguajes y Paradigmas de Programación
Ingeniería de Sistemas — Universidad EAFIT

*Integrante:*

* Laura Santamaría
* Simón Díaz

---

## 📁 Archivos del repositorio

| Archivo         | Descripción                                                                      |
| --------------- | -------------------------------------------------------------------------------- |
| challenge1.pl | Base de datos de plataformas de cómputo y consultas en Prolog.                   |
| challenge2.pl | Sistema de planificación de rutas con cálculo de tiempo, costo y disponibilidad. |
| README.md     | Explicación general, instrucciones y ejemplos de ejecución.                      |

---

## 🧠 Objetivos

1. Aplicar los conceptos básicos de programación lógica.
2. Desarrollar aplicaciones simples en Prolog.
3. Presentar el desarrollo de forma clara y funcional.

---

## ⚙️ Cómo ejecutar el código

### Requisitos

* Tener instalado *SWI-Prolog* ([descargar aquí](https://www.swi-prolog.org/Download.html)).

### Pasos

1. Abrir *Visual Studio Code* o la consola.
2. Navegar a la carpeta donde están los archivos (challenge1.pl y challenge2.pl).
3. Cargar el archivo en Prolog con:

   
   ?- [challenge1].
   ?- [challenge2].
   

---

## 🧩 Challenge 1 — Base de datos de plataformas

### Descripción

Este módulo define una base de conocimiento con más de 50 registros de equipos (platform/10).
Cada plataforma tiene: marca, ID, año, RAM, CPU, núcleos, disco, tipo, GPU y VRAM.
Se incluyen las 5 consultas obligatorias y 4 adicionales más complejas.

### Consultas principales

```prolog 

% 1. AMD después de 2021
?- amd_after_2021(T, ID, Year, RAM, Cores).

% 2. Tablets con más de 2GB RAM
?- tablet_more_than_2gb(T, ID, RAM).

% 3. Discos entre 32GB y 512GB
?- disk_between_32_512(T, ID, Disk).

% 4. Número de plataformas ASUS
?- count_asus(C).

% 5. Laptops con >4GB RAM y <512GB disco
?- count_laptops_ram_disk(C).
```

### Consultas adicionales

```prolog
% Equipos potentes antes de 2022
?- high_cores_and_vram_before_2022(ID, Cores, VRAM, Year).

% Contar por tipo (pc, laptop, tablet...)
?- count_by_type(laptop, C).

% Top 5 por potencia (cores*1000 + VRAM)
?- top_n_by_power(5, R).

% Equipos entre años con RAM mínima
?- platforms_acquired_between_with_min_ram(2019, 2022, 8192, L).
```

---

## ✈️ Challenge 2 — Sistema de rutas

### Descripción

Base de conocimiento con hechos ruta/7:

```prolog
ruta(Origen, Destino, Medio, HoraSalida, HoraLlegada, CostoUSD, Disponible).


Permite encontrar rutas directas o con escalas, calcular tiempos, costos y aplicar filtros.
```

### Consultas útiles

```prolog
% Rutas directas Bogotá–Medellín
?- direct_routes(bogota, medellin, R).

% Todas las rutas posibles Bogotá–Cartagena
?- find_all_routes(bogota, cartagena, R), forall(member(X, R), print_route_info(X)).

% Ruta más rápida
?- fastest_route(bogota, cartagena, Best), print_route_info(Best).

% Ruta más barata
?- cheapest_route(bogota, cartagena, Best), print_route_info(Best).

% Rutas disponibles
?- find_all_routes(medellin, cartagena, R), filter_available_routes(R, Av).

% Filtrar por hora de salida (6–12)
?- find_all_routes(bogota, medellin, R), filter_routes_by_departure_range(R, 6, 12, Out).

% Rutas alternativas
?- suggest_alternatives(medellin, cartagena, Alt), forall(member(A, Alt), print_route_info(A)).
```

documento de google: https://docs.google.com/document/d/1YEuLQkbAYeGu6i_sN8VJEFQKLxUlQsNGRQN81PtbSf4/edit?usp=sharing

---
### Video explicativo

Link a youtube: https://www.youtube.com/@Santiespin108

punto 4:
https://www.loom.com/share/c8c4f05cbcae44f28a0a45d1b34f75cc?sid=9271b7cb-9742-4956-bbb2-64e0ae46b493
https://www.loom.com/share/954bc975852d44558d41e1772064f9fc?sid=1853f23c-ef82-4bb1-bc67-0cc4ce0c93c1

---
