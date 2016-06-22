create database demo;

use demo;

create table productos (nombre VARCHAR (20));

describe productos;

insert into productos (nombre) values ('Impresora');

insert into productos (nombre) values ('Portatil');

insert into productos (nombre) values ('Pantalla');

select * from productos
