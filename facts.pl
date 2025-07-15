:- discontiguous grid_api/1.
:- discontiguous status_api/1.
:- discontiguous cors_enable_all/1.
:- dynamic cell/5.
shape(0 , unknown).
shape(1 , curve).
shape(2 , square).
shape(3 , circle).
shape(4 , sea).

direction(0 , dontCare).
direction(1 , up).
direction(2 , down).
direction(3 , left).
direction(4 , right).

type(1 , sea).
type(2 , ship).
type(3 , empty).

grid(4).

row(1, 2).
row(2, 2).
row(3, 0).
row(4, 0).

col(1, 2).
col(2, 0).
col(3, 2).
col(4, 0).

% ship(Length, Count)
ship(2, 2).

% cell(row , col , type , shape , direction)
cell(1, 1, 2, 2, 0).
cell(1, 2, 1, 1, 4).
cell(1, 3, 2, 4, 0).
cell(1, 4, 1, 4, 0).

cell(2, 1, 2, 2, 0).
cell(2, 2, 1, 1, 3).
cell(2, 3, 2, 4, 0).
cell(2, 4, 1, 4, 0).

cell(3, 1, 1, 4, 0).
cell(3, 2, 1, 4, 0).
cell(3, 3, 1, 4, 0).
cell(3, 4, 1, 4, 0).

cell(4, 1, 1, 4, 0).
cell(4, 2, 1, 4, 0).
cell(4, 3, 1, 4, 0).
cell(4, 4, 1, 4, 0).