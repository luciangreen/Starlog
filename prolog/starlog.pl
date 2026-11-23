% prolog/starlog.pl
% Pack entry point so that, after pack_install(starlog), users can write
%   :- use_module(library(starlog)).
% It re-exports the main library in the repository root.

:- module(starlog_pack, []).
:- reexport('../starlog').
