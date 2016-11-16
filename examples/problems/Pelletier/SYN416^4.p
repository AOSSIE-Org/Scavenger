%------------------------------------------------------------------------------
% File     : SYN416^4 : TPTP v6.4.0. Released v4.0.0.
% Domain   : Logic Calculi (Intuitionistic logic)
% Problem  : Pelletier Problem 16
% Version  : [Goe33] axioms.
% English  :

% Refs     : [Goe33] Goedel (1933), An Interpretation of the Intuitionistic
%          : [Gol06] Goldblatt (2006), Mathematical Modal Logic: A View of
%          : [ROK06] Raths et al. (2006), The ILTP Problem Library for Intu
%          : [Ben09] Benzmueller (2009), Email to Geoff Sutcliffe
%          : [BP10]  Benzmueller & Paulson (2009), Exploring Properties of
% Source   : [Ben09]
% Names    :

% Status   : CounterSatisfiable
% Rating   : 0.33 v5.4.0, 0.67 v5.0.0, 0.33 v4.1.0, 0.50 v4.0.0
% Syntax   : Number of formulae    :   44 (   0 unit;  22 type;  19 defn)
%            Number of atoms       :  123 (  19 equality;  48 variable)
%            Maximal formula depth :    8 (   5 average)
%            Number of connectives :   66 (   3   ~;   1   |;   2   &;  58   @)
%                                         (   0 <=>;   2  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :   97 (  97   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   25 (  22   :;   0   =)
%            Number of variables   :   40 (   1 sgn;   7   !;   2   ?;  31   ^)
%                                         (  40   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_CSA_EQU_NAR

% Comments : This is an ILTP problem embedded in TH0
%          : In classical logic this is a Theorem.
%------------------------------------------------------------------------------
include('Axioms/LCL010^0.ax').
%------------------------------------------------------------------------------
thf(p_type,type,(
    p: $i > $o )).

thf(q_type,type,(
    q: $i > $o )).

thf(pel16,conjecture,
    ( ivalid @ ( ior @ ( iimplies @ ( iatom @ p ) @ ( iatom @ q ) ) @ ( iimplies @ ( iatom @ q ) @ ( iatom @ p ) ) ) )).

%------------------------------------------------------------------------------