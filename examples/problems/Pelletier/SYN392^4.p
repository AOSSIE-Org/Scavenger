%------------------------------------------------------------------------------
% File     : SYN392^4 : TPTP v6.4.0. Released v4.0.0.
% Domain   : Logic Calculi (Intuitionistic logic)
% Problem  : Pelletier 14
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
% Rating   : 0.67 v6.2.0, 0.33 v5.4.0, 1.00 v5.0.0, 0.33 v4.1.0, 0.00 v4.0.0
% Syntax   : Number of formulae    :   44 (   0 unit;  22 type;  19 defn)
%            Number of atoms       :  131 (  19 equality;  48 variable)
%            Maximal formula depth :    8 (   5 average)
%            Number of connectives :   74 (   3   ~;   1   |;   2   &;  66   @)
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
thf(p1_type,type,(
    p1: $i > $o )).

thf(p2_type,type,(
    p2: $i > $o )).

thf(pel14,conjecture,
    ( ivalid @ ( iequiv @ ( iequiv @ ( iatom @ p1 ) @ ( iatom @ p2 ) ) @ ( iand @ ( ior @ ( iatom @ p2 ) @ ( inot @ ( iatom @ p1 ) ) ) @ ( ior @ ( inot @ ( iatom @ p2 ) ) @ ( iatom @ p1 ) ) ) ) )).

%------------------------------------------------------------------------------
