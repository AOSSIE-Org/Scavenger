%------------------------------------------------------------------------------
% File     : SYN391^4 : TPTP v6.4.0. Released v4.0.0.
% Domain   : Logic Calculi (Intuitionistic logic)
% Problem  : Pelletier 9
% Version  : [Goe33] axioms.
% English  :

% Refs     : [Goe33] Goedel (1933), An Interpretation of the Intuitionistic
%          : [Gol06] Goldblatt (2006), Mathematical Modal Logic: A View of
%          : [ROK06] Raths et al. (2006), The ILTP Problem Library for Intu
%          : [Ben09] Benzmueller (2009), Email to Geoff Sutcliffe
%          : [BP10]  Benzmueller & Paulson (2009), Exploring Properties of
% Source   : [Ben09]
% Names    :

% Status   : Theorem
% Rating   : 0.29 v6.4.0, 0.33 v6.3.0, 0.40 v6.2.0, 0.57 v6.1.0, 0.43 v5.5.0, 0.50 v5.4.0, 0.40 v5.3.0, 0.60 v5.0.0, 0.40 v4.1.0, 0.33 v4.0.1, 0.67 v4.0.0
% Syntax   : Number of formulae    :   44 (   0 unit;  22 type;  19 defn)
%            Number of atoms       :  140 (  19 equality;  48 variable)
%            Maximal formula depth :   11 (   5 average)
%            Number of connectives :   83 (   3   ~;   1   |;   2   &;  75   @)
%                                         (   0 <=>;   2  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :   97 (  97   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   25 (  22   :;   0   =)
%            Number of variables   :   40 (   1 sgn;   7   !;   2   ?;  31   ^)
%                                         (  40   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_THM_EQU_NAR

% Comments : This is an ILTP problem embedded in TH0
%------------------------------------------------------------------------------
include('Axioms/LCL010^0.ax').
%------------------------------------------------------------------------------
thf(p1_type,type,(
    p1: $i > $o )).

thf(p2_type,type,(
    p2: $i > $o )).

thf(pel9,conjecture,
    ( ivalid @ ( iimplies @ ( iand @ ( ior @ ( iatom @ p1 ) @ ( iatom @ p2 ) ) @ ( iand @ ( ior @ ( inot @ ( iatom @ p1 ) ) @ ( iatom @ p2 ) ) @ ( ior @ ( iatom @ p1 ) @ ( inot @ ( iatom @ p2 ) ) ) ) ) @ ( inot @ ( ior @ ( inot @ ( iatom @ p1 ) ) @ ( inot @ ( iatom @ p2 ) ) ) ) ) )).

%------------------------------------------------------------------------------
