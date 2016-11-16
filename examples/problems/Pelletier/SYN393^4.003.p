%------------------------------------------------------------------------------
% File     : SYN393^4.003 : TPTP v6.4.0. Released v4.0.0.
% Domain   : Logic Calculi (Intuitionistic logic)
% Problem  : Pelletier 12
% Version  : [Goe33] axioms.
% English  :

% Refs     : [Goe33] Goedel (1933), An Interpretation of the Intuitionistic
%          : [Gol06] Goldblatt (2006), Mathematical Modal Logic: A View of
%          : [ROK06] Raths et al. (2006), The ILTP Problem Library for Intu
%          : [Ben09] Benzmueller (2009), Email to Geoff Sutcliffe
%          : [BP10]  Benzmueller & Paulson (2009), Exploring Properties of
% Source   : [Ben09]
% Names    : SYJ206+1.003 [ROK06]

% Status   : CounterSatisfiable
% Rating   : 0.67 v6.2.0, 0.33 v5.4.0, 1.00 v5.0.0, 0.33 v4.1.0, 0.00 v4.0.0
% Syntax   : Number of formulae    :   45 (   0 unit;  23 type;  19 defn)
%            Number of atoms       :  129 (  19 equality;  48 variable)
%            Maximal formula depth :    9 (   5 average)
%            Number of connectives :   72 (   3   ~;   1   |;   2   &;  64   @)
%                                         (   0 <=>;   2  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :   98 (  98   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   26 (  23   :;   0   =)
%            Number of variables   :   40 (   1 sgn;   7   !;   2   ?;  31   ^)
%                                         (  40   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_CSA_EQU_NAR

% Comments : This is an ILTP problem embedded in TH0
%          : In classical logic this is a Theorem.
%          : SYN390^4 is the size 1 instance.
%------------------------------------------------------------------------------
include('Axioms/LCL010^0.ax').
%------------------------------------------------------------------------------
thf(p1_type,type,(
    p1: $i > $o )).

thf(p2_type,type,(
    p2: $i > $o )).

thf(p3_type,type,(
    p3: $i > $o )).

thf(pel12,conjecture,
    ( ivalid @ ( iequiv @ ( iequiv @ ( iequiv @ ( iatom @ p1 ) @ ( iatom @ p2 ) ) @ ( iatom @ p3 ) ) @ ( iequiv @ ( iatom @ p1 ) @ ( iequiv @ ( iatom @ p2 ) @ ( iatom @ p3 ) ) ) ) )).

%------------------------------------------------------------------------------
