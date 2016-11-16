%------------------------------------------------------------------------------
% File     : SYN393^4.004 : TPTP v6.4.0. Released v4.0.0.
% Domain   : Logic Calculi (Intuitionistic logic)
% Problem  : ILTP Problem SYJ206+1.004
% Version  : [Goe33] axioms.
% English  :

% Refs     : [Goe33] Goedel (1933), An Interpretation of the Intuitionistic
%          : [Gol06] Goldblatt (2006), Mathematical Modal Logic: A View of
%          : [ROK06] Raths et al. (2006), The ILTP Problem Library for Intu
%          : [Ben09] Benzmueller (2009), Email to Geoff Sutcliffe
%          : [BP10]  Benzmueller & Paulson (2009), Exploring Properties of
% Source   : [Ben09]
% Names    : SYJ206+1.004 [ROK06]

% Status   : Theorem
% Rating   : 0.43 v6.4.0, 0.50 v6.3.0, 0.40 v6.2.0, 0.43 v5.5.0, 0.50 v5.4.0, 0.60 v5.2.0, 0.80 v4.1.0, 1.00 v4.0.0
% Syntax   : Number of formulae    :   46 (   0 unit;  24 type;  19 defn)
%            Number of atoms       :  135 (  19 equality;  48 variable)
%            Maximal formula depth :   11 (   5 average)
%            Number of connectives :   78 (   3   ~;   1   |;   2   &;  70   @)
%                                         (   0 <=>;   2  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :   99 (  99   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   27 (  24   :;   0   =)
%            Number of variables   :   40 (   1 sgn;   7   !;   2   ?;  31   ^)
%                                         (  40   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_THM_EQU_NAR

% Comments : This is an ILTP problem embedded in TH0
%          : SYN390^4 is the size 1 instance.
%------------------------------------------------------------------------------
include('Axioms/LCL010^0.ax').
%------------------------------------------------------------------------------
thf(a1_type,type,(
    a1: $i > $o )).

thf(a2_type,type,(
    a2: $i > $o )).

thf(a3_type,type,(
    a3: $i > $o )).

thf(a4_type,type,(
    a4: $i > $o )).

thf(con,conjecture,
    ( ivalid @ ( iequiv @ ( iequiv @ ( iequiv @ ( iequiv @ ( iatom @ a1 ) @ ( iatom @ a2 ) ) @ ( iatom @ a3 ) ) @ ( iatom @ a4 ) ) @ ( iequiv @ ( iatom @ a4 ) @ ( iequiv @ ( iatom @ a3 ) @ ( iequiv @ ( iatom @ a2 ) @ ( iatom @ a1 ) ) ) ) ) )).

%------------------------------------------------------------------------------
