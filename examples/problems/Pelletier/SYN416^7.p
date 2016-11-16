%------------------------------------------------------------------------------
% File     : SYN416^7 : TPTP v6.4.0. Released v5.5.0.
% Domain   : Syntactic
% Problem  : Pelletier Problem 16
% Version  : [Ben12] axioms.
% English  :

% Refs     : [Goe69] Goedel (1969), An Interpretation of the Intuitionistic
%          : [Pel86] Pelletier (1986), Seventy-five Problems for Testing Au
%          : [Ben12] Benzmueller (2012), Email to Geoff Sutcliffe
% Source   : [Ben12]
% Names    : s4-cumul-GSY416+1 [Ben12]

% Status   : CounterSatisfiable
% Rating   : 0.33 v5.5.0
% Syntax   : Number of formulae    :   75 (   0 unit;  38 type;  32 defn)
%            Number of atoms       :  263 (  36 equality; 143 variable)
%            Maximal formula depth :   11 (   6 average)
%            Number of connectives :  159 (   5   ~;   5   |;   9   &; 130   @)
%                                         (   0 <=>;  10  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :  182 ( 182   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   42 (  38   :;   0   =)
%            Number of variables   :   90 (   2 sgn;  34   !;   7   ?;  49   ^)
%                                         (  90   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_CSA_EQU_NAR

% Comments : Goedel translation of SYN416+1
%------------------------------------------------------------------------------
%----Include axioms for Modal logic S4 under cumulative domains
include('Axioms/LCL015^0.ax').
include('Axioms/LCL013^5.ax').
include('Axioms/LCL015^1.ax').
%------------------------------------------------------------------------------
thf(q_type,type,(
    q: $i > $o )).

thf(p_type,type,(
    p: $i > $o )).

thf(pel16,conjecture,
    ( mvalid @ ( mor @ ( mbox_s4 @ ( mimplies @ ( mbox_s4 @ p ) @ ( mbox_s4 @ q ) ) ) @ ( mbox_s4 @ ( mimplies @ ( mbox_s4 @ q ) @ ( mbox_s4 @ p ) ) ) ) )).

%------------------------------------------------------------------------------
