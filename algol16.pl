:- use_module(library(dcg/basics)).


lexer(Tokens) -->
   skip,
   (  ( 
         "+",       !, { Token = tokPlus }
      |  ":=",      !, { Token = tokAssgn }
      |  ";",       !, { Token = tokSColon }
      |  "(",       !, { Token = tokLParen }
      |  ")",       !, { Token = tokRParen }
      |  "-",       !, { Token = tokMinus }
      |  "*",       !, { Token = tokTimes }
      |  "=",       !, { Token = tokEq }
      |  "<>",      !, { Token = tokNeq }
      |  "<=",      !, { Token = tokLeq }
      |  "<",       !, { Token = tokLt }
      |  ">=",      !, { Token = tokGeq }
      |  ">",       !, { Token = tokGt }
      |  ",",       !, { Token = tokColon }
      |  digit(D),  !,
            number(D, N),
            { Token = tokNumber(N) }
      |  letter(L), !, identifier(L, Id),
            {  member((Id, Token), [ (and, tokAnd),
                                     (begin, tokBegin),
                                     (call, tokCall),
                                     (div, tokDiv),
                                     (do, tokDo),
                                     (done, tokDone),
                                     (else, tokElse),
                                     (end, tokEnd),
                                     (fi, tokFi),
                                     (if, tokIf),
                                     (local, tokLocal),
                                     (mod, tokMod),
                                     (not, tokNot),
                                     (or, tokOr),
                                     (procedure, tokProcedure),
                                     (program, tokProgram),
                                     (read, tokRead),
                                     (return, tokReturn),
                                     (then, tokThen),
                                     (value, tokValue),
                                     (while, tokWhile),
                                     (write, tokWrite)]),
               !
            |  Token = tokVar(Id)
            }
      |  [_],
            { Token = tokUnknown }
      ),
      !,
         { Tokens = [Token | TokList] },
      lexer(TokList)
   |  [],
         { Tokens = [] }
   ).

skip --> (comment, skip) | (blank, skip), !. 
skip --> [].

comment --> "(*", anything.
anything --> "*)".
anything --> [_], anything.

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, alpha) }.

alphanum([A|T]) -->
   [A], { A == 39 ; code_type(A, csym)  }, !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As),
      { atom_codes(Id, [L|As]) }.



%% PARSER
program(Prog) -->
  [tokProgram, tokVar(N)], block(Blo),
    {Prog =.. [program, N, Blo]}. 

block(Block) -->
  declarations(Dekl), [tokBegin], complex_instruction(Instr), [tokEnd],
    { Block =.. [block, Dekl, Instr] }.

%% deklaracje
declarations(Dec_F) --> %% to cos chyba popsulem wczesniej
(
  declaration(Dec2),!,declarations(Dec1),
    {Dec = [ Dec2 | Dec1] }
  ; [], { Dec = [] }
),{ flatten(Dec,Dec_F)}.

%% deklaracja
declaration(Dec) -->
  declarator(Dec),!
  ; procedura(Dec).

%% deklarator
declarator(Dec) -->
  [tokLocal], variables(Dec).

variables(Vars) --> 
  variabl(V), [tokColon],!, variables(Va),
  { Vars = [var(V)|Va] }
  ; variabl(Var),
  {Vars = [var(Var)] }.

variabl(Var) -->
  [tokVar(Var)].

procedura(Pro) -->
  [tokProcedure, tokVar(Name), tokLParen], formal_arguments(Arg), [tokRParen], block(Blo),
   {Pro =.. [procedure, Name, Arg, Blo]}. 

formal_arguments(Arg) --> {Arg = []}.
formal_arguments(Arg) -->
  formal_arguments_sequence(Arg).

formal_arguments_sequence(Arg) -->
  formal_argument(Arg1), [tokColon],!, formal_arguments_sequence(Arg2),
    {Arg = [Arg1|Arg2]}
  ; formal_argument(Arg1),
    {Arg = [Arg1]}.

formal_argument(Arg) -->
  [tokValue],!, variabl(Arg)
  ; variabl(Arg).

%% instrukcja zlozona
complex_instruction(Instr) -->
  instruction(In), [tokSColon],!, complex_instruction(In2),
    {Instr = [In | In2] } %% tu cos nie teges chyba
  ; instruction(Ins),
  {Instr = [Ins] }.

instruction(Instr) -->
  [tokVar(N)],!, [tokAssgn], arith_expr(Ar),
    {Instr =.. [asgn, var(N), Ar] }
  ; [tokReturn],!, arith_expr(Ar),
    { Instr =.. [return, Ar] }
  ; [tokWrite],!, arith_expr(Ar),
    {Instr =.. [write, Ar]}
  ; [tokRead],!, [tokVar(N)],
    {Instr =.. [read,var(N)] }     %% !! dodany recznie var
  ; [tokCall],!, procedure_call(Pro),
    {Instr =.. [call, Pro]}
  ; [tokWhile],!, bool_expr(Bool), [tokDo], complex_instruction(Inst), [tokDone],
    {Instr =.. [while,Bool,Inst]}
  ; [tokIf], bool_expr(Bool), [tokThen], complex_instruction(Inst), [tokElse],!, complex_instruction(Instr2), [tokFi],
    { Instr =.. [ife,Bool,Inst,Instr2] } 
  ; [tokIf], bool_expr(Bool), [tokThen], complex_instruction(Inst), [tokFi],
    {Instr =.. [if,Bool,Inst]}.

arith_expr(Lol) -->
   summand(Summand), arith_expr(Summand, Expr), { flatten(Expr, Lol)}.

arith_expr(Acc, Expr) -->
   additive_op(Op), !, summand(Summand),
      { Acc1 = [ Acc, Summand, Op] }, arith_expr(Acc1, Expr).
arith_expr(Acc, Acc) -->
   [].

%% skladnik
summand(Expr) -->
   factor(Factor), summand(Factor, Expr).

summand(Acc, Expr) -->
   multiplicative_op(Op), !, factor(Factor),
      { Acc1 = [ Acc, Factor, Op] }, summand(Acc1, Expr).
summand(Acc, Acc) -->
   [].

%% czynnik
factor(Expr) -->
  [tokMinus],!, simple_expression(Expr1),
    {Expr =  neg( Expr1) } 
  ; simple_expression(Expr).

%% wyrazenie_proste
simple_expression(SimpleEx) -->
  [tokLParen],!, arith_expr(SimpleEx), [tokRParen]
  ; atomic_expression(SimpleEx).

%% wyrazenie_atomowe
atomic_expression(AtomicExpr) -->
  [tokNumber(N)],!, { AtomicExpr = const(N) }
  ; procedure_call(AtomicExpr),! %% jakos tak, 
  ; [tokVar(V)], {AtomicExpr = var(V) }.

%% wywolanie procedury
procedure_call(Proce) -->
  [tokVar(N), tokLParen], fact_arguments(Arg), [tokRParen],
    {Proce =.. [procedure, N, Arg ]}.

%% argumenty faktyczne
fact_arguments(Arg) -->
  fact_arguments_sequence(Arg),!
  ; [], { Arg = [] }. 

%% ciag argumentow faktycznych
fact_arguments_sequence(Args) -->
  fact_argument(Arg), [tokColon],!, fact_arguments_sequence(Args2),
    { Args = [Arg|Args2] }
  ; fact_argument(Arg), {Args = [Arg] }.

%% argument faktyczny
fact_argument(Arg) -->
  arith_expr(Arg).

%% LOGIKA

%% wyrazenie logiczne
bool_expr(Tmp) -->
  conjunct(Conjunct), bool_expr(Conjunct, Bool),{flatten(Bool, Tmp)}.
bool_expr(Acc, Bool) -->
  [tokOr],!, conjunct(Conjunct),
    {Acc1 = or(Acc , Conjunct)}, 
    bool_expr(Acc1,Bool).
bool_expr(Acc,Acc) --> [].

conjunct(Conjunct) -->
  condition(Condition), conjunct(Condition,Conjunct).
conjunct(Acc, Conjunct) -->
  [tokAnd], !, condition(Condition),
  { Acc1 = and(Acc, Condition)},
  conjunct(Acc1,Conjunct).
conjunct(Acc,Acc) --> [].

condition(Condition) -->
  ( [tokNot],!, rel_expr(Expr),
    {Condition = not(Expr) }
  ; rel_expr(Expr),
  {Condition = Expr }
  ).

rel_expr(Rel_expr) -->
  ( arith_expr(Left),!, rel_op(Op), arith_expr(Right),
    { Rel_expr =.. [Op, Left, Right] }
  ; [tokLParen],!, bool_expr(Rel_expr), [tokRParen]
  ). 

%% OPERATORY
additive_op(plus) -->
   [tokPlus], !.
additive_op(minus) -->
   [tokMinus].

multiplicative_op(times) -->
   [tokTimes], !.
multiplicative_op(divs) -->
   [tokDiv], !.
multiplicative_op(modulo) -->
   [tokMod].

rel_op(eq) -->
   [tokEq], !.
rel_op(neq) -->
   [tokNeq], !.
rel_op(lt) -->
   [tokLt], !.
rel_op(leq) -->
   [tokLeq], !.
rel_op(gt) -->
   [tokGt], !.
rel_op(geq) -->
   [tokGeq].

  %% high-assembler
  
prog(program(_Name,block(Declarations,Instructions)),Dec_List) -->
    set_sp,
    make_declarations(Declarations,[],0xfff0,Dec_List),
    instructions(Instructions),footer.

make_declarations([H|T],List_Acc,Adress,Dec_List) -->
    {H = var(Name), Adress_new is Adress -1 },!,
    sp_minus, make_declarations(T,[var(Name,Adress)|List_Acc],Adress_new,Dec_List).
make_declarations([],List_Acc,_,List_Acc) --> [].

replace_var([H|T],Dec) -->
    {ground(H), H = var(Name), member(var(Name,Adress),Dec) },!,
    [Adress],replace_var(T,Dec)
  ; {ground(H), H \= var(_)  },!,
    [H], replace_var(T,Dec)
  ; { ground(H), H = var(Name), not(member(var(Name,_),Dec)),abort} %%abort gdy zmienna nie zadeklarowana
  ; [H],replace_var(T,Dec).
replace_var([],_) --> [].

sp_minus --> load_reg(0xffff),[swapd,swapa,const(0x0001),swapd,sub,swapd,const(0xffff),swapa,swapd,store].
set_sp --> [const(0xffff),swapa,const(0xfff0),store].
footer --> [const(0x0000),syscall]. %%%!!!! UWAGA
push --> [swapd],load_reg(0xffff),[swapa,swapd,store,swapa,swapd,const(0x0001),swapd,sub,swapa,swapd,const(0xffff),swapa,store,swapd].
pop --> load_reg(0xffff),[swapd,swapa,const(0x0001),add,swapd,const(0xffff),swapa,swapd,store].
top --> load_reg(0xffff),[swapd,swapa,const(0x0001),add,swapa,swapd,load].
set_top --> [swapd],load_reg(0xffff),[swapd,swapa,const(0x0001),add,swapa,store].
load_reg(Reg) --> [const(Reg), swapa, load].
store_reg(Reg) --> [swapa,const(Reg),swapa,store].

instructions([I1|Instr]) -->
(
    {I1 = write(Arith) },!,
    arith_eval(Arith), top, [swapd], pop, [const(0x0002), syscall]
  ; {I1 = read(Var) },!,
    [const(1),syscall],store_reg(Var)
  ; {I1 = asgn(Var,Arith)},!,
    arith_eval(Arith), top, [swapd], pop, [swapd], store_reg(Var)
  ; {I1 = if(Bool,Instr2)},!,
    bool_eval(Bool,True,False),
    [label(True)], instructions(Instr2),
    [label(False)]
  ; {I1 = ife(Bool,Instr1,Instr2)},!,
    bool_eval(Bool,True,False),
    [label(True)],instructions(Instr1), [const(Fin), jump],
    [label(False)],instructions(Instr2),
    [label(Fin)]
  ; {I1 = while(Bool,Instr1)},!,
    [label(While_begin)],
    bool_eval(Bool,True,False),
    [label(True)], instructions(Instr1),
    [const(While_begin),jump],
    [label(False)]
),
(
    { Instr = [_|_] },!,
    instructions(Instr)
  ; { Instr = [] }
).

%% liczy wyrazenie arytmetyczne na stosie
arith_eval([A1|Arith]) -->
(   
    {A1 = plus},!,
    top, [swapd], pop, top, [add], set_top
  ; {A1 = const(N)},!,
    [const(N)], push
  ; {A1 = minus},!,
    top, [swapd], pop, top, [sub], set_top
  ; {A1 = times},!,
    top, [swapd], pop, top, [mul], set_top
  ; {A1 = divs},!,
    top, [swapd], pop, top, [div], set_top
  ; {A1 = modulo},!,
    top, [swapd], pop, top, [div, const(0xfff0),swapd,shift],set_top
  ; {A1 = var(X)},!,
    load_reg(var(X)),push
  ; {A1 = neg(Arith)},!,
    arith_eval(Arith), top, [swapd, const(0xffff), mul], set_top
),
(
    { Arith = [_|_]  },!,  
    arith_eval(Arith)
  ; { Arith  = [] }
).


bool_eval([Bool],True,False) -->
    {Bool = eq(Left,Right)},!,
    arith_eval(Left),
    arith_eval(Right),
    top, [swapd], pop, top, [sub, swapd], pop, [const(True),swapa,swapd,branchz,const(False),jump]
  ; {Bool = not(Bool_Expr)},!,
    bool_eval([Bool_Expr],False,True)
  ; {Bool = neq(Left,Right)},!,
    bool_eval([not(eq(Left,Right))],True,False).


%% get right - ustawia ladnie consty i liczby co 4
%% wywolywac z WC = 1 i Number_List = []
get_const_right([L1|List],WC,Number_List) -->
(
    { L1 = label(X) },!,
    [label(X)],
      (   {List = [_|_]},!,
          get_const_right(List,WC,Number_List)
        ; {List = []}
      )
  ; { L1 \= const(_), 0 =\= WC mod 4, WCP is WC + 1 },!,
    [L1],
      (   {List = [_|_]},!,
          get_const_right(List,WCP,Number_List)
        ; {List = [], reverse(Number_List,R) },
          R/*Number_List*/
      )
  ; { L1 \= const(_), 0 is WC mod 4 ,reverse(Number_List,R)},!,
    [L1|R],
      (   {List = [_|_]},!,
          get_const_right(List,1,[])
        ; {List = []}
      )
  ; { L1 = const(N), 0 =\= WC mod 4, WCP is WC + 1 },!,
    [const],
      (   {List = [_|_]},!,
          get_const_right(List,WCP,[N|Number_List])
        ; {List = [],reverse(Number_List,R)},
          R/*Number_List*/,[N]
      )
  ; { L1 = const(N), 0 is WC mod 4,reverse(Number_List,R) },!,
    [const],R/*Number_List*/,[N],
      (   {List = [_|_]},!,
          get_const_right(List,1,[])
        ; {List = []}
      )
).
%%dfsdfjsdkfn
%sdfs fsdklfnlsnf
%% predykat -->
nops(N) --> 
    {N is 0},!,
    []
  ; {N is 1},!,
    [nop]
  ; {N is 2},!,
    [nop,nop]
  ; {N is 3},!,
    [nop,nop,nop].

%% wywolywac z WC = 0
nopping([L1|List],WC) -->
(
    {nonvar(L1), L1 = label(Label), 0 is WC mod 4},!,
    [label(Label)],
      (   {List = [_|_]},!,
          nopping(List,0)
        ; {List = [] }
      )
  ; {nonvar(L1), L1 = label(Label), Pos is WC mod 4, Pos =\= 0, Nop_q is 4 - Pos},!,
    nops(Nop_q),[label(Label)],
      (   {List = [_|_]},!,
          nopping(List,0)
        ; {List = [] }
      )
  ; {nonvar(L1),L1 = jump, 3 is WC mod 4},!,
    [jump],
      (   {List = [_|_]},!,
          nopping(List,0)
        ; {List = [] }
      )
  ; {nonvar(L1),L1 = jump, Pos is WC mod 4, Nop_q is 4 - Pos-1},!,
    [jump],nops(Nop_q),
      (   {List = [_|_]},!,
          nopping(List,0)
        ; {List = [] }
      )
  ; {WCP is WC + 1},
    [L1],
      (   {List = [_|_]},!,
          nopping(List,WCP)
        ; {List = [],Nopi is 3 - (WC mod 4) },
          nopping_extra(Nopi)
      )
).

nopping_extra(Nop_missing) -->
    {Nop_missing is 0},!,
    []
  ; {Nop_missing is 1},!,
    [nop]
  ; {Nop_missing is 2},!,
    [nop,nop]
  ; {Nop_missing is 3},!,
    [nop,nop,nop].



%% zamiast [label(Label)] dac NIC | juz jest zakomentowane
labeling([H|T],WC) -->
    {nonvar(H), H = label(Label), Label is WC div 4 },!,
    /*[label(Label)],*/labeling(T,WC)
  ; {nonvar(H), H \= label(_),not(number(H)), WCP is WC + 1},!,
    [H],labeling(T,WCP)
  ; [H],{WCP is WC + 4},labeling(T,WCP).
labeling([],_) --> [].

%% konstruuje liste liczb w zapisie dziesietnym
construct([H|T],WC,Acc) -->
  {number(H),!},!,[H],construct(T,WC,0)
;
(
  (
      {H = nop,     !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 0*16^Pot},!
    ; {H = syscall, !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 1*16^Pot},!
    ; {H = load,    !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 2*16^Pot},!
    ; {H = store,   !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 3*16^Pot},!
    ; {H = swapa,   !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 4*16^Pot},!
    ; {H = swapd,   !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 5*16^Pot},!
    ; {H = branchz, !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 6*16^Pot},!
    ; {H = branchn, !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 7*16^Pot},!
    ; {H = jump,    !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 8*16^Pot},!
    ; {H = const,   !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 9*16^Pot},!
    ; {H = add,     !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 10*16^Pot},!
    ; {H = sub,     !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 12*16^Pot},!
    ; {H = mul,     !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 12*16^Pot},!
    ; {H = div,     !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 13*16^Pot},!
    ; {H = shift,   !,Pot is 3 - (WC mod 4), WCP is WC + 1, Acc_New is Acc + 14*16^Pot},!
  ),
  (
      {0 is WCP mod 4},!,
      [Acc_New],construct(T,WCP,0)
    ; {0 =\= WCP mod 4},!,
      construct(T,WCP,Acc_New)
  )
).
construct([],_,_) --> [].

shit_to_file(Input, Output) :-
    read_file(Input, SourceCode),
    phrase(lexer(Tokens), SourceCode),
    phrase(program(Absynt), Tokens),
    phrase(prog(Absynt,Dec_List),Assemble),
    phrase(nopping(Assemble,0),Nopped),
    phrase(get_const_right(Nopped,1,[]),Consted),
    phrase(replace_var(Consted,Dec_List),Replaced),
    phrase(labeling(Replaced,0),Labeled),
    open(Output,write, Labeled),
    write(Stream,Absynt),
    close(Stream).

%% add construction predicate to above

read_file(File, Chars) :-
    open(File, read, Stream),
    get_code(Stream, Char),
    read_all(Stream, Char, Chars),
    close(Stream).
read_all(_, -1, []) :- !.
read_all(Stream, Char, [Char|Chars]) :-
    get_code(Stream, NextChar),
    read_all(Stream, NextChar, Chars).

