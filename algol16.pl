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

%% skip --> (comment ; white), skip,!.
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
declarations(Dec) --> %% to cos chyba popsulem wczesniej
  declaration(Dec2),!,declarations(Dec1),
    {Dec = [ Dec2 | Dec1] }
  ; [], { Dec = [] }.

%% deklaracja
declaration(Dec) -->
  declarator(Dec),!
  ; procedura(Dec).

%% deklarator
declarator(Dec) -->
  [tokLocal], variables(Dec).

variables(Vars) -->
  variabl(V), [tokColon],!, variables(Va),
  { Vars = [V|Va] }
  ; variabl(Var),
  {Vars = [Var] }.

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


parse(CharCodeList, Absynt) :-
   phrase(lexer(TokList), CharCodeList),
   phrase(bool_expr(Absynt), TokList).


to_file(Program, File) :-
  assembly(Program, Absynt),
   open(File,write, Stream),
   write(Stream, Absynt),
   close(Stream).

assembly_to_file(Input, Output) :-
    read_file(Input, SourceCode),
    phrase(lexer(Tokens), SourceCode),
    phrase(program(Absynt), Tokens),
    phrase(prog(Absynt),Assemble),
    open(Output,write, Stream),
    write(Stream,Assemble),
    close(Stream).

parse_to_file(Input,Output) :-
    read_file(Input, SourceCode),
    phrase(lexer(Tokens), SourceCode),
    phrase(program(Absynt), Tokens),
    open(Output,write, Stream),
    write(Stream,Absynt),
    close(Stream).

read_file(File, Chars) :-
    open(File, read, Stream),
    get_code(Stream, Char),
    read_all(Stream, Char, Chars),
    close(Stream).
read_all(_, -1, []) :- !.
read_all(Stream, Char, [Char|Chars]) :-
    get_code(Stream, NextChar),
    read_all(Stream, NextChar, Chars).


  %% high-assembler

assembly(In,Out) :-
  phrase(prog(In),Out).

prog(program(_Name,block(_Declarations,Instructions))) -->
   [set_sp], instructions(Instructions),[footer].

set_sp --> [const(ffff),swapa,const(fff0),store].
footer --> [const(0000),syscall].
push --> [swapd],load_reg(ffff),[swapa,swapd,store,swapa,swapd,const(0001),swapd,sub,swapa,swapd,const(ffff),swapa,store,swapd].
pop --> load_reg(ffff),[swapd,swapa,const(0001),add,swapd,const(ffff),swapa,swapd,store].
top --> load_reg(ffff),[swapd,swapa,const(0001),add,swapa,swapd,load].
set_top --> [swapd],load_reg(ffff),[swapd,swapa,const(0001),add,swapa,store].
load_reg(Reg) --> [const(Reg), swapa, load].
store_reg(Reg) --> [swapa,const(Reg),swapa,store].


instructions([I1|Instr]) -->
(
    {I1 = write(Arith) },!,
    arith_eval(Arith), [top, swapd, pop, const(2), syscall]
  ; {I1 = read(Var) },!,
    [const(1),syscall,store_reg(Var)]
  ; {I1 = asgn(Var,Arith)},!,
    arith_eval(Arith), [top, swapd, pop, swapd, store_reg(Var)]
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
    [top, swapd, pop, top, add, set_top]
  ; {A1 = const(N)},!,
    [const(N), push]
  ; {A1 = minus},!,
    [top, swapd, pop, top, sub, set_top]
  ; {A1 = times},!,
    [top, swapd, pop, top, mul, set_top]
  ; {A1 = divs},!,
    [top, swapd, pop, top, div, set_top]
  ; {A1 = modulo},!,
    [top, swapd, pop, top, div, swapd, const(-16),swapd,shift,set_top]
  ; {A1 = var(X)},!,
    [load_reg(var(X)),push]
  ; {A1 = neg(Arith)},!,
    arith_eval(Arith), [top, swapd, const(-1), mul, set_top]
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
    [top, swapd, pop, top, sub, swapd, pop, const(True),swapa,swapd,branchz,const(False),jump]
  ; {Bool = not(Bool_Expr)},!,
    bool_eval([Bool_Expr],False,True)
  ; {Bool = neq(Left,Right)},!,
    bool_eval([not(eq(Left,Right))],True,False).





%% get right - ustawia ladnie consty i liczby co 4
%% wywolywac z WC = 1 i Number_List = []
get_const_right([L1|List],WC,Number_List) -->
(
    { L1 \= const(_), 0 =\= WC mod 4, WCP is WC + 1 },!,
    [L1],
      (   {List = [_|_]},!,
          get_const_right(List,WCP,Number_List)
        ; {List = [], reverse(Number_List,R) },
          R
      )
  ; { L1 \= const(_), 0 is WC mod 4 },!,
    [L1|Number_List],
      (   {List = [_|_]},!,
          get_const_right(List,1,[])
        ; {List = []}
      )
  ; { L1 = const(N), 0 =\= WC mod 4, WCP is WC + 1 },!,
    [const],
      (   {List = [_|_]},!,
          get_const_right(List,WCP,[N|Number_List])
        ; {List = [],reverse(Number_List,R)},
          R,[N]
      )
  ; { L1 = const(N), 0 is WC mod 4,reverse(Number_List,R) },!,
    [const],R,[N],
      (   {List = [_|_]},!,
          get_const_right(List,1,[])
        ; {List = []}
      )
).

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
    {L1 = label(Label), 0 is WC mod 4 },!,
    [label(Label)],
      (   {List = [_|_]},!,
          nopping(List,0)
        ; {List = [] }
      )
  ; {L1 = label(Label), Pos is WC mod 4, Pos =\= 0, Nop_q is 4 - Pos -1},!,
    [label(Label)],nops(Nop_q),
      (   {List = [_|_]},!,
          nopping(List,0)
        ; {List = [] }
      )
  ; {L1 = jump, 3 is WC mod 4},!,
    [jump],
      (   {List = [_|_]},!,
          nopping(List,0)
        ; {List = [] }
      )
  ; {L1 = jump, Pos is WC mod 4, Nop_q is 4 - Pos-1},!,
    [jump],nops(Nop_q),
      (   {List = [_|_]},!,
          nopping(List,0)
        ; {List = [] }
      )
  ; {WCP is WC + 1},
    [L1],
      (   {List = [_|_]},!,
          nopping(List,WCP)
        ; {List = [] }
      )
).

