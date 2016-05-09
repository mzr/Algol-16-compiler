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

%% ARYTMETYKA
%% wyrazenie arytmetyczne
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
    {Expr = [ Expr1, minus] }
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
    {Acc1 = [Acc , Conjunct, or]}, 
    bool_expr(Acc1,Bool).
bool_expr(Acc,Acc) --> [].

conjunct(Conjunct) -->
  condition(Condition), conjunct(Condition,Conjunct).
conjunct(Acc, Conjunct) -->
  [tokAnd], !, condition(Condition),
  { Acc1 = [Acc, Condition, and]},
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
    { Rel_expr = [Left, Right, Op] }
  ; [tokLParen],!, bool_expr(Rel_expr), [tokRParen]
  ). 



%% INSTRUKCJE
program(Prog) -->
  [tokProgram, tokVar(N)], block(Blo),
    {Prog =.. [procedure, N, Blo]}. 

%% blok
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

%% zmienne
variables(Vars) -->
  variabl(V), [tokColon],!, variables(Va),
  { Vars = [V|Va] }
  ; variabl(Var),
  {Vars = [Var] }.

%% zmienna
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

%% instrukcja
instruction(Instr) -->
  [tokVar(N)],!, [tokAssgn], arith_expr(Ar),
    {Instr =.. [asgn, N, Ar] }
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


to_file(CharCodeList,File) :-
  parse(CharCodeList,Absynt),
   open(File,write, Stream),
   write(Stream, Absynt),
   close(Stream).

main(Input,Output) :-
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