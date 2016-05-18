# Algol-16-compiler

%% ****************************************************************************** %%
%%                                                                                %%
%% dingorth                                                                       %%
%% nr indeksu                                                                     %%
%% skorzystalem z pliku while-parser.pl                                           %%
%%                                                                                %%
%% Wersja języka: podstawowa za 22 pkt.                                           %%
%%                                                                                %%
%% Glowny predykat algol16/2 (na dole zgodnie z zadaniem),                        %%
%% jednak duzo ciekawszy jest                                                     %%
%% predykat: instr_list_to_file(Input, Output) (na samym dole)                    %%
%% Input - nazwa pliku wejsciowego programu napisanego w jezyku Algol 16          %%
%% Output - nazwa pliku w ktorym znajdzie sie prologowa lista liczb               %%
%%                                                                                %%
%% ****************************************************************************** %%


%% HOW TO USE?
%% 
%% use instr_list_to_file( input_file_with_algol16_code, machine_code_output_file )
%% then use script ./compile_words_and_run.sh machine_code_output_file
%% 
