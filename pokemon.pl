% cihat kapusuz
% 2016400126
% compiling: yes
% complete: yes

:-[pokemon_data].

% 4.1 find_pokemon_evolution(+PokemonLevel, +Pokemon, -EvolvedPokemon)
% 4.1 checks the given Pokemon at given PokemonLevel is sufficient to evolve or not.
find_pokemon_evolution(PokemonLevel, Pokemon, EvolvedPokemon) :- 
pokemon_evolution(Pokemon, X, Y),
PokemonLevel >= Y -> find_pokemon_evolution(PokemonLevel, X, EvolvedPokemon); 
EvolvedPokemon=Pokemon.

% 4.2 pokemon_level_stats(+PokemonLevel, ?Pokemon, -PokemonHp, -PokemonAttack, -PokemonDefense)
% 4.2 finds Pokemon's stats according to it's level.
pokemon_level_stats(PokemonLevel, Pokemon, PokemonHp, PokemonAttack,PokemonDefense) :-
pokemon_stats(Pokemon,_,H,A,D),
PokemonHp is H+PokemonLevel*2,
PokemonAttack is A+PokemonLevel,
PokemonDefense is D+PokemonLevel.

% 4.3 single_type_multiplier(?AttackerType, +DefenderType, ?Multiplier)
% 4.3 gets multiplier from the AttackerType's list according to the DefenderType.
single_type_multiplier(AttackerType, DefenderType, Multiplier) :-
pokemon_types(Z),
type_chart_attack(AttackerType,List),
nth0(Index,Z,DefenderType),
nth0(Index,List,Multiplier).

% 4.4 type_multiplier(?AttackerType, +DefenderTypeList, ?Multiplier)
% 4.4 gets multiplier according to the DefenderTypeList.
type_multiplier(AttackerType, DefenderTypeList, Multiplier):-
length(DefenderTypeList,A),
DefenderTypeList = [H|T],
(A > 1 -> (single_type_multiplier(AttackerType, H, M1),
T=[Y|_],
single_type_multiplier(AttackerType, Y, M2),Multiplier is M1*M2); 
single_type_multiplier(AttackerType, H, M1),Multiplier is M1). 

% 4.5 pokemon_type_multiplier(?AttackerPokemon, ?DefenderPokemon, ?Multiplier)
% 4.5 finds the multiplier between AttackerPokemon and DefenderPokemon.
pokemon_type_multiplier(AttackerPokemon, DefenderPokemon, Multiplier) :-
pokemon_stats(AttackerPokemon, ListA, _,_,_),
pokemon_stats(DefenderPokemon, ListB, _,_,_),
length(ListA,A),
ListA=[H|T],
(A > 1 -> ((type_multiplier(H, ListB , M1),
T=[Y|_],
type_multiplier(Y, ListB , M2)), (M1 > M2 -> Multiplier=M1; Multiplier=M2)); (type_multiplier(H, ListB , M1), Multiplier=M1)).

% 4.6 pokemon_attack(+AttackerPokemon, +AttackerPokemonLevel, +DefenderPokemon, +DefenderPokemonLevel, -Damage)
% 4.6 finds the Damage after an attack from AttakerPokemon to DefenderPokemon.
pokemon_attack(AttackerPokemon, AttackerPokemonLevel, DefenderPokemon, DefenderPokemonLevel, Damage):-
pokemon_level_stats(AttackerPokemonLevel, AttackerPokemon, PokemonHp1, Attack1,Defense1),
pokemon_level_stats(DefenderPokemonLevel, DefenderPokemon, PokemonHp2, Attack2,Defense2),
pokemon_type_multiplier(AttackerPokemon, DefenderPokemon, Multiplier),
Damage is (0.5 * AttackerPokemonLevel * (Attack1 / Defense2) * Multiplier) +1.

% 4.7 pokemon_fight(+Pokemon1, +Pokemon1Level, +Pokemon2, +Pokemon2Level, -Pokemon1Hp, -Pokemon2Hp, -Rounds)
% 4.7 finds the Rounds which the fight will last, Pokemon1's and Pokemon2's hp after fight.
pokemon_fight(Pokemon1, Pokemon1Level, Pokemon2, Pokemon2Level,Pokemon1Hp, Pokemon2Hp, Rounds):-
pokemon_attack(Pokemon1, Pokemon1Level, Pokemon2, Pokemon2Level, Damage1),
pokemon_attack(Pokemon2, Pokemon2Level, Pokemon1, Pokemon1Level, Damage2),
pokemon_level_stats(Pokemon1Level, Pokemon1, PokemonHp1, Attack1, Defense1),
pokemon_level_stats(Pokemon2Level, Pokemon2, PokemonHp2, Attack2, Defense2),
Round2=floor(PokemonHp1 / Damage2),
Round1=floor(PokemonHp2 / Damage1),
(Round2 > Round1 -> (Pokemon1Hp is PokemonHp1 - (Round1+1)*Damage2,
Pokemon2Hp is PokemonHp2 - (Round1+1)*Damage1, Rounds is Round1+1);
(Pokemon1Hp is PokemonHp1 - (Round2+1)*Damage2,
Pokemon2Hp is PokemonHp2 - (Round2+1)*Damage1, Rounds is Round2+1)).  

% 4.8 pokemon_tournament(+PokemonTrainer1, +PokemonTrainer2, -WinnerTrainerList)
% 4.8 compete 2 PokemonTrainers and give the resulting list as WinnerTrainerList.
pokemon_tournament(PokemonTrainer1, PokemonTrainer2, WinnerTrainerList):-
pokemon_trainer(PokemonTrainer1, List1, Level1),
pokemon_trainer(PokemonTrainer2, List2, Level2),
evolve_all(List1,Level1,[],EvolvedPokemons1),
evolve_all(List2,Level2,[],EvolvedPokemons2),
fight_em(PokemonTrainer1,EvolvedPokemons1,Level1,PokemonTrainer2,EvolvedPokemons2,Level2,[],WinnerTrainerList).

% 4.8.1 evolve's all trainers pokemons and stored the evolvedpokemons as EvolvedPokemons.
evolve_all(List,Level,Tempones,EvolvedPokemons):-
List=[H|T],
Level=[L|K],
find_pokemon_evolution(L, H, Z),
append(Tempones,[Z],Tempones2),
length(T,I),
(I=0 -> EvolvedPokemons=Tempones2 ; evolve_all(T,K,Tempones2,EvolvedPokemons)).

% 4.8.2 makes all fights for all pokemons in same index in the EvolvedPokemons1 and EvolvedPokemons2.
fight_em(PokemonTrainer1,EvolvedPokemons1,Level1,PokemonTrainer2,EvolvedPokemons2,Level2,Tempones,WinnerTrainerList):-
EvolvedPokemons1=[H1|T1],
EvolvedPokemons2=[H2|T2],
Level1=[L1|S1],
Level2=[L2|S2],
pokemon_fight(H1, L1, H2, L2 ,Pokemon1Hp, Pokemon2Hp, Rounds),
(Pokemon1Hp >= Pokemon2Hp -> (append(Tempones,[PokemonTrainer1],Tempones2),!);
(append(Tempones,[PokemonTrainer2],Tempones2))),
length(T1,Len),
(Len=0 -> WinnerTrainerList=Tempones2;fight_em(PokemonTrainer1,T1,S1,PokemonTrainer2,T2,S2,Tempones2,WinnerTrainerList)).

% 4.9 best_pokemon(+EnemyPokemon, +LevelCap, -RemainingHP, -BestPokemon)
% 4.9 finds pokemon with the highest remaining hp as BestPokemon and its remaining hp as RemainingHP. 
best_pokemon(EnemyPokemon, LevelCap, RemainingHP, BestPokemon):-
findall(X,pokemon_stats(X,_,_,_,_),AllPokemons),
AllPokemons=[Head|Tail],
bestie(EnemyPokemon, LevelCap, AllPokemons, Tail, Head, RemainingHP, BestPokemon, []).

% 4.9.1 recursive function that stores all remaining hp's in TempHpList after the fight with the EnemyPokemon.
% 4.9.1 assigns the RemainingHP as the highest value in TempHpList.
% 4.9.1 finds the index of max_member in TempHpList and gets the related Pokemon and assigns it to BestPokemon.
bestie(EnemyPokemon, LevelCap, AllPokemons, Tail, Head, RemainingHP, BestPokemon, TempHpList):-
pokemon_fight(EnemyPokemon,LevelCap,Head,LevelCap,_, Pokemon2Hp, _),
append(TempHpList,[Pokemon2Hp],TempHpList2),
Tail=[H1|T1],
length(T1,L),
(L>0 -> bestie(EnemyPokemon, LevelCap,AllPokemons, T1, H1, RemainingHP, BestPokemon, TempHpList2) ; 
(max_member(X,TempHpList2), nth0(Index,TempHpList2,X), RemainingHP=X, nth0(Index,AllPokemons,Poke), BestPokemon=Poke)).

% 4.10 best_pokemon_team(+OpponentTrainer, -PokemonTeam)
% 4.10 finds best pokemon team against OpponentTrainer and stores it as PokemonTeam.
best_pokemon_team(OpponentTrainer, PokemonTeam):-
pokemon_trainer(OpponentTrainer, List1, Level1),
evolve_all(List1,Level1,[],EvolvedPokemons1),
best_pokemon_team_recursive(Level1,EvolvedPokemons1,[],PokemonTeam).

% goes through all Pokemons and finds all BestPokemons one by one, and stores them as PokemonTeam.
best_pokemon_team_recursive(Level1,EvolvedPokemons1,TempTeam,PokemonTeam):-
EvolvedPokemons1=[H1|T1],
Level1=[L1|S1],
best_pokemon(H1, L1, RemainingHP, BestPokemon),
append(TempTeam,[BestPokemon],TempTeam2),
length(T1,X),
(X>0 -> best_pokemon_team_recursive(S1,T1,TempTeam2,PokemonTeam);(PokemonTeam=TempTeam2)).

% 4.11 pokemon_types(+TypeList, +InitialPokemonList, -PokemonList)
% 4.11 finds the pokemons which have matching type in the TypeList and stores the match in PokemonList.
pokemon_types(TypeList, InitialPokemonList, PokemonList):-
findall(Pokemon,(member(Pokemon,InitialPokemonList),pokemon_types_recursive(TypeList,Pokemon)), PokemonList).


pokemon_types_recursive([H|TypeListTail],Pokemon):-
pokemon_stats(Pokemon,PokemonTypeList,_,_,_),
((member(H,PokemonTypeList),!); pokemon_types_recursive(TypeListTail,Pokemon)).

% 4.12 generate_pokemon_team(+LikedTypes, +DislikedTypes, +Criterion, +Count,-PokemonTeam)
% 4.12 generates a pokemon team with LikedTypes and not with DislikedTypes and sorts them according to the Criterion, crops them according the Count. 
% 4.12 stores the pokemons in PokemonTeam list as [Pokemon,HP,Attack,Defense].
generate_pokemon_team(LikedTypes, DislikedTypes, Criterion, Count,PokemonTeam):-
findall(X,pokemon_stats(X,_,_,_,_),AllPokemons),
pokemon_types(LikedTypes, AllPokemons, LikedPokemonList),
pokemon_types(DislikedTypes, AllPokemons, DislikedPokemonList),
findall(A,(member(A,LikedPokemonList),\+member(A,DislikedPokemonList)),RealWantedList),
findall([A,X],(member(A,RealWantedList), pokemon_stats(A,_,X,_,_)),HpList),
findall([B,Y],(member(B,RealWantedList), pokemon_stats(B,_,_,Y,_)),AttackList),
findall([C,Z],(member(C,RealWantedList), pokemon_stats(C,_,_,_,Z)),DefenseList),
(Criterion=h -> (sort(2, @>=, HpList, AllPokemonTeam1),findall([A1,X1,Y1,Z1],(member([A1,_],AllPokemonTeam1), pokemon_stats(A1,_,X1,Y1,Z1)),AllPokemonTeam));
(Criterion=a -> (sort(2, @>=, AttackList, AllPokemonTeam2),findall([A2,X2,Y2,Z2],(member([A2,_],AllPokemonTeam2), pokemon_stats(A2,_,X2,Y2,Z2)),AllPokemonTeam));
(sort(2, @>=, DefenseList, AllPokemonTeam3),findall([A3,X3,Y3,Z3],(member([A3,_],AllPokemonTeam3), pokemon_stats(A3,_,X3,Y3,Z3)),AllPokemonTeam)))),
crop(AllPokemonTeam,Count,PokemonTeam).

% crops the List1 and stores it in the List2 for the given length N.
conc1([],List0,List0).
conc1([Head|List1],List2,[Head|List0]):-conc1(List1,List2,List0).
crop(List1,N,List2) :-conc1(List2,_,List1) ,length(List2,N).















