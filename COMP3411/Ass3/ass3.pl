% Assignment 3 - Natural Language Processing; (T1, 2020)
% Completed by Dheeraj Satya Sushant Viswanadham (z5204820)
% Started: 25/04/2020 | Last edited: 11/05/2020 
%
% Note: Used various online resources such as CSE Prolog dictionary 
% (http://www.cse.unsw.edu.au/~billw/prologdict.html), 
% CSE NLP Dictionary (http://www.cse.unsw.edu.au/~billw/nlpdict.html)
% and Prolog Reading (https://www.metalevel.at/prolog/reading) as well as
% Tutorial work and Lecture Notes.
%
% This program handles simple sentences only. It will break on complicated 
% sentences such as: 'run([mary,gave,the wallet,to,john], Refs).' and will 
% result in errors if it encounters such a sentence.
%
% ------------------------------------------------------------------------------

% A grammar the covers most of the examples in COMP3411 lectures

:- dynamic(history/1).

sentence(VP) --> noun_phrase(Number, Actor), verb_phrase(Actor, Number, VP).

noun_phrase(plural, set(NP1, NP2)) --> np1(_, NP1), [and], noun_phrase(_, NP2).
noun_phrase(Number, NP1) --> np1(Number, NP1).

np1(Number, thing(Noun, Properties)) -->
	determiner(Number, _),
	adjp(Properties),
	noun(Number, Noun).
np1(Number, thing(Noun, [PP | Properties])) -->
	determiner(Number, _),
	adjp(Properties),
	noun(Number, Noun),
	pp(Number, PP).
np1(Number, thing(Name, [])) -->
	proper_noun(Number, _, Name).
np1(Number, personal(Pro)) -->
	pronoun(Number, _, Pro).
np1(Number1, possessive(Pos, NP)) -->
	possessive_pronoun(Number1, _, Pos), noun_phrase(_, NP).
np1(Number, object(Noun)) -->
	num(Number), noun(Number, Noun).

adjp([Adj]) --> adjective(Adj).
adjp([]) --> [].

verb_phrase(Actor, Number, event(V, [actor(Actor) | Adv])) -->
	verb(Number, V),
	adverb(Adv).
verb_phrase(Actor, Number, event(V, [actor(Actor), object(NP) | Adv])) -->
	verb(Number, V),
	noun_phrase(_, NP),
	adverb(Adv).
verb_phrase(Actor, Number, event(V, [actor(Actor), object(NP), PP])) -->
	verb(Number, V),
	noun_phrase(_, NP),
	pp(Number, PP).
verb_phrase(Actor, Number, event(V, [actor(Actor), PP])) -->
	verb(Number, V),
	pp(_, PP).

pp(_, PP) --> prep(NP, PP), noun_phrase(_, NP).

% The next set of rules represent the lexicon

prep(NP, object(NP)) --> [of].
prep(NP, object(NP)) --> [to].
prep(NP, instrument(NP)) --> [with].
prep(NP, object(NP)) --> [in].
prep(NP, object(NP)) --> [for].

determiner(singular, det(a)) --> [a].
determiner(_, det(the)) --> [the].
determiner(plural, det(those)) --> [those].
determiner(_, _) --> [].

pronoun(singular, masculine, he) --> [he].
pronoun(singular, feminine, she) --> [she].
pronoun(singular, neutral, that) --> [that].
pronoun(plural, neutral, those) --> [those].
pronoun(singular, neutral, Pro) --> [Pro], {member(Pro, [i, someone, it])}.
pronoun(plural, neutral, Pro) --> [Pro], {member(Pro, [they, some])}.

possessive_pronoun(singular, masculine, his) --> [his].
possessive_pronoun(singular, feminine, her) --> [her].

prep(of) --> [of].
prep(to) --> [to].
prep(with) --> [with].
prep(in) --> [in].
prep(for) --> [for].

num(singular) --> [one].
num(plural) --> [two];[three];[four];[five];[six];[seven];[eight];[nine];[ten].

noun(singular, Noun) --> [Noun], {thing(Noun, Props), member(number(singular), Props)}.
noun(plural, Noun) --> [Noun], {thing(Noun, Props), member(number(plural), Props)}.

proper_noun(singular, Gender, Name) -->
	[Name],
	{
		thing(Name, Props), member(isa(person), Props), member(gender(Gender), Props)
	}.
proper_noun(singular, neutral, france) --> [france].

adjective(prop(Adj)) --> [Adj], {member(Adj, [red,green,blue])}.

verb(_, Verb) --> [Verb], {member(Verb, [lost,found,did,gave,looked,saw,forgot,is])}.
verb(singular, Verb) --> [Verb], {member(Verb, [scares,hates])}.
verb(plural, Verb) --> [Verb], {member(Verb, [scare,hate])}.

adverb([adv(too)]) --> [too].
adverb([]) --> [].

% You may chose to use these items in the database to provide another way
% of capturing an objects properties.

thing(john, [isa(person), gender(masculine), number(singular)]).
thing(sam, [isa(person), gender(masculine), number(singular)]).
thing(bill, [isa(person), gender(masculine), number(singular)]).
thing(jack, [isa(person), gender(masculine), number(singular)]).
thing(monet, [isa(person), gender(masculine), number(singular)]).

thing(mary, [isa(person), gender(feminine), number(singular)]).
thing(annie, [isa(person), gender(feminine), number(singular)]).
thing(sue, [isa(person), gender(feminine), number(singular)]).
thing(jill, [isa(person), gender(feminine), number(singular)]).

thing(wallet, [isa(physical_object), gender(neutral), number(singular)]).
thing(car, [isa(physical_object), gender(neutral), number(singular)]).
thing(book, [isa(physical_object), gender(neutral), number(singular)]).
thing(telescope, [isa(physical_object), gender(neutral), number(singular)]).
thing(pen, [isa(physical_object), gender(neutral), number(singular)]).
thing(pencil, [isa(physical_object), gender(neutral), number(singular)]).
thing(cat, [isa(physical_object), gender(neutral), number(singular)]).
thing(mouse, [isa(physical_object), gender(neutral), number(singular)]).
thing(man, [isa(physical_object), gender(neutral), number(singular)]).
thing(bear, [isa(physical_object), gender(neutral), number(singular)]).

thing(cats, [isa(physical_object), gender(neutral), number(plural)]).
thing(mice, [isa(physical_object), gender(neutral), number(plural)]).
thing(men, [isa(physical_object), gender(neutral), number(plural)]).
thing(bears, [isa(physical_object), gender(neutral), number(plural)]).

thing(capital, [isa(abstract_object), gender(neutral), number(singular)]).

thing(france, [isa(place), gender(neutral), number(singular)]).

event(lost, [actor(_), object(_), tense(past)]).
event(found, [actor(_), object(_), tense(past)]).
event(saw, [actor(_), object(_), tense(past)]).
event(forgot, [actor(_), object(_), tense(past)]).
event(scares, [actor(_), object(_), tense(present), number(singular)]).
event(scare, [actor(_), object(_), tense(present), number(plural)]).
event(hates, [actor(_), object(_), tense(present), number(singular)]).
event(hate, [actor(_), object(_), tense(present), number(plural)]).
event(gave, [actor(Person1), recipient(Person2), object(_), tense(past)]) :- Person1 \= Person2.

personal(i, [number(singular), gender(neutral)]).
personal(he, [number(singular), gender(masculine)]).
personal(she, [number(singular), gender(feminine)]).
personal(it, [number(singular), gender(neutral)]).
personal(that, [number(singular), gender(neutral)]).
personal(those, [number(plural), gender(neutral)]).
personal(they, [number(plural), gender(neutral)]).

possessive(his, [number(singular), gender(masculine)]).
possessive(her, [number(singular), gender(feminine)]).

% ------------------------------------------------------------------------------
% Summary - You have to write this:
%
%
% Basic Outline:
% input sentence: S
% output sentence: X
%
% Within process predicate: 
% X = input which is the parse of the sentence, 
% [] = initial list of references corresponding to each of the pronouns 
% which is empty,
% Refs = the final list.
% assert should be in the format of: assert(history(information_from_database)) 
% e.g. 
% assert(history(thing(john, [isa(person), gender(masculine), number(singular)])).
%
% To link the 'thing' we have extracted to its attributes in the database, do:
% thing(Name, PList), where Name = name of the person and PList are the list 
% of properties related to that thing
% 
% E.g. as below:
% process(LogicalForm, Ref1, Ref2).
%
% process(S, Ref1, Ref3) :-
%     .... do some process
%     X is the new reference
%     append(Ref1, [X], Ref2),
%     process(S1, Ref2, Ref3).
%
% process(<pattern>) :- e.g. one process for (thing, and variables for 
%                       'john' and 'bill')
%     assert(history(...)), etc.
% ------------------------------------------------------------------------------

% Base case: simple empty list
process([], _, RefList):-
    append([], [], RefList).


% Recursive Cases:

% Iterate through input getting elements E1 and E2 recursively, 
% (e.g. E1 = lost and E2 = [actor()|object()]) and 
% add the event to the History.
process(event(E1, E2), Ref, Ref1) :-
    assert(history(event(E1, E2))),
    process(E2, Ref, Ref1).

% Iterate through input recursively when we have the format [actor(), object()],
% and append the references obtained to Ref3 List.
process([E1|E2], Ref, Ref3) :-
    process(E1, Ref, Ref1),
    process(E2, Ref, Ref2),
    append(Ref1, Ref2, Ref3).

% Iterate through Actor component of E2 to get the name of the actor e.g. 'john'
% Note: Look at 'person' in the grammar given.
process(actor(Name), Ref, Ref1) :-
    process(Name, Ref, Ref1).

% When there are two 'things' (such as 'jack and jill'), 
% then add the relevant set and history entry:
% Note - We do not know about the property list, hence '_'.
process(set(thing(NameOne, _), thing(NameTwo, _)), _,_) :-
    process(thing(NameOne, _), _, _),
    process(thing(NameTwo, _), _, _),
    asserta(history(set([thing(NameOne, []), thing(NameTwo, [])], [number(plural), gender(neutral)]))).

% Iterate through the Object component of E2 to get the object (e.g. 'wallet'):
% Note: Look at 'physical_object' in the grammar given.
process(object(Obj), Ref, Ref1) :-
    process(Obj, Ref, Ref1).

% Add thing(Name, PList) to History (most recent on top via 'asserta'):
process(thing(Name, _), _, _) :-
    thing(Name, PList),
    asserta(history(thing(Name, PList))).

% Referencing pronouns within the History:
% Note: the following code is similar to the code given in the assignment 
% specifications by Claude Sammut as it follows the same recommended process 
% for resolving pronouns.

% For possessive pronouns:
process(possessive(PNoun, PList), Ref, Ref1) :-
    possessive(PNoun, Elem1),
    member(gender(State), Elem1),
    member(number(Larity), Elem1),
    history(thing(Ref3, Elem2)),
    member(gender(State), Elem2),
    member(number(Larity), Elem2),
    append([Ref3], Ref, Ref1),
    process(PList, Ref, Ref1).

% Specific to any 'sets' in the history:
process(personal(PNoun), Ref, Ref1) :-
    personal(PNoun, Elem1),
    member(gender(State), Elem1),
    member(number(Larity), Elem1),
    history(set([thing(NameOne, _), thing(NameTwo, _)], Elem2)),
    member(gender(State), Elem2),
    member(number(Larity), Elem2),
    append([[NameOne, NameTwo]], Ref, Ref1).

% For personal pronouns:
process(personal(PNoun), Ref, Ref1) :-
    personal(PNoun, Elem1),
    member(gender(State), Elem1),
    member(number(Larity), Elem1),
    history(thing(Ref3, Elem2)),
    member(gender(State), Elem2),
    member(number(Larity), Elem2),
    append([Ref3], Ref, Ref1).


% Running the program:
run(S, Refs) :-
	sentence(X, S, []), !,
	writeln(X),
	process(X, [], Refs),
	listing(history/1).