% Assignment 1 - Prolog and Search; Part 1 (T1, 2020)
% Completed by Dheeraj Satya Sushant VIswanadham (z5204820)
% Started: 07/03/2020 | Last edited: 15/03/2020
%
% Note: Used various online resources such as CSE Prolog dictionary 
% (http://www.cse.unsw.edu.au/~billw/prologdict.html#comment)
% and Prolog Reading (https://www.metalevel.at/prolog/reading) as well as
% Tutorial work and Lecture Notes.
%
% ------------------------------------------------------------------------------

% Question 1.1: List Processing
% Predicate: sumsq_even(Numbers, Sum) 
% Description: Sums the squares of only the even numbers in a list of integers.
% Note: assuming that input data consists of integers only i.e. is valid input.

sumsq_even([H|_T], _Sum):-                  % error -
                                            % valid number not entered
    not(number(H)) -> 
    write('Please enter a valid integer and try again!'),
    break, !.

sumsq_even([], 0) :- !.                     % base case - 
                                            % empty list

sumsq_even([H|T], Sum) :-                   % recursive case - 
                                            % if H is odd then go to next case 
                                            % where it is even and square it, 
                                            % adding the Result to the Sum.
    sumsq_even(T, Sum),
    1 is mod(H, 2), !.
    
sumsq_even([H|T], Sum) :-
    sumsq_even(T, Result),
    0 is mod(H, 2),
    Sum is (H * H) + Result.

% ------------------------------------------------------------------------------

% Question 1.2: List Processing
% Predicate: log_table(NumberList, ResultList)
% Description: Binds ResultList to the list of pairs consisting of a number and 
% its log, for each number in NumberList.
% Note: assuming that input data consists of numbers only i.e. is valid input.

log_table([], []) :- !.                     % base case - 
                                            % empty NumberList

log_table([H|T], Final) :-                  % error - 
                                            % valid number not entered
    not(number(H)) -> 
    write('Please enter a valid number and try Again! \n'),
    log_table(T, Final),
    break, !.
    
log_table([H|T], Final) :-                  % error -
                                            % number has to be greater than 0
    not(H > 0) -> 
    write('The logarithm of numbers 0 and below are undefined. Please try Again! \n'),
    log_table(T, Final),
    break, !.

log_table([H|T], [[H, Value]|Final]) :-     % recursive case - 
                                            % get the log value of H 
                                            % in NumberList.
    log_table(T, Final),
    Value is log(H).
    
% ------------------------------------------------------------------------------

% Question 1.3: List Processing
% Predicate: paruns(List, RunList)
% Description: Converts a list of numbers into the corresponding list of 
% parity runs where a run is a (maximal) sequence of consecutive even or odd 
% numbers within the original list.
% Note: assuming that input data consists of integers only i.e. is valid input.

concatenate([], List, List).                % base case - 
                                            % concatenation of empty List

concatenate([H|A], B, [H|C]) :-             % recursive case - 
                                            % if concatenation of A with B is C, 
                                            % then concatenation of [H|A] and B 
                                            % is [H|C]
    concatenate(A, B, C).

app(Digit, [], [[Digit]]) :- !.

app(Digit, [[H|T1]|T2], Final) :-           % digits are both odd or even
    H mod 2 =:= Digit mod 2,
    concatenate([Digit], [H|T1], New),
    concatenate([New], T2, Final), !.
    
app(Digit, [[H|T1]|T2], Final) :-           % otherwise, make another List
    H mod 2 =\= Digit mod 2,
    concatenate([[H|T1]], T2, New1),
    concatenate([[Digit]], New1, Final).

paruns([], []) :- !.

paruns([H|T], Final) :-
    paruns(T, L),
    app(H, L, Final).

% ------------------------------------------------------------------------------

% Question 1.4: Prolog Terms
% Predicate: eval(Expr, Val)
% Description: Evaluates a given expression i.e. it could be a combination of 
% addition, subtraction, multiplication and division.
% Note: assuming that input data consists of numbers only i.e. is valid input.

eval(Expr, Val) :-                          % checks if valid number entered
    number(Expr),
    Val is Expr.

eval(add(A, B), Val) :-                     % addition component
    eval(A, X),
    eval(B, Y),
    Val is (X + Y).

eval(sub(A, B), Val) :-                     % subtraction component
    eval(A, X),
    eval(B, Y),
    Val is (X - Y).

eval(mul(A, B), Val) :-                     % multiplication component
    eval(A, X),
    eval(B, Y),
    Val is (X * Y).

eval(div(A, B), Val) :-                     % division component
                                            % note: if dividing by zero then
                                            % error will pop up saying you cannot
                                            % which is what we want!
    eval(A, X),
    eval(B, Y),
    not(Y =:= 0) -> Val is (X / Y);
    write('Cannot divide by zero - please try again!'),
    break.
