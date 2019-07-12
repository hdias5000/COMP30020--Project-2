% % Author: Hasitha Dias <diasi@student.unimelb.edu.au>
% % Student ID: 789929
% % Date Modified: 10/10/2018
% %
% % Purpose: Project 2 COMP30020


%%%%%%%%%% Breif Description of this Project
% % This file attempts to solve a fillin puzzle. A fillin puzzle (sometimes
% % called a fill-it-in) is like a crossword puzzle, except that instead of
% % being given obscure clues telling which words go where, you are given a
% % list of all the words to place in the puzzle, but not told where they go.
% %
% % The puzzle consists of a grid of squares, most of which are empty, into
% % which letters or digits are to be written, but some of which are filled
% % in solid, and are not to be written in. More information regarding the
% % game is available in the project specification.
% %
% % The goal is to solve the puzzle successfully and one of the key points
% % was to make finding the solution as efficient as possible.
% % The principal predicate is solve_puzzle/3.



%%%%%%%%%% Beginning of the Prolog Solution to this Project

% Load the correct transpose library
:- ensure_loaded(library(clpfd)).


%%%%%%%%%% Main

% % This is the main predicate used. It gives the filenames of the files
% % containing the Puzzle, WordList and also the filename into which the
% % Solved Puzzle should be put into.
main(PuzzleFile, WordlistFile, SolutionFile) :-
	% read Puzzle and WordList files
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),

	% validates the dimensions of the Puzzle
	valid_puzzle(Puzzle),

	% solves the puzzle
	solve_puzzle(Puzzle, Wordlist, Solved),

	% outputs the Solved Puzzle into specified output file
	print_puzzle(SolutionFile, Solved).


%%%%%%%%%% Input/Output Predicates: Preincluded.

% % Should contain the Filename which will be opened. Content is a list of
% % strings, each corresponding to a line in the file. File is closed once
% % all lines are read.
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

% % Stream should already be opened and Content will contain the lines of the
% % file as they are read iteratively through this predicate.
read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

% % Stream should already be opened. Line is the last line that was read from
% % the file. Last indicates whether EOF of the file is reached.
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

% % Opens a file with filename "SolutionFile" and creates a stream to Output
% % each row into the stream/file. Close the file when entire puzzle is mapped.
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

% % Outputs/Maps the given row to the stream. Each character in the row is
% % handled by put_puzzle_char.
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

% % Handles every character. If it's an empty variable outputs a '_' into the
% % stream else simply outputs the Character in the Char.
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

% % Checks if Puzzle is a list if lists, whether all rows are of the same
% % length but doesn't need to have equal number of columns and rows.
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(same_length(Row), Rows).


%%%%%%%%%% Predicate that will be solving the Puzzle

% % This predicate solves the puzzle when Puzzle, Wordlist are given.
solve_puzzle(Puzzle, Wordlist, SolvedPuzzle) :-
	% spaces('_') in the puzzle are replaced with logical variables when puzzle
	% is given.
	putVariablesInPuzzle(Puzzle,SolvedPuzzle),

	% slots for words are created when puzzle with logical variables is given
	returnAllSlots(SolvedPuzzle, Slots),

	% the slots are filled with the matching words
	fillSlots(Slots, Wordlist).


%%%%%%%%%% Predicates replace the spaces in the Puzzle with logical variables

% % Predicate holds when PuzzleWithVars contains the Puzzle and the spaces are
% % replaced with logical variables.
putVariablesInPuzzle(Puzzle,PuzzleWithVars) :-
	maplist(putVariablesInRow,Puzzle,PuzzleWithVars).

% % Predicate holds when RowWithVars contains the each row and the spaces are
% % replaced with logical variables.
putVariablesInRow(Row,RowWithVars) :-
	maplist(putVariables,Row,RowWithVars).

% % Spaces get replaced by logical variables and all other characters are
% % preserved.
putVariables('_',_).
putVariables(Char,Char) :- Char \= '_'.


%%%%%%%%%% Predicates that get slots from the Puzzle for the words

% % Predicate finds the horizontal and vertical slots in the Puzzle.
returnAllSlots(Puzzle, Slots) :-
	% finds the horizontal slots
	puzzleSlots(Puzzle, [],RowSlots),

	% Holds if TransPuzzle is the transpose of Puzzle
	transpose(Puzzle, TransPuzzle),

	% finds the vertical slots
	puzzleSlots(TransPuzzle,[], ColumnSlots),

	% combines both horizontal and vertical slots
	append(RowSlots,ColumnSlots,Slots).

% % Predicate holds when iterated through the entire puzzle and the
% % CurrentSlots are unified with the return/output Slots.
puzzleSlots([], Slots,Slots).
% iterates through all rows finding the slots for each row
puzzleSlots([Row|Rows],CurrentSlots, Slots) :-
	% NewSlots contains previous slots and the slots from current row
	rowSlots(Row, [], CurrentSlots, NewSlots),
	puzzleSlots(Rows, NewSlots,Slots).

% % Predicate holds when iterated through the entire row and the
% % CurrentSlots are unified with the return/output Slots.
rowSlots([],[],Slots,Slots).
% only words longer than 1 in length are added as Slots at end of row
rowSlots([],Word,CurrentSlots,Slots) :-
	isLengthGreaterThanOne(Word),
	append(CurrentSlots,[Word],Slots).

% words shorter than or equal to 1 are ignored at the end of the row
rowSlots([],Word,CurrentSlots,Slots) :-
	not(isLengthGreaterThanOne(Word)),
	rowSlots([],[],CurrentSlots,Slots).

% when on filled squares('#') word is added to slot only if longer than 1
rowSlots([Var|Row],CurrentWord,CurrentSlots,Slots) :-
	Var=='#',
	isLengthGreaterThanOne(CurrentWord),
	append(CurrentSlots,[CurrentWord],NewSlots),
	rowSlots(Row, [],NewSlots,Slots).

% when on filled squares('#') word is ignored if shorter than or equal to 1
rowSlots([Var|Row],CurrentWord,CurrentSlots,Slots) :-
	Var=='#',
	not(isLengthGreaterThanOne(CurrentWord)),
	rowSlots(Row,[],CurrentSlots,Slots).

% when on logical variable(space) it's added to the CurrentWord
rowSlots([Var|Row],CurrentWord,CurrentSlots,Slots) :-
	Var\=='#',
	append(CurrentWord,[Var],NewWord),
	rowSlots(Row,NewWord,CurrentSlots,Slots).


%%%%%%%%%% Predicates that fill the words into the correct slots

% % This predicates holds if both the Slots and remaining wordlists are empty
% % due to the Slots being filled by the words.
fillSlots([],[]).
% The slots are filled in the most efficient way possible by filling the Slot
% with the least number of matches possible FIRST.
fillSlots(Slots,WordList) :-
	% since max number of matches possible is the total number of matches
	length(WordList,MaxMatches),

	% finds the slot with the lowest number of matches in the wordlist
	findBestSlot(Slots,WordList,MaxMatches,_,BestSlot),

	% finding the words that match the Best slot
	wordsThatMatchWithSlot(BestSlot,WordList,CorrWordlist),

	% iteratively fills the Best Slot; goes to next word if current match fails
	fillSlot(BestSlot,WordList,CorrWordlist,RemainingWordlist),

	% removes the slot that was filled
	exclude(==(BestSlot),Slots,RemainingSlots),

	% fills the remaining slots with the remaining words
	fillSlots(RemainingSlots,RemainingWordlist).

% % Predicate holds if the final BestSlot actually has matches else there is
% % no point considering that slot.
findBestSlot([],_,LeastMatches,BestSlot,BestSlot) :-
	LeastMatches>0.
findBestSlot([Slot|Slots],WordList,LeastMatches,CurrBestSlot,BestSlot) :-
	% finds number of words that can fill the current slot
	numOfMatches(Slot,WordList,TotalMatches),

	% if the total number of matches are less than or equal to previous lowest
	% current slot replaces the previous best slot.
	(TotalMatches=<LeastMatches
	->  findBestSlot(Slots,WordList,TotalMatches,Slot,BestSlot)
	% Previous best slot is better so current slot is skipped
	;    findBestSlot(Slots,WordList,LeastMatches,CurrBestSlot,BestSlot)
	).

% % Predicate finds the length of the list of words that match the given slot
numOfMatches(Slot,WordList,TotalMatches) :-
	wordsThatMatchWithSlot(Slot,WordList,ListOfMatches),
	length(ListOfMatches,TotalMatches).

% % Predicate holds if ListOfMatches is the list of words from WordList that
% % match the Slot.
wordsThatMatchWithSlot(Slot,WordList,ListOfMatches) :-
	include(canMatchWithSlot(Slot),WordList,ListOfMatches).

% % Predicate holds if both Slot and Word have the same length and either Each
% % variable equals the corresponding letter in word or current element in slot
% % is a logical variable, which means it can be filled by a character.
canMatchWithSlot([],[]).
canMatchWithSlot([Var|RemVars],[Letter|Word]) :-
	(Var == Letter; var(Var)),
	canMatchWithSlot(RemVars,Word).

% % This predicate fills a logical variable with the guessed word.
fillSlot(Slot,WordList,[Word|CorrWordlist],RemainingWordlist) :-
	% filling the logical variable with the current word
	(Slot=Word,

	% finds the remaining wordlist when slot is filled with current word
	exclude(==(Word),WordList,RemainingWordlist));

	% if current word fails to solve the puzzle the next word needs to fill the
	% slot
	fillSlot(Slot,WordList,CorrWordlist,RemainingWordlist).


%%%%%%%%%% Additional Predicates

% % Checks if the length of the word/list is longer than one letter/element
isLengthGreaterThanOne(Word) :-
	length(Word,WordLength),
	WordLength>1.

% % Joins two lists together into a third list.
append([],X,X).
append([X|Y],Z,[X|W]) :- append(Y,Z,W).
