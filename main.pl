:- use_module(library(pce)).

% Dynamic predicate to store fixed cells
:- dynamic fxdCell/3.
:- dynamic solved_cell/3.

% check if the cell belong to the grid

within_grid(I, J) :-
    grid_size(Imax, Jmax),
    I > 0, I =< Imax,
    J > 0, J =< Jmax.


% Predicate to start the Nurikabe game with given rows and columns
start_game :-
    delete_all_dynamic_facts,
    new(Window, dialog('Nurikabe Game')),  % Create the main dialog window
    send(Window, size, size(750, 800)),    % Set the size of the window

    % Grid layout for input fields and buttons
    send(Window, append, new(Grid, dialog_group(grid_layout, group))),

    % Add rows and columns fields to the grid
    send(Grid, append, new(RowsField, int_item(rows,7)), right),
    send(Grid, append, new(ColsField, int_item(columns,7)), right),

     % Canvas for drawing the board
    send(Window, append, new(Canvas, picture)),
    send(Canvas, size, size(700, 700)),  % Adjusted size for the canvas

   % Add buttons to the grid
    send(Grid, append, button(draw, message(@prolog, draw_board, RowsField?selection, ColsField?selection, Canvas)), right),
    send(Grid, append, button(solve, message(@prolog, solve_nurikabe)), right),


    % Event handler for applying rows and columns through enter
    send(RowsField, message, message(@prolog, draw_board, RowsField?selection, ColsField?selection, Canvas)),
    send(ColsField, message, message(@prolog, draw_board, RowsField?selection, ColsField?selection, Canvas)),

    % Open the window
    send(Window, open).

% Draw the board with grey cells
draw_board(Rows, Cols, Canvas) :-
    send(Canvas, clear),      % Clear canvas if re-drawing

    % Calculate cell size based on the number of rows and columns
    CellSize is 700 // max(Rows,Cols),
    % Store Canvas and CellSize as current environment
    asserta(current_canvas(Canvas)),
    asserta(current_cell_size(CellSize)),
    % Draw all cells on the board
    forall(between(1, Rows, I),
           forall(between(1, Cols, J),
                  draw_cell(Canvas, I, J, grey, CellSize))),
        % Add mouse click handler for inputting numbers
    send(Canvas, recogniser,
         click_gesture(right, '', single, message(@prolog, cell_right_click, Canvas, Rows, Cols, @event))).

draw_cell(Canvas, I, J, Color, CellSize) :-
    X is (J - 1) * CellSize,
    Y is (I - 1) * CellSize,
    new(Box, box(CellSize, CellSize)),
    send(Box, fill_pattern, colour(Color)),
    send(Box, recogniser, click_gesture(right, '', single, message(@prolog, cell_right_click, Canvas, I, J, CellSize))),
    send(Canvas, display, Box, point(X, Y)).

cell_right_click(Canvas, I, J, CellSize) :-
    % Open a dialog to ask for a number
    new(Dialog, dialog('Enter Number')),
    send(Dialog, append, new(NumberField, int_item('Number'))),
    send(Dialog, append, button(ok, message(@prolog, handle_number_input, Dialog, Canvas, I, J, CellSize, NumberField?selection))),
    send(Dialog, append, button(cancel, message(Dialog, destroy))),
    send(Dialog, open_centered).

% Handle the input number and update the cell
handle_number_input(Dialog, Canvas, I, J, CellSize, Number) :-
    send(Dialog, destroy),  % Close the dialog

    % Update the cell color to white and display the number
    update_cell(Canvas, I, J, white, CellSize, Number),

    % Assert the fixed cell
    assertz(fxdCell(I, J, Number)).

% Update a cell with a specified color and display a number
update_cell(Canvas, I, J, Color, CellSize, Number) :-
    X is (J - 1) * CellSize,
    Y is (I - 1) * CellSize,

    % Draw the cell with the specified color
    new(Box, box(CellSize, CellSize)),
    send(Box, fill_pattern, colour(Color)),
    send(Canvas, display, Box, point(X, Y)),

    % Draw the number in the center of the cell
    MiddleX is X + CellSize // 2 - CellSize // 8,
    MiddleY is Y + CellSize // 2 - CellSize // 5 - 3,
    new(Text, text(string('%s', Number), center)),
    send(Text, font, font(times, bold, 15)),  % Adjust font size as needed
    send(Canvas, display, Text, point(MiddleX, MiddleY)).
% Dummy solve function for demonstration
solve_nurikabe :-
    !.
% Entry point
:- initialization(start_game).


grid_size(7,7).

%fxd_cell(row,column,numbe)
%fxdCell(1,2,3).
%fxdCell(1,6,1).
%fxdCell(3,1,2).
%fxdCell(3,4,1).
%fxdCell(5,2,1).
%fxdCell(5,5,2).
%fxdCell(6,3,2).
%fxdCell(7,1,1).
%fxdCell(7,5,1).
%fxdCell(7,7,6).

%solve_cell(Row,Col,Num).

%solved_cell(1,1,blue).
%solved_cell(1,2,green).
%solved_cell(1,3,green).
%solved_cell(1,4,green).
%solved_cell(1,5,blue).
%solved_cell(1,6,green).
%solved_cell(1,7,blue).

%solved_cell(2,1,blue).
%solved_cell(2,2,blue).
%solved_cell(2,3,blue).
%solved_cell(2,4,blue).
%solved_cell(2,5,blue).
%solved_cell(2,6,blue).
%solved_cell(2,7,blue).

%solved_cell(3,1,green).
%solved_cell(3,2,green).
%solved_cell(3,3,blue).
%solved_cell(3,4,green).
%solved_cell(3,5,blue).
%solved_cell(3,6,green).
%solved_cell(3,7,green).

%solved_cell(4,1,blue).
%solved_cell(4,2,blue).
%solved_cell(4,3,blue).
%solved_cell(4,4,blue).
%solved_cell(4,5,blue).
%solved_cell(4,6,blue).
%solved_cell(4,7,green).

%solved_cell(5,1,blue).
%solved_cell(5,2,green).
%solved_cell(5,3,blue).
%solved_cell(5,4,green).
%solved_cell(5,5,green).
%solved_cell(5,6,blue).
%solved_cell(5,7,green).

%solved_cell(6,1,blue).
%solved_cell(6,2,blue).
%solved_cell(6,3,green).
%solved_cell(6,4,blue).
%solved_cell(6,5,blue).
%solved_cell(6,6,blue).
%solved_cell(6,7,green).

%solved_cell(7,1,green).
%solved_cell(7,2,blue).
%solved_cell(7,3,green).
%solved_cell(7,4,blue).
%solved_cell(7,5,green).
%solved_cell(7,6,blue).
%solved_cell(7,7,green).

% Define adjacent cells
adjacent(I, J, I, J1) :- J1 is J - 1, within_grid(I, J1).  % Left
adjacent(I, J, I, J1) :- J1 is J + 1, within_grid(I, J1). % Right
adjacent(I, J, I1, J) :- I1 is I - 1, within_grid(I1, J). % Up
adjacent(I, J, I1, J) :- I1 is I + 1, within_grid(I1, J).  % Down

% Find adjacent cells of the same color
adjacent_same_color(I, J, I1,J1) :-
    solved_cell(I, J, Color),  % Get the color of the given cell    findall((I1, J1),
            (adjacent(I, J, I1, J1),
             solved_cell(I1, J1, Color)).


% Add a cell to a list if it's not already in it
add_if_not_member(Cell, List, [Cell|List]) :-
    \+ member(Cell, List).
add_if_not_member(_, List, List).

% Recursive search for all connected cells of the same color
color_chain_helper(_, [], Visited, Visited).  % No more cells to visit
color_chain_helper(Color, [(I, J)|ToVisit], Visited, SameColorChain) :-
    findall((I1, J1),
            (adjacent_same_color(I, J, I1, J1),
             \+ member((I1, J1), Visited)),  % Ensure the cell is not visited
            Neighbors),  % Find all unvisited neighbors of the same color
    append(Neighbors, ToVisit, NewToVisit),  % Add them to the to-visit list
    add_if_not_member((I, J), Visited, NewVisited),  % Mark the current cell as visited
    color_chain_helper(Color, NewToVisit, NewVisited, SameColorChain).

% Main predicate to initiate the color chain search
color_chain(I, J, SameColorChain) :-
    solved_cell(I, J, Color),  % Get the color of the starting cell
    color_chain_helper(Color, [(I, J)], [], ReverseChain),
    reverse(ReverseChain, SameColorChain),!.  % Reverse the list to maintain order


% Predicate to find all blue cells
find_all_blue_cells(AllBlueCells) :-
    findall((I, J), (solved_cell(I,J,blue),within_grid(I, J)), AllBlueCells).

one_sea :-
    find_all_blue_cells(AllBlueCells),  % Get all blue cells
    AllBlueCells = [(I, J)|_],  % Get the first blue cell
    color_chain(I, J, SameColorChain),  % Find all connected blue cells starting from (I, J)
    msort(AllBlueCells, SortedAllBlue),  % Sort the list of all blue cells
    msort(SameColorChain, SortedChain),  % Sort the list of connected blue cells
    SortedAllBlue == SortedChain.  % Ensure both sorted lists are equal

% Predicate to check if a cell and its adjacent cells form a 2x2 blue block
is_blue_block(I, J) :-
    I1 is I + 1,
    J1 is J + 1,
    solved_cell(I, J, blue),
    solved_cell(I, J1, blue),
    solved_cell(I1, J, blue),
    solved_cell(I1, J1, blue).

% Predicate to check if there are no 2x2 blue blocks in the grid
no_2_by_2_sea :-
    grid_size(Imax, Jmax),
    \+ (
        between(1, Imax, I),
        between(1, Jmax, J),
        within_grid(I, J),
        within_grid(I+1, J),
        within_grid(I, J+1),
        within_grid(I+1, J+1),
        is_blue_block(I, J)
    ).


one_fixed_cell_in_island :-
    findall((I, J), fxdCell(I, J, _), AllFixedCells),
    forall(
        member((I, J), AllFixedCells),
        (
            color_chain(I, J, Island),
            findall((Ix, Jx), (
                member((Ix, Jx), Island),
                fxdCell(Ix, Jx, _)
            ), FixedCellsInIsland),
             length(FixedCellsInIsland, 1)
        )
    ).

island_number_equals_size :-

    findall((I, J, Num), fxdCell(I, J, Num), AllFixedCells),
     forall(
        member((I, J, Num), AllFixedCells),
        (
            color_chain(I, J, Island),
              length(Island, Size),
            Size =:= Num
        )
    ).

solved:- one_sea,
 no_2_by_2_sea,
 one_fixed_cell_in_island,
 island_number_equals_size.

% Predicate to delete all dynamic facts
delete_all_dynamic_facts :-
    retractall(fxdCell(_, _, _)),      % Remove all fxdCell/3 facts
    retractall(solved_cell(_, _, _)),
    retractall(current_canvas(_)),     % Remove any current canvas facts
    retractall(current_cell_size(_)).  % Remove any current cell size facts
% solutions
%
island_of_1 :-
    fxdCell(I, J, 1),  % Find fixed cells with number 1

    write('1'),

    assertz(solved_cell(I, J, green)),  % Assert the solved cell with blue color
    current_canvas(Canvas),  % Retrieve the current Canvas from the environment
    current_cell_size(CellSize),  % Retrieve the current CellSize from the environment

    % Conditional drawing and asserting for adjacent cells
    ( I1 is I + 1, within_grid(I1, J) ->
        draw_cell(Canvas, I1, J, blue, CellSize),
        assertz(solved_cell(I1, J, blue))
    ; true
    ),

    ( J1 is J + 1, within_grid(I, J1) ->
        draw_cell(Canvas, I, J1, blue, CellSize),
        assertz(solved_cell(I, J1, blue))
    ; true
    ),

    ( I2 is I - 1, within_grid(I2, J) ->
        draw_cell(Canvas, I2, J, blue, CellSize),
        assertz(solved_cell(I2, J, blue))
    ; true
    ),

    ( J2 is J - 1, within_grid(I, J2) ->
        draw_cell(Canvas, I, J2, blue, CellSize),
        assertz(solved_cell(I, J2, blue))
    ; true
    ).

