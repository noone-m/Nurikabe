:- use_module(library(pce)).

% Dynamic predicate to store fixed cells
:- dynamic fxdCell/3.
:- dynamic solved_cell/3.
:- dynamic grid_size/2.
% check if the cell belong to the grid
% Custom sleep predicate for fractional seconds

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
    assertz(grid_size(Rows, Cols)),
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

% Update the cell at (I, J) to blue
update_cell_to_blue(I, J) :-
    % Make sure the cell is within the grid and not already solved
    (   within_grid(I, J),\+ solved_cell(I, J, _) ->
    % Retrieve the current canvas and cell size
    current_canvas(Canvas),
    current_cell_size(CellSize),

    % Update the cell to blue
    draw_cell(Canvas, I, J, blue, CellSize),
    assertz(solved_cell(I, J, blue))

    ; true
    ).

solve_nurikabe :-
    findall(_,all_fixed_cells_green,_),
    findall(_, island_of_1, _),
    separate_adjacent_islands,
    separate_diagonally_adjacent_islands,
    mark_surrounded_squares_as_sea,
    expand_isolated_blue_cells.
% Entry point
:- initialization(start_game).


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
adjacent_no_strict(I, J, I, J1) :- J1 is J - 1.  % Left
adjacent_no_strict(I, J, I, J1) :- J1 is J + 1. % Right
adjacent_no_strict(I, J, I1, J) :- I1 is I - 1. % Up
adjacent_no_strict(I, J, I1, J) :- I1 is I + 1.  % Down


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
    retractall(current_cell_size(_)),  % Remove any current cell size facts
    retractall(grid_size(_,_)).
% solutions
%
all_fixed_cells_green :-
    fxdCell(I,J,Number),
    current_canvas(Canvas),
    current_cell_size(CellSize),
    assertz(solved_cell(I, J, green)),
    update_cell(Canvas, I, J, green, CellSize, Number).

island_of_1 :-
    fxdCell(I, J, 1),  % Find fixed cells with number 1

%assertz(solved_cell(I, J, green)),  % Assert the solved cell with blue color
   %current_canvas(Canvas),  % Retrieve the current Canvas from the environment
   % current_cell_size(CellSize),  % Retrieve the current CellSize from the environment
   % update_cell(Canvas, I, J, green, CellSize, 1) ,
    I1 is I + 1,
    update_cell_to_blue(I1,J),
    J1 is J + 1,
    update_cell_to_blue(I,J1),
    I2 is I - 1,
    update_cell_to_blue(I2,J),
    J2 is J - 1,
    update_cell_to_blue(I,J2).

separate_adjacent_islands :-
    % Retrieve all fixed cells (clues)
    findall((I, J, Num), fxdCell(I, J, Num), AllFixedCells),

    % Check each pair of fixed cells to find those separated by one cell in the same row or column
    forall((member((I1, J1, _), AllFixedCells), member((I2, J2, _), AllFixedCells)),
           (
               % Same row, separated by one column
               (I1 =:= I2, J1 =:= J2 - 2 ->
                   Jmid is (J1 + J2) // 2,
                   \+ fxdCell(I1, Jmid, _),  % Ensure the middle cell is not already a fixed cell
                   update_cell_to_blue(I1, Jmid)  % Mark the middle cell as blue
               ; true),

               % Same column, separated by one row
               (J1 =:= J2, I1 =:= I2 - 2 ->
                   Imid is (I1 + I2) // 2,
                   \+ fxdCell(Imid, J1, _),  % Ensure the middle cell is not already a fixed cell
                   update_cell_to_blue(Imid, J1)  % Mark the middle cell as blue
               ; true)
           )
    ).

% Predicate to handle diagonally adjacent clues
separate_diagonally_adjacent_islands :-
    % Retrieve all fixed cells (clues)
    findall((I, J, Num), fxdCell(I, J, Num), AllFixedCells),

    % Check each pair of fixed cells to find those that are diagonally adjacent
    forall((member((I1, J1, _), AllFixedCells), member((I2, J2, _), AllFixedCells)),
           (
               % Diagonally adjacent top-left to bottom-right
               (I2 =:= I1 + 1, J2 =:= J1 + 1 ->
                   update_cell_to_blue(I2,J1),
                   update_cell_to_blue(I1,J2)
               ; true),

               % Diagonally adjacent top-right to bottom-left
               (I2 =:= I1 + 1, J2 =:= J1 - 1 ->

                   update_cell_to_blue(I1,J2),
                   update_cell_to_blue(I2,J1)

               ; true)

           )
    ).


% Check if a cell (I, J) is surrounded by blue cells horizontally and vertically
is_surrounded_by_sea(I, J) :-
    \+ solved_cell(I, J, _),  % Ensure the cell itself is not solved
    \+ fxdCell(I, J, _),      % Ensure the cell is not a fixed cell
    I1 is I + 1,
    I2 is I - 1,
    J1 is J + 1,
    J2 is J - 1,

    % Check the surrounding cells
    % Continue if adjacent cells are out of bounds, but fail if they are not blue
    (adjacent(I, J, I1, J) -> solved_cell(I1, J, blue) ; true), % Down
    (adjacent(I, J, I2, J) -> solved_cell(I2, J, blue) ; true), % Up
    (adjacent(I, J, I, J1) -> solved_cell(I, J1, blue) ; true), % Right
    (adjacent(I, J, I, J2) -> solved_cell(I, J2, blue) ; true). % Left

% Mark all surrounded cells as blue
mark_surrounded_squares_as_sea :-
    % Iterate through each cell in the grid
    grid_size(Rows, Cols),
    forall(
        (between(1, Rows, I), between(1, Cols, J), is_surrounded_by_sea(I, J)),
        (
            update_cell_to_blue(I, J)

        )
    ).







% Count the number of invalid adjacent cells around a cell (I, J)
count_closed_adjacents(I, J, Count) :-
    findall((INew, JNew),
            (

                adjacent_no_strict(I, J, INew, JNew),
                (fxdCell(INew, JNew, _); \+within_grid(INew,JNew))
               ),
            ClosedSurroundnigs),
    length(ClosedSurroundnigs, Count).

% Count the number of valid unsolved cells around a blue cell (I, J)
count_open_adjacents(I, J, Count) :-
    findall((INew, JNew),
            (
                write(INew),
                write(JNew),
                adjacent(I, J, INew, JNew),
                \+ solved_cell(INew, JNew, _),
                write('\n I is '),
                write(INew),
                write('\n J is '),
                write(JNew)
            ),
            OpenSurroundings),
    length(OpenSurroundings, Count).

count_blue_adjacents(I, J, Count) :-
    findall((INew, JNew),
            (
                adjacent(I, J, INew, JNew),
                solved_cell(INew, JNew, blue)
            ),
            OpenSurroundings),
    length(OpenSurroundings, Count).

% Find isolated blue cells that need to be expanded
find_isolated_blue_cells(IsolatedBlueCells) :-
    findall((I, J),
            (
                solved_cell(I, J, blue),  % The cell is blue

                count_closed_adjacents(I, J, ClosedCount),
                ClosedCount =:= 3,       % Exactly three invalid adjacent cells
                count_open_adjacents(I, J, OpenCount),
                OpenCount =:= 1  % Exactly one valid unsolved adjacent cell
            ),
            IsolatedBlueCells).


% Expand isolated blue cells by marking their valid adjacent cells as blue
expand_isolated_blue_cells :-
    find_isolated_blue_cells(IsolatedBlueCells),
    forall(
        member((I, J), IsolatedBlueCells),
        (
            findall((INew, JNew),
                    (
                        adjacent(I, J, INew, JNew),
                        \+ solved_cell(INew, JNew, _),
                        \+ fxdCell(INew, JNew, _)
                    ),
                    [(TargetI, TargetJ)]  % We expect exactly one valid adjacent cell
            ),
            update_cell_to_blue(TargetI, TargetJ),  % Mark the adjacent cell as blue
            expand_more(TargetI,TargetJ)

        )
    ).

would_expand_more(I,J) :-
             count_open_adjacents(I,J,Count),
             count_blue_adjacents(I,J,Count2),
             write('\n'),
             write(Count),
             write(Count2),
             write('\n'),
             Count =:= 1,
             Count2 =:= 1.

expand_more(I,J):-
    would_expand_more(I,J),
    findall((Iop,Jop),
            (adjacent(I,J,Iop,Jop),\+fxdCell(Iop,Jop,_),\+solved_cell(Iop,Jop,_)),
            (OpenCells)),


    forall(member((Iexp,Jexp),OpenCells),
           (
            update_cell_to_blue(Iexp,Jexp),
            expand_more(Iexp,Jexp)
           )
          ).


