import random
from typing import List, Optional, Tuple

class Board:
    """Connect Four game board representation"""
    
    def __init__(self, rows: int = 6, cols: int = 7):
        self.rows = rows
        self.cols = cols
        self.board = [[0 for _ in range(cols)] for _ in range(rows)]
        
    def display(self):
        """Display the current board state"""
        symbols = {0: '.', 1: 'X', 2: 'O'}
        for row in self.board:
            print(' '.join(symbols[cell] for cell in row))
        print(' '.join(str(i+1) for i in range(self.cols)))
        print('===========================================')
        
    def is_valid_move(self, col: int) -> bool:
        """Check if a move is valid"""
        return 0 <= col < self.cols and self.board[0][col] == 0
        
    def drop_token(self, col: int, player: int) -> Optional[Tuple[int, int]]:
        """Drop a token in the specified column"""
        if not self.is_valid_move(col):
            return None
            
        # Find the lowest empty position
        for row in range(self.rows-1, -1, -1):
            if self.board[row][col] == 0:
                self.board[row][col] = player
                return (row, col)
        return None
        
    def check_win(self, row: int, col: int, player: int) -> bool:
        """Check if the last move at (row, col) created a winning line"""
        directions = [(0,1), (1,0), (1,1), (1,-1)]  # horizontal, vertical, diagonal
        
        for dr, dc in directions:
            count = 1
            # Check forward direction
            r, c = row + dr, col + dc
            while 0 <= r < self.rows and 0 <= c < self.cols and self.board[r][c] == player:
                count += 1
                r, c = r + dr, c + dc
                
            # Check backward direction
            r, c = row - dr, col - dc
            while 0 <= r < self.rows and 0 <= c < self.cols and self.board[r][c] == player:
                count += 1
                r, c = r - dr, c - dc
                
            if count >= 4:
                return True
        return False
        
    def is_full(self) -> bool:
        """Check if the board is full"""
        return all(cell != 0 for row in self.board for cell in row)

class Player:
    """Base class for players"""
    def __init__(self, number: int, name: str):
        self.number = number
        self.name = name
        
    def get_move(self, board: Board) -> int:
        raise NotImplementedError

class HumanPlayer(Player):
    """Human player implementation"""
    def get_move(self, board: Board) -> int:
        while True:
            try:
                col = int(input(f"{self.name}, choose column (1-{board.cols}): ")) - 1
                if board.is_valid_move(col):
                    return col
                print("Invalid move, try again.")
            except ValueError:
                print("Please enter a valid number.")

class ComputerPlayer(Player):

    # DONE: do your best to make this AI more intelligent and powerful.
        
    def get_move(self, board: Board) -> int:
        valid_cols = [col for col in range(board.cols) if board.is_valid_move(col)]
        if not valid_cols:
            return 0

        # Deepen the search when the board is open, shorten it when close to full to keep speed reasonable.
        remaining_slots = sum(1 for row in board.board for cell in row if cell == 0)
        depth = 4 if remaining_slots > 20 else 5

        score, best_col = self._minimax(self._copy_board(board), depth, float("-inf"), float("inf"), True)
        if best_col is None:
            return random.choice(valid_cols)
        return best_col
    
    def _minimax(self, board: Board, depth: int, alpha: float, beta: float, maximizing: bool) -> Tuple[float, Optional[int]]:
        valid_cols = [col for col in range(board.cols) if board.is_valid_move(col)]
        winner = self._detect_winner(board)

        if depth == 0 or winner != 0 or not valid_cols:
            return self._score_board(board, winner, depth), None

        if maximizing:
            value = float("-inf")
            best_col = None
            for col in self._order_moves(valid_cols, board.cols):
                board_copy = self._copy_board(board)
                row = self._simulate_move(board_copy, col, self.number)
                winner_after = self._detect_winner(board_copy) if row != -1 else 0
                new_score, _ = self._minimax(board_copy, depth - 1, alpha, beta, False)
                # Incentivize immediate win and center control
                if winner_after == self.number:
                    new_score += 1000
                if col == board.cols // 2:
                    new_score += 5

                if new_score > value:
                    value = new_score
                    best_col = col
                alpha = max(alpha, value)
                if alpha >= beta:
                    break
            return value, best_col
        else:
            value = float("inf")
            best_col = None
            opponent = 1 if self.number == 2 else 2
            for col in self._order_moves(valid_cols, board.cols):
                board_copy = self._copy_board(board)
                row = self._simulate_move(board_copy, col, opponent)
                winner_after = self._detect_winner(board_copy) if row != -1 else 0
                new_score, _ = self._minimax(board_copy, depth - 1, alpha, beta, True)
                if winner_after == opponent:
                    new_score -= 1000

                if new_score < value:
                    value = new_score
                    best_col = col
                beta = min(beta, value)
                if alpha >= beta:
                    break
            return value, best_col
    
    def _order_moves(self, valid_cols: List[int], total_cols: int) -> List[int]:
        center = total_cols // 2
        return sorted(valid_cols, key=lambda c: abs(center - c))
        
    def _copy_board(self, board: Board) -> Board:
        """Create a deep copy of the board"""
        new_board = Board(board.rows, board.cols)
        for i in range(board.rows):
            for j in range(board.cols):
                new_board.board[i][j] = board.board[i][j]
        return new_board
        
    def _simulate_move(self, board: Board, col: int, player: int) -> int:
        """Drop a token on a copied board and return the row used"""
        for row in range(board.rows-1, -1, -1):
            if board.board[row][col] == 0:
                board.board[row][col] = player
                return row
        return -1
        
    def _evaluate_move(self, board: Board, row: int, col: int, player: int) -> float:
        """Evaluate a single move in isolation"""
        score = 0
        opponent = 1 if player == 2 else 2
        directions = [(0,1), (1,0), (1,1), (1,-1)]
        
        for dr, dc in directions:
            my_count = 1
            space_after = 0
            space_before = 0
            
            r, c = row + dr, col + dc
            while 0 <= r < board.rows and 0 <= c < board.cols:
                if board.board[r][c] == player:
                    my_count += 1
                elif board.board[r][c] == 0:
                    space_after += 1
                    break
                else:
                    break
                r, c = r + dr, c + dc
            
            r, c = row - dr, col - dc
            while 0 <= r < board.rows and 0 <= c < board.cols:
                if board.board[r][c] == player:
                    my_count += 1
                elif board.board[r][c] == 0:
                    space_before += 1
                    break
                else:
                    break
                r, c = r - dr, c - dc
            
            if my_count >= 4:
                score += 1000
            elif my_count == 3 and (space_before > 0 or space_after > 0):
                score += 50
            elif my_count == 2 and space_before > 0 and space_after > 0:
                score += 10
                
            opp_count = 1
            r, c = row + dr, col + dc
            while 0 <= r < board.rows and 0 <= c < board.cols and board.board[r][c] == opponent:
                opp_count += 1
                r, c = r + dr, c + dc
            
            r, c = row - dr, col - dc
            while 0 <= r < board.rows and 0 <= c < board.cols and board.board[r][c] == opponent:
                opp_count += 1
                r, c = r - dr, c - dc
                
            if opp_count >= 3:
                score += 60
        
        return score

    def _detect_winner(self, board: Board) -> int:
        """Return winning player number or 0 if no winner"""
        for r in range(board.rows):
            for c in range(board.cols):
                player = board.board[r][c]
                if player != 0 and board.check_win(r, c, player):
                    return player
        return 0

    def _score_window(self, window: List[int], player: int) -> int:
        opponent = 1 if player == 2 else 2
        player_count = window.count(player)
        opp_count = window.count(opponent)
        empty = window.count(0)

        if player_count == 4:
            return 100
        if player_count == 3 and empty == 1:
            return 10
        if player_count == 2 and empty == 2:
            return 5
        if opp_count == 3 and empty == 1:
            return -12
        if opp_count == 4:
            return -120
        return 0

    def _score_board(self, board: Board, winner: int, depth: int) -> float:
        if winner == self.number:
            return 100000 + depth
        if winner != 0:
            return -100000 - depth

        score = 0
        player = self.number
        rows, cols = board.rows, board.cols

        # Center control
        center_col = cols // 2
        center_column = [board.board[r][center_col] for r in range(rows)]
        score += center_column.count(player) * 6

        # Horizontal
        for r in range(rows):
            row_array = board.board[r]
            for c in range(cols - 3):
                window = row_array[c:c+4]
                score += self._score_window(window, player)

        # Vertical
        for c in range(cols):
            col_array = [board.board[r][c] for r in range(rows)]
            for r in range(rows - 3):
                window = col_array[r:r+4]
                score += self._score_window(window, player)

        # Positive diagonal
        for r in range(rows - 3):
            for c in range(cols - 3):
                window = [board.board[r+i][c+i] for i in range(4)]
                score += self._score_window(window, player)

        # Negative diagonal
        for r in range(3, rows):
            for c in range(cols - 3):
                window = [board.board[r-i][c+i] for i in range(4)]
                score += self._score_window(window, player)

        # Slight penalty for near-full board to encourage faster wins
        empty_cells = sum(1 for row in board.board for cell in row if cell == 0)
        score -= (42 - empty_cells) * 0.1
        return score

class Game:
    """Connect Four game controller"""
    def __init__(self, player1, player2):
        self.board = Board()
        self.players = [player1, player2]
        self.current_player = 0
        
    def play(self):
        """Main game loop"""
        n = 0
        while True:
            if self.current_player == 0:
                n += 1
                print('round ' + str(n))
            self.board.display()
            player = self.players[self.current_player]
            
            # Get player's move
            col = player.get_move(self.board)
            print(f"{player.name} drop at column {col+1}")
            result = self.board.drop_token(col, player.number)
            
            if result is None:
                print("Invalid move!")
                continue
                
            row, col = result
            
            # Check for win
            if self.board.check_win(row, col, player.number):
                self.board.display()
                print(f"{player.name} wins!")
                break
                
            # Check for draw
            if self.board.is_full():
                self.board.display()
                print("Game is a draw!")
                break
                
            # Switch players
            self.current_player = 1 - self.current_player

if __name__ == "__main__":
    player1 = HumanPlayer(1, "You")
    player2 = ComputerPlayer(2, "Play Kang")
    game = Game(player1, player2)
    game.play()
