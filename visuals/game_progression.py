from PIL import Image, ImageDraw

# https://commons.wikimedia.org/wiki/Category:PNG_chess_pieces/Standard_transparent
pieces_mapping = {
    '♔': './pieces/white_king.png',
    '♕': './pieces/white_queen.png',
    '♖': './pieces/white_rook.png',
    '♗': './pieces/white_bishop.png',
    '♘': './pieces/white_knight.png',
    '♙': './pieces/white_pawn.png',
    '♚': './pieces/black_king.png',
    '♛': './pieces/black_queen.png',
    '♜': './pieces/black_rook.png',
    '♝': './pieces/black_bishop.png',
    '♞': './pieces/black_knight.png',
    '♟︎': './pieces/black_pawn.png',
    '.': './pieces/empty_square.png'
}


def chessboard_to_image(position):
    square_size = 64
    board_size = 8 * square_size

    chessboard = Image.new('RGB', (board_size, board_size), color='white')
    draw = ImageDraw.Draw(chessboard)

    for i in range(8):
        for j in range(8):
            if (i + j) % 2 == 0:
                draw.rectangle([(i * square_size, j * square_size), ((i + 1) * square_size, (j + 1) * square_size)], fill='green')

    for i, row in enumerate(position.split('\n')):
        for j, piece in enumerate(row.split()):
            piece_image = Image.open(pieces_mapping[piece])
            chessboard.paste(piece_image, (j * square_size, i * square_size), mask=piece_image.split()[3] if piece_image.mode == 'RGBA' else None)

    return chessboard

sample_position = '''
♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖
♙ ♙ ♙ ♙ . ♙ ♙ ♙
. . . . ♙ . . .
. . . . . . . .
. . . . . . . .
. . . . . . . .
♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
'''

# chessboard_image = chessboard_to_image(sample_position)
# chessboard_image.save('chessboard_image_with_filled_grid.jpg')

with open('../sample_games/minimax_vs_random.txt', 'r', encoding='utf-8') as file:
    contents = file.read()


boards = contents.split('\n\n')


for i, board in enumerate(boards):
    chessboard_image = chessboard_to_image(board)
    chessboard_image.save(f'./board_states/chessboard_{i + 1}.png')
    if i > 100:
        break