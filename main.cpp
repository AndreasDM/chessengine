#include <cstdint>
#include <iostream>
#include <iomanip>
#include <bitset>
#include <array>
#include <string>
#include <map>
#include <vector>
#include <type_traits>
#include <array>
#include <fmt/core.h>
#include <benchmark/benchmark.h>
#include <numeric>
// #define TESTS 1
// #define BENCH 1

#ifdef TESTS
#define CATCH_CONFIG_MAIN
#include <catch2/catch.hpp>
#endif

std::map<std::string, int> move_key
{
    {"a8", 0}, {"b8", 1}, {"c8", 2}, {"d8", 3}, {"e8", 4}, {"f8", 5}, {"g8", 6}, {"h8", 7},
    {"a7", 8}, {"b7", 9}, {"c7",10}, {"d7",11}, {"e7",12}, {"f7",13}, {"g7",14}, {"h7",15},
    {"a6",16}, {"b6",17}, {"c6",18}, {"d6",19}, {"e6",20}, {"f6",21}, {"g6",22}, {"h6",23},
    {"a5",24}, {"b5",25}, {"c5",26}, {"d5",27}, {"e5",28}, {"f5",29}, {"g5",30}, {"h5",31},
    {"a4",32}, {"b4",33}, {"c4",34}, {"d4",35}, {"e4",36}, {"f4",37}, {"g4",38}, {"h4",39},
    {"a3",40}, {"b3",41}, {"c3",42}, {"d3",43}, {"e3",44}, {"f3",45}, {"g3",46}, {"h3",47},
    {"a2",48}, {"b2",49}, {"c2",50}, {"d2",51}, {"e2",52}, {"f2",53}, {"g2",54}, {"h2",55},
    {"a1",56}, {"b1",57}, {"c1",58}, {"d1",59}, {"e1",60}, {"f1",61}, {"g1",62}, {"h1",63},
};

// Board orientation
//   01234567
//
// A 00000000
// B 00000000
// C 00000000
// D 00000000
// E 00000000
// F 00000000
// G 00000000
// H 00000000
enum enum_square {
    a1, a2, a3, a4, a5, a6, a7, a8,
    b1, b2, b3, b4, b5, b6, b7, b8,
    c1, c2, c3, c4, c5, c6, c7, c8,
    d1, d2, d3, d4, d5, d6, d7, d8,
    e1, e2, e3, e4, e5, e6, e7, e8,
    f1, f2, f3, f4, f5, f6, f7, f8,
    g1, g2, g3, g4, g5, g6, g7, g8,
    h1, h2, h3, h4, h5, h6, h7, h8
};

using u64 = uint64_t;

constexpr u64 seventh_rank = 0x4040404040404040;
constexpr u64 second_rank  = 0x202020202020202;

inline u64 lsb (u64 n)
{
    return n & (n-1);
}

inline u64 bitscanF (u64 n)
{
    return __builtin_ctzll(n);
}

#ifdef DEBUG
const auto print = [](u64 b) {
    for (int i = 0; i != 64; ++i) {
        if (i != 0 && i % 8 == 0)
            std::cout << '\n';
        std::cout << (((b) >> (uint64_t)i) & 1UL) << ' ';
    }
    std::cout << '\n';
    std::cout << '\n';
};
#endif

enum class Turn {
    white,
    black
};

constexpr u64 pawn_moves[2][64] = {{0x200,
                                    0x400,
                                    0x800,
                                    0x1000,
                                    0x2000,
                                    0x4000,
                                    0x8000,
                                    0x0,
                                    0x20002,
                                    0x40004,
                                    0x80008,
                                    0x100010,
                                    0x200020,
                                    0x400040,
                                    0x800080,
                                    0x0,
                                    0x2000200,
                                    0x4000400,
                                    0x8000800,
                                    0x10001000,
                                    0x20002000,
                                    0x40004000,
                                    0x80008000,
                                    0x0,
                                    0x200020000,
                                    0x400040000,
                                    0x800080000,
                                    0x1000100000,
                                    0x2000200000,
                                    0x4000400000,
                                    0x8000800000,
                                    0x0,
                                    0x20002000000,
                                    0x40004000000,
                                    0x80008000000,
                                    0x100010000000,
                                    0x200020000000,
                                    0x400040000000,
                                    0x800080000000,
                                    0x0,
                                    0x2000200000000,
                                    0x4000400000000,
                                    0x8000800000000,
                                    0x10001000000000,
                                    0x20002000000000,
                                    0x40004000000000,
                                    0x80008000000000,
                                    0x0,
                                    0x200020000000000,
                                    0x400040000000000,
                                    0x800080000000000,
                                    0x1000100000000000,
                                    0x2000200000000000,
                                    0x4000400000000000,
                                    0x8000800000000000,
                                    0x0,
                                    0x2000000000000,
                                    0x4000000000000,
                                    0x8000000000000,
                                    0x10000000000000,
                                    0x20000000000000,
                                    0x40000000000000,
                                    0x80000000000000,
                                    0x0},
                                   {0x0,
                                    0x100,
                                    0x200,
                                    0x400,
                                    0x800,
                                    0x1000,
                                    0x2000,
                                    0x4000,
                                    0x0,
                                    0x10001,
                                    0x20002,
                                    0x40004,
                                    0x80008,
                                    0x100010,
                                    0x200020,
                                    0x400040,
                                    0x0,
                                    0x1000100,
                                    0x2000200,
                                    0x4000400,
                                    0x8000800,
                                    0x10001000,
                                    0x20002000,
                                    0x40004000,
                                    0x0,
                                    0x100010000,
                                    0x200020000,
                                    0x400040000,
                                    0x800080000,
                                    0x1000100000,
                                    0x2000200000,
                                    0x4000400000,
                                    0x0,
                                    0x10001000000,
                                    0x20002000000,
                                    0x40004000000,
                                    0x80008000000,
                                    0x100010000000,
                                    0x200020000000,
                                    0x400040000000,
                                    0x0,
                                    0x1000100000000,
                                    0x2000200000000,
                                    0x4000400000000,
                                    0x8000800000000,
                                    0x10001000000000,
                                    0x20002000000000,
                                    0x40004000000000,
                                    0x0,
                                    0x100010000000000,
                                    0x200020000000000,
                                    0x400040000000000,
                                    0x800080000000000,
                                    0x1000100000000000,
                                    0x2000200000000000,
                                    0x4000400000000000,
                                    0x0,
                                    0x1000000000000,
                                    0x2000000000000,
                                    0x4000000000000,
                                    0x8000000000000,
                                    0x10000000000000,
                                    0x20000000000000,
                                    0x40000000000000}

};

constexpr u64 king_moves[64] = {0x302,
                                0x705,
                                0xe0a,
                                0x1c14,
                                0x3828,
                                0x7050,
                                0xe0a0,
                                0xc040,
                                0x30203,
                                0x70507,
                                0xe0a0e,
                                0x1c141c,
                                0x382838,
                                0x705070,
                                0xe0a0e0,
                                0xc040c0,
                                0x3020300,
                                0x7050700,
                                0xe0a0e00,
                                0x1c141c00,
                                0x38283800,
                                0x70507000,
                                0xe0a0e000,
                                0xc040c000,
                                0x302030000,
                                0x705070000,
                                0xe0a0e0000,
                                0x1c141c0000,
                                0x3828380000,
                                0x7050700000,
                                0xe0a0e00000,
                                0xc040c00000,
                                0x30203000000,
                                0x70507000000,
                                0xe0a0e000000,
                                0x1c141c000000,
                                0x382838000000,
                                0x705070000000,
                                0xe0a0e0000000,
                                0xc040c0000000,
                                0x3020300000000,
                                0x7050700000000,
                                0xe0a0e00000000,
                                0x1c141c00000000,
                                0x38283800000000,
                                0x70507000000000,
                                0xe0a0e000000000,
                                0xc040c000000000,
                                0x302030000000000,
                                0x705070000000000,
                                0xe0a0e0000000000,
                                0x1c141c0000000000,
                                0x3828380000000000,
                                0x7050700000000000,
                                0xe0a0e00000000000,
                                0xc040c00000000000,
                                0x203000000000000,
                                0x507000000000000,
                                0xa0e000000000000,
                                0x141c000000000000,
                                0x2838000000000000,
                                0x5070000000000000,
                                0xa0e0000000000000,
                                0x40c0000000000000};

constexpr u64 knight_moves[64] = {0x20400,
                                  0x50800,
                                  0xa1100,
                                  0x142200,
                                  0x284400,
                                  0x508800,
                                  0xa01000,
                                  0x402000,
                                  0x2040004,
                                  0x5080008,
                                  0xa110011,
                                  0x14220022,
                                  0x28440044,
                                  0x50880088,
                                  0xa0100010,
                                  0x40200020,
                                  0x204000402,
                                  0x508000805,
                                  0xa1100110a,
                                  0x1422002214,
                                  0x2844004428,
                                  0x5088008850,
                                  0xa0100010a0,
                                  0x4020002040,
                                  0x20400040200,
                                  0x50800080500,
                                  0xa1100110a00,
                                  0x142200221400,
                                  0x284400442800,
                                  0x508800885000,
                                  0xa0100010a000,
                                  0x402000204000,
                                  0x2040004020000,
                                  0x5080008050000,
                                  0xa1100110a0000,
                                  0x14220022140000,
                                  0x28440044280000,
                                  0x50880088500000,
                                  0xa0100010a00000,
                                  0x40200020400000,
                                  0x204000402000000,
                                  0x508000805000000,
                                  0xa1100110a000000,
                                  0x1422002214000000,
                                  0x2844004428000000,
                                  0x5088008850000000,
                                  0xa0100010a0000000,
                                  0x4020002040000000,
                                  0x400040200000000,
                                  0x800080500000000,
                                  0x1100110a00000000,
                                  0x2200221400000000,
                                  0x4400442800000000,
                                  0x8800885000000000,
                                  0x100010a000000000,
                                  0x2000204000000000,
                                  0x4020000000000,
                                  0x8050000000000,
                                  0x110a0000000000,
                                  0x22140000000000,
                                  0x44280000000000,
                                  0x88500000000000,
                                  0x10a00000000000,
                                  0x20400000000000};

struct Board {
    u64 white_pawns{};
    u64 white_knights{};
    u64 white_rooks{};
    u64 white_queens{};
    u64 white_king{};
    u64 white_bishops{};

    u64 black_pawns{};
    u64 black_knights{};
    u64 black_rooks{};
    u64 black_queens{};
    u64 black_king{};
    u64 black_bishops{};

    Turn turn = Turn::white;
    u64 en_passant{};
    char castling{};
};

constexpr int print_mapping[64] {
    a8, b8, c8, d8, e8, f8, g8, h8,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a1, b1, c1, d1, e1, f1, g1, h1
};

constexpr inline u64 rank_mask(u64 sq)
{
    return u64(0x0101010101010101) << (sq & 7UL);
}

constexpr inline u64 file_mask(u64 sq)
{
    return u64(0xff) << (sq & 56UL);
}

constexpr inline u64 diag_mask(u64 sq)
{
    auto main_diag = 0x8040201008040201;
    int shift = (sq & 7) - (sq >> 3);

    return shift >= 0 ? main_diag >> shift * 8UL : main_diag << (-shift) * 8UL;
}

constexpr inline u64 antidiag_mask(u64 sq)
{
    u64 anti_diag = 0x102040810204080;
    int shift = 7 - (sq & 7) - (sq >> 3);

    return shift >= 0 ? anti_diag >> shift * 8UL : anti_diag << (-shift) * 8UL;
}

constexpr inline u64 bishop_attacks(u64 sq)
{
    u64 attack = diag_mask(sq) | antidiag_mask(sq);
    attack &= ~(1UL << sq);
    return attack;
}

constexpr inline u64 rook_attacks(u64 sq)
{
    u64 attack = rank_mask(sq) | file_mask(sq);
    attack &= ~(1UL << sq);
    return attack;
}

constexpr inline u64 queen_attacks(u64 sq)
{
    return rook_attacks(sq) | bishop_attacks(sq);
}

auto reverse_bits = [](u64 b, u64 sq) {
    u64 d = (b >> 8 * (sq >> 3));
    d = (d & 0b1111'0000) >> 4 | (d & 0b0000'1111) << 4;
    d = (d & 0b1100'1100) >> 2 | (d & 0b0011'0011) << 2;
    d = (d & 0b1010'1010) >> 1 | (d & 0b0101'0101) << 1;
    return (d << 8 * (sq >> 3));
};

constexpr inline u64 north_east(u64 square)
{
    u64 attack  = diag_mask(square);
    u64 piece   = 1UL << square;
    u64 ne_mask = (piece - 1UL);
    u64 ray     = (attack & ne_mask) | piece;
    return ~(ray) & attack;
}

constexpr inline u64 south_west(u64 square)
{
    u64 attack  = diag_mask(square);
    u64 piece   = 1UL << square;
    u64 ne_mask = piece - 1UL;
    u64 bar     = (attack & ne_mask);
    return bar;
}

constexpr inline u64 north_west(u64 square)
{
    u64 attack  = antidiag_mask(square);
    u64 piece   = 1UL << square;
    u64 ne_mask = piece - 1UL;
    return ne_mask & attack;
}

constexpr inline u64 south_east(u64 square)
{
    u64 attack  = antidiag_mask(square);
    u64 piece   = 1UL << square;
    u64 ne_mask = (piece - 1UL) | piece;
    return ~(ne_mask) & attack;
}

constexpr inline u64 north(u64 square)
{
    u64 attack = file_mask(square);
    u64 piece  = 1UL << square;
    u64 ne_mask = (piece - 1UL) | piece;
    return ~(ne_mask) & attack;
}

constexpr inline u64 south(u64 square)
{
    u64 attack  = file_mask(square);
    u64 piece   = 1UL << square;
    u64 ne_mask = (piece - 1UL);
    return ne_mask & attack;
}

constexpr inline u64 west(u64 square)
{
    u64 attack = rank_mask(square);
    u64 piece  = 1UL << square;
    u64 ne_mask = (piece - 1UL);
    return ne_mask & attack;
}

constexpr inline u64 east(u64 square)
{
    u64 attack = rank_mask(square);
    u64 piece  = 1UL << square;
    u64 ne_mask = (piece - 1UL) | piece;
    return ~(ne_mask) & attack;
}

/// Diagonal ray attacks
///-----------
/// mask out the north west obstacles
/// find index of the first obstacle from ex. g2-a8
/// create north west attack ray from first obstacle
/// north west attack ray from g2-a8 ^ north west attack ray from first obstacle
constexpr inline u64 attack_ray_north_west(u64 square, u64 obstacles)
{
    if (obstacles & north_west(square))
        return (north_west(square) ^ north_west(63 - __builtin_clzll(obstacles & north_west(square)))) & north_west(square);
    return north_west(square);
}

constexpr inline u64 attack_ray_south_east(u64 square, u64 obstacles)
{
    if (obstacles & south_east(square))
        return (south_east(square) ^ south_east(__builtin_ctzll(obstacles & south_east(square)))) & south_east(square);
    return south_east(square);
}

constexpr inline u64 attack_ray_north_east(u64 square, u64 obstacles)
{
    if (obstacles & north_east(square))
        return (north_east(square) ^ north_east(__builtin_ctzll(obstacles & north_east(square)))) & north_east(square);
    return north_east(square);
}

constexpr inline u64 attack_ray_south_west(u64 square, u64 obstacles)
{
    if (obstacles & south_west(square))
        return (south_west(square) ^ south_west(63 - __builtin_clzll(obstacles & south_west(square)))) & south_west(square);
    return south_west(square);
}

constexpr inline u64 attack_ray_north(u64 square, u64 obstacles)
{
    if (obstacles & north(square))
        return (north(square) ^ north(__builtin_ctzll(obstacles & north(square)))) & north(square);
    return north(square);
}

constexpr inline u64 attack_ray_south(u64 square, u64 obstacles)
{
    if (obstacles & south(square))
        return (south(square) ^ south(63 - __builtin_clzll(obstacles & south(square)))) & south(square);
    return south(square);
}

constexpr inline u64 attack_ray_east(u64 square, u64 obstacles)
{
    if (obstacles & east(square))
        return (east(square) ^ east(__builtin_ctzll(obstacles & east(square)))) & east(square);
    return east(square);
}

constexpr inline u64 attack_ray_west(u64 square, u64 obstacles)
{
    if (obstacles & west(square))
        return (west(square) ^ west(63 - __builtin_clzll(obstacles & west(square)))) & west(square);
    return west(square);
}

constexpr inline u64 white_pieces(const Board & board)
{
    return board.white_pawns
        |  board.white_knights
        |  board.white_bishops
        |  board.white_rooks
        |  board.white_queens
        |  board.white_king;
}

constexpr inline u64 black_pieces(const Board & board)
{
    return board.black_pawns
        |  board.black_knights
        |  board.black_bishops
        |  board.black_rooks
        |  board.black_queens
        |  board.black_king;
}

constexpr inline u64 all_pieces(const Board & board)
{
    return white_pieces(board) | black_pieces(board);
}


constexpr inline u64 filter_attack_rays(u64 rays, const Board & board, Turn to_move)
{
    return to_move == Turn::white ? rays & ~white_pieces(board)
                                  : rays & ~black_pieces(board);
}

constexpr inline u64 diagonal_attack_rays(u64 square, const Board & board)
{
    const auto obs = all_pieces(board);
    return attack_ray_north_west(square, obs) |
           attack_ray_north_east(square, obs) |
           attack_ray_south_west(square, obs) |
           attack_ray_south_east(square, obs);
}

constexpr inline u64 horizontal_attack_rays(u64 square, const Board & board)
{
    const auto obs = all_pieces(board);
    return attack_ray_east(square, obs) | attack_ray_west(square, obs);
}

constexpr inline u64 vertical_attack_rays(u64 square, const Board & board)
{
    const auto obs = all_pieces(board);
    // obs &= ~(square);
    return attack_ray_north(square, obs) | attack_ray_south(square, obs);
}

constexpr inline u64 attack_rays_rook(u64 square, const Board & board)
{
    return horizontal_attack_rays(square, board) |
           vertical_attack_rays(square, board);
}

constexpr inline u64 attack_rays_queen(u64 square, const Board & board)
{
    return attack_rays_rook(square, board) |
           diagonal_attack_rays(square, board);
}

constexpr auto start_position()
{
    Board board;
    board.white_pawns   = 1UL << a2 | 1UL << b2 | 1UL << c2 | 1UL << d2
                        | 1UL << e2 | 1UL << f2 | 1UL << g2 | 1UL << h2;
    board.white_rooks   = 1UL << a1 | 1UL << h1;
    board.white_bishops = 1UL << c1 | 1UL << f1;
    board.white_knights = 1UL << b1 | 1UL << g1;
    board.white_king    = 1UL << e1;
    board.white_queens  = 1UL << d1;

    board.black_pawns   = 1UL << a7 | 1UL << b7 | 1UL << c7 | 1UL << d7
                        | 1UL << e7 | 1UL << f7 | 1UL << g7 | 1UL << h7;
    board.black_rooks   = 1UL << a8 | 1UL << h8;
    board.black_bishops = 1UL << c8 | 1UL << f8;
    board.black_knights = 1UL << b8 | 1UL << g8;
    board.black_king    = 1UL << e8;
    board.black_queens  = 1UL << d8;

    board.castling = 1 << 0 | 1 << 1 | 1 << 2 | 1 << 3;

    return board;
}

enum Promotion {
    Promo_none,
    Promo_queen,
    Promo_rook,
    Promo_bishop,
    Promo_knight
};

struct Move {
    int from{};
    int to{};
    int promote{};
};

enum Piece_type {
    None, Pawn, Knight, Bishop, Rook, Queen, King
};

const std::string board_str[2][7] = {
    {// White
        // None
        " ",
        // Pawn
        "♙",
        // Knight
        "♘",
        // Bishop
        "♗",
        // Rook
        "♖",
        // Queen
        "♕",
        // King
        "♔"
    },
    {// Black
        // None
        " ",
        // Pawn
        "♟",
        // Knight
        "♞",
        // Bishop
        "♝",
        // Rook
        "♜",
        // Queen
        "♛",
        // King
        "♚"
    }
};


constexpr auto piece_type_on_idx(const Board & board, int idx) noexcept
{
    if (board.white_pawns & (1UL << idx) ||
        board.black_pawns & (1UL << idx))
        return Pawn;
    if (board.white_knights & (1UL << idx) ||
        board.black_knights & (1UL << idx))
        return Knight;
    if (board.white_bishops & (1UL << idx) ||
        board.black_bishops & (1UL << idx))
        return Bishop;
    if (board.white_rooks & (1UL << idx) ||
        board.black_rooks & (1UL << idx))
        return Rook;
    if (board.white_queens & (1UL << idx) ||
        board.black_queens & (1UL << idx))
        return Queen;
    if (board.white_king & (1UL << idx) ||
        board.black_king & (1UL << idx))
        return King;

    return None;
}

auto print_board(Board board)
{
    auto pai = [board](int idx) {
        if (piece_type_on_idx(board, print_mapping[idx]) == None)
            return board_str[0][None];

        if (piece_type_on_idx(board, print_mapping[idx]) == Pawn) {
            if (board.white_pawns & (1UL << print_mapping[idx]))
                return board_str[0][Pawn];
            if (board.black_pawns & (1UL << print_mapping[idx]))
                return board_str[1][Pawn];
        }
        if (piece_type_on_idx(board, print_mapping[idx]) == Knight) {
            if (board.white_knights & (1UL << print_mapping[idx]))
                return board_str[0][Knight];
            if (board.black_knights & (1UL << print_mapping[idx]))
                return board_str[1][Knight];
        }
        if (piece_type_on_idx(board, print_mapping[idx]) == Bishop) {
            if (board.white_bishops & (1UL << print_mapping[idx]))
                return board_str[0][Bishop];
            if (board.black_bishops & (1UL << print_mapping[idx]))
                return board_str[1][Bishop];
        }
        if (piece_type_on_idx(board, print_mapping[idx]) == Rook) {
            if (board.white_rooks & (1UL << print_mapping[idx]))
                return board_str[0][Rook];
            if (board.black_rooks & (1UL << print_mapping[idx]))
                return board_str[1][Rook];
        }
        if (piece_type_on_idx(board, print_mapping[idx]) == Queen) {
            if (board.white_queens & (1UL << print_mapping[idx]))
                return board_str[0][Queen];
            if (board.black_queens & (1UL << print_mapping[idx]))
                return board_str[1][Queen];
        }
        if (piece_type_on_idx(board, print_mapping[idx]) == King) {
            if (board.white_king & (1UL << print_mapping[idx]))
                return board_str[0][King];
            if (board.black_king & (1UL << print_mapping[idx]))
                return board_str[1][King];
        }
        return std::string{};
    };

    fmt::print(
R"(
+---+---+---+---+---+---+---+---+
| {} | {} | {} | {} | {} | {} | {} | {} |
+---+---+---+---+---+---+---+---+
| {} | {} | {} | {} | {} | {} | {} | {} |
+---+---+---+---+---+---+---+---+
| {} | {} | {} | {} | {} | {} | {} | {} |
+---+---+---+---+---+---+---+---+
| {} | {} | {} | {} | {} | {} | {} | {} |
+---+---+---+---+---+---+---+---+
| {} | {} | {} | {} | {} | {} | {} | {} |
+---+---+---+---+---+---+---+---+
| {} | {} | {} | {} | {} | {} | {} | {} |
+---+---+---+---+---+---+---+---+
| {} | {} | {} | {} | {} | {} | {} | {} |
+---+---+---+---+---+---+---+---+
| {} | {} | {} | {} | {} | {} | {} | {} |
+---+---+---+---+---+---+---+---+

)", pai(0), pai(1), pai(2), pai(3), pai(4), pai(5), pai(6), pai(7),
    pai(8), pai(9), pai(10), pai(11), pai(12), pai(13), pai(14), pai(15),
    pai(16), pai(17), pai(18), pai(19), pai(20), pai(21), pai(22), pai(23),
    pai(24), pai(25), pai(26), pai(27), pai(28), pai(29), pai(30), pai(31),
    pai(32), pai(33), pai(34), pai(35), pai(36), pai(37), pai(38), pai(39),
    pai(40), pai(41), pai(42), pai(43), pai(44), pai(45), pai(46), pai(47),
    pai(48), pai(49), pai(50), pai(51), pai(52), pai(53), pai(54), pai(55),
    pai(56), pai(57), pai(58), pai(59), pai(60), pai(61), pai(62), pai(63));
}

constexpr inline u64 empty_b(const Board & board)
{
    return ~all_pieces(board);
}

constexpr inline u64 north_one(u64 board)
{
    return (board << 1) & ~0x101010101010101;
}

constexpr inline u64 south_one(u64 board)
{
    return (board >> 1) & ~0x8080808080808080;
}

constexpr inline u64 b_single_push(const Board & board)
{
    return south_one(board.black_pawns) & empty_b(board);
}

constexpr inline u64 b_pawns_able_to_push(const Board & board)
{
    return b_single_push(board) << 1;
}

constexpr inline u64 w_single_push(const Board & board)
{
    return north_one(board.white_pawns) & empty_b(board);
}

constexpr inline u64 w_pawns_able_to_push(const Board & board)
{
    return w_single_push(board) >> 1;
}

constexpr inline u64 w_pawns_able_to_double_push(const Board & board)
{
    const u64 empty_rank3 = south_one(empty_b(board) & 0x808080808080808) & empty_b(board);
    return w_pawns_able_to_push(board) & (empty_rank3 >> 1);
}

constexpr inline u64 b_pawns_able_to_double_push(const Board & board)
{
    const u64 empty_rank6 = north_one(empty_b(board) & 0x1010101010101010) & empty_b(board);
    return b_pawns_able_to_push(board) & (empty_rank6 << 1);
}

// Can white castle kingside?
constexpr inline bool w_can_castle_king_side(const Board & board)
{
    return board.castling & (1 << 0);
}

constexpr inline bool w_can_castle_queen_side(const Board & board)
{
    return board.castling & (1 << 1);
}

constexpr inline bool b_can_castle_king_side(const Board & board)
{
    return board.castling & (1 << 2);
}

constexpr inline bool b_can_castle_queen_side(const Board & board)
{
    return board.castling & (1 << 3);
}

// Doesn't actually check if there exists a piece on the starting square
constexpr inline bool is_pseudo_legal( Piece_type piece_type
                    , const Move & move
                    , const Board & board
                    ) noexcept
{
    // If white turn to move and we try to move a black piece, return false
    if (board.turn == Turn::white &&
        !(white_pieces(board) & (1UL << move.from)))
        return false;
    if (board.turn == Turn::black &&
        !(black_pieces(board) & (1UL << move.from)))
        return false;

    if (piece_type == Queen)
        return filter_attack_rays(attack_rays_queen(move.from, board), board, board.turn)
            & (1UL << move.to);
    if (piece_type == Rook)
        return filter_attack_rays(attack_rays_rook(move.from, board), board, board.turn)
            & (1UL << move.to);
    if (piece_type == Bishop)
        return filter_attack_rays(diagonal_attack_rays(move.from, board), board, board.turn)
            & (1UL << move.to);
    if (piece_type == Knight)
        return filter_attack_rays(knight_moves[move.from], board, board.turn)
            & (1UL << move.to);
    if (piece_type == King) {
        // If the move is a castling move, check if we can castle.
        if (move.from == e1 && w_can_castle_king_side(board)) {
            if (move.to == g1)
                return empty_b(board) & ((1UL << f1) | (1UL << g1));
        }
        if (move.from == e1 && w_can_castle_queen_side(board)) {
            if (move.to == c1)
                return empty_b(board) & ((1UL << d1) | (1UL << c1) | (1UL << b1));
        }
        if (move.from == e8 && b_can_castle_king_side(board)) {
            if (move.to == g8)
                return empty_b(board) & ((1UL << f8) | (1UL << g8));
        }
        if (move.from == e8 && b_can_castle_queen_side(board)) {
            if (move.to == c8)
                return empty_b(board) & ((1UL << d8) | (1UL << c8) | (1UL << b8));
        }

        return filter_attack_rays(king_moves[move.from], board, board.turn)
            & (1UL << move.to);
    }
    if (piece_type == Pawn) {
        if (board.turn == Turn::white) {
            // pawn pushes
            if (((1UL << move.from) << 1) == (1UL << move.to)) {
                return w_pawns_able_to_push(board) & (1UL << move.from);
            }
            if (((1UL << move.from) << 2) == (1UL << move.to)) {
                return w_pawns_able_to_double_push(board) & (1UL << move.from);
            }
            // pawn attacks
            // if we can capture a piece
            if (pawn_moves[0][move.from] & (black_pieces(board) ^ board.en_passant) & (1UL << move.to))
                return true;
        }
        if (board.turn == Turn::black) {
            // pawn pushes
            if (((1UL << move.from) >> 1) == (1UL << move.to))
                return b_pawns_able_to_push(board) & (1UL << move.from);
            if (((1UL << move.from) >> 2) == (1UL << move.to))
                return b_pawns_able_to_double_push(board) & (1UL << move.from);

            if (pawn_moves[1][move.from] & (white_pieces(board) ^ board.en_passant) & (1UL << move.to))
                return true;
        }

    }

    return false;
}

constexpr inline auto clear_white_pawn(Board & board, u64 bit) noexcept
{
    board.white_pawns = board.white_pawns &~ bit;
}

constexpr inline auto clear_black_pawn(Board & board, u64 bit) noexcept
{
    board.black_pawns = board.black_pawns &~ bit;
}

constexpr inline auto clear_square(Board board, Piece_type type, int idx) noexcept
{
    if (type == None)
        return board;

    if (type == Pawn) {
        if (board.turn == Turn::white)
            board.white_pawns = board.white_pawns &~ (1UL << idx);
        else
            board.black_pawns = board.black_pawns &~ (1UL << idx);
        return board;
    }
    if (type == Knight) {
        if (board.turn == Turn::white)
            board.white_knights = board.white_knights &~ (1UL << idx);
        else
            board.black_knights = board.black_knights &~ (1UL << idx);
        return board;
    }
    if (type == Bishop) {
        if (board.turn == Turn::white)
            board.white_bishops = board.white_bishops &~ (1UL << idx);
        else
            board.black_bishops = board.black_bishops &~ (1UL << idx);
        return board;
    }
    if (type == Rook) {
        if (board.turn == Turn::white)
            board.white_rooks = board.white_rooks &~ (1UL << idx);
        else
            board.black_rooks = board.black_rooks &~ (1UL << idx);
        return board;
    }
    if (type == King) {
        if (board.turn == Turn::white)
            board.white_king = board.white_king &~ (1UL << idx);
        else
            board.black_king = board.black_king &~ (1UL << idx);
        return board;
    }
    if (type == Queen) {
        if (board.turn == Turn::white)
            board.white_queens = board.white_queens &~ (1UL << idx);
        else
            board.black_queens = board.black_queens &~ (1UL << idx);
        return board;
    }

    return board;
}

constexpr inline auto set_square(Board board, Piece_type type, int idx) noexcept
{
    if (type == None)
        return board;

    if (type == Pawn) {
        if (board.turn == Turn::white)
            board.white_pawns |= (1UL << idx);
        else
            board.black_pawns |= (1UL << idx);
        return board;
    }
    if (type == Knight) {
        if (board.turn == Turn::white)
            board.white_knights |= (1UL << idx);
        else
            board.black_knights |= (1UL << idx);
        return board;
    }
    if (type == Bishop) {
        if (board.turn == Turn::white)
            board.white_bishops |= (1UL << idx);
        else
            board.black_bishops |= (1UL << idx);
        return board;
    }
    if (type == Rook) {
        if (board.turn == Turn::white)
            board.white_rooks |= (1UL << idx);
        else
            board.black_rooks |= (1UL << idx);
        return board;
    }
    if (type == King) {
        if (board.turn == Turn::white)
            board.white_king |= (1UL << idx);
        else
            board.black_king |= (1UL << idx);
        return board;
    }
    if (type == Queen) {
        if (board.turn == Turn::white)
            board.white_queens |= (1UL << idx);
        else
            board.black_queens |= (1UL << idx);
        return board;
    }

    return board;
}

constexpr inline auto swap_turn(Board board) noexcept
{
    if (board.turn == Turn::white)
        board.turn = Turn::black;
    else
        board.turn = Turn::white;

    return board;
}

// We assume the move is pseudo legal in here
constexpr inline auto en_passant_update(Board board, Piece_type pt, const Move & move) noexcept
{
    if (pt != Pawn) {
        board.en_passant = 0;
        return board;
    }

    if (board.turn == Turn::white) {
        if (move.from == a2 && move.to == a4)
            board.en_passant = (1UL << a3);
        else if (move.from == b2 && move.to == b4)
            board.en_passant = (1UL << b3);
        else if (move.from == c2 && move.to == c4)
            board.en_passant = (1UL << c3);
        else if (move.from == d2 && move.to == d4)
            board.en_passant = (1UL << d3);
        else if (move.from == e2 && move.to == e4)
            board.en_passant = (1UL << e3);
        else if (move.from == f2 && move.to == f4)
            board.en_passant = (1UL << f3);
        else if (move.from == g2 && move.to == g4)
            board.en_passant = (1UL << g3);
        else if (move.from == h2 && move.to == h4)
            board.en_passant = (1UL << h3);
        else
            board.en_passant = 0;

        return board;
    }

    if (move.from == a7 && move.to == a5)
        board.en_passant = (1UL << a6);
    else if (move.from == b7 && move.to == b5)
        board.en_passant = (1UL << b6);
    else if (move.from == c7 && move.to == c5)
        board.en_passant = (1UL << c6);
    else if (move.from == d7 && move.to == d5)
        board.en_passant = (1UL << d6);
    else if (move.from == e7 && move.to == e5)
        board.en_passant = (1UL << e6);
    else if (move.from == f7 && move.to == f5)
        board.en_passant = (1UL << f6);
    else if (move.from == g7 && move.to == g5)
        board.en_passant = (1UL << g6);
    else if (move.from == h7 && move.to == h5)
        board.en_passant = (1UL << h6);
    else
        board.en_passant = 0;

    return board;
}

// Reverse lookup
constexpr inline bool is_in_check(const Board & board) noexcept
{
    // If kings attack each other
    if (king_moves[__builtin_ctzll(board.white_king)] & (1UL << __builtin_ctzll(board.black_king)))
        return true;

    if (board.turn == Turn::white) {
        // Get index of white king
        auto idx = __builtin_ctzll(board.white_king);

        // pretend I'm a pawn from Kings position
        // If I can attack a pawn diagonally, I'm in check
        if (pawn_moves[0][idx] & board.black_pawns)
            return true;

        // pretend I'm a knight from Kings position
        // If I can attack a knight, I'm in check
        if (knight_moves[idx] & board.black_knights)
            return true;

        // Pretend I'm a bishop from Kings position
        // If I can attack a bishop, I'm in check
        if (diagonal_attack_rays(idx, board) & (board.black_bishops ^ board.black_queens))
            return true;

        if (attack_rays_rook(idx, board) & (board.black_rooks ^ board.black_queens))
            return true;
    }
    else if (board.turn == Turn::black) {
        // Get index of black king
        auto idx = __builtin_ctzll(board.black_king);

        // pretend I'm a pawn from Kings position
        // If I can attack a pawn diagonally, I'm in check
        if (pawn_moves[1][idx] & board.white_pawns)
            return true;

        // pretend I'm a knight from Kings position
        // If I can attack a knight, I'm in check
        if (knight_moves[idx] & board.white_knights)
            return true;

        // Pretend I'm a bishop from Kings position
        // If I can attack a bishop, I'm in check
        if (diagonal_attack_rays(idx, board) & (board.white_bishops ^ board.white_queens))
            return true;

        if (attack_rays_rook(idx, board) & (board.white_rooks ^ board.white_queens))
            return true;
    }

    return false;
}

constexpr inline bool is_in_check_turn(const Board & board, Turn turn) noexcept
{
    // If kings attack each other
    if (king_moves[__builtin_ctzll(board.white_king)] & (1UL << __builtin_ctzll(board.black_king)))
        return true;

    if (turn == Turn::white) {
        // Get index of white king
        auto idx = __builtin_ctzll(board.white_king);

        // pretend I'm a pawn from Kings position
        // If I can attack a pawn diagonally, I'm in check
        if (pawn_moves[0][idx] & board.black_pawns)
            return true;

        // pretend I'm a knight from Kings position
        // If I can attack a knight, I'm in check
        if (knight_moves[idx] & board.black_knights)
            return true;

        // Pretend I'm a bishop from Kings position
        // If I can attack a bishop, I'm in check
        if (diagonal_attack_rays(idx, board) & (board.black_bishops ^ board.black_queens))
            return true;

        if (attack_rays_rook(idx, board) & (board.black_rooks ^ board.black_queens))
            return true;
    }
    else if (turn == Turn::black) {
        // Get index of black king
        auto idx = __builtin_ctzll(board.black_king);

        // pretend I'm a pawn from Kings position
        // If I can attack a pawn diagonally, I'm in check
        if (pawn_moves[1][idx] & board.white_pawns)
            return true;

        // pretend I'm a knight from Kings position
        // If I can attack a knight, I'm in check
        if (knight_moves[idx] & board.white_knights)
            return true;

        // Pretend I'm a bishop from Kings position
        // If I can attack a bishop, I'm in check
        if (diagonal_attack_rays(idx, board) & (board.white_bishops ^ board.white_queens))
            return true;

        if (attack_rays_rook(idx, board) & (board.white_rooks ^ board.white_queens))
            return true;
    }

    return false;
}

constexpr inline auto update_castling(Board board, Move move) noexcept
{
    if (move.to == h8)
        board.castling = board.castling &~ (1UL << 2);
    else if (move.to == a8)
        board.castling = board.castling &~ (1UL << 3);
    else if (move.to == h1)
        board.castling = board.castling &~ (1UL << 0);
    else if (move.to == a1)
        board.castling = board.castling &~ (1UL << 1);
    return board;
}

// move  - from, to
// board - the board to modify
constexpr inline auto make_move(Move move, Board board) noexcept
{
    auto piece_type = piece_type_on_idx(board, move.from);

    // If previous move had en_passant enabled AND
    // If this move is a pawn move, AND
    // If this move hits the en_passant square THEN
    // Clear the piece Under/Above the square
    // (We assume this is legal move)
    if (board.en_passant &&
        piece_type == Pawn &&
        (1UL << move.to) == board.en_passant) {
        if (board.turn == Turn::white) {
            clear_black_pawn(board, (1UL << move.to) >> 1);
        }
        else
            clear_white_pawn(board, (1UL << move.to) << 1);
    }


    // If move was e1-g1 & piece was White king, move the rook over
    if (piece_type == King) {
        if (move.from == e1) {
            if (move.to == g1) {
                board.white_rooks = board.white_rooks &~ (1UL << h1);
                board.white_rooks = board.white_rooks | (1UL << f1);
                board.castling = board.castling &~ (1 << 0 | 1 << 1);
            }
            else if (move.to == c1) {
                board.white_rooks = board.white_rooks &~ (1UL << a1);
                board.white_rooks = board.white_rooks | (1UL << d1);
                board.castling = board.castling &~ (1 << 0 | 1 << 1);
            }
            else if (board.turn == Turn::white) {
                board.castling = board.castling &~ (1 << 0 | 1 << 1);
            }
        }
        else if (move.from == e8) {
            if (move.to == g8) {
                board.black_rooks = board.black_rooks &~ (1UL << h8);
                board.black_rooks = board.black_rooks | (1UL << f8);
                board.castling = board.castling &~ (1 << 2 | 1 << 3);
            }
            else if (move.to == c8) {
                board.black_rooks = board.black_rooks &~ (1UL << a8);
                board.black_rooks = board.black_rooks | (1UL << d8);
                board.castling = board.castling &~ (1 << 2 | 1 << 3);
            }
            else if (board.turn == Turn::black) {
                board.castling = board.castling &~ (1 << 2 | 1 << 3);
            }
        }
    }

    // If piece type is rook and move from h1 and turn white
    // disable king side castling
    if (piece_type == Rook) {
        if (move.from == h1 && board.turn == Turn::white) {
            board.castling = board.castling &~ (1 << 0);
        }
        else if (move.from == a1 && board.turn == Turn::white) {
            board.castling = board.castling &~ (1 << 1);
        }
        else if (move.from == h8 && board.turn == Turn::black) {
            board.castling = board.castling &~ (1 << 2);
        }
        else if (move.from == a8 && board.turn == Turn::black) {
            board.castling = board.castling &~ (1 << 3);
        }
    }

    // If is en_passant then
    // trigger en_passant state
    board = en_passant_update(board, piece_type, move);
    board = update_castling(board, move);

    // Clear `move.from` square from `board`
    Board cleared_square_from = clear_square(board, piece_type, move.from);

    auto piece_type_on_to_square = piece_type_on_idx(cleared_square_from, move.to);

    // Clear `move.to`
    Board cleared_square_to = swap_turn(clear_square(swap_turn(cleared_square_from), piece_type_on_to_square, move.to));

    // Update `move.to` square in `board`
    // Set bit for piece type bitboard
    Board updated_board = set_square(cleared_square_to, piece_type, move.to);

    if (updated_board.turn == Turn::white &&
        piece_type == Pawn                &&
        (1UL << move.from) & seventh_rank) {
        updated_board.white_pawns = updated_board.white_pawns &~ (1UL << move.to);

        if (move.promote == Promo_queen || move.promote == Promo_none)
            updated_board.white_queens = updated_board.white_queens | (1UL << move.to);
        if (move.promote == Promo_rook)
            updated_board.white_rooks = updated_board.white_rooks | (1UL << move.to);
        if (move.promote == Promo_knight)
            updated_board.white_knights = updated_board.white_knights | (1UL << move.to);
        if (move.promote == Promo_bishop)
            updated_board.white_bishops = updated_board.white_bishops | (1UL << move.to);
    }
    else if (updated_board.turn == Turn::black &&
             piece_type == Pawn                &&
             (1UL << move.from) & second_rank) {
        updated_board.black_pawns = updated_board.black_pawns &~ (1UL << move.to);
        if (move.promote == Promo_queen || move.promote == Promo_none)
            updated_board.black_queens = updated_board.black_queens | (1UL << move.to);
        if (move.promote == Promo_rook)
            updated_board.black_rooks = updated_board.black_rooks | (1UL << move.to);
        if (move.promote == Promo_knight)
            updated_board.black_knights = updated_board.black_knights | (1UL << move.to);
        if (move.promote == Promo_bishop)
            updated_board.black_bishops = updated_board.black_bishops | (1UL << move.to);
    }

    updated_board = swap_turn(updated_board);

    return updated_board;
}

struct MoveList {
    constexpr std::array<Move, 256>::iterator begin() noexcept
    {
        return list.begin();
    }

    constexpr std::array<Move, 256>::iterator end() noexcept
    {
        return begin() + i;
    }

    void push_back(Move && m) noexcept
    {
        list[i++] = m;
    }

    constexpr size_t size()
    {
        return i;
    }

    size_t i{};
    std::array<Move, 256> list;
};

MoveList generate_moves(const Board & board) noexcept
{
    MoveList ret;
    auto black  = black_pieces(board);
    auto emptys = empty_b(board);
    auto white  = white_pieces(board);

    if (board.turn == Turn::white) {
        {
            auto queens = board.white_queens;
            auto idx     = __builtin_ctzll(queens);
            while (queens) {
                auto attacks_to = filter_attack_rays(attack_rays_queen(idx, board), board, board.turn);
                auto idx_attack = __builtin_ctzll(attacks_to);
                while (attacks_to) {
                    ret.push_back({idx, idx_attack});
                    attacks_to = attacks_to &~ (1UL << idx_attack);
                    idx_attack = __builtin_ctzll(attacks_to);
                }

                queens = queens &~ (1UL << idx);
                idx    = __builtin_ctzll(queens);
            }
        }
        {
            auto rooks = board.white_rooks;
            auto idx     = __builtin_ctzll(rooks);
            while (rooks) {
                auto attacks_to = filter_attack_rays(attack_rays_rook(idx, board), board, board.turn);
                auto idx_attack = __builtin_ctzll(attacks_to);
                while (attacks_to) {
                    ret.push_back({idx, idx_attack});
                    attacks_to = attacks_to &~ (1UL << idx_attack);
                    idx_attack = __builtin_ctzll(attacks_to);
                }

                rooks = rooks &~ (1UL << idx);
                idx   = __builtin_ctzll(rooks);
            }
        }
        {
            auto bishops = board.white_bishops;
            auto idx     = __builtin_ctzll(bishops);
            while (bishops) {
                auto attacks_to = filter_attack_rays(diagonal_attack_rays(idx, board), board, board.turn);
                auto idx_attack = __builtin_ctzll(attacks_to);
                while (attacks_to) {
                    ret.push_back({idx, idx_attack});
                    attacks_to = attacks_to &~ (1UL << idx_attack);
                    idx_attack = __builtin_ctzll(attacks_to);
                }

                bishops = bishops &~ (1UL << idx);
                idx     = __builtin_ctzll(bishops);
            }
        }
        {
            auto knights = board.white_knights;
            auto idx     = __builtin_ctzll(knights);
            while (knights) {
                auto attacks_to = knight_moves[idx] & (black ^ emptys);
                auto idx_attack = __builtin_ctzll(attacks_to);
                while (attacks_to) {
                    ret.push_back({idx, idx_attack});

                    attacks_to = attacks_to &~ (1UL << idx_attack);
                    idx_attack = __builtin_ctzll(attacks_to);
                }

                knights = knights &~ (1UL << idx);
                idx     = __builtin_ctzll(knights);
            }
        }

        // Start with all pawns that can push a single square
        if (auto pawns = w_pawns_able_to_push(board)) {
            auto idx = __builtin_ctzll(pawns);
            while (pawns) {
                if ((1UL << idx) & seventh_rank) { // promotion
                    ret.push_back(Move{idx, idx + 1, Promo_queen});
                    ret.push_back(Move{idx, idx + 1, Promo_rook});
                    ret.push_back(Move{idx, idx + 1, Promo_bishop});
                    ret.push_back(Move{idx, idx + 1, Promo_knight});
                }
                else {
                    ret.push_back(Move{idx, idx + 1});
                }

                pawns = pawns &~ (1UL << idx);
                idx   = __builtin_ctzll(pawns);
            };
        }

        if (auto pawns = w_pawns_able_to_double_push(board)) {
            auto idx = __builtin_ctzll(pawns);
            while (pawns) {
                ret.push_back(Move{idx, idx + 2});
                pawns = pawns &~ (1UL << idx);
                idx   = __builtin_ctzll(pawns);
            }
        }

        {
            auto pawns = board.white_pawns;
            auto idx   = __builtin_ctzll(pawns);
            while (pawns) {
                auto attacks_to = pawn_moves[0][idx] & (black ^ board.en_passant);
                auto idx_attack = __builtin_ctzll(attacks_to);
                while (attacks_to) {
                    if ((1UL << idx) & seventh_rank) {
                        ret.push_back({idx, idx_attack, Promo_queen});
                        ret.push_back({idx, idx_attack, Promo_rook});
                        ret.push_back({idx, idx_attack, Promo_bishop});
                        ret.push_back({idx, idx_attack, Promo_knight});
                    }
                    else {
                        ret.push_back({idx, idx_attack});
                    }

                    attacks_to = attacks_to &~ (1UL << idx_attack);
                    idx_attack = __builtin_ctzll(attacks_to);
                };

                pawns = pawns &~ (1UL << idx);
                idx   = __builtin_ctzll(pawns);
            }
        }
        {
            auto king = board.white_king;
            auto idx = __builtin_ctzll(king);

            // short castle
            if (idx == e1 && (board.castling & (1 << 0)) &&
                emptys & (1UL << f1) &&
                emptys & (1UL << g1)) {
                if (!is_in_check_turn(board, Turn::white) &&
                    !is_in_check_turn(make_move({e1,f1}, board), Turn::white))
                    ret.push_back({idx, g1});
            }
            if (idx == e1 && (board.castling & (1 << 1)) &&
                    emptys & (1UL << d1) &&
                    emptys & (1UL << c1) &&
                    emptys & (1UL << b1)) {
                if (!is_in_check_turn(board, Turn::white) &&
                    !is_in_check_turn(make_move({e1,d1}, board), Turn::white))
                    ret.push_back({idx, c1});
            }

            auto attacks = filter_attack_rays(king_moves[idx], board, board.turn);
            auto idx_attack = __builtin_ctzll(attacks);
            while (attacks) {
                ret.push_back({idx, idx_attack});

                attacks = attacks &~ (1UL << idx_attack);
                idx_attack = __builtin_ctzll(attacks);
            }
        }

    }
    else {
        {
            auto queens = board.black_queens;
            auto idx     = __builtin_ctzll(queens);
            while (queens) {
                auto attacks_to = filter_attack_rays(attack_rays_queen(idx, board), board, board.turn);
                auto idx_attack = __builtin_ctzll(attacks_to);
                while (attacks_to) {
                    ret.push_back({idx, idx_attack});
                    attacks_to = attacks_to &~ (1UL << idx_attack);
                    idx_attack = __builtin_ctzll(attacks_to);
                }

                queens = queens &~ (1UL << idx);
                idx     = __builtin_ctzll(queens);
            }
        }
        {
            auto rooks = board.black_rooks;
            auto idx     = __builtin_ctzll(rooks);
            while (rooks) {
                auto attacks_to = filter_attack_rays(attack_rays_rook(idx, board), board, board.turn);
                auto idx_attack = __builtin_ctzll(attacks_to);
                while (attacks_to) {
                    ret.push_back({idx, idx_attack});
                    attacks_to = attacks_to &~ (1UL << idx_attack);
                    idx_attack = __builtin_ctzll(attacks_to);
                }

                rooks = rooks &~ (1UL << idx);
                idx     = __builtin_ctzll(rooks);
            }
        }
        {
            auto knights = board.black_knights;
            auto idx     = __builtin_ctzll(knights);
            while (knights) {
                auto attacks_to = knight_moves[idx] & (white ^ emptys);
                auto idx_attack = __builtin_ctzll(attacks_to);
                while (attacks_to) {
                    ret.push_back({idx, idx_attack});

                    attacks_to = attacks_to &~ (1UL << idx_attack);
                    idx_attack = __builtin_ctzll(attacks_to);
                }

                knights = knights &~ (1UL << idx);
                idx     = __builtin_ctzll(knights);
            }
        }
        {
            auto bishops = board.black_bishops;
            auto idx     = __builtin_ctzll(bishops);
            while (bishops) {
                auto attacks_to = filter_attack_rays(diagonal_attack_rays(idx, board), board, board.turn);
                auto idx_attack = __builtin_ctzll(attacks_to);
                while (attacks_to) {
                    ret.push_back({idx, idx_attack});
                    attacks_to = attacks_to &~ (1UL << idx_attack);
                    idx_attack = __builtin_ctzll(attacks_to);
                }

                bishops = bishops &~ (1UL << idx);
                idx     = __builtin_ctzll(bishops);
            }
        }

        if (auto pawns = b_pawns_able_to_push(board)) {
            auto idx = __builtin_ctzll(pawns);
            while (pawns) {
                if ((1UL << idx) & second_rank) { // promotion
                    ret.push_back({idx, idx - 1, Promo_queen});
                    ret.push_back({idx, idx - 1, Promo_rook});
                    ret.push_back({idx, idx - 1, Promo_bishop});
                    ret.push_back({idx, idx - 1, Promo_knight});
                }
                else {
                    ret.push_back({idx, idx - 1});
                }

                pawns = pawns &~ (1UL << idx);
                idx   = __builtin_ctzll(pawns);
            };
        }

        if (auto pawns = b_pawns_able_to_double_push(board)) {
            auto idx = __builtin_ctzll(pawns);
            while (pawns) {
                ret.push_back({idx, idx - 2});
                pawns = pawns &~ (1UL << idx);
                idx   = __builtin_ctzll(pawns);
            }
        }

        {
            auto pawns = board.black_pawns;
            auto idx   = __builtin_ctzll(pawns);
            while (pawns) {
                auto attacks_to = pawn_moves[1][idx] & (white ^ board.en_passant);
                auto idx_attack = __builtin_ctzll(attacks_to);
                while (attacks_to) {
                    if ((1UL << idx) & second_rank) {
                        ret.push_back({idx, idx_attack, Promo_queen});
                        ret.push_back({idx, idx_attack, Promo_rook});
                        ret.push_back({idx, idx_attack, Promo_bishop});
                        ret.push_back({idx, idx_attack, Promo_knight});
                    }
                    else {
                        ret.push_back({idx, idx_attack});
                    }

                    attacks_to = attacks_to &~ (1UL << idx_attack);
                    idx_attack = __builtin_ctzll(attacks_to);
                };

                pawns = pawns &~ (1UL << idx);
                idx   = __builtin_ctzll(pawns);
            }
        }
        {
            auto king = board.black_king;
            auto idx = __builtin_ctzll(king);

            // short castle
            if (idx == e8 && (board.castling & (1 << 2)) &&
                emptys & (1UL << f8) &&
                emptys & (1UL << g8)) {
                if (!is_in_check_turn(board, Turn::black) &&
                    !is_in_check_turn(make_move({e8, f8}, board), Turn::black))
                    ret.push_back({idx, g8});
            }
            if (idx == e8 && (board.castling & (1 << 3)) &&
                    emptys & (1UL << d8) &&
                    emptys & (1UL << c8) &&
                    emptys & (1UL << b8)) {
                if (!is_in_check_turn(board, Turn::black) &&
                    !is_in_check_turn(make_move({e8,d8}, board), Turn::black))
                    ret.push_back({idx, c8});
            }

            auto attacks = filter_attack_rays(king_moves[idx], board, board.turn);
            auto idx_attack = __builtin_ctzll(attacks);
            while (attacks) {
                ret.push_back({idx, idx_attack});

                attacks = attacks &~ (1UL << idx_attack);
                idx_attack = __builtin_ctzll(attacks);
            }
        }

    }

    return ret;
}


std::array<const char*, 64> square_to_str {
    "a1", "a2", "a3", "a4","a5","a6","a7","a8",
    "b1", "b2", "b3", "b4","b5","b6","b7","b8",
    "c1", "c2", "c3", "c4","c5","c6","c7","c8",
    "d1", "d2", "d3", "d4","d5","d6","d7","d8",
    "e1", "e2", "e3", "e4","e5","e6","e7","e8",
    "f1", "f2", "f3", "f4","f5","f6","f7","f8",
    "g1", "g2", "g3", "g4","g5","g6","g7","g8",
    "h1", "h2", "h3", "h4","h5","h6","h7","h8",
};

constexpr u64 perft(int depth, const Board & board, int print_at) noexcept
{
    if (depth == 0)
        return 1ULL;

    auto nodes = 0ULL;

    for (auto move : generate_moves(board)) {
        auto inner_node = 0ULL;
        if (auto foo = make_move(move, board); !is_in_check_turn(foo, board.turn))
            inner_node += perft(depth - 1, foo, print_at);

        // if (depth == print_at && inner_node != 0)
        //     fmt::print("{}{}: nodes: {}\n", square_to_str[move.from], square_to_str[move.to], inner_node);
        nodes += inner_node;
    }

    return nodes;
}


constexpr inline u64 num_legal_moves(Board board)
{
    auto ret = 0UL;
    if (board.turn == Turn::white) {
        auto w          = white_pieces(board);
        auto idx        = __builtin_ctzll(w);
        auto piece_type = piece_type_on_idx(board, idx);

        auto foo = __builtin_popcountll(w_pawns_able_to_push(board))
            +  __builtin_popcountll(w_pawns_able_to_double_push(board));

        ret += foo;

        do {
            if (piece_type == Rook) {
                ret += __builtin_popcountll(filter_attack_rays(attack_rays_rook(idx, board), board, board.turn));
            }
            else if (piece_type == Knight) {
                ret += __builtin_popcountll(filter_attack_rays(knight_moves[idx], board, board.turn));
            }
            else if (piece_type == Bishop) {
                ret += __builtin_popcountll(filter_attack_rays(diagonal_attack_rays(idx, board), board, board.turn));
            }
            else if (piece_type == Queen) {
                ret += __builtin_popcountll(filter_attack_rays(attack_rays_queen(idx, board), board, board.turn));
            }
            else if (piece_type == King) {
                ret += __builtin_popcountll(filter_attack_rays(king_moves[idx], board, board.turn));

                // TODO
                // include castle moves
            }

            w          = w&~ (1UL << idx);
            idx        = __builtin_ctzll(w);
            piece_type = piece_type_on_idx(board, idx);

        } while (idx != 64);
    }
    else if (board.turn == Turn::black) {
        auto w          = black_pieces(board);
        auto idx        = __builtin_ctzll(w);
        auto piece_type = piece_type_on_idx(board, idx);

        auto foo = __builtin_popcountll(b_pawns_able_to_push(board))
            +  __builtin_popcountll(b_pawns_able_to_double_push(board));

        ret += foo;

        do {
            if (piece_type == Rook) {
                ret += __builtin_popcountll(filter_attack_rays(attack_rays_rook(idx, board), board, board.turn));
            }
            else if (piece_type == Knight) {
                ret += __builtin_popcountll(filter_attack_rays(knight_moves[idx], board, board.turn));
            }
            else if (piece_type == Bishop) {
                ret += __builtin_popcountll(filter_attack_rays(diagonal_attack_rays(idx, board), board, board.turn));
            }
            else if (piece_type == Queen) {
                ret += __builtin_popcountll(filter_attack_rays(attack_rays_queen(idx, board), board, board.turn));
            }
            else if (piece_type == King) {
                ret += __builtin_popcountll(filter_attack_rays(king_moves[idx], board, board.turn));

                // TODO
                // include castle moves
            }

            w          = w&~ (1UL << idx);
            idx        = __builtin_ctzll(w);
            piece_type = piece_type_on_idx(board, idx);

        } while (idx != 64);
    }

    return ret;
}


Board parse_fen(std::string fen)
{
    Board board;

    // std::string fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq e6 0 1";
    u64 eight_rank = seventh_rank << 1;
    u64 A          = 0x00000000000000FF;

    u64 rank = eight_rank;
    u64 col  = A;

    char enpas[3]{};

    bool parse_state = false;
    bool parse_enpassant = false;
    bool finish = false;
    bool parse_castling = false;
    for (size_t i = 0; i != fen.size(); ++i) {

        if (finish)
            break;

        if (parse_enpassant) {
            switch (fen[i]) {
                case 'a' ... 'h': enpas[0] = fen[i]; break;
                case '1' ... '8': enpas[1] = fen[i]; finish = true; board.en_passant = print_mapping[move_key[enpas]]; break;
            }
            continue;
        }

        if (parse_castling) {
            switch (fen[i]) {
                case 'K': board.castling |= (1 << 0); break;
                case 'Q': board.castling |= (1 << 1); break;
                case 'k': board.castling |= (1 << 2); break;
                case 'q': board.castling |= (1 << 3); break;
                case ' ': parse_enpassant = true; break;
            }
            continue;
        }

        if (parse_state) {
            switch (fen[i]) {
                case 'w': board.turn = Turn::white; break;
                case 'b': board.turn = Turn::black; break;
                case ' ': parse_castling = true; break;
            }
            continue;
        }

        switch (fen[i]) {
            case 'r': board.black_rooks   |= (1UL << __builtin_ctzll(rank & col)); col <<= 8; break;
            case 'n': board.black_knights |= (1UL << __builtin_ctzll(rank & col)); col <<= 8; break;
            case 'b': board.black_bishops |= (1UL << __builtin_ctzll(rank & col)); col <<= 8; break;
            case 'q': board.black_queens  |= (1UL << __builtin_ctzll(rank & col)); col <<= 8; break;
            case 'k': board.black_king    |= (1UL << __builtin_ctzll(rank & col)); col <<= 8; break;
            case 'p': board.black_pawns   |= (1UL << __builtin_ctzll(rank & col)); col <<= 8; break;
            case 'R': board.white_rooks   |= (1UL << __builtin_ctzll(rank & col)); col <<= 8; break;
            case 'N': board.white_knights |= (1UL << __builtin_ctzll(rank & col)); col <<= 8; break;
            case 'B': board.white_bishops |= (1UL << __builtin_ctzll(rank & col)); col <<= 8; break;
            case 'Q': board.white_queens  |= (1UL << __builtin_ctzll(rank & col)); col <<= 8; break;
            case 'K': board.white_king    |= (1UL << __builtin_ctzll(rank & col)); col <<= 8; break;
            case 'P': board.white_pawns   |= (1UL << __builtin_ctzll(rank & col)); col <<= 8; break;
            case '1' ... '8': col <<= int(fen[i] - 48) * 8; break;
            case '/': rank >>= 1; col = A; break;
            case ' ': parse_state = true;
        }
    }

    return board;
}

constexpr inline auto evaluation(Board board)
{
    auto material = 9500 * (__builtin_popcountll(board.white_king) - __builtin_popcountll(board.black_king))
                  + 900 * (__builtin_popcountll(board.white_queens) - __builtin_popcountll(board.black_queens))
                  + 500 * (__builtin_popcountll(board.white_rooks) - __builtin_popcountll(board.black_rooks))
                  + 300 * (__builtin_popcountll(board.white_bishops) - __builtin_popcountll(board.black_bishops))
                  + 300 * (__builtin_popcountll(board.white_knights) - __builtin_popcountll(board.black_knights))
                  + 100 * (__builtin_popcountll(board.white_pawns) - __builtin_popcountll(board.black_pawns));

    auto activity_pawns = (__builtin_popcountll(w_pawns_able_to_push(board)) - __builtin_popcountll(b_pawns_able_to_double_push(board)))
                        + (__builtin_popcountll(w_pawns_able_to_double_push(board) - __builtin_popcountll(b_pawns_able_to_double_push(board))));

    auto pieces = all_pieces(board);
    auto idx = __builtin_ctzll(pieces);

    auto activity_black = 0;
    auto activity_white = 0;

    while (pieces) {
        auto piece_type = piece_type_on_idx(board, idx);

        if (piece_type == Pawn) {
            if ((1UL << idx) & board.black_pawns) {
                activity_black += __builtin_popcountll(filter_attack_rays(pawn_moves[1][idx], board, Turn::black));
            } else {
                activity_white += __builtin_popcountll(filter_attack_rays(pawn_moves[0][idx], board, Turn::white));
            }
        }

        if (piece_type == Knight) {
            if ((1UL << idx) & board.black_knights) {
                activity_black += __builtin_popcountll(filter_attack_rays(knight_moves[idx], board, Turn::black));
            } else {
                activity_white += __builtin_popcountll(filter_attack_rays(knight_moves[idx], board, Turn::white));
            }
        }

        if (piece_type == Bishop) {
            if ((1UL << idx) & board.black_bishops) {
                activity_black += __builtin_popcountll(filter_attack_rays(diagonal_attack_rays(idx, board), board, Turn::black));
            } else {
                activity_white += __builtin_popcountll(filter_attack_rays(diagonal_attack_rays(idx, board), board, Turn::white));
            }
        }

        if (piece_type == Rook) {
            if ((1UL << idx) & board.black_rooks) {
                activity_black += __builtin_popcountll(filter_attack_rays(attack_rays_rook(idx, board), board, Turn::black));
            } else {
                activity_white += __builtin_popcountll(filter_attack_rays(attack_rays_rook(idx, board), board, Turn::white));
            }
        }

        if (piece_type == Queen) {
            if ((1UL << idx) & board.black_queens) {
                activity_black += __builtin_popcountll(filter_attack_rays(attack_rays_queen(idx, board), board, Turn::black));
            } else {
                activity_white += __builtin_popcountll(filter_attack_rays(attack_rays_queen(idx, board), board, Turn::white));
            }
        }

        pieces = lsb(pieces);
        idx = bitscanF(pieces);
    }

    auto mat = material + 1 * ((activity_white - activity_black) + (activity_pawns));

    return board.turn == Turn::white ? mat * 1 : mat * -1;
}

int negamax(int depth, Board board)
{
    if (depth == 0)
        return evaluation(board);

    int max = std::numeric_limits<int>::min();

    auto moves = generate_moves(board);
    if (moves.size() == 0) {
        return evaluation(board) * 999;
    }
    for (auto move : moves) {
        auto foo = make_move(move, board);
        int score = -negamax(depth - 1, foo);
        if (score > max) {
            max = score;
        }
    }

    return max;
}

Move think(Board board, int depth)
{
    auto moves = generate_moves(board);
    Move m;

    int best_eval = std::numeric_limits<int>::min();

    for (auto move : moves) {
        auto foo = make_move(move, board);
        int eval_move = -negamax(depth - 1, foo);

        if (eval_move > best_eval) {
            m = move;
            best_eval = eval_move;
        }
    }

    return m;
}

#if TESTS

TEST_CASE("attack ray north from b2-a8")
{
    u64 ray = north(b2);
    std::bitset<64> expected {
    //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00000000"  // D
        "00000000"  // C
        "11111100"  // B
        "00000000"  // A
    };
    REQUIRE(ray == expected.to_ulong());
}

TEST_CASE("attack ray south from d7-d1")
{
    u64 ray = south(d7);
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00111111"  // D
        "00000000"  // C
        "00000000"  // B
        "00000000"  // A
    };
    REQUIRE(ray == expected.to_ulong());
}

TEST_CASE("queen is on d1 startpos")
{
    auto board = start_position();
    REQUIRE(piece_type_on_idx(board, d1) == Queen);
}

TEST_CASE("d3 is empty in startpos")
{
    auto board = start_position();
    REQUIRE(piece_type_on_idx(board, d3) == None);
}

TEST_CASE("east from d1 == e1,f1,g1,h1")
{
    u64 moves = east(d1);
    REQUIRE(moves & (1UL << e1));
    REQUIRE(moves & (1UL << f1));
    REQUIRE(moves & (1UL << g1));
    REQUIRE(moves & (1UL << h1));
}

TEST_CASE("no obstacles: north attack rays from d1=d2,d3,d4,d5,d6,d7,d8")
{
    Board board;
    board.turn = Turn::white;
    board.white_queens = 1UL << d1;
    auto moves = attack_ray_north(d1, all_pieces(board));
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "11111110"  // D
        "00000000"  // C
        "00000000"  // B
        "00000000"  // A
    };
    CHECK(moves == expected.to_ulong());
}

TEST_CASE("knight moves a1")
{
    u64 attacks = knight_moves[a1];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00000000"  // D
        "00000010"  // C
        "00000100"  // B
        "00000000"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves a2")
{
    u64 attacks = knight_moves[a2];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00000000"  // D
        "00000101"  // C
        "00001000"  // B
        "00000000"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves a3")
{
    u64 attacks = knight_moves[a3];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00000000"  // D
        "00001010"  // C
        "00010001"  // B
        "00000000"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves a4")
{
    u64 attacks = knight_moves[a4];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00000000"  // D
        "00010100"  // C
        "00100010"  // B
        "00000000"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves a5")
{
    u64 attacks = knight_moves[a5];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00000000"  // D
        "00101000"  // C
        "01000100"  // B
        "00000000"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves a6")
{
    u64 attacks = knight_moves[a6];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00000000"  // D
        "01010000"  // C
        "10001000"  // B
        "00000000"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves a7")
{
    u64 attacks = knight_moves[a7];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00000000"  // D
        "10100000"  // C
        "00010000"  // B
        "00000000"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves a8")
{
    u64 attacks = knight_moves[a8];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00000000"  // D
        "01000000"  // C
        "00100000"  // B
        "00000000"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves b1")
{
    u64 attacks = knight_moves[b1];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00000010"  // D
        "00000100"  // C
        "00000000"  // B
        "00000100"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves b2")
{
    u64 attacks = knight_moves[b2];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00000101"  // D
        "00001000"  // C
        "00000000"  // B
        "00001000"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves b3")
{
    u64 attacks = knight_moves[b3];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00001010"  // D
        "00010001"  // C
        "00000000"  // B
        "00010001"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves b4")
{
    u64 attacks = knight_moves[b4];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00010100"  // D
        "00100010"  // C
        "00000000"  // B
        "00100010"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves b5")
{
    u64 attacks = knight_moves[b5];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00101000"  // D
        "01000100"  // C
        "00000000"  // B
        "01000100"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves b6")
{
    u64 attacks = knight_moves[b6];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "01010000"  // D
        "10001000"  // C
        "00000000"  // B
        "10001000"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves b7")
{
    u64 attacks = knight_moves[b7];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "10100000"  // D
        "00010000"  // C
        "00000000"  // B
        "00010000"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("knight moves b8")
{
    u64 attacks = knight_moves[b8];
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "01000000"  // D
        "00100000"  // C
        "00000000"  // B
        "00100000"  // A
    };
    CHECK(expected.to_ulong() == attacks);
}

TEST_CASE("no obstacles: west attack rays from d1=c1,b1,a1")
{
    Board board;
    board.turn = Turn::white;
    board.white_queens = 1UL << d1;
    auto moves = attack_ray_west(d1, all_pieces(board));
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00000000"  // D
        "00000001"  // C
        "00000001"  // B
        "00000001"  // A
    };
    CHECK(moves == expected.to_ulong());
}

TEST_CASE("attack rays east from d1 on empty board")
{
    Board board;
    board.white_queens = 1UL << d1;
    u64 moves = attack_ray_east(d1, all_pieces(board));
    std::bitset<64> expected{
        //   87654321
        "00000001"  // H
        "00000001"  // G
        "00000001"  // F
        "00000001"  // E
        "00000000"  // D
        "00000000"  // C
        "00000000"  // B
        "00000000"  // A
    };
    CHECK(moves == expected.to_ulong());
}

TEST_CASE("attack rays west from d1 on empty board")
{
    Board board;
    board.white_queens = 1UL << d1;
    u64 moves = attack_ray_west(d1, all_pieces(board));
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00000000"  // D
        "00000001"  // C
        "00000001"  // B
        "00000001"  // A
    };
    CHECK(moves == expected.to_ulong());
}

TEST_CASE("attack rays south from d4 on empty board")
{
    Board board;
    board.white_queens = 1UL << d4;
    u64 moves = attack_ray_south(d4, all_pieces(board));
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "00000111"  // D
        "00000000"  // C
        "00000000"  // B
        "00000000"  // A
    };
    CHECK(moves == expected.to_ulong());
}

TEST_CASE("attack rays north from d4 on empty board")
{
    Board board;
    board.white_queens = 1UL << d4;
    u64 moves = attack_ray_north(d4, all_pieces(board));
    std::bitset<64> expected{
        //   87654321
        "00000000"  // H
        "00000000"  // G
        "00000000"  // F
        "00000000"  // E
        "11110000"  // D
        "00000000"  // C
        "00000000"  // B
        "00000000"  // A
    };
    CHECK(moves == expected.to_ulong());
}

TEST_CASE("diagonal attack rays north from d4 on empty board")
{
    Board board;
    board.white_queens = 1UL << d4;
    u64 moves = diagonal_attack_rays(d4, board);
    std::bitset<64> expected{
        //   87654321
        "10000000"  // H
        "01000001"  // G
        "00100010"  // F
        "00010100"  // E
        "00000000"  // D
        "00010100"  // C
        "00100010"  // B
        "01000001"  // A
    };
    CHECK(moves == expected.to_ulong());
}

TEST_CASE("White queen d1 can move to correct squares on empty board")
{
    Board board;
    board.turn = Turn::white;
    board.white_queens = 1UL << d1;

    u64 moves = filter_attack_rays(attack_rays_queen(d1, board), board, board.turn);
    std::bitset<64> expected{
        //   87654321
        "00010001"  // H
        "00001001"  // G
        "00000101"  // F
        "00000011"  // E
        "11111110"  // D
        "00000011"  // C
        "00000101"  // B
        "00001001"  // A
    };
    CHECK(moves == expected.to_ulong());
}

TEST_CASE("White queen on d1 has pseudo legal move d8 on empty board")
{
    Board board;
    board.turn = Turn::white;
    board.white_queens = 1UL << d1;

    Move move;
    move.from = d1;
    move.to = d8;
    REQUIRE(is_pseudo_legal(Queen, move, board));
    move.to = d7;
    REQUIRE(is_pseudo_legal(Queen, move, board));
    move.to = d6;
    REQUIRE(is_pseudo_legal(Queen, move, board));
    move.to = d5;
    REQUIRE(is_pseudo_legal(Queen, move, board));
    move.to = d4;
    REQUIRE(is_pseudo_legal(Queen, move, board));
    move.to = d3;
    REQUIRE(is_pseudo_legal(Queen, move, board));
    move.to = d2;
    REQUIRE(is_pseudo_legal(Queen, move, board));
}

SCENARIO("White bishop a1, obstacle on c3")
{
    Board board;
    board.turn = Turn::white;
    board.white_bishops = 1UL << a1;

    WHEN("black pawn on c3")
    {
        board.black_pawns = 1UL << c3;

        THEN("white bishop can move to c3")
        {
            Move move{ a1, c3 };
            REQUIRE(is_pseudo_legal(Bishop, move, board));
        }

        THEN("white bishop can NOT move to d4")
        {
            REQUIRE(!is_pseudo_legal(Bishop, {a1, d4}, board));
        }
    }

    WHEN("white pawn on c3")
    {
        board.white_pawns = 1UL << c3;

        THEN("white bishop can move to b2")
        {
            REQUIRE(is_pseudo_legal(Bishop, {a1, b2}, board));
        }

        THEN("white bishop can NOT move to c3")
        {
            REQUIRE(!is_pseudo_legal(Bishop, {a1, c3}, board));
        }
    }
}

SCENARIO("White rook on a1")
{
    Board board;
    board.turn = Turn::white;
    board.white_rooks = 1UL << a1;

    THEN("Vertical attack ray = a2-a8")
    {
        auto ray = vertical_attack_rays(a1, board);
        std::bitset<64> expected{
            //   87654321
            "00000000"  // H
            "00000000"  // G
            "00000000"  // F
            "00000000"  // E
            "00000000"  // D
            "00000000"  // C
            "00000000"  // B
            "11111110"  // A
        };
        REQUIRE(ray == expected.to_ulong());
    }

    THEN("White obstacle a3: a2 and b1-h1")
    {
        board.white_pawns = 1UL << a3;
        auto ray = filter_attack_rays(attack_rays_rook(a1, board), board, board.turn);
        std::bitset<64> expected{
            //   87654321
            "00000001"  // H
            "00000001"  // G
            "00000001"  // F
            "00000001"  // E
            "00000001"  // D
            "00000001"  // C
            "00000001"  // B
            "00000010"  // A
        };
        REQUIRE(ray == expected.to_ulong());
    }

    THEN("Board is empty: attack squares = a2-a8 and b1-h1")
    {
        auto ray = attack_rays_rook(a1, board);
        REQUIRE(is_pseudo_legal(Rook, {a1, a2}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, a3}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, a4}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, a5}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, a6}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, a7}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, a8}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, b1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, c1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, d1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, e1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, f1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, g1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, h1}, board));
    }

    WHEN("White obstacle on a2 -> b1-h1")
    {
        board.white_pawns = 1UL << a2;
        REQUIRE(!is_pseudo_legal(Rook, {a1, a2}, board));
        REQUIRE(!is_pseudo_legal(Rook, {a1, a3}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, b1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, c1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, d1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, e1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, f1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, g1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, h1}, board));
    }

    WHEN("Black obstacle on a2 -> b1-h1 and a2")
    {
        board.black_pawns = 1UL << a2;
        REQUIRE(is_pseudo_legal(Rook, {a1, a2}, board));
        REQUIRE(!is_pseudo_legal(Rook, {a1, a3}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, b1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, c1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, d1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, e1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, f1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, g1}, board));
        REQUIRE(is_pseudo_legal(Rook, {a1, h1}, board));
    }
}

TEST_CASE("knight moves from a1")
{
    Board board;
    board.white_knights = 1UL << a1;
    board.turn = Turn::white;

    REQUIRE(is_pseudo_legal(Knight, {a1, b3}, board));
    REQUIRE(is_pseudo_legal(Knight, {a1, c2}, board));

    board.white_pawns = 1UL << c2;

    REQUIRE(!is_pseudo_legal(Knight, {a1, c2}, board));
}

TEST_CASE("King on h1")
{
    Board board;
    board.white_king = 1UL << h1;
    board.turn = Turn::white;

    REQUIRE(is_pseudo_legal(King, {h1, h2}, board));
    REQUIRE(is_pseudo_legal(King, {h1, g2}, board));
    REQUIRE(is_pseudo_legal(King, {h1, g1}, board));
}

TEST_CASE("white pawn moves")
{
    Board board;
    board.white_pawns = 1UL << d2 | 1UL << e2;
    board.turn = Turn::white;

    REQUIRE(is_pseudo_legal(Pawn, {d2, d3}, board));
    REQUIRE(is_pseudo_legal(Pawn, {e2, e4}, board));
}

TEST_CASE("black pawn moves")
{
    Board board;
    board.black_pawns = 1UL << e7;
    board.turn = Turn::black;

    REQUIRE(is_pseudo_legal(Pawn, {e7, e6}, board));
    REQUIRE(is_pseudo_legal(Pawn, {e7, e5}, board));
    REQUIRE(!is_pseudo_legal(Pawn, {e7, e4}, board));
}

TEST_CASE("white pawn attack moves")
{
    Board board;
    board.white_pawns = 1UL << e2;
    board.black_rooks = 1UL << d3;
    board.turn = Turn::white;
    board.en_passant = 1UL << f3;

    REQUIRE(is_pseudo_legal(Pawn, {e2, d3}, board));
    REQUIRE(is_pseudo_legal(Pawn, {e2, f3}, board));
}

TEST_CASE("pawns move north one")
{
    Board board;
    board.white_pawns = std::bitset<64> {
        "01000000"
        "00000010"
        "00000000"
        "00000000"
        "00000010"
        "00000110"
        "00000000"
        "00000000"
    }.to_ulong();

    board.black_pawns = std::bitset<64> {
        "00000000"
        "00000101"
        "00000100"
        "00000100"
        "00000100"
        "00000000"
        "00000000"
        "00000000"
    }.to_ulong();

    // print(w_pawns_able_to_push(board));
    // print(b_pawns_able_to_push(board));
}

TEST_CASE("pawns able to double push")
{
    Board board;
    board.white_pawns = std::bitset<64> {
        "00000010"
        "00000000"
        "00000010"
        "00000000"
        "00000010"
        "00000000"
        "00100110"
        "00000100"
    }.to_ulong();

    board.black_pawns = std::bitset<64> {
        "00001000"
        "00000000"
        "00000100"
        "00100000"
        "00010000"
        "01000000"
        "01000000"
        "00000000"
    }.to_ulong();

    // print(w_pawns_able_to_double_push(board));
    // print(b_pawns_able_to_double_push(board));
}

TEST_CASE("pawn push")
{
    Move move;
    move.from = d2;
    move.to = d3;

    // print((1UL << move.from) << 1);
}

TEST_CASE("clear square d2")
{
    Board board;
    board.white_pawns = 1UL << d2;
    board = clear_square(board, Pawn, d2);

    u64 b = std::bitset<64> {
        "00000000"
        "00000000"
        "00000000"
        "00000000"
        "00000000"
        "00000000"
        "00000000"
        "00000000"
    }.to_ulong();

    REQUIRE(board.white_pawns == b);
}

TEST_CASE("En passant enabled")
{
    auto board = start_position();

    board = make_move({d2, d4}, board);
    REQUIRE(board.en_passant & (1UL << d3));
    board = make_move({h7, h6}, board);
    REQUIRE(board.en_passant == 0);
    board = make_move({d4, d5}, board);
    REQUIRE(board.en_passant == 0);
    board = make_move({e7, e5}, board);
    REQUIRE(board.en_passant & (1UL << e6));
    board = make_move({d5, e6}, board);
    REQUIRE(board.en_passant == 0);
}

TEST_CASE("Black pawns attack")
{
    auto board = start_position();

    board = make_move({d2,d4}, board);
    board = make_move({e7,e5}, board);
    board = make_move({d4,e5}, board);
    board = make_move({g8,f6}, board);
    board = make_move({e5,f6}, board);
    // print(pawn_moves[1][g7]);

}

TEST_CASE("piece at d2")
{
    Board board;
    board.white_pawns = 1UL << d2;
    std::string s = board_str[std::underlying_type_t<Turn>(board.turn)][piece_type_on_idx(board, d2)];
    REQUIRE(s == "♙");
    // print_board(board);
}

TEST_CASE("white can castle king side")
{
    Board board;
    board.castling = 1 << 0;

    REQUIRE(w_can_castle_king_side(board));
    REQUIRE(!w_can_castle_queen_side(board));
    REQUIRE(!b_can_castle_queen_side(board));
    REQUIRE(!b_can_castle_king_side(board));
}

TEST_CASE("white can castle queen side")
{
    Board board;
    board.castling = 1 << 1;

    REQUIRE(!w_can_castle_king_side(board));
    REQUIRE(w_can_castle_queen_side(board));
    REQUIRE(!b_can_castle_queen_side(board));
    REQUIRE(!b_can_castle_king_side(board));
}

TEST_CASE("black can castle king side")
{
    Board board;
    board.castling = 1 << 2;
    REQUIRE(!w_can_castle_king_side(board));
    REQUIRE(!w_can_castle_queen_side(board));
    REQUIRE(!b_can_castle_queen_side(board));
    REQUIRE(b_can_castle_king_side(board));
}

TEST_CASE("black can castle queen side")
{
    Board board;
    board.castling = 1 << 3;
    REQUIRE(!w_can_castle_king_side(board));
    REQUIRE(!w_can_castle_queen_side(board));
    REQUIRE(b_can_castle_queen_side(board));
    REQUIRE(!b_can_castle_king_side(board));
}

TEST_CASE("White castle")
{
    Board board = start_position();
    board.castling = 1 << 0
                   | 1 << 1
                   | 1 << 2
                   | 1 << 3;

    REQUIRE(!is_pseudo_legal(King, {e1,g1}, board));

    board = make_move({g1,f3}, board);
    board = make_move({e7,e5}, board);
    board = make_move({g2,g3}, board);
    board = make_move({h7,h6}, board);
    board = make_move({f1,g2}, board);
    board = make_move({a7,a6}, board);
    board = make_move({e1,g1}, board);
    REQUIRE(!w_can_castle_king_side(board));
    REQUIRE(!w_can_castle_queen_side(board));
    REQUIRE(b_can_castle_queen_side(board));
    REQUIRE(b_can_castle_king_side(board));
}

TEST_CASE("White can not castle kingside if rook moved")
{
    Board board = start_position();
    board.castling = 1 << 0
                   | 1 << 1
                   | 1 << 2
                   | 1 << 3;

    board = make_move({g1,f3}, board);
    board = make_move({e7,e5}, board);
    board = make_move({g2,g3}, board);
    board = make_move({h7,h6}, board);
    board = make_move({f1,g2}, board);
    board = make_move({a7,a6}, board);
    board = make_move({h1,g1}, board);
    board = make_move({d7,d5}, board);
    board = make_move({g1,h1}, board);
    board = make_move({d5,d4}, board);
    REQUIRE(!w_can_castle_king_side(board));
    REQUIRE(w_can_castle_queen_side(board));
    REQUIRE(b_can_castle_queen_side(board));
    REQUIRE(b_can_castle_king_side(board));
    REQUIRE(!is_pseudo_legal(King, {e1,g8}, board));
}

TEST_CASE("White can not castle queen side if rook moved")
{
    Board board = start_position();
    board.castling = 1 << 0
                   | 1 << 1
                   | 1 << 2
                   | 1 << 3;

    board = make_move({a2, a4}, board);
    board = make_move({a7, a5}, board);
    board = make_move({a1, a3}, board);

    REQUIRE(w_can_castle_king_side(board));
    REQUIRE(!w_can_castle_queen_side(board));
    REQUIRE(b_can_castle_queen_side(board));
    REQUIRE(b_can_castle_king_side(board));

    board = make_move({h7,h6}, board);
    board = make_move({a3,a1}, board);
    board = make_move({g7,g6}, board);
    board = make_move({b1,c3}, board);
    board = make_move({e7,e6}, board);
    board = make_move({d2,d3}, board);
    board = make_move({e6,e5}, board);
    board = make_move({d1,d2}, board);
    board = make_move({e5,e4}, board);
    board = make_move({b2,b3}, board);
    board = make_move({b8,c6}, board);
    board = make_move({c1,b2}, board);
    board = make_move({c6,b8}, board);

    REQUIRE(!is_pseudo_legal(King, {e1,c1}, board));
    REQUIRE(w_can_castle_king_side(board));
    REQUIRE(!w_can_castle_queen_side(board));
    REQUIRE(b_can_castle_queen_side(board));
    REQUIRE(b_can_castle_king_side(board));
}

TEST_CASE("White can not castle if he moves king")
{
    Board board = start_position();
    board.castling = 1 << 0
                   | 1 << 1
                   | 1 << 2
                   | 1 << 3;

    board = make_move({e2, e4}, board);
    board = make_move({e7, e5}, board);
    board = make_move({e1, e2}, board);

    REQUIRE(!w_can_castle_king_side(board));
    REQUIRE(!w_can_castle_queen_side(board));
    REQUIRE(b_can_castle_queen_side(board));
    REQUIRE(b_can_castle_king_side(board));
}

TEST_CASE("Black can not castle if he moves king")
{
    Board board = start_position();
    board.castling = 1 << 0
                   | 1 << 1
                   | 1 << 2
                   | 1 << 3;

    board = make_move({e2, e4}, board);
    board = make_move({e7, e5}, board);
    board = make_move({d2, d3}, board);
    board = make_move({e8, e7}, board);

    REQUIRE(w_can_castle_king_side(board));
    REQUIRE(w_can_castle_queen_side(board));
    REQUIRE(!b_can_castle_queen_side(board));
    REQUIRE(!b_can_castle_king_side(board));
}

TEST_CASE("Black castle")
{
    Board board = start_position();
    board.castling = 1 << 0
                   | 1 << 1
                   | 1 << 2
                   | 1 << 3;

    board = make_move({h2,h3}, board);
    board = make_move({g8,f6}, board);
    board = make_move({h3,h4}, board);
    board = make_move({g7,g6}, board);
    board = make_move({g1,f3}, board);
    board = make_move({f8,g7}, board);
    board = make_move({a2,a4}, board);
    board = make_move({e8,g8}, board);
    REQUIRE(w_can_castle_king_side(board));
    REQUIRE(w_can_castle_queen_side(board));
    REQUIRE(!b_can_castle_queen_side(board));
    REQUIRE(!b_can_castle_king_side(board));
}

TEST_CASE("Legal moves from start position a2")
{
    Board board = start_position();

    // Get all White pieces
    //
    // For each bit,
    //   Get piece type
    //   Count bits in attack set, from this square.

    auto w = white_pieces(board);
    // Find index of first bit
    auto idx = __builtin_ctzll(w);

    auto type = piece_type_on_idx(board, idx);
    REQUIRE(type == Rook);

    auto rays = filter_attack_rays(attack_rays_rook(idx, board), board, board.turn);
    REQUIRE(__builtin_popcountll(rays) == 0);

    // Disable bit we already tested
    w = w&~ (1UL << idx);

    // Get index of next bit
    idx = __builtin_ctzll(w);

    // Get the piece type on index
    type = piece_type_on_idx(board, idx);
    REQUIRE(type == Pawn);

    // Get number of attacks for piece type
    rays = __builtin_popcountll(w_pawns_able_to_push(board) & (1UL << idx))
         + __builtin_popcountll(w_pawns_able_to_double_push(board) & (1UL << idx))
         + __builtin_popcountll(pawn_moves[0][idx] & (black_pieces(board) ^ board.en_passant));

    REQUIRE(rays == 2);

    w = w&~ (1UL<< idx);
    idx = __builtin_ctzll(w);

    type = piece_type_on_idx(board, idx);
    REQUIRE(type == Knight);

    rays = __builtin_popcountll(filter_attack_rays(knight_moves[idx], board, board.turn));

    REQUIRE(rays == 2);
}

TEST_CASE("White is in check")
{
    Board board;
    board.white_king = 1UL << d3;
    board.black_king = 1UL << d5;
    REQUIRE(!is_in_check(board));
}

TEST_CASE("In check after sequence")
{
    Board board = start_position();
    board = make_move({e2,e4}, board);
    board = make_move({a7,a5}, board);
    board = make_move({f1,b5}, board);
    board = make_move({h7,h5}, board);
    board = make_move({b5,d7}, board);
    REQUIRE(is_in_check(board));
    board = make_move({e8,d7}, board);
    REQUIRE(!is_in_check(board));
}

TEST_CASE("Legal moves white from start position")
{
    Board board = start_position();
    REQUIRE(num_legal_moves(board) == 20);
    board = make_move({d2,d4}, board);
    board = make_move({d7,d5}, board);
    REQUIRE(num_legal_moves(board) == 27);
}

// TODO ASSERT correctness
TEST_CASE("prmote to queen")
{
    Board board = start_position();
    board = make_move({a2,a4}, board);
    board = make_move({h7,h6}, board);
    board = make_move({a4,a5}, board);
    board = make_move({h6,h5}, board);
    board = make_move({a5,a6}, board);
    board = make_move({h5,h4}, board);
    board = make_move({a6,b7}, board);
    board = make_move({h4,h3}, board);
    board = make_move({b7,a8, Promo_queen}, board);
    board = make_move({h3,g2}, board);
    board = make_move({a8,a7}, board);
    board = make_move({g2,h1, Promo_rook}, board);
    // print_board(board);
}

// TODO ASSERT correctness
TEST_CASE("fen parser")
{
    std::string fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq";
    auto board = parse_fen(fen);
}

TEST_CASE("Finds checkmate: rnb1k1nr/pppp1ppp/5q2/2b1p3/2B1P3/2N5/PPPP1PPP/R1BQK1NR w KQkq - 4 4 moves d2d3")
{
    std::string fen = "rnb1k1nr/pppp1ppp/5q2/2b1p3/2B1P3/2N5/PPPP1PPP/R1BQK1NR w KQkq - 4 4";
    auto board = parse_fen(fen);
    board = make_move({d2,d3}, board);
    auto move = think(board, 4);
    CHECK_THAT(square_to_str[move.from], Catch::Equals("f6"));
    CHECK_THAT(square_to_str[move.to], Catch::Equals("f2"));
}

#endif


#ifdef BENCH
static void perft4_kiwipete(benchmark::State & state)
{
    std::string fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq";
    auto board = parse_fen(fen);

    for (auto _ : state) {
        fmt::print("{}\n",perft(4, board, 4));
    }
}

static void perft5_kiwipete(benchmark::State & state)
{
    std::string fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq";
    auto board = parse_fen(fen);

    for (auto _ : state) {
        fmt::print("{}\n",perft(5, board, 5));
    }
}

BENCHMARK(perft4_kiwipete)->Unit(benchmark::kMillisecond);
BENCHMARK(perft5_kiwipete)->Unit(benchmark::kSecond);
BENCHMARK_MAIN();
#endif

#if !defined(TESTS) && !defined(BENCH)

Board parse_and_move(Board board, std::string const & to_move)
{
    int from = move_key[to_move.substr(0, 2)];
    int to   = move_key[to_move.substr(2, 2)];

    char promo = 'q';
    if (to_move.length() == 5) {
        promo = to_move.substr(4, 1)[0];
    }

    int prom = Promo_none;
    if (promo == 'q')
        prom = Promo_queen;
    if (promo == 'r')
        prom = Promo_rook;
    if (promo == 'b')
        prom = Promo_bishop;
    if (promo == 'n')
        prom = Promo_knight;

    return make_move({print_mapping[from], print_mapping[to], prom}, board);
}

int main()
{
    bool running = true;

    auto board = start_position();
    Move best_move{};

    std::string line;
    while (running) {
        std::getline(std::cin, line);
        std::stringstream iss(line);
        std::string in;
        iss >> in;
        if (in.compare("uci") == 0) {
            fmt::print("id name Foobar\nid author Andreas\n");
            fmt::print("uciok\n");
        } else if (in.compare("quit") == 0) {
            running = false;
        } else if (in.compare("isready") == 0) {
            fmt::print("readyok\n");
        } else if (in.compare("ucinewgame") == 0) {
            board = start_position();
        } else if (in.compare("position") == 0) {
            if (iss >> in) {
                if (in.compare("startpos") == 0) {
                    board = start_position();
                    if (iss >> in && in.compare("moves") == 0) {
                        std::string to_move;
                        while (iss >> to_move)
                            board = parse_and_move(board, to_move);
                    }
                } else if (in.compare("fen") == 0) {
                    std::string fen;
                    std::string to_move;
                    {
                        char empty;
                        iss >> std::noskipws >> empty;
                    }
                    for (char c; iss >> std::noskipws >> c && c != 'm';)
                        fen.push_back(c);
                    board = parse_fen(fen);
                    iss >> to_move;
                    while (iss >> std::skipws >> to_move)
                        board = parse_and_move(board, to_move);
                }
            }
        } else if (in.compare("go") == 0) {
            best_move = think(board, 4);
            fmt::print("bestmove {}{}\n", square_to_str[best_move.from], square_to_str[best_move.to]);
        } else if (in.compare("stop") == 0) {
            fmt::print("bestmove {}{}\n", square_to_str[best_move.from], square_to_str[best_move.to]);
        } else if (in.compare("print") == 0) {
            print_board(board);
        }
    }

    return 0;
}

#endif
