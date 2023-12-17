#include <string.h>
#include <NumCpp.hpp>
#include <algorithm>

typedef nc::NdArray<bool>* board;

struct RenderResult {
    uint8_t* text;
    uint32_t pop;
};

nc::Slice safe_slice(int start, int end, int maximum) {
    return nc::Slice(std::max(start, 0), std::min(end, maximum));
}

RenderResult* render_board(board board) {
    nc::NdArray<bool> ncarray = *board;

    int newl_size = 1;
    int null_size = 1;

    auto display_char = u8"█";
    int char_size = std::char_traits<char8_t>::length(display_char);

    size_t arraySize = (ncarray.numRows() * ncarray.numCols() * char_size) +
                       (ncarray.numRows() * newl_size) + null_size;

    uint8_t* text = (uint8_t*)malloc(arraySize * sizeof(uint8_t));

    auto i = 0;
    for (auto row = 0; row < ncarray.numRows(); ++row) {
        for (auto col = 0; col < ncarray.numCols(); ++col) {
            if (ncarray(row, col)) {
                for (auto b = 0; b < char_size; b++) {
                    text[i] = display_char[b];
                    i++;
                }
            } else {
                text[i] = u8' ';
                i++;
            }
        }
        text[i] = u8'\n';
        i++;
    }
    text[i] = '\0';

    return new RenderResult{text, nc::count_nonzero(ncarray).item()};
}

void resize_board(board old_arr_ptr, int rows, int cols) {
    auto old_arr = *old_arr_ptr;
    (*old_arr_ptr) = old_arr.resizeSlow(rows, cols);
}

void clear_board(board old_arr_ptr) {
    (*old_arr_ptr) = nc::zeros_like<bool>(*old_arr_ptr);
}

void step_board(board old_arr_ptr) {
    auto old_arr = *old_arr_ptr;

    auto old_arr_rows = old_arr.numRows();
    auto old_arr_cols = old_arr.numCols();

    auto ncarray = nc::zeros<bool>(old_arr_rows, old_arr_cols);

    for (auto row = 0; row < old_arr_rows; ++row) {
        for (auto col = 0; col < old_arr_cols; ++col) {
            auto row_w = nc::Slice(std::max(row - 1, 0),
                                   std::min(row + 2, (int)old_arr_rows));

            auto col_w = nc::Slice(std::max(col - 1, 0),
                                   std::min(col + 2, (int)old_arr_cols));

            auto neighbours = nc::count_nonzero(old_arr(row_w, col_w)).item() -
                              old_arr(row, col);

            bool alive = old_arr(row, col);

            if (alive) {
                if (neighbours == 2 || neighbours == 3) {
                    ncarray(row, col) = true;
                }
            } else {
                if (neighbours == 3) {
                    ncarray(row, col) = true;
                }
            }
        }
    }

    *old_arr_ptr = ncarray;
}

nc::NdArray<uint8_t> smooth_uint8_ndarray(nc::NdArray<uint8_t> arr) {
    auto arr_rows = arr.numRows();
    auto arr_cols = arr.numCols();

    for (auto row = 0; row < arr_rows; ++row) {
        for (auto col = 0; col < arr_cols; ++col) {
            auto row_w = nc::Slice(std::max(row - 1, 0),
                                   std::min(row + 2, (int)arr_rows));

            auto col_w = nc::Slice(std::max(col - 1, 0),
                                   std::min(col + 2, (int)arr_cols));

            auto av = nc::average(arr(row_w, col_w)).astype<uint8_t>();
            arr(row, col) = av.item();
        }
    }

    return arr;
}

// Very slow implementation
void cull_boring_patterns(board arr_ptr) {
    auto arr = *arr_ptr;

    arr = nc::zeros<bool>(arr.numRows() + 2, arr.numCols() + 2)
              .put({1, -1}, {1, -1}, arr);

    auto arr_rows = arr.numRows();
    auto arr_cols = arr.numCols();

    // clang-format off
    // https://conwaylife.com/wiki/Still_life#Enumerating_still_lifes
    auto block = 
        nc::NdArray({{0, 0, 0, 0}, 
                     {0, 1, 1, 0}, 
                     {0, 1, 1, 0}, 
                     {0, 0, 0, 0}}).astype<bool>();
        
    auto bee_hive = 
        nc::NdArray({{0, 0, 0, 0, 0}, 
                     {0, 0, 1, 0, 0}, 
                     {0, 1, 0, 1, 0}, 
                     {0, 1, 0, 1, 0}, 
                     {0, 0, 1, 0, 0}, 
                     {0, 0, 0, 0, 0}}).astype<bool>();
    
    auto loaf = 
        nc::NdArray({{0, 0, 0, 0, 0, 0}, 
                     {0, 0, 1, 1, 0, 0}, 
                     {0, 1, 0, 0, 1, 0}, 
                     {0, 0, 1, 0, 1, 0}, 
                     {0, 0, 0, 1, 0, 0}, 
                     {0, 0, 0, 0, 0, 0}}).astype<bool>();

    auto boat = 
        nc::NdArray({{0, 0, 0, 0, 0}, 
                     {0, 1, 1, 0, 0}, 
                     {0, 1, 0, 1, 0}, 
                     {0, 0, 1, 0, 0}, 
                     {0, 0, 0, 0, 0}}).astype<bool>();
    
    auto tub = 
        nc::NdArray({{0, 0, 0, 0, 0}, 
                     {0, 0, 1, 0, 0}, 
                     {0, 1, 0, 1, 0}, 
                     {0, 0, 1, 0, 0}, 
                     {0, 0, 0, 0, 0}}).astype<bool>();

    auto pond = 
        nc::NdArray({{0, 0, 0, 0, 0, 0}, 
                     {0, 0, 1, 1, 0, 0}, 
                     {0, 1, 0, 0, 1, 0}, 
                     {0, 1, 0, 0, 1, 0}, 
                     {0, 0, 1, 1, 0, 0}, 
                     {0, 0, 0, 0, 0, 0}}).astype<bool>();

    // boring oscillator
    auto blinker = 
        nc::NdArray({{0, 0, 0, 0, 0}, 
                     {0, 1, 1, 1, 0}, 
                     {0, 0, 0, 0, 0}}).astype<bool>();
    // clang-format on

    nc::NdArray<bool> boring_patterns[] = {block,
                                           bee_hive,
                                           nc::rot90(bee_hive),
                                           loaf,
                                           nc::rot90(loaf),
                                           nc::rot90(loaf, 2),
                                           nc::rot90(loaf, 3),
                                           boat,
                                           nc::rot90(boat),
                                           nc::rot90(boat, 2),
                                           nc::rot90(boat, 3),
                                           tub,
                                           pond,
                                           blinker,
                                           nc::rot90(blinker)};

    for (auto row = 0; row < arr_rows; ++row) {
        for (auto col = 0; col < arr_cols; ++col) {
            for (auto& pattern : boring_patterns) {
                auto row_w =
                    safe_slice(row, row + (int)pattern.numRows(), arr_rows);
                auto col_w =
                    safe_slice(col, col + (int)pattern.numCols(), arr_cols);

                if (nc::array_equal(arr(row_w, col_w), pattern)) {
                    arr.put(row_w, col_w, nc::zeros_like<bool>(pattern));
                };
            }
        }
    }

    (*arr_ptr) = arr({1, -1}, {1, -1});
}

void randomize_board(board old_arr_ptr) {
    auto old_arr = *old_arr_ptr;

    auto old_arr_rows = old_arr.numRows();
    auto old_arr_cols = old_arr.numCols();

    auto newArr =
        nc::random::randInt<uint8_t>({old_arr_rows, old_arr_cols}, 0, 255);

    newArr = smooth_uint8_ndarray(newArr);

    auto tav = nc::average(newArr).astype<uint8_t>();

    for (auto row = 0; row < old_arr_rows; ++row) {
        for (auto col = 0; col < old_arr_cols; ++col) {
            newArr(row, col) = newArr(row, col) - 27 > tav.item() ? 1 : 0;
        }
    }

    auto newArrBool = newArr.astype<bool>();

    for (int i = 0; i < 7; i++) {
        step_board(&newArrBool);
    }

    cull_boring_patterns(&newArrBool);

    *old_arr_ptr = newArrBool;
}
