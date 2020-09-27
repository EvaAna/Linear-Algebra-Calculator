/*DANIEL SENIK*/

#include <iostream>
#include <cmath>
using namespace std;

#define N 5  //max size of matrix

//filling matrix with values
void fillMatrix(int(&x)[N][N], int rows, int columns) {
    cout << "FILL THE MATRIX ROW BY ROW\n";

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < columns; j++) {
            cin >> x[i][j];  //reading input from the user
        }
    }
}

//initialize matrix size
void inputSize(int &rows, int &columns) {
    cout << "INPUT THE MATRIX SIZE (ROWS THEN COLUMNS)\n";

    cin >> rows >> columns;  //reading matrix size from the user
}

//addition of two matrices
void addMatrices(int(&a)[N][N], int(&b)[N][N], int rows, int columns) {
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < columns; j++) {
            a[i][j] = a[i][j] + b[i][j];
        }
    }
}

//subtraction of two matrices
void substractMatrices(int(&a)[N][N], int(&b)[N][N], int rows, int columns) {
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < columns; j++) {
            a[i][j] = a[i][j] - b[i][j];
        }
    }
}

//showing the final matrix that is calculated after calculations
void showResult(int(&x)[N][N], int rows, int columns) {
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < columns; j++) {
            cout << x[i][j] << " ";
        }

        cout << "\n";
    }
}

//check matrices for correct dimensions
bool matrixDimensions(int &rows, int &columns) {
    return ((rows >= 1 && rows <= N) && (columns >= 1 && columns <= N));
}

//multiplication of two matrices
void multiplyMatrices(int(&a)[N][N], int(&b)[N][N], int(&m)[N][N], int rows_a, int columns_a, int rows_b, int columns_b) {
    int sum = 0, mul = 0;

    for (int i = 0; i < rows_a; i++) {  //going through every row component of the first matrix
        for (int j = 0; j < columns_b; j++) {  //going through every column component of the second matrix
            for (int k = 0; k < columns_a; k++) {
                mul = a[i][k] * b[k][j];  //multiplication of matrices
                sum = sum + mul;  //adding results to corresponding cell of the final matrix
            }

            m[i][j] = sum;  //the final matrix has the same number of rows as the first matrix, and the same number of columns as the second matrix 
            sum = 0;
        }
    }
}

void getCofactor(int a[N][N], int temp[N][N], int p, int q, int rows) {
    int i = 0, j = 0;

    for (int row = 0; row < rows; row++) {
        for (int col = 0; col < rows; col++) {
            if (row != p && col != q) {  //copying into temporary matrix only those element which are not in given row and column
                temp[i][j++] = a[row][col];

                if (j == rows - 1) {
                    j = 0;
                    i++;
                }
            }
        }
    }
}

//recursive function for finding determinant of matrix
double determinant(int(&x)[N][N], int rows, int columns) {
    double det = 0;
    int temp[N][N], sign = 1;

    if (rows == 1) return x[0][0];  //base case : if matrix contains single element

    for (int i = 0; i < rows; i++) { //iterate for each element of first row 
        getCofactor(x, temp, 0, i, rows);
        det += sign * x[0][i] * determinant(temp, rows - 1, columns);
        sign = -sign;
    }

    return det;
}

//calculate transpose matrix of an original matrix
void transpose(int(&a)[N][N], int(&b)[N][N], int rows, int columns) {
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < columns; j++) {
            b[j][i] = a[i][j];  //the rows of the initial array become the columns of the final array
        }
    }
}

//calculate inverse matrix of an original matrix
void inverse(int(&a)[N][N], int rows) {
    int z, x, r = 0, c = 0, temp[N][N], trans[N][N];
    double det, adj[N][N];

    if ((det = determinant(a, rows, rows)) != 0) {
        transpose(a, trans, rows, rows);

        for (int i = 0; i < rows; i++) {  //iterate for each element of the transpose matrix 
            z = i;

            for (int j = 0; j < rows; j++) {
                x = j;

                for (int k = 0; k < rows; k++) {  //searching elements that are not in the same row and column with the future cofactor  
                    for (int p = 0; p < rows; p++) {
                        if (z != k && x != p) {
                            temp[r][c] = trans[k][p];  //copying the element of the transpose matrix to a temporary array 

                            if (c == rows - 2) {  //when reaching the final possible number of columns the counter starts over 
                                r++;
                                c = 0;
                            } else c++;  //if the maximum number of possible columns isn't reached we move to the next one
                        }
                    }
                }

                r = 0;
                adj[i][j] = determinant(temp, rows - 1, rows - 1);  //the elements of the temporary array are used to calculate the determinant of the corresponding cofactor

                if (((i + j) % 2) == 1) adj[i][j] = -adj[i][j];  //if the (i + j) is an odd number the cofactor must be multiplied with -1
            }
        }

        cout << "RESULT\n";

        for (int i = 0; i < rows; i++) {  //printing the final result
            for (int j = 0; j < rows; j++) {
                cout << adj[i][j] << "/" << det << " ";  //inverse matrix is the adjugate matrix divided by the determinant
            }

            cout << "\n";
        }
    } else cout << "THE INVERSE MATRIX CAN'T BE CALCULATED\n";
}

void linearEquation(int(&a)[N][N], int(&b)[N][N], int rows) {
    int counter = 0, zeros = 0, temp[N] = { 0 };
    double det[N];

    det[counter] = determinant(a, rows, rows);

    for (int i = 0; i < rows; i++) {  //cramer's method of solving linear equations
        counter++;

        for (int j = 0; j < rows; j++) {
            temp[j] = a[j][i];
            a[j][i] = b[j][0];
        }

        det[counter] = determinant(a, rows, rows);

        for (int j = 0; j < rows; j++) a[j][i] = temp[j];
    }

    if (det[0] == 0) {
        for (int i = 0; i < rows; i++) {
            if (det[i + 1] == 0) zeros++;
        }

        if (zeros == rows) cout << "THE LINEAR EQUATION IS INDEFINITE\n";
        else cout << "THE LINEAR EQUATION IS IMPOSSIBLE TO BE SOLVED\n";
    } else {
        for (int i = 0; i < rows; i++) cout << "X" << i << " = " << det[i + 1] << " / " << det[0] << "\n";
    }
}

//calculating rank of matrix
int matrixRank(double(&c)[N][N], int rows, int columns, int rank) {
    int zeros;

    for (int i = 0; i < rows; i++) {
        zeros = 0;

        for (int j = 0; j < columns; j++) {
            if (c[i][j] == 0.0) zeros++;
        }

        if (zeros == columns) rank--;
    }

    return rank;
}

//calculate row echelon form of matrix
void rowEchelonForm(int(&a)[N][N], double(&c)[N][N], int rows, int columns, int &rank) {
    bool flag = false, change = false;
    int index, num1, num2, element, temp[N], zeros_up, zeros_down;

    if (columns > rows) {
        swap(rows, columns);
        change = true;
    }

    for (int i = 0; i < rows; i++) {  //loop for traversing the diagonal elements
        index = i;

        while (a[index][i] == 0 && index < rows) index++;  //searching the index which has non zero value

        if (index == rows) continue;  //if there is non zero element   

        if (index != i) {
            for (int j = 0; j < rows; j++) swap(a[index][j], a[i][j]);  //loop for swaping the diagonal element row and index row
        }

        for (int j = 0; j < rows; j++) temp[j] = a[i][j];  //storing the values of diagonal row elements

        for (int j = i + 1; j < rows; j++) {  //traversing every row below the diagonal element
            num1 = temp[i];  //value of diagonal element   
            num2 = a[j][i];  //value of next row element   

            for (int k = 0; k < rows; k++) {  //traversing every column of row and multiplying to every row   
                a[j][k] = (num1 * a[j][k]) - (num2 * temp[k]);  //multiplying to make the diagonal element and next row element equal
            }
        }
    }

    if (change) swap(rows, columns);

    for (int i = 0; i < rows - 1; i++) {  //if after elimination we have a zero row not in the last row, then we move the non zero row up, and the zero row down
        zeros_up = 0;
        zeros_down = 0;

        for (int j = 0; j < columns; j++) {
            if (a[i][j] == 0) zeros_up++;  //counting the amount of zero's in one row
            if (a[i + 1][j] == 0) zeros_down++;  //counting the amount of zero's in the exactly second row
        }

        if (zeros_up == columns && zeros_down != columns) {  //if a row is full of zero's and the exactly second row isn't, we swap those two rows
            for (int n = 0; n < columns; n++) {
                a[i][n] = a[i + 1][n];
                a[i + 1][n] = 0;
            }
        }
    }

    for (int i = 0; i < rows; i++) {  //copying the elements of a matrix into c matrix 
        for (int j = 0; j < columns; j++) {
            c[i][j] = (double)a[i][j];
        }
    }

    rank = rows;  //the matrix rank initializes with the maximum possible value, then it is going to be decreased if needed

    cout << "ROW ECHELON FORM\n";

    for (int i = 0; i < rows; i++) {  //checking each element of every row
        element = 0;

        for (int j = 0; j < columns; j++) {  //checking each element of every column
            if (a[i][j] != 0 && flag == false) {  //find an element that is non zero 
                element = a[i][j];
                flag = true;  //if element is found it remains the same for the whole row
            }

            if (element != 0) {
                c[i][j] = (double)a[i][j] / element;

                if (a[i][j] % element == 0) cout << a[i][j] / element << " ";  //if a row element is divided exactly with the pivot element of that row, we output the result of the division 
                else cout << a[i][j] << "/" << element << " ";  //divide every number of the row with the pivot element to get the row echelon form

                a[i][j] /= element;
            } else {
                cout << "0 ";  //if a row element is 0 we just output 0
                a[i][j] = 0;
                c[i][j] = 0.0;
            }
        }

        flag = false;
        cout << "\n";
    }
}

//transforms a row echelon form matrix into reduced row echelon form
void reducedRowEchelonForm(double(&c)[N][N], int rows, int columns) {
    int current_column, max_row = rows - 1, count = rows - 1;
    double abovePivotElement, temp[N] = { 0 };
    bool flag = false;

    for (int i = max_row; i > 0; i--) {  //starting the elimination from the last row to make sure all the elements of the matrix above the pivot remain 0 after every procedure
        for (int j = 0; j < columns; j++) {  //checking every row's column if the current element is non zero
            temp[j] = c[i][j];  //copying the last row elements to a temporary array 

            if (temp[j] != 0.0 && !flag) {
                current_column = j;  //storing the value of the column that the first non zero element found
                flag = true;
            }
        }

        if (flag) {
            for (int n = 0; n < count; n++) {  //decrement every row by the elements of the temporary array 
                abovePivotElement = c[n][current_column];

                for (int k = current_column; k < columns; k++) c[n][k] = c[n][k] - abovePivotElement * temp[k];  //start decrementing from the point non zero element found
            }
        }

        count--;
        flag = false;
    }

    cout << "REDUCED ROW ECHELON FORM\n";

    for (int i = 0; i < rows; i++) {  //showing result
        for (int j = 0; j < columns; j++) {
            cout << c[i][j] << " ";
        }

        cout << "\n";
    }
}

//filling vectors with values
void fillVectors(int(&x)[N][N], int(&y)[N][N], int columns) {
    cout << "ENTER THE ELEMENTS OF THE FIRST VECTOR\n";

    for (int i = 0; i < columns; i++) cin >> x[0][i];

    cout << "ENTER THE ELEMENTS OF THE SECOND VECTOR\n";

    for (int i = 0; i < columns; i++) cin >> y[0][i];
}

//calculate product of vectors
int vectorProduct(int(&a)[N][N], int(&b)[N][N], int &columns) {
    int sum = 0;

    for (int i = 0; i < columns; i++) sum += a[0][i] * b[0][i];

    return sum;
}

//calculate length of vector
int vectorLength(int(&a)[N][N], int &columns) {
    int sum = 0;

    for (int i = 0; i < columns; i++)  sum += pow(a[0][i], 2);

    return sum;
}

//calculate vector distance
int vectorDistance(int(&a)[N][N], int(&b)[N][N], int &columns) {
    int sum = 0;

    for (int i = 0; i < columns; i++) sum += pow(a[0][i] - b[0][i], 2);

    return sum;
}

int main(void) {
    int operation, rows = 0, columns = 0, rank, current_column, plus, j = 0, rows_a = 0, columns_a = 0, rows_b = 0, columns_b = 0, zeros = 0;
    int a[N][N] = { 0 }, b[N][N] = { 0 }, m[N][N] = { 0 };
    double c[N][N] = { 0 };  //used to store the results of calculations in decimals
    bool flag;

    cout << "SELECT FROM THE FOLLOWING OPERATIONS:\n\n"
        << "1 MATRIX ADDITION\n"
        << "2 MATRIX SUBTRACTION\n"
        << "3 MATRIX MULTIPLICATION\n"
        << "4 MATRIX DETERMINANT\n"
        << "5 MATRIX TRANSPOSE\n"
        << "6 INVERSE MATRIX\n"
        << "7 LINEAR EQUATION\n"
        << "8 ROW ECHELON FORM\n"
        << "9 REDUCED ROW ECHELON FORM\n"
        << "10 MATRIX RANK\n"
        << "11 ORTHOGONALITY CHECK BETWEEN TWO VECTORS\n"
        << "12 LENGTH OF A VECTOR\n"
        << "13 DISTANCE BETWEEN TWO VECTORS\n"
        << "14 ANGLE BETWEEN TWO VECTORS\n"
        << "15 COMPONENT OF V2 ALONG V1\n"
        << "16 VECTOR PROJECTION OF V2 ALONG V1\n\n"
        << "TYPE THE NUMBER OF OPERATION\n";

    cin >> operation;

    switch (operation) {
        case 1:
            cout << "TYPE THE SIZE OF THE MATRICES(ROWS THEN COLUMNS)\n";
            cin >> rows >> columns;

            if (matrixDimensions(rows, columns)) {
                fillMatrix(a, rows, columns);
                fillMatrix(b, rows, columns);
                addMatrices(a, b, rows, columns);
                cout << "RESULT\n";
                showResult(a, rows, columns);
            } else cout << "WRONG INPUT\n";
            break;
        case 2:
            cout << "TYPE THE SIZE OF THE MATRICES(ROWS THEN COLUMNS)\n";
            cin >> rows >> columns;

            if (matrixDimensions(rows, columns)) {
                fillMatrix(a, rows, columns);
                fillMatrix(b, rows, columns);
                substractMatrices(a, b, rows, columns);
                cout << "RESULT\n";
                showResult(a, rows, columns);
            } else cout << "WRONG INPUT\n";
            break;
        case 3:
            cout << "TYPE THE SIZE OF THE FIRST MATRIX (ROWS THEN COLUMNS)\n";
            cin >> rows_a >> columns_a;
            cout << "TYPE THE SIZE OF THE SECOND MATRIX (ROWS THEN COLUMNS)\n";
            cin >> rows_b >> columns_b;

            if (matrixDimensions(rows_a, columns_a) && matrixDimensions(rows_b, columns_b) && columns_a == rows_b) {  //check if the number of rows of the first matrix is equal to the number of columns of the second matrix
                fillMatrix(a, rows_a, columns_a);
                fillMatrix(b, rows_b, columns_b);
                multiplyMatrices(a, b, m, rows_a, columns_a, rows_b, columns_b);
                cout << "RESULT\n";
                showResult(m, rows_a, columns_b);
            } else cout << "THE MULTIPLICATION CAN'T BE DONE\n";
            break;
        case 4:
            inputSize(rows, columns);

            if (matrixDimensions(rows, columns) && rows == columns) {
                fillMatrix(a, rows, columns);
                cout << "RESULT\n" << determinant(a, rows, columns) << "\n";
            } else cout << "THE DETERMINANT CAN'T BE CALCULATED\n";
            break;
        case 5:
            inputSize(rows, columns);

            if (matrixDimensions(rows, columns)) {
                fillMatrix(a, rows, columns);
                transpose(a, b, rows, columns);
                cout << "RESULT\n";
                showResult(b, columns, rows);
            } else cout << "WRONG SIZE OF THE MATRIX\n";
            break;
        case 6:
            inputSize(rows, columns);

            if (matrixDimensions(rows, columns) && rows == columns) {
                fillMatrix(a, rows, columns);
                inverse(a, rows);
            } else cout << "WRONG SIZE OF THE MATRIX\n";
            break;
        case 7:
            inputSize(rows, columns);

            if (matrixDimensions(rows, columns) && rows == columns) {
                fillMatrix(a, rows, columns);
                cout << "ENTER THE CONSTANT TERMS\n";
                for (int i = 0; i < rows; i++) cin >> b[i][0];
                linearEquation(a, b, rows);
            } else cout << "THE EQUATION CAN'T BE SOLVED\n";
            break;
        case 8:
            inputSize(rows, columns);

            if (matrixDimensions(rows, columns)) {
                fillMatrix(a, rows, columns);
                rowEchelonForm(a, c, rows, columns, rank);
            } else cout << "WRONG SIZE OF THE MATRIX\n";
            break;
        case 9:
            inputSize(rows, columns);

            if (matrixDimensions(rows, columns)) {
                fillMatrix(a, rows, columns);
                rowEchelonForm(a, c, rows, columns, rank);
                reducedRowEchelonForm(c, rows, columns);
            } else cout << "WRONG SIZE OF THE MATRIX\n";
            break;
        case 10:
            inputSize(rows, columns);

            if (matrixDimensions(rows, columns)) {
                fillMatrix(a, rows, columns);
                rowEchelonForm(a, c, rows, columns, rank);
                cout << "RESULT\n" << matrixRank(c, rows, columns, rank) << "\n";
            } else cout << "WRONG SIZE OF THE MATRIX\n";
            break;
        case 11:
            cout << "ENTER SIZE OF THE VECTORS\n";
            cin >> columns;

            if (columns > 0) {
                fillVectors(a, b, columns);
                if (vectorProduct(a, b, columns) == 0) cout << "THE VECTORS ARE ORTHOGONAL\n";
                else cout << "THE VECTORS ARE NOT ORTHOGONAL\n";
            } else cout << "WRONG SIZE OF VECTOR\n";
            break;
        case 12:
            cout << "ENTER SIZE OF THE VECTOR\n";
            cin >> columns;

            if (columns > 0) {
                cout << "ENTER THE ELEMENTS OF THE VECTOR\n";
                for (int i = 0; i < columns; i++) cin >> a[0][i];
                cout << "THE LENGTH OF THE VECTOR IS SQUARE ROOT OF (" << vectorLength(a, columns) << ")\n";
            } else cout << "WRONG SIZE OF VECTOR\n";
            break;
        case 13:
            cout << "ENTER SIZE OF THE VECTORS\n";
            cin >> columns;

            if (columns > 0) {
                fillVectors(a, b, columns);
                cout << "THE DISTANCE BETWEEN THE VECTORS IS SQUARE ROOT OF (" << vectorDistance(a, b, columns) << ")\n";
            } else cout << "WRONG SIZE OF VECTOR\n";
            break;
        case 14:
            cout << "ENTER SIZE OF THE VECTORS\n";
            cin >> columns;

            if (columns > 0) {
                fillVectors(a, b, columns);
                cout << "THE ANGLE BETWEEN THE TWO VECTORS IS COSINUS OF "
                    << vectorProduct(a, b, columns)
                    << " DIVIDED BY SQUARE ROOT OF "
                    << vectorLength(a, columns)
                    << " * SQUARE ROOT OF "
                    << vectorLength(b, columns) << "\n";
            } else cout << "WRONG SIZE OF VECTOR\n";
            break;
        case 15:
            cout << "ENTER SIZE OF THE VECTORS\n";
            cin >> columns;

            if (columns > 0) {
                fillVectors(a, b, columns);
                cout << "THE COMPONENT OF V2 ALONG V1 IS " << vectorProduct(a, b, columns) << " DIVIDED BY SQUARE ROOT OF " << vectorLength(a, columns) << "\n";
            } else cout << "WRONG SIZE OF VECTOR\n";
            break;
        case 16:
            cout << "ENTER SIZE OF THE VECTORS\n";
            cin >> columns;

            if (columns > 0) {
                fillVectors(a, b, columns);
                cout << "THE VECTOR PROJECTION OF V2 ALONG V1 IS " << vectorProduct(a, b, columns) << " / " << vectorLength(a, columns) << " * [ ";
                for (int i = 0; i < columns; i++) cout << a[0][i] << " ";
                cout << "]\n";
            } else cout << "WRONG SIZE OF VECTOR\n";
            break;
        default:
            cout << "WRONG OPERATION SELECTED\n";
    }

    cin.ignore();
    getchar();

    return 0;
}
