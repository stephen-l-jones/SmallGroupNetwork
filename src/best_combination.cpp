#include <algorithm>
#include <cmath>
#include <iostream>
#include <numeric>
#include <Rcpp.h>
#include <utility>
#include <vector>

using namespace Rcpp;
using namespace std;

// Fork of GNU next_permutation. Iterates a pointer vector instead of the vector itself.
bool next_permutation_ptr(int** begin, int** end) {
  if (begin == end) return false;
  int** i = begin;
  ++i;
  if (i == end) return false;
  
  i = end;
  --i;
  while(true) {
    int** j = i;
    --i;
    if (**i < **j) {
      int** k = end;
      while (!(**i < **--k)) {}
      iter_swap(i, k);
      reverse(j, end);
      return true;
    }
    if (i == begin) {
      reverse(begin, end);
      return false;
    }
  }
}

vector<int> make_index(vector<int> v, int n) {
  vector<int> index(n * 2);
  for (int i = 0; i < n; i++) 
    index[i] = v[i];
  for (int i = 0; i < n; i++)
    index[i + n] = i;
  return index;
}

vector<int*> make_permuter(vector<int> v, int n, int* index_ptr) {
  vector<int*> permuter(n * 2);
  for (int i = 0; i < n * 2; i++)
    permuter[i] = index_ptr + i;
  return permuter;
}

vector<int> best_combination_prod(vector<double> w, vector<double> f, 
                                  vector<int> v, bool loops) {
  int n = v.size();
  vector<int>  index(make_index(v, n));
  vector<int*> permuter(make_permuter(v, n, index.data()));
  
  double score, best_score;
  double* best_score_ptr = nullptr;
  int row_index, col_index;
  vector<int> best_combo(n);
  do {
    score = 0;
    for (int i = 0; i < n; i++) {
      col_index = *(permuter[i] + n);
      for (int j = 0; j < n; j++) {
        if (!loops && (i == j)) continue;
        row_index = *(permuter[j] + n);
        score += f[col_index * n + row_index] * w[i * n + j];
      }
    }
    if (best_score_ptr == nullptr || best_score < score) {
      best_score     = score;
      best_score_ptr = &best_score;
      for (int i = 0; i < n; i++) {
        best_combo[i] = *(permuter[i] + n);
      }
    }
  } while(next_permutation_ptr(&permuter[0], &permuter[n]));
  return best_combo;
}

vector<int> best_combination_absdiff(vector<double> w, vector<double> f, 
                                          vector<int> v, bool loops, 
                                          vector<int> w_post, vector<int> f_post) {
  int n = v.size();
  vector<int>  index(make_index(v, n));
  vector<int*> permuter(make_permuter(v, n, index.data()));
  
  double score, best_score;
  double* best_score_ptr = nullptr;
  int row_index, col_index;
  vector<int> best_combo(n);
  do {
    score = 0;
    for (int i = 0; i < n; i++) {
      col_index = *(permuter[i] + n);
      for (int j = 0; j < n; j++) {
        if (!loops && (i == j)) continue;
        row_index = *(permuter[j] + n);
        score += abs(f[col_index * n + row_index] - w[i * n + j]) * 
          f_post[col_index * n + row_index] * w_post[i * n + j];
      }
    }
    if (best_score_ptr == nullptr || best_score > score) {
      best_score     = score;
      best_score_ptr = &best_score;
      for (int i = 0; i < n; i++) {
        best_combo[i] = *(permuter[i] + n);
      }
    }
  } while(next_permutation_ptr(&permuter[0], &permuter[n]));
  return best_combo;
}

// [[Rcpp::export(".best_combination")]]
IntegerVector best_combination(NumericVector Rw, NumericVector Rf, IntegerVector Rv, 
                               bool loops, bool prod) {
  vector<int> w_post(Rw.size(), 1), f_post(Rf.size(), 1);
  for (int i = 0; i < Rw.size(); i ++) {
    if (NumericVector::is_na(Rw[i])) {
      Rw[i]     = 0;
      w_post[i] = 0;
    }
  }
  for (int i = 0; i < Rf.size(); i ++) {
    if (NumericVector::is_na(Rf[i])) {
      Rf[i]     = 0;
      f_post[i] = 0;
    }
  }
  vector<double> w = as<vector<double>>(Rw);
  vector<double> f = as<vector<double>>(Rf);
  vector<int>    v = as<vector<int>>(Rv);
  
  vector<int> best_combo;
  if (prod) {
    best_combo = best_combination_prod(w, f, v, loops);
  } else {
    best_combo = best_combination_absdiff(w, f, v, loops, w_post, f_post);
  }
  IntegerVector return_combo = IntegerVector(best_combo.begin(), best_combo.end());
  return_combo = return_combo + 1;
  return return_combo;
}

// [[Rcpp::export]]
void test_rcpp() {
  vector<double> w = {0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 0, 1.6};
  vector<double> f = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0};
  vector<int> w_post = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
  vector<int> f_post = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
  vector<int> v = {1, 1, 2, 3};
  vector<int> x = best_combination_prod(w, f, v, false);
  for (unsigned i = 0; i < x.size(); i++)
    Rcout << x[i] << " ";
  Rcout << "\n";
  vector<int> y = best_combination_absdiff(w, f, v, false, w_post, f_post);
  for (unsigned i = 0; i < y.size(); i++)
    Rcout << y[i] << " ";
  Rcout << "\n";
}

