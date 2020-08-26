#include <gravity.h>

// [[Rcpp::plugins("cpp11")]]

struct EdgeType {
  int id1;
  int id2;
};

// [[Rcpp::export]]
NumericMatrix build_network(NumericMatrix locs, NumericVector weights,
                            int num_loc_candidate, unsigned int seed, 
                            double min_x = -87.78555, double min_y = 24.46990,
                            int steps = 2) {
  
  // Build network among locs based on weights
  int nlocs = locs.nrow();
  vector<EdgeType*> edges;
  mt19937 rng(seed);
  min_x_center = min_x;
  min_y_center = min_y;
  
  vector<LocationType*> locs_obj = to_locs_obj(locs, weights, weights);
  vector<LocationType*> locs_candidate = locs_obj;
  shuffle(locs_obj.begin(), locs_obj.end(), rng);
  
  Rcout << "Starting to loop\n";
  for (int i = 0; i < nlocs; i++) {
    if ((i+1) % 1000 == 0) Rcout << "Cycling through locs " << i+1 << "\n";
    LocationType* l = locs_obj[i];
    PtsType* p = new PtsType();
    p->id = l->id;
    p->x = l->x;
    p->y = l->y;
    
    if (l->capacity == 0) continue;
    if (l->capacity > locs_candidate.size()) continue;
    const int pxi = x_to_col_num(p->x);
    const int pyi = y_to_row_num(p->y);
    vector<LocationType*> chosen;
    
    if (locs_candidate.size() <= num_loc_candidate) {
      locs_candidate.erase(std::remove(locs_candidate.begin(), locs_candidate.end(), l));
      chosen = choose_mult_loc(p, locs_candidate, l->capacity, rng);
    } else {
      locs_candidate.erase(std::remove(locs_candidate.begin(), locs_candidate.end(), l));
      vector<LocationType*> nearby_places = get_nearby_places2(pxi, pyi, locs_candidate, 
                                                               num_loc_candidate, steps);
      chosen = choose_mult_loc(p, nearby_places, l->capacity, rng);
    }
    
    l->capacity = 0;
    
    for (unsigned int j = 0; j < chosen.size(); j++) {
      EdgeType* e = new EdgeType();
      e->id1 = l->id;
      e->id2 = chosen[j]->id;
      
      edges.push_back(e);
      
      chosen[j]->weight--;
      chosen[j]->capacity--;
      if (chosen[j]->capacity == 0) {
        locs_candidate.erase(std::remove(locs_candidate.begin(), locs_candidate.end(), chosen[j]));
      }
    }
    
  }
  
  NumericMatrix out(edges.size(), 2);
  
  for (unsigned int k = 0; k < edges.size(); k++) {
    out(k, 0) = edges[k]->id1;
    out(k, 1) = edges[k]->id2;
  }
  
  return(out);
}

// [[Rcpp::export]]
NumericMatrix assign_by_gravity(NumericMatrix pts, NumericMatrix locs, NumericVector weights,
                                int num_loc, unsigned int seed, 
                                double min_x = -87.78555, double min_y = 24.46990,
                                int steps = 2, bool use_capacity = false) {
  
  // Choose one location per pts
  int npts = pts.nrow();
  int nlocs = locs.nrow();
  mt19937 rng(seed);
  // NumericVector out(npts);
  NumericMatrix out(npts, 2);
  min_x_center = min_x;
  min_y_center = min_y;
  
  vector<LocationType*> locs_obj = to_locs_obj(locs, weights, weights);
  vector<PtsType*> pts_obj = to_pts_obj(pts);
  shuffle(pts_obj.begin(), pts_obj.end(), rng);
  
  for (int i = 0; i < npts; i++) {
    if ((i+1) % 10000 == 0) Rcout << "Assigning point " << i+1 << "\n";
    PtsType* p = pts_obj[i];
    const int pxi = x_to_col_num(p->x);
    const int pyi = y_to_row_num(p->y);
    
    vector<LocationType*> nearby_places = get_nearby_places2(pxi, pyi, locs_obj, num_loc, steps);
    LocationType* chosen = choose_one_loc(p, nearby_places, rng);
    
    if (use_capacity) {
      chosen->capacity--;
      if (chosen->capacity == 0) {
        locs_obj.erase(std::remove(locs_obj.begin(), locs_obj.end(), chosen));
        if ((nlocs - locs_obj.size()) % 10000 == 0) {
          Rcout << (nlocs - locs_obj.size()) << " locations have been filled to purported capacity\n";
        }
      }
    }
    
    out(i, 0) = p->id;
    out(i, 1) = chosen->id;
  }
  
  return(out);
}

// [[Rcpp::export]]
NumericMatrix assign_by_gravity2(NumericMatrix pts, NumericMatrix locs, NumericVector weights,
                                 int num_loc_choose, int num_loc_candidate, unsigned int seed, 
                                 double min_x = -87.78555, double min_y = 24.46990,
                                 int steps = 2, bool use_capacity = false, 
                                 bool replace = false) {
  
  // Choose multiple location per pts
  int npts = pts.nrow();
  // int nlocs = locs.nrow();
  mt19937 rng(seed);
  NumericMatrix out(npts, num_loc_choose+1);
  min_x_center = min_x;
  min_y_center = min_y;
  
  vector<LocationType*> locs_obj = to_locs_obj(locs, weights, weights);
  vector<PtsType*> pts_obj = to_pts_obj(pts);
  shuffle(pts_obj.begin(), pts_obj.end(), rng);
  
  for (int i = 0; i < npts; i++) {
    if ((i+1) % 1000 == 0) Rcout << "Assigning point " << i+1 << "\n";
    PtsType* p = pts_obj[i];
    const int pxi = x_to_col_num(p->x);
    const int pyi = y_to_row_num(p->y);
    
    vector<LocationType*> nearby_places = get_nearby_places2(pxi, pyi, locs_obj, num_loc_candidate, steps);
    vector<LocationType*> chosen = choose_mult_loc(p, nearby_places, num_loc_choose, rng, replace);
    
    out(i, 0) = p->id;
    for (int j = 0; j < num_loc_choose; j++) {
      LocationType* ch = chosen[j];
      out(i, j+1) = ch->id;
    }
  }
  
  return(out);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
assign_by_gravity2(matrix(c(-87.45, -87.0, 24.85, 25.05), 2, 2), 
                   matrix(c(-87.4, -87.5, -87.1, -87.1,
                            24.8,  24.9,  25.1,  25.0), 4, 2),
                   c(1, 1, 1, 1), 2, 3, 4328)

locs <- matrix(c(-87.45, -87.0, -87.4, -87.5, -87.1, -87.1, 
                 24.8,  24.9,  25.1,  25.0, 24.85, 25.05), 6, 2)
build_network(locs, rep(2, 6), 3, 4326)
*/