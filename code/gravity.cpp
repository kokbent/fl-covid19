#include <Rcpp.h>
#include <math.h>
#include <vector>
#include <map>
#include <random>

using namespace std;
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]

double pixel_size   = 0.00416667;
double min_x_center = -87.78555;
double min_y_center = 24.46990;

struct PtsType {
  int id;
  double x;
  double y;
  int lid;
};

struct LocationType {
  int id;
  double x; // longitude
  double y; // latitude
  map<string,int> pixel = {{"xi",0}, {"yi",0}};
  int weight;
};

double rad_to_deg(double radians) { return 180*radians/M_PI; }
double deg_to_rad(double degree) { return M_PI*degree/180; }
int x_to_col_num(double x) { return (int) round((x - min_x_center)/pixel_size); }
int y_to_row_num(double y) { return (int) round((y - min_y_center)/pixel_size); }


bool xy_cmp(LocationType* a, LocationType* b) {
  if (a->pixel["xi"] < b->pixel["xi"]) {
    return true;
  } else if (a->pixel["xi"] > b->pixel["xi"]) {
    return false;
  } else {
    if (a->pixel["yi"] < b->pixel["yi"]) {
      return true;
    } else if (a->pixel["yi"] > b->pixel["yi"]) {
      return false;
    } else {
      return false; // c++ sort requires strict weak ordering--equality must result in false
    }
  }
}


double haversine(double lon1, double lat1, double lon2, double lat2) {
  // Calculate the great circle distance between two points
  // on the earth (specified in decimal degrees)
  
  // convert decimal degrees to radians
  lon1 = deg_to_rad(lon1);
  lon2 = deg_to_rad(lon2);
  lat1 = deg_to_rad(lat1);
  lat2 = deg_to_rad(lat2);
  // haversine formula
  double dlon = lon2 - lon1;
  double dlat = lat2 - lat1;
  double a = pow(sin(dlat/2),2) + cos(lat1) * cos(lat2) * pow(sin(dlon/2),2);
  double c = 2 * asin(sqrt(a));
  double km = 6371 * c;
  return km;
}


vector<LocationType*> to_locs_obj (NumericMatrix locs, NumericVector weights) {
  int nlocs = locs.nrow();
  vector<LocationType*> locs_vec;
  
  for (int i = 0; i < nlocs; i++) {
    LocationType* w = new LocationType();
    w->id = i + 1;
    w->x = locs(i, 0);
    w->y = locs(i, 1);
    w->weight = weights[i];
    
    w->pixel["xi"] = x_to_col_num(w->x);
    w->pixel["yi"] = y_to_row_num(w->y);
    
    locs_vec.push_back(w);
  }
  
  sort(locs_vec.begin(), locs_vec.end(), xy_cmp);
  
  return(locs_vec);
}


vector<PtsType*> to_pts_obj (NumericMatrix pts) {
  int npts = pts.nrow();
  vector<PtsType*> pts_vec;
  
  for (int i = 0; i < npts; i++) {
    PtsType* p = new PtsType();
    p->id = i + 1;
    p->x = pts(i, 0);
    p->y = pts(i, 1);
    p->lid = 0;
    
    pts_vec.push_back(p);
  }
  
  return(pts_vec);
}


unsigned int binary_search(const vector<LocationType*> &loc_vec, int search_coord, 
                           string label, unsigned int start, unsigned int end_plus_one) {
  if (start >= end_plus_one) return end_plus_one;
  if (loc_vec[start]->pixel[label] > search_coord)  return start;
  if (loc_vec[end_plus_one-1]->pixel[label] < search_coord) return end_plus_one;
  
  unsigned int imin = start;
  unsigned int imax = end_plus_one;
  
  while (imin < imax) {
    unsigned int imid = imin + (imax-imin)/2; // TODO - verify that integer math is safe here
    if (loc_vec[imid]->pixel[label] < search_coord) {
      imin = imid+1;
    } else {
      imax = imid;
    }
  }
  
  return imin;
}


vector<LocationType*> get_nearby_places(int pxi, int pyi, const vector<LocationType*> &places, 
                                        unsigned int num_loc_needed, int steps = 20) {
  int commute_range = 0;
  vector<LocationType*> nearby_places; // by index in places
  
  while ((nearby_places.size() < num_loc_needed and nearby_places.size() < places.size())) {
    nearby_places.clear();
    commute_range += steps;
    
    for (int x_val = pxi-commute_range; x_val <= pxi+commute_range; ++x_val) {
      unsigned int start_pos = 0;
      unsigned int end_pos_plus_one = places.size();
      
      start_pos = binary_search(places, x_val, "xi", start_pos, end_pos_plus_one);
      end_pos_plus_one = binary_search(places, x_val+1, "xi", start_pos, end_pos_plus_one);
      
      if (start_pos == end_pos_plus_one) { continue; }
      
      start_pos = binary_search(places, pyi-commute_range, "yi", start_pos, end_pos_plus_one);
      end_pos_plus_one = binary_search(places, pyi+commute_range+1, "yi", start_pos, end_pos_plus_one);
      
      for (unsigned int i = start_pos; i < end_pos_plus_one; ++i) { nearby_places.push_back(places[i]); }
    }
  }
  
  return nearby_places;
}


LocationType* choose_one_loc(PtsType* p, vector<LocationType*> nearby_places, mt19937& rng) {
  assert(nearby_places.size() > 0);
  LocationType* chosen_place = nearby_places.back();
  vector<double> raw_weights(nearby_places.size(), 0.0);
  double total_weight = 0.0;
  for (unsigned int i = 0; i < nearby_places.size(); ++i) {
    const LocationType* w = nearby_places[i];
    const double dist = haversine(p->x, p->y, w->x, w->y);
    const double size = w->weight;
    raw_weights[i] = size / (dist*dist);
    total_weight += raw_weights[i];
  }
  
  uniform_real_distribution<double> runif(0.0, total_weight);
  double r = runif(rng);
  
  for (unsigned int i = 0; i < raw_weights.size(); ++i) {
    if (r < raw_weights[i]) {
      chosen_place = nearby_places[i];
      break;
    } else {
      r -= raw_weights[i];
    }
  }
  
  return chosen_place;
}


// [[Rcpp::export]]
NumericMatrix assign_by_gravity(NumericMatrix pts, NumericMatrix locs, NumericVector weights,
                                int num_loc, unsigned int seed, 
                                double min_x = -87.78555, double min_y = 24.46990,
                                int steps = 20) {
  int npts = pts.nrow();
  // int nlocs = locs.nrow();
  mt19937 rng(seed);
  // NumericVector out(npts);
  NumericMatrix out(npts, 2);
  min_x_center = min_x;
  min_y_center = min_y;
  
  vector<LocationType*> locs_obj = to_locs_obj(locs, weights);
  vector<PtsType*> pts_obj = to_pts_obj(pts);
  shuffle(pts_obj.begin(), pts_obj.end(), rng);
  
  for (int i = 0; i < npts; i++) {
    PtsType* p = pts_obj[i];
    const int pxi = x_to_col_num(p->x);
    const int pyi = y_to_row_num(p->y);
    
    vector<LocationType*> nearby_places = get_nearby_places(pxi, pyi, locs_obj, num_loc, steps);
    LocationType* chosen = choose_one_loc(p, nearby_places, rng);
    out(i, 0) = p->id;
    out(i, 1) = chosen->id;
  }
  
  return(out);
}




// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
assign_by_gravity(matrix(c(-87.45, -87.0, 24.85, 25.05), 2, 2), 
                  matrix(c(-87.4, -87.5, -87.1, -87.1,
                            24.8,  24.9,  25.1,  25.0), 4, 2),
                  c(1, 1, 1, 1), 1, 4327)
*/
