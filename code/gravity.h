#ifndef gravity_h
#define gravity_h

#include <iostream>
#include <Rcpp.h>
#include <math.h>
#include <vector>
#include <map>
#include <random>
#include <cmath>

using namespace std;
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]

double pixel_size   = 0.00416667 * 2;
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
  int capacity;
};

// struct StartEnd {
//   int start;
//   int end;
// };

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


vector<LocationType*> to_locs_obj (NumericMatrix locs, NumericVector weights,
                                   NumericVector capacity) {
  int nlocs = locs.nrow();
  vector<LocationType*> locs_vec;
  
  for (int i = 0; i < nlocs; i++) {
    LocationType* w = new LocationType();
    w->id = i + 1;
    w->x = locs(i, 0);
    w->y = locs(i, 1);
    w->weight = weights[i];
    w->capacity = capacity[i];
    
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
                                        unsigned int num_loc_needed, int steps = 2) {
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


vector<LocationType*> get_nearby_places2(int pxi, int pyi, const vector<LocationType*> &places, 
                                         unsigned int num_loc_needed, int steps = 1) {
  int commute_range = -steps;
  vector<LocationType*> nearby_places; // by index in places
  
  while ((nearby_places.size() < num_loc_needed and nearby_places.size() < places.size())) {
    // nearby_places.clear();
    commute_range += steps;
    
    // DEBUG MODE 
    if (commute_range > 10000) {
      Rcout << "Fatal error " << nearby_places.size() << "\n";
      // vector<LocationType*> nearby_places1;
      return(nearby_places);
    }
    
    for (int x_val = pxi-commute_range; x_val <= pxi+commute_range; ++x_val) {
      unsigned int start_posx = 0, start_posy = 0;
      unsigned int end_pos_plus_onex = places.size(), end_pos_plus_oney = places.size();
      
      if ((x_val < pxi-commute_range+steps) | (x_val > pxi+commute_range-steps) | (commute_range == 0)) {
        start_posx = binary_search(places, x_val, "xi", start_posx, end_pos_plus_onex);
        end_pos_plus_onex = binary_search(places, x_val+1, "xi", start_posx, end_pos_plus_onex);
        
        if (start_posx == end_pos_plus_onex) { continue; }
        
        start_posy = binary_search(places, pyi-commute_range, "yi", start_posx, end_pos_plus_onex);
        end_pos_plus_oney = binary_search(places, pyi+commute_range+1, "yi", start_posy, end_pos_plus_onex);
        
        for (unsigned int i = start_posy; i < end_pos_plus_oney; ++i) { nearby_places.push_back(places[i]); }
      } else {
        start_posx = binary_search(places, x_val, "xi", start_posx, end_pos_plus_onex);
        end_pos_plus_onex = binary_search(places, x_val+1, "xi", start_posx, end_pos_plus_onex);
        
        if (start_posx == end_pos_plus_onex) { continue; }
        
        // Upper
        start_posy = binary_search(places, pyi-commute_range, "yi", start_posx, end_pos_plus_onex);
        end_pos_plus_oney = binary_search(places, pyi-commute_range+steps, "yi", start_posy, end_pos_plus_onex);
        
        for (unsigned int i = start_posy; i < end_pos_plus_oney; ++i) { nearby_places.push_back(places[i]); }
        
        // Lower
        start_posy = binary_search(places, pyi+commute_range-steps+1, "yi", start_posx, end_pos_plus_onex);
        end_pos_plus_oney = binary_search(places, pyi+commute_range+1, "yi", start_posy, end_pos_plus_onex);
        
        for (unsigned int i = start_posy; i < end_pos_plus_oney; ++i) { nearby_places.push_back(places[i]); }
        
      }
      
      
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
    double dist = haversine(p->x, p->y, w->x, w->y);
    // if (dist == 0) dist = 0.001;
    const double size = w->weight;
    raw_weights[i] = size / (dist*dist);
    if (isinf(raw_weights[i]) || isnan(raw_weights[i])) raw_weights[i] = 0.0; // cannot share same coordinates
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


vector<LocationType*> choose_mult_loc(PtsType* p, vector<LocationType*> nearby_places,
                                      int num_loc_choose, mt19937& rng,
                                      bool replace = false) {
  assert(nearby_places.size() > 0);
  vector<LocationType*> chosen_places;
  vector<double> raw_weights(nearby_places.size(), 0.0);
  double total_weight = 0.0;
  for (unsigned int i = 0; i < nearby_places.size(); ++i) {
    const LocationType* w = nearby_places[i];
    double dist = haversine(p->x, p->y, w->x, w->y);
    const double size = w->weight;
    // if (dist == 0) dist = 0.001;
    raw_weights[i] = size / (dist*dist);
    if (isinf(raw_weights[i]) || isnan(raw_weights[i])) raw_weights[i] = 0.0; // cannot share same coordinates
    
    // DEBUG MODE
    // if (isnan(raw_weights[i])) {
    //   Rcout << size << "\t" << dist << endl;
    // }
    
    total_weight += raw_weights[i];
  }
  
  // Rcout << total_weight << endl;
  
  for (int j = 0; j < num_loc_choose; j++) {
    uniform_real_distribution<double> runif(0.0, total_weight);
    double r = runif(rng);
    // Rcout << j << " " << total_weight << " " << r << "\n";
    
    for (unsigned int i = 0; i < raw_weights.size(); i++) {
      if (r < raw_weights[i]) {
        chosen_places.push_back(nearby_places[i]);
        total_weight -= raw_weights[i];
        if (!replace) raw_weights[i] = raw_weights[i] - raw_weights[i];
        break;
      } else {
        r -= raw_weights[i];
      }
    }
  }
  
  return chosen_places;
}

#endif
