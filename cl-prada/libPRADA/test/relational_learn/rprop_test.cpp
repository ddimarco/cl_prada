#define MT_IMPLEMENT_TEMPLATES

#include "MT/array.h"
#include "relational/utilTL.h"

typedef MT::Array<double> arr;

double calc_cost(const arr& xy) {
    double xdist = xy.elem(0) - 1.0;
    double ydist = xy.elem(1) - 2.0;
    return (10.0 * (xdist * xdist)) +
            (20 * (ydist * ydist)) + 30.0;
}

void calc_grad(const arr& xy, arr& out) {
    double xdist = xy.elem(0) - 1.0;
    double ydist = xy.elem(1) - 2.0;

    out[0] = 20.0 * xdist;
    out[1] = 40.0 * ydist;
}
#define DEBUG 3

int main(int argc, char** argv){

    double STOPPING_THRESHOLD = 0.001;
    uint MAX_STEPS = 1000;

    TL::Rprop rp;
    rp.init(0.025);
    int i=0;

    arr probs(2);
    probs.elem(0) = 0.5;
    probs.elem(1) = 0.5;
    arr gradients(2);
    double diff_cost = 10.0;
    double oldCost = 0.0;
    double cost = calc_cost(probs);

    if (DEBUG>0) cout<<"RProp:"<<endl;
      while(fabs(diff_cost) > STOPPING_THRESHOLD) {
        calc_grad(gradients, probs);
        rp.step(probs, gradients);
        oldCost = cost;
        cost = calc_cost(probs);
        diff_cost = cost - oldCost;
        if (DEBUG > 0) {
          if (DEBUG>1)
            cout << i << ": " << probs << " C=" << cost << " diff=" << diff_cost << " sum=" << sum(probs) << endl;
          else {
            if (i>1000) {
              cout << i << ": " << probs << " C=" << cost << " diff=" << diff_cost << " sum=" << sum(probs) << endl;
            }
            else {
              if (i%10==0)
                cout << i << ": " << probs << " C=" << cost << " diff=" << diff_cost << " sum=" << sum(probs) << endl;
            }
          }
        }
        i++;
        if (i>MAX_STEPS) {
          std::cerr <<endl<<endl<<endl<< "probs = " << probs << endl;
          MT_MSG("WARNING!!! Cannot learn rule probabilities! (No convergence.)");
        }
      }

      std::cerr <<endl<<endl<<endl<< "probs = " << probs << endl;

    return 0;
}


