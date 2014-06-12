#define MT_IMPLEMENT_TEMPLATES

#include <relational/learn.h>


void test_learn() {
  // Rule learning algorithm is heuristic and makes some random choices.
  rnd.seed(12345);


  // -------------------------------------
  //  PARAMETERS
  // -------------------------------------
  // Regularizer
  double alpha_pen = 0.5;
  // Lower bounds for probabilities of states in case of noise outcome
  double prob_state_given_NoisyOutcome = 1e-10; // p_min
  // ... same, only for noisy default rule
  double prob_state_given_NoisyOutcome__in_noisyDefaultRule = 1e-11;
  // Log-file
  MT::String logfile("learn.log");

  // Symbols
  relational::SymL symbols;
  relational::ArgumentTypeL types;
  relational::readSymbolsAndTypes(symbols, types, "symbols.dat");

  // Data
  relational::StateTransitionL experiences = relational::StateTransition::read_SASAS("data.dat");
  PRINT(experiences.N);
//   write(transitions);

  // Rules
  relational::RuleSet rules;
  relational::RuleSet::read("rules.dat", rules);
  cout<<"Rules successfully read from file \""<<rulesFile_name<<"\"."<<endl;
  cout<<"Rules ("<<rules.num()<<"):"<<endl;   rules.write(cout);


  // -------------------------------------
  //  LEARN
  // -------------------------------------

  relational::learn::set_penalty(alpha_pen);
  relational::learn::set_p_min(prob_state_given_NoisyOutcome, prob_state_given_NoisyOutcome__in_noisyDefaultRule);
  relational::RuleSetContainer rulesC;
  //cout<<"Starting rule-learning... (might take quite a while; watch '"<<logfile<<"')"<<endl;
  //relational::learn::learn_rules(rulesC, transitions);

  //relational::write(rulesC.rules, "learned_rules.dat");
  //cout<<"Learned rules have been written to 'learned_rules.dat'."<<endl;

{
      arr experience_weights(experiences.N);
      experience_weights.setUni(1.);

  rulesC.clear();
  uint i;
  FOR1D(experiences, i) {
      relational::Literal::sort(experiences(i)->pre.lits);
  }
  relational::Literal::sort(experiences.last()->post.lits);

  rulesC.init(&experiences);

  //relational::learn::__initUsedFunctionValues(experiences);

  // Init default rule
  rulesC.rules.append(relational::Rule::generateDefaultRule());
  rulesC.recomputeDefaultRule();
  double bestscore = relational::learn::score(rulesC, experiences, TL::TL_DOUBLE_MIN, experience_weights);
  cout << "score: " << bestscore << endl;
  }
}



int main(int argc, char** argv){
  cout.precision(2);
  test_learn();

  return 0;
}

