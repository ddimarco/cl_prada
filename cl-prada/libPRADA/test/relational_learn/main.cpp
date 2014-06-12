#define MT_IMPLEMENT_TEMPLATES

#include <relational/learn.h>

#include <boost/program_options.hpp>

// -------------------------------------
//  PARAMETERS
// -------------------------------------
// Regularizer
double alpha_pen = 0.5;
// Lower bounds for probabilities of states in case of noise outcome
double prob_state_given_NoisyOutcome = 1e-10; // p_min
// ... same, only for noisy default rule
double prob_state_given_NoisyOutcome__in_noisyDefaultRule = 1e-11;


void test_learn(const std::string& symbols_file, const std::string& data_file, bool sas, const std::string& rules_file) {
    // Rule learning algorithm is heuristic and makes some random choices.
    rnd.seed(12345);

    // Log-file
    MT::String logfile("learn.log");

    // Symbols
    relational::SymL symbols;
    relational::ArgumentTypeL types;
    cout << "loading symbols file" << endl;
    relational::readSymbolsAndTypes(symbols, types, symbols_file.c_str());

    // Data
    relational::StateTransitionL transitions;
    if (sas) {
        transitions = relational::StateTransition::read_SAS_SAS(data_file.c_str());
    } else {
        transitions = relational::StateTransition::read_SASAS(data_file.c_str());
    }
    PRINT(transitions.N);
    //   write(transitions);


    // -------------------------------------
    //  LEARN
    // -------------------------------------

    relational::learn::set_penalty(alpha_pen);
    relational::learn::set_p_min(prob_state_given_NoisyOutcome, prob_state_given_NoisyOutcome__in_noisyDefaultRule);
    relational::RuleSetContainer rulesC;
    cout<<"Starting rule-learning... (might take quite a while; watch '"<<logfile<<"')"<<endl;
    relational::learn::learn_rules(rulesC, transitions);

    relational::write(rulesC.rules, rules_file.c_str());
    cout<<"Learned rules have been written to 'learned_rules.dat'."<<endl;
}



int main(int argc, char** argv){
    cout.precision(2);

    namespace po = boost::program_options;
    po::options_description desc("Options");
    desc.add_options()
            ("help", "print this overview")

            ("data,d", po::value<std::string>(), "filename referring to learning data")
            ("symbols,s", po::value<std::string>(), "filename referring to the symbol definitions")
            ("rules,o", po::value<std::string>(), "filename in which the learned rules will be written")

            ("sasas", "data is in state - action - state - action format (default is: state - action - state)")

            ("alpha-pen", po::value<double>(), "regularization penalty α (see Pasula et al., 2007)")
            ("p-min", po::value<double>(), "minimum probability for states in the noise outcome")
            ;

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, desc), vm);
    if (vm.count("help")) {
        cout << desc << endl;
        return 0;
    }

    if (vm.count("alpha-pen")) {
        alpha_pen = vm["alpha-pen"].as<double>();
    }
    if (vm.count("p-min")) {
        prob_state_given_NoisyOutcome = vm["p-min"].as<double>();
    }

    std::string symbols, data, rules;
    if (vm.count("data")) {
        data = vm["data"].as<std::string>();
    } else {
        data = "data.dat";
    }

    if (vm.count("symbols")) {
        symbols = vm["symbols"].as<std::string>();
    } else {
        symbols = "symbols.dat";
    }
    if (vm.count("rules")) {
        rules = vm["rules"].as<std::string>();
    } else {
        rules = "learned_rules.dat";
    }

    test_learn(symbols, data, !vm.count("sasas"), rules);

    return 0;
}

