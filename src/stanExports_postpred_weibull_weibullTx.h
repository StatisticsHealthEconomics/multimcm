// Generated by rstantools.  Do not edit by hand.

/*
    rstanbmcm is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    rstanbmcm is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with rstanbmcm.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.21.0
#include <stan/model/model_header.hpp>
namespace model_postpred_weibull_weibullTx_namespace {
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;
static int current_statement_begin__;
stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_postpred_weibull_weibullTx");
    reader.add_event(72, 70, "end", "model_postpred_weibull_weibullTx");
    return reader;
}
template <typename T0__, typename T1__, typename T2__, typename T3__, class RNG>
Eigen::Matrix<typename boost::math::tools::promote_args<T0__, T1__, T2__, T3__>::type, 1, Eigen::Dynamic>
os_casemix_rng(const Eigen::Matrix<T0__, 1, Eigen::Dynamic>& curefrac,
                   const T1__& shape,
                   const Eigen::Matrix<T2__, 1, Eigen::Dynamic>& lambda0,
                   const Eigen::Matrix<T3__, 1, Eigen::Dynamic>& lambda_bg, RNG& base_rng__, std::ostream* pstream__) {
    typedef typename boost::math::tools::promote_args<T0__, T1__, T2__, T3__>::type local_scalar_t__;
    typedef local_scalar_t__ fun_return_scalar_t__;
    const static bool propto__ = true;
    (void) propto__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
    int current_statement_begin__ = -1;
    try {
        {
        current_statement_begin__ = 4;
        int n(0);
        (void) n;  // dummy to suppress unused var warning
        stan::math::fill(n, std::numeric_limits<int>::min());
        stan::math::assign(n,num_elements(curefrac));
        current_statement_begin__ = 5;
        validate_non_negative_index("time", "n", n);
        Eigen::Matrix<local_scalar_t__, 1, Eigen::Dynamic> time(n);
        stan::math::initialize(time, DUMMY_VAR__);
        stan::math::fill(time, DUMMY_VAR__);
        current_statement_begin__ = 6;
        validate_non_negative_index("cf", "n", n);
        std::vector<local_scalar_t__  > cf(n, local_scalar_t__(DUMMY_VAR__));
        stan::math::initialize(cf, DUMMY_VAR__);
        stan::math::fill(cf, DUMMY_VAR__);
        current_statement_begin__ = 8;
        for (int i = 1; i <= n; ++i) {
            current_statement_begin__ = 9;
            stan::model::assign(cf, 
                        stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                        uniform_rng(0, 1, base_rng__), 
                        "assigning variable cf");
            current_statement_begin__ = 11;
            if (as_bool(logical_lt(get_base1(cf, i, "cf", 1), get_base1(curefrac, i, "curefrac", 1)))) {
                current_statement_begin__ = 12;
                stan::model::assign(time, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            exponential_rng(get_base1(lambda_bg, i, "lambda_bg", 1), base_rng__), 
                            "assigning variable time");
            } else {
                current_statement_begin__ = 14;
                stan::model::assign(time, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            stan::math::fmin(weibull_rng(shape, get_base1(lambda0, i, "lambda0", 1), base_rng__), exponential_rng(get_base1(lambda_bg, i, "lambda_bg", 1), base_rng__)), 
                            "assigning variable time");
            }
        }
        current_statement_begin__ = 17;
        return stan::math::promote_scalar<fun_return_scalar_t__>(time);
        }
    } catch (const std::exception& e) {
        stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
        // Next line prevents compiler griping about no return
        throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
    }
}
struct os_casemix_rng_functor__ {
    template <typename T0__, typename T1__, typename T2__, typename T3__, class RNG>
        Eigen::Matrix<typename boost::math::tools::promote_args<T0__, T1__, T2__, T3__>::type, 1, Eigen::Dynamic>
    operator()(const Eigen::Matrix<T0__, 1, Eigen::Dynamic>& curefrac,
                   const T1__& shape,
                   const Eigen::Matrix<T2__, 1, Eigen::Dynamic>& lambda0,
                   const Eigen::Matrix<T3__, 1, Eigen::Dynamic>& lambda_bg, RNG& base_rng__, std::ostream* pstream__) const {
        return os_casemix_rng(curefrac, shape, lambda0, lambda_bg, base_rng__, pstream__);
    }
};
template <typename T0__, typename T1__, typename T2__, typename T3__, class RNG>
Eigen::Matrix<typename boost::math::tools::promote_args<T0__, T1__, T2__, T3__>::type, 1, Eigen::Dynamic>
pfs_casemix_rng(const Eigen::Matrix<T0__, 1, Eigen::Dynamic>& curefrac,
                    const T1__& shape,
                    const Eigen::Matrix<T2__, 1, Eigen::Dynamic>& lambda0,
                    const Eigen::Matrix<T3__, 1, Eigen::Dynamic>& lambda_bg, RNG& base_rng__, std::ostream* pstream__) {
    typedef typename boost::math::tools::promote_args<T0__, T1__, T2__, T3__>::type local_scalar_t__;
    typedef local_scalar_t__ fun_return_scalar_t__;
    const static bool propto__ = true;
    (void) propto__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
    int current_statement_begin__ = -1;
    try {
        {
        current_statement_begin__ = 23;
        int n(0);
        (void) n;  // dummy to suppress unused var warning
        stan::math::fill(n, std::numeric_limits<int>::min());
        stan::math::assign(n,num_elements(curefrac));
        current_statement_begin__ = 24;
        validate_non_negative_index("time", "n", n);
        Eigen::Matrix<local_scalar_t__, 1, Eigen::Dynamic> time(n);
        stan::math::initialize(time, DUMMY_VAR__);
        stan::math::fill(time, DUMMY_VAR__);
        current_statement_begin__ = 25;
        validate_non_negative_index("cf", "n", n);
        std::vector<local_scalar_t__  > cf(n, local_scalar_t__(DUMMY_VAR__));
        stan::math::initialize(cf, DUMMY_VAR__);
        stan::math::fill(cf, DUMMY_VAR__);
        current_statement_begin__ = 27;
        for (int i = 1; i <= n; ++i) {
            current_statement_begin__ = 28;
            stan::model::assign(cf, 
                        stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                        uniform_rng(0, 1, base_rng__), 
                        "assigning variable cf");
            current_statement_begin__ = 30;
            if (as_bool(logical_lt(get_base1(cf, i, "cf", 1), get_base1(curefrac, i, "curefrac", 1)))) {
                current_statement_begin__ = 31;
                stan::model::assign(time, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            exponential_rng(get_base1(lambda_bg, i, "lambda_bg", 1), base_rng__), 
                            "assigning variable time");
            } else {
                current_statement_begin__ = 33;
                stan::model::assign(time, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            stan::math::fmin(weibull_rng(shape, get_base1(lambda0, i, "lambda0", 1), base_rng__), exponential_rng(get_base1(lambda_bg, i, "lambda_bg", 1), base_rng__)), 
                            "assigning variable time");
            }
        }
        current_statement_begin__ = 36;
        return stan::math::promote_scalar<fun_return_scalar_t__>(time);
        }
    } catch (const std::exception& e) {
        stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
        // Next line prevents compiler griping about no return
        throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
    }
}
struct pfs_casemix_rng_functor__ {
    template <typename T0__, typename T1__, typename T2__, typename T3__, class RNG>
        Eigen::Matrix<typename boost::math::tools::promote_args<T0__, T1__, T2__, T3__>::type, 1, Eigen::Dynamic>
    operator()(const Eigen::Matrix<T0__, 1, Eigen::Dynamic>& curefrac,
                    const T1__& shape,
                    const Eigen::Matrix<T2__, 1, Eigen::Dynamic>& lambda0,
                    const Eigen::Matrix<T3__, 1, Eigen::Dynamic>& lambda_bg, RNG& base_rng__, std::ostream* pstream__) const {
        return pfs_casemix_rng(curefrac, shape, lambda0, lambda_bg, base_rng__, pstream__);
    }
};
#include <stan_meta_header.hpp>
class model_postpred_weibull_weibullTx
  : public stan::model::model_base_crtp<model_postpred_weibull_weibullTx> {
private:
        int N_os;
        int N_pfs;
        int n_samples;
        matrix_d cf_os;
        matrix_d cf_pfs;
        matrix_d lambda_os;
        matrix_d lambda_pfs;
        matrix_d lambda_os_bg;
        matrix_d lambda_pfs_bg;
        vector_d shape_os;
        vector_d shape_pfs;
public:
    model_postpred_weibull_weibullTx(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, 0, pstream__);
    }
    model_postpred_weibull_weibullTx(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, random_seed__, pstream__);
    }
    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;
        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning
        current_statement_begin__ = -1;
        static const char* function__ = "model_postpred_weibull_weibullTx_namespace::model_postpred_weibull_weibullTx";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        try {
            // initialize data block variables from context__
            current_statement_begin__ = 41;
            context__.validate_dims("data initialization", "N_os", "int", context__.to_vec());
            N_os = int(0);
            vals_i__ = context__.vals_i("N_os");
            pos__ = 0;
            N_os = vals_i__[pos__++];
            check_greater_or_equal(function__, "N_os", N_os, 0);
            current_statement_begin__ = 42;
            context__.validate_dims("data initialization", "N_pfs", "int", context__.to_vec());
            N_pfs = int(0);
            vals_i__ = context__.vals_i("N_pfs");
            pos__ = 0;
            N_pfs = vals_i__[pos__++];
            check_greater_or_equal(function__, "N_pfs", N_pfs, 0);
            current_statement_begin__ = 44;
            context__.validate_dims("data initialization", "n_samples", "int", context__.to_vec());
            n_samples = int(0);
            vals_i__ = context__.vals_i("n_samples");
            pos__ = 0;
            n_samples = vals_i__[pos__++];
            check_greater_or_equal(function__, "n_samples", n_samples, 1);
            current_statement_begin__ = 45;
            validate_non_negative_index("cf_os", "n_samples", n_samples);
            validate_non_negative_index("cf_os", "N_os", N_os);
            context__.validate_dims("data initialization", "cf_os", "matrix_d", context__.to_vec(n_samples,N_os));
            cf_os = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(n_samples, N_os);
            vals_r__ = context__.vals_r("cf_os");
            pos__ = 0;
            size_t cf_os_j_2_max__ = N_os;
            size_t cf_os_j_1_max__ = n_samples;
            for (size_t j_2__ = 0; j_2__ < cf_os_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < cf_os_j_1_max__; ++j_1__) {
                    cf_os(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 46;
            validate_non_negative_index("cf_pfs", "n_samples", n_samples);
            validate_non_negative_index("cf_pfs", "N_pfs", N_pfs);
            context__.validate_dims("data initialization", "cf_pfs", "matrix_d", context__.to_vec(n_samples,N_pfs));
            cf_pfs = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(n_samples, N_pfs);
            vals_r__ = context__.vals_r("cf_pfs");
            pos__ = 0;
            size_t cf_pfs_j_2_max__ = N_pfs;
            size_t cf_pfs_j_1_max__ = n_samples;
            for (size_t j_2__ = 0; j_2__ < cf_pfs_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < cf_pfs_j_1_max__; ++j_1__) {
                    cf_pfs(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 48;
            validate_non_negative_index("lambda_os", "n_samples", n_samples);
            validate_non_negative_index("lambda_os", "N_os", N_os);
            context__.validate_dims("data initialization", "lambda_os", "matrix_d", context__.to_vec(n_samples,N_os));
            lambda_os = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(n_samples, N_os);
            vals_r__ = context__.vals_r("lambda_os");
            pos__ = 0;
            size_t lambda_os_j_2_max__ = N_os;
            size_t lambda_os_j_1_max__ = n_samples;
            for (size_t j_2__ = 0; j_2__ < lambda_os_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < lambda_os_j_1_max__; ++j_1__) {
                    lambda_os(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 49;
            validate_non_negative_index("lambda_pfs", "n_samples", n_samples);
            validate_non_negative_index("lambda_pfs", "N_pfs", N_pfs);
            context__.validate_dims("data initialization", "lambda_pfs", "matrix_d", context__.to_vec(n_samples,N_pfs));
            lambda_pfs = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(n_samples, N_pfs);
            vals_r__ = context__.vals_r("lambda_pfs");
            pos__ = 0;
            size_t lambda_pfs_j_2_max__ = N_pfs;
            size_t lambda_pfs_j_1_max__ = n_samples;
            for (size_t j_2__ = 0; j_2__ < lambda_pfs_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < lambda_pfs_j_1_max__; ++j_1__) {
                    lambda_pfs(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 51;
            validate_non_negative_index("lambda_os_bg", "n_samples", n_samples);
            validate_non_negative_index("lambda_os_bg", "N_os", N_os);
            context__.validate_dims("data initialization", "lambda_os_bg", "matrix_d", context__.to_vec(n_samples,N_os));
            lambda_os_bg = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(n_samples, N_os);
            vals_r__ = context__.vals_r("lambda_os_bg");
            pos__ = 0;
            size_t lambda_os_bg_j_2_max__ = N_os;
            size_t lambda_os_bg_j_1_max__ = n_samples;
            for (size_t j_2__ = 0; j_2__ < lambda_os_bg_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < lambda_os_bg_j_1_max__; ++j_1__) {
                    lambda_os_bg(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 52;
            validate_non_negative_index("lambda_pfs_bg", "n_samples", n_samples);
            validate_non_negative_index("lambda_pfs_bg", "N_pfs", N_pfs);
            context__.validate_dims("data initialization", "lambda_pfs_bg", "matrix_d", context__.to_vec(n_samples,N_pfs));
            lambda_pfs_bg = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(n_samples, N_pfs);
            vals_r__ = context__.vals_r("lambda_pfs_bg");
            pos__ = 0;
            size_t lambda_pfs_bg_j_2_max__ = N_pfs;
            size_t lambda_pfs_bg_j_1_max__ = n_samples;
            for (size_t j_2__ = 0; j_2__ < lambda_pfs_bg_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < lambda_pfs_bg_j_1_max__; ++j_1__) {
                    lambda_pfs_bg(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 53;
            validate_non_negative_index("shape_os", "n_samples", n_samples);
            context__.validate_dims("data initialization", "shape_os", "vector_d", context__.to_vec(n_samples));
            shape_os = Eigen::Matrix<double, Eigen::Dynamic, 1>(n_samples);
            vals_r__ = context__.vals_r("shape_os");
            pos__ = 0;
            size_t shape_os_j_1_max__ = n_samples;
            for (size_t j_1__ = 0; j_1__ < shape_os_j_1_max__; ++j_1__) {
                shape_os(j_1__) = vals_r__[pos__++];
            }
            current_statement_begin__ = 54;
            validate_non_negative_index("shape_pfs", "n_samples", n_samples);
            context__.validate_dims("data initialization", "shape_pfs", "vector_d", context__.to_vec(n_samples));
            shape_pfs = Eigen::Matrix<double, Eigen::Dynamic, 1>(n_samples);
            vals_r__ = context__.vals_r("shape_pfs");
            pos__ = 0;
            size_t shape_pfs_j_1_max__ = n_samples;
            for (size_t j_1__ = 0; j_1__ < shape_pfs_j_1_max__; ++j_1__) {
                shape_pfs(j_1__) = vals_r__[pos__++];
            }
            // initialize transformed data variables
            // execute transformed data statements
            // validate transformed data
            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    ~model_postpred_weibull_weibullTx() { }
    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        typedef double local_scalar_t__;
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;
        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }
    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }
    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {
        typedef T__ local_scalar_t__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning
        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
            // model parameters
            // model body
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
        lp_accum__.add(lp__);
        return lp_accum__.sum();
    } // log_prob()
    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }
    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("t_os_tilde");
        names__.push_back("t_pfs_tilde");
    }
    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dims__.push_back(n_samples);
        dims__.push_back(N_os);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(n_samples);
        dims__.push_back(N_pfs);
        dimss__.push_back(dims__);
    }
    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;
        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "model_postpred_weibull_weibullTx_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        if (!include_tparams__ && !include_gqs__) return;
        try {
            if (!include_gqs__ && !include_tparams__) return;
            if (!include_gqs__) return;
            // declare and define generated quantities
            current_statement_begin__ = 62;
            validate_non_negative_index("t_os_tilde", "n_samples", n_samples);
            validate_non_negative_index("t_os_tilde", "N_os", N_os);
            Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> t_os_tilde(n_samples, N_os);
            stan::math::initialize(t_os_tilde, DUMMY_VAR__);
            stan::math::fill(t_os_tilde, DUMMY_VAR__);
            current_statement_begin__ = 63;
            validate_non_negative_index("t_pfs_tilde", "n_samples", n_samples);
            validate_non_negative_index("t_pfs_tilde", "N_pfs", N_pfs);
            Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> t_pfs_tilde(n_samples, N_pfs);
            stan::math::initialize(t_pfs_tilde, DUMMY_VAR__);
            stan::math::fill(t_pfs_tilde, DUMMY_VAR__);
            // generated quantities statements
            current_statement_begin__ = 65;
            for (int i = 1; i <= n_samples; ++i) {
                current_statement_begin__ = 66;
                stan::model::assign(t_os_tilde, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::cons_list(stan::model::index_omni(), stan::model::nil_index_list())), 
                            os_casemix_rng(stan::model::rvalue(cf_os, stan::model::cons_list(stan::model::index_uni(i), stan::model::cons_list(stan::model::index_omni(), stan::model::nil_index_list())), "cf_os"), get_base1(shape_os, i, "shape_os", 1), stan::model::rvalue(lambda_os, stan::model::cons_list(stan::model::index_uni(i), stan::model::cons_list(stan::model::index_omni(), stan::model::nil_index_list())), "lambda_os"), stan::model::rvalue(lambda_os_bg, stan::model::cons_list(stan::model::index_uni(i), stan::model::cons_list(stan::model::index_omni(), stan::model::nil_index_list())), "lambda_os_bg"), base_rng__, pstream__), 
                            "assigning variable t_os_tilde");
                current_statement_begin__ = 67;
                stan::model::assign(t_pfs_tilde, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::cons_list(stan::model::index_omni(), stan::model::nil_index_list())), 
                            pfs_casemix_rng(stan::model::rvalue(cf_pfs, stan::model::cons_list(stan::model::index_uni(i), stan::model::cons_list(stan::model::index_omni(), stan::model::nil_index_list())), "cf_pfs"), get_base1(shape_pfs, i, "shape_pfs", 1), stan::model::rvalue(lambda_pfs, stan::model::cons_list(stan::model::index_uni(i), stan::model::cons_list(stan::model::index_omni(), stan::model::nil_index_list())), "lambda_pfs"), stan::model::rvalue(lambda_pfs_bg, stan::model::cons_list(stan::model::index_uni(i), stan::model::cons_list(stan::model::index_omni(), stan::model::nil_index_list())), "lambda_pfs_bg"), base_rng__, pstream__), 
                            "assigning variable t_pfs_tilde");
            }
            // validate, write generated quantities
            current_statement_begin__ = 62;
            size_t t_os_tilde_j_2_max__ = N_os;
            size_t t_os_tilde_j_1_max__ = n_samples;
            for (size_t j_2__ = 0; j_2__ < t_os_tilde_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < t_os_tilde_j_1_max__; ++j_1__) {
                    vars__.push_back(t_os_tilde(j_1__, j_2__));
                }
            }
            current_statement_begin__ = 63;
            size_t t_pfs_tilde_j_2_max__ = N_pfs;
            size_t t_pfs_tilde_j_1_max__ = n_samples;
            for (size_t j_2__ = 0; j_2__ < t_pfs_tilde_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < t_pfs_tilde_j_1_max__; ++j_1__) {
                    vars__.push_back(t_pfs_tilde(j_1__, j_2__));
                }
            }
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }
    std::string model_name() const {
        return "model_postpred_weibull_weibullTx";
    }
    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
        }
        if (!include_gqs__) return;
        size_t t_os_tilde_j_2_max__ = N_os;
        size_t t_os_tilde_j_1_max__ = n_samples;
        for (size_t j_2__ = 0; j_2__ < t_os_tilde_j_2_max__; ++j_2__) {
            for (size_t j_1__ = 0; j_1__ < t_os_tilde_j_1_max__; ++j_1__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "t_os_tilde" << '.' << j_1__ + 1 << '.' << j_2__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
        size_t t_pfs_tilde_j_2_max__ = N_pfs;
        size_t t_pfs_tilde_j_1_max__ = n_samples;
        for (size_t j_2__ = 0; j_2__ < t_pfs_tilde_j_2_max__; ++j_2__) {
            for (size_t j_1__ = 0; j_1__ < t_pfs_tilde_j_1_max__; ++j_1__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "t_pfs_tilde" << '.' << j_1__ + 1 << '.' << j_2__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
    }
    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
        }
        if (!include_gqs__) return;
        size_t t_os_tilde_j_2_max__ = N_os;
        size_t t_os_tilde_j_1_max__ = n_samples;
        for (size_t j_2__ = 0; j_2__ < t_os_tilde_j_2_max__; ++j_2__) {
            for (size_t j_1__ = 0; j_1__ < t_os_tilde_j_1_max__; ++j_1__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "t_os_tilde" << '.' << j_1__ + 1 << '.' << j_2__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
        size_t t_pfs_tilde_j_2_max__ = N_pfs;
        size_t t_pfs_tilde_j_1_max__ = n_samples;
        for (size_t j_2__ = 0; j_2__ < t_pfs_tilde_j_2_max__; ++j_2__) {
            for (size_t j_1__ = 0; j_1__ < t_pfs_tilde_j_1_max__; ++j_1__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "t_pfs_tilde" << '.' << j_1__ + 1 << '.' << j_2__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
    }
}; // model
}  // namespace
typedef model_postpred_weibull_weibullTx_namespace::model_postpred_weibull_weibullTx stan_model;
#ifndef USING_R
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
#endif
#endif