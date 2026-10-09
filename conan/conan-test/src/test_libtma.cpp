// Conan install-verification test: libtma's main entry points, to confirm the
// headers (and libqe's adjacency.hpp, which they include) compile and link.
#include <armadillo>
#include <libtma/libtma.hpp>
#include <cassert>
#include <cmath>
#include <vector>

int main() {
    arma::mat codes = {{1, 1, 0}, {1, 0, 1}, {0, 1, 1}};

    // ── stanza model ─────────────────────────────────────────────────────────
    arma::mat stanza = qe::accumulate_stanza(codes, /*back=*/2, /*forward=*/0, /*binary=*/true);
    assert(stanza.n_rows == 3 && stanza.n_cols == 3);   // choose_two(3)

    // ── context-tensor model + weight model ─────────────────────────────────
    arma::vec tensor = {1.0, 3.0};                       // default tensor
    qe::TensorNetworks t = qe::apply_tensor_unit(
        tensor, {2}, {}, {}, {}, arma::imat(3, 0), {0, 1, 2}, codes, arma::vec{0, 1, 2}, true);
    assert(t.row_connection_counts.n_rows == 3 && t.row_connection_counts.n_cols == 9);
    arma::rowvec unit = qe::aggregate_row_connections(
        t.row_connection_counts, 3, false, qe::WeightModel::Sqrt);
    assert(unit.n_elem == 3 && std::isfinite(unit(0)));

    return 0;
}
