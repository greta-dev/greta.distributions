# need some internal greta functions accessible

check_dims <- .internals$checks$check_dims
check_in_family <- .internals$checks$check_in_family
check_positive <- .internals$checks$check_positive
check_tf_version <- .internals$checks$check_tf_version
check_unit <- .internals$checks$check_unit

create_progress_bar <- .internals$inference$progress_bar$create_progress_bar

has_representation <- .internals$greta_arrays$has_representation
representation <- .internals$greta_arrays$representation

tf_lchoose <- .internals$tensors$tf_lchoose
tf_lbeta <- .internals$tensors$tf_lbeta

distrib <- .internals$nodes$constructors$distrib
distribution_node <- .internals$nodes$node_classes$distribution_node

as.greta_array <- .internals$greta_arrays$as.greta_array

tf_iprobit <- .internals$tensors$tf_iprobit
fl <- .internals$utils$misc$fl
tf_as_float <- .internals$tensors$tf_as_float
tf_rowsums <- .internals$tensors$tf_rowsums
op <- .internals$nodes$constructors$op

get_node <- .internals$greta_arrays$get_node
dag_class <- .internals$inference$dag_class
