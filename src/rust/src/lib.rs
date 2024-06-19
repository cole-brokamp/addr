use extendr_api::prelude::*;

/// Return list of lists of address tags to R.
/// @param input a character vector of addresses
/// @rdname addr_tag
/// @export
#[extendr]
fn usaddress_tag(input: Vec<String>) -> Robj {
    let ta: Vec<_> = input
	.iter().map(|x| usaddress::parse(x).unwrap())
	.map(|x| Pairlist::from_pairs(x))
	.collect();
    return r!(List::from_values(ta))
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod addr;
    fn usaddress_tag;
}
