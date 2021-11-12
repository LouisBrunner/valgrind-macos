// Valgrind supports demangling Rust symbols (both the "v0" and "legacy"
// mangling schemes), but we don't want to add a dependency on the Rust
// compiler for a single test. So this is a C program with function names that
// are mangled Rust symbols. In the output, they become demangled Rust names.
// It's a hack, but a useful one.

#include <stdlib.h>

// A v0 symbol that demangles to: <rustc_middle::ty::PredicateKind as rustc_middle::ty::fold::TypeFoldable>::fold_with::<rustc_infer::infer::resolve::OpportunisticVarResolver>
int _RINvYNtNtCs4uGc65yWeeX_12rustc_middle2ty13PredicateKindNtNtB5_4fold12TypeFoldable9fold_withNtNtNtCsgI90OQiJWEs_11rustc_infer5infer7resolve24OpportunisticVarResolverECsdozMG8X9FIu_21rustc_trait_selection(int *p)
{
   free(p);
   free(p);
   return 1;
}

// A v0 symbol that demangles to: rustc_expand::mbe::macro_parser::parse_tt
int _RNvNtNtCsaqSe1lZGvEL_12rustc_expand3mbe12macro_parser8parse_tt(int* p)
{
   return _RINvYNtNtCs4uGc65yWeeX_12rustc_middle2ty13PredicateKindNtNtB5_4fold12TypeFoldable9fold_withNtNtNtCsgI90OQiJWEs_11rustc_infer5infer7resolve24OpportunisticVarResolverECsdozMG8X9FIu_21rustc_trait_selection(p);
}

// A legacy symbol that demangles to: core::str::lossy::Utf8Lossy::from_bytes
int _ZN4core3str5lossy9Utf8Lossy10from_bytes17heb1677c8cb728b0bE(int* p)
{
   return _RNvNtNtCsaqSe1lZGvEL_12rustc_expand3mbe12macro_parser8parse_tt(p);
}

int main(void)
{
   return _ZN4core3str5lossy9Utf8Lossy10from_bytes17heb1677c8cb728b0bE(malloc(sizeof(int)));
}

