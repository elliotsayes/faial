# DRF Analysis

Code specific to the DRF analysis.

- bin/tui.ml: Text UI widgets
- bin/main.ml: Faial v1.0
- bin/parse2.mly: parser used by Faial v1.0
- bin/scan.mll: lexer used by Faial v1.0
- bin/next_gen.ml: Faial v2.0
- bin/z3_solver.ml: SMT solving functionality used by Faial v2.0
- lib/typecheck.ml: Lightweight sanity checks for protocols
- lib/wellformed.ml: Step 1. converts proto.ml into well-formed terms (synchronized/unsynchronized loops/conditionals, etc)
- lib/phasealign.ml: Step 2. well-formed into aligned protocols
- lib/phasesplit.ml: Step 3. aligned-protocols into phase-split
- lib/locsplit.ml: Step 4. phase-split into location split (one phase-split per array)
- lib/flatacc.ml: Step 5. location-split into flat-acc: flattens control-flow flow into lists of conditional accesses
- lib/symbexp.ml: Step 6. flat-acc into boolean expressions
- lib/gensmtlib2.ml: Step 7. boolean expressions into smtlib2 (Faial v1.0 only)
