@echo off
ftnchek -nonovice -brief -nowrap -nopure -noarray -nopretty -arguments=no-arrayness -usage=no-arg-unused,no-arg-alias,no-arg-array-alias,no-arg-common-alias,no-arg-common-array-alias,no-com-block-unused,no-com-var-unused,no-com-var-set-unused,no-ext-undefined,no-ext-unused,no-label-unused,no-var-unused,no-var-set-unused -truncation=no-promotion,no-size-demotion,no-type-demotion,no-significant-figures %*

rem Options
rem  -usage=no-ext-unused,no-label-unused,no-var-unused,no-var-set-unused,no-arg-unused,no-com-block-unused,no-com-var-unused,no-com-var-set-unused,no-ext-undefined
rem  -f77=automatic-array,common-subprog-name,param-noparen,statement-order
rem  -f77=mixed-expr  Mixed precision expressions
rem  -truncation=no-promotion,no-size-demotion,no-type-demotion,no-significant-figures
rem  -arguments=no-arrayness  Ignore array dimension/size mismatches
rem  -array=no-dimensions  Don't warn if the actual and dummy arguments differ in their number of dimensions, or if the actual argument is an array element while the dummy argument is a whole array
rem  -array=no-size  Don't warn if the actual and dummy arguments are arrays, but they differ in number of elements
rem  -array=none  Don't warn for either dimensions or size
rem  -pretty=no-continuation,no-long-line

rem Notes
rem  ftnchek doesn't support ENCODE/DECODE and this causes a number of erroneous warnings
rem  ftnchek doesn't support F9x module USE declarations
