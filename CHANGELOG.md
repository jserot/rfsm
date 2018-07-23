# Changes

# 1.1 (Jul XX, 2018)
    * Support for float values (see examples/single/heron/v1)
    * Changed syntax for integer range (int<lo:hi> instead of int<lo..hi>)
    * Support for global functions (see examples/single/heron/v2)
    * Support for (1D) array type (see examples/single/conv13/v2)
    * Bug fix for negative constants in parser
    * Boolean constants are now denoted (and written) 0 (resp. 1) 
    * Boolean type is now translated as [std_logic] in VHDL (unless option [-vhld_bool_as_bool] is asserted)
    * With option [-vhdl_numeric_std], ranged integers are translated as [unsigned] and [signed] in VHDL 
    * The simulator does not stop when encountering an initialized value but propagates it

# 1.0 (Feb 25, 2018)
    * First public version
