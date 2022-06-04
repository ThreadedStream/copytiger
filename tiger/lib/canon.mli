open Ir 
open Temp

type block = stmt list 

val linearize : stmt -> stmt list 

val basic_blocks : stmt list -> label * block list 

val trace_schedule : label * block list -> stmt list 